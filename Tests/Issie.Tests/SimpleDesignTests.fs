/// The Simple wire types (CommonTypes.SimpleDesign) are the contract for sending a design to
/// the dotnet sidecar, and integer canvas ids are what make them a straight projection. Four
/// things are pinned here:
///   - the exact JSON encoding the renderer sends (the vendored SimpleJson serializer, which
///     compiles under .NET too) deserialises on the .NET side via SimpleJsonDotNet;
///   - a SimpleDesign carries ALL the electrical information of a design, proven by shimming it
///     back into skeleton LoadedComponents and simulating: the shim must behave identically to
///     the original, cycle for cycle, on the golden-model harness;
///   - design admission (RegenerateIds.admitDesign): component ids come out dense and unique
///     across the design, per-sheet invariants hold, admission is idempotent, and parameter
///     slots and waveform references follow their components through renumbering;
///   - the per-sheet decode cache on the sidecar (DesignCache) skips unchanged sheets.
module SimpleDesignTests

open Expecto
open CommonTypes
open ParameterTypes
open Fable.SimpleJson
open Helpers
open TestFixtures
open CanvasBuilder

module DesignCache = Issie.Sidecar.DesignCache

// The shim lives in production now (SimpleDesignShim): the sidecar's baseline path uses it,
// and these tests exercise that same code rather than a private copy.
let private portCounts = SimpleDesignShim.portCounts
let private shimDesign = SimpleDesignShim.designToLoadedComponents

/// Load (which admits, as the app does), convert, shim - the whole journey a design makes,
/// minus the JSON.
let private convertProject (projectName: string) =
    let admitted = loadProject projectName
    let design = CanvasExtractor.simpleDesignOfLoadedComponents admitted
    admitted, design, shimDesign design

let private compIds (ldc: LoadedComponent) =
    fst ldc.CanvasState |> List.map (fun comp -> comp.Id)

let private portIds (ldc: LoadedComponent) =
    fst ldc.CanvasState
    |> List.collect (fun comp -> comp.InputPorts @ comp.OutputPorts)
    |> List.map (fun port -> port.Id)

let private connIds (ldc: LoadedComponent) =
    snd ldc.CanvasState |> List.map (fun conn -> conn.Id)

/// A representative SimpleDesign touching every encoding the wire types use: DU cases nullary
/// and with fields, records, options, bigints, Map with bigint keys, Map with structural keys.
let private representativeDesign : SimpleDesign =
    { TopSheet = "top"
      Sheets =
        [ { SheetName = "top"
            Components =
              [ { CompId = 1; TypeS = GateN(And, 2); Label = "G1" }
                { CompId = 2
                  TypeS =
                    Custom
                        { Name = "child"
                          InputLabels = [ "A", 4 ]
                          OutputLabels = [ "Q", 4 ]
                          Form = Some User
                          ParameterBindings = Some(Map.ofList [ ParamName "W", PInt 4I ])
                          Description = None }
                  Label = "CH1" }
                { CompId = 3
                  TypeS =
                    AsyncROM1
                        { Init = FromData
                          AddressWidth = 2
                          WordWidth = 8
                          Data = Map.ofList [ 0I, 255I; 3I, 1I ]
                          Comments = Some(Map.ofList [ 0I, "reset vector" ]) }
                  Label = "MEM" } ]
            Connections = [ { ConnId = 9; SrcComp = 1; SrcPort = 0; DestComp = 2; DestPort = 0 } ]
            DefaultBindings =
              Map.ofList [ ParamName "W", { Expression = PInt 4I; Description = "bus width" } ]
            ParamSlots =
              Map.ofList
                  [ { CompId = 2; CompSlot = CustomCompParam "W" },
                    { Expression = PParameter(ParamName "W")
                      Constraints = [ MinVal(PInt 1I, "width must be at least 1") ] } ] } ] }

let tests =
    testList "SimpleDesign" [

        test "fable json encoding round-trips through the dotnet decoder" {
            // Json.serialize here is the vendored SimpleJson reflection serializer - the same
            // code the renderer runs under Fable - so this pins the wire encoding without Fable.
            let json = Json.serialize<SimpleDesign> representativeDesign

            match SimpleJsonDotNet.tryDeserialise<SimpleDesign> json with
            | Error e -> failtest $"dotnet decode of the renderer encoding failed: {e}"
            | Ok decoded ->
                Expect.equal decoded representativeDesign "decoded design differs from what was encoded"
        }

        test "admission produces dense design-unique component ids and per-sheet invariants" {
            let admitted = loadProject "3cpu"

            let allCompIds = admitted |> List.collect compIds
            allCompIds |> List.iter (fun id -> Expect.isTrue (id > 0) $"component id {id} is not positive")

            Expect.equal (List.length (List.distinct allCompIds)) (List.length allCompIds)
                "component ids collide across the design"

            Expect.equal (List.max allCompIds) (List.length allCompIds)
                "component ids are not dense: the largest exceeds the count"

            for ldc in admitted do
                let ports = portIds ldc
                let conns = connIds ldc
                Expect.equal (List.length (List.distinct ports)) (List.length ports) $"{ldc.Name}: port ids collide"
                Expect.equal (List.length (List.distinct conns)) (List.length conns) $"{ldc.Name}: connection ids collide"
                (ports @ conns) |> List.iter (fun id -> Expect.isTrue (id > 0) $"{ldc.Name}: id {id} is not positive")
        }

        test "admission is idempotent" {
            let once = loadProject "3cpu"
            let twice, changed = RegenerateIds.admitDesign once
            Expect.isEmpty changed "a second admission renumbered a sheet"
            Expect.equal twice once "a second admission changed an already-admitted design"
        }

        test "admission renumbers collisions and keeps slots and wave references in step" {
            // Two sheets whose component ids collide (both use 1). The child declares a slot on
            // its component; the parent holds a wave selection whose access path names its own
            // component 1 and whose component element names the child's component 1.
            let childComp = makeComp 1 1 1 (NbitsNot 3) "N1"
            let slots: ComponentSlotExpr =
                Map.ofList [ { CompId = 1; CompSlot = Buswidth }, { Expression = PInt 3I; Constraints = [] } ]
            let child =
                makeLdc "child" (Some { DefaultBindings = Map.empty; ParamSlots = slots }) ([ childComp ], [])

            let parentComp = makeComp 1 0 1 (Input1(3, None)) "I0"
            let waveInfo: SavedWaveInfo =
                { SelectedWaves =
                    Some
                        [ { SimArrayIndex = 0
                            Id = ComponentId 1, [ ComponentId 1 ]   // (child's comp, [parent's instance])
                            PortType = PortType.Output
                            PortNumber = 0 } ]
                  Radix = None
                  WaveformColumnWidth = None
                  SelectedRams = Some(Map.ofList [ ComponentId 1, "ram" ])
                  SelectedFRams = None
                  WSConfig = None
                  ClkWidth = None
                  Cursor = None
                  LastClk = None
                  DisplayedPortIds = None }
            let parent =
                { makeLdc "parent" None ([ parentComp ], []) with WaveInfo = Some waveInfo }

            match RegenerateIds.admitDesign [ child; parent ] with
            | [ child'; parent' ], changed ->
                Expect.equal changed [ "parent" ] "only the second (colliding) sheet should renumber"

                let childId = (fst child'.CanvasState |> List.exactlyOne).Id
                let parentId = (fst parent'.CanvasState |> List.exactlyOne).Id
                Expect.equal childId 1 "the first sheet keeps its id"
                Expect.equal parentId 2 "the second sheet takes the next free id"

                let slotIds =
                    child'.LCParameterSlots
                    |> Option.map (fun defs -> defs.ParamSlots |> Map.toList |> List.map (fun (slot, _) -> slot.CompId))
                    |> Option.defaultValue []
                Expect.equal slotIds [ childId ] "the parameter slot does not follow its component"

                match parent'.WaveInfo with
                | Some { SelectedWaves = Some [ wave ]; SelectedRams = Some rams } ->
                    let (ComponentId waveComp), path = wave.Id
                    Expect.equal path [ ComponentId parentId ] "the access path does not follow the parent's own component"
                    Expect.equal waveComp childId "the child's component in the wave ref must NOT be touched by the parent's renumbering"
                    Expect.isTrue (Map.containsKey (ComponentId parentId) rams) "the RAM selection does not follow its component"
                | _ -> failtest "waveform info lost in admission"
            | _ -> failtest "admission changed the number of sheets"
        }

        test "allocator grows past its initial size" {
            // more component ids than the allocator's initial 10,000 entries
            let comps = List.init 12_000 (fun i -> makeComp (i + 1) 0 0 (Input1(1, None)) $"I{i}")
            let big = makeLdc "big" None (comps, [])
            let clash = makeLdc "clash" None ([ makeComp 5_000 0 0 (Input1(1, None)) "X" ], [])

            match RegenerateIds.admitDesign [ big; clash ] with
            | [ _; clash' ], changed ->
                Expect.equal changed [ "clash" ] "the colliding sheet renumbers"
                Expect.equal (compIds clash') [ 12_001 ] "the next free id sits past the grown region"
            | _ -> failtest "admission changed the number of sheets"
        }

        test "3cpu converts: every connection endpoint names a real component and port" {
            let _, design, _ = convertProject "3cpu"

            for sheet in design.Sheets do
                let arities =
                    sheet.Components
                    |> List.map (fun sc -> sc.CompId, portCounts sc.TypeS sc.Label)
                    |> Map.ofList

                for conn in sheet.Connections do
                    match Map.tryFind conn.SrcComp arities, Map.tryFind conn.DestComp arities with
                    | Some (_, nOut), Some (nIn, _) ->
                        Expect.isLessThan conn.SrcPort nOut
                            $"{sheet.SheetName}: connection {conn.ConnId} source port out of range"
                        Expect.isLessThan conn.DestPort nIn
                            $"{sheet.SheetName}: connection {conn.ConnId} target port out of range"
                    | _ -> failtest $"{sheet.SheetName}: connection {conn.ConnId} names a missing component"
        }

        test "custom instance signatures agree between original and shimmed design" {
            for project in [ "3cpu"; "adder4"; "1fulladder"; "customPair" ] do
                let admitted, _, shimmed = convertProject project

                for ldc in admitted do
                    let parentBindings =
                        ldc.LCParameterSlots
                        |> Option.map (fun defs -> bindingsOf defs.DefaultBindings)
                        |> Option.defaultValue Map.empty

                    for comp in fst ldc.CanvasState do
                        match comp.Type with
                        | Custom custom ->
                            let instanceBindings = Option.defaultValue Map.empty custom.ParameterBindings
                            let signatureIn ldcs =
                                CanvasExtractor.signatureOfInstance ldcs parentBindings custom.Name instanceBindings
                                |> Option.map (fun (ins, outs) -> List.sort ins, List.sort outs)
                            Expect.equal (signatureIn shimmed) (signatureIn admitted)
                                $"{project}/{ldc.Name}: instance of {custom.Name} resolves differently after the shim"
                        | _ -> ()
        }

        test "shimmed designs simulate identically to their originals" {
            // the same (project, top, ticks) triples the golden-model tests pin
            for project, top, ticks in [ "1fulladder", "fulladd", 8; "adder4", "fa4", 8; "3cpu", "eep1", 30 ] do
                let admitted, design, shimmed = convertProject project
                Expect.equal design.TopSheet top $"{project}: converter picked the wrong top sheet"

                let original = GoldenModel.runGoldenLdcs admitted top ticks
                let viaWire = GoldenModel.runGoldenLdcs shimmed top ticks
                Expect.equal viaWire original
                    $"{project}/{top}: the design that crossed the wire behaves differently"
        }

        test "sendDesign framing parses and the sheet cache skips unchanged sheets" {
            let _, design, _ = convertProject "3cpu"
            let sheetJsons = design.Sheets |> List.map Json.serialize<SimpleSheet>

            // the renderer's packStrings framing, built the .NET way: uint32 LE length + UTF-8
            let pack (strings: string list) =
                strings
                |> List.collect (fun s ->
                    let bytes = System.Text.Encoding.UTF8.GetBytes s
                    [ System.BitConverter.GetBytes bytes.Length; bytes ])
                |> Array.concat

            match DesignCache.parsePayload (pack (design.TopSheet :: sheetJsons)) with
            | Error e -> failtest e
            | Ok(topSheet, jsons) ->
                Expect.equal topSheet design.TopSheet "top sheet name survives the framing"
                Expect.equal jsons sheetJsons "sheet JSONs survive the framing"

                match DesignCache.decodeSheets Map.empty jsons with
                | Error e -> failtest e
                | Ok(sheets, decodedCold, cache) ->
                    Expect.equal sheets design.Sheets "decoded sheets equal the originals"
                    Expect.equal decodedCold design.Sheets.Length "cold: every sheet decodes"

                    match DesignCache.decodeSheets cache jsons with
                    | Error e -> failtest e
                    | Ok(sheetsAgain, decodedWarm, _) ->
                        Expect.equal sheetsAgain design.Sheets "cached sheets equal the originals"
                        Expect.equal decodedWarm 0 "warm: nothing decodes"

                    // one edited sheet costs exactly one decode
                    let sheet0 = design.Sheets.Head

                    let modified =
                        { sheet0 with
                            Components =
                                sheet0.Components
                                |> List.map (fun comp -> { comp with Label = comp.Label + "X" }) }

                    let changedJsons = Json.serialize<SimpleSheet> modified :: List.tail sheetJsons

                    match DesignCache.decodeSheets cache changedJsons with
                    | Error e -> failtest e
                    | Ok(_, decodedAfterEdit, _) ->
                        Expect.equal decodedAfterEdit 1 "an edit to one sheet decodes one sheet"
        }

        test "sidecar sim session: build, chunked run, digest identical to local render" {
            let admitted, design, _ = convertProject "3cpu"

            // build the baseline simulation from the wire-form design
            let buildReply = Issie.Sidecar.SimSession.build design 250
            Expect.isFalse (buildReply.Contains "error") $"build failed: {buildReply}"

            // every session-dependent command names the session it means, and the sidecar refuses
            // one that names another - so a test driving the wire format has to carry it too
            let epoch = Issie.Sidecar.SimSession.currentEpoch ()
            Expect.isGreaterThan epoch 0 "a successful build issues a session epoch"

            // chunked run with a 1ms budget per chunk: the client-driven progress contract -
            // repeat until done, each chunk one SimLog record
            let mutable chunks = 0
            let mutable finished = false

            while not finished && chunks < 10_000 do
                let reply = Issie.Sidecar.SimSession.run epoch 2_000 1
                chunks <- chunks + 1
                Expect.isFalse (reply.Contains "error") $"run failed: {reply}"
                finished <- reply.Contains "\"done\":true"

            Expect.isTrue finished "the chunked run never reached its target cycle"

            // the digest of the design that crossed the wire equals this runtime's own render
            // of the original - correctness of the whole baseline path in one string
            let viaSession = Issie.Sidecar.SimSession.digest design 30

            let local =
                match SimDigest.render admitted design.TopSheet 30 with
                | Ok text -> text
                | Error e -> failtest e

            Expect.equal viaSession local "session digest differs from the local render"
            Issie.Sidecar.SimSession.endSession epoch |> ignore

            // both kinds of invocation were recorded, in the same log both runtimes share
            let log = SimLog.recent ()
            Expect.isTrue
                (log |> List.exists (fun r -> r.Kind = SimLog.SimBuild && r.Sheet = design.TopSheet))
                "no build record in SimLog"
            Expect.isTrue
                (log |> List.exists (fun r -> r.Kind = SimLog.SimRun && r.Sheet = design.TopSheet))
                "no run record in SimLog"
        }

        test "a command naming a session the sidecar no longer holds is refused" {
            // The renderer cannot see inside the sidecar, so everything it believes about the
            // session there - that one exists, that it is of the design last sent, how far its
            // clock has run - is a belief with nothing to check it. The epoch is that check: a
            // build issues one, every session-dependent command names it, and a command naming any
            // other is refused rather than answered from whatever session happens to exist.
            let _, design, _ = convertProject "3cpu"

            let firstBuild = Issie.Sidecar.SimSession.build design 250
            Expect.isFalse (firstBuild.Contains "error") $"first build failed: {firstBuild}"
            let stale = Issie.Sidecar.SimSession.currentEpoch ()

            let secondBuild = Issie.Sidecar.SimSession.build design 250
            Expect.isFalse (secondBuild.Contains "error") $"second build failed: {secondBuild}"
            let current = Issie.Sidecar.SimSession.currentEpoch ()

            Expect.notEqual current stale "a rebuild issues a session of its own"

            // the run, the write and the read all refuse it, and each says so rather than failing
            // silently or answering from the new session
            let staleRun = Issie.Sidecar.SimSession.run stale 5 0
            Expect.stringContains staleRun "stale session" $"a stale run was answered: {staleRun}"

            let staleSet = Issie.Sidecar.SimSession.setInputs stale [||]
            Expect.stringContains staleSet "stale session" $"a stale write was answered: {staleSet}"

            match Issie.Sidecar.SimSession.read stale [||] with
            | Ok _ -> failtest "a stale read was answered"
            | Error e -> Expect.stringContains e "stale session" $"unexpected read error: {e}"

            // and the current one still works
            let liveRun = Issie.Sidecar.SimSession.run current 5 0
            Expect.isFalse (liveRun.Contains "error") $"the live session was refused: {liveRun}"

            // ending names a session too, so a stale end cannot drop the live one
            let staleEnd = Issie.Sidecar.SimSession.endSession stale
            Expect.stringContains staleEnd "stale session" $"a stale end was accepted: {staleEnd}"
            Expect.equal (Issie.Sidecar.SimSession.currentEpoch ()) current "the live session survived"

            Issie.Sidecar.SimSession.endSession current |> ignore
            Expect.equal (Issie.Sidecar.SimSession.currentEpoch ()) 0 "ending leaves no session"
        }

        test "sidecar SimSetInputs and SimRead agree with a local simulation, via wire payloads" {
            let admitted, design, shimmed = convertProject "3cpu"
            let ticks = 30

            // the session, driven through the exact wire-format payloads
            let buildReply = Issie.Sidecar.SimSession.build design 250
            Expect.isFalse (buildReply.Contains "error") $"build failed: {buildReply}"
            let epoch = Issie.Sidecar.SimSession.currentEpoch ()

            // a local simulation of the same shimmed design, driven identically
            let top = shimmed |> List.find (fun ldc -> ldc.Name = design.TopSheet)
            let localSim =
                match Simulator.startCircuitSimulation 250 design.TopSheet top.CanvasState shimmed with
                | Ok simData -> simData
                | Error e -> failtest $"local build failed: %A{e.ErrType}"
            let localFs = localSim.FastSim

            let u32s (values: int list) =
                values |> List.collect (System.BitConverter.GetBytes >> Array.toList) |> Array.ofList

            // drive both with the digest stimulus, tick by tick: run to the tick, set inputs at it
            for tick in 0 .. ticks - 1 do
                if tick > 0 then
                    Issie.Sidecar.SimSession.run epoch tick 0 |> ignore
                    FastRun.runFastSimulation None tick localFs |> ignore

                let inputs = localSim.Inputs |> List.sortBy (fun (_, ComponentLabel l, _) -> l)

                let setPayload =
                    [ tick; List.length inputs ]
                    @ (inputs
                       |> List.mapi (fun i (ComponentId cid, _, width) ->
                           let value = SimDigest.stimulus i tick width
                           [ cid; int (uint32 (value &&& 4294967295I)); int (uint32 (value >>> 32)) ])
                       |> List.concat)
                    |> u32s

                let setReply = Issie.Sidecar.SimSession.setInputs epoch setPayload
                Expect.isFalse (setReply.Contains "error") $"setInputs failed: {setReply}"

                inputs
                |> List.iteri (fun i (cid, _, width) ->
                    let fd = NumberHelpers.convertBigintToFastData width (SimDigest.stimulus i tick width)
                    FastExtract.changeInput cid (SimGraphTypes.IData fd) tick localFs)

            Issie.Sidecar.SimSession.run epoch (ticks - 1) 0 |> ignore
            FastRun.runFastSimulation None (ticks - 1) localFs |> ignore

            // read a window of every top-level clocked component plus one nested one (path in
            // the payload), and compare word for word with local extraction
            let itemsWanted =
                localFs.FClockedComps
                |> Array.filter (fun fc ->
                    (match fc.FType with ROM1 _ -> false | _ -> true)
                    && (FastExtract.extractFastSimulationOutput localFs 0 fc.fId (OutputPortNumber 0)
                        |> function SimGraphTypes.IData fd -> fd.Width <= 32 | _ -> false))
                |> Array.truncate 6
                |> Array.toList
                |> List.map (fun fc -> fc.fId)

            Expect.isTrue (itemsWanted |> List.exists (fun (_, path) -> not (List.isEmpty path)))
                "expected at least one nested item so access-path parsing is exercised"

            // the sampled read at three settings: a dense window (rep 1), a zoomed-out window
            // (rep 3 - the viewer's SamplingZoom), and the tooltip degenerate case (1,1)
            let readSampled startCycle rep samples =
                let payload =
                    [ startCycle; rep; samples; List.length itemsWanted ]
                    @ (itemsWanted
                       |> List.collect (fun (ComponentId cid, path) ->
                           [ cid; 0; List.length path ] @ (path |> List.map (fun (ComponentId p) -> p))))
                    |> u32s

                match Issie.Sidecar.SimSession.read epoch payload with
                | Error e -> failtest $"SimRead (start {startCycle}, rep {rep}) failed: {e}"
                | Ok reply ->
                    Expect.equal (int (System.BitConverter.ToUInt32(reply, 0))) (List.length itemsWanted) "signal count"
                    Expect.equal (int (System.BitConverter.ToUInt32(reply, 4))) samples "sample count"

                    itemsWanted
                    |> List.iteri (fun signalIndex fid ->
                        for j in 0 .. samples - 1 do
                            let cycle = startCycle + j * rep
                            let wire = System.BitConverter.ToUInt32(reply, 8 + 4 * (signalIndex * samples + j))

                            let local =
                                match FastExtract.extractFastSimulationOutput localFs cycle fid (OutputPortNumber 0) with
                                | SimGraphTypes.IData fd -> uint32 fd.GetBigInt
                                | _ -> failtest "algebraic value in local read"

                            Expect.equal wire local $"signal {signalIndex} cycle {cycle} (rep {rep}) differs")

            readSampled (ticks - 10) 1 10
            readSampled 2 3 9
            readSampled (ticks - 1) 1 1

            Issie.Sidecar.SimSession.endSession epoch |> ignore
        }

        test "integer ids survive a save and reload round trip" {
            let admitted = loadProject "3cpu"
            let ldc = admitted |> List.find (fun l -> l.Name = "eep1")

            match JsonHelpers.stateToJsonString (ldc.CanvasState, ldc.WaveInfo, None) with
            | Error e -> failtest $"save failed: {e}"
            | Ok json ->
                match JsonHelpers.jsonStringToState json with
                | Error e -> failtest $"reload failed: {e}"
                | Ok saved ->
                    // saved ids are decimal STRINGS on disk; the boundary parses them back to
                    // the identical integers
                    let (comps, conns), _, _ = sheetOfJson saved.getCanvas saved.getWaveInfo None
                    Expect.equal
                        (comps |> List.map (fun comp -> comp.Id) |> List.sort)
                        (compIds ldc |> List.sort)
                        "component ids changed across save and reload"
                    Expect.equal
                        (conns |> List.map (fun conn -> conn.Id) |> List.sort)
                        (connIds ldc |> List.sort)
                        "connection ids changed across save and reload"
        }
    ]
