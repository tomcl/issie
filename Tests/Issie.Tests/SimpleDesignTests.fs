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

        test "admission renumbers collisions, keeps slots in step and leaves the selection alone" {
            // Two sheets whose component ids collide (both use 1). The child declares a slot on
            // its component; the parent holds a wave selection naming the child's component by
            // label path, and a legacy RAM selection naming its own component by id.
            let childComp = makeComp 1 1 1 (NbitsNot 3) "N1"
            let slots: ComponentSlotExpr =
                Map.ofList [ { CompId = 1; CompSlot = Buswidth }, { Expression = PInt 3I; Constraints = [] } ]
            let child =
                makeLdc "child" (Some { DefaultBindings = Map.empty; ParamSlots = slots }) ([ childComp ], [])

            let parentComp = makeComp 1 0 1 (Input1(3, None)) "I0"
            let selected: WavePath =
                { WPLabels = [ "CHILD1"; "N1" ]; WPPortType = PortType.Output; WPPortNumber = 0 }
            let waveInfo: SavedWaveInfo =
                { SelectedWaves = Some [ selected ]
                  Radix = None
                  WaveformColumnWidth = None
                  SelectedRams = Some(Map.ofList [ ComponentId 1, "ram" ])
                  SelectedFRams = Some [ [ "CHILD1"; "M1" ], "ram" ]
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
                | Some info ->
                    // Renumbering cannot disturb what holds no ids. This is the whole point of
                    // saving the selection as label paths rather than as component ids: the
                    // careful partial remapping it used to need is not merely correct now, it is
                    // absent.
                    Expect.equal info.SelectedWaves (Some [ selected ])
                        "renumbering must not touch a selection named by labels"
                    Expect.equal info.SelectedFRams (Some [ [ "CHILD1"; "M1" ], "ram" ])
                        "nor the RAM selection"
                    match info.SelectedRams with
                    | Some rams ->
                        Expect.isTrue (Map.containsKey (ComponentId parentId) rams)
                            "the legacy id-keyed RAM field does follow its component"
                    | None -> failtest "legacy RAM selection lost in admission"
                | None -> failtest "waveform info lost in admission"
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

            // one message per sheet, as the sidecar receives them: the top sheet's name and one
            // sheet's JSON
            match DesignCache.parsePayload (pack [ design.TopSheet; List.head sheetJsons ]) with
            | Error e -> failtest e
            | Ok(topSheet, jsons) ->
                Expect.equal topSheet design.TopSheet "top sheet name survives the framing"
                Expect.equal jsons [ List.head sheetJsons ] "the sheet's JSON survives the framing"

            /// What the sidecar's SendDesign loop does across the messages of one upload: decode
            /// each sheet through the cache, which grows as it goes, then keep exactly this
            /// design's sheets once the last has arrived.
            let uploadDesign (cache: Map<string, SimpleSheet>) (jsons: string list) =
                let cacheAfter, sheets, decoded =
                    ((cache, [], 0), jsons)
                    ||> List.fold (fun (c, sheets, decoded) json ->
                        match DesignCache.decodeSheet c json with
                        | Error e -> failtest e
                        | Ok(sheet, wasDecoded, c') ->
                            c', sheets @ [ sheet ], decoded + (if wasDecoded then 1 else 0))

                sheets, decoded, DesignCache.keepOnly jsons cacheAfter

            let sheets, decodedCold, cache = uploadDesign Map.empty sheetJsons
            Expect.equal sheets design.Sheets "decoded sheets equal the originals"
            Expect.equal decodedCold design.Sheets.Length "cold: every sheet decodes"

            let sheetsAgain, decodedWarm, _ = uploadDesign cache sheetJsons
            Expect.equal sheetsAgain design.Sheets "cached sheets equal the originals"
            Expect.equal decodedWarm 0 "warm: nothing decodes"

            // one edited sheet costs exactly one decode
            let sheet0 = design.Sheets.Head

            let modified =
                { sheet0 with
                    Components = sheet0.Components |> List.map (fun comp -> { comp with Label = comp.Label + "X" }) }

            let changedJsons = Json.serialize<SimpleSheet> modified :: List.tail sheetJsons
            let _, decodedAfterEdit, prunedCache = uploadDesign cache changedJsons
            Expect.equal decodedAfterEdit 1 "an edit to one sheet decodes one sheet"

            // and the cache ends holding that design and no more, so it stays bounded by one
            // design however many uploads it has seen
            Expect.equal (Map.count prunedCache) changedJsons.Length "the cache holds exactly the design just uploaded"

            Expect.isFalse
                (prunedCache |> Map.containsKey (List.head sheetJsons))
                "the sheet that was edited away is gone from the cache"
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

        test "SimRead carries a signal wider than one word" {
            // 3cpu is a 16-bit machine, so nothing in it exercises a sample of more than one
            // uint32. A wide register does, and it is the case that used to be REFUSED: the
            // viewer asked for such a wave, was given a reply that silently omitted it, and went
            // on drawing whatever that waveform had shown before.
            let width = 70

            let input = makeComp 1 0 1 (Input1(width, None)) "IN"
            let reg = makeComp 2 1 1 (Register width) "REG"
            let output = makeComp 3 1 0 (Output width) "OUT"

            let ldc =
                makeLdc "wide" None ([ input; reg; output ], [ conn input 0 reg 0; conn reg 0 output 0 ])

            let design = CanvasExtractor.simpleDesignOfLoadedComponents [ ldc ]
            let shimmed = shimDesign design

            let buildReply = Issie.Sidecar.SimSession.build design 250
            Expect.isFalse (buildReply.Contains "error") $"build failed: {buildReply}"
            let epoch = Issie.Sidecar.SimSession.currentEpoch ()

            let localSim =
                let top = shimmed |> List.find (fun l -> l.Name = design.TopSheet)

                match Simulator.startCircuitSimulation 250 design.TopSheet top.CanvasState shimmed with
                | Ok simData -> simData
                | Error e -> failtest $"local build failed: %A{e.ErrType}"

            let u32s (values: int list) =
                values |> List.collect (System.BitConverter.GetBytes >> Array.toList) |> Array.ofList

            // Not symmetric, so a wrong word order shows - and inside 64 bits, because
            // SimSetInputs carries a value as two words and would truncate anything wider. What is
            // being tested here is the WIDTH of the signal, which decides the words per sample
            // whatever the value: 70 bits needs three of them however few are set.
            let value = (1I <<< 63) + (1I <<< 33) + 12345I
            let inputId = localSim.Inputs |> List.map (fun (ComponentId c, _, _) -> c) |> List.head

            let setPayload =
                [ 0; 1; inputId; int (uint32 (value &&& 4294967295I)); int (uint32 ((value >>> 32) &&& 4294967295I)) ]
                |> u32s

            Expect.isFalse
                ((Issie.Sidecar.SimSession.setInputs epoch setPayload).Contains "error")
                "setting a wide input"

            Issie.Sidecar.SimSession.run epoch 3 0 |> ignore

            // read the register's output over three cycles
            let regId = localSim.FastSim.FClockedComps |> Array.head |> fun fc -> fc.fId
            let (ComponentId regCid, regPath) = regId

            let payload =
                [ 0; 1; 3; 1; regCid; 0; List.length regPath ]
                @ (regPath |> List.map (fun (ComponentId p) -> p))
                |> u32s

            match Issie.Sidecar.SimSession.read epoch payload with
            | Error e -> failtest $"SimRead of a {width}-bit signal failed: {e}"
            | Ok reply ->
                let wordsPerSample = int (System.BitConverter.ToUInt32(reply, 8))
                Expect.equal wordsPerSample 3 $"a {width}-bit signal needs three uint32 words a sample"
                Expect.equal (reply.Length) (16 + 4 * 3 * wordsPerSample) "the reply is exactly its stated shape"

                // the same simulation locally, driven the same way, for the values to match
                let fd = NumberHelpers.convertBigintToFastData width value
                FastExtract.changeInput (ComponentId inputId) (SimGraphTypes.IData fd) 0 localSim.FastSim
                FastRun.runFastSimulation None 3 localSim.FastSim |> ignore

                for j in 0 .. 2 do
                    let at = 16 + 4 * (j * wordsPerSample)

                    let wire =
                        (0I, [ wordsPerSample - 1 .. -1 .. 0 ])
                        ||> List.fold (fun v w -> (v <<< 32) + bigint (System.BitConverter.ToUInt32(reply, at + 4 * w)))

                    let local =
                        match FastExtract.extractFastSimulationOutput localSim.FastSim j regId (OutputPortNumber 0) with
                        | SimGraphTypes.IData d -> d.GetBigInt
                        | _ -> failtest "algebraic value in local read"

                    // the high word being non-zero here is what caught SimSetInputs reading a
                    // value word as a signed int - see SimSession.word
                    Expect.equal wire local $"cycle {j} of a {width}-bit signal differs"

            Issie.Sidecar.SimSession.endSession epoch |> ignore
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
            // widths are NOT filtered: a sample is as many words as it needs, and a read of
            // ordinary buses must stay one word per sample while one wide bus widens its own reply
            let itemsWanted =
                localFs.FClockedComps
                |> Array.filter (fun fc ->
                    (match fc.FType with ROM1 _ -> false | _ -> true)
                    && (FastExtract.extractFastSimulationOutput localFs 0 fc.fId (OutputPortNumber 0)
                        |> function SimGraphTypes.IData _ -> true | _ -> false))
                |> Array.truncate 6
                |> Array.toList
                |> List.map (fun fc -> fc.fId)

            let widthOfItem fid =
                match FastExtract.extractFastSimulationOutput localFs 0 fid (OutputPortNumber 0) with
                | SimGraphTypes.IData fd -> fd.Width
                | _ -> failtest "algebraic value in local read"

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
                    let wordsPerSample = int (System.BitConverter.ToUInt32(reply, 8))
                    let widest = itemsWanted |> List.map widthOfItem |> List.max

                    Expect.equal (int (System.BitConverter.ToUInt32(reply, 0))) (List.length itemsWanted) "signal count"
                    Expect.equal (int (System.BitConverter.ToUInt32(reply, 4))) samples "sample count"
                    Expect.equal wordsPerSample ((widest + 31) / 32) "words per sample is the widest signal's"

                    itemsWanted
                    |> List.iteri (fun signalIndex fid ->
                        for j in 0 .. samples - 1 do
                            let cycle = startCycle + j * rep
                            let at = 16 + 4 * ((signalIndex * samples + j) * wordsPerSample)

                            // least significant word first, so the words rebuild the number
                            let wire =
                                (0I, [ wordsPerSample - 1 .. -1 .. 0 ])
                                ||> List.fold (fun v w -> (v <<< 32) + bigint (System.BitConverter.ToUInt32(reply, at + 4 * w)))

                            let local =
                                match FastExtract.extractFastSimulationOutput localFs cycle fid (OutputPortNumber 0) with
                                | SimGraphTypes.IData fd -> fd.GetBigInt
                                | _ -> failtest "algebraic value in local read"

                            Expect.equal wire local $"signal {signalIndex} cycle {cycle} (rep {rep}) differs")

            readSampled (ticks - 10) 1 10
            readSampled 2 3 9
            readSampled (ticks - 1) 1 1

            // and a signal wider than one word, if the design has one, read on its own so that
            // wordsPerSample is genuinely greater than 1
            let wideItems =
                localFs.FClockedComps
                |> Array.filter (fun fc ->
                    match FastExtract.extractFastSimulationOutput localFs 0 fc.fId (OutputPortNumber 0) with
                    | SimGraphTypes.IData fd -> fd.Width > 32
                    | _ -> false)
                |> Array.truncate 1
                |> Array.toList
                |> List.map (fun fc -> fc.fId)

            for (ComponentId cid, path) as fid in wideItems do
                let payload =
                    [ ticks - 3; 1; 3; 1; cid; 0; List.length path ]
                    @ (path |> List.map (fun (ComponentId p) -> p))
                    |> u32s

                match Issie.Sidecar.SimSession.read epoch payload with
                | Error e -> failtest $"SimRead of a wide signal failed: {e}"
                | Ok reply ->
                    let wordsPerSample = int (System.BitConverter.ToUInt32(reply, 8))
                    Expect.isGreaterThan wordsPerSample 1 "a signal over 32 bits needs more than one word"

                    for j in 0 .. 2 do
                        let cycle = ticks - 3 + j
                        let at = 16 + 4 * (j * wordsPerSample)

                        let wire =
                            (0I, [ wordsPerSample - 1 .. -1 .. 0 ])
                            ||> List.fold (fun v w -> (v <<< 32) + bigint (System.BitConverter.ToUInt32(reply, at + 4 * w)))

                        let local =
                            match FastExtract.extractFastSimulationOutput localFs cycle fid (OutputPortNumber 0) with
                            | SimGraphTypes.IData fd -> fd.GetBigInt
                            | _ -> failtest "algebraic value in local read"

                        Expect.equal wire local $"wide signal at cycle {cycle} differs"

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
