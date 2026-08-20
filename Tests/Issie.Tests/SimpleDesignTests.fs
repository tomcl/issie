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

// ---------------------------------------------------------------------------------------------
// The shim: SimpleDesign back to skeleton LoadedComponents that the existing (dual-compiled)
// simulation creation accepts. Test scaffolding only - the real .NET simulator will have its
// own types; this exists to prove the wire format is sufficient, not to prescribe anything.
// ---------------------------------------------------------------------------------------------

/// Port counts for a component type: custom components from their own label lists, everything
/// else from the same table symbol creation uses.
let private portCounts (typeS: ComponentType) (label: string) =
    match typeS with
    | Custom custom -> custom.InputLabels.Length, custom.OutputLabels.Length
    | t ->
        let nIn, nOut, _, _ = Symbol.getComponentProperties t label
        nIn, nOut

let private shimSheet (sheet: SimpleSheet) : LoadedComponent =
    let comps =
        sheet.Components
        |> List.map (fun sc ->
            let nIn, nOut = portCounts sc.TypeS sc.Label
            makeComp sc.CompId nIn nOut sc.TypeS sc.Label)

    let compById = comps |> List.map (fun comp -> comp.Id, comp) |> Map.ofList

    let conns =
        sheet.Connections
        |> List.map (fun sc ->
            { conn compById[sc.SrcComp] sc.SrcPort compById[sc.DestComp] sc.DestPort with
                Id = sc.ConnId })

    let paramDefs =
        if Map.isEmpty sheet.DefaultBindings && Map.isEmpty sheet.ParamSlots then
            None
        else
            Some { DefaultBindings = sheet.DefaultBindings; ParamSlots = sheet.ParamSlots }

    makeLdc sheet.SheetName paramDefs (comps, conns)

let private shimDesign (design: SimpleDesign) : LoadedComponent list =
    List.map shimSheet design.Sheets

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
