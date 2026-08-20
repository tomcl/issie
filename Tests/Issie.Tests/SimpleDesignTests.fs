/// The Simple wire types (CommonTypes.SimpleDesign) are the contract for sending a design to
/// the dotnet sidecar, and the id reducer is what makes their integer ids possible. Three things
/// are pinned here:
///   - the exact JSON encoding the renderer sends (the vendored SimpleJson serializer, which
///     compiles under .NET too) deserialises on the .NET side via SimpleJsonDotNet;
///   - a SimpleDesign carries ALL the electrical information of a design, proven by shimming it
///     back into skeleton LoadedComponents and simulating: the shim must behave identically to
///     the original, cycle for cycle, on the golden-model harness;
///   - the reducer's properties: dense positive integer ids, project-wide uniqueness (so the
///     duplicate-id repair at project open can never fire), idempotence, and consistent
///     rewriting of parameter slots and cross-sheet waveform references.
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
            makeComp (string sc.CompId) nIn nOut sc.TypeS sc.Label)

    let compById = comps |> List.map (fun comp -> int comp.Id, comp) |> Map.ofList

    let conns =
        sheet.Connections
        |> List.map (fun sc ->
            { conn compById[sc.SrcComp] sc.SrcPort compById[sc.DestComp] sc.DestPort with
                Id = string sc.ConnId })

    let paramDefs =
        if Map.isEmpty sheet.DefaultBindings && Map.isEmpty sheet.ParamSlots then
            None
        else
            Some { DefaultBindings = sheet.DefaultBindings; ParamSlots = sheet.ParamSlots }

    makeLdc sheet.SheetName paramDefs (comps, conns)

let private shimDesign (design: SimpleDesign) : LoadedComponent list =
    List.map shimSheet design.Sheets

/// Reduce, convert, shim - the whole journey a design makes, minus the JSON.
let private convertProject (projectName: string) =
    let reduced = RegenerateIds.reduceLoadedComponents (loadProject projectName)
    let design = CanvasExtractor.simpleDesignOfLoadedComponents reduced
    reduced, design, shimDesign design

let private allCanvasIds (ldc: LoadedComponent) =
    let comps, conns = ldc.CanvasState
    let compIds = comps |> List.map (fun (comp: Component) -> comp.Id)

    let portIds =
        comps
        |> List.collect (fun comp -> comp.InputPorts @ comp.OutputPorts)
        |> List.map (fun port -> port.Id)

    let connIds = conns |> List.map (fun (conn: Connection) -> conn.Id)
    compIds @ portIds @ connIds

let private isReducedForm (s: string) =
    s.Length >= 1 && s.Length <= 6 && s[0] <> '0' && String.forall System.Char.IsDigit s

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
                  [ { CompId = "2"; CompSlot = CustomCompParam "W" },
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

        test "reducer produces dense positive integer ids, unique across the project" {
            let reduced = RegenerateIds.reduceLoadedComponents (loadProject "3cpu")
            let ids = reduced |> List.collect allCanvasIds

            ids
            |> List.iter (fun id ->
                Expect.isTrue (isReducedForm id) $"id '{id}' is not a reduced integer id")

            Expect.equal (List.length (List.distinct ids)) (List.length ids)
                "reduced ids collide across the project"

            // dense: n ids drawn from 1..n
            let asInts = ids |> List.map int
            Expect.isLessThanOrEqual (List.max asInts) (List.length ids)
                "ids are not dense: the largest exceeds the count"
        }

        test "reduction is idempotent and satisfies the duplicate-id check" {
            let once = RegenerateIds.reduceLoadedComponents (loadProject "3cpu")
            let twice = RegenerateIds.reduceLoadedComponents once
            Expect.equal twice once "a second reduction changed an already-reduced design"

            let _, corrected = RegenerateIds.correctDuplicateIds once
            Expect.isEmpty corrected "the duplicate-id repair fired on a reduced project"
        }

        test "reduction rewrites parameter slots and cross-sheet waveform references in step" {
            // Two synthetic sheets: "child" has a slot keyed by its own component's uuid, and
            // "parent" holds a saved waveform selection whose access path names that component.
            let childCompId = DrawHelpers.uuid ()
            let childComp = makeComp childCompId 1 1 (NbitsNot 3) "N1"
            let slots: ComponentSlotExpr =
                Map.ofList
                    [ { CompId = childCompId; CompSlot = Buswidth },
                      { Expression = PInt 3I; Constraints = [] } ]
            let child =
                makeLdc "child" (Some { DefaultBindings = Map.empty; ParamSlots = slots })
                    ([ childComp ], [])

            let parentCompId = DrawHelpers.uuid ()
            let parentComp = makeComp parentCompId 0 1 (Input1(3, None)) "I0"
            let waveInfo: SavedWaveInfo =
                { SelectedWaves =
                    Some
                        [ { SimArrayIndex = 0
                            Id = ComponentId childCompId, [ ComponentId parentCompId ]
                            PortType = PortType.Output
                            PortNumber = 0 } ]
                  Radix = None
                  WaveformColumnWidth = None
                  SelectedRams = Some(Map.ofList [ ComponentId childCompId, "ram" ])
                  SelectedFRams = None
                  WSConfig = None
                  ClkWidth = None
                  Cursor = None
                  LastClk = None
                  DisplayedPortIds = None }
            let parent =
                { makeLdc "parent" None ([ parentComp ], []) with WaveInfo = Some waveInfo }

            match RegenerateIds.reduceLoadedComponents [ child; parent ] with
            | [ child'; parent' ] ->
                let childComp' = (fst child'.CanvasState) |> List.exactlyOne
                let parentComp' = (fst parent'.CanvasState) |> List.exactlyOne

                let slotIds =
                    child'.LCParameterSlots
                    |> Option.map (fun defs -> defs.ParamSlots |> Map.toList |> List.map (fun (slot, _) -> slot.CompId))
                    |> Option.defaultValue []
                Expect.equal slotIds [ childComp'.Id ] "the parameter slot does not follow its component's new id"

                match parent'.WaveInfo with
                | Some { SelectedWaves = Some [ wave ]; SelectedRams = Some rams } ->
                    let (ComponentId waveComp), path = wave.Id
                    Expect.equal waveComp childComp'.Id "the waveform selection does not follow the other sheet's component"
                    Expect.equal path [ ComponentId parentComp'.Id ] "the waveform access path does not follow its component"
                    Expect.isTrue (Map.containsKey (ComponentId childComp'.Id) rams) "the RAM selection does not follow its component"
                | _ -> failtest "waveform info lost in reduction"
            | _ -> failtest "reduction changed the number of sheets"
        }

        test "allocator grows past its initial size" {
            // more ids than the initial 10,000-entry array: 4,000 comps at 3 ids each
            let comps = List.init 4000 (fun i -> makeComp (DrawHelpers.uuid ()) 1 1 (NbitsNot 1) $"N{i}")
            let ldc = makeLdc "big" None (comps, [])

            let reduced = RegenerateIds.reduceLoadedComponents [ ldc ]
            let ids = reduced |> List.collect allCanvasIds
            Expect.equal (List.length (List.distinct ids)) 12000 "wrong id count after growth"
            ids |> List.iter (fun id -> Expect.isTrue (isReducedForm id) $"id '{id}' is not reduced")
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
                let reduced, _, shimmed = convertProject project

                for ldc in reduced do
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
                            Expect.equal (signatureIn shimmed) (signatureIn reduced)
                                $"{project}/{ldc.Name}: instance of {custom.Name} resolves differently after the shim"
                        | _ -> ()
        }

        test "shimmed designs simulate identically to their originals" {
            // the same (project, top, ticks) triples the golden-model tests pin
            for project, top, ticks in [ "1fulladder", "fulladd", 8; "adder4", "fa4", 8; "3cpu", "eep1", 30 ] do
                let reduced, design, shimmed = convertProject project
                Expect.equal design.TopSheet top $"{project}: converter picked the wrong top sheet"

                let original = GoldenModel.runGoldenLdcs reduced top ticks
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

        test "reduced ids survive a save and reload round trip" {
            let reduced = RegenerateIds.reduceLoadedComponents (loadProject "3cpu")
            let ldc = reduced |> List.find (fun l -> l.Name = "eep1")

            match JsonHelpers.stateToJsonString (ldc.CanvasState, ldc.WaveInfo, None) with
            | Error e -> failtest $"save failed: {e}"
            | Ok json ->
                match JsonHelpers.jsonStringToState json with
                | Error e -> failtest $"reload failed: {e}"
                | Ok saved ->
                    let savedIds =
                        saved.getCanvas
                        |> fst
                        |> List.map (fun (comp: JSONComponent.Component) -> comp.Id)
                        |> List.sort
                    let originalIds =
                        fst ldc.CanvasState |> List.map (fun comp -> comp.Id) |> List.sort
                    Expect.equal savedIds originalIds "component ids changed across save and reload"
        }
    ]
