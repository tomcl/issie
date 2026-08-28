/// Naming a saved selection in a way that survives being saved.
///
/// A `WaveIndexT` names a signal by component id and access path. Those are facts about one loaded
/// design: ids are reallocated when a project is opened (`Helpers.remapLoadedComponent`), and a
/// saved path's ids belong to OTHER sheets, which the loader's per-sheet mapping cannot see - so
/// what it cannot place became the 0 sentinel (`Helpers.sheetOfJson`). A saved selection named that
/// way was only as durable as the id scheme it was written under, which is why `3cpu/eep1`'s
/// selection came back as component 0 and was dropped.
///
/// What a person would use instead is what this uses: the LABELS of the custom component instances
/// entered from the sheet being simulated, then the component's own label. That survives ids being
/// reallocated, survives the file being written by a different version, and best-effort survives
/// edits elsewhere in the design - which is what a saved selection is for, since it is a
/// convenience rather than a record of anything.
///
/// **A label path names exactly one wave-carrying component.** Labels are unique per sheet and
/// enforced: `CanvasStateAnalyser.checkComponentNamesAreOk` refuses to simulate a sheet with
/// duplicates. The types it exempts - `MergeWires`, `SplitWire`, `BusSelection`, `NotConnected` -
/// carry no waveform at all, and the fifth, `IOLabel`, shares a label deliberately: a labelled net
/// has one driver and every member reads it, so naming the group is naming the signal.
///
/// Compared exactly, not case-insensitively. Issie's duplicate check groups on the label as
/// written, so `abc` and `ABC` are two components and matching them loosely would make a path
/// ambiguous where the design is not.
///
/// Nothing here knows about a simulation: it walks the design's sheets, so it works before one is
/// built - which is when the selection is loaded.
module WavePath

open CommonTypes

/// The components of each sheet, by sheet name. Built once per conversion: a selection is at most
/// a few hundred entries and a design a handful of sheets, so this is small either way.
let private componentsBySheet (ldcs: LoadedComponent list) =
    ldcs |> List.map (fun ldc -> ldc.Name, fst ldc.CanvasState) |> Map.ofList

/// The sheet a custom component instantiates, or None if it is not one.
let private sheetInstantiatedBy (comp: Component) =
    match comp.Type with
    | Custom ct -> Some ct.Name
    | _ -> None

/// The label path of a component named by id and access path, or None where the design does not
/// hold it - a selection made against a design that has since changed.
let pathOfComponent (ldcs: LoadedComponent list) (topSheet: string) ((compId, accessPath): FComponentId) =
    let sheets = componentsBySheet ldcs

    let componentOn sheet (id: ComponentId) =
        Map.tryFind sheet sheets
        |> Option.bind (List.tryFind (fun comp -> comp.Id = id))

    // down the path, one custom component at a time, collecting the label each one is drawn with
    let rec walk sheet labels remaining =
        match remaining with
        | [] -> componentOn sheet compId |> Option.map (fun comp -> List.rev (comp.Label :: labels))
        | id :: rest ->
            componentOn sheet id
            |> Option.bind (fun comp ->
                sheetInstantiatedBy comp |> Option.bind (fun inner -> walk inner (comp.Label :: labels) rest))

    walk topSheet [] accessPath

/// The component a label path names, or None where the design does not hold it - renamed or
/// deleted since the selection was saved, which is a wave that is simply no longer offered.
let componentOfPath (ldcs: LoadedComponent list) (topSheet: string) (labels: string list) =
    let sheets = componentsBySheet ldcs

    let componentOn sheet label =
        Map.tryFind sheet sheets |> Option.bind (List.tryFind (fun comp -> comp.Label = label))

    let rec walk sheet ids labels =
        match labels with
        | [] -> None // a path with no component at the end of it names nothing
        | [ last ] -> componentOn sheet last |> Option.map (fun comp -> comp.Id, List.rev ids)
        | label :: rest ->
            componentOn sheet label
            |> Option.bind (fun comp ->
                sheetInstantiatedBy comp
                |> Option.bind (fun inner -> walk inner (comp.Id :: ids) rest))

    walk topSheet [] labels

/// The saved form of one selected wave.
let pathOfSignal (ldcs: LoadedComponent list) (topSheet: string) (wi: WaveIndexT) : WavePath option =
    pathOfComponent ldcs topSheet wi.Id
    |> Option.map (fun labels ->
        { WPLabels = labels
          WPPortType = wi.PortType
          WPPortNumber = wi.PortNumber })

/// The wave a saved path names.
///
/// `SimArrayIndex` is left at -1: it is a fact about a build, and there is no build here. The wave
/// simulator fills it in when one exists (`WaveSimHelpers.reResolveWave`), which it has to do after
/// every rebuild in any case.
let signalOfPath (ldcs: LoadedComponent list) (topSheet: string) (path: WavePath) : WaveIndexT option =
    componentOfPath ldcs topSheet path.WPLabels
    |> Option.map (fun fId ->
        { SimArrayIndex = DriverIndex -1
          Id = fId
          PortType = path.WPPortType
          PortNumber = path.WPPortNumber })
