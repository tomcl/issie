///Miscellaneous helpers used tby waveform simulator
module WaveSimHelpers
//---------------------------------------------------------------------------------------//
//-----------------------Miscellaneous low=level helper functions------------------------//
//---------------------------------------------------------------------------------------//


open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props
open EEExtensions
open CommonTypes
open ModelType
open ModelHelpers
open SimGraphTypes
open SimTypes
open WaveNames
open WaveSimTypes
open WaveSimStyle


//-----------------------------List & Map utilities to deal with exceptions---------------//

/// Helper function to create Bulma buttons
let button options func label = Button.button (List.append options [ Button.OnClick func ]) [ label ]

/// convenience functions
let isWaveSelected (index: WaveIndexT) (wsModel: WaveSimModel) : bool = List.contains index wsModel.SelectedWaves
let isRamSelected (ramId: FComponentId) (wsModel: WaveSimModel) : bool = Map.containsKey ramId wsModel.SelectedRams


let portBits n = if n < 2 then "" else $"({n-1}:0)"

/// Which group (for selector classification) is a component of this type in?
///
/// A function of the component TYPE alone, which is all it ever needed: it took a whole
/// FastSimulation only to look the type up. Callers pass the type, so that when a port carries
/// its own type there is nothing here left to change - and so that this says nothing about where
/// the simulation ran.
let getCompGroup (compType: ComponentType) =
    match compType with
    | Input1 _ | Output _ | Constant1 _ | Viewer _ | IOLabel | NotConnected ->
        InputOutput
    | Not | GateN _ ->
        Gates
    | BusCompare _ | BusCompare1 _->
        Buses
    | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 | Decode4 ->
        MuxDemux
    | NbitsAdder _ | NbitsAdderNoCin _ | NbitsAdderNoCout _ | NbitsAdderNoCinCout _ | NbitsXor _ | NbitsAnd _ | NbitsNot _ | NbitSpreader _ | NbitsOr _ ->
        Arithmetic
    | Custom _ -> CustomComp
    | DFF | DFFE | Register _ | RegisterE _ |Counter _ |CounterNoEnable _ |CounterNoLoad _ |CounterNoEnableLoad _ ->
        FFRegister
    | AsyncROM1 _ | ROM1 _ | RAM1 _ | AsyncRAM1 _ ->
        Memories
    | BusSelection _ | MergeWires | MergeN _ | SplitWire _ | SplitN _ ->
        failwithf "Bus select, MergeWires, MergeN, SplitWire should not appear"
    | Input _ | Constant _ | AsyncROM _ | ROM _ | RAM _ ->
        failwithf "Legacy component types should not appear"
    | Shift _ ->
        failwithf "Shift is an internal-only component which should never appear on the canvas"

/// The heading over a group of components within one sheet of the wave selector.
///
/// Every case of ComponentGroup, so that adding one is a compile error rather than a row headed
/// "What? Not used!" - which is what the wildcard this replaces would have given. Three of them,
/// WireLabel, Viewers and Component, are not produced by getCompGroup: they are there for grouping
/// components by type rather than by name, which the selector does not currently do.
let groupHeading (compGroup: ComponentGroup): ReactElement =
    let name =
        match compGroup with
        | InputOutput -> "Inputs / Outputs / Labels / Viewers"
        | Viewers -> "Viewers"
        | WireLabel -> "Wire Labels"
        | Buses -> "Buses"
        | Gates -> "Logic Gates"
        | MuxDemux -> "Multiplexers"
        | Arithmetic -> "Arithmetic"
        | FFRegister -> "Flip Flops and Registers"
        | Memories -> "RAMs and ROMs"
        | Component compLabel -> compLabel
        | CustomComp -> "Custom Components"
    str (name.ToUpper())

/// Convert Wave list to list of WaveIndexT
let wavesToIds (waves: Wave list) =
    waves |> List.map (fun wave -> wave.WaveId)

/// The name of the sheet a wave's component sits on, which is how ComponentsById and
/// ConnectionsByPort are both keyed.
/// A component's access path names the custom component instances it sits inside, innermost first,
/// so its head is an instance of the sheet wanted. It is that instance's TYPE which names the
/// sheet: its LABEL is chosen by whoever placed it, and only happens to be the sheet's name until
/// someone renames the instance. Asking the label instead left every wave in a renamed subsheet -
/// REGFILE for reg16x8, say - with no connections found, and so no wires highlighted on hover.
let sheetOfWave (fs: FastSimulation) (wave: Wave) : string option =
    match snd wave.WaveId.Id with
    | [] ->
        Some fs.SimulatedTopSheet
    | cid :: outer ->
        fs.ComponentOf(cid, outer)
        |> Option.bind (fun instance ->
            match instance.FType with
            | Custom cc -> Some cc.Name
            | _ -> None)

/// Work out a SheetPort from a wave, if one exists
/// SheetPorts may not exist in some corner cases when simulation is ending etc.
///
/// Every step is best effort and gives nothing rather than throwing. The link between a wave and
/// the schematic is only as good as the schematic still being the one that was simulated, and this
/// runs from the mouse handlers on a waveform's name - where an exception is not caught by
/// showWaveforms' error boundary, which has already returned by then.
let waveToSheetPort fs (wave:Wave) =
    let wi = wave.WaveId
    sheetOfWave fs wave
    |> Option.bind (fun sheet ->
        fs.ComponentsById
        |> Map.tryFind sheet
        |> Option.bind (Map.tryFind (fst wi.Id))
        |> Option.map (fun comp ->
            // Which set of ports the wave's port number indexes. A component with neither - which
            // is what the last case is - has nothing to connect and so nothing to highlight.
            let ports =
                match wi.PortType, comp.InputPorts.Length > 0, comp.OutputPorts.Length > 0 with
                | PortType.Input, true, _ | PortType.Output, true, false -> comp.InputPorts
                | PortType.Output ,_, true | PortType.Input, false, true -> comp.OutputPorts
                | _ -> []
            List.tryItem wi.PortNumber ports
            |> Option.map (fun port -> [{ Sheet = sheet; PortOnComp = port }])
            |> Option.defaultValue []))
    |> Option.defaultValue []


/// given a SheetPort, get all directly connected SheetPorts
let connectedPorts (fs: FastSimulation) sheetPort =
    let compMap = fs.ComponentsById
    let portMap = fs.ConnectionsByPort
    let name = sheetPort.Sheet
    Map.tryFind sheetPort portMap
    |> Option.defaultValue []
    |> List.collect (fun conn -> 
        [conn.Source; conn.Target]
        |> List.map (Simulator.portSheetPort compMap[name] name)
        |> List.collect (function | None -> [] | Some sheetPort -> [sheetPort]))

/// given an IOlabel port, get all same-name IOLabels on the same sheet
let connectedIOs (fs: FastSimulation) (sp: SheetPort) =
    let comps = fs.ComponentsById[sp.Sheet]
    match comps[sp.PortOnComp.HostId] with
    | {Type = IOLabel} as comp -> 
        let sheet = sp.Sheet
        comps
        |> Map.valuesL
        |> List.collect (
            function | {Type=IOLabel; Label = label} as comp1 when label = comp.Label -> 
                        (
                            (if comp1.OutputPorts.Length > 0 then [{Sheet = sheet; PortOnComp = comp1.OutputPorts[0]}] else [])@
                            (if comp1.InputPorts.Length > 0 then [{Sheet = sheet; PortOnComp = comp1.InputPorts[0]}] else [])@
                            [sp]
                        )
                     | _ -> 
                        [])

    | _ -> [sp]

/// Given a list of ports, get all ports connected to any port in it.
/// used by connsOfWave
let rec allConnectedPorts (fs: FastSimulation) (sp:SheetPort list) =
    let newSP =
        sp
        |> List.collect (connectedIOs fs)
        |> List.distinct
        |> List.collect (connectedPorts fs)
        |> List.distinct
    match newSP.Length - sp.Length with
    | 0 ->
        newSP
    | n when n >= 0 -> 
        allConnectedPorts fs newSP
    | _ -> 
        newSP

/// Get all the connections of a given wave signal
let connsOfWave (fs:FastSimulation) (wave:Wave) =
    wave
    |> waveToSheetPort fs
    |> allConnectedPorts fs
    |> List.collect (fun sp -> match Map.tryFind sp fs.ConnectionsByPort with | None -> [] | Some conns -> conns)
    |> List.map (fun conn -> conn.Id)
    |> List.distinct


/// button driving a popup with a page of info about waveform simulator
let waveInfoButton (name:string) (dispatch: Msg -> Unit) : ReactElement =
    button 
        (topHalfButtonProps IsInfo "RefreshButton" false)
        (fun _ -> (UIPopups.viewWaveInfoPopup dispatch name))
        (str name)

/// remove highlights on components generated by hovering on waveform labels
let removeHighlights (model:Model) dispatch =
    if model.Sheet.SelectedWires.Length > 0 || model.Sheet.SelectedComponents.Length > 0 then
        dispatch <| Sheet (DrawModelType.SheetT.ResetSelection) // Remove highlights.

type WaveSimButtonOptions = {
    IsDirty: bool
    IsRunning: bool
    IsErrored: bool
    StartEndMsg: string
    StartEndColor: IColor
    }

/// end the current simulation
let endButtonAction canvasState model dispatch ev =
    removeHighlights model dispatch
    dispatch <| EndWaveSim

/// Return info about current state of waveform simulator
/// which is used to switch buttons on/off etc.
///
/// Runs on every render of the wave sim panel, so nothing here works out anything about the design
/// that the model has already been told.
let getWaveSimButtonOptions (canv: CanvasState) (model:Model) (ws:WaveSimModel) dispatch : WaveSimButtonOptions =
    let fs = Simulator.getFastSim()
    let simExists = model.WaveSimSheet <> Some "" && model.WaveSimSheet <> None
    let success = (ws.State = Success || ws.State=Loading)

    // Whether the design builds is read from the model rather than worked out here. It used to run
    // the whole of validateCircuitSimulation - every sheet of the design checked and its widths
    // inferred - once per render, to choose a word and a colour. See ModelType.CircuitCheck, and
    // StepSimulationTop, which reads the same verdict for the same reason.
    //
    // The verdict is about the sheet that is OPEN. That is the sheet this is about whenever the
    // answer changes anything: a waveform simulation running on some other sheet is running, and
    // says "End Simulation", whether the design still builds or not.
    if circuitCheckIsNeeded model canv then
        dispatch RequestCircuitCheck
    /// A design not yet checked reads as buildable, as in the step simulator: pressing the button
    /// does the real build and reports any error then, so an optimistic colour costs a click.
    let hasSimErr =
        match model.CircuitCheck.Verdict with
        | Some (Error _, _) -> true
        | _ -> false

    let errored =
        match hasSimErr, ws.State with
        | true, _ -> true
        | false, NonSequential -> true
        | false, _ -> false

    let running = (success || errored) && simExists
        
    let isDirty = 
        simExists &&
        running && 
        not <| FastExtract.compareLoadedStates fs canv model.CurrentProj &&
        model.UIState = None &&
        not model.IsLoading
    
    
    let startEndMsg, startEndColor =
        match running, errored with
        | false, true -> "View Problems", IsWarning
        | false, false -> "Start Simulation" + ModelHelpers.simulatorLabel model, IsSuccess
        | true, _ -> "EndSimulation" + ModelHelpers.simulatorLabel model, IsDanger


    
    {
        IsDirty =  isDirty
        IsRunning = running
        IsErrored = errored
        StartEndMsg = startEndMsg
        StartEndColor = startEndColor
    } 



    

// ---------------------------------------------------------------------------------------------
// What a waveform is called, and the Wave record that carries it
// ---------------------------------------------------------------------------------------------
//
// This was the top third of WaveSimSelect, which compiles after the selection dialog that needs
// it: the dialog now describes the waves of the instances it is drawing rather than looking them
// up in a map of every wave in the simulation, so it has to be able to make a Wave. Nothing here
// is about selection - it is naming, which is what this module is for, and the pieces it is built
// from (camelCaseDottedWords, bitLimsString, portBits) are just above.


/// The Wave record for one selected wave: its name, its width, and where its data lies.
///
/// Built from the ports of the wave's own INSTANCE - `PortView.ofInstance`, which costs one sheet
/// - and from the design, which supplies everything about where the wave sits. What the simulation
/// is asked for is the two things only it knows: the port's width, and its driver.
///
/// A wave whose port the simulation no longer offers gets a record saying so rather than throwing.
/// It is what a selection saved against an older version of the design resolves to, and
/// reconcileWaves drops it moments later; failing here instead took the whole viewer down.
let makeWave (ws: WaveSimModel) (fastSim: FastSimulation) (wi: WaveIndexT) : Wave =
    let compId, ap = wi.Id
    let instance = InstancePath ap
    let view = PortView.ofInstanceCached fastSim instance

    let port =
        view.ViewPorts
        |> List.tryFind (fun p -> p.PortComp = compId && p.PortIs = wi.PortType && p.PortNum = wi.PortNumber)

    match port with
    | None ->
        Log.warn $"no port for %A{wi.PortType}[{wi.PortNumber}] of a component in {view.ViewSheet}"

        { WaveId = wi
          SubSheet = []
          DisplayName = "?"
          ViewerDisplayName = "?"
          CompLabel = "?"
          PortLabel = "?"
          Width = 0
          DriverIndex = wi.SimArrayIndex
          SheetId = instance }
    | Some port ->
        if port.PortWidth = 0 then
            Log.warn $"zero-width driver for {port.PortCompLabel}.%A{wi.PortType}[{wi.PortNumber}]"

        { WaveId = wi
          SubSheet = view.ViewSubSheet
          DisplayName = port.PortDisplayName
          ViewerDisplayName = WaveNames.camelCaseDottedWords view.ViewSheet + "." + port.PortDisplayName
          CompLabel = port.PortCompLabel
          PortLabel = port.PortLabel
          Width = port.PortWidth
          DriverIndex = port.PortDriver
          SheetId = instance }

/// The sheets of the simulated design that came from a component library.
///
/// Read off the design the simulation was built from, so nothing has to thread it down from the
/// project. It was recomputed from Model.CurrentProj on every refresh, which is every tick of a
/// checkbox.
let librarySheetsOf: FastSimulation -> Set<string> =
    Helpers.memoizeByIdentity (fun fs ->
        fs.SimulatedCanvasState
        |> List.filter ComponentLibraries.isLibrarySheet
        |> List.map (fun ldc -> ldc.Name)
        |> Set.ofList)

/// True when the component sits inside an instance of a library sheet.
/// A library component is opaque: its innards are no more offered here than its sheet is offered
/// in the Sheets menu. The instance's own ports are unaffected - they belong to the sheet it was
/// placed on, and appear like any other custom component's, as sheet.L<n>_Comp1.port.
/// AccessPath is the chain of custom component instances the component sits within, so the test
/// is whether any of them is an instance of a library sheet.
///
/// The selector does not need this: the hierarchy it draws already makes a library component
/// opaque, so no instance inside one is ever on show. It is for the two places that reach a
/// component without going through the hierarchy - the default selection, which looks for Viewers
/// anywhere in the design, and the schematic's right-click menu.
let isInsideLibraryComponent (fs: FastSimulation) (InstancePath ap) =
    let librarySheets = librarySheetsOf fs

    // every sheet entered on the way down, which is the sheet of each non-empty TAIL of the path -
    // the path being innermost first, its tails are the instances it sits inside
    let rec entersLibrary ap =
        match ap with
        | [] -> false
        | _ :: outer ->
            Set.contains (fs.Design.SheetOfInstance(InstancePath ap)) librarySheets || entersLibrary outer

    not (Set.isEmpty librarySheets) && entersLibrary ap

/// The ports of one elaborated component that carry a waveform, as wave indices into the
/// simulation as it is now. Read off the instance's port view - the one derivation of what
/// carries a wave - rather than deciding again from a FastComponent.
let waveIndicesOfFComp (fs: FastSimulation) ((compId, ap) as fId: FComponentId) : WaveIndexT list =
    (PortView.ofInstanceCached fs (InstancePath ap)).ViewPorts
    |> List.filter (fun p -> p.PortComp = compId)
    |> List.map (PortView.waveIndexOf (InstancePath ap))

/// The wave one index names, in the simulation as it is NOW - or None if that simulation does not
/// offer it any more.
///
/// A WaveIndexT says where its data lies (SimArrayIndex) as well as which port it is. The first is
/// true only of the build it came from, so a selection made before a rebuild has to be resolved
/// against the new one. The other three fields are the stable name and are what this looks up.
///
/// It used to be answered by indexing a map of every wave in the simulation, built by inverting
/// that map's 208,896 keys. The component knows where its own ports are, so a selection of a
/// hundred waves is a hundred lookups.
/// None drops the wave. That is said only by an instance that HAS been described and does not
/// offer the port - a component renamed or deleted. An instance not yet described (the sidecar
/// has not answered for it) keeps the wave, unresolved: SimArrayIndex = DriverIndex -1, which nothing draws
/// and nothing fetches, until the slice lands and the next refresh resolves it - or drops it
/// then, knowing.
let reResolveWave (fs: FastSimulation) (wi: WaveIndexT) : WaveIndexT option =
    let compId, ap = wi.Id

    match PortView.tryOfInstanceCached fs (InstancePath ap) with
    | None -> Some { wi with SimArrayIndex = DriverIndex -1 }
    | Some view ->
        view.ViewPorts
        |> List.tryFind (fun p -> p.PortComp = compId && p.PortIs = wi.PortType && p.PortNum = wi.PortNumber)
        |> Option.map (fun port -> { wi with SimArrayIndex = port.PortArrayIndex })

/// Build a wave map in a scope that holds nothing.
///
/// This exists because a Fable map carries the comparer it was built with, and Fable creates that
/// comparer - an object holding a Compare closure - at the construction site. V8 gives all the
/// closures of a function one shared context holding every variable any of them captures, so a
/// comparer made where a FastSimulation is in scope shares a context with it - and the map then
/// pins the whole simulation, step arrays and all, for as long as the map lives. WaveDetails lives
/// in the WaveSimModel, which outlives its simulation by design, and a copy of it sits in every
/// model generation React retains - which is how an ended simulation's memory survived every
/// explicit release. Built here, where no closure can capture anything, the comparer retains
/// nothing. Do not inline this into a caller that has a simulation in scope.
let makeWaveMap (pairs: (WaveIndexT * Wave) list) : Map<WaveIndexT, Wave> = Map.ofList pairs
