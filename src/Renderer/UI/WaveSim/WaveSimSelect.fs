module WaveSimSelect

//---------------------------------------------------------------------------------------//
//-------------Waveform Selection Popup and RAM Selection Popup--------------------------//
//---------------------------------------------------------------------------------------//

// Functions to make modal popups that allows waveforms and RAMs
// to be selected or deselected for display in the waveform simulator.


// TODO: should RAM selection go to separate module or is it too small for that?

open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props

open CommonTypes
open ModelType
open WaveSimStyle
open WaveNames
open WaveSimHelpers
open SimGraphTypes
open SimTypes
open DiagramStyle
open UIPopups
open MenuHelpers
open TopMenuView
open WaveSimSelectHelpers

//--------------------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------//
//----------------------------Miscellaneous subfunctions for Wave Selection-------------------------------//
//--------------------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------//




/// Button to activate wave selection modal
let selectWavesButton (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    // WaveDetails holds the SELECTED waves, so it says nothing about whether there is anything to
    // select. The simulation does.
    let hasWaves = wsModel.State = Success && not (Array.isEmpty (Simulator.getFastSim()).WaveIndex)
    let props, buttonFunc =
        if hasWaves then
            selectWavesButtonProps "selectButton" true, (fun _ -> dispatch <| UpdateWSModel (fun ws -> {ws with WaveModalActive = true}))
        else selectWavesButtonPropsLight "selectButton", (fun _ -> ())
    button 
        props
        buttonFunc
        (str "Select Waves")



//--------------------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------//
//-------------------------------------RAM Selection from Wave Simulator----------------------------------//
//--------------------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------//

/// Button to activate RAM selection modal.
let selectRamButton (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let ramCount = List.length wsModel.RamComps
    let props, buttonFunc =
        if ramCount > 0 && wsModel.State=Success then
            selectRamButtonProps "selectRamButton", (fun _ -> dispatch <| UpdateWSModel (fun ws -> {ws with RamModalActive = true}))
        else selectRamButtonPropsLight "selectRamButton", (fun _ -> ())
    button 
        props
        buttonFunc
        (str "Select RAM")

/// Toggle if a RAM's contents is selected for viewing.
/// The selection is read from the model the update is applied to, not from the one this element
/// was rendered with, so that a change made between the two is not undone here.
let toggleRamSelection (ramId: FComponentId) (ramLabel: string) dispatch =
    dispatch <| UpdateWSModel (fun ws ->
        let selectedRams =
            if isRamSelected ramId ws then
                Map.remove ramId ws.SelectedRams
            else
                Map.add ramId ramLabel ws.SelectedRams
        {ws with SelectedRams = selectedRams})

/// Modal that, when active, allows users to select RAMs to view their contents.
let selectRamModal (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
        let fs = Simulator.getFastSim()
        let ramRows (ramId: FComponentId) : ReactElement =
            match Map.tryFind ramId fs.FComps with
            | Some ram ->
                tr [] [
                    td []
                        [ Checkbox.checkbox []
                            [ Checkbox.input [
                                Props (checkboxInputProps @ [
                                    Checked <| isRamSelected ram.fId wsModel
                                    OnChange (fun _ -> toggleRamSelection ram.fId ram.FullName dispatch)
                                ])
                            ] ]
                        ]
                    td [] [ label [ ramRowStyle ] [ str ram.FullName ] ]
                ]
            | None -> tr [] [td [] []]
        Modal.modal [
            Modal.IsActive wsModel.RamModalActive
            Modal.Props [Style [ZIndex 20000]]
        ] [
            Modal.background [
                Props [
                    OnClick (fun _ -> dispatch <| UpdateWSModel (fun ws -> {ws with RamModalActive = false}))
                ]
            ] []
            Modal.Card.card [Props [Style [Width 800]]] [
                Modal.Card.head [] [
                    Modal.Card.title [] [
                        Level.level [] [
                            Level.left [] [ str "Select RAM" ]
                            Level.right [] [
                                Delete.delete [
                                    Delete.Option.Size IsMedium
                                    Delete.Option.OnClick (fun _ -> dispatch <| UpdateWSModel (fun ws -> {ws with RamModalActive = false}))
                                ] []
                            ]
                        ]
                    ]
                ]
                Modal.Card.body [] [
                    str "Select ROM or asynchronous RAM components to view their contents in any clock cycle. "
                    str "Note that synchronous RAM components cannot currently be viewed in the waveform simulator. "
                    br []
                    br []
                    str "On a write, the corresponding location will be "; colorSpan "red" "highlighted in red during the clock cycle in which the written value is first output.";
                    str " On a read, the corresponding location will be "; colorSpan "blue" "highlighted in blue.";
                    br [] ; br []
                    str "The RAM display has two modes: "; bSpan "sparse display"; str " and "; bSpan "windowed display. ";
                    br []; str "Type in the"; iSpan " Window start"; str " box to set the locations viewed in a window. Leave it blank for sparse display.";
                    br []; br [] 
                    str "If the RAM has too many non-zero locations to display all at once, the windowed display will be used."
                    hr []
                    Table.table [] [
                        tbody []
                            (List.map ramRows wsModel.RamComps)
                    ]
                ]

                Modal.Card.foot [] []
            ]
        ]

//--------------------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------//
//---------------------Waveform Selection for One Component Picked on the Schematic-----------------------//
//--------------------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------//

// The schematic's right-click menu offers the ports of the component clicked on. That component is
// one on the canvas, of which the simulation may hold more than one copy: a sheet instantiated
// twice holds two of everything in it. Picking between those copies from the canvas would be
// guesswork - the symbol clicked on is not any one of them - so the menu is offered only when there
// is exactly one, and the wave selector is what serves the rest.

/// The copies of each canvas component the simulation holds, by the id that component has on the
/// canvas it was drawn on.
///
/// The copies of one canvas component that the simulation holds, in no particular order - or, when
/// there are several, how many rather than which.
///
/// Everything asking this wants to know whether there is exactly one, because a component in a
/// sheet placed twice has no single wave to offer. So it is answered from the DESIGN: which sheet
/// the component was drawn on, and how many instances that sheet has. Both come off the sheet
/// graph, which is the size of what somebody drew.
///
/// It used to be an index of every component in the simulation grouped by the canvas component it
/// is a copy of. main6 of largeTest holds about 480,000 of them, and the probe below asks on every
/// render while the mouse rests on a wire.
///
/// Not every copy has waves: an Input or Output inside a subsheet is simulated by the port of the
/// instance holding it and gets no step array of its own, which is the case waveOfInstancePort
/// exists for. So a component with no waves is still a copy, which is why this counts instances of
/// its sheet rather than waves.
/// Worked out once per simulation, not once per render: the probe asks on every frame while the
/// mouse rests on a wire, and while each of these questions is now the size of the design, the
/// design is not nothing. Keyed on the simulation, which is rebuilt rather than mutated, so a new
/// one is the signal that this is stale. Emptied when a simulation ends - see
/// Helpers.clearIdentityMemos.
let private canvasCompCopies: FastSimulation -> (ComponentId -> Result<FComponentId, int>) =
    Helpers.memoizeByIdentity (fun (fs: FastSimulation) ->
        let counts = fs.Design.SheetInstanceCounts

        let sheetOfComp =
            fs.Design.DesignComponentsById
            |> Map.toList
            |> List.collect (fun (sheet, comps) -> comps |> Map.toList |> List.map (fun (cid, _) -> cid, sheet))
            |> Map.ofList

        let soleInstance =
            counts
            |> Map.toList
            |> List.choose (fun (sheet, n) ->
                if n = 1 then
                    fs.Design.SoleInstanceOfSheet sheet |> Option.map (fun path -> sheet, path)
                else
                    None)
            |> Map.ofList

        fun compId ->
            match Map.tryFind compId sheetOfComp with
            | None -> Error 0
            | Some sheet ->
                match Map.tryFind sheet soleInstance with
                | Some(InstancePath ap) -> Ok(compId, ap)
                | None -> Error(Map.tryFind sheet counts |> Option.defaultValue 0))

let private copiesOfCanvasComp (fs: FastSimulation) (compId: ComponentId) : Result<FComponentId, int> =
    canvasCompCopies fs compId

/// The custom component instance a component sits directly inside, if any.
/// An access path is ordered from the root of the simulation, so its last element is the instance
/// the component is immediately within.
let enclosingInstance (accessPath: ComponentId list) : FComponentId option =
    match accessPath with
    | [] -> None
    | path -> Some (path[path.Length - 1], path[0 .. path.Length - 2])

/// An Input or Output inside a subsheet drives no wave of its own: the signal belongs to the port
/// of the custom component instance that the sheet sits in, and is named after that port. Return
/// that port's wave, so the menu offers these components rather than passing over them.
/// FastCreate.linkFastCustomComponentsToDriverArrays draws the same correspondence when it links
/// the two sets of data arrays together; this follows it in the opposite direction.
let private waveOfInstancePort
        (fs: FastSimulation)
        (ws: WaveSimModel)
        (fc: FastComponent)
            : Wave list =
    let portOfLabel (labels: (string * int) list) =
        labels |> List.tryFindIndex (fun (label, _) -> label = fc.FLabel)
    match fc.FType, enclosingInstance (snd fc.fId) with
    | (Input1 _ | Output _), Some instanceId ->
        match Map.tryFind instanceId fs.FCustomComps with
        | Some { FType = Custom cc } ->
            let portType, portNum =
                match fc.FType with
                | Input1 _ -> PortType.Input, portOfLabel cc.InputLabels
                | _ -> PortType.Output, portOfLabel cc.OutputLabels
            portNum
            |> Option.bind (fun pNum ->
                waveIndicesOfFComp fs instanceId
                |> List.tryFind (fun wi -> wi.PortType = portType && wi.PortNumber = pNum)
                |> Option.map (makeWave ws fs))
            |> Option.toList
        | _ -> []
    | _ -> []

/// The waves to offer for one component on the canvas, and the number of copies of that component
/// the simulation holds. A component in a sheet instantiated more than once has one copy per
/// instantiation, and none of them is offered.
let wavesOfComponent
        (fs: FastSimulation)
        (ws: WaveSimModel)
        (compId: ComponentId)
            : Wave list * int =
    match copiesOfCanvasComp fs compId with
    | Error copies -> [], copies
    // the innards of a library component are not offered here any more than they are in the
    // selector, whose hierarchy makes one opaque
    | Ok((_, ap) as fId) when isInsideLibraryComponent fs (InstancePath ap) -> [], 1
    | Ok fId ->
        let waves = waveIndicesOfFComp fs fId |> List.map (makeWave ws fs)

        match waves, Map.tryFind fId fs.WaveComps with
        | [], Some fc -> waveOfInstancePort fs ws fc, 1
        | _ -> waves, 1

/// The label the simulation gives one component on the canvas, when it holds exactly one of it.
let private simLabelOfComponent (fs: FastSimulation) (compId: ComponentId) : string option =
    match copiesOfCanvasComp fs compId with
    | Error _ -> None
    | Ok fId -> Map.tryFind fId fs.WaveComps |> Option.map (fun fc -> fc.FLabel)

/// The waves to offer on the schematic's right-click menu for the component clicked on: none
/// unless a wave simulation is running and holds exactly one copy of that component.
let compWavesToOffer (model: Model) (compId: ComponentId) : Wave list =
    let ws = ModelHelpers.getWSModel model
    match model.WaveSimSheet, ws.State with
    | Some sheet, Success when sheet <> "" ->
        match wavesOfComponent (Simulator.getFastSim()) ws compId with
        | waves, 1 -> waves
        | _ -> []
    | _ -> []

//--------------------------------------------------------------------------------------------------------//
//---------------------------Reading one value off the schematic (the probe)------------------------------//
//--------------------------------------------------------------------------------------------------------//

// Both simulators can answer this, and neither needs anything built for it: every FastSimulation
// carries WaveIndex, WaveComps and Drivers, because buildFastSimulation ends in
// addWavesToFastSimulation whichever simulator asked for it. So the whole probe is two map lookups
// and an array read, and the step simulator and the waveform simulator differ only in which
// simulation and which cycle are handed in.

/// The signal a wire of the open sheet carries, as an index into a simulation's waves.
///
/// A wire is driven by one output port, so the wire's value is that port's. Which copy of the port
/// depends on how many times the open sheet is instantiated in the design being simulated: with
/// more than one there is no single answer, so none is given - the same rule wavesOfComponent
/// applies to the schematic's right-click menu.
let waveIndexOfWire
        (fs: FastSimulation)
        (wireModel: DrawModelType.BusWireT.Model)
        (cid: ConnectionId)
            : WaveIndexT option =
    match Map.tryFind cid wireModel.Wires with
    | None -> None
    | Some wire ->
        match Map.tryFind (BlockHelpers.outputPortStr wire.OutputPort) wireModel.Symbol.Ports with
        | None -> None
        | Some port ->
            match port.PortNumber with
            | None -> None
            | Some portNum ->
                match copiesOfCanvasComp fs (ComponentId port.HostId) with
                | Ok fId ->
                    waveIndicesOfFComp fs fId
                    |> List.tryFind (fun wi ->
                        wi.PortType = PortType.Output && wi.PortNumber = portNum)
                | Error _ -> None

/// The value of one wave at one cycle, written as the waveform viewer's value column writes it.
///
/// The step index wraps: the step simulator uses its data arrays as a circular buffer, so a long
/// step simulation is holding only the last MaxArraySize cycles. The waveform simulator does not
/// wrap - its array is sized for the whole run - so there the modulo does nothing.
/// This one reads the given simulation DIRECTLY, and is the only value read left that does.
///
/// Not an oversight. The probe is handed whichever simulation it should answer from - the
/// waveform simulator's while one is running, and the STEP simulator's otherwise (MainView's
/// Probe.source) - and the cache models one active source, not a choice made per call. Routing
/// this through it would have the probe read the waveform simulation while the user is stepping
/// the other one. It goes through a provider when the step simulator gets one, which is also when
/// this stops working by reaching into a FastSimulation that .NET mode will not have.
let waveValueAt (fs: FastSimulation) (cycle: int) (radix: NumberBase) (wi: WaveIndexT) : string option =
    match Array.tryItem wi.SimArrayIndex fs.Drivers with
    | Some(Some driver) ->
        let index = if fs.MaxArraySize > 0 then cycle % fs.MaxArraySize else cycle

        (if driver.DriverWidth > 32 then
             driver.DriverData.TryBig index
             |> Option.map (fun v -> { Dat = BigWord v; Width = driver.DriverWidth })
         else
             driver.DriverData.TryU32 index
             |> Option.map (fun v -> { Dat = Word v; Width = driver.DriverWidth }))
        // padded to a width nothing will be truncated at: unlike the viewer's value column, the
        // probe label sizes itself to its text rather than the text to a column
        |> Option.map (fun fd -> (NumberHelpers.fastDataToPaddedString 60 radix fd).Trim())
    | _ -> None


/// One synchronous read, remembered.
///
/// The probe asks on every render while the pointer rests on a wire, and Issie renders on every
/// mouse move - so without this, moving across a wire costs a blocking round trip per frame.
/// Measured at 1.8ms each, which is cheap for one question and not for sixty a second.
///
/// The answer cannot change while the session, the cycle and the signal are all the same, with one
/// exception: setting an input re-runs the simulation, and a value at the same cycle of the same
/// session can then differ. `memoizeBy` holds only the LAST key, which bounds that exactly - the
/// stale answer survives only while the pointer sits on one wire across an input change, and
/// moving to any other wire, or any other cycle, evicts it. Someone typing in the input panel is
/// not hovering over a wire at the same moment.
let private syncReadOne: (int * int * SidecarSync.SyncSignal) -> bigint option =
    Helpers.memoizeBy id (fun (epoch, cycle, signal) ->
        SidecarSync.readAt epoch cycle [ signal ] |> Option.bind List.tryHead)

/// The same value, read from the .NET simulator, synchronously, inside the render that draws it.
///
/// **The first synchronous operation, and the shape the rest should take.** It is asked only when
/// nothing is in flight, and what it asks for is bounded by construction: one signal, at a cycle
/// the session has already reached. It is not asked while a build or a run is outstanding, so it
/// never waits behind either.
///
/// A width is needed to write the value, and a width is a fact about the elaborated instance - so
/// it comes from `PortView`, which is where every other width the wave simulator uses comes from.
///
/// None wherever the answer is not simply there: nothing built, nothing connected, the wire's
/// cycle not yet run, or something in flight. The probe then draws no label, which is right - a
/// label saying zero would be a value, and there is no value to give.
let syncValueAt
    (model: Model)
    (fs: FastSimulation)
    (cycle: int)
    (radix: NumberBase)
    (wi: WaveIndexT)
    : string option =
    let compId, path = wi.Id

    match model.SidecarSession.Epoch with
    | Some epoch when
        not (ModelHelpers.sidecarIsBusy model)
        && cycle <= model.SidecarSession.Clock
        && wi.PortType = PortType.Output
        ->
        let width =
            (PortView.ofInstanceCached fs (InstancePath path)).ViewPorts
            |> List.tryFind (fun p ->
                p.PortComp = compId && p.PortIs = PortType.Output && p.PortNum = wi.PortNumber)
            |> Option.map (fun p -> p.PortWidth)

        match width with
        | None
        | Some 0 -> None
        | Some width ->
            let signal =
                { SidecarSync.SyncComp = compId
                  SidecarSync.SyncPath = path
                  SidecarSync.SyncPort = wi.PortNumber }

            match syncReadOne (epoch, cycle, signal) with
            | Some value ->
                let fd =
                    if width > 32 then
                        { Dat = BigWord value; Width = width }
                    else
                        { Dat = Word(uint32 value); Width = width }

                Some((NumberHelpers.fastDataToPaddedString 60 radix fd).Trim())
            | None -> None
    | _ -> None

/// What to write beside the cursor for the wire it is resting on: the signal's name and its value
/// at `cycle`. None when the simulation cannot answer - the wire is on a sheet it holds more than
/// one copy of, carries no wave of its own, or has not been simulated as far as this cycle.
///
/// getName, not nameWithSheet: the waveform viewer prefixes the sheet because its rows are drawn
/// away from the design and come from all over it, so a bare "Q" there could be any of them. Here
/// the label is on the wire, on the sheet the user is looking at, and the sheet name is the one
/// thing the position already says. It is also the longest part of the name, on a label that has
/// to fit beside the pointer without covering the circuit.
let probeLabelForWire
        (model: Model)
        (fs: FastSimulation)
        (cycle: int)
        (radix: NumberBase)
        (wireModel: DrawModelType.BusWireT.Model)
        (cid: ConnectionId)
            : string option =
    waveIndexOfWire fs wireModel cid
    |> Option.bind (fun wi ->
        // whichever simulator holds the value: the local arrays, or a synchronous read of the
        // .NET one. Both answer here, so the probe works in either mode and the caller does not
        // have to know which.
        (if model.SimulateInRenderer then
             waveValueAt fs cycle radix wi
         else
             syncValueAt model fs cycle radix wi)
        |> Option.map (fun text -> $"{getName wi fs} = {text}"))

//--------------------------------------------------------------------------------------------------------//
//------------------------------What is shown before anything has been chosen-----------------------------//
//--------------------------------------------------------------------------------------------------------//

/// Most waves a first start will select for itself. A design with more top-level ports than this
/// gets the first few rather than a screenful of waveforms nobody asked for; Select Waves is one
/// click away for the rest.
let private maxDefaultWaves = 12

/// The waves to show when a wave simulation starts with nothing chosen.
///
/// An empty viewer is never what the user wants: it makes the first thing they see after pressing
/// Start a sentence telling them to press a different button.
///
/// First choice is the simulated top sheet's own ports - inputs, then outputs, then Viewers. They
/// are the signals every design has and the ones a beginner came to look at.
///
/// A top sheet can have no ports at all: a whole CPU is often a ROM, a RAM and a couple of
/// subsystem instances, wired to each other and to nothing outside. The `3cpu` demo's `eep1` is
/// exactly that. Falling back to Viewers **anywhere in the design** answers it, because a Viewer
/// is placed for one reason only - somebody wanted to watch that net - so wherever they are, they
/// are the signals the author of the design thought were worth looking at.
///
/// Both are read off the DESIGN: the top sheet's own components for the first, and one instance of
/// each sheet for the second, since which Viewers a sheet has is a fact about the sheet. This used
/// to be one pass over every wave in the simulation - 208,896 of them on largeTest, to choose
/// twelve - and it was the last thing the wave selector did that grew with the expansion.
let defaultSelectedWaves (fs: FastSimulation) : WaveIndexT list =
    /// Ports of the simulated top sheet: inputs, then outputs, then Viewers - the order they are
    /// read in, and the order a schematic is drawn in. Ranked with the label, which orders the
    /// waves within each of the three and is what makes the choice repeatable.
    let topSheetPort (comp: Component) =
        match comp.Type with
        | Input1 _ -> Some(0, comp.Label)
        | Output _ -> Some(1, comp.Label)
        | Viewer _ -> Some(2, comp.Label)
        | _ -> None

    /// Every Viewer in the design, at whatever depth.
    let anyViewer (comp: Component) =
        match comp.Type with
        | Viewer _ -> Some(0, comp.Label)
        | _ -> None

    let ranked pick instances =
        instances
        |> List.collect (waveIndicesOfInstanceBy pick fs)
        |> List.sortBy fst
        |> List.truncate maxDefaultWaves
        |> List.map snd

    match ranked topSheetPort [ InstancePath [] ] with
    | [] -> ranked anyViewer (defaultInstanceOfEachSheet fs)
    | topPorts -> topPorts

/// Choose waves for a wave simulation that has none. Applied only when the user has selected
/// nothing at all - no waves and no RAMs - so a deliberately pared-down selection is never added
/// to, and a selection saved with the sheet is never overridden.
let withDefaultSelectionIfEmpty (fs: FastSimulation) (wsModel: WaveSimModel) : WaveSimModel =
    match List.isEmpty wsModel.SelectedWaves && Map.isEmpty wsModel.SelectedRams with
    | false -> wsModel
    | true -> { wsModel with SelectedWaves = defaultSelectedWaves fs }

/// Modal that, when active, shows the ports of one component picked on the schematic, which of
/// them are displayed as waveforms, and allows that to be changed. Opened from the right-click
/// menu on that component.
let selectCompWavesModal (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let close = fun _ -> dispatch <| UpdateWSModel (fun ws -> {ws with PortSelectComp = None})
    match wsModel.PortSelectComp with
    | None ->
        div [] []
    | Some compId ->
        let fs = Simulator.getFastSim()
        let waves =
            wavesOfComponent fs wsModel compId
            |> fst
            |> List.sortBy (fun wave -> wave.WaveId.PortType, wave.WaveId.PortNumber)
        let compLabel = simLabelOfComponent fs compId |> Option.defaultValue "component"

        /// Said only when the waves are not on the component clicked on but on the ports of the
        /// instance holding it, which is where an Input or Output of a subsheet is simulated.
        let portsOfInstanceNote =
            match waves with
            | wave :: _ when wave.CompLabel <> compLabel ->
                [ str $"The signal is on the "
                  bSpan $"{wave.CompLabel}"
                  str $" port of the sheet instance {compLabel} belongs to, which is where the \
                        waveform viewer shows it."
                  br []; br [] ]
            | _ -> []

        let portRow (wave: Wave) =
            let isSelected = isWaveSelected wave.WaveId wsModel
            let fontStyle = if isSelected then boldFontStyle else normalFontStyle
            waveRow fontStyle [
                input [
                    Type "Checkbox"
                    Checked isSelected
                    OnChange (fun _ -> toggleWaveSelection wave.WaveId wsModel dispatch)
                    Style [MarginLeft "10px"; MarginRight "10px"]
                ]
                p [Style fontStyle] [str wave.PortLabel]
                p [Style fontStyle] [str (match wave.WaveId.PortType with
                                          | PortType.Output -> "Output"
                                          | PortType.Input -> "Input")]
                p [Style fontStyle] [str (if wave.Width = 1 then "1 bit" else $"{wave.Width} bits")]
            ]

        Modal.modal [
            Modal.IsActive true
            Modal.Props [Style [ZIndex 20000]]
        ] [
            Modal.background [ Props [ OnClick close ] ] []
            Modal.Card.card [Props [Style [Width 600]]] [
                Modal.Card.head [] [
                    Modal.Card.title [] [
                        Level.level [] [
                            Level.left [] [ str $"Waveforms from {compLabel}" ]
                            Level.right [] [
                                Delete.delete [
                                    Delete.Option.Size IsMedium
                                    Delete.Option.OnClick close
                                ] []
                            ]
                        ]
                    ]
                ]
                Modal.Card.body [] (
                    portsOfInstanceNote @
                    match waves with
                    | [] ->
                        [ str "No waveforms are available from this component." ]
                    | waves ->
                        [ str "Ticked ports are shown in the waveform viewer. Use "
                          bSpan "Select Waves"
                          str " for anything not on this component."
                          hr []
                          wavePropsTable (List.map portRow waves) ])
                Modal.Card.foot [] []
            ]
        ]
