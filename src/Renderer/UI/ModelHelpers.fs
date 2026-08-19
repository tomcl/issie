module ModelHelpers
open Fulma
open Fable.React
open Fable.React.Props
open CommonTypes
open Sheet.SheetInterface
open ModelType
open Elmish
open Optics
open Optics.Operators
open SimTypes



module Constants =
    /// How often the project browser re-reads the folder it is showing, so that a folder made or
    /// renamed outside Issie appears without being asked for. Listing a folder is one directory
    /// read plus one per subfolder in it - a millisecond or so for an ordinary folder, and 14ms
    /// for the worst one on a typical Windows machine - so this costs nothing worth measuring.
    let projectBrowserRefreshMs = 1000
    /// How tall the project browser's list of folders is. Fixed, and scrolling inside itself, so
    /// that a folder of 200 subfolders does not push the path bar off the top of the dialog.
    let projectBrowserListHeight = "300px"
    /// How long after an edit the Simulation tab works out whether the design still builds.
    /// The check flattens the whole hierarchy, so this is what stops a burst of edits - drawing a
    /// run of wires, dragging out a selection - costing one flatten each. Long enough to cover the
    /// gap between two deliberate edits, short enough that the button follows the design rather
    /// than lagging behind it.
    let circuitCheckDelayMs = 300
    /// Needed to prevent possible overrun of simulation arrays. Defined in CommonTypes, beside
    /// WSConfig, because FastCreate's memory check must agree with the dialog about the array
    /// size a configuration implies; aliased here for the many existing readers.
    let multipliers = CommonTypes.waveSimMultipliers
    let maxStepsOverflow = CommonTypes.waveSimStepsOverflow

    /// How many cycles a waveform simulation must hold beyond its last clock.
    ///
    /// Zooming out samples every Nth cycle, and reading the last sample can run a multiplier past
    /// LastClock - so the arrays carry a margin of one. It used to be the largest multiplier there
    /// is, whatever the simulation was: a 200-cycle simulation allocated 1203 cycles of arrays, six
    /// times what it could use. A multiplier bigger than LastClock samples nothing, so the margin
    /// is the largest that is not, and the one in use whatever that is - the zoom menu offers the
    /// current multiplier back even when the configuration has since shrunk under it.
    let waveSimZoomMargin (wsModel: WaveSimModel) =
        multipliers
        |> List.filter (fun m -> m <= wsModel.WSConfig.LastClock)
        |> List.fold max wsModel.SamplingZoom

    let waveSimRequiredArraySize wsModel =
        wsModel.WSConfig.LastClock + maxStepsOverflow + waveSimZoomMargin wsModel

    /// Where a waveform simulation of a big design starts, in clock cycles. Enough to watch a
    /// design work; short enough that starting one is seconds rather than minutes.
    let shortStartClock = 200
    /// The share of the heap budget its expanded design may take before a simulation is started at
    /// shortStartClock rather than at what it was configured for.
    let bigDesignHeapShare = 0.2
    let defaultWSConfig = {
            LastClock = 2000; // Simulation array limit during wave simulation
            FirstClock = 0; // first clock accessible - limits scroll range. NOT IMPLEMENTED
            FontSize = 15; // size of text on waveforms
            FontWeight = 500 // weight of text on waveforms
            }
    let maxWarnSimulationSize = 100000
    /// The absolute ceiling on a waveform simulation's last clock, whatever fits in memory:
    /// past this the viewer itself cannot display the whole waveform nicely.
    let maxSimulationSize = 4000000
    let minScrollingWindow = 200

    let wsButtonHeight = 30
    let wsButtonWidth = 120
    let wsButtonFontSize = 16

    /// initial number of clock cycles navigated by the scrollbar.
    let scrollbarBkgRepCyclesInit = 100

/// Keep a project browser selection on a row that exists. The folder can grow or shrink under it
/// between one refresh and the next, and an index past the end would leave Enter doing nothing.
let clampSelection (index: int) (entries: 'a list) =
    if List.isEmpty entries then 0 else max 0 (min index (List.length entries - 1))

/// Read a folder for the project browser: what is in it, or why it cannot be shown.
///
/// Done here, from the update function, rather than while rendering. A popup body runs on every
/// message, so a view that read the disk would read it continuously - and the keyboard needs the
/// number of rows before it can move between them.
let readProjectBrowserFolder (folder: string) (selected: int) : ProjectBrowserState =
    // The reason comes from the read itself rather than from a separate isDirectory beforehand.
    // Asking first said "That folder does not exist" about every folder Issie was not allowed to
    // look in, which was most of them and never what the user was looking at.
    let listing = FilesIO.browseFolderForOpening folder
    { Folder = folder
      Listing = listing
      Selected = listing |> Result.map (clampSelection selected) |> Result.defaultValue 0 }

/// type used for CSS grids in the UI to position an item on a grid
type CSSGridPos =
    | PosElement of int * int
    | PosAreaSpan of startX: int * startY: int * spanX: int * spanY: int
    | PosAreaAbsolute of startX: int * startY: int * spanX: int * spanY: int


let initWSModel  : WaveSimModel = {
    DefaultCursor = CursorType.Default
    WSConfig = Constants.defaultWSConfig
    WSConfigDialog = None
    State = Empty
    AllWaves = Map.empty
    SelectedWaves = List.empty
    StartCycle = 0
    ShownCycles = 5
    SamplingZoom = 1
    CursorDisplayCycle = 0
    CursorExactClkCycle = 0
    ClkCycleBoxIsEmpty = false
    Radix = Hex
    WaveformColumnWidth = Constants.initialWaveformColWidth
    WaveModalActive = false
    RamModalActive = false
    PortSelectComp = None
    RamComps = []
    SelectedRams = Map.empty
    RamStartLocation = Map.empty
    ShowSheetDetail = Set.empty
    SelectedSheetInstance = Map.empty
    ShowGroupDetail = Set.empty
    HoveredLabel = None
    DraggedIndex = None
    PrevSelectedWaves = None
    
    ScrollbarTbWidth = 0.0 // overwritten when first rendered
    ScrollbarTbPos = 0.0 // overwritten when first rendered
    ScrollbarTbOffset = None // default value: not in scroll
    ScrollbarBkgWidth = 0.0 // overwritten when first rendered
    ScrollbarBkgRepCycs = Constants.scrollbarBkgRepCyclesInit // default value
    ScrollbarQueueIsEmpty = true // default value: empty scroll queue
    SheetSearchString = ""
    ComponentSearchString = ""
    PortSearchString = ""
    ShowOnlySelected = false
}

/// This is needed because DrawBlock cannot directly access Issie Model.
/// can be replaced when all Model is placed at start of compile order and DB
/// model is refactored
let drawBlockModelToUserData (model: Model) (userData: UserData)=
    let bwModel =model.Sheet.Wire
    {userData with WireType = bwModel.Type; ArrowDisplay = bwModel.ArrowDisplay}

/// This is needed because DrawBlock cannot directly access Issie Model.
/// can be replaced when all Model is placed at start of compile order and DB
/// model is refactored
let userDataToDrawBlockModel (model: Model) =
    let userData = model.UserData
    {model with 
        Sheet = 
            {model.Sheet with 
                Wire = {
                    model.Sheet.Wire with 
                        Type = userData.WireType
                        ArrowDisplay = userData.ArrowDisplay
                        Symbol = {
                            model.Sheet.Wire.Symbol with Theme = userData.Theme
                        }}}}

//-------------------------------------------------------------------------------------------//
//------------------------READ-ONLY SHEETS (VIEWED LIBRARY COMPONENTS)-----------------------//
//-------------------------------------------------------------------------------------------//

/// Whether the sheet now open is a library sheet the user asked to look inside, and so must not
/// be editable.
let openSheetIsReadOnly (model: Model) =
    match model.CurrentProj with
    | Some p -> Set.contains p.OpenFileName model.OpenedLibrarySheets
    | None -> false

/// The draw block state to hold a read-only sheet at. Taken once the sheet has settled, never
/// while it loads: loading recomputes symbol sizes, reroutes wires whose ports have moved and
/// centres the circuit, all of which are changes the pin would otherwise undo one by one until
/// the sheet never finished opening.
let pinnedCanvasOf (sheet: DrawModelType.SheetT.Model): PinnedCanvas =
    let sym = sheet.Wire.Symbol
    {
        Symbols = sym.Symbols
        Ports = sym.Ports
        InputPortsConnected = sym.InputPortsConnected
        OutputPortsConnected = sym.OutputPortsConnected
        CopiedSymbols = sym.CopiedSymbols
        Wires = sheet.Wire.Wires
        CopiedWires = sheet.Wire.CopiedWires
        BoundingBoxes = sheet.BoundingBoxes
    }

/// One symbol held at its pinned state, keeping from the live one the fields that are never
/// saved. Appearance is colour, opacity, port visibility and corner handles - everything
/// selection and hover change - and the InWidth fields are recomputed by width inference, so all
/// of those stay free. Component, Pos, PortMaps, STransform, the scales and the label geometry
/// reach the .dgm and so come from the pin, as do MovingPort and Moving: transient, but left
/// free a port drag would paint a preview that then vanished.
let private pinSymbol (pinned: DrawModelType.SymbolT.Symbol) (live: DrawModelType.SymbolT.Symbol) =
    { pinned with
        Appearance = live.Appearance
        InWidth0 = live.InWidth0
        InWidth1 = live.InWidth1
        InWidths = live.InWidths
        IsClocked = live.IsClocked
        CentrePos = live.CentrePos
        OffsetFromBBCentre = live.OffsetFromBBCentre }

/// Write the pinned state back over the live draw block, so that nothing which would be saved
/// can change while a library sheet is being viewed.
///
/// This is the whole of read-only enforcement. It restores rather than refuses because there are
/// too many ways to edit to block them one at a time and be sure: 58 mutating cases across the
/// three draw block Msg types, half a dozen places that write model.Sheet directly through
/// Optic.map without reaching any update function, and UpdateModel, which carries an arbitrary
/// Model -> Model and cannot be inspected at all. Undoing the change afterwards catches every
/// one of those, and goes on catching messages added later. Everything the user can reach that
/// would be reverted is separately disabled, so nothing appears to work and then springs back.
///
/// Called after every message, so it first asks whether anything it pins was touched at all: the
/// maps are persistent, so an untouched one is still the same object and the usual case costs
/// seven reference comparisons and no allocation.
let pinSheet (pinned: PinnedCanvas) (sheet: DrawModelType.SheetT.Model) =
    let wire = sheet.Wire
    let sym = wire.Symbol
    let same a b = LanguagePrimitives.PhysicalEquality a b
    if same sym.Symbols pinned.Symbols
       && same sym.Ports pinned.Ports
       && same sym.CopiedSymbols pinned.CopiedSymbols
       && same wire.Wires pinned.Wires
       && same wire.CopiedWires pinned.CopiedWires
       && same sheet.BoundingBoxes pinned.BoundingBoxes
       && List.isEmpty sheet.UndoList && List.isEmpty sheet.RedoList && sheet.TmpModel.IsNone
    then
        sheet
    else
        // Map over the PINNED symbols, not the live ones: a symbol the sheet has deleted must
        // come back, and one it has added must not survive.
        let symbols =
            if same sym.Symbols pinned.Symbols then
                pinned.Symbols
            else
                pinned.Symbols
                |> Map.map (fun cId pinnedSym ->
                    match Map.tryFind cId sym.Symbols with
                    | Some liveSym -> pinSymbol pinnedSym liveSym
                    | None -> pinnedSym)
        { sheet with
            Wire =
                { wire with
                    Wires = pinned.Wires
                    CopiedWires = pinned.CopiedWires
                    Symbol =
                        { sym with
                            Symbols = symbols
                            Ports = pinned.Ports
                            InputPortsConnected = pinned.InputPortsConnected
                            OutputPortsConnected = pinned.OutputPortsConnected
                            CopiedSymbols = pinned.CopiedSymbols } }
            BoundingBoxes = pinned.BoundingBoxes
            // A read-only sheet has no history to step through, and letting the lists grow would
            // leave Ctrl+Z restoring a model the pin then had to undo again.
            UndoList = []
            RedoList = []
            TmpModel = None }

/// pinSheet applied to the draw block of the whole model. The model is returned untouched when the
/// sheet is, so a message that changed nothing pinned allocates nothing.
let pinDrawBlock (pinned: PinnedCanvas) (model: Model) =
    let sheet = pinSheet pinned model.Sheet
    if LanguagePrimitives.PhysicalEquality sheet model.Sheet then model
    else Optic.set sheet_ sheet model

/// Whether a message moves the whole circuit without changing the design.
///
/// Fit to window recentres the schematic by translating every symbol and wire (Sheet.moveCircuit),
/// so a plain pin would put them all back and the one navigation command that matters on a sheet
/// the user can only look at would appear to do nothing. Nothing about the design changes, and a
/// viewed sheet is never written, so the pin is taken again at the new positions instead. This is
/// a list of view operations, not an exception to read-only: everything absent from it is pinned,
/// which is why it can stay this short without going stale.
let private movesWholeCircuit (msg: Msg) =
    match msg with
    | Sheet (DrawModelType.SheetT.KeyPress DrawModelType.SheetT.KeyboardMsg.CtrlW) -> true
    | _ -> false

/// Hold the draw block at its pinned state if the open sheet is being viewed read-only. Applied
/// to the result of every message.
let pinIfReadOnly (msg: Msg) (model: Model) =
    match model.ReadOnlyBaseline with
    | None -> model
    | Some _ when movesWholeCircuit msg ->
        Optic.set readOnlyBaseline_ (Some (pinnedCanvasOf model.Sheet)) model
    | Some pinned -> pinDrawBlock pinned model

let reduce (this: Model) = {|
         RightTab = this.RightPaneTabVisible
         Hilighted = this.Hilighted
         Clipboard = this.Clipboard
         LastSimulatedCanvasState = this.LastSimulatedCanvasState
         LastSelectedIds = this.LastSelectedIds
         CurrentSelected = this.CurrentSelected
         LastUsedDialogWidth = this.LastUsedDialogWidth
         SelectedComponent= this.SelectedComponent
         CreateComponent = this.LastCreatedComponent
         HasUnsavedChanges = false
         CurrProject = match this.PopupViewFunc with None -> false | _ -> true
         PopupDialogData = this.PopupDialogData
         TopMenu = this.TopMenuOpenState
         DragMode = this.DividerDragMode
         ViewerWidth = this.WaveSimViewerWidth
         ConnsToBeHighlighted = this.ConnsOfSelectedWavesAreHighlighted

 |} 
       
let reduceApprox (this: Model) = {|
         RightTab = this.RightPaneTabVisible
         Clipboard = this.Clipboard
         CurrProject = match this.PopupViewFunc with None -> false | _ -> true
         LastUsedDialogWidth = this.LastUsedDialogWidth
         CreateComponent = this.LastCreatedComponent
         HasUnsavedChanges = false
         PopupDialogData = this.PopupDialogData
         DragMode = this.DividerDragMode
         ViewerWidth = this.WaveSimViewerWidth
 |} 

let mapFst mapFn (model,cmd) = mapFn model, cmd

let mapOverProject defaultValue (model: Model) transform =
    match model.CurrentProj with
    | None -> defaultValue
    | Some p -> transform p

let getComponentIds (model: Model) =
    let extractIds ((comps,conns): Component list * Connection list) = 
        conns
        |> List.map (fun comp -> ComponentId comp.Id)
        
    model.Sheet.GetCanvasState()
    |> extractIds
    |> Set.ofList

//------------------------//
// Saving WaveSim Model   //
//------------------------//

/// Get saveable record of WaveSimModel
let getSavedWaveInfo (wsModel: WaveSimModel) : SavedWaveInfo =
    {
        SelectedWaves = Some wsModel.SelectedWaves
        Radix = Some wsModel.Radix
        WaveformColumnWidth = Some wsModel.WaveformColumnWidth
        SelectedFRams = Some wsModel.SelectedRams
        SelectedRams = None

        WSConfig = Some wsModel.WSConfig

        // The following fields are from the old waveform simulator.
        // They are no longer used.
        ClkWidth = None
        Cursor = None
        LastClk = None
        DisplayedPortIds = None
    }

/// Setup current WaveSimModel from saved record
/// NB: note that SavedWaveInfo can only be changed if code is added to make loading backwards compatible with
/// old designs
let loadWSModelFromSavedWaveInfo (swInfo: SavedWaveInfo) : WaveSimModel =
    {
        initWSModel with
            SelectedWaves = Option.defaultValue initWSModel.SelectedWaves swInfo.SelectedWaves
            Radix = Option.defaultValue initWSModel.Radix swInfo.Radix
            WaveformColumnWidth = Option.defaultValue initWSModel.WaveformColumnWidth swInfo.WaveformColumnWidth
            SelectedRams = Option.defaultValue initWSModel.SelectedRams swInfo.SelectedFRams
            WSConfig =Option.defaultValue initWSModel.WSConfig swInfo.WSConfig
    }

//----------------------Print functions-----------------------------//
//------------------------------------------------------------------//

let spComp (comp:Component) =
    match comp.Type with
    | Custom {Name=name; InputLabels=il; OutputLabels=ol} -> sprintf "Custom:%s(ins=%A:outs=%A)" name il ol
    | x -> sprintf "%A" x

let spConn (conn:Connection) = 
    sprintf "Conn:%A" conn.Vertices

let spState ((comps,conns):CanvasState) = 
    sprintf "Canvas<%A,%A>" (List.map spComp comps) (List.map spConn conns)

let spCanvas (model : Model) = 
    model.Sheet.GetCanvasState()
    |> spState

let spComps comps =  
    sprintf "Comps%A" (List.map spComp comps)

let spOpt f thingOpt = match thingOpt with |None -> "None" | Some x -> sprintf "Some %s" (f x)

let spLdComp (ldc: LoadedComponent) =
    sprintf "LDC<%s:%A:%s>" ldc.Name ldc.TimeStamp ((fst >>spComps) ldc.CanvasState)

let spProj (p:Project) =
    sprintf "PROJ||Sheet=%s\n%s||ENDP\n" p.OpenFileName (String.concat "\n" (List.map spLdComp p.LoadedComponents))

let spMess msg =
    match msg with
    //| SetProject p -> sprintf "MSG<<SetProject:%s>>ENDM" (spProj p)
    //| SetLastSimulatedCanvasState canvasOpt-> sprintf "MSG<SetLastSimCanv:%s>>ENDM" (spOpt spState canvasOpt)
    | x -> sprintf "MSG<<%20A>>ENDM" x

let tryGetLoadedComponents model =
    match model.CurrentProj with
    | Some p -> p.LoadedComponents
    | _ -> []

let updateLdComps (name:string) (changeFun: LoadedComponent -> LoadedComponent)  (ldComps: LoadedComponent list)=
    ldComps
    |> List.map (fun ldc -> if ldc.Name=name then changeFun ldc else ldc)

let updateLdCompsWithCompOpt (newCompOpt:LoadedComponent option) (ldComps: LoadedComponent list) =
    match newCompOpt with 
    | None -> ldComps // no update
    | Some newComp -> 
        match List.tryFind (fun (ldc:LoadedComponent) -> ldc.Name = newComp.Name) ldComps with
        | None -> newComp :: ldComps
        | Some _ -> updateLdComps newComp.Name (fun _ -> newComp) ldComps

/// The address and data widths one memory component of the open sheet has ACROSS ITS DESIGN: one
/// pair per set of parameter values the sheet is used at, never empty.
///
/// Contents are one map shared by every instance of the sheet, so data offered to a memory - typed
/// into the editor, or read from a .ram file it is linked to - has to fit every one of these, not
/// merely the shape the sheet happens to be drawn at. Where nothing is parameterised there is one
/// pair and this says what it always said.
let memoryWidthsInDesign (model: Model) (compId: ComponentId) (mem: Memory1) : (int * int) list =
    let (ComponentId compIdStr) = compId
    match model.CurrentProj with
    | None -> [ mem.AddressWidth, mem.WordWidth ]
    | Some project ->
        ParameterAnalysis.memoryWidthsInDesign
            project.LoadedComponents project.OpenFileName compIdStr mem

/// returns a string option representing the current file name if file is loaded, otherwise None
let getCurrFile (model: Model) =
    match model.CurrentProj with
    | Some proj -> Some proj.OpenFileName
    | None -> None

let getCurrSheets (model: Model) =
    match model.CurrentProj with
    | Some proj -> 
        proj.LoadedComponents
        |> List.map (fun lc -> lc.Name)
        |> Some
    | None -> None

/// Release what a WaveSimModel holds that should live only as long as its simulation does.
///
/// AllWaves looks like plain data and is not. A Fable Map carries its comparer, a closure made
/// where the map was built - inside getWaves, with the FastSimulation in scope - and a V8 closure
/// context captures the scope's variables whether or not this closure uses them. So the comparer
/// of every AllWaves map pins the whole simulation it was built from, step arrays and all.
/// A wave simulation keeps its WaveSimModel after it ends, for its configuration and its selected
/// waves; keeping AllWaves too kept the dead simulation - hundreds of MB on a large design, one
/// per sheet ever wave-simulated - for the life of the project. Found with a heap snapshot, after
/// every holder the code knows about was confirmed empty.
///
/// Success and Loading become Ended because AllWaves is what the viewer draws from: a state that
/// says "showing waveforms" over an emptied map would be a lie some view would act on.
let private releaseWaveSimData (ws: WaveSimModel) : WaveSimModel =
    { ws with
        AllWaves = Map.empty
        State =
            match ws.State with
            | Success | Loading -> Ended
            | s -> s }

/// For reasons of space efficiency, ensure that no non-empty unused FastSimulation records are kept.
/// A FastSimulation holds a step array per net and a SimulationGraph node per component instance, so a
/// large design's is hundreds of MB: one left behind slows every later edit, because each major GC must
/// trace all of it. Call this before building a new simulation.
///
/// CurrentStepSimulationStep is the only field of the model holding one. Every WaveSim entry is
/// released as well - see releaseWaveSimData - which covers the sheet the caller is about to
/// resimulate (its AllWaves is rebuilt by the refresh), the sheets left behind by switching the
/// waveform simulator between sheets, and the entry EndWaveSim is about to mark Ended. The truth
/// table's TableSimData is deliberately left alone: it is what regenerates the table when a
/// constraint changes, so it is in use rather than stale.
let removeAllSimulationsFromModel (model:Model) =
    model
    |> Optic.set currentStepSimulationStep_ None
    |> Optic.map waveSim_ (Map.map (fun _ -> releaseWaveSimData))

/// True if a step simulation, truth table or waveform simulation is currently open.
/// Parameters create dependencies across a whole design, so they cannot be changed while one is open.
let simulationIsOpen (model: Model) =
    model.CurrentStepSimulationStep <> None
    || model.CurrentTruthTable <> None
    || (model.WaveSimSheet <> None && model.WaveSimSheet <> Some "")


/// Get the current WaveSimModel used by the Model (index the map using the current wavesim sheet).
/// If no WaveSimModel for that sheet, return an empty wave sim model.
let rec getWSModel model : WaveSimModel =
    match model.WaveSimSheet with
    | Some sheet ->
        Map.tryFind sheet model.WaveSim
        |> function
            | Some wsModel ->
                wsModel
            | None ->
                initWSModel
    | None ->
        match getCurrFile model with
        | None -> 
            initWSModel
        | Some sheet ->
            getWSModel {model with WaveSimSheet = Some sheet}        

/// Set WaveSimModel of current sheet.
let setWSModel (wsModel: WaveSimModel) (model: Model) =
    match getCurrSheets model, model.WaveSimOrCurrentSheet with
    | Some sheets, wsSheet when List.contains wsSheet sheets ->
        { model with WaveSim = Map.add wsSheet wsModel model.WaveSim }
    | Some sheets, wsSheet ->
        failwithf $"What? can't find {wsSheet} in {sheets} to set WSModel"
    | None, _ ->
        Log.warn $"cannot set the waveform simulator model for the current sheet: the project is closed"
        model

/// This will - given a project is open - never fail. The getter returns the default WaveSimModel record if none
/// exists. The setter will add the WaveSimModel to the WaveSim map in the model.
let waveSimModel_ =
    let setter (wsr: WaveSimModel) (m: Model) =
        {m with WaveSim = Map.add m.WaveSimOrCurrentSheet wsr m.WaveSim}
    let getter (m: Model) =
        match Map.tryFind m.WaveSimOrCurrentSheet m.WaveSim with
        | Some wsm-> wsm
        | None -> initWSModel
    Lens.create getter setter

/// Update WaveSimModel of current sheet.
let updateWSModel (updateFn: WaveSimModel -> WaveSimModel) (model: Model) =
    match getCurrSheets model, model.WaveSimOrCurrentSheet with
    | Some sheets, wsSheet when List.contains wsSheet sheets ->
        let ws = model.WaveSim[wsSheet]
        { model with WaveSim = Map.add wsSheet (updateFn ws) model.WaveSim }
    | Some sheets, wsSheet ->
        failwithf $"What? can't find {wsSheet} in {sheets} to set WSModel"
    | None, _ ->
        Log.warn $"cannot set the waveform simulator model for the current sheet: the project is closed"
        model

/// Update WaveSimModel of given sheet - if it does not exist do nothing
let updateWSModelOfSheet (sheet: string) (updateFn: WaveSimModel -> WaveSimModel) (model: Model) =
    match getCurrSheets model, sheet with
    | Some sheets, wsSheet when List.contains wsSheet sheets ->
        let ws = model.WaveSim[wsSheet]
        { model with WaveSim = Map.add wsSheet (updateFn ws) model.WaveSim }
    | None, _ ->
        Log.warn $"cannot set the waveform simulator model for the current sheet: the project is closed"
        model
    | Some sheets, wsSheet ->
        Log.warn $"cannot set the waveform simulator model for '{wsSheet}': it is not one of {sheets}"
        model

/// a long function to be executed in a message after the view function has run at least once
type ViewableJob = {
    JobWork: Model-> Model * Cmd<Msg>
    ViewHasExecuted: bool
    JobName: string
    }

/// list of jobs awaiting execution
let mutable asyncJobs: ViewableJob list = []

let runAfterView (jobName:string) ( workFn: Model -> Model * Cmd<Msg>) =
    let job = {JobWork=workFn; ViewHasExecuted = false; JobName = jobName}
    Log.dbg Log.Update $"scheduling deferred job {jobName}"
    asyncJobs <- List.append asyncJobs [job]

let setAsyncJobsRunnable dispatch =
    dispatch DoNothing
    asyncJobs <- 
        asyncJobs 
        |> List.map (fun job -> {job with ViewHasExecuted = true}); 

/// called from update function, it will execute outstanding async jobs.
/// each job modifies model.
let execOneAsyncJobIfPossible (model: Model,cmd: Cmd<Msg>)=
    asyncJobs
    |> List.filter (fun job -> job.ViewHasExecuted) 
    |> function 
        | [] -> (model,cmd)
        | job::_ -> 
            asyncJobs <- List.filter (fun job' -> job'.JobName <> job.JobName) asyncJobs 
            job.JobWork model
            |> (fun (model', cmd') -> model', Cmd.batch [cmd; cmd'])

/// Return the project with with open file contents in loadedcomponents updated according to
/// current Draw Block contents.
let getUpdatedLoadedComponents (project: Project) (model: Model) : Project =
    mapOverProject project model ( fun p ->
        p
        |> Optic.set (loadedComponentOf_ p.OpenFileName >-> canvasState_) (model.Sheet.GetCanvasState()))

/// Set the part of model specified by optic_ to initToSet: bounded by maxVal, minVal.
/// dispatch: the Elmihs dispatch function.
let setModelInt (optic_: Lens<Model,int>) (dispatch: Msg -> unit) maxVal minVal intToSet : unit =
    let intToSet = if intToSet > maxVal then maxVal else if intToSet < minVal then minVal else intToSet
    dispatch <| UpdateModel (Optics.Optic.set optic_ intToSet)

//--------------------------------------------------------------------------------------------//
//------------------------React Input Boxes for numeric Parsing with Elmish-------------------//
//--------------------------------------------------------------------------------------------//

// Code should be refactored to use these throughout

/// Both input text and its parsed numeric value must be stored in the model.
/// The two fields contain optics used to access these items in the model
type ModelLocations = {
     TextOptic_: Optics.Lens<Model,string>
     ValOptic_: Lens<Model,bigint>
}

/// <summary> Display an input box which is parsed as a bigint and written back to the Model using
/// textOptic (the text) and valOptic (the value). isValid must return true for the value to be written back to
/// the model</summary>
let inputBigint
        (props: IHTMLProp list)
        (placeholder:string)
        (locs: ModelLocations)
        (isValid: bigint -> Model -> bool)
        (dispatch: Msg -> unit) 
        (model:Model): ReactElement =

    let isNowValid bigNum =  isValid bigNum (Optic.set locs.ValOptic_ bigNum model)

    let parseInput (text:string) =
        dispatch <| UpdateModel (Optics.Optic.set locs.TextOptic_ text)
        match NumberHelpers.strToBigint text with
        | Ok big when isNowValid big ->
            dispatch <| UpdateModel (Optic.set locs.ValOptic_ big)
        | _ -> ()
            
    Input.text [
        // caller's props go last so they win: these defaults used to be appended after them, which
        // silently overrode an explicit AutoFocus false at the one call site that asked for it
        Input.Props ([OnPaste PopupHelpers.preventDefault; AutoFocus true; SpellCheck false] @ props)
        Input.Placeholder placeholder
        Input.DefaultValue (model |> Optics.Optic.get locs.TextOptic_)
        Input.OnChange (JSHelpers.getTextEventValue >> parseInput)
    ]


//---------------------------------------------------------------------------------------------//
//----------------------------View level simulation interface-----------------------------------//
//---------------------------------------------------------------------------------------------//
//
// Add-on to simulator.fs code. This is the interface to the simulator from the view level.
// it must be here because it references Model types.
//
open SimGraphTypes

let simReset dispatch =
    dispatch CloseSimulationNotification // Close error notifications.
    dispatch ClosePropertiesNotification
    dispatch <| Sheet (DrawModelType.SheetT.ResetSelection) // Remove highlights.
    dispatch <| (JSDiagramMsg << InferWidths) () // Repaint connections.

/// Start simulating the current Diagram.
/// Return SimulationData that can be used to extend the simulation
/// as needed, or error if simulation fails.
/// Note that simulation is only redone if current canvas changes.
/// What the design a simulation of this sheet would expand to costs in heap, worked out the way
/// GraphMerger refuses on and from the same pair of canvas and project the simulation itself is
/// assembled from.
let simulationHeapEstimate (simulatedSheet: string option) (openSheetCanvasState: CanvasState) model : float =
    match model.CurrentProj with
    | None -> 0.0
    | Some project ->
        let simSheet = Option.defaultValue project.OpenFileName simulatedSheet
        // The open sheet as it is on the canvas now, every other sheet as the project holds it -
        // which is the design simulateModel goes on to build.
        let ldcs =
            project.LoadedComponents
            |> List.map (fun ldc ->
                if ldc.Name = project.OpenFileName then { ldc with CanvasState = openSheetCanvasState } else ldc)
        ldcs
        |> List.tryFind (fun ldc -> ldc.Name = simSheet)
        |> Option.map (fun ldc -> GraphMerger.expandedHeapEstimate simSheet ldc.CanvasState ldcs)
        |> Option.defaultValue 0.0

/// The clock cycle a waveform simulation should start at, given what its expanded design will cost
/// in heap and what it was configured for.
///
/// A big design costs minutes to start, not seconds: main6 of the largeTest project is 480,000
/// components, and at the configured 2000 cycles it spent three minutes building 6GB of step
/// arrays and another two building 835,000 wave records - all before showing anything. Started
/// short it is seconds, and the configuration is still there to raise once the user has seen the
/// design work. This only ever lowers what was asked for.
///
/// Only the UNTOUCHED DEFAULT of a design past bigDesignHeapShare is lowered: a value anyone has
/// set in the configuration dialog is honoured exactly, however big the design. The dialog
/// already refuses what will not fit in memory (FastCreate.maxLastClockFor), so an explicit
/// value is one the user has been told the cost of - and overriding it made the configuration a
/// lie: main6 of largeTest, configured to 4000 cycles well inside its stated limit, was silently
/// started at 200 with no way to raise it, since the dialog is closed to a running simulation.
/// The overwrite even outlived the session, because WSConfig is saved in the sheet.
/// (An earlier version also tapered the allowed cycles as 40/share below the threshold, which
/// capped a 350-component CPU at 75,000 of its configured 1,000,000 cycles.)
let startingLastClock (configured: int) (heapEstimate: float) =
    let budget = SimTypes.SimulationBudget.maxHeapBytes
    if configured = Constants.defaultWSConfig.LastClock
       && heapEstimate > 0.0 && budget > 0.0
       && heapEstimate / budget > Constants.bigDesignHeapShare then
        min configured Constants.shortStartClock
    else
        configured

let simulateModel (isWaveSim: bool) (simulatedSheet: string option) (simulationArraySize: int) openSheetCanvasState model =
    let start = TimeHelpers.getTimeMs()
    match openSheetCanvasState, model.CurrentProj with
    | _, None -> 
        Error (Simulator.makeDummySimulationError "What - Internal Simulation Error starting simulation - I don't think this can happen!"), openSheetCanvasState
    | canvasState, Some project ->
        let simSheet = Option.defaultValue project.OpenFileName simulatedSheet
        let otherComponents = 
            project.LoadedComponents 
            |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        (canvasState, otherComponents)
        ||> Simulator.prepareSimulationMemoized isWaveSim simulationArraySize project.OpenFileName simSheet 
        |> TimeHelpers.instrumentInterval "MakeSimData" start

//------------------------------------------------------------------------------------------------//
//--------------------------- Does the open design build into a simulation? ----------------------//
//------------------------------------------------------------------------------------------------//

(*
    The Simulation tab shows a button whose colour says whether the design currently builds, and a
    line of text saying whether it is synchronous. Both used to come from a full simulation built
    during render - which on a large design meant flattening the whole hierarchy, and allocating a
    step array for every net, for every frame the tab was visible.

    What follows answers the same two questions without either cost: validateCircuitSimulation
    stops once it has a checked graph, and the answer is stored in the model so that renders read
    it rather than recompute it. See ModelType.CircuitCheck.

    Deliberately NOT through prepareSimulationMemoized. That cache holds exactly one simulation, and
    a check asking for a different array size would evict the simulation the user is running - so
    this keeps out of its way entirely.
*)

/// The design as the simulator sees it: the open sheet as it is on the canvas now, and every other
/// sheet as the project holds it. The same list prepareSimulationMemoized compares against, so a
/// verdict and a simulation go stale together.
///
/// canvasState is passed in rather than taken from model.Sheet: extracting it walks every symbol
/// and wire on the sheet, and the caller that runs per render already holds the one MainView
/// extracted for this frame.
let private designOf (project: Project) (canvasState: CanvasState) =
    project.LoadedComponents
    |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
    |> CanvasExtractor.addStateToLoadedComponents project.OpenFileName canvasState

/// Are two versions of a design the same circuit? Cheap in the ordinary case: only the open sheet
/// is ever rebuilt, so loadedComponentIsEqual settles every other sheet by reference.
let private designIsUnchanged (ldcs1: LoadedComponent list) (ldcs2: LoadedComponent list) =
    List.length ldcs1 = List.length ldcs2
    && ldcs1
       |> List.forall (fun ldc ->
            ldcs2
            |> List.tryFind (fun ldc' -> ldc'.Name = ldc.Name)
            |> Option.map (CanvasExtractor.loadedComponentIsEqual ldc)
            |> (=) (Some true))

/// Should the Simulation tab ask for a new verdict? True when the stored one is missing or no
/// longer describes the design, and no check is already on its way.
///
/// The CheckPending test comes first, and not only to save the comparison: this is asked while
/// rendering, and a render that asked again for a check already scheduled would be answered with
/// another render, and so on without ever reaching the check.
let circuitCheckIsNeeded (model: Model) (canvasState: CanvasState) =
    if model.CircuitCheck.CheckPending then
        false
    else
        match model.CircuitCheck.Verdict, model.CurrentProj with
        | Some(_, checkedLdcs), Some project ->
            not (designIsUnchanged checkedLdcs (designOf project canvasState))
        | _, Some _ -> true
        | _, None -> false

/// Work out whether the open design builds into a simulation, and whether it is synchronous.
/// Called from the update function on a delay, never while rendering.
let runCircuitCheck (model: Model) : CircuitCheck =
    match model.CurrentProj with
    | None -> { Verdict = None; CheckPending = false }
    | Some project ->
        let ldcs = designOf project (model.Sheet.GetCanvasState())
        // Caught rather than allowed to propagate: the checks reach a good deal of code, and one
        // of them raising must not leave CheckPending set, which would stop every later check and
        // freeze the button on whatever it last said.
        let verdict =
            try
                CanvasExtractor.getStateAndDependencies project.OpenFileName ldcs
                |> Result.mapError Simulator.makeDummySimulationError
                |> Result.bind (fun (_, state, deps) ->
                    Simulator.validateCircuitSimulation project.OpenFileName state deps)
                |> Result.map SynchronousUtils.hasSynchronousComponents
            with e ->
                Log.error $"exception while checking the circuit: {e.Message}"
                Error
                    { ErrType = InternalError e
                      InDependency = None
                      ComponentsAffected = []
                      ConnectionsAffected = [] }
        { Verdict = Some(verdict, ldcs); CheckPending = false }

/// What the design (result * ldcs) the memoised waveform cost below was computed for.
/// A cache in the simCache mould: the dialog that reads the cost renders per keystroke.
let mutable private waveSimCostMemo: (string * LoadedComponent list * Result<SimTypes.StepCost, SimulationError>) option =
    None

/// The per-cycle memory cost of wave-simulating the open design: what the waveform configuration
/// dialog's size message and its OK gating need, and nothing more.
///
/// This exists because the dialog used to get the same number by building a complete 10-cycle
/// simulation - every FastComponent, every step array - and reading one field off the result:
/// 49 seconds of frozen dialog on a 480,000-component design, and the waveform simulation cache
/// evicted on the way. The cost is a fact about the flattened design, so this stops at the
/// flattening: check the circuit, gather it, price it, allocate nothing.
let waveSimStepCost (model: Model) : Result<SimTypes.StepCost, SimulationError> =
    match model.CurrentProj with
    | None -> Error (Simulator.makeDummySimulationError "No project is open")
    | Some project ->
        let simSheet =
            match model.WaveSimSheet with
            | Some s when s <> "" -> s
            | _ -> project.OpenFileName
        let ldcs = designOf project (model.Sheet.GetCanvasState())
        match waveSimCostMemo with
        | Some(memoSheet, memoLdcs, result) when memoSheet = simSheet && designIsUnchanged memoLdcs ldcs ->
            result
        | _ ->
            let result =
                try
                    CanvasExtractor.getStateAndDependencies simSheet ldcs
                    |> Result.mapError Simulator.makeDummySimulationError
                    |> Result.bind (fun (_, state, deps) ->
                        Simulator.validateCircuitSimulation simSheet state deps)
                    |> Result.map (FastCreate.gatherSimulation >> FastCreate.stepCostOfDesign)
                with e ->
                    Error (Simulator.makeDummySimulationError $"exception while pricing the design: {e.Message}")
            waveSimCostMemo <- Some(simSheet, ldcs, result)
            result


//------------------------------------------------------------------------------------------------//
//------------------------------------ Canvas inspection -----------------------------------------//
//------------------------------------------------------------------------------------------------//

(*
    What the draw block is showing, made readable from outside the app.

    MainView.displayView publishes both of the functions below as `window.issie` in debug builds,
    and scripts/inspect-canvas.js reads them over the Chrome DevTools Protocol. They answer
    questions the rendered SVG can only be used to guess at: where a symbol really is, how a wire
    is really routed, and whether a symbol is being drawn at a computed parameter value.

    canvasRaw is the complete answer and canvasInspection the legible one. Prefer canvasRaw when
    the question is "what is actually in there"; the summary exists because a whole BusWireT.Model
    is thousands of lines and most questions are answered by one line per symbol.
*)

/// One symbol as data: what it is, where it is, how big it is, how it is oriented, and where its
/// ports sit. Coordinates are diagram units, the same ones the SVG is drawn in.
let private symbolInspection (sym: DrawModelType.SymbolT.Symbol) =
    let comp = sym.Component
    let (ComponentId id) = sym.Id
    {|
        Id = id
        Label = comp.Label
        Type = string comp.Type
        // Pos is the live top-left; Component.X/Y agree with it only once layout has been stored
        X = sym.Pos.X
        Y = sym.Pos.Y
        W = comp.W
        H = comp.H
        HScale = Option.defaultValue 1.0 sym.HScale
        VScale = Option.defaultValue 1.0 sym.VScale
        Rotation = string sym.STransform.Rotation
        Flipped = sym.STransform.flipped
        IsClocked = sym.IsClocked
        IsAnnotation = Option.isSome sym.Annotation
        Ports =
            sym.PortMaps.Orientation
            |> Map.toArray
            |> Array.map (fun (portId, edge) -> {| Id = portId; Edge = string edge |})
    |}

/// One wire as data, with its segments in ABSOLUTE coordinates - the geometry actually drawn,
/// rather than the relative lengths the model stores.
let private wireInspection (wire: DrawModelType.BusWireT.Wire) =
    let (ConnectionId id) = wire.WId
    let (InputPortId toPort) = wire.InputPort
    let (OutputPortId fromPort) = wire.OutputPort
    {|
        Id = id
        FromPort = fromPort
        ToPort = toPort
        Width = wire.Width
        InitialOrientation = string wire.InitialOrientation
        Segments =
            BlockHelpers.getAbsSegments wire
            |> List.map (fun aSeg ->
                {|
                    Index = aSeg.Segment.Index
                    StartX = aSeg.Start.X
                    StartY = aSeg.Start.Y
                    EndX = aSeg.End.X
                    EndY = aSeg.End.Y
                    Mode = string aSeg.Segment.Mode
                    Draggable = aSeg.Segment.Draggable
                |})
            |> Array.ofList
    |}

/// Everything the draw block is showing on the open sheet, as data. Computed on demand, not on
/// every render: MainView publishes a function that calls this.
let canvasInspection (model: Model) =
    let sheet = model.Sheet
    {|
        Sheet = model.CurrentProj |> Option.map (fun p -> p.OpenFileName) |> Option.defaultValue ""
        Zoom = sheet.Zoom
        ScrollX = sheet.ScreenScrollPos.X
        ScrollY = sheet.ScreenScrollPos.Y
        Selected = sheet.SelectedComponents |> List.map (fun (ComponentId id) -> id) |> Array.ofList
        Symbols = sheet.Wire.Symbol.Symbols |> Map.toArray |> Array.map (snd >> symbolInspection)
        Wires = sheet.Wire.Wires |> Map.toArray |> Array.map (snd >> wireInspection)
    |}

module private RawDump =
    open Fable.SimpleJson
    let ofDrawBlock (wire: DrawModelType.BusWireT.Model) =
        Json.serialize<DrawModelType.BusWireT.Model> wire

/// The complete drawing state, serialised with the same library that writes .dgm files.
///
/// SimpleJson round-trips F# maps - including the ones keyed by a record or a single-case DU,
/// which it writes as an array of [key, value] pairs rather than as a JSON object - along with
/// sets, options and bigints. So there is no need to reduce anything by hand: this is every
/// symbol, wire, port and port map exactly as the model holds it.
///
/// BusWireT.Model is the largest part of the model that contains no functions at all. SheetT.Model
/// above it holds PopupViewFunc and a ChildProcess, and its undo and redo lists are whole models,
/// which would multiply the dump by the undo depth.
///
/// Returns a message rather than throwing if serialisation fails, so that a type SimpleJson cannot
/// represent degrades to a readable error instead of breaking the caller. Fable only: SimpleJson's
/// reflection does not work under .NET, which does not matter for renderer debug code.
let canvasRaw (model: Model) : string =
    try
        // scoped open, as in Helpers.JsonHelpers: unqualified `Json` is SimpleJson's DU type, and
        // it is the module of the same name that carries serialize
        RawDump.ofDrawBlock model.Sheet.Wire
    with e ->
        $"""{{"error": "serialisation failed: {e.Message}"}}"""
