(*
    ModelType.fs

    This module provides the type for the FRP UI.
    It could be put next to CommonTypes but non-UI modules should be agnostic of
    the FRP model and run independently of Fable
*)

module rec ModelType

open CommonTypes
open SimGraphTypes
open SimTypes
open TruthTableTypes
open Fable.React
open VerilogTypes
open Optics
open Optics.Operators

module Constants =
    /// waveform simulator constant here for WSHelpers.initialWSModel reference
    /// maybe better to have this with WaveSim and parametrise initilaWSModel?
    let initialWaveformColWidth = 650 - 20 - 20 - 20 - 130 - 100


/// Groups components together in the wave selection table.
/// NB: There are fields which are commented out: these can be added back in
/// later on if we want to group those components together by type rather than
/// separately by name.
type ComponentGroup =
    | WireLabel
    | InputOutput
    | Viewers
    | Buses
    | Gates
    | MuxDemux
    | Arithmetic
    | CustomComp
    | FFRegister
    | Memories
    | Component of string


/// control checkboxes in waveform simulator wave selection
type CheckBoxStyle =
    | PortItem of Wave * string
    | ComponentItem of FastComponent
    | GroupItem of ComponentGroup * string list
    | SheetItem of string list

type RightTab =
    | Properties
    | Catalogue
    | Simulation
    | Build
    | Transition // hack to make a transition from Simulation to Catalog without a scrollbar artifact

type SimSubTab =
    | StepSim
    | TruthTable
    | WaveSim

type MemoryEditorData = {
    OnlyDiff : bool // Only show diffs in Memory Diff Viewer.
    Address : bigint option // Only show the specified memory address.
    Start: bigint
    NumberBase : NumberBase
}

type ImportDecision =
    | Overwrite
    | Rename

/// Possible fields that may (or may not) be used in a dialog popup.
type PopupDialogData = {
    Text : string option;
    Int : int option;
    ImportDecisions : Map<string, ImportDecision option>
    Int2: bigint option
    Int3: bigint option
    ProjectPath: string
    MemorySetup : (int * int * InitMemData * string option) option // AddressWidth, WordWidth. 
    MemoryEditorData : MemoryEditorData option // For memory editor and viewer.
    Progress: PopupProgress option
    ConstraintTypeSel: ConstraintType option
    ConstraintIOSel: CellIO option
    ConstraintErrorMsg: string option
    NewConstraint: Constraint option
    AlgebraInputs: SimulationIO list option
    AlgebraError: SimulationError option
    VerilogCode: string option
    VerilogErrors: ErrorInfo list
    BadLabel: bool
    IntList: int list option;
    IntList2: int list option;
}

let text_ = Lens.create (fun a -> a.Text) (fun s a -> {a with Text = s})
let importDecisions_ = Lens.create (fun a -> a.ImportDecisions) (fun s a -> {a with ImportDecisions = s})
let int_ = Lens.create (fun a -> a.Int) (fun s a -> {a with Int = s})
let int2_ = Lens.create (fun a -> a.Int2) (fun s a -> {a with Int2 = s})
let int3_ = Lens.create (fun a -> a.Int3) (fun s a -> {a with Int3 = s})
let projectPath_ = Lens.create (fun a -> a.ProjectPath) (fun s a -> {a with ProjectPath = s})
let memorySetup_ = Lens.create (fun a -> a.MemorySetup) (fun s a -> {a with MemorySetup = s})
let memoryEditorData_ = Lens.create (fun a -> a.MemoryEditorData) (fun s a -> {a with MemoryEditorData = s})
let progress_ = Lens.create (fun a -> a.Progress) (fun s a -> {a with Progress = s})
let constraintTypeSel_ = Lens.create (fun a -> a.ConstraintTypeSel) (fun s a -> {a with ConstraintTypeSel = s})
let constraintIOSel_ = Lens.create (fun a -> a.ConstraintIOSel) (fun s a -> {a with ConstraintIOSel = s})
let constraintErrorMsg_ = Lens.create (fun a -> a.ConstraintErrorMsg) (fun s a -> {a with ConstraintErrorMsg = s})
let newConstraint_ = Lens.create (fun a -> a.NewConstraint) (fun s a -> {a with NewConstraint = s})
let algebraInputs_ = Lens.create (fun a -> a.AlgebraInputs) (fun s a -> {a with AlgebraInputs = s})
let algebraError_ = Lens.create (fun a -> a.AlgebraError) (fun s a -> {a with AlgebraError = s})
let verilogCode_ = Lens.create (fun a -> a.VerilogCode) (fun s a -> {a with VerilogCode = s})
let verilogErrors_ = Lens.create (fun a -> a.VerilogErrors) (fun s a -> {a with VerilogErrors = s})
let badLabel_ = Lens.create (fun a -> a.BadLabel) (fun s a -> {a with BadLabel = s})
let intlist_ = Lens.create (fun a -> a.IntList) (fun s a -> {a with IntList = s})
let intlist2_ = Lens.create (fun a -> a.IntList2) (fun s a -> {a with IntList2 = s})
type TopMenu = | Closed | Project | Files |TransientClosed

//==========//
// Messages //
//==========//



// Messages that will be triggered on key combinations.
type KeyboardShortcutMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL

type UICommandType =
    | CloseProject
    | ChangeSheet
    | RenameSheet
    | ImportSheet
    | DeleteSheet
    | AddSheet
    | SaveSheet
    | StartWaveSim
    | ViewWaveSim
    | CloseWaveSim

type FileCommandType =
    | FileImportSheet
    /// argument is true to save the current project
    | FileNewProject of bool
    /// argumnet is true to save the current project
    | FileOpenProject of bool 
    | FileCloseProject
    | FileSaveOpenFile
    | FileAddFile
    | FileShowDemos of ((string * int * int) list)
    
    
//---------------------------------------------------------------
//---------------------WaveSim types-----------------------------
//---------------------------------------------------------------

/// Determines whether the user is able to see the wave viewer pane.
/// Changes value depending on the state of the circuit and whether
/// the wave simulator has been run.
type WaveSimState =
    /// If the Wave Sim has not been before
    | Empty
    /// If no project is open
    | NoProject
    /// If there is an error in the circuit diagram
    | SimError of SimulationError
    /// If there is no sequential (clocked) logic in the circuit
    | NonSequential
    /// While waiting for the fast simulator to finish running
    | Loading
    /// If there are no errors in the circuit diagram
    | Success
    /// if waveSim has been explicitly ended
    | Ended

/// <summary>Describe WaveSim's scrollbar's mouse actions' type of operation.</summary>
type ScrollbarMouseAction =
    | StartScrollbarDrag
    | InScrollbarDrag
    | ClearScrollbarDrag
    | ReleaseScrollQueue

/// Identifies which Component and Port drives a waveform.
/// Must be an Output port (Input ports cannot drive waveforms).
type DriverT = {
    DriverId: FComponentId
    Port: OutputPortNumber
}

/// Information required to display a waveform.
type Wave = {
    /// Uniquely identifies a waveform
    WaveId: WaveIndexT
    /// First cycle displayed
    StartCycle: int
    /// Number of cycles displayed
    ShownCycles: int
    /// rate at which the simulated waveforms are sampled to achive very high zooms
    Multiplier: int
    /// width of one cycle: TODO - remove this and stretch SVGs to fit
    CycleWidth: float
    /// radix of waveform numbers
    Radix: NumberBase
    /// unique within design sheet (SheetId)
    /// [] for top-level waveform: path to sheet
    /// Currently unused.
    SheetId: ComponentId list
    SubSheet: string list // SheetId mapped to custom component names
    /// Wires connected to this waveform. Used to highlight wires
    /// when hovering over wave label.
    Conns: ConnectionId list
    /// Name shown in the waveform viewer. Not guaranteed to be unique.
    DisplayName: string
    /// Number of bits in wave
    ViewerDisplayName: string
    CompLabel: string
    PortLabel: string
    /// width of the waveform's bus
    Width: int
    /// Array indexed by clock cycle to show value of wave.
    DriverIndex: int
    /// SVG of waveform
    SVG: ReactElement option
}

let lastClock_ = Lens.create (fun a -> a.LastClock) (fun s a -> {a with LastClock = s})
let firstClock_ = Lens.create (fun a -> a.FirstClock) (fun s a -> {a with FirstClock = s})
let fontSize_ = Lens.create (fun a -> a.FontSize) (fun s a -> {a with FontSize = s})
let fontWeight_ = Lens.create (fun a -> a.FontWeight) (fun s a -> {a with FontWeight = s})

/// Contains all information required by waveform simulator.
/// One WaveSimModel per sheet.
type WaveSimModel = {
    /// default value for cursor in waveform Simulator
    DefaultCursor : CursorType
    /// Configuration for the waveform simulator.//
    WSConfig: WSConfig
    /// temp copy of configuration used by dialog
    WSConfigDialog: WSConfig option
    /// Current state of WaveSimModel.
    State: WaveSimState
    /// Top-level sheet for current waveform simulation: copy of model.WaveSimSheet when simulation is running
    TopSheet: string
    /// Copy of all sheets used with reduced canvasState as simulated
    Sheets: Map<string,CanvasState>
    /// Map of all simulatable waves
    AllWaves: Map<WaveIndexT, Wave>
    /// List of which waves are currently visible in the waveform viewer.
    SelectedWaves: WaveIndexT list
    /// Left-most visible clock cycle.
    /// this is scaled by CycleMultiplier, and therefore not the real clock cycle
    /// for sampling zoom > 1X.
    StartCycle: int
    /// Total number of visible clock cycles.
    /// This is scaled by cycleMultiplier, and therefore not the real clock cycle
    /// for sampling zoom > 1X.
    ShownCycles: int
    /// Used for extreme zoom out. Sample waveforms every this number of cycles. Display sampled data.
    SamplingZoom: int
    /// Current highlighted clock cycle displayed in the waveform viewer.
    /// This is scaled by CycleMultiplier, and therefore not the real clock cycle
    /// for sampling zoom > 1X.
    CursorDisplayCycle: int
    /// This is the real clock cycle of the cursor which determined the
    /// values column contents. If sampling, a single highlighted waveform cycle
    /// may represent multiple real clock cycles.
    CursorExactClkCycle: int
    /// True if no number in clcock cycle box (special case).
    ClkCycleBoxIsEmpty: bool
    /// Radix in which values are being displayed in the wave simulator.
    Radix: NumberBase
    /// Width of the waveform column.
    WaveformColumnWidth: float
    /// TODO: Should this be refactored into an ActiveModal type option?
    /// If the wave selection modal is visible.
    WaveModalActive: bool
    /// If the ram selection modal is visible.
    RamModalActive: bool
    /// List of RAM components on the sheet.
    RamComps: FComponentId list
    /// Map of which RAM components have been selected.
    SelectedRams: Map<FComponentId, string>
    /// If it exists this is the start location from which RAM locations are displayed.
    /// It is transient.
    /// The first component of the tuple is the text used to define the location.
    /// The second component is the actual location.
    RamStartLocation: Map<FComponentId, string * bigint>
    /// String which the user is searching the list of waves by.
    SearchString: string
    /// What is shown in wave sim sheet detail elements
    ShowSheetDetail: Set<string list>
    /// What is shown in wave sim component detail elements
    ShowComponentDetail: Set<FComponentId>
    /// What is shown in wave sim group detail elements
    ShowGroupDetail: Set<ComponentGroup * string list>    /// The label which a user is hovering over.
    HoveredLabel: WaveIndexT option
    /// The index of the wave which the user is dragging.
    DraggedIndex: WaveIndexT option
    /// The value of SelectedWaves when the user started dragging a label.
    /// Used to restore SelectedWaves if the user drops a label in an illegal location.
    PrevSelectedWaves: WaveIndexT list option

    // Scrollbar properties:
    /// <summary>Width of scrollbar's thumb, in pixels.</summary>
    ScrollbarTbWidth: float
    /// <summary>Starting position of scrollbar's thumb, in pixels.</summary>
    ScrollbarTbPos: float
    /// <summary>Offset between scrollbar's thumb's position and cursor's position.
    /// If is Some float, scrollbar is in drag mode; otherwise scrollbar is NOT in drag.</summary>
    ScrollbarTbOffset: float option
    /// <summary>Width of scrollbar's gray background, in pixels.</summary>
    ScrollbarBkgWidth: float
    /// <summary>Number of clock cycles scrollbar's background represents.</summary>
    ScrollbarBkgRepCycs: int
    /// <summary>Counter used to coalesce scrollbar mouse actions together.
    /// If true, queue is clear and can dispatch scrollbar update.
    /// Otherwise, an update is in progress and mouse event should not be pushed onto the queue.</summary>
    ScrollbarQueueIsEmpty: bool
}

let wSConfig_ = Lens.create (fun a -> a.WSConfig) (fun s a -> {a with WSConfig = s})
let ramStartLocation_ = Lens.create (fun a -> a.RamStartLocation) (fun s a -> {a with RamStartLocation = s})
let wSConfigDialog_ = Lens.create (fun a -> a.WSConfigDialog) (fun s a -> {a with WSConfigDialog = s})
let defaultCursor_ = Lens.create (fun a -> a.DefaultCursor) (fun s a -> {a with DefaultCursor = s})

type DiagEl = | Comp of Component | Conn of Connection

type DragMode = DragModeOn of int | DragModeOff

type IntMode = FirstInt | SecondInt | ThirdInt

type MenuCommand =
    | MenuPrint
    | MenuSaveFile
    | MenuSaveProjectInNewFormat
    | MenuNewFile
    | MenuExit
    | MenuZoom of float
    | MenuVerilogOutput
    | MenuLostFocus

type SimulationProgress =
    {
        InitialClock: int
        FinalClock: int
        ClocksPerChunk: int       
    }

type PopupProgress =
    {
        Value: int
        Max: int
        Title: string
        Speed: float
    }

type TTMsg =
    | GenerateTruthTable of option<Result<SimulationData,SimulationError> * CanvasState>
    | RegenerateTruthTable
    | FilterTruthTable
    | SortTruthTable
    | DCReduceTruthTable
    | HideTTColumns
    | CloseTruthTable
    | ClearInputConstraints
    | ClearOutputConstraints
    | AddInputConstraint of Constraint
    | AddOutputConstraint of Constraint
    | DeleteInputConstraint of Constraint
    | DeleteOutputConstraint of Constraint
    | ToggleHideTTColumn of CellIO
    | ClearHiddenTTColumns
    | ClearDCMap
    | SetTTSortType of (CellIO * SortType) option
    | MoveColumn of (CellIO * MoveDirection)
    | SetIOOrder of CellIO []
    | SetTTAlgebraInputs of SimulationIO list
    | SetTTBase of NumberBase
    | SetTTGridCache of ReactElement option
    | TogglePopupAlgebraInput of (SimulationIO * SimulationData)
    | SetPopupInputConstraints of ConstraintSet option
    | SetPopupOutputConstraints of ConstraintSet option
    | SetPopupConstraintTypeSel of ConstraintType option
    | SetPopupConstraintIOSel of CellIO option
    | SetPopupConstraintErrorMsg of string option
    | SetPopupNewConstraint of Constraint option
    | SetPopupAlgebraInputs of SimulationIO list option
    | SetPopupAlgebraError of SimulationError option



type Msg =
    | WaveSimKeyPress of string
    | ShowExitDialog
    | Sheet of DrawModelType.SheetT.Msg
    | UpdateUISheetTrail of (string list -> string list)
    | SheetBackAction of (Msg -> unit)
    | SynchroniseCanvas
    | JSDiagramMsg of JSDiagramMsg
    | KeyboardShortcutMsg of KeyboardShortcutMsg
    | Benchmark
    | StartSimulation of Result<SimulationData, SimulationError>
    /// Add WaveSimModel to Model.WaveSim map.
    /// String is name of current sheet.
    | AddWSModel of (string * WaveSimModel)
    /// Update the WaveSimModel of the current sheet.
    | SetWSModel of WaveSimModel
    /// Update the WaveSimModel of the specified sheet from update function
    | UpdateWSModel of (WaveSimModel -> WaveSimModel)
    /// Set the current WaveSimModel to the specified sheet
    /// and update the WaveSimModel of the specified sheet.
    | SetWSModelAndSheet of WaveSimModel * string
    /// Generate waveforms according to the current parameters
    /// of the given WaveSimModel
    | GenerateWaveforms of WaveSimModel
    /// Generate waveforms according to the model paramerts of Wavesim
    | GenerateCurrentWaveforms 
    /// Run, or rerun, the FastSimulation with the current state of the Canvas.
    | RefreshWaveSim of WaveSimModel
    /// Sets or clears ShowSheetDetail (clearing will remove all child values in the set)
    | SetWaveSheetSelectionOpen of (string list list * bool)
    /// Sets or clears ShowComponentDetail
    | SetWaveComponentSelectionOpen of (FComponentId list * bool)
    /// Sets or clears GroupDetail
    | SetWaveGroupSelectionOpen of ((ComponentGroup * string list) list * bool)
    | LockTabsToWaveSim
    | UnlockTabsFromWaveSim
    | TryStartSimulationAfterErrorFix of SimSubTab
    | SetSimulationGraph of SimulationGraph  * FastSimulation
    | SetSimulationBase of NumberBase
    | IncrementSimulationClockTick of int
    | EndSimulation
    /// Clears the Model.WaveSim and Model.WaveSimSheet fields.
    | EndWaveSim
    | TruthTableMsg of TTMsg // all the messages used by the truth table code
    | ChangeRightTab of RightTab
    | ChangeSimSubTab of SimSubTab
    | SetHighlighted of ComponentId list * ConnectionId list
    | SetSelWavesHighlighted of ConnectionId array
    | SetClipboard of CanvasState
    | SetCreateComponent of Component
    | SetProject of Project
    | UpdateProject of (Project -> Project)
    | UpdateModel of (Model -> Model)
    | DispatchDelayed of (int * Msg)
    | UpdateImportDecisions of Map<string, ImportDecision option>
    | UpdateProjectWithoutSyncing of (Project->Project)
    | ShowPopup of ((Msg -> Unit) -> Model -> ReactElement)
    | ShowStaticInfoPopup of (string * ReactElement * (Msg -> Unit))
    | ClosePopup
    | SetPopupDialogText of string option
    | SetPopupDialogBadLabel of bool
    | SetPopupDialogCode of string option
    | SetPopupDialogVerilogErrors of ErrorInfo list
    | SetPopupDialogInt of int option
    | SetPopupDialogInt2 of bigint option
    | SetPopupDialogInt3 of bigint option
    | SetPopupDialogTwoInts of (bigint option * IntMode * string option)
    | SetPopupDialogIntList of int list option
    | SetPopupDialogIntList2 of int list option
    | SetPropertiesExtraDialogText of string option
    | SetPopupDialogMemorySetup of (int * int * InitMemData * string option) option
    | SetPopupMemoryEditorData of MemoryEditorData option
    | SetPopupProgress of PopupProgress option
    | UpdatePopupProgress of (PopupProgress -> PopupProgress)
    | SimulateWithProgressBar of SimulationProgress
    | SetSelectedComponentMemoryLocation of bigint * bigint
    | CloseDiagramNotification
    | SetSimulationNotification of ((Msg -> unit) -> ReactElement)
    | CloseSimulationNotification
    | CloseWaveSimNotification
    | SetFilesNotification of ((Msg -> unit) -> ReactElement)
    | CloseFilesNotification
    | SetMemoryEditorNotification of ((Msg -> unit) -> ReactElement)
    | CloseMemoryEditorNotification
    | SetPropertiesNotification of ((Msg -> unit) -> ReactElement)
    | ClosePropertiesNotification
    | SetTopMenu of TopMenu
    | ReloadSelectedComponent of int
    | SetDragMode of DragMode
    | ChangeBuildTabVisibility
    /// Set width of right-hand pane when tab is WaveSimulator or TruthTable
    | SetViewerWidth of int
    | MenuAction of MenuCommand * (Msg -> unit)
    | DiagramMouseEvent
    | SelectionHasChanged
    | SetIsLoading of bool
    | SetRouterInteractive of bool
    | CloseApp
    | SetExitDialog of bool
    | ExecutePendingMessages of int
    | DoNothing
    | StartUICmd of UICommandType
    | FinishUICmd
    | FileCommand of FileCommandType * (Msg -> Unit)
    | ReadUserData of string
    | SetUserData of UserData
    | SetThemeUserData of DrawModelType.SymbolT.ThemeType
    | ExecCmd of Elmish.Cmd<Msg>
    | ExecFuncInMessage of (Model -> (Msg->Unit) -> Unit) * (Msg -> Unit)
    | ExecFuncAsynch of (Unit -> Elmish.Cmd<Msg>)
    | ExecCmdAsynch of Elmish.Cmd<Msg>
    | SendSeqMsgAsynch of seq<Msg>
    | ContextMenuAction of e: Browser.Types.MouseEvent
    | ContextMenuItemClick of menuType:string * item:string * dispatch: (Msg -> unit)
    | ScrollbarMouseMsg of cursor:float * action:ScrollbarMouseAction * dispatch:(Msg->unit)
    | SaveModel
    | CheckMemory
    | ChangeWaveSimMultiplier of int
    | RunAfterRender of (bool * ((Msg -> unit) -> Model -> Model))


//================================//
// Componenents loaded from files //
//================================//

type Notifications = {
    FromDiagram : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromSimulation : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromWaveSim : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromFiles : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromMemoryEditor : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromProperties : ((Msg -> unit) -> Fable.React.ReactElement) option
}

let fromDiagram_ = Lens.create (fun n -> n.FromDiagram) (fun s n -> {n with FromDiagram = s})
let fromSimulation_ = Lens.create (fun n -> n.FromSimulation) (fun s n -> {n with FromSimulation = s})
let fromWaveSim_ = Lens.create (fun n -> n.FromWaveSim) (fun s n -> {n with FromWaveSim = s})
let fromFiles_ = Lens.create (fun n -> n.FromFiles) (fun s n -> {n with FromFiles = s})
let fromMemoryEditor_ = Lens.create (fun n -> n.FromMemoryEditor) (fun s n -> {n with FromMemoryEditor = s})
let fromProperties_ = Lens.create (fun n -> n.FromProperties) (fun s n -> {n with FromProperties = s})


type UserData = {
    /// Where to save the persistent app data
    UserAppDir : string option
    LastUsedDirectory: string option
    RecentProjects: string list option
    ArrowDisplay: bool
    WireType: DrawModelType.BusWireT.WireType
    Theme: DrawModelType.SymbolT.ThemeType
    }



type SpinPayload = {
    /// if false do not show progress bat screen, but still show spinner in button.
    UseProgressBar: bool
    /// text displayed with progress bar
    Name: string
    /// ToDo / Total = progress bar level
    ToDo: int
    /// ToDo / Total = progress bar level 
    Total: int
    }

type TTType = {
    /// bits associated with the maximum number of input rows allowed in a Truth Table
    BitLimit: int
    /// input constraints on truth table generation
    InputConstraints: ConstraintSet
    /// output constraints on truth table viewing
    OutputConstraints: ConstraintSet
    /// which output or viewer columns in the Truth Table should be hidden
    HiddenColumns: CellIO list
    /// by which IO and in what way is the Table being sorted
    SortType: (CellIO * SortType) option
    /// what is the display order of IOs in Table
    IOOrder: CellIO []
    /// Grid Styles for each column in the Table
    GridStyles: Map<CellIO,Props.CSSProp list>
    /// Cached CSS Grid for displaying the Truth Table
    GridCache: ReactElement option
    /// which of the Truth Table's inputs are currently algebra
    AlgebraIns: SimulationIO list
}
let gridStyles_ = Lens.create (fun a -> a.GridStyles) (fun s a -> {a with GridStyles = s})
let ioOrder_ = Lens.create (fun a -> a.IOOrder) (fun s a -> {a with IOOrder = s})
let inputConstraints_ = Lens.create (fun a -> a.InputConstraints) (fun s a -> {a with InputConstraints = s})
let outputConstraints_ = Lens.create (fun a -> a.OutputConstraints) (fun s a -> {a with OutputConstraints = s})
let hiddenColumns_ = Lens.create (fun a -> a.HiddenColumns) (fun s a -> {a with HiddenColumns = s})
let sortType_ = Lens.create (fun a -> a.SortType) (fun s a -> {a with SortType = s})
let algebraIns_ = Lens.create (fun a -> a.AlgebraIns) (fun s a -> {a with AlgebraIns = s})
let gridCache_ = Lens.create (fun a -> a.GridCache) (fun s a -> {a with GridCache = s})

type RunData = {
    ButtonSpinnerOn: bool
    FnToRun: ((Msg -> unit) -> (Model -> Model))
}

type Model = {
    /// remember from last mouse movement which side of grey divider it is on.
    MousePointerIsOnRightSection: bool
    /// Function to be run after rendering to update the model
    RunAfterRenderWithSpinner: RunData option
    /// User data for the application
    UserData: UserData
    /// Map of sheet name to WaveSimModel
    WaveSim : Map<string, WaveSimModel>

    /// which top-level sheet is used by wavesim
    WaveSimSheet: string option

    /// A breadcrumb-like trail of visited sheets used for UI back button
    UISheetTrail: string list

    /// If the application has a modal spinner waiting for simulation
    Spinner: (Model -> Model) option
        
    /// Draw Canvas
    Sheet: DrawModelType.SheetT.Model

    /// true during period when a sheet or project is loading
    IsLoading: bool

    /// last time check for changes was made
    LastChangeCheckTime: float

    /// top-level canvas used for current wave simulation
    LastSimulatedCanvasState: CanvasState option // reduced (without layout) canvas state
    /// used to determine whether current canvas has been saved (includes any change)
    LastDetailedSavedState: CanvasState
    /// components and connections currently selected

    CurrentSelected: Component list * Connection list
    /// component ids and connection ids previously selected (used to detect changes)
    LastSelectedIds: string list * string list
    /// last used bus width in bits - used as default in next component create dialog
    LastUsedDialogWidth: int
    /// component currently selected in properties dialog
    SelectedComponent : Component option // None if no component is selected.
    /// used during step simulation: simgraph for current clock tick
    CurrentStepSimulationStep : Result<SimulationData,SimulationError> option // None if no simulation is running.
    /// stores the generated truth table 
    CurrentTruthTable: Result<TruthTable,SimulationError> option // None if no Truth Table is being displayed.
    /// style info for the truth table
    TTConfig: TTType
    /// which of the tabbed panes is currently visible
    RightPaneTabVisible : RightTab
    /// which of the subtabs for the right pane simulation is visible
    SimSubTabVisible: SimSubTab
    /// components and connections which are highlighted
    Hilighted : (ComponentId list * ConnectionId list) * ConnectionId list
    /// Components and connections that have been selected and copied.
    Clipboard : CanvasState 
    /// Track the last added component
    LastCreatedComponent : Component option 
    /// used to enable "SAVE" button
    SavedSheetIsOutOfDate : bool
    /// the project contains, as loadable components, the state of each of its sheets
    CurrentProj : Project option
    /// function to create popup pane if present
    PopupViewFunc : ((Msg -> Unit) -> Model -> Fable.React.ReactElement) option
    /// function to create spinner popup pane if present (overrides otehr popups)
    SpinnerPayload : SpinPayload option
    /// data to populate popup (may not all be used)
    PopupDialogData : PopupDialogData
    /// record containing functions that create react elements of notifications
    Notifications : Notifications
    /// State of menus for sheets, projects etc
    TopMenuOpenState : TopMenu
    /// used to determine whether mouse is currently dragging the divider, or used normally
    DividerDragMode: DragMode
    /// viewer width in pixels altered by dragging the divider
    WaveSimViewerWidth: int
    /// if true highlight connections from wavesim editor
    ConnsOfSelectedWavesAreHighlighted: bool
    /// Contains a list of pending messages
    Pending: Msg list
    /// Bad way to tidy up the messy UI commands - better - do them all in just one message!
    UIState: UICommandType Option
    /// if true the "build" tab appears on the RHS
    BuildVisible: bool
} 

    with member this.WaveSimOrCurrentSheet =
            match this.WaveSimSheet, this.CurrentProj with
            | None, Some {OpenFileName = name} -> name
            | Some name, _ -> name
            | None, None -> failwithf "What? Project is not open cannot guess sheet!"

let waveSimSheet_ = Lens.create (fun a -> a.WaveSimSheet) (fun s a -> {a with WaveSimSheet = s})
let waveSim_ = Lens.create (fun a -> a.WaveSim) (fun s a -> {a with WaveSim = s})

let runAfterRender_ = Lens.create (fun a -> a.RunAfterRenderWithSpinner) (fun s a -> {a with RunAfterRenderWithSpinner = s})
let rightPaneTabVisible_ = Lens.create (fun a -> a.RightPaneTabVisible) (fun s a -> {a with RightPaneTabVisible = s})
let simSubTabVisible_ = Lens.create (fun a -> a.SimSubTabVisible) (fun s a -> {a with SimSubTabVisible = s})
let buildVisible_ = Lens.create (fun a -> a.BuildVisible) (fun s a -> {a with BuildVisible = s})
let popupViewFunc_ = Lens.create (fun a -> a.PopupViewFunc) (fun s a -> {a with PopupViewFunc = s})

let sheet_ = Lens.create (fun a -> a.Sheet) (fun s a -> {a with Sheet = s})
let tTType_ = Lens.create (fun a -> a.TTConfig) (fun s a -> {a with TTConfig = s})
let currentStepSimulationStep_ = Lens.create (fun a -> a.CurrentStepSimulationStep) (fun s a -> {a with CurrentStepSimulationStep = s})
let currentTruthTable_ = Lens.create (fun a -> a.CurrentTruthTable) (fun s a -> {a with CurrentTruthTable = s})
let popupDialogData_ = Lens.create (fun a -> a.PopupDialogData) (fun p a -> {a with PopupDialogData = p})
let selectedComponent_ = Lens.create (fun a -> a.SelectedComponent) (fun s a -> {a with SelectedComponent = s})
let userData_ = Lens.create (fun a -> a.UserData) (fun s a -> {a with UserData = s})
let uISheetTrail_ = Lens.create (fun a -> a.UISheetTrail) (fun s a -> {a with UISheetTrail = s})
let savedSheetIsOutOfDate_ = Lens.create (fun a -> a.SavedSheetIsOutOfDate) (fun s a -> {a with SavedSheetIsOutOfDate = s})

let currentProj_ = Lens.create (fun a -> a.CurrentProj) (fun s a -> {a with CurrentProj = s})
let openLoadedComponentOfModel_ = currentProj_ >-> Optics.Option.value_ >?> openLoadedComponent_
let notifications_ = Lens.create (fun a -> a.Notifications) (fun s a -> {a with Notifications = s})
let project_ = Lens.create (fun a -> Option.get (a.CurrentProj)) (fun s a -> {a with CurrentProj = Some s})
let projectOpt_ = Prism.create (fun a -> a.CurrentProj) (fun s a -> {a with CurrentProj =  a.CurrentProj |> Option.map (fun _ -> s)})
let ldcM = project_ >-> loadedComponents_
let ldcOptM = projectOpt_ >?> loadedComponents_
let nameM = project_ >-> openFileName_
let nameOptM = projectOpt_ >?> openFileName_

/// Update the WaveSimModel of the current waveSim sheet.
let putWaveSim (wsm: WaveSimModel) (model: Model) =
    match model.WaveSimSheet with
    | None -> model
    | Some sheet -> 
        model
        |> Optic.map waveSim_ (Map.add sheet wsm)






