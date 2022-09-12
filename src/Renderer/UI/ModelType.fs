(*
    ModelType.fs

    This module provides the type for the FRP UI.
    It could be put next to CommonTypes but non-UI modules should be agnostic of
    the FRP model and run independently of Fable
*)

module rec ModelType

open CommonTypes
open SimulatorTypes
open TruthTableTypes
open Fable.React
open Sheet.SheetInterface
open VerilogTypes

module Constants =
    /// DiagramStyle.rightSectinoWidthL = 650,
    /// WaveSimStyle.Constants.leftMargin = 50,
    /// WaveSimStyle.Constants.rightMargin = 50,
    /// 2 * MainView.Constants.dividerBarWidth = 20,
    /// WaveSimStyle.namesColWidth = 200,
    /// WaveSimStyle.valeusColWidth = 100,
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
    Address : int64 option // Only show the specified memory address.
    Start: int64
    NumberBase : NumberBase
}

/// Possible fields that may (or may not) be used in a dialog popup.
type PopupDialogData = {
    Text : string option;
    Int : int option;
    Int2: int64 option
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
}

let progress_ = Optics.Lens.create (fun a -> a.Progress) (fun s a -> {a with Progress = s})

type TopMenu = | Closed | Project | Files

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
    | DeleteSheet
    | AddSheet
    | SaveSheet
    | StartWaveSim
    | ViewWaveSim
    | CloseWaveSim
    
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
    Width: int
    /// TODO: Consider changing to a map keyed by clock cycle.
    /// List indexed by clock cycle to show value of wave.
    WaveValues: StepArray<FData>
    /// SVG of waveform
    SVG: ReactElement option
}

/// Contains all information required by waveform simulator.
/// One WaveSimModel per sheet.
type WaveSimModel = {
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
    StartCycle: int
    /// Total number of visible clock cycles.
    ShownCycles: int
    /// Current highlighted clock cycle.
    CurrClkCycle: int
    /// If the user is typing a clock cycle in but erases the contents of the box.
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
    RamComps: FastComponent list
    /// Map of which RAM components have been selected.
    SelectedRams: Map<FComponentId, string>
    /// FastSimulation used in the wave simulator.
    FastSim: FastSimulation
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
}



type DiagEl = | Comp of Component | Conn of Connection

type DragMode = DragModeOn of int | DragModeOff

type IntMode = FirstInt | SecondInt

type MenuCommand =
    | MenuPrint
    | MenuSaveFile
    | MenuNewFile
    | MenuExit
    | MenuZoom of float
    | MenuVerilogOutput

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

type Msg =
    | ShowExitDialog
    | Sheet of DrawModelType.SheetT.Msg
    | JSDiagramMsg of JSDiagramMsg
    | KeyboardShortcutMsg of KeyboardShortcutMsg
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
    /// of the WaveSimModel
    | GenerateWaveforms of WaveSimModel
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
    | SetSimulationGraph of SimulationGraph  * FastSimulation
    | SetSimulationBase of NumberBase
    | IncrementSimulationClockTick of int
    | EndSimulation
    /// Clears the Model.WaveSim and Model.WaveSimSheet fields.
    | EndWaveSim
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
    | ChangeRightTab of RightTab
    | ChangeSimSubTab of SimSubTab
    | SetHighlighted of ComponentId list * ConnectionId list
    | SetSelWavesHighlighted of ConnectionId array
    | SetClipboard of CanvasState
    | SetCreateComponent of Component
    | SetProject of Project
    | UpdateProject of (Project -> Project)
    | UpdateModel of (Model -> Model)
    | UpdateProjectWithoutSyncing of (Project->Project)
    | ShowPopup of ((Msg -> Unit) -> PopupDialogData -> ReactElement)
    | ShowStaticInfoPopup of (string * ReactElement * (Msg -> Unit))
    | ClosePopup
    | SetPopupDialogText of string option
    | SetPopupDialogBadLabel of bool
    | SetPopupDialogCode of string option
    | SetPopupDialogVerilogErrors of ErrorInfo list
    | SetPopupDialogInt of int option
    | SetPopupDialogInt2 of int64 option
    | SetPopupDialogTwoInts of (int64 option * IntMode * string option)
    | SetPropertiesExtraDialogText of string option
    | SetPopupDialogMemorySetup of (int * int * InitMemData * string option) option
    | SetPopupMemoryEditorData of MemoryEditorData option
    | SetPopupProgress of PopupProgress option
    | UpdatePopupProgress of (PopupProgress -> PopupProgress)
    | SetPopupInputConstraints of ConstraintSet option
    | SetPopupOutputConstraints of ConstraintSet option
    | SetPopupConstraintTypeSel of ConstraintType option
    | SetPopupConstraintIOSel of CellIO option
    | SetPopupConstraintErrorMsg of string option
    | SetPopupNewConstraint of Constraint option
    | SetPopupAlgebraInputs of SimulationIO list option
    | SetPopupAlgebraError of SimulationError option
    | TogglePopupAlgebraInput of (SimulationIO * SimulationData)
    | SimulateWithProgressBar of SimulationProgress
    | SetSelectedComponentMemoryLocation of int64 * int64
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
    | ReadUserData of string
    | SetUserData of UserData
    | SetThemeUserData of DrawModelType.SymbolT.ThemeType
    | ExecCmd of Elmish.Cmd<Msg>
    | ExecFuncInMessage of (Model -> (Msg->Unit) -> Unit) * (Msg -> Unit)
    | ExecFuncAsynch of (Unit -> Elmish.Cmd<Msg>)
    | ExecCmdAsynch of Elmish.Cmd<Msg>
    | SendSeqMsgAsynch of seq<Msg>


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

type UserData = {
    /// Where to save the persistent app data
    UserAppDir : string option
    LastUsedDirectory: string option
    RecentProjects: string list option
    ArrowDisplay: bool
    WireType: DrawModelType.BusWireT.WireType
    Theme: DrawModelType.SymbolT.ThemeType
    }

type SpinnerState =
   | WaveSimSpinner

type SpinPayload = {
    Payload: Model -> Model
    Name: string
    ToDo: int
    Total: int
    }

type Model = {
    UserData: UserData
    /// Map of sheet name to WaveSimModel
    WaveSim : Map<string, WaveSimModel>

    /// which top-level sheet is used by wavesim
    WaveSimSheet: string option

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
    /// bits associated with the maximum number of input rows allowed in a Truth Table
    TTBitLimit: int
    /// input constraints on truth table generation
    TTInputConstraints: ConstraintSet
    /// output constraints on truth table viewing
    TTOutputConstraints: ConstraintSet
    /// which output or viewer columns in the Truth Table should be hidden
    TTHiddenColumns: CellIO list
    /// by which IO and in what way is the Table being sorted
    TTSortType: (CellIO * SortType) option
    /// what is the display order of IOs in Table
    TTIOOrder: CellIO []
    /// Grid Styles for each column in the Table
    TTGridStyles: Map<CellIO,Props.CSSProp list>
    /// Cached CSS Grid for displaying the Truth Table
    TTGridCache: ReactElement option
    /// which of the Truth Table's inputs are currently algebra
    TTAlgebraInputs: SimulationIO list
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
    PopupViewFunc : ((Msg -> Unit) -> PopupDialogData -> Fable.React.ReactElement) option
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
    UIState: UICommandType Option
    /// if true the "build" tab appears on the RHS
    BuildVisible: bool
} 

    with member this.WaveSimOrCurrentSheet =
            match this.WaveSimSheet, this.CurrentProj with
            | None, Some {OpenFileName = name} -> name
            | Some name, _ -> name
            | None, None -> failwithf "What? Project is not open cannot guess sheet!"


let sheet_ = Optics.Lens.create (fun a -> a.Sheet) (fun s a -> {a with Sheet = s})
let popupDialogData_ = Optics.Lens.create (fun a -> a.PopupDialogData) (fun p a -> {a with PopupDialogData = p})



    