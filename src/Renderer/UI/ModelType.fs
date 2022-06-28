(*
    ModelType.fs

    This module provides the type for the FRP UI.
    It could be put next to CommonTypes but non-UI modules should be agnostic of
    the FRP model and run independently of Fable
*)

module rec ModelType

open CommonTypes
open SimulatorTypes
open Fable.React
open Sheet.SheetInterface

type RightTab =
    | Properties
    | Catalogue
    | Simulation

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


type SheetWave = {
    // path to sheet from simulation graph root
    Path: ComponentId list
    Sheet: string
    CSort: string
    Label: string
    }


type MoreWaveSetup = SheetWave list * Set<ComponentId list>
   

/// Possible fields that may (or may not) be used in a dialog popup.
type PopupDialogData = {
    Text : string option;
    Int : int option;
    Int2: int64 option
    ProjectPath: string
    MemorySetup : (int * int * InitMemData * string option) option // AddressWidth, WordWidth. 
    MemoryEditorData : MemoryEditorData option // For memory editor and viewer.
    WaveSetup: MoreWaveSetup option
    Progress: PopupProgress option
}

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

(*
WaveSim state.

Principles: 
1) at any time wavesim simulates a stored model.LastSimulatedCanvas circuit which is guaranteed working if it exists 
2) pressing "simulate button" updates this circuit
3) wavesim has two views: editor and waveforms. each waveform is the signal on a NetGroup (set of connections from one driver to multiple inputs).
4) waveforms display view based on: selected waveforms, zoom, cursor position, etc
5) simulation is rerun automatically as needed to generate current display
6) editor view interfaces with current circuit, colouring selected nets green, and allowing selection on nets to determine
default selected waveforms. If current circuit has changed only driving components still on circuit can be used this way.
7) simulate button color determines status; if circuit has chnaged from that simulated it will be orange (errors) or green (OK to rerun simulation).
8) list of currently displayed ports is held in state and saved / restored with each sheet. LastSimulatedCanvas (and simulation data) are not saved/restored but
are recalculated when needed. List of possible to display ports used by waveadder

Data structures for internal state

SimParams: parameters that can be changed during simulation of one ckt that affect what is displayed.

*)



type WaveName = string

type Wire = {
    NBits: uint32
    BitData: bigint 
}

type StateSample = string array
type Sample = | Wire of Wire | StateSample of StateSample
type SimTime = Sample array
type Waveform = Sample array


type WaveSimState = 
    | WSClosed
    | WSOpen

type DriverT = {
    DriverId: FComponentId
    Port: OutputPortNumber
}

type Wave = {
    WaveId: WaveIndexT
    Type: ComponentType
    CompLabel: string
    // unique within one simulation run, mostly conserved across runs
    // WaveId: string
    // unique within design sheet (SheetId)
    // [] for top-level waveform: path to sheet
    SheetId: ComponentId list
    // This is used to key the AllWaves map, since this is guaranteed to be unique.
    Driver: DriverT
    DisplayName: string
    // Number of bits in wave
    Width: int
    // Map keyed by clock cycle
    WaveValues: WireData list
    // Store SVG cache here maybe?
    Polylines: ReactElement list option
}

type WaveSimModel = {
    State: WaveSimState
    // List of all simulatable waves
    AllWaves: Map<WaveIndexT, Wave>
    SelectedWaves: WaveIndexT list
    StartCycle: int
    ShownCycles: int
    CurrClkCycle: int
    ClkCycleBoxIsEmpty: bool
    Radix: NumberBase
    WaveformColumnWidth: int
    WaveModalActive: bool
    RamModalActive: bool
    RamComps: Component list
    SelectedRams: Map<ComponentId, string>
    FastSim: FastSimulation
    SearchString: string
    HoveredLabel: WaveIndexT option
}

/// TODO: Decide a better number then move to Constants module.
/// TODO: Explain why: 30*width, width is 1.5, so that's 45. This is 8 cycles (0 to 7)
/// This should be divisible by 45
let initialWaveformColWidth = int( 1.5 * float (30 * 7)) //rightSectionWidthViewerDefault - namesColMinWidth - valuesColMinWidth

let initWSModel : WaveSimModel = {
    State = WSClosed
    AllWaves = Map.empty
    SelectedWaves = List.empty
    StartCycle = 0
    ShownCycles = 6
    CurrClkCycle = 0
    ClkCycleBoxIsEmpty = false
    Radix = Hex
    WaveformColumnWidth = initialWaveformColWidth
    WaveModalActive = false
    RamModalActive = false
    RamComps = []
    SelectedRams = Map.empty
    FastSim = FastCreate.emptyFastSimulation ()
    SearchString = ""
    HoveredLabel = None
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
    | AddWSModel of (string * WaveSimModel)
    | SetWSModel of WaveSimModel
    | SetWSModelAndSheet of WaveSimModel * string
    | InitiateWaveSimulation of WaveSimModel
    | SetSimulationGraph of SimulationGraph  * FastSimulation
    | SetSimulationBase of NumberBase
    | IncrementSimulationClockTick of int
    | EndSimulation
    | EndWaveSim
    | ChangeRightTab of RightTab
    | ChangeSimSubTab of SimSubTab
    | SetHighlighted of ComponentId list * ConnectionId list
    | SetSelWavesHighlighted of ConnectionId array
    | SetClipboard of CanvasState
    | SetCreateComponent of Component
    | SetProject of Project
    | UpdateProject of (Project -> Project)
    | UpdateProjectWithoutSyncing of (Project->Project)
    | ShowPopup of ((Msg -> Unit) -> PopupDialogData -> ReactElement)
    | ShowStaticInfoPopup of (string * ReactElement * (Msg -> Unit))
    | ClosePopup
    | SetPopupDialogText of string option
    | SetPopupDialogInt of int option
    | SetPopupDialogTwoInts of (int64 option * IntMode * string option)
    | SetPropertiesExtraDialogText of string option
    | SetPopupDialogMemorySetup of (int * int * InitMemData * string option) option
    | SetPopupMemoryEditorData of MemoryEditorData option
    | SetPopupWaveSetup of MoreWaveSetup
    | SetPopupProgress of PopupProgress option
    | UpdatePopupProgress of (PopupProgress -> PopupProgress)
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
    }

type Model = {
    UserData: UserData

    WaveSim : Map<string, WaveSimModel>
    // which top-level sheet is used by wavesim
    WaveSimSheet: string
        
    // Draw Canvas
    Sheet: DrawModelType.SheetT.Model

    // true during period when a sheet or project is loading
    IsLoading: bool

    // last time check for changes was made
    LastChangeCheckTime: float

    // top-level canvas used for current wave simulation
    LastSimulatedCanvasState: CanvasState option // reduced (without layout) canvas state
    // used to determine whether current canvas has been saved (includes any change)
    LastDetailedSavedState: CanvasState
    // components and connections currently selected
    CurrentSelected: Component list * Connection list
    // component ids and connection ids previously selected (used to detect changes)
    LastSelectedIds: string list * string list
    // last used bus width in bits - used as default in next component create dialog
    LastUsedDialogWidth: int
    // component currently selected in properties dialog
    SelectedComponent : Component option // None if no component is selected.
    // used during step simulation: simgraph for current clock tick
    CurrentStepSimulationStep : Result<SimulationData,SimulationError> option // None if no simulation is running.
    // which of the tabbed panes is currently visible
    RightPaneTabVisible : RightTab
    // which of the subtabs for the right pane simulation is visible
    SimSubTabVisible: SimSubTab
    // components and connections which are highlighted
    Hilighted : (ComponentId list * ConnectionId list) * ConnectionId list
    // Components and connections that have been selected and copied.
    Clipboard : CanvasState 
    // Track the last added component
    LastCreatedComponent : Component option 
    // used to enable "SAVE" button
    SavedSheetIsOutOfDate : bool
    // the project contains, as loadable components, the state of each of its sheets
    CurrentProj : Project option
    // function to create popup pane if present
    PopupViewFunc : ((Msg -> Unit) -> PopupDialogData -> Fable.React.ReactElement) option
    // data to populate popup (may not all be used)
    PopupDialogData : PopupDialogData
    // record containing functions that create react elements of notifications
    Notifications : Notifications
    // State of menus for sheets, projects etc
    TopMenuOpenState : TopMenu
    // used to determine whether mouse is currently dragging the divider, or used normally
    DividerDragMode: DragMode
    // viewer width in pixels altered by dragging the divider
    WaveSimViewerWidth: int
    // if true highlight connections from wavesim editor
    ConnsOfSelectedWavesAreHighlighted: bool
    // true if wavesim scroll position needs checking
    Pending: Msg list
    UIState: UICommandType Option
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
                }}}

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
         CurrProject = match this.PopupViewFunc with None -> false | _ -> true
         PopupDialogData = this.PopupDialogData
         DragMode = this.DividerDragMode
         ViewerWidth = this.WaveSimViewerWidth
 |} 

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

/// get saveable record of waveform setup
let getSavedWaveInfo (wsModel: WaveSimModel) : SavedWaveInfo =
    {
        SelectedWaves = Some wsModel.SelectedWaves
        Radix = Some wsModel.Radix
        WaveformColumnWidth = Some wsModel.WaveformColumnWidth
        ShownCycles = Some wsModel.ShownCycles
        SelectedRams = Some wsModel.SelectedRams

        // The following fields are from the old waveform simulator.
        ClkWidth = None
        Cursor = None
        LastClk = None
        DisplayedPortIds = None
    }

/// setup current WaveSimModel from saved record
/// currently only the set of nets displayed by default and the radix is actually preserved
/// TODO: work out better idea for what should be preserved here.
/// NB - note that SavedWaveInfo can only be changed if code is added to make loading backwards compatible with
/// old designs
let loadWSModelFromSavedWaveInfo (swInfo: SavedWaveInfo) : WaveSimModel =
    {
        initWSModel with
            SelectedWaves = Option.defaultValue initWSModel.SelectedWaves swInfo.SelectedWaves
            Radix = Option.defaultValue initWSModel.Radix swInfo.Radix
            WaveformColumnWidth = Option.defaultValue initWSModel.WaveformColumnWidth swInfo.WaveformColumnWidth
            ShownCycles = Option.defaultValue initWSModel.ShownCycles swInfo.ShownCycles
            SelectedRams = Option.defaultValue initWSModel.SelectedRams swInfo.SelectedRams
    }

//----------------------Print functions-----------------------------//
//------------------------------------------------------------------//

let spComp (comp:Component) =
    match comp.Type with
    | Custom {Name=name; InputLabels=il; OutputLabels=ol} -> sprintf "Custom:%s(ins=%A:outs=%A)" name il il
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

let pp model =
    printf "\n%s\n%s" (spCanvas model) (spOpt spProj model.CurrentProj)

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

let setWSModel (wsModel: WaveSimModel) (model: Model) =
    match getCurrSheets model, model.WaveSimSheet with
    | Some sheets, wsSheet when List.contains wsSheet sheets ->
        { model with WaveSim = Map.add model.WaveSimSheet wsModel model.WaveSim }
    | None, _ ->
        printfn "\n\n******* What? trying to set wsmod when WaveSimSheet '%A' is not valid, project is closed" model.WaveSimSheet
        model
    | Some sheets, wsSheet ->
        printfn "\n\n******* What? trying to set wsmod when WaveSimSheet '%A' is not valid, sheets=%A" wsSheet sheets
        model
