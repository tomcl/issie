(*
    ModelType.fs

    This module provides the type for the FRP UI.
    It is not possible to put this type among the CommonTypes as it has to
    depend on Draw2dWrapper. Furthermore, non-UI modules should be agnostic of
    the FRP model.
*)

module rec ModelType

open CommonTypes
open SimulatorTypes
open JSTypes
open Fable.React

type RightTab =
    | Properties
    | Catalogue
    | Simulation
    | WaveSim

type MemoryEditorData = {
    OnlyDiff : bool // Only show diffs in Memory Diff Viewer.
    Address : int64 option // Only show the specified memory address.
    Start: int64
    NumberBase : NumberBase
}


type SheetWave = {
    /// path to sheet from simulation graph root
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
type SVGCacheT = {
    Top: ReactElement []
    Waves: Map<string,ReactElement>
    Bottom: ReactElement []
    }

type SimParamsT = {
    /// radix for numbers on SVG waveforms display
    WaveViewerRadix: NumberBase
    /// last clock cycle (index) of the generated SVG
    LastClkTime: uint
    /// position of cursor (0 = first cycle)
    CursorTime: uint
    /// width of one clock in SVG units
    ClkSvgWidth: float
    /// names of NetGroups selected in wave editor and displayed in wave viewer
    DispNames: string array
    /// RAMs to be displayed
    MoreWaves: ComponentId list list
    /// current scrolling position of waveform svg (used to possibly extend svgs if scrolling off screen)
    MoreNames: MoreWaveData list
    LastScrollPos: float option
}


type WSViewT = 
    | WSClosed 
    | WSInitEditorOpen
    | WSEditorOpen
    | WSViewerOpen

type SimActionT = 
    | MakeSVGs of NetGroup array 
    | ChangeParameters of SimParamsT


type WaveSimModel = {
    /// generate data using this 0 clock simulation, which comes from makeSimData
    /// TODO: get rid of this and use only SimDataCache, since this is SimDataCache[0]
    InitWaveSimGraph : SimulationData option
    
    /// parameters determining how and which viewer waves are displayed
    SimParams: SimParamsT

    /// Waveform names and details shown in the editor
    AllWaves: Map<string,WaveformSpec>

    /// react SVG for each waveform, indexed by name
    DispWaveSVGCache: SVGCacheT 
    /// Simulation output sample array of variable length
    SimDataCache: SimulatorTypes.SimulationData array
    
    /// Hack to detect when  cursor text box is empty and use 0.
    /// TODO - get rid of this - it should not be needed
    CursorBoxIsEmpty: bool
   
    WSViewState: WSViewT

    WSTransition: (SimParamsT * WSViewT) option
    /// the circuit that is being simulated - the canvas may have changed
    LastCanvasState: CanvasState option 
    /// the top-level sheet thsi is being simulated - the canvas may have changed
    } 

let setSimParams (setFn: SimParamsT -> SimParamsT) (wsm:WaveSimModel) =
    {wsm with SimParams = setFn wsm.SimParams}

let setDispNames names wsMod = 
    setSimParams (fun sp -> {sp with DispNames=names}) wsMod

let setEditorView view wsModel =
    {wsModel with WSViewState = view; WSTransition = None}
    
let setEditorNextView nView simParas wsModel =
    {wsModel with WSTransition = Some(simParas, nView)}   

let inline getWave (ws:WaveSimModel) (name: string) = ws.AllWaves.[name]

let inline getDispName (ws:WaveSimModel) (wave:WaveformSpec) =
    Map.tryFindKey (fun k v -> v = wave) ws.AllWaves
    |> Option.defaultValue "name not found"
    
let inline dispWaves (ws: WaveSimModel) =
    ws.SimParams.DispNames
    |> Array.map (fun name -> ws.AllWaves.[name])

let inline AllPorts (ws: WaveSimModel) =
    ws.AllWaves

let initWS (allNames:string array) (allPorts: Map<string,NetGroup>): WaveSimModel =
    { 
      InitWaveSimGraph = None
      AllWaves = Map.empty
      SimDataCache = [||]
      DispWaveSVGCache = { Top = [||]; Waves = Map.empty; Bottom = [||]}
      SimParams = {
        MoreNames = []
        DispNames = [||]
        ClkSvgWidth = 1.0
        CursorTime = 0u
        WaveViewerRadix = Bin
        LastClkTime = 9u 
        LastScrollPos = None
        MoreWaves = []
      }
      WSViewState = WSClosed
      WSTransition =None
      LastCanvasState = None 
      CursorBoxIsEmpty = false
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
    | Sheet of Sheet.Msg
    | JSDiagramMsg of JSDiagramMsg<JSCanvas,JSComponent>
    | KeyboardShortcutMsg of KeyboardShortcutMsg
    | StartSimulation of Result<SimulationData, SimulationError>
    | SetWSMod of WaveSimModel
    | UpdateWSModel of (WaveSimModel -> WaveSimModel)
    | SetWSModAndSheet of (WaveSimModel*string)
    | SetWSError of SimulationError option
    | AddWaveSimFile of string * WaveSimModel
    | SetSimulationGraph of SimulationGraph  * FastSimulation
    | SetSimulationBase of NumberBase
    | IncrementSimulationClockTick of int
    | EndSimulation
    | EndWaveSim
    | ChangeRightTab of RightTab
    | SetHighlighted of ComponentId list * ConnectionId list
    | SetSelWavesHighlighted of ConnectionId array
    | SetClipboard of CanvasState
    | SetCreateComponent of Component
    | SetProject of Project
    | UpdateProject of (Project -> Project)
    | UpdateProjectWithoutSyncing of (Project->Project)
    | ShowPopup of ((Msg -> Unit) -> PopupDialogData -> ReactElement)
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
    | SetWaveSimIsOutOfDate of bool
    | SetIsLoading of bool
    | SetWaveSimModel of Sheet: string * WSModel: WaveSimModel
    | WaveSimulateNow
    | InitiateWaveSimulation of (WSViewT * SimParamsT)
    | SetLastSimulatedCanvasState of CanvasState option
    | StartNewWaveSimulation of CanvasState
    | UpdateScrollPos of bool
    | SetLastScrollPos of float option
    | SetRouterInteractive of bool
    | CloseApp
    | SetExitDialog of bool
    | ExecutePendingMessages of int
    | DoNothing
    | StartUICmd of UICommandType
    | FinishUICmd
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





type Model = {
    /// All the data for waveform simulation (separate for each sheet)
    /// TODO: remove the simulation error.
    WaveSim : Map<string, WaveSimModel> * (SimulationError option)
    /// which top-level sheet is used by wavesim
    WaveSimSheet: string
        
    /// Draw Canvas
    Sheet: Sheet.Model

    /// true during period when a sheet or project is loading
    IsLoading: bool

    /// if canvas is now different from that which is currently used by wave sim.
    WaveSimulationIsOutOfDate: bool

    // last time check for changes was made

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
    /// which of the tabbed panes is currentlky visible
    RightPaneTabVisible : RightTab
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
    /// TODO - delete this, I think no longer needed
    SimulationInProgress:  SimActionT option
    /// if true highlight connections from wavesim editor
    ConnsOfSelectedWavesAreHighlighted: bool
    /// true if wavesim scroll position needs checking
    CheckWaveformScrollPosition: bool
    /// Contains a list of pending messages
    Pending: Msg list
    UIState: UICommandType Option
} 



let reduce (this: Model) = {|
         RightTab = this.RightPaneTabVisible
         Hilighted = this.Hilighted
         Clipboard = this.Clipboard
         SimulationIsStale = this.WaveSimulationIsOutOfDate
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
         SimulationInProgress = this.SimulationInProgress
         ConnsToBeHighlighted = this.ConnsOfSelectedWavesAreHighlighted

 |} 
       
let reduceApprox (this: Model) = {|
         RightTab = this.RightPaneTabVisible
         Clipboard = this.Clipboard
         CurrProject = match this.PopupViewFunc with None -> false | _ -> true
         SimulationIsStale = this.WaveSimulationIsOutOfDate
         LastUsedDialogWidth = this.LastUsedDialogWidth
         CreateComponent = this.LastCreatedComponent
         HasUnsavedChanges = false
         CurrProject = match this.PopupViewFunc with None -> false | _ -> true
         PopupDialogData = this.PopupDialogData
         DragMode = this.DividerDragMode
         ViewerWidth = this.WaveSimViewerWidth
         SimulationInProgress = this.SimulationInProgress
 |} 

let mapOverProject defaultValue (model: Model) transform =
    match model.CurrentProj with
    | None -> defaultValue
    | Some p -> transform p


let changeSimulationIsStale (b:bool) (m:Model) = 
    //printfn "Changing WaveSimulationIsStale to %A" b
    { m with WaveSimulationIsOutOfDate = b}


let getComponentIds (model: Model) =
    let extractIds ((comps,conns): Component list * Connection list) = 
        conns
        |> List.map (fun comp -> ComponentId comp.Id)
        
    model.Sheet.GetCanvasState()
    |> extractIds
    |> Set.ofList

////////////////////////////
/// Saving WaveSim Model ///
////////////////////////////

/// get saveable record of waveform setup
let waveSimModel2SavedWaveInfo (wsMod: WaveSimModel) : SavedWaveInfo =
    let pars = wsMod.SimParams
    { 
        ClkWidth = pars.ClkSvgWidth
        Cursor = pars.CursorTime
        Radix = pars.WaveViewerRadix
        LastClk = pars.LastClkTime
        DisplayedPortIds = 
            wsMod.SimParams.DispNames
    }

/// setup current WaveSimModel from saved record
/// currently only the set of nets displayed by default and the radix is actually preserved
/// TODO: work out better idea for what should be preserved here.
/// NB - note that SavedWaveInfo can only be changed if code is added to make loading backwards compatible with
/// old designs
let savedWaveInfo2WaveSimModel (sWInfo: SavedWaveInfo) : WaveSimModel =
    { 
        InitWaveSimGraph = None
        SimDataCache = [||]
        DispWaveSVGCache = {Top=[||]; Waves = Map.empty; Bottom = [||]}
        AllWaves = Map.empty // will be reconstituted
        SimParams = {
            MoreNames = []
            DispNames = sWInfo.DisplayedPortIds // actually names not ids
            ClkSvgWidth = 1.0
            CursorTime = 0u
            WaveViewerRadix = sWInfo.Radix
            LastClkTime = 9u
            LastScrollPos = None
            MoreWaves = []
        }
        WSViewState =WSClosed
        WSTransition =None
        LastCanvasState = None 
        CursorBoxIsEmpty = false

    }

let getSheetWaveSimOpt (model:Model) : WaveSimModel option = 
    model.CurrentProj
    |> Option.bind (fun p -> Map.tryFind p.OpenFileName (fst model.WaveSim))
    

let getSheetWaveSimErr (model:Model) =
    model.CurrentProj
    |> Option.map (fun p -> snd model.WaveSim)
    |> Option.defaultValue None

let getSheetWaveCanvasState (model:Model) =
    getSheetWaveSimOpt model
    |> Option.map (fun (ws:WaveSimModel) -> ws.LastCanvasState)
    |> Option.defaultValue None

let getSheetWaveNetList (model:Model) =
    getSheetWaveCanvasState model
    |> Option.map Helpers.getNetList
   

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

let getCurrentWSMod(model: Model) =
    let wsMap = fst model.WaveSim
    Map.tryFind model.WaveSimSheet wsMap

let getWSModelOrFail (model:Model) (errMsg: string) =
    match getCurrentWSMod model with
    | Some ws -> ws
    | None -> failwithf "%s" errMsg

let getCurrentWSModNextView(model:Model) =
    getCurrentWSMod model
    |> Option.bind (fun ws -> ws.WSTransition)


/// returns a string option representig the current file name if file is loaded, otherwise None
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

let setWSMod (ws: WaveSimModel) (model: Model) =
    match getCurrSheets model, model.WaveSimSheet with
    | Some sheets, sheet when List.contains sheet sheets ->
        { model with WaveSim = Map.add model.WaveSimSheet ws (fst model.WaveSim), 
                               snd model.WaveSim }
    | None,_ -> 
        printfn "\n\n******* What? trying to set wsmod when WaveSimSheet '%A' is not valid, project is closed" model.WaveSimSheet
        model
    | Some sheets, sheet -> 
        printfn "\n\n******* What? trying to set wsmod when WaveSimSheet '%A' is not valid, sheets=%A" model.WaveSimSheet sheets
        model


let updateCurrentWSMod(updateFun: WaveSimModel -> WaveSimModel) (model: Model) =
   match getCurrentWSMod model with
    | None -> model // should this be an error?
    | Some ws ->
        let ws = updateFun ws
        setWSMod ws model


let switchToWaveEditor (model:Model) dispatch =
    match getCurrentWSMod model with
    | None -> ()
    | Some ws when ws.WSViewState = WSClosed ->
        printf "What? Can't switch to wave editor when wave sim is closed!"
    | Some ws -> 
        dispatch <| SetWSMod {ws with WSViewState=WSViewT.WSEditorOpen}
        dispatch <| ChangeRightTab WaveSim
 