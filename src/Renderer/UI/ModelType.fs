(*
    ModelType.fs

    This module provides the type for the FRP UI.
    It is not possible to put this type among the CommonTypes as it has to
    depend on Draw2dWrapper. Furthermore, non-UI modules should be agnostic of
    the FRP model.
*)

module rec DiagramModelType

open CommonTypes
open DiagramMessageType
open SimulatorTypes
open Draw2dWrapper

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


type AutoSaveT = Saving | Deleting | Inactive

type AsyncTasksT = {
    AutoSave: AutoSaveT
    LastAutoSave: Map<string,System.DateTime>
    LastAutoSaveCheck: System.DateTime
    LastSavedCanvasState: Map<string,CanvasState>
    RunningSimulation: bool // placeholder - not used yet
    }

[<CustomEquality;NoComparison>]
type Model = {
    AsyncActivity: AsyncTasksT
    Diagram : Draw2dWrapper
    SimulationIsStale: bool
    LastSimulatedCanvasState: CanvasState option // reduced (without layout) canvas state
    LastSelectedIds: string list * string list
    CurrentSelected: Component list * Connection list
    LastUsedDialogWidth: int
    SelectedComponent : Component option // None if no component is selected.
    Simulation : Result<SimulationData,SimulationError> option // None if no simulation is running.
    WaveSim : Map<string, WaveSimModel> * (SimulationError option)
    RightTab : RightTab
    Hilighted : (ComponentId list * ConnectionId list) * ConnectionId list
    Clipboard : CanvasState // Components and connections that have been selected and copied.
    CreateComponent : Component option // Track the last added component
    HasUnsavedChanges : bool
    CurrProject : Project option
    Popup : (PopupDialogData -> Fable.React.ReactElement) option
    PopupDialogData : PopupDialogData
    Notifications : Notifications
    TopMenu : TopMenu
    DragMode: DragMode
    ViewerWidth: int // waveform viewer width in pixels
    SimulationInProgress:  Result<NetGroup array,{| LastClk: uint; Curs: uint; ClkW: float |}> option
    ConnsToBeHighlighted: bool
    CheckScrollPos: bool
} with
 
    override this.GetHashCode() =
        hash (reduce this)
        
    override this.Equals(x) = 
        match x with
        | :? Model as x' -> reduce this = reduce x'
        | _ -> false

let reduce (this: Model) = {|
         RightTab = this.RightTab
         Hilighted = this.Hilighted
         Clipboard = this.Clipboard
         AsyncActivity = this.AsyncActivity
        
         SimulationIsStale = this.SimulationIsStale
         LastSimulatedCanvasState = this.LastSimulatedCanvasState
         LastSelectedIds = this.LastSelectedIds
         CurrentSelected = this.CurrentSelected
         LastUsedDialogWidth = this.LastUsedDialogWidth
         SelectedComponent= this.SelectedComponent
         CreateComponent = this.CreateComponent
         HasUnsavedChanges = this.HasUnsavedChanges
         CurrProject = match this.Popup with None -> false | _ -> true
         PopupDialogData = this.PopupDialogData
         TopMenu = this.TopMenu
         DragMode = this.DragMode
         ViewerWidth = this.ViewerWidth
         SimulationInProgress = this.SimulationInProgress
         ConnsToBeHighlighted = this.ConnsToBeHighlighted

 |} 
       
let reduceApprox (this: Model) = {|
         RightTab = this.RightTab
         Clipboard = this.Clipboard
         CurrProject = match this.Popup with None -> false | _ -> true
         SimulationIsStale = this.SimulationIsStale
         LastUsedDialogWidth = this.LastUsedDialogWidth
         CreateComponent = this.CreateComponent
         HasUnsavedChanges = this.HasUnsavedChanges
         CurrProject = match this.Popup with None -> false | _ -> true
         PopupDialogData = this.PopupDialogData
         DragMode = this.DragMode
         ViewerWidth = this.ViewerWidth
         SimulationInProgress = this.SimulationInProgress
 |} 

/// Lens to facilitate changing AsyncActivity
let setActivity (f: AsyncTasksT -> AsyncTasksT) (model: Model) =
    {model with AsyncActivity = f model.AsyncActivity }
    
let changeSimulationIsStale (b:bool) (m:Model) = {m with SimulationIsStale = b}

let getComponentIds (model: Model) =
    let extractIds (jsComps,jsConns) = 
        jsComps
        |> List.map Extractor.extractComponent
        |> List.map (fun comp -> ComponentId comp.Id)
    model.Diagram.GetCanvasState()
    |> Option.map extractIds
    |> Option.defaultValue []
    |> Set.ofList

////////////////////////////
/// Saving WaveSim Model ///
////////////////////////////

/// get saveable record of waveform setup
let waveSimModel2SavedWaveInfo (wsMod: WaveSimModel) : SavedWaveInfo =
    { 
        Ports = wsMod.Ports
        ClkWidth = wsMod.ClkWidth
        Cursor = wsMod.Cursor
        Radix = wsMod.Radix
        LastClk = wsMod.LastClk
        WaveAdderOpen = wsMod.WaveAdderOpen
        WaveAdderPorts = 
            wsMod.WaveAdder
            |> Option.map (fun wa -> wa.Ports)
            |> Option.defaultValue [||]
    }

/// setup current WaveSimModel from saved record
let savedWaveInfo2WaveSimModel (sWInfo: SavedWaveInfo) : WaveSimModel =
    {   
        SimData = [||]
        WaveTable = [||]
        WaveNames = [||]
        Ports = sWInfo.Ports
        ClkWidth = sWInfo.ClkWidth
        Cursor = sWInfo.Cursor
        CursorEmpty = false
        Radix = sWInfo.Radix
        LastClk = sWInfo.LastClk
        WaveAdderOpen = sWInfo.WaveAdderOpen
        WaveAdder = None
        LastCanvasState = None 
    }
      
