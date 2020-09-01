(*
    ModelType.fs

    This module provides the type for the FRP UI.
    It is not possible to put this type among the CommonTypes as it has to
    depend on Draw2dWrapper. Furthermore, non-UI modules should be agnostic of
    the FRP model.
*)

module DiagramModelType

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
    LastAutoSave: System.DateTime
    LastAutoSaveCheck: System.DateTime
    LastSavedCanvasState: CanvasState option
    RunningSimulation: bool // placeholder - not used yet
    }


type Model = {
    AsyncActivity: AsyncTasksT
    Diagram : Draw2dWrapper
    SimulationIsStale: bool
    LastSimulatedCanvasState: CanvasState option // reduced (without layout) canvas state
    LastSelected: Component list * Connection list
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
    SimulationInProgress: (WaveSimPort array option) * ({| NewVal: uint; NewCurs: uint; NewClkW: float |} option)
}

/// Lens to facilitate changing AsyncActivity
let setActivity (f: AsyncTasksT -> AsyncTasksT) (model: Model) =
    {model with AsyncActivity = f model.AsyncActivity }
    
let changeSimulationIsStale (b:bool) (m:Model) = {m with SimulationIsStale = b}
