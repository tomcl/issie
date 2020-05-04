(*
    ModelType.fs

    This module provides the type for the FRP UI.
    It is not possible to put this type among the DiagramTypes as it has to
    depend on Draw2dWrapper. Furthermore, non-UI modules should be agnostic of
    the FRP model.
*)

module DiagramModelType

open DiagramTypes
open DiagramMessageType
open Draw2dWrapper

type Model = {
    Diagram : Draw2dWrapper
    State : CanvasState // TODO: remove
    SelectedComponent : Component option // None if no component is selected.
    Simulation : Result<SimulationData,SimulationError> option // None if no simulation is running.
    RightTab : RightTab
    // If the content of the diagram has been loaded or saved from/to file keep
    // track of the path, instead of reasking every time.
    OpenPath : string option
    Hilighted : ComponentId list * ConnectionId list
    Clipboard : CanvasState // Components and connections that have been selected and copied.
    LoadedComponents : LoadedComponent list
    Popup : Fable.Import.React.ReactElement option
}

(*
type Project = {
    OpenFileName : string
    LoadedComponents : LoadedComponent list
}

type Model = {
    Diagram : Draw2dWrapper
    State : CanvasState // TODO: remove
    SelectedComponent : Component option // None if no component is selected.
    Simulation : Result<SimulationData,SimulationError> option // None if no simulation is running.
    RightTab : RightTab
    Hilighted : ComponentId list * ConnectionId list
    Clipboard : CanvasState // Components and connections that have been selected and copied.
    CurrProject : LoadedComponent list
    // If the content of the diagram has been loaded or saved from/to file keep
    // track of the path, instead of reasking every time.
    OpenPath : string option
}
*)
