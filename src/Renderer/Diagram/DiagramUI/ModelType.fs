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
open SimulatorTypes
open Draw2dWrapper

type Notifications = {
    FromDiagram : ((Msg -> unit) -> Fable.Import.React.ReactElement) option
    // TODO: add a general notification?
}

type Model = {
    Diagram : Draw2dWrapper
    SelectedComponent : Component option // None if no component is selected.
    Simulation : Result<SimulationData,SimulationError> option // None if no simulation is running.
    RightTab : RightTab
    Hilighted : ComponentId list * ConnectionId list
    Clipboard : CanvasState // Components and connections that have been selected and copied.
    CurrProject : Project option
    Popup : (PopupDialogData -> Fable.Import.React.ReactElement) option
    PopupDialogData : PopupDialogData
    Notifications : Notifications
}
