module DiagramMessageType

open DiagramTypes
open SimulatorTypes
open Fable.Import.React

type DisplayModeType = Hidden | Visible

type RightTab =
    | Properties
    | Catalogue
    | Simulation

/// Possible fields that may (or may not) be used in a dialog popup.
type PopupDialogData = {
    Text: string option;
    Int: int option;
}

//==========//
// Messages //
//==========//

// Messages that will be sent from JS code.
type JSDiagramMsg =
    | InitCanvas of JSCanvas // Has to be dispatched only once.
    | SelectComponent of JSComponent
    | UnselectComponent of unit
    | InferWidths of unit
    | SetHasUnsavedChanges of bool

type Msg =
    | JSDiagramMsg of JSDiagramMsg
    | StartSimulation of Result<SimulationData, SimulationError>
    | SetSimulationGraph of SimulationGraph
    | EndSimulation
    | ChangeRightTab of RightTab
    | SetHighlighted of ComponentId list * ConnectionId list
    | SetClipboard of CanvasState
    | SetCreateComponentOffset of int
    | SetProject of Project
    | CloseProject
    | ShowPopup of (PopupDialogData -> ReactElement)
    | ClosePopup
    | SetPopupDialogText of string option
    | SetPopupDialogInt of int option
    | CloseDiagramNotification
    | SetSimulationNotification of ((Msg -> unit) -> ReactElement)
    | CloseSimulationNotification
    | SetFilesNotification of ((Msg -> unit) -> ReactElement)
    | CloseFilesNotification
