module DiagramMessageType

open DiagramTypes

type DisplayModeType = Hidden | Visible

type RightTab =
    | Properties
    | Catalogue
    | Simulation

//==========//
// Messages //
//==========//

// Messages that will be sent from JS code.
type JSDiagramMsg =
    | InitCanvas of JSCanvas // Has to be dispatched only once.
    | SelectComponent of JSComponent
    | UnselectComponent of JSComponent

type Msg =
    | JSDiagramMsg of JSDiagramMsg
    | UpdateState of CanvasState
    | StartSimulation of Result<SimulationData, SimulationError>
    | SetSimulationGraph of SimulationGraph
    | EndSimulation
    | ChangeRightTab of RightTab
    | SetOpenPath of string option
    | SetHighlighted of ComponentId list * ConnectionId list
    | SetClipboard of CanvasState
    | SetLoadedComponents of LoadedComponent list
    | ShowPopup of Fable.Import.React.ReactElement
    | ClosePopup

