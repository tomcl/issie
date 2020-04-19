module DiagramTypes

//=============================//
// Types for library interface //
//=============================//

type JSCanvas      = | JSCanvas of obj
type JSCanvasState = | JSCanvasState of obj // Only the relevant fields from a JSCanvas.
type JSComponent   = | JSComponent of obj
type JSComponents  = | JSComponents of obj // JS list of JSComponent.
type JSConnection  = | JSConnection of obj
type JSConnections = | JSConnections of obj // JS list of JSConnection.
type JSPort        = | JSPort of obj
type JSPorts       = | JSPorts of obj // JS list of JSPort.

// Specify the position and type of a port in a JSComponent.
type PortType = Input | Output
type PortLocation = Left | Right | Top | Bottom

type Port = {
    Id : string
    PortType : PortType
    HostId : string
}

// Types instantiating objects in the Digital extension.
type ComponentType =
    | Input | Output
    | Not | And | Or | Xor | Nand | Nor | Xnor
    | Mux2

// JSComponent mapped to f# object.
type Component = {
    Id : string
    Type : ComponentType
    Label : string // All components have a label that may be empty.
    InputPorts : Port list
    OutputPorts : Port list
    X : int
    Y : int
    // Maybe there will be the need for other fields for stateful components
    // such as RAMs.
}

// JSConnection mapped to f# object.
type Connection = {
    Id : string
    Source : Port
    Target : Port
}

//====================//
// Types for the page //
//====================//

type RightTab =
    | Properties
    | Catalogue

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
    | UpdateState of Component list * Connection list
    | ChangeRightTab of RightTab
