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

// Specify the position and type of a port in a JSComponent.
type PortType = Input | Output
type PortLocation = Left | Right | Top | Bottom

// TODO unify the type for ports.

// Component mapped to f# object.
type Component = {
    Id : string
    InputPorts : string list // list of ids.
    OutputPorts : string list // list of ids.
    Label : string
}

// Connection mapped to f# object.
type ConnectionPort = {
    ComponentId : string
    PortId : string
}
type Connection = {
    Id : string
    Source : ConnectionPort // Will always be an output port.
    Target : ConnectionPort // Will always be an input port.
}

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
    //| ZoomIn
    //| ZoomOut
