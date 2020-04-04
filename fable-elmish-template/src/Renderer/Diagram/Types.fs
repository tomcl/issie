module DiagramTypes

// JS objects.
type Canvas      = | Canvas of obj
type Box         = | Box of obj
// JS objects for the state.
type CanvasState   = | CanvasState of obj // A JSON object.
type JSComponents  = | JSComponents of obj // A JSON object.
type JSConnections = | JSConnections of obj // A JSON object.
// Component mapped to f# object.
type Component = {
    Id : string
    InputPorts : string list // list of ids.
    OutputPorts : string list // list of ids.
}
// Connection mapped to f# object.
type ConnectionPort = {
    ComponentId : string
    PortId : string
}
type Connection = {
    Id : string
    Source : ConnectionPort
    Target : ConnectionPort
}
