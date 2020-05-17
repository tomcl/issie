module DiagramTypes

//=============================//
// Types for library interface //
//=============================//

type JSCanvas      = | JSCanvas of obj
type JSComponent   = | JSComponent of obj
type JSComponents  = | JSComponents of obj // JS list of JSComponent.
type JSConnection  = | JSConnection of obj
type JSConnections = | JSConnections of obj // JS list of JSConnection.
type JSPort        = | JSPort of obj
type JSPorts       = | JSPorts of obj // JS list of JSPort.
type JSVertices    = | JSVertices of obj // Js list of x,y objects.

type JSCanvasState = JSComponent list * JSConnection list

//==========================================//
// Canvas state mapped to f# data structure //
//==========================================//

// Specify the position and type of a port in a JSComponent.
type PortType = Input | Output

type Port = {
    Id : string
    // For example, an And would have input ports 0 and 1, and output port 0.
    // If the port is used in a connection, the Number is None.
    PortNumber : int option
    PortType : PortType
    HostId : string
}

type CustomComponentType = {
    Name: string
    // Tuples with (label * connection width).
    InputLabels: (string * int) list
    OutputLabels: (string * int) list 
}

// Types instantiating objects in the Digital extension.
type ComponentType =
    | Input of int | Output of int
    | Not | And | Or | Xor | Nand | Nor | Xnor
    | Mux2
    | Custom of CustomComponentType
    | MakeBus2 | PushToBusFirst | PushToBusLast
    | SplitBus2 | PopFirstFromBus | PopLastFromBus

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
    Vertices : (float * float) list
}

type CanvasState   = Component list * Connection list

//================================//
// Componenents loaded from files //
//================================//

type LoadedComponent = {
    Name: string
    FilePath : string
    CanvasState : CanvasState
    InputLabels : (string * int) list
    OutputLabels : (string * int) list
}

type Project = {
    ProjectPath : string
    OpenFileName : string
    LoadedComponents : LoadedComponent list
}
