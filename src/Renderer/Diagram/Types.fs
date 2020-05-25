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

type Memory = {
    // How many bits the address should have.
    // The memory will have 2^AddressWidth memory locations.
    AddressWidth : int 
    // How wide each memory word should be, in bits.
    WordWidth : int
    // Data is a list of <2^AddressWidth> elements, where each element is a
    // 64 bit integer. This makes words longer than 64 bits not supported.
    // This can be changed by using strings instead of int64, but that is way
    // less memory efficient.
    Data : int64 list
}

// Types instantiating objects in the Digital extension.
type ComponentType =
    | Input of int | Output of int
    | Not | And | Or | Xor | Nand | Nor | Xnor
    | Mux2 | Demux2
    | Custom of CustomComponentType
    | MergeWires | SplitWire of int
    | DFF // No initial state for DFF?
    | ROM of Memory | RAM of Memory

// JSComponent mapped to f# object.
type Component = {
    Id : string
    Type : ComponentType
    Label : string // All components have a label that may be empty.
    InputPorts : Port list
    OutputPorts : Port list
    X : int
    Y : int
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

//=======//
// Other //
//=======//

type NumberBase = | Hex | Dec | Bin
