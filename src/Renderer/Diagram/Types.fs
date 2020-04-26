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
type PortLocation = Left | Right | Top | Bottom // TODO: remove.

type Port = {
    Id : string
    // For example, an And would have input ports 0 and 1, and output port 0.
    // If the port is used in a connection, the Number is None.
    PortNumber : int option
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
    Vertices : (float * float) list
}

type CanvasState   = Component list * Connection list

//==========================//
// Types for the simulation //
//==========================//

type Bit = Zero | One

// The next types are not strictly necessary,
// but help in understanding what is what.
type ComponentId      = | ComponentId of string
type ConnectionId     = | ConnectionId of string
type ComponentLabel   = | ComponentLabel of string
type InputPortId      = | InputPortId of string
type OutputPortId     = | OutputPortId of string
type InputPortNumber  = | InputPortNumber of int
type OutputPortNumber = | OutputPortNumber of int

type SimulationComponent = {
    Id : ComponentId
    // Mapping from each input port number to its value (it will be set
    // during the simulation process).
    // TODO: maybe using a list would improve performace?
    Inputs : Map<InputPortNumber, Bit>
    // Mapping from each output port number to all of the ports and
    // Components connected to that port.
    Outputs : Map<OutputPortNumber, (ComponentId * InputPortNumber) list>
    // Function that takes the inputs and transforms them into the outputs.
    // The size of input map, must be as expected by otherwhise the reducer will
    // return None (i.e. keep on waiting for more inputs to arrive).
    // The idea is similar to partial application, keep on providing inputs
    // until the output can be evaluated.
    // The reducer should fail if more inputs than expected are received.
    Reducer : Map<InputPortNumber, Bit> -> Map<OutputPortNumber, Bit> option
}

// Map every ComponentId to its SimulationComponent.
type SimulationGraph = Map<ComponentId, SimulationComponent>

// For every IO node, keep track of its Id and Label.
// - Id: to feed values into the simulationGraph.
// - Label: to display a nice form to the user.
type SimulationIO = ComponentId * ComponentLabel

type SimulationData = {
    Graph : SimulationGraph
    // For each input/output, keep its Id and Label to easily access it.
    Inputs : SimulationIO list
    Outputs : SimulationIO list
}

type SimulationError = {
    Msg : string
    ComponentsAffected : ComponentId list
    ConnectionsAffected : ConnectionId list
}

//====================//
// Types for the page //
//====================//

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
