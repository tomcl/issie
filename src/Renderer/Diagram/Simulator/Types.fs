module SimulatorTypes

open DiagramTypes

type Bit = Zero | One

type WireData = Bit list

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
    Type : ComponentType
    Label : ComponentLabel
    // Mapping from each input port number to its value (it will be set
    // during the simulation process).
    // TODO: maybe using a list would improve performace?
    Inputs : Map<InputPortNumber, WireData>
    // Mapping from each output port number to all of the ports and
    // Components connected to that port.
    Outputs : Map<OutputPortNumber, (ComponentId * InputPortNumber) list>
    // This CustomSimulationGraph should only be Some when the component Type is
    // Custom. A custom component keeps track of its internal state using this
    // CustomSimulationGraph. This graph will be passed to the reducer and
    // updated from the reducer return value.
    CustomSimulationGraph : Map<ComponentId, SimulationComponent> option
    // Function that takes the inputs and transforms them into the outputs.
    // The size of input map, must be as expected by otherwhise the reducer will
    // return None (i.e. keep on waiting for more inputs to arrive).
    // The idea is similar to partial application, keep on providing inputs
    // until the output can be evaluated.
    // The reducer should fail if more inputs than expected are received.
    // The reducer accepts a SimulationGraph for custom components only.
    Reducer : Map<InputPortNumber, WireData>                    // Inputs.
              -> Map<ComponentId, SimulationComponent> option   // CustomSimulationGraph.
              -> (Map<OutputPortNumber, WireData> option *      // Outputs.
                  Map<ComponentId, SimulationComponent> option) // Updated CustomSimulationGraph.
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
    InDependency : string option
    ComponentsAffected : ComponentId list
    ConnectionsAffected : ConnectionId list
}