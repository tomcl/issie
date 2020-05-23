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

type SimulationComponentState =
    | NoState // For all stateless components.
    | DffState of Bit // Represents the bit that will become the output at the NEXT clock tick.

type IsClockTick =
    | No
    | Yes of SimulationComponentState // Pass the state only for clock ticks.

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
    CustomSimulationGraph : SimulationGraph option
    // State for synchronous stateful components, like flip flops and memories.
    // The state should only be changed when clock ticks are fed. Other changes
    // will be ignored.
    State : SimulationComponentState
    // Function that takes the inputs and transforms them into the outputs,
    // according to the behaviour of the component.
    // The size of the Inputs map, must be as expected by the component,
    // otherwhise the reducer will return None (i.e. keep on waiting for more
    // inputs to arrive).
    // The idea is similar to partial application, keep on providing inputs
    // until the output can be evaluated.
    // The reducer should fail if more inputs than expected are received.
    // The reducer accepts a SimulationGraph for custom components only.
    // The reducer accepts an IsClockTick flag that tells you if that is an
    // update due to the global clock.
    Reducer : ReducerInput -> ReducerOutput
}

// Map every ComponentId to its SimulationComponent.
and SimulationGraph = Map<ComponentId, SimulationComponent>

and ReducerInput = {
    Inputs: Map<InputPortNumber, WireData>
    CustomSimulationGraph: SimulationGraph option
    IsClockTick: IsClockTick
}

and ReducerOutput = {
    Outputs: Map<OutputPortNumber, WireData> option
    NewCustomSimulationGraph: SimulationGraph option
    NewState: SimulationComponentState // Will be saved only after clock ticks.
}

// For every IO node, keep track of its Id, Label and wire width.
// - Id: to feed values into the simulationGraph.
// - Label: to display a nice form to the user.
// - Width: to feed the right values into the simulation.
type SimulationIO = ComponentId * ComponentLabel * int

type SimulationData = {
    Graph : SimulationGraph
    // For each input/output, keep its Id and Label to easily access it.
    Inputs : SimulationIO list
    Outputs : SimulationIO list
    // Whether the graph contains synchronous logic.
    IsSynchronous : bool
}

type SimulationError = {
    Msg : string
    InDependency : string option
    ComponentsAffected : ComponentId list
    ConnectionsAffected : ConnectionId list
}