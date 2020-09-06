(*
    Types.fs

    This module collects a series of types used in the simulator logic.
*)

module SimulatorTypes

open CommonTypes

/// Binary data used in simulation
type Bit = Zero | One

/// Fixed width bus data used in simulation
/// TODO: refactor as int64 or bigint for efficiency
type WireData = Bit list


/// State (possibly none) remembered by component
/// from previous clock cycle. Combinational components
/// have no state.
type SimulationComponentState =
    | NoState // For all stateless components.
    | DffState of Bit
    | RegisterState of WireData
    | RamState of Memory

/// Message used to feed forward evaluation. Clock
/// tick => state changes to that in next cycle
type IsClockTick =
    | No
    | Yes of SimulationComponentState // Pass the state only for clock ticks.

/// Like Component but with additional dynamic info used by simulator
/// Clocked components have state data.
/// All components have optional data on inputs that propagates
/// During evaluation of combinational logic
/// Components require all inputs to have data before they can
/// generate output data
/// Note that reducer is a function that generates the outputs
/// TODO: make this equatable data?
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

/// Map every ComponentId to its SimulationComponent.
and SimulationGraph = Map<ComponentId, SimulationComponent>

/// This drives the generation of component outputs
/// it is processed by the Reducer function.
and ReducerInput = {
    Inputs: Map<InputPortNumber, WireData>
    CustomSimulationGraph: SimulationGraph option
    IsClockTick: IsClockTick
}

/// When all inputs are available the reducer function will generate
/// these outputs. For custom components the SimulationGraph contains
/// embedded state.
and ReducerOutput = {
    Outputs: Map<OutputPortNumber, WireData> option
    NewCustomSimulationGraph: SimulationGraph option
    NewState: SimulationComponentState // Will be saved only after clock ticks.
}

/// For every IO node, keep track of its Id, Label and wire width.
/// - Id: to feed values into the simulationGraph.
/// - Label: to display a nice form to the user.
/// - Width: to feed the right values into the simulation.
type SimulationIO = ComponentId * ComponentLabel * int

/// - Top level data tracking a simulation
type SimulationData = {
    Graph : SimulationGraph
    // For each input/output, keep its Id and Label to easily access it.
    Inputs : SimulationIO list
    Outputs : SimulationIO list
    // Whether the graph contains synchronous logic.
    IsSynchronous : bool
    // The base that should be used to display numbers in the simulation.
    NumberBase : NumberBase
    // Keep track of the number of clock ticks of the simulation.
    ClockTickNumber : int
}

/// - Documents an error found while simulating.
/// - Should never happen
type SimulationError = {
    Msg : string
    InDependency : string option
    ComponentsAffected : ComponentId list
    ConnectionsAffected : ConnectionId list
}

/// Wrapper for Javascript (Diagram) component. Why here?
type JSComponent   = | JSComponent of obj
/// Wrapper for Javascript (Diagram) connection. Why here?
type JSConnection  = | JSConnection of obj
/// State retrieves directly from Diagram has Javascript objects
type JSCanvasState = JSComponent list * JSConnection list


