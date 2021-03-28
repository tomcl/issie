(*
    Types.fs

    This module collects a series of types used in the simulator logic.
*)

module rec SimulatorTypes
open Fable.Core
open CommonTypes
open Helpers

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
    // TODO: maybe using a list would improve performance?
    Inputs : Map<InputPortNumber, WireData>
    // Mapping from each output port number to all of the ports and
    // Components connected to that port.
    Outputs : Map<OutputPortNumber,(ComponentId * InputPortNumber) list>
    // this is MUTABLE and used only during clock tick change propagation
    // location n = true => the output (of a synchronous component) has been
    // propagated in propagateStateChanges. Location n corresponds to
    // OutputPortNumber n.
    // not used except for synchronous components and custom components
    OutputsPropagated: bool array
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

/// contains info needed to propagate wire value changes through a simulation.
and OutputChange = {
    CComp: SimulationComponent
    COutputs: Map<OutputPortNumber, WireData>
    }


/// For every IO node, keep track of its Id, Label and wire width.
/// - Id: to feed values into the simulationGraph.
/// - Label: to display a nice form to the user.
/// - Width: to feed the right values into the simulation.
type SimulationIO = ComponentId * ComponentLabel * int

/// - Top level data tracking a simulation
type SimulationData = {
    FastSim: FastSimulation
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

[<Erase>]
type JSComponent   = | JSComponent of obj
/// Wrapper for Javascript (Diagram) connection. Why here?

[<Erase>]
type JSConnection  = | JSConnection of obj
/// State retrieves directly from Diagram has Javascript objects
type JSCanvasState = JSComponent list * JSConnection list



//----------------------------------------------------------------------------------------------//
//--------------------------------Fast Digital Bus Data Type------------------------------------//
//----------------------------------------------------------------------------------------------//
// data is stored differently according to its buswidth.
// We use all three options for efficiency
// Bit is more efficient than word for known boolean ops but it can be normalised to Word 
// to make implementation of multiple bit components (that may carry one bit) simpler.
// BigWord is needed for > 32 bits, and much less efficient for < 32 bits.
type FastData =
    | FBit of uint32 // must be 0 or 1, allows bitwise logical operators
    | Word of dat:uint32 * width:int
    | BigWord of dat:bigint * width:int with
    /// convert to form where single Bit is turned into Word equivalent
    member inline this.Normalise = 
        match this with 
        | FBit n -> Word(n, 1) 
        | BigWord(x, n) when n <= 32 -> Word(uint32 x, n)
        | x -> x
    /// return width of the data
    member inline this.Width = 
        match this with 
        | FBit _ -> 1 
        | Word (_,n) -> n 
        | BigWord(_,n) -> n
    /// return Some 0 or Some 1 as the single bit, or None if not a single bit
    member inline this.GetBitAsInt = // not possible if too large
        match this with 
        | FBit n -> Some n 
        | Word(n, 1) -> Some n 
        | _ -> None
    member inline this.GetBigInt = // always possible
        match this with
        | FBit n -> bigint n
        | Word(n,_) -> bigint n
        | BigWord(n,_) -> n
    /// return Some uint32 representing data if possible else None
    member inline this.GetUint32 = // not possible if too large
        match this with
        | FBit n -> Some n
        | Word(n,w) -> Some n
        | BigWord(n, w) when w <= 32 -> Some (uint32 n)
        | _ -> None
    

//--------------------------------Fast Simulation Data Structure-------------------------//
//---------------------------------------------------------------------------------------//
            

    

   
type FComponentId = ComponentId * ComponentId list


type FData = WireData // for now...

/// Wrapper to allow arrays to be resized for longer simulations while keeping the links between inputs
/// and outputs
type StepArray<'T> = {
    /// this field is mutable to allow resizing
    mutable Step: 'T array
    }



type FastComponent = {
    fId: FComponentId
    cId: ComponentId
    FType: ComponentType
    State: StepArray<SimulationComponentState> option
    mutable Inactive: bool
    OutputWidth: int option array
    InputLinks: StepArray<FData> array
    Outputs: StepArray<FData> array
    SimComponent: SimulationComponent
    AccessPath: ComponentId list
    FullName: string
    mutable Touched: bool

    } with

    member inline this.GetInput (epoch)  (InputPortNumber n) = this.InputLinks.[n].Step.[epoch]
    member this.ShortId =
        let (ComponentId sid,ap) = this.fId
        (EEExtensions.String.substringLength 0 5 sid)
    member inline this.PutOutput (epoch) (OutputPortNumber n) dat = this.Outputs.[n].Step.[epoch] <- dat
    member inline this.Id = this.SimComponent.Id
    
    
      

// The fast simulation components are similar to the issie components they are based on but with addition of arrays
// for direct lookup of inputs and fast access of outputs. The input arrays contain pointers to the output arrays the
// inputs are connected to, the InputPortNumber integer indexes this.
// In addition outputs are contained in a big array indexed by epoch (simulation time). This allows results for multiple
// steps to begin built efficiently and also allows clocked outputs for the next cycle to be constructed without overwriting
// previous outputs needed for that construction.
//
// For reasons of efficiency Issie's list-style WireData type is optimised by using integers as bit arrays.
//
// For ease of implementation Input and Output components are given a single output (input) port not present on issie.
// this allows sub-sheet I/Os to be linked as normal in the constructed graph via their respective Input and Output connections.
//
// Although keeping input and output connections in the new graph is slightly less efficient it makes things simpler because there is a
// 1-1 connection between components (except for custom components which are removed by the gathering process).
// Note that custom component info is still kept because each component in the graph has a path - the list of custom component ids
// between its graph and root. Following issie this does not include a custom component for the sheet being simulated, which is viewed as
// root. Since custom components have been removed this no longer complicates the simulation.

type FastSimulation = {
    /// last step number (starting from 0) which is simulated.
    mutable ClockTick: int
    /// The step number of the last step that can be simulated in the
    /// current simulation outputs
    mutable MaxStepNum: int 
    /// top-level inputs to the simulation
    FGlobalInputComps: FastComponent array
    /// constants
    FConstantComps: FastComponent array
    /// clocked components
    FClockedComps: FastComponent array
    /// Components that will be reduced in order allowing sequential reduction to implement simulation
    FOrderedComps: FastComponent array
    /// Additional record of IOLabels, with list of all the IOLabel components
    /// In the simulation only one of these will actually be reduced, but all outputs will
    /// use the same array.
    FIOLabels: Map<ComponentLabel*ComponentId list,FComponentId list>
    /// Fast components: this array is longer than FOrderedComps because it contains
    /// IOlabel components that are redundant in the simulation
    FComps: Map<FComponentId,FastComponent>
    FSComps: Map<FComponentId,SimulationComponent * ComponentId list>
    /// look up from output port of custom component to the relevant Output component
    FCustomOutputCompLookup: Map<(ComponentId*ComponentId list)*OutputPortNumber, FComponentId>
    /// GatherData from which this simulation was made
    G: GatherData
    } with
        member this.getSimulationData (step: int) ((cid,ap): FComponentId) (opn: OutputPortNumber) =
            let (OutputPortNumber n) = opn
            match Map.tryFind (cid,ap) this.FComps with
            | Some fc -> fc.Outputs.[n].Step.[step]
            | None ->
                match Map.tryFind ((cid,ap), opn) this.FCustomOutputCompLookup with
                | Some fid -> this.FComps.[fid].Outputs.[0].Step.[step]
                | None -> failwithf "What? can't find %A in the fast simulation data" (cid,ap)

and  GatherData = {
    /// Existing Issie data structure representing circuit for simulation - generated by runCanvasStateChecksAndBuildGraph
    Simulation: SimulationGraph
    /// Maps Custom Component Id and input port number to corresponding Input 
    /// Component Id (of an Input component which is not top-level)
    CustomInputCompLinks: Map<FComponentId * InputPortNumber, FComponentId>
    /// Maps (non-top-level) Output component Id to corresponding Custom Component Id & output port number
    CustomOutputCompLinks: Map<FComponentId,  FComponentId * OutputPortNumber>
    /// Maps custom component output to corresponding output FastComponent.
    /// Inverse of CustomOutputCompLinks
    CustomOutputLookup: Map<FComponentId * OutputPortNumber, FComponentId>
    /// Shortcut to find the label of a component; notice that the access path is not needed here because
    /// Labels of the graph inside a custom component are identical for different instances of the component
    Labels: Map<ComponentId,string>
    /// Each entry here corresponds to one linked set of IOLabels, indexed by name and access path. the map looks up
    /// the list of all corresponding IOLabel components
    IOLabels: Map<ComponentLabel*ComponentId list,FComponentId list>
    /// This indexes the SimulationGraph components from Id and access path. Note that the same simulation
    /// component Id can appear with different access paths if a sheet is instantiated more than once.
    /// Each entry corresponds to a single FastComponent.
    /// Note that Custom components are not included in this list.
    AllComps: Map<ComponentId*ComponentId list,SimulationComponent * ComponentId list> // maps to component and its path in the graph
    /// List of component Input components that are driven externally to simulation
    } with


    member this.getFullName (cid,ap) = 
            List.map ( fun cid -> match Map.tryFind cid this.Labels with | Some lab -> lab | None -> "*" ) (ap @ [cid])
            |> String.concat "."


//-------------------------------------------------------------------------------------//
//-------------------Helper functions for simulation types-----------------------------//
//-------------------------------------------------------------------------------------//

let sprintSimComponent (sComp: SimulationComponent) =
    sprintf "'%A': %20s" sComp.Label (sComp.Type.ToString() |> Helpers.sprintInitial 20)

let shortPSComp (comp:SimulationComponent) =
    let lab = match comp.Label with | ComponentLabel lab' -> lab'
    match comp.Type with
    | Custom sc -> sprintf "%s:Custom.(%s.%A->%A)" lab sc.Name sc.InputLabels sc.OutputLabels
    | _ -> sprintf "%s:%A.{%A}" lab comp.Type comp.State


let printSimGraph (sg: SimulationGraph) =
    printfn "%s" (String.concat "\n" (sg |> Map.toList |> List.map (fun (ComponentId id,comp) -> sprintSimComponent comp + id)))

let tryGetCompLabel (compId: ComponentId) (sg: SimulationGraph) =
    Map.tryPick (fun k v -> if k = compId then Some v else None) sg
    |> Option.map (fun comp -> comp.Label)
    |> Option.map (fun (ComponentLabel s) -> s)
    |> Option.defaultValue "'Not in SimGraph'"



