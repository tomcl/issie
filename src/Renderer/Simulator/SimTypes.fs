module SimTypes

//---------------------------------------------------------------------------------------//
//--------------------------------Fast Simulation Data Structures------------------------//
//---------------------------------------------------------------------------------------//

open Fable.Core
open CommonTypes
open SimGraphTypes

// type FComponentId = ComponentId * ComponentId list moved to CommonTypes

type FData =
    | Data of FastData
    | Alg of FastAlgExp

    member this.Width =
        match this with
        | Data d -> d.Width
        | Alg exp -> getAlgExpWidth exp

    member this.fdToString =
        match this with
        | Data { Dat = Word w; Width = _ } -> string w
        | Data { Dat = BigWord w; Width = _ } -> w.ToString()
        | Alg exp -> expToString exp

    member this.toExp =
        match this with
        | Alg exp -> exp
        | Data fd -> DataLiteral fd

    member this.toFastData =
        match this with
        | Data fd -> fd
        | _ -> failwithf "Expected data, found Alg FData"

/// Wrapper to allow arrays to be resized for longer simulations while keeping the links between inputs
/// and outputs
type StepArray<'T> = { Step: 'T array; Index: int }

// [<Struct>] // TODO - check whether fable optimzed Struct

/// This type represents an array of time steps of simulation data.
/// In any simulation, for a given IOArray, only one of the three 'Step' arrays will be used.
/// For (very strong) efficiency reasons this cannot be implemented as a disjoint union:
/// the code that reads and writes IOArray array elements will access the appropriate array.
/// Truthtable simulations use FDataStep everywhere.
/// Normal simulations use UInt32step or BigIntStep according to the size of the relevant bus.
type IOArray =
    { FDataStep: FData array
      UInt32Step: uint32 array
      BigIntStep: bigint array
      Width: int
      Index: int }

/// Used for efficiency reasons.
/// For a given normal simulation these arrays show whether the corresponding
/// component input or output is a bigint or a unint32 type bus, and therefore
/// show IOArray array is used for the data.
type BigIntState =
    { InputIsBigInt: bool array // NOTE - whether each input uses BigInt or UInt32
      OutputIsBigInt: bool array }

/// FastComponent represents a physical component in a simulation. Because sheets can be
/// instantiated in multiple places a given sheet component can have multiple FastComponents
/// in the simulation.
/// Arrays on FastComponent are filled up with simulation data per clock step as a clocked
/// simulation progresses.
type FastComponent =
    {
      /// contains component path to root of simulation - unique
      fId: FComponentId
      /// allows access to the underlying component
      cId: ComponentId
      /// convenience access to the Type of the underlying component
      FType: ComponentType
      /// Used only by clocked components, contains an array of the component state in
      /// every clock cycle. Filled as simulation progresses.
      State: StepArray<SimulationComponentState> option
      mutable Active: bool
      /// Most components have all bus inputs and outputs the same width. This gives the
      /// default array field to use - BigIntStep or UInt32Step - in IOArray.
      mutable UseBigInt: bool
      /// components that may have variable inputs and output widths use this instead of UseBigInt to
      /// determine the correct array.
      mutable BigIntState: BigIntState option // This is only used for components that have variable input/output widths
      /// Input data - this an array of fxed links to the relevant driver output data arrays
      InputLinks: IOArray array
      /// info on where the drivers are for each input
      InputDrivers: (FComponentId * OutputPortNumber) option array
      /// the output data for this component (this gets linked to all the conmponents driven
      Outputs: IOArray array
      /// the legacy SimulationConmponent from which this FastComponent is generated.
      SimComponent: SimulationComponent
      /// Path from thsi component to root of simulation, if it is in a subsheet.
      AccessPath: ComponentId list
      /// for human use: long name of component
      FullName: string
      /// label of component
      FLabel: string
      SheetName: string list
      // these fields are used only to determine component ordering for correct evaluation
      mutable Touched: bool // legacy field
      mutable DrivenComponents: FastComponent list
      mutable NumMissingInputValues: int
      // these fields are used only by the Verilog output code
      mutable VerilogOutputName: string array
      mutable VerilogComponentName: string }
    /// Number of component inputs
    member inline this.InputWidth(n) = this.InputLinks[n].Width
    /// Number of component outputs
    member inline this.OutputWidth(n) = this.Outputs[n].Width
    /// Get the uint32 data array for a given input
    member inline this.GetInputUInt32 (epoch) (InputPortNumber n) = this.InputLinks[n].UInt32Step[epoch]
    /// Get the BigInt data array for a given input
    member inline this.GetInputBigInt (epoch) (InputPortNumber n) = this.InputLinks[n].BigIntStep[epoch]
    /// Get the FData array for a given input
    member inline this.GetInputFData (epoch) (InputPortNumber n) = this.InputLinks[n].FDataStep[epoch]
    /// for debugging - get a short usually unique truncation of the fId
    member this.ShortId =
        let (ComponentId sid, ap) = this.fId
        (EEExtensions.String.substringLength 0 5 sid)
    /// write data to the Unint32Step output array for the given time step (epoch) and output (n)
    member inline this.PutOutputUInt32 (epoch) (OutputPortNumber n) dat =
        this.Outputs[n].UInt32Step[ epoch ] <- dat
    /// write data to the BigIntStep output array for the given time step (epoch) and output (n)
    member inline this.PutOutputBigInt (epoch) (OutputPortNumber n) dat =
        this.Outputs[n].BigIntStep[ epoch ] <- dat
    /// write data to the FData output array for the given time step (epoch) and output (n)
    member inline this.PutOutputFData (epoch) (OutputPortNumber n) dat =
        this.Outputs[n].FDataStep[ epoch ] <- dat
    member inline this.Id = this.SimComponent.Id
    member inline this.SubSheet = this.SheetName[0 .. this.SheetName.Length - 2]

/// Convenience array used so that waveform simulation can access
/// component outputs (drivers) without a Map lookup
type Driver =
    {
      /// Index of this driver in the array of drivers
      Index: int
      /// Bus width of the driven bus
      DriverWidth: int
      /// Simulation data for the driven bus
      DriverData: IOArray }

/// Type used to tie component ports to simulation data
/// for advanved wavefor simulation features.
type SheetPort = {
    Sheet: string;
    PortOnComp: Port
    } // must include port number (which ports on connections do not)

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
type FastSimulation =
    {
        // last step number (starting from 0) which is simulated.
        mutable ClockTick: int
        // Maximum size of simulation arrays - after which they form a circular buffer
        MaxArraySize: int
        // top-level inputs to the simulation
        FGlobalInputComps: FastComponent array
        // constants
        FConstantComps: FastComponent array
        // clocked components
        FClockedComps: FastComponent array
        // Components that will be reduced in order allowing sequential reduction to implement simulation
        FOrderedComps: FastComponent array
        // which is the active component for each set of labels?
        mutable FIOActive: Map<ComponentLabel * ComponentId list, FastComponent>
        // list of deferred links driven from inactive IOlabls - at end of linkage the
        // corresponding active IOLabel can be substituted as driver an dthe link made
        mutable FIOLinks: ((FastComponent * InputPortNumber) * FastComponent) list
        // Fast components: this array is longer than FOrderedComps because it contains
        // IOlabel components that are redundant in the simulation
        FComps: Map<FComponentId, FastComponent>
        FCustomComps: Map<FComponentId, FastComponent>
        // Fast components: this map is longer than FComps because it contains
        // Custom components not used by Fast simulation but needed in Waveform simulation
        WaveComps: Map<FComponentId, FastComponent>
        FSComps: Map<FComponentId, SimulationComponent * ComponentId list>
        // look up from output port of custom component to the relevant Output component
        FCustomOutputCompLookup: Map<(ComponentId * ComponentId list) * OutputPortNumber, FComponentId>
        // GatherData from which this simulation was made
        G: GatherData
        // Total number of step arrays (= drivers)
        NumStepArrays: int
        // Each driver represents one output with its data
        Drivers: Driver option array
        // Each wave index represents one component port with associated driver and data
        WaveIndex: WaveIndexT array
        /// Connections on simulated sheets indexed by directly connected port. Each connection appears twice.
        ConnectionsByPort: Map<SheetPort, Connection list>
        /// This contains all components in the sheets of the simulation indexed by ComponentId.
        /// Subsheet components are indexed only once.
        /// Contrast this with Fast Components - which have the design expanded out with
        /// one per instance: the different versions correspond to distinct access paths.
        ComponentsById: Map<string, Map<ComponentId, Component>>
        /// Circuit simulated (sheet and all dependencies)
        SimulatedCanvasState: LoadedComponent list
        /// The root sheet being simulated
        SimulatedTopSheet: string
        /// Total size of the output arrays per time-step.
        TotalArraySizePerStep: int
    }

/// GatherTemp is the output type used to accumulate lists of data links when recursively exploring SimulationGraph
/// as first step in flattening it.
/// Each list of pairs is converted into a map at the end in the final GatherData structure
/// The cost of creating maps makes it important to use lists here as the intermediate structures
and GatherTemp =
    {
      // Links Custom Component Id and input port number to corresponding Input
      // Component Id (of an Input component which is not top-level)
      CustomInputCompLinksT: ((FComponentId * InputPortNumber) * FComponentId) list
      // Links (non-top-level) Output component Id to corresponding Custom Component Id & output port number
      CustomOutputCompLinksT: (FComponentId * (FComponentId * OutputPortNumber)) list
      // Shortcut to find the label of a component; notice that the access path is not needed here because
      // Labels of the graph inside a custom component are identical for different instances of the component
      Labels: (ComponentId * string) list
      // This indexes the SimulationGraph components from Id and access path. Note that the same simulation
      // component Id can appear with different access paths if a sheet is instantiated more than once.
      // Each entry corresponds to a single FastComponent.
      // Note that Custom components are not included in this list.
      AllCompsT: ((ComponentId * ComponentId list) * (SimulationComponent * ComponentId list)) list } // links to component and its path in the graph

and GatherData =
    {
      /// Existing Issie data structure representing circuit for simulation - generated by runCanvasStateChecksAndBuildGraph
      Simulation: SimulationGraph
      /// Maps Custom Component Id and input port number to corresponding Input
      /// Component Id (of an Input component which is not top-level)
      CustomInputCompLinks: Map<FComponentId * InputPortNumber, FComponentId>
      /// Maps (non-top-level) Output component Id to corresponding Custom Component Id & output port number
      CustomOutputCompLinks: Map<FComponentId, FComponentId * OutputPortNumber>
      /// Maps custom component output to corresponding output FastComponent.
      /// Inverse of CustomOutputCompLinks
      CustomOutputLookup: Map<FComponentId * OutputPortNumber, FComponentId>
      /// Shortcut to find the label of a component; notice that the access path is not needed here because
      /// Labels of the graph inside a custom component are identical for different instances of the component
      Labels: Map<ComponentId, string>
      /// This indexes the SimulationGraph components from Id and access path. Note that the same simulation
      /// component Id can appear with different access paths if a sheet is instantiated more than once.
      /// Each entry corresponds to a single FastComponent.
      /// Note that Custom components are not included in this list.
      AllComps: Map<ComponentId * ComponentId list, SimulationComponent * ComponentId list>  // maps to component and its path in the graph
     }

    member this.getFullName(cid, ap) =
        List.map
            (fun cid ->
                match Map.tryFind cid this.Labels with
                | Some lab -> lab
                | None -> "*")
            (ap @ [ cid ])
        |> String.concat "."

    member this.getSheetName(cid, ap) =
        List.map
            (fun cid ->
                match Map.tryFind cid this.Labels with
                | Some lab -> lab.ToUpper()
                | None -> "*")
            (ap @ [ cid ])


/// - Top level data tracking a simulation
type SimulationData =
    { FastSim: FastSimulation
      Graph: SimulationGraph
      // For each input/output, keep its Id and Label to easily access it.
      Inputs: SimulationIO list
      Outputs: SimulationIO list
      // Whether the graph contains synchronous logic.
      IsSynchronous: bool
      // The base that should be used to display numbers in the simulation.
      NumberBase: NumberBase
      // Keep track of the number of clock ticks of the simulation.
      ClockTickNumber: int }

let graph_ = Optics.Lens.create (fun a -> a.Graph) (fun s a -> {a with Graph = s})
let fastSim_ = Optics.Lens.create (fun a -> a.FastSim) (fun s a -> {a with FastSim = s})
let numberBase_ = Optics.Lens.create (fun a -> a.NumberBase) (fun s a -> {a with NumberBase = s})
let clockTickNumber_ = Optics.Lens.create (fun a -> a.ClockTickNumber) (fun s a -> {a with ClockTickNumber = s})

/// document current status of a simulation as used by waveform simulator
type SimulationRunStatus =
    | SimEmpty // simulation has been created but not yet setup from a circuit
    | SimOutOfDate // one of more of the sheets being simulated has changed after the simulation was setup
    | SimValidSameSheet
    /// simulation has run and is currently uptodate. The current sheet is being simulated
    | SimValidDifferentSheet // The simulation is uptodate, but a differnt sheet from the current one is being simulated
    | SimNoProject // there is no open project - this should not normally happen.


//-------------------------------------------------------------------------------------//
//-------------------Helper functions for simulation types-----------------------------//
//-------------------------------------------------------------------------------------//

let sprintSimComponent (sComp: SimulationComponent) =
    sprintf "'%A': %20s" sComp.Label (sComp.Type.ToString() |> Helpers.sprintInitial 20)

let shortPSComp (comp: SimulationComponent) =
    let lab =
        match comp.Label with
        | ComponentLabel lab' -> lab'

    match comp.Type with
    | Custom sc -> sprintf "%s:Custom.(%s.%A->%A)" lab sc.Name sc.InputLabels sc.OutputLabels
    | _ -> sprintf "%s:%A" lab comp.Type

let printSimGraph (sg: SimulationGraph) =
    printfn
        "%s"
        (String.concat
            "\n"
            (sg
             |> Map.toList
             |> List.map (fun (ComponentId id, comp) -> sprintSimComponent comp + id)))

let tryGetCompLabel (compId: ComponentId) (sg: SimulationGraph) =
    Map.tryPick (fun k v -> if k = compId then Some v else None) sg
    |> Option.map (fun comp -> comp.Label)
    |> Option.map (fun (ComponentLabel s) -> s)
    |> Option.defaultValue "'Not in SimGraph'"

let extractLabel (label: ComponentLabel) =
    let (ComponentLabel name) = label
    name

//-------------------------------------------------------------------------------------//
//-------------------Helper functions for WaveformSim----------------------------------//
//-------------------------------------------------------------------------------------//

// NB - all the NetGroup functions assume a working netlist in which NO NET IS UNDRIVEN
// Every Net must be driven by exactly one componnet output port (NLSource).
// IOLabels are nout counted as drivers themselves; every group of same label IOlabels
// and all of their output nets
// makes a netgroup which must be driven by just one NLSource (connected to one of the IOLabel inputs).
// every net is therefore part of one netgroup which is either a single net, or a group of nets associated
// with a set of IOLabel connectors having a given common label.

let mapKeys (map: Map<'a, 'b>) = Map.keys map |> Array.ofSeq
let mapValues (map: Map<'a, 'b>) = Map.values map |> Array.ofSeq
let mapItems (map: Map<'a, 'b>) = Map.toArray map


