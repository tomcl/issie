(*
    Types.fs

    This module collects a series of types used in the simulator logic.
*)

module rec SimulatorTypes
open Fable.Core
open CommonTypes

/// Binary data used in simulation
type Bit = Zero | One

/// Fixed width bus data used in simulation
/// TODO: refactor as int64 or bigint for efficiency
/// The list is little-endian: the LSB is at index 0, and the MSB is at index N,
/// where N is the length of the list.
type WireData = Bit list

/// State (possibly none) remembered by component
/// from previous clock cycle. Combinational components
/// have no state.
type SimulationComponentState =
    | NoState // For all stateless components.
    | DffState of uint32
    | RegisterState of FastData
    | RamState of Memory1

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

type FastBits =
    | Word of dat:uint32 
    | BigWord of dat:bigint 
  
type FastData =
    {
        Dat : FastBits
        Width : int
    } with

    member inline this.GetBigInt = // always possible
        match this.Dat with
        | Word n -> bigint n
        | BigWord n -> n

    /// return Some uint32 representing data if possible else None
    member inline this.GetUint32 = // not possible if too large
        match this.Dat with
        | Word n -> Some n
        | BigWord n when this.Width <= 32 -> Some (uint32 n)
        | _ -> None
    /// can fail - for fast access to word data
    member inline this.GetQUint32 =
        match this.Dat with
        | Word n -> n
        | BigWord n when this.Width <= 32 -> uint32 n
        | _ -> failwithf $"Can't turn {this} into a uint32"

//------------------------------------------------------------------------------//
//-------------------EXPERIMENTAL - new data structure to replace WireData------//
//------------------------------------------------------------------------------//

type BitInt = uint32 array

let getBIBit (bits: BitInt) (pos:int) : uint32 =
    bits[pos / 32] >>> (pos % 32)

/// get a field of bits width 'width' offset by 'offset', return the field with 0 offset
let inline getUpperField (x:uint32) (width:int) (offset:int) : uint32 =
    (x >>> offset) &&& ((1u <<< width) - 1u)

/// get the lower 'width' bits, return then offset by 'offset' bits
let inline getLowerField (x:uint32) (width:int) (offset:int) : uint32 =
    (x &&& ((1u <<< width) - 1u)) <<< offset

let getBIBitsInt (bits:BitInt) (msb: int) (lsb:int) : uint32 =
    let width = msb - lsb + 1
    if width < 32 then
        let lowerWord = bits[lsb / 32]
        let offset = lsb % 32
        let lowerChunk = (lowerWord >>> offset) 
        if offset + width <= 32 then
            // output from only one word
            lowerChunk 
        else
            // one word output from two words of source
            let upperChunk = getLowerField bits[lsb / 32 + 1] (width - offset - 32) offset
            lowerChunk ||| upperChunk
    else
        failwithf "Cannot extract bits {msb}..{lsb} as a single 32 bit word"
            
let getBIBits  (bits:BitInt) (msb: int) (lsb:int) : BitInt =
    let lsw = lsb / 32
    let outWidth = msb - lsb + 1
    let msw = msb / 32
    let offset = lsb % 32
    if offset = 0 then
        bits[msw..lsw]
    else
        let outWords = outWidth / 32 + 1
        Array.init outWords (fun n ->
            match n with
            | n when n + lsw = msw ->
                getLowerField bits[msw] (outWidth - offset % 32) offset
            | n ->
                getLowerField bits[n + lsw + 1] (32 - offset) offset |||
                getUpperField bits[n + lsw] offset offset)

let floatCarryBit = Seq.init 32 (fun _ -> 2.) |> Seq.reduce (*)  

let addBIBits (bits1:BitInt) (bits2:BitInt) (cin: uint32) : BitInt * uint32 =
    let mutable tempCarry = if cin = 1u then floatCarryBit else 0.
    let outs =
        Array.init bits1.Length ( fun n ->
            tempCarry <- float bits1[n] + float bits2[n] + (if tempCarry >= floatCarryBit then 1. else 0.)
            uint32 tempCarry)
    outs, (if tempCarry >= floatCarryBit then 1u else 0u)

let binopBIBits (op: uint32 -> uint32 -> uint32) (bits1:BitInt) (bits2:BitInt) : BitInt =
    Array.init bits1.Length (fun n ->
        op bits1[n] bits2[n])

/// invert bits1: assuming that width is the bit width of bits1
/// MS bits not used by bits1 are not inverted.
let invertBIBits (bits:BitInt) (width: int) =
    let msw = width / 32
    Array.init bits.Length (fun n ->
        let x = bits[n]
        if n = msw then 
            x &&& ((1u <<< width % 32) - 1u)
        else x ^^^ 0xFFFFFFFFu)

/// append bits2 on MSB side of bits1
let appendBIBits ((bits1:BitInt,width1:int)) ((bits2:BitInt, width2:int)) =
    let outWidth = width1 + width2
    let outMSW = outWidth / 32
    let offset = width1 % 32
    let msw1 = width1 / 32
    if offset = 0 then
        // we can do straight array append
        Array.append bits1 bits2
    elif outMSW = width1 / 32 then
        // the added bits can be put in the existing MSW of width1
        let out = Array.copy bits1
        out[outMSW] <- out[outMSW] ||| getLowerField bits2[0] width2 offset
        out
    else
        Array.init (outMSW + 1) (fun n ->
         match n with
         | _ when n = outMSW -> 
            getLowerField bits2[n - msw1] (32 - offset) offset |||
            getUpperField bits2[n - msw1 + 1] (offset + outWidth - 32) offset            

         | _ when n = width1 / 32 -> 
            getLowerField bits1[n - width1 % 32] (32 - offset) offset |||
            getUpperField bits2[n - width1 / 32 + 1] offset offset            
         | _ when n >= width1 / 32 -> 
            getLowerField bits2[n - width1 / 32] (32 - offset) offset |||
            getUpperField bits2[n - width1 / 32 + 1] offset offset            
         | _  -> 
            bits1[n])

let bigIntMaskA =
    [|1..128|]
    |> Array.map ( fun width -> (bigint 1 <<< width) - bigint 1)

let bigIntBitMaskA =
    [|0..128|]
    |> Array.map ( fun width -> (bigint 1 <<< width))
    
    
let bigIntMask width =
    if width <= 128 then bigIntMaskA[width] else (bigint 1 <<< width) - bigint 1

let bigIntBitMask pos =
    if pos <= 128 then bigIntBitMaskA[pos] else (bigint 1 <<< pos)   

let fastBit (n: uint32) =
#if ASSERTS
    Helpers.assertThat (n < 2u) (sprintf "Can't convert %d to a single bit FastData" n)
#endif
    { Dat = Word n; Width = 1}

let rec bitsToInt (lst: Bit list) =
    match lst with
    | [] -> 0u
    | x :: rest ->
        (if x = Zero then 0u else 1u)
        + (bitsToInt rest)*2u

let rec bitsToBig (lst: Bit list) =
    match lst with
    | [] -> bigint 0
    | x :: rest ->
        (if x = Zero then bigint 0 else bigint 1)
        + ((bitsToBig rest) <<< 1)

/// convert Wiredata to FastData equivalent
let rec wireToFast (wd: WireData) =
    let n = wd.Length
    let dat = 
        if n <= 32 then
            Word (bitsToInt wd)
        else 
            BigWord (bitsToBig wd)
    { Dat = dat; Width = n}

/// convert FastData to WireData equivalent
let rec fastToWire (f: FastData) =
    match f.Dat with
    | Word x ->
        [ 0 .. f.Width - 1 ]
        |> List.map
            (fun n ->
                if (x &&& (1u <<< n)) = 0u then
                    Zero
                else
                    One)
    | BigWord x ->
        [ 0 .. f.Width - 1 ]
        |> List.map
            (fun n ->
                if (x &&& bigIntBitMask n) = bigint 0 then
                    Zero
                else
                    One)

let fastDataZero = {Dat=Word 0u; Width = 1}
let fastDataOne = {Dat=Word 1u; Width = 1}

let rec b2s (b:bigint) =
    let lsw = b &&& ((bigint 1 <<< 32) - bigint 1)
    let hex = $"%08x{uint32 lsw}"
    let msws = b >>> 32
    if msws <> bigint 0 then
        b2s msws + hex
    else
        hex

/// Extract bit field (msb:lsb) from f. Bits are numbered little-endian from 0.
/// Note that for a single bit result the un-normalised version is used, so it will
/// be compatible with fast implementation of boolean logic.
let getBits (msb: int) (lsb: int) (f: FastData) =
    let outW = msb - lsb + 1
#if ASSERTS
    Helpers.assertThat
        (msb <= f.Width - 1 && lsb <= msb && lsb >= 0)
        (sprintf "Bits selected out of range (%d:%d) from %A" msb lsb f)
#endif
    match f.Dat with
    | Word x ->
        let bits = (x >>> lsb) &&& ((1u <<< (msb - lsb + 1)) - 1u)
        {Dat = Word bits; Width = outW}
    | BigWord x ->
        let mask = bigIntMask (msb - lsb + 1)
        let bits = (x >>> lsb) &&& mask
        //printfn $"lsb={lsb},msb={msb},outW={outW}, mask={b2s mask}, x={b2s x},x/lsb = {b2s(x >>> lsb)} bits={b2s bits}, bits=%x{uint32 bits}"
        let dat =
            if outW <= 32 then
                Word ((uint32 bits) &&& (1u <<< outW) - 1u)
            else
                BigWord (bits &&& bigIntMask outW)
        { Dat = dat; Width = outW}

let appendBits (fMS: FastData) (fLS: FastData) : FastData =
    let ms = fMS.Dat
    let ls = fMS.Dat
    let w = fMS.Width + fLS.Width
    let dat = 
        match ms, ls with
        | Word x1, Word x2 when w <= 32 -> Word((x1 <<< fLS.Width) + x2)
        | Word x1, Word x2 -> BigWord((bigint x1 <<< fLS.Width) + bigint x2)
        | _ -> BigWord((fMS.GetBigInt <<< fLS.Width) ||| fLS.GetBigInt)
    {Dat=dat;Width=w}

//---------------------------------------------------------------------------------------//   
//--------------------------------Fast Simulation Data Structure-------------------------//
//---------------------------------------------------------------------------------------//
   
type FComponentId = ComponentId * ComponentId list

type FData = FastData // for now...

/// Wrapper to allow arrays to be resized for longer simulations while keeping the links between inputs
/// and outputs
type StepArray<'T> = {
    // this field is mutable to allow resizing
    mutable Step: 'T array
    }

type FastComponent = {
    fId: FComponentId
    cId: ComponentId
    FType: ComponentType
    State: StepArray<SimulationComponentState> option
    mutable Active: bool
    OutputWidth: int option array
    InputLinks: StepArray<FData> array
    InputDrivers: (FComponentId * OutputPortNumber) option array
    Outputs: StepArray<FData> array
    SimComponent: SimulationComponent
    AccessPath: ComponentId list
    FullName: string
    // these fields are used only to determine component ordering for correct evaluation
    mutable Touched: bool // legacy field
    mutable DrivenComponents: FastComponent list
    mutable NumMissingInputValues: int
    // these fields are used only by the Verilog output code
    mutable VerilogOutputName: string array
    mutable VerilogComponentName: string

    } with

    member inline this.GetInput (epoch) (InputPortNumber n) = this.InputLinks[n].Step[epoch]
    member this.ShortId =
        let (ComponentId sid,ap) = this.fId
        (EEExtensions.String.substringLength 0 5 sid)
    member inline this.PutOutput (epoch) (OutputPortNumber n) dat = this.Outputs[n].Step[epoch] <- dat
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
    // last step number (starting from 0) which is simulated.
    mutable ClockTick: int
    // The step number of the last step that can be simulated in the
    // current simulation outputs
    mutable MaxStepNum: int 
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
    mutable FIOActive: Map<ComponentLabel*ComponentId list,FastComponent>
    // list of deferred links driven from inactive IOlabls - at end of linkage the
    // corresponding active IOLabel can be substituted as driver an dthe link made
    mutable FIOLinks: ((FastComponent*InputPortNumber)*FastComponent) list
    // Fast components: this array is longer than FOrderedComps because it contains
    // IOlabel components that are redundant in the simulation
    FComps: Map<FComponentId,FastComponent>
    FSComps: Map<FComponentId,SimulationComponent * ComponentId list>
    // look up from output port of custom component to the relevant Output component
    FCustomOutputCompLookup: Map<(ComponentId*ComponentId list)*OutputPortNumber, FComponentId>
    // GatherData from which this simulation was made
    G: GatherData
    } with
        member this.getSimulationData (step: int) ((cid,ap): FComponentId) (opn: OutputPortNumber) =
            let (OutputPortNumber n) = opn
            match Map.tryFind (cid,ap) this.FComps with
            | Some fc -> fc.Outputs[n].Step[step]
            | None ->
                match Map.tryFind ((cid,ap), opn) this.FCustomOutputCompLookup with
                | Some fid -> this.FComps[fid].Outputs[0].Step[step]
                | None -> failwithf "What? can't find %A in the fast simulation data" (cid,ap)

/// GatherTemp is the output type used to accumulate lists of data links when recursively exploring SimulationGraph
/// as first step in flattening it.
/// Each list of pairs is converted into a map at the end in the final GatherData structure
/// The cost of creating maps makes it important to use lists here as the intermediate structures
and GatherTemp = {
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
    AllCompsT: ((ComponentId*ComponentId list) * (SimulationComponent * ComponentId list)) list // links to component and its path in the graph
}

and  GatherData = {
    // Existing Issie data structure representing circuit for simulation - generated by runCanvasStateChecksAndBuildGraph
    Simulation: SimulationGraph
    // Maps Custom Component Id and input port number to corresponding Input 
    // Component Id (of an Input component which is not top-level)
    CustomInputCompLinks: Map<FComponentId * InputPortNumber, FComponentId>
    // Maps (non-top-level) Output component Id to corresponding Custom Component Id & output port number
    CustomOutputCompLinks: Map<FComponentId,  FComponentId * OutputPortNumber>
    // Maps custom component output to corresponding output FastComponent.
    // Inverse of CustomOutputCompLinks
    CustomOutputLookup: Map<FComponentId * OutputPortNumber, FComponentId>
    // Shortcut to find the label of a component; notice that the access path is not needed here because
    // Labels of the graph inside a custom component are identical for different instances of the component
    Labels: Map<ComponentId,string>
    // This indexes the SimulationGraph components from Id and access path. Note that the same simulation
    // component Id can appear with different access paths if a sheet is instantiated more than once.
    // Each entry corresponds to a single FastComponent.
    // Note that Custom components are not included in this list.
    AllComps: Map<ComponentId*ComponentId list,SimulationComponent * ComponentId list> // maps to component and its path in the graph
    // List of component Input components that are driven externally to simulation
    } with

    member this.getFullName (cid,ap) = 
            List.map ( fun cid -> match Map.tryFind cid this.Labels with | Some lab -> lab | None -> "*" ) (ap @ [cid])
            |> String.concat "."

/// ViewerWaveform added in later.
/// Normally can select top-level waveform and nothing else
/// Viewers can be on subsheets. Any viewer waveform can be selected.
/// ViewerWaveform - boolean maybe for whether it is shown or not? Not sure.
type WaveformType =
    | ViewerWaveform of bool
    //| IOLabelWaveform - not yet
    | NormalWaveform

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

let extractLabel (label: ComponentLabel) =
    let (ComponentLabel name) = label
    name

//-------------------------------------------------------------------------------------//
//-------------------Helper functions for WaveformSpec---------------------------------//
//-------------------------------------------------------------------------------------//

// NB - all the NetGroup functions assume a working netlist in which NO NET IS UNDRIVEN
// Every Net must be driven by exactly one componnet output port (NLSource).
// IOLabels are nout counted as drivers themselves; every group of same label IOlabels 
// and all of their output nets 
// makes a netgroup which must be driven by just one NLSource (connected to one of the IOLabel inputs).
// every net is therefore part of one netgroup which is either a single net, or a group of nets associated
// with a set of IOLabel connectors having a given common label.

let mapKeys (map:Map<'a,'b>) = Map.keys map |> Array.ofSeq
let mapValues (map:Map<'a,'b>) = Map.values map |> Array.ofSeq
let mapItems (map:Map<'a,'b>) = Map.toArray map

let getFastOutputWidth (fc: FastComponent) (opn: OutputPortNumber) =
    let (OutputPortNumber n) = opn
    fc.Outputs[n].Step[0].Width

let getFastDriver (fs: FastSimulation) (driverComp: NetListComponent) (driverPort: OutputPortNumber) =
    match driverComp.Type with
    | Custom _ ->
        let customFId: FComponentId = driverComp.Id, []
        let customOutput =
            Map.tryFind (customFId, driverPort) fs.FCustomOutputCompLookup
            |> function
            | Some x -> x
            | None -> failwithf "Cannot find custom component %A in fast simulation" (customFId, driverPort)
#if ASSERTS
        Helpers.assertThat (Map.containsKey customOutput fs.FComps)
            (sprintf "Help: can't find custom component output in fast Simulation")
#endif
        customOutput, OutputPortNumber 0
        
    | _ -> 
        (driverComp.Id, []), driverPort
