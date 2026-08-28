module FastExtract

(*
    Functions that extract data from a fast simulation, or that manipulate the
    simulation input data. NB - WaveSimSVGs as a few more data extraction functions
    that cannot be placed here because they depend on model types.
    TODO: Refactor to make the boundary between FastSim and Model clearer.
*)

open CommonTypes
open SimGraphTypes
open SimTypes
open Helpers
open FastBuild
open FastRun
open NumberHelpers


/// sets the mutable simulation data for a given input at a given time step
let private setSimulationInput (cid: ComponentId) (fd: FastData) (step: int) (fs: FastSimulation) =
    match fs.ComponentOf(cid, []), fd.Width with
    | Some fc, w when w > 32 -> fc.Outputs[0].SetBig (step % fs.MaxArraySize) fd.GetBigInt
    | Some fc, w -> fc.Outputs[0].SetU32 (step % fs.MaxArraySize) fd.GetQUint32
    | None, _ -> failwithf "Can't find %A in FastSim" cid

let private setSimulationInputFData (cid: ComponentId) (fd: FData) (step: int) (fs: FastSimulation) =
    match fs.ComponentOf(cid, []) with
    | Some fc -> fc.Outputs[0].FDataStep[ step % fs.MaxArraySize ] <- fd
    | None -> failwithf "Can't find %A in FastSim" cid

/// Change an input and make simulation correct. N.B. step must be the latest
/// time-step since future steps are not rerun (TODO: perhaps they should be!)
let changeInput (cid: ComponentId) (input: FSInterface) (step: int) (fastSim: FastSimulation) =
    let fd =
        match input with
        | IData fd -> fd
        | IAlg _ -> failwithf "Algebraic inputs not supported."

    setSimulationInput cid fd step fastSim
    runCombinationalLogic step fastSim

let changeInputFData (cid: ComponentId) (input: FSInterface) (step: int) (fastSim: FastSimulation) =
    let fd =
        match input with
        | IData fd -> Data fd
        | IAlg exp -> Alg exp

    setSimulationInputFData cid fd step fastSim
    runCombinationalLogicFData step fastSim

/// Change multiple inputs in one batch before re-running the simulation
/// NOTE - Only used in TruthTable
let changeInputBatch (step: int) (fastSim: FastSimulation) (changes: (ComponentId * FSInterface) list) =

    changes
    |> List.iter (fun (cid, input) ->
        let fd =
            match input with
            | IData fd -> Data fd
            | IAlg exp -> Alg exp

        setSimulationInputFData cid fd step fastSim)

    runCombinationalLogicFData step fastSim

let extractStatefulComponents (step: int) (fastSim: FastSimulation) =
    fastSim.FClockedComps
    |> Array.collect (fun fc ->
        match fc.AccessPath with
        | [] ->
            match fc.FType with
            | Register w
            | RegisterE w
            | Counter w
            | CounterNoEnable w
            | CounterNoLoad w
            | CounterNoEnableLoad w when w > 32 ->
                [| fc,
                   RegisterState
                       { Dat = BigWord(fc.Outputs[0].Big (step % fastSim.MaxArraySize))
                         Width = w } |]
            | DFF
            | DFFE ->
                [| fc, RegisterState { Dat = Word(fc.Outputs[0].U32 (step % fastSim.MaxArraySize)); Width = 1 } |]
            | Register w
            | RegisterE w
            | Counter w
            | CounterNoEnable w
            | CounterNoLoad w
            | CounterNoEnableLoad w ->
                [| fc, RegisterState { Dat = Word(fc.Outputs[0].U32 (step % fastSim.MaxArraySize)); Width = w } |]
            // a ROM's contents are part of its type and never change, so it gets a read-only
            // store built here rather than one carried through the simulation
            | ROM1 state -> [| fc, RamState(RamStore.fixedOf state) |]
            | RAM1 _
            | AsyncRAM1 _ ->
                match
                    fc.State
                    |> Option.map (fun state -> state.Step[step % fastSim.MaxArraySize])
                with
                | None -> failwithf "Missing RAM state for step %d of %s" step fc.FullName
                | Some memState -> [| fc, memState |]
            | _ -> failwithf "Unsupported state extraction from clocked component type %s %A" fc.FullName fc.FType
        | _ -> [||])


/// The step array an output port's data lives in, resolved ONCE.
///
/// Everything `extractFastSimulationOutput` does per read except the read itself: the map lookup
/// on (component, access path) - a structural comparison on a list - and the hop through
/// FCustomOutputCompLookup that answers a custom component's output with the array of the Output
/// component inside the subsheet. What comes back is the array and its width, from which a value
/// at a cycle is an indexed load.
///
/// For anything reading MANY samples of the same signal. Reading a whole window per sample cost
/// that lookup, a FastData and an FSInterface allocation, and a conversion to bigint, per sample -
/// for a wave that is often one bit wide.
///
/// A Result, not an exception: the caller may be a server answering a request it did not compose,
/// where a bad signal name is an answer rather than a crash.
let rec tryFindOutputArray
    (fs: FastSimulation)
    ((cid, ap): FComponentId)
    (opn: OutputPortNumber)
    : Result<IOArray, string> =
    let (OutputPortNumber n) = opn

    match fs.ComponentOf(cid, ap) with
    | Some fc ->
        match Array.tryItem n fc.Outputs with
        | None -> Error $"{fc.FullName} has no output port {n}"
        // width 0 is the dummy array a port is created with: nothing was ever linked to it
        | Some io when io.Width = 0 -> Error $"output {n} of {fc.FullName} carries no data"
        | Some io -> Ok io
    | None ->
        match Map.tryFind ((cid, ap), opn) fs.FCustomOutputCompLookup with
        | Some inner -> tryFindOutputArray fs (fs.ComponentAt inner).fId (OutputPortNumber 0)
        | None -> Error $"no component {cid} at {ap} in this simulation"

/// Every step array of a build, dense by its own index - what turns a driver HANDLE back into
/// the array it names, in one indexed load.
///
/// A driver index is what the port slice hands the renderer (PortView.sheetSliceOf) and what a
/// read quotes back, so the simulator answering reads must be able to look one up without the
/// wave tables a lean build does not have. Slots nothing owns - a clocked component's state
/// array, allocated an index but not an IOArray - stay None, and a read naming one is an error.
///
/// Memoised on the simulation: one array per build, the length of NumStepArrays, filled by one
/// walk of the components. On the renderer this is never built - the renderer reads no arrays by
/// handle - so the memo costs nothing there.
let driverArraysOf: FastSimulation -> IOArray option array =
    Helpers.memoizeByIdentity (fun (fs: FastSimulation) ->
        let arrays: IOArray option array = Array.create fs.NumStepArrays None

        let add (io: IOArray) =
            if io.Index >= 0 && io.Index < arrays.Length then
                arrays[io.Index] <- Some io

        fs.FCompsByIndex
        |> Array.iter (fun fc ->
            Array.iter add fc.Outputs
            Array.iter add fc.InputLinks)
        arrays)

/// One sample of a resolved array, as a bigint whatever the width.
///
/// The modulo is not optional. A step array is a REGION of a shared slab, so a step past the end
/// of the region reads the next port's data with nothing to catch it - see IOArray.
let sampleOfArray (fs: FastSimulation) (io: IOArray) (cycle: int) : bigint option =
    let step = cycle % fs.MaxArraySize

    if io.Width > 32 then
        io.TryBig step
    else
        io.TryU32 step |> Option.map bigint

/// return output port data from simulation as a Bit list
/// Each element in list is one bit
let rec extractFastSimulationOutput
    (fs: FastSimulation)
    (step: int)
    ((cid, ap): FComponentId)
    (opn: OutputPortNumber)
    : FSInterface
    =
    let (OutputPortNumber n) = opn

    match fs.ComponentOf(cid, ap) with
    | Some fc ->
        match fc.OutputWidth n with
        | 0 ->
            failwithf
                $"Can't find valid data in step {step}:index{step % fs.MaxArraySize} from {fc.FullName} with clockTick={fs.ClockTick}"
        | w when w > 32 ->
            match fc.Outputs[n].TryBig (step % fs.MaxArraySize) with
            | None ->
                failwithf
                    $"What? extracting output {n}- in step {step} from {fc.FullName} failed with clockTick={fs.ClockTick}"
            | Some d -> { Dat = BigWord d; Width = w } |> IData
        | w ->
            match fc.Outputs[n].TryU32 (step % fs.MaxArraySize) with
            | None ->
                failwithf
                    $"What? extracting output {n}- in step {step} from {fc.FullName} failed with clockTick={fs.ClockTick}"
            | Some d -> { Dat = Word d; Width = w } |> IData

    | None ->
        // if it is a custom component output extract from the corresponding Output FastComponent
        match Map.tryFind ((cid, ap), opn) fs.FCustomOutputCompLookup with
        | Some inner -> extractFastSimulationOutput fs step (fs.ComponentAt inner).fId (OutputPortNumber 0)
        | None -> failwithf "What? extracting component data failed - can't find component from id"

let rec extractFastSimulationOutputFData
    (fs: FastSimulation)
    (step: int)
    ((cid, ap): FComponentId)
    (opn: OutputPortNumber)
    : FSInterface
    =

    let (OutputPortNumber n) = opn

    match fs.ComponentOf(cid, ap) with
    | Some fc ->
        match Array.tryItem (step % fs.MaxArraySize) fc.Outputs[n].FDataStep with
        | None ->
            failwithf
                $"What? extracting output {n} in step {step} from {fc.FullName} failed with clockTick={fs.ClockTick}"
        | Some(Data d) ->
            if d.Width = 0 then
                failwithf
                    $"Can't find valid data in step {step}:index{step % fs.MaxArraySize} from {fc.FullName} with clockTick={fs.ClockTick}"
            else
                d |> IData
        | Some(Alg exp) ->
            let evaluated = evalExp exp
            IAlg evaluated

    | None ->
        // if it is a custom component output extract from the corresponding Output FastComponent
        match Map.tryFind ((cid, ap), opn) fs.FCustomOutputCompLookup with
        | Some inner ->
            extractFastSimulationOutputFData fs step (fs.ComponentAt inner).fId (OutputPortNumber 0)
        | None -> failwithf "What? extracting component data failed - can't find component from id"

/// return state data from simulation
let rec extractFastSimulationState
    (fs: FastSimulation)
    (step: int)
    ((cid, ap): FComponentId)
    : SimulationComponentState
    =

    match fs.ComponentOf(cid, ap) with
    | Some fc ->
        match fc.State with
        | None ->
            match fc.SimComponent.Type with
            // asynch ROMs have no state: value is always the same
            | AsyncROM1 romContents -> RamState(RamStore.fixedOf romContents)
            | _ -> failwithf "What? extracting State in step %d from %s failed" step fc.FullName
        | Some stepArr ->
            match Array.tryItem (step % fs.MaxArraySize) stepArr.Step with
            | Some state -> state
            | None -> failwithf $"What? Can't extract state in step {step} from {fc.FullName}"
    | None -> failwithf $"What? Can't find fast component {(cid, ap)}"

/// Extract top-level inputs or outputs with names and wire widths. Used by legacy code.
let extractFastSimulationIOs
    (simIOs: SimulationIO list)
    (simulationData: SimulationData)
    : (SimulationIO * FSInterface) list
    =
    let fs = simulationData.FastSim
    let inputs = simulationData.Inputs

    simIOs
    |> List.map (fun ((cid, label, width) as io) ->
        let out =
            extractFastSimulationOutput fs simulationData.ClockTickNumber (cid, []) (OutputPortNumber 0)
        io, out)

/// As extractFastSimulationIOs but for FData - used by truth table logic.
let extractFastSimulationIOsFData
    (simIOs: SimulationIO list)
    (simulationData: SimulationData)
    : (SimulationIO * FSInterface) list
    =
    let fs = simulationData.FastSim
    let inputs = simulationData.Inputs

    simIOs
    |> List.map (fun ((cid, label, width) as io) ->
        let out =
            extractFastSimulationOutputFData fs simulationData.ClockTickNumber (cid, []) (OutputPortNumber 0)
        io, out)

/// Extract a component's label and its full name which includes all of its path to root of simulation.
let getFLabel (fs: FastSimulation) (fId: FComponentId) =
    let fc = fs.FastComponentOf fId
    let (ComponentLabel name) = fc.SimComponent.Label
    name, fc.FullName

/// Extract the width of a component's output port.
let extractFastSimulationWidth (fs: FastSimulation) (fid: FComponentId) (opn: OutputPortNumber) =
    let (OutputPortNumber n) = opn
    (fs.FastComponentOf fid).OutputWidth n

/// Extract all Viewer components with names and wire widths. 
let extractViewers (simulationData: SimulationData) : ((string * string) * int * FSInterface) list =
    let fs = simulationData.FastSim

    let viewers =
        simulationData.FastSim.FCompsByIndex
        |> Array.filter (fun fc ->
            match fc.FType with
            | Viewer _ -> true
            | _ -> false)

    viewers
    |> Array.toList
    |> List.map (fun fc ->
        let width = fc.OutputWidth 0
        getFLabel fs fc.fId,
        width,
        extractFastSimulationOutput fs simulationData.ClockTickNumber fc.fId (OutputPortNumber 0))

/// Check if the components and connections of a fast simulation and a canvas are the same.
let compareLoadedStates (fs: FastSimulation) (canv: CanvasState) (p: Project option) =
    List.forall (fun ldc -> CanvasExtractor.loadedComponentIsSameAsProject canv ldc p) fs.SimulatedCanvasState

/// Get the input value of a fast component as a bigint.
/// fc: the fast component to get the input from.
/// inputNum: which input as numbered in the inputs array.
/// step: which time step to get the value from.
/// used by waveSimSVGs, maybe this function could be moved there and optimised as code there at cost of modularity.
let inline getFastComponentInput (fc: FastComponent) (inputNum: int) (step:int) : bigint =
    let a = fc.InputLinks[inputNum]
    let w = a.Width
    match w with
    | w when w > 32 -> a.Big step
    | _ -> a.U32 step |> bigint


/// Get the output value of a fast component as a bigint.
/// fc: the fast component to get the output from.
/// outputNum: which output as numbered in the outputs array.
/// step: which time step to get the value from.
/// used by waveSimSVGs, maybe this function could be moved there and optimised as code there at cost of modularity.
let inline getFastComponentOutput (fc:FastComponent) (outputNum: int) (step:int) =
    if fc.OutputWidth 0 > 32 then
        fc.Outputs[outputNum].Big step
    else
        bigint (fc.Outputs[outputNum].U32 step)

/// Check if a fastComponent output is the same as the default value for all time steps.
/// Used by simulationView.
/// The current implementation seems not quite right when tick > MaxArraySize.
/// ToDo - make this work for all time steps
let outputsAreTheSameAsDefault (fs: FastSimulation) (fc: FastComponent) (tick: int) (currdefault: bigint) =
    let outputarray =
        if fc.OutputWidth 0 > 32 then
            fc.Outputs[0].BigContents
        else
            Array.map (fun (x: uint32) -> twosComp (fc.OutputWidth 0) (bigint x)) fc.Outputs[0].U32Contents
    let slicedArray = Array.sub outputarray 0 ((tick+1) % fs.MaxArraySize)
    let areAllElementsSame (arr: bigint array) =
        match tick with
        | n when n < 2 ->
            true
        | _ ->
            Array.forall (fun elem -> elem = currdefault) arr
    areAllElementsSame slicedArray

/// Return array of simulation output values for a given output port.
/// Values are returned from steos 0 to (tick - 1).
/// Used by testParsser. Only works if tick < maxArraySize.
let getArrayOfOutputs (fc: FastComponent) (outputNum: int) (ticks: int) : bigint array =
    (if fc.Outputs[outputNum].UInt32Slab.Length > 0
    then 
        Array.map (fun (d:uint32) -> bigint d) fc.Outputs[outputNum].U32Contents
    else
        fc.Outputs[0].BigContents)
    |> (fun a -> Array.sub a 0 (ticks - 1))



(******************************************************************************
 *                                                                            *
 * NOTE: See WaveSimSVGs module for additional functions that extract data    *
 * from a fast simulation. These cannot be included here because they depend  *
 * on Model datatypes                                                         *
 *                                                                            *
 ******************************************************************************)
