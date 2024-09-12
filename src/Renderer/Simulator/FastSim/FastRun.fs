module FastRun

open CommonTypes
open TimeHelpers
open SimGraphTypes
open SimTypes
open SynchronousUtils
open NumberHelpers
open FastCreate
open FastReduce
open FastReduceTT
open Helpers
open System.Numerics

//-------------------------------------------------------------------------------------------------------//
//-----------Functions to Determine Component Reduction Order, and to Run the Fast Simulation------------//
//-------------------------------------------------------------------------------------------------------//

//--------------------------Determine Correct omponent Reduction Order-----------------------------------//

module Constants =
    /// a number bigger than any possible simulation time in ms
    let maxSimulationTime = 1.0 ** 10.0
    /// used to prevent time instrument overhead in simulation - too large and simulations prevent responsiveness
    let numberOfStepsBeforeTimeCheck = 5

/// Invalid data is used as default to determine which inputs have been given data when ordering components
let private isValidFData (fd: FData) =
    match fd with
    | Data d -> d <> emptyFastData
    | _ -> false

let private isValidData (fd: IOArray) = fd.Width <> 0

/// True if the component is combinational
let inline isComb (comp: FastComponent) =
    match comp.FType with
    | Input1 _ when comp.AccessPath = [] -> false
    | AsyncRAM1 _ -> true
    | ct when couldBeSynchronousComponent ct -> false
    | _ -> true

/// True if all conditions are fulfiled for the component to be in the next batch to be reduced.
/// Used when ordering components.
let inline canBeReduced (fs: FastSimulation) (step: int) (fc: FastComponent) =
    fc.NumMissingInputValues = 0
    && not fc.Touched
    && fc.Active
    && isComb fc

/// print function for debugging
let printComp (fs: FastSimulation) (step: int) (fc: FastComponent) =
    let attr =
        [ if isComb fc then "Co" else "  "
          if fc.Touched then "T" else "U"
          if fc.Active then "Act" else "Inact"
          "    "
          (fc.InputLinks
           |> Array.map (fun (arr: IOArray) ->
               ((arr.UInt32Step.Length > 0)
                || (arr.BigIntStep.Length > 0))
               && isValidData arr)
           |> Array.map (function
               | true -> "*"
               | false -> "X")
           |> String.concat "") ]
        |> String.concat ""

    let ins =
        (fc.InputDrivers
         |> Array.map (
             Option.map (fun (fid, _) ->
                 let fc = fs.FComps[fid]
                 fc.FullName, fc.ShortId)
         ))

    sprintf "%25s %s %15s %A %A" fc.ShortId fc.FullName attr (canBeReduced fs step fc) ins

/// print function for debugging
let private printComps (step: int) (fs: FastSimulation) =

    fs.FComps
    |> mapValues
    |> Array.map (fun fComp -> printComp fs step fComp)
    |> String.concat "\n"
    |> printfn "COMPONENTS\n----------------\n%s\n---------------"

/// Create arrays of components in corrected format for efficient reduction
/// Combinational components are ordered: clokced, constant, global input components are
/// separated.
let private orderCombinationalComponents (numSteps: int) (fs: FastSimulation) : FastSimulation =
    let startTime = getTimeMs ()
    let mutable readyToReduce: FastComponent list = []
    let mutable orderedComps: FastComponent list = fs.FConstantComps |> Array.toList

    let propagateEval (fc: FastComponent) =
        fc.DrivenComponents
        |> List.iter (fun fc' ->
            fc'.NumMissingInputValues <- fc'.NumMissingInputValues - 1

            if canBeReduced fs 0 fc' then
                readyToReduce <- fc' :: readyToReduce)

    let init fc =
        fastReduce 1 0 false fc
        fc.Touched <- true
        propagateEval fc

    let initInput (fc: FastComponent) =
        let inputVal: uint32 =
            match fc.FType with
            | Input1(w, defaultVal) ->
                match defaultVal with
                | Some defaultVal -> defaultVal
                | None -> 0I
            | _ ->
                printf "non-input type component in initInput"
                0I
            |> uint32
        //printfn "Init input..."
        // REVIEW - Input initialisation is no longer required
        // fc.InputLinks[0].FastDataStep
        // |> Array.iteri (fun i _ -> fc.InputLinks[0].FastDataStep[ i ] <- convertIntToFastData (fc.OutputWidth 0) 0u)
        //printfn "Initialised input: %A" fc.InputLinks
        fastReduce fs.MaxArraySize 0 false fc
        fc.Touched <- true
        propagateEval fc

    let initClockedOuts (fc: FastComponent) =
        fc.Outputs
        |> Array.iteri (fun i vec ->
            if not (isHybridComponent fc.FType) then
                fc.Touched <- true
                propagateEval fc

            match fc.FType, (fc.OutputWidth i) with
            | RAM1 mem, w
            | AsyncRAM1 mem, w ->
                match fc.State with
                | Some arr -> arr.Step[0] <- RamState mem
                | _ -> failwithf "Component %s does not have correct state vector" fc.FullName

                let initD =
                    match Map.tryFind 0I mem.Data with
                    | Some n -> convertBigintToFastData w n
                    | _ -> convertIntToFastData w 0u
                // change simulation semantics to output 0 in cycle 0
                match vec.Width with
                | w when w <= 32 -> vec.UInt32Step[0] <- 0u
                | w -> vec.BigIntStep[0] <- 0I
            | _, w ->
                match vec.Width with
                | w when w <= 32 -> vec.UInt32Step[0] <- 0u
                | w -> vec.BigIntStep[0] <- 0I)

    /// print function for debugging
    let pp (fL: FastComponent array) =
        Array.map (fun fc -> sprintf "%A (%A)" fc.FullName fc.FType) fL
        |> String.concat ","
    //printComps 0 fs
    //printfn "Ordering %d clocked outputs: " fs.FClockedComps.Length
    fs.FClockedComps |> Array.iter initClockedOuts
    //printfn "Ordering %d constants" fs.FConstantComps.Length
    fs.FConstantComps |> Array.iter init
    //printfn "Ordering %d global inputs" fs.FGlobalInputComps.Length
    fs.FGlobalInputComps |> Array.iter initInput
    (*printfn "Loop init done"
    //"%d constant, %d input, %d clocked, %d ready to reduce from %d"
    fs.FConstantComps.Length
    fs.FGlobalInputComps.Length
    fs.FClockedComps.Length
    readyToReduce.Length
    fs.FComps.Count*)

    while readyToReduce.Length <> 0 do
        //printf "Adding %d combinational components %A" nextBatch.Length (pp nextBatch)
        let readyL = readyToReduce
        readyToReduce <- []

        readyL
        |> List.iter (fun fc ->
            fastReduce fs.MaxArraySize 0 false fc // this is always a combinational reduction
            orderedComps <- fc :: orderedComps
            fc.Touched <- true
            propagateEval fc)

    let orderedSet =
        orderedComps
        |> List.toArray
        |> Array.map (fun co -> co.fId)
        |> Set

    instrumentTime "orderCombinationalComponents" startTime

    { fs with FOrderedComps = orderedComps |> Array.ofList |> Array.rev }

let private orderCombinationalComponentsFData (numSteps: int) (fs: FastSimulation) : FastSimulation =
    let startTime = getTimeMs ()
    let mutable readyToReduce: FastComponent list = []
    let mutable orderedComps: FastComponent list = fs.FConstantComps |> Array.toList

    let propagateEval (fc: FastComponent) =
        fc.DrivenComponents
        |> List.iter (fun fc' ->
            fc'.NumMissingInputValues <- fc'.NumMissingInputValues - 1

            if canBeReduced fs 0 fc' then
                readyToReduce <- fc' :: readyToReduce)

    let init fc =
        fastReduceFData 0 0 false fc
        fc.Touched <- true
        propagateEval fc

    let initInput (fc: FastComponent) =
        let inputVal: uint32 =
            match fc.FType with
            | Input1(w, defaultVal) ->
                match defaultVal with
                | Some defaultVal -> defaultVal
                | None -> 0I
            | _ ->
                printf "non-input type component in initInput"
                0I
            |> uint32
        //printfn "Init input..."
        fc.InputLinks[0].FDataStep
        |> Array.iteri (fun i _ -> fc.InputLinks[0].FDataStep[ i ] <- Data(convertIntToFastData (fc.OutputWidth 0) 0u))
        //printfn "Initialised input: %A" fc.InputLinks
        fastReduceFData fs.MaxArraySize 0 false fc
        fc.Touched <- true
        propagateEval fc

    let initClockedOuts (fc: FastComponent) =
        fc.Outputs
        |> Array.iteri (fun i vec ->
            if not (isHybridComponent fc.FType) then
                fc.Touched <- true
                propagateEval fc

            match fc.FType, (fc.OutputWidth i) with
            | RAM1 mem, w
            | AsyncRAM1 mem, w ->
                match fc.State with
                | Some arr -> arr.Step[0] <- RamState mem
                | _ -> failwithf "Component %s does not have correct state vector" fc.FullName

                let initD =
                    match Map.tryFind 0I mem.Data with
                    | Some n -> convertBigintToFastData w n
                    | _ -> convertIntToFastData w 0u
                // change simulation semantics to output 0 in cycle 0
                vec.FDataStep[0] <- Data(convertIntToFastData w 0u)
            | _, w -> vec.FDataStep[0] <- Data(convertIntToFastData w 0u))

    /// print function for debugging
    let pp (fL: FastComponent array) =
        Array.map (fun fc -> sprintf "%A (%A)" fc.FullName fc.FType) fL
        |> String.concat ","
    //printComps 0 fs
    //printfn "Ordering %d clocked outputs: " fs.FClockedComps.Length
    fs.FClockedComps |> Array.iter initClockedOuts
    //printfn "Ordering %d constants" fs.FConstantComps.Length
    fs.FConstantComps |> Array.iter init
    //printfn "Ordering %d global inputs" fs.FGlobalInputComps.Length
    fs.FGlobalInputComps |> Array.iter initInput
    //printfn "Loop init done"
    printfn
        "%d constant, %d input, %d clocked, %d ready to reduce from %d"
        fs.FConstantComps.Length
        fs.FGlobalInputComps.Length
        fs.FClockedComps.Length
        readyToReduce.Length
        fs.FComps.Count

    while readyToReduce.Length <> 0 do
        //printf "Adding %d combinational components %A" nextBatch.Length (pp nextBatch)
        let readyL = readyToReduce
        readyToReduce <- []

        readyL
        |> List.iter (fun fc ->
            fastReduceFData fs.MaxArraySize 0 false fc // this is always a combinational reduction
            orderedComps <- fc :: orderedComps
            fc.Touched <- true
            propagateEval fc)

    let orderedSet =
        orderedComps
        |> List.toArray
        |> Array.map (fun co -> co.fId)
        |> Set

    instrumentTime "orderCombinationalComponents" startTime

    { fs with FOrderedComps = orderedComps |> Array.ofList |> Array.rev }

/// Calculates the size in bytes / clock step of the simulation arrays
/// bigint arrayshave very uncertain size, so an estimate of 16 bytes is assumed.
/// this should be revisited for more accuracy
let calculateTotalSimArraySizePerStep (activeComps: FastComponent array) (fs: FastSimulation) =
    let arraySumBy f a = if Array.length a = 0 then 0 else Array.sumBy f a
    activeComps
    |> arraySumBy (fun fc ->
        fc.Outputs
        |> arraySumBy (fun output ->
            let width = output.Width
            if width > 32 then
                16
            else
                4))
    |> (fun size -> {fs with TotalArraySizePerStep = size})
    

/// Check all the active FastComponents to ensure everything is valid
/// Use data from initialisation to write any not-yet-written component output widths
let checkAndValidate (fs: FastSimulation) =
    let start = getTimeMs ()
    let activeComps =
        fs.FComps
        |> mapValues
        |> Array.filter (fun fc -> fc.Active)

    let inSimulationComps =
        [| Array.filter (fun fc -> not (isHybridComponent fc.FType)) fs.FClockedComps
           fs.FGlobalInputComps
           fs.FOrderedComps |]
        |> Array.concat

    if (activeComps.Length <> inSimulationComps.Length) then
        printf
            "Validation problem: %d active components, %d components in simulation"
            activeComps.Length
            inSimulationComps.Length

        inSimulationComps
        |> Array.iter (fun fc -> printfn "Simulation: %s\n" (printComp fs 0 fc))

        fs.FComps
        |> Map.iter (fun fid fc -> printfn "FComps: %s\n" (printComp fs 0 fc))

        let possibleCycleComps =
            Set(
                List.ofArray activeComps
                |> List.map (fun fc -> fc.SimComponent.Id)
            )
            - Set(
                List.ofArray inSimulationComps
                |> List.map (fun fc -> fc.SimComponent.Id)
            )
            |> Set.toList

        Error
            { ErrType = CycleDetected "Issie has discovered an asynchronous cyclic path in your circuit - probably through asynchronous RAM address and dout ports. This is not allowed.\
                    This cycle detection is not precise, the components in red comprise this cycle and all components driven only from it"
              InDependency = None
              ComponentsAffected = possibleCycleComps
              ConnectionsAffected = [] }

    // check and add (if necessary) output widths
    else
        activeComps
        |> Array.iter (fun fc ->
            fc.Outputs
            |> Array.iteri (fun i output ->
                let width = fc.Outputs[i].Width

                match width, output.Width with
                | n, m when n <> m ->
                    failwithf
                        "Inconsistent simulation data width found on signal output width %d from %A %s:%d"
                        m
                        fc.FType
                        fc.FullName
                        i
                | 0, _ ->
                    failwithf
                        "Unexpected output data width %A found on initialised component %A %s:%d"
                        width
                        fc.FType
                        fc.FullName
                        i
                | _ -> () // Ok in this case
            ))
        instrumentTime "checkAndValidate" start
        fs
        |> calculateTotalSimArraySizePerStep activeComps
        |> Ok

let checkAndValidateFData (fs: FastSimulation) =
    let start = getTimeMs ()
    let activeComps =
        fs.FComps
        |> mapValues
        |> Array.filter (fun fc -> fc.Active)

    let inSimulationComps =
        [| Array.filter (fun fc -> not (isHybridComponent fc.FType)) fs.FClockedComps
           fs.FGlobalInputComps
           fs.FOrderedComps |]
        |> Array.concat

    if (activeComps.Length <> inSimulationComps.Length) then
        printf
            "Validation problem: %d active components, %d components in simulation"
            activeComps.Length
            inSimulationComps.Length

        inSimulationComps
        |> Array.iter (fun fc -> printfn "Simulation: %s\n" (printComp fs 0 fc))

        fs.FComps
        |> Map.iter (fun fid fc -> printfn "FComps: %s\n" (printComp fs 0 fc))

        let possibleCycleComps =
            Set(
                List.ofArray activeComps
                |> List.map (fun fc -> fc.SimComponent.Id)
            )
            - Set(
                List.ofArray inSimulationComps
                |> List.map (fun fc -> fc.SimComponent.Id)
            )
            |> Set.toList

        Error
            { ErrType = CycleDetected "Issie has discovered an asynchronous cyclic path in your circuit - probably through asynchronous RAM address and dout ports. This is not allowed.\
                    This cycle detection is not precise, the components in red comprise this cycle and all components driven only from it"
              InDependency = None
              ComponentsAffected = possibleCycleComps
              ConnectionsAffected = [] }

    // check and add (if necessary) output widths
    else
        activeComps
        |> Array.iter (fun fc ->
            fc.Outputs
            |> Array.iteri (fun i output ->
                let data = fc.Outputs[i].FDataStep[0]

                match data.Width, output.Width with
                | n, m when n <> m ->
                    failwithf
                        "Inconsistent simulation data %A data found on signal output width %d from %s:%d"
                        data
                        m
                        fc.FullName
                        i
                | 0, _ ->
                    failwithf "Unexpected output data %A found on initialised component %s:%d" data fc.FullName i
                | _ -> () // Ok in this case
            ))

        instrumentTime "checkAndValidate" start
        Ok fs

let createFastArrays fs gather =
    let getArrayOf pred fComps =
        fComps
        |> Map.filter (fun cid comp -> pred comp)
        |> Map.toArray
        |> Array.map snd

    { fs with
        FGlobalInputComps =
            fs.FComps
            |> getArrayOf (fun fc -> isInput fc.FType && fc.AccessPath = [])
        FConstantComps =
            fs.FComps
            |> getArrayOf (fun fc ->
                match fc.FType with
                | Constant1 _ -> true
                | _ -> false)
        FClockedComps =
            fs.FComps
            |> getArrayOf (fun fc -> couldBeSynchronousComponent fc.FType)
        FOrderedComps = Array.empty
        FSComps = gather.AllComps
        G = gather }

/// Create a fast simulation data structure, with all necessary arrays, and components
/// ordered for evaluation.
/// This function also creates the reducer functions for each component
/// similar to the reducer builder in Builder, but with inputs and outputs using the FastSimulation
/// mutable arrays
let buildFastSimulation
    (simulationArraySize: int)
    (diagramName: string)
    (graph: SimulationGraph)
    : Result<FastSimulation, SimulationError>
    =
    
    let gather = gatherSimulation graph

    let fs =
        emptyFastSimulation diagramName
        |> createInitFastCompPhase simulationArraySize gather
        |> linkFastComponents gather
        |> determineBigIntState // This step is not needed for TruthTable

    gather
    |> createFastArrays fs
    |> orderCombinationalComponents simulationArraySize
    |> checkAndValidate
    |> Result.map addWavesToFastSimulation

let buildFastSimulationFData
    (simulationArraySize: int)
    (diagramName: string)
    (graph: SimulationGraph)
    : Result<FastSimulation, SimulationError>
    =
    let gather = gatherSimulation graph

    let fs =
        emptyFastSimulation diagramName
        |> createInitFastCompPhase simulationArraySize gather
        |> linkFastComponents gather

    gather
    |> createFastArrays fs
    |> orderCombinationalComponentsFData simulationArraySize
    |> checkAndValidateFData
    |> Result.map addWavesToFastSimulation // REVIEW - Waves are not used in TruthTable, mark for removal

//---------------------------------------------------------------------------------------------------//
//--------------------------------Code To Run The Simulation-----------------------------------------//
//---------------------------------------------------------------------------------------------------//

/// sets up default no-change input values for the next step
let private propagateInputsFromLastStep (step: int) (fastSim: FastSimulation) =
    let stepsim = 
        if step = 0 then
            fastSim.MaxArraySize
        else 
            step
    fastSim.FGlobalInputComps
    |> Array.iter (fun fc ->
        let vec = fc.Outputs[0]
        if vec.Width > 32 then
            vec.BigIntStep[step] <- vec.BigIntStep[stepsim - 1]
        else
            vec.UInt32Step[step] <- vec.UInt32Step[stepsim - 1])


let private setInputstoDefault (fastSim: FastSimulation) =
    fastSim.FGlobalInputComps
    |> Array.iter (fun fc ->
        match fc.FType with
        | Input1(w, defaultVal) ->
            match defaultVal with
            | Some defaultVal -> 
                let vec = fc.Outputs[0]
                if vec.Width > 32 then
                    vec.BigIntStep[0] <- defaultVal
                else
                    vec.UInt32Step[0] <- uint32 defaultVal
            | None -> ()
        | _ -> ()
    )

/// advance the simulation one step
let private stepSimulation (fs: FastSimulation) =
    let index = (fs.ClockTick + 1) % fs.MaxArraySize // index of circular array
    
    propagateInputsFromLastStep index fs
    Array.iter (fastReduce fs.MaxArraySize (fs.ClockTick + 1) true) fs.FClockedComps
    Array.iter (fastReduce fs.MaxArraySize (fs.ClockTick + 1) false) fs.FOrderedComps

    fs.ClockTick <- fs.ClockTick + 1

/// set simulation data for clock tick 0 when regenerating data
let private restartSimulation (fs: FastSimulation) =
    setInputstoDefault fs
    Array.iter (fastReduce fs.MaxArraySize 0 true) fs.FClockedComps
    Array.iter (fastReduce fs.MaxArraySize 0 false) fs.FOrderedComps

    fs.ClockTick <- 0

/// Re-evaluates the combinational logic for the given timestep - used if a combinational
/// input has changed
let runCombinationalLogic (step: int) (fastSim: FastSimulation) =
    fastSim.FOrderedComps
    |> Array.iter (fastReduce fastSim.MaxArraySize step false)

let runCombinationalLogicFData (step: int) (fastSim: FastSimulation) =
    fastSim.FOrderedComps
    |> Array.iter (fastReduceFData fastSim.MaxArraySize step false)


/// Run an existing fast simulation up to the given number of steps. This function will mutate the write-once data arrays
/// of simulation data and only simulate the new steps needed, so it may return immediately doing no work.
/// If the simulation data arrays are not large enough they are extended up to a limit. After that, they act as a circular buffer.
/// TimeOut if not None is the cutoff time after which the simulation terminates execution unfinished.
/// Use fs.ClockTick to determine whether simulation has completed.
/// returns speed, in clock cycles per ms, or None if complete
let runFastSimulation (timeOut: float option) (lastStepNeeded: int) (fs: FastSimulation) : float option =
    if fs.MaxArraySize = 0 then
        failwithf "ERROR: can't run a fast simulation with 0 length arrays!"
    // printfn $"running sim clocktick={fs.ClockTick}, arraySize = {fs.MaxArraySize}, laststepneeded={lastStepNeeded}"
    let simStartTime = getTimeMs ()
    let stepsToDo = lastStepNeeded - fs.ClockTick

    if stepsToDo <= 0 then
        if (fs.ClockTick - lastStepNeeded) < fs.MaxArraySize then
            None
        else 
            restartSimulation fs
            let startTick = fs.ClockTick
            let mutable time = simStartTime

            let stepsBeforeCheck = 100 // REVIEW - make this a parameter or move this to Constants

            match timeOut with
            | None ->
                while fs.ClockTick < lastStepNeeded do
                    stepSimulation fs
            | Some incr ->
                while fs.ClockTick < lastStepNeeded
                    && time < simStartTime + incr do
                    stepSimulation fs

                    if (fs.ClockTick - startTick) % stepsBeforeCheck = 0 then
                        time <- getTimeMs ()

            float (fs.ClockTick - startTick) / (getTimeMs () - simStartTime)
            |> Some
    else
        let startTick = fs.ClockTick
        let mutable time = simStartTime

        let stepsBeforeCheck = 100 // REVIEW - make this a parameter or move this to Constants

        match timeOut with
        | None ->
            while fs.ClockTick < lastStepNeeded do
                stepSimulation fs
        | Some incr ->
            while fs.ClockTick < lastStepNeeded
                  && time < simStartTime + incr do
                stepSimulation fs

                if (fs.ClockTick - startTick) % stepsBeforeCheck = 0 then
                    time <- getTimeMs ()

        float (fs.ClockTick - startTick) / (getTimeMs () - simStartTime)
        |> Some



