module FastBuild

open CommonTypes
open TimeHelpers
open SimGraphTypes
open SimTypes
open SynchronousUtils
open NumberHelpers
open FastCreate
open Helpers
open EvalReference
open FastOrder
open FastValidate

// Assembling a simulation: gather the components into the arrays the run loop iterates, order
// them, validate the result, and bind each component's reducer.

/// Bind each simulated component's reducer to the component itself, once, when the simulation
/// is built. The loop then calls a component's own code rather than dispatching on FType for
/// every component of every step. EvalCompiled.reducerFor returns None for a type it does not
/// handle yet, and that component keeps the general fastReduce.
///
/// This must run last. The reducers capture the step arrays their ports currently point at, and
/// linkFastComponents, reLinkIOLabels and addWavesToFastSimulation all re-point ports; a reducer
/// installed before them could hold an array the simulation no longer uses. It is also why only
/// the components that are actually reduced get one - the custom-component FastComponents that
/// addWavesToFastSimulation re-points exist for waveform display and are never reduced.
let installReducers (fs: FastSimulation) : FastSimulation =
    let install (fc: FastComponent) =
        fc.ReduceComb <-
            match EvalCompiled.reducerFor fc false with
            | Some reduce -> reduce
            | None -> fun step -> fastReduce step false fc

        fc.ReduceClocked <-
            match EvalCompiled.reducerFor fc true with
            | Some reduce -> reduce
            | None -> fun step -> fastReduce step true fc

    Array.iter install fs.FClockedComps
    Array.iter install fs.FOrderedComps
    fs

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
    |> Result.map installReducers

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

