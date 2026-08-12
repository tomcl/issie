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
        // built once and used for both passes - see EvalCompiled.reducerFor. Only the fallback
        // distinguishes them, because only the hybrid components it serves need it to.
        match EvalCompiled.reducerFor fc with
        | Some reduce ->
            fc.ReduceComb <- reduce
            fc.ReduceClocked <- reduce
        | None ->
            fc.ReduceComb <- fun step -> fastReduce step false fc
            fc.ReduceClocked <- fun step -> fastReduce step true fc

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

/// The width limit the algebraic evaluator behind a truth table works to, checked at the door.
///
/// EvalAlgebraic works in uint32. Refusing a wider bus here, once, is what lets each of its
/// reducers assume the limit instead of testing for it - and it is a Result rather than an
/// exception, so the truth table tab can say which component is too wide. TruthTableView refuses
/// before this, alongside its check that the circuit is combinational, so a user meets the
/// explanation there and this is the backstop for any other caller.
let private checkWidthsForFData (fs: FastSimulation) : Result<FastSimulation, SimulationError> =
    let tooWide =
        fs.FComps
        |> Map.toList
        |> List.collect (fun (_, fc) ->
            fc.Outputs
            |> Array.toList
            |> List.map (fun out -> fc, out.Width))
        |> List.filter (fun (_, width) -> width > TruthTableTypes.Constants.maxTruthTableBusWidth)

    match tooWide with
    | [] -> Ok fs
    | (fc, width) :: _ ->
        Error
            { ErrType =
                GenericSimError (
                    sprintf
                        "A truth table is made for combinational logic narrow enough to tabulate, and \
                         '%s' carries a %d-bit bus - wider than the %d bits this supports. Use the Wave \
                         Simulation tab to see a wide design working, or select a narrower part of the \
                         sheet for a table."
                        fc.FullName
                        width
                        TruthTableTypes.Constants.maxTruthTableBusWidth)
              InDependency = None
              ComponentsAffected = tooWide |> List.map (fun (fc, _) -> fc.cId)
              ConnectionsAffected = [] }

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

    // before ordering, which reduces every component: a bus too wide to tabulate must not reach a
    // reducer at all
    gather
    |> createFastArrays fs
    |> checkWidthsForFData
    |> Result.map (orderCombinationalComponentsFData simulationArraySize)
    |> Result.bind checkAndValidateFData
    |> Result.map addWavesToFastSimulation // REVIEW - Waves are not used in TruthTable, mark for removal

