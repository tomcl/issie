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

/// The three arrays the run loop and the ordering pass iterate, taken from `comps` - every
/// component of the build, in the order the gather created them.
///
/// Not just to avoid building three throwaway Maps to filter with, though it does that. The
/// components are ALLOCATED in gather order, so walking them in that order walks memory forwards;
/// walking them through a map keyed by (ComponentId, access path) - which is what a built
/// simulation offered when this was written - visits the same objects in an order unrelated to
/// where they sit. On a 15,000-component design that difference was worth more
/// than everything else in this phase put together - and it applies to every pass that walks all
/// the components, which is why determineBigIntState and addWavesToFastSimulation take the array
/// too.
let createFastArrays (comps: FastComponent array) fs =
    let getArrayOf pred =
        comps |> Array.filter (fun fc -> not (isCustom fc.FType) && pred fc)

    { fs with
        FGlobalInputComps = getArrayOf (fun fc -> isInput fc.FType && fc.AccessPath = [])
        FConstantComps =
            getArrayOf (fun fc ->
                match fc.FType with
                | Constant1 _ -> true
                | _ -> false)
        FClockedComps = getArrayOf (fun fc -> couldBeSynchronousComponent fc.FType)
        FOrderedComps = Array.empty }

/// Create a fast simulation data structure, with all necessary arrays, and components
/// ordered for evaluation.
/// This function also creates the reducer functions for each component
/// similar to the reducer builder in Builder, but with inputs and outputs using the FastSimulation
/// mutable arrays
///
/// `waveTables` says whether to build the structures only a wave VIEWER reads - see WaveTables.
/// Everything else about the build is the same either way, deliberately: the two simulators must
/// run identical code over identical structures, or they agree until they do not.
let buildFastSimulationWith
    (waveTables: WaveTables)
    (simulationArraySize: int)
    (diagramName: string)
    (graph: SimulationGraph)
    : Result<FastSimulation, SimulationError>
    =
    
    // Each phase is marked with the time and the memory when it finished, and the table is
    // logged under the perf category. Memory is usedJSHeapSize, so a phase's delta is all its
    // allocation - step-array backing stores included - and garbage not yet collected counts,
    // which is the point: the transient peak during a build is real occupancy, whether or not
    // it survives. This is how the budget coefficients in SimulationBudget were measured, and
    // how to check them again when the structures change.
    let marks = ResizeArray<string * float * float>()
    let mark name (x: 'a) : 'a =
        if Log.isOn Log.Perf then marks.Add(name, getTimeMs (), usedHeapBytes ())
        x

    let logMarks (result: Result<FastSimulation, SimulationError>) =
        if Log.isOn Log.Perf && marks.Count > 1 then
            let name0, t0, m0 = marks[0]
            ((name0, t0, m0), Seq.skip 1 marks)
            ||> Seq.fold (fun (_, tPrev, mPrev) (name, t, m) ->
                Log.dbg Log.Perf $"build %-12s{name} %8.0f{t - tPrev}ms  %+6.0f{(m - mPrev) / 1.0e6}MB"
                (name, t, m))
            |> fun (_, tLast, mLast) ->
                Log.dbg Log.Perf $"build %-12s{diagramName} %8.0f{tLast - t0}ms  %+6.0f{(mLast - m0) / 1.0e6}MB total"
        result

    mark "start" () |> ignore

    // Before the design is even flattened, let alone allocated: what a clock cycle of it costs,
    // and how many components it comes to. The flatten now creates the step arrays as it goes, so
    // the refusal has to come before it, and the count is what sizes the store it fills.
    let cost, size = costAndSizeOfGraph graph

    checkSimulationFits simulationArraySize cost
    |> Result.bind (fun () ->
        // The step arrays this build allocates come from arena slabs rather than one external
        // allocation each - see the arena in FastCreate for what that buys and what it does not.
        // finally, so that a build that raises cannot leave its arena open for an unrelated
        // later build to draw from. It wraps the gather as well as the creation, because the
        // gather is where the step arrays are allocated.
        startStepArena cost simulationArraySize

        try
            let gather = gatherSimulation simulationArraySize size graph |> mark "gather"
            // every component of the build, in the order it was created: what the phases that walk
            // all of them use, instead of walking a map of them - see createFastArrays
            let comps = LookupArray.toArray gather.Comps

            let fs =
                emptyFastSimulation diagramName
                |> createInitFastCompPhase simulationArraySize gather
                |> mark "createInit"
                |> linkFastComponents gather
                |> mark "link"
                |> determineBigIntState comps // This step is not needed for TruthTable
                |> mark "bigIntState"

            createFastArrays comps fs
            |> mark "arrays"
            |> orderCombinationalComponents simulationArraySize
            |> mark "order"
            |> checkAndValidate
            |> mark "validate"
            // The custom-component linking happens either way - a port that is not re-pointed
            // reads the dummy array it was created with. Only the tables are optional.
            |> Result.map (linkCustomComponentPorts comps)
            |> Result.map (fun fs ->
                match waveTables with
                | WithWaveTables -> addWavesToFastSimulation comps fs
                | NoWaveTables -> fs)
            |> mark "waves"
            |> Result.map installReducers
            |> mark "reducers"
            |> Result.map (fun fs -> { fs with StepCost = cost })
        finally
            finishStepArena ())
    |> logMarks

/// A build with everything, which is what a simulation interrogated in this process needs.
let buildFastSimulation
    (simulationArraySize: int)
    (diagramName: string)
    (graph: SimulationGraph)
    : Result<FastSimulation, SimulationError>
    =
    buildFastSimulationWith WithWaveTables simulationArraySize diagramName graph

/// The width limit the algebraic evaluator behind a truth table works to, checked at the door.
///
/// EvalAlgebraic works in uint32. Refusing a wider bus here, once, is what lets each of its
/// reducers assume the limit instead of testing for it - and it is a Result rather than an
/// exception, so the truth table tab can say which component is too wide. TruthTableView refuses
/// before this, alongside its check that the circuit is combinational, so a user meets the
/// explanation there and this is the backstop for any other caller.
let private checkWidthsForFData (fs: FastSimulation) : Result<FastSimulation, SimulationError> =
    let tooWide =
        // the custom-component placeholders reduce nothing, so their ports are not what this
        // check is about
        fs.FCompsByIndex
        |> Array.toList
        |> List.filter (fun fc -> match fc.FType with Custom _ -> false | _ -> true)
        |> List.collect (fun fc ->
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
    // before the gather, which is what allocates the step arrays
    let cost, size = costAndSizeOfGraph graph

    checkSimulationFits simulationArraySize cost
    |> Result.bind (fun () ->
        startStepArena cost simulationArraySize

        try
            let gather = gatherSimulation simulationArraySize size graph
            let comps = LookupArray.toArray gather.Comps

            let fs =
                emptyFastSimulation diagramName
                |> createInitFastCompPhase simulationArraySize gather
                |> linkFastComponents gather

            // before ordering, which reduces every component: a bus too wide to tabulate must not reach a
            // reducer at all
            createFastArrays comps fs
            |> checkWidthsForFData
            |> Result.map (orderCombinationalComponentsFData simulationArraySize)
            |> Result.bind checkAndValidateFData
            |> Result.map (addWavesToFastSimulation comps) // REVIEW - Waves are not used in TruthTable, mark for removal
            |> Result.map (fun fs -> { fs with StepCost = cost })
        finally
            finishStepArena ())

