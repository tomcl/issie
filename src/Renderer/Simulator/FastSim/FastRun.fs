module FastRun

open CommonTypes
open TimeHelpers
open SimGraphTypes
open SimTypes
open SynchronousUtils
open NumberHelpers
open FastCreate
open Helpers
open EvalAlgebraic
open FastBuild

// Running a built simulation. Everything here is on the hot path or one step from it.

module Constants =
    /// a number bigger than any possible simulation time in ms
    let maxSimulationTime = 1.0e10
    /// used to prevent time instrument overhead in simulation - too large and simulations prevent responsiveness
    let numberOfStepsBeforeTimeCheck = 5


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
            vec.SetBig step (vec.Big (stepsim - 1))
        else
            vec.SetU32 step (vec.U32 (stepsim - 1)))


let private setInputstoDefault (fastSim: FastSimulation) =
    fastSim.FGlobalInputComps
    |> Array.iter (fun fc ->
        match fc.FType with
        | Input1(w, defaultVal) ->
            match defaultVal with
            | Some defaultVal -> 
                let vec = fc.Outputs[0]
                if vec.Width > 32 then
                    vec.SetBig 0 defaultVal
                else
                    vec.SetU32 0 (uint32 defaultVal)
            | None -> ()
        | _ -> ()
    )

/// advance the simulation one step
let private stepSimulation (fs: FastSimulation) =
    // where this step sits in the circular arrays, worked out once for the whole step rather
    // than again for each of its components
    let step = stepIndexOf fs.MaxArraySize (fs.ClockTick + 1)

    propagateInputsFromLastStep step.SimStep fs
    Array.iter (fun fc -> fc.ReduceClocked step) fs.FClockedComps
    Array.iter (fun fc -> fc.ReduceComb step) fs.FOrderedComps

    fs.ClockTick <- step.NumStep

/// set simulation data for clock tick 0 when regenerating data
let private restartSimulation (fs: FastSimulation) =
    let step = stepIndexOf fs.MaxArraySize 0
    setInputstoDefault fs
    Array.iter (fun fc -> fc.ReduceClocked step) fs.FClockedComps
    Array.iter (fun fc -> fc.ReduceComb step) fs.FOrderedComps

    fs.ClockTick <- 0

/// Re-evaluates the combinational logic for the given timestep - used if a combinational
/// input has changed
let runCombinationalLogic (stepNum: int) (fastSim: FastSimulation) =
    let step = stepIndexOf fastSim.MaxArraySize stepNum

    fastSim.FOrderedComps
    |> Array.iter (fun fc -> fc.ReduceComb step)

let runCombinationalLogicFData (step: int) (fastSim: FastSimulation) =
    fastSim.FOrderedComps
    |> Array.iter (fastReduceFData fastSim.MaxArraySize step false)


/// About how many component evaluations should pass between readings of the clock.
///
/// The unit is WORK, not cycles, and that is the whole point. A clock read costs tens of
/// nanoseconds and a component evaluation a few, so a thousand of them puts the clock well under
/// 1% - and because the count is work rather than cycles, the time between readings is roughly
/// constant whatever the design's size, which is what bounds how far a timed run can overrun its
/// budget.
let private pollWorkTarget = 1000

/// How many cycles to run between readings of the clock, for a simulation of this size.
///
/// The fixed 100 this replaces bounded neither cost nor overshoot. On a two-component sheet it read
/// the clock every hundred trivial cycles; on a 480,000-component design a hundred cycles is on the
/// order of a hundred milliseconds, so a hundred-millisecond budget overshot by about double. That
/// one constant was most of why timed running behaved badly at both ends of the size range - and it
/// appeared twice, because the loop it guarded was written out twice.
let private cyclesBetweenClockReads (fs: FastSimulation) =
    let components = max 1 (fs.FComps.Count + fs.FCustomComps.Count)
    max 1 (pollWorkTarget / components)

/// Run an existing fast simulation to `lastStepNeeded`, mutating its data arrays, and doing no
/// work if it is already there.
///
/// `timeOut` is a budget in ms, or None for "however long it takes". A budget that runs out is not
/// a failure and loses nothing: the reply says where the clock reached, and running again continues
/// from there. Nothing is inferred from how long the work took - see the time rule in
/// docs/dev/sidecarInvariants.md - so a clock that jumps mid-run costs one extra call and cannot
/// produce a wrong answer.
///
/// Asking for a cycle EARLIER than the clock is ordinary: the step simulator does it every time the
/// user steps back. It costs nothing while that cycle is still in the circular buffer, and restarts
/// the simulation when it is not.
let private runFastSimulationCore (timeOut: float option) (lastStepNeeded: int) (fs: FastSimulation) : RunOutcome =
    if fs.MaxArraySize = 0 then
        failwithf "ERROR: can't run a fast simulation with 0 length arrays!"

    // going further back than the buffer still holds: the answer is no longer there to be read
    if fs.ClockTick - lastStepNeeded >= fs.MaxArraySize then
        restartSimulation fs

    if fs.ClockTick >= lastStepNeeded then
        RunCompleted
    else
        let deadline = timeOut |> Option.map (fun budget -> getTimeMs () + budget)
        let cyclesPerRead = cyclesBetweenClockReads fs
        let mutable outOfTime = false

        while not outOfTime && fs.ClockTick < lastStepNeeded do
            let until = min lastStepNeeded (fs.ClockTick + cyclesPerRead)

            while fs.ClockTick < until do
                stepSimulation fs

            match deadline with
            | None -> ()
            | Some by ->
                SimLog.sampleCore ()
                outOfTime <- getTimeMs () > by

        if fs.ClockTick >= lastStepNeeded then
            RunCompleted
        else
            RunStoppedAt fs.ClockTick

/// runFastSimulationCore with its invocation recorded: one SimLog entry per call that advances
/// the clock. The renderer's progress loop is repeated timed calls to this, so each progress
/// update is one record; calls that find nothing to do (the wave viewer re-asking for a tick it
/// already has, on every render) are not recorded, or they would flood the ring.
let runFastSimulation (timeOut: float option) (lastStepNeeded: int) (fs: FastSimulation) : RunOutcome =
    let fromTick = fs.ClockTick
    SimLog.beginInvocation ()
    let start = getTimeMs ()
    let result = runFastSimulationCore timeOut lastStepNeeded fs

    if fs.ClockTick <> fromTick then
        SimLog.record
            SimLog.SimRun
            fs.SimulatedTopSheet
            (fs.FComps.Count + fs.FCustomComps.Count)
            fromTick
            fs.ClockTick
            (getTimeMs () - start)

    result
