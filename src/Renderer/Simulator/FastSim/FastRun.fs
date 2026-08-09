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


/// Run an existing fast simulation up to the given number of steps. This function will mutate the write-once data arrays
/// of simulation data and only simulate the new steps needed, so it may return immediately doing no work.
/// If the simulation data arrays are not large enough they are extended up to a limit. After that, they act as a circular buffer.
/// TimeOut if not None is the cutoff time after which the simulation terminates execution unfinished.
/// Use fs.ClockTick to determine whether simulation has completed.
/// returns speed, in clock cycles per ms, or None if complete
let runFastSimulation (timeOut: float option) (lastStepNeeded: int) (fs: FastSimulation) : float option =
    if fs.MaxArraySize = 0 then
        failwithf "ERROR: can't run a fast simulation with 0 length arrays!"
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



