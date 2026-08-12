module FastValidate

open CommonTypes
open TimeHelpers
open SimGraphTypes
open SimTypes
open SynchronousUtils
open NumberHelpers
open FastCreate
open Helpers
open FastOrder

// Checking a built simulation before it is run: that every active component was placed in the
// evaluation order, which is how a cycle is reported, and that the array widths agree with the
// widths the components declare.

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
        // the counts are the fact; the per-component dump behind them ran to thousands of lines,
        // on a path that already returns a cycle-detected error to the user
        Log.warn $"{activeComps.Length} active components but {inSimulationComps.Length} in the simulation"
        if Log.isOn Log.Sim then
            inSimulationComps
            |> Array.iter (fun fc -> Log.dbg Log.Sim $"simulation: {printComp fs 0 fc}")
            fs.FComps
            |> Map.iter (fun fid fc -> Log.dbg Log.Sim $"FComps: {printComp fs 0 fc}")

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
                // the array width against the component's declared output width. This used to
                // compare fc.Outputs[i].Width with output.Width - the same value - so the
                // inconsistency check could never fire and only the zero-width case was caught
                match output.Width, fc.OutputWidth i with
                | 0, _ ->
                    failwithf
                        "Unexpected output data width 0 found on initialised component %A %s:%d"
                        fc.FType
                        fc.FullName
                        i
                | n, m when n <> m ->
                    failwithf
                        "Inconsistent simulation data width found on signal output: array width %d but declared width %d from %A %s:%d"
                        n
                        m
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
        // the counts are the fact; the per-component dump behind them ran to thousands of lines,
        // on a path that already returns a cycle-detected error to the user
        Log.warn $"{activeComps.Length} active components but {inSimulationComps.Length} in the simulation"
        if Log.isOn Log.Sim then
            inSimulationComps
            |> Array.iter (fun fc -> Log.dbg Log.Sim $"simulation: {printComp fs 0 fc}")
            fs.FComps
            |> Map.iter (fun fid fc -> Log.dbg Log.Sim $"FComps: {printComp fs 0 fc}")

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
                let expectedWidth = fc.OutputWidth i

                // A width of zero is not a width, and a component that declares one cannot be
                // simulated - the same thing the uint32 path above refuses. This used to be tested
                // after the mismatch below, where it could only fire when the two agreed at zero:
                // a step array starts out holding emptyFastData, whose width IS zero, so a zero
                // there is the ordinary uninitialised state that the mismatch case exists to fill
                // in. It is the declared width that must not be zero.
                match data.Width, expectedWidth with
                | _, 0 ->
                    failwithf "Unexpected output width 0 declared by component %A %s:%d" fc.FType fc.FullName i
                | n, m when n <> m ->
                    // Re-initialize with correct width if there's a mismatch
                    output.FDataStep[0] <- Data(convertIntToFastData m 0u)
                | _ -> () // Ok in this case
            ))

        instrumentTime "checkAndValidate" start
        Ok fs

