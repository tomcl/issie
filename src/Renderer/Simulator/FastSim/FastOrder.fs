module FastOrder

open CommonTypes
open TimeHelpers
open SimGraphTypes
open SimTypes
open SynchronousUtils
open NumberHelpers
open FastCreate
open Helpers
open EvalReference
open EvalAlgebraic

// Deciding the order combinational components must be reduced in, so that every component's
// inputs are already computed when it runs. This is the one genuinely subtle algorithm in the
// build: a breadth-first sweep that releases a component once its last missing input arrives,
// and whose failure mode - a component never released - is how an asynchronous cycle is found.

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
               ((arr.UInt32Slab.Length > 0)
                || (arr.BigIntSlab.Length > 0))
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

/// Create arrays of components in corrected format for efficient reduction
/// Combinational components are ordered: clokced, constant, global input components are
/// separated.
let orderCombinationalComponents (numSteps: int) (fs: FastSimulation) : FastSimulation =
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
        fastReduceStep 1 0 false fc
        fc.Touched <- true
        propagateEval fc

    let initInput (fc: FastComponent) =
        // NB no default value is applied here - setInputstoDefault does that. A value was
        // computed at this point and discarded
        // REVIEW - Input initialisation is no longer required
        // fc.InputLinks[0].FastDataStep
        // |> Array.iteri (fun i _ -> fc.InputLinks[0].FastDataStep[ i ] <- convertIntToFastData (fc.OutputWidth 0) 0u)
        fastReduceStep fs.MaxArraySize 0 false fc
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
                // Build the RAM's store once, here, and hand the same one to every step: it is
                // mutable, so a step slot records which memory this is rather than what it held.
                | Some arr -> arr.Step[0] <- RamState(RamStore.ofMemory fs.MaxArraySize mem)
                | _ -> failwithf "Component %s does not have correct state vector" fc.FullName

                // change simulation semantics to output 0 in cycle 0 (the memory word at
                // address 0 was read into an unused binding here)
                match vec.Width with
                | w when w <= 32 -> vec.SetU32 0 0u
                | w -> vec.SetBig 0 0I
            | _, w ->
                match vec.Width with
                | w when w <= 32 -> vec.SetU32 0 0u
                | w -> vec.SetBig 0 0I)

    fs.FClockedComps |> Array.iter initClockedOuts
    fs.FConstantComps |> Array.iter init
    fs.FGlobalInputComps |> Array.iter initInput

    while readyToReduce.Length <> 0 do
        let readyL = readyToReduce
        readyToReduce <- []

        readyL
        |> List.iter (fun fc ->
            fastReduceStep fs.MaxArraySize 0 false fc // this is always a combinational reduction
            orderedComps <- fc :: orderedComps
            fc.Touched <- true
            propagateEval fc)

    instrumentTime "orderCombinationalComponents" startTime

    { fs with FOrderedComps = orderedComps |> Array.ofList |> Array.rev }

let orderCombinationalComponentsFData (numSteps: int) (fs: FastSimulation) : FastSimulation =
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
        // NB no default value is applied here - setInputstoDefault does that. A value was
        // computed at this point and discarded
        fc.InputLinks[0].FDataStep
        |> Array.iteri (fun i _ -> fc.InputLinks[0].FDataStep[ i ] <- Data(convertIntToFastData (fc.OutputWidth 0) 0u))
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
                // Build the RAM's store once, here, and hand the same one to every step: it is
                // mutable, so a step slot records which memory this is rather than what it held.
                | Some arr -> arr.Step[0] <- RamState(RamStore.ofMemory fs.MaxArraySize mem)
                | _ -> failwithf "Component %s does not have correct state vector" fc.FullName

                // change simulation semantics to output 0 in cycle 0 (the memory word at
                // address 0 was read into an unused binding here)
                vec.FDataStep[0] <- Data(convertIntToFastData w 0u)
            | _, w -> vec.FDataStep[0] <- Data(convertIntToFastData w 0u))

    fs.FClockedComps |> Array.iter initClockedOuts
    fs.FConstantComps |> Array.iter init
    fs.FGlobalInputComps |> Array.iter initInput

    while readyToReduce.Length <> 0 do
        let readyL = readyToReduce
        readyToReduce <- []

        readyL
        |> List.iter (fun fc ->
            fastReduceFData fs.MaxArraySize 0 false fc // this is always a combinational reduction
            orderedComps <- fc :: orderedComps
            fc.Touched <- true
            propagateEval fc)

    instrumentTime "orderCombinationalComponents" startTime

    { fs with FOrderedComps = orderedComps |> Array.ofList |> Array.rev }

