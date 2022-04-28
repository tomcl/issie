module TruthTableCreate
open CommonTypes
open TimeHelpers
open SimulatorTypes
open TruthTableTypes
open SynchronousUtils
open NumberHelpers

let tableAsList tMap =
    tMap
    |> Map.toList
    |> List.map (fun (lhs,rhs) -> List.append lhs rhs)

/// From a TableInput data structure, find all possible values the input can take
let inputValues (tInput: TableInput) =
    match tInput.Constraints.Equalities, tInput.Constraints.Inequalities with
    | [], [] ->
        let (_,_,w) = tInput.IO
        let upper = int(2.0**w - 1.0)
        if upper < tInput.AllowedRowCount then
            [0..upper]
        else
            [0..(tInput.AllowedRowCount-1)]
    | equ, ineq ->
        let values1 =
            ([],equ)
            ||> List.fold (fun rows con ->
                if rows.Length < tInput.AllowedRowCount then
                    con.Value::rows
                else
                    rows)

        (values1,ineq)
        ||> List.fold (fun rows con ->
            if rows.Length < tInput.AllowedRowCount then
                if con.Range < tInput.AllowedRowCount then
                    [con.LowerBound..con.UpperBound] @ rows
                else
                    [con.LowerBound..(con.LowerBound + tInput.AllowedRowCount - 1)]
            else
                rows)
        |> List.sort

/// Find all combinations (cartesian product) of all possible values for table inputs
let inputCombinations (tInputs: TableInput list) =
    let masterList =
        tInputs
        |> List.map inputValues

    let combs =
        if tInputs.Length = 0 then
            []
        else if tInputs.Length = 1 then
            masterList.Head
            |> List.map (fun n -> [n])
        else if tInputs.Length = 2 then
            List.allPairs (List.head masterList) (List.last masterList)
            |> List.map (fun (a,b) -> [a;b])
        else
            let el1::el2::remaining = masterList
            let starter =
                List.allPairs el1 el2
                |> List.map (fun (a,b) -> [a;b])

            let rec numComb (acc: int list list) (rem: int list list) =
                match rem with
                | hd::tl ->
                    let newAcc =
                        acc
                        |> List.collect (fun l -> List.map (fun i -> l @ [i]) hd)
                    numComb newAcc tl
                | [] ->
                    acc

            numComb starter remaining

    combs
    |> List.map (fun l -> l |> List.mapi (fun i n ->
        //convertIntToWireData widths[i] n
        let (_,_,w) = tInputs[i].IO
        {IO = SimIO (tInputs[i].IO); Data = Bits (convertIntToWireData w n)}

        ))

/// Returns a TableInput list with Max and Constrained Row Counts correctly calculated.
/// Allowed Row Counts will be set to zero.
let inputsWithCRC (inputs: SimulationIO list) (inputConstraints: ConstraintSet) =
    let findConstrainedRowCount (tInput: TableInput) =
    // We asume that all constraints are validated on entry.
    // Therefore, there should be no overlaps.
        match tInput.Constraints with
        | {Equalities = []; Inequalities = []} -> tInput.MaxRowCount
        | {Equalities = equ; Inequalities = []} -> equ.Length
        | {Equalities = equ; Inequalities = ineq} ->
            ((0,ineq)
            ||> List.fold (fun n con -> n + con.Range))
            + equ.Length
    inputs
    |> List.map (fun input ->
        let ti =
            (input,inputConstraints)
            ||> initTableInput
        let crc = findConstrainedRowCount ti
        {ti with ConstrainedRowCount = crc})

/// Calculates Allowed Row Counts for each Truth Table input.
/// Also returns the actual row count for the given inputs and constraints.
let inputsWithARC limit (tInputs: TableInput list) =
    let sortedInputs =
        tInputs
        |> List.sortBy (fun ti -> ti.ConstrainedRowCount)
    (1,sortedInputs)
    ||> List.mapFold (fun rowcount ti ->
        let newRowCount = rowcount*ti.ConstrainedRowCount
        let capacity = limit/rowcount
        // Case where constrained values of this input can be entirely included
        if capacity >= ti.ConstrainedRowCount then
            {ti with AllowedRowCount = ti.ConstrainedRowCount}, newRowCount
        // Case where constrained values of this input must be truncated
        else if capacity > 1 then
            {ti with AllowedRowCount = capacity}, newRowCount
        else
            {ti with AllowedRowCount = 1}, newRowCount
        )

/// Find all LHS rows of the Truth Table, limited by input constraints and bit limit
let tableLHS (inputs: SimulationIO list) (inputConstraints: ConstraintSet) bitLimit:
    TruthTableRow list * int =

    // Maximum number of rows on LHS of Truth Table.
    // Limited for speed and memory consumption reasons.
    let rowLimit = int(2.0**bitLimit)

    let tInputs,tCRC =
        (inputs, inputConstraints)
        ||> inputsWithCRC
        |> inputsWithARC rowLimit
    printfn "Inputs %A" tInputs

    (tInputs
    |> inputCombinations), tCRC

/// Find the RHS (output) for every input row by simulating the input combination
let rowRHS (rowLHS: TruthTableRow) (outputs: SimulationIO list) viewers (simData: SimulationData): TruthTableRow =
    let updateOutputs (cell: TruthTableCell) =
        match cell.IO, cell.Data with
        | SimIO io, Bits wd ->
            let (cid,_,_) = io
            FastRun.changeInput cid wd simData.ClockTickNumber simData.FastSim
        | x, y -> failwithf "what? CellData from input rows has IO: %A, and Data: %A." x y
    let _ = List.map updateOutputs rowLHS
    let outputRow =
        (outputs,simData)
        ||> FastRun.extractFastSimulationIOs
        |> List.map (fun (comp,wd) -> {IO = SimIO comp; Data = Bits wd})
    let viewerRow =
        FastRun.extractViewers simData
        |> List.map (fun ((l,f),w,wd) -> {IO = Viewer ((l,f),w); Data = Bits wd})
    outputRow @ viewerRow

/// Create a Truth Table from the Simulation Data and input constraints
let truthTable (simData: SimulationData) (inputConstraints: ConstraintSet) bitLimit: TruthTable =
    let start = TimeHelpers.getTimeMs()
    printfn "Truth Table Gen Called"
    let tempSimData =
        match FastRun.buildFastSimulation 2 simData.Graph with
        | Ok tempFS -> {simData with FastSim = tempFS}
        | _ -> failwithf "Error in building fast simulation for Truth Table evaluation"
    let inputs = List.map fst (FastRun.extractFastSimulationIOs simData.Inputs tempSimData)
    let outputs = List.map fst (FastRun.extractFastSimulationIOs simData.Outputs tempSimData)
    let viewers = FastRun.extractViewers simData
    let lhs,tCRC = tableLHS inputs inputConstraints bitLimit
    let rhs = List.map (fun i -> rowRHS i outputs viewers tempSimData) lhs

    List.zip lhs rhs
    |> Map.ofList
    |> (fun tableMap ->
        printfn "RealRowCount: %A" tableMap.Count
        {
            TableMap = tableMap
            HiddenColMap = tableMap
            FilteredMap = tableMap
            DCMap = None
            SortedListRep = tableAsList tableMap
            IsTruncated = (tableMap.Count <> tCRC)
            MaxRowsWithConstraints = tCRC
            TableSimData = tempSimData
            })
    |> TimeHelpers.instrumentInterval "truthTableGeneration" start

/// Regenenerate the truth table after the input constraints change
let truthTableRegen tableSD inputConstraints bitLimit =
    let start = TimeHelpers.getTimeMs()
    let inputs = List.map fst (FastRun.extractFastSimulationIOs tableSD.Inputs tableSD)
    let outputs = List.map fst (FastRun.extractFastSimulationIOs tableSD.Outputs tableSD)
    let viewers = FastRun.extractViewers tableSD
    let lhs,tCRC = tableLHS inputs inputConstraints bitLimit
    let rhs = List.map (fun i -> rowRHS i outputs viewers tableSD) lhs

    List.zip lhs rhs
    |> Map.ofList
    |> (fun tableMap ->
        printfn "RealRowCount: %A" tableMap.Count
        {
            TableMap = tableMap
            HiddenColMap = tableMap
            FilteredMap = tableMap
            DCMap = None
            SortedListRep = tableAsList tableMap
            IsTruncated = (tableMap.Count <> tCRC)
            MaxRowsWithConstraints = tCRC
            TableSimData = tableSD
            })
    |> TimeHelpers.instrumentInterval "truthTableRegen" start

