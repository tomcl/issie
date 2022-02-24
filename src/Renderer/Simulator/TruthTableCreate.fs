module TruthTableCreate
open CommonTypes
open TimeHelpers
open SimulatorTypes
open TruthTableTypes
open SynchronousUtils
open NumberHelpers

// REMEMBER TO REMOVE EVENTUALLY!
let print x =
    printfn "%A" x

let inputValues (tInput: TableInput) =
    let values1 =
        ([],tInput.Constraints.Equalities)
        ||> List.fold (fun rows con ->
            if rows.Length < tInput.AllowedRowCount then
                con.Value::rows
            else
                rows)

    (values1,tInput.Constraints.Inequalities)
    ||> List.fold (fun rows con ->
        if rows.Length < tInput.AllowedRowCount then
            [con.LowerBound..con.UpperBound] @ rows
        else
            rows)
    |> List.sort


let bitCombinations (width: int) (num: int): CellData list =
    convertIntToWireData width num
    |> List.map (fun x -> Bits [x])

let multibitCombinations (widths: int list) =
    let masterList = 
        widths
        |> List.map (fun n -> [0 .. int (2.0**n - 1.0) ])

    let combs =
        if widths.Length = 0 then
            []
        else if widths.Length = 1 then
            masterList.Head
            |> List.map (fun n -> [n])
        else if widths.Length = 2 then
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
        convertIntToWireData widths[i] n))

/// Returns a TableInput list with Max and Constrained Row Counts correctly calculated.
/// Allowed Row Counts will be set to zero.
let inputsWithCRC (inputs: SimulationIO list) (inputConstraints: ConstraintSet) =
    let findConstrainedRowCount (tInput: TableInput) =
    // We asume that all constraints are validated on entry
    // Therefore, there should be no overlaps
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
let inputsWithARC (tInputs: TableInput list) limit = 
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


let tableLHS (inputs: SimulationIO list): TruthTableRow list =
    // Bits associated with max rows allowed in Truth Table (2^10 = 1024)
    let maxBits = 10

    let widthOneInputs = List.filter (fun (_,_,w) -> w = 1) inputs
    let widthMultiInputs = List.filter (fun (_,_,w) -> w > 1) inputs

    // Restricts number of rows if there are > 10 single-bit inputs
    let power =
        if widthOneInputs.Length > maxBits then
            printfn "Truth Table truncated over single-bit inputs"
            maxBits
        else
            widthOneInputs.Length
    
    let widthOneRows = 
        [0 .. int (2.0**(float power))]
        |> List.map (bitCombinations widthOneInputs.Length)
        |> List.map (fun ttRow ->
            List.map2 (fun comp wd -> {IO = comp; Data = wd}) widthOneInputs ttRow)

    let widthMultiRows =
        widthMultiInputs
        |> List.map (fun (_,_,w)  -> w)
        |> multibitCombinations
        |> List.map (fun l -> (widthMultiInputs,l) ||> List.map2 (fun comp wd -> {IO = comp; Data = Bits wd}))

    match (widthOneInputs.IsEmpty,widthMultiInputs.IsEmpty) with
    | (false,true) -> widthOneRows
    | (true,false) -> widthMultiRows
    | (false,false) ->
        (widthMultiRows,widthOneRows)
        ||> List.allPairs
        |> List.map (fun (a,b) -> a @ b)
    | (true,true) -> failwithf "what? Can't create truth table with no inputs"

let rowRHS (rowLHS: TruthTableRow) (outputs: SimulationIO list) (simData: SimulationData): TruthTableRow =
    let updateOutputs (cell: TruthTableCell) =
        let (cid,_,_) = cell.IO
        match cell.Data with
        | Bits wd -> FastRun.changeInput cid wd simData.ClockTickNumber simData.FastSim
        | _ -> failwithf "what? CellData was not WireData during truth table generation"
    let _ = List.map updateOutputs rowLHS

    (outputs,simData)
    ||> FastRun.extractFastSimulationIOs
    |> List.map (fun (comp,wd) -> {IO = comp; Data = Bits wd})

let truthTable (simData: SimulationData) : TruthTable =
    let start = TimeHelpers.getTimeMs()
    let tempSimData = 
        match FastRun.buildFastSimulation 2 simData.Graph with
        | Ok tempFS -> {simData with FastSim = tempFS}
        | _ -> failwithf "Error in building fast simulation for Truth Table evaluation" 
    let inputs = List.map fst (FastRun.extractFastSimulationIOs simData.Inputs tempSimData)
    let outputs = List.map fst (FastRun.extractFastSimulationIOs simData.Outputs tempSimData)
    let lhs = tableLHS inputs
    let rhs = List.map (fun i -> rowRHS i outputs tempSimData) lhs

    List.zip lhs rhs
    |> Map.ofList
    |> (fun tableMap -> {TableMap = tableMap; XRows = None})
    |> TimeHelpers.instrumentInterval "truthTableGeneration" start

let printTruthTable (simData: SimulationData) =
    let tt = truthTable simData
    print <| tt


let printTableLHS (input_tuples : (SimulationIO * WireData) list) =
    printf "Printing TT Inputs"
    let inputs = 
        input_tuples
        |> List.map fst
    let lhs = tableLHS inputs

    //lhs
    //|> List.map (fun ttRow ->
    //    ttRow
    //    |> List.map (fun (_,wd) ->
    //        printf "%A ," wd))
    print <| lhs


