module TruthTableCreate
open CommonTypes
open TimeHelpers
open SimulatorTypes
open TruthTableTypes
open SynchronousUtils
open NumberHelpers
open Helpers

let toCellIO simIOs viewers =
    (List.map (fun io -> SimIO io) simIOs
    @
    List.map (fun ((l,f),w,_) -> Viewer ((l,f),w)) viewers)

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
    let rec numComb (acc: int list list) (rem: int list list) =
                match rem with
                | hd::tl ->
                    let newAcc =
                        acc
                        |> List.collect (fun l -> List.map (fun i -> l @ [i]) hd)
                    numComb newAcc tl
                | [] ->
                    acc
    
    let masterList =
        tInputs
        |> List.map inputValues

    let combs =
        match masterList with
        | [] -> []
        | [el] ->
            el |> List.map (fun n -> [n])
        | [el1;el2] ->
            List.allPairs el1 el2
            |> List.map (fun (a,b) -> [a;b])
        | el1::el2::remaining ->
            let starter =
                List.allPairs el1 el2
                |> List.map (fun (a,b) -> [a;b])
            numComb starter remaining

    combs
    |> List.map (fun l -> l |> List.mapi (fun i n ->
        let (_,_,w) = tInputs[i].IO
        {IO = SimIO (tInputs[i].IO); Data = Bits (convertIntToWireData w n)}

        ))

/// Returns a TableInput list with Max and Constrained Row Counts correctly calculated.
/// Allowed Row Counts will be set to zero.
let inputsWithCRC (inputs: SimulationIO list) (inputConstraints: ConstraintSet) (algebraIOs: SimulationIO list) =
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
        let ti = initTableInput input inputConstraints algebraIOs
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
let tableLHS 
    (inputs: SimulationIO list) 
    (inputConstraints: ConstraintSet) 
    (algebraIOs: SimulationIO list)
    bitLimit:
    TruthTableRow list * int =

    // Maximum number of rows on LHS of Truth Table.
    // Limited for speed and memory consumption reasons.
    let rowLimit = int(2.0**bitLimit)

    let tInputs,tCRC =
        inputsWithCRC inputs inputConstraints algebraIOs
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
            FastRun.changeInput cid (IData wd) simData.ClockTickNumber simData.FastSim
        | SimIO io, Algebra exp ->
            let (cid,_,_) = io
            FastRun.changeInput cid (IAlg (SingleTerm io)) simData.ClockTickNumber simData.FastSim
        | x, y -> failwithf "what? CellData from input rows has IO: %A, and Data: %A." x y
    let _ = List.map updateOutputs rowLHS
    let outputRow =
        (outputs,simData)
        ||> FastRun.extractFastSimulationIOs
        |> List.map (fun (comp,out) -> 
            match out with
            | IData wd -> {IO = SimIO comp; Data = Bits wd}
            | IAlg exp -> {IO = SimIO comp; Data = Algebra (expToString exp)})
    let viewerRow =
        FastRun.extractViewers simData
        |> List.map (fun ((l,f),w,wd) -> {IO = Viewer ((l,f),w); Data = Bits wd})
    outputRow @ viewerRow

/// Create a Truth Table from the Simulation Data and input constraints
let truthTable 
    (simData: SimulationData) 
    (inputConstraints: ConstraintSet)
    (algebraIOs: SimulationIO list)
    bitLimit: TruthTable =
    let start = TimeHelpers.getTimeMs()
    printfn "Truth Table Gen Called"
    let tempSimData =
        match FastRun.buildFastSimulation 2 simData.Graph with
        | Ok tempFS -> {simData with FastSim = tempFS}
        | _ -> failwithf "Error in building fast simulation for Truth Table evaluation"
    let inputs = List.map fst (FastRun.extractFastSimulationIOs simData.Inputs tempSimData)
    let outputs = List.map fst (FastRun.extractFastSimulationIOs simData.Outputs tempSimData)
    let viewers = FastRun.extractViewers simData
    let lhs,tCRC = tableLHS inputs inputConstraints algebraIOs bitLimit
    let rhs = List.map (fun i -> rowRHS i outputs viewers tempSimData) lhs

    List.zip lhs rhs
    |> Map.ofList
    |> (fun tableMap ->
        printfn "RealRowCount: %A" tableMap.Count
        let listRep = tableAsList tableMap
        {
            TableMap = tableMap
            HiddenColMap = tableMap
            FilteredMap = tableMap
            DCMap = None
            SortedListRep = listRep
            OrderedArrayRep = list2DToArray2D listRep
            IsTruncated = (tableMap.Count <> tCRC)
            MaxRowsWithConstraints = tCRC
            TableSimData = tempSimData
            IOOrder = toCellIO (inputs@outputs) viewers
            })
    |> TimeHelpers.instrumentInterval "truthTableGeneration" start

/// Regenenerate the truth table after the input constraints change
let truthTableRegen tableSD inputConstraints (algebraIOs: SimulationIO list) bitLimit =
    let start = TimeHelpers.getTimeMs()
    let inputs = List.map fst (FastRun.extractFastSimulationIOs tableSD.Inputs tableSD)
    let outputs = List.map fst (FastRun.extractFastSimulationIOs tableSD.Outputs tableSD)
    let viewers = FastRun.extractViewers tableSD
    let lhs,tCRC = tableLHS inputs inputConstraints algebraIOs bitLimit
    let rhs = List.map (fun i -> rowRHS i outputs viewers tableSD) lhs

    List.zip lhs rhs
    |> Map.ofList
    |> (fun tableMap ->
        printfn "RealRowCount: %A" tableMap.Count
        let listRep = tableAsList tableMap
        {
            TableMap = tableMap
            HiddenColMap = tableMap
            FilteredMap = tableMap
            DCMap = None
            SortedListRep = listRep
            OrderedArrayRep = list2DToArray2D listRep
            IsTruncated = (tableMap.Count <> tCRC)
            MaxRowsWithConstraints = tCRC
            TableSimData = tableSD
            IOOrder = toCellIO (inputs@outputs) viewers
            })
    |> TimeHelpers.instrumentInterval "truthTableRegen" start

