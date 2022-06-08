module TruthTableCreate
open CommonTypes
open TimeHelpers
open SimulatorTypes
open TruthTableTypes
open SynchronousUtils
open NumberHelpers
open Helpers

/// Wraps SimulationIO and Viewer types in the CellIO DU Type
let toCellIO simIOs viewers =
    (List.map (fun io -> SimIO io) simIOs
    @
    List.map (fun ((l,f),w,_) -> Viewer ((l,f),w)) viewers)

/// Converts a Truth Table from a Mapping to a list of its rows.
/// Note that separation of inputs and outputs is lost.
let tableAsList tMap =
    tMap
    |> Map.toList
    |> List.map (fun (lhs,rhs) -> List.append lhs rhs)

/// From a TableInput data structure, return all possible values a non-algebraic input can take
/// without the truth table as a whole exceeding the row limit.
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
        // First infer values from from equality constraints
        let values1 =
            ([],equ)
            ||> List.fold (fun rows con ->
                if rows.Length < tInput.AllowedRowCount then
                    con.Value::rows
                else
                    rows)
        // Infer values from inequality constraints
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
        {IO = SimIO (tInputs[i].IO); Data = Bits (convertIntToWireData w n)}))

/// Returns a TableInput list with Max and Constrained Row Counts correctly calculated.
/// Allowed Row Counts will be set to zero and calculated later.
// We assume that all constraints are validated on entry, so they don't overlap.
let inputsWithCRC 
        (inputs: SimulationIO list) 
        (inputConstraints: ConstraintSet) 
        (algebraIOs: SimulationIO list) =
    let findConstrainedRowCount (tInput: TableInput) =
        match tInput.Constraints, tInput.IsAlgebra with
        | _, true -> 1
        | {Equalities = []; Inequalities = []}, false -> tInput.MaxRowCount
        | {Equalities = equ; Inequalities = []}, false -> equ.Length
        | {Equalities = equ; Inequalities = ineq}, false ->
            ((0,ineq)
            ||> List.fold (fun n con -> n + con.Range))
            + equ.Length
    inputs
    |> List.map (fun input ->
        let ti = initTableInput input inputConstraints algebraIOs
        let crc = findConstrainedRowCount ti
        {ti with ConstrainedRowCount = crc})

/// Calculates Allowed Row Counts for each Truth Table input in the list and updates the field in
/// the TableInput data structure. Also returns the number of rows the constrained truth table
/// should have in theory (which may be different in practice due to truncation).
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

    let algInputs,numInputs =
        tInputs
        |> List.partition (fun ti -> ti.IsAlgebra)

    // Subset of a row with algebraic inputs
    let algRow =
        algInputs
        |> List.map (fun ti -> 
            let (_,label,_) = ti.IO
            {IO = SimIO ti.IO; Data = Algebra (string label)})
    if numInputs.IsEmpty then
        // All inputs are algebraic, so only one row in table with all algebra
        [algRow],1
    else
        // Append algebra to each numeric row to get full LHS
        (numInputs
        |> inputCombinations
        |> List.map (fun r -> algRow@r)), (tCRC)

/// Feeds the given input combination (LHS Row) to the table's Fast Simulation and extracts the
/// simulated output combination (RHS Row).
// NOTE: This function mutates the table's Fast Simulation data structure.
let simulateInputCombination 
        (rowLHS: TruthTableRow) 
        (outputs: SimulationIO list)  
        (simData: SimulationData)
        : TruthTableRow =
    let updateOutputs (cell: TruthTableCell) =
        match cell.IO, cell.Data with
        | SimIO io, Bits wd ->
            let (cid,_,_) = io
            FastRun.changeInput cid (IData wd) simData.ClockTickNumber simData.FastSim
        | SimIO io, Algebra exp ->
            let (cid,_,_) = io
            FastRun.changeInput cid (IAlg (SingleTerm io)) simData.ClockTickNumber simData.FastSim
        | x, y -> 
            failwithf "what? CellData from input rows has IO: %A, and Data: %A." x y
    // Feed the current input combination to the Fast Simulation
    let _ = List.map updateOutputs rowLHS
    
    // Extract Output and Viewer values from the Fast Simulation
    let outputRow =
        (outputs,simData)
        ||> FastRun.extractFastSimulationIOs
        |> List.map (fun (comp,out) -> 
            match out with
            | IData wd -> {IO = SimIO comp; Data = Bits wd}
            | IAlg exp -> {IO = SimIO comp; Data = Algebra (expToString exp)})
    let viewerRow =
        FastRun.extractViewers simData
        |> List.map (fun ((l,f),w,fs) ->
            match fs with
            | IData wd -> {IO = Viewer ((l,f),w); Data = Bits wd}
            | IAlg exp -> {IO = Viewer ((l,f),w); Data = Algebra l})
    outputRow @ viewerRow

/// Create a Truth Table from Simulation Data, taking into account 
/// algebraic inputs and input constriants.
let truthTable 
    (simData: SimulationData) // Simulation Data for sheet
    (inputConstraints: ConstraintSet) // All input constraints to be applied to truth table
    (algebraIOs: SimulationIO list) // All inputs which are algebraic
    (bitLimit: int) // Limits the max rows in the truth table (2^bitLimit)
    (isRegeneration: bool) // Is this function call regeneration (true) or first time (false)
    : TruthTable =
    let start = TimeHelpers.getTimeMs()

    // Create a new SimData separate from the one used by the Step Simulator
    let tableSimData =
        if isRegeneration then 
            simData
        else
            match FastRun.buildFastSimulation 1 simData.Graph with
            | Ok fs -> 
                {simData with FastSim = fs}
            | _ -> 
                failwithf "Error in building fast simulation for Truth Table evaluation"
    let inputs = List.map fst (FastRun.extractFastSimulationIOs simData.Inputs tableSimData)
    let outputs = List.map fst (FastRun.extractFastSimulationIOs simData.Outputs tableSimData)
    let viewers = FastRun.extractViewers simData
    let lhs,tCRC = tableLHS inputs inputConstraints algebraIOs bitLimit
    let rhs = List.map (fun i -> simulateInputCombination i outputs tableSimData) lhs

    List.zip lhs rhs
    |> Map.ofList
    |> (fun tableMap ->
        let listRep = tableAsList tableMap
        {
            TableMap = tableMap
            FilteredMap = tableMap
            DCMap = None
            SortedListRep = listRep
            IsTruncated = (tableMap.Count <> tCRC)
            MaxRowsWithConstraints = tCRC
            TableSimData = tableSimData
            IOOrder = toCellIO (inputs@outputs) viewers
        })
    |> TimeHelpers.instrumentInterval "truthTableGeneration" start

