//(*
//    ConstraintView.fs
//
//    Utility and View functions for viewing/handling constraints on Truth Tables
//*)
//

module TruthTableReduce
open CommonTypes
open TimeHelpers
open SimulatorTypes
open TruthTableTypes
open SynchronousUtils
open NumberHelpers

/// Returns true if two rows are equal, supporting Don't Care terms
let rowEquals (row1: TruthTableRow) (row2: TruthTableRow) =
    if row1.Length <> row2.Length then
        false
    else
        (row1,row2)
        ||> List.forall2 (fun c1 c2 ->
            match c1.Data, c2.Data with
            | Bits a, Bits b -> a = b
            | DC, _ -> true
            | _, DC -> true
            | _, _ ->
                failwithf "what? Rows containing algebraic cells passed to rowEquals")

/// Alternative to Map.tryFind for Table Maps which supports Don't Care terms.
/// If a row maps to multiple rows (happens with Don't Care Rows), all rows are returned.
let tableTryFind (row: TruthTableRow) (tMap: Map<TruthTableRow,TruthTableRow>) =
    match (Map.tryFind row tMap), (rowContainsDC row) with
    | Some rhs, _ -> Some [rhs]
    | None, false -> None
    | None, true ->
        ([], Map.toList tMap)
        ||> List.fold (fun acc (lhs,rhs) ->
            if rowEquals row lhs then
                rhs::acc
            else
                acc)
        |> function
            | [] -> None
            | lst -> Some lst

/// Given a row containing Don't Care terms, validate it against the Truth Table to check
/// if the relationship it describes is correct.
let isValidDCRow row table =
    match tableTryFind row table.FilteredMap with
    | None -> None
    | Some outputs ->
        outputs
        |> List.forall (fun r -> r = outputs.Head)
        |> function
            | false -> None
            | true -> Some <| outputs.Head

/// Finds all rows where a given input is Don't Care (X)
let inputDCRows (input: CellIO) (table: TruthTable) =
    let allInputs = table.Inputs
    let tMap =
        match table.DCMap with
        | None -> table.FilteredMap
        | Some m -> m
    if allInputs.Length = 1 then
        []
    else
        let allInputs = table.Inputs
        let inputIdx =
            match List.tryFindIndex (fun c -> c = input) allInputs with
            | None ->
                failwithf "what? Trying to DC Reduce a table over an input not present in the table"
            | Some idx -> idx
        let tableLst = Map.toList tMap
        ([],tableLst)
        ||> List.fold (fun acc (lhs,rhs) ->
            // Is the input value in this row already DC?
            let alreadyDC = 
                match lhs[inputIdx].Data with 
                | DC -> true
                | _ -> false
            // Sets the current input value in the row to DC for checking
            let possible: TruthTableRow =
                lhs
                |> List.updateAt inputIdx {IO = input; Data = DC}
            // If the possible row is valid, and the row was already not DC,
            // add this to the list of new valid DC rows.
            match isValidDCRow possible table, alreadyDC with
            | _,true | None,_ -> acc
            | Some _,false -> (possible,rhs)::acc)

/// Reduce the Truth Table by removing rows covered by Don't Care Rows.
let reduceWithDCRow regularRows (dcLeft,dcRight) =
    regularRows
    |> List.filter (fun (regLeft,regRight) ->
        rowEquals (dcLeft @ dcRight) (regLeft @ regRight)
        |> not)

/// Recursive function for Don't Care reduction of a Truth Table.
/// Table is repeatedly reduced until running the reduction logic does not change
/// the returned table.
let rec reduceTruthTable' 
        (table: TruthTable)  
        depth
        maxDepth =

    let tMap =
        match table.DCMap with
        | None -> table.TableMap
        | Some m -> m

    let allDCRows =
        table.Inputs
        |> List.collect (fun input ->
            inputDCRows input table)
            
    let remainingRegularRows =
        (Map.toList tMap, allDCRows)
        ||> List.fold reduceWithDCRow

    let newMap =
        allDCRows @ remainingRegularRows
        |> Map.ofList

    match maxDepth with
    | Some md when md < 1 ->
            failwithf 
                $"DC Reduction with maximum depth of recursion {md} called.
                Maximum Depth must at least 1."
    | Some md when md = depth ->
        {table with DCMap = Some newMap}
    | _ ->
        if tMap = newMap then
            {table with DCMap = Some newMap}
        else
            reduceTruthTable' 
                {table with DCMap = Some newMap}  
                (depth+1) 
                maxDepth

/// Returns true if a truth table has redundancies
let hasRedundancies table =
    let onceReduced = 
        reduceTruthTable' table 1 (Some 1)
    match onceReduced.DCMap with
    | None -> 
        failwithf "what? reduceTruthTable' should never return with DCMap = None"
    | Some dcMap ->
        dcMap <> table.TableMap

/// Top-level function for DC reduction of truth tables
let reduceTruthTable table maxDepth =
    reduceTruthTable' table 1 maxDepth




