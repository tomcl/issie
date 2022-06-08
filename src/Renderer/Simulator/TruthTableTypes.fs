//(*
//    TruthTableTypes.fs
//
//    Types associated with Truth Tables used in the application.
//*)
//

module TruthTableTypes

open SimulatorTypes

//-------------------------------------------------------------------------------------//
//-----------------------------Truth Table Types---------------------------------------//
//-------------------------------------------------------------------------------------//

(*
    Note on Truth Tables

    1. In Issie, Truth Tables are stored as Maps, mapping left-hand side (inputs)
        to right hand side (outputs).
    2. The Truth Table type contains multiple Maps along with other relevant data. Multiple
        Maps are stored for efficiency purposes, this way the Truth Table is only recalculated
        when absolutely necessary.
    3. Each cell in the truth table comprises an IO/Viewer (CellIO) and data (CellData).
    4. TruthTableRows are lists of cells, and Maps store the mapping from row to row.
    5. Description of Stored Maps:
        TableMap: The initial main Map for the Truth Table. Only Input Constraints or Algebraic Inputs
            are applied to this.
        DCMap option: The Don't Care Reduced version of the Truth Table, calculated from TableMap.
            None if Table has not been DC Reduced.
        FilteredMap: Map with Output Numerical Constraints applied. Map that is dispayed.
    6. As Maps in F# are unordered, sorting of Truth Tables is done when the table is a
        list of rows. This sorted table is stored in SortedListRep.
*)

type CellData =
    | Bits of wd: WireData
    | Algebra of var: string
    | DC //Don't Care

type CellIO =
    | SimIO of SimulationIO
    | Viewer of (string*string)*int
    with
    member this.getLabel =
        match this with
        | SimIO (_,l,_) -> string l
        | Viewer ((l,_),_) -> l
    member this.getWidth =
        match this with
        | SimIO (_,_,w) -> w
        | Viewer ((_,_),w) -> w

type TruthTableCell = {
    IO: CellIO
    Data: CellData
    } with
    member this.IsBits =
        match this.Data with
        | Bits _ -> true
        | _ -> false
    member this.IsDC =
        match this.Data with
        | DC -> true
        | _ -> false
    member this.IsAlgebra =
        match this.Data with
        | Algebra _ -> true
        | _ -> false

type TruthTableRow = TruthTableCell list

// Identifiers for Map Type
type MapToUse = | Table | HiddenCol | Filtered | DCReduced

type SortType = | Ascending | Descending

// Direction to move a TT Column in
type MoveDirection = | MLeft | MRight

// Actual Truth Table Data Structure
type TruthTable = {
    // Actual Table: Mapping from Input row to Output row
    TableMap: Map<TruthTableRow,TruthTableRow>
    // Truth Table filtered by Output Constraints
    FilteredMap: Map<TruthTableRow,TruthTableRow>
    // Truth Table reduced with Don't Cares on Inputs
    DCMap: Map<TruthTableRow,TruthTableRow> option
    // List Representation of table to which sorting is applied
    SortedListRep: TruthTableRow list
    // If the Truth Table has been truncated
    IsTruncated: bool
    // Maximum rows the truth table could have with current input constraints
    MaxRowsWithConstraints: int
    // Simulation Data for the Truth Table's own Simulation
    // Used when re-generating the Truth Table on change in input constraints
    TableSimData: SimulationData
    // Ordered List of all IOs in the order they originally were
    IOOrder: CellIO list
    } with
    member this.Inputs =
        this.TableMap
        |> Map.toList
        |> List.head
        |> fst
        |> List.map (fun cell -> cell.IO)

/// Returns true if a row contains a Don't Care (X)
let rowContainsDC (row: TruthTableRow) =
    row
    |> List.exists (fun cell -> cell.IsDC)

/// Returns true if a row contains algebra
let rowContainsAlgebra (row: TruthTableRow) =
    row
    |> List.exists (fun cell -> cell.IsAlgebra)
     
//-------------------------------------------------------------------------------------//
//-----------------------------Constraint Types----------------------------------------//
//-------------------------------------------------------------------------------------//

// A numerical constraint set contains Equality and Inequality Constraints
type ConstraintSet = {
    Equalities: EqualityConstraint list
    Inequalities: InequalityConstraint list
} with
    /// Returns a new ConstraintSet without constraints for a specific CellIO
    member this.withoutIO io =
        let newEqu =
            this.Equalities
            |> List.filter (fun e -> e.IO <> io)
        let newIneq =
            this.Inequalities
            |> List.filter (fun i -> i.IO <> io)
        {Equalities = newEqu; Inequalities = newIneq}
// IO = Value
and EqualityConstraint = {
    IO: CellIO
    Value: int
}
// LowerBound < IO < UpperBound
and InequalityConstraint = {
    LowerBound: int
    IO: CellIO
    UpperBound: int
    Range: int
}

type Constraint =
    | Equality of EqualityConstraint
    | Inequality of InequalityConstraint

type ConstraintType = Equ | Ineq

// Reason why the current Truth Table Map is out of date
type ReasonOutOfDate =
    // Table needs to be regenerated, likely due to a change in Input Constraints
    | Regenerate
    // Columns have been hidden/unhidden, change Map to reflect this
    | HideColumn
    // Table needs to be refiltered, likely due to change in Output Constraints
    | Refilter
    // Table needs to be sorted
    | ReSort

let isEqu c =
    match c with
    | Equ -> true
    | _ -> false

let emptyConstraintSet = {
    Equalities = []
    Inequalities = []
}

let makeInequalityConstraint lower io upper =
    let range = upper - lower + 1
    {
        LowerBound = lower
        IO = io
        UpperBound = upper
        Range = range
    }

let orderConstraints set =
    let ordered =
        set.Inequalities
        |> List.sortByDescending (fun c -> c.Range)
    {set with Inequalities = ordered}

// Data structure containing information about an input used when calculating Truth Table LHS.
// RowCount for an input refers to the number of unique values it contributes to the table
type TableInput = {
    // SimulationIO associated with the input
    IO: SimulationIO
    // Is the Input algebraic (or numeric)
    IsAlgebra: bool
    // Number of possible unique input values, based on width of input
    MaxRowCount: int
    // Number of possible unique input values after applying input constraints
    ConstrainedRowCount: int
    // Number of possible unique input values that will fit in the Truth Table after truncation
    AllowedRowCount: int
    // Constraints on the input
    Constraints: ConstraintSet
}

/// Create a TableInput data structure from a SimulationIO using application state
let initTableInput (simIO:SimulationIO) (allConstraints: ConstraintSet) (algebraIOs: SimulationIO list) =
    let (_,_,w) = simIO
    let specificEqualities =
        allConstraints.Equalities
        |> List.filter (fun con -> con.IO = SimIO simIO)
    let specificInequalities =
        allConstraints.Inequalities
        |> List.filter (fun con -> con.IO = SimIO simIO)
    let isAlg = List.contains simIO algebraIOs
    {
        IO = simIO
        IsAlgebra = isAlg
        MaxRowCount = int (2.0**w)
        ConstrainedRowCount = 0
        AllowedRowCount = 0
        Constraints = { Equalities = specificEqualities
                        Inequalities = specificInequalities}
    }
