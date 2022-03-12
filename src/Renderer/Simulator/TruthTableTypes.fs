module TruthTableTypes

open SimulatorTypes

//-------------------------------------------------------------------------------------//
//-----------------------------Truth Table Types---------------------------------------//
//-------------------------------------------------------------------------------------//

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

type MapToUse = | Table | HiddenCol | Filtered | DCReduced
        
type TruthTable = {
    // Actual Table: Mapping from Input row to Output row
    TableMap: Map<TruthTableRow,TruthTableRow>
    // Truth Table excluding any hidden output columns
    HiddenColMap: Map<TruthTableRow,TruthTableRow>
    // Truth Table filtered by Output Constraints
    FilteredMap: Map<TruthTableRow,TruthTableRow>
    // Truth Table reduced with Don't Cares on Inputs
    DCMap: Map<TruthTableRow,TruthTableRow> option
    // If the Truth Table has been truncated
    IsTruncated: bool
    // Maximum rows the truth table could have with current input constraints
    MaxRowsWithConstraints: int
    // Simulation Data for the Truth Table's own Simulation
    // Used when re-generating the Truth Table on change in input constraints
    TableSimData: SimulationData
    } with
    member this.Inputs =
        this.TableMap
        |> Map.toList
        |> List.head
        |> fst
        |> List.map (fun cell -> cell.IO)
    member this.AllOutputs =
        this.TableMap
        |> Map.toList
        |> List.head
        |> snd
        |> List.map (fun cell -> cell.IO)
    member this.VisibleOutputs =
        this.FilteredMap
        |> Map.toList
        |> List.head
        |> snd
        |> List.map (fun cell -> cell.IO)
    member this.getMap (mapType: MapToUse) =
        match mapType with
        | Table -> this.TableMap
        | HiddenCol -> this.HiddenColMap
        | Filtered -> this.FilteredMap
        | DCReduced -> 
            match this.DCMap with
            | Some m -> m
            | None -> Map.empty

let rowContainsDC (row: TruthTableRow) =
    row
    |> List.exists (fun cell -> cell.IsDC)

let rowContainsAlgebra (row: TruthTableRow) =
    row
    |> List.exists (fun cell -> cell.IsAlgebra)

//-------------------------------------------------------------------------------------//
//-----------------------------Constraint Types----------------------------------------//
//-------------------------------------------------------------------------------------//

type ConstraintSet = {
    Equalities: EqualityConstraint list
    Inequalities: InequalityConstraint list
} with
    member this.withoutIO io =
        let newEqu =
            this.Equalities
            |> List.filter (fun e -> e.IO <> io)
        let newIneq =
            this.Inequalities
            |> List.filter (fun i -> i.IO <> io)
        {Equalities = newEqu; Inequalities = newIneq}
and EqualityConstraint = {
    IO: CellIO
    Value: int
}
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

type ReasonOutOfDate = 
    | Regenerate 
    | HideColumn
    | Refilter
    
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

type TableInput = {
    IO: SimulationIO
    MaxRowCount: int
    ConstrainedRowCount: int
    AllowedRowCount: int
    Constraints: ConstraintSet
}

let initTableInput (simIO:SimulationIO) (allConstraints: ConstraintSet) =
    let (_,_,w) = simIO
    let specificEqualities =
        allConstraints.Equalities 
        |> List.filter (fun con -> con.IO = SimIO simIO)
    let specificInequalities =
        allConstraints.Inequalities
        |> List.filter (fun con -> con.IO = SimIO simIO)
    {
        IO = simIO
        MaxRowCount = int (2.0**w)
        ConstrainedRowCount = 0
        AllowedRowCount = 0
        Constraints = { Equalities = specificEqualities
                        Inequalities = specificInequalities}
    }
