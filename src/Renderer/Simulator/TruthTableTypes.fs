module TruthTableTypes

open SimulatorTypes

//-------------------------------------------------------------------------------------//
//-----------------------------Truth Table Types---------------------------------------//
//-------------------------------------------------------------------------------------//

type CellData = 
    | Bits of wd: WireData
    | Algebra of var: string
    | DC //Don't Care

type TruthTableCell = {
    IO: SimulationIO
    Data: CellData
    }

type TruthTableRow = TruthTableCell list

type TruthTable = {
    // Actual Table: Mapping from Input row to Output row
    TableMap: Map<TruthTableRow,TruthTableRow>
    // Rows featuring Don't Care Terms - currently unused
    XRows: Map<TruthTableRow,TruthTableRow> option
    // If the Truth Table has been truncated
    IsTruncated: bool
    // Maximum rows the truth table could have with current input constraints
    MaxRowsWithConstraints: int
    }

//-------------------------------------------------------------------------------------//
//-----------------------------Constraint Types----------------------------------------//
//-------------------------------------------------------------------------------------//

type ConstraintSet = {
    Equalities: EqualityConstraint list
    Inequalities: InequalityConstraint list
}
and EqualityConstraint = {
    IO: SimulationIO
    Value: int
}
and InequalityConstraint = {
    LowerBound: int
    IO: SimulationIO
    UpperBound: int
    Range: int
}

type Constraint = 
    | Equality of EqualityConstraint
    | Inequality of InequalityConstraint


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
        |> List.filter (fun con -> con.IO = simIO)
    let specificInequalities =
        allConstraints.Inequalities
        |> List.filter (fun con -> con.IO = simIO)
    {
        IO = simIO
        MaxRowCount = int (2.0**w)
        ConstrainedRowCount = 0
        AllowedRowCount = 0
        Constraints = { Equalities = specificEqualities
                        Inequalities = specificInequalities}
    }
