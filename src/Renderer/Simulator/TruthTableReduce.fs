module TruthTableReduce
open CommonTypes
open TimeHelpers
open SimulatorTypes
open SynchronousUtils
open NumberHelpers
open TruthTableCreate

let tableTryFind (row: TruthTableRow) (table: TruthTable) =
    match table.XRows with
    | None -> Map.tryFind row table.TableMap
    | Some xrows ->
        match Map.tryFind row table.TableMap with
        | Some r -> Some r
        | None -> 
            match Map.tryFind row xrows with
            | None -> None
            | Some xrow -> Map.tryFind xrow table.TableMap

            