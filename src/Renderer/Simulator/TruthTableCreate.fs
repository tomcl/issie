module TruthTableCreate
open CommonTypes
open TimeHelpers
open SimulatorTypes
open SynchronousUtils
open NumberHelpers


type TruthTableCell = SimulationIO * WireData
type TruthTableRow = TruthTableCell list
type TruthTable = Map<TruthTableRow,TruthTableRow>

// REMEMBER TO REMOVE EVENTUALLY!
let print x =
    printfn "%A" x

let wdFromBitVal i =
    match i with
    | 0 -> [Zero]
    | 1 -> [One]
    | _ -> failwithf "what? bits can only be created as 0 or 1"

let bitCombinations (length: int) (num: int): WireData list =
    let rec bitComb (num: int) (lst: WireData list): WireData list =
        match num with
        | 0 -> if lst.IsEmpty then [Zero]::lst else lst
        | _ -> bitComb (num/2) ((wdFromBitVal (num%2))::lst)

    let combination = bitComb num []

    if combination.Length = length then
        combination
    else
        let extraZeros = 
            [1 .. (length - combination.Length)]
            |> List.map (fun _ -> [Zero])
        extraZeros @ combination

let tableLHS (inputs: SimulationIO list) =
    let tableCell (comp: SimulationIO) (wd: WireData): TruthTableCell =
        (comp,wd)
    
    let tableRow (comp: SimulationIO) (wdList: WireData list): TruthTableRow =
        wdList
        |> List.map (fun wd -> tableCell comp wd)

    let numRows = int (2.0**(float inputs.Length))

    let rowCombinations = 
        [0 .. numRows-1]
        |> List.map (bitCombinations inputs.Length)

    (inputs,rowCombinations)
    ||> List.map2 tableRow

let printTableLHS (input_tuples : (SimulationIO * WireData) list) =
    printf "Printing TT Inputs"
    let inputs = 
        input_tuples
        |> List.map fst
    let lhs = tableLHS inputs
    print <| lhs


