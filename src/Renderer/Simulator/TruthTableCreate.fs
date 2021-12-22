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

let tableLHS (inputs: SimulationIO list): TruthTableRow list =
    let tableCell (comp: SimulationIO) (wd: WireData): TruthTableCell =
        (comp,wd)
    
    let tableRow (comp: SimulationIO) (wdList: WireData list): TruthTableRow =
        wdList
        |> List.map (fun wd -> tableCell comp wd)

    let numRows = int (2.0**(float inputs.Length))
    printfn "numRows: %i" numRows 
    let rowCombinations = 
        [0 .. numRows-1]
        |> List.map (bitCombinations inputs.Length)

    rowCombinations
    |> List.map (fun ttRow ->
        List.map2 tableCell inputs ttRow)

let rowRHS (rowLHS: TruthTableRow) (outputs: SimulationIO list) (simData: SimulationData): TruthTableRow =
    let updateOutputs (cell: TruthTableCell) =
        let ((cid,_,_),wd) = cell
        FastRun.changeInput cid wd simData.ClockTickNumber simData.FastSim
    let _ = List.map updateOutputs rowLHS

    (outputs,simData)
    ||> FastRun.extractFastSimulationIOs

let truthTable (simData: SimulationData) : TruthTable =

    //let tempFastSim = 
    //    { ClockTick = simData.FastSim.ClockTick
    //      MaxStepNum = simData.FastSim.MaxStepNum
    //      MaxArraySize = simData.FastSim.MaxArraySize
    //      FGlobalInputComps = Array.copy simData.FastSim.FGlobalInputComps
    //      FConstantComps = Array.copy simData.FastSim.FConstantComps
    //      FClockedComps = Array.copy simData.FastSim.FClockedComps
    //      FOrderedComps = Array.copy simData.FastSim.FOrderedComps
    //      FIOActive = simData.FastSim.FIOActive
    //      FIOLinks = simData.FastSim.FIOLinks
    //      FComps = simData.FastSim.FComps
    //      FSComps = simData.FastSim.FSComps
    //      FCustomOutputCompLookup = simData.FastSim.FCustomOutputCompLookup
    //      G = simData.FastSim.G }

    let tempFastSim = FastRun.buildFastSimulation 2 simData.Graph
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


