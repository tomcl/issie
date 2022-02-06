module TruthTableCreate
open CommonTypes
open TimeHelpers
open SimulatorTypes
open SynchronousUtils
open NumberHelpers

// REMEMBER TO REMOVE EVENTUALLY!
let print x =
    printfn "%A" x

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
        printfn "l: %A" l
        printfn "i = %i" i
        convertIntToWireData widths[i] n))
    

let tableLHS (inputs: SimulationIO list): TruthTableRow list =
    let tableCell (comp: SimulationIO) (wd: WireData): TruthTableCell =
        {IO = comp; Data = Bits wd}

    let widthOneInputs = List.filter (fun (_,_,w) -> w = 1) inputs
    let widthMultiInputs = List.filter (fun (_,_,w) -> w > 1) inputs
    
    let widthOneRows = 
        [0 .. int (2.0**(float widthOneInputs.Length))]
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


