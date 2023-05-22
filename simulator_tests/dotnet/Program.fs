open FilesIO
open Helpers.JsonHelpers
open System.Diagnostics
open TimeHelpers

let timer = new Stopwatch()

let loadAllComponentFiles (folderPath: string) =
    let x =
        try
            Ok <| readdir folderPath
        with e ->
            Error <| sprintf "Error reading Issie project directory at '%s: %A" folderPath e

    match x with
    | Error msg -> Error msg
    | Ok x ->
        x
        |> Seq.toList
        |> List.filter (extName >> ((=) ".dgmNew"))
        |> List.map (fun fileName ->
            if fileNameIsBad (getBaseNameNoExtension fileName) then
                Error
                <| sprintf
                    @"Can't load file name '%s' from project '%s' because it contains incorrect characters.\n \
                    File names used as sheets must contain only alphanumeric and space characters before the '.dgm' extension"
                    fileName
                    folderPath
            else
                let filePath = pathJoin [| folderPath; fileName |]
                filePath |> tryLoadComponentFromPath)
        |> Helpers.tryFindError

let path = "/home/yujie/workspace/EEE1labs/DECA/Part3-Section1-Lab3/lab3i"
let simulationArraySize = 500
let steps = 2000
let warmup = 5
let simulationRound = 10
let benchmarkRound = 20

let geometricMean (values: float list) =
    (values |> List.reduce (*)) ** (1.0 / (float values.Length))

let benchmark path =
    let loadedComps =
        match loadAllComponentFiles path with
        | Error msg -> failwithf "Error: %s" msg
        | Ok loadedComps -> loadedComps

    loadedComps
    |> List.map (fun c ->
        printfn "Running Simulation for component %A" c.Name

        match Simulator.startCircuitSimulation simulationArraySize c.Name c.CanvasState loadedComps with
        | Error e -> failwithf "%A" e
        | Ok simData ->
            let fastSim = simData.FastSim
            let comps = fastSim.FOrderedComps |> Array.length

            [ 1 .. (warmup + simulationRound) ]
            |> List.map (fun x ->
                fastSim.ClockTick <- 0
                let start = TimeHelpers.getTimeMs ()
                FastRun.runFastSimulation None steps fastSim |> ignore
                TimeHelpers.getTimeMs () - start)
            |> List.skip warmup
            |> List.map (fun time -> float (steps * comps) / time)
            |> List.average)
    |> geometricMean

[ 1..benchmarkRound ]
|> List.map (fun i -> 
    printfn "========== %A ==========" i
    benchmark path)
|> printfn "%A"
