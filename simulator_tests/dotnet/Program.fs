module DotnetTest

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
            Error
            <| sprintf "Error reading Issie project directory at '%s: %A" folderPath e

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
    let product = values |> List.reduce (*)
    product ** (1.0 / (float values.Length))

let benchmark path =
    let loadedComps =
        match loadAllComponentFiles path with
        | Error msg -> failwithf "Error: %s" msg
        | Ok loadedComps -> loadedComps

    loadedComps
    |> List.map (fun c ->
        printfn "Running Simulation for component %A" c.Name

        let debugPrint times =
            times
            |> List.iteri (fun idx time ->
                match idx with
                | 0 -> printf "Time: [%.2f, " time
                | i when i < (times.Length - 1) -> printf "%.2f, " time
                | _ -> printfn "%.2f]" time)
            times

        match Simulator.startCircuitSimulation simulationArraySize c.Name c.CanvasState loadedComps with
        | Error e -> failwithf "%A" e
        | Ok simData ->
            let fastSim = simData.FastSim

            let comps =
                fastSim.FComps.Values
                |> Seq.filter (fun fc -> not <| SynchronousUtils.isIOLabel fc.FType)
                |> Seq.length

            [ 1 .. (warmup + simulationRound) ]
            |> List.map (fun x ->
                fastSim.ClockTick <- 0
                let start = TimeHelpers.getTimeMs ()
                FastRun.runFastSimulation None steps fastSim
                |> ignore
                TimeHelpers.getTimeMs () - start)
            |> debugPrint
            |> List.skip warmup
            |> List.average
            |> fun time -> float (steps * comps) / time)
    |> geometricMean

[<EntryPoint>]
let main argv =
    [ 1..benchmarkRound ]
    |> List.map (fun i ->
        printfn "========== %A ==========" i
        benchmark path)
    |> printfn "%A"

    0
