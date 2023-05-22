open FilesIO
open Helpers.JsonHelpers

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

let simulationArraySize = 500
let maxSteps = 2000
let path = "/home/yujie/workspace/EEE1labs/DECA/Part3-Section1-Lab3/lab3i"

let runSimulation path =
    let loadedComps =
        match loadAllComponentFiles path with
        | Error msg -> failwithf "Error: %s" msg
        | Ok loadedComps -> loadedComps

    loadedComps
    |> List.map (fun topComp ->
        printfn "Running Simulation for component %A" topComp.Name

        match Simulator.startCircuitSimulation simulationArraySize topComp.Name topComp.CanvasState loadedComps with
        | Error e -> failwithf "%A" e
        | Ok simData ->
            let fastSim = simData.FastSim
            FastRun.runFastSimulation None 2000 fastSim |> ignore
            printfn "%A" fastSim.ClockTick)

runSimulation path
