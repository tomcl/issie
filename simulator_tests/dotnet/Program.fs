module DotnetTest

open System
open Helpers.JsonHelpers
open System.Diagnostics
open FilesIO
open CommonTypes

let getBaseNameNoExtension (filePath: string) =
    filePath.Split([| '/'; '\\' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.last
    |> fun x -> x.Split([| '.' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.head

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
let steps = 2000
let warmup = 5
let simulationRound = 10
let benchmarkRound = 20

let geometricMean (values: float list) =
    let product = values |> List.reduce (*)
    product ** (1.0 / (float values.Length))

let endsWith (suffix: string) (str: string) =
    str.Length >= suffix.Length
    && str.Substring(str.Length - suffix.Length) = suffix

let readMemDefns (addressWidth: int) (wordWidth: int) (fPath: string) (memories: Map<string, string>) =
    printfn "starting defn read"

    match memories |> Map.tryFind fPath with
    | Some contents ->
        printfn "read file:\n contents={contents}"

        contents.Split([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
        |> readMemLines addressWidth wordWidth
        |> (fun x ->
            printfn "read lines"
            x)
        |> Result.map Map.ofArray
    | None -> Error "File not found"

let checkMemoryContents (projectPath: string) (memories: Map<string, string>) (comp: Component) : Component =
    match comp.Type with
    | RAM1 mem
    | ROM1 mem
    | AsyncROM1 mem
    | AsyncRAM1 mem when not (endsWith "backup" (projectPath |> String.map Char.ToLower)) ->
        match mem.Init with
        | FromFile fName ->
            let fPath = pathJoin [| projectPath; (fName + ".ram") |]
            let memData = readMemDefns mem.AddressWidth mem.WordWidth fPath memories

            match memData with
            | Ok memDat ->
                if memDat <> mem.Data then
                    printfn "%s" $"Warning! RAM file {fPath} has changed so component {comp.Label} is now different"

                let mem = { mem with Data = memDat }

                { comp with
                    Type = CommonTypes.getMemType comp.Type mem }
            | Error msg ->
                printfn $"Error reloading component {comp.Label} from its file {fPath}:\n{msg}"
                comp // ignore errors for now
        | _ -> comp
    | _ -> comp

let makeLoadedComponentFromCanvasData
    (canvas: CanvasState)
    filePath
    timeStamp
    waveInfo
    (memories: Map<string, string>)
    (sheetInfo: SheetInfo option)
    =
    let projectPath = dirName filePath
    let inputs, outputs = Extractor.parseDiagramSignature canvas
    //printfn "parsed component"
    let comps, conns = canvas
    let comps' = List.map (checkMemoryContents projectPath memories) comps
    //printfn "checked component"
    let canvas = comps', conns

    let ramChanges =
        List.zip comps' comps
        |> List.filter (fun (c1, c2) -> c1.Type <> c2.Type)
        |> List.map fst
    //printfn "ram changes processed"
    let form, description =
        match sheetInfo with
        | None -> (Some User), None
        | Some sI -> sI.Form, sI.Description

    let ldc =
        { Name = getBaseNameNoExtension filePath
          TimeStamp = timeStamp
          WaveInfo = waveInfo
          FilePath = filePath
          CanvasState = canvas
          InputLabels = inputs
          OutputLabels = outputs
          Form = form
          Description = description }

    ldc, ramChanges

let tryLoadComponentFromState (filePath: string) (state: Result<SavedInfo, string>) (memories: Map<string, string>) =
    match state with
    | Error msg ->
        failwith
        <| sprintf "Can't load component %s because of Error: %s" (getBaseNameNoExtension filePath) msg
    | Ok state ->
        let canvas = getLatestCanvas state

        makeLoadedComponentFromCanvasData
            canvas
            filePath
            state.getTimeStamp
            state.getWaveInfo
            memories
            state.getSheetInfo
        |> fst

let benchmarkWithJSON designSheets memories =
#if ASSERTS
    printfn "Asserts are on"
#endif

    let loadedComps =
        designSheets
        |> Map.map (fun filePath json ->
            tryLoadComponentFromState filePath (Helpers.JsonHelpers.jsonStringToState json) memories)
        |> Map.values
        |> Seq.toList

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
                FastRun.runFastSimulation None steps fastSim |> ignore
                TimeHelpers.getTimeMs () - start)
            |> debugPrint
            |> List.skip warmup
            |> List.average
            |> fun time -> float (steps * comps) / time)
    |> geometricMean

let benchmarkWithJSONN jsonPaths jsons ramPaths rams n =
    let designSheets = Array.zip jsonPaths jsons |> Map.ofArray
    let memories = Array.zip ramPaths rams |> Map.ofArray

    [ 1..n ]
    |> List.map (fun i ->
        printfn "========== %A ==========" i
        benchmarkWithJSON designSheets memories)
    |> printfn "%A"

let benchmark path =
#if ASSERTS
    printfn "Asserts are on"
#endif

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
                FastRun.runFastSimulation None steps fastSim |> ignore
                TimeHelpers.getTimeMs () - start)
            |> debugPrint
            |> List.skip warmup
            |> List.average
            |> fun time -> float (steps * comps) / time)
    |> geometricMean

[<EntryPoint>]
let main argv =
    let path = argv[0]
    printfn "Benchmarking %s" path

    [ 1..benchmarkRound ]
    |> List.map (fun i ->
        printfn "========== %A ==========" i
        benchmark path)
    |> printfn "%A"

    0
