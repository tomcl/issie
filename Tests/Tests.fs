open TestLib
open Expecto
open Simulator
open BusWidthInferer
open SimulatorTests
open SimulatorSyncTests
open SimulatorMemoriesTests
open WidthInfererTests
open EEExtensions

let runSimulatorTest (diagramName, state, loadedComponents, inputs) =
    // Build simulation graph.
    match prepareSimulation diagramName state loadedComponents with
    | Error e -> Error e
    | Ok simData ->
        // Feed all the inputs and extract the outputs.
        (simData.Graph, inputs)
        ||> List.fold (fun graph (inputId, bit) -> feedSimulationInput graph inputId bit)
        |> extractSimulationIOs simData.Outputs
        |> Ok

let safeListItem lst idx =
    if idx >= List.length lst then [] else List.item idx lst

let runSimulatorSyncTest (diagramName, state, loadedComponents, ticks, inputss) =
    // Build simulation graph.
    match prepareSimulation diagramName state loadedComponents with
    | Error e -> Error e
    | Ok simData ->
        let results, graph =
            (simData.Graph, [0..ticks-1])
            ||> List.mapFold (fun graph i ->
                // Feed the inputs for this time iteration before the clock
                // tick.
                let inputs = safeListItem inputss i
                let graph =
                    (graph, inputs)
                    ||> List.fold (fun graph (inputId, bit) ->
                        feedSimulationInput graph inputId bit)
                // Extract output, then feed a new clock tick.
                extractSimulationIOs simData.Outputs graph, feedClockTick graph
            )
        Ok results

[<Tests>]
let simulatorTests =
    createTestList "simulator" runSimulatorTest testCasesSimulator
[<Tests>]
let simulatorSyncTests =
    createTestList "simulatorSync" runSimulatorSyncTest testCasesSimulatorSync
[<Tests>]
let simulatorMemoriesTests =
    createTestList "simulatorMemories" runSimulatorSyncTest testCasesSimulatorMemories
[<Tests>]
let widthInfererTests =
    createTestList "widthInferer" inferConnectionsWidth testCasesWidthInferer

let mutable Watches: Map<string,System.Diagnostics.Stopwatch> = Map.empty


/// dotnet core timer function
let stopTimer s =
    let timer = Map.tryFind s Watches
    match timer with
    | None -> failwithf "error: Timer '%s' has been stopped before it was started. timers: %A" s (Map.keys Watches)
    | Some stopWatch ->
        stopWatch.Stop()
        printfn "%s: %.4fms" s stopWatch.Elapsed.TotalMilliseconds

/// dotnet core timer function
let startTimer s = 
    Watches <-  Map.add s (System.Diagnostics.Stopwatch.StartNew()) Watches

/// measure various dotnet core performance stats
/// m = structure size.
/// n = number of iterations.
let displayPerformance m n = Helpers.checkPerformance m n startTimer stopTimer


[<EntryPoint>]
let main argv =
    displayPerformance 100 1000000
    //TestLib.runTests ()
    0
