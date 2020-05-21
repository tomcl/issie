open TestLib
open Expecto
open Simulator
open BusWidthInferer
open SimulatorTests
open SimulatorSyncTests
open WidthInfererTests

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

let runSimulatorSyncTest (diagramName, state, loadedComponents, ticks, inputs) =
    // Build simulation graph.
    match prepareSimulation diagramName state loadedComponents with
    | Error e -> Error e
    | Ok simData ->
        // Feed all the inputs.
        let graph =
            (simData.Graph, inputs)
            ||> List.fold (fun graph (inputId, bit) -> feedSimulationInput graph inputId bit)
        let results, graph =
            (graph, [0..ticks-1])
            ||> List.mapFold (fun graph i ->
                // Extract output, then feed a new clock tick.
                extractSimulationIOs simData.Outputs graph, feedClockTick graph
            )
        Ok results

//[<Tests>]
//let simulatorTests =
//    createTestList "simulator" runSimulatorTest testCasesSimulator
[<Tests>]
let simulatorSyncTests =
    createTestList "simulatorSync" runSimulatorSyncTest testCasesSimulatorSync
//[<Tests>]
//let widthInfererTests =
//    createTestList "widthInferer" inferConnectionsWidth testCasesWidthInferer

[<EntryPoint>]
let main argv =
    TestLib.runTests ()
    0
