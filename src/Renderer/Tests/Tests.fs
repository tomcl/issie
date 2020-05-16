open TestLib
open Expecto
open Simulator
open BusWidthInferer
open SimulatorTests
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

[<Tests>]
let simulatorTests =
    createTestList "simulator" runSimulatorTest testCasesSimulator
[<Tests>]
let widthInfererTests =
    createTestList "widthInferer" inferConnectionsWidth testCasesWidthInferer

[<EntryPoint>]
let main argv =
    TestLib.runTests ()
    0
