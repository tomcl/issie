open TestLib
open Expecto
open Simulator
open SimulatorTests

let runSimulatorTest (state, loadedComponents, inputs) =
    match prepareSimulation state loadedComponents with
    | Error e -> Error e
    | Ok simData ->
        (simData.Graph, inputs)
        ||> List.fold (fun graph (inputId, bit) -> feedSimulationInput graph inputId bit)
        |> extractSimulationIOs simData.Outputs
        |> Ok

[<Tests>]
let simulatorTests =
    createTestList "simulator" runSimulatorTest testCasesSimulator

[<EntryPoint>]
let main argv =
    TestLib.runTests ()
    0
