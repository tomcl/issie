open TestLib
open Expecto
open Simulator
open SimulatorTests

[<Tests>]
let simulatorTests =
    createTestList "simulator"
        (fun (state, inputs) ->
            match prepareSimulation state with
            | Error e -> Error e
            | Ok simData ->
                (simData.Graph, inputs) ||> List.fold (fun graph (inputId, bit) ->
                    feedSimulationInput graph inputId bit
                )
                |> extractSimulationIOs simData.Outputs
                |> Ok
        )
        testCasesSimulator

[<EntryPoint>]
let main argv =
    TestLib.runTests ()
    0
