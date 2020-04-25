open TestLib
open Expecto
open Analyser
open AnalyserTests
open Simulator
open SimulatorTests

[<Tests>]
let portChecksTests =
    // The idea is that the checks in checkPortTypesAreConsistent should never
    // fail, since the user should not be able to make them fail.
    createTestList "portChecks"
        (fun inp -> match checkPortTypesAreConsistent inp,
                          checkPortsAreConnectedProperly inp with
                    | Some err, _ | _, Some err -> Some err
                    | None, None -> None
        )
        testCasesCheckPortsAreConnectedProperly

[<Tests>]
let cycleChecksTests =
    createTestList "cycleCheks"
        (fun (graph, connections) -> analyseGraph graph connections)
        testCasesAnalyseGraph

[<Tests>]
let simulatorTests =
    createTestList "simulator"
        (fun (state, inputs) ->
            match prepareSimulation state with
            | Error e -> failwithf "what? Incorrect diagrams for simulatorTests %A" e
            | Ok simData ->
                (simData.Graph, inputs) ||> List.fold (fun graph (inputId, bit) ->
                    feedSimulationInput graph inputId bit
                )
                |> extractSimulationIOs simData.Outputs
        )
        testCasesSimulator

[<EntryPoint>]
let main argv =
    TestLib.runTests ()
    0
