module Tests

open Expecto
open SimulatorTypes
open Simulator
open FastRun
open FastCreate
open FastReduce
open CommonTypes
open TestCases

let runNewSimulator
    (diagramName: string)
    (simulationArraySize: int)
    (canvasState: CommonTypes.CanvasState)
    (loadedDependencies: CommonTypes.LoadedComponent list)
    =
    // Build simulation graph.
    match
        Simulator.startCircuitSimulation
            simulationArraySize
            diagramName
            canvasState
            loadedDependencies
    with
    | Error err -> Error err
    | Ok simData ->
        printfn "new simData"
        Ok simData.FastSim.Drivers

let runOldSimulator
    (diagramName: string)
    (simulationArraySize: int)
    (canvasState: CommonTypes.CanvasState)
    (loadedDependencies: CommonTypes.LoadedComponent list)
    =
    // Build simulation graph.
    match
        OldSimulator.startCircuitSimulation
            simulationArraySize
            diagramName
            canvasState
            loadedDependencies
    with
    | Error err -> Error err
    | Ok simData ->
        printfn "old simData"
        Ok simData.FastSim.Drivers

[<Tests>]
let simulatorTests =
    testList
        "fake"
        [ testCase "fake"
          <| fun _ ->
              let n = 2
              let observed = runNewSimulator "" n TestCases.register []
              let expected = runOldSimulator "" n TestCases.register []
              // printfn "observed: %A" observed
              // printfn "expected: %A" expected
              Expect.equal
                  expected
                  observed
                  "Outputs of new simulator is not the same as outputs of old simulator" ]
