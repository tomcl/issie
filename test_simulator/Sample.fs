module Tests

open Expecto
open SimulatorTypes
open Simulator
open FastRun
open FastCreate
open FastReduce
open CommonTypes

let runSimulator
    (diagramName: string)
    (simulationArraySize: int)
    (canvasState: CommonTypes.CanvasState)
    (loadedDependencies: CommonTypes.LoadedComponent list)
    =
    // Build simulation graph.
    match Simulator.startCircuitSimulation simulationArraySize diagramName canvasState loadedDependencies with
    | Error err -> Error err
    | Ok simData ->
        printfn "simData"
        Ok simData.FastSim.Drivers

let runOldSimulator
    (diagramName: string)
    (simulationArraySize: int)
    (canvasState: CommonTypes.CanvasState)
    (loadedDependencies: CommonTypes.LoadedComponent list)
    =
    // Build simulation graph.
    match OldSimulator.startCircuitSimulation simulationArraySize diagramName canvasState loadedDependencies with
    | Error err -> Error err
    | Ok simData ->
        printfn "simData"
        Ok simData.FastSim.Drivers

[<Tests>]
let simulatorTests =
    testList
        "fake"
        [ testCase "fake"
          <| fun _ ->
              let n = 2
              let canvasState = CanvasStates.registor
              let observed = runSimulator "" n canvasState []
              let expected = runOldSimulator "" n canvasState []
              // printfn "observed: %A" observed
              // printfn "expected: %A" expected
              Expect.equal expected observed "Outputs of new simulator is not the same as outputs of old simulator" ]
