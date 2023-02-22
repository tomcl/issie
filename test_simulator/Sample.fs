module Tests

open Expecto
open SimulatorTypes
open Simulator
open FastRun
open FastCreate
open FastReduce
open CommonTypes
open FilesIONative

// step 1: load components from file
// step 2: merge canvas state of loaded components into one

// Random tests
// create new project template using FileMenuView.newProject
// add random components

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
              let folderPath = "/home/yujie/fyp/testcases"
              let sheetName, loadedComponents = FilesIONative.readProjectFromPath folderPath
              let topSheet =
                  loadedComponents
                  |> List.find (fun ldc -> ldc.Name = sheetName)
              let observed = runNewSimulator sheetName n topSheet.CanvasState loadedComponents
              let expected = runOldSimulator sheetName n topSheet.CanvasState loadedComponents
              // printfn "observed: %A" observed
              // printfn "expected: %A" expected
              Expect.equal
                  expected
                  observed
                  "Outputs of new simulator is not the same as outputs of old simulator" ]
