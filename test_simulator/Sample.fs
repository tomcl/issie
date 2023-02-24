module Tests

open Expecto
open SimulatorTypes
open Simulator
open FastRun
open FastCreate
open FastReduce
open CommonTypes
open TestCases

let testcases2loadedComponents: LoadedComponent list =
    canvasStates
    |> List.map (fun (cs, name) -> Extractor.extractLoadedSimulatorComponent cs name)

let drivers2DriversFData (drivers: Driver option array) : DriverFData option array =
    drivers
    |> Array.map (fun driver ->
        match driver with
        | None -> None
        | Some d ->
            Some
                { Index = d.Index
                  DriverWidth = d.DriverWidth
                  DriverData =
                    { Index = d.DriverData.Index
                      Step =
                        d.DriverData.Step
                        |> Array.map (fun data -> Data(data)) } })

let printDriversFData (drivers: DriverFData option[]) =
    drivers
    |> Array.iter (fun driver ->
        match driver with
        | None -> ()
        | Some d ->
            printfn "[%d, width=%d] : %s" d.Index d.DriverWidth
            <| (d.DriverData.Step
                |> Array.map (fun data ->
                    match data with
                    | Data dat ->
                        match dat.Dat with
                        | Word w -> sprintf "%d" w
                        | BigWord w -> sprintf "%A" w
                    | _ -> "")
                |> String.concat ","))

let printDrivers (drivers: Driver option[]) =
    drivers
    |> Array.iter (fun driver ->
        match driver with
        | None -> ()
        | Some d ->
            printfn "[%d, width=%d] : %s" d.Index d.DriverWidth
            <| (d.DriverData.Step
                |> Array.map (fun data ->
                    match data.Dat with
                    | Word w -> sprintf "%d" w
                    | BigWord w -> sprintf "%A" w)
                |> String.concat ","))

let runNewSimulator
    (diagramName: string)
    (simulationArraySize: int)
    (clockTick: int)
    (canvasState: CommonTypes.CanvasState)
    (loadedDependencies: CommonTypes.LoadedComponent list)
    =
    printfn "===== running new simulator"
    // Build simulation
    match
        Simulator.startCircuitSimulation
            simulationArraySize
            diagramName
            canvasState
            loadedDependencies
    with
    | Error err -> Error err
    | Ok simData ->
        let fs = simData.FastSim
        // Run simulation
        printfn "fs.MaxStepNum: %d, fs.MaxArraySize: %d" fs.MaxStepNum fs.MaxArraySize
        FastRun.runFastSimulation None (clockTick + simData.ClockTickNumber) fs
        |> ignore
        printfn "fs.MaxStepNum: %d, fs.MaxArraySize: %d" fs.MaxStepNum fs.MaxArraySize
        FastRun.runFastSimulation None (clockTick + simData.ClockTickNumber) fs
        |> ignore

        printDrivers fs.Drivers

        Ok <| drivers2DriversFData fs.Drivers

let runOldSimulator
    (diagramName: string)
    (simulationArraySize: int)
    (clockTick: int)
    (canvasState: CommonTypes.CanvasState)
    (loadedDependencies: CommonTypes.LoadedComponent list)
    =
    printfn "===== running old simulator"
    // Build simulation
    match
        OldSimulator.startCircuitSimulation
            simulationArraySize
            diagramName
            canvasState
            loadedDependencies
    with
    | Error err -> Error err
    | Ok simData ->
        let fs = simData.FastSim
        // Run simulation
        printfn "fs.MaxStepNum: %d, fs.MaxArraySize: %d" fs.MaxStepNum fs.MaxArraySize
        OldFastRun.runFastSimulation None (clockTick + simData.ClockTickNumber) fs
        |> ignore
        printfn "fs.MaxStepNum: %d, fs.MaxArraySize: %d" fs.MaxStepNum fs.MaxArraySize
        OldFastRun.runFastSimulation None (clockTick + simData.ClockTickNumber) fs
        |> ignore

        printDriversFData fs.DriversFData
        Ok fs.DriversFData

[<Tests>]
let simulatorTests =
    testList
        "fake"
        [ testCase "fake"
          <| fun _ ->
              let n = 10 // size of simulation array
              let ticks = 15
              let ldcs = testcases2loadedComponents
              let entry = ldcs[1]
              printfn "Entry: %A" entry.Name
              let observed = runNewSimulator "" n ticks entry.CanvasState ldcs
              let expected = runOldSimulator "" n ticks entry.CanvasState ldcs
              Expect.equal
                  expected
                  observed
                  "Outputs of new simulator is not the same as outputs of old simulator" ]
