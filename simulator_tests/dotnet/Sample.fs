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
            eprintfn "[%d, width=%d] : %s" d.Index d.DriverWidth
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
            eprintfn "[%d, width=%d] : %s" d.Index d.DriverWidth
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
    eprintfn "===== running new simulator"
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
        eprintfn "fs.MaxArraySize: %d" fs.MaxArraySize
        FastRun.runFastSimulation None (clockTick + simData.ClockTickNumber) fs
        |> ignore
        eprintfn "fs.MaxArraySize: %d" fs.MaxArraySize
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
    eprintfn "===== running old simulator"
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
        eprintfn "fs.MaxArraySize: %d" fs.MaxArraySize
        OldFastRun.runFastSimulation None (clockTick + simData.ClockTickNumber) fs
        |> ignore
        eprintfn "fs.MaxArraySize: %d" fs.MaxArraySize
        OldFastRun.runFastSimulation None (clockTick + simData.ClockTickNumber) fs
        |> ignore

        printDriversFData fs.DriversFData
        Ok fs.DriversFData

let n = 100 // size of simulation array
let ticks = 1000
let ldcs = testcases2loadedComponents

[<Tests>]
let functionalityTests =
    let runTestForComp comp =
        test (sprintf "compare simulator output on %A after %d ticks" comp.Name ticks) {
            let observed = runNewSimulator "" n ticks comp.CanvasState ldcs
            let expected = runOldSimulator "" n ticks comp.CanvasState ldcs
            Expect.equal
                expected
                observed
                "Outputs of new simulator is not the same as outputs of old simulator"
        }

    testSequenced
    <| testList "functionality tests" (ldcs |> List.map runTestForComp)

[<Tests>]
let performanceTests =
    let runPerformanceTestForComp comp =
        test (sprintf "compare simulator performance on %A after %d ticks" comp.Name ticks) {
            Expect.isFasterThan
                (fun () -> runNewSimulator "" n ticks comp.CanvasState ldcs)
                (fun () -> runOldSimulator "" n ticks comp.CanvasState ldcs)
                "New simulator is faster than old simulator"
        }
    testSequenced
    <| testList "performace tests" (ldcs |> List.map runPerformanceTestForComp)
