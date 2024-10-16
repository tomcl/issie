(*
    Simulator.fs

    This module collects all the APIs required for a simulation. 
*)

module Simulator

open CommonTypes
open SimGraphTypes
open SimTypes
open SynchronousUtils
open GraphBuilder
open GraphMerger
open SimulationGraphAnalyser
open CanvasExtractor

// Building a SimulationGraph has four phases (not precisely in order of execution):
// 1. Building a simulation graph made of SimulationComponents.
// 2. Merging all the necessary dependencies.
// 3. Analyse the graph to look for errors, such as unconnected ports,
//    combinatorial loops, etc...

let cap (sheet: string) = sheet.ToUpper()

/// look up a sheet in a set of loaded components
let getSheet (ldcs: LoadedComponent list) (openSheet: string) =
    match List.tryFind (fun ldc -> cap ldc.Name = cap openSheet) ldcs with
    | None -> failwithf $"getSheet failed to look up '{openSheet}' in {ldcs |> List.map (fun ldc -> ldc.Name)}"
    | Some name -> name

/// look up a sheet in a set of loaded components, return [] or a list of the matching LoadedComponent
let getLdcList (ldcs: LoadedComponent list) (openSheet: string) =
    match List.tryFind (fun ldc -> cap ldc.Name = cap openSheet) ldcs with
    | None -> failwithf $"getSheet failed to look up '{openSheet}' in {ldcs |> List.map (fun ldc -> ldc.Name)}"
    | Some name -> name

let getDirectDependencies (cs: CanvasState) =
    fst cs
    |> List.collect (fun comp ->
        match comp.Type with
        | Custom ct -> [ comp.Label, ct.Name ]
        | _ -> [])

let childrenOf (ldcs: LoadedComponent list) (sheet: string) =
    getSheet ldcs sheet
    |> (fun ldc -> getDirectDependencies ldc.CanvasState)

/// Sheets needed to simulate sheet with name sheet.
/// Sheets form a dependency tree.
/// ldcs is a list of loaded components which must include sheet
let rec sheetsNeeded (ldcs: LoadedComponent list) (sheet: string) : string list =
    let children = childrenOf ldcs sheet |> List.map snd

    children
    |> List.map (sheetsNeeded ldcs)
    |> List.concat
    |> List.append children
    |> List.append [ sheet ]
    |> List.distinct

/// canvasState: extracted canvasState from draw block.
/// projLdcs: ldcs from project (current sheet ldc may be outofdate)
/// diagramName: name of current open sheet.
/// return updated list of all LDCs
let getUpdatedLoadedComponentState diagramName canvasState projLdcs =
    let ldc' = CanvasExtractor.extractLoadedSimulatorComponent canvasState diagramName

    let ldcs =
        let ldcIsOpen ldc = ldc.Name = diagramName

        projLdcs
        |> List.map (fun ldc -> if ldcIsOpen ldc then ldc' else ldc)
        |> (fun ldcs ->
            if not <| List.exists ldcIsOpen ldcs then
                ldc' :: ldcs
            else
                ldcs)

    ldcs

/// gets the status of the simulation given current canvasState and project
let getCurrentSimulationState
    (canvState: CanvasState)
    (project: Project option)
    (fs: FastSimulation)
    : SimulationRunStatus
    =
    match project with
    | None -> SimNoProject
    | _ when fs.SimulatedTopSheet = "" -> SimEmpty
    | Some p ->
        let simIsUpToDate =
            fs.SimulatedCanvasState
            |> List.forall (fun ldc ->
                match
                    p.OpenFileName = ldc.Name,
                    List.tryFind (fun (ldc': LoadedComponent) -> ldc'.Name = ldc.Name) p.LoadedComponents
                with
                | _, None -> false
                | false, Some ldc' -> CanvasExtractor.loadedComponentIsEqual ldc ldc'
                | true, Some _ -> CanvasExtractor.stateIsEqual ldc.CanvasState canvState)

        match simIsUpToDate, p.OpenFileName = fs.SimulatedTopSheet with
        | false, _ -> SimOutOfDate
        | true, true -> SimValidSameSheet
        | true, false -> SimValidDifferentSheet

/// Helper used convert port into SheetPort for use by wave simulator determining connectivity
/// within a design sheet.
/// name is the name of the containing sheet.
let portSheetPort (compsWithIds: Map<ComponentId, Component>) (name: string) port =
    let comp = compsWithIds[ComponentId port.HostId]
    let compPort = comp.getPort (PortId port.Id)

    match compPort with
    | None -> None
    | Some cPort -> { PortOnComp = cPort; Sheet = name } |> Some

/// canvasState: extracted canvasState from draw block.
/// loadedComponents: from project
/// diagramName: name of current open sheet.
/// save all needed by simulation ldcs in the FastSimulation record.
/// The top sheet name must be saved separately - since if a simulation is being refreshed it must not change
let saveStateInSimulation
    (canvasState: CanvasState)
    (openFileName: string)
    (loadedComponents: LoadedComponent list)
    (fs: FastSimulation)
    =
    let diagramName = openFileName
    let ldcs = getUpdatedLoadedComponentState diagramName canvasState loadedComponents
    //printfn $"diagramName={diagramName}, sheetNames = {ldcs |> List.map (fun ldc -> ldc.Name)}"
    sheetsNeeded ldcs diagramName
    |> List.map (getSheet ldcs)
    |> (fun updatedLdcs ->

        let compMap, portMap =
            updatedLdcs
            |> List.map (fun ldc ->
                let comps, conns = ldc.CanvasState

                let compsWithIds =
                    comps
                    |> List.map (fun comp -> ComponentId comp.Id, comp)
                    |> Map.ofList

                let portSheetPort = portSheetPort compsWithIds ldc.Name

                let addConnToPort (portOpt: SheetPort option) conn pMap : Map<SheetPort, Connection list> =
                    match portOpt with
                    | None -> pMap
                    | Some port ->
                        pMap
                        |> Map.change port (function
                            | Some conns -> Some(conn :: conns)
                            | None -> Some [ conn ])

                let portsToConnections =
                    (Map.empty, conns)
                    ||> List.fold (fun pMap conn ->
                        pMap
                        |> addConnToPort (portSheetPort conn.Source) conn
                        |> addConnToPort (portSheetPort conn.Target) conn)
                    |> Map.toList

                ((ldc.Name, compsWithIds), portsToConnections))
            |> List.unzip

        let compMap = compMap |> Map.ofList
        let portMap = portMap |> List.concat |> Map.ofList

        { fs with
            SimulatedCanvasState = updatedLdcs
            ComponentsById = compMap
            ConnectionsByPort = portMap })



/// Extract circuit data from inputs and return a valid SimulationGraph or an error
let validateCircuitSimulation
    (diagramName: string)
    (canvasState: CanvasState)
    (loadedDependencies: LoadedComponent list)
    : Result<SimulationGraph, SimulationError>
    =
    // Tune for performance of initial zero-length simulation versus longer run.
    // Probably this is not critical.
    match runCanvasStateChecksAndBuildGraph canvasState loadedDependencies with
    | Error err -> Error err
    | Ok graph ->
        match mergeDependencies diagramName graph canvasState loadedDependencies with
        | Error err -> Error err
        | Ok graph ->
            // Simulation graph is fully merged with dependencies.
            // Perform checks on it
            let components, connections = canvasState
            match analyseSimulationGraph diagramName graph connections with
            | Some err -> Error err
            | None -> Ok graph


/// Extract circuit data from inputs and return a checked SimulationData object or an error
/// SimulationData has some technical debt, it wraps FastSimulation adding some redundant data
let startCircuitSimulation
    (simulationArraySize: int)
    (diagramName: string)
    (canvasState: CanvasState)
    (loadedDependencies: LoadedComponent list)
    : Result<SimulationData, SimulationError>
    =

    match validateCircuitSimulation diagramName canvasState loadedDependencies with
    | Error e -> Error e
    | Ok graph ->
        try
            match FastRun.buildFastSimulation simulationArraySize diagramName graph with
            | Ok fs ->
                let fs = saveStateInSimulation canvasState diagramName loadedDependencies fs
                let components, _ = canvasState
                let inputs, outputs = getSimulationIOs components


                Ok
                    {   FastSim = fs
                        Graph = graph // NB graph is now not initialised with data
                        Inputs = inputs
                        Outputs = outputs
                        IsSynchronous = hasSynchronousComponents graph
                        NumberBase = Hex
                        ClockTickNumber = 0 }
            | Error e -> Error e
        with e ->
            printfn "\nEXCEPTION:\n\n%A\n%A\n\n" e.Message e.StackTrace

            Error
                {   ErrType = InternalError e
                    InDependency = None
                    ComponentsAffected = []
                    ConnectionsAffected = [] }
        |> Result.map (fun sd ->
            //Fast.compareFastWithGraph sd |> ignore
            sd)

let startCircuitSimulationFData
    (simulationArraySize: int)
    (diagramName: string)
    (canvasState: CanvasState)
    (loadedDependencies: LoadedComponent list)
    : Result<SimulationData, SimulationError>
    =

    // Tune for performance of initial zero-length simulation versus longer run.
    // Probably this is not critical.
    match runCanvasStateChecksAndBuildGraph canvasState loadedDependencies with
    | Error err -> Error err
    | Ok graph ->
        match mergeDependencies diagramName graph canvasState loadedDependencies with
        | Error err -> Error err
        | Ok graph ->
            // Simulation graph is fully merged with dependencies.
            // Perform checks on it
            let components, connections = canvasState
            let inputs, outputs = getSimulationIOs components
            match analyseSimulationGraph diagramName graph connections with
            | Some err -> Error err
            | None ->
                try
                    match FastRun.buildFastSimulationFData simulationArraySize diagramName graph with
                    | Ok fs ->
                        let fs = saveStateInSimulation canvasState diagramName loadedDependencies fs

                        Ok
                            { FastSim = fs
                              Graph = graph // NB graph is now not initialised with data
                              Inputs = inputs
                              Outputs = outputs
                              IsSynchronous = hasSynchronousComponents graph
                              NumberBase = Hex
                              ClockTickNumber = 0 }
                    | Error e -> Error e
                with
                | AlgebraNotImplemented e -> Error e
                | e ->
                    printfn "\nEXCEPTION:\n\n%A\n%A\n\n" e.Message e.StackTrace

                    Error
                        { ErrType = InternalError e
                          InDependency = None
                          ComponentsAffected = []
                          ConnectionsAffected = [] }
                |> Result.map (fun sd ->
                    //Fast.compareFastWithGraph sd |> ignore
                    sd)


type SimCache = {
    Name: string
    ClockTickRefresh: int
    RestartSim: bool
    StoredState: LoadedComponent list
    StoredResult: Result<SimulationData, SimulationError>
    /// quick access link to Fast Simulation, or placeholder if one does not exist
    FastSim: FastSimulation
    }


        



let simCacheInit () = {
    Name = ""; 
    ClockTickRefresh = 0
    FastSim = FastCreate.simulationPlaceholder
    RestartSim = false
    StoredState = []
    StoredResult = Ok {
        FastSim = 
            FastCreate.simulationPlaceholder
        Graph = Map.empty 
        Inputs = []
        Outputs = []
        IsSynchronous=false
        NumberBase = NumberBase.Hex
        ClockTickNumber = 0
        }
    }
        
/// Used to store last canvas state and its simulation for Step & Truth Table simulation.
/// This is memoized to avoid unnecessary recomputation, stored as a single global state
/// to stop memory leaks caused by multiple simulations being stored.
let mutable simCache: SimCache = simCacheInit ()

/// Used to store last canvas state and its simulation for waveform simulation.
/// This is memoized to avoid unnecessary recomputation, stored as a single global state
/// to stop memory leaks caused by multiple simulations being stored.
let mutable simCacheWS: SimCache = simCacheInit ()

/// Used in wave simulation to obtain the currently active FastSimulation.
/// This should have previously been created, but if not it will contain a dummy
let getFastSim() = simCacheWS.FastSim

let cacheIsEqual (cache: SimCache) (ldcs: LoadedComponent list ) : bool=
    match cache.StoredResult with
    | Error _ -> false
    | Ok {FastSim =fs} -> 
        fs.SimulatedCanvasState
        |> List.forall (fun ldc' ->
            ldcs
            |> List.tryFind (fun ldc'' -> ldc''.Name = ldc'.Name)
            |> Option.map (loadedComponentIsEqual ldc')
            |> (=) (Some true))

let storedstateisEqual (cache: SimCache) (ldcs: LoadedComponent list) : bool =
    match cache.StoredState with
    | [] -> false
    | ldcsstate -> 
        ldcsstate
        |> List.forall (fun ldc' ->
            ldcs
            |> List.tryFind (fun ldc'' -> ldc''.Name = ldc'.Name)
            |> Option.map (loadedComponentIsEqualExInputDefault ldc')
            |> (=) (Some true))
            
let makeDummySimulationError msg = {
        ErrType = GenericSimError msg
        InDependency = None
        ConnectionsAffected = []
        ComponentsAffected = []
    }

/// check a waveform simulation circuit for errors without actually creating the simulation
let validateWaveSimulation
        (openFileName: string)
        (diagramName : string)
        (canvasState : CanvasState)
        (loadedDependencies : LoadedComponent list)
        : Result<SimulationGraph, SimulationError>  =
    let ldcs = addStateToLoadedComponents openFileName canvasState loadedDependencies
    let name, state, ldcs = getStateAndDependencies diagramName ldcs
    validateCircuitSimulation diagramName state ldcs

/// Start up a simulation, doing all necessary checks and generating simulation errors
/// if necesary. The code to do this is quite long so results are memoized. 
let prepareSimulationMemoized
        (isWaveSim: bool)
        (simulationArraySize: int)
        (openFileName: string)
        (diagramName : string)
        (canvasState : CanvasState)
        (loadedDependencies : LoadedComponent list)
        : Result<SimulationData, SimulationError> * CanvasState =
    //printfn $"Diagram{diagramName}, open={openFileName}, deps = {loadedDependencies |> List.map (fun dp -> dp.Name)}"
    if isWaveSim then
        let storedArraySize = simCacheWS.FastSim.MaxArraySize
        
        let ldcs = addStateToLoadedComponents openFileName canvasState loadedDependencies
        let isSame = 
                storedArraySize = simulationArraySize &&
                diagramName = simCacheWS.Name &&
                cacheIsEqual simCacheWS ldcs
        if  isSame then
            simCacheWS.StoredResult, canvasState
        else
            printfn $"New Waveform simulation of {simulationArraySize} clocks"
            simCacheWS <- {simCacheWS with StoredResult = Error <| makeDummySimulationError "Simulation deleted"; FastSim = FastCreate.simulationPlaceholder}
            let name, state, ldcs = getStateAndDependencies diagramName ldcs
            let simResult = startCircuitSimulation simulationArraySize diagramName state ldcs
            let fastSim =
                simResult
                |> Result.map (fun sd -> sd.FastSim)
                |> Result.defaultValue FastCreate.simulationPlaceholder
            simCacheWS <- {simCacheWS with Name = diagramName; StoredResult = simResult; FastSim = fastSim}
            simResult, canvasState
    else
        let storedArraySize = simCache.FastSim.MaxArraySize
        
        let ldcs = addStateToLoadedComponents openFileName canvasState loadedDependencies
        let isSame = 
                storedArraySize = simulationArraySize &&
                diagramName = simCache.Name &&
                cacheIsEqual simCache ldcs
        if  isSame then
            simCache.StoredResult, canvasState
        else
            printfn $"New simulation of {simulationArraySize} clocks"
            simCache <- {simCache with StoredResult = Error <| makeDummySimulationError "Simulation deleted"; FastSim = FastCreate.simulationPlaceholder}
            let name, state, ldcs = getStateAndDependencies diagramName ldcs
            let simResult = startCircuitSimulation simulationArraySize diagramName state ldcs
            let fastSim =
                simResult
                |> Result.map (fun sd -> sd.FastSim)
                |> Result.defaultValue FastCreate.simulationPlaceholder
            simCache <- {simCache with Name = diagramName; StoredResult = simResult; FastSim = fastSim}
            simResult, canvasState
        
