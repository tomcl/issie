module GraphMerger
(*
    GraphMerger.fs

    This module collects functions that allow to validate and merge all the
    dependencies of a SimulationGraph.
*)
open CommonTypes
open SimGraphTypes
open GraphBuilder
open Helpers
open ParameterTypes

/// Map a dependency name to its simulation graph.
type private DependencyMap = Map<string, SimulationGraph>

//======================//
// Analyse dependencies //
// =====================//

/// Map every dependency name to its list of dependencies.
type private DependencyGraph = Map<string, string list>

/// Get the name of all the dependency in a CanvasState.
let private getComponentDependencies (state: CanvasState) : string list =
    let components, _ = state

    components
    |> List.filter (fun comp ->
        match comp.Type with
        | Custom _ -> true
        | _ -> false)
    |> List.map (fun comp ->
        match comp.Type with
        | Custom c -> c.Name
        | _ -> failwith "what? Impossible, getComponentDependency")

/// Try to get the canvasState for a dependency, or return error if none could
/// be found.
let private getDependencyState
    (name: string)
    (dependencies: LoadedComponent list)
    : Result<CanvasState, SimulationError>
    =
    dependencies
    |> List.tryFind (fun dep -> dep.Name = name)
    |> function
        | Some dep -> Ok dep.CanvasState
        | None ->
            Error
                { ErrType = DependencyNotFound name
                  InDependency = None
                  ComponentsAffected = []
                  ConnectionsAffected = [] }

/// Try to build a dependencyGraph for the dependencies, or return an error
/// if there are unknown unresolved dependencies.
let rec private buildDependencyGraph
    (componentName: string)
    (state: CanvasState)
    (dependencies: LoadedComponent list)
    (dependencyGraph: DependencyGraph)
    : Result<DependencyGraph, SimulationError>
    =
    let rec iterateChildren children (dependencyGraph: DependencyGraph) =
        match children with
        | [] -> Ok dependencyGraph
        | child :: children' ->
            // Only analyse a child if it is not already in the dependencyGraph.
            match dependencyGraph.TryFind child with
            | Some _ -> iterateChildren children' dependencyGraph
            | None ->
                match getDependencyState child dependencies with
                | Error err -> Error { err with InDependency = Some componentName }
                | Ok childState ->
                    // Recur over child.
                    match buildDependencyGraph child childState dependencies dependencyGraph with
                    | Error err -> Error err
                    | Ok dependencyGraph -> iterateChildren children' dependencyGraph
    // We basically perform a dfs.
    let children = getComponentDependencies state
    let dependencyGraph = dependencyGraph.Add(componentName, children)
    iterateChildren children dependencyGraph

// Note: this cycle detection algorithm is similar to the one used in the
// analyser to spot cycles in combinatorial logic. Nonetheless, they are
// different enough that trying to make one general cycle detection algorithm
// would be quite a mess.

type private DfsType =
    // No cycle detected in the subtree. Return the new visited set and keep
    // on exploring.
    | NoCycle of Set<string>
    // Found a cycle and bactracking to record all components that form the
    // cycle. Stop recording when the dependency name that closes the loop is
    // reached.
    | Backtracking of string list * string
    // Done with backtracking. A cycle has been found and all the dependencies
    // that form it have been recorded.
    | Cycle of string list

let rec private checkDependencyCycle
    (currNode: string)
    (depGraph: DependencyGraph)
    (visited: Set<string>)
    (currStack: Set<string>)
    : DfsType
    =
    let rec exploreChildren visited currStack children : DfsType =
        match children with
        | [] -> NoCycle visited
        | child :: children' ->
            match checkDependencyCycle child depGraph visited currStack with
            | NoCycle visited ->
                // Keep on exploring other children.
                exploreChildren visited currStack children'
            | Backtracking(cycle, cycleEnd) ->
                match cycleEnd = currNode with
                | true -> Cycle(currNode :: cycle)
                | false -> Backtracking(currNode :: cycle, cycleEnd)
            | Cycle cycle -> Cycle cycle

    match currStack.Contains currNode, visited.Contains currNode with
    | true, true ->
        // Already visited in this subtree: cycle detected.
        Backtracking([ currNode ], currNode)
    | false, true ->
        // Already visited, and this node is part of no cycles.
        NoCycle visited
    | false, false ->
        // New node.
        let visited = visited.Add currNode
        let currStack = currStack.Add currNode

        match depGraph.TryFind currNode with
        | None -> failwithf "what? Could not find dependency %s in cycle detection" currNode
        | Some children -> children
        |> exploreChildren visited currStack
    | true, false ->
        // A node in the stack must always be visited.
        failwithf "what? Node never visited but in the stack, while detecting cycle: %s" currNode

/// Validate and get simulation graph for all loaded dependencies.
let private buildDependencyMap (loadedDependencies: LoadedComponent list) : Result<DependencyMap, SimulationError> =
    let dependenciesRes =
        loadedDependencies
        |> List.map (fun dep -> dep.Name, runCanvasStateChecksAndBuildGraph dep.CanvasState loadedDependencies)
    // Check if any dependency has given an error.
    let hasError (name, res) =
        match res with
        | Error _ -> true
        | Ok _ -> false

    let extractOk (name, res) =
        match res with
        | Ok d -> name, d
        | Error e -> failwithf "what? Dependency %s expected to be Ok, but has error %A" name e

    match List.tryFind hasError dependenciesRes with
    | Some(name, Error err) ->
        // Augument error saying that it happened in a dependency, so no
        // irrelevant affected components or connections will be highlighted.
        Error
            { err with
                InDependency = Some name
                ComponentsAffected = []
                ConnectionsAffected = [] }
    | None ->
        // All dependencies are Ok.
        // Create a map from their name to their simulation graph.
        dependenciesRes
        |> List.map extractOk
        |> Map.ofList
        |> Ok
    | _ -> failwith "what? Impossible case in buildDependencyMap"

/// Check if there are:
/// - unresolved dependencies
/// - loops in the dependencies
/// - errors in dependencies
/// If all dependencies are ok, return the dependencyMap.
/// Checks are only performed on the dependencies directly required by the
/// CanvasState passed.
let private checkDependenciesAndBuildMap
    (currDiagramName: string)
    (state: CanvasState)
    (dependencies: LoadedComponent list)
    : Result<DependencyMap, SimulationError>
    =
    let rec prettyPrintCycle (cycle: string list) =
        match cycle with
        | [] -> ""
        | [ name ] -> "\"" + name + "\""
        | name :: cycle' ->
            "\""
            + name
            + "\" --> "
            + (prettyPrintCycle cycle')

    match buildDependencyGraph currDiagramName state dependencies Map.empty with
    | Error err -> Error err
    | Ok dependencyGraph ->
        match checkDependencyCycle currDiagramName dependencyGraph Set.empty Set.empty with
        | Backtracking _ -> // Impossible.
            failwith "what? checkDependencyCycle finished while Backtracking"
        | Cycle cycle ->
#if ASSERTS
            assertThat (cycle.Length >= 2)
            <| sprintf "Cycle must have at least 2 dependencies: %A" cycle
#endif
            Error
                { ErrType = CycleDetected (sprintf "Found a cycle in dependencies: %s."
                    <| prettyPrintCycle cycle)
                  InDependency = None
                  ComponentsAffected = []
                  ConnectionsAffected = [] }
        | NoCycle depsUsed ->
            // Build dependency map for these dependencies.
            dependencies
            |> List.filter (fun dep -> depsUsed.Contains dep.Name)
            |> buildDependencyMap

//====================//
// Merge dependencies //
//====================//




/// Recursively merge the simulationGraph with its dependencies (a dependecy can
/// have its own dependencies).
/// This function assumes there are no circular dependencies, otherwise it will
/// never terminate.
let rec private merger (currGraph: SimulationGraph) (dependencyMap: DependencyMap) (loadedDependencies: LoadedComponent list) : SimulationGraph =
    // For each custom component, replace the Reducer with one that:
    // - when receiving an (InputPortNumber * Bit) entry (i.e. a new input),
    //   maps the InputPortNumber to the its label.
    // - find the Input node in the dependency simulationGraph with that label.
    // - feed the bit to that Input node.
    // - extracts the outputs.
    // - map the output labels to OutputPortNumbers, and this is the output of
    //   the reducer function.
    //
    // A dependency may have dependencies itself, so recursively call the merger
    // as well.
    let currGraphCopy = currGraph

    (currGraph, currGraphCopy)
    ||> Map.fold (fun currGraph compId comp ->
        match comp.Type with
        | Custom custom ->
            let dependencyGraph =
                match dependencyMap.TryFind custom.Name with
                | None -> failwithf "what? Could not find dependency %s in dependencyMap" custom.Name
                | Some dependencyGraph -> dependencyGraph

            let recursivelyMergedGraph = merger dependencyGraph dependencyMap loadedDependencies
            
            // Store the parameter bindings in the component for later resolution
            // We'll resolve parameters after all merging is complete to avoid forward reference issues
            let newComp = { comp with CustomSimulationGraph = Some recursivelyMergedGraph }

            currGraph.Add(compId, newComp)
        | _ -> currGraph // Ignore non-custom components.
    )
/// Recursively update the SimulationGraph replacing integers with the correct parameter values.
/// Parameter names, and slots using parameters, can be picked up from loadedDependencies
/// Parameters can be resolved by looking at the parameter bindings of the custom components.
/// bindings: parameter bindings for the current sheet.
/// currDiagramName: the name of the current sheet.
/// state: the current CanvasState.
/// loadedDependencies: the loaded dependencies.
/// graph: the fully merged SimulationGraph to update.
/// NB SimulationGraph components include the widths of all input and output busses.
let rec resolveParametersInSimulationGraph
    (bindings: Map<ParameterTypes.ParamName, ParameterTypes.ParamExpression>)
    (currDiagramName: string)
    (state: CanvasState)
    (loadedDependencies: LoadedComponent list)
    (graph: SimulationGraph)
    : Result<SimulationGraph, SimulationError>
    =
    let paramSlots = 
        loadedDependencies
        |> List.tryFind (fun lc -> lc.Name = currDiagramName)
        |> Option.bind (fun sheet -> sheet.LCParameterSlots)
        |> Option.map (fun ps -> ps.ParamSlots)
        |> Option.defaultValue Map.empty

    // Simplified expression evaluation
    let rec evalExpr expr =
        match expr with
        | PInt n -> Some n
        | PParameter name -> Map.tryFind name bindings |> Option.bind evalExpr
        | PAdd (l, r) -> Option.map2 (+) (evalExpr l) (evalExpr r)
        | PSubtract (l, r) -> Option.map2 (-) (evalExpr l) (evalExpr r)
        | PMultiply (l, r) -> Option.map2 (*) (evalExpr l) (evalExpr r)
        | PDivide (l, r) -> Option.map2 (/) (evalExpr l) (evalExpr r)
        | PRemainder (l, r) -> Option.map2 (%) (evalExpr l) (evalExpr r)

    // Update component type with new parameter value
    let applySlotValue compType (slot: CompSlotName) value =
        match slot, compType with
        // Buswidth updates
        | Buswidth, Viewer _ -> Viewer value
        | Buswidth, BusCompare1 (_, cv, dt) -> BusCompare1 (value, cv, dt)
        | Buswidth, BusSelection (_, lsb) -> BusSelection (value, lsb)
        | Buswidth, Constant1 (_, cv, dt) -> Constant1 (value, cv, dt)
        | Buswidth, NbitsAdder _ -> NbitsAdder value
        | Buswidth, NbitsAdderNoCin _ -> NbitsAdderNoCin value
        | Buswidth, NbitsAdderNoCout _ -> NbitsAdderNoCout value
        | Buswidth, NbitsAdderNoCinCout _ -> NbitsAdderNoCinCout value
        | Buswidth, NbitsXor (_, op) -> NbitsXor (value, op)
        | Buswidth, NbitsAnd _ -> NbitsAnd value
        | Buswidth, NbitsNot _ -> NbitsNot value
        | Buswidth, NbitsOr _ -> NbitsOr value
        | Buswidth, NbitSpreader _ -> NbitSpreader value
        | Buswidth, SplitWire _ -> SplitWire value
        | Buswidth, Register _ -> Register value
        | Buswidth, RegisterE _ -> RegisterE value
        | Buswidth, Counter _ -> Counter value
        | Buswidth, CounterNoLoad _ -> CounterNoLoad value
        | Buswidth, CounterNoEnable _ -> CounterNoEnable value
        | Buswidth, CounterNoEnableLoad _ -> CounterNoEnableLoad value
        | Buswidth, Shift (_, sw, st) -> Shift (value, sw, st)
        | Buswidth, BusCompare (_, cv) -> BusCompare (value, cv)
        | Buswidth, Input _ -> Input value
        | Buswidth, Input1 (_, dv) -> Input1 (value, dv)
        | Buswidth, Output _ -> Output value
        | Buswidth, Constant (_, cv) -> Constant (value, cv)
        // Gate inputs
        | NGateInputs, GateN (gt, _) -> GateN (gt, value)
        // IO ports
        | IO _, Input1 (_, dv) -> Input1 (value, dv)
        | IO _, Output _ -> Output value
        | _ -> compType

    // Process a single component
    let processComponent (ComponentId compIdStr as compId) comp =
        let relevantSlots = paramSlots |> Map.filter (fun slot _ -> slot.CompId = compIdStr)
        
        relevantSlots
        |> Map.toList
        |> List.fold (fun compRes (slot, expr) ->
            compRes |> Result.bind (fun c ->
                match evalExpr expr.Expression with
                | Some value -> 
                    Ok { c with Type = applySlotValue c.Type slot.CompSlot value }
                | None ->
                    Error ({ 
                        ErrType = GenericSimError "Parameter expression could not be fully evaluated"
                        InDependency = Some currDiagramName
                        ComponentsAffected = [compId]
                        ConnectionsAffected = [] 
                    }: SimulationError)
            )
        ) (Ok comp)

    // Process all components and handle custom component bindings
    graph
    |> Map.map (fun compId comp -> 
        processComponent compId comp
        |> Result.map (fun c ->
            match c.Type, c.CustomSimulationGraph with
            | Custom cc, Some cGraph ->
                let bindings = 
                    cc.ParameterBindings 
                    |> Option.defaultValue (
                        loadedDependencies
                        |> List.tryFind (fun lc -> lc.Name = cc.Name)
                        |> Option.bind (fun lc -> lc.LCParameterSlots)
                        |> Option.map (fun ps -> ps.DefaultBindings)
                        |> Option.defaultValue Map.empty
                    )
                let resolvedGraph = 
                    loadedDependencies
                    |> List.tryFind (fun lc -> lc.Name = cc.Name)
                    |> Option.bind (fun dep ->
                        match resolveParametersInSimulationGraph bindings cc.Name dep.CanvasState loadedDependencies cGraph with
                        | Ok resolved -> Some resolved
                        | Error _ -> None
                    )
                    |> Option.defaultValue cGraph
                { c with Type = Custom { cc with ParameterBindings = Some bindings }
                         CustomSimulationGraph = Some resolvedGraph }
            | _ -> c
        )
    )
    |> Map.toList
    |> List.fold (fun res (id, compRes) ->
        match res, compRes with
        | Ok m, Ok c -> Ok (Map.add id c m)
        | Error e, _ | _, Error e -> Error e
    ) (Ok Map.empty)

/// Resolve parameters for all custom components in the simulation graph
let rec private resolveCustomComponentParameters 
    (graph: SimulationGraph) 
    (loadedDependencies: LoadedComponent list)
    : Result<SimulationGraph, SimulationError> =
    graph
    |> Map.map (fun _ comp ->
        match comp.Type, comp.CustomSimulationGraph with
        | Custom custom, Some customGraph when custom.ParameterBindings |> Option.exists (Map.isEmpty >> not) ->
            loadedDependencies
            |> List.tryFind (fun ldc -> ldc.Name = custom.Name)
            |> Option.bind (fun ldc ->
                let bindings = custom.ParameterBindings |> Option.defaultValue Map.empty
                match resolveParametersInSimulationGraph bindings custom.Name ldc.CanvasState loadedDependencies customGraph with
                | Ok resolved -> Some { comp with CustomSimulationGraph = Some resolved }
                | Error _ -> None
            )
            |> Option.defaultValue comp
        | _ -> comp
    )
    |> Ok

/// Try to resolve all the dependencies in a graph, and replace the reducer
/// of the custom components with a simulationgraph.
/// Return an error if there are problems with the dependencies.
/// For example, if the graph of an ALU refers to custom component such as
/// adders, replace them with the actual simulation graph for the adders.
let mergeDependencies
    (currDiagramName: string)
    (graph: SimulationGraph)
    (state: CanvasState)
    (loadedDependencies: LoadedComponent list)
    : Result<SimulationGraph, SimulationError>
    =
    match checkDependenciesAndBuildMap currDiagramName state loadedDependencies with
    | Error e -> Error e
    | Ok dependencyMap ->
        // Recursively replace the dependencies, in a top down fashion.
        Ok <| merger graph dependencyMap loadedDependencies
    |> Result.bind (fun graph ->
        // First resolve instance-specific parameters for custom components
        resolveCustomComponentParameters graph loadedDependencies)
    |> Result.bind (fun graph ->
        // Then resolve sheet-level parameters using default bindings
        let currentSheet = 
            loadedDependencies
            |> List.tryFind (fun lc -> lc.Name = currDiagramName)
        
        let parameterBindings = 
            match currentSheet with
            | Some sheet -> 
                match sheet.LCParameterSlots with
                | Some paramInfo -> paramInfo.DefaultBindings
                | None -> Map.empty
            | None -> Map.empty
        
        resolveParametersInSimulationGraph parameterBindings currDiagramName state loadedDependencies graph)
