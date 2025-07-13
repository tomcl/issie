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
    try
        // Find current sheet's parameter information
        let currentSheetOpt = 
            loadedDependencies
            |> List.tryFind (fun lc -> lc.Name = currDiagramName)

        let paramSlots = 
            match currentSheetOpt with
            | Some sheet -> 
                match sheet.LCParameterSlots with
                | Some paramInfo -> paramInfo.ParamSlots
                | None -> Map.empty
            | None -> Map.empty

        // Helper function to evaluate a parameter expression
        let evaluateExpression expr paramBindings =
            let rec recursiveEvaluation (expr: ParamExpression) : ParamExpression =
                match expr with
                | PInt _ -> expr 
                | PParameter name -> 
                    match Map.tryFind name paramBindings with
                    | Some evaluated -> evaluated
                    | None -> PParameter name
                | PAdd (left, right) ->
                    match recursiveEvaluation left, recursiveEvaluation right with
                    | PInt l, PInt r -> PInt (l+r)
                    | newLeft, newRight -> PAdd (newLeft, newRight)
                | PSubtract (left, right) -> 
                    match recursiveEvaluation left, recursiveEvaluation right with
                    | PInt l, PInt r -> PInt (l-r)
                    | newLeft, newRight -> PSubtract (newLeft, newRight)
                | PMultiply (left, right) ->
                    match recursiveEvaluation left, recursiveEvaluation right with
                    | PInt l, PInt r -> PInt (l*r)
                    | newLeft, newRight -> PMultiply (newLeft, newRight)
                | PDivide (left, right) ->
                    match recursiveEvaluation left, recursiveEvaluation right with
                    | PInt l, PInt r -> PInt (l/r)
                    | newLeft, newRight -> PDivide (newLeft, newRight)
                | PRemainder (left, right) ->
                    match recursiveEvaluation left, recursiveEvaluation right with
                    | PInt l, PInt r -> PInt (l%r)
                    | newLeft, newRight -> PRemainder (newLeft, newRight)
            
            match recursiveEvaluation expr with
            | PInt value -> Ok value
            | _ -> 
                let error = { 
                    ErrType = GenericSimError "Parameter expression could not be fully evaluated"
                    InDependency = Some currDiagramName
                    ComponentsAffected = []
                    ConnectionsAffected = [] 
                }
                Error error

        // Helper function to update component type based on parameter slot
        let updateComponentType compType slot newValue =
            match slot with
            | Buswidth ->
                match compType with
                | Viewer _ -> Viewer newValue
                | BusCompare1 (_, compareValue, dialogText) -> BusCompare1 (newValue, compareValue, dialogText)
                | BusSelection (_, outputLSBit) -> BusSelection (newValue, outputLSBit)
                | Constant1 (_, constValue, dialogText) -> Constant1 (newValue, constValue, dialogText)
                | NbitsAdder _ -> NbitsAdder newValue
                | NbitsAdderNoCin _ -> NbitsAdderNoCin newValue
                | NbitsAdderNoCout _ -> NbitsAdderNoCout newValue
                | NbitsAdderNoCinCout _ -> NbitsAdderNoCinCout newValue
                | NbitsXor (_, arithmeticOp) -> NbitsXor (newValue, arithmeticOp)
                | NbitsAnd _ -> NbitsAnd newValue
                | NbitsNot _ -> NbitsNot newValue
                | NbitsOr _ -> NbitsOr newValue
                | NbitSpreader _ -> NbitSpreader newValue
                | SplitWire _ -> SplitWire newValue
                | Register _ -> Register newValue
                | RegisterE _ -> RegisterE newValue
                | Counter _ -> Counter newValue
                | CounterNoLoad _ -> CounterNoLoad newValue
                | CounterNoEnable _ -> CounterNoEnable newValue
                | CounterNoEnableLoad _ -> CounterNoEnableLoad newValue
                | Shift (_, shifterWidth, shiftType) -> Shift (newValue, shifterWidth, shiftType)
                | BusCompare (_, compareValue) -> BusCompare (newValue, compareValue)
                | Input _ -> Input newValue
                | Input1 (_, defaultValue) -> Input1 (newValue, defaultValue)
                | Output _ -> Output newValue
                | Constant (_, constValue) -> Constant (newValue, constValue)
                | _ -> compType
            | NGateInputs ->
                match compType with
                | GateN (gateType, _) -> GateN (gateType, newValue)
                | _ -> compType
            | IO _ ->
                match compType with
                | Input1 (_, defaultValue) -> Input1 (newValue, defaultValue)
                | Output _ -> Output newValue
                | _ -> compType
            | CustomCompParam _ -> compType // Custom component parameters handled separately

        // Process each component in the graph
        let processComponent (compId: ComponentId) (comp: SimulationComponent) =
            try
                // Check if this component has parameter slots
                let (ComponentId compIdStr) = compId
                let relevantSlots = 
                    paramSlots 
                    |> Map.filter (fun slot _ -> slot.CompId = compIdStr)

                if Map.isEmpty relevantSlots then
                    // No parameters to resolve for this component
                    Ok comp
                else
                    // Resolve parameters for this component
                    let mutable updatedCompType = comp.Type
                    let mutable lastError = None
                    
                    for KeyValue(slot, constrainedExpr) in relevantSlots do
                        match evaluateExpression constrainedExpr.Expression bindings with
                        | Ok evaluatedValue -> 
                            updatedCompType <- updateComponentType updatedCompType slot.CompSlot evaluatedValue
                        | Error err -> 
                            lastError <- Some err

                    match lastError with
                    | Some err -> Error err
                    | None ->
                        // Handle custom components with parameter bindings
                        let finalCompType = 
                            match updatedCompType with
                            | Custom customComp ->
                                // Merge default bindings with instance-specific bindings
                                let defaultBindings = 
                                    loadedDependencies
                                    |> List.tryFind (fun lc -> lc.Name = customComp.Name)
                                    |> Option.bind (fun lc -> lc.LCParameterSlots)
                                    |> Option.map (fun ps -> ps.DefaultBindings)
                                    |> Option.defaultValue Map.empty

                                let mergedBindings = 
                                    match customComp.ParameterBindings with
                                    | Some instanceBindings -> 
                                        Map.fold (fun acc key value -> Map.add key value acc) defaultBindings instanceBindings
                                    | None -> defaultBindings

                                Custom { customComp with ParameterBindings = Some mergedBindings }
                            | _ -> updatedCompType

                        Ok { comp with Type = finalCompType }
            with
            | ex -> 
                let error = {
                    ErrType = InternalError ex
                    InDependency = Some currDiagramName
                    ComponentsAffected = [compId]
                    ConnectionsAffected = []
                }
                Error error

        // Apply parameter resolution to all components
        let processedGraphResult = 
            graph
            |> Map.toList
            |> List.map (fun (compId, comp) -> 
                match processComponent compId comp with
                | Ok updatedComp -> Ok (compId, updatedComp)
                | Error err -> Error err)
            |> List.fold (fun acc item ->
                match acc, item with
                | Ok components, Ok comp -> Ok (comp :: components)
                | Error err, _ | _, Error err -> Error err) (Ok [])

        match processedGraphResult with
        | Ok components -> 
            let updatedGraph = Map.ofList components
            
            // Recursively process custom component simulation graphs
            let processCustomGraphs (graph: SimulationGraph) =
                graph
                |> Map.map (fun _ comp ->
                    match comp.CustomSimulationGraph with
                    | Some customGraph ->
                        // Get parameter bindings for this custom component
                        let customBindings = 
                            match comp.Type with
                            | Custom customComp -> 
                                customComp.ParameterBindings |> Option.defaultValue Map.empty
                            | _ -> Map.empty
                        
                        // Find the dependency for this custom component
                        let customDep = 
                            loadedDependencies
                            |> List.tryFind (fun lc -> 
                                match comp.Type with
                                | Custom customComp -> lc.Name = customComp.Name
                                | _ -> false)
                        
                        match customDep with
                        | Some dep ->
                            match resolveParametersInSimulationGraph customBindings dep.Name dep.CanvasState loadedDependencies customGraph with
                            | Ok resolvedGraph -> { comp with CustomSimulationGraph = Some resolvedGraph }
                            | Error _ -> comp // Keep original on error
                        | None -> comp
                    | None -> comp)
            
            Ok (processCustomGraphs updatedGraph)
        | Error err -> Error err

    with
    | ex -> 
        let error = {
            ErrType = InternalError ex
            InDependency = Some currDiagramName
            ComponentsAffected = []
            ConnectionsAffected = []
        }
        Error error

/// Resolve parameters for all custom components in the simulation graph
/// This is done after initial merging to avoid forward reference issues
let rec private resolveCustomComponentParameters 
    (graph: SimulationGraph) 
    (loadedDependencies: LoadedComponent list)
    : Result<SimulationGraph, SimulationError> =
    try
        let updatedGraph = 
            (graph, graph)
            ||> Map.fold (fun updatedGraph compId comp ->
                match comp.Type with
                | Custom custom ->
                    match comp.CustomSimulationGraph, custom.ParameterBindings with
                    | Some customGraph, Some instanceBindings when not (Map.isEmpty instanceBindings) ->
                        // Find the LoadedComponent for this custom component
                        let customComponentLdc = 
                            loadedDependencies |> List.tryFind (fun ldc -> ldc.Name = custom.Name)
                        
                        match customComponentLdc with
                        | Some ldc ->
                            // Apply parameter resolution using instance bindings
                            match resolveParametersInSimulationGraph instanceBindings custom.Name ldc.CanvasState loadedDependencies customGraph with
                            | Ok resolvedGraph -> 
                                let updatedComp = { comp with CustomSimulationGraph = Some resolvedGraph }
                                updatedGraph.Add(compId, updatedComp)
                            | Error _ -> updatedGraph // Keep original on error
                        | None -> updatedGraph
                    | _ -> updatedGraph
                | _ -> updatedGraph
            )
        Ok updatedGraph
    with
    | ex -> 
        let error = {
            ErrType = InternalError ex
            InDependency = None
            ComponentsAffected = []
            ConnectionsAffected = []
        }
        Error error

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
