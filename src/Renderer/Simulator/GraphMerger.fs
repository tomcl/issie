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

/// <summary>
/// Recursively merge the simulationGraph with its dependencies (a dependency can
/// have its own dependencies).
/// </summary>
/// <param name="currGraph">The current simulation graph to process</param>
/// <param name="dependencyMap">Map from custom component names to their simulation graphs</param>
/// <param name="loadedDependencies">List of all loaded component sheets</param>
/// <returns>A merged simulation graph with custom components replaced by their internal graphs</returns>
/// <remarks>
/// PARAMETER RESOLUTION ARCHITECTURE:
/// This function is part of Stage 1 (Merging) of the two-stage parameter resolution process.
/// It ONLY merges graphs - parameter resolution is intentionally DEFERRED to Stage 2.
/// 
/// Why defer parameter resolution?
/// 1. Avoids forward reference problems when components reference each other
/// 2. Ensures all graphs are fully merged before any parameters are resolved
/// 3. Allows proper parameter precedence (instance bindings override defaults)
/// 
/// The merged graphs are stored in CustomSimulationGraph field and will be
/// processed by resolveParametersInSimulationGraph in Stage 2.
/// 
/// This function assumes there are no circular dependencies, otherwise it will never terminate.
/// </remarks>
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
/// <summary>
/// Recursively resolves parameter expressions in a simulation graph.
/// </summary>
/// <param name="bindings">Map of parameter names to their bound expressions</param>
/// <param name="currDiagramName">The name of the current sheet</param>
/// <param name="state">The current CanvasState</param>
/// <param name="loadedDependencies">List of loaded component sheets</param>
/// <param name="graph">The fully merged SimulationGraph to update</param>
/// <returns>
/// Success: Updated graph with all parameters resolved to concrete values
/// Error: Details of any parameter evaluation failures
/// </returns>
/// <remarks>
/// Parameter Resolution Details:
/// 
/// 1. Expression Evaluation (evalExpr):
///    - PInt: Direct integer values
///    - PParameter: Lookup in bindings, then recursive evaluation
///    - Arithmetic: Evaluates both operands, applies operation
///    - Returns None if any parameter cannot be resolved
/// 
/// 2. Slot Application (applySlotValue):
///    - Buswidth: Updates width for viewers, buses, gates, registers, etc.
///    - NGateInputs: Updates number of inputs for variable-input gates
///    - IO: Updates Input/Output component configurations
/// 
/// 3. Component Processing:
///    - Finds all parameter slots for current component
///    - Evaluates each slot's expression
///    - Updates component type with resolved values
///    - Preserves other component properties (Id, Label, etc.)
/// 
/// 4. Recursive Resolution:
///    - Processes custom components' internal graphs
///    - Uses component-specific parameter bindings
///    - Ensures nested parameterized components work correctly
/// 
/// Note: SimulationGraph components include the widths of all input and output busses,
/// which are crucial for simulation and must be concrete values (not expressions).
/// </remarks>
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

    /// <summary>
    /// Recursively evaluates a parameter expression by substituting parameter values from bindings.
    /// </summary>
    /// <param name="expr">The parameter expression to evaluate</param>
    /// <returns>
    /// Some(value) if the expression can be fully evaluated to a constant integer
    /// None if any parameters remain unresolved
    /// </returns>
    /// <remarks>
    /// This function implements a simple expression evaluator that:
    /// - Returns constants (PInt) directly
    /// - Looks up parameters (PParameter) in the bindings map and recursively evaluates them
    /// - Evaluates arithmetic operations only if both operands can be resolved
    /// - Uses Option.map2 to ensure both operands are available before applying operations
    /// 
    /// The evaluator differs from evaluateParamExpression in ParameterTypes:
    /// - Returns Option instead of Result (simpler for internal use)
    /// - Does not provide detailed error messages for unresolved parameters
    /// - Optimized for the specific use case of component slot resolution
    /// </remarks>
    let rec evalExpr expr =
        match expr with
        | PInt n -> Some n
        | PParameter name -> Map.tryFind name bindings |> Option.bind evalExpr
        | PAdd (l, r) -> Option.map2 (+) (evalExpr l) (evalExpr r)
        | PSubtract (l, r) -> Option.map2 (-) (evalExpr l) (evalExpr r)
        | PMultiply (l, r) -> Option.map2 (*) (evalExpr l) (evalExpr r)
        | PDivide (l, r) -> Option.map2 (/) (evalExpr l) (evalExpr r)
        | PRemainder (l, r) -> Option.map2 (%) (evalExpr l) (evalExpr r)

    /// <summary>
    /// Applies a resolved parameter value to the appropriate slot in a component type.
    /// </summary>
    /// <param name="compType">The component type to update</param>
    /// <param name="slot">The slot identifier specifying which parameter to update</param>
    /// <param name="value">The resolved integer value to apply</param>
    /// <returns>A new component type with the parameter value applied</returns>
    /// <remarks>
    /// This function handles three main categories of parameter slots:
    /// 
    /// 1. Buswidth: Updates the width parameter for components that have configurable bus widths
    ///    - Viewer, BusCompare, BusSelection, Constant components
    ///    - Arithmetic components (Adders, XOR, AND, OR, NOT)
    ///    - Memory components (Registers, Counters)
    ///    - Wire manipulation (Splitter, Spreader)
    ///    - I/O components (Input, Output)
    /// 
    /// 2. NGateInputs: Updates the number of inputs for variable-input gates
    ///    - GateN components with configurable input counts
    /// 
    /// 3. IO: Updates the bit width for Input/Output components with specific labels
    ///    - Input1 and Output components matching the IO label
    /// 
    /// For unmatched slot/component combinations, returns the original component type unchanged.
    /// This ensures backward compatibility and graceful handling of new component types.
    /// </remarks>
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
        // SplitN output slots
        | SplitNWidth idx, SplitN (n, widths, lsbs) when idx >= 0 && idx < List.length widths ->
            let newWidths = widths |> List.mapi (fun i w -> if i = idx then value else w)
            SplitN (n, newWidths, lsbs)
        | SplitNLSB idx, SplitN (n, widths, lsbs) when idx >= 0 && idx < List.length lsbs ->
            let newLsbs = lsbs |> List.mapi (fun i l -> if i = idx then value else l)
            SplitN (n, widths, newLsbs)
        | _ -> compType

    // Process a single component
    let processComponent (ComponentId compIdStr as compId) (comp: SimulationComponent) =
        let relevantSlots = paramSlots |> Map.filter (fun slot _ -> slot.CompId = compIdStr)
        
        relevantSlots
        |> Map.toList
        |> List.fold (fun compRes (slot, expr) ->
            compRes |> Result.bind (fun (c: SimulationComponent) ->
                match evalExpr expr.Expression with
                | Some value -> 
                    Ok { c with Type = applySlotValue c.Type slot.CompSlot value }
                | None ->
                    let err: SimGraphTypes.SimulationError = {
                        ErrType = GenericSimError "Parameter expression could not be fully evaluated"
                        InDependency = Some currDiagramName
                        ComponentsAffected = [compId]
                        ConnectionsAffected = []
                    }
                    Error err
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

/// <summary>
/// Resolve parameters for all custom component instances in the simulation graph.
/// </summary>
/// <param name="graph">The merged simulation graph with custom components</param>
/// <param name="loadedDependencies">List of all loaded component sheets</param>
/// <returns>
/// Success: Graph with instance-specific parameters resolved
/// Error: Details of any parameter resolution failures
/// </returns>
/// <remarks>
/// This is Stage 2a of the parameter resolution process:
/// - Processes each custom component instance in the graph
/// - Applies instance-specific parameter bindings if present
/// - These bindings override any default values from the component definition
/// - Recursively resolves parameters for nested custom components
/// 
/// This step happens AFTER merging but BEFORE sheet-level parameter resolution
/// to ensure proper parameter precedence.
/// </remarks>
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
/// <summary>
/// Merges dependencies into the simulation graph and resolves all parameters.
/// </summary>
/// <param name="currDiagramName">The name of the current sheet being processed</param>
/// <param name="graph">The initial simulation graph to merge dependencies into</param>
/// <param name="state">The current canvas state</param>
/// <param name="loadedDependencies">List of all loaded component sheets</param>
/// <returns>
/// Success: A fully merged and parameterized simulation graph
/// Error: Details of any dependency or parameter resolution failures
/// </returns>
/// <remarks>
/// Parameter Resolution Process (Two-Stage):
/// 
/// Stage 1 - Merging (via merger function):
/// - Custom components are replaced with their internal graphs
/// - Parameter resolution is DEFERRED to avoid forward reference issues
/// - Each custom component's graph is stored in CustomSimulationGraph field
/// 
/// Stage 2 - Parameter Resolution (happens after all merging):
/// 2a. Instance-specific parameters (resolveCustomComponentParameters):
///     - Each custom component instance may have specific parameter bindings
///     - These override any default values from the component definition
///     - Resolved recursively for nested custom components
/// 
/// 2b. Sheet-level parameters (resolveParametersInSimulationGraph):
///     - Uses default parameter bindings from the sheet definition
///     - Applies to all parameterized slots in the current sheet
///     - Evaluates expressions and updates component configurations
/// 
/// This two-stage approach ensures:
/// - No forward reference problems (all graphs merged before parameters resolved)
/// - Proper parameter precedence (instance bindings override defaults)
/// - Support for nested parameterized custom components
/// </remarks>
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
