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
// How big will it be? //
//====================//

(*
    merger, below, expands the design: it walks into every custom component and stores the whole
    merged subtree on that INSTANCE. A sheet used twice is therefore merged twice, and since that
    holds at every level the total multiplies down the hierarchy - eight sheets of eight instances
    each is not 64 components but 16 million. Nothing about the design on disk looks large, so this
    is the one way a modest-looking project exhausts memory, and it does it while the graph is
    being built rather than while anything is simulated.

    The size is exactly predictable, and much more cheaply than by building it: it depends only on
    how many components each sheet has and which sheets it instantiates. So it is worked out first,
    and a design too large to hold is refused before merger allocates any of it.
*)

/// The expanded size of the top sheet, and of every sheet it reaches, as the walk works them out.
///
/// float rather than int because these are precisely the numbers that can be astronomically large -
/// an int would overflow into a small or negative answer on exactly the designs this exists to
/// catch, and report them as tiny.
///
/// Each sheet is counted once however often it appears, so this is linear in the design even when
/// what it is counting is not. Safe to recurse because checkDependenciesAndBuildMap has already
/// rejected circular dependencies and missing sheets.
let private expandedSizes (topSheet: string) (state: CanvasState) (ldcs: LoadedComponent list) =
    let canvasOf =
        ldcs
        |> List.map (fun ldc -> ldc.Name, ldc.CanvasState)
        |> Map.ofList
        |> Map.add topSheet state

    let instancesIn (sheet: string) =
        match Map.tryFind sheet canvasOf with
        | None -> [] // a missing sheet is reported by the dependency check, not by counting
        | Some(comps, _) ->
            comps
            |> List.choose (fun comp ->
                match comp.Type with
                | Custom ct -> Some ct.Name
                | _ -> None)

    /// A sheet's own components, and its own ports. Both are needed: what a component costs in
    /// memory depends on how many ports it has, and a sheet of wide components costs more than a
    /// sheet of the same number of narrow ones.
    let ownCounts (sheet: string) =
        match Map.tryFind sheet canvasOf with
        | None -> 0.0, 0.0
        | Some(comps, _) ->
            float (List.length comps),
            comps |> List.sumBy (fun c -> float (List.length c.InputPorts + List.length c.OutputPorts))

    /// sizeOf threads the memo through rather than holding it in a mutable, so that one sheet
    /// reached by several paths is still counted once.
    let rec sizeOf (memo: Map<string, float * float>) (sheet: string) =
        match Map.tryFind sheet memo with
        | Some size -> size, memo
        | None ->
            let (childComps, childPorts), memo =
                (((0.0, 0.0), memo), instancesIn sheet)
                ||> List.fold (fun ((comps, ports), memo) child ->
                    let (cComps, cPorts), memo = sizeOf memo child
                    (comps + cComps, ports + cPorts), memo)
            let ownComps, ownPorts = ownCounts sheet
            let size = ownComps + childComps, ownPorts + childPorts
            size, Map.add sheet size memo

    sizeOf Map.empty topSheet

/// How many SimulationComponents the merged graph will hold, and how many ports they have between
/// them - which is what says what they will cost.
let expandedSize (topSheet: string) (state: CanvasState) (ldcs: LoadedComponent list) =
    expandedSizes topSheet state ldcs |> fst

/// How many SimulationComponents the merged graph will hold.
let expandedComponentCount (topSheet: string) (state: CanvasState) (ldcs: LoadedComponent list) : float =
    expandedSize topSheet state ldcs |> fst

/// What the expanded design will cost in heap, by the same reckoning checkExpansionFits refuses on.
/// Costs a walk of each sheet's canvas and never of the expansion, so it is cheap enough to ask
/// before every simulation - which the waveform simulator does, to decide how many clock cycles to
/// start with rather than only whether to start at all.
let expandedHeapEstimate (topSheet: string) (state: CanvasState) (ldcs: LoadedComponent list) : float =
    let components, ports = expandedSize topSheet state ldcs
    let portsEach = if components > 0.0 then ports / components else 0.0
    components * SimTypes.SimulationBudget.heapBytesPerComponent portsEach

/// The largest sheets below the top, which is where a design that is too large got its size.
/// Named in the refusal because "too large" on a project of small sheets is not something a user
/// can act on without being told where to look.
///
/// Read off the sizes the count already worked out, rather than counted again: those cover exactly
/// the sheets the top sheet reaches, which is both cheaper and the only set it is safe to walk -
/// a sheet elsewhere in the project may be part of a dependency cycle that was never checked.
let private biggestContributors (topSheet: string) (state: CanvasState) (ldcs: LoadedComponent list) =
    expandedSizes topSheet state ldcs
    |> snd
    |> Map.toList
    |> List.filter (fun (name, _) -> name <> topSheet)
    |> List.sortByDescending (snd >> fst)
    |> List.truncate 3
    |> List.map fst

/// Refuse a design whose expanded graph will not fit in memory, before any of it is built.
let checkExpansionFits
    (topSheet: string)
    (state: CanvasState)
    (ldcs: LoadedComponent list)
    : Result<unit, SimulationError>
    =
    let components, ports = expandedSize topSheet state ldcs
    let portsEach = if components > 0.0 then ports / components else 0.0
    let needed = expandedHeapEstimate topSheet state ldcs

    if needed <= SimTypes.SimulationBudget.maxHeapBytes then
        Ok()
    else
        let fits =
            SimTypes.SimulationBudget.maxHeapBytes
            / SimTypes.SimulationBudget.heapBytesPerComponent portsEach
        let worst = biggestContributors topSheet state ldcs |> String.concat ", "
        Error
            { ErrType =
                GenericSimError
                    $"Simulating this design means expanding every sheet into every place it is \
                      used, which comes to %.0f{components} components needing \
                      {SimTypes.SimulationBudget.formatBytes needed} - more than the \
                      {SimTypes.SimulationBudget.formatBytes SimTypes.SimulationBudget.maxHeapBytes} Issie will use \
                      for it, and more than the %.0f{fits} components that would fit. This grows by \
                      multiplication rather than addition: a sheet used four times, each of whose \
                      four instances uses another sheet four times, is sixteen copies of the \
                      innermost sheet. Most of the size here is in {worst}. \
                      Simulate one subsheet on its own, or reduce how many times the sheets below \
                      it are instantiated."
              InDependency = None
              ComponentsAffected = []
              ConnectionsAffected = [] }

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
/// The parameters of a sheet whose effective value differs from that sheet's default value.
/// An empty diff means the sheet resolves exactly as it was saved, and so exactly as
/// buildDependencyMap has already checked it.
type private BindingDiff = Map<ParameterTypes.ParamName, ParameterTypes.ParamInt>

/// Resolved sheet graphs, keyed by sheet name and the diff of its bindings from its defaults.
/// Every instance of a sheet whose bindings give the same diff resolves to the same graph, so a
/// sheet tree that recurs in several places is walked once. Sharing the graph is safe: nothing
/// mutates a SimulationComponent, and merger already shares the OutputWidths arrays across
/// instances of a sheet.
type private ResolvedSheets = Map<string * BindingDiff, SimulationGraph>

/// Evaluate every binding of a sheet to an integer.
/// None if any of them cannot be evaluated.
let private evaluateAllBindings (bindings: ParameterTypes.ParamBindings) =
    (Some Map.empty, bindings)
    ||> Map.fold (fun values name expr ->
        values
        |> Option.bind (fun values ->
            match ParameterTypes.evaluateParamExpression bindings expr with
            | Ok value -> Some (Map.add name value values)
            | Error _ -> None))

/// Which of a sheet's parameters have an effective value differing from its default value.
/// None if either set of bindings cannot be fully evaluated, in which case the sheet must be
/// processed regardless so that the failure is reported rather than skipped.
let private bindingDiff (defaults: ParameterTypes.ParamBindings) (effective: ParameterTypes.ParamBindings) =
    match evaluateAllBindings defaults, evaluateAllBindings effective with
    | Some defaultValues, Some effectiveValues ->
        // both maps have the sheet's parameters as their key set, so a value-wise filter is enough
        effectiveValues
        |> Map.filter (fun name value -> Map.tryFind name defaultValues <> Some value)
        |> Some
    | _ -> None

/// The default bindings of a sheet, which declare its parameters.
let private defaultBindingsOfSheet (loadedDependencies: LoadedComponent list) (sheetName: string) =
    loadedDependencies
    |> List.tryFind (fun (lc: LoadedComponent) -> lc.Name = sheetName)
    |> Option.bind (fun lc -> lc.LCParameterSlots)
    |> Option.map (fun ps -> ParameterTypes.bindingsOf ps.DefaultBindings)
    |> Option.defaultValue Map.empty

/// The bindings a sheet is resolved with inside one instance of it: the instance's bindings
/// override the sheet's defaults, and every parameter the sheet declares always has a value.
/// A binding for a parameter the sheet no longer declares is dropped, as it is in the properties
/// pane. This is the same merge the UI shows, so the two agree on what an instance resolves to.
let private effectiveBindings (defaults: ParameterTypes.ParamBindings) (instance: ParameterTypes.ParamBindings option) =
    match instance with
    | None -> defaults
    | Some given -> defaults |> Map.map (fun name expr -> Map.tryFind name given |> Option.defaultValue expr)

/// Recursively resolve the parameterised slots of a sheet and of the sheets inside the custom
/// components it contains, replacing the integers in each component type with the values its
/// parameter expressions evaluate to.
/// `bindings` are the effective parameter bindings for this sheet: the top sheet uses its own
/// defaults, and each sheet below uses the bindings given by the instance that contains it.
/// `memo` holds the sheets already resolved, keyed by name and by how far their bindings differ
/// from their defaults, so a sheet tree occurring in several places is walked once.
/// An expression that cannot be evaluated fails the whole simulation rather than being skipped:
/// the widths in a SimulationGraph must be concrete, and leaving one unresolved would simulate
/// hardware that differs from the design.
let rec private resolveSheet
    (memo: ResolvedSheets)
    (bindings: Map<ParameterTypes.ParamName, ParameterTypes.ParamExpression>)
    (currDiagramName: string)
    (loadedDependencies: LoadedComponent list)
    (graph: SimulationGraph)
    : Result<SimulationGraph * ResolvedSheets, SimulationError>
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
    /// This is the same evaluator the properties pane uses, so a parameter expression that cannot
    /// be evaluated - an undefined parameter, a parameter defined in terms of itself, or division
    /// or remainder by zero - is reported here in the same words, before the simulation is built.
    /// </remarks>
    let evalExpr expr = ParameterTypes.evaluateParamExpression bindings expr

    // Process a single component
    let processComponent (ComponentId compIdStr as compId) (comp: SimulationComponent) =
        let relevantSlots = paramSlots |> Map.filter (fun slot _ -> slot.CompId = compIdStr)
        
        relevantSlots
        |> Map.toList
        |> List.fold (fun compRes (slot, expr) ->
            compRes |> Result.bind (fun (c: SimulationComponent) ->
                match evalExpr expr.Expression with
                // ComponentSlots is the one place that knows how a slot maps onto a field of a
                // ComponentType; elaboration used to carry its own copy, which drifted from the
                // one the properties pane used
                | Ok value ->
                    Ok { c with Type = ComponentSlots.setSlotValue slot.CompSlot value c.Type }
                | Error msg ->
                    let exprText = ParameterTypes.renderParamExpression expr.Expression 0
                    let err: SimGraphTypes.SimulationError = {
                        ErrType = GenericSimError $"Parameter expression '{exprText}' could not be evaluated. {msg}"
                        InDependency = Some currDiagramName
                        ComponentsAffected = [compId]
                        ConnectionsAffected = []
                    }
                    Error err
            )
        ) (Ok comp)

    /// Resolve the parameters of one custom component instance and of the sheet inside it.
    /// A failure here is a failure of the whole simulation: silently keeping the unresolved
    /// sub-graph would simulate hardware that differs from the design.
    let resolveCustomComp memo compId (c: SimulationComponent) (cc: CustomComponentType) (cGraph: SimulationGraph) =
        match loadedDependencies |> List.tryFind (fun (lc: LoadedComponent) -> lc.Name = cc.Name) with
        | None ->
            Error {
                ErrType = DependencyNotFound cc.Name
                InDependency = Some currDiagramName
                ComponentsAffected = [compId]
                ConnectionsAffected = [] }
        | Some dep ->
            // An instance binding is an expression in THIS sheet's parameters; the sheet
            // inside the instance knows nothing of them, so it must receive plain values.
            let evaluatedInstanceBindings =
                match cc.ParameterBindings with
                | None -> Ok None
                | Some instanceBindings ->
                    (Ok Map.empty, instanceBindings)
                    ||> Map.fold (fun acc name expr ->
                        acc
                        |> Result.bind (fun evaluated ->
                            match evalExpr expr with
                            | Ok value -> Ok (Map.add name (ParameterTypes.PInt value) evaluated)
                            | Error msg ->
                                let (ParameterTypes.ParamName paramName) = name
                                Error {
                                    ErrType = GenericSimError $"Parameter '{paramName}' of {cc.Name} could not be evaluated. {msg}"
                                    InDependency = Some currDiagramName
                                    ComponentsAffected = [compId]
                                    ConnectionsAffected = [] }))
                    |> Result.map Some
            evaluatedInstanceBindings
            |> Result.bind (fun instanceBindings ->
                let bindings =
                    effectiveBindings (defaultBindingsOfSheet loadedDependencies cc.Name) instanceBindings
                resolveSheet memo bindings cc.Name loadedDependencies cGraph
                |> Result.map (fun (resolved, memo') ->
                    let c' =
                        { c with
                            Type = Custom { cc with ParameterBindings = Some bindings }
                            CustomSimulationGraph = Some resolved }
                    c', memo'))

    /// Apply this sheet's slots to every component, then descend into each custom component.
    let resolveComponents memo =
        ((Ok (Map.empty, memo)), graph)
        ||> Map.fold (fun acc compId comp ->
            acc
            |> Result.bind (fun (resolvedSoFar, memo) ->
                processComponent compId comp
                |> Result.bind (fun c ->
                    match c.Type, c.CustomSimulationGraph with
                    | Custom cc, Some cGraph -> resolveCustomComp memo compId c cc cGraph
                    | _ -> Ok (c, memo))
                |> Result.map (fun (c, memo') -> Map.add compId c resolvedSoFar, memo')))

    /// The widths held in a sheet's graph (OutputWidths, InputWidths) were inferred from the
    /// canvas as saved, i.e. at default parameter values, and FastSim sizes its data arrays
    /// from them. When the effective bindings differ from the defaults the resolved types
    /// differ too, so re-run width inference on the canvas with the resolved types
    /// substituted, rebuild the sheet's graph from it, and graft the resolved custom
    /// sub-graphs back on. Failures surface as ordinary simulation errors, e.g. a width
    /// mismatch that only exists at these parameter values.
    let rebuildWidths (resolved: SimulationGraph) : Result<SimulationGraph, SimulationError> =
        match loadedDependencies |> List.tryFind (fun (lc: LoadedComponent) -> lc.Name = currDiagramName) with
        | None -> Ok resolved
        | Some ldc ->
            let comps, conns = ldc.CanvasState
            /// The canvas component's type after resolution. A custom component's port
            /// widths are read from its resolved sheet, since this sheet's wires follow them.
            let resolvedType (comp: Component) =
                match Map.tryFind (comp.Id) resolved with
                | None -> comp.Type
                | Some sc ->
                    match sc.Type, sc.CustomSimulationGraph with
                    | Custom cc, Some cGraph ->
                        let portWidth isOutput ((label, width): string * int) =
                            cGraph
                            |> Map.tryPick (fun _ (child: SimulationComponent) ->
                                match child.Label, child.Type, isOutput with
                                | ComponentLabel l, Output w, true when l = label -> Some w
                                | ComponentLabel l, Input1 (w, _), false when l = label -> Some w
                                | _ -> None)
                            |> Option.defaultValue width
                            |> fun w -> label, w
                        Custom { cc with
                                    InputLabels = List.map (portWidth false) cc.InputLabels
                                    OutputLabels = List.map (portWidth true) cc.OutputLabels }
                    | t, _ -> t
            let substituted =
                comps |> List.map (fun comp -> { comp with Type = resolvedType comp }), conns
            GraphBuilder.runCanvasStateChecksAndBuildGraph substituted loadedDependencies
            |> Result.map (fun fresh ->
                fresh
                |> Map.map (fun cid freshComp ->
                    match Map.tryFind cid resolved with
                    | Some rc -> { freshComp with CustomSimulationGraph = rc.CustomSimulationGraph }
                    | None -> freshComp))
            |> Result.mapError (fun e ->
                match e.InDependency with
                | Some _ -> e
                | None -> { e with InDependency = Some currDiagramName })

    // A sheet resolved once with a given diff from its defaults resolves the same way everywhere,
    // so the whole tree below it need only be walked for the first instance with that diff.
    let diffOpt = bindingDiff (defaultBindingsOfSheet loadedDependencies currDiagramName) bindings
    match diffOpt |> Option.bind (fun diff -> Map.tryFind (currDiagramName, diff) memo) with
    | Some resolved -> Ok (resolved, memo)
    | None ->
        resolveComponents memo
        |> Result.bind (fun (resolved, memo') ->
            // at default values the saved canvas was already inferred correctly
            match Map.isEmpty paramSlots || diffOpt = Some Map.empty with
            | true -> Ok (resolved, memo')
            | false -> rebuildWidths resolved |> Result.map (fun rebuilt -> rebuilt, memo'))
        |> Result.map (fun (resolved, memo') ->
            match diffOpt with
            | Some diff -> resolved, Map.add (currDiagramName, diff) resolved memo'
            | None -> resolved, memo')

/// Resolve every parameterised slot in a sheet and, recursively, in the sheets of the custom
/// components it contains. `state` is unused: the slots come from the LoadedComponent.
let resolveParametersInSimulationGraph
    (bindings: Map<ParameterTypes.ParamName, ParameterTypes.ParamExpression>)
    (currDiagramName: string)
    (state: CanvasState)
    (loadedDependencies: LoadedComponent list)
    (graph: SimulationGraph)
    : Result<SimulationGraph, SimulationError>
    =
    resolveSheet Map.empty bindings currDiagramName loadedDependencies graph
    |> Result.map fst

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
/// Parameter Resolution:
///
/// Stage 1 - Merging (via merger function):
/// - Custom components are replaced with their internal graphs
/// - Parameter resolution is DEFERRED to avoid forward reference issues
/// - Each custom component's graph is stored in CustomSimulationGraph field
///
/// Stage 2 - Parameter Resolution (resolveParametersInSimulationGraph, after all merging):
/// - The top sheet is resolved with its own default bindings
/// - Each sheet below is resolved with the bindings of the instance containing it, so instance
///   bindings override defaults
/// - Sheets whose bindings give the same difference from their defaults are resolved once
///
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
        // After the dependency check, which is what makes the count safe to walk, and before
        // merger, which is what would allocate the design this refuses to hold.
        checkExpansionFits currDiagramName state loadedDependencies
        // Recursively replace the dependencies, in a top down fashion.
        |> Result.map (fun () -> merger graph dependencyMap loadedDependencies)
    |> Result.bind (fun graph ->
        // Resolve the top sheet with its default bindings, and every sheet below it with the
        // bindings its instance gives. There is no separate instance-binding pass: the walk
        // applies each sheet's slots and then descends using the bindings of each instance,
        // which is the same order but does the work once rather than twice.
        let parameterBindings = defaultBindingsOfSheet loadedDependencies currDiagramName
        resolveParametersInSimulationGraph parameterBindings currDiagramName state loadedDependencies graph)
