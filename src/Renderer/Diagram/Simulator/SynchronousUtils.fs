(*
    SynchronousUtils.fs

    Collection of functions that help to detect and deal with synchronous logic.
*)

module SynchronousUtils

open DiagramTypes
open SimulatorTypes

/// Find out whether a simulation graph has some synchronous components.
let rec hasSynchronousComponents graph =
    graph
    |> Map.map (fun compId comp ->
            match comp.Type with
            | DFF -> true
            | Custom _ -> hasSynchronousComponents <| Option.get comp.CustomSimulationGraph
            | _ -> false
        )
    |> Map.tryPick (fun compId isSync -> if isSync then Some () else None)
    |> function | Some _ -> true | None -> false

/// Determine whether a component is synchronous or not.
let isSynchronousComponent
        (hasRoutesFromInputToOutpuMap : Map<string, bool>)
        (compType : ComponentType) : bool =
    match compType with
    | DFF -> true
    | Custom custom ->
        // If there is at least one combinatorial path from an input to an
        // output in the graph of the custom component, then consider it
        // combinatorial, otherwise consider it synchrnous.
        // This heuristic may block legitimate (weird) designs but will never
        // allow a combinational loop.
        match hasRoutesFromInputToOutpuMap.TryFind custom.Name with
        | None -> failwithf "what? could not find custom component name in the hasRoutesFromInputToOutpuMap: %s" custom.Name
        | Some hasRoutes -> not hasRoutes
    | Input _ | Output _ | MergeWires | SplitWire _ | Not | And | Or | Xor
    | Nand | Nor | Xnor | Mux2 | Demux2 -> false

let isInput = function | Input _ -> true | _ -> false
let isOutput = function | Output _ -> true | _ -> false
let isCustom = function | Custom _ -> true | _ -> false
let getCustomName =
    function
    | Custom custom -> custom.Name
    | _ -> failwithf "what? getCustomName should only be called with custom components"

let rec getNodeOrFail
        (graph : SimulationGraph)
        (id : ComponentId)
        : SimulationComponent =
    match graph.TryFind id with
    | None -> failwithf "what? getNodeOrFail received invalid component id: %A" id
    | Some comp -> comp

/// Return None if the search reached an Output node, otherwise return the
/// updated set of visited nodes.
let rec private dfs
        (graph : SimulationGraph)
        (hasRoutes : Map<string, bool>)
        (currId : ComponentId)
        (visited: Set<ComponentId>)
        : Set<ComponentId> option =
    let rec exploreChildren visited children : Set<ComponentId> option =
        match children with
        | [] -> Some visited
        | child :: children' ->
            match dfs graph hasRoutes child visited with
            | Some visited ->
                // Keep on exploring other children.
                exploreChildren visited children'
            | None -> None

    match visited.Contains currId with
    | true -> Some visited // Ignore already visited nodes.
    | false ->
        let visited = visited.Add currId
        let currNode = getNodeOrFail graph currId
        match currNode.Type with
        | Output _ -> None // Found a route to the outputs.
        | t when isSynchronousComponent hasRoutes t -> Some visited // Stop recursion when encountering synchronous components.
        | _ -> // Combinatorial component. Recur on all children.
            currNode.Outputs
            |> Map.toList
            |> List.collect // Extract all children Ids for all of the ports.
                (fun (_, portChildren) ->
                    portChildren |> List.map (fun (childId, _) -> childId))
            |> exploreChildren visited

/// Determine wether a graph has at least one combinatorial path from input to
/// output.
let private hasAnyCombinatorialPathInputToOutput
        (currGraph : SimulationGraph)
        (hasRoutes : Map<string, bool>)
        : bool =
    let rec runDfs visited inputs =
        match inputs with
        | [] -> false
        | (_, input) :: inputs' ->
            match dfs currGraph hasRoutes input.Id visited with
            | None -> true // Found path.
            | Some visited -> runDfs visited inputs' // Keep on exploring.

    // Run a dfs from each input node of the graph and see if an output node can
    // be reached. Stop as soon as one such path is found
    currGraph
    |> Map.filter (fun compId comp -> isInput comp.Type)
    |> Map.toList
    |> runDfs Set.empty

let rec private exploreNestedComponents
        (currGraph : SimulationGraph)
        (currName : string)
        (currStack : Set<string>)
        (result : Map<string, bool>)
        : Map<string, bool> option =
    let currStack = currStack.Add currName
    let rec iterateNestedComponents (res : Map<string, bool> option) nested =
        match nested with
        | [] -> res
        | (_, (nextGraph, nextName)) :: nested' when res.IsNone -> None
        | (_, (nextGraph, nextName)) :: nested' when res.IsSome ->
            let result = Option.get res
            match inferGraphsCouldBeCombinatorial'
                    nextGraph nextName currStack result with
            | None -> None
            | Some result -> iterateNestedComponents (Some result) nested'
        | _ -> failwithf "what? Impossible case in iterateNestedComponents"

    // Extract all custom components.
    currGraph
    |> Map.filter (fun compId comp -> isCustom comp.Type)
    |> Map.map (fun compId comp ->
        Option.get comp.CustomSimulationGraph, getCustomName comp.Type)
    |> Map.toList
    |> iterateNestedComponents (Some result)

and private inferGraphsCouldBeCombinatorial'
        (currGraph : SimulationGraph)
        (currName : string)
        (currStack : Set<string>)
        (result : Map<string, bool>)
        : Map<string, bool> option =
    // Check if the current name is in the stack. If so, there is a circular
    // dependency and return None.
    // If the current graph has already an inferred value, return it
    // immediately.
    match currStack.Contains currName, result.ContainsKey currName with
    | true, _ -> None // Cyclic dependency.
    | false, true -> Some result // Already inferred.
    | false, false ->
        // New graph never explored.
        // Infer the information for all the custom components first. If any
        // nested custom component returns None, then return None here too.
        match exploreNestedComponents currGraph currName currStack result with
        | None -> None
        | Some result ->
            // All nested components are fine. Infer this graph and add it to
            // the map.
            result.Add (currName, hasAnyCombinatorialPathInputToOutput currGraph result)
            |> Some

/// Heuristically determines wether a graph and its dependencies have any
/// combinatorial logic path connecting an input directly with an output. In
/// other words, the function determines whether there is at least one route
/// from input to output that does not encounter any synchronous component.
/// Return a map containing such information for every custom component and
/// for the diagram itself. Return None if such information cannot be inferred,
/// for example if there is a circular dependency.
let makeIsCustomComponentCombinatorialMap
        (diagramName : string)
        (graph : SimulationGraph)
        : Map<string, bool> option =
    inferGraphsCouldBeCombinatorial' graph diagramName Set.empty Map.empty
