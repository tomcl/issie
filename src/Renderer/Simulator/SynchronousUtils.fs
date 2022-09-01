(*
    SynchronousUtils.fs

    Collection of functions that help to detect and deal with synchronous logic.
*)

module SynchronousUtils

open CommonTypes
open SimulatorTypes

/// Tells wether a component is clocked or not. Note that Custom components may
/// be clocked (cannot tell without recursively analysing them), so they are
/// considered synchronous.
let couldBeSynchronousComponent compType : bool =
    match compType with
    | DFF | DFFE | Register _ | RegisterE _ |Counter _ | ROM1 _ | RAM1 _ | AsyncRAM1 _ | Custom _ -> true // We have to assume custom components are clocked as they may be.
    | Input1 _ | Output _ | IOLabel | Constant1 _ | BusSelection _ | BusCompare _ | MergeWires | SplitWire _ | Not | And | Or | Xor
    | Nand | Nor | Xnor | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 | NbitsAdder _ | NbitsAdderNoCin _ | NbitsAdderNoCout _ | NbitsAdderNoCinCout _ | NbitsOr _ | NbitsXor _ | NbitsNot _ | NbitSpreader _ |  NbitsAnd _ | Decode4 | AsyncROM1 _ | Viewer _ -> false
    | _ -> failwithf $"Legacy components {compType} should never be read!"

/// used to do asynchronous cycle checking on atomic components with non-trivial asynch paths.
/// should this, or something like it, also be used for in dependency cycle checking?
/// returns Some (async outputPortNumbers from inputPortNumber) if component is hybrid, otherwise None.
let getHybridComponentAsyncOuts compType inputPortNumber =
    match compType, inputPortNumber with
    | AsyncRAM1 _, InputPortNumber 0 -> Some [OutputPortNumber 0]
    | AsyncRAM1 _, _ -> Some []
    | _ -> None

let isHybridComponent compType = 
    getHybridComponentAsyncOuts compType (InputPortNumber 0)
    |> Option.isSome

/// Find out whether a simulation graph has some synchronous components.
let rec hasSynchronousComponents graph : bool =
    graph
    |> Map.map (fun compId comp ->
            match comp.Type with
            | DFF | DFFE | Register _ | RegisterE _ |Counter _ | ROM1 _ | RAM1 _ | AsyncRAM1 _ -> true
            | Custom _ -> hasSynchronousComponents <| Option.get comp.CustomSimulationGraph
            | Input1 _ | Output _ | IOLabel | BusSelection _ | BusCompare _ | MergeWires | SplitWire _ | Not | And | Or
            | Xor | Nand | Nor | Xnor | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 | NbitsAdder _ | NbitsAdderNoCin _ | NbitsAdderNoCout _ | NbitsAdderNoCinCout _ | NbitSpreader _ | NbitsXor _ | NbitsOr _ | NbitsNot _ | NbitsAnd _ | Decode4 | AsyncROM1 _ | Constant1 _ | Viewer _ -> false
            | _ -> failwithf $"legacy components should never be read {comp.Type}"
        )
    |> Map.tryPick (fun compId isSync -> if isSync then Some () else None)
    |> function | Some _ -> true | None -> false

let isInput = function | Input1 _ -> true | _ -> false
let isOutput = function | Output _ -> true | _ -> false
let isCustom = function | Custom _ -> true | _ -> false
let isIOLabel = function | IOLabel _ -> true | _ -> false

let getCustomName =
    function
    | Custom custom -> custom.Name
    | _ -> failwithf "what? getCustomName should only be called with custom components"
let getCustomComponentType =
    function
    | Custom custom -> custom
    | _ -> failwithf "what? getCustomComponentType should only be called with custom components"

let getNodeOrFail
        (graph : SimulationGraph)
        (id : ComponentId)
        : SimulationComponent =
    match graph.TryFind id with
    | None -> failwithf "what? getNodeOrFail received invalid component id: %A" id
    | Some comp -> comp

/// For each graph identified by its name, keep a mapping from input ports and
/// what output ports can be reached.
type private CustomCompsCombPaths =
    Map<string, Map<InputPortNumber, OutputPortNumber list>>

/// Convert the label of a port on a custom component to its port number.
/// Assumes that all labels are unique, otherwise it is undefined behaviour.
let private labelToPortNumber label (labels : string list) =
    match List.tryFindIndex ((=) label) labels with
    | None -> failwithf "what? Label %s not present in %A" label labels
    | Some pNumber -> pNumber

/// Specific to custom components.
/// Given a map of combinatorial routes from inputs to outputs for every
/// simulation graph, perform a lookup to find the combinatorial routes from a
/// given input to the outputs. Then filter the outputs of the custom node to
/// only point to the combinatorial children (i.e. the ones connected to the
/// combinatorial outptus).
let private getCustomCombinatorialOutputs
        (combRoutes : CustomCompsCombPaths)
        (customNode : SimulationComponent)
        (inputPortNumber : InputPortNumber)
        : Map<OutputPortNumber, (ComponentId * InputPortNumber) list> =
    // Determine the outputs connected to the input port.
    let combOutputs =
        match combRoutes.TryFind <| getCustomName customNode.Type with
        | None -> failwithf "what? getCustomCombinatorialOutputs 1"
        | Some routes ->
            match routes.TryFind inputPortNumber with
            | None -> failwithf "what? getCustomCombinatorialOutputs 2"
            | Some outputs -> outputs
    // Filter only the children of the combinatorial outputs.
    customNode.Outputs
    |> Map.filter (fun outputPortNumber _ -> List.contains outputPortNumber combOutputs)


/// Given a map of combinatorial routes from inputs to outputs for every
/// simulation graph, perform a lookup to find the combinatorial routes from a
/// given input to the outputs. Then filter the outputs of the custom node to
/// only point to the combinatorial children (i.e. the ones connected to the
/// combinatorial outptus).
let getCombinatorialOutputs
        (combRoutes : CustomCompsCombPaths)
        (node : SimulationComponent)
        (inputPortNumberOpt : InputPortNumber option)
        : Map<OutputPortNumber, (ComponentId * InputPortNumber) list> =
    match node.Type with
    | Custom _ ->
        // Only extract the combinatorial outputs. When calling this function
        // with a custom component an inputPortNumber is expected as well.
        getCustomCombinatorialOutputs combRoutes node
        <| Option.get inputPortNumberOpt
    | AsyncRAM1 _ when inputPortNumberOpt = Some (InputPortNumber 0) ->
            // special case of hybrid component
            node.Outputs
    | comp when couldBeSynchronousComponent comp ->
        // Synchronous components, no combinatorial outputs.
        Map.empty
    | comp ->
        // Combinatorial component, return all outpus.
        node.Outputs


/// Start a dfs from the given node and input port number. Return the labels
/// of all output nodes that can be reached from there via a combinatorial path.
/// Note that the information about InputPortNumber is only used by custom
/// component.
let rec private dfs
        (graph : SimulationGraph)
        (combPaths : CustomCompsCombPaths)
        (currId : ComponentId)
        (inputPortNumber : InputPortNumber)
        (visited: Set<ComponentId * InputPortNumber option>)
        (outputsReached: ComponentLabel list)
        : Set<ComponentId * InputPortNumber option> * ComponentLabel list =
    let rec exploreChildren visited outputsReached children
            : Set<ComponentId * InputPortNumber option> * ComponentLabel list =
        match children with
        | [] -> visited, outputsReached
        | (childId, childInpPNum) :: children' ->
            let visited, outputsReached =
                dfs graph combPaths childId childInpPNum visited outputsReached
            // Keep on exploring other children.
            exploreChildren visited outputsReached children'

    let currNode = getNodeOrFail graph currId
    // Ignore the info about port number unless node is custom node, or a hybrid component
    let inputPortNumber =
        match currNode.Type with
        | Custom _  | AsyncRAM1 _ ->  Some inputPortNumber
        | _ -> None

    match visited.Contains (currId, inputPortNumber) with
    | true -> visited, outputsReached // Ignore already visited nodes.
    | false ->
        let visited = visited.Add (currId, inputPortNumber)
        match currNode.Type with
        | Output _ ->
            // Found a route to an output. Add its label to the list of
            // combinatorial outputs.
            visited, currNode.Label :: outputsReached
        | _ ->
            // Normal component. Get all of its combinatorial children.
            getCombinatorialOutputs combPaths currNode inputPortNumber
            |> Map.toList
            // Extract all the children for all the ports.
            |> List.collect (fun (_, portChildren) -> portChildren)
            |> exploreChildren visited outputsReached

/// For each input node in a simulation graph, determine all the output nodes it
/// can reach by just following combinatorial paths.
let private findCombinatorialPaths
        (customComp : CustomComponentType)
        (currGraph : SimulationGraph)
        (combPaths : CustomCompsCombPaths)
        : Map<InputPortNumber, OutputPortNumber list> =
    let labelToString (ComponentLabel label) = label
    let labelsToStrings (labels) = List.map fst labels
    let rec runDfs inputs =
        match inputs with
        | [] -> Map.empty
        | (_, input) :: inputs' ->
            let _, outputsReached =
                dfs currGraph combPaths input.Id (InputPortNumber 0) Set.empty []
            let res = runDfs inputs' // Keep on exploring.
            // Add results for the current inputs to the map.
            // Need to transform form labels to port numbers.
            let outputsPNums =
                outputsReached
                |> List.map (labelToString >> (fun out ->
                    labelToPortNumber out (labelsToStrings customComp.OutputLabels)
                ))
            res.Add (
                labelToPortNumber (labelToString input.Label)
                                  (labelsToStrings customComp.InputLabels)
                |> InputPortNumber,
                outputsPNums |> List.map OutputPortNumber
            )

    currGraph
    |> Map.filter (fun compId comp -> isInput comp.Type)
    |> Map.toList
    |> runDfs

/// Calculate the combinatorial paths for each custom component in a simulation
/// graph.
let rec private exploreNestedComponents
        (currGraph : SimulationGraph)
        (currName : string)
        (currStack : Set<string>)
        (result : CustomCompsCombPaths)
        : CustomCompsCombPaths option =
    let currStack = currStack.Add currName

    let rec iterateNestedComponents (res : CustomCompsCombPaths option) nested =
        match nested with
        | [] -> res
        | (_, (nextGraph, customNode)) :: nested' when res.IsNone -> None
        | (_, (nextGraph, customNode)) :: nested' when res.IsSome ->
            let result = Option.get res
            match calculateCustomCompCombPaths
                    nextGraph (getCustomName customNode)
                    (getCustomComponentType customNode) currStack result with
            | None -> None
            | Some result -> iterateNestedComponents (Some result) nested'
        | _ -> failwithf "what? Impossible case in iterateNestedComponents"

    // Extract all custom components.
    currGraph
    |> Map.filter (fun compId comp -> isCustom comp.Type)
    |> Map.map (fun compId comp -> Option.get comp.CustomSimulationGraph,
                                   comp.Type)
    |> Map.toList
    |> iterateNestedComponents (Some result)

/// Calculate the combinatorial paths for a custom component and add it to the
/// result map.
and private calculateCustomCompCombPaths
        (currGraph : SimulationGraph)
        (currName : string)
        (customComp : CustomComponentType)
        (currStack : Set<string>)
        (result : CustomCompsCombPaths)
        : CustomCompsCombPaths option =
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
            result.Add (currName, findCombinatorialPaths customComp currGraph result)
            |> Some

/// For each dependecy in a simulation graph, create a map containing:
/// - key: name of the custom component.
/// - value: a map with:
///   - key: each InputPortNumber
///   - value: a list of OutputPortNumber combinatorially connected to the
///            input.
/// An input is considered combinatorially connected to an output if there is at
/// least one logic path connecting an input directly with the output. In other
/// words, there must be at least one route from the input to output that does
/// not encounter any synchronous component.
/// Return None if such information cannot be inferred, for example if there is
/// a circular dependency.
let calculateCustomComponentsCombinatorialPaths
        (diagramName : string)
        (graph : SimulationGraph)
        : CustomCompsCombPaths option =
    exploreNestedComponents graph diagramName Set.empty Map.empty
