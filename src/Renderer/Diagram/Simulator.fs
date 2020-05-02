module Simulator

open DiagramTypes
open Analyser

open JSHelpers

// Simulating a circuit has three phases:
// 1. Building a simulation graph made of SimulationComponents.
// 2. Analyse the graph to look for errors, such as unconnected ports,
//    combinatorial loops, etc... In Analyser.fs.
// 3. Setting the values of the input nodes of the graph to kickstart the
//    simulation process.

//========================//
// Build simulation graph //
//========================//

/// This function should only be called on Component ports, never on Connection
/// ports: ports in Components should always have Some portNumber, ports in
/// Connections should have None.
let private getPortNumberOrFail port =
    match port with
    | None -> failwithf "what? Component ports should always have a portNumber"
    | Some p -> p

let private bitNot bit =
    match bit with
    | Zero -> One
    | One -> Zero

let private bitAnd bit0 bit1 =
    match bit0, bit1 with
    | One, One -> One
    | _, _ -> Zero

let private bitOr bit0 bit1 =
    match bit0, bit1 with
    | Zero, Zero -> Zero
    | _, _ -> One

let private bitXor bit0 bit1 =
    match bit0, bit1 with
    | Zero, One | One, Zero -> One
    | _, _ -> Zero

let private bitNand bit0 bit1 =
    bitAnd bit0 bit1 |> bitNot

let private bitNor bit0 bit1 =
    bitOr bit0 bit1 |> bitNot

let private bitXnor bit0 bit1 =
    bitXor bit0 bit1 |> bitNot

/// Make sure that the size of the inputs of a SimulationComponent is as
/// expected.
let private assertNotTooManyInputs
        (inputs : Map<InputPortNumber, Bit>)
        (cType : ComponentType)
        (expected : int)
        : unit =
    if inputs.Count > expected
    then failwithf "what? assertNotTooManyInputs failed for %A: %d > %d" cType inputs.Count expected  

/// Extract the values of the inputs of a SimulationComponent.
/// If any of these inputs is missing, return None.
/// The values are returned in the the passed order. E.g. if portNumbers is
/// [0, 1, 2], the returned value will be [bit0, bit1, bit2].
let rec private getValuesForPorts
        (inputs : Map<InputPortNumber, Bit>)
        (portNumbers : InputPortNumber list)
        : (Bit list) option =
    match portNumbers with
    | [] -> Some []
    | portNumber :: portNumbers' ->
        match inputs.TryFind portNumber with
        | None -> None
        | Some bit ->
            match getValuesForPorts inputs portNumbers' with
            | None -> None
            | Some bits -> Some <| bit :: bits

let private getBinaryGateReducer (op : Bit -> Bit -> Bit) componentType =
    fun inputs ->
        assertNotTooManyInputs inputs componentType 2
        match getValuesForPorts inputs [InputPortNumber 0; InputPortNumber 1] with
        | None -> None // Wait for more inputs.
        | Some [bit0; bit1] -> Some <| Map.empty.Add (OutputPortNumber 0, op bit1 bit0)
        | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType inputs

/// Given a component type, return a function takes its inputs and transforms
/// them into outputs. The reducer should return None if there are not enough
/// inputs to calculate the outputs.
let private getReducer
        (componentType : ComponentType)
        : Map<InputPortNumber, Bit> -> Map<OutputPortNumber, Bit> option =
    match componentType with
    | Input ->
        fun inputs ->
            assertNotTooManyInputs inputs componentType 1
            // Simply forward the input.
            // Note that the input of and Input node must be feeded manually.
            match getValuesForPorts inputs [InputPortNumber 0] with
            | None -> None // Wait for more inputs.
            | Some [bit] -> Some <| Map.empty.Add (OutputPortNumber 0, bit)
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType inputs
    | Output ->
        fun inputs ->
            assertNotTooManyInputs inputs componentType 1
            match getValuesForPorts inputs [InputPortNumber 0] with
            | None | Some [_] -> None // Do nothing with it. Just make sure it is received.
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType inputs
    | Not ->
        fun inputs ->
            assertNotTooManyInputs inputs componentType 1
            match getValuesForPorts inputs [InputPortNumber 0] with
            | None -> None // Wait for more inputs.
            | Some [bit] -> Some <| Map.empty.Add (OutputPortNumber 0, bitNot bit)
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType inputs
    | And  -> getBinaryGateReducer bitAnd And
    | Or   -> getBinaryGateReducer bitOr Or
    | Xor  -> getBinaryGateReducer bitXor Xor
    | Nand -> getBinaryGateReducer bitNand Nand
    | Nor  -> getBinaryGateReducer bitNor Nor
    | Xnor -> getBinaryGateReducer bitXnor Xnor
    | Mux2 ->
        fun inputs ->
            assertNotTooManyInputs inputs componentType 3
            match getValuesForPorts inputs [InputPortNumber 0; InputPortNumber 1; InputPortNumber 2] with
            | None -> None // Wait for more inputs.
            | Some [bit0; bit1; bitSelect] ->
                let out = if bitSelect = Zero then bit0 else bit1
                Some <| Map.empty.Add (OutputPortNumber 0, out)
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType inputs
    | Custom c ->
        fun inputs ->
            failwithf "what? Custom components used during a simulation: %A" c

/// Build a map that, for each source port in the connections, keeps track of
/// the ports it targets.
/// It makes no sense to extract the PortNumber in this function as it is always
/// set to None for ports in connections.
let private buildSourceToTargetPortMap
        (connections : Connection list)
        : Map<OutputPortId, (ComponentId * InputPortId) list> =
    (Map.empty, connections) ||> List.fold (fun map conn ->
        let key = OutputPortId conn.Source.Id
        let target = ComponentId conn.Target.HostId, InputPortId conn.Target.Id
        // Append the new target to the list associated with the key.
        let newValue =
            match map.TryFind key with
            | None -> [target]
            | Some oldValue -> target :: oldValue
        map.Add (key, newValue)
    )

/// For each input port in each component, map it to its port number.
let private mapInputPortIdToPortNumber
        (components : Component list)
        : Map<InputPortId, InputPortNumber> =
    (Map.empty, components) ||> List.fold (fun map comp ->
        (map, comp.InputPorts) ||> List.fold (fun map port ->
            map.Add (InputPortId port.Id,
                     InputPortNumber (getPortNumberOrFail port.PortNumber))
        )
    )

/// Build a simulation component.
let private buildSimulationComponent
        (sourceToTargetPort : Map<OutputPortId, (ComponentId * InputPortId) list>)
        (portIdToPortNumber : Map<InputPortId, InputPortNumber>)
        (comp : Component)
        : SimulationComponent =
    // Remove portIds and use portNumbers instead.
    let mapPortIdsToPortNumbers
            (targets : (ComponentId * InputPortId) list)
            : (ComponentId * InputPortNumber) list =
        targets |> List.map (fun (compId, portId) ->
            match portIdToPortNumber.TryFind <| portId with
            | None -> failwithf "what? Input port with portId %A has no portNumber associated" portId
            | Some portNumber -> compId, portNumber
        )
    // For each output port, find out which other components and ports are
    // connected to it.
    let outputs =
        comp.OutputPorts
        |> List.map (fun port ->
            match sourceToTargetPort.TryFind <| OutputPortId port.Id with
            | None -> failwithf "what? Unconnected output port %s in comp %s" port.Id comp.Id
            | Some targets -> OutputPortNumber (getPortNumberOrFail port.PortNumber),
                              mapPortIdsToPortNumbers targets
        )
        |> Map.ofList
    {
        Id = ComponentId comp.Id
        Type = comp.Type
        Label = comp.Label
        Inputs = Map.empty // The inputs will be set during the simulation.
        Outputs = outputs
        Reducer = getReducer comp.Type
    }

/// Transforms a canvas state into a simulation graph.
let private buildSimulationGraph (canvasState : CanvasState) : SimulationGraph =
    let components, connections = canvasState
    let sourceToTargetPort = buildSourceToTargetPortMap connections
    let portIdToPortNumber = mapInputPortIdToPortNumber components
    let mapper = buildSimulationComponent sourceToTargetPort portIdToPortNumber
    components
    |> List.map (fun comp -> ComponentId comp.Id, mapper comp)
    |> Map.ofList

//================//
// Run simulation //
//================//

// During simulation, a Component Reducer function will produce the output only
// when all of the expected inputs have a value. Once this happens, it will
// calculate its outputs and set them in the next simulationComponent(s).

/// Take the Input, and feed it to the Component with the specified Id.
/// If the Component is then ready to produce an output, propagate this output
/// by recursively feeding it as an input to the connected Components.
let rec private feedInput
        (graph : SimulationGraph)
        (compId : ComponentId)
        (input : InputPortNumber * Bit)
        : SimulationGraph =
    // Extract component.
    let comp = match graph.TryFind compId with
               | None -> failwithf "what? Could not find component %A in simulation step" compId
               | Some c -> c
    // Add input to the simulation component.
    let comp = { comp with Inputs = comp.Inputs.Add input }
    let graph = graph.Add (comp.Id, comp)
    // Try to reduce the component.
    match comp.Reducer comp.Inputs with
    | None -> graph // Keep on waiting for more inputs.
    | Some outputMap ->
        // Received enough inputs and produced an output.
        // Propagate each output produced to all the ports connected.
        (graph, outputMap) ||> Map.fold (fun graph outPortNumber bit ->
            match comp.Outputs.TryFind outPortNumber with
            | None -> failwithf "what? Reducer produced inexistent output portNumber %A in component %A" outPortNumber comp
            | Some targets ->
                // Trigger simulation step with the newly produced input in
                // every target.
                (graph, targets) ||> List.fold (fun graph (nextCompId, nextPortNumber) ->
                    feedInput graph nextCompId (nextPortNumber, bit)
                )
        )

/// Feed zero to a simulation input.
/// This function is supposed to be used with Components of type Input.
let feedSimulationInput graph inputId bit =
    feedInput graph inputId (InputPortNumber 0, bit)

/// Feed zeros to all simulation inputs.
let private simulateWithAllInputsToZero
        (inputIds : SimulationIO list)
        (graph : SimulationGraph)
        : SimulationGraph =
    (graph, inputIds) ||> List.fold (fun graph (inputId, _) ->
        feedSimulationInput graph inputId Zero
    )

/// Given a list of IO nodes (i.e. Inputs or outputs) extract their value.
/// If they dont all have a value, an error is thrown.
let extractSimulationIOs
        (simulationIOs : SimulationIO list)
        (graph : SimulationGraph)
        : (SimulationIO * Bit) list =
    let extractBit (inputs : Map<InputPortNumber, Bit>) : Bit =
        match inputs.TryFind <| InputPortNumber 0 with
        | None -> failwith "what? IO bit not set"
        | Some bit -> bit
    ([], simulationIOs) ||> List.fold (fun result (ioId, ioLabel) ->
        match graph.TryFind ioId with
        | None -> failwithf "what? Could not find io node: %A" (ioId, ioLabel)
        | Some comp -> ((ioId, ioLabel), extractBit comp.Inputs) :: result
    )

/// Get the ComponentIds and ComponentLabels of all input and output nodes.
let getSimulationIOs
        (components : Component list)
        : SimulationIO list * SimulationIO list =
    (([], []), components) ||> List.fold (fun (inputIds, outputIds) comp ->
        match comp.Type with
        | Input  -> ((ComponentId comp.Id, ComponentLabel comp.Label) :: inputIds, outputIds)
        | Output -> (inputIds, (ComponentId comp.Id, ComponentLabel comp.Label) :: outputIds)
        | _ -> (inputIds, outputIds)
    )

/// Validate a diagram and generate its simulation graph.
let private runChecksAndBuildGraph
        (canvasState : CanvasState)
        : Result<SimulationGraph, SimulationError> =
    match checkPortTypesAreConsistent canvasState,
          checkPortsAreConnectedProperly canvasState with
    | Some err, _ | _, Some err -> Error err
    | None, None ->
        let _, connections = canvasState
        let graph = canvasState |> buildSimulationGraph
        match analyseGraph graph connections with
        | Some err -> Error err
        | None -> Ok graph

//====================//
// Merge dependencies //
//====================//

/// Map a dependency name to its simulation graph.
type private DependencyMap = Map<string, SimulationGraph>

/// Validate and get simulation graph for all loaded dependencies.
let private buildDependencyMap
        (loadedDependencies : LoadedComponent list)
        : Result<DependencyMap, SimulationError> =
    let dependenciesRes =
        loadedDependencies
        |> List.map (fun dep -> dep.Name, runChecksAndBuildGraph dep.CanvasState)
    // Check if any dependency has given an error.
    let hasError (name, res) = match res with | Error _ -> true | Ok _ -> false
    let extractOk (name, res) = match res with | Ok d -> name, d | Error e -> failwithf "what? Dependency %s expected to be Ok, but has error %A" name e
    match List.tryFind hasError dependenciesRes with
    | Some (name, Error err) -> Error err // TODO: augument error saying that it happened in a dependency, so no affected components or connections will be highlighted.
    | None ->
        // All dependencies are Ok.
        // Create a map from their name to their simulation graph.
        dependenciesRes |> List.map extractOk |> Map.ofList |> Ok
    | _ -> failwith "what? Impossible case in buildDependencyMap"


// Outer diagram will have connections to ports of the dependency.
// This connections will target a portId. This portId can be used to find label
// and portNumber on the custom component.
// The label can then be matched to the corresponding input node in the
// dependency, which is a SimulationComponent with some Outputs.
// At this point, we just need to find all the connections in the outer diagram
// that target that specific port of the custom component, and replace them with
// the Outputs of the input node in the dependency.

// Something similar has to be done for the outputs of the custom component.

// Then we need to add all the components of the custom component into the outer
// simulation graph (map). Note that we need to make sure the ids are unique if
// a custom component is used multiple times.

// >> Maybe easier to:
// create mapping: old custom port -> new port in dependency
// add all dependency components in the graph, with their unique ids
// replace the outputs connecting to the old custom components

/// Lookup the simulation graph of a dependency in the dependency map, and
/// append the uniqueId to all ComponentIds (so that the graph is unique).
let private getUniqueGraph
        (dependencyMap : DependencyMap)
        (dependencyName : string)
        (uniqueInt : int)
        : SimulationGraph =
    let appendUniqueInt (ComponentId compId) : ComponentId =
        ComponentId <| sprintf "%s-unique-%d" compId uniqueInt
    let makeUniqueOutputs
            (oldOutputs : Map<OutputPortNumber, (ComponentId * InputPortNumber) list>)
            : Map<OutputPortNumber, (ComponentId * InputPortNumber) list> =
        oldOutputs
        |> Map.map (fun _ lst ->
            lst
            |> List.map (fun (compId, pNumber) -> appendUniqueInt compId, pNumber)
        )
    match dependencyMap.TryFind dependencyName with
    | None -> failwithf "what? Dependencey %s not found in dependencyMap when merging" dependencyName
    | Some depGraph ->
        // Copy the map into a new one with all unique Ids.
        // The only parts of the component that require changes are the Id and
        // the Outputs (they are the only the contains ComponentIds).
        (Map.empty, depGraph) ||> Map.fold (fun uniqueGraph compId comp ->
            uniqueGraph.Add(
                appendUniqueInt compId,
                { comp with Id = appendUniqueInt compId;
                            Outputs = makeUniqueOutputs comp.Outputs }
            )
        )

/// Given a custom component (a single component in the outer diagram), its
/// corresponding simulationGraph and its list of inputs labels; create a
/// mapping from the ports of the custom component in the outer diagram to the
/// ports in the components of the simulationGraph.
/// In essence, create a mapping to connect the two graphs.
let private mapCustomInputPorts
        (customComp : SimulationComponent)
        (customGraph : SimulationGraph)
        (inputLabels : string list)
        : Map<ComponentId * InputPortNumber, (ComponentId * InputPortNumber) list> =
    // For every input in customComp:
    // 1. get customComp.InputPortNumber
    // 2. get label from label arrays at that idx
    // 3. search label among Input nodes in customGraph
    // 4. take the (ComponentId * InputPortNumber) list of the Outputs of the
    //    Input node that matches that label.
    let extractOutputsFromInputNode (inputNode : SimulationComponent) =
        // TODO: assert inputNode.Type = Input
        match inputNode.Outputs.TryFind <| OutputPortNumber 0 with
        | None -> failwithf "what? extractOnlyOuputFromInputNode called with node without outputPortNumber 0: %A" inputNode
        | Some outputs -> outputs
    let getOutputsForInputNodeWithLabel (label : string) : (ComponentId * InputPortNumber) list =
        let chooser compId comp =
            if comp.Type = Input && comp.Label = label
            then Some <| extractOutputsFromInputNode comp
            else None
        // TODO: I have the feeling Map.tryPick has linear complexity. Maybe
        // premapping all these information can improve performance.
        match Map.tryPick chooser customGraph with
        | None -> failwithf "what? Input node with label %s could not be found in customGraph" label
        | Some outputs -> outputs

    (Map.empty, customComp.Inputs)
    ||> Map.fold (fun mappings (InputPortNumber inputPortNumber) _ ->
        // Step 2.
        let portLabel = inputLabels.[inputPortNumber]
        // Step 3 and 4.
        mappings.Add(
            (customComp.Id, InputPortNumber inputPortNumber),
            getOutputsForInputNodeWithLabel portLabel
        )
    )

/// Simular to mapCustomInputPorts but for the output ports of a custom
/// component.
let private mapCustomOutputPorts
        (customComp : SimulationComponent)
        (customGraph : SimulationGraph)
        (outputLabels : string list)
        : Map<ComponentId * InputPortNumber, (ComponentId * InputPortNumber) list> =
    // For every output in customComp:
    // 1. get customComp.OutputPortNumber
    // 2. get label from label arrays at that idx
    // 3. search label among Output nodes in customGraph and extract its ComponentId
    // 4. create a mapping from that Output the list of componentId and
    //    InputPortNumber of the outer diagram
    let getOutputNodeComponetnId (label : string) : ComponentId =
        let chooser compId comp =
            if comp.Type = Output && comp.Label = label
            then Some <| comp.Id
            else None
        // TODO: I have the feeling Map.tryPick has linear complexity. Maybe
        // premapping all these information can improve performance.
        match Map.tryPick chooser customGraph with
        | None -> failwithf "what? Output node with label %s could not be found in customGraph" label
        | Some outputNodeId -> outputNodeId

    (Map.empty, customComp.Outputs)
    ||> Map.fold (fun mappings (OutputPortNumber outputPortNumber) targets ->
        // Step 2.
        let portLabel = outputLabels.[outputPortNumber]
        // Step 3.
        let outputComponentId = getOutputNodeComponetnId portLabel
        // Step 4.
        mappings.Add((outputComponentId, InputPortNumber 0), targets)
    )

/// For every component that was targeting a custom component in the outer
/// diagram, change the outputs that were targeting the custom component with
/// new outputs that target components in the custom simulationDiagram.
let private applyCustomComponentMappings
        (currGraph : SimulationGraph)
        (portMappings : Map<ComponentId * InputPortNumber, (ComponentId * InputPortNumber) list>)
        : SimulationGraph =
    currGraph |> Map.map (fun compId comp ->
        let newOutputs =
            comp.Outputs |> Map.map (fun outPortNumber targets ->
                targets |> List.collect (fun target ->
                    match portMappings.TryFind target with
                    | None -> [target]
                    | Some newTargets -> newTargets 
                )
            )
        { comp with Outputs = newOutputs }
    )

/// Join two maps.
let private joinMaps (map1 : Map<'K,'V>) (map2 : Map<'K,'V>) : Map<'K,'V> =
    Map.fold (fun joined key value -> joined.Add(key, value)) map1 map2

/// Top down merger called by mergeDependencies. Return the merged graph and the
/// a new unique integer.
let rec private merger
        (currGraph : SimulationGraph)
        (dependencyMap : DependencyMap)
        (uniqueInt : int)
        : SimulationGraph * int =
    // Search the current graph for any Custom component that requires merging.
    // When a custom component is found:
    // 1. lookup the dependency map for the simulation graph of that custom component
    // 2. make all components id for that graph unique (append the same integer to all)
    // 3. recur the merger on all custom components of tne new unique graph
    // 4. add all components of the simulation graph to the current graph
    // 5. create mapping between ports of the outer simulation graph to ports of the inner
    // 6. remove the custom component
    //
    // (outside the fold)
    // 7. Now we have a new graph, with some connections pointing to elements that
    //    do not exist anymore. Apply all the port mapping the new graph and return.
    let currGraphCopy = currGraph
    let currGraph, mappings, uniqueInt =
        ((currGraph, Map.empty, uniqueInt), currGraphCopy)
        ||> Map.fold (fun (currGraph, portMappings, uniqueInt) compId comp ->
            match comp.Type with
            | Custom custom ->
                // Steps 1 and 2.
                let customGraph = getUniqueGraph dependencyMap custom.Name uniqueInt
                // Step 3.
                let customGraph, uniqueInt = merger customGraph dependencyMap uniqueInt
                // Step 4.
                let joinedGraph = joinMaps currGraph customGraph
                // Step 5.
                // TODO do something similar for the outputs.
                let newInpPortMappings = mapCustomInputPorts comp customGraph custom.InputLabels
                let newOutPortMappings = mapCustomOutputPorts comp customGraph custom.OutputLabels
                let portMappings =
                    joinMaps newInpPortMappings newOutPortMappings
                    |> joinMaps portMappings
                log newInpPortMappings
                log newOutPortMappings
                // Step 6.
                let joinedGraph = joinedGraph.Remove(compId)

                joinedGraph, portMappings, uniqueInt
            | _ -> (currGraph, portMappings, uniqueInt) // Ignore non-custom components.
        )
    // Step 7.
    let currGraph = applyCustomComponentMappings currGraph mappings
    currGraph, uniqueInt

/// Try to resolve all the dependencies in a graph, creating a single big graph.
/// For example, if the graph of an ALU refers to custom component such as
/// adders, replace them with the actual simulation graph for the adders.
let private mergeDependencies
        (graph : SimulationGraph)
        (loadedDependencies : LoadedComponent list)
        : Result<SimulationGraph, SimulationError> =
    match buildDependencyMap loadedDependencies with
    | Error e -> Error e
    | Ok dependencyMap ->
        // Recursively replace the dependencies, in a top down fashion.
        let mergedGraph, _ = merger graph dependencyMap 0
        Ok mergedGraph

/// Builds the graph and simulates it with all inputs zeroed.
let prepareSimulation
        (canvasState : CanvasState)
        (loadedDependencies : LoadedComponent list)
        : Result<SimulationData, SimulationError> =
    match checkPortTypesAreConsistent canvasState,
          checkPortsAreConnectedProperly canvasState with
    | Some err, _ | _, Some err -> Error err
    | None, None ->
        // All ports tests have been passed.
        let components, connections = canvasState
        let inputs, outputs = getSimulationIOs components
        let graph = canvasState |> buildSimulationGraph
        match analyseGraph graph connections with
        | Some err -> Error err
        | None ->
            // The current diagram is fine.
            // Add eventual dependency.
            match mergeDependencies graph loadedDependencies with
            | Error e -> Error e
            | Ok graph -> Ok {
                Graph = graph |> simulateWithAllInputsToZero inputs;
                Inputs = inputs;
                Outputs = outputs }
