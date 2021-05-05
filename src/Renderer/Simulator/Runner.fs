(*
    Runner.fs

    This module collects functions that allow to feed input into the simulation,
    effectively allowing to run it.
*)

module SimulationRunner

open CommonTypes
open Helpers
open SimulatorTypes
open SynchronousUtils

let mutable simTrace = None //Some ["adder40";"4bitbusmux20";"dff430"]



// During simulation, a Component Reducer function will produce the output only
// when all of the expected inputs have a value. Once this happens, it will
// calculate its outputs and set them in the next simulationComponent(s).

/// Function to determine what reducer inputs or outputs have changed.
let diffReducerInputsOrOutputs
        (newIO : Map<'a, WireData>)
        (oldIO : Map<'a, WireData>)
        : Map<'a, WireData> =
    // 'a type is either InputPortNumber or OutputPortNumber.
    // New inputs/outputs either:
    // - have more keys than old ones,
    // - have the same keys as old ones, but their values have changed.
    assertThat (oldIO.Count <= newIO.Count) (sprintf "diffReducerInputsOrOutputs: (%A:%A)" oldIO newIO)
    (Map.empty, newIO)
    ||> Map.fold (fun diff portNumber wireData ->
        match oldIO.TryFind portNumber with
        | None -> diff.Add(portNumber, wireData)
        | Some oldData when oldData <> wireData -> diff.Add(portNumber, wireData)
        | Some oldData when oldData = wireData -> diff
        | _ -> failwith "what? Impossible case in diffReducerInputsOrOutputs"
    )

let traceReduction action (comp:SimulationComponent) (reducerInput:ReducerInput) (reducerOutput:ReducerOutput) =
    match simTrace, comp with
    | None,_ -> ()
    | Some traceLabs, {Label=ComponentLabel lab} when List.contains lab traceLabs ->
        printfn "\n%s>>>>> %A \n\toutputs: %A \n\tinputs=%A \n\tnewState=%A" 
            action (shortPSComp comp) reducerOutput.Outputs reducerInput.Inputs    reducerOutput.NewState
    | _ -> ()




/// Take the Input, and feed it to the Component with the specified Id.
/// This function should be used to feed combinational logic inputs, not to
/// trigger a clock tick.
/// If the Component is then ready to produce an output, propagate this output
/// by recursively feeding it as an input to the connected Components.
let rec private feedInput
        (graph : SimulationGraph)
        (compId : ComponentId)
        (input : InputPortNumber * WireData)
        : SimulationGraph =
    // Extract component.
    let comp = match graph.TryFind compId with
               | None -> failwithf "what? Could not find component %A in simulationStep" compId
               | Some c -> c
    // Performance optimization: feed the reducer with the old inputs, to figure
    // out what the old ouputs were. Use these old outputs to determine what
    // outputs have changed by the new input, and only propagate those changes.
    // An extra call to the reducer should be quite cheap compared to many saved
    // recursions of the feedInput function.
    let oldReducerInput = {
        Inputs = comp.Inputs
        CustomSimulationGraph = comp.CustomSimulationGraph
        IsClockTick = No
    }
    let oldReducerOutput = comp.Reducer oldReducerInput

    // Add input to the simulation component.
    let comp = { comp with Inputs = comp.Inputs.Add input }
    let graph = graph.Add (comp.Id, comp)
    // Prepare reducer Input.
    let reducerInput = {
        Inputs = comp.Inputs
        CustomSimulationGraph = comp.CustomSimulationGraph
        IsClockTick = No
    }
    // Try to reduce the component.
    let reducerOutput = comp.Reducer reducerInput
    // Check wether the reducer produced any outputs.

    traceReduction "comb-feedin" comp reducerInput reducerOutput

    match reducerOutput.Outputs with
    | None -> graph // Keep on waiting for more inputs.
    | Some outputMap ->
        // Received enough inputs and produced an output.

        // Performance optimization, only propagate the outputs that changed.
        let oldOutputMap = Option.defaultValue Map.empty oldReducerOutput.Outputs
        let diffedOutputMap = diffReducerInputsOrOutputs outputMap oldOutputMap

        // Propagate each output produced.
        let graph = feedReducerOutput comp graph diffedOutputMap
        // Update the CustomSimulationGraph and return the new simulation graph.
        let comp = { comp with CustomSimulationGraph = reducerOutput.NewCustomSimulationGraph }
        graph.Add (comp.Id, comp)

/// Propagate each output produced by a simulation component to all the
/// components connected to its output ports.
/// Return the updated simulationGraph.
and private feedReducerOutput
        (comp : SimulationComponent)
        (graph : SimulationGraph)
        (outputMap : Map<OutputPortNumber, WireData>)
        : SimulationGraph  =
    (graph, outputMap) ||> Map.fold (fun graph ((OutputPortNumber opNum) as outPortNumber) wireData ->
        match Map.tryFind (OutputPortNumber opNum) comp.Outputs with
        | None when comp.Type = IOLabel -> graph // special case, these components can generate output that is connected to nothing!
        | None -> failwithf "what? Reducer produced inexistent output portNumber %A in component %A" outPortNumber comp
        | Some targets ->
            comp.OutputsPropagated.[opNum] <- true // disable further propagation if clocked.
            // Trigger simulation step with the newly produced input in
            // every target.
            (graph, targets) ||> List.fold (fun graph (nextCompId, nextPortNumber) ->
                let graph = feedInput graph nextCompId (nextPortNumber, wireData)
                graph
            )
    )


let clockedComps (graph:SimulationGraph) =
    graph 
    |> Map.toArray
    |> Array.filter (fun (_,comp) -> couldBeSynchronousComponent comp.Type)
    |> Array.sortBy (fun (cid,comp) -> match comp.Type with Custom _ -> 0 | _ -> 1)

let calculateStateChanges (graph : SimulationGraph) : SimulationGraph * OutputChange list =
    let clockedCompsBeforeTick = clockedComps graph
    // For each clocked component, feed the clock tick together with the inputs
    // snapshotted just before the clock tick.
    ((graph,[]), clockedCompsBeforeTick) ||> Array.fold (fun (graph,changes) (compId,comp) ->
        let reducerInput = {
            Inputs = comp.Inputs
            CustomSimulationGraph = comp.CustomSimulationGraph
            IsClockTick = Yes comp.State
        }
        // comp>Reducer recursively calls feedClockTick (and hence calculateStateChanges) if comp is a custom component
        // in that case the recursive call does all the result of internal state change
        // subgraph update for the Tick - propagateStateChanges will not do that
        // The change in the subgraph is Ok here because it does not affect the evaluation of any other components at this
        // level until the output changes are propagated.
        // Note that the subgraphs may still need later changes as a result of the delayed processing of higher-level
        // output changes. This change propagation must go through the whole design including subsheets again!
        let reducerTickOutput = comp.Reducer reducerInput 
        traceReduction (sprintf "clockTick %A" reducerInput.IsClockTick)  comp reducerInput reducerTickOutput

        match reducerTickOutput.Outputs with
        | None -> failwithf "what? A clocked component should ALWAYS produce outputs after a clock tick: %A" comp
        | Some outputMap ->
            // Note that updating the CustomSimulationGraph is necessary since
            // we may be dealing with custom clocked components, which means
            // the feedClockTick operaion changes the graph of that custom
            // component.
            let comp = 
                { comp with 
                    CustomSimulationGraph = reducerTickOutput.NewCustomSimulationGraph
                    OutputsPropagated = Array.replicate comp.Outputs.Count false
                    State = reducerTickOutput.NewState 
                }
            let change = {CComp = comp; COutputs = outputMap}
            Map.add comp.Id comp graph, (change :: changes)
            // Feed the newly produced outputs into the combinational logic.
    )

let propagateStateChanges (graph : SimulationGraph) (changes: OutputChange list) : SimulationGraph =
    // For each output change recorded in changes, update graph as follows
    // Update the inputs driven by the changed wires
    // Recursively propagate changes through graph
    // record outputs changed and do not propagate chnages that have already touched as result of propagation
    (graph, changes) ||> List.fold (fun graph change ->
        let comp = change.CComp
        let outputMap = 
            change.COutputs // if output has already been propagated don't propagate it here
            |> Map.filter (fun (OutputPortNumber n) wData ->  
                not <| comp.OutputsPropagated.[n])
        if simTrace <> None then
            printfn "|prop|----> %A (%A)" comp.Label outputMap
        // Note that here we update component inputs in the graph as we propagate changes.
        // component inputs in comp are not uptodate, but this does not matter, they are not used
        let graph = feedReducerOutput comp graph outputMap
        graph
    )

let checkPropagation (graph : SimulationGraph) : SimulationGraph =
    let ll (ComponentLabel s) = s
    let customComps =
        graph
        |> Map.toList
        |> List.collect (fun (cid,sc) -> 
            match sc.Type, sc.CustomSimulationGraph with 
            | Custom c, Some cGraph -> [{|Comp=sc;Type=c;Graph=cGraph|}]
            |_ -> [])

    let findOutputComp label (cGraph: SimulationGraph) =
        cGraph
        |> Map.filter (fun cid comp -> (match comp.Type with Output _ -> true | _ -> false) && comp.Label=label)
        |> Map.toList
        |> (function | [comp] -> snd comp 
                     | x -> failwithf "can't find output '%s' in graph: %d candidates: %A." (ll label) x.Length x)
    let propagateCombinationalComponents graph =
        (graph,graph)
        ||> Map.fold (fun graph cid comp ->
            let comp = graph.[cid]
            match comp.Type, couldBeSynchronousComponent comp.Type with
            | _,true -> graph
            | Input _,_ | Constant1 _, _-> graph
            | _,false -> 
                let reducerInput = {
                    Inputs = comp.Inputs
                    CustomSimulationGraph = comp.CustomSimulationGraph
                    IsClockTick = No
                }
                // Try to reduce the component.
                let reducerOutput = comp.Reducer reducerInput
                // Check wether the reducer produced any outputs.

                traceReduction "final-feedin" comp reducerInput reducerOutput

                match reducerOutput.Outputs with
                | None -> graph // Keep on waiting for more inputs.
                | Some outputMap ->
                    //printfn "%s -> %A" (ll comp.Label) outputMap
                    // Received enough inputs and produced an output.                
                    feedReducerOutput  comp graph outputMap
        )
    let checkCustomOutputs()  =
        customComps
        |> List.map (fun cc ->
            let outLabs = cc.Type.OutputLabels
            outLabs
            |> List.mapi (fun i (label,width) ->
                let comp = findOutputComp (ComponentLabel label) cc.Graph
                let propVal = comp.Inputs.[InputPortNumber 0]
                let propToList = cc.Comp.Outputs.[OutputPortNumber i]
                propToList
                |> List.map (fun (cid, inPort) ->
                    let receiver = graph.[cid]
                    let recVal = receiver.Inputs.[inPort]
                    if propVal <> recVal then 
                        printfn "***CustomOutput: %s on %s fails to propagate to %s\n \
                            output=%A\n propagated input=%A" label (ll cc.Comp.Label) (ll receiver.Label) propVal recVal    
                        1
                    else 
                        //printfn "%s.%s -> %A" (ll cc.Comp.Label) label propVal
                        0
                    )   
                )                
            )
    checkCustomOutputs()
    |> List.sumBy (List.sumBy List.sum)
    |> (fun n -> 
        if n > 0 
        then failwithf "*****Failing with %d Propagation errors*****" n
        else 
            // printfn "Propagation checks OK for %d components and %d customs!" graph.Count customComps.Length)
            ())
    propagateCombinationalComponents graph

let feedClockTick (graph : SimulationGraph) : SimulationGraph =
    calculateStateChanges graph
    ||> propagateStateChanges
    |> checkPropagation

/// Feed zero to a simulation input.
/// This function is supposed to be used with Components of type Input.
let feedSimulationInput graph inputId wireData =
    feedInput graph inputId (InputPortNumber 0, wireData)

/// Feed in constant outputs so that they propagated to things connected to them
/// This function is supposed to be used with Components of type Constant
let rec feedSimulationConstants (graph:SimulationGraph) =
    let comps = 
        graph 
        |> Map.toList 
        |> List.map snd
    let getWireData (comp:SimulationComponent) =
        match comp.Type with 
        | Constant1 (w,c,_) -> NumberHelpers.convertIntToWireData w (int64 c) 
        | _ -> failwithf "What? Problem with non-constant component used in feedSimulationConstants"
    comps
    |> List.filter (fun c -> match c.Type with | Custom cComp -> true| Constant1 _ -> true | _ -> false)
    |> (fun cL ->
                let feedConstant (graph:SimulationGraph) (comp:SimulationComponent) =
                    // graph gets updates each iteration of fold, but cL (and hence comp) does not
                    // we need to extract the Id from comp and look up the fold updated version in graph
                    let comp = graph.[comp.Id] // refresh to get latest version of comp as updated by fold
                    match comp.Type with
                    | Constant1 _ -> 
                        feedReducerOutput comp graph (Map.ofList [OutputPortNumber 0, getWireData comp])
                    | Custom cComp -> 
                  
                        Option.map feedSimulationConstants comp.CustomSimulationGraph
                        |> (fun graphOpt -> {comp with CustomSimulationGraph = graphOpt})
                        |> (fun comp' -> Map.add comp.Id comp' graph)
                    | _ -> failwithf "What? other components are filtered out"
                (graph, cL) ||> List.fold feedConstant)

/// Feed zeros to all simulation inputs, and feed a single clock tick.
/// This way all combinational logic has been touched once and had produced its
/// outputs.
let InitialiseGraphWithZeros
        (inputIds : SimulationIO list)
        (graph : SimulationGraph)
        : SimulationGraph =
    // Feed a clock tick to initialize all of the nets that are after clocked
    // components, which cannot be initialised by just feeding the inputs.
    let graph = feedClockTick graph
    // Feed zero to all simulation inputs.
    (graph, inputIds) ||> List.fold (fun graph (inputId, _, width) ->
        let data = List.replicate width Zero
        feedSimulationInput graph inputId data
    )
    |> feedSimulationConstants 
    

/// Given a list of IO nodes (i.e. Inputs or outputs) extract their value.
/// If they dont all have a value, an error is thrown.
let extractSimulationIOs
        (simulationIOs : SimulationIO list)
        (graph : SimulationGraph)
        : (SimulationIO * WireData) list =
    let extractWireData (inputs : Map<InputPortNumber, WireData>) : WireData =
        match inputs.TryFind <| InputPortNumber 0 with
        | None -> failwith "what? IO bit not set"
        | Some bit -> bit
    ([], simulationIOs) ||> List.fold (fun result (ioId, ioLabel, width) ->
        match graph.TryFind ioId with
        | None -> failwithf "what? Could not find io node: %A" (ioId, ioLabel)
        | Some comp -> ((ioId, ioLabel, width), extractWireData comp.Inputs) :: result
    )

/// Simlar to extractSimulationIOs, but do not fail if a bit is not set, just
/// ignore it.
let extractIncompleteSimulationIOs
        (simulationIOs : SimulationIO list)
        (graph : SimulationGraph)
        : (SimulationIO * WireData) list =
    let extractWireData (inputs : Map<InputPortNumber, WireData>) : WireData option =
        inputs.TryFind <| InputPortNumber 0
    ([], simulationIOs) ||> List.fold (fun result (ioId, ioLabel, width) ->
        match graph.TryFind ioId with
        | None -> failwithf "what? Could not find io node: %A" (ioId, ioLabel, width)
        | Some comp -> match extractWireData comp.Inputs with
                       | None -> result
                       | Some wireData -> ((ioId, ioLabel, width), wireData) :: result
    )

/// Get ComponentIds, ComponentLabels and wire widths of all input and output
/// nodes.
let getSimulationIOs
        (components : Component list)
        : SimulationIO list * SimulationIO list =
    (([], []), components) ||> List.fold (fun (inputs, outputs) comp ->
        match comp.Type with
        | Input w  -> ((ComponentId comp.Id, ComponentLabel comp.Label, w) :: inputs, outputs)
        | Output w -> (inputs, (ComponentId comp.Id, ComponentLabel comp.Label, w) :: outputs)
        | _ -> (inputs, outputs)
    )

/// Get ComponentIds, ComponentLabels and wire widths of all input and output
/// nodes in a simulationGraph.
let getSimulationIOsFromGraph
        (graph : SimulationGraph)
        : SimulationIO list * SimulationIO list =
    (([], []), graph) ||> Map.fold (fun (inputs, outputs) compId comp ->
        match comp.Type with
        | Input w  -> ((comp.Id, comp.Label, w) :: inputs, outputs)
        | Output w -> (inputs, (comp.Id, comp.Label, w) :: outputs)
        | _ -> (inputs, outputs)
    )
