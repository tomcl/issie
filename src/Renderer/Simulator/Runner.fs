(*
    Runner.fs

    This module collects functions that allow to feed input into the simulation,
    effectively allowing to run it.
*)

module OldSimulationRunner

open CommonTypes
open Helpers
open SimulatorTypes
open OldSynchronousUtils

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
#if ASSERTS
    assertThat (oldIO.Count <= newIO.Count) (sprintf "diffReducerInputsOrOutputs: (%A:%A)" oldIO newIO)
#endif
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







let clockedComps (graph:SimulationGraph) =
    graph 
    |> Map.toArray
    |> Array.filter (fun (_,comp) -> couldBeSynchronousComponent comp.Type)
    |> Array.sortBy (fun (cid,comp) -> match comp.Type with Custom _ -> 0 | _ -> 1)


    

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
        | Input1 (w, _)  -> ((ComponentId comp.Id, ComponentLabel comp.Label, w) :: inputs, outputs)
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
        | Input1 (w, _)  -> ((comp.Id, comp.Label, w) :: inputs, outputs)
        | Output w -> (inputs, (comp.Id, comp.Label, w) :: outputs)
        | _ -> (inputs, outputs)
    )
