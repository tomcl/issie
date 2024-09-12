module GraphBuilder
(*
    GraphBuilder.fs

    This module collects functions to build a SimulationGraph, starting from
    a CanvasState. It also runs all the checks contained in Analyser to validate
    the graph is correct (and can be built in the first place).
*)
open Helpers
open NumberHelpers
open CommonTypes
open SimGraphTypes
open SimTypes
open CanvasStateAnalyser

/// Assert that the FData only contain a single bit, and return such bit.
let inline extractBit (fd_: FData) : uint32 =
    match fd_ with
    | Alg _ -> failwithf "Can't extract data from Algebra"
    | Data fd ->
#if ASSERTS
        assertThat (fd.Width = 1)
        <| sprintf "extractBit called with wireData: %A" fd
#endif
        match fd.Dat with
        | Word n -> n
        | BigWord _ -> failwithf "Can't extract 1 bit from BigWord data {wireData}"

let inline packBit (bit: uint32) : FData =
    if bit = 0u then
        Data { Dat = Word 0u; Width = 1 }
    else
        Data { Dat = Word 1u; Width = 1 }

/// This function should only be called on Component ports, never on Connection
/// ports: ports in Components should always have Some portNumber, ports in
/// Connections should have None.
let private getPortNumberOrFail port =
    match port with
    | None -> failwithf "what? Component ports should always have a portNumber"
    | Some p -> p

/// Extract the values of the inputs of a SimulationComponent.
/// If any of these inputs is missing, return None.
/// The values are returned in the the passed order. E.g. if portNumbers is
/// [0, 1, 2], the returned value will be [bit0, bit1, bit2].
let rec private getValuesForPorts
    (inputs: Map<InputPortNumber, WireData>)
    (portNumbers: InputPortNumber list)
    : (WireData list) option
    =
    match portNumbers with
    | [] -> Some []
    | portNumber :: portNumbers' ->
        match inputs.TryFind portNumber with
        | None -> None
        | Some wireData ->
            match getValuesForPorts inputs portNumbers' with
            | None -> None
            | Some values -> Some <| wireData :: values



/// Build a map that, for each source port in the connections, keeps track of
/// the ports it targets.
/// It makes no sense to extract the PortNumber in this function as it is always
/// set to None for ports in connections.
let private buildSourceToTargetPortMap
    (connections: Connection list)
    : Map<OutputPortId, (ComponentId * InputPortId) list>
    =
    (Map.empty, connections)
    ||> List.fold (fun map conn ->
        let key = OutputPortId conn.Source.Id
        let target = ComponentId conn.Target.HostId, InputPortId conn.Target.Id
        // Append the new target to the list associated with the key.
        let newValue =
            match map.TryFind key with
            | None -> [ target ]
            | Some oldValue -> target :: oldValue

        map.Add(key, newValue))

/// For each input port in each component, map it to its port number.
let private mapInputPortIdToPortNumber (components: Component list) : Map<InputPortId, InputPortNumber> =
    (Map.empty, components)
    ||> List.fold (fun map comp ->
        (map, comp.InputPorts)
        ||> List.fold (fun map port ->
            map.Add(InputPortId port.Id, InputPortNumber(getPortNumberOrFail port.PortNumber))))

/// Get the default state for a component.
/// Note that custom components are stateless, even though they may contain
/// stateful components. The state of such stateful components is maintained
/// in the CustomSimulationGraph.
/// ROMs are stateless (they are only defined by their initial content).
let private getDefaultState compType =
    match compType with
    | ROM _
    | RAM _
    | AsyncROM _ -> failwithf "What? Legacy RAM component types should never occur"
    | Input _ -> failwithf "Legacy Input component types should never occur"
    | Input1 _
    | Output _
    | IOLabel
    | NotConnected
    | BusSelection _
    | BusCompare _
    | BusCompare1 _
    | Not
    | GateN _
    | Mux2
    | Mux4
    | Mux8
    | Decode4
    | NbitSpreader _
    | Demux2
    | Demux4
    | Demux8
    | NbitsAdder _
    | NbitsOr _
    | NbitsXor _
    | NbitsAnd _
    | NbitsNot _
    | Custom _
    | MergeWires
    | MergeN _
    | SplitWire _
    | SplitN _
    | ROM1 _
    | Viewer _
    | NbitsAdderNoCin _
    | NbitsAdderNoCout _
    | NbitsAdderNoCinCout _
    | Shift _ -> NoState
    | Constant1 _
    | Constant _ -> NoState
    | AsyncROM1 _ -> NoState
    | DFF
    | DFFE -> DffState 0u
    | Register w
    | RegisterE w
    | Counter w
    | CounterNoEnable w
    | CounterNoLoad w
    | CounterNoEnableLoad w -> RegisterState <| convertIntToFastData w 0u
    | RAM1 memory
    | AsyncRAM1 memory -> RamState memory // The RamState content may change during the simulation.

let compType t =
    match t with
    | Custom c -> c.Name
    | _ -> t.ToString()

/// Build a simulation component.
let private buildSimulationComponent
    (sourceToTargetPort: Map<OutputPortId, (ComponentId * InputPortId) list>)
    (portIdToPortNumber: Map<InputPortId, InputPortNumber>)
    (comp: Component)
    (outputWidths: int array)
    : SimulationComponent
    =
    // Remove portIds and use portNumbers instead.
    let mapPortIdsToPortNumbers (targets: (ComponentId * InputPortId) list) : (ComponentId * InputPortNumber) list =
        targets
        |> List.map (fun (compId, portId) ->
            match portIdToPortNumber.TryFind <| portId with
            | None -> failwithf "what? Input port with portId %A has no portNumber associated" portId
            | Some portNumber -> compId, portNumber)
    // For each output port, find out which other components and ports are
    // connected to it.
    let outputs =
        comp.OutputPorts
        |> List.collect (fun port ->
            match sourceToTargetPort.TryFind <| OutputPortId port.Id with
            | None when comp.Type = IOLabel -> [] // IOLabels are allowed to be connected to nothing
            | None -> failwithf "what? Unconnected output port %s in comp %s" port.Id comp.Id
            | Some targets ->
                [ OutputPortNumber
                  <| getPortNumberOrFail port.PortNumber,
                  mapPortIdsToPortNumbers targets ])
        |> Map.ofList

    // The inputs will be set during the simulation, we just need to initialise
    // the ones for Output nodes, see below.
    let inputs =
        match comp.Type with
        | Output width ->
            // Initialise all outputs to zero. This is necessary because upon
            // starting the simulation we need to feed all zeros. To do so, we
            // need to feed all simulation inputs set to zero and a global clock
            // tick. The problem is that both operations expect all outputs to
            // be set as result, but they don't necessarily set all the outputs
            // themselves. Therefore there is not an order you can run them in
            // that will always work. Presetting the outputs solves the problem
            // and the value does not matter as all outputs will be set again
            // in that initialization process.
            Map.empty.Add(InputPortNumber 0, List.replicate width Zero)
        | _ -> Map.empty

    let outputWidths =
        match comp.Type with
        | Output width
        | Viewer width -> [| width |]
        | _ -> outputWidths

    { Id = ComponentId comp.Id
      Type = comp.Type
      Label = ComponentLabel comp.Label
      Inputs = inputs
      Outputs = outputs
      OutputWidths = outputWidths
      CustomSimulationGraph = None // Custom components will be augumented by the DependencyMerger.
    }

let getLabelConnections (comps: Component list) (conns: Connection list) =
    let labels = comps |> List.filter (fun co -> co.Type = IOLabel)

    let compIdMap =
        labels
        |> List.map (fun co -> ComponentId co.Id, co)
        |> Map.ofList

    let getComp n = compIdMap[n]

    let targetMap =
        conns
        |> List.map (fun conn -> ComponentId conn.Target.HostId, conn)
        |> Map.ofList

    let getConnection (compTarget: Component) = targetMap[ComponentId compTarget.Id]

    let copyConnection (conn: Connection) (compTarget: Component) (tagNum: int) =
        { conn with
            Target = compTarget.InputPorts[0]
            Id = sprintf "iolab%d" tagNum + conn.Id }

    let getDriverConnection (comps: Component list) =
        comps
        |> List.tryFind (fun co ->
            (Map.tryFind (ComponentId co.Id) targetMap)
            <> None)
        |> function
            | None -> failwithf "What? component cannot be found in %A" targetMap
            | Some comp -> targetMap[ComponentId comp.Id]

    labels
    |> List.groupBy (fun co -> co.Label)
    |> List.collect (fun (lab, lst) ->
        let dConn = getDriverConnection lst

        lst
        |> List.filter (fun co -> co.Id <> dConn.Target.HostId)
        |> List.indexed
        |> List.map (fun (i, co) -> copyConnection dConn co i))

/// Transforms a canvas state into a simulation graph.
let private buildSimulationGraph (canvasState: CanvasState) outputWidths : (SimulationGraph) =
    let components, connections' = canvasState
    let labConns = getLabelConnections components connections'
    let connections = labConns @ connections'
    let sourceToTargetPort = buildSourceToTargetPortMap connections
    let portIdToPortNumber: Map<InputPortId, InputPortNumber> =
        mapInputPortIdToPortNumber components
    let mapper = buildSimulationComponent sourceToTargetPort portIdToPortNumber

    let debugPrint() =
        printfn "Building Simulation Graph for Debug"
        List.map (fun (id, comp) ->
            printfn
                "Built sComp %A │ %-30A │ %-25A │ %A"
                comp.Id
                (match comp.Type with
                 | Custom c -> c.Name
                 | t -> sprintf "%A" t)
                comp.Label
                (comp.OutputWidths |> Array.toList)
            (id, comp))

    components
    |> List.map (fun comp ->
        // find output widths for this component
        let ws =
            outputWidths
            |> Array.filter (fun ((id, _), w) -> id = comp.Id)
            |> Array.map (fun ((_, pn), w) -> (pn, w))
            |> Array.sortBy fst
            |> Array.map snd
        ComponentId comp.Id, mapper comp ws)
    // |> debugPrint() // NOTE - for debugging only
    |> Map.ofList

// Find out width of outputs of components from ConnectionsWidth map. Map<ConnectionId, "Width" option> -> Map<(ComponentId * "PortNumber"), "Width">
let private findOutputWidths (canvasState: CanvasState) (connsWidth: ConnectionsWidth) =
    let comps, conns = canvasState

    // NOTE -get rid of Option as we know that all widths are valid at this point
    let connsWidth =
        connsWidth
        |> Map.toList
        |> List.map (fun (ConnectionId k, w) -> k, (w |> Option.get))

    let inline findComp (id: string) =
        comps
        |> List.tryFind (fun c -> c.Id = id)
        |> Option.get

    let inline findOutputNumber (comp: Component) portId =
        comp.OutputPorts
        |> List.tryFind (fun p -> p.Id = portId)
        |> Option.get
        |> fun p -> p.PortNumber
        |> Option.get

    let IOLabelsAsOutput =
        conns
        |> List.map (fun conn -> conn.Id, conn.Target.HostId)
        |> List.filter (fun (connId, compId) ->
            let comp = findComp compId
            match comp.Type with
            | IOLabel -> true
            | _ -> false)
        |> List.map (fun (connId, compId) ->
            connsWidth
            |> List.tryFind (fun (id, w) -> connId = id)
            |> function
                | Some(_, w) -> (compId, 0), w
                | None -> failwithf "what? connection %A not found" connId)

    connsWidth
    |> List.map (fun (id, w) ->
        match conns |> List.tryFind (fun conn -> conn.Id = id) with
        | None -> failwithf "what? connection %A not found" id
        | Some conn -> (conn, w))
    |> List.map (fun (conn, w) ->
        match conn.Source.PortNumber with
        | Some pn -> (conn.Source.HostId, pn), w
        | None ->
            let comp = findComp conn.Source.HostId
            let pn = findOutputNumber comp conn.Source.Id
            (conn.Source.HostId, pn), w)
    |> List.append IOLabelsAsOutput
    |> Map.ofList // keep unqiue outputs
    |> Map.toArray

/// Validate a diagram and generate its simulation graph.
let runCanvasStateChecksAndBuildGraph
    (canvasState: CanvasState)
    (loadedComponents: LoadedComponent list)
    : Result<SimulationGraph, SimulationError>
    =
    match analyseState canvasState loadedComponents with
    | Some err, _ -> Error err
    | None, Some connectionsWidth ->
        let outputsWidth = findOutputWidths canvasState connectionsWidth
        Ok
        <| buildSimulationGraph canvasState outputsWidth
    | None, None -> failwith "This should not happen, connections width should be available if no error was found."

let mutable simTrace = false 



/// Get ComponentIds, ComponentLabels and wire widths of all input and output
/// nodes.
let getSimulationIOs (components: Component list) : SimulationIO list * SimulationIO list =
    (([], []), components)
    ||> List.fold (fun (inputs, outputs) comp ->
        match comp.Type with
        | Input1(w, _) ->
            ((ComponentId comp.Id, ComponentLabel comp.Label, w)
             :: inputs,
             outputs)
        | Output w ->
            (inputs,
             (ComponentId comp.Id, ComponentLabel comp.Label, w)
             :: outputs)
        | _ -> (inputs, outputs))


