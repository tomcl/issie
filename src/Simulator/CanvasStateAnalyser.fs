(*
    CanvasStateAnalyser.fs

    This module collects a series of functions that perform checks on
    CanvasState and SimulationGraph.
*)

module CanvasStateAnalyser

open CommonTypes
open SimulatorTypes
open BusWidthInferer

// -- Checks performed
//
// Ports constraints:
// - Source ports must be output ports.
// - Target ports must be input ports.
// - All ports have at least one connection that touches them.
// - Input ports have precisely one connection that touches them.
// - Custom components must have I/Os consistent with the sheet that defines them.
//
// Input/Output components in a simulationgraph must all have unique labels.
//
// All wire widths are consistent (rely on WidthInferer.fs).

// IOLabel input and output ports are special.
// Input: each same label group must be driven just once.
// Output: relax normal restriction that all outputs must be connected

/// Return all the Ids of all input ports across all components.
/// Return also the ComponentId which may be used in error messages.

type MapData =
    {
         Connections : Connection list
         Components : Component list
         LabComp : Component list
         LabGroup : Map<string,Component list>
         LabInputPorts:  Port list
         LabOutputPorts: Port list
         LabTargetConns: Connection list
         OtherTargetConns: Connection list
         LabSourceConns: Connection list
         OtherSourceConns: Connection list
         OtherInputPorts: Port list
         OtherOutputPorts: Port list
         ToComp: Map<ComponentId,Component>
         ToInputPort: Map<InputPortId,Port>
         ToOutputPort: Map<OutputPortId, Port>
     }
let private getAllInputPortIds (components : Component list) : (InputPortId * ComponentId) list =
    components |> List.collect
        (fun comp -> comp.InputPorts |> List.map (fun port -> InputPortId port.Id, ComponentId comp.Id))

/// Return all the Ids of all ouput ports across all components.
/// Return also the ComponentId which may be used in error messages.
let private getAllOutputPortIds (components : Component list) : (OutputPortId * ComponentId) list =
    components |> List.collect
        (fun comp -> comp.OutputPorts |> List.map (fun port -> OutputPortId port.Id, ComponentId comp.Id))


/// maps for use in various places
let private genMaps ((comps,conns):CanvasState) = 

    
    let labComps:Component list = List.filter (fun co -> co.Type = IOLabel) comps

    let idToComp =
        comps
        |> List.map (fun co -> ComponentId co.Id, co)
        |> Map.ofList

    let targetIsLabel (c: Connection) = idToComp.[ComponentId c.Target.HostId].Type = IOLabel
    let sourceIsLabel (c: Connection) = idToComp.[ComponentId c.Source.HostId].Type = IOLabel
    let splitBy pred lst =
        (([],[]), lst) ||> List.fold (fun (is, isNot) x -> 
                if pred x then (x :: is, isNot) else (is,x::isNot))

    let labTargetConns,otherTargetConns = splitBy targetIsLabel conns
    let labSourceConns, otherSourceConns = splitBy sourceIsLabel conns

    let normalise (p:Port) = {p with PortNumber=None}
    let normaliseL (pL: Port list) = List.map normalise pL


    let idToInputPort = 
        comps 
        |> List.collect (fun co -> co.InputPorts)
        |> List.map (fun po -> InputPortId po.Id, normalise po)
        |> Map.ofList

    let idToOutputPort =
        comps 
        |> List.collect (fun co -> co.OutputPorts)
        |> List.map (fun po -> OutputPortId po.Id, normalise po)
        |> Map.ofList

    let otherInputPorts = 
        comps 
        |> List.filter (fun co -> co.Type <> IOLabel)
        |> List.collect (fun co -> normaliseL co.InputPorts)

    let otherOutputPorts = 
        comps
        |> List.filter (fun co -> co.Type <> IOLabel)
        |> List.collect (fun co -> normaliseL co.OutputPorts)

    let labGroup = 
        List.groupBy (fun (co:Component) -> co.Label) labComps
        |> Map.ofList

    let labInputPorts = 
        labComps 
        |> List.collect (fun co -> normaliseL co.InputPorts)
        
    let labOutputPorts =
        labComps 
        |> List.collect (fun co -> normaliseL co.OutputPorts)
    {
        Connections = conns
        Components = comps
        LabComp = labComps
        LabGroup = labGroup
        OtherInputPorts = otherInputPorts
        OtherOutputPorts = otherOutputPorts
        LabInputPorts = labInputPorts
        LabOutputPorts = labOutputPorts
        ToComp = idToComp
        ToInputPort = idToInputPort
        ToOutputPort = idToOutputPort
        LabTargetConns = labTargetConns
        LabSourceConns = labSourceConns
        OtherTargetConns = otherTargetConns
        OtherSourceConns = otherSourceConns
    }
    
    

/// Check that:
/// 1- all source ports in connections are Output ports,
/// 2- all target ports in connections are Input ports,
/// 3- all input ports in a component are actually input ports,
/// 4- all output ports in a component are actually output ports,
/// 5- all ports on components have a port number,
/// 6- all ports on connection do not have a port number.
/// These conditions should always hold, unless there are bugs in the code (i.e.
/// no user behaviour should be able to trigger such errors).
/// The costruction of the Simulation graph assumes that these rules hold.
/// TODO: should they crash the program then?
let private checkPortTypesAreConsistent (canvasState : CanvasState) : SimulationError option =
    let rec checkComponentPorts (ports : Port list) (correctType : PortType) =
        match ports with
        | [] -> None
        | port :: _ when port.PortNumber = None -> Some {
            Msg = sprintf "%A port appears to not have a port number" correctType
            InDependency = None
            ComponentsAffected = [ComponentId port.HostId]
            ConnectionsAffected = [] }
        | port :: _ when port.PortType <> correctType -> Some {
            Msg = sprintf "%A port %d appears to be an %A port" correctType (Option.get port.PortNumber) port.PortType
            InDependency = None
            ComponentsAffected = [ComponentId port.HostId]
            ConnectionsAffected = [] }
        | _ :: ports' ->
            checkComponentPorts ports' correctType
    /// Check conditions 3, 4, 5
    let rec checkComponentsPorts (components : Component list) =
        match components with
        | [] -> None
        | comp :: components' ->
            match checkComponentPorts comp.InputPorts PortType.Input,
                  checkComponentPorts comp.OutputPorts PortType.Output with
            | Some err, _ | _, Some err -> Some err
            | None, None -> checkComponentsPorts components' // Check next.

    let checkConnectionPort (port : Port) (correctType : PortType) (connId : string) =
        match port.PortType = correctType, port.PortNumber with
        | false, _ -> Some {
            Msg = sprintf "%A port appears to be an %A port" correctType port.PortType
            InDependency = None
            ComponentsAffected = [ComponentId port.HostId]
            ConnectionsAffected = [ConnectionId connId] }
        | _, Some pNumber -> Some {
            Msg = sprintf "%A port appears to have a port number: %d" correctType pNumber
            InDependency = None
            ComponentsAffected = [ComponentId port.HostId]
            ConnectionsAffected = [ConnectionId connId] }
        | true, None -> None // All right.
    /// Check conditions 1, 2, 6
    let rec checkConnectionsPorts (connections : Connection list) =
        match connections with
        | [] -> None
        | conn :: connections' ->
            match checkConnectionPort conn.Source PortType.Output conn.Id,
                  checkConnectionPort conn.Target PortType.Input conn.Id with
            | Some err, _ | _, Some err -> Some err
            | None, None -> checkConnectionsPorts connections' // Check next.

    let components, connections = canvasState
    match checkComponentsPorts components,
          checkConnectionsPorts connections with
    | Some err, _ | _, Some err -> Some err
    | None, None -> None // All right.

/// Apply condition on every element of the map (tailored to this specific
/// problem).
let private checkEvery
        (counts : List<(Component list*Connection  list)*int>) // 'a is either InputPortId or OutputPortId.
        (cond : int -> bool)
        errMsg
        : SimulationError option =
    (None, counts) ||> List.fold (fun maybeErr ((comps, conns), count)  ->
        match maybeErr with
        | Some err -> Some err
        | None ->
            match cond count with
            | true -> None
            | false -> Some {
                Msg = sprintf errMsg count 
                InDependency = None
                ComponentsAffected = comps |> List.map (fun comp -> ComponentId comp.Id)
                ConnectionsAffected = conns |> List.map (fun conn -> ConnectionId conn.Id)  }
    )


/// Count the number of connections that target each port or group of label input ports
let private countPortsConnections 
        (conns: Connection list)
        (connMap: Connection -> 'b)
        (bins: 'b list)
        (binMap: 'b -> Component list) =
    let rec countPortsConnections' (conns: (Connection list)) (counts : Map<'b, int*Connection list>) =
        match conns with
        | [] -> counts |> Map.toList |> List.map (fun (key, (count,conns)) -> (binMap key, conns),count)
        | conn :: conns' ->
            let countsRes =
                let key = connMap conn
                let binCount, binConns = counts.[key]
                counts.Add( key, (binCount + 1, conn :: binConns))
            countPortsConnections' conns' countsRes
    countPortsConnections' conns (bins |> List.map (fun b -> b,(0,[])) |> Map.ofList)



let private checkCounts (conns: Connection list) connMap bins binMap cond errMsg =
    let totals = countPortsConnections conns connMap bins binMap 
    checkEvery totals cond errMsg



/// Check that:
/// - any port has at least one connection,
/// - any input port has precisely one connection.
/// These conditions may not hold due to user errors.
let private checkPortsAreConnectedProperly 
        (canvasState : CanvasState) =
    let m = genMaps canvasState
    let conns = m.Connections
    let portMap (p:Port) = [m.ToComp.[ComponentId p.HostId]]
    let inPIdMap pid = m.ToInputPort.[InputPortId pid]
    let labMap (lab:string) = m.LabGroup.[lab]
    let l2Pid (lst: Port list) = lst |> List.map (fun x -> x.Id)

    [
        checkCounts m.OtherTargetConns (fun conn -> conn.Target.Id) (l2Pid m.OtherInputPorts) (inPIdMap >> portMap) ((=) 1) (
                "A wire must have precisely one driving component, but %d \
                were found. If you want to merge wires together use a MergeWires component")

        checkCounts m.LabTargetConns (fun conn -> m.ToComp.[ComponentId conn.Target.HostId].Label) (m.LabGroup |> Map.toList |> List.map fst)  labMap ((=) 1) (
                "A set of labelled wires must have precisely one driving component, but %d \
                were found. If you want to merge wires together use a MergeWires component. \
                If you are driving two labels from the same component delete one of them: \
                a set of labels with the same name are all connected together and only one \
                label in each set must be driven.")

        checkCounts m.OtherSourceConns (fun conn -> conn.Source) m.OtherOutputPorts portMap ((<) 0) (
                "Output ports must have at least one, not %d, connections. If the output \
                is meant to be disconnected you can add a wire label to stop this error")


    ] |> List.tryPick id


/// Input/Output components in a simulationgraph all have unique labels.
let private checkIOLabels (canvasState : CanvasState) : SimulationError option =
    let rec checkDuplicate (comps : Component list) (map : Map<string,string>) (ioType : string) =
        match comps with
        | [] -> None
        | comp :: comps' ->
            match map.TryFind comp.Label with
            | None -> checkDuplicate comps' map ioType
            | Some compId when compId = comp.Id -> checkDuplicate comps' map ioType
            | Some compId -> Some {
                Msg = sprintf "Two %s components cannot have the same label: %s." ioType comp.Label
                InDependency = None
                ComponentsAffected = [comp.Id; compId] |> List.map ComponentId
                ConnectionsAffected = []
            }
    let toMap (comps : Component list) =
        comps |> List.map (fun comp -> comp.Label, comp.Id) |> Map.ofList
    let components, _ = canvasState
    let inputs =
        components
        |> List.filter (fun comp -> match comp.Type with | Input _ -> true | _ -> false)
    let outputs =
        components
        |> List.filter (fun comp -> match comp.Type with | Output _ -> true | _ -> false)
    let labels =
        components
        |> List.filter (fun comp -> match comp.Type with | IOLabel _ -> true | _ -> false)

    match checkDuplicate inputs (toMap inputs) "Input",
          checkDuplicate outputs (toMap outputs) "Output" with
    | Some err, _| _, Some err -> Some err
    | None, None -> None

/// Checks that all connections have consistent widths.
/// This function relies on the bus inferer, but also makes sure that all widths
/// can be inferred.
let private checkConnectionsWidths
        (canvasState : CanvasState)
        : SimulationError option =
    let convertConnId (ConnectionId cId) = ConnectionId cId
    let convertError (err : BusTypes.WidthInferError) : SimulationError = {
        Msg = err.Msg
        InDependency = None
        ConnectionsAffected = err.ConnectionsAffected |> List.map convertConnId
        ComponentsAffected = []
    }
    match inferConnectionsWidth canvasState with
    | Error err -> Some <| convertError err
    | Ok connWidths ->
        let faulty = connWidths |> Map.filter (fun _ width -> Option.isNone width)
        match faulty.IsEmpty with
        | true -> None // All good.
        | _ -> Some {
            Msg = "Could not infer all connections widths."
            InDependency = None
            ConnectionsAffected =
                faulty |> Map.toList |> List.map (fun (cId, _) -> convertConnId cId)
            ComponentsAffected = []
        }

/// Analyse a CanvasState and return any error (or None).
let analyseState
        (state : CanvasState)
        : SimulationError option =
    [
        checkPortTypesAreConsistent state
        checkPortsAreConnectedProperly state
        checkIOLabels state
        checkConnectionsWidths state
    ]
    |> List.tryFind Option.isSome
    |> Option.flatten
