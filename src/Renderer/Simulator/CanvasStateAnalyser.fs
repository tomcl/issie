module CanvasStateAnalyser

(*
    CanvasStateAnalyser.fs

    This module collects a series of functions that perform checks on
    CanvasState and SimulationGraph.
*)


open CommonTypes
open SimGraphTypes
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
    { Connections: Connection list
      Components: Component list
      LabComp: Component list
      LabGroup: Map<string, Component list>
      LabInputPorts: Port list
      LabOutputPorts: Port list
      LabTargetConns: Connection list
      OtherTargetConns: Connection list
      LabSourceConns: Connection list
      OtherSourceConns: Connection list
      OtherInputPorts: Port list
      OtherOutputPorts: Port list
      ToComp: Map<ComponentId, Component>
      ToInputPort: Map<InputPortId, Port>
      ToOutputPort: Map<OutputPortId, Port> }

/// Input and Output names of the ports depending on their ComponentType
let portNames (componentType:ComponentType)  = //(input port names, output port names)
    match componentType with
    | Decode4 -> (["SEL";"DATA"],["0"; "1";"2"; "3"])
    | NbitsAdder _ -> (["CIN";"P";"Q"],["SUM "; "COUT"])
    | NbitsAdderNoCin _ -> (["P";"Q"],["SUM "; "COUT"])
    | NbitsAdderNoCinCout _ -> (["P";"Q"],["SUM "])
    | NbitsAdderNoCout _ -> (["CIN";"P";"Q"],["SUM "])
    | Register _ -> (["D"],["Q"])
    | RegisterE _ -> (["D"; "EN"],["Q"])
    | Counter _ -> (["D"; "LOAD"; "EN"],["Q"])
    | CounterNoEnable _ -> (["D"; "LOAD"],["Q"])
    | CounterNoLoad _ -> (["EN"],["Q"])
    | CounterNoEnableLoad _ ->  ([],["Q"])
    | ROM1 _ |AsyncROM1 _ -> (["ADDR"],["DOUT"])
    | RAM1 _ -> (["ADDR"; "DIN";"WEN" ],["DOUT"])
    | AsyncRAM1 _ -> (["ADDR"; "DIN";"WEN" ],["DOUT"])
    | DFF -> (["D"],["Q"])
    | DFFE -> (["D";"EN"],["Q"])
    | Mux2 -> (["0"; "1";"SEL"],["OUT"])
    | MergeN nInp -> (List.init nInp (fun i -> 
        match i with
        | n when n = 0 -> "LSB"
        | n when n = nInp-1 -> "MSB"
        | _ -> ""), ["OUT"])
    | SplitN (n, _, _) -> (["IN"], [])
    | Mux4 -> (["0"; "1"; "2"; "3" ;"SEL"],["OUT"])
    | Mux8 -> (["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7";"SEL"],["OUT"])
    | Demux2 -> (["DATA" ; "SEL"],["0"; "1"])
    | Demux4 -> (["DATA"; "SEL"],["0"; "1";"2"; "3";])
    | Demux8 -> (["DATA"; "SEL"],["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7"])
    | NbitsXor _ | NbitsAnd _ | NbitsOr _ -> (["P"; "Q"], ["OUT"])
    | NbitsNot _ -> (["IN"],["OUT"])
    | Shift _ -> (["IN" ; "SHIFTER"],["OUT"])
    | Custom x -> (List.map fst x.InputLabels), (List.map fst x.OutputLabels)
    | _ -> ([],[])
   // |Demux8 -> (["IN"; "SEL"],["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7"])
   // |_ -> ([],[])
   // EXTENSION: Extra Components made that are not currently in Issie. Can be extended later by using this code as it is .

let getPortName (comp:Component) (port: Port) =
    let pNames = portNames comp.Type
    let getIdx pList =
        List.tryFindIndex (fun (po: Port) -> po.Id = port.Id) pList
        |> function
            | Some i -> i
            | None -> failwith "getPortName: Port not contained in given list"
    match port.PortType with
    | PortType.Input ->
        List.tryItem (getIdx comp.InputPorts) (fst pNames)
    | PortType.Output ->
        let idx = List.tryFindIndex (fun (po: Port) -> po.Id = port.Id) comp.OutputPorts
        List.tryItem (getIdx comp.OutputPorts) (snd pNames)
    |> function
        | Some name -> name
        | None -> ""

let private getAllInputPortIds (components: Component list) : (InputPortId * ComponentId) list =
    components
    |> List.collect (fun comp ->
        comp.InputPorts
        |> List.map (fun port -> InputPortId port.Id, ComponentId comp.Id))

/// Return all the Ids of all ouput ports across all components.
/// Return also the ComponentId which may be used in error messages.
let private getAllOutputPortIds (components: Component list) : (OutputPortId * ComponentId) list =
    components
    |> List.collect (fun comp ->
        comp.OutputPorts
        |> List.map (fun port -> OutputPortId port.Id, ComponentId comp.Id))

/// maps for use in various places
let private genMaps ((comps, conns): CanvasState) =

    let labComps: Component list = List.filter (fun co -> co.Type = IOLabel) comps

    let idToComp =
        comps
        |> List.map (fun co -> ComponentId co.Id, co)
        |> Map.ofList

    let targetIsLabel (c: Connection) =
        idToComp[ComponentId c.Target.HostId].Type = IOLabel

    let sourceIsLabel (c: Connection) =
        idToComp[ComponentId c.Source.HostId].Type = IOLabel

    let splitBy pred lst =
        (([], []), lst)
        ||> List.fold (fun (is, isNot) x ->
            if pred x then
                (x :: is, isNot)
            else
                (is, x :: isNot))

    let labTargetConns, otherTargetConns = splitBy targetIsLabel conns
    let labSourceConns, otherSourceConns = splitBy sourceIsLabel conns

    let normalise (p: Port) = { p with PortNumber = None }
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
        List.groupBy (fun (co: Component) -> co.Label) labComps
        |> Map.ofList

    let labInputPorts =
        labComps
        |> List.collect (fun co -> normaliseL co.InputPorts)

    let labOutputPorts =
        labComps
        |> List.collect (fun co -> normaliseL co.OutputPorts)

    { Connections = conns
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
      OtherSourceConns = otherSourceConns }

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
let private checkPortTypesAreConsistent (canvasState: CanvasState) : SimulationError option =
    let rec checkComponentPorts (ports: Port list) (correctType: PortType) =
        match ports with
        | [] -> None
        | port :: _ when port.PortNumber = None ->
            Some
                { ErrType = PortNumMissing correctType
                  InDependency = None
                  ComponentsAffected = [ ComponentId port.HostId ]
                  ConnectionsAffected = [] }
        | port :: _ when port.PortType <> correctType ->
            Some 
                { ErrType = WrongPortType (correctType, port)
                  InDependency = None
                  ComponentsAffected = [ ComponentId port.HostId ]
                  ConnectionsAffected = [] }
        | _ :: ports' -> checkComponentPorts ports' correctType

    /// Check conditions 3, 4, 5
    let rec checkComponentsPorts (components: Component list) =
        match components with
        | [] -> None
        | comp :: components' ->
            match
                checkComponentPorts comp.InputPorts PortType.Input, checkComponentPorts comp.OutputPorts PortType.Output
            with
            | Some err, _
            | _, Some err -> Some err
            | None, None -> checkComponentsPorts components' // Check next.

    let checkConnectionPort (port: Port) (correctType: PortType) (connId: string) =
        match port.PortType = correctType, port.PortNumber with
        | false, _ ->
            Some
                { ErrType = WrongPortType (correctType, port)
                  InDependency = None
                  ComponentsAffected = [ ComponentId port.HostId ]
                  ConnectionsAffected = [ ConnectionId connId ] }
        | _, Some portNumber ->
            Some
                { ErrType = ConnTypeHasNum (correctType, portNumber)
                  InDependency = None
                  ComponentsAffected = [ ComponentId port.HostId ]
                  ConnectionsAffected = [ ConnectionId connId ] }
        | true, None -> None // All right.

    /// Check conditions 1, 2, 6
    let rec checkConnectionsPorts (connections: Connection list) =
        match connections with
        | [] -> None
        | conn :: connections' ->
            match
                checkConnectionPort conn.Source PortType.Output conn.Id,
                checkConnectionPort conn.Target PortType.Input conn.Id
            with
            | Some err, _
            | _, Some err -> Some err
            | None, None -> checkConnectionsPorts connections' // Check next.

    let components, connections = canvasState

    match checkComponentsPorts components, checkConnectionsPorts connections with
    | Some err, _
    | _, Some err -> Some err
    | None, None -> None // All right.

/// Apply condition on every element of the map (tailored to this specific
/// problem).
let private checkEvery
    (counts: Map<string, (Connection list * int)>) // 'a is either InputPortId or OutputPortId.
    (cond: int -> bool)
    : (string * ConnectionId list * int) option
    =
    (None, Map.toList counts)
    ||> List.fold (fun maybeErr (idStr, (conns, count)) ->
        match maybeErr with
        | Some err -> Some err
        | None ->
            match cond count with
            | true -> None
            | false ->
                Some
                    ( idStr,
                      conns
                        |> List.map (fun conn -> ConnectionId conn.Id),
                      count))

/// Count the number of connections that target each port or group of label input ports
let private countPortsConnections
    (conns: Connection list)
    (connMap: Connection -> 'b)
    (bins: 'b list)
    =
    let rec countPortsConnections' (conns: Connection list) (counts: Map<'b, int * Connection list>) =
        match conns with
        | [] ->
            counts
            |> Map.toList
            |> List.map (fun (key, (count, conns)) -> key, (conns, count))
            |> Map.ofList
        | conn :: conns' ->
            let countsRes =
                let key = connMap conn

                // Hacky fix
                match Map.tryFind key counts with
                | Some(binCount, binConns) -> counts.Add(key, (binCount + 1, conn :: binConns))
                | None -> counts

            countPortsConnections' conns' countsRes

    countPortsConnections'
        conns
        (bins
         |> List.map (fun b -> b, (0, []))
         |> Map.ofList)

let private checkCounts (conns: Connection list) connMap bins binMap cond =
    let totals = countPortsConnections conns connMap bins
    checkEvery totals cond

let getPortNum (pList: Port list) (port: Port) =
    pList
    |> List.tryFind (fun po -> po.Id = port.Id)
    |> function
        | Some po -> po.PortNumber
        | None -> failwithf "port should be in given port list"

let getRmInfoData m port =
    let parentComp = m.ToComp[ComponentId port.HostId]
    let portNum =
        getPortNum
            (if port.PortType = PortType.Input then parentComp.InputPorts else parentComp.OutputPorts)
            port
    let portName = getPortName parentComp {port with PortNumber = portNum}
    (parentComp, portName)

let private getInPortRmInfo (m: MapData) (counts: Map<string,Connection list * int>) (port: Port) : PortRmInfo =
    let parentComp, portName = getRmInfoData m port
    let checkAllPorts (cond: int -> Port -> bool) (ports: Port list) =
        ports
        |> List.forall (fun port -> cond (snd counts[port.Id]) port)
    
    let condDisconnected count port =
        match count with
        | 0 -> true
        | _ -> false
    
    let condSelDisconnected (portNumSelection: string list) (count: int) (port: Port) =
        match (portNumSelection |> List.contains portName) with
        | true -> condDisconnected count port
        | false -> true

    match parentComp.Type with
    | NbitsAdder w ->
        match portName with
        | "CIN" -> Removable (NbitsAdderNoCin w) // must be CIN port
        | _ -> Unremovable
    | NbitsAdderNoCout w ->
        match portName with
        | "CIN" -> Removable (NbitsAdderNoCinCout w) // must be CIN port
        | _ -> Unremovable
    | CounterNoLoad w -> Removable (CounterNoEnableLoad w)
    | CounterNoEnable w -> // check if both Load and D are disconnected because only both can be removed or none
        match (checkAllPorts condDisconnected parentComp.InputPorts) with
        | true -> Removable (CounterNoEnableLoad w)
        | false -> Unremovable
    | Counter w ->
        let isDAndLoadDisconnected = checkAllPorts (condSelDisconnected ["D"; "LOAD"]) parentComp.InputPorts
        let isEnDisconnected = checkAllPorts (condSelDisconnected ["EN"]) parentComp.InputPorts
        match portName, isDAndLoadDisconnected, isEnDisconnected with
        | "D", true, _ | "LOAD", true, _ -> Removable (CounterNoLoad w)
        | "D", false, _ | "LOAD", false, _ -> Unremovable
        | "EN", _, true -> Removable (CounterNoEnable w)
        | "EN", _, false -> Unremovable
        | _, false, false -> Unremovable
        | _, _, _ -> failwith "Ports can never be connected like this"
    | _ -> Unremovable


let private getOutPortRmInfo (m: MapData) (counts: Map<string,Connection list * int>) (port: Port) : PortRmInfo =
    let parentComp, portName = getRmInfoData m port
    match parentComp.Type with
    | NbitsAdder w ->
        match portName with
        | "COUT" -> Removable (NbitsAdderNoCout w)
        | _ -> Unremovable
    | NbitsAdderNoCin w ->
        match portName with
        | "COUT" -> Removable (NbitsAdderNoCinCout w)
        | _ -> Unremovable
    | _ -> Unremovable

let private getInErrType count port rmInfo =
    InputConnError (count, port, rmInfo)
let getOutErrType count port rmInfo =
    OutputConnError (count, port, rmInfo)

let private checkPortConnections
    (m: MapData)
    (connMap: Connection -> string)
    (bins: Port list)
    (cond: int -> bool)
    pidMap
    rmInfoGen
    errTypeGen
    : SimulationError option
    =
    let counts =
        countPortsConnections
            m.Connections
            connMap
            (bins |> List.map (fun port -> port.Id))
    
    match checkEvery counts cond with
    | Some (portId, connsAffected, count) ->
        let port = pidMap portId
        let rmInfo = rmInfoGen m counts port
        Some {
            ErrType = errTypeGen count port rmInfo
            InDependency = None
            ComponentsAffected = [ComponentId port.HostId]
            ConnectionsAffected = connsAffected
        }
    | None -> None

let private checkConns (conns: Connection list) (m: MapData) : SimulationError option =
    let compOfPort p = m.ToComp[ComponentId p.HostId]

    conns
    |> List.tryPick (fun conn ->
        let s = compOfPort conn.Source
        let t = compOfPort conn.Target

        if s.Type = IOLabel && t.Type = IOLabel then
            Some(s, t, conn)
        else
            None)
    |> Option.map (fun (s, t, conn) ->
        {   ErrType = LabelConnect
            InDependency = None
            ComponentsAffected = [ ComponentId s.Id; ComponentId t.Id ]
            ConnectionsAffected = [ ConnectionId conn.Id ] })

/// Check that:
/// - any port has at least one connection,
/// - any input port has precisely one connection.
/// These conditions may not hold due to user errors.
let private checkPortsAreConnectedProperly (canvasState: CanvasState) =
    let m = genMaps canvasState
    let conns = m.Connections
    let labMap (lab: string) = m.LabGroup[lab]

    [
      checkPortConnections
        m
        (fun conn -> conn.Target.Id)
        m.OtherInputPorts
        ((=) 1)
        (fun pid -> m.ToInputPort[InputPortId pid])
        getInPortRmInfo
        getInErrType

      match (checkCounts
          m.LabTargetConns
          (fun conn -> m.ToComp[ComponentId conn.Target.HostId].Label)
          (m.LabGroup |> Map.toList |> List.map fst)
          (labMap)
          ((=) 1)) with
            | Some (labelId, connsAffected, count) ->
                Some {
                    ErrType = LabelConnError count
                    InDependency = None
                    ComponentsAffected = (labMap labelId) |> List.map (fun comp -> ComponentId comp.Id)
                    ConnectionsAffected = connsAffected
                }
            | None -> None

      checkPortConnections
        m
        (fun conn -> conn.Source.Id)
        m.OtherOutputPorts
        ((<) 0)
        (fun pid -> m.ToOutputPort[OutputPortId pid])
        getOutPortRmInfo
        getOutErrType

      checkConns conns m

      ]
    |> List.tryPick id

/// Input/Output components in a simulationgraph all have unique labels.
let private checkIOLabels (canvasState: CanvasState) : SimulationError option =
    let rec checkDuplicate (comps: Component list) (map: Map<string, string>) (ioType: string) =
        match comps with
        | [] -> None
        | comp :: comps' ->
            match map.TryFind comp.Label with
            | None -> checkDuplicate comps' map ioType
            | Some compId when compId = comp.Id -> checkDuplicate comps' map ioType
            | Some compId ->
                Some
                    { ErrType = LabelDuplicate (ioType, comp.Label)
                      InDependency = None
                      ComponentsAffected = [ comp.Id; compId ] |> List.map ComponentId
                      ConnectionsAffected = [] }

    let toMap (comps: Component list) =
        comps
        |> List.map (fun comp -> comp.Label, comp.Id)
        |> Map.ofList

    let components, _ = canvasState

    let inputs =
        components
        |> List.filter (fun comp ->
            match comp.Type with
            | Input1 _ -> true
            | _ -> false)

    let outputs =
        components
        |> List.filter (fun comp ->
            match comp.Type with
            | Output _ -> true
            | _ -> false)

    let labels =
        components
        |> List.filter (fun comp ->
            match comp.Type with
            | IOLabel -> true
            | _ -> false)

    match checkDuplicate inputs (toMap inputs) "Input", checkDuplicate outputs (toMap outputs) "Output" with
    | Some err, _
    | _, Some err -> Some err
    | None, None -> None

type CustomComponentError =
    | NoSheet of string
    | BadInputs of ComponentSheet: string * InstLists: ((string * int) list) * CompLists: ((string * int) list)
    | BadOutputs of ComponentSheet: string * InstLists: ((string * int) list) * CompLists: ((string * int) list)

/// Check a single custom component for correct I/Os
let checkCustomComponentForOkIOs (c: Component) (args: CustomComponentType) (sheets: LoadedComponent list) =
    let inouts = args.InputLabels, args.OutputLabels
    let name = args.Name
    let compare labs1 labs2 = (labs1 |> Set) = (labs2 |> Set)

    sheets
    |> List.tryFind (fun sheet -> sheet.Name = name)
    |> Option.map (fun sheet ->
        sheet, compare sheet.InputLabels args.InputLabels, compare sheet.OutputLabels args.OutputLabels)
    |> function
        | None -> Error(c, NoSheet name)
        | Some(_, true, true) -> Ok()
        | Some(sheet, false, _) ->
            Error
            <| (c, BadInputs(name, sheet.InputLabels, args.InputLabels))
        | Some(sheet, true, false) ->
            Error
            <| (c, BadOutputs(name, sheet.OutputLabels, args.OutputLabels))

/// Custom components have I/Os which are the same (names) as the I/Os in the corresponding sheet
/// This can change if a sheet made into a custom component is edited
/// We do this check whenever a new sheet is opened
let checkCustomComponentsOk ((comps, _): CanvasState) (sheets: LoadedComponent list) : SimulationError option =
    let error (c: Component) (t: SimulationErrorType) =
        Some
            { ErrType = t
              InDependency = None
              ComponentsAffected = [ ComponentId c.Id ]
              ConnectionsAffected = [] }

    let disp portL =
        portL
        |> List.map fst
        |> String.concat " , "
        |> sprintf "%s"

    comps
    |> List.collect (function
        | { Type = Custom args } as c -> [ checkCustomComponentForOkIOs c args sheets ]
        | _ -> [])
    |> Helpers.tryFindError
    |> function
        | Ok _ -> None
        | Error(c, NoSheet cName) ->
            error c (MissingSheet cName)
        | Error(c, BadInputs(cName, instIns, compIns)) ->
            let instIns, compIns = disp instIns, disp compIns

            error c (InPortMismatch (cName, instIns, compIns))
        | Error(c, BadOutputs(cName, instOuts, compOuts)) ->
            let instOuts, compOuts = disp instOuts, disp compOuts

            error c (OutPortMismatch (cName, instOuts, compOuts))

/// Check whether Adders have a NotConnected component connected to their COUT
/// this is unnecessary since it can be disabled via its options
let checkAdderUnnecessaryNC ((comps, conns): CanvasState) : SimulationError option =
    let isNotConnected id =
        comps
        |> List.exists (fun comp ->
            comp.Id = id && comp.Type = NotConnected)
    
    let getNCConnectedOutPorts (outPortsList: Port list) =
        outPortsList
        |> List.filter (fun port ->
            conns
            |> List.exists (fun conn ->
                conn.Source.Id = port.Id && isNotConnected conn.Target.HostId))

    let affectedPorts =
        comps
        |> List.filter (function comp ->
                                    match comp.Type with
                                    | NbitsAdder _ | NbitsAdderNoCin _ -> true
                                    | _ -> false) // only check adders with COUT
        |> List.collect (fun comp -> getNCConnectedOutPorts comp.OutputPorts)
        |> List.filter (fun port -> (Option.get port.PortNumber) <> 0) // ignore SUM port
    
    

    match affectedPorts with
        | [] -> None
        | affPorts ->
            let affectedComps =
                affPorts
                |> List.map (fun port -> ComponentId port.HostId)
            
            let affectedConns =
                conns
                |> List.filter (fun conn ->
                    affPorts
                    |> List.exists (fun port ->
                        port.Id = conn.Source.Id))
                |> List.map (fun conn -> ConnectionId conn.Id)
            Some {
                ErrType = UnnecessaryNC
                InDependency = None
                ComponentsAffected = affectedComps
                ConnectionsAffected = affectedConns
            }

/// Checks that all connections have consistent widths.
/// This function relies on the bus inferer, but also makes sure that all widths
/// can be inferred.
let private checkConnectionsWidths (canvasState: CanvasState) : SimulationError option * ConnectionsWidth option =
    let convertConnId (ConnectionId cId) = ConnectionId cId
    let convertError (err: WidthInferError) : SimulationError =
        { ErrType = WidthMismatch err
          InDependency = None
          ConnectionsAffected = err.ConnectionsAffected |> List.map convertConnId
          ComponentsAffected = [] }
    match inferConnectionsWidth canvasState with
    | Error err -> Some <| convertError err, None
    | Ok connWidths ->
        let faulty =
            connWidths
            |> Map.filter (fun _ width -> Option.isNone width)
        match faulty.IsEmpty with
        | true -> None, Some connWidths // All good.
        | _ ->
            Some
                { ErrType = InferConnWidths "Could not infer all connections widths."
                  InDependency = None
                  ConnectionsAffected =
                    faulty
                    |> Map.toList
                    |> List.map (fun (cId, _) -> convertConnId cId)
                  ComponentsAffected = [] },
            None

/// check component labels are all unique and do not include protected values (CLK)
let checkComponentNamesAreOk ((comps, conns): CanvasState) =
    let badNameErrors =
        comps
        |> List.filter (function
            | { Type = MergeWires }
            | { Type = SplitWire _ }
            | { Type = BusSelection _ }
            | { Type = NotConnected } -> false
            | _ -> true)
        |> List.collect (fun comp ->
            let label = comp.Label.ToUpper()

            match label with
            | "CLK" ->
                [ comp,
                  "Clk is not allowed as a name for a component or a Net. \
                        Use the properties tab to give a different name to the highlighted component(s)." ]
            | "" ->
                [ comp,
                  "All components must have a unique alphanumeric name (e.g. 'G1'). An empty name is not allowed except for split and join and bus select.\
                        Use the properties tab to give a non-empty name to the highlighted component(s)." ]
            | _ -> [])
        |> List.groupBy snd
        |> List.map (fun (msg, eLst) -> List.map fst eLst, msg)

    let duplicateNameErrors =
        comps
        |> List.filter (function
            | { Type = IOLabel }
            | { Type = MergeWires }
            | { Type = SplitWire _ }
            | { Type = BusSelection _ }
            | { Type = NotConnected } -> false
            | _ -> true)
        |> List.groupBy (fun comp -> comp.Label)
        |> List.filter (fun (_, compL) -> List.length compL > 1)
        |> List.map (fun (_, compL) ->
            compL,
            "Component names must be distinct. \
                            Use the properties tab to give different names to the highlighted components")

    List.tryHead (badNameErrors @ duplicateNameErrors)
    |> Option.map (fun (comps, msg) ->
        { ErrType = BadName msg
          InDependency = None
          ConnectionsAffected = []
          ComponentsAffected =
            comps
            |> List.map (fun comp -> ComponentId comp.Id) })

/// Analyse a CanvasState and return any error (or None).
let analyseState
    (state: CanvasState)
    (ldComps: LoadedComponent list)
    : SimulationError option * ConnectionsWidth option
    =
    let widthErr, connectionsWidth = checkConnectionsWidths state
    [ checkPortTypesAreConsistent state
      checkPortsAreConnectedProperly state
      checkIOLabels state
      checkCustomComponentsOk state ldComps
      widthErr
      checkComponentNamesAreOk state
      checkAdderUnnecessaryNC state ]
    |> List.tryFind Option.isSome
    |> Option.flatten,
    connectionsWidth
