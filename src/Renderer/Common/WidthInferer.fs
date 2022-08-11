(*
   Function to perform bus width inference on a canvas
*)

module BusWidthInferer

open CommonTypes
open Helpers

// 1. Initialise Map<ConnectionId, int option> for all connections, to None
//    (None means width not inferred yet).
// 2. Extract Input Components.
// 3. Starting from all those input components, run the inference process:
//    a. Case: component has all information about connections connected to it.
//       (for example, an input node or an and gate. They know they expect bits).
//       - Get the width of the incoming wires
//       - If there is any inconsistence, return the error
//       - Set the width of the outgoing wires
//       - follow the wires you just set and repeat the inference process on the
//         new components.
//    b. Case: component does not have all the information for computing the
//       width of outgoing wires yet.
//       (for example, a mergeBus components with only one bus connected)
//       - return


let mapKeys (map:Map<'a,'b>) = map |> Map.toSeq |> Seq.map fst |> List.ofSeq
let mapValues (map:Map<'a,'b>) = map |> Map.toSeq |> Seq.map snd |> List.ofSeq
let mapItems (map:Map<'a,'b>) = map |> Map.toSeq |> List.ofSeq


/// Extract the port number of a component port. Port numbers on Components
/// should always be populated (while they are always None for ports in
/// Connections).
let private extractComponentPortNumber (port: Port) =
    match port.PortNumber with
    | None -> failwithf "what? extractComponentPortNumber should always be called with component ports: %A" port
    | Some pNumber -> pNumber


let private assertInputsSize
        (inputs : Map<InputPortNumber, (int option * ConnectionId) option>)
        (expected : int)
        (comp : Component) =
#if ASSERTS
    assertThat (inputs.Count = expected)
    <| sprintf "assertInputsSize failed for: %A" comp
#else
        ()
#endif




let private getOutputPortId (comp : Component) (idx : int) : OutputPortId =
    match List.tryFind (fun p -> extractComponentPortNumber p = idx) comp.OutputPorts with
    | None -> failwithf "what? getOutputPortId called with inexistent port idx (%d): %A " idx comp
    | Some port -> OutputPortId port.Id

/// Extract the widths of the connections to input ports of a component.
/// The values are returned in the the passed order. E.g. if portNumbers is
/// [0, 1, 2], the returned value will be [width0, width1, width2].
let rec private getWidthsForPorts
        (inputs : Map<InputPortNumber, (int option * ConnectionId) option>)
        (portNumbers : InputPortNumber list)
        : (int option) list =
    match portNumbers with
    | [] -> []
    | portNumber :: portNumbers' ->
        match inputs.TryFind portNumber with
        | None -> failwithf "what? getWidthForPorts received a not extistent port: %A %A" portNumber inputs
        | Some (Some (width, _)) -> width :: getWidthsForPorts inputs portNumbers'
        | Some None -> None :: getWidthsForPorts inputs portNumbers'

/// Extract the ConnectionId of the connection connected to a certain input
/// port. Fail if such connection does not exist.
let private getConnectionIdForPort
        (inputs : Map<InputPortNumber, (int option * ConnectionId) option>)
        (portNumber : InputPortNumber)
        : ConnectionId =
    match inputs.TryFind portNumber with
    | None -> failwithf "what? getConnectionIdForPort received a not extistent port: %A %A" portNumber inputs
    | Some None -> failwithf "what? getConnectionIdForPort called with an unconnected port: %A %A" portNumber inputs
    | Some (Some (_, connId)) -> connId

let private makeWidthInferErrorEqual expected actual connectionsAffected = Error {
    Msg = sprintf "Wrong wire width. Target port expects a %d-bit signal, but source port produces a %d-bit signal." expected actual
    ConnectionsAffected = connectionsAffected
}

let private makeWidthInferErrorAtLeast atLeast actual connectionsAffected = Error {
    Msg = sprintf "Wrong wire width. Target port expects a signal with at least %d bits, but source port produces a %d-bit signal." atLeast actual
    ConnectionsAffected = connectionsAffected
}
    

/// Add to the map the extra (virtual) connections formed from each set of similarlky named bus labels.
/// each unconnected bus label input is virtually connected to the (single) connection
/// that drives the set
let addVirtualBusLabelConnections 
        (compIdToComp: Map<ComponentId,Component>)
        (inputPortsToConnectionIds: Map<InputPortId,ConnectionId>) : Map<InputPortId,ConnectionId> =

    let comps = mapValues compIdToComp

    let inputPort0Id (comp:Component) = InputPortId comp.InputPorts[0].Id

    let labelGroups =
        comps
        |> List.filter (fun (comp:Component) -> comp.Type=IOLabel)
        |> List.groupBy (fun comp -> comp.Label)

    let createVirtualMappings (compLst:Component list) (connId: ConnectionId): (InputPortId*ConnectionId) list=
        compLst                
        |> List.map (fun comp -> inputPort0Id comp,connId)

    let extraLabelConns =
        labelGroups
        |> List.collect (fun (name, labComps) -> 
            labComps
            |> List.tryPick (fun comp -> Map.tryFind (inputPort0Id comp) inputPortsToConnectionIds)
            |> Option.map (createVirtualMappings labComps)
            |> Option.defaultValue [])
            
    inputPortsToConnectionIds
    |> Map.toSeq
    |> Seq.append (extraLabelConns |> Seq.ofList)
    |> Map.ofSeq
   

/// For width inference, because IOLabel components join nets,
/// the single allowed input connection to a set of labels must
/// be replicated as an virtual input to all in the width inferrer and
/// simulation logic
let private makeOutputPortsOfLabels (components: Component list) : Map<string,OutputPortId list>=
        components
        |> List.filter (function | { Type=IOLabel} -> true; | _ -> false)
        |> List.groupBy (fun c -> c.Label)
        |> List.map (fun (label, lst) -> 
            label, lst 
            |> List.map (fun comp -> getOutputPortId comp 0))
        |> Map.ofList
    

/// Given a component and a set of input connection widths, check these inputs
/// widths are as expected and try to calculate the width of the outgoing
/// connections.
/// Components can produce outputs as soon as they have enough info (e.g.
/// gates can output width 1 straight away, PushToBusFirst can output n + 1 as
/// soon as it finds out n, and so on). This is possible because
/// setConnectionsWidth will make sure that we do not re-explore an already set
/// connection. This should allow partially connected components to still work
/// if they have already enough info.
let private calculateOutputPortsWidth 
        (comp : Component)
        (outputPortsOfBusLabels: Map<string,OutputPortId list>)
        (inputConnectionsWidth : Map<InputPortNumber, (int option * ConnectionId) option>)
        : Result<Map<OutputPortId, int>, WidthInferError> =
    let getConnectionIdForPort =
        InputPortNumber >> (getConnectionIdForPort inputConnectionsWidth)
    match comp.Type with
    | ROM _ | RAM _ | AsyncROM _ ->
        failwithf "What? Legacy RAM component types should never occur"
    | Input _ ->
        failwithf "Legacy Input components should never occur"
    | Input1 (width, _) | Constant1(width,_,_) | Constant(width,_)->
        // Expects no inputs, and has an outgoing wire of the given width.
        assertInputsSize inputConnectionsWidth 0 comp
        Ok <| Map.empty.Add (getOutputPortId comp 0, width)
        
    | Output width | Viewer width ->
        assertInputsSize inputConnectionsWidth 1 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0] with
        | [None] -> Ok Map.empty
        | [Some n] -> if n = width then Ok Map.empty // Output node has no outputs.
                      else  
                        makeWidthInferErrorEqual width n [getConnectionIdForPort 0]
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    | IOLabel->
        match getWidthsForPorts inputConnectionsWidth ([InputPortNumber 0]) with
        | [None] -> Ok <| Map.empty
        | [Some n] -> 
            let outs = outputPortsOfBusLabels[comp.Label]
            outs
            |> List.map (fun out -> out,n)
            |> Map.ofList |> Ok
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    | BusSelection(width, lsBitNum) ->
        assertInputsSize inputConnectionsWidth 1 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0] with
        | [Some n] when n < width + lsBitNum -> makeWidthInferErrorAtLeast (lsBitNum + n) n [getConnectionIdForPort 0]
        | [None] | [Some _ ] -> Ok <| Map.empty.Add (getOutputPortId comp 0, width)
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    | BusCompare(width, compareVal) ->
        assertInputsSize inputConnectionsWidth 1 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0] with
        | [Some n] when n <> width -> makeWidthInferErrorEqual width n [getConnectionIdForPort 0]
        | [None] | [Some _ ] -> Ok <| Map.empty.Add (getOutputPortId comp 0, 1)
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type

    | Not ->
        assertInputsSize inputConnectionsWidth 1 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0] with
        | [None] | [Some 1] -> Ok <| Map.empty.Add (getOutputPortId comp 0, 1)
        | [Some n] -> makeWidthInferErrorEqual 1 n [getConnectionIdForPort 0]
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    | And | Or | Xor | Nand | Nor | Xnor ->
        assertInputsSize inputConnectionsWidth 2 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1] with
        | [Some n; _] when n <> 1 -> makeWidthInferErrorEqual 1 n [getConnectionIdForPort 0]
        | [_; Some n] when n <> 1 -> makeWidthInferErrorEqual 1 n [getConnectionIdForPort 1]
        | [None; _] | [_; None]
        | [Some 1; Some 1] -> Ok <| Map.empty.Add (getOutputPortId comp 0, 1)
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    | Mux2 ->
        // Mux also allowes buses.
        assertInputsSize inputConnectionsWidth 3 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1; InputPortNumber 2] with
        | [Some n; Some m; Some 1] when n = m -> Ok <| Map.empty.Add (getOutputPortId comp 0, n)
        | [Some n; Some m; _] when n <> m ->
            // Two inputs have different widths, this is not allowed.
            Error {
                Msg = sprintf "Wrong wire width. The two inputs to a multiplexer are expected to have the same width, but top input has %d bits and bottom input has %d bits." n m
                ConnectionsAffected = [getConnectionIdForPort 0; getConnectionIdForPort 1]
            }
        | [_; _; Some n] when n <> 1 -> makeWidthInferErrorEqual 1 n [getConnectionIdForPort 2]
        | [Some n; None; _]
        | [None; Some n; _] -> Ok <| Map.empty.Add (getOutputPortId comp 0, n)
        | [_; _; _] -> Ok Map.empty // Keep on waiting.
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    | Mux4 ->
        // Mux also allowes buses.
        assertInputsSize inputConnectionsWidth 5 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1; InputPortNumber 2; InputPortNumber 3; InputPortNumber 4] with
        | [Some n; Some m; Some a; Some b; Some 2] when (n = m && n = a && n = b) -> Ok <| Map.empty.Add (getOutputPortId comp 0, n)
        | [Some n; Some m; Some a; Some b; _] when (n <> m || n <> a || n <> b) ->
            // Two inputs have different widths, this is not allowed.
            Error {
                Msg = sprintf "Wrong wire width. The four inputs to a multiplexer are expected to have the same width, but 1st input has %d bits, 2nd input has %d bits, 3rd input has %d bits, 4th input has %d bits." n m a b
                ConnectionsAffected = [getConnectionIdForPort 0; getConnectionIdForPort 1; getConnectionIdForPort 2; getConnectionIdForPort 3]
            }
        | [_; _; _; _; Some n] when n <> 2 -> makeWidthInferErrorEqual 2 n [getConnectionIdForPort 4]
        | [Some n; Some m; Some a; None; _]
        | [Some n; Some m; None; Some a; _]
        | [Some n; None; Some m; Some a; _]
        | [None; Some n; Some m; Some a; _] -> Ok <| Map.empty.Add (getOutputPortId comp 0, n)
        | [_; _; _; _; _] -> Ok Map.empty // Keep on waiting.
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    | Mux8 ->
        // Mux also allowes buses.
        assertInputsSize inputConnectionsWidth 9 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1; InputPortNumber 2; InputPortNumber 3; InputPortNumber 4;InputPortNumber 5; InputPortNumber 6; InputPortNumber 7; InputPortNumber 8] with
        | [Some n; Some m; Some a; Some b; Some c; Some d; Some e; Some f; Some 3] when 
            (n = m && n = a && n = b && n = c && n = d && n = e && n = f) -> Ok <| Map.empty.Add (getOutputPortId comp 0, n)
        | [Some n; Some m; Some a; Some b; Some c; Some d; Some e; Some f; _] when 
            (n <> m || n <> a || n <> b || n <> c || n <> d || n <> e || n<>f) ->
            // Two inputs have different widths, this is not allowed.
            Error {
                Msg = sprintf "Wrong wire width. The eight inputs to a multiplexer are expected to have the same width, but 1st input has %d bits, 2nd input has %d bits, 3rd input has %d bits, 4th input has %d bits, 5th input has %d bits, 6th input has %d bits, 7th input has %d bits, 8th input has %d bits." n m a b c d e f
                ConnectionsAffected = [getConnectionIdForPort 0; getConnectionIdForPort 1; getConnectionIdForPort 2; getConnectionIdForPort 3; getConnectionIdForPort 4; getConnectionIdForPort 5; getConnectionIdForPort 6; getConnectionIdForPort 7]
            }
        | [_; _; _; _; _; _; _; _; Some n] when n <> 3 -> makeWidthInferErrorEqual 3 n [getConnectionIdForPort 8]
        | [Some n; Some m; Some a; Some b; Some c; Some d; Some e; None; _]
        | [Some n; Some m; Some a; Some b; Some c; Some d; None; Some e; _]
        | [Some n; Some m; Some a; Some b; Some c; None; Some d; Some e; _]
        | [Some n; Some m; Some a; Some b; None; Some c; Some d; Some e; _]
        | [Some n; Some m; Some a; None; Some b; Some c; Some d; Some e; _]
        | [Some n; Some m; None; Some a; Some b; Some c; Some d; Some e; _]
        | [Some n; None; Some m; Some a; Some b; Some c; Some d; Some e; _] 
        | [None; Some n; Some m; Some a; Some b; Some c; Some d; Some e; _] -> Ok <| Map.empty.Add (getOutputPortId comp 0, n)
        | [_; _; _; _; _; _; _; _; _] -> Ok Map.empty // Keep on waiting.
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    | Demux2 ->
        // Demux also allowes buses.
        assertInputsSize inputConnectionsWidth 2 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1] with
        | [Some n; Some 1] | [Some n; None] ->
            let out = Map.empty.Add (getOutputPortId comp 0, n)
            let out = out.Add (getOutputPortId comp 1, n)
            Ok out
        | [_; Some n] when n <> 1 -> makeWidthInferErrorEqual 1 n [getConnectionIdForPort 1]
        | [_; _] -> Ok Map.empty // Keep on waiting.
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    | Demux4 ->
        // Demux also allowes buses.
        assertInputsSize inputConnectionsWidth 2 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1] with
        | [Some n; Some 2] | [Some n; None] ->
            let map = Map.empty.Add (getOutputPortId comp 0 , n)
            let out = (map, [1..3]) ||> List.fold (fun s v -> s |> Map.add (getOutputPortId comp v) n)
            Ok out
        | [_; Some n] when n <> 2 -> makeWidthInferErrorEqual 2 n [getConnectionIdForPort 1]
        | [_; _] -> Ok Map.empty // Keep on waiting.
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    | Demux8 ->
        // Demux also allowes buses.
        assertInputsSize inputConnectionsWidth 2 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1] with
        | [Some n; Some 3] | [Some n; None] ->
            let map = Map.empty.Add (getOutputPortId comp 0 , n)
            let out = (map, [1..7]) ||> List.fold (fun s v -> s |> Map.add (getOutputPortId comp v) n)
            Ok out
        | [_; Some n] when n <> 3 -> makeWidthInferErrorEqual 3 n [getConnectionIdForPort 1]
        | [_; _] -> Ok Map.empty // Keep on waiting.
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    | NbitsAdder numberOfBits ->
        assertInputsSize inputConnectionsWidth 3 comp
        let okOutMap =
            let out = Map.empty.Add (getOutputPortId comp 0, numberOfBits)
            let out = out.Add (getOutputPortId comp 1, 1)
            Ok out
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1; InputPortNumber 2] with
        | [Some n; _; _] when n <> 1 -> makeWidthInferErrorEqual 1 n [getConnectionIdForPort 0]
        | [_; Some n; _] when n <> numberOfBits -> makeWidthInferErrorEqual numberOfBits n [getConnectionIdForPort 1]
        | [_; _; Some n] when n <> numberOfBits -> makeWidthInferErrorEqual numberOfBits n [getConnectionIdForPort 2]
        | [_; _; _] -> okOutMap
        | x -> failwithf "what? Impossible case (%A) in calculateOutputPortsWidth for: %A" x comp.Type
    | NbitsXor numberOfBits ->
        assertInputsSize inputConnectionsWidth 2 comp
        let okOutMap =
            let out = Map.empty.Add (getOutputPortId comp 0, numberOfBits)
            Ok out
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1] with
        | [Some n; _] when n <> numberOfBits -> makeWidthInferErrorEqual 1 n [getConnectionIdForPort 0]
        | [_; Some n] when n <> numberOfBits -> makeWidthInferErrorEqual numberOfBits n [getConnectionIdForPort 1]
        | [_; _] -> okOutMap
        | x -> failwithf "what? Impossible case (%A) in calculateOutputPortsWidth for: %A" x comp.Type
    | NbitsAnd numberOfBits ->
        assertInputsSize inputConnectionsWidth 2 comp
        let okOutMap =
            let out = Map.empty.Add (getOutputPortId comp 0, numberOfBits)
            Ok out
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1] with
        | [Some n; _] when n <> numberOfBits -> makeWidthInferErrorEqual 1 n [getConnectionIdForPort 0]
        | [_; Some n] when n <> numberOfBits -> makeWidthInferErrorEqual numberOfBits n [getConnectionIdForPort 1]
        | [_; _] -> okOutMap
        | x -> failwithf "what? Impossible case (%A) in calculateOutputPortsWidth for: %A" x comp.Type
    | NbitsNot numberOfBits ->
        assertInputsSize inputConnectionsWidth 1 comp
        let okOutMap =
            let out = Map.empty.Add (getOutputPortId comp 0, numberOfBits)
            Ok out
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0] with
        | [Some n] when n <> numberOfBits -> makeWidthInferErrorEqual numberOfBits n [getConnectionIdForPort 0]
        // | [Some n] when n <> numberOfBits -> makeWidthInferErrorEqual numberOfBits n [getConnectionIdForPort 1]
        | [_] -> okOutMap
        | x -> failwithf "what? Impossible case (%A) in calculateOutputPortsWidth for: %A" x comp.Type
    
        // | [None] | [Some 1] -> Ok <| Map.empty.Add (getOutputPortId comp 0, 1)
        // | [Some n] -> makeWidthInferErrorEqual 1 n [getConnectionIdForPort 0]
        // | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    
    | Decode4  ->
        assertInputsSize inputConnectionsWidth 2 comp
        let okOutMap =
            [0..3]
            |> List.map (fun n -> getOutputPortId comp n, 1)
            |> Map.ofList
            |> Ok
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1] with
        | [Some 2; Some 1] -> okOutMap
        | [Some n; _] when n <> 2 -> makeWidthInferErrorEqual 2 n [getConnectionIdForPort 0]
        | [_; Some n] when n <> 1 -> makeWidthInferErrorEqual 1 n [getConnectionIdForPort 1]
        | [_; _] -> okOutMap
        | x -> failwithf "what? Impossible case (%A) in calculateOutputPortsWidth for: %A" x comp.Type
    | Custom custom ->
        assertInputsSize inputConnectionsWidth custom.InputLabels.Length comp
        let inputWidths =
            [0..custom.InputLabels.Length - 1]
            |> List.map InputPortNumber
            |> getWidthsForPorts inputConnectionsWidth
        // Make sure that input Widths match what expected.
        let maybeError =
            (inputWidths, custom.InputLabels)
            ||> List.mapi2 (fun idx actual (_, expected) ->
                match actual with
                | None -> None // Cannot determine if it is ok yet.
                | Some w when w = expected -> None // No error.
                | Some w -> Some <| makeWidthInferErrorEqual expected w [getConnectionIdForPort idx]
            )
        match List.tryFind (fun el -> el <> None) maybeError with
        | Some (Some err) -> err
        | None -> custom.OutputLabels
                  |> List.mapi (fun idx (_, w) -> getOutputPortId comp idx, w)
                  |> Map.ofList |> Ok
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    | MergeWires ->
        assertInputsSize inputConnectionsWidth 2 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1] with
        | [Some n; _] when n < 1 -> makeWidthInferErrorAtLeast 1 n [getConnectionIdForPort 0]
        | [_; Some m] when m < 1 -> makeWidthInferErrorAtLeast 1 m [getConnectionIdForPort 1]
        | [None; _] | [_; None] -> Ok Map.empty // Keep on waiting.
        | [Some n; Some m] -> Ok <| Map.empty.Add (getOutputPortId comp 0, n + m)
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    | SplitWire topWireWidth ->
        assertInputsSize inputConnectionsWidth 1 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0] with
        | [None] -> Ok Map.empty // Keep on waiting.
        | [Some n] when n < topWireWidth + 1 -> makeWidthInferErrorAtLeast (topWireWidth + 1) n [getConnectionIdForPort 0]
        | [Some n] ->
            let out = Map.empty.Add (getOutputPortId comp 0, topWireWidth)
            let out = out.Add (getOutputPortId comp 1, n - topWireWidth)
            Ok out
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    | DFF ->
        assertInputsSize inputConnectionsWidth 1 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0] with
        | [None] | [Some 1] -> Ok <| Map.empty.Add (getOutputPortId comp 0, 1)
        | [Some n] -> makeWidthInferErrorEqual 1 n [getConnectionIdForPort 0]
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    | DFFE ->
        assertInputsSize inputConnectionsWidth 2 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1] with
        | [None; None] | [Some 1; None] | [None; Some 1] | [Some 1; Some 1] ->
            Ok <| Map.empty.Add (getOutputPortId comp 0, 1)
        | [Some n; _] when n <> 1 -> makeWidthInferErrorEqual 1 n [getConnectionIdForPort 0]
        | [_; Some n] when n <> 1 -> makeWidthInferErrorEqual 1 n [getConnectionIdForPort 1]
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    | Register width ->
        assertInputsSize inputConnectionsWidth 1 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0] with
        | [None] -> Ok <| Map.empty.Add (getOutputPortId comp 0, width)
        | [Some n] when n = width -> Ok <| Map.empty.Add (getOutputPortId comp 0, width)
        | [Some n] when n <> width -> makeWidthInferErrorEqual width n [getConnectionIdForPort 0]
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    | RegisterE width ->
        assertInputsSize inputConnectionsWidth 2 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1] with
        | [Some n; Some 1] when n = width -> Ok <| Map.empty.Add (getOutputPortId comp 0, width)
        | [Some n; _] when n <> width -> makeWidthInferErrorEqual width n [getConnectionIdForPort 0]
        | [_; Some n] when n <> 1 -> makeWidthInferErrorEqual 1 n [getConnectionIdForPort 1]
        | [_; _] -> Ok <| Map.empty.Add (getOutputPortId comp 0, width)
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    | AsyncROM1 mem | ROM1 mem ->
        assertInputsSize inputConnectionsWidth 1 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0] with
        | [None] -> Ok <| Map.empty.Add (getOutputPortId comp 0, mem.WordWidth)
        | [Some aw] when aw = mem.AddressWidth ->
            Ok <| Map.empty.Add (getOutputPortId comp 0, mem.WordWidth)
        | [Some aw]  when aw <> mem.AddressWidth ->
            makeWidthInferErrorEqual mem.AddressWidth aw [getConnectionIdForPort 0]
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type
    | RAM1 mem | AsyncRAM1 mem->
        assertInputsSize inputConnectionsWidth 3 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1; InputPortNumber 2] with
        | [Some addr; Some datain; Some write] when addr = mem.AddressWidth &&
                                                    datain = mem.WordWidth &&
                                                    write = 1 ->
            Ok <| Map.empty.Add (getOutputPortId comp 0, mem.WordWidth)
        | [Some addr; _; _] when addr <> mem.AddressWidth ->
            makeWidthInferErrorEqual mem.AddressWidth addr [getConnectionIdForPort 0]
        | [_; Some datain; _] when datain <> mem.WordWidth ->
            makeWidthInferErrorEqual mem.WordWidth datain [getConnectionIdForPort 1]
        | [_; _; Some write;] when write <> 1 ->
            makeWidthInferErrorEqual 1 write [getConnectionIdForPort 2]
        | [_; _; _] -> Ok <| Map.empty.Add (getOutputPortId comp 0, mem.WordWidth)
        | _ -> failwithf "what? Impossible case in calculateOutputPortsWidth for: %A" comp.Type

/// Find the connection connected to an input port. Return None if no such
/// connection exists.
let private findConnectionToInputPort
        (inputPortIdsToConnectionIds : Map<InputPortId, ConnectionId>)
        (portId : InputPortId)
        : ConnectionId option =
    inputPortIdsToConnectionIds.TryFind portId

/// Find all the connections connected to an output port.
let private findConnectionsFromOutputPort
        (outputPortIdsToConnections : Map<OutputPortId, Connection list>)
        (portId : OutputPortId)
        : (Connection list) option =
    outputPortIdsToConnections.TryFind portId

/// Lookup the width of a connection in the connectionsWidth map or fail.
let getConnectionWidth
        (connectionsWidth : ConnectionsWidth)
        (connId : ConnectionId)
        : int option =
    match connectionsWidth.TryFind connId with
    | None -> failwithf "what? getConnectionWidth received inexistent connectionId: %A" connId
    | Some width -> width

/// For each input port on a given component, obtain the width of the wire
/// connecting to it, and the ConnectionId of such wire. If there is no wire
/// connecting to the port, or the wire width is unknown, return None.
let private getInputPortsConnectionsWidth
        (connectionsWidth : ConnectionsWidth)
        (currNode : Component)
        (inputPortIdsToConnectionIds : Map<InputPortId, ConnectionId>)
        : Map<InputPortNumber, (int option * ConnectionId) option> =
    currNode.InputPorts
    |> List.map (fun inputPort ->
        InputPortId inputPort.Id
        |> findConnectionToInputPort inputPortIdsToConnectionIds
        |> function
           | Some connId ->
               // If some connection is present, try to etract is width.
               InputPortNumber <| extractComponentPortNumber inputPort,
               Some (getConnectionWidth connectionsWidth connId, connId)
           | None ->
               // If no connection is present, just use None.
               InputPortNumber <| extractComponentPortNumber inputPort,
               None
    )
    |> Map.ofList

let private setConnectionWidth
        (connectionId : ConnectionId)
        (connectionWidth : int)
        (connectionsWidth : ConnectionsWidth) =
    connectionsWidth.Add (connectionId, Some connectionWidth)

/// Set the width of a bunch of connections, and return the updated
/// connectionsWidth together with the list of connections that have had their
/// width value updated.
/// If the width for a connection is already set:
/// - if the value we are going to set is identical to the already set value:
///   connection already visited (loop). Do not return the connection.
/// - if the value is different, return an error.
let private setConnectionsWidth
        (connections : Connection list)
        (connWidth : int)
        (connectionsWidth : ConnectionsWidth)
        : Result<ConnectionsWidth * (Connection list), WidthInferError> =
    (Ok (connectionsWidth, []), connections)
    ||> List.fold (fun res conn ->
        res |> Result.bind (fun (connectionsWidth, connectionsToReturn) ->
            let connId = ConnectionId conn.Id
            match getConnectionWidth connectionsWidth connId with
            | None ->
                // Width for the connection was never set. Set it and return
                // the connection.
                Ok (setConnectionWidth connId connWidth connectionsWidth,
                    conn :: connectionsToReturn)
            | Some oldWidth when oldWidth = connWidth ->
                // Width for the connection is already set. The old width
                // matches the current width. Do not return the connection.
                Ok (connectionsWidth, connectionsToReturn)
            | Some oldWidth when oldWidth <> connWidth ->
                // Width for the connection is already set, but the old width
                // does not match the current width.
                Error {
                    Msg = sprintf "Wire has been inferred to have two different widths: %d and %d. This is probably due to an error such as a combinatorial loop." oldWidth connWidth
                    ConnectionsAffected = [connId]
                }
            | _ -> failwithf "what? Impossible case in setConnectionsWidth."
        )
    )

let private getComponentFromId
        (compId : ComponentId)
        (compIdsToComps : Map<ComponentId, Component>)
        : Component =
    match compIdsToComps.TryFind compId with
    | None -> failwithf "what? getComponentFromId called with invalid componentId: %A" compId
    | Some comp -> comp

/// Given a node, try to infer the width of its outgoing connections, and
/// possibly recur on the nodes targeted by those connections.
let rec private infer
        // Static maps. Necessary for fast lookups.
        ((
            (inputPortIdsToConnectionIds : Map<InputPortId, ConnectionId>),
            (outputPortIdsToConnections : Map<OutputPortId, Connection list>),
            (compIdsToComps : Map<ComponentId, Component>),
            (outputPortsOfBusLabels: Map<string,OutputPortId list>)
        ) as staticMaps)
        (currNode : Component)
        (connectionsWidth : ConnectionsWidth)
        : Result<ConnectionsWidth, WidthInferError> =
    let iterateChildren outgoingConnections connectionsWidth =
        let children =
            outgoingConnections
            |> List.map (fun conn -> getComponentFromId (ComponentId conn.Target.HostId) compIdsToComps)
        (Ok connectionsWidth, children)
        ||> List.fold (fun connectionsWidthRes child ->
            connectionsWidthRes
            |> Result.bind (fun connectionsWidth ->
                infer staticMaps child connectionsWidth
            )
        )

    getInputPortsConnectionsWidth connectionsWidth currNode inputPortIdsToConnectionIds
    |> calculateOutputPortsWidth currNode outputPortsOfBusLabels
    |> Result.bind (fun outputPortsWidths ->
        // For each output in the map:
        // - Get all connections that are connected to that port.
        // - Set the width of the connection to the inferred value. If the
        //   connection has already been inferred, it must be because of a loop.
        //   If the value is different, return error. If the value is the same,
        //   just ignore the connection otherwise you would get stuck in the
        //   loop.
        // - For the non-ingored connections, take recur infer on the
        //   Target.HostId.
        (Ok connectionsWidth, outputPortsWidths)
        ||> Map.fold (fun connectionsWidthRes outPortId connWidth ->
            connectionsWidthRes
            |> Result.bind (fun connectionsWidth ->
                match findConnectionsFromOutputPort
                          outputPortIdsToConnections outPortId with
                | None ->
                    // Unconnected port. Do not recur.
                    Ok connectionsWidth
                | Some outgoingConnections ->
                    setConnectionsWidth outgoingConnections connWidth connectionsWidth
                    |> Result.bind (fun (connectionsWidth, updatedConnections) ->
                        iterateChildren updatedConnections connectionsWidth
                    )
            )
        )
    )

let private initialiseConnectionsWidth connections : ConnectionsWidth =
    connections
    |> List.map (fun (conn:Connection) -> ConnectionId conn.Id, None)
    |> Map.ofList

let private getAllInputNodes components : Component list =
    components |> List.filter (fun comp -> match comp.Type with | Input1 _ -> true | _ -> false)

/// For each connected input port, map the connection that is connected to it.
/// Fail if there are multiple connections connected to the same input port.
/// Such a scenario would mean that a wire is driven by multiple components.
let private mapInputPortIdsToConnectionIds
        (connections : Connection list)
        : Result<Map<InputPortId, ConnectionId>, WidthInferError> =
    (Ok Map.empty, connections)
    ||> List.fold (fun mapRes conn ->
        mapRes |> Result.bind (fun map ->
            let inputPortId = InputPortId conn.Target.Id
            let connId = ConnectionId conn.Id
            match map.TryFind inputPortId with
            | None -> Ok <| map.Add (inputPortId, connId)
            | Some otherConnId -> Error {
                Msg = "A wire must have precisely one driving component. If you want to merge two wires together, use a MergeWires component."
                ConnectionsAffected = [connId; otherConnId]
            }
        )
    )

let private mapComponentIdsToComponents
        (components : Component list)
        : Map<ComponentId, Component> =
    components
    |> List.map (fun comp -> ComponentId comp.Id, comp)
    |> Map.ofList 

    

/// return all connections connected to an output port
let private mapOutputPortIdsToConnections
        (connections : Connection list)
        : Map<OutputPortId, Connection list> =
    connections
    |> List.groupBy (fun conn -> OutputPortId conn.Source.Id)
    |> Map.ofList

/// Here each input port has associated with the connection that drives it.
/// Normally that is the connection connected to the port.
/// However BusLabels are a special case because a set of similarly named labels
/// have outputs all connected together and driven by the single connection that goes to
/// one of the BusLabel inputs (there must be exactly one such).
/// In this function any input driven by a connection from a BusLabel output gets associated
/// with the BusLabel set input connection, allowing correct width inference.
let private mapInputPortIdsToVirtualConnectionIds (conns: Connection list) (comps:Component list) =
    let mapPortIdToConnId = mapInputPortIdsToConnectionIds conns

    let filteredComps =
        comps
        |> List.filter (fun (comp:Component) -> comp.Type=IOLabel)
        
    let targetPortIdToConId =
        conns
        |> List.map (fun conn -> InputPortId conn.Target.Id, ConnectionId conn.Id)
        |> Map.ofList
    
    let getBusLabelConns (compLst: Component list)  =
        compLst
        |> List.collect (fun comp ->
            Map.tryFind (InputPortId comp.InputPorts[0].Id) targetPortIdToConId
            |> function | None -> [] | Some cId -> [cId])
    
    let mapLabels =
        filteredComps
        |> List.groupBy (fun comp -> comp.Label)
        |> List.map (fun (lab,compLst) ->
            match getBusLabelConns compLst  with
            | [cId] -> List.map (fun comp -> (InputPortId comp.InputPorts[0].Id, cId)) compLst |> Ok
            | h when h.Length <> 1 -> Error {
                Msg = sprintf "A wire label must have exactly one driving component but the label '%s' has %d" lab h.Length
                ConnectionsAffected = h 
                }            
            | _ -> Ok []
        ) 
        |> tryFindError
        |> Result.map (List.concat >> Map.ofList)

    match mapLabels, mapPortIdToConnId with
    | _, Error e | Error e, _ -> Error e
    | Ok mapL, Ok map ->
        comps
        |> List.collect (fun comp -> comp.InputPorts)
        |> List.map (fun p -> InputPortId p.Id)
        |> List.collect ( fun pId ->    
            match Map.tryFind pId map, Map.tryFind pId mapL with
            | None, None -> []
            | _, Some conn
            | Some conn, None -> [pId, conn])
        |> Map.ofList
        |> Ok


/// Return Inferred width of all connections or an error.
/// Width inference is done without mutable state. 
/// It is to be run when component widths or circuit is changed,
/// Note that it does not matter (except for performance) if it is run too many times.
let inferConnectionsWidth
        ((comps: Component list,conns: Connection list) : CanvasState)
        : Result<ConnectionsWidth, WidthInferError> =
    let start = TimeHelpers.getTimeMs()
    let connectionsWidth = initialiseConnectionsWidth conns // start with all as None 
    match mapInputPortIdsToVirtualConnectionIds conns comps with
    | Error e -> Error e
    | Ok  inputPortIdsToVirtualConnectionIds' ->
        let staticMapComponentIdsToComponents = mapComponentIdsToComponents comps
        let staticMaps = (
                inputPortIdsToVirtualConnectionIds', 
                mapOutputPortIdsToConnections conns, 
                staticMapComponentIdsToComponents,
                makeOutputPortsOfLabels comps
               )
        // If this is too slow, one could start the process only from input and constant
        // components. To do so, pass the (getAllInputNodes components) instead
        // of components. (But this would not work for ckts with no inputs or constants).
        (Ok connectionsWidth, comps)
        ||> List.fold (fun connectionsWidthRes inputNode ->
            connectionsWidthRes |> Result.bind (fun connectionsWidth ->
                infer staticMaps inputNode connectionsWidth
            )
        )
    |> TimeHelpers.instrumentInterval "widthInference" start
