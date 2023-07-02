module SmartPortOrder

open Elmish
open Fable.React.Props
open CommonTypes
open Fable.React
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open SmartHelpers
open Symbol
open Optics
open Optic
open Operators


(* 
    HLP23: This module will normally be used exclusively by team member doing the "smart port reorder" 
    part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    Normally it will update multiple wires and symbols in the BusWire model so could use the SmartHelper 
    functions for this purpose.
*)

(*
    HLP23: AUTHOR dgs119

    Flow of the Smart Port Reordering Algorithm:
    1) We first get SymbolReorderPair information. This includes the Symbol, OtherSymbol, Ports and their Dominant Edges.
        - Ports
            - We ensure ports from the same net are next to each other. This allows intersecting nets to be unwrapped.
        - Dominant Edge: 
            - Heuristic calculated from a subset of Symbol's port.
            - The Direction a Symbol faces. It's an edge where a symbol has ports but no port on another edge.
            - Otherwise its the edge with the most number of ports.
    2) Depending on the dominant edge of a port, we unwrap both Symbol ports in opposite directions.
        - For a Top Dominant Edge AntiClockwise ordering, we unwrap the ports from Right, Top, Left then Bottom Edge.
        - If ports around a component are the reverse order of pins around another component, they can be wired without crosses.
        - We force these connection pairs to avoid wire crosses.
    3) Swap ports on the the symbol to order such that connection pairs in (2) is followed.
        - There are two valid reorderings depending on how we unwrap a symbol's port. We chose one with least swaps.
        - Only Components with 2 IO ports on an Edge are ordered (eg. Mux2, DeMux2, etc).    
    4) Flip the Select of Mux/Demux.
        - We flip the select if its connected to a port on another component on the same edge.
        - And if all the Input ports are connected to the same component.
*)

/// Holds data about Port, its Edge and Location.
type PortInfo = { Port: Port; Orientation: Edge }

/// Holds data about Symbol pairs we compute port reorderings.
type SymbolReorderPair =
    { Symbol: Symbol
      OtherSymbol: Symbol
      Ports: Map<ComponentId, PortInfo list>
      DominantEdges: Map<ComponentId, Edge> }

/// Finds the Dominant Edge of a Symbol.
let symDominantEdge (symPorts: PortInfo list) =

    /// Checks if we have ports for an Edge.
    let edgeExists (edge: Edge) =
        symPorts |> List.exists (fun port -> port.Orientation = edge)

    /// Calculates Edge with most ports.
    let modeEdge (ports: PortInfo list) =
        ports // Otherwise get Mode Edge
        |> List.countBy (fun port -> port.Orientation)
        |> List.sortByDescending snd
        |> List.head
        |> fst

    match symPorts with
    | [] -> Left // Default to a Left Dominant Edge.
    | _ ->
        symPorts
        |> List.tryFind (fun port -> not (edgeExists port.Orientation.Opposite))
        |> function
            | Some port -> port.Orientation // Edge with no ports on opposite Edge.
            | _ -> modeEdge symPorts // Otherwise Mode Edge

/// Given a dominant edge, unwrap is symbol's port in a Clockwise or AntiClockwise direction.
let unwrapSymPorts (domEdge: Edge) (direction: Direction) (sym: Symbol) =

    let order = sym.PortMaps.Order

    /// Rotates the starting edge of an AntiClockwise Port Ordering Clockwise
    let rotClkwise (n: int) =
        ([ order[Right] ] @ [ order[Top] ] @ [ order[Left] ] @ [ order[Bottom] ])
        |> List.permute (fun idx -> (idx + n) % 4)

    /// Gets a Clockwise Port Reordering for a Dominant Edge Anticlockwise Ordering
    let revDirection (ports: string list list) =
        let frnt, back = List.splitAt (List.length ports - 1) ports
        (List.rev frnt) @ back |> List.map List.rev |> List.concat

    /// Unwraps Ports of a Symbol by dominant Edge and Direction.
    let unwrpByDirection (n: int) =
        match direction with
        | AntiClockwise -> rotClkwise n |> List.concat
        | Clockwise -> rotClkwise n |> revDirection

    match domEdge with
    | Top -> unwrpByDirection 0
    | Right -> unwrpByDirection 1
    | Bottom -> unwrpByDirection 2
    | Left -> unwrpByDirection 3

/// Groups Symbols to Reorder Into Sets.
let symbolMatch (sym: Symbol) =
    match sym.Component.Type with
    | And | Or | Xor | Nand | Nor | Xnor 
    | NbitsXor _ | NbitsAnd _ | NbitsNot _ | NbitsOr _ 
    | Mux2 | Demux2 -> // Two IO Components
        And
    | Custom _ as customComp -> customComp
    | otherCompType -> otherCompType

/// Get Ports Betweem Symbols
let portsBtwnSyms (model: BusWireT.Model) (sym: Symbol) (otherSym: Symbol) =

    /// Keeps Wire if its connected between 2IO or Custom Components.
    let keepWire (wire: Wire) =
        let symOfPort (portId: string) =
            match isPortInSymbol portId sym with
            | true -> sym
            | _ -> otherSym

        let keepPort (portId: string) =
            let sym = symOfPort portId

            match symbolMatch sym with
            | And -> // Flip 2IO Components.
                let portIdsOfIntrst =
                    sym.PortMaps.Order
                    |> Map.pick (fun _ ports' -> if List.length ports' = 2 then Some ports' else None)

                List.contains portId portIdsOfIntrst
            | Custom _ -> // Reorder.
                true
            | _ -> // Don't Reorder.
                false

        keepPort (getInputPortIdStr wire.InputPort)
        && keepPort (getOutputPortIdStr wire.OutputPort)

    let groupPortsBySym (ports: Port list) =
        ports |> List.partition (fun port -> ComponentId port.HostId = sym.Id)

    let portInfos (portsByNet: Port list list) =
        portsByNet
        |> List.concat
        |> List.map (fun port ->
            { Port = port
              Orientation = getPortOrientationFrmPortIdStr model.Symbol port.Id })

    let symPorts, otherSymPorts =
        connsBtwnSyms model sym otherSym
        |> Map.filter (fun _ wire -> keepWire wire)
        |> groupWiresByNet // Keep Ports of the same Net together.
        |> List.map (portsOfWires model >> groupPortsBySym)
        |> List.unzip

    [ portInfos symPorts; portInfos otherSymPorts ]
    |> List.zip [ sym.Id; otherSym.Id ]
    |> Map.ofList

/// Gets Symbol Reordering Information.
let symReorderPair (model: BusWireT.Model) (sym: Symbol) (otherSym: Symbol) =

    let portsBySym = portsBtwnSyms model sym otherSym

    let edgesBySym =
        [ portsBySym[sym.Id]; portsBySym[otherSym.Id] ]
        |> List.map symDominantEdge
        |> List.zip [ sym.Id; otherSym.Id ]
        |> Map.ofList

    { Symbol = sym
      OtherSymbol = otherSym
      Ports = portsBySym
      DominantEdges = edgesBySym }

/// Reorder's Symbol Ports such to prevent wire crossings.
let reorderSymPorts (reorderPair: SymbolReorderPair) =

    let sym, otherSym = reorderPair.Symbol, reorderPair.OtherSymbol

    let sortByUnwrpPorts (sym: Symbol) (dir: Direction) =
        let unwrppedPorts = unwrapSymPorts reorderPair.DominantEdges[sym.Id] dir sym

        reorderPair.Ports[sym.Id]
        |> List.sortBy (fun port -> List.findIndex (fun id -> id = port.Port.Id) unwrppedPorts)

    let symPorts, otherSymPorts =
        sortByUnwrpPorts sym Clockwise, sortByUnwrpPorts otherSym AntiClockwise

    [ (sym.Id, symPorts); (otherSym.Id, otherSymPorts) ] |> Map.ofList

/// Find reordering that minimizes swaps.
let optSwaps (reorderPair: SymbolReorderPair) =

    let portsOrdered = reorderSymPorts reorderPair
    let portsOrderedRev = portsOrdered |> Map.map (fun _ ports -> List.rev ports) // Use reorderSymPorts? But inefficient.

    let swapsBySym (portsBySym: Map<ComponentId, PortInfo list>) =
        let symId, othId = reorderPair.Symbol.Id, reorderPair.OtherSymbol.Id

        let swapsOfSym (symId: ComponentId) =
            (portsBySym[symId], reorderPair.Ports[symId])
            ||> List.zip
            |> List.map (fun (portA, portB) -> portA.Port.Id, portB.Port.Id)
            |> Map.ofList

        [ swapsOfSym symId; swapsOfSym othId ]
        |> List.zip [ symId; othId ]
        |> Map.ofList

    let countSwaps (swapsBySym: Map<ComponentId, Map<string, string>>) =
        let numSwaps (sym: Symbol) =
            let swaps', _ =
                swapsBySym[sym.Id]
                |> Map.toList
                |> List.partition (fun (portA, portB) -> portA <> portB)

            List.length swaps'

        (numSwaps reorderPair.Symbol) + (numSwaps reorderPair.OtherSymbol)

    // Pick Swaps with fewest orderings.
    [ swapsBySym portsOrdered; swapsBySym portsOrderedRev ]
    |> List.map (fun swaps -> (countSwaps swaps, swaps)) // Keep as tuple to use count as Hueristic
    |> List.minBy fst

// Swaps around portIds in symToOrder to minimize crossing of wires.
let swapPortIds (reorderPair: SymbolReorderPair) =

    let swapsBySym = optSwaps reorderPair |> snd

    let swapIds (sym: Symbol) =
        let swapsPortIds = swapsBySym[sym.Id]

        let swapId (id: string) =
            Map.tryFind id swapsPortIds |> Option.defaultWith (fun () -> id)

        let updateOrd =
            let newOrder =
                sym.PortMaps.Order |> Map.map (fun _ order -> List.map swapId order)

            set (portMaps_ >-> order_) newOrder

        let updateOri =
            let newOrientation =
                (Map.empty, sym.PortMaps.Orientation)
                ||> Map.fold (fun newOrien' id edge ->
                    let id = swapId id
                    Map.add id edge newOrien')

            set (portMaps_ >-> orientation_) newOrientation

        (updateOrd >> updateOri) sym

    [ swapIds reorderPair.Symbol; swapIds reorderPair.OtherSymbol ]

/// Flips Mux Select
/// If all its Inputs are connected to the same component and Sel is on same Edge as its connected Port.
let flipMuxSel (model: BusWireT.Model) (sym: Symbol) (othSym: Symbol) =

    /// Sets of components to flip select.
    let symMatch (sym: Symbol) =
        match sym.Component.Type with
        | Mux2 | Mux4 | Mux8
        | Demux2 | Demux4 | Demux8 -> Mux2
        | otherCompType -> otherCompType

    /// Gets Ports connected to Mux/Demux's Inputs
    let muxInpConn (mux: Symbol) (othSym: Symbol) =
        let portInfoFrmWire (wire: Wire) =
            let portInfo (portId: string) =
                { Port = getPort model.Symbol portId
                  Orientation = getPortOrientationFrmPortIdStr model.Symbol portId }

            let inpPort, outPort =
                getInputPortIdStr wire.InputPort, getOutputPortIdStr wire.OutputPort

            portInfo inpPort, portInfo outPort

        let inpPorts = mux.Component.InputPorts |> List.map (fun port -> port.Id)

        connsBtwnSyms model mux othSym
        |> Map.filter (fun _ wire -> List.contains (getInputPortIdStr wire.InputPort) inpPorts)
        |> Map.toList
        |> List.map (snd >> portInfoFrmWire)

    /// Gets Mux Sel Port.
    let muxSelPort (mux: Symbol) =
        mux.PortMaps.Order
        |> Map.pick (fun edge ports ->
            match List.length ports = 0 with // Sel is always opposite an Edge with no Ports.
            | true -> Some mux.PortMaps.Order[edge.Opposite]
            | _ -> None)
        |> List.head

    /// Flips Mux Select
    let flipSel (mux: Symbol) (othSym: Symbol) =

        let muxInpConns = muxInpConn mux othSym
        let muxSelConns = 
            muxInpConns |> List.tryFind (fun (muxP, _) -> muxP.Port.Id = muxSelPort mux)

        match muxInpConns.Length = mux.Component.InputPorts.Length, muxSelConns with // All input ports are connected.
        | true, Some(muxSelP, othConP) when muxSelP.Orientation = othConP.Orientation -> // Sel Port is on same Edge as Input Port.
            /// Update PortMap Orientation
            let updateOri =
                let portId, portOri = muxSelP.Port.Id, muxSelP.Orientation

                mux.PortMaps.Orientation
                |> Map.add portId portOri.Opposite
                |> set (portMaps_ >-> orientation_)

            /// Update PortMap Order
            let updateOrd =
                let portId, portOri = muxSelP.Port.Id, muxSelP.Orientation

                mux.PortMaps.Order
                |> Map.add portOri.Opposite [ portId ]
                |> Map.change portOri (fun ports ->
                    match ports with
                    | Some ports' -> List.filter (fun id -> id <> portId) ports' |> Some
                    | _ -> None)
                |> set (portMaps_ >-> order_)

            (updateOrd >> updateOri) mux, othSym
        | _ -> mux, othSym

    match symMatch sym, symMatch othSym with
    | Mux2, _ -> flipSel sym othSym
    | _, Mux2 -> flipSel othSym sym
    | _ -> sym, othSym

/// Reorders ports so interconnecting wires do not cross.
let reOrderPorts
    (wModel: BusWireT.Model)
    (symToOrder: Symbol)
    (otherSym: Symbol)
    : BusWireT.Model =

    printfn $"ReorderPorts: ToOrder:{symToOrder.Component.Label}, Other:{otherSym.Component.Label}"

    let updatedSymbols =
        flipMuxSel wModel symToOrder otherSym ||> symReorderPair wModel |> swapPortIds

    let updatedModel =
        let model = updateModelSymbols wModel updatedSymbols

        (model, updatedSymbols)
        ||> List.fold (fun model' symbol -> SmartWire.updateSymbolWires model' symbol.Id)

    updatedModel
