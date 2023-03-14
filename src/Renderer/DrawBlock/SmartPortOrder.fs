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
        - Only Components with 2 IO ports are ordered (eg. Mux2, DeMux2, etc).    
    4) Flip the Select of Mux/Demux.
        - We flip the select if its connected to a port on another component on the same edge.
*)

/// Holds data about External Helpers required.
type ExternalSmartHelpers = // TODO: Transfer to SmartHelpers.
    { UpdateSymbolWires: Model -> ComponentId -> Model }

/// Holds data about Port, its Edge and Location.
type PortInfo = { Port: Port; Orientation: Edge }

/// Holds data about Symbol pairs we compute port reorderings.
type SymbolReorderPair =
    { Symbol: Symbol
      OtherSymbol: Symbol
      Ports: Map<ComponentId, PortInfo list>
      DominantEdges: Map<ComponentId, Edge> }

/// Holds possible directions to sort ports.
type Direction =
    | Clockwise
    | AntiClockwise

    member this.Opposite =
        match this with
        | Clockwise -> AntiClockwise
        | _ -> Clockwise

/// Finds the Dominant Edge of a Symbol.
let getSymDominantEdge (symPorts: PortInfo list) =
    let edgeExists (edge: Edge) =
        symPorts |> List.exists (fun port -> port.Orientation = edge)

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

    // Rotates the starting edge of an AntiClockwise Port Ordering Clockwise
    let rotClkwise (n: int) =
        ([ order[Right] ] @ [ order[Top] ] @ [ order[Left] ] @ [ order[Bottom] ])
        |> List.permute (fun idx -> (idx + n) % 4)

    // Gets a Clockwise Port Reordering for a Dominant Edge Anticlockwise Ordering
    let revDirection (ports: string list list) =
        let frnt, back = List.splitAt (List.length ports - 1) ports
        (List.rev frnt) @ back |> List.map List.rev |> List.concat

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
let getPortsBtwnSyms (model: BusWireT.Model) (sym: Symbol) (otherSym: Symbol) =

    let keepWire (wire: Wire) =
        let getPortSymbol (portId: string) =
            match isPortInSymbol portId sym with
            | true -> sym
            | _ -> otherSym

        let keepPort (portId: string) =
            let sym = getPortSymbol portId

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

    let paritionPortsBySym (ports: Port list) =
        ports |> List.partition (fun port -> ComponentId port.HostId = sym.Id)

    let getPortInfos (portsByNet: Port list list) =
        portsByNet
        |> List.concat
        |> List.map (fun port ->
            { Port = port
              Orientation = getPortOrientationFrmPortIdStr model.Symbol port.Id })

    let symPorts, otherSymPorts =
        getConnBtwnSyms model sym otherSym
        |> Map.filter (fun _ wire -> keepWire wire)
        |> partitionWiresByNet
        |> List.map (getPortsFrmWires model >> paritionPortsBySym)
        |> List.unzip

    [ getPortInfos symPorts; getPortInfos otherSymPorts ]
    |> List.zip [ sym.Id; otherSym.Id ]
    |> Map.ofList

/// Gets Symbol Reordering Information.
let getSymReorderPair (model: BusWireT.Model) (sym: Symbol) (otherSym: Symbol) =

    let portsBySym = getPortsBtwnSyms model sym otherSym

    let edgesBySym =
        [ portsBySym[sym.Id]; portsBySym[otherSym.Id] ]
        |> List.map getSymDominantEdge
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
let getOptSwaps (reorderPair: SymbolReorderPair) =

    let portsOrdered = reorderSymPorts reorderPair
    let portsOrderedRev = portsOrdered |> Map.map (fun _ ports -> List.rev ports) // Use reorderSymPorts? But inefficient.

    let getSwapsBySym (portsBySym: Map<ComponentId, PortInfo list>) =
        let symId, othId = reorderPair.Symbol.Id, reorderPair.OtherSymbol.Id

        let getSwapsForSym (symId: ComponentId) =
            (portsBySym[symId], reorderPair.Ports[symId])
            ||> List.zip
            |> List.map (fun (portA, portB) -> portA.Port.Id, portB.Port.Id)
            |> Map.ofList

        [ getSwapsForSym symId; getSwapsForSym othId ]
        |> List.zip [ symId; othId ]
        |> Map.ofList

    let countSwaps (swapsBySym: Map<ComponentId, Map<string, string>>) =
        let countSwapsBySym (sym: Symbol) =
            let swaps', _ =
                swapsBySym[sym.Id]
                |> Map.toList
                |> List.partition (fun (portA, portB) -> portA <> portB)

            List.length swaps'

        (countSwapsBySym reorderPair.Symbol) + (countSwapsBySym reorderPair.OtherSymbol)

    // Pick Swaps with fewest orderings.
    [ getSwapsBySym portsOrdered; getSwapsBySym portsOrderedRev ]
    |> List.map (fun swaps -> (countSwaps swaps, swaps)) // Keep as tuple to use count as Hueristic
    |> List.minBy fst

// Swaps around portIds in symToOrder to minimize crossing of wires.
let swapPortIds (reorderPair: SymbolReorderPair) =

    let swapsBySym = getOptSwaps reorderPair |> snd

    let swapIds (sym: Symbol) =
        let swapsPortIds = swapsBySym[sym.Id]

        let tryFindSwapId (id: string) =
            Map.tryFind id swapsPortIds |> Option.defaultWith (fun () -> id)

        let updatePortMapOrder (symbol: Symbol) =
            let newOrder =
                symbol.PortMaps.Order |> Map.map (fun _ order -> List.map tryFindSwapId order)

            Optic.set (portMaps_ >-> order_) newOrder symbol

        let updatePortMapOrientation (symbol: Symbol) =
            let newOrientation =
                (Map.empty, symbol.PortMaps.Orientation)
                ||> Map.fold (fun newOrien' id edge ->
                    let id = tryFindSwapId id
                    Map.add id edge newOrien')

            Optic.set (portMaps_ >-> orientation_) newOrientation symbol

        (updatePortMapOrder >> updatePortMapOrientation) sym

    [ swapIds reorderPair.Symbol; swapIds reorderPair.OtherSymbol ]

/// Flips Mux Select.
let flipMuxSel (model: BusWireT.Model) (switch: bool) (sym: Symbol) =

    /// Sets of components to flip select.
    let symMatch (sym: Symbol) =
        match sym.Component.Type with
        | Mux2 | Mux4 | Mux8
        | Demux2 | Demux4 | Demux8 -> Mux2
        | otherCompType -> otherCompType

    /// Gets Wire Connected to Mux Sel and its Ports.
    let getMuxSelPorts (sym: Symbol) =
        let muxSelEdge =
            sym.PortMaps.Order
            |> Map.pick (fun edge ports -> if List.length ports = 0 then Some edge.Opposite else None)

        let muxSelPort = sym.PortMaps.Order[muxSelEdge] |> List.head

        let muxSelWire =
            model.Wires
            |> Map.filter (fun _ wire -> getInputPortIdStr wire.InputPort = muxSelPort)
            |> Map.toList
            |> List.tryHead

        match muxSelWire with
        | Some(_, wire) -> Some(getInputPortIdStr wire.InputPort, getOutputPortIdStr wire.OutputPort)
        | _ -> None

    /// Moves Mux Sel if Wire exists.
    let moveMuxSel (sym: Symbol) =

        let updateOri (portId: string) (ori: Edge) =
            sym.PortMaps.Orientation
            |> Map.add portId ori
            |> Optic.set (portMaps_ >-> orientation_)

        let updateOrd (portId: string) (ori: Edge) =
            sym.PortMaps.Order
            |> Map.add ori [ portId ]
            |> Map.change ori.Opposite (fun ports ->
                match ports with
                | Some ports' -> List.filter (fun id -> id <> portId) ports' |> Some
                | _ -> None)
            |> Optic.set (portMaps_ >-> order_)

        match getMuxSelPorts sym with
        | Some(selPort, othPort) ->
            let selOrie, othOrie =
                getPortOrientationFrmPortIdStr model.Symbol selPort, getPortOrientationFrmPortIdStr model.Symbol othPort

            match (selOrie = othOrie) && switch with // Only update when Switch enabled.
            | false -> sym
            | _ -> (updateOrd selPort selOrie.Opposite >> updateOri selPort selOrie.Opposite) sym
        | None -> sym

    match symMatch sym with
    | Mux2 -> moveMuxSel sym
    | _ -> sym

/// Reorders ports so interconnecting wires do not cross.
let reOrderPorts
    (wModel: BusWireT.Model)
    (symToOrder: Symbol)
    (otherSym: Symbol)
    (muxFlipSwitch: bool)
    (smartHelpers: ExternalSmartHelpers)
    : BusWireT.Model =

    printfn $"ReorderPorts: ToOrder:{symToOrder.Component.Label}, Other:{otherSym.Component.Label}"

    let updatedSymbols =
        getSymReorderPair wModel symToOrder otherSym
        |> swapPortIds
        |> List.map (flipMuxSel wModel muxFlipSwitch)   

    let updatedModel =
        let model = updateModelSymbols wModel updatedSymbols

        (model, updatedSymbols)
        ||> List.fold (fun model' symbol -> smartHelpers.UpdateSymbolWires model' symbol.Id)

    updatedModel
