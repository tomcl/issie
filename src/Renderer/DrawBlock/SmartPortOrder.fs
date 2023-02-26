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

/// Holds data about External Helpers required.
type ExternalSmartHelpers =
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

/// Gets port metadata which includes the port itself and Orientation.
let getPortInfo (model: SymbolT.Model) (portId: PortId) =
    { Port = getPort model (getPortIdStr portId)
      Orientation = getPortOrientation model portId }

/// Finds the Dominant Edge of a Symbol given a subset of its ports.
/// A dominant edge is defined as an edge where a symbol has a port but no port on its opposite edge.
/// It's the direction a symbol faces.
let getSymDominantEdge (symPorts: PortInfo list) =
    let edgeExists (edge: Edge) =
        symPorts |> List.exists (fun port -> port.Orientation = edge)

    match symPorts with
    | [] -> Left // Default to a Left Dominant Edge.
    | hd :: _ ->
        let port =
            symPorts
            |> List.tryFind (fun port -> not (edgeExists port.Orientation.Opposite))
            |> Option.defaultValue hd

        port.Orientation

/// Unwrap a symbol's port in a Clockwise or AntiClockwise given a dominant edge.
/// Eg. For a Top Dominant Edge AntiClockwise ordering, we order the ports from Right, Top, Left then Bottom Edge.
let unwrapSymPorts (domEdge: Edge) (direction: Direction) (sym: Symbol) =

    let order = sym.PortMaps.Order

    // Rotates the starting edge of an AntiClockwise Port Ordering Clockwise
    let rotClkwise (n: int) =
        ([ order[Right] ] @ [ order[Top] ] @ [ order[Left] ] @ [ order[Bottom] ])
        |> List.permute (fun idx -> (idx + n) % 4)

    // Gets a Clockwise Port Reordering for a Dominant Edge Anticlockwise Ordering
    let revDirection (ports: string list list) =
        let frnt, back = List.splitAt (List.length ports - 1) ports
        (List.rev frnt) @ back

    let unwrpByDirection (n: int) =
        match direction with
        | AntiClockwise -> rotClkwise n |> List.concat
        | Clockwise -> rotClkwise n |> revDirection |> List.map List.rev |> List.concat

    match domEdge with
    | Top -> unwrpByDirection 0
    | Right -> unwrpByDirection 1
    | Bottom -> unwrpByDirection 2
    | Left -> unwrpByDirection 3


/// Gets Symbol Reordering Information.
let getSymReorderPair (model: BusWireT.Model) (symToOrder: Symbol) (otherSym: Symbol) =
    let getPortInfo (ports: Port list) =
        ports
        |> List.map (fun port ->
            { Port = port
              Orientation = getPortOrientationFrmPortIdStr model.Symbol port.Id })

    let zipToMap (values: 'a list) =
        values |> List.zip [ symToOrder.Id; otherSym.Id ] |> Map.ofList

    let portBySym =
        let symToOrderPorts, otherSymPorts = getPortsBtwnSyms model symToOrder otherSym

        [ symToOrderPorts; otherSymPorts ] |> List.map getPortInfo |> zipToMap

    let domEdgeBySym =
        [ portBySym[symToOrder.Id]; portBySym[otherSym.Id] ]
        |> List.map getSymDominantEdge
        |> zipToMap

    { Symbol = symToOrder
      OtherSymbol = otherSym
      Ports = portBySym
      DominantEdges = domEdgeBySym }

/// Guard that ensures ports of non custom components are not reordered.
/// Done by removing ports of symbol to order if its not a custom component.
let rmvNonCustomCompPorts (reorderPair: SymbolReorderPair) =
    match reorderPair.Symbol.Component.Type with
    | Custom _ -> reorderPair
    | _ -> { reorderPair with Ports = reorderPair.Ports |> Map.add reorderPair.Symbol.Id [] }

/// Reorder's Symbol Ports such to minimize wire crossings.
/// To minimize wire crossings ports of one symbol must be the reverse of ports on another symbol.
/// We achieve this by unwrapping ports of a symbol by their dominant edge in opposite directions.
let reorderSymPorts (reorderPair: SymbolReorderPair) =

    let sortByUnwrpPorts (sym: Symbol) (dir: Direction) =
        let unwrppedPorts = unwrapSymPorts reorderPair.DominantEdges[sym.Id] dir sym

        reorderPair.Ports[sym.Id]
        |> List.sortBy (fun port -> List.findIndex (fun id -> id = port.Port.Id) unwrppedPorts)

    let symPorts, otherSymPorts =
        sortByUnwrpPorts reorderPair.Symbol Clockwise, sortByUnwrpPorts reorderPair.OtherSymbol AntiClockwise

    [ (reorderPair.Symbol.Id, symPorts)
      (reorderPair.OtherSymbol.Id, otherSymPorts) ]
    |> Map.ofList

/// Computes port swaps on symbol to reorder ports on. Port swaps are represented as a Map.
/// Keys and values are old ports and new ports respectively on the symbol to order.
let getPortSwaps (reorderPair: SymbolReorderPair) =

    let zipPorts (portsA: PortInfo list) (portsB: PortInfo list) =
        (portsA, portsB) ||> List.zip |> Map.ofList

    let getConnections (ports: Map<ComponentId, PortInfo list>) =
        (ports[reorderPair.OtherSymbol.Id], ports[reorderPair.Symbol.Id]) ||> zipPorts

    let oldConns, newConns =
        getConnections reorderPair.Ports, getConnections (reorderSymPorts reorderPair)

    (Map.empty, newConns)
    ||> Map.fold (fun state src oldPort -> Map.add oldPort oldConns[src] state)

/// Swaps around portIds in symToOrder to minimize criss crossing of wires.
/// Keys and values of swaps represents old ports and new ports respectively.
let swapPortIds (reorderPair: SymbolReorderPair) =

    let swaps = getPortSwaps reorderPair

    let swapsPortIds =
        (Map.empty, swaps)
        ||> Map.fold (fun swaps' oldPort newPort -> Map.add oldPort.Port.Id newPort.Port.Id swaps')

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

    (updatePortMapOrder >> updatePortMapOrientation) reorderPair.Symbol


/// Reorders ports so interconnecting wires do not cross.
let reOrderPorts
    (wModel: BusWireT.Model)
    (symToOrder: Symbol)
    (otherSym: Symbol)
    (smartHelpers: ExternalSmartHelpers)
    : BusWireT.Model =

    printfn $"ReorderPorts: ToOrder:{symToOrder.Component.Label}, Other:{otherSym.Component.Label}"

    let model' =
        getSymReorderPair wModel symToOrder otherSym
        |> rmvNonCustomCompPorts
        |> swapPortIds
        |> List.singleton
        |> updateModelSymbols wModel

    smartHelpers.UpdateSymbolWires model' symToOrder.Id
