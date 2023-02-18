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
/// Record containing External Helpers required. 
type ExternalSmartHelpers =
    { UpdateSymbolWires: Model -> ComponentId -> Model }

/// Holds meta data about port. This includes the Port, its Edge and Location.
type PortInfo = { Port: Port; Orientation: Edge }

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

/// Gets port info from wires that are connected to two given symbols.
let getPortsBtwnSyms (model: BusWireT.Model) (symToOrder: Symbol) (otherSym: Symbol) =
    let ports =
        getConnBtwnSyms model symToOrder otherSym
        |> List.map (fun wire ->
            [ getPortInfo model.Symbol (InputId wire.InputPort)
              getPortInfo model.Symbol (OutputId wire.OutputPort) ])
        |> List.concat

    let fiterPortInfoBySym (symbol: Symbol) =
        ports |> List.filter (fun port -> ComponentId port.Port.HostId = symbol.Id)

    (fiterPortInfoBySym symToOrder, fiterPortInfoBySym otherSym)

/// Unwraps a symbol's port in either a Clockwise or AntiClockwise Order starting from a symbol's Top Edge.
let unwrapSymPortsClkwise (direction: Direction) (symbol: Symbol) =
    let order = symbol.PortMaps.Order

    match direction with
    | Clockwise -> order[Top] @ order[Right] @ order[Bottom] @ order[Left]
    | _ -> order[Right] @ order[Bottom] @ order[Left] @ order[Top] |> List.rev

/// Computes port swaps on symbol to reorder ports on. Port swaps are represented as a Map.
/// Keys and values are old ports and new ports respectively on the symbol to order.
let getPortSwaps (model: BusWireT.Model) (symToOrder: Symbol) (otherSym: Symbol) =

    let symToOrderPorts, otherSymPorts = getPortsBtwnSyms model symToOrder otherSym

    let sortSymPorts (dir: Direction) (ports: PortInfo list) (symbol: Symbol) =
        let unwrapSymPorts = unwrapSymPortsClkwise dir symbol

        ports
        |> List.sortBy (fun port -> List.findIndex (fun id -> id = port.Port.Id) unwrapSymPorts)

    let zipPorts (portsA: PortInfo list) (portsB: PortInfo list) =
        (portsA, portsB) ||> List.zip |> Map.ofList

    let makeConnections (order: Option<Direction>) =
        match order with
        | Some dir ->
            (sortSymPorts dir otherSymPorts otherSym, sortSymPorts dir.Opposite symToOrderPorts symToOrder)
            ||> zipPorts
        | None -> (otherSymPorts, symToOrderPorts) ||> zipPorts

    let oldConns = makeConnections None
    let newConns = makeConnections (Some Clockwise)

    (Map.empty, newConns)
    ||> Map.fold (fun state src oldPort -> Map.add oldPort oldConns[src] state)

/// Swaps around portIds in symToOrder to minimize criss crossing of wires.
/// Keys and values of swaps represents old ports and new ports respectively.
let swapPortIds (symToOrder: Symbol) (swaps: Map<PortInfo, PortInfo>) =

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

    (updatePortMapOrder >> updatePortMapOrientation) symToOrder


/// Reorders ports so interconnecting wires do not cross.
let reOrderPorts
    (wModel: BusWireT.Model)
    (symToOrder: Symbol)
    (otherSym: Symbol)
    (smartHelpers: ExternalSmartHelpers)
    : BusWireT.Model =

    printfn $"ReorderPorts: ToOrder:{symToOrder.Component.Label}, Other:{otherSym.Component.Label}"

    let model' =
        getPortSwaps wModel symToOrder otherSym
        |> swapPortIds symToOrder
        |> List.singleton
        |> updateModelSymbols wModel

    smartHelpers.UpdateSymbolWires model' symToOrder.Id
