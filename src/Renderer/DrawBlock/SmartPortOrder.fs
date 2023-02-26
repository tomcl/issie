module SmartPortOrder
open BusWireUpdateHelpers
open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
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

/// To test this, it must be given two symbols interconnected by wires. It then reorders the ports on
/// symbolToOrder so that the connecting wires do not cross.
/// Tt should work out the interconnecting wires (wiresToOrder) from
////the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has re-orderable ports).
let reOrderPorts (wModel: BusWireT.Model) (symbolToOrder: Symbol) (otherSymbol: Symbol) : BusWireT.Model =
    printfn $"ReorderPorts: ToOrder:{symbolToOrder.Component.Label}, Other:{otherSymbol.Component.Label}"
    let sModel = wModel.Symbol
    printfn $"Input ports:{symbolToOrder.Component.InputPorts}, Output ports:{symbolToOrder.Component.OutputPorts}"
    let wiresToOrder1 = getConnectedWires wModel [ symbolToOrder.Id ] |> Set
    let wiresToOrder2 = getConnectedWires wModel [ otherSymbol.Id ] |> Set
    let wiresToOrder = Set.intersect wiresToOrder1 wiresToOrder2 |> Set.toList

    let portConnections =
        wiresToOrder
        |> List.map (fun x -> (x.OutputPort, x.InputPort))
    let list1, list2 = portConnections |> List.unzip

    let symbolAPortMap =
        otherSymbol.Component.OutputPorts
        |> List.map (fun port -> (port.Id, port.PortNumber))
        |> Map.ofList
    let symbolBPortMap =
        symbolToOrder.Component.InputPorts
        |> List.map (fun port -> (port.Id, port.PortNumber))
        |> Map.ofList

    let stringPortConnections =
        portConnections
        |> List.map (fun (outputId, inputId) -> (inputId.ToString(), outputId.ToString()))

    let PortNumberConnections =
        stringPortConnections
        |> List.map (fun (inputId, outputId) ->
            let inputPortNumber = symbolBPortMap.[inputId]
            let outputPortNumber = symbolAPortMap.[outputId]
            (outputPortNumber, inputPortNumber))
        |> List.map (fun (x, y) -> (x |> Option.defaultValue 0, y |> Option.defaultValue 0))
        |> List.sortBy fst
        |> List.map snd
        
    let reorderList (strList: string list) (intList: int list) =
        match strList.Length with
                | 0 -> strList
                | _ -> List.map (fun index -> strList.[index]) intList

    let updatedMapOrder =
        symbolToOrder.PortMaps.Order
        |> Map.map (fun _ L -> reorderList L PortNumberConnections)
    printfn $"updatedMapOrder:{updatedMapOrder}"
    let updatedPortMaps = { symbolToOrder.PortMaps with Order = updatedMapOrder }
    let updatedSymbol = { symbolToOrder with PortMaps = updatedPortMaps }

    let symbol' = updatedSymbol // no change at the moment
    { wModel with
        Wires = wModel.Wires // no change for now, but probably this function should use update wires after reordering.
        Symbol = { sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols } }
