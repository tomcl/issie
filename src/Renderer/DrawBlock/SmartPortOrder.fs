module SmartPortOrder
open BusWireUpdateHelpers
open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Optics
open Operators
open BusWire

type Tick3BusWireHelpers = {
    AutoRoute: BusWireT.Model -> Wire -> Wire
    ReverseWire: Wire -> Wire
    MoveSegment: Model -> Segment -> float -> Wire
    }

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
let reOrderPorts (wModel: BusWireT.Model) (symbolToOrder: Symbol) (otherSymbol: Symbol) (tick3Helpers: Tick3BusWireHelpers) : BusWireT.Model =
    printfn $"ReorderPorts: ToOrder:{symbolToOrder.Component.Label}, Other:{otherSymbol.Component.Label}"
    let sModel = wModel.Symbol
    printfn $"Input ports:{symbolToOrder.Component.InputPorts}, Output ports:{symbolToOrder.Component.OutputPorts}"
    let wiresToOrder1 = getConnectedWires wModel [ symbolToOrder.Id ] |> Set
    let wiresToOrder2 = getConnectedWires wModel [ otherSymbol.Id ] |> Set
    let wiresToOrder = Set.intersect wiresToOrder1 wiresToOrder2 |> Set.toList
    
    let portConnections =
        wiresToOrder
        |> List.map (fun x -> (x.OutputPort, x.InputPort))
        |> List.map (fun (outputId, inputId) -> (outputId.ToString(), inputId.ToString()))
    printfn $"Port Connections:{portConnections}"
    
    let list1, list2 = portConnections |> List.unzip
    
    let symbolAPortMap =
        match symbolToOrder.Pos.X > otherSymbol.Pos.X with
            | true -> otherSymbol.Component.OutputPorts
                    |> List.map (fun port -> (port.Id, port.PortNumber))
                    |> Map.ofList
            | false -> otherSymbol.Component.InputPorts
                    |> List.map (fun port -> (port.Id, port.PortNumber))
                    |> Map.ofList
        
    let symbolBPortMap =
        match symbolToOrder.Pos.X > otherSymbol.Pos.X with
            | true -> symbolToOrder.Component.InputPorts
                    |> List.map (fun port -> (port.Id, port.PortNumber))
                    |> Map.ofList
            | false -> symbolToOrder.Component.OutputPorts
                    |> List.map (fun port -> (port.Id, port.PortNumber))
                    |> Map.ofList
        
    printfn $"SymbolBPortMap:{symbolBPortMap}"
    printfn $"SymbolAPortMap:{symbolAPortMap}"
    
    let AtoBConnections=
        match symbolToOrder.Pos.X > otherSymbol.Pos.X with
            | true -> (portConnections
                    |> List.map (fun (outputId, inputId) -> (symbolAPortMap.[outputId], symbolBPortMap.[inputId]))
                    |> List.map (fun (x, y) -> (x |> Option.defaultValue 0, y |> Option.defaultValue 0))
                    |> List.sortBy fst
                    |> List.map snd)
            | false -> portConnections
                    |> List.map (fun (outputId, inputId) -> (symbolBPortMap.[outputId], symbolAPortMap.[inputId]))
                    |> List.map (fun (x, y) -> (x |> Option.defaultValue 0, y |> Option.defaultValue 0))
                    |> List.sortBy fst
                    |> List.map snd
            
    printfn $"AtoBConnections:{AtoBConnections}"
    
    (*let BtoAConnections=
        portConnections
        |> List.map (fun (inputId, outputId) ->
            let outputPortNumber = symbolAPortMap.[outputId]
            let inputPortNumber = symbolBPortMap.[inputId]
            (outputPortNumber, inputPortNumber))
        |> List.map (fun (x, y) -> (x |> Option.defaultValue 0, y |> Option.defaultValue 0))
        |> List.sortBy fst
        |> List.map fst*)
 
    let reorderList (strList: string list) (intList: int list) =
        match strList.Length with
                | 0 -> strList
                | _ -> List.map (fun index -> strList.[index]) intList

    let updatedMapOrder =
        
        //let portMapRight = Map.find Right symbolToOrder.PortMaps.Order
        //let reorderedListRight = reorderList portMapRight BtoAConnections
        match symbolToOrder.Pos.X > otherSymbol.Pos.X with
            | true -> let portMap =  Map.find Left symbolToOrder.PortMaps.Order 
                      let reorderedList = reorderList portMap AtoBConnections
                      symbolToOrder.PortMaps.Order |> Map.add Left reorderedList
                      
            | false -> let portMap =  Map.find Right symbolToOrder.PortMaps.Order 
                       let reorderedList = reorderList portMap AtoBConnections
                       symbolToOrder.PortMaps.Order |> Map.add Right reorderedList
    
    printfn $"updatedMapOrder:{updatedMapOrder}"
    printfn $"APortMap:{symbolAPortMap}, BPortMap:{symbolBPortMap}"
    let updatedPortMaps = { symbolToOrder.PortMaps with Order = updatedMapOrder }
    let updatedSymbol = { symbolToOrder with PortMaps = updatedPortMaps }
    
    //let updateWires (model : Model) (compIdList : ComponentId list) (diff : XYPos) =
    //let updatedWires = updateWires wModel [symbolToOrder.Id; otherSymbol.Id] 
    let symbol' = updatedSymbol // no change at the moment
    { wModel with
        Wires = wModel.Wires // no change for now, but probably this function should use update wires after reordering.
        Symbol = { sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols } }