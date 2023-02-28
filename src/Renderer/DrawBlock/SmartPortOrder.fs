module SmartPortOrder
open System
open BusWireUpdateHelpers
open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Optics
open Operators
open BusWireUpdate

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
let reOrderPorts (wModel: BusWireT.Model) (symbolToOrder: Symbol) (otherSymbol: Symbol): BusWireT.Model =
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
                    |> List.map (fun port -> (port.Id, port.PortNumber |> Option.defaultValue 0))
                    |> Map.ofList
            | false -> otherSymbol.Component.InputPorts
                    |> List.map (fun port -> (port.Id, port.PortNumber |> Option.defaultValue 0))
                    |> Map.ofList
        
    let symbolBPortMap =
        match symbolToOrder.Pos.X > otherSymbol.Pos.X with
            | true -> symbolToOrder.Component.InputPorts
                    |> List.map (fun port -> (port.Id, port.PortNumber |> Option.defaultValue 0))
                    |> Map.ofList
            | false -> symbolToOrder.Component.OutputPorts
                    |> List.map (fun port -> (port.Id, port.PortNumber |> Option.defaultValue 0))
                    |> Map.ofList

        
    printfn $"SymbolBPortMap:{symbolBPortMap}"
    printfn $"SymbolAPortMap:{symbolAPortMap}"
    
    let reverseTuple tupleList =
        tupleList
        |> List.map (fun (x,y) -> (y,x))
    
    let getConnectedNumbers (map1: Map<string, int>) (map2: Map<string, int>) (connections: (string * string) list) : (int * int option) list =
        match symbolToOrder.Pos.X > otherSymbol.Pos.X with
            | true -> [for kvp1 in map1 do
                            match List.tryFind (fun (id1, id2) -> id1 = kvp1.Key) connections with
                            | Some (id1, id2) ->
                                match map2.TryFind(id2) with
                                | Some (int2) -> yield (kvp1.Value, Some(int2))
                                | None -> yield (kvp1.Value, None)
                            | None -> yield (kvp1.Value, None)
                        ]
                        |> List.sortBy fst
            | false -> [for kvp1 in map2 do
                            match List.tryFind (fun (id1, id2) -> id2 = kvp1.Key) (reverseTuple connections) with
                            | Some (id1, id2) ->
                                match map1.TryFind(id1) with
                                | Some (int2) -> yield (kvp1.Value, Some(int2))
                                | None -> yield (kvp1.Value, None)
                            | None -> yield (kvp1.Value, None)
                        ]
                        |> List.sortBy fst

    let map1 = Map.ofList [("a", 0); ("b", 1); ("c", 2)]
    let map2 = Map.ofList [("x", 0); ("y", 1); ("z", 2)]
    let connections = [("a", "z"); ("b", "x"); ("c", "y")]

    let connectedNumbers = getConnectedNumbers symbolAPortMap symbolBPortMap portConnections
    printfn $"RIGHT HERE:{connectedNumbers}"
    
    let portMap =  Map.find Left symbolToOrder.PortMaps.Order
    printfn $"PORT MAP:{portMap}"
    
    let AtoBConnections=
        match symbolToOrder.Pos.X > otherSymbol.Pos.X with
            | true -> (portConnections
                    |> List.map (fun (outputId, inputId) -> (symbolAPortMap.[outputId], symbolBPortMap.[inputId]))
                    |> List.sortBy fst)
            | false -> (portConnections
                    |> List.map (fun (outputId, inputId) -> (symbolBPortMap.[outputId], symbolAPortMap.[inputId]))
                    |> List.sortBy fst)
 
    let reorderList (portIds: string list) (connections: (int*int option) list) =
        match portIds.Length with
                | 0 -> portIds
                | _ -> List.mapi (fun i (_,index) -> match index with
                                                        | Some int -> portIds.[int]
                                                        | None -> portIds.[i]) connections
                
    printfn $"AtoBConnections:{AtoBConnections}"
    let updatedMapOrder =
        //let portMapRight = Map.find Right symbolToOrder.PortMaps.Order
        //let reorderedListRight = reorderList portMapRight BtoAConnections
        match symbolToOrder.Pos.X > otherSymbol.Pos.X with
            | true -> let portMap =  Map.find Left symbolToOrder.PortMaps.Order
                      let reorderedList = reorderList portMap connectedNumbers
                      symbolToOrder.PortMaps.Order |> Map.add Left reorderedList
                      
            | false -> let portMap =  Map.find Right symbolToOrder.PortMaps.Order 
                       let reorderedList = reorderList portMap connectedNumbers
                       symbolToOrder.PortMaps.Order |> Map.add Right reorderedList
                       
    printfn $"originalMapOrder:{symbolToOrder.PortMaps.Order}"              
    printfn $"updatedMapOrder:{updatedMapOrder}"
    
    
    let updatedPortMaps = { symbolToOrder.PortMaps with Order = updatedMapOrder }
    let updatedSymbol = { symbolToOrder with PortMaps = updatedPortMaps }
    
    //let updateWires (model : Model) (compIdList : ComponentId list) (diff : XYPos) =
    //let autoroute (model: Model) (wire: Wire) : Wire =

        
    let symbol' = updatedSymbol // no change at the moment
    { wModel with
        Wires = wModel.Wires //no change for now, but probably this function should use update wires after reordering.
        Symbol = { sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols } }
    