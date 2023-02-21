module SmartPortOrder
open Elmish
open Fable.React.Props
open CommonTypes
open Fable.React
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Symbol
open Optics
open Operators
open BusWireUpdateHelpers

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

// Start of HLP23: SmartPortOrder with X and Y

// let getPort 
//     (symbol: Symbol) 
//     (portIdString: string) 
//         : Port =
//     let portId = PortId (portIdString)
//     match symbol.Component.getPort portId with 
//     | Some p -> p
//     | _ -> failwith "Port not found"
    

// let sortWires 
//     (symbol: Symbol)
//     (orientationEdge: Edge)
//     (wireUtilList: (XYPos*string) list)
//         :  (XYPos*string) list =
//     if orientationEdge = Left || orientationEdge = Right then
//         wireUtilList |> List.sortBy (fun (x,y)->x.Y)
//     else
//         wireUtilList |> List.sortBy (fun (x,y)->x.X)


// let checkInputOutput  
//     (symbol: Symbol) 
//     (wireInputPortsList : (XYPos*InputPortId) list)
//     (wireOutputPortsList : (XYPos*OutputPortId) list) 
//         :(XYPos*string) list  =
//     if symbol.PortMaps.Orientation |> Map.containsKey $"{(snd wireInputPortsList[0])}" then
//         printf "wireInputPortsList: %A" wireInputPortsList
//         wireInputPortsList 
//         |> List.map (fun (x,y)-> (x,$"{y}"))
//     else
//         printf "wireOutputPortsList: %A" wireOutputPortsList
//         wireOutputPortsList 
//         |> List.map (fun (_,y)->(getPortPos symbol (getPort symbol $"{y}") ,$"{y}"))

// let listtoMap lst =
//         lst
//         |> Map.toSeq
//         |> Seq.map (fun (_, v) -> v)
//         |> List.ofSeq 
        



// let reOrderPorts 
//     (wModel: BusWireT.Model) 
//     (symbolToOrder: Symbol) 
//     (otherSymbol: Symbol) 
//         : BusWireT.Model =
//     printfn $"ReorderPorts: ToOrder:{symbolToOrder.Component.Label}, Other:{otherSymbol.Component.Label}"
//     let sModel = wModel.Symbol

//     //NEED TO CHECK IF WE WANT TO USE INPUT OR OUTPUT PORTS DEPENDING ON WMODEL WIRES

//     //Map of WireId to startposition* inputportID
//     let wireUtilMap1 = wModel.Wires |> Map.map (fun k v -> (v.StartPos, v.InputPort))
//     //Map of WireId to OutputPortId
//     let wireUtilMap2 = wModel.Wires |> Map.map (fun k v -> (v.StartPos,v.OutputPort))

//     //List of WireIds
    // let connectionIdList =
    //     wModel.Wires
    //     |> Map.toSeq
    //     |> Seq.map (fun (v, _) ->v )
    //     |> List.ofSeq

//     // List of wires
//     let wireList = listtoMap wModel.Wires

//     //List of InputportIDs
//     let wireInputPortsList =  listtoMap wireUtilMap1

//     //List of OutputportIDs
//     let wireOutputPortsList = listtoMap wireUtilMap2


//     let symbolToOrderPortList = checkInputOutput symbolToOrder wireInputPortsList wireOutputPortsList
//     let otherSymbolPortList = checkInputOutput otherSymbol wireInputPortsList wireOutputPortsList
//     printf "symbolToOrderPortList: %A" symbolToOrderPortList
            
//     // otherSymbol.PortMaps.Orientation |> Map.iter (fun k v -> printfn "%A -> %A" k v)
    

//     // Edge location of ports on other Symbol
//     let otherSymbolEdge = otherSymbol.PortMaps.Orientation |> Map.find (snd otherSymbolPortList[0])

//     //Edge location of ports on SymbolToOrder
//     let symbolToOrderEdge = symbolToOrder.PortMaps.Orientation|>Map.find (snd symbolToOrderPortList[0])

//     //Sorting the wires so that we get the sorted ports
//     let sortedwires = sortWires otherSymbol otherSymbolEdge symbolToOrderPortList

//     //Edge location of ports on SymbolToOrder

//     //List of the sorted ports
//     let mapPortsValtoOrder =
//         sortedwires 
//         |> List.map (fun (a,b)-> b)

//     // Adding the ports that do not have wires connected to them
//     let newList = List.fold (fun acc x -> if List.contains x mapPortsValtoOrder then acc else x::acc) mapPortsValtoOrder (symbolToOrder.PortMaps.Order |> Map.find symbolToOrderEdge)

//     //Adding our new order to the Order map in our Symbol
//     let mapPorts = symbolToOrder.PortMaps.Order |> Map.add symbolToOrderEdge newList

//     mapPorts |> Map.iter (fun k v -> printfn "%A -> %A" k v)



//     let portMapsReplace = {symbolToOrder.PortMaps with Order= mapPorts} // replace this with correct wires
//     let symbol' = {symbolToOrder with PortMaps = portMapsReplace} // no change at the moment

//     // Rerouting the wires with our new model
//     let wiresToOrder = List.map (autoroute {wModel with Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}}) wireList

//     // getting the new wires with their Ids
//     let ListWires = List.zip connectionIdList wiresToOrder
//     // HLP23: This could be cleaned up using Optics - see SmartHelpers for examples
//     {wModel with 
//         Wires = Map.ofList ListWires // no change for now, but probably this function should use update wires after reordering.
//                              // to make that happen the test function which calls this would need to provide an updateWire
//                              // function to this as a parameter (as was done in Tick3)
//         Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
//     }

// End of HLP23: SmartPortOrder with X and Y

// Start of HLP23: SmartPortOrder with order list 

let checkInputOutput 
    (symbolToOrder : Symbol)
    (connectedPorts : (InputPortId*OutputPortId) list)
        :  (string*string) list =
    if symbolToOrder.PortMaps.Orientation |> Map.containsKey ($"{fst connectedPorts[0]}") then
        connectedPorts 
        |> List.map (fun (x,y)-> ($"{y}",$"{x}"))
    else 
        connectedPorts 
        |> List.map (fun (x,y)-> ($"{x}",$"{y}"))

let reorderPorts 
    (list1 : (string*string) list) 
    (list2 : string list)
        : (string*string) list=
    list1
    |> List.sortBy (fun (x,_) -> List.findIndex (fun s -> s = x) list2)


let reOrderPorts 
    (wModel: BusWireT.Model) 
    (symbolToOrder: Symbol) 
    (otherSymbol: Symbol) 
        : BusWireT.Model =
    printfn $"ReorderPorts: ToOrder:{symbolToOrder.Component.Label}, Other:{otherSymbol.Component.Label}"
    let sModel = wModel.Symbol

    let connectionIdList =
        wModel.Wires
        |> Map.toSeq
        |> Seq.map (fun (v, _) ->v )
        |> List.ofSeq

    let wireList =
        wModel.Wires
        |> Map.toSeq
        |> Seq.map (fun (_, v) -> v)
        |> List.ofSeq

    let connectedPorts = 
        wModel.Wires
        |> Map.toSeq
        |> Seq.map (fun (_, v) -> (v.InputPort, v.OutputPort))
        |> List.ofSeq
    
    let PortsToReorder = checkInputOutput symbolToOrder connectedPorts

    let orientationOtherSymbol = 
        otherSymbol.PortMaps.Orientation 
        |> Map.find (fst PortsToReorder[0])
    
    let orientationSymbolToOrder = 
        symbolToOrder.PortMaps.Orientation 
        |> Map.find (snd PortsToReorder[0])
    
    let ReorderedPorts = reorderPorts PortsToReorder (otherSymbol.PortMaps.Order |> Map.find orientationOtherSymbol)
                        |> List.map (fun (_,y)-> y)

    let NewPorts = List.fold (fun acc x -> if List.contains x ReorderedPorts then acc else x::acc) ReorderedPorts (symbolToOrder.PortMaps.Order |> Map.find orientationSymbolToOrder) |> List.rev

    let NewPortsMap = symbolToOrder.PortMaps.Order |> Map.add orientationSymbolToOrder NewPorts

    let portMapsReplace = {symbolToOrder.PortMaps with Order= NewPortsMap} // replace this with correct wires
    let symbol' = {symbolToOrder with PortMaps = portMapsReplace} // no change at the moment

    // Rerouting the wires with our new model
    let wiresToOrder = List.map (autoroute {wModel with Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}}) wireList

    // getting the new wires with their Ids
    let ListWires = List.zip connectionIdList wiresToOrder
    // HLP23: This could be cleaned up using Optics - see SmartHelpers for examples
    {wModel with 
        Wires = Map.ofList ListWires // no change for now, but probably this function should use update wires after reordering.
                             // to make that happen the test function which calls this would need to provide an updateWire
                             // function to this as a parameter (as was done in Tick3)
        Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
    }
