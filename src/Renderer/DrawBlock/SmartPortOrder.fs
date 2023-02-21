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


// Start of HLP23: SmartPortOrder with order list 

let checkInputOutput 
    (symbolToOrder : Symbol)
    (connectedPorts : (InputPortId*OutputPortId) list)
        :  (string*string) list =
    connectedPorts
    |> List.map (fun x  -> 
                match symbolToOrder.PortMaps.Orientation |> Map.containsKey ($"{fst x}") with 
                | true -> ($"{snd x}",$"{fst x}")
                | false -> ($"{fst x}",$"{snd x}"))

let reorderPorts 
    (list2 : string list)
    (list1 : (string*string*Edge*Edge) list) 
        : (string*string*Edge*Edge) list=
    list1
    |> List.sortBy (fun (x,_,_,_) -> List.findIndex (fun s -> s = x) list2)


let sortOrientation 
    (orderedPorts:(string*string) list)
    (symbolToOrder: Symbol)
    (otherSymbol: Symbol)
        : ((string*string*Edge*Edge) list)list =
    // printfn $"SmartPortOrder: orderedPorts {orderedPorts}"
    let test = List.collect (fun (x, y) ->
                match Map.tryFind x otherSymbol.PortMaps.Orientation, Map.tryFind y symbolToOrder.PortMaps.Orientation with
                | Some e1, Some e2 -> [(e1, e2, x, y)]
                | _ -> []
                ) orderedPorts
    // printfn $"SmartPortOrder: test {test}"
    // printfn $"OtherSymbol {otherSymbol.PortMaps.Orientation}"
    // printfn $"SymbolToOrder {symbolToOrder.PortMaps.Orientation}"
    test
    |> List.groupBy (fun (e1, e2, _, _) -> e1, e2)
    |> List.map (fun (_, group) -> List.map (fun (e1, e2, x, y) -> (x,y,e1,e2)) group)

let groupByEdges list map mapi =
    List.collect (fun (x, y) ->
        match Map.tryFind x map, Map.tryFind y mapi with
        | Some e1, Some e2 ->
            [(e1, e2, x, y)]
        | _ -> []
    ) list
    |> List.groupBy (fun (e1, e2, _, _) -> e1, e2)
    |> List.map (fun (_, group) -> List.map (fun (e1, e2, x, y) -> (x, y,e1,e2)) group)

let FindOtherOrientation 
    (list: (string*string*Edge*Edge) list)
        : Edge =
    list 
    |> List.map (fun (_,_,x,_) -> x)
    |> List.head

let FindSymbolOrientation 
    (list: (string*string*Edge*Edge) list)
        : Edge =
    list 
    |> List.map (fun (_,_,_,x) -> x)
    |> List.head

let FindOrientationList
    (list: (string*string*Edge*Edge) list list)
        : Edge list =
        list 
        |>  List.map (fun x -> FindSymbolOrientation x)
    


let reOrderPorts 
    (wModel: BusWireT.Model) 
    (symbolToOrder: Symbol) 
    (otherSymbol: Symbol) 
        : BusWireT.Model =
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
    printfn "connectedPorts: %A" connectedPorts
    let PortsToReorder = checkInputOutput symbolToOrder connectedPorts
    printfn "PortsToReorder: %A" PortsToReorder
    let SortedOrientationPorts = sortOrientation PortsToReorder symbolToOrder otherSymbol
    printfn "SortedOrientationPorts: %A" SortedOrientationPorts
    let ReorderedPorts = SortedOrientationPorts 
                        |> List.map (fun x -> reorderPorts  (otherSymbol.PortMaps.Order |> Map.find (FindOtherOrientation x) )x) 

    let OrientationList = FindOrientationList ReorderedPorts

    let SymbolPorts = 
        ReorderedPorts
        |> List.map (fun x -> x |> List.map (fun (_,x,_,_) -> x))

    let NewPorts =
        SymbolPorts
        |> List.mapi (fun i x ->
           List.fold (fun acc y -> 
                            if List.contains y acc 
                            then acc 
                            else y::acc) x (symbolToOrder.PortMaps.Order |> Map.find (OrientationList[i]))
            |> List.rev
        )
        
    let OldPortsList = Map.toList symbolToOrder.PortMaps.Order


    let NewPortsList =
        OldPortsList
        |> List.map (fun (x, y) ->
            match List.tryFindIndex ((=) x) OrientationList with
            | Some i -> (x, NewPorts.[i])
            | None -> (x, y))


    let portMapsReplace = {symbolToOrder.PortMaps with Order= Map.ofList NewPortsList} // replace this with correct wires
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
