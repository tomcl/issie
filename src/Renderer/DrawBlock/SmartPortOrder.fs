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
open SymbolUpdate
open SmartHelpers
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

//Sort the ports by the otherSymbol Order Ports
let SymbolToOrderOrientationList
    (list: (string*string*Edge*Edge) list list)
        : Edge list =
    list 
    |>  List.map (fun x ->  x
                                                                |> List.map (fun (_,_,_,y) -> y)
                                                                |> List.head)

let CreateNewPortsList 
    (newSymbolToOrder: Symbol)
    (orientationList: Edge list)
    (newPorts: string list list)
        : (Edge*string list) list =
        newSymbolToOrder.PortMaps.Order
        |> Map.toList
        |> List.map (fun (x, y) ->
            match List.tryFindIndex ((=) x) orientationList with
            | Some i -> (x, newPorts[i])
            | None -> (x, y))

let AddUnconnectedPorts 
    (reorderedPorts: (string*string*Edge*Edge) list list)
    (newSymbolToOrder: Symbol)
    (orientationList: Edge list)
        : string list list =
        reorderedPorts
        |> List.map (fun x -> x |> List.map (fun (_,x,_,_) -> x))
        |> List.mapi (fun i x ->
            List.fold (fun acc y -> 
                            if List.contains y acc 
                            then acc 
                            else y::acc) x (newSymbolToOrder.PortMaps.Order |> Map.find (orientationList[i]))
            |> List.rev
        )



let reOrderPorts 
    (wModel: BusWireT.Model) 
    (symbolToOrder: Symbol) 
    (otherSymbol: Symbol) 
        : BusWireT.Model =
    let sModel = wModel.Symbol
    printfn $"ReorderPorts: ToOrder:{symbolToOrder.Component.Label}, Other:{otherSymbol.Component.Label}"

    //Getting a list of the connection IDs of wires needed for wModel NewWires
    let connectionIdList =
        wModel.Wires |> Map.toList |> List.map fst

    //List of wires 
    let wireList =
        wModel.Wires |> Map.toList |> List.map snd

    //List of the ports connected by wires
    let connectedPorts = 
        wireList |> List.map (fun x -> x.InputPort, x.OutputPort)

    //Check if the symbolToOrder is a MUX or a Demux and if a flip is needed
    let NewSymbolToOrder, NewOtherSymbol, NewWModel = CheckforFlip sModel wireList wModel symbolToOrder otherSymbol

    //Reorder the symbol to order ports by comparing with the other symbol ports
    let ReorderedPorts = SortPorts connectedPorts NewSymbolToOrder NewOtherSymbol 
     
    //Get the orientation of the ports we reordered on the symbolToOrder
    let OrientationList = SymbolToOrderOrientationList ReorderedPorts

    //Adding the ports that aren't connected to any wires
    let NewPorts =  AddUnconnectedPorts ReorderedPorts NewSymbolToOrder OrientationList

    //Creating a new list of ports in new order to replace the old ports
    let ReorderedPortsList = CreateNewPortsList NewSymbolToOrder OrientationList NewPorts

    // Replacing the ports to get the new symbol
    let portMapsReplace = {NewSymbolToOrder.PortMaps with Order= Map.ofList ReorderedPortsList} 
    let symbol' = {symbolToOrder with PortMaps = portMapsReplace} 

    //New model with the new symbols
    let newSmodel = {sModel with Symbols = sModel.Symbols 
                                                  |> Map.add symbol'.Id symbol'
                                                  |> Map.add otherSymbol.Id NewOtherSymbol }
    // Rerouting the wires with our new model
    let wiresToOrder = List.map (autoroute {NewWModel with Symbol = newSmodel}) wireList

    // getting the new wires with their Ids
    let ListWires = List.zip connectionIdList wiresToOrder

    // HLP23: This could be cleaned up using Optics - see SmartHelpers for examples
    {wModel with 
        Wires = Map.ofList ListWires
        Symbol = newSmodel
    }
