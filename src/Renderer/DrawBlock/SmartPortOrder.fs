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

let checkSymbolToOrder 
    (symbolToOrder : Symbol)
    (otherSymbol : Symbol)
        : Symbol*Symbol =
        if CheckSelectComponent symbolToOrder.Component.Type && false= CheckSelectComponent otherSymbol.Component.Type then
            otherSymbol, symbolToOrder
        else
            symbolToOrder, otherSymbol

let FixOtherSymbolPorts 
    (finalSymbol: Symbol)
    (newSymbol: Symbol)
    (isFlipped: bool) 
        : Map<Edge, string list> =
    if isFlipped then
        finalSymbol.PortMaps.Order
        |> Map.add  Left newSymbol.PortMaps.Order.[Left]
        |> Map.add  Right newSymbol.PortMaps.Order.[Right]
    else 
        finalSymbol.PortMaps.Order


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
    

    printfn $"HERE"

    //Check if the symbolToOrder is a MUX or a Demux and if a flip is needed
    let NewSymbolToOrder, NewOtherSymbol = checkSymbolToOrder symbolToOrder otherSymbol 
    printfn $"ReorderPorts: ToOrder:{NewSymbolToOrder.Component.Label}, Other:{NewOtherSymbol.Component.Label}"
    // let NewSymbolToOrder = symbolToOrder
    // let NewOtherSymbol = otherSymbol
    // let NewWModel = wModel

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
    let symbol' = {NewSymbolToOrder with PortMaps = portMapsReplace} 
    let finalSymbolToOrder, finalOtherSymbol, finalWModel, isFlipped = CheckforFlip sModel wireList wModel symbol' NewOtherSymbol
    let OtherSymbolPorts = FixOtherSymbolPorts finalOtherSymbol NewOtherSymbol isFlipped
    let symbol'' = {finalOtherSymbol with PortMaps = {finalOtherSymbol.PortMaps with Order = OtherSymbolPorts}}


    //New model with the new symbols
    let newSmodel = {sModel with Symbols = sModel.Symbols 
                                                  |> Map.add symbolToOrder.Id symbol'
                                                  |> Map.add otherSymbol.Id symbol''}

    // Rerouting the wires with our new model
    let wiresToOrder = List.map (autoroute {finalWModel with Symbol = newSmodel}) wireList


    // getting the new wires with their Ids
    let ListWires = List.zip connectionIdList wiresToOrder


    {wModel with 
        Wires = Map.ofList ListWires
        Symbol = newSmodel
    }



//Start of reordering ports with more than two components
//Helper functions for multipleReorderPorts

//Counts the number of ports connected to a component by checking if the ports connecvted to wires are contained in the map of ports of the componenet 
//Helper function for getMaxComponent
let countMatchingKeys 
    (connectedPorts: string list)
    (map: Map<string, Edge>)
        : (Map<string, Edge> * int) =
    let count = Map.filter (fun k _ -> connectedPorts |> List.contains k) map |> Map.count
    (map, count)

//Getting the component that has the most wires connected to it.
let getMaxComponent 
    (connectedPorts: string list)
    (listofOrientationMaps: Map<string, Edge> list)
        : Map<string, Edge> =
        listofOrientationMaps
        |> List.map (countMatchingKeys connectedPorts)
        |> List.maxBy snd
        |> fst

//Combining all Order Lists of OtherComponents in order so that we can then sort the ports of the SymbolToOrder
let concatenateOrderLists 
    (orderLists: (Edge * string list) list) 
        : Map<Edge, string list> =
            let folder (acc: Map<Edge, string list>) (edge, strList) =
                match Map.tryFind edge acc with
                | Some existingList -> Map.add edge (existingList @ strList) acc
                | None -> Map.add edge strList acc
            orderLists
            |> List.fold folder Map.empty


//Combining all Orientation Lists of OtherComponents to form one symbol representing all the other components
let concatenateOrientationLists 
    (orientationLists: (string*Edge) list)
        : Map<string,Edge> =
            let folder (acc: Map<string, Edge>) (str, edge) =
                match Map.tryFind str acc with
                | Some existingList -> failwithf "Error: Orientation list has duplicate keys. Can't happen."
                | None -> Map.add str edge acc
            orientationLists
            |> List.fold folder Map.empty


let CombineOtherSymbols 
    (symbolsInOrder : Symbol list)
    (symbolToOrder: Symbol)
      : Symbol =
        let OrderLists = symbolsInOrder |> List.map (fun x -> (x.PortMaps.Order |> Map.toList))  
        let newOrderMap = concatenateOrderLists (List.concat OrderLists)
        let OrientationLists = symbolsInOrder |> List.map (fun x -> (x.PortMaps.Orientation |> Map.toList))
        let newOrientationMap = concatenateOrientationLists (List.concat OrientationLists)

        {symbolToOrder with  
                PortMaps = {symbolToOrder.PortMaps with Order= newOrderMap
                                                        Orientation= newOrientationMap}
        }


let createOtherSymbol 
    (otherSymbols: Symbol list)
    (symbolToOrder: Symbol)
    (connectedPortsNeeded: list<Edge * list<string * string * Edge * Edge>>)
        : Symbol =

    let edgeToSortBy= 
        connectedPortsNeeded
        |> List.maxBy (fun (_, lst) -> lst.Length)
        |> fst
    
    match edgeToSortBy with
        | Left | Right->  
                let SymbolsInOrder =
                    otherSymbols 
                    |> List.sortByDescending (fun x -> x.Pos.Y)
                CombineOtherSymbols SymbolsInOrder symbolToOrder
        | Top | Bottom ->  
                    let SymbolsInOrder =
                        otherSymbols 
                        |> List.sortBy (fun x -> x.Pos.X)
                    CombineOtherSymbols SymbolsInOrder symbolToOrder

let groupPorts 
    (wiresSymbolToOrder: list<ConnectionId * Wire>)
    (symbolToOrderOrientationList: Map<string, Edge>)
      : list<Edge * list<string * string * Edge * Edge>> =
      //I really tried to put this part in a function and call it because it is similar to the previous function but it did not work I think there is a bug in Issie
      wiresSymbolToOrder
        |> List.map (fun (_,x)->(x.InputPort,x.OutputPort))
        |> List.map (fun x  -> 
                match symbolToOrderOrientationList |> Map.containsKey ($"{fst x}") with 
                | true -> ($"{snd x}",$"{fst x}")
                | false -> ($"{fst x}",$"{snd x}"))
        |> List.collect (fun (x,y) -> match symbolToOrderOrientationList  |> Map.tryFind  y with
                                                        | Some e -> [(x,y,e,e)]
                                                        | None -> [] )
        |> List.groupBy (fun (_,_, e, _) -> e)


let sortMultPorts 
    (connectedPortsNeeded: list<Edge * list<string * string * Edge * Edge>>)
    (combinedOtherSymbolsOrientation: Map<string,Edge>)
    (combinedOtherSymbolsOrder: Map<Edge,string list>)
       : list<list<string * string * Edge * Edge>> =
       connectedPortsNeeded 
                |> List.filter (fun (_,lst) -> combinedOtherSymbolsOrientation 
                                                                                    |> Map.containsKey (lst[0]|> (fun (x,_,_,_)->x)))
                |> List.map (fun (_,lst)->lst)
                |> List.map (fun y -> 
                                                    sortPortsHelper  (combinedOtherSymbolsOrder 
                                                                                |> Map.find (combinedOtherSymbolsOrientation
                                                                                                    |>Map.find (y[0] |> (fun (x,_,_,_)-> x)))) 
                                                                        y)
    

// Will only reorder the component with the most wires connected to it.
// Only works if components are connected to the SymbolToReorder from the same edge.
// All Components connected to that edge of the symbolToOrder should be selected or it will not work.
let multipleReorderPorts 
        (wModel: BusWireT.Model)
        (symbols : Symbol list)
        : BusWireT.Model =
    let sModel = wModel.Symbol

    printfn $"MultipleReorderPorts: Symbols:{symbols |> List.map (fun x -> x.Component.Type)}"
    let connectedPorts = wModel.Wires |> Map.toList |> List.collect (fun (_,x) -> [$"{x.InputPort}"; $"{x.OutputPort}"])
    let listofOrientationMaps = symbols |> List.map (fun x -> x.PortMaps.Orientation)
    let symbolToOrderOrientationList = getMaxComponent connectedPorts listofOrientationMaps
    let otherSymbols =  symbols |> List.filter (fun x -> x.PortMaps.Orientation <> symbolToOrderOrientationList)
    let symbolToOrder=  (List.except otherSymbols symbols)[0]

    let wiresSymbolToOrder = 
        wModel.Wires 
        |> Map.toList 
        |> List.filter (fun (_,x) -> 
                            symbolToOrderOrientationList |> Map.containsKey $"{x.InputPort}" || 
                            symbolToOrderOrientationList |> Map.containsKey $"{x.OutputPort}")

    let connectedPortsNeeded = groupPorts wiresSymbolToOrder symbolToOrderOrientationList
    
    let CombinedOtherSymbols = createOtherSymbol otherSymbols symbolToOrder connectedPortsNeeded
    let CombinedOtherSymbolsOrder = CombinedOtherSymbols.PortMaps.Order
    let CombinedOtherSymbolsOrientation = CombinedOtherSymbols.PortMaps.Orientation

    
    let SortedPorts = sortMultPorts connectedPortsNeeded CombinedOtherSymbolsOrientation CombinedOtherSymbolsOrder
                                                                                 

    //Get the orientation of the ports we reordered on the symbolToOrder
    let OrientationList = SymbolToOrderOrientationList  SortedPorts

    let NewPorts = AddUnconnectedPorts  SortedPorts symbolToOrder OrientationList

    let NewOrderList = CreateNewPortsList symbolToOrder OrientationList NewPorts

    let newSymbolToOrder = {symbolToOrder with PortMaps = {symbolToOrder.PortMaps with Order = Map.ofList NewOrderList}}

    //New model with the new symbols
    let newSmodel = {sModel with Symbols = sModel.Symbols 
                                                  |> Map.add newSymbolToOrder.Id newSymbolToOrder}

    // Rerouting the wires with our new model
    let wiresToOrder = List.map (autoroute {wModel with Symbol = newSmodel}) (wModel.Wires |> Map.toList |> List.map (fun (_,x) -> x))


    // getting the new wires with their Ids
    let ListWires = List.zip (wModel.Wires |> Map.toList |> List.map (fun (y,_)->y)) wiresToOrder


    // HLP23: This could be cleaned up using Optics - see SmartHelpers for examples
    {wModel with 
        Wires = Map.ofList ListWires
        Symbol = newSmodel
    }

