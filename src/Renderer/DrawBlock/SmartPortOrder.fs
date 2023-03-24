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

// HLP23 AUTHOR: Khoury
// Start of HLP23: SmartPortOrder with order list 
// Helper functions for SmartPortOrder

//Sort the ports by the otherSymbol Order Ports
let getSymEdgeList
    (list: (string*string*Edge*Edge) list list)
        : Edge list =
    list 
    |>  List.map (fun x ->  x
                                                                |> List.map (fun (_,_,_,y) -> y)
                                                                |> List.head)

let CreateNewPortsList 
    (newSymbolToOrder: Symbol)
    (edgeList: Edge list)
    (newPorts: string list list)
        : (Edge*string list) list =
        newSymbolToOrder.PortMaps.Order
        |> Map.toList
        |> List.map (fun (x, y) ->
            match List.tryFindIndex ((=) x) edgeList with
            | Some i -> (x, newPorts[i])
            | None -> (x, y))

let AddUnconnectedPorts 
    (reorderedPorts: (string*string*Edge*Edge) list list)
    (newSymbolToOrder: Symbol)
    (edgeList: Edge list)
        : string list list =
        reorderedPorts
        |> List.map (fun x -> x |> List.map (fun (_,x,_,_) -> x))
        |> List.mapi (fun i x ->
            List.fold (fun acc y -> 
                            if List.contains y acc 
                            then acc 
                            else y::acc) x (newSymbolToOrder.PortMaps.Order |> Map.find (edgeList[i]))
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
    match isFlipped with
        |true ->  finalSymbol.PortMaps.Order
                |> Map.add  Left newSymbol.PortMaps.Order.[Left]
                |> Map.add  Right newSymbol.PortMaps.Order.[Right]
        |false -> finalSymbol.PortMaps.Order



//Main Function
let reOrderPorts 
    (wModel: BusWireT.Model) 
    (symbolToOrder: Symbol) 
    (otherSymbol: Symbol) 
        : BusWireT.Model =
    let sModel = wModel.Symbol

    //Getting a list of the connection IDs of wires needed for wModel NewWires
    let connectionIdList =
        wModel.Wires |> Map.toList |> List.map fst

    //List of wires 
    let wireList =
        wModel.Wires |> Map.toList |> List.map snd

    //List of the ports connected by wires
    let connectedPorts = 
        wireList |> List.map (fun x -> x.InputPort, x.OutputPort)
    

    //Check if the symbolToOrder is a MUX or a Demux and if so set it otherSymbol because it is better not to reorder the MUX or Demux
    let newSymbolToOrder, newOtherSymbol = checkSymbolToOrder symbolToOrder otherSymbol 

    //Reorder the symbol to order ports by comparing with the other symbol ports
    let reorderedPorts = sortPorts connectedPorts newSymbolToOrder newOtherSymbol 
     

    //Get the orientation of the ports we reordered on the symbolToOrder
    let symEdgeList = getSymEdgeList reorderedPorts


    //Adding the ports that aren't connected to any wires
    let fullPorts =  AddUnconnectedPorts reorderedPorts newSymbolToOrder symEdgeList


    //Creating a new list of ports in new order to replace the old ports
    let ReorderedPortsList = CreateNewPortsList newSymbolToOrder symEdgeList fullPorts

    
    // Replacing the ports to get the new symbol
    let portMapsReplace = {newSymbolToOrder.PortMaps with Order= Map.ofList ReorderedPortsList} 
    let symbol' = {newSymbolToOrder with PortMaps = portMapsReplace} 
    let finalOtherSymbol, finalWModel, isFlipped = CheckforFlip sModel wireList wModel symbol' newOtherSymbol
    let OtherSymbolPorts = FixOtherSymbolPorts finalOtherSymbol newOtherSymbol isFlipped
    let symbol'' = {finalOtherSymbol with PortMaps = {finalOtherSymbol.PortMaps with Order = OtherSymbolPorts}}


    //New model with the new symbols
    let newSmodel = {sModel with Symbols = sModel.Symbols 
                                                  |> Map.add symbol'.Id symbol'
                                                  |> Map.add symbol''.Id symbol''}

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
            printf "Orientation Lists: %A" orientationLists
            let folder (acc: Map<string, Edge>) (str, edge) =
                match Map.tryFind str acc with
                | Some existingList -> failwithf "Error: Orientation list has duplicate keys. Can't happen."
                | None -> Map.add str edge acc
            orientationLists
            |> List.fold folder Map.empty

//Combines all Othersymbols to get one symbol Order and Orientation map so that we can compare with the SymbolToOrder
let CombineOtherSymbols 
    (symbolToOrder: Symbol)
    (symbolsInOrder : Symbol list)
      : Symbol =
        printfn "Symbols in Order: %A" symbolsInOrder
        let OrderLists = symbolsInOrder |> List.map (fun x -> (x.PortMaps.Order |> Map.toList))  
        printfn "Order Lists: %A" OrderLists
        let newOrderMap = concatenateOrderLists (List.concat OrderLists)
        printfn "New Order Map: %A" newOrderMap
        let OrientationLists = symbolsInOrder |> List.map (fun x -> (x.PortMaps.Orientation |> Map.toList))
        let newOrientationMap = concatenateOrientationLists (List.concat OrientationLists)

        {symbolToOrder with  
                PortMaps = {symbolToOrder.PortMaps with Order= newOrderMap
                                                        Orientation= newOrientationMap}
        }

let fst4 (quad:string*string*Edge*Edge) = 
    match quad with
    | (a,_,_,_) -> a

let getRelativeSymbols 
        (symbolsList : list<Symbol>)
        (portsNeeded: list<string * string * Edge * Edge>)
            : list<Symbol> =
            symbolsList
            |> List.filter (fun symbol -> 
                                portsNeeded |> List.map (fun x -> fst4 x)
                                |> List.exists (fun key -> 
                                                symbol.PortMaps.Orientation |> Map.containsKey key))
        


//Creates a symbol that combines all ports of Othersymbols
let createOtherSymbol 
    (otherSymbols: Symbol list)
    (symbolToOrder: Symbol)
    (connectedPortsNeeded: list<Edge * list<string * string * Edge * Edge>>)
        : Symbol =
    printfn "Connected Ports Needed: %A" connectedPortsNeeded
    printfn "Other Symbols: %A" (otherSymbols |> List.map (fun x -> x.PortMaps.Orientation))
    let symbolsInOrder =
        connectedPortsNeeded
        |> List.map (fun (edge, ports) -> 
            let filteredPorts =
                ports
                |> List.filter (fun (str1, _, _, _) -> 
                    otherSymbols
                    |> List.exists (fun symbol -> symbol.PortMaps.Orientation.ContainsKey str1))
            (edge, filteredPorts))
        |> List.collect (fun (x,y) -> 
            match x with
                | Left ->   getRelativeSymbols otherSymbols y
                            |> List.sortByDescending (fun x -> x.Pos.Y)
                | Right ->  
                            getRelativeSymbols otherSymbols y 
                            |> List.sortBy (fun x -> x.Pos.Y)
                | Bottom -> 
                            getRelativeSymbols otherSymbols y
                            |> List.sortByDescending (fun x -> x.Pos.X)
                | Top ->  
                            getRelativeSymbols otherSymbols y 
                            |> List.sortBy (fun x -> x.Pos.X))
    CombineOtherSymbols symbolToOrder symbolsInOrder

//Groups the ports that are connected to the wires by the edge of the symbolToOrder they are on or connected to
let groupPorts 
    (wiresSymbolToOrder: list<ConnectionId * Wire>)
    (symbolToOrder: Symbol)
    (symOrientationList: Map<string, Edge>)
      : list<Edge * list<string * string * Edge * Edge>> =
      wiresSymbolToOrder
        |> List.map (fun (_,x)->(x.InputPort,x.OutputPort))
        |> sortInputOutput symbolToOrder
        |> List.collect (fun (x,y) -> match symOrientationList  |> Map.tryFind  y with
                                                        | Some e -> [(x,y,e,e)]
                                                        | None -> [] )
        |> List.groupBy (fun (_,_, e, _) -> e)


//Sorts the ports of the SymbolToOrder by the order of the ports of the new combined othercomponents
let sortMultPorts 
    (connectedPortsNeeded: list<Edge * list<string * string * Edge * Edge>>)
    (combinedOtherSymbolsOrientation: Map<string,Edge>)
    (combinedOtherSymbolsOrder: Map<Edge,string list>)
       : list<list<string * string * Edge * Edge>> =
    connectedPortsNeeded 
        |> List.map (fun (e,lst) ->
                            List.filter (fun (x,y,z,i)-> combinedOtherSymbolsOrientation |> Map.containsKey x) lst)
        |> List.filter (fun x -> x<>[])
        |> List.map (fun y -> 
                                            sortByOther  (combinedOtherSymbolsOrder 
                                                                    |> Map.find (combinedOtherSymbolsOrientation
                                                                                      |>Map.find (y[0] |> (fun (x,_,_,_)-> x)))) 
                                                         y)


// Will only reorder the component with the most wires connected to it.
// Only works if components are connected to the SymbolToReorder from the same edge.
// Main function for this part
let multipleReorderPorts 
        (wModel: BusWireT.Model)
        (symbols : Symbol list)
        : BusWireT.Model =
    let sModel = wModel.Symbol

    printfn $"MultipleReorderPorts: Symbols:{symbols |> List.map (fun x -> x.Component.Type, x.PortMaps.Orientation)}"

    let connectedPorts = wModel.Wires |> Map.toList |> List.collect (fun (_,x) -> [$"{x.InputPort}"; $"{x.OutputPort}"])
    let listofOrientationMaps = symbols |> List.map (fun x -> x.PortMaps.Orientation)
    let symOrientationList = getMaxComponent connectedPorts listofOrientationMaps
    let otherSymbols =  symbols |> List.filter (fun x -> x.PortMaps.Orientation <> symOrientationList)
    let symbolToOrder=  (List.except otherSymbols symbols)[0]

    printfn $"OtherSymbolOrientationList : {otherSymbols |> List.map (fun x -> x.PortMaps.Orientation)}"
    let wiresSymbolToOrder = 
        wModel.Wires 
        |> Map.toList 
        |> List.filter (fun (_,x) -> 
                            symOrientationList |> Map.containsKey $"{x.InputPort}" || 
                            symOrientationList |> Map.containsKey $"{x.OutputPort}")

    let connectedPortsNeeded = groupPorts wiresSymbolToOrder symbolToOrder symOrientationList
    let CombinedOtherSymbols = createOtherSymbol otherSymbols symbolToOrder connectedPortsNeeded
    let CombinedOtherSymbolsOrder = CombinedOtherSymbols.PortMaps.Order
    let CombinedOtherSymbolsOrientation = CombinedOtherSymbols.PortMaps.Orientation

    let wiresToAutoroute = 
                wiresSymbolToOrder
                |> List.filter (fun (_,x) ->
                                    CombinedOtherSymbolsOrientation |> Map.containsKey $"{x.InputPort}" ||
                                    CombinedOtherSymbolsOrientation |> Map.containsKey $"{x.OutputPort}")

    
    let SortedPorts = sortMultPorts connectedPortsNeeded CombinedOtherSymbolsOrientation CombinedOtherSymbolsOrder
                                                                                 

    //Get the orientation of the ports we reordered on the symbolToOrder
    let edgeList = getSymEdgeList  SortedPorts

    let fullPorts = AddUnconnectedPorts  SortedPorts symbolToOrder edgeList

    let NewOrderList = CreateNewPortsList symbolToOrder edgeList fullPorts

    let newSymbolToOrder = {symbolToOrder with PortMaps = {symbolToOrder.PortMaps with Order = Map.ofList NewOrderList}}

    //New model with the new symbols
    let newSmodel = {sModel with Symbols = sModel.Symbols 
                                                  |> Map.add newSymbolToOrder.Id newSymbolToOrder}

    // Rerouting the wires with our new model
    // Could be replaced with smart autoroute 
    let wiresAutorouted = 
                            wiresToAutoroute
                            |> List.map (fun (id, wire) -> (id, autoroute {wModel with Symbol = newSmodel} wire)) 
                            |> List.fold (fun accMap (id, wire) -> Map.add id wire accMap) wModel.Wires


    {wModel with 
        Wires =  wiresAutorouted
        Symbol = newSmodel
    }

