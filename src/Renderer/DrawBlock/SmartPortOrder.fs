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
// Start of HLP23: SmartPortOrder 


// Helper functions for SmartPortOrder

//The input is a quadruple of the two connected port IDs in string type by a wire and the two edges they are on respectiveley. 
//The first string is always the port corresponding to the symbol to order
///Sort the ports by the otherSymbol Order Ports
let getSymEdgeList
    (list: (string*string*Edge*Edge) list list)
        : Edge list =
    list 
    |>  List.map (fun x ->  x
                                    |> List.map (fun (_,_,_,y) -> y)
                                    |> List.head)

///Creates a new list of ports in new order to replace the old ports
///Returns a list of an Order Port Map with the new order
let createNewPortsList 
    (symbol: Symbol)
    (edgeList: Edge list)
    (newPorts: string list list)
        : (Edge*string list) list =
        symbol.PortMaps.Order
        |> Map.toList
        |> List.map (fun (x, y) ->
            match List.tryFindIndex ((=) x) edgeList with
            | Some i -> (x, newPorts[i])
            | None -> (x, y))

///Adds the ports that aren't connected to any wires
///Returns a list of lists of port IDs each list is corresponnding to an edge 
let addUnconnectedPorts 
    (reorderedPorts: (string*string*Edge*Edge) list list)
    (newSymbolToOrder: Symbol)
    (edgeList: Edge list)
        : string list list =
        reorderedPorts
        |> List.map (fun x -> x |> List.map (fun (_,x,_,_) -> x))
        |> List.mapi (fun i x ->
            List.distinct (x @ (newSymbolToOrder.PortMaps.Order |> Map.find (edgeList[i])))
            |> List.rev
        )

let checkSymbolToOrder 
    (symbolToOrder : Symbol)
    (otherSymbol : Symbol)
        : Symbol*Symbol =
        if isMuxDemux symbolToOrder.Component.Type && not (isMuxDemux otherSymbol.Component.Type) then
            otherSymbol, symbolToOrder
        else
            symbolToOrder, otherSymbol

let fixOtherSymbolPorts 
    (finalSymbol: Symbol)
    (newSymbol: Symbol)
    (isFlipped: bool) 
        : Map<Edge, string list> =
    let finalOrder = finalSymbol.PortMaps.Order
    let newOrder = newSymbol.PortMaps.Order
    match isFlipped with
        |true ->  finalOrder
                |> Map.add  Left newOrder.[Left]
                |> Map.add  Right newOrder.[Right]
        |false -> finalOrder


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
    let fullPorts =  addUnconnectedPorts reorderedPorts newSymbolToOrder symEdgeList


    //Creating a new list of ports in new order to replace the old ports
    let ReorderedPortsList = createNewPortsList newSymbolToOrder symEdgeList fullPorts

    
    // Replacing the ports to get the new symbol
    let portMapsReplace = {newSymbolToOrder.PortMaps with Order= Map.ofList ReorderedPortsList} 
    let symbol' = {newSymbolToOrder with PortMaps = portMapsReplace} 
    let finalOtherSymbol, finalWModel, isFlipped = modelWithBestFlip sModel wireList wModel symbol' newOtherSymbol
    let OtherSymbolPorts = fixOtherSymbolPorts finalOtherSymbol newOtherSymbol isFlipped
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

///Counts the number of ports connected to a component by checking if the ports connected to wires are contained in the map of ports of the componenet 
//Helper function for getMaxComponent
let countMatchingKeys
    (connectedPorts: string list)   
    (map: Map<string, Edge>)
    : int =
        map
        |> Map.filter (fun k _ -> List.contains k connectedPorts)
        |> Map.count

///Gets the component that has the most wires connected to it.
let getMaxComponent 
    (connectedPorts: string list)
    (listofOrientationMaps: Map<string, Edge> list)
        : Map<string, Edge> =
        listofOrientationMaps
        |> List.map (fun map->map,countMatchingKeys connectedPorts map)
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


///Combines all Orientation Lists of OtherComponents to form one symbol representing all the other components
let concatenateOrientationLists 
    (orientationLists: (string*Edge) list)
        : Map<string,Edge> =
            let folder (acc: Map<string, Edge>) (str, edge) =
                match Map.tryFind str acc with
                | Some existingList -> failwithf "Error: Orientation list has duplicate keys. Can't happen."
                | None -> Map.add str edge acc
            (Map.empty,orientationLists)
            ||> List.fold folder 

//Combines all Othersymbols to get one symbol Order and Orientation map so that we can compare with the SymbolToOrder
let CombineOtherSymbols 
    (symbolToOrder: Symbol)
    (symbolsInOrder : Symbol list)
      : Symbol =
        let OrderLists = symbolsInOrder 
                         |> List.map (fun x -> (x.PortMaps.Order |> Map.toList))  
        let newOrderMap = concatenateOrderLists (List.concat OrderLists)
        let newOrientationList = symbolsInOrder 
                               |> List.map (fun x -> (x.PortMaps.Orientation |> Map.toList))
                               |> List.concat 
                               |> concatenateOrientationLists

        {symbolToOrder with  
                PortMaps = {symbolToOrder.PortMaps with Order= newOrderMap
                                                        Orientation= newOrientationList}
        }

///Gets 1st element of a quad
let fst4 (quad:string*string*Edge*Edge) = 
    match quad with
    | (a,_,_,_) -> a

///Gets the symbols needed for sorting them in X or Y direction
let getRelativeSymbols 
        (symbolsList : list<Symbol>)
        (portsNeeded: list<string * string * Edge * Edge>)
            : list<Symbol> =
            symbolsList
            |> List.filter (fun symbol -> 
                                portsNeeded |> List.map (fun x -> fst4 x)
                                            |> List.exists (fun key -> 
                                                            symbol.PortMaps.Orientation |> Map.containsKey key))
        


///Creates a symbol that combines all ports of Othersymbols
let createOtherSymbol 
    (otherSymbols: Symbol list)
    (symbolToOrder: Symbol)
    (connectedPortsNeeded: list<Edge * list<string * string * Edge * Edge>>)
        : Symbol =
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

///Groups the ports that are connected to the wires by the edge of the symbolToOrder they are on or connected to
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


    let connectedPorts = wModel.Wires |> Map.toList |> List.collect (fun (_,x) -> [typeConversionInput(x.InputPort); 
                                                                                                       typeConversionOutput(x.OutputPort)])
    let listofOrientationMaps = symbols |> List.map (fun x -> x.PortMaps.Orientation)
    let symOrientationList = getMaxComponent connectedPorts listofOrientationMaps
    let otherSymbols =  symbols |> List.filter (fun x -> x.PortMaps.Orientation <> symOrientationList)
    let symbolToOrder=  (List.except otherSymbols symbols)[0]

    let wiresSymbolToOrder = 
        wModel.Wires 
        |> Map.toList 
        |> List.filter (fun (_,x) -> 
                            symOrientationList |> Map.containsKey (typeConversionInput(x.InputPort))|| 
                            symOrientationList |> Map.containsKey (typeConversionOutput(x.OutputPort)))

    let connectedPortsNeeded = groupPorts wiresSymbolToOrder symbolToOrder symOrientationList
    let bigSymbol = createOtherSymbol otherSymbols symbolToOrder connectedPortsNeeded
    let bigSymbolOrder = bigSymbol.PortMaps.Order
    let bigSymbolOrientation = bigSymbol.PortMaps.Orientation
    
    let SortedPorts = sortMultPorts connectedPortsNeeded bigSymbolOrientation bigSymbolOrder
                                                                                 

    //Get the orientation of the ports we reordered on the symbolToOrder
    let edgeList = getSymEdgeList  SortedPorts

    let fullPorts = addUnconnectedPorts  SortedPorts symbolToOrder edgeList

    let NewOrderList = createNewPortsList symbolToOrder edgeList fullPorts

    let newSymbolToOrder = {symbolToOrder with PortMaps = {symbolToOrder.PortMaps with Order = Map.ofList NewOrderList}}

    //New model with the new symbols
    let newSmodel = {sModel with Symbols = sModel.Symbols 
                                                  |> Map.add newSymbolToOrder.Id newSymbolToOrder}

    //Only reroute wires that are between the component we are reordering and other selected components
    //Done to fit with Zsombor's code
    let wiresToAutoroute =
        wiresSymbolToOrder
        |> List.filter (fun (_,str) ->
            match (symOrientationList |> Map.tryFind $"{str.InputPort}" , symOrientationList |> Map.tryFind $"{str.OutputPort}") with
            | Some x , _ -> List.contains x edgeList
            | _ , Some x -> List.contains x edgeList
            | _ , _ -> false
        )

    // Rerouting the wires with our new model
    // Could be replaced with smart autoroute 
    let wiresAutorouted = 
                            wiresToAutoroute
                            |> List.map (fun (id, wire) -> 
                                                            (id, autoroute {wModel with Symbol = newSmodel} wire)) 
                            |> List.fold (fun accMap (id, wire) -> 
                                                            Map.add id wire accMap) wModel.Wires


    {wModel with 
        Wires =  wiresAutorouted
        Symbol = newSmodel
    }



