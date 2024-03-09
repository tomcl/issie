module SheetBeautifyD2

open SheetBeautifyHelpers
open DrawModelType
open CommonTypes
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open Optics
open Operators

let optimalEdgeOrder (model: SheetT.Model):Result<SheetT.Model, string> =

    // Get arbitary first symbol for testing purposes.
    // let idSymbolPair = Seq.head model.Wire.Symbol.Symbols
    let idSymbolMap = model.Wire.Symbol.Symbols

    let filtere (id: ComponentId, sym: SymbolT.Symbol) = sym.Component.Type = Mux2

    let idSymbolPair = idSymbolMap
                                        |> Map.toList
                                        |> List.filter filtere
                                        |> List.head
    let symID: ComponentId = fst idSymbolPair
    let symbol = snd idSymbolPair
    
    let currPortOrder = symbol.PortMaps.Order

    printf $"ID of symbol to have ports reordered: {(snd idSymbolPair).Component.Type}"
    //printf $"ID of symbol to have ports reordered:"
    // Get arbitary first edge for testing purposes.
    let edgePortsPair = Seq.head symbol.PortMaps.Order
    let edge = Left

    let ports = currPortOrder[edge]

    printf $"Ports: {ports}"

    let permute list =
        let rec inserts e = function
            | [] -> [[e]]
            | x::xs as list -> (e::list)::[for xs' in inserts e xs -> x::xs']

        List.fold (fun accum x -> List.collect (inserts x) accum) [[]] list

    let possibleOrders = permute ports

    printf $"possible port orders: {possibleOrders}"

    let sheetSymbolLens = symbolOf_ symID
    let symbolPortMapsLens = portMaps_
    let PortMapsPortOrderLens = order_
    let sheetPortMapsLens = sheetSymbolLens >-> symbolPortMapsLens
    let sheetPortOrderLens = sheetPortMapsLens >-> PortMapsPortOrderLens

    let alteredOrdersMaps = List.map (fun possibleOrder -> Map.add edge possibleOrder currPortOrder) possibleOrders
    let orderSetter = snd sheetPortOrderLens
    let possibleModels = List.map (fun alteredOrdersMap -> orderSetter alteredOrdersMap model) alteredOrdersMaps

    let bestModel = List.fold (fun minModel newModel -> if ((numOfWireRightAngleCrossings newModel) < (numOfWireRightAngleCrossings minModel)) then newModel else minModel) model possibleModels
    Ok bestModel