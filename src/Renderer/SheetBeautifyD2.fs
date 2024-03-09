module SheetBeautifyD2

open SheetBeautifyHelpers
open DrawModelType
open CommonTypes
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open Optics
open Operators

let optimalEdgeOrder (edge:Edge) (symID:ComponentId) (model: SheetT.Model) =

    let symbol = model.Wire.Symbol.Symbols[symID]
    let currPortOrder = symbol.PortMaps.Order

    let ports = currPortOrder[edge]
    let permute list =
        let rec inserts e = function
            | [] -> [[e]]
            | x::xs as list -> (e::list)::[for xs' in inserts e xs -> x::xs']

        List.fold (fun accum x -> List.collect (inserts x) accum) [[]] list

    let possibleOrders = permute ports

    let sheetSymbolLens = symbolOf_ symID
    let symbolPortMapsLens = portMaps_
    let PortMapsPortOrderLens = order_
    let sheetPortMapsLens = sheetSymbolLens >-> symbolPortMapsLens
    let sheetPortOrderLens = sheetPortMapsLens >-> PortMapsPortOrderLens

    let alteredOrdersMaps = List.map (fun possibleOrder -> Map.add edge possibleOrder currPortOrder) possibleOrders
    let orderSetter = snd sheetPortOrderLens
    let possibleModels = List.map (fun alteredOrdersMap -> orderSetter alteredOrdersMap model) alteredOrdersMaps

    List.fold (fun minModel newModel -> if ((numOfWireRightAngleCrossings newModel) < (numOfWireRightAngleCrossings minModel)) then newModel else minModel) model possibleModels