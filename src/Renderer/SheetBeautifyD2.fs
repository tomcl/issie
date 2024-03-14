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

let getInputReversalList (sym:Symbol): Symbol list =
    let reverseSetter = snd reversedInputPorts_
    [sym; (reverseSetter (Some true) sym)]

let getFlipList (sym:Symbol): Symbol list =
    let flipSetter = snd symbol_flipped_
    [sym; (flipSetter true sym)]

let getRotationList (sym:Symbol): Symbol list =
    let rotationSetter = snd symbol_rotation_
    [sym; (rotationSetter Degree90 sym); (rotationSetter Degree180 sym); (rotationSetter Degree270 sym)]

// Configs due to input reversal, flipping and rotation.
let getMuxConfigs (sym:Symbol): Symbol list =
    getInputReversalList sym
    |> List.map getFlipList
    |> List.concat
    |> List.map getRotationList
    |> List.concat

let getGateConfigs (sym:Symbol): Symbol list =
    getFlipList sym

let getAdderConfigs (sym:Symbol): Symbol list =
    getInputReversalList sym
    |> List.map getFlipList
    |> List.concat

let getConfigurations (sym:Symbol): Symbol list =
    match sym.Component.Type with
    | Mux2 -> getMuxConfigs sym
    | GateN _ -> getGateConfigs sym
    | NbitsAnd _ -> getAdderConfigs sym
    | _ -> [sym]

let sheetOrderFlip (model: SheetT.Model):Result<SheetT.Model, string> =
    let rec cartesian lstlst =
        match lstlst with
        | h::[] ->
            List.fold (fun acc elem -> [elem]::acc) [] h
        | h::t ->
            List.fold (fun cacc celem ->
                (List.fold (fun acc elem -> (elem::celem)::acc) [] h) @ cacc
                ) [] (cartesian t)
        | _ -> []
    
    // IDList and symList must be the same length.
    let buildMap (idList:ComponentId list) (symList:Symbol list):Map<ComponentId,Symbol> =
        List.zip idList symList
        |> Map.ofList

    let idSymbolMap = model.Wire.Symbol.Symbols
    let IDList: ComponentId list =
        Map.keys idSymbolMap
        |> Seq.toList
    let SymbolList: Symbol list =
        Map.values idSymbolMap
        |> Seq.toList

    let possibleMaps =
        List.map getConfigurations SymbolList
        |> cartesian
        |> List.map (fun symList -> buildMap IDList symList)

    let symbolsSetter = snd symbols_

    let possibleModels = List.map (fun possibleMap -> symbolsSetter possibleMap model) possibleMaps

    let bestModel = List.fold (fun minModel newModel -> if ((numOfWireRightAngleCrossings newModel) < (numOfWireRightAngleCrossings minModel)) then newModel else minModel) model possibleModels
    Ok bestModel