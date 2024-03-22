module SheetBeautifyD1

open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open SheetBeautifyHelpers
open Optics
open BusWireRoute

module Constants =
    ()

// D1. sheetAlignScale implementation part - Custom component scaling. Positioning of all components to reduce segments without increasing wire crossings.

///---------------------------------------------------------------------------------------------------------------///
///----------------------------------------processing-only-one-parallel----------------------------------------///
///---------------------------------------------------------------------------------------------------------------///
// determine if a wire is parallel
let isParallel (model: SheetT.Model) (wId: ConnectionId) =
    let segments = visibleSegments wId model
    let rec aux previous segments =
        match previous, segments with
        // Horizontal -> Vertical -> Horizontal with a and c having the same sign
        | Some ({X = a; Y = 0.}), ({X = 0.; Y = _} as middle) :: ({X = c; Y = 0.} as last) :: rest when a * c > 0. ->
            true
        // Vertical -> Horizontal -> Vertical with b and d having the same sign
        | Some ({X = 0.; Y = b}), ({X = _; Y = 0.} as middle) :: ({X = 0.; Y = d} as last) :: rest when b * d > 0. ->
            true
        | _, current :: rest ->
            aux (Some current) rest
        | _, [] ->
            false

    aux None segments


// find the singly connected Components on the sheet
let findSinglyConnectedComponents (model:SheetT.Model) =
    // Get all the parallel wires of the model
    let singlyConnectedWires = 
        model.Wire.Wires
        |> Map.toList
        |> List.map snd
        |> List.filter (fun wire -> isParallel model wire.WId)
    // Get all the ports that are singly connected(both inputs and outputs)
    let singlyConnectedPorts = 
        singlyConnectedWires
        |> List.map (fun wire -> wire.InputPort)
        |> List.map (fun (InputPortId id) -> id)
        |> List.append 
            (singlyConnectedWires
            |> List.map (fun wire -> wire.OutputPort)
            |> List.map (fun (OutputPortId id) -> id))
    // Get all the ports of the model
    let allPorts = 
        model.Wire.Symbol.Ports
        |> Map.toList
        |> List.map snd
    // create a map of the host id and the number of singly connected ports
    let initialMap: Map<string, int> = Map.empty
    let updateMapWithPort (acc: Map<string, int>) (port: Port) =
        if List.contains port.Id singlyConnectedPorts then
            // If the port ID is in the list, update the count for its HostId
            match Map.tryFind port.HostId acc with
            | Some(count) -> Map.add port.HostId (count + 1) acc
            | None -> Map.add port.HostId 1 acc
        else
            acc
    // filter the map to only include the host id with one singly connected port, returned as a list
    allPorts
    |> List.fold updateMapWithPort initialMap
    |> Map.filter (fun _ count -> count = 1)
    |> Map.toList
    |> List.map fst


let calculateShiftForSimplification (segments: XYPos list) : XYPos =
    let inline sign x = if x < 0. then -1. elif x > 0. then 1. else 0.

    // Function to process segments and return updated segments and their shift
    let rec processSegments segments (accShift: XYPos) =
        let rec reduceSegments acc processed rest =
            match rest with
            | ({X = x1; Y = 0.} as first) :: ({X = 0.; Y = y} as second) :: ({X = x3; Y = 0.} as third) :: tail when sign x1 = sign x3 ->
                // Simplify H-V-H by shifting subsequent segments
                let newAccShift = {X = accShift.X; Y = accShift.Y + y}
                let newProcessed = processed @ [first; {X = x1 + x3; Y = 0.}]
                let adjustedTail = tail |> List.map (fun seg -> {X = seg.X; Y = seg.Y - y})
                reduceSegments newAccShift newProcessed (adjustedTail)
            | ({X = 0.; Y = y1} as first) :: ({X = x; Y = 0.} as second) :: ({X = 0.; Y = y3} as third) :: tail when sign y1 = sign y3 ->
                // Simplify V-H-V by shifting subsequent segments
                let newAccShift = {X = accShift.X + x; Y = accShift.Y}
                let newProcessed = processed @ [{X = 0.; Y = y1 + y3}]
                let adjustedTail = tail |> List.map (fun seg -> {X = seg.X - x; Y = seg.Y})
                reduceSegments newAccShift newProcessed (adjustedTail)
            | head :: tail ->
                reduceSegments acc (processed @ [head]) tail
            | [] ->
                (acc, processed)

        let (newShift, newSegments) = reduceSegments {X = 0.; Y = 0.} [] segments
        if newSegments = segments then
            (segments, {X = accShift.X + newShift.X; Y = accShift.Y + newShift.Y})
        else
            processSegments newSegments {X = accShift.X + newShift.X; Y = accShift.Y + newShift.Y}

    let (_, totalShift) = processSegments segments {X = 0.0; Y = 0.0}
    totalShift




// find the singly connected Wires on the sheet
let findSinglyConnectedWiresAndShifts (model:SheetT.Model) =
    // Get all the connected wires of the model
    let wires = 
        model.Wire.Wires
        |> Map.toList
        |> List.map snd

    // Filter wires to find those that are singly connected
    let singlyConnectedWires = 
        wires
        |> List.filter (fun wire -> 
            // Assuming isSinglyConnected checks if a wire is singly connected
            // You might need to implement this check based on your model's specifics.
            isParallel model wire.WId)

    // For each singly connected wire, calculate the required shift to remove the parallel feature
    singlyConnectedWires
    |> List.map (fun wire ->
        let segments = visibleSegments wire.WId model // Assuming this retrieves the segments of the wire
        let shiftRequired = calculateShiftForSimplification segments
        (wire.WId, shiftRequired))
    |> Map.ofList

// find the connection ids of a symbol id
let findConnectionIdsOfSymbol (model:SheetT.Model) (symbolId: string) =

// find all ports of a symbol id
    let findPortsOfSymbol (model:SheetT.Model) (symbolId: string) =
        model.Wire.Symbol.Ports
        |> Map.toList
        |> List.filter (fun (_, port) -> port.HostId = symbolId)
        |> List.map fst

    // find all the connection ids of a port
    let findConnectionIdsOfPort (model:SheetT.Model) (portId: string) =
        let inputConnectionIds = 
            model.Wire.Wires
            |> Map.toList
            |> List.filter (fun (_, wire) -> 
                match wire.InputPort with
                | InputPortId id -> id = portId)
            |> List.map (fun (id, _) -> id)
        let outputConnectionIds = 
            model.Wire.Wires
            |> Map.toList
            |> List.filter (fun (_, wire) -> 
                match wire.OutputPort with
                | OutputPortId id -> id = portId)
            |> List.map (fun (id, _) -> id)
        inputConnectionIds @ outputConnectionIds
    // First, find all ports of the given symbol
    let portsOfSymbol = findPortsOfSymbol model symbolId
    
    // Then, for each port, find all associated connection IDs
    let connectionIds = 
        portsOfSymbol
        |> List.collect (fun portId -> findConnectionIdsOfPort model portId)
        |> List.distinct // Remove duplicates if any

    connectionIds

// Decide if a connection id is the Input of a symbol
let isInputOfSymbol (model: SheetT.Model) (symbolId: string) (connectionId: ConnectionId) =
    match model.Wire.Wires.TryFind(connectionId) with
    | Some(wire) ->
    // Get the output port id of the wire
        let outputPortId = 
            match wire.InputPort with
            | InputPortId id -> id
        // Find all ports of a the symbol id
        let allPorts = 
            model.Wire.Symbol.Ports
            |> Map.toList
            |> List.map snd
            |> List.filter (fun port -> port.PortType = PortType.Input && port.HostId = symbolId)
        let fin = allPorts |> List.exists (fun port -> port.Id = outputPortId)
        fin
    | None -> false
    
        

// Function to find how much each symbol needs to be shifted to align singly connected wires
let findAlignment (model: SheetT.Model) =
    // Find all singly connected components ids
    let singlyConnectedComponents = findSinglyConnectedComponents model
    // Find all singly connected wire ids and their required shifts
    let singlyConnectedWiresAndShifts = findSinglyConnectedWiresAndShifts model
    // Find map of singly connected symbol id to connection ids
    let singlyConnectedSymbolToConnectionIds = 
        singlyConnectedComponents
        |> List.map (fun symbolId -> (symbolId, findConnectionIdsOfSymbol model symbolId))
        |> Map.ofList
    // Find all singly connected wire ids
    let allSinglyConnectedWires = 
        singlyConnectedWiresAndShifts
        |> Map.toList
        |> List.map fst
    
    let findSymbolIdForConnectionId connectionId =
        singlyConnectedSymbolToConnectionIds
        |> Map.toList
        |> List.tryFind (fun (_, connIds) -> List.contains connectionId connIds)
        |> Option.map fst

    let findShiftForSymbol symbolId =
        allSinglyConnectedWires
        |> List.choose (fun wireId ->
            match findSymbolIdForConnectionId wireId with
            | Some id when id = symbolId -> Some wireId
            | _ -> None)
        |> List.tryPick (fun wireId -> Map.tryFind wireId singlyConnectedWiresAndShifts)

    let alignmentMap = 
        singlyConnectedComponents
        |> List.map (fun symbolId -> (symbolId, findShiftForSymbol symbolId))
        |> List.choose (fun (symbolId, shiftOpt) ->
            match shiftOpt with
            | Some shift -> Some (symbolId, shift)
            | None -> None)
        |> Map.ofList

    // Additional logic to check if the component is the input of any connection ID
    // and to negate the Y value if true
    let updatedAlignmentMap =
        alignmentMap
        |> Map.map (fun symbolId xyPos ->
            let isInput = 
                allSinglyConnectedWires
                |> List.exists (fun wireId -> isInputOfSymbol model symbolId wireId)
            
            if isInput then
                { X = xyPos.X ; Y = -xyPos.Y }
            else
                xyPos)

    updatedAlignmentMap
    
// Function to move a symbol by a given shift
let moveSymbol (move: XYPos) (sym: Symbol) : Symbol =
        {sym with
            Moving = true
            Pos = sym.Pos + move
            Component = {sym.Component with
                            X = sym.Component.X + move.X
                            Y = sym.Component.Y + move.Y
                        }
            LabelBoundingBox = {sym.LabelBoundingBox with
                                    TopLeft =  sym.LabelBoundingBox.TopLeft + move}
        }

// Function to adjust the position of singly-connected components
let alignSinglyConnectedComponents (model: SheetT.Model) : SheetT.Model =

    
    let alignmentString = findAlignment model

    // Convert map keys from string to ComponentId
    let alignment = 
        alignmentString
        |> Map.toList
        |> List.map (fun (keyStr, shift) -> (ComponentId keyStr, shift))
        |> Map.ofList

    // Function to adjust the position of a symbol based on the provided shift (XYPos)
    let adjustSymbolPosition (symbol: Symbol) (shift: XYPos) : Symbol =
        // let newPos = { X = symbol.Pos.X + shift.X; Y = symbol.Pos.Y + shift.Y }
        moveSymbol shift symbol
    
    

    // Convert ComponentId to string for matching
    let componentIdToString (ComponentId s) = s

    // Adjust the positions of all singly-connected symbols
    let adjustedSymbols =
        model.Wire.Symbol.Symbols
        |> Map.map (fun cid sym ->
            let cidStr = componentIdToString cid
            match Map.tryFind cid alignment with
            | Some shift -> adjustSymbolPosition sym shift
            | None -> sym)

    let allPorts = 
        model.Wire.Symbol.Ports
        |> Map.toList
        |> List.map snd
        |> List.filter (fun port -> port.PortType = PortType.Input || port.PortType = PortType.Output)
        |> List.map (fun port -> ComponentId(port.Id))

    // Return a new model with the adjusted symbols
    let newWire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = adjustedSymbols } }
    let symList = alignment|> Map.keys |> Seq.toList
    let newModel = updateWires newWire  symList {X=0;Y=0}


    { model with Wire = newModel }



    
///----------------------------------------processing-more-than-one-parallel----------------------------------------///
/// ---------------------------------------------------------------------------------------------------------------///
/// ---------------------------------------------------------------------------------------------------------------///

// find the rest parallel wires and its shifts of the model
let findParallelWiresAndShifts (model:SheetT.Model) =
    // Get all the connected wires of the model
    let wires = 
        model.Wire.Wires
        |> Map.toList
        |> List.map snd

    // Filter wires to find those that are parallel
    let parallelWires = 
        wires
        |> List.filter (fun wire -> 
            // Assuming isParallel checks if a wire is parallel
            // You might need to implement this check based on your model's specifics.
            isParallel model wire.WId)

    // For each parallel wire, calculate the required shift to remove the parallel feature
    parallelWires
    |> List.map (fun wire ->
        let segments = visibleSegments wire.WId model // Assuming this retrieves the segments of the wire
        let shiftRequired = calculateShiftForSimplification segments
        (wire.WId, shiftRequired))
    |> Map.ofList
    // filter out the values that are too small, to reduce the complexity for re-routing
    |> Map.filter (fun _ xyPos -> (xyPos.X >= 0.0 && xyPos.Y >= 0.2) || (xyPos.X >= 0.2 && xyPos.Y >= 0.0))


// Function to adjust the position of multiple-connected components
let alignMultipleComponents (model: SheetT.Model) : SheetT.Model =
    let parallelWires = findParallelWiresAndShifts model

    let InputPortMap = 
        parallelWires
        |> Map.fold (fun acc connectionId xyPos ->
            match model.Wire.Wires.TryFind(connectionId) with
            | Some(wire) ->
                let (OutputPortId inputPortIdStr) = wire.OutputPort // Extracting the string from InputPortId
                Map.add inputPortIdStr xyPos acc
            | None -> acc
        ) Map.empty
    let symbolIdMap = 
        InputPortMap
        |> Map.fold (fun acc inputPortId xyPos ->
            match model.Wire.Symbol.Ports.TryFind(inputPortId) with
            | Some(port) ->
                let hostId = port.HostId
                Map.add hostId xyPos acc
            | None -> acc
        ) Map.empty

    let shiftPerComponent = 
        symbolIdMap
        |> Map.toList
        |> List.map (fun (keyStr, shift) -> (ComponentId keyStr, shift))
        |> Map.ofList
        // |> Map.filter (fun _ xyPos -> xyPos.X >= 0.5 && xyPos.Y >= 0.5)
    
    let updatedSymbols = 
        model.Wire.Symbol.Symbols
        |> Map.map (fun key symbol ->
            match shiftPerComponent.TryFind(key) with
            | Some(shift) -> moveSymbol shift symbol
            | None -> symbol)
    
    let newWire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbols } }
    let symList = shiftPerComponent|> Map.keys |> Seq.toList
    let newModel = updateWires newWire  symList {X=0;Y=0}


    { model with Wire = newModel }


///----------------------------------------------ensure-no-overlapping---------------------------------------------///
/// ---------------------------------------------------------------------------------------------------------------///
/// ---------------------------------------------------------------------------------------------------------------///

// funtions to find overlapping symbols
let overlap2DBox (box1: BoundingBox) (box2: BoundingBox) : bool =
    let rightOfBox1 = box1.TopLeft.X + box1.W
    let bottomOfBox1 = box1.TopLeft.Y + box1.H
    let rightOfBox2 = box2.TopLeft.X + box2.W
    let bottomOfBox2 = box2.TopLeft.Y + box2.H

    if rightOfBox1 < box2.TopLeft.X then false // Box1 is to the left of Box2
    elif box1.TopLeft.X > rightOfBox2 then false // Box1 is to the right of Box2
    elif bottomOfBox1 < box2.TopLeft.Y then false // Box1 is above Box2
    elif box1.TopLeft.Y > bottomOfBox2 then false // Box1 is below Box2
    else true // Boxes overlap

let findIntersectingSymbols (model: Model) : (Symbol * Symbol) list =
    let boxes =  model.BoundingBoxes
    let symbols = model.Wire.Symbol.Symbols |> Map.toList |> List.map snd |> List.indexed
    let checkIntersects (acc: (Symbol * Symbol) list) ((i, symbol1): int * Symbol) =
        symbols
        |> List.filter (fun (j, _) -> j > i)
        |> List.fold (fun innerAcc (_, symbol2) ->
            let box1 = boxes.[symbol1.Id]
            let box2 = boxes.[symbol2.Id]
            if overlap2DBox box1 box2 then (symbol1, symbol2) :: innerAcc else innerAcc
        ) acc
    symbols |> List.fold checkIntersects []

let getSymbolBoundingBox (symbol: Symbol) : BoundingBox =  
    let topLeft = symbol.Pos
    let w = symbol.Component.W
    let h = symbol.Component.H
    { TopLeft = topLeft; W = w; H = h }

let doesSymbolIntersect (symbol: Symbol) (otherSymbols: Symbol list) : bool =
    let symbolBox = getSymbolBoundingBox(symbol)
    otherSymbols
    |> List.exists (fun otherSymbol ->
        let otherSymbolBox = getSymbolBoundingBox(otherSymbol)
        overlap2DBox symbolBox otherSymbolBox)

let findNonIntersectingTranslation (symbol: Symbol) (otherSymbols: Symbol list) initialTranslations =
    // let initialTranslations = [(10., 0.); (-10., 0.); (0., 10.); (0., -10.)]
    let rec tryTranslations translations =
        match translations with
        | [] -> None // If all translations result in intersection, return None.
        | (dx, dy) :: rest ->
            let newPos = { X = symbol.Pos.X + dx; Y = symbol.Pos.Y + dy }
            let updatedSymbol = { symbol with Pos = newPos }
            if not (doesSymbolIntersect updatedSymbol otherSymbols) then Some(newPos)
            else tryTranslations rest

    let result = tryTranslations initialTranslations
    match result with
    | Some(_) -> result
    | None -> 
        // Double the translations and try again if all initial attempts fail
        let doubledTranslations = List.map (fun (dx, dy) -> (dx * 2.0, dy * 2.0)) initialTranslations
        tryTranslations doubledTranslations

// move symbols to avoid overlapping
let updateModelPositions (model: Model) (symPair: (Symbol * Symbol) list) : Model =
    let initialTranslations = [(50., 0.); (-50., 0.); (0., 50.); (0., -50.)]
    let symbolIdMap=
        symPair
        |> List.fold (fun accMap (sym1, _) ->
            // Assuming 'otherSymbols' contains all symbols in the model except 'sym1'
            let otherSymbols =
                symPair
                |> List.filter (fun (otherSym1, otherSym2) -> otherSym1.Id <> sym1.Id && otherSym2.Id <> sym1.Id)
                |> List.collect (fun (s1, s2) -> [s1; s2])
                |> List.filter (fun sym -> sym.Id <> sym1.Id)
            
            // Attempt to find a new position for sym1 that doesn't result in an intersection
            match findNonIntersectingTranslation sym1 otherSymbols initialTranslations with
            | Some(newPos) -> accMap |> Map.add sym1.Id newPos
            | None -> accMap // If no non-intersecting position is found, do not update the position in the map
        ) Map.empty
    
    let updatedSymbols = 
        model.Wire.Symbol.Symbols
        |> Map.map (fun key symbol ->
            match symbolIdMap.TryFind(key) with
            | Some(shift) -> moveSymbol shift symbol
            | None -> symbol)
    
    let newWire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbols } }
    
    let symList = symbolIdMap |> Map.keys |> Seq.toList
    let newModel = updateWires newWire symList {X=0;Y=0} // Assuming {X=0;Y=0} is the default or reset shift

    { model with Wire = newModel }


let resolveIntersectingSymbols (model: Model) : Model =
    // Step 1: Find intersecting symbols
    let intersectingSymbols = findIntersectingSymbols model

    // Step 2: Update model positions based on intersecting pairs
    let updatedModel = updateModelPositions model intersectingSymbols

    updatedModel


///--------------------------------------------aligning-custom-component-------------------------------------------///
/// ---------------------------------------------------------------------------------------------------------------///
/// ---------------------------------------------------------------------------------------------------------------///


// Function to find all custom components in the model
let findCustomComponents (model: SheetT.Model) =
    model.Wire.Symbol.Symbols
    |> Map.toList
    |> List.filter (fun (_, sym) ->
        match sym.Component.Type with
        | Custom _ -> true
        | _ -> false)
    |> List.map fst

let findConnectionIdsForPort (model: Model) (portId: string) : ConnectionId list =

    []


// Function to find the port map of a symbol
let findPortMap (model: Model) (symbolId: ComponentId) =
    match model.Wire.Symbol.Symbols.TryFind(symbolId) with
    | Some(symbol) -> 
        symbol.PortMaps.Order
        |> Map.map (fun edge portIds ->
            portIds
            |> List.collect (findConnectionIdsForPort model) // Collects all connection IDs for the port IDs on this edge
        )
    | None -> Map.empty
    

///--------------------------------------------intergrating-translation--------------------------------------------///
/// ---------------------------------------------------------------------------------------------------------------///
/// ---------------------------------------------------------------------------------------------------------------///

let finalUpdate (model: SheetT.Model) =
    let model1 = alignSinglyConnectedComponents model
    let model2 = alignSinglyConnectedComponents model1
    let model3 = alignMultipleComponents model2
    let model4 = alignSinglyConnectedComponents model3
    // let model5 = resolveIntersectingSymbols model4

    model4