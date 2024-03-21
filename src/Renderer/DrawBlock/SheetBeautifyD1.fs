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
///----------------------------------------processing-only-one-connection----------------------------------------///
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

// calculate how much to shift the wire to remove the parallel feature
// let calculateShiftForSimplification (segments: XYPos list) : XYPos =
//     let inline sign x = if x < 0. then -1. else if x > 0. then 1. else 0.

//     let rec aux (accX, accY) segments =
//         match segments with
//         | ({X = x1; Y = 0.} :: {X = 0.; Y = y} :: {X = x2; Y = 0.} :: rest) when sign x1 = sign x2 ->
//             // Simplify H-V-H to a single vertical shift, continue with the rest
//             aux (accX, accY + y) rest
//         | ({X = 0.; Y = y1} :: {X = x; Y = 0.} :: {X = 0.; Y = y2} :: rest) when sign y1 = sign y2 ->
//             // Simplify V-H-V to a single horizontal shift, continue with the rest
//             aux (accX + x, accY) rest
//         | _ :: rest ->
//             // Non-simplifiable pattern or single segment left, just skip it
//             aux (accX, accY) rest
//         | [] ->
//             // Return total accumulated shifts in x and y as an XYPos
//             {X = accX; Y = accY}

//     aux (0.0, 0.0) segments



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



    
///----------------------------------------processing-more-than-one-connections----------------------------------------///
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
    
    let updatedSymbols = 
        model.Wire.Symbol.Symbols
        |> Map.map (fun key symbol ->
            match shiftPerComponent.TryFind(key) with
            | Some(shift) -> 
                let updatedPos = { X = symbol.Pos.X + shift.X; Y = symbol.Pos.Y + shift.Y }
                { symbol with Pos = updatedPos }
            | None -> symbol)
    
    let newWire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbols } }
    let symList = shiftPerComponent|> Map.keys |> Seq.toList
    let newModel = updateWires newWire  symList {X=0;Y=0}


    { model with Wire = newModel }



// find the rest multiple connected Components on the sheet
// let findMultipleConnectedComponents (model:SheetT.Model) =
//     // Get all the parallel wires of the model
//     let singlyConnectedWires = 
//         model.Wire.Wires
//         |> Map.toList
//         |> List.map snd
//         |> List.filter (fun wire -> isParallel model wire.WId)
//     // printfn "singlyConnectedWires: %A" singlyConnectedWires
//     // Get all the ports that are singly connected(both inputs and outputs)
//     let singlyConnectedPorts = 
//         singlyConnectedWires
//         |> List.map (fun wire -> wire.InputPort)
//         |> List.map (fun (InputPortId id) -> id)
//         |> List.append 
//             (singlyConnectedWires
//             |> List.map (fun wire -> wire.OutputPort)
//             |> List.map (fun (OutputPortId id) -> id))
//     // printfn "singlyConnectedPorts: %A" singlyConnectedPorts
//     // Get all the ports of the model
//     let allPorts = 
//         model.Wire.Symbol.Ports
//         |> Map.toList
//         |> List.map snd
//     // create a map of the host id and the number of singly connected ports
//     let initialMap: Map<string, int> = Map.empty
//     let updateMapWithPort (acc: Map<string, int>) (port: Port) =
//         if List.contains port.Id singlyConnectedPorts then
//             // If the port ID is in the list, update the count for its HostId
//             match Map.tryFind port.HostId acc with
//             | Some(count) -> Map.add port.HostId (count + 1) acc
//             | None -> Map.add port.HostId 1 acc
//         else
//             acc
//     // filter the map to only include the component id with one singly connected port, returned as a list
//     allPorts
//     |> List.fold updateMapWithPort initialMap
//     |> Map.filter (fun _ count -> count > 1)
//     |> Map.toList
//     |> List.map fst

// Function to find how much each symbol needs to be shifted to align multiple connected wires
// let findMultipleAlignment (model: SheetT.Model) =
//     // Find all singly connected components ids
//     let singlyConnectedComponents = findMultipleConnectedComponents model
//     // Find all singly connected wire ids and their required shifts
//     let singlyConnectedWiresAndShifts = findSinglyConnectedWiresAndShifts model
//     // Find map of singly connected symbol id to connection ids
//     let singlyConnectedSymbolToConnectionIds = 
//         singlyConnectedComponents
//         |> List.map (fun symbolId -> (symbolId, findConnectionIdsOfSymbol model symbolId))
//         |> Map.ofList
//     // Find all singly connected wire ids
//     let allSinglyConnectedWires = 
//         singlyConnectedWiresAndShifts
//         |> Map.toList
//         |> List.map fst
    
//     let findSymbolIdForConnectionId connectionId =
//         singlyConnectedSymbolToConnectionIds
//         |> Map.toList
//         |> List.tryFind (fun (_, connIds) -> List.contains connectionId connIds)
//         |> Option.map fst

//     let findShiftForSymbol symbolId =
//         allSinglyConnectedWires
//         |> List.choose (fun wireId ->
//             match findSymbolIdForConnectionId wireId with
//             | Some id when id = symbolId -> Some wireId
//             | _ -> None)
//         |> List.tryPick (fun wireId -> Map.tryFind wireId singlyConnectedWiresAndShifts)

//     let alignmentMap = 
//         singlyConnectedComponents
//         |> List.map (fun symbolId -> (symbolId, findShiftForSymbol symbolId))
//         |> List.choose (fun (symbolId, shiftOpt) ->
//             match shiftOpt with
//             | Some shift -> Some (symbolId, shift)
//             | None -> None)
//         |> Map.ofList

//     // Additional logic to check if the component is the input of any connection ID
//     // and to negate the Y value if true
//     let updatedAlignmentMap =
//         alignmentMap
//         |> Map.map (fun symbolId xyPos ->
//             let isInput = 
//                 allSinglyConnectedWires
//                 |> List.exists (fun wireId -> isInputOfSymbol model symbolId wireId)
            
//             if isInput then
//                 { X = xyPos.X ; Y = -xyPos.Y }
//             else
//                 xyPos)

//     updatedAlignmentMap


// Function to adjust the position of multiple-connected components
// let alignMultipleComponents (model: SheetT.Model) : SheetT.Model =
//     let alignmentString = findMultipleAlignment model

//     let allSymbol = 
//         model.Wire.Symbol.Symbols
//         |> Map.toList
//         |> List.map fst
//     // Convert map keys from string to ComponentId
//     let alignment = 
//         alignmentString
//         |> Map.toList
//         |> List.map (fun (keyStr, shift) -> (ComponentId keyStr, shift))
//         |> Map.ofList

//     // Function to adjust the position of a symbol based on the provided shift (XYPos)
//     let adjustSymbolPosition (symbol: Symbol) (shift: XYPos) : Symbol =
//         let newPos = { X = symbol.Pos.X + shift.X; Y = symbol.Pos.Y + shift.Y }
//         { symbol with Pos = newPos }

//     // Convert ComponentId to string for matching
//     let componentIdToString (ComponentId s) = s

//     // Adjust the positions of all multiple-connected symbols
//     let adjustedSymbols =
//         model.Wire.Symbol.Symbols
//         |> Map.map (fun cid sym ->
//             let cidStr = componentIdToString cid
//             match Map.tryFind cid alignment with
//             | Some shift -> adjustSymbolPosition sym shift
//             | None -> sym)

//     // Return a new model with the adjusted symbols
//     let newWire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = adjustedSymbols } }
//     let symList = alignment|> Map.keys |> Seq.toList
//     let newModel = updateWires newWire  allSymbol {X=0;Y=0}


//     { model with Wire = newModel }


// determine if two ports are on the opposite sides of a symbol
let isOppositeSide (sym1: Symbol) (p1:Port) (sym2: Symbol) (p2:Port) =
    let side1 = Map.tryFind p1.Id sym1.PortMaps.Orientation
    let side2 = Map.tryFind p2.Id sym2.PortMaps.Orientation
    match (side1, side2) with
    | (Some side1, Some side2) -> side1.Opposite = side2
    | _ -> false

// generate lists of all the ports that are connected to a given port(one of them is empty so only output one list)
let connectedPorts (model: SheetT.Model) (port: Port) =
    let inputPorts, outputPorts = 
        model.Wire.Wires 
        |> Map.values
        |> Seq.toList
        |> List.fold (fun (accInput, accOutput) wire ->
            match port.PortType, wire.InputPort, wire.OutputPort with
            | PortType.Input, InputPortId id, outputPort when id = port.Id -> (accInput, outputPort :: accOutput)
            | PortType.Output, inputPort, OutputPortId id when id = port.Id -> (inputPort :: accInput, accOutput)
            | _ -> (accInput, accOutput)
        ) ([], [])
    // return the list of connected ports as string list
    match port.PortType with
    | PortType.Input -> outputPorts |> List.map (fun (OutputPortId id) -> id)
    | PortType.Output -> inputPorts |> List.map (fun (InputPortId id) -> id)

// Enlarge the space between two ports such that two symbols have same port space alignment, make sure it is opposite side already
let alignScale (model: SymbolT.Model) 
    (sym1: Symbol) (sym1Port1:Port) (sym1Port2:Port) 
    (sym2: Symbol) (sym2Port1:Port) (sym2Port2:Port) =
    let xy1= readPortPosition sym1 sym1Port1
    let xy2= readPortPosition sym2 sym1Port2
    let xy3= readPortPosition sym2 sym2Port1
    let xy4= readPortPosition sym2 sym2Port2
    // Determine alignment axis
    let axis =
        match Map.tryFind sym1Port1.Id sym1.PortMaps.Orientation, Map.tryFind sym2Port1.Id sym2.PortMaps.Orientation with
        | Some(Top), Some(Bottom) | Some(Bottom), Some(Top) -> "Horizontal"
        | Some(Left), Some(Right) | Some(Right), Some(Left) -> "Vertical"
        | _ -> failwith "Invalid or unsupported port orientation combination"
    
    // Calculate new dimensions
    let newSym1Dims, newSym2Dims =
        match axis with
        | "Horizontal" ->
            // Calculate new dimensions/positions for horizontal alignment
            let width1 = abs(xy2.X - xy1.X)
            let width2 = abs(xy4.X - xy3.X)
            match width1 < width2 with
            | true -> 
                let newWidthsym1 = (width2-width1)/width1 * sym1.Component.W + sym1.Component.W
                let newHeightsym1 = sym1.Component.H
                let newWidthsym2 = sym2.Component.W
                let newHeightsym2 = sym2.Component.H
                ((newWidthsym1,newHeightsym1), (newWidthsym2,newHeightsym2))
            | false ->
                let newWidthsym1 = sym1.Component.W
                let newHeightsym1 = sym1.Component.H
                let newWidthsym2 = (width1-width2)/width2 * sym2.Component.W + sym2.Component.W
                let newHeightsym2 = sym2.Component.H
                ((newWidthsym1,newHeightsym1), (newWidthsym2,newHeightsym2))

        | "Vertical" ->
            // Calculate new dimensions/positions for vertical alignment
            let height1 = abs(xy2.Y - xy1.Y)
            let height2 = abs(xy4.Y - xy3.Y)
            match height1 < height2 with
            | true -> 
                let newWidthsym1 = sym1.Component.W
                let newHeightsym1 = (height2-height1)/height1 * sym1.Component.H + sym1.Component.H
                let newWidthsym2 = sym2.Component.W
                let newHeightsym2 = sym2.Component.H
                ((newWidthsym1,newHeightsym1), (newWidthsym2,newHeightsym2))
            | false ->
                let newWidthsym1 = sym1.Component.W
                let newHeightsym1 = sym1.Component.H
                let newWidthsym2 = sym2.Component.W
                let newHeightsym2 = (height1-height2)/height2 * sym2.Component.H + sym2.Component.H
                ((newWidthsym1,newHeightsym1), (newWidthsym2,newHeightsym2))
        
        | _ -> failwith "Invalid or unsupported axis"

    // Apply changes using lenses
    let sym1Option = Map.tryFind sym1.Id model.Symbols
    let sym2Option = Map.tryFind sym2.Id model.Symbols

    // Update dimensions if symbols are found
    let updatedSymbols = model.Symbols
                          |> Map.change sym1.Id (Option.map (fun sym -> snd customComponentDimensionsLens newSym1Dims sym))
                          |> Map.change sym2.Id (Option.map (fun sym -> snd customComponentDimensionsLens newSym2Dims sym))

    // Return updated model
    { model with Symbols = updatedSymbols }


// change the position of the symbol to align with two ports

