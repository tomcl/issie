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
    // Get all the connected wires of the model
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
let calculateShiftForSimplification (segments: XYPos list) : XYPos =
    let inline sign x = if x < 0. then -1. else if x > 0. then 1. else 0.

    let rec aux (accX, accY) segments =
        match segments with
        | ({X = x1; Y = 0.} :: {X = 0.; Y = y} :: {X = x2; Y = 0.} :: rest) when sign x1 = sign x2 ->
            // Simplify H-V-H to a single vertical shift, continue with the rest
            aux (accX, accY + y) rest
        | ({X = 0.; Y = y1} :: {X = x; Y = 0.} :: {X = 0.; Y = y2} :: rest) when sign y1 = sign y2 ->
            // Simplify V-H-V to a single horizontal shift, continue with the rest
            aux (accX + x, accY) rest
        | _ :: rest ->
            // Non-simplifiable pattern or single segment left, just skip it
            aux (accX, accY) rest
        | [] ->
            // Return total accumulated shifts in x and y as an XYPos
            {X = accX; Y = accY}

    aux (0.0, 0.0) segments


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







// Function to adjust the position of singly-connected components
// let alignSinglyConnectedComponents (model: SheetT.Model) : SheetT.Model =
//     // Find all singly connected components
//     let singlyConnectedComponents = findSinglyConnectedComponents model

//     // Define a function to determine the orientation of the parallel wire connecting to a component
//     let wireOrientation (wId: ConnectionId) : string =
//         // This involve looking at the wire's segments and determining if they are mostly horizontal or vertical
//         // Return "Horizontal", "Vertical", or None if it cannot be determined
//         let segments = visibleSegments wId model
    
//         // Sum up the number of the x and y components of the vectors
//         let (sumX, sumY) =
//             segments
//             |> List.fold (fun (accX, accY) segment ->
//                 // Increment the accumulator by segment's X if Y is zero, and vice versa
//                 if segment.Y = 0.0 then (accX + 1, accY)
//                 else (accX, accY + 1)
//             ) (0, 0)
    
//         // Determine the orientation based on whether the sum of the absolute x or y components is greater
//         if sumX > sumY then "Horizontal" else "Vertical"

//     // Define a function to adjust the component's position based on the wire's orientation
//     let adjustComponentPosition (model: SheetT.Model) (dx: float) (dy: float) : SheetT.Model =
//         // Find singly connected component IDs
//         let singlyConnectedComponentIds = findSinglyConnectedComponents model

//         // Map to update each symbol's position if it's in the list of singly connected component IDs
//         let updatedSymbols = model.Wire.Symbol.Symbols |> Map.map (fun compId symbol ->
//             if List.contains compId singlyConnectedComponentIds then
//                 // If the symbol's component ID is in the list, adjust its position
//                 let newPos = { symbol.Pos with X = symbol.Pos.X + dx; Y = symbol.Pos.Y + dy }
//                 { symbol with Pos = newPos }
//             else
//                 // Otherwise, leave the symbol unchanged
//                 symbol
//         )

//         // Construct a new model with the updated symbols
//         let updatedModel = {
//             model with
//             Wire = {
//                 model.Wire with
//                 Symbol = {
//                     model.Wire.Symbol with
//                     Symbols = updatedSymbols
//                 }
//             }
//         }

//         // Return the updated model
//         updatedModel

//     // Iterate over each singly connected component, determine the wire's orientation, and adjust the component's position
//     // singlyConnectedComponents |> List.iter (fun componentId ->
//     //     match wireOrientation componentId with
//     //     | Some(orientation) -> adjustComponentPosition componentId orientation
//     //     | None -> () // Do nothing if the wire's orientation cannot be determined
//     // )

//     // Return the updated model with aligned components
//     model

    
///----------------------------------------processing-more-than-one-connections----------------------------------------///
/// ---------------------------------------------------------------------------------------------------------------///
/// ---------------------------------------------------------------------------------------------------------------///

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

