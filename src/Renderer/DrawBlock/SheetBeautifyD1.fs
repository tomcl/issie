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
let isParallel(model: SheetT.Model)(wId: ConnectionId) =
    let segments = visibleSegments wId model
    let rec aux previousDirection segments =
        match previousDirection, segments with
        | _, ({X = 0.; Y = _} :: {X = _; Y = 0.} :: {X = 0.; Y = _} :: _)
        | _, ({X = _; Y = 0.} :: {X = 0.; Y = _} :: {X = _; Y = 0.} :: _) ->
            // Detected two continuous direction changes: H -> V -> H or V -> H -> V
            true
        | _, ({X = 0.; Y = _} as current) :: rest ->
            // Current segment is horizontal, proceed to check the rest
            aux "Horizontal" rest
        | _, ({X = _; Y = 0.} as current) :: rest ->
            // Current segment is vertical, proceed to check the rest
            aux "Vertical" rest
        | _, _ :: rest ->
            // No direction change detected, proceed with the next segment
            aux previousDirection rest
        | _, [] ->
            // No more segments to check, and no pattern found
            false

    // Start checking from the beginning without a predefined direction
    aux "" segments

// find the singly connected Ports on the sheet
let findSinglyConnectedPorts (model:SheetT.Model) =
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


// find the singly connected component on the sheet
let findSinglyConnectedComponents (model: SheetT.Model) : ComponentId list =
    let singlyConnectedPorts = findSinglyConnectedPorts model

    // Collecting ComponentIds from singly connected ports
    let singlyConnectedComponentIds =
        model.Wire.Symbol.Ports
        |> Map.toList
        |> List.choose (fun (portId, port) ->
            if List.contains port.Id singlyConnectedPorts then
                // Finding the symbol that contains the port
                let symbolOption = 
                    model.Wire.Symbol.Symbols
                    |> Map.filter (fun _ symbol -> 
                        symbol.PortMaps.Orientation |> Map.containsKey portId)
                    |> Map.tryPick (fun _ symbol -> Some(symbol.Id))
                match symbolOption with
                | Some(componentId) -> Some(componentId)
                | None -> None
            else None
        )
        |> List.distinct

    singlyConnectedComponentIds

type SegVector = {Start: XYPos; Dir: XYPos} // A segment vector with absolute start and direction

// Function to transform wire segments into a map of wire ID to a list of segments with absolute start and direction
let transformWireSegmentsToAbsoluteMap (model: SheetT.Model) =
    model.Wire.Wires
    |> Map.fold (fun accMap wId wire ->
        let origin = wire.StartPos
        let vectors = visibleSegments wId model

        // Transform the list of vectors into a list of segments with absolute start and direction
        let segments, _ = 
            vectors
            |> List.fold (fun (acc: SegVector list, lastPos: XYPos) vector -> 
                let newPos = lastPos + vector
                let segment = { Start = lastPos; Dir = vector }
                (acc @ [segment], newPos)
            ) ([], origin)

        // Add the wire ID and its segments to the map
        Map.add wId segments accMap
    ) Map.empty



// Function to adjust the position of singly-connected components
let alignSinglyConnectedComponents (model: SheetT.Model) : SheetT.Model =
    // Find all singly connected components
    let singlyConnectedComponents = findSinglyConnectedComponents model

    // Define a function to determine the orientation of the parallel wire connecting to a component
    let wireOrientation (wId: ConnectionId) : string =
        // This involve looking at the wire's segments and determining if they are mostly horizontal or vertical
        // Return "Horizontal", "Vertical", or None if it cannot be determined
        let segments = visibleSegments wId model
    
        // Sum up the number of the x and y components of the vectors
        let (sumX, sumY) =
            segments
            |> List.fold (fun (accX, accY) segment ->
                // Increment the accumulator by segment's X if Y is zero, and vice versa
                if segment.Y = 0.0 then (accX + 1, accY)
                else (accX, accY + 1)
            ) (0, 0)
    
        // Determine the orientation based on whether the sum of the absolute x or y components is greater
        if sumX > sumY then "Horizontal" else "Vertical"

    // Define a function to adjust the component's position based on the wire's orientation
    let adjustComponentPosition (componentId: ComponentId) (orientation: string) : unit =
        // Implementation depends on how you can adjust component positions within the model
        // This involves calculating a new position that aligns with the wire's other endpoint
        // and then updating the model accordingly
        ()

    // Iterate over each singly connected component, determine the wire's orientation, and adjust the component's position
    singlyConnectedComponents |> List.iter (fun componentId ->
        match wireOrientation componentId with
        | Some(orientation) -> adjustComponentPosition componentId orientation
        | None -> () // Do nothing if the wire's orientation cannot be determined
    )

    // Return the updated model with aligned components
    model

    
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

