module SheetBeautifyHelpers

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team
open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BlockHelpers
open Symbol
open Helpers

//TestDrawBlock
let visibleSegments (wId: ConnectionId) (model: SheetT.Model): XYPos list =

        let wire = model.Wire.Wires[wId] // get wire from model 

        /// helper to match even and off integers in patterns (active pattern)
        let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

        /// Convert seg into its XY Vector (from start to end of segment).
        /// index must be the index of seg in its containing wire.
        let getSegmentVector (index:int) (seg: BusWireT.Segment) =
            // The implicit horizontal or vertical direction  of a segment is determined by 
            // its index in the list of wire segments and the wire initial direction
            match index, wire.InitialOrientation with
            | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> {X=0.; Y=seg.Length}
            | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> {X=seg.Length; Y=0.}

        /// Return a list of segment vectors with 3 vectors coalesced into one visible equivalent
        /// if this is possible, otherwise return segVecs unchanged.
        /// Index must be in range 1..segVecs
        let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int)  =
            if segVecs[index] =~ XYPos.zero
            then
                segVecs[0..index-2] @
                [segVecs[index-1] + segVecs[index+1]] @
                segVecs[index+2..segVecs.Length - 1]
            else
                segVecs

        wire.Segments
        |> List.mapi getSegmentVector
        |> (fun segVecs ->
                (segVecs,[1..segVecs.Length-2])
                ||> List.fold tryCoalesceAboutIndex)

//B1R
let getCustomDimensions (sym: Symbol) : float*float =
    (sym.Component.H, sym.Component.W)

//B1W
let updateCustomDimensions (h: float) (w:float) (sym: Symbol) : Symbol =
    let updatedComponent = {sym.Component with H = h; W = w}
    {sym with Component = updatedComponent}

//B2W
let updateSymbolPos (sym: Symbol) (pos: XYPos): Symbol =
    let newPos = pos
    let comp' = {sym.Component with X = newPos.X; Y = newPos.Y}
    {sym with 
        Component = comp'
        Pos = newPos
        LabelBoundingBox = {sym.LabelBoundingBox with TopLeft = sym.LabelBoundingBox.TopLeft + pos}
    }

//B3R
let getSidePorts (sym:Symbol) (side: Edge) : string list =
    match sym.PortMaps.Order.TryFind(side) with
    | Some(ports) -> ports
    | None -> []

//B3W
let updateSidePorts (sym:Symbol) (side: Edge) (ports: string list) : Symbol =
    let updatedOrder = Map.add side ports sym.PortMaps.Order
    let updatedOrientation = 
        ports
        |> List.fold (fun accMap port -> Map.add port side accMap) sym.PortMaps.Orientation
    let updatedPortMaps = { Order = updatedOrder; Orientation = updatedOrientation }
    { sym with PortMaps = updatedPortMaps }

//B4R
let getReverseStateMux2 (sym: Symbol): bool option =
    match sym.Component.Type with
    | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 -> sym.ReversedInputPorts
    | _ -> None

//B4W
let updateReverseStateMux2 (sym: Symbol) (state: bool option): Symbol =
    match sym.Component.Type with
    | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 -> {sym with ReversedInputPorts = state}
    | _ -> sym

//B5
let getPortPos (port: Port) (model: SymbolT.Model) : XYPos =
    let portId = port.Id
    getPortLocation None model portId

//B6
let getSymbolOutlineBoundingBox (sym:Symbol): BoundingBox =
    sym.SymbolBoundingBox

//B7R
let getRotationState (sym:Symbol): Rotation =
    sym.STransform.Rotation

//B7W
let updateRotationState (sym: Symbol) (rotation: Rotation): Symbol =
    match rotation with
    | Degree0 | Degree90 | Degree180 | Degree270 ->
        let updatedTransform = {sym.STransform with Rotation = rotation}
        {sym with STransform = updatedTransform}
    | _ -> sym

//B8R
let getFlipState (sym:Symbol): bool =
    sym.STransform.Flipped

//B8W
let updateFlipState (sym: Symbol) (flip: bool): Symbol =
    let updatedTransform = {sym.STransform with Flipped = flip}
    {sym with STransform = updatedTransform}

//T1R
let getSymbolOverlapCount (sym: Symbol) (sheet: SheetT.Model): int =
    let wireModel = sheet.Wire
    let boxes =
        mapValues sheet.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> n,box)
    let overlappingPairs =
        List.allPairs boxes boxes
        |> List.filter (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
    List.length overlappingPairs

//T2R
let getSegmentOverlapCount (sheet: SheetT.Model): int =
    let allWires = sheet.Wire.Wires
    let allSymbolsBoundingBox =
        sheet.Wire.Symbol.Symbols
        |> Map.values
        |> Seq.toList
        |> List.filter (fun s -> s.Annotation = None)
        |> List.map (fun s -> (Symbol.getSymbolBoundingBox s))

    allWires
    |> Map.fold (fun count _ wire ->
        // Start with the initial position of the wire
        let mutable segStart = wire.StartPos
        // Get the visible segments for the wire
        let visibleSegs = visibleSegments wire.WId sheet
        // Check for intersections with each segment
        let intersects =
            visibleSegs
            |> List.exists (fun seg ->
                // Calculate the end position of the current segment
                let segEnd = segStart + seg
                // Check if the current segment intersects with any bounding box
                let result = allSymbolsBoundingBox
                             |> List.exists (fun box -> 
                                 match segmentIntersectsBoundingBox box segStart segEnd with
                                 | Some _ -> true 
                                 | None -> false 
                             )
                segStart <- segEnd
                result
            )
        if intersects then count + 1 else count) 0

//T3R
let countPerpendicularSegments (sheet: SheetT.Model): int =
    let allWires = sheet.Wire.Wires |> Map.toList |> List.map snd

    // Convert a wire and its segment displacement into actual segment start and end positions
    let getSegmentPositions wire =
        let startPos = wire.StartPos
        visibleSegments wire.WId sheet
        |> List.fold (fun (acc, lastPos) seg ->
            let newPos = lastPos + seg
            ((lastPos, newPos) :: acc, newPos) // Prepend to list for efficiency
        ) ([], startPos)
        |> fst
        |> List.rev // Reverse the list to maintain original order

    // Helper to check if segments intersect perpendicularly
    let doSegmentsIntersect ((startPos1, endPos1): XYPos * XYPos) ((startPos2, endPos2): XYPos * XYPos) : bool =
        // Use the epsilon for direct float comparisons
        let epsilon = XYPos.epsilon

        // Determine if segment 1 is vertical or horizontal by comparing X or Y values with epsilon
        let vertical1 = abs(startPos1.X - endPos1.X) <= epsilon
        let horizontal1 = abs(startPos1.Y - endPos1.Y) <= epsilon

        // Determine if segment 2 is vertical or horizontal
        let vertical2 = abs(startPos2.X - endPos2.X) <= epsilon
        let horizontal2 = abs(startPos2.Y - endPos2.Y) <= epsilon

        match vertical1, horizontal1, vertical2, horizontal2 with
        | true, false, false, true -> // Segment 1 is vertical, Segment 2 is horizontal
            (startPos1.X >= min startPos2.X endPos2.X && startPos1.X <= max startPos2.X endPos2.X) &&
            (startPos2.Y >= min startPos1.Y endPos1.Y && startPos2.Y <= max startPos1.Y endPos1.Y)
        | false, true, true, false -> // Segment 1 is horizontal, Segment 2 is vertical
            (startPos2.X >= min startPos1.X endPos1.X && startPos2.X <= max startPos1.X endPos1.X) &&
            (startPos1.Y >= min startPos2.Y endPos2.Y && startPos1.Y <= max startPos2.Y endPos2.Y)
        | _ -> false // Other combinations imply parallel or non-perpendicular segments


    // Helper to check if two wires are from the same net
    let areFromSameNet (wire1: Wire) (wire2: Wire) =
        wire1.OutputPort = wire2.OutputPort

    // Compare segments from different wires to count intersections
    let countIntersections wire1 wire2 =
        let segments1 = getSegmentPositions wire1
        let segments2 = getSegmentPositions wire2
        List.fold (fun acc (startPos1, endPos1) ->
            acc + (segments2 |> List.fold (fun acc' (startPos2, endPos2) ->
                if doSegmentsIntersect (startPos1, endPos1) (startPos2, endPos2) then acc' + 1 else acc'
            ) 0)
        ) 0 segments1

    // Main logic for counting perpendicular intersections
    allWires
    |> List.collect (fun wire1 -> 
        allWires
        |> List.filter (fun wire2 -> not (areFromSameNet wire1 wire2))
        |> List.map (fun wire2 -> countIntersections wire1 wire2)
    )
    |> List.sum

//T4
let countTotalWireLength (sheet: SheetT.Model): float =
    let allWires = sheet.Wire.Wires |> Map.toList |> List.map snd

    //helper: Get list of XY coords of all segments from a wire
    let getSegmentPositions wire =
        let startPos = wire.StartPos
        visibleSegments wire.WId sheet
        |> List.fold (fun (acc, lastPos) seg ->
            let newPos = lastPos + seg
            ((lastPos, newPos) :: acc, newPos) // Prepend to list for efficiency
        ) ([], startPos)
        |> fst
        |> List.rev // Reverse the list to maintain original order

    let overlapLength ((startPos1, endPos1): XYPos * XYPos)  ((startPos2, endPos2): XYPos * XYPos) =
        let epsilon = XYPos.epsilon
        // Determine if segments are vertical or horizontal using epsilon
        let vertical1 = abs(startPos1.X - endPos1.X) <= epsilon
        let horizontal1 = abs(startPos1.Y - endPos1.Y) <= epsilon
        let vertical2 = abs(startPos2.X - endPos2.X) <= epsilon
        let horizontal2 = abs(startPos2.Y - endPos2.Y) <= epsilon
        // Check for parallel and overlapping segments
        if vertical1 && vertical2 && abs(startPos1.X - startPos2.X) <= epsilon then
            // Vertical segments
            let yMin1, yMax1 = min startPos1.Y endPos1.Y, max startPos1.Y endPos1.Y
            let yMin2, yMax2 = min startPos2.Y endPos2.Y, max startPos2.Y endPos2.Y
            if yMax1 >= yMin2 && yMax2 >= yMin1 then
                min yMax1 yMax2 - max yMin1 yMin2  // Overlap length
            else
                0.0  // No overlap
        elif horizontal1 && horizontal2 && abs(startPos1.Y - startPos2.Y) <= epsilon then
            // Horizontal segments
            let xMin1, xMax1 = min startPos1.X endPos1.X, max startPos1.X endPos1.X
            let xMin2, xMax2 = min startPos2.X endPos2.X, max startPos2.X endPos2.X
            if xMax1 >= xMin2 && xMax2 >= xMin1 then
                min xMax1 xMax2 - max xMin1 xMin2  // Overlap length
            else
                0.0  // No overlap
        else
            0.0  // Segments are not parallel or not aligned

    let totalLength =
        allWires
        |> List.collect (fun wire -> getSegmentPositions wire)
        |> List.map (fun (startPos, endPos) -> 
            sqrt (((endPos.X - startPos.X) ** 2.0) + ((endPos.Y - startPos.Y) ** 2.0)))
        |> List.sum

    let totalOverlapLength =
        allWires
        |> List.groupBy (fun wire -> wire.OutputPort)
        |> List.collect (fun (_, wiresInNet) ->
            let segments = wiresInNet |> List.collect getSegmentPositions
            let overlaps = 
                List.collect (fun seg1 -> 
                    segments 
                    |> List.map (fun seg2 -> overlapLength seg1 seg2)
                ) segments
            overlaps
        )
        |> List.sum

    totalLength - totalOverlapLength

//T5
let countWireRightAngles (sheet: SheetT.Model): int =
    let allWires = sheet.Wire.Wires |> Map.toList |> List.map snd
    allWires
    |> List.map (fun wire ->
        let segments = visibleSegments wire.WId sheet
        let numberOfSegments = List.length segments
        // Each wire with at least 2 segments forms a right angle between each consecutive pair of segments
        if numberOfSegments > 1 then numberOfSegments - 1 else 0
    )
    |> List.sum

//T6
let countRetraceSegments (sheet: SheetT.Model) =
    let allWires = sheet.Wire.Wires |> Map.toList |> List.map snd

    let allSymbolsBoundingBox =
        sheet.Wire.Symbol.Symbols
        |> Map.values
        |> Seq.toList
        |> List.filter (fun s -> s.Annotation = None)
        |> List.map (fun s -> (Symbol.getSymbolBoundingBox s))

    let doesSegmentStartInsideSymbol (segStart: XYPos) (bbox: BoundingBox) =
        match segmentIntersectsBoundingBox bbox segStart segStart with
        | Some _ -> true
        | None -> false

    let getSegmentPositions wire =
        let startPos = wire.StartPos
        visibleSegments wire.WId sheet
        |> List.fold (fun (acc, lastPos) seg ->
            let newPos = lastPos + seg
            ((lastPos, newPos) :: acc, newPos) // Prepend to list for efficiency
        ) ([], startPos)
        |> fst
        |> List.rev // Reverse the list to maintain original order

    let findRetracingSegments (segments: Segment list) =
        let sign x = compare x 0.
        List.mapi (fun index _ ->
            if index >= 0 && index < List.length segments - 1 then
                let currSegment = segments.[index]
                let nextSegment = segments.[index + 1]
                if currSegment.IsZero && sign currSegment.Length <> sign nextSegment.Length then
                    Some(index, nextSegment) // Use currSegment to indicate the zero-length segment before retracing
                else
                    None
            else
                None) segments
        |> List.choose id

    let findSegmentsStartingInSymbol (retracingIndices: (int * Segment) list) (positions: (XYPos * XYPos) list) =
        retracingIndices
        |> List.filter (fun (index, _) ->
            let nextSegStartPos = (List.item (index + 1) positions |> fst) // Get start position of the next segment
            allSymbolsBoundingBox |> List.exists (doesSegmentStartInsideSymbol nextSegStartPos))
        |> List.map snd // Return only the segments

    allWires
    |> List.collect (fun wire ->
        let segments = wire.Segments
        let positions = getSegmentPositions wire
        let retracingSegmentsWithIndex = findRetracingSegments segments
        let segmentsStartingInSymbol = findSegmentsStartingInSymbol retracingSegmentsWithIndex positions
        // This ensures each wire contributes a tuple of lists to the collection
        [(List.map snd retracingSegmentsWithIndex, segmentsStartingInSymbol)])
    |> List.reduce (fun (acc1, acc2) (segs1, segs2) ->
        // Accumulate results from all wires, combining the lists appropriately
        (acc1 @ segs1, acc2 @ segs2))
    |> fun (retracingSegments, segmentsStartingInSymbol) ->
        // Returns the final aggregated lists
        (retracingSegments, segmentsStartingInSymbol)


    

    




            


    
    
















    

