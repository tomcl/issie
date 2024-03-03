module SheetBeautifyHelpers

open CommonTypes
open DrawModelType.SymbolT
open DrawModelType
open Optics
open Symbol
open BlockHelpers
open Helpers

/////////////////////////////////// 'visibleSegments' copied from TestDrawBlock.fs ////////////////////////////////////

/// The visible segments of a wire, as a list of vectors, from source end to target end.
/// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed 
/// which if present causes the two segments on either side of it to coalesce into a single visible segment.
/// A wire can have any number of visible segments - even 1.
let visibleSegments' (wId: ConnectionId) (model: SheetT.Model): XYPos list =

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
    /// Index must be in range >= 1
    let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int)  =
        if index < segVecs.Length - 1 && segVecs[index] =~ XYPos.zero
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

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



/////////////////////////////////// Individual Submission Helper Functions ////////////////////////////////////////////

// B1 RW
/// A lens for accessing or modifying the dimensions (width and height) of a custom component symbol.
/// Utilizes the Lens.create function to facilitate easy reading and updating of component dimensions.
/// Returns a lens for the dimensions.
let customCompDimensions_ =
    Lens.create (fun sym -> (sym.Component.W, sym.Component.H))
                (fun (newW, newH) sym -> {sym with Component = {sym.Component with W = newW; H = newH}})

// B2 W
/// Updates the position of a symbol on the sheet to a new specified position.
/// Takes as input the new position (X and Y coordinates) for the symbol and the symbol to update.
/// Returns a new symbol instance with the updated position.
let updateSymbolPosition (newPos : XYPos) (sym : Symbol) =
    fun newPos sym -> {sym with Pos = { sym.Pos with X=newPos.X; Y=newPos.Y}}

// B3 R
/// Retrieves the current order of ports on a specified side (edge) of a symbol.
/// Takes as input the symbol to read from and the edge of the symbol to check.
/// Returns a list of port identifiers in their current order on the specified edge.
let readPortsOrder (sym : Symbol) (edge : Edge) : string list =
    sym.PortMaps.Order.[edge]

// B3 W
/// Assigns a new order to the ports on a specified side (edge) of a symbol.
/// Takes as input the new order for the ports as a list of identifiers, the symbol to update, and the edge of the symbol to update.
/// Returns a new symbol instance reflecting the updated ports order.
let writePortsOrder (newOrder : string List) (sym : Symbol) (edge : Edge) : Symbol =
    {sym with PortMaps = {sym.PortMaps with Order = (Map.add edge newOrder sym.PortMaps.Order)}}

// B4 RW
/// A lens for accessing or modifying the state of ReversedInputPorts of a MUX2 symbol.
/// Provides a functional approach to read or update the boolean state indicating whether input ports are reversed.
/// Returns a lens for the ReversedInputPorts state.
let mux2ReverseInPorts_ =
    Lens.create (fun sym -> sym.ReversedInputPorts)
                (fun newState sym -> {sym with ReversedInputPorts = newState})

// B5 R
/// Retrieves the position of a specified port on the sheet.
/// Takes as input the symbol containing the port, the model context providing additional data necessary for calculating the port's position, and the identifier of the port.
/// Returns the position (X and Y coordinates) of the port on the sheet.
let getPortPosition (sym: Symbol) (model: Model) (portId : string) : XYPos =
    getPortLocation (Some sym.Pos)  model portId
     
// B6 R
/// Retrieves the bounding box for a symbol, incorporating its position.
/// Takes as input the symbol to calculate the bounding box for.
/// Returns a BoundingBox structure containing the top-left corner position and dimensions of the symbol.
let getSymbolBoundingBox  (sym : Symbol) : BoundingBox =
    {TopLeft = sym.Pos; W = sym.Component.W; H = sym.Component.H}

// B7 RW
/// A lens for accessing or modifying the rotation state of a symbol.
/// Enables direct manipulation of the symbol's rotation angle in a functional manner.
/// Returns a lens for the rotation state.
let symbolRotation_ =
    Lens.create (fun sym -> sym.STransform.Rotation)
                (fun newState sym -> {sym with STransform = {sym.STransform with Rotation =newState}})

// B8 RW
/// A lens for accessing or modifying the flip state of a symbol.
/// Facilitates toggling the boolean state indicating whether a symbol is flipped.
/// Returns a lens for the flip state.
let symbolFlip_ =
    Lens.create (fun sym -> sym.STransform.Flipped)
                (fun newState sym -> {sym with STransform = {sym.STransform with Flipped =newState}})

// T1 R
/// Counts the number of pairs of intersecting symbols on the sheet.
/// Takes as input the sheet model containing the symbols to be evaluated.
/// Returns the count of intersecting symbol pairs.
/// Intersections are determined by overlapping bounding boxes. Each pair is considered once, despite being evaluated twice.
let countIntersectingSymPairs (model: SheetT.Model) : int =
    let boundingBoxes = model.BoundingBoxes
                        |> mapValues
                        |> Array.toList

    let indexedBoxes = List.mapi (fun idx box -> idx, box) boundingBoxes

    let allPossiblePairs = List.allPairs indexedBoxes indexedBoxes 

    // Each pair is considered twice.
    let count =
        allPossiblePairs
        |> List.fold (fun acc ((idx1,box1),(idx2,box2)) ->
            if idx1 < idx2 && BlockHelpers.overlap2DBox box1 box2 then acc + 1 else acc
        ) 0

    // Divide by 2 since each pair was counted twice.
    count / 2  
            
// T2 R
/// Counts the number of distinct, visible wire segments that intersect with any symbol on the sheet.
/// Takes as input the sheet model containing wires and symbols to be evaluated.
/// Returns the count of wire segment-symbol intersections.
/// Only considers visible segments and checks for intersections with symbol bounding boxes.
let countSegSymIntersections (model: SheetT.Model) : int =

    // Extracts all visible segments and convert positions from relative to absolute.
    let getAbsoluteSegmentPositions (connectionId, _) (startPos: XYPos) =
        let segments = visibleSegments' connectionId model
        segments
        |> List.fold (fun (accumulated, lastPos) segment ->
                                                      let newAbsPos = lastPos + segment
                                                      (accumulated @ [(lastPos, newAbsPos)], newAbsPos)
        ) ([], startPos)
        |> fst

    // Checks if a segment intersects with atleast one symbol.
    let segmentIntersects (segmentStart, segmentEnd) =
        model.BoundingBoxes
        |> Map.toList
        |> List.exists (fun (_, box) -> 
            match segmentIntersectsBoundingBox box segmentStart segmentEnd with
            | Some _ -> true
            | None -> false
        )

    let allWires = model.Wire.Wires
                   |> Map.toList

    let wireStartPositions = allWires
                             |> List.map (snd >> (fun wire -> wire.StartPos))

    // Get the starting and ending absolute positions of all segments.                          
    let allSegmentPositions = List.zip wireStartPositions allWires
                              |> List.map (fun (startPos, wireData) ->
                                                                 getAbsoluteSegmentPositions wireData startPos
                              )

    allSegmentPositions
    |> List.concat // turning List<List<(XYPos * XYPos)>> into List<(XYPos * XYPos)>
    |> List.filter segmentIntersects
    |> List.length

// T3 R
/// Counts the distinct pairs of wire segments that cross each other at right angles on the sheet.
/// Takes as input the sheet model to analyze for right-angle wire segment intersections.
/// Returns the number of distinct segment pairs intersecting at right angles.
/// Each intersecting pair is counted once, considering their relative positioning and ensuring right-angle intersections.
let countRightAngleSegPairs (model: SheetT.Model) : int =

    let allWires = model.Wire.Wires
                    |> Map.toList

    // Extracts all visible segments and convert positions from relative to absolute. And, pairs with PortID.
    let segmentPositionsWithPortIds =
        allWires
        |> List.collect (fun (connectId, wire) -> 
            let startPos = wire.StartPos
            let outputPortId = wire.OutputPort
            let relativeSegmentPositions  = visibleSegments' connectId model
            relativeSegmentPositions 
            |> List.fold (fun (accumulated, lastPos) segment ->
                let newAbsPos = lastPos + segment
                (((lastPos, newAbsPos), outputPortId) :: accumulated, newAbsPos)
            ) ([], startPos)
            |> fst
        )

    // Check if two segments form a right angle.
    let isRightAngle  ((a1, a2): XYPos * XYPos) ((b1, b2): XYPos * XYPos) : bool =
        let vectorA = { X = a2.X - a1.X; Y = a2.Y - a1.Y }
        let vectorB = { X = b2.X - b1.X; Y = b2.Y - b1.Y }
        let dotProduct = vectorA.X * vectorB.X + vectorA.Y * vectorB.Y
        dotProduct = 0.0

    // Get the number of pairs that intersect at right angles. Each pair is considered twice.
    let countRightAngleIntersections =
        segmentPositionsWithPortIds 
        |> List.collect (fun segA -> 
            segmentPositionsWithPortIds 
            |> List.filter (fun segB -> 
                let ((startA, endA), portIdA) = segA
                let ((startB, endB), portIdB) = segB
                portIdA <> portIdB && isRightAngle (startA, endA) (startB, endB) && overlap2D (startA, endA) (startB, endB)
            )
        )
        |> List.length  

    // Divide by 2 since each pair was counted twice.
    countRightAngleIntersections / 2    

// T4 R
/// Calculates total length of all visible wire segments on a sheet.
/// Input: Sheet model with wires.
/// Output: Total length of visible wire segments.
/// Before calculating total length, segments are merged to handle overlaps.
let totalVisibleWireLength (model: SheetT.Model) : float =
    // Pick out nets from model's wires to check.
    let identifyNetsForInspection (model: SheetT.Model) =
        partitionWiresIntoNets model.Wire
        |> List.collect (fun (_, wires) -> if wires.Length > 1 then [wires] else [])

    // Fetch start points for wires in a net.
    let fetchWireStarts (netWires: (ConnectionId * BusWireT.Wire) list) = 
        netWires |> List.map (snd >> fun wire -> wire.StartPos)

    // Identify start and end of visible wire segments.
    let findVisibleSegments (netWires: (ConnectionId * BusWireT.Wire) list) (model: SheetT.Model) =
        netWires
        |> List.map (fun (id, _) -> visibleSegments' id model)
        |> List.zip (fetchWireStarts netWires) 
        |> List.map (fun (start, segments) ->
            segments
            |> List.fold (fun (accum, lastPos) seg ->
                let newPos = lastPos + seg
                (accum @ [(lastPos, newPos)], newPos)
            ) ([], start)
            |> fst)

    // Combine two overlapping segments into one.
    let unifySegments (seg1: XYPos * XYPos, seg2: XYPos * XYPos) =
        let ((a1, a2), (b1, b2)) = seg1, seg2
        let startX = min (min a1.X a2.X) (min b1.X b2.X)
        let startY = min (min a1.Y a2.Y) (min b1.Y b2.Y)
        let endX = max (max a1.X a2.X) (max b1.X b2.X)
        let endY = max (max a1.Y a2.Y) (max b1.Y b2.Y)
        ({X = startX; Y = startY}, {X = endX; Y = endY})

    // Merge segments that overlap.
    let mergeSegments (segmentsLists: ((XYPos * XYPos) list) list) =
        let allSegments = List.concat segmentsLists

        let rec integrateSegment segment integrated =
            match integrated with
            | [] -> [segment]
            | head :: tail ->
                if overlap2D segment head then
                    integrateSegment (unifySegments (segment, head)) tail
                else
                    head :: integrateSegment segment tail

        List.fold integrateSegment [] allSegments

    // Calculate total length of segments.
    let totalLength (segments: (XYPos * XYPos) list) =
        segments
        |> List.map (fun (start, endPos) ->
            let delta = endPos - start
            sqrt(delta.X ** 2.0 + delta.Y ** 2.0))
        |> List.sum

    model
    |> identifyNetsForInspection
    |> List.map (fun net ->
        net
        |> findVisibleSegments model
        |> mergeSegments
        |> totalLength)
    |> List.sum


// T5 R
/// Counts the number of visible wire segments forming right angles on the sheet.
/// Takes as input the model containing the wires to evaluate.
/// Returns the count of right-angle bends in visible wire segments.
/// Uses wire segments' direction changes to identify right-angle bends.
let countWireRightAngles (model: SheetT.Model) : int =

    let isRightAngle (v1: XYPos) (v2: XYPos) : bool =
        (v1.X * v2.X + v1.Y * v2.Y) = 0.0

    let countWireRightAngles (wireVectors: XYPos list) : int =
        wireVectors
        |> List.pairwise // Create pairs of consecutive elements
        |> List.filter (fun (v1, v2) -> isRightAngle v1 v2) // Keep only those pairs that form a right angle
        |> List.length // Count the remaining pairs

    model.Wire.Wires
    |> Seq.map (fun wires -> wires.Key, wires.Value)
    |> Seq.map (fun (wId, _) -> visibleSegments' wId model) 
    |> Seq.map countWireRightAngles 
    |> Seq.sum

// T6 R
/// Identifies wire segments that retrace their path and segments at the end of wires that extend into symbol boundaries.
/// Takes as input the model containing wires and symbols to analyze for retracing segments.
/// Returns 2 lists as a pair:  First of retracing segments, and the Second of segments that retrace to go within symbol boundaries.
/// Focuses on detecting wiring patterns that may indicate routing inefficiencies or potential design issues.
let getRetracingSegments (model : SheetT.Model) =

    // Extracts all absolute segments from the wires.
    let allAbsoluteSegments = model.Wire.Wires
                                 |> Map.toList
                                 |> List.map snd
                                 |> List.map getAbsSegments

    // Extracts all segments from the wires.
    let allSegments = model.Wire.Wires
                             |> Map.toList
                             |> List.map (fun (_, wire) -> wire.Segments)

    // Extracts all bounding boxes.
    let allBoundingBoxes = model.BoundingBoxes
                               |> Map.toList
                               |> List.map snd

    // Checks if an absolute segment intersects with any symbol's bounding box.
    let doesSegIntersectSym (aSeg: BusWireT.ASegment) =
        allBoundingBoxes
        |> List.exists (fun currentBox ->
            match (segmentIntersectsBoundingBox currentBox aSeg.Start aSeg.End) with
            | Some _ -> true  // If there's at least one intersection, return true
            | None -> false) 

    // Identifies end-of-wire retracing segments in wires that intersect with symbols.
    // Specifically targeting segments at fixed positions (third from the start or end).
    // These cases may indicate a wiring artifact due to routing or separation errors.
    let edgeRetracingSegments =
            allAbsoluteSegments
            |> List.map (fun segments ->
                let length = List.length segments
                segments
                |> List.indexed
                |> List.fold (fun acc (i, segment) ->
                    if length > 4 then
                        match i with
                        | 2 ->
                            if doesSegIntersectSym segment then
                                let prevSegment = segments.[i - 1]
                                let nextSegment = segments.[i + 1]
                                acc @ [[prevSegment.Segment; segment.Segment; nextSegment.Segment]]  // Nest inside another list
                            else acc
                        | i when i = length - 3 ->
                            if doesSegIntersectSym segment then
                                let prevSegment = segments.[i - 1]
                                let nextSegment = segments.[i + 1]
                                acc @ [[prevSegment.Segment; segment.Segment; nextSegment.Segment]]  // Nest inside another list
                            else acc
                        | _ -> acc
                    else acc
                ) [])
            |> List.concat


    // Identifies general retracing segments in wires. 
    // Focuses on zero-length segments flanked by segments with non-zero lengths of opposite signs.
    // Highlights potential wiring artifacts, excluding the first and last segments.
    let generalRetracingSegments = 
        allSegments
        |> List.map (fun segments ->
            segments
            |> List.indexed 
            |> List.fold (fun acc (i, segment) ->
                if segment.Length = 0.0 then
                    match i with
                    // Ignore first and last segments of 0.0 length
                    | 0  -> acc
                    | _ when i = List.length segments - 1 -> acc
                    | _ ->
                        let prevSegment = segments.[i - 1]
                        let nextSegment = segments.[i + 1]
                        if (prevSegment.Length > 0.0 && nextSegment.Length < 0.0) || (prevSegment.Length < 0.0 && nextSegment.Length > 0.0) then
                            acc @ [[prevSegment; segment; nextSegment]]
                        else acc
                else acc
            ) []
        )
        |> List.concat

    (generalRetracingSegments, edgeRetracingSegments)

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////




