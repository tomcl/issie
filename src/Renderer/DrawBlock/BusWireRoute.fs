module BusWireRoute
open EEExtensions
open CommonTypes
open BlockHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers
open BusWireRoutingHelpers


open Optics
open Operators



(* 
NOTE:   For ease of understanding, algorithm, variable names and documentation of code below are all explained 
        in the simple case of no rotated symbols (ie wire.InitialOrientation = Horizontal).

        However, the code implemented supports the rotated case as well.

Implemented the following Smart Routing Algorithm:

    1)  Check if initial autorouted wire has any intersections with symbols. 
        If yes, calculate the bounding boxes of all the intersected symbols.
    2)  Attempt to shift the vertical seg of the 7 seg wire to wireSeparationFromSymbol amount left of the left most
        bound of the intersected symbols. 
        If there are still intersections, try shifting to the right most bound + wireSeparationFromSymbol.
    3)  If there are still intersections, recursively try to shift the horizontal seg of the 7 seg 
        or 9 seg wire to either the top or bottom most bound of the intersected symbols. 
        If both shifted wires still result in an intersection, compute the vertical distances between 
        the start/end pos of the wire and the top/bottom bound of the intersected symbols. 
        Using the 4 vertical distances computed, decide whether to try shifting the wire up or down 
        depending on which results in a wire with shorter vertical distance.
        
        A max recursion depth is defined for step 3 so that Issie will not break when there are physically 
        no possible routes that will not intersect any symbol (eg when dragging a symbol around such that 
        the dragged symbol is within another symbol) or when there are special corner cases that have not 
        been implemented yet (eg symbol A is in top left quadrant with input port facing up, connected to
        symbol B in bottom right quadrant with output port facing down, with other symbols in between the
        2 symbols).
*)

//*************************************************************************************************************
//                                 See SmartHelpers for Constants submodule
//**************************************************************************************************************

open BusWireRoutingHelpers.Constants

/// add a nub and zero length segment to the start of the wire if needed
let ensureStartingNub (wire: Wire) =

    let updateIndices: Segment list -> Segment list =
        List.mapi (fun i seg -> { seg with Index = i })

    let segs = wire.Segments
    if segs.Length <= 2 then
        wire // no nub needed, but should never happen???
    elif segs[1].Length = 0. && (sign segs.[0].Length * sign segs[2].Length = -1) then
        let totalLength = segs[0].Length + segs[2].Length
        let dir = float <| sign segs[0].Length
        let thisNubLength = min nubLength (abs totalLength)
        let nub = { segs[0] with Length = dir * thisNubLength; Draggable = false; IntersectOrJumpList = [] }
        let newSeg2 = { segs[0] with Length = totalLength - dir * thisNubLength; Draggable = true; IntersectOrJumpList = [] }
        let newSegs = nub :: segs[1] :: newSeg2 :: segs[3..]
        { wire with Segments = newSegs |> updateIndices }
    elif segs[1].Length <> 0.  then
        let seg0 = segs[0]
        let seg1 = segs[1]
        let dir = sign segs[0].Length |> float
        let thisNubLength = min nubLength (abs seg0.Length)
        let nub = {seg0 with Length = dir * thisNubLength; Draggable = false ; IntersectOrJumpList = []}
        let zero = { seg1 with Length = 0. ; IntersectOrJumpList = []; Draggable = true}
        let newSeg2 = {seg0 with Length = seg0.Length - dir*thisNubLength; IntersectOrJumpList = []; Draggable = true}
        { wire with Segments = (nub :: zero :: newSeg2 :: segs[1..]) |> updateIndices }
    else
        wire
        
let ensureBothNubs = ensureStartingNub >> reverseWire >> ensureStartingNub >> reverseWire


/// Checks if a wire intersects any symbol within +/- minWireSeparation.
/// Returns, for each segment which intersects something, its index and the boxes it intersects.
///
/// Which segment is in the way is what decides which segment is worth moving, so it is kept rather
/// than flattened away: the shift code below used to choose a segment by the wire's segment count,
/// which is a guess where this is the answer.
let findWireSymbolIntersectionsBySegment (model: Model) (wire: Wire) : (int * BoundingBox list) list =

    let allBoundingBoxes =
        model.Symbol.Symbols
        |> Map.valuesL
        |> List.filter (fun s -> s.Annotation = None)
        |> List.map (fun s -> (s.Component, Symbol.getSymbolBoundingBox s))

    /// absolute coordinates of wire vertices
    let wireVertices =
        segmentsToIssieVertices wire.Segments wire
        |> List.mapi (fun i (x, y, _) -> i, { X = x; Y = y })


    /// indexes of all the vertices except the end ones
    let indexes = List.init ((List.length wireVertices)-2) (fun i -> i+1)

    let lastIndex = indexes[List.length indexes - 1]

    /// list of segments (except for end ones) and their vertices
    ///
    /// There is always one more interior vertex than there are segments between interior vertices,
    /// so the last index has no segment to pair with and is dropped. `List.zip` would do that on
    /// its own (see `ListPairs`), but a pairing that never lines up is worth saying out loud.
    let segVertices =
        let segments = List.pairwise wireVertices.[1 .. wireVertices.Length - 2] // do not consider the nubs
        List.zip (List.truncate segments.Length indexes) segments

    // NB inputPortStr, not `string`: InputPortId is [<Erase>], so `string` gives the bare id under
    // Fable and "InputPortId \"...\"" under .NET, where the lookup then throws.
    let inputCompId = model.Symbol.Ports.[inputPortStr wire.InputPort].HostId
    let outputCompId = model.Symbol.Ports.[outputPortStr wire.OutputPort].HostId

    let componentIsMux (comp:Component) =
        match comp.Type with
        | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 -> true
        | _ -> false

    // this was added to fix MUX SEL port wire rooting bug, it is irrelevant in other cases
    let inputIsSelect =
        let inputSymbol = model.Symbol.Symbols.[ComponentId inputCompId]
        let inputCompInPorts = inputSymbol.Component.InputPorts
        
        componentIsMux inputSymbol.Component && (inputCompInPorts.[List.length inputCompInPorts - 1].Id = inputPortStr wire.InputPort)

    let inputCompRotation =
        model.Symbol.Symbols.[ComponentId inputCompId].STransform.Rotation

    let outputCompRotation =
        model.Symbol.Symbols.[ComponentId outputCompId].STransform.Rotation

    let isConnectedToSelf = inputCompId = outputCompId

    let expandBox (box: BoundingBox) (borderSize: float)=
               {
                    W = box.W + borderSize * 2.
                    H = box.H + borderSize * 2.
                    TopLeft =
                        box.TopLeft
                        |> updatePos Left_ borderSize
                        |> updatePos Up_ borderSize
                }
 

    let boxesIntersectedBySegment (lastSeg:bool) (startIndex,startPos) (endIndex,endPos) =
        allBoundingBoxes
        |> List.map (fun (comp, boundingBox) ->
                let borderSize =
                    if comp.Id = outputCompId && startIndex <= 2 || comp.Id = inputCompId && endIndex >= lastIndex - 2 then
                        0. // do not consider the nubs
                    else
                        minWireSeparation
                comp, expandBox boundingBox borderSize)
        |> List.filter (fun (comp, boundingBox) ->
            // A mux SEL port sits inside its own symbol's bounding box, so the final segments of a
            // wire reaching one have to be allowed into that box. Only THAT box: this used to
            // exempt every mux and demux on the sheet, so a wire climbing to a SEL port past
            // another mux could not see it and was drawn straight through it - and since the check
            // found nothing, no shift was attempted either.
            match comp.Type, lastSeg && comp.Id = inputCompId with
            | Mux2, true | Mux4, true | Mux8, true | Demux2, true | Demux4, true | Demux8, true -> false
            | _, _ ->
                 match segmentIntersectsBoundingBox boundingBox startPos endPos with // do not consider the symbols that the wire is connected to
                 | Some _ ->
                    true // segment intersects bounding box
                 | None -> false // no intersection
        )
        |> List.map (fun (compType, boundingBox) -> boundingBox)


    segVertices
    |> List.map (fun (i, (startPos, endPos)) ->
        i, boxesIntersectedBySegment (i > List.length segVertices - 2 && inputIsSelect) startPos endPos)
    |> List.filter (fun (_, boxes) -> not boxes.IsEmpty)

/// Bounding boxes of symbols intersected by wire, however many of its segments hit them.
let findWireSymbolIntersections (model: Model) (wire: Wire) : BoundingBox list =
    findWireSymbolIntersectionsBySegment model wire
    |> List.collect snd
    |> List.distinct


//------------------------------------------------------------------------//
//--------------------------Shifting Vertical Segment---------------------//
//------------------------------------------------------------------------//

/// update the length of a segment in a list of segments
let changeSegment (segIndex: int) (newLength: float) (segments: Segment list) =
    List.updateAt segIndex { segments[segIndex] with Length = newLength } segments

/// Try shifting vertical seg (index 3) to either - wireSeparationFromSymbol or + wireSeparationFromSymbol from
/// the edge of all the intersectedBoundingBoxes symbols.
/// Returns None if no route found.
let tryShiftVerticalSeg (model: Model) (intersectedBoxes: BoundingBox list) (wire: Wire) : Wire option =
    let origWireVertices  =
        segmentsToIssieVertices wire.Segments wire
        |> List.map (fun (x, y, _) -> { X = x; Y = y })

    /// shift segment 3 to the left or right
    let shiftVerticalSeg amountToShift =
        let newSegments =
            wire.Segments
            |> List.updateAt 2 { wire.Segments[2] with Length = wire.Segments[2].Length + amountToShift }
            |> List.updateAt 4 { wire.Segments[4] with Length = wire.Segments[4].Length - amountToShift }

        { wire with Segments = newSegments }
        //|> ensureBothNubs

    
    let shiftVertWireInDirection (dir: DirectionToMove) =
        /// Return intersecting bounding box to avoid when shifting in direction dir
        let boxToGoRound =
            intersectedBoxes
            |> List.sortWith (fun box1 box2 ->
                let box1 = swapBBXAndY box1 wire.InitialOrientation
                let box2 = swapBBXAndY box2 wire.InitialOrientation

                match dir with
                | Left_ -> compare (box1.TopLeft.X) (box2.TopLeft.X)
                | Right_ -> compare (box2.TopLeft.X + box2.W) (box1.TopLeft.X + box1.W)
                | _ -> failwith "Invalid direction to shift wire")
            |> List.head

        /// Return the edge that the wire should go around when shifting in direction dir.
        /// boxToGoRound is already expanded by minWireSeparation, so clearing a further
        /// (wireSeparationFromSymbol - minWireSeparation) puts the wire wireSeparationFromSymbol
        /// from the symbol itself.
        let clearBy = wireSeparationFromSymbol - minWireSeparation + smallOffset
        let edgeOfBoxToGoRound =
            match dir, wire.InitialOrientation with
            | Left_, Horizontal ->
                let initialAttemptPos = updatePos Left_ clearBy boxToGoRound.TopLeft
                initialAttemptPos
            | Right_, Horizontal ->
                let initialAttemptPos =
                    updatePos Right_ (boxToGoRound.W + clearBy) boxToGoRound.TopLeft
                initialAttemptPos
            | Left_, Vertical ->
                let initialAttemptPos = updatePos Up_ clearBy boxToGoRound.TopLeft
                initialAttemptPos
            | Right_, Vertical ->
                let initialAttemptPos =
                    updatePos Down_ (boxToGoRound.H + clearBy) boxToGoRound.TopLeft
                initialAttemptPos
            | _ -> failwith "Invalid direction to shift wire"

        let amountToShift =
            (swapXYWhenVertical edgeOfBoxToGoRound wire.InitialOrientation).X
            - (swapXYWhenVertical origWireVertices[4] wire.InitialOrientation).X

        shiftVerticalSeg amountToShift

    /// Try shifting wire in direction dir, return Some shifted wire if no intersections.
    /// even though we have avoided obvious vertical segment intersections,
    /// we may still have horizontal segment intersections, or non-obvious
    /// vertical segment intersections.
    let tryshiftedWireWithoutIntersections (dir:DirectionToMove) =
        let shifted = shiftVertWireInDirection dir
        findWireSymbolIntersections model shifted
        |> function | [] -> Some shifted | _ -> None

    // Check which newly generated wire has no intersections, return that
    tryshiftedWireWithoutIntersections Left_
    |> Option.orElseWith (fun () -> tryshiftedWireWithoutIntersections Right_)


//------------------------------------------------------------------------//
//-------------------------Shifting Horizontal Segment--------------------//
//------------------------------------------------------------------------//
type VertDistFromBoundingBox =
    | Above of float // Vertical distance between pos and a bounding box above
    | Below of float // Vertical distance between pos and a bounding box below


//***************************************************************************************************************//
//**************************************** NEW implementation ****************************************************
//***************************************************************************************************************//

// return Some max distance above or below, if one exists, or None
let tryMaxDistance (distances: VertDistFromBoundingBox option list) =
    match distances with
    | [] -> None
    | _ ->
        List.maxBy
            (function
            | Some(Above d)
            | Some(Below d) -> d
            | None -> -infinity)
            distances

/// returns the maximum vertical distance of pos from intersectedBoxes as a VertDistFromBoundingBox or None if there are no intersections
let maxVertDistanceFromBox
    (intersectedBoxes: BoundingBox list)
    (wireOrientation: Orientation)
    (pos: XYPos)
    : VertDistFromBoundingBox option =

    let isCloseToBoxHoriz (box: BoundingBox) (pos: XYPos) =
        inMiddleOrEndOf box.TopLeft.X pos.X (box.TopLeft.X + box.W)

    let getVertDistanceToBox (pos: XYPos) (box: BoundingBox) : VertDistFromBoundingBox option list =
        (swapXYWhenVertical pos wireOrientation, swapBBXAndY box wireOrientation)
        ||> (fun pos box ->
            if isCloseToBoxHoriz box pos then
                if pos.Y > box.TopLeft.Y then
                    [ pos.Y - box.TopLeft.Y |> Above |> Some ]
                else
                    [ box.TopLeft.Y - pos.Y + box.H |> Below |> Some ]
            else
                [])

    intersectedBoxes
    |> List.collect (fun box -> getVertDistanceToBox pos box)
    |> tryMaxDistance



/// Recursively shift horizontal seg up/down until no symbol intersections.
/// Limit in recursion depth defined by argument callsLeft given to initial function call.
/// Limit needed to prevent Issie from breaking when there are physically
/// no possible routes that achieve 0 intersections.
/// Returns None if no route found
let rec tryShiftHorizontalSeg
    (callsLeft: int)
    (model: Model)
    (intersectedBoxes: BoundingBox list)
    (wire: Wire)
    : Wire option =
    match callsLeft with
    | 0 -> None
    | n ->
        /// recursive call to tryShiftHorizontalSeg with n-1
        let tryShiftHorizontalSeg = tryShiftHorizontalSeg (n - 1)

        let currentStartPos, currentEndPos = getStartAndEndWirePos wire

        let shiftWireHorizontally firstVerticalSegLength secondVerticalSegLength =

            let moveHorizSegment vertSegIndex =
                changeSegment (vertSegIndex - 1) firstVerticalSegLength
                >> changeSegment (vertSegIndex + 1) secondVerticalSegLength

            let newSegments =
                match wire.Segments.Length with
                | 5
                | 6 -> wire.Segments |> moveHorizSegment 2

                | 7 ->
                    // Change into a 5 segment wire
                    wire.Segments[..4]
                    |> moveHorizSegment 2
                    |> changeSegment 2 (wire.Segments.[2].Length + wire.Segments.[4].Length)
                    |> List.updateAt 4 { wire.Segments.[6] with Index = 4 }

                | 8 ->
                    // As for 9 segments, but the last segment is the nub perpendicular to the
                    // moved segment rather than parallel to it, so it must keep its length.
                    // These are the wires reaching a Top or Bottom port from beyond it.
                    wire.Segments |> changeSegment 1 0. |> moveHorizSegment 4

                | 9 ->
                    // Change segments index 1,3,5,7. Leave rest as is
                    wire.Segments |> changeSegment 1 0. |> moveHorizSegment 4 |> changeSegment 7 0.

                | n ->
                    // makeInitialWireVerticesList makes wires of 6-9 segments, all handled above.
                    // Returning the wire unchanged here means the caller retries the same wire
                    // until it runs out of recursion, so say so rather than route silently badly.
                    Log.dbg Log.Wire $"cannot shift the horizontal segment of a {n} segment wire"
                    wire.Segments

            { wire with Segments = newSegments }

        /// Move the horizontal segment at segIndex to the chosen coordinate: set the two vertical
        /// segments flanking it and flatten every other vertical one, so the wire runs straight
        /// out of its port, across at the new coordinate, and straight in again. The last segment
        /// keeps its length when it is a nub perpendicular to the moved segment - that nub is what
        /// joins the wire to its port.
        ///
        /// segIndex must be even and at most Length-3: the segments either side of it have to be
        /// perpendicular, and neither may be an end nub.
        let moveOneHorizontalSegment segIndex firstVerticalSegLength secondVerticalSegLength =
            let lastIndex = wire.Segments.Length - 1
            wire.Segments
            |> List.mapi (fun i seg ->
                if i = segIndex - 1 then { seg with Length = firstVerticalSegLength }
                elif i = segIndex + 1 then { seg with Length = secondVerticalSegLength }
                elif i % 2 = 1 && i <> lastIndex then { seg with Length = 0. }
                else seg)
            |> (fun segments -> { wire with Segments = segments })

        let orientation = wire.InitialOrientation

        /// the coordinate a segment of the same orientation as segment 0 runs along
        let getXOrY =
            fun (pos: XYPos) ->
                match orientation with
                | Horizontal -> pos.X
                | Vertical -> pos.Y

        /// and the one perpendicular to it, which is what a shift changes
        let getOppositeXOrY =
            fun (pos: XYPos) ->
                match orientation with
                | Horizontal -> pos.Y
                | Vertical -> pos.X

        let getWOrH =
            fun (box: BoundingBox) ->
                match orientation with
                | Horizontal -> box.W
                | Vertical -> box.H

        let getOppositeWOrH =
            fun (box: BoundingBox) ->
                match orientation with
                | Horizontal -> box.H
                | Vertical -> box.W

        /// The segments actually obstructed by a symbol. Moving one of those is what might help;
        /// moving any other one cannot.
        let blockedSegments =
            findWireSymbolIntersectionsBySegment model wire |> List.map fst

        /// Move the horizontal segment at segIndex onto `bound` AND put the turn which follows it
        /// at `turn` along the wire, collapsing everything else. The wire then leaves its port,
        /// crosses at `bound`, turns at `turn` and runs in to the other port.
        ///
        /// This is the move neither shift can make on its own. Shifting a segment sideways cannot
        /// change where the wire turns, and shifting the turn cannot change which row the wire
        /// crosses on - so an obstacle which blocks both the row the wire leaves on and the row it
        /// arrives on needs the two done together. Reaching a port on a top or bottom edge from
        /// beyond an obstacle is the common case: the crossing has to happen past the obstacle,
        /// near the destination rather than near the source.
        let moveHorizontalRunTo segIndex bound turn =
            let segs = wire.Segments
            let last = segs.Length - 1
            let alongStart, alongEnd = getXOrY currentStartPos, getXOrY currentEndPos
            let perpStart, perpEnd = getOppositeXOrY currentStartPos, getOppositeXOrY currentEndPos
            /// the one segment after the turn which takes whatever distance is left - never the
            /// end nub, which has to keep its length
            let remainderIndex =
                [ segIndex + 2 .. 2 .. last - 1 ] |> List.tryHead
            remainderIndex
            |> Option.map (fun remainder ->
                segs
                |> List.mapi (fun i seg ->
                    let length =
                        match i with
                        | 0 -> seg.Length // leaves the port
                        | i when i = last -> seg.Length // and enters the other one
                        | i when i = segIndex - 1 -> bound - perpStart
                        | i when i = segIndex + 1 -> perpEnd - bound
                        | i when i = segIndex -> turn - alongStart - segs[0].Length
                        | i when i = remainder -> alongEnd - turn
                        | _ -> 0.
                    { seg with Length = length })
                |> fun segments -> { wire with Segments = segments })

        /// Places along the wire where turning could get it past the obstacles: hard against
        /// either side of them, and at the destination itself - which is what puts the crossing
        /// next to the port rather than next to the source.
        let turnPositions =
            let nearSide = intersectedBoxes |> List.map (fun b -> getXOrY b.TopLeft) |> List.min
            let farSide =
                intersectedBoxes |> List.map (fun b -> getXOrY b.TopLeft + getWOrH b) |> List.max
            [ getXOrY currentEndPos
              nearSide - minWireSeparation - smallOffset
              farSide + minWireSeparation + smallOffset ]

        // directionToMove must be UP_ or DOWN_
        let shiftedWire (direction: DirectionToMove) =
            let offsetOfBox, otherDir =
                match direction with
                | Up_ -> (fun _ -> 0.), Left_
                | Down_ -> (fun box -> getOppositeWOrH box), Right_
                | _ -> failwithf "What? Can't happen"

            let boundBox =
                intersectedBoxes
                |> match direction with
                   | Down_ -> List.maxBy (fun box -> getOppositeXOrY box.TopLeft + getOppositeWOrH box)
                   | Up_ -> List.minBy (fun box -> getOppositeXOrY box.TopLeft)
                   | _ -> failwithf "What? Can't happen"

            let bound =
                // boundBox is expanded by minWireSeparation; clear the rest of
                // wireSeparationFromSymbol beyond it - see tryShiftVerticalSeg
                let offset = wireSeparationFromSymbol - minWireSeparation + smallOffset + offsetOfBox boundBox

                let otherOrientation =
                    match orientation with
                    | Horizontal -> direction
                    | Vertical -> otherDir

                let initialAttemptPos = updatePos otherOrientation offset boundBox.TopLeft
                initialAttemptPos |> getOppositeXOrY

            let firstVerticalSegLength, secondVerticalSegLength =
                bound - getOppositeXOrY currentStartPos, getOppositeXOrY currentEndPos - bound


            // Every way of putting one horizontal segment at `bound`, most promising first. The
            // shape the segment-count table picks leads, so a wire that routes today routes
            // identically; the rest are tried only when that one leaves the wire over a symbol.
            // Among those, a segment which is actually obstructed comes before one which is not.
            let movableHorizontals =
                [ 2 .. 2 .. wire.Segments.Length - 3 ]
                |> List.sortBy (fun i -> if List.contains i blockedSegments then 0 else 1)
            shiftWireHorizontally firstVerticalSegLength secondVerticalSegLength
            :: (movableHorizontals
                |> List.map (fun i ->
                    moveOneHorizontalSegment i firstVerticalSegLength secondVerticalSegLength))
            // last, and only reached when everything above still crosses something: move the
            // crossing and the turn together
            @ (List.allPairs movableHorizontals turnPositions
               |> List.choose (fun (i, turn) -> moveHorizontalRunTo i bound turn))

        let goodWire dir =
            let candidates = shiftedWire dir
            match candidates |> List.tryFind (fun w -> List.isEmpty (findWireSymbolIntersections model w)) with
            | Some clear -> Ok clear
            | None ->
                // none of them is clear: recurse on the one the old code would have produced
                let firstTry = List.head candidates
                Error(findWireSymbolIntersections model firstTry, firstTry)

        // If newly generated wire has no intersections, return that
        // Otherwise, decide to shift up or down based on which is closer
        match goodWire Up_, goodWire Down_ with
        | Ok upWire, Ok downWire ->
            if getWireLength upWire < getWireLength downWire then
                Some upWire
            else
                Some downWire
        | Ok upWire, _ -> Some upWire
        | _, Ok downWire -> Some downWire
        | Error(upIntersections, upShiftedWire), Error(downIntersections, downShiftedWire) ->
            [ currentStartPos; currentEndPos ]
            |> List.map (maxVertDistanceFromBox intersectedBoxes wire.InitialOrientation)
            |> tryMaxDistance
            |> (function
            | None
            | Some(Above _) -> tryShiftHorizontalSeg model downIntersections downShiftedWire
            | Some(Below _) -> tryShiftHorizontalSeg model upIntersections upShiftedWire)



//------------------------------------------------------------------------//
//-----------------------------Snapping to Net----------------------------//
//------------------------------------------------------------------------//

let getWireVertices (wire: Wire) =
    segmentsToIssieVertices wire.Segments wire
    |> List.map (fun (x, y, _) -> { X = x; Y = y })

let copySegments (wire: Wire) (refWire: Wire) (numOfSegsToCopy: int) : Segment list =
    [ 0 .. numOfSegsToCopy - 1 ]
    |> List.map (fun i -> { wire.Segments[i] with Length = refWire.Segments[i].Length })

let generateEndSegments (startIndex: int) (numOfSegs: int) (wire: Wire) : Segment list =
    [ startIndex .. startIndex + numOfSegs - 1 ]
    |> List.map (fun i ->
        { wire.Segments[i % 2] with
            Length = 0.
            Index = i })
    |> List.updateAt (numOfSegs - 1) { wire.Segments.[numOfSegs - 1] with Length = nubLength }

/// Finds the first reference wire in a net and keeps the same segment lengths
/// as much as possible based on a heuristic.
/// Snap to net only implemented for one orientation
let snapToNet (model: Model) (wireToRoute: Wire) : Wire =

    let inputCompId =
        ComponentId model.Symbol.Ports[inputPortStr wireToRoute.InputPort].HostId

    let outputCompId =
        ComponentId model.Symbol.Ports[outputPortStr wireToRoute.OutputPort].HostId

    let isRotated =
        model.Symbol.Symbols[inputCompId].STransform.Rotation = Degree90
        || model.Symbol.Symbols[inputCompId].STransform.Rotation = Degree270
        || model.Symbol.Symbols[outputCompId].STransform.Rotation = Degree90
        || model.Symbol.Symbols[outputCompId].STransform.Rotation = Degree270

    let wireToRouteStartPos, wireToRouteEndPos = getStartAndEndWirePos wireToRoute

    match
        wireToRoute.Segments.Length,
        isRotated,
        wireToRoute.InitialOrientation,
        wireToRouteStartPos.X > wireToRouteEndPos.X,
        isWireInNet model wireToRoute
    with
    | n, _, _, _, _ when n <> 5 && n <> 7 -> wireToRoute // If wire is not 5 or 7 seg, return original wire
    | _, true, _, _, _ -> wireToRoute // If either input or output component is rotated, return original wire
    | _, _, orientation, _, _ when orientation <> Horizontal -> wireToRoute // If wire is not horizontal, return original wire
    | _, _, _, true, _ -> wireToRoute // If input is on right side of output, return original wire
    | _, _, _, _, None -> wireToRoute // If wire is not in net, return original wire
    | _, _, _, _, Some(_, netlist) ->
        // Take first wire in netlist that is not wireToRoute as reference wire for snapping
        let refWire = netlist |> List.find (fun (_, w) -> w.WId <> wireToRoute.WId) |> snd

        let refWireVertices = getWireVertices refWire

        let _, refEndPos = getStartAndEndWirePos refWire

        let firstBendPos = refWireVertices[3]
        let horizontalSegLength = refWire.Segments[2].Length

        let isHorizontalSegTooShort =
            (wireToRouteEndPos.X - wireToRouteStartPos.X) < horizontalSegLength / 2.

        let numOfSegsToCopy =
            let simpleCase =
                match wireToRouteEndPos.X < firstBendPos.X, isHorizontalSegTooShort with
                | true, true -> 1
                | true, false -> 2
                | false, _ -> 3

            match refWire.Segments.Length with
            | 5 ->
                match firstBendPos.Y < refEndPos.Y, firstBendPos.Y > wireToRouteEndPos.Y with
                | (true, true)
                | (false, false) -> if wireToRouteEndPos.X < firstBendPos.X then 2 else 3
                | _ -> simpleCase
            | 7 -> simpleCase
            | _ -> 0 // Not implemented for ref wires that are not 5 or 7 seg

        let newSegments =
            match numOfSegsToCopy with
            | 3 ->
                copySegments wireToRoute refWire 3
                @ [ { wireToRoute.Segments[3] with Length = wireToRouteEndPos.Y - firstBendPos.Y } ]
                  @ [ { wireToRoute.Segments[4] with Length = wireToRouteEndPos.X - firstBendPos.X } ]
                    @ generateEndSegments 5 2 wireToRoute
            | 2 ->
                copySegments wireToRoute refWire 2
                @ [ { wireToRoute.Segments[2] with Length = wireToRouteEndPos.X - wireToRouteStartPos.X - nubLength } ]
                  @ [ { wireToRoute.Segments[3] with Length = wireToRouteEndPos.Y - firstBendPos.Y } ]
                    @ generateEndSegments 4 3 wireToRoute
            | 1 ->
                copySegments wireToRoute refWire 1
                @ [ { wireToRoute.Segments[1] with Length = wireToRouteEndPos.Y - wireToRouteStartPos.Y } ]
                  @ [ { wireToRoute.Segments[2] with Length = wireToRouteEndPos.X - wireToRouteStartPos.X - nubLength } ]
                    @ [ { wireToRoute.Segments[3] with Length = 0. } ]
                      @ generateEndSegments 4 3 wireToRoute
            | 0 -> wireToRoute.Segments // Not implemented for ref wires that are not 5 or 7 seg
            | _ -> failwithf "Shouldn't happen"

        { wireToRoute with Segments = newSegments }


//------------------------------------------------------------------------//
//----------------------Branching off the same net------------------------//
//------------------------------------------------------------------------//

/// The symbol edge a segment travelling in this direction would leave from: a segment running
/// right continues out of a Right edge, and so on. Used to carry on routing from a point part way
/// along a wire as though that point were a port facing the way the wire was going.
let private edgeOfTravel (seg: ASegment) =
    match seg.Orientation, seg.Segment.Length > 0. with
    | Horizontal, true -> CommonTypes.Right
    | Horizontal, false -> CommonTypes.Left
    | Vertical, true -> CommonTypes.Bottom
    | Vertical, false -> CommonTypes.Top

/// The ordinary routing of a pair of points, given the edge each leaves by. This is the body of
/// autoroute with the port lookups taken out, so that it can also route from a point part way
/// along an existing wire.
let private routeBetween wid (startPos: XYPos) (startEdge: Edge) (destPos: XYPos) (destEdge: Edge) =
    let normStart, normEnd =
        rotateStartDest CommonTypes.Right (genPortInfo startEdge startPos, genPortInfo destEdge destPos)
    {| edge = CommonTypes.Right
       segments = makeInitialSegmentsList wid normStart.Position normEnd.Position normEnd.Edge |}
    |> rotateSegments startEdge
    |> (fun w -> w.segments)

/// Route `wire` by following `refWire` - another wire of the same net - as far as the end of its
/// segment `branchAt`, and going on from there.
///
/// The two wires start at the same port, so the leading segments can simply be copied. Routing
/// continues from the branch point as though it were a port facing the way refWire was going, so
/// the first segment it generates runs ON along refWire before turning off. That overlap is free:
/// it is the same net, so separation links the two and they are drawn as one line. It is also why
/// branching only at the ends of segments loses nothing - a branch that ought to leave from the
/// middle of a segment leaves at the end of the one before and runs back along it.
let private branchFrom (wire: Wire) (refWire: Wire) (branchAt: int) (branchPos: XYPos) (edge: Edge)
                       (destPos: XYPos) (destEdge: Edge) : Wire =
    let onwards = routeBetween wire.WId branchPos edge destPos destEdge
    let shared = refWire.Segments[0 .. branchAt]
    // The first segment routed onwards runs ALONG refWire's last shared segment, so the two are one
    // segment and not two. And a route begins nub, zero-length, rest - the zero is what makes the
    // first visible segment draggable - so where that zero is present the segment after it is
    // collinear as well, and all three are one.
    //
    // Leaving the zero where it fell would put a vertex that is not a vertex in the middle of the
    // wire: two coincident vertices with a zero segment between them. Those belong beside a nub and
    // nowhere else. A later separation move which crosses one draws the wire back over itself,
    // which is where the spikes on a redrawn sheet came from.
    let joinedLength, rest =
        if onwards.Length > 2 && onwards[1].IsZero then
            shared[branchAt].Length + onwards[0].Length + onwards[2].Length, onwards[3..]
        else
            shared[branchAt].Length + onwards[0].Length, onwards[1..]
    { wire with
        StartPos = refWire.StartPos
        InitialOrientation = refWire.InitialOrientation
        Segments =
            shared[.. branchAt - 1] @ [ { shared[branchAt] with Length = joinedLength } ] @ rest
            |> List.map (fun seg -> { seg with WireId = wire.WId; Mode = Auto })
            // Branching at the end of segment 0 merges the shared run INTO the nub, which stops it
            // being a nub: it is then a long segment at index 0, and everything which treats index
            // 0 as the short stick out of a port - findWireSymbolIntersections included - looks
            // straight past it. A trunk hidden there is a trunk nothing checks for obstacles, and
            // it is how SFTIN came to be drawn across three bus selects. makeEndsDraggable splits
            // it back into nub, zero, remainder, which is what every other wire looks like.
            |> makeEndsDraggable }

/// Every way of routing `wire` as a branch off a wire of its own net which is already routed, each
/// paired with how far its branch point is from the destination.
///
/// A branch takes over the reference wire's start position, so every wire it may follow has to be
/// one that is drawn from where the driver port is NOW. A wire with no segments is passed over,
/// which is what makes taking the routing off a set of wires before routing any of them enough to
/// guarantee it - see `rerouteMovedWires`. Follow a wire that has not been re-routed since its
/// driver moved and this wire is drawn from where that port used to be, joined to nothing.
///
/// That distance is the whole of the choice: take the first of these that is legal and the branch
/// point is the nearest one to the destination that works, which is the one the wire can follow
/// for longest. The ordinary route belongs in the same ordering - it is the branch at the driver
/// port, where nothing is shared - so a branch is taken only when it starts nearer the destination
/// than starting again from the port would.
let sameNetRoutes (model: Model) (wire: Wire) : (float * Wire) list =
    let destPos = Symbol.getInputPortLocation None model.Symbol wire.InputPort
    let destEdge = getInputPortOrientation model.Symbol wire.InputPort
    model.Wires
    |> Map.valuesL
    |> List.filter (fun w ->
        w.OutputPort = wire.OutputPort && w.WId <> wire.WId && not w.Segments.IsEmpty)
    |> List.collect (fun refWire ->
        getAbsSegments refWire
        |> List.indexed
        // never the last segment: its end is the other wire's own port, and a branch from there
        // would start inside that symbol
        |> List.filter (fun (i, seg) -> i < refWire.Segments.Length - 1 && not seg.IsZero)
        |> List.map (fun (i, seg) -> refWire, i, seg.End, edgeOfTravel seg))
    |> List.map (fun (refWire, i, branchPos, edge) ->
        euclideanDistance branchPos destPos,
        branchFrom wire refWire i branchPos edge destPos destEdge)

/// top-level function which replaces autoupdate and implements a smarter version of same
/// it is called every time a new wire is created, so is easily tested.
let smartAutoroute (model: Model) (wire: Wire) : Wire =
    let initialWire = (autoroute model wire)

    /// A wire of a net which is drawn on its own, when it could have shared a trunk with the rest
    /// of the net, is the failure that shows most: not because it is longer, but because a reader
    /// can no longer see at a glance which wires are one signal. So a route which branches off a
    /// wire of the same net is preferred to the ordinary one whenever it is legal, and the branch
    /// points nearest the destination - the ones which share the most - are tried first.
    ///
    /// This matters most for the long wires, which often have several destinations: three long
    /// wires crossing a sheet nearly in parallel is what this is here to prevent.
    let snappedToNetWire =
        match model.SnapToNet with
        | false -> initialWire
        | true ->
            // nearest branch point to the destination first, and the first that is legal wins. The
            // ordinary route is in the running as the branch at the driver port, so a branch has to
            // start nearer the destination than the port does before it is taken at all.
            let destPos = Symbol.getInputPortLocation None model.Symbol wire.InputPort
            (euclideanDistance initialWire.StartPos destPos, initialWire) :: sameNetRoutes model wire
            |> List.sortBy fst
            |> List.tryFind (fun (_, w) -> List.isEmpty (findWireSymbolIntersections model w))
            |> Option.map snd
            |> Option.defaultValue initialWire

    let intersectedBoxes = findWireSymbolIntersections model snappedToNetWire 

    match intersectedBoxes.Length with
    | 0 -> snappedToNetWire
    | _ ->
        let nubbedWire = ensureBothNubs snappedToNetWire
        nubbedWire
        |> tryShiftVerticalSeg model intersectedBoxes
        |> Option.orElseWith ( fun () ->
            tryShiftHorizontalSeg maxCallsToShiftHorizontalSeg model intersectedBoxes snappedToNetWire)
        |> Option.defaultValue nubbedWire
   


//-----------------------------------------------------------------------------------------------------------//
//---------------------------------------------Top-level Wire Routing Functions------------------------------//
//-----------------------------------------------------------------------------------------------------------//

/// Returns a single re-routed wire from the given model.
/// First attempts partial autorouting, and defaults to full autorouting if this is not possible.
/// Reverse indicates if the wire should be processed in reverse, 
/// used when an input port (end of wire) is moved.
let updateWire (model : Model) (wire : Wire) (reverse : bool) =
    let newPort = 
        match reverse with
        | true -> Symbol.getInputPortLocation None model.Symbol wire.InputPort
        | false -> Symbol.getOutputPortLocation None model.Symbol wire.OutputPort
    if reverse then
        partialAutoroute model (reverseWire wire) newPort true
        |> Option.map reverseWire
    else 
        partialAutoroute model wire newPort false
    |> Option.defaultWith (fun () ->
        smartAutoroute model wire)

/// Re-route the wires which touch a symbol that has moved. A wire with both ends on moved symbols
/// keeps its shape and is handed to `translate` - it moves with them.
///
/// The routing comes off the wires that are going to be autorouted BEFORE any of them is routed, as
/// `redrawWires` does and for the same reason: a wire may be routed as a branch off another wire of
/// its own net, and a route which is about to be thrown away is not one to follow. `sameNetRoutes`
/// passes over a wire with no segments, so what each wire can see is exactly the wires that are
/// where they now belong - the ones already re-routed, and the ones that never moved.
///
/// A wire the user has routed by hand keeps its segments: `partialAutoroute` holds the shape they
/// dragged it into by working from the segments already there, and there is nothing to recover
/// that from once they are gone. Those are re-routed before the autorouted ones, so that the rest
/// of their net can follow them.
///
/// Autorouted wires go shortest first, again as `redrawWires` does: a short wire has the least
/// freedom in where it can go, so it is the one that should already be there when a longer wire of
/// its net is looking for something to join.
let rerouteMovedWires (translate: Model -> Wire -> Wire) (compIdList: ComponentId list) (model: Model) =
    let wires = filterWiresByCompMoved model compIdList
    let bothEndsMoved = Set.ofList wires.Both
    let inputEndMoved = Set.ofList wires.Inputs
    let portDistance (wire: Wire) =
        let destPos, startPos = Symbol.getTwoPortLocations model.Symbol wire.InputPort wire.OutputPort
        euclideanDistance startPos destPos
    let handRouted, autoRouted =
        wires.Inputs @ wires.Outputs
        |> List.distinct
        |> List.filter (bothEndsMoved.Contains >> not)
        |> List.sortBy (fun wid -> portDistance model.Wires[wid])
        |> List.partition (fun wid -> isManuallyRouted model.Wires[wid])
    /// An input end that moved is routed from that end, which is what `reverse` means.
    let route model wid =
        Optic.set (wireOf_ wid) (updateWire model model.Wires[wid] (inputEndMoved.Contains wid)) model
    (model, wires.Both)
    ||> List.fold (fun model wid -> Optic.set (wireOf_ wid) (translate model model.Wires[wid]) model)
    // The stripping comes before the hand-routed wires are done as well as before the autorouted
    // ones: partialAutoroute can fail, and a hand-routed wire that falls back to a full route is
    // looking at the same net as everything else.
    |> fun model ->
        (model, autoRouted)
        ||> List.fold (fun model wid -> Optic.set (wireOf_ wid >-> segments_) [] model)
    |> fun model -> (model, handRouted) ||> List.fold route
    |> fun model -> (model, autoRouted) ||> List.fold route

/// Re-routes the wires in the model based on a list of components that have been altered.
/// If the wire input and output ports are both in the list of moved components,
/// it does not re-route wire but instead translates it.
/// Keeps manual wires manual (up to a point).
/// Otherwise it will auto-route wires connected to components that have moved
let updateWires (model : Model) (compIdList : ComponentId list) (diff : XYPos) =
    rerouteMovedWires (fun _ wire -> moveWire wire diff) compIdList model



