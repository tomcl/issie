module SmartWire

open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers
open SmartHelpers

open Optics
open Operators

(* HLP23

    This module will normally be used exclusively by team member doing the "smart autoroute on single
    wire creation" part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    It does not need specific "get started" test code since is called whenever a new wire is created 
    or (not usual) a symbol is moved so far that the topology of a wire chnages and it is autorouted 
    again from scratch.

*)

(* HLP23 AUTHOR: Jian Fu Eng (jfe20)
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

module Constants =
    let wireSeparationFromSymbol = 10.
    let maxCallsToShiftHorizontalSeg = 5
    let minWireSeparation = 10.
    let smallOffset = 0.0001

type DirectionToMove =
    | Up_
    | Down_
    | Left_
    | Right_

let swapXY (pos: XYPos) (orientation: Orientation) : XYPos =
    match orientation with
    | Horizontal -> pos
    | Vertical -> { X = pos.Y; Y = pos.X }

let swapBB (box: BoundingBox) (orientation: Orientation) : BoundingBox =
    match orientation with
    | Horizontal -> box
    | Vertical ->
        { TopLeft = swapXY box.TopLeft orientation
          W = box.H
          H = box.W }

let updatePos (direction: DirectionToMove) (distanceToShift: float) (pos: XYPos) : XYPos =
    match direction with
    | Up_ -> { pos with Y = pos.Y - distanceToShift }
    | Down_ -> { pos with Y = pos.Y + distanceToShift }
    | Left_ -> { pos with X = pos.X - distanceToShift }
    | Right_ -> { pos with X = pos.X + distanceToShift }

/// Recursively tries to find the minimum wire separation between a wire and a symbol.
/// If attempted position is too close to another parallel wire, increment separation by Constants.minWireSeparation
let rec findMinWireSeparation
    (model: Model)
    (pos: XYPos)
    (wire: Wire)
    (direction: DirectionToMove)
    (movedSegOrientation: Orientation)
    (symbolEdgeLength: float)
    =
    // Search for intersecting wires in a search box given by symbolEdgeLength
    let searchH, searchW =
        match movedSegOrientation with
        | Horizontal -> Constants.minWireSeparation * 2., symbolEdgeLength
        | Vertical -> symbolEdgeLength, Constants.minWireSeparation * 2.

    let searchBox =
        { TopLeft =
            pos
            |> updatePos Up_ (Constants.minWireSeparation / 2.)
            |> updatePos Left_ (Constants.minWireSeparation / 2.)
          H = searchH
          W = searchW }

    let intersectingWires = getWiresInBox searchBox model
    // If wires are in same net, do not need to increment wire separation
    let wiresInSameNet = isWireInNet model wire

    let isWireInNetlist (w: Wire) : bool =
        match wiresInSameNet with
        | None -> false
        | Some(_, netlist) -> netlist |> List.exists (fun (_, w2) -> w2.WId = w.WId)

    let rec isShiftNeeded =
        function
        | [] -> false
        | (w, segIndex) :: rest ->
            let segIndex = segIndex + if wire.InitialOrientation = Vertical then 1 else 0

            match w.WId = wire.WId, isWireInNetlist w, movedSegOrientation, segIndex % 2 with
            | false, false, movedSegOrientation, 1 when movedSegOrientation = Vertical -> true
            | false, false, movedSegOrientation, 0 when movedSegOrientation = Horizontal -> true
            | _ -> isShiftNeeded rest

    match isShiftNeeded intersectingWires with
    | false -> pos
    | true ->
        let newPos = updatePos direction Constants.minWireSeparation pos
        findMinWireSeparation model newPos wire direction movedSegOrientation symbolEdgeLength

/// Checks if a wire intersects any symbol within +/- Constants.minWireSeparation
/// Returns list of bounding boxes of symbols intersected by wire.
let findWireSymbolIntersections (model: Model) (wire: Wire) : (ComponentId * BoundingBox) list =
    let allSymbolBoundingBoxes = Symbol.getBoundingBoxes model.Symbol

    let wireVertices =
        segmentsToIssieVertices wire.Segments wire
        |> List.map (fun (x, y, _) -> { X = x; Y = y })

    let segVertices = List.pairwise wireVertices[1 .. wireVertices.Length - 2] // do not consider the nubs

    let inputCompId = model.Symbol.Ports[string wire.InputPort].HostId
    let outputCompId = model.Symbol.Ports[string wire.OutputPort].HostId

    let inputCompRotation =
        model.Symbol.Symbols[ComponentId inputCompId].STransform.Rotation

    let outputCompRotation =
        model.Symbol.Symbols[ComponentId outputCompId].STransform.Rotation

    let isConnectedToSelf = inputCompId = outputCompId

    let boxesIntersectedBySegment startPos endPos =
        allSymbolBoundingBoxes
        |> Map.map (fun compID boundingBox ->
            let isInputComp = inputCompId = string compID
            let isOutputComp = outputCompId = string compID

            match isConnectedToSelf, isInputComp, isOutputComp, inputCompRotation with
            | true, true, _, n when n = Degree0 || n = Degree180 ->
                { W = boundingBox.W
                  H = boundingBox.H + Constants.minWireSeparation * 2.
                  TopLeft = boundingBox.TopLeft |> updatePos Up_ Constants.minWireSeparation }
            | true, true, _, n when n = Degree90 || n = Degree270 ->
                { W = boundingBox.W + Constants.minWireSeparation * 2.
                  H = boundingBox.H
                  TopLeft = boundingBox.TopLeft |> updatePos Left_ Constants.minWireSeparation }
            | false, true, _, _ -> boundingBox
            | false, _, true, _ -> boundingBox
            | _ ->
                { W = boundingBox.W + Constants.minWireSeparation * 2.
                  H = boundingBox.H + Constants.minWireSeparation * 2.
                  TopLeft =
                    boundingBox.TopLeft
                    |> updatePos Left_ Constants.minWireSeparation
                    |> updatePos Up_ Constants.minWireSeparation })
        |> Map.filter (fun _ boundingBox ->
            match segmentIntersectsBoundingBox boundingBox startPos endPos with // do not consider the symbols that the wire is connected to
            | Some _ -> true // segment intersects bounding box
            | None -> false // no intersection
        )
        |> Map.toList

    segVertices
    |> List.collect (fun (startPos, endPos) -> boxesIntersectedBySegment startPos endPos)
    |> List.distinct

//------------------------------------------------------------------------//
//--------------------------Shifting Vertical Segment---------------------//
//------------------------------------------------------------------------//

/// Try shifting vertical seg to either - wireSeparationFromSymbol or + wireSeparationFromSymbol of intersected symbols.
/// Returns None if no route found.
let tryShiftVerticalSeg (model: Model) (intersectedBoxes: (ComponentId * BoundingBox) list) (wire: Wire) : Wire option =
    let wireVertices =
        segmentsToIssieVertices wire.Segments wire
        |> List.map (fun (x, y, _) -> { X = x; Y = y })

    let shiftVerticalSeg amountToShift =
        let newSegments =
            wire.Segments
            |> List.updateAt 2 { wire.Segments[2] with Length = wire.Segments[2].Length + amountToShift }
            |> List.updateAt 4 { wire.Segments[4] with Length = wire.Segments[4].Length - amountToShift }

        { wire with Segments = newSegments }

    let tryShiftWireVert (dir: DirectionToMove) =

        let boundBox =
            intersectedBoxes
            |> List.sortWith (fun (_, box1) (_, box2) ->
                let box1 = swapBB box1 wire.InitialOrientation
                let box2 = swapBB box2 wire.InitialOrientation

                match dir with
                | Left_ -> compare (box1.TopLeft.X) (box2.TopLeft.X)
                | Right_ -> compare (box2.TopLeft.X + box2.W) (box1.TopLeft.X + box1.W)
                | _ -> failwith "Invalid direction to shift wire")
            |> List.head
            |> snd

        let viablePos =
            match dir, wire.InitialOrientation with
            | Left_, Horizontal ->
                let initialAttemptPos = updatePos Left_ Constants.smallOffset boundBox.TopLeft

                findMinWireSeparation model initialAttemptPos wire Left_ Vertical boundBox.H
            | Right_, Horizontal ->
                let initialAttemptPos =
                    updatePos Right_ (boundBox.W + Constants.smallOffset) boundBox.TopLeft

                findMinWireSeparation model initialAttemptPos wire Right_ Vertical boundBox.H
            | Left_, Vertical ->
                let initialAttemptPos = updatePos Up_ Constants.smallOffset boundBox.TopLeft

                findMinWireSeparation model initialAttemptPos wire Up_ Horizontal boundBox.W
            | Right_, Vertical ->
                let initialAttemptPos =
                    updatePos Down_ (boundBox.H + Constants.smallOffset) boundBox.TopLeft

                findMinWireSeparation model initialAttemptPos wire Down_ Horizontal boundBox.W
            | _ -> failwith "Invalid direction to shift wire"

        let amountToShift =
            (swapXY viablePos wire.InitialOrientation).X
            - (swapXY wireVertices[4] wire.InitialOrientation).X

        shiftVerticalSeg amountToShift

    let tryShiftLeftWire = tryShiftWireVert Left_
    let tryShiftRightWire = tryShiftWireVert Right_

    let leftShiftedWireIntersections =
        findWireSymbolIntersections model tryShiftLeftWire

    let rightShiftedWireIntersections =
        findWireSymbolIntersections model tryShiftRightWire

    // Check which newly generated wire has no intersections, return that
    match leftShiftedWireIntersections, rightShiftedWireIntersections with
    | [], _ -> Some tryShiftLeftWire
    | _, [] -> Some tryShiftRightWire
    | _, _ -> None

//------------------------------------------------------------------------//
//-------------------------Shifting Horizontal Segment--------------------//
//------------------------------------------------------------------------//
type VertDistFromBoundingBox =
    | Above of float // Vertical distance between pos and a bounding box above
    | Below of float // Vertical distance between pos and a bounding box below

/// Check if any bounding box is directly above or below startPos and endPos.
/// If yes, returns a tuple of form:
/// distance between pos and the furthest box above, distance between pos and the furthest box below
let isBoundingBoxAboveOrBelowPos
    (intersectedBoxes: (ComponentId * BoundingBox) list)
    (pos: XYPos)
    (wireOrientation: Orientation)
    : float * float =

    let getVertDistanceToBox (pos: XYPos) (box: BoundingBox) : VertDistFromBoundingBox option =
        (swapXY pos wireOrientation, swapBB box wireOrientation)
        ||> (fun pos box ->
            if inMiddleOrEndOf box.TopLeft.X pos.X (box.TopLeft.X + box.W) then
                if pos.Y > box.TopLeft.Y then
                    pos.Y - box.TopLeft.Y |> Above |> Some
                else
                    box.TopLeft.Y - pos.Y + box.H |> Below |> Some
            else
                None)

    let verticalDistances =
        intersectedBoxes
        |> List.map (fun (_compID, box) -> getVertDistanceToBox pos box)
        |> List.filter (fun x -> x <> None)
        |> List.map (Option.get)

    // Recursively extracts largest distance above and below pos from list of distances
    let rec largestDistance verticalDistances (currentLargestAbove, currentLargestBelow) =
        match verticalDistances with
        | [] -> currentLargestAbove, currentLargestBelow
        | Above d :: rest ->
            if d > currentLargestAbove then
                largestDistance rest (d, currentLargestBelow)
            else
                largestDistance rest (currentLargestAbove, currentLargestBelow)
        | Below d :: rest ->
            if d > currentLargestBelow then
                largestDistance rest (currentLargestAbove, d)
            else
                largestDistance rest (currentLargestAbove, currentLargestBelow)

    largestDistance verticalDistances (0., 0.)

/// Recursively shift horizontal seg up/down until no symbol intersections.
/// Limit in recursion depth defined by argument callsLeft given to initial function call.
/// Limit needed to prevent Issie from breaking when there are physically
/// no possible routes that achieve 0 intersections.
/// Returns None if no route found
let rec tryShiftHorizontalSeg
    (callsLeft: int)
    (model: Model)
    (intersectedBoxes: (ComponentId * BoundingBox) list)
    (wire: Wire)
    : Wire option =
    match callsLeft with
    | 0 -> None
    | n ->
        let tryShiftHorizontalSeg = tryShiftHorizontalSeg (n - 1)

        let currentStartPos, currentEndPos = getStartAndEndWirePos wire

        let shiftWireHorizontally firstVerticalSegLength secondVerticalSegLength =
            let newSegments =
                match wire.Segments.Length with
                | 5
                | 6 ->
                    wire.Segments
                    |> List.updateAt 1 { wire.Segments.[1] with Length = firstVerticalSegLength }
                    |> List.updateAt 3 { wire.Segments.[3] with Length = secondVerticalSegLength }
                | 7 ->
                    // Change into a 5 segment wire
                    wire.Segments[..4]
                    |> List.updateAt 1 { wire.Segments.[1] with Length = firstVerticalSegLength }
                    |> List.updateAt
                        2
                        { wire.Segments.[2] with Length = wire.Segments.[2].Length + wire.Segments.[4].Length }
                    |> List.updateAt 3 { wire.Segments.[3] with Length = secondVerticalSegLength }
                    |> List.updateAt 4 { wire.Segments.[6] with Index = 4 }
                | 9 ->
                    // Change segments index 1,3,5,7. Leave rest as is
                    wire.Segments
                    |> List.updateAt 1 { wire.Segments[1] with Length = 0. }
                    |> List.updateAt 3 { wire.Segments[3] with Length = firstVerticalSegLength }
                    |> List.updateAt 5 { wire.Segments[5] with Length = secondVerticalSegLength }
                    |> List.updateAt 7 { wire.Segments[7] with Length = 0. }
                | _ -> wire.Segments

            { wire with Segments = newSegments }

        let tryShiftUpWire =
            let topBoundBox =
                intersectedBoxes
                |> List.sortWith (fun (_, box1) (_, box2) ->
                    match wire.InitialOrientation with
                    | Horizontal -> compare box1.TopLeft.Y box2.TopLeft.Y
                    | Vertical -> compare box1.TopLeft.X box2.TopLeft.X)
                |> List.head
                |> snd

            let topBound =
                let viablePos =
                    match wire.InitialOrientation with
                    | Horizontal ->
                        let initialAttemptPos = updatePos Up_ Constants.smallOffset topBoundBox.TopLeft

                        findMinWireSeparation model initialAttemptPos wire Up_ Horizontal topBoundBox.W
                    | Vertical ->
                        let initialAttemptPos = updatePos Left_ Constants.smallOffset topBoundBox.TopLeft

                        findMinWireSeparation model initialAttemptPos wire Left_ Vertical topBoundBox.H

                match wire.InitialOrientation with
                | Horizontal -> viablePos.Y
                | Vertical -> viablePos.X

            let firstVerticalSegLength, secondVerticalSegLength =
                match wire.InitialOrientation with
                | Horizontal -> topBound - currentStartPos.Y, currentEndPos.Y - topBound
                | Vertical -> topBound - currentStartPos.X, currentEndPos.X - topBound

            shiftWireHorizontally firstVerticalSegLength secondVerticalSegLength

        let tryShiftDownWire =
            let bottomBoundBox =
                intersectedBoxes
                |> List.sortWith (fun (_, box1) (_, box2) ->
                    match wire.InitialOrientation with
                    | Horizontal -> compare (box1.TopLeft.Y + box1.H) (box2.TopLeft.Y + box2.H)
                    | Vertical -> compare (box1.TopLeft.X + box1.W) (box2.TopLeft.X + box2.W))
                |> List.rev
                |> List.head
                |> snd

            let bottomBound =
                let viablePos =
                    match wire.InitialOrientation with
                    | Horizontal ->
                        let initialAttemptPos =
                            updatePos Down_ (Constants.smallOffset + bottomBoundBox.H) bottomBoundBox.TopLeft

                        findMinWireSeparation model initialAttemptPos wire Down_ Horizontal bottomBoundBox.W
                    | Vertical ->
                        let initialAttemptPos =
                            updatePos Right_ (Constants.smallOffset + bottomBoundBox.W) bottomBoundBox.TopLeft

                        findMinWireSeparation model initialAttemptPos wire Right_ Vertical bottomBoundBox.H

                match wire.InitialOrientation with
                | Horizontal -> viablePos.Y
                | Vertical -> viablePos.X

            let firstVerticalSegLength, secondVerticalSegLength =
                match wire.InitialOrientation with
                | Horizontal -> bottomBound - currentStartPos.Y, currentEndPos.Y - bottomBound
                | Vertical -> bottomBound - currentStartPos.X, currentEndPos.X - bottomBound

            shiftWireHorizontally firstVerticalSegLength secondVerticalSegLength

        let upShiftedWireIntersections = findWireSymbolIntersections model tryShiftUpWire

        let downShiftedWireIntersections =
            findWireSymbolIntersections model tryShiftDownWire

        // If newly generated wire has no intersections, return that
        // Otherwise, decide to shift up or down based on which is closer
        match upShiftedWireIntersections, downShiftedWireIntersections with
        | [], [] ->
            if getWireLength tryShiftDownWire < getWireLength tryShiftUpWire then
                Some tryShiftDownWire
            else
                Some tryShiftUpWire
        | [], _ -> Some tryShiftUpWire
        | _, [] -> Some tryShiftDownWire
        | _, _ ->
            let (distanceAboveFromStart, distanceBelowFromStart) =
                isBoundingBoxAboveOrBelowPos intersectedBoxes currentStartPos wire.InitialOrientation

            let (distanceAboveFromEnd, distanceBelowFromEnd) =
                isBoundingBoxAboveOrBelowPos intersectedBoxes currentEndPos wire.InitialOrientation

            match max distanceAboveFromStart distanceAboveFromEnd, max distanceBelowFromStart distanceBelowFromEnd with
            | distanceFromAbove, distanceFromBelow when distanceFromAbove > distanceFromBelow ->
                tryShiftHorizontalSeg model downShiftedWireIntersections tryShiftDownWire
            | _distanceFromAbove, _distanceFromBelow (*when _distanceFromAbove <= _distanceFromBelow*)  ->
                tryShiftHorizontalSeg model upShiftedWireIntersections tryShiftUpWire

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
        ComponentId model.Symbol.Ports[string wireToRoute.InputPort].HostId

    let outputCompId =
        ComponentId model.Symbol.Ports[string wireToRoute.OutputPort].HostId

    let isRotated =
        model.Symbol.Symbols[inputCompId].STransform.Rotation <> Degree0
        || model.Symbol.Symbols[outputCompId].STransform.Rotation <> Degree0

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
        // Take first wire in netlist as reference wire for snapping
        let refWire = netlist.Head |> snd
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


/// top-level function which replaces autoupdate and implements a smarter version of same
/// it is called every time a new wire is created, so is easily tested.
let smartAutoroute (model: Model) (wire: Wire) : Wire =

    let initialWire = (autoroute model wire)

    // Snapping to Net only if model.SnapToNet toggled to be true
    let snappedToNetWire =
        match model.SnapToNet with
        | false -> initialWire
        | true -> snapToNet model initialWire

    let intersectedBoxes = findWireSymbolIntersections model snappedToNetWire

    match intersectedBoxes.Length with
    | 0 -> snappedToNetWire
    | _ ->
        tryShiftVerticalSeg model intersectedBoxes snappedToNetWire
        |> Option.orElse (
            tryShiftHorizontalSeg Constants.maxCallsToShiftHorizontalSeg model intersectedBoxes snappedToNetWire
        )
        |> Option.defaultValue snappedToNetWire
