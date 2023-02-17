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

module Constants =
    let buffer = 10.
    let maxCallsToShiftHorizontalSeg = 10

//------------------------------------------------------------------------//
//--------------------------Shifting Vertical Segment---------------------//
//------------------------------------------------------------------------//

/// Try shifting vertical seg to either - buffer or + buffer of intersected symbols.
/// Returns None if no route found
let tryShiftVerticalSeg (model: Model) (intersectedBoxes: BoundingBox list) (wire: Wire) : Wire option =
    let wireVertices =
        segmentsToIssieVertices wire.Segments wire
        |> List.map (fun (x, y, _) -> { X = x; Y = y })

    let currentVerticalSegXPos = wireVertices[4].X

    let shiftVerticalSeg amountToShift =
        let prevSeg = wire.Segments[2]
        let nextSeg = wire.Segments[4]
        let newPrevSeg = { prevSeg with Length = prevSeg.Length + amountToShift }
        let newNextSeg = { nextSeg with Length = nextSeg.Length - amountToShift }

        let newSegments =
            wire.Segments[..1]
            @ [ newPrevSeg ] @ wire.Segments[3..3] @ [ newNextSeg ] @ wire.Segments[5..]

        { wire with Segments = newSegments }

    let tryShiftLeftWire =
        let leftBound = intersectedBoxes |> List.map (fun box -> box.TopLeft.X) |> List.min
        let amountToShift = currentVerticalSegXPos - leftBound + Constants.buffer
        shiftVerticalSeg -amountToShift

    let tryShiftRightWire =
        let rightBound =
            intersectedBoxes |> List.map (fun box -> box.TopLeft.X + box.W) |> List.max

        let amountToShift = rightBound - currentVerticalSegXPos + Constants.buffer
        shiftVerticalSeg amountToShift

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
type BoundingBoxAboveOrBelow =
    | Above of float
    | Below of float

/// Check if any bounding box is directly above or below startPos and endPos
/// If yes, returns a tuple of form
/// distance between pos and the furthest box above, distance between pos and the furthest box below
let isBoundingBoxAboveOrBelowPos (intersectedBoxes: BoundingBox list) (pos: XYPos) : float * float =

    let isBoxAboveOrBelowPos (pos: XYPos) (box: BoundingBox) : BoundingBoxAboveOrBelow option =
        if pos.X > box.TopLeft.X && pos.X < box.TopLeft.X + box.W then
            if pos.Y > box.TopLeft.Y then
                Above(pos.Y - box.TopLeft.Y) |> Some
            else
                Below(box.TopLeft.Y - pos.Y + box.H) |> Some
        else
            None

    let verticalDistances =
        intersectedBoxes
        |> List.map (isBoxAboveOrBelowPos pos)
        |> List.filter (fun x -> x <> None)
        |> List.map (Option.get)

    let largestDistanceAbove =
        if verticalDistances.Length = 0 then
            0.
        else
            verticalDistances
            |> List.map (fun x ->
                match x with
                | Above d -> d
                | Below _ -> 0.)
            |> List.max

    let largestDistanceBelow =
        if verticalDistances.Length = 0 then
            0.
        else
            verticalDistances
            |> List.map (fun x ->
                match x with
                | Above _ -> 0.
                | Below d -> d)
            |> List.max

    largestDistanceAbove, largestDistanceBelow


/// Recursively call this function to try shifting horizontal seg up/down until no symbol intersections.
/// Limit in recursion depth defined by argument callsLeft given to initial function call.
/// Limit needed to prevent Issie from breaking when there are physically
/// no possible routes that achieve 0 intersections.
/// Returns None if no route found
let rec tryShiftHorizontalSeg
    (model: Model)
    (intersectedBoxes: BoundingBox list)
    (wire: Wire)
    (callsLeft: int)
    : Wire option =
    if callsLeft = 0 then
        None
    else
        let currentStartPos, currentEndPos = getStartAndEndWirePos wire

        let generateLongHorizontalWire firstVerticalSegLength secondVerticalSegLength =
            let newSegments =
                if wire.Segments.Length = 7 then
                    // Change segments index 1,3,5. Leave rest as is
                    wire.Segments[..0]
                    @ [ { wire.Segments[1] with Length = firstVerticalSegLength } ]
                      @ wire.Segments[2..2]
                        @ [ { wire.Segments[3] with Length = 0 } ]
                          @ wire.Segments[4..4]
                            @ [ { wire.Segments[5] with Length = secondVerticalSegLength } ]
                              @ wire.Segments[6..]
                else
                    // Change segments index 1,3,5,7. Leave rest as is
                    wire.Segments[..0]
                    @ [ { wire.Segments[1] with Length = 0. } ]
                      @ wire.Segments[2..2]
                        @ [ { wire.Segments[3] with Length = firstVerticalSegLength } ]
                          @ wire.Segments[4..4]
                            @ [ { wire.Segments[5] with Length = secondVerticalSegLength } ]
                              @ wire.Segments[6..6]
                                @ [ { wire.Segments[7] with Length = 0 } ] @ wire.Segments[8..]

            { wire with Segments = newSegments }

        let tryShiftUpWire =
            let topBound = intersectedBoxes |> List.map (fun box -> box.TopLeft.Y) |> List.min
            let firstVerticalSegLength = topBound - Constants.buffer - currentStartPos.Y
            let secondVerticalSegLength = currentEndPos.Y - (topBound - Constants.buffer)
            generateLongHorizontalWire firstVerticalSegLength secondVerticalSegLength

        let tryShiftDownWire =
            let bottomBound =
                intersectedBoxes |> List.map (fun box -> box.TopLeft.Y + box.H) |> List.max

            let firstVerticalSegLength = bottomBound + Constants.buffer - currentStartPos.Y
            let secondVerticalSegLength = currentEndPos.Y - (bottomBound + Constants.buffer)
            generateLongHorizontalWire firstVerticalSegLength secondVerticalSegLength

        let upShiftedWireIntersections = findWireSymbolIntersections model tryShiftUpWire

        let downShiftedWireIntersections =
            findWireSymbolIntersections model tryShiftDownWire

        // If newly generated wire has no intersections, return that
        // Otherwise, decide to shift up or down based on which is closer
        match upShiftedWireIntersections, downShiftedWireIntersections with
        | [], _ -> Some tryShiftUpWire
        | _, [] -> Some tryShiftDownWire
        | _, _ ->
            let (distanceAboveFromStart, distanceBelowFromStart) =
                isBoundingBoxAboveOrBelowPos intersectedBoxes currentStartPos

            let (distanceAboveFromEnd, distanceBelowFromEnd) =
                isBoundingBoxAboveOrBelowPos intersectedBoxes currentEndPos

            match max distanceAboveFromStart distanceAboveFromEnd, max distanceBelowFromStart distanceBelowFromEnd with
            | distanceFromAbove, distanceFromBelow when distanceFromAbove > distanceFromBelow ->
                tryShiftHorizontalSeg model downShiftedWireIntersections tryShiftDownWire (callsLeft - 1)
            | distanceFromAbove, distanceFromBelow (*when distanceFromAbove <= distanceFromBelow*)  ->
                tryShiftHorizontalSeg model upShiftedWireIntersections tryShiftUpWire (callsLeft - 1)


/// top-level function which replaces autoupdate and implements a smarter version of same
/// it is called every time a new wire is created, so is easily tested.
let smartAutoroute (model: Model) (wire: Wire) : Wire =

    let initialWire = autoroute model wire

    let intersectedBoxes = findWireSymbolIntersections model initialWire

    match intersectedBoxes.Length, wire.InitialOrientation with
    | _, initialOrientation when initialOrientation = Vertical -> initialWire
    | 0, _ -> initialWire
    | _, initialOrientation ->
        // TODO: Ask about this very very strange bug... - Only breaks for port 3 of MUX4
        // printfn "Called"
        // printfn "%A" initialWire.InitialOrientation
        // printfn "%A" (initialOrientation = Vertical)
        // initialWire

        tryShiftVerticalSeg model intersectedBoxes initialWire
        |> Option.orElse (tryShiftHorizontalSeg model intersectedBoxes initialWire Constants.maxCallsToShiftHorizontalSeg)
        |> Option.defaultValue initialWire
