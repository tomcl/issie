module SmartWire
open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers

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
/// This function returns (XYPos * XYPos) list corresponding to the Segment list 
/// The list of tupled XYPos coordinate, correspond to each segment in the segment list
/// HLP23: AUTHOR Rahimi
let getXYPosPairsOfSegments (segments: list<Segment>) (startPos: XYPos) (initialOrientation: Orientation)= 
        ([], segments)
        ||> List.fold (fun xyPosPairs segment -> 
            match xyPosPairs, (initialOrientation), (segment.Index % 2 = 0) with
                |[], Horizontal, true -> xyPosPairs@[(startPos), ((+) (startPos) {X=segment.Length; Y=0.0})]
                |[], Vertical, true -> xyPosPairs@[(startPos), ((+) (startPos) {X=0.0; Y=segment.Length})]
                |_ , Horizontal, true -> xyPosPairs@[snd(xyPosPairs[xyPosPairs.Length-1]), 
                                        ((+) (snd(xyPosPairs[xyPosPairs.Length-1])) {X=segment.Length; Y=0.0})]
                |_ , Vertical, true -> xyPosPairs@[snd(xyPosPairs[xyPosPairs.Length-1]), 
                                        ((+) (snd(xyPosPairs[xyPosPairs.Length-1])) {X=0.0; Y=segment.Length})]
                |_ , Vertical, false -> xyPosPairs@[snd(xyPosPairs[xyPosPairs.Length-1]), 
                                        ((+) (snd(xyPosPairs[xyPosPairs.Length-1])) {X=segment.Length; Y=0.0})]
                |_ , Horizontal, false -> xyPosPairs@[snd(xyPosPairs[xyPosPairs.Length-1]), 
                                        ((+) (snd(xyPosPairs[xyPosPairs.Length-1])) {X=0.0; Y=segment.Length})])

/// This function returns the minimum and maximum distance of Bounding Box (BB) from the XYPos tuple
/// The minimum distance is typically leftmost and top-most distance of BB from vertical and horizontal segment respectively
/// and the maximum distance is typically rightmost and bottom-most distance of BB from vertical and horizontal segment respectively.
/// The segment correspond to the segment that generate the XYPos tuple, but this can also be used to check the BB exists in some
/// XYPos tuple coordinates.
/// HLP23: AUTHOR Rahimi
let getMinMaxDistOfBBfromXYPosPair (model:Model) (xyPosPair: XYPos * XYPos) =
    let allBoundingBoxes: Map<ComponentId,BoundingBox> = Symbol.getBoundingBoxes model.Symbol
    let allLabelBoundingBoxes = Symbol.getLabelBoundingBoxes model.Symbol

    let xyPosPairOrientation = 
        match xyPosPair with
        |(A, B) when A.X = B.X -> Vertical
        |_,_ -> Horizontal
    
    let xyPosPairXmin =
        match xyPosPair with
        |(A, B) when A.X <= B.X -> A.X
        |(A, B) -> B.X

    let xyPosPairXmax =
        match xyPosPair with
        |(A, B) when A.X <= B.X -> B.X
        |(A, B) -> A.X

    let xyPosPairYmin =
        match xyPosPair with
        |(A, B) when A.Y <= B.Y -> A.Y
        |(A, B) -> B.Y

    let xyPosPairYmax =
        match xyPosPair with
        |(A, B) when A.Y <= B.Y -> B.Y
        |(A, B) -> A.Y

    let allBBList =
        allBoundingBoxes
        |> Map.filter (match xyPosPairOrientation with
                        |Horizontal -> fun key bb -> 
                                        (bb.TopLeft.X < xyPosPairXmax) && ((bb.TopLeft.X + bb.W) > xyPosPairXmin) &&
                                        (bb.TopLeft.Y < xyPosPairYmin) && ((bb.TopLeft.Y + bb.H) > xyPosPairYmin)
                        | Vertical -> fun key bb -> 
                                        (bb.TopLeft.Y < xyPosPairYmax) && ((bb.TopLeft.Y + bb.H) > xyPosPairYmin) &&
                                        (bb.TopLeft.X < xyPosPairXmin) && ((bb.TopLeft.X + bb.W) > xyPosPairXmin))
        |> Map.toList
    
    let allLabelBBList = 
        allLabelBoundingBoxes
        |> Map.filter (match xyPosPairOrientation with
                        |Horizontal -> fun key bb -> 
                                        (bb.TopLeft.X < xyPosPairXmax) && ((bb.TopLeft.X + bb.W) > xyPosPairXmin) &&
                                        (bb.TopLeft.Y < xyPosPairYmin) && ((bb.TopLeft.Y + bb.H) > xyPosPairYmin)
                        | Vertical -> fun key bb -> 
                                        (bb.TopLeft.Y < xyPosPairYmax) && ((bb.TopLeft.Y + bb.H) > xyPosPairYmin) &&
                                        (bb.TopLeft.X < xyPosPairXmin) && ((bb.TopLeft.X + bb.W) > xyPosPairXmin))
        |> Map.toList
    
    (allBBList @ allLabelBBList)
    |> List.map (fun (a,b) -> b)
    |> List.fold (match xyPosPairOrientation with
                    |Horizontal -> (fun state bb -> match state with 
                                    |A, B -> ((min A (bb.TopLeft.Y - xyPosPairYmin)), (max B (bb.TopLeft.Y + bb.H - xyPosPairYmin))))
                    |Vertical  -> (fun state bb -> match state with 
                                    |A, B -> ((min A (bb.TopLeft.X - xyPosPairXmin)), (max B (bb.TopLeft.X + bb.W - xyPosPairXmin)))))
                    (0.0,0.0)

/// This function is the extension of getMinMaxDistOfBBfromXYPosPair applied on list of XYPos tuple with slight modifcation
/// The first two and the last two elements are zeroed to avoid modification on the nubs
let getMinMaxDistanceOfBBfromXYPosPairs (model:Model) (xyPosPairs: (XYPos * XYPos) list) =
    xyPosPairs
    |> List.map (getMinMaxDistOfBBfromXYPosPair model)
    |> List.rev
    |> List.mapi (fun i el -> 
                    (match i with
                    | 0 | 1 -> (0.0,0.0)
                    | _ -> el))
    |> List.rev
    |> List.mapi (fun i el -> 
                    (match i with
                    | 0 | 1 -> (0.0,0.0)
                    | _ -> el))

/// This function is inspired by manual updateSegments, however it is implemented for autorouting
let autoUpdateSegments (segments:Segment list) (index: int) (distance:float) = 
    
        let idx = index

        if idx <= 0 || idx >= segments.Length - 1 then // Should never happen
            printfn $"Trying to move wire segment {idx}:{logSegmentId segments[idx]}, out of range in wire length {segments.Length}"
            segments
        else
            let safeDistance = getSafeDistanceForMove segments index distance
        
            let prevSeg = segments[idx - 1]
            let nextSeg = segments[idx + 1]
            let movedSeg = segments[idx]

            let newPrevSeg = { prevSeg with Length = prevSeg.Length + safeDistance }
            let newNextSeg = { nextSeg with Length = nextSeg.Length - safeDistance }
            let newMovedSeg = { movedSeg with Mode = Auto }
        
            let newSegments = 
                segments[.. idx - 2] @ [newPrevSeg; newMovedSeg; newNextSeg] @ segments[idx + 2 ..]
            newSegments

/// Returns a list of index sorted from the middle with length listLen.
/// Example 1: INPUT(5) -> OUTPUT([2,1,3,0,4])
/// Example 2: INPUT(6) -> OUTPUT([2,3,1,4,0,5])
let getSortedIndexListFromMiddle (listLen:int) =
    let maxI = listLen - 1
    let result = []

    let rec sortIndexFromMiddle n =
        match (n % 2 = 0), (n >= 0) with
        |true, true -> result@[n/2]@(sortIndexFromMiddle (n-1))
        |false, true -> result@[(n-1)/2]@[maxI-((n-1)/2)]@(sortIndexFromMiddle (n-2))
        |_, false -> result
    
    sortIndexFromMiddle (listLen-1)

/// This function returns a clean smartrouted segment if possible with priority on shorter route
/// If clean segment (segment with no intersection with Bounding Boxes) is not possible, the closest possible solution is returned
/// Limit is introduced to stop the function finding for optimal solution for cases where there is no solution
/// instead returning the best possible solution
let rec smartRouteSegment1 model segments startPos initialOrientation limit index =
    // printf $"HERE"
    let MinMaxDistanceOfBBfromXYPosPairs = getMinMaxDistanceOfBBfromXYPosPairs model (getXYPosPairsOfSegments segments startPos initialOrientation)
    let distancePair = MinMaxDistanceOfBBfromXYPosPairs[index]
    let distance = match distancePair with
                    |(a,b) when abs(a) < abs(b) -> a - 10.
                    |(a, b) -> b + 10.
    let segsNeedUpdate = (distancePair <> (0.0,0.0)) && (limit <> 0)
    // printf $"safe: {segsNeedUpdate}, dist: {distance}"
    match segsNeedUpdate with
    |true -> smartRouteSegment1 model (autoUpdateSegments segments index distance) startPos initialOrientation (limit-1) index 
    |false -> segments

let smartRouteSegments1 model (segments: Segment list) startPos initialOrientation: Segment list =
    let preSortedIndex = getSortedIndexListFromMiddle segments.Length
    let indexOf1 = preSortedIndex |> List.findIndex (fun x -> x = 1)

    let notSortedIndex, nubIndex = preSortedIndex |> List.splitAt indexOf1
    
    let sortedIndex = notSortedIndex
    (segments, (sortedIndex))
    ||> List.fold (fun segs i -> smartRouteSegment1 model segs startPos initialOrientation 15 i)        
                              
let rec smartRouteSegment2 model segments startPos initialOrientation limit index =
    // printf $"HERE"
    let MinMaxDistanceOfBBfromXYPosPairs = getMinMaxDistanceOfBBfromXYPosPairs model (getXYPosPairsOfSegments segments startPos initialOrientation)
    let distancePair = MinMaxDistanceOfBBfromXYPosPairs[index]
    let distance = match distancePair with
                    |(a,b) when abs(a) > abs(b) -> a - 10.
                    |(a, b) -> b + 10.
    let segsNeedUpdate = (distancePair <> (0.0,0.0)) && (limit <> 0)
    // printf $"safe: {segsNeedUpdate}, dist: {distance}"
    match segsNeedUpdate with
    |true -> smartRouteSegment1 model (autoUpdateSegments segments index distance) startPos initialOrientation (limit-1) index 
    |false -> segments

/// This function returns a clean smartrouted segment if possible with priority on longer route
/// If clean segment (segment with no intersection with Bounding Boxes) is not possible, the closest possible solution is returned
/// Further optimization: smartRouteSegments2 can be merged with smartRouteSegments1 with some conditioning logic,
/// the idea to have to approach in smartrouting the segment came in late thus resulting in two different but almost identical functions.
let smartRouteSegments2 model (segments: Segment list) startPos initialOrientation: Segment list =
    let preSortedIndex = getSortedIndexListFromMiddle segments.Length
    let indexOf1 = preSortedIndex |> List.findIndex (fun x -> x = 1)
    let sortedIndex, nubIndex = preSortedIndex |> List.splitAt indexOf1
    
    (segments, (sortedIndex))
    ||> List.fold (fun segs i -> smartRouteSegment2 model segs startPos initialOrientation 15 i)  

/// top-level function which replaces autoupdate and implements a smarter version of same
/// it is called every time a new wire is created, so is easily tested.

/// This function improves the previous autoroute function
/// This function identify if the segment produced by the previous function is intersected with Bounding Boxes (BB)
/// If intersections exist, this function update the segment such that it has the least possible intersection with BB
let smartAutoroute (model: Model) (wire: Wire): Wire = 
    let initWire = autoroute model wire
    let segments = initWire.Segments
    let startPos = initWire.StartPos
    let initialOrientation = initWire.InitialOrientation

    let smartSegments1 = smartRouteSegments1 model segments startPos initialOrientation
    let smartSeg1StillIntersect = getMinMaxDistanceOfBBfromXYPosPairs model (getXYPosPairsOfSegments smartSegments1 startPos initialOrientation) |> List.exists (fun (x, y) -> x <> 0. || y <> 0.)
    let smartSegments2 = smartRouteSegments2 model segments startPos initialOrientation
    let smartSeg2StillIntersect = getMinMaxDistanceOfBBfromXYPosPairs model (getXYPosPairsOfSegments smartSegments2 startPos initialOrientation) |> List.exists (fun (x, y) -> x <> 0. || y <> 0.)
    
    let newSegments = if smartSeg1StillIntersect && smartSeg2StillIntersect then smartSegments1
                                    elif smartSeg1StillIntersect then smartSegments2
                                    else smartSegments1
    // printf $"{newSegments}"
    { wire with
          Segments = newSegments
          InitialOrientation = initialOrientation
          StartPos = startPos
    }
