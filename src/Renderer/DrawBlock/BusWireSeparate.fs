module BusWireSeparate
open CommonTypes
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWireUpdateHelpers
open BusWireRoutingHelpers
open BlockHelpers
open Optics
open Operators
open BusWireRoute
open BusWire

//*****************************************************************************************************//
//---------------------------------Smart Channel / Segment Order / Separate----------------------------//
//*****************************************************************************************************//

(*-----------------------------------------------------------------------------------------------------
    This code implements a sheet beautify function that is designed to be called at the end of a symbol drag, 
    wire creation, etc, after smart autoroute. Therefore it has time to analyse the whole circuit and make changes. 
 
    Currently implements:
    - spread out overlapping wire segments
    - order wire segments to minimise crossings
    - order wire segments to minimise overlaps
    - allow same-net segments to overlap

    Does not implement:
    - Re-order ports on custom components, or flip other components. That would be an obvious and quite easy 
    extension.
    - Chunk together same net segments with manually routed ones in a way that makes manual movement
    - of overlapped nets simple. Not clear what is the coorrect UI for this
  -----------------------------------------------------------------------------------------------------*)

open BusWireRoutingHelpers.Constants // for easy access to SmartWire Constant definitions

//-------------------------------------------------------------------------------------------------//
//---------------------------------LINE ARRAY CREATION FROM MODEL----------------------------------//
//-------------------------------------------------------------------------------------------------//

// see Line type definition for context on what are lines

/// return wire and segment index of line, if line a segment, otehrwise return None.
let lineToWire 
        (model: Model) 
        (line:Line)  
            : (Wire * int) option =
    match line.Seg1 with
    | Some seg ->
        let (int,wid) = seg.Segment.GetId
        let wire = model.Wires[wid]
        Some (wire,int)
    | None -> None
    

/// Convert a segment into a fixed or movable line (of given orientation).
/// seg: ASegment of given segment to convert.
/// wire: wire of given segment to convert.
/// ori: orientation of segment (for reasons of efficiecny - it could be calculated from seg).
/// lType: type of line generated.
let segmentToLine 
        (lType: LType) 
        (ori: Orientation) 
        (wire:Wire) 
        (seg: ASegment) 
            : Line =
    let order a b =
        if a < b then
            { MinB = a; MaxB = b }
        else
            { MinB = b; MaxB = a }

    let line: Line = // the Horizontal case
        {   P = seg.Start.Y
            Orientation = ori
            B = order seg.Start.X seg.End.X
            LType = lType
            Seg1 = Some seg
            SameNetLink = []
            Wid = wire.WId
            PortId = wire.OutputPort
            Lid = LineId 0 }

    match ori with
    | Horizontal ->
        line
    | Vertical ->
        {line with // changes needed for Vertical case
            P = seg.Start.X; 
            B = order seg.Start.Y seg.End.Y}

/// Convert a symbol BoundingBox into two fixed lines (of given orientation).
/// The lines correspond to the two box edges of the specified orientation.
let bBoxToLines (ori: Orientation) (box: BoundingBox) : Line list =
    let tl = box.TopLeft

    match ori with
    | Horizontal -> [ tl.Y, tl.X, tl.X + box.W; tl.Y + box.H, tl.X, tl.X + box.W ]
    | Vertical -> [ tl.X, tl.Y, tl.Y + box.H; tl.X + box.W, tl.Y, tl.Y + box.H ]
    |> List.map (fun (p, minB, maxB) ->
        {   P = p
            B =
              { MinB = minB + smallOffset
                MaxB = maxB - smallOffset }
            Orientation = ori
            LType = BARRIER
            Seg1 = None
            SameNetLink = []
            Wid = ConnectionId ""
            PortId = OutputPortId ""
            Lid = LineId 0 })

/// Where two segments in lines are on the same Net and on top of each other we must NEVER separate them.
/// This function links such segments, and marks all except the head one as a LINKEDSEG 
/// so that the clustering algorithm will ignore them.
/// sameNetCapture specified how close segments muts be to be linked.
let linkSameNetLines (sameNetCapture: float) (lines: Line list) : Line list =
    /// input: list of lines all in the same Net (same outputPort)
    /// output: similar list, with lines that are on top of each other and in different wires linked
    let overlaps = hasNearOverlap separateCaptureOverlap
    let linkSameNetGroup (lines: Line list) =
        let lines = List.toArray lines
        let hasLinkedOverlap (la: Line) (lb:Line) =
            overlaps la.B lb.B 
        let tryToLink (a:int) (b:int) =
            let la, lb = lines[a], lines[b]
            if (la.LType = NORMSEG || la.LType = FIXEDMANUALSEG || la.LType = FIXEDSEG ) &&
                lb.LType <> FIXEDMANUALSEG && lb.LType <> FIXEDSEG && lb.LType <> LINKEDSEG && la.Wid <> lb.Wid &&
                closeBy sameNetCapture la.P lb.P && hasLinkedOverlap la lb  then
                lines[b].LType <- LINKEDSEG                    
                lines[a].B <- boundUnion la.B lb.B;
                lines[a].SameNetLink <-  lines[b] :: lines[b].SameNetLink @ lines[a].SameNetLink
                lines[b].SameNetLink <- []
        // in this loop the first lines[a] in each linkable set links all the set, setting ClusterSegment = false
        // Linked lines are then skipped.
        for a in [0..lines.Length-1] do
            for b in [0..lines.Length-1] do
                tryToLink a b
        Array.toList lines

    lines
    |> List.groupBy (fun line -> line.PortId)
    |> List.collect (fun (port, lines) -> linkSameNetGroup lines)

/// Make all lines, fixed and movable, of given orientation from wires and symbols in Model
/// ori - orientation of Lines (P coord is reverse of this)
let makeLines (wiresToRoute: ConnectionId list) (ori: Orientation) (model: Model) =

    /// Which segments in wires are included as Lines?
    let selectSegments (wire: Wire) (orient: Orientation) (seg: Segment) =
        let numSegs = wire.Segments.Length
        let wireLength = euclideanDistance wire.StartPos wire.EndPos
        ori = orient && seg.Index <> 0 && seg.Index <> numSegs - 1 && not seg.IsZero && wireLength > minWireLengthToSeparate

    /// Lines coming from wire segments
    /// Manually routed segments are considered fixed
    /// Segments next to zero length segments are considered fixed
    /// (they form part of straight lines extending the fixed nub)
    let segLines =
        ([], model.Wires)
        ||> Map.fold (fun (lines: Line list) _ wire ->
            let wireIsRoutable = List.contains wire.WId wiresToRoute
            getFilteredAbsSegments (selectSegments wire) wire
            |> List.map (fun aSeg ->
                let segs = wire.Segments
                let seg = aSeg.Segment
                let lType =
                    match wireIsRoutable, seg.Mode, seg.Index=2, seg.Index=segs.Length-3 with
                    | _, Manual , _ , _
                    | false, _, _, _ ->
                        FIXEDMANUALSEG
                    | _, _ , true , _ when segs[ 1 ].IsZero -> 
                        FIXEDSEG
                    | _, _ , _ , true when  segs[ segs.Length - 2 ].IsZero -> 
                        FIXEDSEG
                    | _ -> 
                        NORMSEG
                segmentToLine lType ori wire aSeg)
            |> (fun wireLines -> wireLines @ lines))
        |> List.mapi (fun i line -> line.Lid <- LineId i; line) // add temp Lid so that linkSameNetLines works
        |> linkSameNetLines Constants.modernCirclePositionTolerance

    /// Lines coming from the bounding boxes of symbols
    let symLines =
        model.Symbol.Symbols
        |> Map.toList
        |> List.collect (fun (_, sym) ->
            if sym.Annotation = None then
                Symbol.getSymbolBoundingBox sym |> bBoxToLines ori
            else
                [])
    symLines @ segLines
    |> List.toArray
    |> Array.sortBy (fun line -> line.P)
    |> Array.mapi (fun i line -> line.Lid <- LineId i; line) // rewrite Lid


//-------------------------------------------------------------------------------------------------//
//-----------------------------------------SEGMENT ORDERING----------------------------------------//
//-------------------------------------------------------------------------------------------------//

/// Returns integers +/- 1 indicating direction of wire leaving ends of line segment.
/// Pair returned is MaxB, MinB end of line
let turnDirs (line: Line) (wires: Map<ConnectionId, Wire>) =
    match line.Seg1 with
    | None -> failwithf "What? Expected Some segment - not None"
    | Some aSeg ->
        let seg = aSeg.Segment
        let wSegs = wires[seg.WireId].Segments
        // segment length is + or - according to whether segment.P end is larger or samller than start.
        let segLength segIndex = wSegs[segIndex].Length
        // len1, len2 is P coordinate (P = X or Y) change from the line segment at MaxB, MinB end of line.
        // the seg.Index-1 end has change inverted because its change is from, not to line.
        let len1, len2 =
            if seg.Length > 0 then
                segLength (seg.Index + 1), - segLength(seg.Index - 1)
            else
                - segLength(seg.Index - 1), segLength (seg.Index + 1)

        sign len1, sign len2


/// +1 if line1.P > line2.P for zero crossings.
/// -1 if line1.P < line2.P for zero crossings.
/// 0 if line1.P and line2.P have one crossing.
let numCrossingsSign 
        (model: Model) 
        (line1: Line) 
        (line2: Line) 
        (wires: Map<ConnectionId, Wire>) 
            : int =
    let (max1, min1), (max2, min2) = turnDirs line1 wires, turnDirs line2 wires
    // if line1.P > line2.P then a +1 line1 turnDir or a -1 line2 turnDir from an inner endpoint
    // will NOT cause a crossing. -1 will cause a crossing.
    // The match sums the two inner turnDirs, inverting sign if they come from a line2
    // turning. Dividing this by 2 gives the required answer!
    // NB this is simpler than expected since it does not matter what order the two inner ends are
    // in - which makes identifying them (as one of the MaxB and one of the MinB ends) easier.

    /// Returns Some segment  if segment 3 or 5 of a 9 segment wire
    /// These segments should be adjusted for zero crossings at end only
    let endSegOpt (line:Line) = 
        match lineToWire model line with
        | Some({Segments=segs}, index) when segs.Length = 9 && (index = 3 || index = 5) -> 
            Some segs[index]
        | _ -> None

    /// checkMinCross,checkMaxCross = 1 if the respective bound = min or max end should be ordered to minimise
    /// crossings, 0 to disable the check.
    /// If we have an end Seg (index = 3 or 5 of 9 segs) then only one end of the segment should be checked in this way
    /// If we have a middle segment both ends are checked.
    /// in unrecognised cases default to checking both ends.
    let checkMinCross, checkMaxCross =
        match endSegOpt line1, endSegOpt line2 with
        | Some seg, _ 
        | _, Some seg ->
            if seg.Length > 0 && seg.Index = 3 then
                1 , 0
            else
                0 , 1
        | None, None -> 1, 1

    // Put it all together. The min & max values are chosen based on
    // the relative positions of the min & max ends of the two lines.
    // To check this, write out all 4 combinations on paper.
    match line1.B.MinB > line2.B.MinB, line1.B.MaxB < line2.B.MaxB with
    | true, true ->   min1 , max1
    | true, false ->  min1 , -max2
    | false, true ->  -min2 , max1
    | false, false -> -min2 , - max2
    |> (fun (minC, maxC) -> checkMinCross * minC + checkMaxCross * maxC)




/// segL is a list of lines array indexes representing segments found close together.
/// Return the list ordered in such a way that wire crossings are minimised if the
/// segments are placed as ordered. The return list is placed with required P value increasing
/// along the list.
let orderPairwiseToMinimiseCrossings (model: Model) (lines: Line array) (segL: int list) =
    let wires = model.Wires
    let numCrossingsSign' l0 l1 = numCrossingsSign model l0 l1 wires
    // special case - included for efficency
    match segL.Length with
    | 1 ->
        segL // special case: nothing to do
    | 2 -> // special case for efficiency (would work without this)
        let l0, l1 = lines[segL[0]], lines[segL[1]]
        if numCrossingsSign' l0 l1 > 0 then
            [segL[1]; segL[0]]
        else segL 
    | numSegments -> 
        let wires = model.Wires
        let segA = segL |> Array.ofList
        /// inverse of segA[index]: NB indexes [0..numSegments-1] are NOT Segment index.
        /// These indexes are used inside this function only to allow contiguous arrays
        /// to calculate the sort order
        let indexOf seg = Array.findIndex ((=) seg) segA
        let swapSegs a b =
            let tmp = segA[b]
            segA[b] <- segA[a]
            segA[a] <- tmp
        /// correctly order segments segA[n] and segA[n+1] by mutating segA indices.
        /// mutation is local to this function and used for efficiency since this is time critical
        let orderPair n =
            if numCrossingsSign' lines[segA[n]] lines[segA[n+1]] > 0 then
                swapSegs n (n+1)
            else ()
            
        // Map each index [0..numSegments-1] to a number that will determine its (optimal) ordering
        // Use bubblesort based on adjacent pairs of lines: this will make order correct where possible
        // this will not cope with more complex mis-orderings - but it is fast.
        // should we check (and reorder) groups of 3 segments?
        for i =  0 to numSegments do
            for j = 0 to numSegments-2 do
                orderPair j

        segA
        |> Array.toList
        |> List.map (fun index -> match lines[index].Lid with LineId n -> n)
      
//-------------------------------------------------------------------------------------------------//
//---------------------------------------SEGMENT CLUSTERING----------------------------------------//
//-------------------------------------------------------------------------------------------------//

/// When given a segment index search for nearby segments to be considered with it as a single cluster
/// for spreading out. To be included segments must be close enough and overlapping. Search
/// terminates given large gap or a fixed boundary segments are not allowed to move across.
let expandCluster (index: int) (searchDir: LocSearchDir) (lines: Line array) =
    let nextIndex i =
        match searchDir with
        | Upwards -> i + 1
        | _ -> i - 1

    let searchStart = lines[index].P

    let initLoc, lowestDownwardsIndex =
        match searchDir with
        | Upwards ->
              { UpperFix = None
                LowerFix = None
                Bound = lines[index].B
                Segments = [ index ] },
              None
        | Downwards loc ->
            let index = List.max loc.Segments
            { loc with Segments = [ index ] }, Some(List.min loc.Segments)

    let rec expand i loc =
        let nSegs = float loc.Segments.Length
        if (i < 0 || i >= lines.Length) || abs (lines[i].P - searchStart) > maxSegmentSeparation * (nSegs+2.) + smallOffset then
            {loc with Segments = List.sortDescending loc.Segments}
        elif not  (hasOverlap loc.Bound lines[i].B) then
            expand (nextIndex i) loc
        else
            let p = lines[i].P
            match lines[i].LType with
            | BARRIER | FIXEDMANUALSEG | FIXEDSEG ->
                let p = lines[i].P
                match searchDir with
                | Upwards -> { loc with UpperFix = Some p }
                | _ -> { loc with LowerFix = Some p } // fixed boundary 
                |> (fun loc -> {loc with Segments = List.sortDescending loc.Segments})
            | LINKEDSEG ->
                expand (nextIndex i) loc

            | NORMSEG ->
                match lowestDownwardsIndex with
                //| Some index when i < index -> expand (nextIndex i) loc // past starting point, so can't add segments, but still check for a Fix
                | _ ->
                    expand
                        (nextIndex i)
                        { loc with
                            Segments = i :: loc.Segments
                            Bound = boundUnion loc.Bound lines[i].B }

    expand (nextIndex index) initLoc

/// Check a cluster for same net segments within separateCaptureOverlap
/// Remove from cluster and all except one in every such same net group
/// The removed segments are marked LINKEDSEG and linked for later processing
let linkAndRemoveSameNetSegments (lines: Line array) (cluster: Cluster) =
    cluster.Segments
    |> List.map (fun seg -> lines[seg])
    |> linkSameNetLines (separateCaptureOverlap)
    |> List.filter (fun line -> line.LType <> LINKEDSEG)
    |> List.map (fun line -> line.Lid.Index)
    |> (fun newSegs -> {cluster with Segments = newSegs})

/// print diagnostics in rare case that a segment gets "orphaned"
/// this should probably never happened and be fixed if anything is printed.
let printLostSegmentInCluster (msg:string) (lines: Line array) (lostIndex: int) (loc: Cluster) =
    printf "%s" msg
    // TODO: add diagnostic info here
    ()

/// Scan through segments in P order creating a list of local Clusters.
/// Within one cluster segments are adjacent and overlapping. Note that
/// different clusters may occupy the same P values if their segments do
/// not overlap.
/// Segments within each cluster will be repositioned and reordered after
/// clusters are identified.
/// Every segment must be part of a unique cluster.
let makeClusters (lines: Line array) : Cluster list =
    /// true if corresponding line can be grouped in a cluster as a segment
    let groupableA =
        Array.init lines.Length (fun i ->lines[i].LType = NORMSEG)

    let groupable seg = groupableA[seg]

    let keepOnlyGroupableSegments (loc: Cluster) =
        { loc with Segments = List.filter groupable loc.Segments }

    let markSegmentsAsGroupable (loc: Cluster) =
        loc.Segments |> List.iter (fun seg -> groupableA[seg] <- false)

    /// Recursive function identifies a new cluster from the 
    let rec getClusters lines =
        match Array.tryFindIndex ((=) true) groupableA with
        | None -> []
        // nextIndex is the lowest groupable index in lines, around which another cluster can be constructed.
        | Some nextIndex ->
            /// print diagnostics for unexpected case where the original segment in the cluster
            /// ends up 'lost' and not included in any cluster.
            /// Return original cluster in a list with new cluster containing lost segment if needed.
            let handleLostNextIndex (msg: string) (loc: Cluster) =
                if not <| List.contains nextIndex loc.Segments then
                    printLostSegmentInCluster msg lines nextIndex loc
                    let orphanLoc = {
                        Segments = [nextIndex]
                        UpperFix = None
                        LowerFix = None
                        Bound = loc.Bound}
                    [orphanLoc; loc]
                else
                    [loc]

                

            // to find a cluster of overlapping segments search forward first until there is a gap
            let loc1 = expandCluster nextIndex Upwards lines
            // now, using the (larger) union of bounds fond searching forward, search backwards. This may find
            // extra lines due to larger bound and in any case will search at least a little way beyond the initial
            // start - enough to see if there is a second barrier.
            // note that every segment can only be grouped once, so this search will not pick up previously clustered
            // segments when searching backwards.
            let loc2 = expandCluster (List.max loc1.Segments) (Downwards loc1) lines

            match loc2 with
            | { Segments = lowestLoc2Index :: _
                LowerFix = lowerFix } when lines[lowestLoc2Index].P > lines[nextIndex].P ->
                    List.except loc2.Segments loc1.Segments
                    |> (fun loc1LostSegs ->
                        if loc1LostSegs = [] then
                            // no original (upward search) segments not also found in loc2 (downward search).
                            // So return loc2.
                            [ loc2 ]  
                        else
                            // we have some loc1 segments (segs)  not captured by l2
                            if not <| List.contains nextIndex loc1LostSegs then
                                printf "What? nextIndex has got lost from loc1! Trying to repair..."

                            { loc1 with
                                Segments = loc1LostSegs
                                UpperFix = lowerFix }
                            |> (fun loc -> expandCluster (List.max loc.Segments) (Downwards loc) lines)
                            |> handleLostNextIndex "What? nextIndex has got lost from loc1 after expansion!"
                            |> List.append [loc2]) // return the expanded loc1LostSegs as  a cluster with loc2
            | _ ->
                if not <| List.contains nextIndex loc2.Segments then
                    handleLostNextIndex  "What? nextIndex has got lost from loc2!" loc2
                else
                    [ loc2 ]
            |> List.map keepOnlyGroupableSegments
            |> List.filter (fun loc -> loc.Segments <> [])
            |> (fun newLocs ->
                    List.iter markSegmentsAsGroupable newLocs
                    if groupable nextIndex then
                        failwithf "Error: infinite loop detected in cluster find code"
                    newLocs @ getClusters lines)
    getClusters lines
    |> List.map (linkAndRemoveSameNetSegments lines)

// Currently not used. Running the algorithm twice fixes problems otherwise needing merge (and other things).
// Should decide what is an acceptable space between merged clusters so as not to move
// segments too far.
// /// Return single cluster with segments from loc1 and loc2 merged
(*
let mergeLocs (lines: Line array) (loc1: Cluster) (loc2: Cluster) =
    if upperB lines loc1 < lowerB lines loc2 || not (hasOverlap loc1.Bound loc2.Bound) then
        [ loc1; loc2 ] // do not merge
    else
        // Bound and SearchStart fields are no longer used.
        [ { loc1 with
                UpperFix = loc2.UpperFix
                Segments = loc1.Segments @ loc2.Segments } ]

/// Currently not used.
/// Go through the list of clusters merging where possible, return merged list.
/// lines is array of Lines from which clusters are generated
let mergeLocalities (lines: Line array) (locL: Cluster list) =
    let rec merge (mergedLocs: Cluster list) (locL: Cluster list) =
        match mergedLocs, locL with
        | mLocs, [] -> mLocs // no clusters to merge!
        | [], loc :: locs -> merge [ loc ] locs
        | currLoc :: mLocL, loc :: locL ->
            match currLoc.UpperFix with
            | Some upperB -> merge (loc :: currLoc :: mLocL) locL
            | None -> merge (mergeLocs lines currLoc loc @ mLocL) locL

    merge [] locL
*)
/// Function which given a cluster (loc) works out how to
/// spread out the contained segments optimally, spacing them from other segments and symbols.
/// Return value is a list of segments, represented as Lines, paired with where they move.
/// lines is the source list of lines (vertical or horizontal according to which is being processed).
/// model is the Buswire model needed to access wires.
let calcSegPositions model lines (loc: Cluster) =
    let segs = loc.Segments |> List.distinct |> orderPairwiseToMinimiseCrossings model lines
    // if segs.Length > 1 then
    let pts = segs |> List.map (fun i -> lines[i].P)
    let nSeg = loc.Segments.Length

    let spreadFromStart start sep =
        segs |> List.iteri (fun i seg -> lines[seg].P <- start + sep * float i)

    let spreadFromMiddle mid sep =
        segs
        |> List.iteri (fun i seg -> lines[seg].P <-  mid + sep * float i - float (nSeg - 1) * sep / 2.)

    let spreadFromEnd endP sep =
        segs |> List.iteri (fun i seg -> lines[seg].P <-  endP + sep * float (i - (nSeg - 1)))

    let maxSep = maxSegmentSeparation
    let halfMaxSep = maxSegmentSeparation / 2.
    let idealMidpoint = (List.min pts + List.max pts) / 2.
    let halfIdealWidth = float (nSeg - 1) * halfMaxSep

    let idealStart, idealEnd =
        idealMidpoint - halfIdealWidth, idealMidpoint + halfIdealWidth
    // Fixed bounds and soft segment bounds behave differently
    // Segments are placed maxSegmentSeparation away from fixed bound but only halfSep away from soft bounds
    match loc.UpperFix, loc.LowerFix, nSeg with
    | None, None, 1 -> () // no change
    | Some bMax, Some bMin, n when (bMax - bMin) / (float n + 1.) < maxSep ->
        //printf $"spread {nSeg} constrained"
        spreadFromMiddle ((bMax + bMin) / 2.) ((bMax - bMin) / (float n + 1.))
    | _, Some bMin, _ when bMin + maxSep > idealStart ->
        //printf $"spread {nSeg} from start"
        spreadFromStart (bMin + maxSep) maxSep
    | Some bMax, _, n when bMax - maxSep < idealEnd ->
        //printf $"spread {nSeg} from end - endP={bMax-maxSep}"
        spreadFromEnd (bMax - maxSep) maxSep
    | bMax, bMin, n ->
        //printf $"spread {nSeg} from middle bmax= {bMax}, bMin={bMin}"
        spreadFromMiddle idealMidpoint maxSep


/// Given a list of segment changes of given orientation apply them to the model
let adjustSegmentsInModel 
        (ori: Orientation) 
        (model: Model) 
        (lines: Line list) 
            : Model =
    lines
    |> List.iter (fun line ->
            (line.SameNetLink |> List.iter (fun line2 -> line2.P <- line.P)))
    let lines = lines |> List.filter (fun line -> line.LType <> BARRIER)
    let wires =
        (model.Wires, lines)
        ||> List.fold (fun wires line ->
            let seg = Option.get line.Seg1
            moveLine ori line.P line wires)

    Optic.set wires_ wires model

/// Segments which could be moved, but would make an extra segment if moved, are marked Fixed
/// and not moved by the normal cluster-based separation functions.
/// This function looks at these segments and moves them a little in the special case that they
/// overlap. It is called after the main segment separation is complete.
let separateFixedSegments (wiresToRoute: ConnectionId list) (ori: Orientation) (model: Model) =
    /// direction from line which has maximum available P space, up to maxOffset,
    /// Return value is space available - negative if more space is in negative direction.
    let getSpacefromLine (lines: Line array) (line: Line) (excludeLine: Line) (maxOffset: float) =
        let p = line.P
        let find offset dir = 
            tryFindIndexInArray 
                (LineId(line.Lid.Index + dir)) 
                dir 
                (fun line2 -> hasOverlap line2.B line.B && line2.Lid <> excludeLine.Lid ) 
                (fun l1 -> abs (l1.P - p) > 2. * offset) 
                lines
        match find maxOffset 1, find maxOffset -1 with
        | None, _ -> maxOffset
        | _, None -> -maxOffset
        | Some a, Some b -> 
            if abs (lines[a.Index].P - p) > abs (lines[b.Index].P - p) then 
                lines[a.Index].P - p
            else 
                lines[b.Index].P - p

    let allLines = makeLines wiresToRoute ori model
    allLines
    |> Array.filter (fun line -> line.LType = FIXEDSEG)
    |> (fun checkedLines ->
        checkedLines
        |> Array.toSeq
        |> Seq.iter ( fun line1 ->
           checkedLines
           |> Array.toSeq
           |> Seq.filter (fun line2 ->
                line1.Lid < line2.Lid &&
                abs (line1.P - line2.P) < overlapTolerance &&
                line1.PortId <> line2.PortId &&
                hasOverlap line1.B line2.B)
           |> Seq.iter (fun line2 ->
                let space1 = getSpacefromLine allLines line1 line2 2*maxSegmentSeparation
                let space2 = getSpacefromLine allLines line2 line1 2*maxSegmentSeparation
                if space1 < overlapTolerance && space2 < overlapTolerance then
                    printf "WARNING: No space for fixed segment shifting overlap"
                if abs space1 > abs space2 then
                    line1.P <- line1.P + space1 * 0.5
                else
                    line2.P <- line1.P + space2 * 0.5)))
    allLines
    |> Array.toList
    |> adjustSegmentsInModel ori model

    
//-------------------------------------------------------------------------------------------------//
//--------------------------------------WIRE ARTIFACT CLEANUP--------------------------------------//
//-------------------------------------------------------------------------------------------------//
(*
    The segment-based optimisations can sometimes leave wires in a non-optimal state with too many
    corners. This code scans down each 9 segment wire and attempts to remove redundant corners:

    ----              ------           ------               ----
        |      ==>          |                |         ===>     |
        ---                 |              ---                  |
          |                 |              |                    |
    
    Note that these two cases are the same: two consecutive turns are removed and a 3rd turn is moved 
    as required to keep wires joining.

    The optimised wire can be accepted as long as 
    (1) it does not go inside or too close to symbols
    (2) it does not go too close to other wires.

*)

/// Return the index of the Line with the smallest value of P > p
/// Use binary earch for speed.
let findInterval (lines: Line array) ( p: float): int =
    let rec find above below =
        if above - below < 2 then above
        else
            let mid = (above + below) / 2
            if lines[mid].P < p then
                find above mid
            else
                find mid below
    find (lines.Length - 1) 0

/// Return true if there is no overlap between line and lines array (with exception of excludedLine).
/// All lines are the same type (parallel)
let checkExtensionNoOverlap 
        (overlap: float) 
        (ext: Extension)
        (excludedWire: ConnectionId) 
        (info: LineInfo) : bool =
    let lines =
        match ext.ExtOri with
        | Horizontal -> info.HLines
        | Vertical -> info.VLines
    let b = ext.ExtB
    let p = ext.ExtP
    let iMin = findInterval lines (p - overlap)
    let rec check i =
        if i >= lines.Length || i < 0  || lines[i].P > p + overlap then 
            true
        elif lines[i].Wid = excludedWire || not (hasNearOverlap overlap b lines[i].B) then
            check (i+1)
        else
            false
    check iMin


/// Return true if there is no crossing symbol boundary between line 
/// and lines array (with exception of excludedLine).
/// Lines and excludedLine or opposite orientation from line
let checkExtensionNoCrossings 
        (overlap: float) 
        (ext: Extension)
        (excludedWire: ConnectionId) 
        (info: LineInfo) : bool =

    let lines =
        match ext.ExtOri with
        | Horizontal -> info.VLines
        | Vertical -> info.HLines
    let b = ext.ExtB
    let p = ext.ExtP
    let iMin = findInterval lines (b.MinB - overlap)
    let rec check i =
        let otherLine = lines[i]
        if i >= lines.Length || i < 0 || otherLine.P > otherLine.B.MaxB + overlap then 
            true
        else  
            let b = otherLine.B; 
            if lines[i].Wid = excludedWire || b.MinB > p || b.MaxB < p || not (lines[i].LType = BARRIER) then
                check (i+1)
            else
                false
    check iMin


/// Process the symbols and wires in Model generating arrays of Horizontal and Vertical lines.
/// In addition the inverse map is generated which can map each segmnet to the corresponding Line if that
/// exists.
/// Note that Lines reference segments, which contain wire Id and segment Index and can therefore be used to
/// reference the corresponding wire via the model.Wires map.
let makeLineInfo (wiresToRoute: ConnectionId list) (model:Model) : LineInfo =
    
        let hLines = makeLines wiresToRoute Horizontal model
        let vLines = makeLines wiresToRoute Vertical model
        let wireMap = model.Wires
        let lineMap =
            Array.append hLines vLines
            |> Array.collect (fun line -> 
                match line.Seg1 with
                | None -> [||]
                | Some aSeg -> 
                    [| aSeg.Segment.GetId, line.Lid |] )
            |> Map.ofArray
        {
            HLines = hLines
            VLines = vLines
            WireMap = wireMap
            LineMap = lineMap
        }
    
/// Return true if the given segment length change is allowed.
/// If the new segment creates a part line segment
/// that did not previouly exist this is checked for overlap
/// with symbols and other wires.
let isSegmentExtensionOk
        (info: LineInfo)
        (wire: Wire)
        (segNum: int)
        (ori: Orientation)
        (newLength: float)
            : bool =
    let segs = wire.Segments
    let seg = wire.Segments[segNum]
    let len = seg.Length
    let aSegStart, _ = getAbsoluteSegmentPos wire segNum
    let p, startC =
        match ori with
        | Vertical -> aSegStart.X, aSegStart.Y
        | Horizontal -> aSegStart.Y, aSegStart.X
    /// check there is room for the proposed segment extension
    let extension = {ExtP = p; ExtOri = ori; ExtB = {MinB = min startC startC+newLength; MaxB = max startC startC+newLength}}
    // printf $"P=%.0f{extension.ExtP}, ori={extension.ExtOri}, B=%A{extension.ExtB}"
    // a zero-length segment means the two segments on either side of it are parallel and may overlap.
    // if we change the length of a segment next to a zero-length segment we must ensure that it does not double back on itself.
    // usually that will mean coming thr wrong wau out of a component edge (inside the component)!
    if segNum = 2 && segs[1].IsZero && sign segs[0].Length <> sign newLength ||
       segNum = segs.Length - 3 && segs[segs.Length-2].IsZero && sign segs[segs.Length-1].Length <> sign newLength
    then
        false // in this case a segment must backtrack from a nub - a bad idea
    else
        // finally, check whetehr the new extended segments overlap or cross other segments or symbol edges.
        checkExtensionNoOverlap extensionTolerance extension wire.WId info &&
        checkExtensionNoCrossings extensionTolerance extension wire.WId info


/// Return the list of wire corners found in given wire with all corner
/// edges smaller than cornerSizeLimit. A wire can have at most one corner.
let findWireCorner (info: LineInfo) (cornerSizeLimit: float) (wire:Wire): WireCorner list =
    let segs = wire.Segments
    let nSegs = wire.Segments.Length
    let pickStartOfCorner (start:int) : WireCorner option =
        // the "corner" consists of segments start, start=1, start+2,start+3
        // start+1, start+2 segments are deleted, replaced by extensions of segments start and start +3
        // this function determines whether wire as a corner at index start, and if so returns
        // Some wc where wc data structure represnets the Corner.

        let seg = segs[start]    
        if segs[start].IsZero || segs[start+3].IsZero then  // we don't want to extend a zero-length segment - it would not simplify the wire
            None
        else
            let deletedSeg1,deletedSeg2 = segs[start+1], segs[start+2]
            let hasManualSegment = List.exists (fun i -> segs[i].Mode = Manual) [start..start+3]
            let hasLongSegment = max (abs deletedSeg1.Length) (abs deletedSeg2.Length) > cornerSizeLimit
            if hasManualSegment || hasLongSegment || deletedSeg1.IsZero || deletedSeg2.IsZero then 
                // segments which are very long maybe should not be removed - perhaps there is some reson for them?
                // "manual" segments are never chnaged by the wire separation and routing - the user has said they should
                // be as they are.
                None
            else
                let ori = wire.InitialOrientation
                let startSegOrientation = if seg.Index % 2 = 0 then ori else switchOrientation ori
                let newLength1 = seg.Length + deletedSeg2.Length
                let newLength2 = deletedSeg1.Length - segs[start+3].Length
                if isSegmentExtensionOk info wire start startSegOrientation newLength1 &&
                    isSegmentExtensionOk info wire (start+3)  (switchOrientation startSegOrientation) newLength2
                then
                    {
                        Wire = wire
                        StartSeg = start
                        StartSegOrientation = startSegOrientation
                        StartSegChange = deletedSeg2.Length
                        EndSegChange = deletedSeg1.Length
                    } |> Some
                else
                    None                        
    // Wire corners cannot start on zero-length segments (that would introduce
    // an extra bend). The 4 segments changed by the corner cannot be manually
    // routed.
    [0.. nSegs-5]
    |> List.tryPick pickStartOfCorner
    |> function | None -> [] | Some x -> [x]

/// Change LineInfo removing a corner from a wire.
/// TODO: currently only WireMap changes
let removeCorner (info: LineInfo) (wc: WireCorner): LineInfo =
    let removeSegments start num (segments: Segment list) =
        segments
        |> List.removeManyAt start num
        |> (List.mapi (fun i seg -> if i > start - 1 then {seg with Index = i} else seg))

    //printf $"**Removing corner: visible nub={getVisibleNubLength false wc.Wire}, {getVisibleNubLength true wc.Wire} **"
    let addLengthToSegment (delta:float) (seg: Segment)=
        {seg with Length = seg.Length + delta}
    let wire' = 
        wc.Wire.Segments
        |> List.updateAt wc.StartSeg (addLengthToSegment wc.StartSegChange wc.Wire.Segments[wc.StartSeg])
        |> List.updateAt (wc.StartSeg + 3) (addLengthToSegment wc.EndSegChange wc.Wire.Segments[wc.StartSeg + 3])
        |> removeSegments (wc.StartSeg+1) 2
        |> (fun segs -> {wc.Wire with Segments = segs})
    {info with WireMap = Map.add wire'.WId wire' info.WireMap}

/// Return model with corners identified and removed where possible. 
/// Corners are artifacts - usually small - which give wires more visible segments than is needed.
let removeModelCorners wires (model: Model) =
    let info = makeLineInfo wires model
    let wires = model.Wires
    let corners =
        wires
        |> Map.values
        |> Seq.toList
        |> List.collect (findWireCorner info maxCornerSize)
    (info, corners)
    ||> List.fold removeCorner
    |> (fun info' -> Optic.set wires_ info'.WireMap model)       
    
/// Return None, or Some wire' where wire' is wire with spikes removed.
/// Spikes segments that turn back on previous ones (with a zero-length segment in between).
/// Optimised for the case that there are no spikes and None is returned.
let removeWireSpikes (wire: Wire) : Wire option =
    let segs = wire.Segments
    (None, segs)
    ||> List.fold (fun segsOpt seg ->
        let n = seg.Index
        let segs = Option.defaultValue segs segsOpt
        let nSeg = segs.Length
        if n > nSeg - 3 || not segs[n+1].IsZero || sign segs[n].Length = sign segs[n+2].Length then 
            segsOpt
        else
            let newSegN = {segs[n] with Length = segs[n].Length + segs[n+2].Length}
            let lastSegs = 
                segs[n+3..nSeg-1]
                    
            [
                segs[0..n-1]
                [newSegN]
                (List.mapi (fun i seg -> {seg with Index = i + n + 1}) lastSegs)
            ]
            |> List.concat
            |> Some)  
    |> Option.map (fun segs ->
            {wire with Segments = segs})

/// return model with all wire spikes removed
let removeModelSpikes (model: Model) =
    (model.Wires, model.Wires)
    ||> Map.fold (fun wires wid wire ->
        match removeWireSpikes wire with
        | None -> wires
        | Some wire' -> Map.add wid wire' wires)
    |> (fun wires -> {model with Wires = wires})


//-------------------------------------------------------------------------------------------------//
//----------------------------------------TOP LEVEL FUNCTIONS--------------------------------------//
//-------------------------------------------------------------------------------------------------//

/// Perform complete segment ordering and separation for segments of given orientation.
/// wires: set of wires allowed to be moved.
let separateModelSegmentsOneOrientation (wiresToRoute: ConnectionId list) (ori: Orientation) (model: Model) =

    (*
        TODO: would it be better, overall, to separate all wires?
        This was done before excludeclustersWithoutWiresToRoute
        To go back to this - remove excludeClustersWithoutWiresToRoute.
    *)
    /// Add linked line changes before movement changes. Movement changes will override
    /// linked line chnages if need be.

    let allWires = model.Wires |> Map.keys |> Seq.toList

    /// We do the line generation for ALL wires
    /// Then, after all  segments are clustered, we actually change segments only in clusters
    /// that contains wiresToRoute. The other clusters should not need to be reseparated.
    let excludeClustersWithoutWiresToRoute (lines: Line array) =
        let routedLines =
            lines
            |> Array.filter (fun line -> List.contains line.Wid wiresToRoute)
            
        List.filter (fun (cluster:Cluster) ->
            cluster.Segments
            |> List.exists (fun seg -> (Array.exists (fun line -> line.Lid.Index = seg) routedLines)))
       

    let lines = makeLines allWires ori model

    makeClusters lines
    |> excludeClustersWithoutWiresToRoute lines
    |> List.iter (calcSegPositions model lines)

    lines
    |> Array.toList
    |> adjustSegmentsInModel ori model

/// Perform complete wire segment separation and ordering for all orientations.
/// wiresToRoute: set of wires to have segments separated and ordered
let separateAndOrderModelSegments (wiresToRoute: ConnectionId list) (model: Model) : Model =
        if wiresToRoute = [] then
            model // do nothing
        else
            printfn "Separating all segments!"
            // Currently: separate all wires - not just those (in wiresToRoute) that
            // have changed. This prevents unrouted segments from pinning new segments.
            // TODO: see whetehr something better can be worked out, and whether routing segments
            // can be done interactively.

            /// convenience abbreviation
            let separate = separateModelSegmentsOneOrientation wiresToRoute

            // In theory one run Vertical and Horizontal of separate should be enough. However multiple runs work better
            // chunking togetherclusters that should be connected etc.
            // TODO: revisit this and see how necessary it is.

            separate Horizontal model // separate all horizontal wire segments
            |> separate Vertical // separate all vertical wire segments
            |> separate Horizontal // a final pair of checks allows ordering and "chunking" to work nicely in almost all cases
            |> separate Vertical  //
            |> separate Horizontal //

            // after normal separation there may be "fixed" segments which should be separated because they overlap
            // one run for Vert and then Horiz segments is enough for this
            // TODO - include a comprehensive check for any remaining overlapping wires after this - and fix them
            |> separateFixedSegments wiresToRoute Horizontal  
            |> separateFixedSegments wiresToRoute Vertical  

            // after the previous two phases there may be artifacts where wires have an unnecessary number of corners.
            // this code attempts to remove such corners if it can be done while keeping routing ok

            |> removeModelCorners wiresToRoute // code to clean up some non-optimal routing


/// Top-level function to replace updateWireSegmentJumps
/// and call the Segment separate code as well. This should
/// run when significant circuit wiring changes have been made
/// e.g. at the end of symbol drags.
let updateWireSegmentJumpsAndSeparations wires model  =
    model
    |> separateAndOrderModelSegments wires
    |> BusWireUpdateHelpers.updateWireSegmentJumps []

/// Top-level function does routing and then separation of set of wires.
/// Uses partial routing if possible.
let routeAndSeparateSymbolWires (model: Model) (compId: ComponentId) =
    let wires = filterWiresByCompMoved model [compId]
    printfn "Routing and separating symbol wires:\n\
        %d inputs, %d outputs, %d both" wires.Inputs.Length wires.Outputs.Length wires.Both.Length
    let newWires =
        model.Wires
        |> Map.toList
        |> List.map (fun (cId, wire) ->
            if List.contains cId wires.Both then // Update wires that are connected on both sides
                cId, (
                    updateWire model wire true 
                    |> fun wire -> updateWire model wire false)
            elif List.contains cId wires.Inputs then 
                cId, updateWire model wire true
            elif List.contains cId wires.Outputs then
                cId, updateWire model wire false
            else cId, wire)
        |> Map.ofList
    { model with Wires = newWires }
    |> updateWireSegmentJumpsAndSeparations (Map.keys newWires |> Seq.toList)

/// all wires from comps have all segments made auto.
/// then the separation logic is rerun on these wires
let reSeparateWiresFrom (comps: ComponentId list) (model: Model) =
    printfn "reseparating wires"
    let wires' =
        getConnectedWires model comps
        |> List.collect (fun w -> Option.toList (resetWireToAutoKeepingPositionOpt w))
        |> (fun wires -> model.Wires, wires)
        ||> List.fold (fun wMap wire -> Map.add wire.WId wire wMap)
    wires'
    |> Map.toList
    |> List.map (fun (wId, wire) -> wId)
    |> fun wires -> updateWireSegmentJumpsAndSeparations wires {model with Wires = wires'}

/// all wires from comps are autorouted from scratch
/// then the separation logic is rerun on these wires
let reRouteWiresFrom  (comps: ComponentId list) (model: Model) =
    printfn "reroute wires"
    let wires' =
        getConnectedWires model comps
        |> List.collect (fun w -> Option.toList (resetWireToAutoKeepingPositionOpt w))
        |> (fun wires -> model.Wires, wires)
        ||> List.fold (fun wMap wire -> Map.add wire.WId wire wMap)
    let model = {model with Wires = wires'}
    (model, wires')
    ||> Map.fold (fun model wid wire -> Optic.map (wireOf_ wid) (smartAutoroute model) model)
    |> fun model -> updateWireSegmentJumpsAndSeparations (wires' |> Map.keys |> Seq.toList) model


