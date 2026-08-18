module BusWireSeparate
open EEExtensions
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
    | Horizontal -> [ tl.Y, tl.X, tl.X + box.W, BARRIERNEG; tl.Y + box.H, tl.X, tl.X + box.W, BARRIERPOS ]
    | Vertical -> [ tl.X, tl.Y, tl.Y + box.H, BARRIERNEG; tl.X + box.W, tl.Y, tl.Y + box.H, BARRIERPOS ]
    |> List.map (fun (p, minB, maxB, bType) ->
        {   P = p
            B =
              { MinB = minB + smallOffset
                MaxB = maxB - smallOffset }
            Orientation = ori
            LType = bType
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
        |> linkSameNetLines sameNetTrunkCapture

    /// Lines coming from the bounding boxes of symbols
    let symLines =
        model.Symbol.Symbols
        |> Map.toList
        |> List.collect (fun (_, sym) ->
            if sym.Annotation = None then
                Symbol.getSymbolBoundingBox sym |> bBoxToLines ori
            else
                [])
    // A TOTAL order, not just by P. Lines at the same P are the norm rather than the exception -
    // two same-net segments leaving one port start life exactly on top of each other - and every
    // pass below reads this array by index: `Lid` is assigned from the position, clusters grow by
    // walking neighbours, and the ordering that decides crossings breaks its ties by arrival. So
    // which of two coincident lines comes first is not a detail, it decides where wires end up.
    //
    // Sorting by P alone left that to whatever order the lines happened to be generated in, and
    // that order is NOT the same under .NET and under Fable. The same sheet then separated one way
    // in the tests and another in the application: on the eep1 TEST1/TEST2 pair, .NET drew the
    // two-wire net as one trunk while the app split it into a loop, from identical routing. A
    // measurement that does not describe what ships is worse than no measurement.
    symLines @ segLines
    |> List.toArray
    |> Array.sortBy (fun line -> line.P, line.B.MinB, line.B.MaxB, connectionIdStr line.Wid)
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
    | _ ->
        // Whether two wires cross depends on which of the two is placed first and on nothing else,
        // so a cluster's crossings are the sum over its pairs, and numCrossingsSign is one term of
        // that sum: which way round this pair is cheaper, or zero where the pair costs the same
        // either way. A pair whose spans nest is the second kind, and in a bundle fanning out from
        // one place most pairs nest - 22 of the 36 pairs in one cluster on reg16x8 - so what comes
        // back is a partial order, definite about some pairs and genuinely indifferent about the
        // rest.
        //
        // Treat it as one. Complete it to a partial order, take the longest chain in it, and place
        // that chain's minimal element: nothing in the cluster can go before it, since a chain of
        // that length hangs off it. Remove it and repeat. A segment the relation is silent about
        // sits in no long chain and falls out wherever its own preferences allow, which is the
        // right answer to a genuine indifference.
        //
        // This was a bubble sort over ADJACENT pairs, which cannot carry a segment past a run of
        // segments it ties with to reach the one segment that does have an opinion about it - so
        // every tie was settled by wherever routing happened to leave things.
        let before =
            segL
            |> List.map (fun a ->
                a, segL |> List.filter (fun b -> b <> a && numCrossingsSign' lines[a] lines[b] < 0) |> Set.ofList)
            |> Map.ofList

        /// the partial order completion: a before b whenever a chain of preferences says so
        let rec complete (order: Map<int, Set<int>>) =
            let joined =
                order |> Map.map (fun _ afters -> Set.fold (fun acc b -> Set.union acc order[b]) afters afters)
            if joined = order then order else complete joined
        let order = complete before

        /// Length of the longest chain starting at each segment still to be placed. Relaxed rather
        /// than recursed so that a cycle of preferences - which the relation does not forbid -
        /// stops at a bound instead of never returning.
        let chainLengths (remaining: Set<int>) =
            let step (lengths: Map<int, int>) =
                lengths
                |> Map.map (fun seg _ ->
                    match Set.intersect order[seg] remaining |> Set.toList with
                    | [] -> 1
                    | afters -> 1 + (afters |> List.map (fun a -> lengths[a]) |> List.max))
            (remaining |> Set.toList |> List.map (fun seg -> seg, 1) |> Map.ofList, [ 1 .. remaining.Count ])
            ||> List.fold (fun lengths _ -> step lengths)

        let rec place remaining placed =
            if Set.isEmpty remaining then List.rev placed
            else
                let lengths = chainLengths remaining
                // the minimal element of the longest chain. List.maxBy keeps the first of equals,
                // so segments in no chain of their own keep the order the cluster arrived in.
                let next = segL |> List.filter remaining.Contains |> List.maxBy (fun seg -> lengths[seg])
                place (Set.remove next remaining) (next :: placed)

        place (Set.ofList segL) []
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
            | BARRIERPOS | BARRIERNEG | FIXEDMANUALSEG | FIXEDSEG ->
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

/// Report the rare case of a segment getting "orphaned" - left out of every cluster.
/// This should probably never happen, and should be fixed if it is ever seen.
///
/// Once per key, not once per occurrence: this is reached from the separation scan, which runs on
/// every symbol move, so an unconditional complaint here would arrive at drag rate.
let warnLostSegmentInCluster (msg: string) (lines: Line array) (lostIndex: int) (loc: Cluster) =
    Log.warnOnce msg $"{msg} (segment index {lostIndex}, cluster of {loc.Segments.Length})"

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
                    warnLostSegmentInCluster msg lines nextIndex loc
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
                                Log.warnOnce "cluster-lost-segment" "a segment was lost from a wire cluster during separation, and was repaired"

                            { loc1 with
                                Segments = loc1LostSegs
                                UpperFix = lowerFix
                                // NB the bound must be recomputed. loc1.Bound is the union over
                                // loc1, and the segments loc2 has taken are no longer here - but a
                                // union is as wide as its widest member, so keeping it lets a
                                // barrier which only ever obstructed a segment now in loc2 stop
                                // the search for this cluster, at which point every segment below
                                // that barrier is dropped. That is where "nextIndex has got lost"
                                // came from: a symbol edge spanning x=70..90 halting a cluster
                                // whose every segment starts beyond x=186.
                                Bound = loc1LostSegs |> List.map (fun i -> lines[i].B) |> List.reduce boundUnion }
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
        spreadFromMiddle ((bMax + bMin) / 2.) ((bMax - bMin) / (float n + 1.))
    | _, Some bMin, _ when bMin + maxSep > idealStart ->
        spreadFromStart (bMin + maxSep) maxSep
    | Some bMax, _, n when bMax - maxSep < idealEnd ->
        spreadFromEnd (bMax - maxSep) maxSep
    | bMax, bMin, n ->
        spreadFromMiddle idealMidpoint maxSep


/// Given a list of segment changes of given orientation apply them to the model.
/// Also returns whether any segment actually moved: a settling round which moved nothing needs no
/// further examination, and that is the common case after a drag.
let adjustSegmentsInModel
        (ori: Orientation)
        (model: Model)
        (lines: Line list)
            : bool * Model =
    lines
    |> List.iter (fun line ->
            (line.SameNetLink |> List.iter (fun line2 -> line2.P <- line.P)))
    let lines = lines |> List.filter (fun line -> line.LType <> BARRIERPOS && line.LType <> BARRIERNEG)
    /// where the segment is now: Seg1 holds the position it had when the line was made, and
    /// clustering changes only Line.P
    let positionNow (line: Line) =
        match line.Seg1 with
        | None -> line.P
        | Some seg -> match ori with | Horizontal -> seg.Start.Y | Vertical -> seg.Start.X
    let wires, moved =
        ((model.Wires, false), lines)
        ||> List.fold (fun (wires, moved) line ->
            moveLine ori line.P line wires, moved || line.P <> positionNow line)

    moved, Optic.set wires_ wires model

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
                // NB the offset must be bracketed: function application binds tighter than *, so
                // an unbracketed `2*maxSegmentSeparation` here passed 2 and scaled the result by 30.
                let maxOffset = 2. * maxSegmentSeparation
                let space1 = getSpacefromLine allLines line1 line2 maxOffset
                let space2 = getSpacefromLine allLines line2 line1 maxOffset
                // space is signed: negative means the room is in the negative P direction, which
                // is room. It is the magnitude that says whether there is anywhere to go.
                if abs space1 < overlapTolerance && abs space2 < overlapTolerance then
                    Log.warnOnce "no-space-for-overlap" "no space to shift a fixed segment out of an overlap"
                if abs space1 > abs space2 then
                    line1.P <- line1.P + space1 * 0.5
                else
                    line2.P <- line2.P + space2 * 0.5)))
    allLines
    |> Array.toList
    |> adjustSegmentsInModel ori model
    |> snd


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
///
/// The search narrows towards `lines[below].P < p <= lines[above].P`, so the bottom end has to be
/// checked before it starts: with below = 0 taken on trust, an array whose first line is already
/// at or above p returns 1 and the caller never looks at line 0.
/// If every line is below p the last index is returned, which callers detect for themselves.
let findInterval (lines: Line array) ( p: float): int =
    let rec find above below =
        if above - below < 2 then above
        else
            let mid = (above + below) / 2
            if lines[mid].P < p then
                find above mid
            else
                find mid below
    if lines.Length = 0 || lines[0].P >= p then
        0
    else
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
    /// lines are sorted by P, which for the crossing lines is the coordinate the extension runs
    /// along. So the scan stops once it is past the far end of the extension: comparing a line's
    /// own P with its own B (as this used to) compares two different axes and stops arbitrarily.
    let rec check i =
        if i >= lines.Length || i < 0 then
            true
        else
            let otherLine = lines[i]
            if otherLine.P > b.MaxB + overlap then
                true
            else
                let otherB = otherLine.B
                if otherLine.Wid = excludedWire || otherB.MinB > p || otherB.MaxB < p || not (otherLine.LType = BARRIERPOS || otherLine.LType = BARRIERNEG) then
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
/// startShift is how far the start of the segment itself moves along its own axis: zero when the
/// segment keeps its start point, negative when it is extended backwards (as the second segment of
/// a removed corner is, since the segment before it has gone).
let isSegmentExtensionOk
        (info: LineInfo)
        (wire: Wire)
        (segNum: int)
        (ori: Orientation)
        (startShift: float)
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
    /// check there is room for the proposed segment extension.
    /// NB both bounds must be bracketed: `min startC startC+newLength` parses as
    /// `(min startC startC) + newLength`, which collapses the interval to a point.
    let newStartC = startC + startShift
    let extension =
        {   ExtP = p
            ExtOri = ori
            ExtB =
              { MinB = min newStartC (newStartC + newLength)
                MaxB = max newStartC (newStartC + newLength) } }
    // a zero-length segment means the two segments on either side of it are parallel and may overlap.
    // if we change the length of a segment next to a zero-length segment we must ensure that it does not double back on itself.
    // usually that will mean coming thr wrong wau out of a component edge (inside the component)!
    if segNum = 2 && segs[1].IsZero && sign segs[0].Length <> sign newLength ||
       segNum = segs.Length - 3 && segs[segs.Length-2].IsZero && sign segs[segs.Length-1].Length <> sign newLength
    then
        false // in this case a segment must backtrack from a nub - a bad idea
    elif segNum = 0 && sign len <> sign newLength
    then
        false // in this case a segment must backtrack from a port - a bad idea
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
                // removeCorner adds deletedSeg2 to the start segment and deletedSeg1 to the end
                // segment. These must be the lengths checked, or the check is of a wire that will
                // never exist. The end segment also starts deletedSeg1 earlier than it does now,
                // since the segment which used to get it there is one of the two being deleted.
                let newLength1 = seg.Length + deletedSeg2.Length
                let newLength2 = segs[start+3].Length + deletedSeg1.Length
                let endSegStartShift = -deletedSeg1.Length
                if isSegmentExtensionOk info wire start startSegOrientation 0. newLength1 &&
                    isSegmentExtensionOk info wire (start+3) (switchOrientation startSegOrientation)
                        endSegStartShift newLength2
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
        |> Map.valuesL
        |> List.collect (findWireCorner info maxCornerSize)
    (info, corners)
    ||> List.fold removeCorner
    |> (fun info' -> Optic.set wires_ info'.WireMap model)       
    
/// Return None, or Some wire' where wire' is wire with spikes removed.
/// Spikes segments that turn back on previous ones (with a zero-length segment in between).
/// Optimised for the case that there are no spikes and None is returned.
let removeWireSpikes (wire: Wire) : Wire option =
    /// Scan for a spike at index n of segs, and rescan from n after removing one: the merged
    /// segment can spike against what followed the pair just removed.
    ///
    /// This was a fold over the original segment list whose index was used to look into the
    /// rebuilt one. That works - the two lists agree below the removal, and the fold's own guard
    /// skips the steps that run off the shortened end - but it is only readable if you check
    /// that, and it is the one window at the removal which it never looks at again.
    let rec removeFrom (n: int) (removedAny: bool) (segs: Segment list) : Segment list option =
        let nSeg = segs.Length
        if n > nSeg - 3 then
            if removedAny then Some segs else None
        elif not segs[n+1].IsZero || sign segs[n].Length = sign segs[n+2].Length then
            removeFrom (n+1) removedAny segs
        else
            let newSegN = {segs[n] with Length = segs[n].Length + segs[n+2].Length}
            let lastSegs = segs[n+3..nSeg-1]
            [
                segs[0..n-1]
                [newSegN]
                (List.mapi (fun i seg -> {seg with Index = i + n + 1}) lastSegs)
            ]
            |> List.concat
            |> removeFrom n true
    removeFrom 0 false wire.Segments
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

/// A drawn segment reduced to one dimension for costing: which way it runs (0 horizontal, 1
/// vertical), the coordinate perpendicular to it, the interval it covers along itself, and the
/// net that drew it.
type private DrawnSeg =
    { Ori: int
      P: float
      Lo: float
      Hi: float
      Net: OutputPortId }

/// State of the sweep in wiringCost. Segments arrive sorted by direction, then by perpendicular
/// coordinate, then along themselves, so every interval that could merge with the one being
/// tracked arrives while it is still open.
type private CostSweep =
    { /// wire drawn: closed runs, each net's own overlaps already merged away
      Drawn: float
      /// the same counting all nets together, so Drawn - Covered is what two nets share
      Covered: float
      /// direction and perpendicular coordinate of the line being swept
      Line: (int * float) option
      /// open run over every net on this line
      AllNets: (float * float) option
      /// open run for each net on this line - at most a handful, so a list beats a map
      ByNet: (OutputPortId * float * float) list }

/// How bad a wiring is: the length of wire actually drawn, plus a heavy penalty for two different
/// nets drawn on top of each other.
///
/// Wire drawn is the length of the UNION of the segments on each line of the drawing, so two
/// segments of one net lying on top of each other are one wire and are counted once. That makes
/// "keep wires short" and "let a net share a trunk" the same objective rather than two which have
/// to be traded off by hand.
///
/// This is called once per settling round, so it is one sort and one sweep. Deliberately not built
/// on makeLines, which links same-net lines pairwise and costs as much as a separation pass.
let wiringCost (model: Model) : float =
    let drawn =
        model.Wires
        |> Map.toArray
        |> Array.collect (fun (_, wire) ->
            getFilteredAbsSegments (fun _ seg -> not seg.IsZero) wire
            |> List.map (fun aSeg ->
                match aSeg.Orientation with
                | Horizontal ->
                    { Ori = 0; P = aSeg.Start.Y; Net = wire.OutputPort
                      Lo = min aSeg.Start.X aSeg.End.X; Hi = max aSeg.Start.X aSeg.End.X }
                | Vertical ->
                    { Ori = 1; P = aSeg.Start.X; Net = wire.OutputPort
                      Lo = min aSeg.Start.Y aSeg.End.Y; Hi = max aSeg.Start.Y aSeg.End.Y })
            |> Array.ofList)
    Array.sortInPlaceBy (fun s -> s.Ori, s.P, s.Lo) drawn

    /// close every open run: their lengths are now known
    let flush (state: CostSweep) =
        { state with
            Drawn = state.Drawn + (state.ByNet |> List.sumBy (fun (_, lo, hi) -> hi - lo))
            Covered = state.Covered + (match state.AllNets with Some (lo, hi) -> hi - lo | None -> 0.)
            Line = None
            AllNets = None
            ByNet = [] }

    let swept =
        (   { Drawn = 0.; Covered = 0.; Line = None; AllNets = None; ByNet = [] }, drawn)
        ||> Array.fold (fun state seg ->
            // a new line of the drawing starts where the perpendicular gap opens
            let state =
                match state.Line with
                | Some (ori, p) when ori = seg.Ori && abs (seg.P - p) < overlapTolerance -> state
                | _ -> { flush state with Line = Some(seg.Ori, seg.P) }
            let allNets, coveredNow =
                match state.AllNets with
                | Some (lo, hi) when seg.Lo <= hi -> Some(lo, max hi seg.Hi), 0.
                | Some (lo, hi) -> Some(seg.Lo, seg.Hi), hi - lo
                | None -> Some(seg.Lo, seg.Hi), 0.
            let byNet, drawnNow =
                match state.ByNet |> List.tryFind (fun (net, _, _) -> net = seg.Net) with
                | Some ((_, lo, hi) as run) when seg.Lo <= hi ->
                    (seg.Net, lo, max hi seg.Hi) :: List.except [ run ] state.ByNet, 0.
                | Some ((_, lo, hi) as run) ->
                    (seg.Net, seg.Lo, seg.Hi) :: List.except [ run ] state.ByNet, hi - lo
                | None -> (seg.Net, seg.Lo, seg.Hi) :: state.ByNet, 0.
            { state with
                Drawn = state.Drawn + drawnNow
                Covered = state.Covered + coveredNow
                AllNets = allNets
                ByNet = byNet })
        |> flush

    swept.Drawn + overlapCostWeight * (swept.Drawn - swept.Covered)

/// Perform complete segment ordering and separation for segments of given orientation.
///
/// Every cluster on the sheet is separated, not only those holding a wire that has just changed.
/// Restricting it to those was worth doing while a separation pass could not be relied on to
/// return what it was given: re-separating a cluster nothing had touched could move it, so the
/// drawing shifted about under wires the user had not gone near. The settling loop in
/// `separateAndOrderModelSegments` makes the pass idempotent - a round which cannot show it
/// improved the sheet is discarded - so a cluster that is already settled costs a little time and
/// changes nothing, while one that is not gets the adjustment it was owed. A drag which frees up
/// space for wires elsewhere on the sheet now has that space taken up.
let separateModelSegmentsOneOrientation (ori: Orientation) (model: Model) =
    let lines = makeLines (Map.keysL model.Wires) ori model

    makeClusters lines
    |> List.iter (calcSegPositions model lines)

    lines
    |> Array.toList
    |> adjustSegmentsInModel ori model

/// Perform complete wire segment separation and ordering for all orientations.
///
/// `changedWires` says only whether there is anything to do: an empty list means nothing has
/// moved, and the whole pass is skipped. What is separated is the whole design, every wire of it.
/// A wire that has not changed can still be in the way of one that has, or be sitting where a wire
/// that has changed no longer is, and the pass is idempotent - so re-separating a part of the
/// sheet nothing touched costs a little time and leaves it as it was.
let separateAndOrderModelSegments (changedWires: ConnectionId list) (model: Model) : Model =
        if changedWires = [] then
            model // do nothing
        else
            let allWires = Map.keysL model.Wires

            /// convenience abbreviation
            let separate = separateModelSegmentsOneOrientation

            // Horizontal and vertical segments are separated independently, which is what makes
            // this fast: each pass is a one-dimensional problem. The two are not independent
            // though - where a horizontal segment can go depends on where the vertical ones it
            // joins ended up - so the passes alternate, and repetition resolves most of that.
            //
            // Not all of it. Some pairs of decisions are mutually exclusive: doing what the
            // horizontal pass wants means undoing what the vertical pass did, and the other way
            // about. Alternating then oscillates instead of converging, and a fixed number of
            // passes lands on whichever phase the count happens to end on - so the wiring a user
            // is left with would depend on how many passes there were. `wrappedArrays` in
            // WireQuality.fs is such a sheet: an array of ports facing an array of ports on the
            // far side of a symbol, so the whole bundle has to turn back and go round it.
            //
            // So rounds are repeated only while the sheet is getting better, and a round which
            // does not improve it is discarded rather than kept. Two consequences, both wanted:
            // an oscillation resolves to its better phase instead of flipping for ever, and the
            // whole pass is idempotent - it returns what it was given unless it can show it
            // improved it, so running it again changes nothing. Sheets which settle (nearly all
            // of them) now cost two passes rather than five.
            let rec settle roundsLeft (best: Model) (bestCost: float) (model: Model) =
                if roundsLeft <= 0 then
                    best
                else
                    let movedH, afterH = separate Horizontal model
                    let movedV, next = separate Vertical afterH
                    if not (movedH || movedV) then
                        next // a fixed point, reached without having to cost anything
                    else
                        let cost = wiringCost next
                        if cost < bestCost - settlingTolerance then
                            settle (roundsLeft - 1) next cost next
                        else
                            best // the two directions are fighting: keep the best seen

            settle maxSettlingRounds model (wiringCost model) model

            // after normal separation there may be "fixed" segments which should be separated because they overlap
            // one run for Vert and then Horiz segments is enough for this
            // TODO - include a comprehensive check for any remaining overlapping wires after this - and fix them
            |> separateFixedSegments allWires Horizontal
            |> separateFixedSegments allWires Vertical

            // after the previous two phases there may be artifacts where wires have an unnecessary number of corners.
            // this code attempts to remove such corners if it can be done while keeping routing ok

            |> removeModelCorners allWires // code to clean up some non-optimal routing


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
    Log.dbg Log.Wire $"routing and separating symbol wires: {wires.Inputs.Length} inputs, \
                       {wires.Outputs.Length} outputs, {wires.Both.Length} both"
    // A wire with both ends on the moved symbol is re-routed from each end in turn rather than
    // translated: the symbol may have been resized or rotated, not just moved.
    model
    |> rerouteMovedWires
        (fun model wire -> updateWire model wire true |> fun wire -> updateWire model wire false)
        [ compId ]
    |> fun model -> updateWireSegmentJumpsAndSeparations (Map.keysL model.Wires) model

/// Take the routing off the wires `toRedraw` selects, route them all again from nothing, and then
/// separate the whole sheet as usual. Neither pass is changed: this is the ordinary pair of them,
/// applied to many wires at once instead of to the few a drag reaches. That is what makes it worth
/// having - a sheet laid out before a routing change keeps most of its old routing, and so says
/// almost nothing about whether the change helped.
///
/// The routing comes off every one of them BEFORE any of them is routed. Routing looks only at
/// symbols today, so that makes no difference yet; it will as soon as a wire is routed with any
/// regard for the wires already there, since a route which is about to be thrown away is not
/// something the next wire should be following.
///
/// Wires are reset rather than deleted and recreated: a wire is identified by the two ports it
/// joins, so deleting one would only lose its ConnectionId, which the saved file, undo and the
/// current selection all refer to.
///
/// They are routed shortest first, by the straight-line distance between the two ports. Order is
/// immaterial while routing considers only symbols, and this is the order to have when it stops
/// being: a short wire has the least freedom in where it can go, so it is the one that should
/// already be there when a longer wire of the same net is routed and looking for something to
/// join. The re-route this replaces went in Map order - by ConnectionId, which is a GUID, so in no
/// order at all and not the same one twice.
let redrawWires (toRedraw: Wire -> bool) (model: Model) : Model =
    let portDistance (wire: Wire) =
        let destPos, startPos =
            Symbol.getTwoPortLocations model.Symbol wire.InputPort wire.OutputPort
        euclideanDistance startPos destPos
    let toRoute =
        model.Wires
        |> Map.valuesL
        |> List.filter toRedraw
        |> List.sortBy portDistance
        |> List.map (fun wire -> wire.WId)
    let unrouted =
        (model, toRoute)
        ||> List.fold (fun model wid -> Optic.set (wireOf_ wid >-> segments_) [] model)
    (unrouted, toRoute)
    ||> List.fold (fun model wid ->
            Optic.set (wireOf_ wid) (smartAutoroute model model.Wires[wid]) model)
    |> (fun model -> updateWireSegmentJumpsAndSeparations (Map.keysL model.Wires) model)

/// Redraw every wire the user has not routed by hand, leaving those alone.
let redrawFloatingWires (model: Model) =
    redrawWires (BusWireUpdateHelpers.isManuallyRouted >> not) model

/// Redraw every wire, hand routing included.
let redrawAllWires (model: Model) = redrawWires (fun _ -> true) model
