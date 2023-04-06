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
    let wireSeparationFromSymbol = 7. // must be smaller than Buswire.nubLength
    let maxCallsToShiftHorizontalSeg = 5
    /// Must be smaller than Buswire.nubLength
    let minWireSeparation = 7.
    let smallOffset = 0.0001
    let minSegmentSeparation = 12.
    let maxSegmentSeparation = 15.
    /// lines within this distance of each other are considered to overlap
    let overlapTolerance = 2.

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




module SeparateSegments =

    //*****************************************************************************************************//
    //---------------------------------Smart Channel / Segment Order / Separate----------------------------//
    //*****************************************************************************************************//

    (*-----------------------------------------------------------------------------------------------------
     this code implements a sheet beautify function that is designed to be called at the end of a symbol drag, 
     wire creation, etc, after smart autoroute. Therefore it has time to analyse the whole circuit and make changes. 
 
     Currently implements:
     - spread out overlapping wire segments
     - order wire segments to minimise crossings
     - order wire segments to minimise overlaps
     - allow same-net segments to overlap

     Does not implement:
     - re-order ports on custom components, or flip other components. That would be an obvious and quite easy 
       extension.
     *)

    open Constants // for easy access to SmartWire Constant definitions

    //-------------------------------------------------------------------------------------------------//
    //------------------------TYPES USED INTERNALLY FOR SEPARATION AND ORDERING------------------------//
    //-------------------------------------------------------------------------------------------------//

    /// Used to capture the 1D coordinates of the two ends of a line. (see Line).
    type Bound = { MinB: float; MaxB: float }

    
    type LineId = LineId of int
    with member this.Index = match this with | LineId i -> i

    type LType = 
        /// a non-segment fixed (symbol boundary) barrier
        | FIXED  
        /// a movable line segment
        | NORMSEG 
        /// a segment which is a fixed barrier in clustering but can change after.
        | FIXEDSEG 
        /// a fixed segment which has been manually routed and can never move
        | FIXEDMANUALSEG
        /// a segment linked to another on the same net which is not clustered
        | LINKEDSEG

    /// Used to represent a line on the canvas, e.g. a wire segment or symbol edge.
    /// The array of lines will all have the same orientation - so optimisation is done in two phases
    /// for vertical and horizontal segments.
    type Line =
        { P: float // the coordinate X or Y perpendicular to the line.
          B: Bound // the two "other" coordinates
          Seg1: ASegment option // if the line comes from a wire segment this references the segment and wire
          LType: LType
          SameNetLink: Line list
          Wid: ConnectionId
          PortId: OutputPortId
          Lid: LineId } // index in lines array of this Line.


    /// Used to cluster together overlapping and adjacent lines into a group that
    /// can be spread out. This is the parameter in a tail recursion used to do the clustering
    type Cluster =
        { UpperFix: float option // if clustering is stopped by a barrier
          LowerFix: float option // if clustering is stopped by a barrier
          Segments: int list // list of movable lines found (which will be spread out)
          Bound: Bound } // union of bounds of all segments found so far

    /// Controls direction of Cluster search in expandCluster.
    /// Search is upwards first and then downwards so downwards search takes a Cluster
    /// (generated from upwards search) as parameter.
    type LocSearchDir =
        | Upwards
        | Downwards of Cluster


    //-------------------------------------------------------------------------------------------------//
    //--------------------------------HELPERS USED IN CLUSTERING SEGMENTS------------------------------//
    //-------------------------------------------------------------------------------------------------//


    /// Get the segment indexes within a Cluster (loc)
    let inline segPL (lines: Line array) loc =
        loc.Segments |> (List.map (fun n -> lines[n].P))

    /// ideal (max) width of segments in loc
    let inline widthS (loc: Cluster) =
        float loc.Segments.Length * maxSegmentSeparation

    /// ideal upper bound in P direction of segments with P value in pts.
    let inline upperS pts =
        (List.min pts + List.max pts) / 2.
        + float pts.Length * maxSegmentSeparation / 2.

    /// ideal lower bound in P direction of segments with P value in pts
    let inline lowerS pts =
        (List.min pts + List.max pts) / 2.
        - float pts.Length * maxSegmentSeparation / 2.

    /// ideal upper bound in P direction of loc including possible fixed constraint
    let inline upperB (lines: Line array) (loc: Cluster) =
        let pts = segPL lines loc

        match loc.UpperFix, loc.LowerFix with
        | Some u, _ -> u
        | None, Some l when l > lowerS pts -> l + widthS loc
        | _ -> upperS pts

    /// ideal lower bound in P direction of loc including possible fixed constraint
    let inline lowerB (lines: Line array) loc =
        let pts = segPL lines loc

        match loc.UpperFix, loc.LowerFix with
        | _, Some l -> l
        | Some u, None when u < upperS pts -> u - widthS loc
        | _ -> lowerS pts

    //-------------------------------------------------------------------------------------------------//
    //--------------------------------LOW-LEVEL PRINTING (returns strings)-----------------------------//
    //-------------------------------------------------------------------------------------------------//


    let pOpt (x: 'a option) = match x with | None -> "None" | Some x -> $"^{x}^"

    let pLineType (line:Line) = $"{line.LType}"

    let pLine (line:Line) = 
        $"|L{line.Lid.Index}.P=%.0f{line.P}.{pLineType line}:B=%.0f{line.B.MinB}-%.0f{line.B.MaxB}|"

    let pLines (lineA: Line array) =
        $"""{lineA |> Array.map (fun line -> pLine line) |> String.concat "\n"}"""

    let pCluster (loc:Cluster) =
        $"Cluster:<{pOpt loc.LowerFix}-{loc.Segments}-{pOpt loc.UpperFix}>"

    let pAllCluster (lines: Line array) (loc:Cluster) =
        $"""Cluster:<L={pOpt loc.LowerFix}-{loc.Segments |> List.map (fun n -> pLine lines[n]) |> String.concat ","}-U={pOpt loc.UpperFix}>"""


    //-------------------------------------------------------------------------------------------------//
    //-----------------------------------------UTILITY FUNCTIONS---------------------------------------//
    //-------------------------------------------------------------------------------------------------//

    let rec tryFindIndexInArray (searchStart: LineId) (dir: int) (predicate: 'T -> bool) (giveUp: 'T -> bool) (arr: 'T array) =
        if searchStart.Index < 0 || searchStart.Index > arr.Length - 1 then
            None
        else
            match predicate arr[searchStart.Index], giveUp arr[searchStart.Index] with
            | _, true -> None
            | true, _ -> Some searchStart
            | false, _ -> tryFindIndexInArray (LineId(searchStart.Index + dir)) dir predicate giveUp arr
            

    /// true if bounds b1 and b2 overlap or are exactly adjacent
    let hasOverlap (b1: Bound) (b2: Bound) =
        inMiddleOrEndOf b1.MinB b2.MinB b1.MaxB
        || inMiddleOrEndOf b1.MinB b2.MaxB b1.MinB
        || inMiddleOrEndOf b2.MinB b1.MinB b2.MaxB

    /// Union of two bounds b1 and b2. b1 & b2 must overlap or be adjacent,
    /// otherwise the inclusive interval containing b1 and b2 is returned.
    let boundUnion (b1: Bound) (b2: Bound) =
        { MinB = min b1.MinB b2.MinB
          MaxB = max b1.MaxB b2.MaxB }


    /// Move segment by amount posDelta in direction perpendicular to segment - + => X or y increases.
    /// movement is by changing lengths of two segments on either side.
    /// will fail if called to change a nub at either end of a wire (nubs cannot move).
    let moveSegment (index: int) (posDelta: float) (wire: Wire) =
        let segs = wire.Segments

        if index < 1 || index > segs.Length - 2 then
            failwithf $"What? trying to move segment {index} of a wire length {segs.Length}"

        { wire with
            Segments =
                segs
                |> List.updateAt (index - 1) { segs[index - 1] with Length = segs[index - 1].Length + posDelta }
                |> List.updateAt (index + 1) { segs[index + 1] with Length = segs[index + 1].Length - posDelta } }

    /// Change wires to move a wire segment represented by line to the given new value of P coordinate.
    /// P is X or Y according to ori.
    let moveLine (ori: Orientation) (newP: float) (line: Line) (wires: Map<ConnectionId, Wire>) =
        match line.Seg1 with
        | None -> failwithf "Can't move Line {line} - it is not a segment"
        | Some seg ->
            let oldP =
                match ori with
                | Horizontal -> seg.Start.Y
                | Vertical -> seg.Start.X

            let segIndex = seg.Segment.Index
            let wid = seg.Segment.WireId
            let updateWire = Option.map (moveSegment segIndex (newP - oldP))
            Map.change seg.Segment.WireId updateWire wires

    //-------------------------------------------------------------------------------------------------//
    //---------------------------------LINE ARRAY CREATION FROM MODEL----------------------------------//
    //-------------------------------------------------------------------------------------------------//

    /// return wire and segment index of line, if it is a segment
    let lineToWire (model: Model) (line:Line)  : (Wire * int) option =
        match line.Seg1 with
        | Some seg ->
            let (int,wid) = seg.Segment.GetId()
            let wire = model.Wires[wid]
            Some (wire,int)
        | None -> None
        

    /// Convert a segment into a fixed or movable line (of given orientation).
    let segmentToLine (lType: LType) (ori: Orientation) (wire:Wire) (seg: ASegment) : Line =
        let order a b =
            if a < b then
                { MinB = a; MaxB = b }
            else
                { MinB = b; MaxB = a }

        let line: Line =
            { P = seg.Start.Y
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
            {line with 
                P = seg.Start.X; 
                B = order seg.Start.Y seg.End.Y}

    /// Convert a symbol BoundingBox into two fixed lines (of given orientation).
    let bBoxToLines (ori: Orientation) (box: BoundingBox) : Line list =
        let tl = box.TopLeft

        match ori with
        | Horizontal -> [ tl.Y, tl.X, tl.X + box.W; tl.Y + box.H, tl.X, tl.X + box.W ]
        | Vertical -> [ tl.X, tl.Y, tl.Y + box.H; tl.X + box.W, tl.Y, tl.Y + box.H ]
        |> List.map (fun (p, minB, maxB) ->
            { P = p
              B =
                { MinB = minB + smallOffset
                  MaxB = maxB - smallOffset }
              LType = FIXED
              Seg1 = None
              SameNetLink = []
              Wid = ConnectionId ""
              PortId = OutputPortId ""
              Lid = LineId 0 })

    /// Where two segments are on the same Net and on top of each other we muct NEVER separate them.
    /// This function links such segments, and marks all except the head one with ClusterSegment = false, 
    /// so that the clustering algorithm will ignore them.
    let linkSameNetLines (lines: Line list) : Line list =
        /// input: list of lines all in the same Net (same outputPort)
        /// output: similar list, with lines that are on top of each other and in different wires linked
        let linkSameNetGroup (lines: Line list) =
            let lines = List.toArray lines
            /// if needed, link lines[b] to lines[a] mutating elements in lines array for efficiency
            let tryToLink (a:int) (b:int) =
                let la, lb = lines[a], lines[b]
                if la.LType = NORMSEG && la.Wid <> lb.Wid && close la.P lb.P && hasOverlap la.B lb.B  then
                    lines[b] <- 
                        { lb with
                            LType = LINKEDSEG}
                    lines[a] <- 
                        { la with
                            B = boundUnion la.B lb.B;
                            SameNetLink = lines[b] :: lines[a].SameNetLink}                    
            // in this loop the first lines[a] in each linkable set links all the set, setting ClusterSegment = false
            // Linked lines are then skipped.
            for a in [0..lines.Length-1] do
                for b in [a+1..lines.Length-1] do
                    tryToLink a b
            Array.toList lines

        lines
        |> List.groupBy (fun line -> line.PortId)
        |> List.collect (fun (port, lines) -> linkSameNetGroup lines)

    /// Make all lines, fixed and movable, of given orientation from wires and symbols in Model
    /// ori - orientation of Lines (P coord is reverse of this)
    let makeLines (ori: Orientation) (model: Model) =
        /// Which segments in wires are included as Lines?
        let selectSegments (ori: Orientation) (wire: Wire) (orient: Orientation) (seg: Segment) =
            let numSegs = wire.Segments.Length
            ori = orient && seg.Index <> 0 && seg.Index <> numSegs - 1 && not (seg.IsZero()) //|| (segN -1).IsZero() || (segN 1).IsZero())

        /// Lines coming from wire segments
        /// Manually routed segments are considered fixed
        /// Segments next to zero length segments are considered fixed
        /// (they form part of straight lines extending the fixed nub)
        let segLines =
            ([], model.Wires)
            ||> Map.fold (fun (lines: Line list) _ wire ->
                getFilteredAbsSegments (selectSegments ori wire) wire
                |> List.map (fun aSeg ->
                    let segs = wire.Segments
                    let seg = aSeg.Segment
                    let lType =
                        match seg.Mode, seg.Index=2, seg.Index=segs.Length-3 with
                        | Manual , _ , _ -> 
                            FIXEDMANUALSEG
                        | _ , true , _ when segs[ 1 ].IsZero() -> 
                            FIXEDSEG
                        | _ , _ , true when  segs[ segs.Length - 2 ].IsZero() -> 
                            FIXEDSEG
                        | _ -> 
                            NORMSEG
                    segmentToLine lType ori wire aSeg)
                |> (fun wireLines -> wireLines @ lines))
                |> linkSameNetLines




        /// Lines coming from the bounding boxes of symbols
        let symLines =
            model.Symbol.Symbols
            |> Map.toList
            |> List.collect (fun (_, sym) -> Symbol.getSymbolBoundingBox sym |> bBoxToLines ori)

        symLines @ segLines
        |> List.toArray
        |> Array.sortBy (fun line -> line.P)
        |> Array.mapi (fun i line -> { line with Lid = LineId i })
        //|>  (fun arr -> printf "%s" (pLines arr); arr)

    //-------------------------------------------------------------------------------------------------//
    //-----------------------------------------SEGMENT ORDERING----------------------------------------//
    //-------------------------------------------------------------------------------------------------//


    /// Returns integers +/- 1 indicating direction of wire leaving ends of line segment.
    /// Pair returned is MaxB, MinB end of line
    let inline turnDirs (line: Line) (wires: Map<ConnectionId, Wire>) =
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

    // The functions tests two segment ends - one from each wire - for whether the
    // segments connected to the ends (and therefore turning one direction or the other)
    // might overlap.
    /// Return +1. if two wires turn towards each other (and therefore might overlap), else -1.
    /// turnDir1, turnDir2 - direction in which turns go.
    /// bound1, bound2 - the MinB or MaxB bound of each end - which must be close.
    /// The P value of each segment.
    let inline linesMaybeMeeting
        ((turnDir1, bound1, p1): int * float * float)
        ((turnDir2, bound2, p2): int * float * float)
        : float =
        // if the two segment ends do not line up return 0.
        // and the two segments that join turn towards eachother
        match close bound1 bound2,  p1 > p2, turnDir1, turnDir2 with
        | false, _, _, _ -> 0.
        | _, true, -1, 1
        | _, false, 1, -1 -> 1.
        | _ -> -1.


    /// +1 if line1.P > line2.P for zero crossings.
    /// -1 if line1.P < line2.P for zero crossings.
    /// 0 if line1.P and line2.P have one crossing.
    let numCrossingsSignAndMaybeOverlaps (line1: Line) (line2: Line) (wires: Map<ConnectionId, Wire>) =
        let (max1, min1), (max2, min2) = turnDirs line1 wires, turnDirs line2 wires
        // if line1.P > line2.P then a +1 line1 turnDir or a -1 line2 turnDir from an inner endpoint
        // will NOT cause a crossing. -1 will cause a crossing.
        // The match sums the two inner turnDirs, inverting sign if they come from a line2
        // turning. Dividing this by 2 gives the required answer!
        // NB this is simpler than expected since it does not matter what order the two inner ends are
        // in - which makes identifying them (as one of the MaxB and one of the MinB ends) easier.
        let crossingsNumSign =
            match line1.B.MinB > line2.B.MinB, line1.B.MaxB < line2.B.MaxB with
            | true, true -> min1 + max1
            | true, false -> min1 - max2
            | false, true -> -min2 + max1
            | false, false -> -min2 + max2
            |> (fun n -> n / 2)
        // if two segment ends have the same Bound (MaxB or MinB) value and turn towards each other
        // still experimental (the negative weighting of this perhaps means it should be the otehr way round)?
        let maybeMeeting =
            linesMaybeMeeting (max1, line1.B.MaxB, line1.P) (max2, line2.B.MaxB, line2.P)
            + linesMaybeMeeting (max1, line1.B.MaxB, line1.P) (min2, line2.B.MinB, line2.P)
            + linesMaybeMeeting (min1, line1.B.MinB, line1.P) (max2, line2.B.MaxB, line2.P)
            + linesMaybeMeeting (min1, line1.B.MinB, line1.P) (min2, line2.B.MinB, line2.P)

        //printfn $"line1 = {line1.Index}, line2 = {line2.Index}. MaybeMeeting = {maybeMeeting}"
        float crossingsNumSign + maybeMeeting 

    /// segL is a list of lines array indexes representing segments found close together.
    /// Return the list ordered in such a way that wire crossings are minimised if the
    /// segments are placed as ordered. The return list is placed with P value increasing
    /// along the list.
    let orderToMinimiseCrossings (model: Model) (lines: Line array) (segL: int list) =
        // special case - included for efficency
        if segL.Length = 1 then
            segL
        else
            let wires = model.Wires
            let numSegments = segL.Length
            let segA = segL |> Array.ofList
            /// inverse of segA[index]: NB indexes [0..numSegmnets-1] are NOT Segment index.
            /// These indexes are used inside this function only to allow contiguous arrays
            /// to calculate the sort order
            let indexOf seg = Array.findIndex ((=) seg) segA
            // Map each index [0..numSegments-1] to a number that will determine its (optimal) ordering
            let sortOrderA =
                let arr = Array.create numSegments 0.

                for i in [ 0 .. numSegments - 1 ] do
                    for j in [ 0 .. i - 1 ] do
                        let num = numCrossingsSignAndMaybeOverlaps lines[segL[i]] lines[segL[j]] wires
                        arr[i] <- arr[i] + num
                        arr[j] <- arr[j] - num

                arr

            let sortFun i = sortOrderA[indexOf i]
            List.sortBy sortFun segL

    //-------------------------------------------------------------------------------------------------//
    //---------------------------------------SEGMENT CLUSTERING----------------------------------------//
    //-------------------------------------------------------------------------------------------------//

    /// When given a segment index search for nearby segments to be considered with it as a single cluster
    /// for spreading out. To be included segments must be close enough and overlapping. Search
    /// terminates given large gap or a fixed boundary segments are not allowed to move across.
    let expandCluster (groupableA: bool array) (index: int) (searchDir: LocSearchDir) (lines: Line array) =
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

            if (i < 0 || i >= lines.Length) then
                loc
            elif (not  (hasOverlap loc.Bound lines[i].B)) || (not groupableA[i] && (lines[i].LType <> FIXED)) then
                expand (nextIndex i) loc
            else
                let p = lines[i].P
                match lines[i].LType with
                | FIXED | FIXEDMANUALSEG | FIXEDSEG ->
                    let p = lines[i].P
                    match searchDir with
                    | Upwards -> { loc with UpperFix = Some p }
                    | _ -> { loc with LowerFix = Some p } // fixed boundary
                | _ when abs (p - searchStart) > maxSegmentSeparation * nSegs + smallOffset -> loc
                | _ ->
                    match lowestDownwardsIndex with
                    | Some index when i < index -> expand (nextIndex i) loc // past starting point, so can't add segments, but still check for a Fix
                    | _ ->
                        expand
                            (nextIndex i)
                            { loc with
                                Segments = i :: loc.Segments
                                Bound = boundUnion loc.Bound lines[i].B }

        expand (nextIndex index) initLoc

    /// Scan through segments in P order creating a list of local Clusters.
    /// Within one cluster segments are adjacent and overlapping. Note that
    /// different clusters may occupy the same P values if their segments do
    /// not overlap.
    /// Segments within each cluster will be repositioned and reordered after
    /// clusters are identified.
    /// Every segment must be part of a unique cluster.
    let makeClusters (lines: Line array) =
        /// true if corresponding line can be grouped in a cluster as a segment
        let groupableA =
            Array.init lines.Length (fun i ->lines[i].LType = NORMSEG)

        let groupable seg = groupableA[seg]
        let expandCluster = expandCluster groupableA

        let keepOnlyGroupableSegments (loc: Cluster) =
            { loc with Segments = List.filter groupable loc.Segments }

        let rec getClusters lines =
            match Array.tryFindIndex ((=) true) groupableA with
            | None -> []
            | Some nextIndex ->
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
                    |> (fun segs ->
                        if segs = [] then
                            [ loc2 ]
                        else
                            if not <| List.contains nextIndex segs then
                                failwithf "What? nextIndex has got lost from loc1!"

                            segs
                            |> (fun segs ->
                                { loc1 with
                                    Segments = segs
                                    UpperFix = lowerFix })
                            |> (fun loc -> expandCluster (List.max loc.Segments) (Downwards loc) lines)
                            |> (fun loc1 ->
                                (if not <| List.contains nextIndex loc1.Segments then
                                     failwithf "What? nextIndex has got lost from loc1 after expansion!")

                                loc1)
                            |> (fun loc1 -> [ loc2; loc1 ]))
                | _ ->
                    (if not <| List.contains nextIndex loc2.Segments then
                         failwithf "What? nextIndex has got lost from loc2!")

                    [ loc2 ]
                |> List.map keepOnlyGroupableSegments
                |> List.filter (fun loc -> loc.Segments <> [])
                |> (fun newLocs ->
                    newLocs
                    |> List.iter (fun loc -> 
                        //printf "%s" (pAllCluster lines loc)
                        loc.Segments |> List.iter (fun seg -> groupableA[seg] <- false))

                    if groupable nextIndex then
                        failwithf "Error: infinite loop detected in cluster find code"

                    newLocs @ getClusters lines)

        getClusters lines

    // Currently not used. Running the algorithm twice fixes problems otherwise needing merge (and other things).
    // Should decide what is an acceptable space between merged clusters so as not to move
    // segments too far.
    /// Return single cluster with segments from loc1 and loc2 merged
    let mergeLocs (lines: Line array) (loc1: Cluster) (loc2: Cluster) =
        if upperB lines loc1 < lowerB lines loc2 || not (hasOverlap loc1.Bound loc2.Bound) then
            [ loc1; loc2 ] // do not merge
        else
            // Bound and SearchStart fields are no longer used.
            // printf $"Merging:\n{pAllCluster lines loc1}\n{pAllCluster lines loc2}"

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

    /// Function which given a cluster (loc) works out how to
    /// spread out the contained segments optimally, spacing them from other segments and symbols.
    /// Return value is a list of segments, represented as Lines, paired with where they move.
    /// lines is the source list of lines (vertical or horizontal according to which is being processed).
    /// model is the Buswire model needed to access wires.
    let calcSegPositions model lines loc =
        let segs = loc.Segments |> List.distinct |> orderToMinimiseCrossings model lines
        // if segs.Length > 1 then
        // printfn $"** Grouping: {segs |> List.map (fun i -> i, lines[i].P)} **"
        let pts = segs |> List.map (fun i -> lines[i].P)
        let nSeg = loc.Segments.Length

        let spreadFromStart start sep =
            //printfn $"spread: %.2f{start}: %.2f{sep} {segs} {loc.UpperFix} {loc.LowerFix}"
            segs |> List.mapi (fun i seg -> lines[seg], start + sep * float i)

        let spreadFromMiddle mid sep =
            segs
            |> List.mapi (fun i seg -> lines[seg], mid + sep * float i - float (nSeg - 1) * sep / 2.)

        let spreadFromEnd endP sep =
            segs |> List.mapi (fun i seg -> lines[seg], endP + sep * float (i - (nSeg - 1)))

        let maxSep = maxSegmentSeparation
        let halfMaxSep = maxSegmentSeparation / 2.
        let idealMidpoint = (List.min pts + List.max pts) / 2.
        let halfIdealWidth = float (nSeg - 1) * halfMaxSep

        let idealStart, idealEnd =
            idealMidpoint - halfIdealWidth, idealMidpoint + halfIdealWidth
        // Fixed bounds and soft segment bounds behave differently
        // Segments are placed maxSegmentSeparation away from fixed bound but only halfSep away from soft bounds
        match loc.UpperFix, loc.LowerFix, nSeg with
        | None, None, 1 -> [] // no change
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
    let adjustSegmentsInModel (ori: Orientation) (model: Model) (changes: (Line * float) list) =
        let changes =
            changes 
            |> List.collect (fun (line, p) ->
                if line.SameNetLink = [] then 
                    [line,p]
                else
                    line.SameNetLink |> List.iteri (fun i lin -> printfn $"{line.Lid.Index}({i}): Linked net: {lin.Lid.Index},{lin.P} -> {p}")
                    [(line,p)] @ (line.SameNetLink |> List.map (fun line2 -> line2,p)))
        let wires =
            (model.Wires, changes)
            ||> List.fold (fun wires (line, newP) ->
                let seg = Option.get line.Seg1
                moveLine ori newP line wires)

        Optic.set wires_ wires model

    /// Segments which could be moved, but would make an extra segment if moved, are marked Fixed
    /// and not moved by the normal cluster-based separation functions.
    /// This function looks at these segments and moves them a little in the special case that they
    /// overlap. It is called after the main segment separation is complete.
    let separateFixedSegments (ori: Orientation) (model: Model) =
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

        makeLines ori model
        |> (fun lines -> 
            Array.pairwise lines
            |> Array.filter (fun (line1, line2) -> 
                    line1.LType = FIXEDSEG && line2.LType = FIXEDSEG &&
                    abs (line1.P - line2.P) < overlapTolerance &&
                    line1.PortId <> line2.PortId &&
                    hasOverlap line1.B line2.B)
            |> Array.map (fun (line1, line2) ->
                let space1 = getSpacefromLine lines line1 line2 2*maxSegmentSeparation
                let space2 = getSpacefromLine lines line2 line1 2*maxSegmentSeparation
                if abs space1 > abs space2 then
                    line1, line1.P + space1 * 0.5
                else
                    line2, line1.P + space2 * 0.5)
            |> List.ofArray)
        |> adjustSegmentsInModel ori model
                

    //-------------------------------------------------------------------------------------------------//
    //----------------------------------------TOP LEVEL FUNCTIONS--------------------------------------//
    //-------------------------------------------------------------------------------------------------//

    /// Perform complete segment ordering and separation for segments of given orientation.
    let separateModelSegmentsOneOrientation (ori: Orientation) (model: Model) =
        //printf $"{ori} Separation starting..."
        makeLines ori model
        |> fun lines ->
            makeClusters lines
            //|> mergeLocalities lines // merging does not seem necessary?
            |> List.collect (calcSegPositions model lines)
        |> adjustSegmentsInModel ori model

    /// Perform complete segment separation and ordering for all orientations
    let separateAndOrderModelSegments =
        separateModelSegmentsOneOrientation Vertical
        >> separateModelSegmentsOneOrientation Horizontal
        //>> separateFixedSegments Vertical 
        //>> separateFixedSegments Horizontal

        >> separateModelSegmentsOneOrientation Vertical // repeat vertical separation since moved segments may now group
        >> separateFixedSegments Vertical 
        >> separateModelSegmentsOneOrientation Horizontal // as above
        >> separateFixedSegments Horizontal

/// Top-level function to replace updateWireSegmentJumps
/// and call the new tidyup code as well. This should
/// run when significant circuit wiring chnages have been made
/// e.g. at the end of symbol drags.
let updateWireSegmentJumpsAndSeparations model =
    model
    |> SeparateSegments.separateAndOrderModelSegments
    |> (BusWireUpdateHelpers.updateWireSegmentJumps [])



//*****************************************************************************************************//
//*****************************************************************************************************//
//-----------------------------------------SmartWire---------------------------------------------------//
//*****************************************************************************************************//
//*****************************************************************************************************//

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
let findWireSymbolIntersections (model: Model) (wire: Wire) : BoundingBox list =
    let allSymbolBoundingBoxes =
        model.Symbol.Symbols
        |> Map.values
        |> Seq.toList
        |> List.map Symbol.getSymbolBoundingBox


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
        |> List.map (fun boundingBox ->
            //let isInputComp = inputCompId = string compID
            //let isOutputComp = outputCompId = string compID
            (*
            match true, isConnectedToSelf, isInputComp, isOutputComp, inputCompRotation with
            | false, true, true, _, n when n = Degree0 || n = Degree180 ->
                { W = boundingBox.W
                  H = boundingBox.H + Constants.minWireSeparation * 2.
                  TopLeft = boundingBox.TopLeft |> updatePos Up_ Constants.minWireSeparation }
            | false, true, true, _, n when n = Degree90 || n = Degree270 ->
                { W = boundingBox.W + Constants.minWireSeparation * 2.
                  H = boundingBox.H
                  TopLeft = boundingBox.TopLeft |> updatePos Left_ Constants.minWireSeparation }
            | false, false, true, _, _ -> boundingBox
            | false,  false, _, true, _ -> boundingBox
            | _ ->*)
            { W = boundingBox.W + Constants.minWireSeparation * 2.
              H = boundingBox.H + Constants.minWireSeparation * 2.
              TopLeft =
                boundingBox.TopLeft
                |> updatePos Left_ Constants.minWireSeparation
                |> updatePos Up_ Constants.minWireSeparation })
        |> List.filter (fun boundingBox ->
            match segmentIntersectsBoundingBox boundingBox startPos endPos with // do not consider the symbols that the wire is connected to
            | Some _ -> true // segment intersects bounding box
            | None -> false // no intersection
        )


    segVertices
    |> List.collect (fun (startPos, endPos) -> boxesIntersectedBySegment startPos endPos)
    |> List.distinct

//------------------------------------------------------------------------//
//--------------------------Shifting Vertical Segment---------------------//
//------------------------------------------------------------------------//

let changeSegment (segIndex: int) (newLength: float) (segments: Segment list) =
    List.updateAt segIndex { segments[segIndex] with Length = newLength } segments

/// Try shifting vertical seg to either - wireSeparationFromSymbol or + wireSeparationFromSymbol of intersected symbols.
/// Returns None if no route found.
let tryShiftVerticalSeg (model: Model) (intersectedBoxes: BoundingBox list) (wire: Wire) : Wire option =
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
            |> List.sortWith (fun box1 box2 ->
                let box1 = swapBB box1 wire.InitialOrientation
                let box2 = swapBB box2 wire.InitialOrientation

                match dir with
                | Left_ -> compare (box1.TopLeft.X) (box2.TopLeft.X)
                | Right_ -> compare (box2.TopLeft.X + box2.W) (box1.TopLeft.X + box1.W)
                | _ -> failwith "Invalid direction to shift wire")
            |> List.head


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

//***************************************************************************************************************//
//**************************************** V3 implementation ****************************************************
//***************************************************************************************************************//
module v3 =
    //-----------------------------------------------------------------------------------------------------------//
    //----------------------------------------------Normalisation------------------------------------------------//
    //-----------------------------------------------------------------------------------------------------------//

    let rotCW (pos: XYPos) = { X = pos.Y; Y = -pos.X }
    let rotACW (pos: XYPos) = { X = -pos.Y; Y = pos.X }

    let rotBox (rot: XYPos -> XYPos) (box: BoundingBox) =
        let pos = rot { X = box.W; Y = box.H }

        { TopLeft = rot box.TopLeft
          W = pos.X
          H = pos.Y }

    let rotBoxCW = rotBox rotCW
    let rotBoxACW = rotBox rotACW
    let negLength (seg: Segment) = { seg with Length = -seg.Length }

    let negateAlternateLengths negateFirst (seg: Segment) =
        match (seg.Index % 2 = 0) = negateFirst with
        | false -> seg
        | true -> negLength seg

    /// makes StartPos and Segments normalised, port positions are not normalised
    let normaliseWire (wire: Wire) =
        match wire.InitialOrientation with
        | Horizontal -> wire
        | Vertical ->
            { wire with
                Segments = wire.Segments |> List.map (negateAlternateLengths false)
                StartPos = rotACW wire.StartPos }

    /// reverses the effect of normaliseWire
    let deNormaliseWire (wire: Wire) =
        match wire.InitialOrientation with
        | Horizontal -> wire
        | Vertical ->
            { wire with
                Segments = wire.Segments |> List.map (negateAlternateLengths true)
                StartPos = rotCW wire.StartPos }

    //------------------------------------------------------------------------------------------------------------------------------//
    //---------------------------------------------------Implementation-------------------------------------------------------------//
    //------------------------------------------------------------------------------------------------------------------------------//
    // return max distance above or below
    let tryMaxDistance (distances: VertDistFromBoundingBox list) =
        match distances with
        | [] -> Above 0.
        | _ ->
            List.maxBy
                (function
                | Above d
                | Below d -> d)
                distances

    /// returns the maximum vertical distance of pos from intersectedBoxes as a VertDistFromBoundingBox or Above 0. if are no intersections
    let maxVertDistanceFromBox
        (intersectedBoxes: BoundingBox list)
        (wireOrientation: Orientation)
        (pos: XYPos)
        : VertDistFromBoundingBox =

        let isCloseToBoxHoriz (box: BoundingBox) (pos: XYPos) =
            inMiddleOrEndOf box.TopLeft.X pos.X (box.TopLeft.X + box.W)

        let getVertDistanceToBox (pos: XYPos) (box: BoundingBox) : VertDistFromBoundingBox list =
            (swapXY pos wireOrientation, swapBB box wireOrientation)
            ||> (fun pos box ->
                if isCloseToBoxHoriz box pos then
                    if pos.Y > box.TopLeft.Y then
                        [ pos.Y - box.TopLeft.Y |> Above ]
                    else
                        [ box.TopLeft.Y - pos.Y + box.H |> Below ]
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
    let rec tryShiftHorizontalSegAlt
        (callsLeft: int)
        (model: Model)
        (intersectedBoxes: BoundingBox list)
        (wire: Wire)
        : Wire option =
        match callsLeft with
        | 0 -> None
        | n ->
            let tryShiftHorizontalSegAlt = tryShiftHorizontalSegAlt (n - 1)

            let currentStartPos, currentEndPos = getStartAndEndWirePos wire

            let shiftWireHorizontally firstVerticalSegLength secondVerticalSegLength =
                let newSegments =
                    match wire.Segments.Length with
                    | 5
                    | 6 ->
                        wire.Segments
                        |> changeSegment 1 firstVerticalSegLength
                        |> changeSegment 3 secondVerticalSegLength
                    | 7 ->
                        // Change into a 5 segment wire
                        wire.Segments[..4]
                        |> changeSegment 1 firstVerticalSegLength
                        |> changeSegment 2 (wire.Segments.[2].Length + wire.Segments.[4].Length)
                        |> changeSegment 3 secondVerticalSegLength
                        |> List.updateAt 4 { wire.Segments.[6] with Index = 4 }
                    | 9 ->
                        // Change segments index 1,3,5,7. Leave rest as is
                        wire.Segments
                        |> changeSegment 1 0.
                        |> changeSegment 3 firstVerticalSegLength
                        |> changeSegment 5 secondVerticalSegLength
                        |> changeSegment 7 0.
                    | _ -> wire.Segments

                { wire with Segments = newSegments }

            // directionToMove must be UP_ or DOWN_
            let shiftedWire (direction: DirectionToMove) =
                let orientation = wire.InitialOrientation

                let getXOrY =
                    fun (pos: XYPos) ->
                        match orientation with
                        | Horizontal -> pos.X
                        | Vertical -> pos.Y

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

                let boundFun, offsetOfBox, otherDir =
                    match direction with
                    | Up_ -> List.maxBy, (fun _ -> 0.), Left_
                    | Down_ -> List.minBy, getWOrH, Right_
                    | _ -> failwithf "What? Can't happen"

                let boundBox = intersectedBoxes |> boundFun (fun box -> getOppositeXOrY box.TopLeft)

                let bound =
                    let offset = Constants.smallOffset + offsetOfBox boundBox

                    let otherOrientation =
                        match orientation with
                        | Horizontal -> direction
                        | Vertical -> otherDir

                    let initialAttemptPos = updatePos direction offset boundBox.TopLeft

                    findMinWireSeparation model initialAttemptPos wire direction orientation (getWOrH boundBox)
                    |> getOppositeXOrY

                let firstVerticalSegLength, secondVerticalSegLength =
                    bound - getOppositeXOrY currentStartPos, getOppositeXOrY currentEndPos - bound

                shiftWireHorizontally firstVerticalSegLength secondVerticalSegLength

            let goodWire dir =
                let shiftedWire = shiftedWire dir

                match findWireSymbolIntersections model shiftedWire with
                | [] -> Ok shiftedWire
                | intersectedBoxes -> Error(intersectedBoxes, shiftedWire)

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
                | Above _ -> tryShiftHorizontalSegAlt model downIntersections downShiftedWire
                | Below _ -> tryShiftHorizontalSegAlt model upIntersections upShiftedWire)


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
        (swapXY pos wireOrientation, swapBB box wireOrientation)
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

                | 9 ->
                    // Change segments index 1,3,5,7. Leave rest as is
                    wire.Segments |> changeSegment 1 0. |> moveHorizSegment 4 |> changeSegment 7 0.

                | _ -> wire.Segments

            { wire with Segments = newSegments }

        // directionToMove must be UP_ or DOWN_
        let shiftedWire (direction: DirectionToMove) =
            let orientation = wire.InitialOrientation

            let getXOrY =
                fun (pos: XYPos) ->
                    match orientation with
                    | Horizontal -> pos.X
                    | Vertical -> pos.Y

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
                let offset = Constants.smallOffset + offsetOfBox boundBox

                let otherOrientation =
                    match orientation with
                    | Horizontal -> direction
                    | Vertical -> otherDir

                let initialAttemptPos = updatePos otherOrientation offset boundBox.TopLeft
                initialAttemptPos |> getOppositeXOrY

            let firstVerticalSegLength, secondVerticalSegLength =
                bound - getOppositeXOrY currentStartPos, getOppositeXOrY currentEndPos - bound


            shiftWireHorizontally firstVerticalSegLength secondVerticalSegLength

        let goodWire dir =
            let shiftedWire = shiftedWire dir

            match findWireSymbolIntersections model shiftedWire with
            | [] -> Ok shiftedWire
            | intersectedBoxes -> Error(intersectedBoxes, shiftedWire)

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


//***************************************************************************************************************//
//**************************************** Old implementation ****************************************************
//***************************************************************************************************************//


/// Check if any bounding box is directly above or below startPos and endPos.
/// If yes, returns a tuple of form:
/// distance between pos and the furthest box above, distance between pos and the furthest box below
let isBoundingBoxAboveOrBelowPos
    (intersectedBoxes: BoundingBox list)
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
        |> List.map (fun box -> getVertDistanceToBox pos box)
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
let rec tryShiftHorizontalSegOld
    (callsLeft: int)
    (model: Model)
    (intersectedBoxes: BoundingBox list)
    (wire: Wire)
    : Wire option =
    match callsLeft with
    | 0 -> None
    | n ->
        let tryShiftHorizontalSegOld = tryShiftHorizontalSegOld (n - 1)

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
                |> List.sortWith (fun box1 box2 ->
                    match wire.InitialOrientation with
                    | Horizontal -> compare box1.TopLeft.Y box2.TopLeft.Y
                    | Vertical -> compare box1.TopLeft.X box2.TopLeft.X)
                |> List.head


            let topBound =
                let viablePos =
                    match wire.InitialOrientation with
                    | Horizontal ->
                        let initialAttemptPos = updatePos Up_ Constants.smallOffset topBoundBox.TopLeft

                        //findMinWireSeparation model initialAttemptPos wire Up_ Horizontal topBoundBox.W
                        initialAttemptPos
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
                |> List.sortWith (fun box1 box2 ->
                    match wire.InitialOrientation with
                    | Horizontal -> compare (box1.TopLeft.Y + box1.H) (box2.TopLeft.Y + box2.H)
                    | Vertical -> compare (box1.TopLeft.X + box1.W) (box2.TopLeft.X + box2.W))
                |> List.rev
                |> List.head


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

                        //findMinWireSeparation model initialAttemptPos wire Right_ Vertical bottomBoundBox.H
                        initialAttemptPos

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
                tryShiftHorizontalSegOld model downShiftedWireIntersections tryShiftDownWire
            | _distanceFromAbove, _distanceFromBelow (*when _distanceFromAbove <= _distanceFromBelow*)  ->
                tryShiftHorizontalSegOld model upShiftedWireIntersections tryShiftUpWire

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
