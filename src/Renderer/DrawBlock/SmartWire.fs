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
    let minWireSeparation = 7. // must be smaller than Buswire.nubLength
    let smallOffset = 0.1
    let localitySize = 12.
    let maxSegmentSeparation = 15.

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


type Bound = { MinB:float; MaxB:float}



type Line = {
    P: float
    B: Bound
    IsFixed: bool
    Seg: ASegment option
    Index: int
    }

type Locality = {
    Fix: float option
    Segments: int list
    Bound: Bound
    SearchLimit: float
    }


 
module Separate =
    open Constants
    /// convert a segment into a fixed or movable line (of given orientation)
    let segmentToLine (ori: Orientation) (seg: ASegment) : Line =
        let order a b = if a < b then {MinB = a; MaxB = b} else {MinB = b; MaxB = a}
        match ori with
        | Horizontal -> {P=seg.Start.Y; B = order seg.Start.X seg.End.X; IsFixed = seg.Segment.Mode=Manual; Seg = Some seg; Index=0}
        | Vertical -> {P=seg.Start.X; B = order seg.Start.Y seg.End.Y; IsFixed = seg.Segment.Mode=Manual; Seg = Some seg; Index=0}

    let moveSegment (index: int) (posDelta: float) (wire: Wire) =
        let segs = wire.Segments
        if index < 1 || index > segs.Length - 2 then
            failwithf $"What? trying to move segment {index} of a wire length {segs.Length}"
//        if abs posDelta > 0.01 then 
//            printf $"""moving segment {index} pd={posDelta} in wire {segs |> List.mapi (fun i seg -> i,seg.Length)}"""
        { wire with 
            Segments = 
                segs
                |> List.updateAt (index-1) {segs[index-1] with Length = segs[index-1].Length + posDelta}
                |> List.updateAt (index+1) {segs[index+1] with Length = segs[index+1].Length - posDelta}
        }       

    let moveLine (ori: Orientation) (newP: float) (line: Line) (wires: Map<ConnectionId,Wire>) =
        match line.Seg with
        | None -> failwithf "Can't move Line {line} - it is not a segment"
        | Some seg ->
            let oldP = 
                match ori with
                | Horizontal -> seg.Start.Y
                | Vertical -> seg.Start.X
            let segIndex = seg.Segment.Index
            let wid = seg.Segment.WireId
            let getPosFromWires (wires:Map<ConnectionId,Wire>) =
                let wire = wires[wid]
                let seg' = 
                    wire
                    |> getAbsSegments
                    |> fun segs -> segs[segIndex]
                match ori with | Vertical -> seg'.Start.X | Horizontal -> seg'.Start.Y
//            printf $"Before move pos: {getPosFromWires wires}"
            let updateWire = Option.map (moveSegment segIndex (newP - oldP))
            Map.change seg.Segment.WireId updateWire wires
//            |> (fun wires -> printf $"New Pos: {getPosFromWires wires}"; wires)

    /// convert a symbol BoundingBox into two fixed lines (of given orientation)
    let bBoxToLines (ori:Orientation) (box:BoundingBox) : Line list =
        let tl = box.TopLeft
        match ori with
        | Horizontal -> [tl.Y, tl.X, tl.X + box.W; tl.Y+box.H, tl.X,tl.X+box.W]
        | Vertical -> [tl.X, tl.Y, tl.Y + box.H; tl.X+box.W, tl.Y,tl.Y+box.H]
        |> List.map (fun (p, minB,maxB) -> {P=p; B = {MinB=minB;MaxB=maxB}; IsFixed=true;Seg=None; Index=0})

    /// make all lines, fixed and movable, of given orientation from Model
    /// ori - orientation of Lines (P coord is reverse of this)
    let makeLines (ori: Orientation) (model:Model) =
        let segLines = 
            ([], model.Wires)
            ||> Map.fold (fun (lines: Line list) _ wire ->
                getAbsSegmentsWithOrientation false ori wire
                |> List.map (segmentToLine ori)
                |> (fun wireLines -> wireLines @ lines))
        let symLines =
            model.Symbol.Symbols
            |> Map.toList
            |> List.collect ( fun (_,sym) -> 
                Symbol.getSymbolBoundingBox sym
                |> bBoxToLines ori)
        symLines @ segLines


    type End = Upper | Lower
  
    /// Returns integers +/- 1 indicating direction of wire leaving ends of line.
    /// Pair returned is MaxB, MinB end of line
    let inline turnDirs (line: Line) (wires: Map<ConnectionId,Wire>)=
        match line.Seg with
        | None -> failwithf "What? Expected Some segment - not None"
        | Some aSeg ->
            let seg = aSeg.Segment
            let wSegs =wires[seg.WireId].Segments
            // s1 is segment at MaxB end of line
            let s1,s2 = 
                if seg.Length > 0 then
                    wSegs[seg.Index+1], wSegs[seg.Index-1]
                else
                    wSegs[seg.Index-1], wSegs[seg.Index+1]
            (sign s1.Length), (sign s2.Length)

    /// +1 if line1.P > line2.P for zero crossings.
    /// -1 if line1.P < line2.P for zero crossings.
    /// 0 if line1.P and line2.P have one crossing.
    let  numCrossingsSign (line1:Line) (line2:Line) (wires: Map<ConnectionId,Wire>) =
        let (max1,min1),(max2,min2) = turnDirs line1 wires, turnDirs line2 wires
        // if line1.P > line2.P then a +1 line1 turnDir or a -1 line2 turnDir from an inner endpoint
        // will NOT cause a crossing. -1 will cause a crossing.
        // The match sums the two inner turnDirs, inverting sign if they come from a line2
        // turning. Dividing this by 2 gives the required answer!
        // NB this is simpler than expected since it does not matter what order the two inner ends are
        // in - which makes identifying them (as one of the MaxB and one of the MinB ends) easier.
        match line1.B.MinB > line2.B.MinB, line1.B.MaxB < line2.B.MaxB with
        | true, true -> min1+max1
        | true, false -> min1-max2
        | false, true -> -min2+max1
        | false, false -> -min2+max2
        |> (fun n -> n / 2)
            
        

    let orderToMinimiseCrossings (model:Model) (indexes:int list) (lines: Line array) =
        if indexes.Length = 1 then
            indexes
        else
            let wires = model.Wires
            let numIndexes = indexes.Length
            let indexesA = indexes |> Array.ofList
            let findIndexKey Array.findIndex ((=) index) indexesA
            let sortOrderA = 
                (let arr = Array.create numIndexes 0
                for i in [0..numIndexes-1] do
                    for j in [0..i-1] do
                        let num = numCrossingsSign lines[i] lines[j] wires
                        arr[indexes[i]] <- arr[indexes[i]] + num
                        arr[indexes[j]] <- arr[indexes[j]] - num
                arr)
                
            let sortFun i j = 
                let iK = findIndexKey i
                let jK = findIndexKey j
                sortOrderA[i] - sortOrderA[j]
            List.sortBy sortFun indexes

                    
            

    let separateSegmentChanges (orientation: Orientation) (model: Model) =
        /// true if bounds of Lines l1 and l2 overlap
        let hasOverlap (b1:Bound) (b2:Bound) =
            inMiddleOf b1.MinB b2.MinB b1.MaxB ||
            inMiddleOf b1.MinB b2.MaxB b1.MinB ||
            inMiddleOf b2.MinB b1.MinB b2.MaxB

        /// union of two bounds b1 and b2. b1 & b2 must overlap
        let boundUnion (b1: Bound) (b2:Bound) =
            {MinB= min b1.MinB b2.MinB; MaxB = max b1.MaxB b2.MaxB}

        let lines = 
            makeLines orientation model
            |> List.toArray
            |> Array.sortBy (fun line -> line.P)
            |> Array.mapi (fun i line -> {line with Index = i})

        //Array.iteri (fun i item -> if Option.isSome item.Seg then printfn $"LINE={i}:{item}") lines

        let lineIsGrouped = Array.create lines.Length false

        lines 
        |> Array.iteri (fun i line -> if line.IsFixed then lineIsGrouped[i] <- true)           

        let expandLocality (index: int) (offset: float) (bound:Bound) (lines: Line array)  =
            let searchDir = sign offset
            let nextIndex i = i + searchDir
            let line = lines[index]
            let initLoc = {Fix = None; Bound = bound; Segments = [index]; SearchLimit = lines[index].P + offset}
            let rec expand i loc =
                if (i < 0 || i >= lines.Length) then 
                    loc
                elif not (hasOverlap loc.Bound lines[i].B) then 
//                    printfn $"Skipping {i} - no overlap"
                    expand (nextIndex i) loc
                else
                    match lines[i] with
                    | {P=p} when p * float searchDir > loc.SearchLimit * float searchDir-> 
//                        printfn $"Skipping {i} - distance={abs (p - p0)} > {abs spaceNeeded}"
                        loc
                    | {IsFixed=true; P = p} -> 
                        {loc with Fix = Some p}
                    | {IsFixed=false; Seg = aSeg; B=b} -> 
                        if lineIsGrouped[i] then 
                            expand (nextIndex i) loc
                        else
//                            printfn $"Adding {i} to locality..."
                            expand 
                              (nextIndex i) 
                              {
                                loc with 
                                    SearchLimit = loc.SearchLimit + offset 
                                    Segments = i :: loc.Segments;
                                    Bound = boundUnion loc.Bound b
                              }
            expand index initLoc

        let makeSegGroup loc1 loc2 index =
            let segs = 
                loc1.Segments @ [index] @ loc2.Segments 
                |> List.distinct
                |> orderToMinimiseCrossings model indexes lines
            if segs.Length > 1 then
                printfn $"** Grouping: {segs} **"
            segs |> List.iter (fun i -> lineIsGrouped[i] <- true)
            let spreadout start sep = 
                [1..segs.Length] 
                |> List.map (fun i -> start + sep * (float i))
                |> List.zip (segs |> List.map (fun i -> lines[i]))
//                |> (fun lst -> (lst |> List.iter (fun (seg,p) -> printf $"Move: {seg.P} -> {p}"));lst)
            match loc1.Fix,loc2.Fix, segs.Length with
            | Some bMax, Some bMin, n ->
//                printfn $"Channel: {bMin} - {bMax} segs: {segs |> List.map (fun n -> n, lines[n].P)}"
                spreadout bMin ((bMax - bMin) / (float n + 1.))
            | None, Some bMin, _ ->
//                printfn $"Change with %.2f{bMin} Min Fix: {segs}"
                spreadout bMin maxSegmentSeparation
            | Some bMax, None, n -> 
//                printfn $"Change with Max %.2f{bMax} Fix: {segs}"
                spreadout (bMax - (float n+1.)*maxSegmentSeparation) maxSegmentSeparation
            | None, None, 1 -> [] // no change
            | None, None, n -> 
                let start = (lines[segs[0]].P + lines[segs[n-1]].P) / 2. - (float n + 1.) * maxSegmentSeparation / 2.
//                printfn $"Change with no Fix: {segs}"
                spreadout start maxSegmentSeparation

        let rec segGroups()  =
            let unGroupedIndex = Array.tryFindIndex ((=) false) lineIsGrouped
            match unGroupedIndex with
            | None -> []
            | Some nextIndex -> 
                //printfn $"found {nextIndex}"
                let loc1 = expandLocality nextIndex localitySize lines[nextIndex].B lines
                let loc2 = expandLocality  (List.max (nextIndex :: loc1.Segments)) -localitySize loc1.Bound lines
                makeSegGroup loc1 loc2 nextIndex :: segGroups()
        segGroups() 
        |> List.concat


    let changeModelFromGroups (ori: Orientation) (model: Model) (changes: (Line * float) list) =
        let wires = 
            (model.Wires, changes)
            ||> List.fold (fun wires (line, newP) -> 
                let seg = Option.get line.Seg
                printfn $"Line {line.Index} {line.P} ->{newP}"
                moveLine ori newP line wires )
        Optic.set wires_ wires model

    let separateModelOneWay  (ori: Orientation) (model: Model) =
        printfn $"**********Checking {ori} segments**********"
        separateSegmentChanges ori model
        |> changeModelFromGroups ori model

    let separateModel = 
        (fun model -> printfn "Starting separate..."; model)
        >> separateModelOneWay Vertical 
        >> separateModelOneWay Horizontal
     

let updateWireSegmentJumpsAndSeparations model =
    model
    |> Separate.separateModel
    |> (BusWireUpdateHelpers.updateWireSegmentJumps [])
           

            


        

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
            {   
                W = boundingBox.W + Constants.minWireSeparation * 2.
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
    List.updateAt segIndex {segments[segIndex] with Length = newLength} segments

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

    let rotCW (pos: XYPos) = {X= pos.Y; Y = -pos.X}
    let rotACW (pos : XYPos) = {X= -pos.Y; Y = pos.X}

    let rotBox (rot: XYPos -> XYPos) (box: BoundingBox) = 
        let pos = rot {X=box.W; Y=box.H}
        {TopLeft = rot box.TopLeft; W = pos.X; H = pos.Y}

    let rotBoxCW = rotBox rotCW
    let rotBoxACW = rotBox rotACW
    let negLength (seg:Segment) = {seg with Length = -seg.Length}
    let negateAlternateLengths negateFirst (seg: Segment) = match (seg.Index % 2 = 0) = negateFirst with | false -> seg | true -> negLength seg      

    /// makes StartPos and Segments normalised, port positions are not normalised
    let normaliseWire (wire:Wire) =
        match wire.InitialOrientation with
        | Horizontal -> wire
        | Vertical -> {wire with Segments = wire.Segments |> List.map (negateAlternateLengths false); StartPos = rotACW wire.StartPos; }

    /// reverses the effect of normaliseWire
    let deNormaliseWire (wire:Wire) =
        match wire.InitialOrientation with
        | Horizontal -> wire
        | Vertical -> {wire with Segments = wire.Segments |> List.map (negateAlternateLengths true); StartPos = rotCW wire.StartPos}

    //------------------------------------------------------------------------------------------------------------------------------//
    //---------------------------------------------------Implementation-------------------------------------------------------------//
    //------------------------------------------------------------------------------------------------------------------------------//
    // return Some max distance above or below, if one exists, or None
    let tryMaxDistance  (distances: VertDistFromBoundingBox option list) =
        match distances with
        | [] -> None
        | _  -> List.maxBy (function | Some (Above d) | Some (Below d) -> d | None -> -infinity) distances

    /// returns the maximum vertical distance of pos from intersectedBoxes as a VertDistFromBoundingBox or None if there are no intersections
    let maxVertDistanceFromBox
        (intersectedBoxes: BoundingBox list)
        (wireOrientation: Orientation)
        (pos: XYPos)
        : VertDistFromBoundingBox option =

        let isCloseToBoxHoriz (box: BoundingBox) (pos: XYPos) = inMiddleOrEndOf box.TopLeft.X pos.X (box.TopLeft.X + box.W)

        let getVertDistanceToBox (pos: XYPos) (box: BoundingBox) : VertDistFromBoundingBox option list =
            (swapXY pos wireOrientation, swapBB box wireOrientation)
            ||> (fun pos box ->
                if isCloseToBoxHoriz box pos then
                    if pos.Y > box.TopLeft.Y then
                        [pos.Y - box.TopLeft.Y |> Above |> Some]
                    else
                        [box.TopLeft.Y - pos.Y + box.H |> Below |> Some]
                else [])

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
                let getXOrY = fun (pos:XYPos) -> match orientation with | Horizontal -> pos.X | Vertical -> pos.Y
                let getOppositeXOrY = fun (pos:XYPos) -> match orientation with | Horizontal -> pos.Y | Vertical -> pos.X
                let getWOrH = fun (box:BoundingBox) -> match orientation with |Horizontal -> box.W | Vertical -> box.H
                let getOppositeWOrH = fun (box:BoundingBox) -> match orientation with |Horizontal -> box.H | Vertical -> box.W

                let boundFun, offsetOfBox, otherDir = 
                    match direction with 
                    | Up_ -> List.maxBy, (fun _ -> 0.), Left_ 
                    | Down_ -> List.minBy, getWOrH, Right_ 
                    | _ -> failwithf "What? Can't happen"

                let boundBox =
                    intersectedBoxes
                    |>  boundFun ( fun box -> getOppositeXOrY box.TopLeft)

                let bound =
                    let offset = Constants.smallOffset + offsetOfBox boundBox
                    let otherOrientation = match orientation with | Horizontal -> direction | Vertical -> otherDir
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
                | intersectedBoxes -> Error (intersectedBoxes, shiftedWire) 

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
            | Error (upIntersections, upShiftedWire),  Error (downIntersections, downShiftedWire) ->
                [currentStartPos;currentEndPos]
                |> List.map (maxVertDistanceFromBox intersectedBoxes wire.InitialOrientation)
                |> tryMaxDistance
                |> (function
                    | None
          
                    | Some (Above _) ->
                        tryShiftHorizontalSegAlt model downIntersections downShiftedWire
                    | Some (Below _)  ->
                        tryShiftHorizontalSegAlt model upIntersections upShiftedWire)


//***************************************************************************************************************//
//**************************************** NEW implementation ****************************************************
//***************************************************************************************************************//

// return Some max distance above or below, if one exists, or None
let tryMaxDistance  (distances: VertDistFromBoundingBox option list) =
    match distances with
    | [] -> None
    | _  -> List.maxBy (function | Some (Above d) | Some (Below d) -> d | None -> -infinity) distances

/// returns the maximum vertical distance of pos from intersectedBoxes as a VertDistFromBoundingBox or None if there are no intersections
let maxVertDistanceFromBox
    (intersectedBoxes: BoundingBox list)
    (wireOrientation: Orientation)
    (pos: XYPos)
    : VertDistFromBoundingBox option =

    let isCloseToBoxHoriz (box: BoundingBox) (pos: XYPos) = inMiddleOrEndOf box.TopLeft.X pos.X (box.TopLeft.X + box.W)

    let getVertDistanceToBox (pos: XYPos) (box: BoundingBox) : VertDistFromBoundingBox option list =
        (swapXY pos wireOrientation, swapBB box wireOrientation)
        ||> (fun pos box ->
            if isCloseToBoxHoriz box pos then
                if pos.Y > box.TopLeft.Y then
                    [pos.Y - box.TopLeft.Y |> Above |> Some]
                else
                    [box.TopLeft.Y - pos.Y + box.H |> Below |> Some]
            else [])

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
                | 6 ->
                    wire.Segments
                    |> moveHorizSegment 2 
                    
                | 7 ->
                    // Change into a 5 segment wire
                    wire.Segments[..4]
                    |> moveHorizSegment 2
                    |> changeSegment 2 (wire.Segments.[2].Length + wire.Segments.[4].Length)
                    |> List.updateAt 4 { wire.Segments.[6] with Index = 4 }

                | 9 ->
                    // Change segments index 1,3,5,7. Leave rest as is
                    wire.Segments
                    |> changeSegment 1 0.
                    |> moveHorizSegment 4
                    |> changeSegment 7 0.

                | _ -> wire.Segments

            { wire with Segments = newSegments }

        // directionToMove must be UP_ or DOWN_
        let shiftedWire (direction: DirectionToMove) =
            let orientation = wire.InitialOrientation
            let getXOrY = fun (pos:XYPos) -> match orientation with | Horizontal -> pos.X | Vertical -> pos.Y
            let getOppositeXOrY = fun (pos:XYPos) -> match orientation with | Horizontal -> pos.Y | Vertical -> pos.X
            let getWOrH = fun (box:BoundingBox) -> match orientation with |Horizontal -> box.W | Vertical -> box.H
            let getOppositeWOrH = fun (box:BoundingBox) -> match orientation with |Horizontal -> box.H | Vertical -> box.W

            let offsetOfBox, otherDir = 
                match direction with 
                | Up_ -> (fun _ -> 0.), Left_ 
                | Down_ -> (fun box -> getOppositeWOrH box) , Right_ 
                | _ -> failwithf "What? Can't happen"

            let boundBox =
                intersectedBoxes
                |>  match direction with
                    | Down_ -> List.maxBy (fun box -> getOppositeXOrY box.TopLeft + getOppositeWOrH box)
                    | Up_ -> List.minBy (fun box -> getOppositeXOrY box.TopLeft)
                    | _ -> failwithf "What? Can't happen"

            let bound =
                let offset =  Constants.smallOffset + offsetOfBox boundBox
                let otherOrientation = match orientation with | Horizontal -> direction | Vertical -> otherDir
                let initialAttemptPos = updatePos otherOrientation offset boundBox.TopLeft
                initialAttemptPos
                |> getOppositeXOrY

            let firstVerticalSegLength, secondVerticalSegLength =
                bound - getOppositeXOrY currentStartPos, getOppositeXOrY currentEndPos - bound


            shiftWireHorizontally firstVerticalSegLength secondVerticalSegLength

        let goodWire dir = 
            let shiftedWire = shiftedWire dir
            match findWireSymbolIntersections model shiftedWire with
            | [] -> Ok shiftedWire
            | intersectedBoxes -> Error (intersectedBoxes, shiftedWire) 

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
        | Error (upIntersections, upShiftedWire),  Error (downIntersections, downShiftedWire) ->
            [currentStartPos;currentEndPos]
            |> List.map (maxVertDistanceFromBox intersectedBoxes wire.InitialOrientation)
            |> tryMaxDistance
            |> (function
                | None         
                | Some (Above _) ->
                    tryShiftHorizontalSeg model downIntersections downShiftedWire
                | Some (Below _)  ->
                    tryShiftHorizontalSeg model upIntersections upShiftedWire)
                           

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
