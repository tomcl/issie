module SheetBeautifyHelpers

open CommonTypes
open DrawModelType
open SymbolT
open SheetT
open Symbol
open BlockHelpers



//-----------------------------------------------------------------------------------------------
// visibleSegments is included here as ahelper for info, and because it is needed in project work
//-----------------------------------------------------------------------------------------------

/// The visible segments of a wire, as a list of vectors, from source end to target end.
/// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed 
/// which if present causes the two segments on either side of it to coalesce into a single visible segment.
/// A wire can have any number of visible segments - even 1.
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



/// key: B1R Type: R Descrip: The dimensions of a custom component symbol
let getCustomSymbolDimensions (sym: SymbolT.Symbol) : float * float =
    let height = sym.Component.H
    let width = sym.Component.W
    match sym.HScale with
    | Some hScale -> 
        match sym.VScale with
        | Some vScale -> (hScale * width, vScale * height)
        | None -> (hScale, 0.0)
    | None -> failwith "cannot get dimensions of a non-custom symbol"

/// key: B1W Type: W Descrip: The dimensions of a custom component symbol
let writeCustomSymbolDimensions (sym: SymbolT.Symbol) (dimensions: float * float) : SymbolT.Symbol =
    let height = sym.Component.H
    let width = sym.Component.W
    {sym with HScale = Some ((fst dimensions) / width) ; VScale = Some ((snd dimensions) / height)}

/// key: B2W Type: W Descrip: The position of a symbol on the sheet
let writeSymbolPositionOnSheet (pos: XYPos) (symId: ComponentId) (sheetModel: SheetT.Model) : SheetT.Model  =
    let symModel = sheetModel.Wire.Symbol.Symbols[symId]
    let newSymModel = {symModel with Pos = pos}
    {sheetModel with Wire.Symbol.Symbols = sheetModel.Wire.Symbol.Symbols.Add(symId, newSymModel)}

/// key: B3R Type: R Descrip: Read the order of ports on a specified side of a symbol
let getPortOrder (sym: Symbol) (edge: Edge):  string list =
    sym.PortMaps.Order[edge]

/// key: B3W Type: W Descrip: Write the order of ports on a specified side of a symbol
let writePortOrder (sym: Symbol) (edge: Edge) (order: string list):  Symbol =
    {sym with PortMaps = {sym.PortMaps with Order = sym.PortMaps.Order.Add(edge, order)}}

/// key: B4R Type: R Descrip: The reverses state of the inputs of a MUX2
let getReverseStateOfInputsMux2 (sym: Symbol) :  bool option=
    sym.ReversedInputPorts

/// key: B4W Type: W Descrip: The reverses state of the inputs of a MUX2
let writeReverseStateOfInputsMux2 (sym: Symbol) (reverseStateOfInput: bool option):  Symbol=
    {sym with ReversedInputPorts = reverseStateOfInput}

/// key: B5R type: R Descrip: The position of a port on the sheet. It cannot directly be written
let getPortPositionOnSheet (portId: PortId) (sheetModel:SheetT.Model) :  XYPos =
    getPortIdStr portId
    |> getPortLocation None sheetModel.Wire.Symbol

/// key: B6R Type: R Descrip: The Bounding box of a symbol outline (position is contained in this)
let getSymbolBoudingboxOutline (sym: Symbol) :  BoundingBox=
    getSymbolBoundingBox sym

/// key: B7R Type: R Descrip: The rotation state of a symbol
let getSymbolRotateState (sym: SymbolT.Symbol) : Rotation=
    sym.STransform.Rotation

/// key: B7W Type: W Descrip: The rotation state of a symbol
let writeSymbolRotateState (rotation: Rotation) (sym: SymbolT.Symbol) : SymbolT.Symbol=
    {sym with STransform = {sym.STransform with Rotation = rotation}}

/// key: B8R Type: R Descrip: The flip state of a symbol
let getSymbolFlipState (sym: SymbolT.Symbol) : bool =
    sym.STransform.Flipped

/// key: B8W Type: W Descrip: The flip state of a symbol
let writeSymbolFlipState (sym: SymbolT.Symbol) (filp: bool) :  SymbolT.Symbol=
    {sym with STransform = {sym.STransform with Flipped = filp}}

/// key: T1R Type: R Descrip: The number of pairs of symbols that intersect each other. See Tick3 for a related function.
/// Count over all pairs of symbols.
let getIntersectingSymbolsCount (sheetModel:SheetT.Model) : int =
    let boxes =
        Helpers.mapValues sheetModel.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> n,box)
    List.allPairs boxes boxes
    |> List.filter (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
    |> List.length


/// key: T2R Type: R Descrip: The number of distinct wire visible segments that intersect with one or more symbols.
/// See Tick3.HLPTick3.visibleSegments for a helper. Count over all visible wire segments.
let getIntersectingWireSegmentsCount (sheetModel:SheetT.Model) : int =
    let segments = 
        sheetModel.Wire.Wires
        |> Helpers.mapValues 
        |> Array.toList
        |> List.collect (fun wire -> getNonZeroAbsSegments wire)
    
    let symbolBoundingBoxes =
        sheetModel.Wire.Symbol.Symbols
        |> Helpers.mapValues
        |> Array.toList
        |> List.map getSymbolBoundingBox

    //return true if seg intersects with any symbol
    let segIntersectWithSymbol (seg: BusWireT.ASegment) : bool =
        let intersectBoxCount =
            symbolBoundingBoxes
            |> List.map (fun box -> segmentIntersectsBoundingBox box seg.Start seg.End )
            |> List.filter (fun b -> b.IsSome)
            |> List.length
        match intersectBoxCount with
        | 0 -> false
        | _ -> true
        
    segments
    |> List.filter (fun seg -> segIntersectWithSymbol seg)
    |> List.length



/// key: T3R Type: R Descrip: The number of distinct pairs of segments that cross each other at right angles. 
/// Does not include 0 length segments or segments on same net intersecting at one end, 
/// or segments on same net on top of each other. Count over whole sheet.
let getRightAngleCrossingSegmentsCount (sheetModel:SheetT.Model) : int =    
    let segLst = 
        sheetModel.Wire.Wires
        |> Helpers.mapValues
        |> Array.toList
        |> List.collect (fun wire -> getNonZeroAbsSegments wire)
        
    let segPairs =
        List.allPairs segLst segLst
        |> List.filter (fun (seg1, seg2) -> not (seg1 = seg2))
    
    let segPairNotTIntersect (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) =
        if seg1.Start = seg2.Start || seg1.Start = seg2.End || seg1.End = seg2.Start || seg1.End = seg2.End then false
        else true

    let segPairCrossIntersect (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) =
        if overlap2D (seg1.Start, seg1.End) (seg2.Start, seg2.End)
            && segPairNotTIntersect seg1 seg2 then true
        else false
        
    segPairs
    |> List.filter (fun (seg1, seg2) -> segPairCrossIntersect seg1 seg2)
    |> List.length



/// key: T4R Type: R Sum of wiring segment length, counting only one when there are N same-net 
/// segments overlapping (this is the visible wire length on the sheet). 
/// Count over whole sheet.
let getVisibleWireLength (sheetModel:SheetT.Model) : float =    
    let isOverlap (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) =
        let seg1Box = (seg1.Start, seg1.End)
        let seg2Box = (seg2.Start, seg2.End)
        overlap2D seg1Box seg2Box

    // gouping wires by source port
    let sameNetSegments =
        sheetModel.Wire.Wires
        |> Helpers.mapValues
        |> Array.toList
        |> List.groupBy (fun wire -> wire.InputPort)
        |> List.map (fun (port, wires) -> wires |> List.collect (fun wire -> getNonZeroAbsSegments wire))
    
    // gouping segments by orientation fst: horizontal, snd: vertical
    let sameOrientationSegments (segLst: BusWireT.ASegment list) =
        segLst
        |> List.partition (fun seg -> seg.Start.X = seg.End.X)

    let horizontalSegments, verticalSegments = 
        sameNetSegments
        |> List.map sameOrientationSegments
        |> List.unzip

    let groupOverlapSegments (segmentsList: BusWireT.ASegment list) =
        segmentsList 
        |> List.fold (fun (segGroups, remainingSegments) segment ->
            let overlappingGroup, nonOverlappingGroups =
                segGroups
                |> List.partition (fun group ->
                    group |> List.exists (fun s -> isOverlap s segment))

            match overlappingGroup with
            | [] ->
                // no overlap: create a new group for the current segment
                ([segment] :: nonOverlappingGroups, remainingSegments)
            | group :: _ ->
                // overlap: add the current segment to the first overlapping group found
                ((segment :: group) :: nonOverlappingGroups, remainingSegments)
        ) ([], segmentsList)
        |> fst
        
    // given segLst whose element overlaps with each other, get the visible length
    let getLongestLength (segLst: BusWireT.ASegment list) =
        let minStartSeg = segLst |> List.minBy (fun seg -> seg.Start.X + seg.Start.Y)
        let maxEndSeg = segLst |> List.maxBy (fun seg -> seg.End.X + seg.End.Y)
        let lengthXY =  maxEndSeg.End - minStartSeg.Start
        abs(lengthXY.X + lengthXY.Y)

    let segmengtsOverlappingGroups = 
        horizontalSegments |> List.collect groupOverlapSegments
        |> List.append (verticalSegments |> List.collect groupOverlapSegments)
        
    segmengtsOverlappingGroups |> List.map getLongestLength |> List.sum
    

/// key: T5R Type: R Descrip: Number of visible wire right-angles. Count over whole sheet.
let getVisibleWireRightAnglesCount (sheetModel:SheetT.Model) : int =

    let segmentsHaveRightAngle (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) = 
        let seg1Orientation = BusWire.getSegmentOrientation seg1.Start seg1.End
        let seg2Orientation = BusWire.getSegmentOrientation seg1.Start seg1.End
        if seg1Orientation = seg2Orientation then false
        else true

    let countRightAnglePairs (wire: BusWireT.Wire) =
        getNonZeroAbsSegments wire
        |> List.windowed 2 //pair two successive segmnts
        |> List.filter (fun aSeg12 -> segmentsHaveRightAngle aSeg12[1] aSeg12[2])
        |> List.length

    sheetModel.Wire.Wires
    |> Helpers.mapValues
    |> Array.toList
    |> List.map countRightAnglePairs
    |> List.sum


/// key: T6R Type: R Descrip: The zero-length segments in a wire with non-zero segments on either side that have 
/// Lengths of opposite signs lead to a wire retracing itself. Note that this can also apply 
/// at the end of a wire (where the zero-length segment is one from the end). This is a 
/// wiring artifact that should never happen but errors in routing or separation can 
/// cause it. Count over the whole sheet. Return from one function a list of all the 
/// segments that retrace, and also a list of all the end of wire segments that retrace so 
/// far that the next segment (index = 3 or Segments.Length – 4) - starts inside a symbol.
let getRetracingSegmentsLsts (sheetModel:SheetT.Model) : BusWireT.ASegment list * BusWireT.ASegment list=
    
    let wiresLst = 
        sheetModel.Wire.Wires

    let segsLst :  BusWireT.ASegment list= 
        sheetModel.Wire.Wires
            |> Helpers.mapValues
            |> Array.toList
            |> List.collect (fun wire ->  getAbsSegments wire)

    let isRetracing (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) : bool=
        seg1.Segment.Length * seg2.Segment.Length < 0 && 
        BusWire.getSegmentOrientation seg1.Start seg1.End = BusWire.getSegmentOrientation seg2.Start seg2.End

    let endZeroSegment = 
        let lastSeg = List.last segsLst
        if lastSeg.Segment.IsZero
        then [lastSeg]
        else []
    
    let segIntersectSym (seg: BusWireT.ASegment) : bool=
        let wire = wiresLst[seg.Segment.WireId]
        let sourceSym = getSourceSymbol sheetModel.Wire wire
        let targetSym = getTargetSymbol sheetModel.Wire wire
        (segmentIntersectsBoundingBox (getSymbolBoundingBox sourceSym) seg.Start seg.End).IsSome && 
        (segmentIntersectsBoundingBox (getSymbolBoundingBox targetSym) seg.Start seg.End).IsSome

    let findRetracingEndSegments : BusWireT.ASegment list * BusWireT.ASegment list=
        segsLst
        |> List.windowed 3
        |> List.fold (fun (retracingSegs, retracingIntersectSymSegs) segs ->
            //if is retracing
            if isRetracing segs.[0] segs.[2] then
                let newretracingIntersectSymSegs = 
                    //if the third segment is retracing and intersect with either source or target symbol
                    if segIntersectSym segs[2] 
                    then segs[1] :: retracingIntersectSymSegs
                    else retracingIntersectSymSegs
                (segs[1] :: retracingSegs, newretracingIntersectSymSegs)
            else //if no retracing, return previous state
                (retracingSegs, retracingIntersectSymSegs)
        ) ([], [])

    let (retracingSegs, retracingIntersectSymSegs) = findRetracingEndSegments
    
    //add the end zero-length segment if exist
    List.append retracingSegs endZeroSegment, retracingIntersectSymSegs
