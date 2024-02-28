module SheetBeautifyHelpers

open CommonTypes
open DrawModelType
open SymbolT
open SheetT
open Symbol
open BlockHelpers

//B1R
/// Get dimensions of a custom symbol
let getCustomSymbolDimensions (sym: SymbolT.Symbol) : float * float =
    let height = sym.Component.H
    let width = sym.Component.W
    match sym.HScale with
    | Some hScale -> 
        match sym.VScale with
        | Some vScale -> (hScale * width, vScale * height)
        | None -> (hScale, 0.0)
    | None -> failwith "cannot get dimensions of a non-custom symbol"

//B1W
/// Write dimensions of a custom symbol
let writeCustomSymbolDimensions (sym: SymbolT.Symbol) (dimensions: float * float) : SymbolT.Symbol =
    let height = sym.Component.H
    let width = sym.Component.W
    {sym with HScale = Some ((fst dimensions) / width) ; VScale = Some ((snd dimensions) / height)}

//B2W
/// Write the position of a symbol on sheet
let writeSymbolPositionOnSheet (pos: XYPos) (symId: ComponentId) (sheetModel: SheetT.Model) : SheetT.Model  =
    let symModel = sheetModel.Wire.Symbol.Symbols[symId]
    let newSymModel = {symModel with Pos = pos}
    {sheetModel with Wire.Symbol.Symbols = sheetModel.Wire.Symbol.Symbols.Add(symId, newSymModel)}

//B3R
/// Get the order of ports on a specified side of a symbol
let getPortOrder (sym: Symbol) (edge: Edge):  string list =
    sym.PortMaps.Order[edge]

//B3W
/// Write the order of ports on a specified side of a symbol
let writePortOrder (sym: Symbol) (edge: Edge) (order: string list):  Symbol =
    {sym with PortMaps = {sym.PortMaps with Order = sym.PortMaps.Order.Add(edge, order)}}

//B4R
/// Get the reverses state of the inputs of a MUX2
let getReverseStateOfInputsMux2 (sym: Symbol) :  bool option=
    sym.ReversedInputPorts

//B4W
/// Write the reverses state of the inputs of a MUX2
let writeReverseStateOfInputsMux2 (sym: Symbol) (reverseStateOfInput: bool option):  Symbol=
    {sym with ReversedInputPorts = reverseStateOfInput}

//B5R
/// Get the position of a port on the sheet
let getPortPositionOnSheet (portId: PortId) (sheetModel:SheetT.Model) :  XYPos =
    getPortIdStr portId
    |> getPortLocation None sheetModel.Wire.Symbol

//B6R
/// Get the Bounding box of a symbol outline (position is contained in this)
let getSymbolBoudingboxOutline (sym: Symbol) :  BoundingBox=
    getSymbolBoundingBox sym

//B7R
/// Get the rotation state of a symbol
let getSymbolRotateState (sym: SymbolT.Symbol) : Rotation=
    sym.STransform.Rotation

//B7W
/// Write the rotation state of a symbol
let writeSymbolRotateState (rotation: Rotation) (sym: SymbolT.Symbol) : SymbolT.Symbol=
    {sym with STransform = {sym.STransform with Rotation = rotation}}

//B8R
/// Get the flip state of a symbol
let getSymbolFlipState (sym: SymbolT.Symbol) : bool =
    sym.STransform.Flipped

//B8W
/// Write the flip state of a symbol
let writeSymbolFlipState (sym: SymbolT.Symbol) (filp: bool) :  SymbolT.Symbol=
    {sym with STransform = {sym.STransform with Flipped = filp}}

//T1R
/// Count the number of pair of symbols that intersect with each other in a sheet
let getIntersectingSymbolsCount (sheetModel:SheetT.Model) : int =
    let boxes =
        Helpers.mapValues sheetModel.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> n,box)
    List.allPairs boxes boxes
    |> List.filter (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
    |> List.length


//T2R
/// Count the number of distinct wire visible segments that intersect with at least one symbol.
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


//T3R
/// Count the number of pairs of non-zero segments that cross each other at right angles.
let getRightAngleCrossingSegmentsCount (sheetModel:SheetT.Model) : int =    
    let segLst = 
        sheetModel.Wire.Wires
        |> Helpers.mapValues
        |> Array.toList
        |> List.collect (fun wire -> getNonZeroAbsSegments wire)
        
    let segPairs =
        List.allPairs segLst segLst
        |> List.filter (fun (seg1, seg2) -> not (seg1 = seg2))
    
    // check whether two segments have T intersection
    let segPairNotTIntersect (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) =
        if seg1.Start = seg2.Start || seg1.Start = seg2.End || seg1.End = seg2.Start || seg1.End = seg2.End then false
        else true

    // exclude T intersection onlt consider cross intersection
    let segPairCrossIntersect (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) =
        if overlap2D (seg1.Start, seg1.End) (seg2.Start, seg2.End)
            && segPairNotTIntersect seg1 seg2 then true
        else false
        
    segPairs
    |> List.filter (fun (seg1, seg2) -> segPairCrossIntersect seg1 seg2)
    |> List.length


//T4R 
/// Calculate the sum of visible wire length on the sheet.
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

    // counting only one when there are N same-net segments overlapping
    // given segLst whose elements overlap with each other, get the visible length
    let getLongestLength (segLst: BusWireT.ASegment list) =
        let minStartSeg = segLst |> List.minBy (fun seg -> seg.Start.X + seg.Start.Y)
        let maxEndSeg = segLst |> List.maxBy (fun seg -> seg.End.X + seg.End.Y)
        let lengthXY =  maxEndSeg.End - minStartSeg.Start
        abs(lengthXY.X + lengthXY.Y)

    let segmengtsOverlappingGroups = 
        horizontalSegments |> List.collect groupOverlapSegments
        |> List.append (verticalSegments |> List.collect groupOverlapSegments)
        
    segmengtsOverlappingGroups |> List.map getLongestLength |> List.sum
    
// T5R
/// Count the number of visible wire right-angles.
let getVisibleWireRightAnglesCount (sheetModel:SheetT.Model) : int =

    let segmentsHaveRightAngle (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) = 
        let seg1Orientation = BusWire.getSegmentOrientation seg1.Start seg1.End
        let seg2Orientation = BusWire.getSegmentOrientation seg1.Start seg1.End
        if seg1Orientation = seg2Orientation then false
        else true

    // only consider right angles in one wire, right angles formed by different wires are not taken into consideration
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

//T6R
/// Get the list of segments that retrace its previous non-zero segment and list of retrace segments that start inside a symbol.
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
