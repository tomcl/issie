module SheetBeautifyHelpers
open Optics
open DrawModelType.SymbolT
open CommonTypes
open DrawModelType
open Symbol


// TODO: See where you can use state cahgne instead of extra pipeline

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team


/// B1R, B1W
/// Read/write the dimensions of a custom component symbol

let customComponentDimensions_ = 
    
    let read (symbol:Symbol) = {|H = symbol.Component.H; W = symbol.Component.W|}

    let write (dimensions: {| H: float; W:float |}) (symbol:Symbol) = 
        {symbol with Component = {symbol.Component with H = dimensions.H; W = dimensions.W} }

    Lens.create (read) (write)

    
/// B2W
/// Write position of symbol on the sheet

let writeSymbolPosition (symbol : Symbol) (pos: XYPos) =
    {symbol with Pos = pos }


/// B3R, B3W
/// Read/write the order of ports on a specified side of a symbol

let symbolPortOrder_ (edge: Edge) = 

    let read (symbol: Symbol) = 
        let order = Optic.get order_ symbol.PortMaps 
        match Map.tryFind edge order with
        | Some portOrder -> portOrder
        | None -> []

    let write (portOrder: string list) (symbol: Symbol) =
        let order = Optic.get order_ symbol.PortMaps
        let newPortOrder = 
            match Map.tryFind edge order with
            | Some _ -> order.Add(edge, portOrder)
            | None -> order

        {symbol with PortMaps = Optic.set order_ newPortOrder symbol.PortMaps}


    Lens.create read write


/// B4R, B4W
/// Read/write 'ReversedInputPorts' of MUXs and DEMUXs

let reversedInputPorts_  = Lens.create (fun (s: Symbol) -> s.ReversedInputPorts) (fun i s -> {s with ReversedInputPorts = i})


/// B5R
/// Reads the position of a port within a sheet
let portPositionInSheet (port: Port) (sheet: SheetT.Model) =
    let symbolWithPort = sheet.Wire.Symbol.Symbols[ComponentId port.HostId]
    symbolWithPort.Pos + (getPortPos symbolWithPort port)


/// B6R
/// Reads the bounding box of a symbol outline

let getSymbolBB (symbol: Symbol) =
    getSymbolBoundingBox symbol

/// B7R, B7W
/// Read/ write the rotation state of a symbol
let rotationState_ = Lens.create (fun (s : Symbol) -> s.STransform.Rotation)(fun (r:Rotation) (s : Symbol) -> {s with STransform = {s.STransform with Rotation = r} })

/// B8R, B8W
/// Read/ write the rotation state of a symbol
let flipState_ = Lens.create (fun (s : Symbol) -> s.STransform.Flipped)(fun (f:bool) (s : Symbol) -> {s with STransform = {s.STransform with Flipped = f} })
    
/// T1R
/// Returns the number of pairs of symbols that intersect each other

let symbolPairIntersects (sheet : SheetT.Model) = 

    let createUniquePairs (symbols: Symbol list)=
        symbols
        |> List.allPairs symbols
        |> List.where (fun (x,y) -> (x.Id <> y.Id))
        |> List.map (fun (x,y)-> if(x.Id>y.Id) then (x,y) else (y,x))
        |> List.distinct

    let overlapSymbols = 
        sheet.Wire.Symbol.Symbols
        |> Map.toList
        |> List.map snd
        |> createUniquePairs
        |> List.filter (fun (s1 : Symbol,s2 : Symbol)-> BlockHelpers.overlap2DBox s1.SymbolBoundingBox s2.SymbolBoundingBox )
        |> List.length
    
    overlapSymbols
    


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


/// T2R
/// Returns the number of distinct visible wire segments that intersect one or more symbols
let segmentIntersectSymbolCount (sheet: SheetT.Model) =

    let getSegmentVertices (wire: BusWireT.Wire) =
        let (segVecs : XYPos list) = visibleSegments wire.WId sheet
        (wire.StartPos, segVecs) ||> List.scan (fun (currVec : XYPos) (nextVec : XYPos) -> 
                                                {currVec with X = currVec.X+nextVec.X; Y = currVec.Y+nextVec.Y})
    let segmentIntersectsBB (boundingBox : BoundingBox) (startPos: XYPos) (endPos : XYPos) = 
        match BlockHelpers.segmentIntersectsBoundingBox boundingBox startPos endPos with // do not consider the symbols that the wire is connected to
            | Some _ -> true // segment intersects bounding box
            | None -> false // no intersection

    let symbols = 
        sheet.Wire.Symbol.Symbols
        |> Map.toList
        |> List.map snd
        |> List.map (getSymbolBB)

    let intersectingSegmentCount =
        sheet.Wire.Wires
        |> Map.toList
        |> List.map snd
        |> List.map (getSegmentVertices) 
        |> List.map (List.pairwise)
        |> List.map(List.allPairs symbols)
        |> List.map(List.filter (fun (bb, (segStart, segEnd)) -> segmentIntersectsBB bb segStart segEnd))
        |> List.map(List.map snd)
        |> List.concat
        |> List.distinct
        |> List.length

    intersectingSegmentCount


/// T3R
/// Return the number of distinct pairs of segments that cross each other at right angles
/// but are not from the same net
let rightAngleSegCount (sheet : SheetT.Model) =

    let isPerpendicular (seg1Start: XYPos) (seg1End: XYPos) (seg2Start: XYPos) (seg2End: XYPos) =

        let isPointEqual (p1: XYPos) (p2: XYPos) =
            p1.X = p2.X && p1.Y = p2.Y
        
        let p1 = seg1Start
        let p2 = seg1End
        let p3 = seg2Start
        let p4 = seg2End

        (* Check if the slopes are negative reciprocals of each other (excluding cases where both slopes are 0 or infinite) *)
        let m1 = 
            try 
                (p2.Y - p1.Y) / (p2.X - p1.X) 
            with 
                | :? System.DivideByZeroException -> 0.0
        let m2 = 
            try 
                (p4.Y - p3.Y) / (p4.X - p3.X) 
            with 
                | :? System.DivideByZeroException -> 0.0
        let isPerpendicularSlope = (m1 * m2) = -1.0

        (* Additional check to handle cases where one line is horizontal and the other is vertical *)
        let isHorizontalLine1 = isPointEqual p1 p2 && p1.Y <> p2.Y
        let isVerticalLine1 = isPointEqual p1 p2 && p1.X <> p2.X
        let isHorizontalLine2 = isPointEqual p3 p4 && p3.Y <> p4.Y
        let isVerticalLine2 = isPointEqual p3 p4 && p3.X <> p4.X

        isPerpendicularSlope && (isHorizontalLine1 || isVerticalLine1) && (isHorizontalLine2 || isVerticalLine2)

    let getSegmentVertices (wire: BusWireT.Wire) =
        let (segVecs : XYPos list) = visibleSegments wire.WId sheet
        (wire.StartPos, segVecs) ||> List.scan (fun (currVec : XYPos) (nextVec : XYPos) -> 
                                                {currVec with X = currVec.X+nextVec.X; Y = currVec.Y+nextVec.Y})

    let wireCombinations = 
        let wireList = 
            sheet.Wire.Wires
            |> Map.toList
            |> List.map snd

        List.allPairs wireList wireList
        |> List.filter (fun (w1,w2) ->  w1.OutputPort <> w2.OutputPort)
  
    let segVectorPairs (wire1: BusWireT.Wire) (wire2: BusWireT.Wire) = 
        
        let wire1Segments: (XYPos * XYPos) list = 
            getSegmentVertices wire1 |> List.pairwise

        let wire2Segments = 
            getSegmentVertices wire2 |> List.pairwise

        List.allPairs wire1Segments wire2Segments

    wireCombinations
    |> List.map (fun (w1,w2) -> segVectorPairs w1 w2)
    |> List.concat
    |> List.filter (fun ((start1, end1),(start2, end2)) -> isPerpendicular start1 end1 start2 end2)
    |> List.length

/// T4R
/// Returns the sum of visible wire segment length

let totalSegmentLength (sheet : SheetT.Model) =

    let getSegLength (segStart: XYPos) (segEnd : XYPos) =
        BlockHelpers.squaredDistance segStart segEnd



    let doLinesOverlap (seg1Start : XYPos) (seg1End : XYPos) (seg2Start : XYPos) (seg2End : XYPos)  =

        let isPointEqual (p1: XYPos) (p2: XYPos) =
            p1.X = p2.X && p1.Y = p2.Y

        let isCollinear (p1: XYPos) (p2: XYPos) (p3: XYPos) =
            (p2.X - p1.X) * (p3.Y - p1.Y) = (p2.Y - p1.Y) * (p3.X - p1.X)

        let p1 = seg1Start
        let p2 = seg1End
        let p3 = seg2Start
        let p4 = seg2End

        //Check if one endpoint of line1 lies on line2
        let isLine1StartOnLine2 = isCollinear p3 p4 p1 && not(isPointEqual p1 p3) && not(isPointEqual p1 p4)
        let isLine1EndOnLine2 = isCollinear p3 p4 p2 && not (isPointEqual p2 p3) && not (isPointEqual p2 p4)

        // Check if one endpoint of line2 lies on line1 
        let isLine2StartOnLine1 = isCollinear p1 p2 p3 && not (isPointEqual p3 p1) && not (isPointEqual p3 p2)
        let isLine2EndOnLine1 = isCollinear p1 p2 p4 && not (isPointEqual p4 p1) && not (isPointEqual p4 p2)

        // Check if the lines intersect properly (not just endpoints touching) 
        let det = (p2.X - p1.X) * (p4.Y - p3.Y) - (p2.Y - p1.Y) * (p4.X - p3.X)
        let d1 = ((p4.X - p3.X) * (p1.Y - p3.Y) + (p4.Y - p3.Y) * (p1.X - p3.X)) / det
        let d2 = ((p2.X - p1.X) * (p1.Y - p3.Y) + (p2.Y - p1.Y) * (p1.X - p3.X)) / det

        isLine1StartOnLine2 || isLine1EndOnLine2 || isLine2StartOnLine1 || isLine2EndOnLine1 || 0.0 < d1 && d1 < 1.0 && 0.0 < d2 && d2 < 1.0


    let getSegmentVertices (wire: BusWireT.Wire) =
        let (segVecs : XYPos list) = visibleSegments wire.WId sheet
        (wire.StartPos, segVecs) ||> List.scan (fun (currVec : XYPos) (nextVec : XYPos) -> 
                                                {currVec with X = currVec.X+nextVec.X; Y = currVec.Y+nextVec.Y})
                                                
    let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

    let getSegmentVector (index:int) (segStart: XYPos) (segEnd: XYPos) =
            match index, BusWire.getSegmentOrientation segStart segEnd with
            | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> {X=0.; Y= getSegLength segStart segEnd}
            | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> {X=getSegLength segStart segEnd; Y=0.}

    let getAllPairs (group: list<{| segstart : XYPos; segend : XYPos; port : OutputPortId; unitVector : XYPos; index: int |}>) =
        group
        |> List.allPairs group
        |> List.filter (fun (s1: {| index: int; port: OutputPortId; segend: XYPos; segstart: XYPos; unitVector: XYPos |},s2) -> not (s1.index = s2.index) && (s1.index < s2.index))
    
    let segmentTagging (segments: list<XYPos*XYPos>) (outputPort : OutputPortId) =
        List.mapi(fun i (segStart, segEnd) -> {| segstart = segStart; segend = segEnd; port = outputPort; unitVector = (getSegmentVector i segStart segEnd); index=i |}) segments

    let overlapCondition ((group: list<{| segstart : XYPos; segend : XYPos; port : OutputPortId; unitVector : XYPos; index: int |}*{| segstart : XYPos; segend : XYPos; port : OutputPortId; unitVector : XYPos; index: int |}>)) =
        group
        |> List.filter(fun (s1,s2)-> doLinesOverlap s1.segstart s1.segend s2.segstart s2.segend )

    let overlapsInNets =
        BlockHelpers.groupWiresByNet sheet.Wire.Wires
        |> List.map(List.map(fun (w : BusWireT.Wire) -> (w, w.OutputPort, w.InitialOrientation)))
        |> List.map(List.map (fun (w,p,o) -> (getSegmentVertices w |> List.pairwise,p)))
        |> List.map(List.map (fun (s,p) -> segmentTagging s p))
        |> List.concat
        |> List.map getAllPairs
        |> List.map overlapCondition
        |> List.map (List.map(fun (s1,s2) -> 
                                if getSegLength s1.segstart s1.segend < getSegLength s2.segstart s2.segend then s1 else s2
        ))
        |> List.concat
        |> List.distinct


    let validSegments = 
        sheet.Wire.Wires
        |>  Map.toList
        |>List.map snd
        |> List.map((fun (w : BusWireT.Wire) -> (w, w.OutputPort, w.InitialOrientation)))
        |> List.map((fun (w,p,o) -> (getSegmentVertices w |> List.pairwise,p)))
        |> List.map((fun (s,p) -> segmentTagging s p))
        |> List.concat
        |> List.except overlapsInNets
    
    (0.0, validSegments) ||> List.fold(fun s v -> s+ getSegLength v.segstart v.segend)
    |> int 

//T5R
//Returns visible wire right-angles


let visibleWireRightAngles (sheet: SheetT.Model) =

    let getSegmentVertices (wire: BusWireT.Wire) =
        let (segVecs : XYPos list) = visibleSegments wire.WId sheet
        (wire.StartPos, segVecs) ||> List.scan (fun (currVec : XYPos) (nextVec : XYPos) -> 
                                                {currVec with X = currVec.X+nextVec.X; Y = currVec.Y+nextVec.Y})


    let isPerpendicular (seg1Start: XYPos) (seg1End: XYPos) (seg2Start: XYPos) (seg2End: XYPos) =

        let isPointEqual (p1: XYPos) (p2: XYPos) =
            p1.X = p2.X && p1.Y = p2.Y
        
        let p1 = seg1Start
        let p2 = seg1End
        let p3 = seg2Start
        let p4 = seg2End

        //Check if the slopes are negative reciprocals of each other (excluding cases where both slopes are 0 or infinite)
        let m1 = 
            try 
                (p2.Y - p1.Y) / (p2.X - p1.X) 
            with 
                | :? System.DivideByZeroException -> 0.0
        let m2 = 
            try 
                (p4.Y - p3.Y) / (p4.X - p3.X) 
            with 
                | :? System.DivideByZeroException -> 0.0
        let isPerpendicularSlope = (m1 * m2) = -1.0

        (* Additional check to handle cases where one line is horizontal and the other is vertical *)
        let isHorizontalLine1 = isPointEqual p1 p2 && p1.Y <> p2.Y
        let isVerticalLine1 = isPointEqual p1 p2 && p1.X <> p2.X
        let isHorizontalLine2 = isPointEqual p3 p4 && p3.Y <> p4.Y
        let isVerticalLine2 = isPointEqual p3 p4 && p3.X <> p4.X

        isPerpendicularSlope && (isHorizontalLine1 || isVerticalLine1) && (isHorizontalLine2 || isVerticalLine2)

    let segmentStartEnd = 
        sheet.Wire.Wires
        |> Map.toList
        |> List.map snd
        |> List.map getSegmentVertices
        |> List.map(List.pairwise)
        |> List.concat

    let uniqueSegmentPairs = 
        List.allPairs segmentStartEnd segmentStartEnd
        |> List.map (fun (x, y) -> if (fst x).X <= (fst y).X then (x, y) else (y, x)) 
        |> List.distinct

    (0, uniqueSegmentPairs) 
    ||> List.fold(fun s ((seg1Start, seg1End),(seg2Start, seg2End))->
                    if isPerpendicular seg1Start seg1End seg2Start seg2End then s+1 else s)


//T6R
//Returns the zero-length segments in a wire with non-zero segments on either side that have lengths of opposite signs lead to a wire retracing itself

let findSelfRetracingSegments (sheet: SheetT.Model) = 


    let getSegmentVertices (wire: BusWireT.Wire) =
        let (segVecs : XYPos list) = visibleSegments wire.WId sheet
        (wire.StartPos, segVecs) ||> List.scan (fun (currVec : XYPos) (nextVec : XYPos) -> 
                                                {currVec with X = currVec.X+nextVec.X; Y = currVec.Y+nextVec.Y})
    let segmentIntersectsBB (boundingBox : BoundingBox) (startPos: XYPos) (endPos : XYPos) = 
        match BlockHelpers.segmentIntersectsBoundingBox boundingBox startPos endPos with // do not consider the symbols that the wire is connected to
            | Some _ -> true // segment intersects bounding box
            | None -> false // no 

    let symbols = 
        sheet.Wire.Symbol.Symbols
        |> Map.toList
        |> List.map snd
        |> List.map (getSymbolBB)

    let intersectingSegment =
        sheet.Wire.Wires
        |> Map.toList
        |> List.map snd
        |> List.map (getSegmentVertices) 
        |> List.map (List.pairwise)
        |> List.map(List.allPairs symbols)
        |> List.map(List.filter (fun (bb, (segStart, segEnd)) -> segmentIntersectsBB bb segStart segEnd))
        |> List.map(List.map snd)
        |> List.concat
        |> List.distinct

    let overRetracingIndex =
        sheet.Wire.Wires
        |> Map.toList
        |> List.map snd
        |> List.map (getSegmentVertices) 
        |> List.map (List.pairwise)
        |> List.map (fun  l -> [l.[3];l.[(List.length l)-2]])
        |> List.concat
        |> List.mapi (fun i v -> (v,i))
        |> List.filter (fun (s, _)-> List.contains s intersectingSegment)
        |> List.map snd

    let retracementCheck (list: list<BusWireT.Segment>) =
        List.mapi (fun i x -> 
            if i = 0 then [x; list.[i + 1]]
            elif i = list.Length - 1 then [list.[i - 1]; x]
            else [list.[i - 1]; x; list.[i + 1]]) list
        |> List.filter(fun l -> (List.length l) = 3)
        |> List.filter (fun l -> ((l.[0].Length*l.[2].Length)<0) && l.[1].IsZero)
        |> List.map( fun l-> l.[1])

    let spikeList (list: list<BusWireT.Segment>) =
        List.mapi (fun i x -> 
            if ((i = 3) || (i = ((List.length list)-4))) then [list.[i-1]]
            else []       
        ) list
        |> List.concat

    let getSelfRetracingSegments = 
        sheet.Wire.Wires
        |> Map.toList
        |> List.map snd 
        |> List.map (fun w -> w.Segments)
        |> List.map retracementCheck
        |> List.concat
        
    let getOverRetracingSegment =
        sheet.Wire.Wires
        |> Map.toList
        |> List.map snd 
        |> List.map (fun w -> w.Segments)
        |> List.map spikeList
        |> List.concat
        |> List.mapi(fun i v -> (v,i))
        |> List.filter (fun (_,i) -> List.contains i overRetracingIndex)
        |> List.map snd

    (getSelfRetracingSegments, getOverRetracingSegment)
    

    





    

    

    



 
