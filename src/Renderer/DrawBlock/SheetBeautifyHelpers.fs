module SheetBeautifyHelpers

open CommonTypes
open DrawModelType
open SymbolT
open SheetT
open Optics
open Symbol
open BlockHelpers



///B1R, dimension should be given in width * height
let getCustomSymbolDimension (symbol: SymbolT.Symbol): (float option*float option) =
    (symbol.HScale, symbol.VScale)
///B1W, dimension should be given in width * height, no option for input since no need to specifically write none
let writeCustomSymbolDimension(dimension:float option*float option) (symbol: SymbolT.Symbol): SymbolT.Symbol =
    {symbol with HScale = fst dimension; VScale = snd dimension}

///B1R B1W, The dimensions of a custom component symbol
let CustomeSymbolDimension_ = Lens.create getCustomSymbolDimension writeCustomSymbolDimension

/// B2W: The position of a symbol on the sheet
let writeSymbolPosition (position: XYPos) (symbolID: ComponentId) (sheetModel: SheetT.Model): SheetT.Model =
    let symbol = sheetModel.Wire.Symbol.Symbols[symbolID]
    let newSymbol = {symbol with Pos = position}
    {sheetModel with Wire = {sheetModel.Wire with Symbol = {sheetModel.Wire.Symbol with Symbols = sheetModel.Wire.Symbol.Symbols.Add(symbolID, newSymbol)}}}


///B3R: Read the order of ports on a specified side of a symbol
let readPortsOrder (symbol: SymbolT.Symbol) (edge: Edge)  : string list =
    symbol.PortMaps.Order[edge] 

///B3W: write the order of ports on a specified side of a symbol
let writePortsOrder (edge: Edge) (order: string list) (symbol: SymbolT.Symbol): SymbolT.Symbol =
    {symbol with PortMaps = {symbol.PortMaps with Order = symbol.PortMaps.Order.Add(edge, order)}}

//let PortsOrder_ = Lens.create readPortsOrder writePortsOrder

/// B4R
let getReversedInputPortsMUX2 (symbol: SymbolT.Symbol): bool option =
    symbol.ReversedInputPorts
/// B4W
let writeReversedInputPortsMUX2 (reversed: bool option) (symbol: SymbolT.Symbol): SymbolT.Symbol =
    {symbol with ReversedInputPorts = reversed}
/// B4R B4W: The reverses state of the inputs of a MUX2
let ReversedInputPortsMUX2_ = Lens.create getReversedInputPortsMUX2 writeReversedInputPortsMUX2

/// B5R: The position of a port on the sheet. It cannot directly be written.
let getPortPositionOnSheet (portId: PortId) (sheetModel: SheetT.Model): XYPos =
    getPortIdStr portId|>
    getPortLocation None sheetModel.Wire.Symbol

/// B6R: The Bounding box of a symbol outline (position is contained in this)
let getSymbolBoundingBoxOutline (symbol: SymbolT.Symbol): BoundingBox =
    getSymbolBoundingBox symbol

/// B7R
let getSymbolRotationState (symbol: SymbolT.Symbol): Rotation =
    symbol.STransform.Rotation
/// B7W
let writeSymbolRotationState (rotation: Rotation) (symbol: SymbolT.Symbol): SymbolT.Symbol =
    {symbol with STransform = {symbol.STransform with Rotation = rotation}}
///B7R B7W: The rotation state of a symbol
let SymbolRotationState_ = Lens.create getSymbolRotationState writeSymbolRotationState

/// B8R
let getSymbolFlipState (symbol: SymbolT.Symbol): bool =
    symbol.STransform.Flipped
/// B8W
let writeSymbolFlipState (flipped: bool) (symbol: SymbolT.Symbol): SymbolT.Symbol =
    {symbol with STransform = {symbol.STransform with Flipped = flipped}}
///B8R B8W: The flip state of a symbol
let SymbolFlipState_ = Lens.create getSymbolFlipState writeSymbolFlipState


///T1R: The number of pairs of symbols that intersect each other. See Tick3 for a related
///function. Count over all pairs of symbols
let numberSymbolIntersection (sheetModel: SheetT.Model): int =
    let SymbolBoundingBoxs: BoundingBox list = 
        sheetModel.Wire.Symbol.Symbols 
        |> Map.toList
        |> List.map (fun (id, symbol) -> getSymbolBoundingBoxOutline symbol)

    List.allPairs SymbolBoundingBoxs SymbolBoundingBoxs
    |> List.filter (fun (bb1, bb2) -> (bb1 <> bb2) && (overlap2DBox bb1 bb2))
    |> List.length

///T2R: The number of distinct wire visible segments that intersect with one or more
///symbols. See Tick3.HLPTick3.visibleSegments for a helper. Count over all visible wire
///segments.
let numberWireSegmentsIntersectSymbol (sheetModel: SheetT.Model):int =
    let wireSegments =
        sheetModel.Wire.Wires
        |> Map.toList
        |> List.collect (fun (id, wire) -> getNonZeroAbsSegments wire)
    let symbolBoundingBoxs: BoundingBox list =
        sheetModel.Wire.Symbol.Symbols
        |> Map.toList
        |> List.map (fun (id, symbol) -> getSymbolBoundingBoxOutline symbol)

    let whetherIntersectSymbol (seg: BusWireT.ASegment): bool =
        symbolBoundingBoxs
        |> List.map(fun bb -> segmentIntersectsBoundingBox bb seg.Start seg.End)
        |> List.filter (fun x ->x.IsSome )
        |> List.length
        |> (fun x -> x > 0)
    wireSegments
    |> List.filter (fun seg -> whetherIntersectSymbol seg)
    |> List.length

///T3R: The number of distinct pairs of segments that cross each other at right angles. Does
///not include 0 length segments or segments on same net intersecting at one end, or
///segments on same net on top of each other. Count over whole sheet.
let numberWireSegementsIntersectingAtRightAngle (sheetModel: SheetT.Model): int =
    let wireSegments =
        sheetModel.Wire.Wires
        |> Map.toList
        |> List.map (fun (id, wire) -> (getNonZeroAbsSegments wire, wire.InitialOrientation))
        |> List.collect (fun (segs, orientation) -> segs |> List.map (fun seg -> (seg, orientation)))
    let rightAngleIntersect (seg1: BusWireT.ASegment * BusWireT.Orientation) (seg2: BusWireT.ASegment * BusWireT.Orientation): bool =
        if (snd seg1) = (snd seg2) then
            false
        else
            overlap2D ((fst seg1).Start,(fst seg1).End) ((fst seg2).Start,(fst seg2).End)
    List.allPairs wireSegments wireSegments
    |> List.filter (fun (seg1, seg2) -> rightAngleIntersect seg1 seg2)
    |> List.length

///Sum of wiring segment length, counting only one when there are N same-net
///segments overlapping (this is the visible wire length on the sheet). Count over whole
///sheet.
let sumWireSegmentLengh (sheetModel: SheetT.Model): float =
    let wireSegments =
        sheetModel.Wire.Wires
        |> Map.toList
        |> List.map (fun (id, wire) -> (getNonZeroAbsSegments wire, wire.InitialOrientation))
        |> List.collect (fun (segs, orientation) -> segs |> List.map (fun seg -> (seg, orientation)))
    let calculateSegmentOverlapLength (seg1: BusWireT.ASegment * BusWireT.Orientation) (seg2: BusWireT.ASegment * BusWireT.Orientation): float =
        if (snd seg1) <> (snd seg2) then
            0.
        else
            let overlap = overlap2D ((fst seg1).Start,(fst seg1).End) ((fst seg2).Start,(fst seg2).End)
            if overlap then
                let seg1Length = max (fst seg1).Start (fst seg1).End - min (fst seg1).Start (fst seg1).End
                let seg2Length = max (fst seg2).Start (fst seg2).End - min (fst seg2).Start (fst seg2).End

                let maxStart = max (fst seg2).Start (fst seg1).Start
                let maxEnd = max (fst seg2).End (fst seg1).End
                let maxPosition = max maxStart maxEnd
                let minStart = min (fst seg2).Start (fst seg1).Start
                let minEnd = min (fst seg2).End (fst seg1).End
                let minPosition = min minStart minEnd

                let overlapLength = (maxPosition - minPosition)

                let overlapSegment = seg1Length + seg2Length - overlapLength
                overlapSegment.X + overlapSegment.Y
            else
                0.
    let totalSegemntOverlapLength =
        List.allPairs wireSegments wireSegments
        |> List.map (fun (seg1, seg2) -> calculateSegmentOverlapLength seg1 seg2)
        |> List.sum
        |> (fun x -> x / 2.)
    wireSegments
    |> List.map (fun (seg, _) -> seg.Segment.Length)
    |> List.sum
    |> (fun x -> x - totalSegemntOverlapLength)

///T5R: Number of visible wire right-angles. Count over whole sheet.
let numberWireRightAngles (sheetModel: SheetT.Model): int =
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
        /// Index must be in range 1..segVecs
        let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int)  =
            if segVecs[index] =~ XYPos.zero
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
    sheetModel.Wire.Wires
    |> Map.toList
    |> List.map (fun (id, wire) -> visibleSegments id sheetModel)
    |> List.map (fun segs -> (List.length segs) - 1 )
    |> List.sum

///T6R: The zero-length segments in a wire with non-zero segments on either side that have
///Lengths of opposite signs lead to a wire retracing itself. Note that this can also apply
///at the end of a wire (where the zero-length segment is one from the end). This is a
///wiring artifact that should never happen but errors in routing or separation can
///cause it. Count over the whole sheet. Return from one function a list of all the
///segments that retrace, and also a list of all the end of wire segments that retrace so
///far that the next segment (index = 3 or Segments.Length – 4) - starts inside a symbol.
let retracingSegements (sheetModel: SheetT.Model) : (BusWireT.Segment list * BusWireT.Segment list) =
    let allWires = 
        sheetModel.Wire.Wires 
        |> Map.toList
        |> List.map (fun (id, wire) -> wire)
    
    let getAllRetracingSegments (wire: BusWireT.Wire) : (BusWireT.Segment list) =
        let isRetracing (startSeg : BusWireT.Segment) (midSeg: BusWireT.Segment) (endSeg: BusWireT.Segment) : bool =
            if midSeg.Length <> 0. then
                false
            else
                (startSeg.Length >0. && endSeg.Length < 0.) || (startSeg.Length < 0. && endSeg.Length > 0.) 
        wire.Segments
            |> List.windowed 3
            |> List.map (fun segs -> segs.[0], segs.[1], segs.[2])
            |> List.filter (fun (startSeg, midSeg, endSeg) -> isRetracing startSeg midSeg endSeg)
            |> List.map (fun (startSeg, midSeg, endSeg) -> endSeg )
    
    let getEndRetracingSegments (wire: BusWireT.Wire) : (BusWireT.Segment list) =
        let isRetracing (index0Seg : BusWireT.Segment) (index1Seg: BusWireT.Segment) (index2Seg: BusWireT.Segment) : bool =
            if index1Seg.Length <> 0. then
                false
            else
                (index0Seg.Length >0. && index2Seg.Length < 0. && index0Seg.Length + index2Seg.Length <0.) || (index0Seg.Length < 0. && index2Seg.Length > 0. && index0Seg.Length + index2Seg.Length >0.) 
        let startingTriple = 
            wire.Segments
                |> List.windowed 3
                |> List.head
        let endingTriple =
            wire.Segments
                |> List.windowed 3
                |> List.last
                |> List.rev
        [startingTriple; endingTriple]
            |> List.map (fun segs -> segs.[0], segs.[1], segs.[2])
            |> List.filter (fun (startSeg, midSeg, endSeg) -> isRetracing startSeg midSeg endSeg)
            |> List.map (fun (startSeg, midSeg, endSeg) -> endSeg )

    let allRetracingSegments = 
        allWires 
        |> List.map getAllRetracingSegments 
        |> List.concat
    let allEndRetracingSegments = 
        allWires 
        |> List.map getEndRetracingSegments 
        |> List.concat
    (allRetracingSegments, allEndRetracingSegments)
