module SheetBeautifyHelpers

open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open Optics
open Optic
open Operators
open Helpers
open BlockHelpers

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



// B1R, B1W
// The dimensions of a custom component symbol
let getSymbolDimensions (symbol: Symbol) : (float * float) =
    (symbol.Component.W, symbol.Component.H)

let setSymbolDimensions (symbol: Symbol) (width, height) =
    { symbol with Component = { symbol.Component with H = height; W = width }}



// B2W
// The position of a symbol on the sheet
// TODO: Is this too simple? What are the other things to take into consideration?
let setSymbolPosition (symbol: Symbol) (pos : XYPos) =
    symbol |> set posOfSym_ pos



// B3R, B3W
// Read/write the order of ports on a specified side of a symbol
let getSymbolPortOrder (symbol : Symbol) (side : Edge) : string list =
    symbol.PortMaps.Order[side]

let setSymbolPortOrder (symbol : Symbol) (side : Edge) (portOrder : string list) : Symbol =
    { symbol with PortMaps = {symbol.PortMaps with Order = symbol.PortMaps.Order.Add(side, portOrder)}}


// B4R, B4W
// The reverses state of the inputs of a MUX2
let getMux2Reversed (mux2 : Symbol) : bool option =
    mux2.ReversedInputPorts

let setMux2Reversed (mux2 : Symbol) (reversed : bool option) : Symbol =
    { mux2 with ReversedInputPorts = reversed }


// B5R
// The position of a port on the sheet. It cannot directly be written
let getPortPosition (symbol: Symbol) (port: Port) : XYPos =
    Symbol.getPortPos symbol port


// B6R
// The Bounding box of a symbol outline (position is contained in this)
let getSymbolBoundingBox (symbol: Symbol) : BoundingBox =
    Symbol.getSymbolBoundingBox symbol


// B7R, B7W
// The rotation state of a symbol
let getSymbolRotation (symbol: Symbol) : Rotation =
    symbol.STransform.Rotation

let setSymbolRotation (symbol: Symbol) (rotation: Rotation) : Symbol =
    { symbol with STransform = {symbol.STransform with Rotation = rotation} }


// B8R, B8W
// The flip state of a symbol
let getSymbolFlip (symbol: Symbol) : bool =
    symbol.STransform.Flipped

let setSymbolFlip (symbol: Symbol) (flip: bool) : Symbol =
    { symbol with STransform = {symbol.STransform with Flipped = flip} }



// T1R
// The number of pairs of symbols that intersect each other. See Tick3 for a related function. Count over all pairs of symbols.
let getIntersectingSymbols (sheet: SheetT.Model) : int =
    let boxes =
        mapValues sheet.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> (n, box))

    List.allPairs boxes boxes
    |> List.filter (fun ((i, box1), (j, box2)) -> i < j && BlockHelpers.overlap2DBox box1 box2)
    |> List.length


// T2R
// The number of distinct wire visible segments that intersect with one or more 
// symbols. See Tick3.HLPTick3.visibleSegments for a helper. Count over all visible wire segments.
let getIntersectingWireSegments (sheet: SheetT.Model) : int =
    let wireModel = sheet.Wire

    wireModel.Wires
    |> Map.filter (fun _ wire -> BusWireRoute.findWireSymbolIntersections wireModel wire <> [])
    |> Map.keys
    |> Seq.toList
    |> List.map (fun wId -> visibleSegments wId sheet)
    |> List.sumBy (fun segs -> segs |> List.length)



// T3R
// The number of distinct pairs of segments that cross each other at right angles. Does 
// not include 0 length segments or segments on same net intersecting at one end, or 
// segments on same net on top of each other. Count over whole sheet
let getRightAngleCrossings (sheet: SheetT.Model) : int =
    let allWires = List.collect getNonZeroAbsSegments (Seq.toList sheet.Wire.Wires.Values)

    let horizontalSegments = allWires |> List.filter (fun aSeg -> aSeg.Orientation = Horizontal)
    let verticalSegments = allWires |> List.filter (fun aSeg -> aSeg.Orientation = Vertical)

    let getRightAngles (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) =
        if seg1.Orientation = Horizontal && seg2.Orientation = Vertical then
            if seg1.Start.X < seg2.Start.X && seg1.End.X > seg2.Start.X && seg2.Start.Y < seg1.Start.Y && seg2.End.Y > seg1.Start.Y then 1
            else 0
        else if seg1.Orientation = Vertical && seg2.Orientation = Horizontal then
            if seg2.Start.X < seg1.Start.X && seg2.End.X > seg1.Start.X && seg1.Start.Y < seg2.Start.Y && seg1.End.Y > seg2.Start.Y then 1
            else 0
        else 0

    let rightAngles = List.allPairs horizontalSegments verticalSegments |> List.sumBy (fun (seg1, seg2) -> getRightAngles seg1 seg2)

    rightAngles


// T4R
// Sum of wiring segment length, counting only one when there are N same-net 
// segments overlapping (this is the visible wire length on the sheet). Count over whole 
// sheet.
let getVisibleWireLength (sheet: SheetT.Model) : float =
    let allWires = List.collect getNonZeroAbsSegments (Seq.toList sheet.Wire.Wires.Values)

    let horizontalSegments = allWires |> List.filter (fun aSeg -> aSeg.Orientation = Horizontal)
    let verticalSegments = allWires |> List.filter (fun aSeg -> aSeg.Orientation = Vertical)

    let horizontalLength = horizontalSegments |> List.sumBy (fun seg -> seg.Segment.Length)
    let verticalLength = verticalSegments |> List.sumBy (fun seg -> seg.Segment.Length)


    let getOverlapLength (start1, end1) (start2, end2) =
        if start1 < start2 then
            if end1 < start2 then 0.
            else if end1 < end2 then end1 - start2
            else end2 - start2
        else
            if end2 < start1 then 0.
            else if end2 < end1 then end2 - start1
            else end1 - start1

    let getOverlapXLength (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) =
        getOverlapLength (seg1.Start.X, seg1.End.X) (seg2.Start.X, seg2.End.X)

    let getOverlapYLength (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) =
        getOverlapLength (seg1.Start.Y, seg1.End.Y) (seg2.Start.Y, seg2.End.Y)

    // for horizontal segments, we need to remove the overlap
    // we check if two segments have the same y position. If they do, we subtract horizontalLength from the overlapped x length
    let finalhorizontalLength = (List.fold (fun acc seg -> 
        let overlap = horizontalSegments |> List.filter (fun aSeg -> aSeg.Start.Y = seg.Start.Y)
        let overlapLength = overlap |> List.sumBy (fun aSeg -> getOverlapXLength aSeg seg)
        acc - overlapLength
    ) horizontalLength horizontalSegments)

    // similar to horizontal segments
    let finalVerticalLength = (List.fold (fun acc seg -> 
        let overlap = verticalSegments |> List.filter (fun aSeg -> aSeg.Start.X = seg.Start.X)
        let overlapLength = overlap |> List.sumBy (fun aSeg -> getOverlapYLength aSeg seg)
        acc - overlapLength
    ) verticalLength verticalSegments)

    finalhorizontalLength + finalVerticalLength


// T5R
// Number of visible wire right-angles. Count over whole sheet.
let getVisibleWireRightAngles (sheet: SheetT.Model) : int =
    let allWires = List.map getNonZeroAbsSegments (Seq.toList sheet.Wire.Wires.Values)
    let wireNumber = List.length allWires

    // we will add up the non-zero segments in each wire while removing duplicates
    // since each segment except the first one means a right angle
    let rightAngles =
        allWires
        |> List.collect id // flatten the list
        |> Set.ofList // remove duplicates
        |> Set.count // count the number of segments

    rightAngles - wireNumber
    


// T6R
// The zero-length segments in a wire with non-zero segments on either side that have 
// Lengths of opposite signs lead to a wire retracing itself. Note that this can also apply 
// at the end of a wire (where the zero-length segment is one from the end). This is a 
// wiring artifact that should never happen but errors in routing or separation can 
// cause it. Count over the whole sheet. Return from one function a list of all the 
// segments that retrace, and also a list of all the end of wire segments that retrace so 
// far that the next segment (index = 3 or Segments.Length – 4) - starts inside a symbol.


