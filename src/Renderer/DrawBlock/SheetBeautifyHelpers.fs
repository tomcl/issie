module SheetBeautifyHelpers

open CommonTypes
open DrawModelType
open SymbolT
open SheetT
open Symbol
open BlockHelpers
open Optics
open BusWireT

// Moved from TestDrawBlock
//-----------------------------------------------------------------------------------------------
// visibleSegments is included here as ahelper for info, and because it is needed in project work
//-----------------------------------------------------------------------------------------------

/// The visible segments of a wire, as a list of vectors, from source end to target end.
/// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed 
/// which if present causes the two segments on either side of it to coalesce into a single visible segment.
/// A wire can have any number of visible segments - even 1.
let visibleSegments (model: SheetT.Model) (wId: ConnectionId): XYPos list =

    let wire = model.Wire.Wires[wId] // get wire from model

    /// helper to match even and off integers in patterns (active pattern)
    let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

    /// Convert seg into its XY Vector (from start to end of segment).
    /// index must be the index of seg in its containing wire.
    let getSegmentVector (index:int) (seg: Segment) =
        // The implicit horizontal or vertical direction  of a segment is determined by 
        // its index in the list of wire segments and the wire initial direction
        match index, wire.InitialOrientation with
        | IsEven, Vertical | IsOdd, Horizontal -> {X=0.; Y=seg.Length}
        | IsEven, Horizontal | IsOdd, Vertical -> {X=seg.Length; Y=0.}

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
let getCustDimensions (symbol: Symbol) : float * float =
    let width = Optic.get w_ symbol.Component
    let height = Optic.get h_ symbol.Component
    width,height

let setCustDimensions (symbol: Symbol) ((width,height) : float*float) : Symbol =
    let newComponent =
        symbol.Component
        |> Optic.set w_ (width)
        |> Optic.set h_ (height)
    {symbol with Component = newComponent}

// B2W
// The position of a symbol on the sheet 
let setPosition (symbol: Symbol) (newPos: XYPos) : Symbol =
    Optic.set posOfSym_ newPos symbol

// B3R, B3W
// Read/write the order of ports on a specified side of a symbol 
let getPortOrder (symbol: Symbol) (side: Edge) : string list option =
    Map.tryFind side symbol.PortMaps.Order

let setPortOrder (symbol: Symbol) (side: Edge) (newOrder: string list) : Symbol =
    let portMaps = symbol.PortMaps
    let portsOrder = portMaps.Order
    let sideExists = getPortOrder symbol side
    match sideExists with
    | Some portOrder -> {symbol with PortMaps = Optic.set order_ (Map.add side newOrder portsOrder) portMaps}
    | None -> failwithf $"No ports on symbol side: side='{side}', Symbol='{symbol}"

// B4R, B4W
// The reverses state of the inputs of a MUX2 
let getMux2Reverse (symbol: Symbol) =
    symbol.ReversedInputPorts

let setMux2Reverse (symbol: Symbol) (reverse : bool option) =
    {symbol with ReversedInputPorts = reverse}

// B5R
// The position of a port on the sheet. It cannot directly be written. 
let getPortPos (symbol: Symbol) (port: Port): XYPos =
    let portOffset = Symbol.getPortPos symbol port
    portOffset + symbol.Pos

// B6R
// The Bounding box of a symbol outline (position is contained in this) 
let getBoundBox (symbol: Symbol) : BoundingBox =
    symbol.LabelBoundingBox

// B7R, B7W
// The rotation state of a symbol 
let getSymRotation (symbol: Symbol) : Rotation =
    symbol.STransform.Rotation

let setSymRotation (symbol: Symbol) (rotation: Rotation) : Symbol =
    {symbol with STransform = {symbol.STransform with Rotation = rotation}}

// B8R, B8W
// The flip state of a symbol 
let getSymFlip (symbol: Symbol) : bool =
    symbol.STransform.Flipped

let setSymFlip (symbol: Symbol) (flip: bool) : Symbol =
    {symbol with STransform = {symbol.STransform with Flipped = flip}}


// T1R
// The number of pairs of symbols that intersect each other. See Tick3 for a related function. Count over all pairs of symbols. 
let countSymbolIntersectSymbols (sheet: SheetT.Model) : int =
            let boxes =
                Helpers.mapValues sheet.BoundingBoxes
                |> Array.toList
                |> List.mapi (fun n box -> n,box)
            List.allPairs boxes boxes 
            |> List.filter (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && overlap2DBox box1 box2)
            |> List.length

// T2R
// The number of distinct wire visible segments that intersect with one or more symbols. See Tick3.HLPTick3.visibleSegments for a helper.
// Count over all visible wire segments. 
let countWireIntersectSymbols (model: SheetT.Model) : int =
    let boxes =
        Helpers.mapValues model.BoundingBoxes
        |> Array.toList

    let WireIntersectSymbol ((box,seg): BoundingBox*ASegment) =
        let segAsBox = {TopLeft = seg.Start; W = 1; H =1}
        overlap2DBox segAsBox box

    model.Wire.Wires
    |> Helpers.mapValues
    |> Array.toList
    |> List.collect (fun wire -> getNonZeroAbsSegments wire)
    |> List.allPairs boxes
    |> List.filter WireIntersectSymbol
    |> List.length
    
// T3R
// The number of distinct pairs of segments that cross each other at right angles. Does not include 0 length segments or segments on same
// net intersecting at one end, or segments on same net on top of each other. Count over whole sheet. 
let countCrossingWires (model:SheetT.Model) : int =    
    let segList = 
        model.Wire.Wires
        |> Helpers.mapValues
        |> Array.toList
        |> List.collect (fun wire -> getNonZeroAbsSegments wire)
        
    let segmentsNotLinked (seg1: ASegment) (seg2: ASegment) =
        not (seg1.Start = seg2.Start || seg1.Start = seg2.End || seg1.End = seg2.Start || seg1.End = seg2.End)

    let segmentsCross (seg1: ASegment) (seg2: ASegment) =
        overlap2D (seg1.Start, seg1.End) (seg2.Start, seg2.End) && segmentsNotLinked seg1 seg2
        
    List.allPairs segList segList
    |> List.filter (fun (seg1, seg2) -> (seg1 <> seg2) && segmentsCross seg1 seg2)
    |> List.length


// T4R
// Sum of wiring segment length, counting only one when there are N same-net segments overlapping (this is the visible wire length on the sheet).
// Count over whole sheet. 
let visibleWireLength (model: SheetT.Model) : float =

    let hasOverlap (seg1: ASegment) (seg2: ASegment) =
        let seg1Box = (seg1.Start, seg1.End)
        let seg2Box = (seg2.Start, seg2.End)
        overlap2D seg1Box seg2Box

    let horizontalSegments, verticalSegments =
        model.Wire.Wires
        |> Helpers.mapValues
        |> Array.toList
        |> List.collect getNonZeroAbsSegments
        |> List.partition (fun seg -> seg.Start.X = seg.End.X)

    // split into a two lists based on whether a segment overlaps another in the list
    let splitByOverlaps segList =
        let comparingList = segList
        segList
        |> List.partition (fun seg -> List.exists (hasOverlap seg) comparingList)

    // finds the total length of a list of overlap wires
    let getOverlapLength (segList: ASegment list)=
        let minStartSeg =
            segList
            |> List.minBy (fun seg -> seg.Start.X + seg.Start.Y)
        let maxEndSeg =
            segList
            |> List.maxBy (fun seg -> seg.End.X + seg.End.Y)
        let lengthXY =  maxEndSeg.End - minStartSeg.Start
        abs(lengthXY.X + lengthXY.Y)

    // groups horizontal overlaps by their starting point and calculates overall length
    let HorizontalOverlapsLength (segList: ASegment list)=
        segList
        |> List.groupBy (fun seg -> seg.Start.X)
        |> List.collect snd
        |> getOverlapLength

    // groups vertical overlaps by their starting point and calculates overall length
    let VerticalOverlapsLength (segList: ASegment list)=
        segList
        |> List.groupBy (fun seg -> seg.Start.X)
        |> List.collect snd
        |> getOverlapLength

    let sumSegments (segList: ASegment list)=
        segList
        |> List.map (fun seg -> abs(seg.Start.X - seg.End.X) + abs(seg.Start.Y - seg.End.Y))
        |> List.sum

    let horizontalLength =
        horizontalSegments
        |> splitByOverlaps
        |> (fun (overlaps,noOverlaps) -> (sumSegments noOverlaps) + (HorizontalOverlapsLength overlaps))

    let verticalLength =
        verticalSegments
        |> splitByOverlaps
        |> (fun (overlaps,noOverlaps) -> (sumSegments noOverlaps) + (VerticalOverlapsLength overlaps))

    horizontalLength + verticalLength
    

// T5R
// Number of visible wire right-angles. Count over whole sheet.
let countWireRightAngles (model: SheetT.Model) : int =
    model.Wire.Wires
    |> Map.toList
    |> List.map fst
    |> List.collect (fun wId -> model.Wire.Wires[wId].Segments)
    |> List.fold (fun acc seg -> if seg.Length = 0 then acc - 2 else acc + 1) -1

// T6R
// The zero-length segments in a wire with non-zero segments on either side that have Lengths of opposite signs lead to a wire retracing itself.
// Note that this can also apply at the end of a wire (where the zero-length segment is one from the end). This is a wiring artifact that should
// never happen but errors in routing or separation can cause it. Count over the whole sheet. Return from one function a list of all the segments
// that retrace, and also a list of all the end of wire segments that retrace so far that the next segment (index = 3 or Segments.Length – 4) -
// starts inside a symbol.
// Returns 
let retracingSegments (model: SheetT.Model) : (Segment list * Segment list) =

    let get3ConsecutiveSegments (segList: Segment list) = 
        List.windowed 3 segList

    // checks for retrace at the beginning and end of segment list
    let hasRetraceInSymbol (comb: Segment list) =
        if (comb[0].Length = 0 && (comb[1].Length *  comb[2].Length) < 0)// eg. [0;-5;10]
        then [comb[0]]
        else if (comb[2].Length = 0 && (comb[0].Length *  comb[1].Length) < 0)// eg. [5;-10;0]
        then [comb[2]]
        else []

    // checks for retrace in the middle of segment list
    let hasRetrace (comb: Segment list) =
        if (comb[1].Length = 0 && (comb[0].Length * comb[2].Length) < 0) // eg. [5;0;-10], [-5;0;10]
        then [comb[1]]
        else []

    let possibleRetracingWires =
        model.Wire.Wires
        |> Map.toList
        |> List.map fst
        |> List.map (fun wId -> model.Wire.Wires[wId].Segments)
        |> List.map get3ConsecutiveSegments

    let retraceInSymbol =
        let lastCombination = List.last possibleRetracingWires
        [possibleRetracingWires[0]; lastCombination]
        |> List.collect (fun combinationList -> List.collect hasRetraceInSymbol combinationList)

    let allRetrace =
        possibleRetracingWires
        |> List.collect (fun combinationList -> List.collect hasRetrace combinationList)
        |> List.append retraceInSymbol

    allRetrace, retraceInSymbol

