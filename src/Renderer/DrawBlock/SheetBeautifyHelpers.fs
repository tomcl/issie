module SheetBeautifyHelpers

open CommonTypes
open DrawModelType
open SymbolT
open SheetT
open Symbol
open BlockHelpers
open Optics

// B1R, B1W
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
let setPosition (symbol: Symbol) (newPos: XYPos) : Symbol =
    Optic.set posOfSym_ newPos symbol

// B3R, B3W
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
let getMux2Reverse (symbol: Symbol) =
    symbol.ReversedInputPorts

let setMux2Reverse (symbol: Symbol) (reverse : bool option) =
    {symbol with ReversedInputPorts = reverse}

// B5R
let getPortPos (symbol: Symbol) (port: Port): XYPos =
    let portOffset = Symbol.getPortPos symbol port
    portOffset + symbol.Pos

// B6R
let getBoundBox (symbol: Symbol) : BoundingBox =
    symbol.LabelBoundingBox

// B7R, B7W
let getSymRotation (symbol: Symbol) : Rotation =
    symbol.STransform.Rotation

let setSymRotation (symbol: Symbol) (rotation: Rotation) : Symbol =
    {symbol with STransform = {symbol.STransform with Rotation = rotation}}

// B8R, B8W
let getSymFlip (symbol: Symbol) : bool =
    symbol.STransform.Flipped

let setSymFlip (symbol: Symbol) (flip: bool) : Symbol =
    {symbol with STransform = {symbol.STransform with Flipped = flip}}


// T1R

let countSymbolIntersectSymbols (sheet: SheetT.Model) : int =
            let boxes =
                Helpers.mapValues sheet.BoundingBoxes
                |> Array.toList
                |> List.mapi (fun n box -> n,box)
            List.allPairs boxes boxes 
            |> List.filter (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && overlap2DBox box1 box2)
            |> List.length

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

// T2R
let countWireIntersectSymbols (model: SheetT.Model) : int =
    let boxes =
        Helpers.mapValues model.BoundingBoxes
        |> Array.toList

    let WireIntersectSymbol ((box,seg): BoundingBox*BusWireT.ASegment) =
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
let countCrossingWires (model:SheetT.Model) : int =    
    let segList = 
        model.Wire.Wires
        |> Helpers.mapValues
        |> Array.toList
        |> List.collect (fun wire -> getNonZeroAbsSegments wire)
        
    let segmentsNotLinked (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) =
        not (seg1.Start = seg2.Start || seg1.Start = seg2.End || seg1.End = seg2.Start || seg1.End = seg2.End)

    let segmentsCross (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) =
        overlap2D (seg1.Start, seg1.End) (seg2.Start, seg2.End) && segmentsNotLinked seg1 seg2
        
    List.allPairs segList segList
    |> List.filter (fun (seg1, seg2) -> (seg1 <> seg2) && segmentsCross seg1 seg2)
    |> List.length


// T4R
let visibleWireLength (model: SheetT.Model) : float =

    let hasOverlap (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) =
        let seg1Box = (seg1.Start, seg1.End)
        let seg2Box = (seg2.Start, seg2.End)
        overlap2D seg1Box seg2Box

    let horizontalSegments, verticalSegments =
        model.Wire.Wires
        |> Helpers.mapValues
        |> Array.toList
        |> List.collect getNonZeroAbsSegments
        |> List.partition (fun seg -> seg.Start.X = seg.End.X)

    let splitByOverlaps segList =
        let comparingList = segList
        segList
        |> List.partition (fun seg -> List.exists (hasOverlap seg) comparingList)

    let getOverlapLength (segList: BusWireT.ASegment list)=
        let minStartSeg =
            segList
            |> List.minBy (fun seg -> seg.Start.X + seg.Start.Y)
        let maxEndSeg =
            segList
            |> List.maxBy (fun seg -> seg.End.X + seg.End.Y)
        let lengthXY =  maxEndSeg.End - minStartSeg.Start
        abs(lengthXY.X + lengthXY.Y)

    let OverlapsLengthH (segList: BusWireT.ASegment list)=
        segList
        |> List.groupBy (fun seg -> seg.Start.X)
        |> List.collect snd
        |> getOverlapLength

    let OverlapsLengthV (segList: BusWireT.ASegment list)=
        segList
        |> List.groupBy (fun seg -> seg.Start.X)
        |> List.collect snd
        |> getOverlapLength

    let sumSegments (segList: BusWireT.ASegment list)=
        segList
        |> List.map (fun seg -> abs(seg.Start.X - seg.End.X) + abs(seg.Start.Y - seg.End.Y))
        |> List.sum

    let horizontalLength =
        horizontalSegments
        |> splitByOverlaps
        |> (fun (overlaps,noOverlaps) -> (sumSegments noOverlaps) + (OverlapsLengthH overlaps))

    let verticalLength =
        verticalSegments
        |> splitByOverlaps
        |> (fun (overlaps,noOverlaps) -> (sumSegments noOverlaps) + (OverlapsLengthV overlaps))

    horizontalLength + verticalLength
    

// T5R
let CountWireRightAngles (model: SheetT.Model) : int =
    model.Wire.Wires
    |> Map.toList
    |> List.map fst
    |> List.collect (fun wId -> model.Wire.Wires[wId].Segments)
    |> List.fold (fun acc seg -> if seg.Length = 0 then acc - 2 else acc + 1) -1

// T6R
