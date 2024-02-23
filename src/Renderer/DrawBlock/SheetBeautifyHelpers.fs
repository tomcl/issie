module SheetBeautifyHelpers

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

open BlockHelpers
open CommonTypes
open SymbolHelpers
open SymbolUpdate
open DrawModelType
open Optics
open Operators
open Symbol
open RotateScale
open Helpers
open BusWireRoutingHelpers

////// Helper functions //////

/// Copied from TestDrawBlock
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

/// Returns all unique pairs of elements from a list.
let rec uniquePairs (lst: 'a list) =
    match lst with
    | [] -> []
    | h::t -> List.map (fun x -> (h, x)) t @ uniquePairs t



////// Requested functions begin here //////

/// A lens for the dimensions of a custom component symbol.
let customComponentDims_ =

    let get (sym: SymbolT.Symbol) : XYPos =
        match sym.Component.Type with
        | Custom _ -> (getCustomSymCorners sym)[2]
        | _ -> {X=0; Y=0} // Shouldn't match

    let set (dims: XYPos) (symbol: SymbolT.Symbol) : SymbolT.Symbol =
        /// Code adapted from manualSymbolResize
        let symPos = Optic.get SymbolT.posOfSym_ symbol
        let comp = symbol.Component 
        let scale = Optic.get SymbolT.scaleF_ symbol

        /// Componentwise multiply
        let outerProduct ({X=x;Y=y}:XYPos) ({X=x1;Y=y1}:XYPos) = {X=x*x1;Y=y*y1}

        /// True if symbol is rotated 90 or 270 degrees swapping X,Y box dimensions
        let symXyAreSwapped =
            match symbol.STransform.Rotation with
            | Degree0 | Degree180 -> false
            | Degree90 | Degree270 -> true

        /// Does not include rotation or scaling
        let compBox = {X=comp.W; Y=comp.H}

        /// Function will return X,Y swapped iff symbols is rotated 90 or 270 degrees
        let swapXYByRot ({X=x;Y=y}:XYPos) =
            if symXyAreSwapped then {X=y; Y=x} else {X=x; Y=y}

        /// Correct dimensions for scaled and unscaled component box
        let scaledSymBox, symBox =
            outerProduct compBox scale |> swapXYByRot,
            compBox |> swapXYByRot

        /// Vector outer product
        let outerProduct (a: XYPos) (b: XYPos) = {X=a.X*b.X; Y=a.Y*b.Y}

        /// apply function f to both the components of xy
        let xyApplyF (f: float -> float) (xy: XYPos) =
            {X = f xy.X; Y = f xy.Y}

        /// Indicates for X & Y is the diagonal dimension negative
        let invert = xyApplyF (sign >> float) dims

        /// diagonal with abs value of each component
        let posDiag = outerProduct dims invert
        /// difference between current and required moving corner position.
        /// signs pos or neg.
        let deltaDiag = posDiag - scaledSymBox
        let scale' = {X=posDiag.X/symBox.X; Y = posDiag.Y/symBox.Y}

        /// Adjustment to top left corner symbol position
        /// to keep fixed point (which may not be top left) fixed
        let posDelta: XYPos =
            match int invert.X, int invert.Y with
            | 1, -1 ->     0.,          -deltaDiag.Y
            | -1, 1 ->    -deltaDiag.X,  0.
            | -1, -1 ->   -deltaDiag.X, -deltaDiag.Y
            | 1, 1 | _ ->  0.,           0.
            |> fun (x,y) -> {X=x; Y=y}

        let scale'' = swapXYByRot scale'
        match scale' with
        | {X=x;Y=y} when x <= 0.001 || y <= 0.001 -> symbol // hack to avoid divide by zero errors
        | _ ->
            symbol 
            |> Optic.set  SymbolT.scaleF_ scale'' // set symbol scaling
            |> Optic.set SymbolT.posOfSym_ (posDelta + symPos) // set symbol position
           
        |> Optic.set (SymbolT.appearance_ >-> SymbolT.showCorners_) SymbolT.ShowAll
        |> Optic.map (SymbolT.labelBoundingBox_ >-> topLeft_) (fun lPos -> lPos + posDelta) // set label position as symbol

    Lens.create get set

/// Change the position of a symbol on the sheet. New coordinates are of top-left corner.
let setSymbolPosition (model: SymbolT.Model) (symbol: SymbolT.Symbol) (newPos: XYPos) : SymbolT.Model =
    moveSymbols model [symbol.Id] (newPos - symbol.Pos)

/// Returns the list of ordered ports for a given side of a symbol.
let getOrderedPorts (edge: Edge) (symbol: SymbolT.Symbol) : string list =
    symbol.PortMaps.Order[edge]

/// Sets the list of ordered ports for a given side of a symbol.
/// The provided list of ports must be a reordering of the existing list of ports
/// (i.e. this function can only change order).
let setOrderedPorts (edge: Edge) (ports: string list) (symbol: SymbolT.Symbol) : SymbolT.Symbol =
    let oldMap = Optic.get (SymbolT.portMaps_ >-> SymbolT.order_) symbol
    let newMap = Map.add edge ports oldMap
    Optic.set (SymbolT.portMaps_ >-> SymbolT.order_) newMap symbol

/// A lens for ReversedInputPorts in a symbol.
/// TODO how to handle old circuits (ReversedInputPorts = None) properly?
let reversedInputPorts_ =

    let get (symbol: SymbolT.Symbol) : bool option = symbol.ReversedInputPorts

    let set (revInputPorts: bool option) (symbol: SymbolT.Symbol) : SymbolT.Symbol =
        {symbol with ReversedInputPorts=revInputPorts}

    Lens.create get set

/// Returns the position of a port on the sheet, given the symbol it belongs to.
let getPortSheetPos (symbol: SymbolT.Symbol) (port: Port) : XYPos =
    (getPortPos symbol port) + symbol.Pos

/// Get the bounding box of a symbol.
let getSymbolBoundingBox (symbol: SymbolT.Symbol) : BoundingBox =
    /// Code adapted from Symbol.getSymbolBoundingBox
    let h,w = getRotatedHAndW symbol
    {TopLeft = symbol.Pos; H = float(h) ; W = float(w)}

/// A lens for the rotation state of a symbol.
// TODO should set rotate by the given number of degrees, or set the rotation state to the given one?
let rotation_ =

    let get (symbol: SymbolT.Symbol) : Rotation = symbol.STransform.Rotation

    let set (rotation: Rotation) (symbol: SymbolT.Symbol) =
        rotateSymbolInBlock rotation symbol.CentrePos symbol

    Lens.create get set

/// Returns whether or not a symbol is flipped.
let getSymbolFlip (symbol: SymbolT.Symbol) : bool =
    symbol.STransform.Flipped

/// Flips a symbol.
let setSymbolFlip (flip: SymbolT.FlipType) (symbol: SymbolT.Symbol) =
    flipSymbolInBlock flip symbol.CentrePos symbol

/// Returns the number of pairs of symbols that intersect each other.
let numPairsIntersectingSymbols (sheet: SheetT.Model) =
    let boxes =
        mapValues sheet.BoundingBoxes
        |> Array.toList
    uniquePairs boxes
    |> List.filter (fun (box1, box2) -> BlockHelpers.overlap2DBox box1 box2)
    |> List.length

/// Returns a list of pairs of vertices representing all visible segments in the sheet.
let getVisibleSegs (sheet: SheetT.Model) : (XYPos * XYPos) list =

    // Convert a wire, as a starting position and a set of vectors for each segment,
    // to a list of vertices.
    let wireVectorsToVertices (vectors: XYPos list) (startPos: XYPos) : XYPos list =
        (startPos, vectors)
        ||> List.scan (fun currPos vector -> currPos + vector)

    // Extract wire vectors for each wire in sheet.
    let wireVectors =
        sheet.Wire.Wires
        |> Map.toList
        |> List.map fst
        |> List.map (fun x -> visibleSegments x sheet)

    // Extract start pos for each wire in sheet.
    let startPosList =
        sheet.Wire.Wires
        |> Map.toList
        |> List.map snd
        |> List.map (fun x -> x.StartPos)

    List.map2 wireVectorsToVertices wireVectors startPosList
    |> List.map List.pairwise
    |> List.collect id

/// Returns the number of visible wire segments that intersect one or more symbols.
let numSegmentsIntersectSymbols (sheet: SheetT.Model) : int =

    // TODO consider use of getNonZeroAbsSegments

    let allSymbolsIntersected =
        sheet.Wire.Symbol.Symbols
        |> Map.values
        |> Seq.toList
        |> List.filter (fun s -> s.Annotation = None)
        |> List.map (fun s -> (s.Component.Type, Symbol.getSymbolBoundingBox s))

    // Adapted from BusWireRoute
    let boxesIntersectedBySegment startPos endPos =
        allSymbolsIntersected
        |> List.map (fun (compType, boundingBox) ->
            (
                compType,
                {
                    W = boundingBox.W + Constants.minWireSeparation * 2.
                    H = boundingBox.H + Constants.minWireSeparation * 2.
                    TopLeft =
                    boundingBox.TopLeft
                    |> updatePos Left_ Constants.minWireSeparation
                    |> updatePos Up_ Constants.minWireSeparation
                }
            ))
        |> List.filter (fun (compType, boundingBox) ->
            match segmentIntersectsBoundingBox boundingBox startPos endPos with // do not consider the symbols that the wire is connected to
            | Some _ -> true // segment intersects bounding box
            | None -> false // no intersection
        )
        |> List.map (fun (compType, boundingBox) -> boundingBox)

    getVisibleSegs sheet
    |> List.filter (fun (startPos, endPos) ->
        boxesIntersectedBySegment startPos endPos
        |> List.isEmpty
        |> not
    )
    |> List.length

/// Returns the number of segments intersecting at right angles.
/// We look for segments that form a + shape, rather than a T shape.
let numRightAngleSegCrossings (sheet: SheetT.Model) : int =

    /// Check if a given horizontal and vertical segment are crossing.
    let isCrossing (hSeg: BusWireT.ASegment) (vSeg: BusWireT.ASegment) : bool =
        let top = min vSeg.Start vSeg.End
        let bottom = max vSeg.Start vSeg.End
        let left = min hSeg.Start hSeg.End
        let right = max hSeg.Start hSeg.End

        top.X - left.X > XYPos.epsilon && right.X - top.X > XYPos.epsilon &&
        left.Y - top.Y > XYPos.epsilon && bottom.Y - left.Y > XYPos.epsilon

    let aSegList =
        sheet.Wire.Wires
        |> Map.toList
        |> List.map snd
        |> List.collect getNonZeroAbsSegments

    uniquePairs aSegList
    |> List.filter (fun (seg1, seg2) ->
        match (seg1.Orientation, seg2.Orientation) with
        | BusWireT.Horizontal, BusWireT.Vertical -> isCrossing seg1 seg2
        | BusWireT.Vertical, BusWireT.Horizontal -> isCrossing seg2 seg1
        | _, _ -> false
    )
    |> List.length

/// Find the total visible wire length on the sheet.
/// Overlapping segments are only counted once.
let visibleWireLength (sheet: SheetT.Model) : float =

    // The total length of wires, counting overlap multiple times.
    let totalLength =
        sheet.Wire.Wires
        |> Map.toList
        |> List.map snd
        |> List.collect (fun x -> x.Segments)
        |> List.fold (fun curr_len seg -> curr_len + (abs seg.Length)) 0.0

    // Returns amount of overlap in 1D. Returns 0 if no overlap.
    let overlap1D (s1: float) (e1: float) (s2: float) (e2: float) : float =
        let l1 = min s1 e1
        let r1 = max s1 e1
        let l2 = min s2 e2
        let r2 = max s2 e2

        if l1 < l2 && r1 > l2 && r1 < r2 then r1 - l2
        elif l1 > l2 && r1 < r2 then r1 - l1
        elif l1 < l2 && r1 > r2 then r2 - l2
        elif l2 < l1 && r2 > l1 && r2 < r1 then r2 - l1
        else 0.0

    // Returns amount of overlap between two segments, or 0 if no overlap.
    let overlapSegs (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) : float =
        match seg1.Orientation, seg2.Orientation with
        | BusWireT.Horizontal, BusWireT.Horizontal ->
            if abs (seg1.Start.Y - seg2.Start.Y) < XYPos.epsilon then
                overlap1D seg1.Start.X seg1.End.X seg2.Start.X seg2.End.X
            else 0.0
        | BusWireT.Vertical, BusWireT.Vertical ->
            if abs (seg1.Start.X - seg2.Start.X) < XYPos.epsilon then
                overlap1D seg1.Start.Y seg1.End.Y seg2.Start.Y seg2.End.Y
            else 0.0
        | _, _ -> 0.0

    // Find total overlapping length for a list of wires.
    let getOverlapLength (wires: BusWireT.Wire list) : float =
        wires
        |> List.collect getAbsSegments
        |> uniquePairs
        |> List.map (fun (seg1, seg2) -> overlapSegs seg1 seg2)
        |> List.fold (+) 0.0

    // The total overlapping wire length in the sheet.
    let totalOverlapLength =
        partitionWiresIntoNets sheet.Wire
        |> List.map snd
        |> List.map (List.map snd)
        |> List.map getOverlapLength
        |> List.fold (+) 0.0

    totalLength - totalOverlapLength

/// Returns the number of visible wire right-angles in the sheet.
// TODO check if this actually works how I want it to
let numWireRightAngles (sheet: SheetT.Model) : int =
    sheet.Wire.Wires
    |> Map.toList
    |> List.map fst
    |> List.map (fun connectionId -> visibleSegments connectionId sheet)
    |> List.map (fun lst -> lst.Length - 1)
    |> List.fold (+) 0

/// Check if the segment at the given index is a zero-length segment connecting
/// two segments that retrace each other.
/// NOTE that this function does not check if i is a valid index.
let isRetrace (segs: BusWireT.Segment list) (i: int) : bool =
    abs segs[i].Length < XYPos.epsilon &&
    abs segs[i - 1].Length > XYPos.epsilon &&
    abs segs[i + 1].Length > XYPos.epsilon &&
    (segs[i - 1].Length < 0 && segs[i + 1].Length > 0) ||
    (segs[i - 1].Length > 0 && segs[i + 1].Length < 0)

/// Returns a list of all pairs of wire segments that retrace themselves, across the whole sheet.
let getAllRetracedSegs (sheet: SheetT.Model) : (BusWireT.Segment*BusWireT.Segment) list =

    // Returns a list of pairs of segments from the provided segment list that retrace themselves.
    let getWireRetracedSegs (segs: BusWireT.Segment list) : (BusWireT.Segment*BusWireT.Segment) list =
        ([], [1..segs.Length-2])
        ||> List.fold (fun res i ->
            if isRetrace segs i then res @ [(segs[i - 1], segs[i + 1])]
            else res
        )

    sheet.Wire.Wires
    |> Map.toList
    |> List.map snd
    |> List.map (fun wire -> wire.Segments)
    |> List.collect getWireRetracedSegs

/// Returns a list of all visible endpoint segments (not the nubs)
/// that retrace into their connected symbol.
let getSymbolRetraceSegs (sheet: SheetT.Model) : BusWireT.Segment list =

    // Finds symbol-retraced segments for a given segment list.
    // Returned list can have 0, 1, or 2 segments.
    let findRetraced (segs: BusWireT.Segment list) : BusWireT.Segment list =
        if segs.Length < 4 then []
        else
            let segStart =
                match isRetrace segs 1 with
                | true -> [segs[2]]
                | false -> []
            let segEnd =
                match isRetrace segs (segs.Length-2) with
                | true -> [segs[segs.Length-3]]
                | false -> []
            segStart @ segEnd

    sheet.Wire.Wires
    |> Map.toList
    |> List.map snd
    |> List.map (fun wire -> wire.Segments)
    |> List.collect findRetraced

/// Returns a tuple of:
/// 1. A list of all the pairs of segments that retrace themselves.
/// 2. A list of all end-of-wire segments that retrace into their symbols.
let getRetraced (sheet: SheetT.Model) : ((BusWireT.Segment*BusWireT.Segment) list * BusWireT.Segment list) =
    // Note there is some repeated work for this implementation,
    // but it is the easiest to understand.
    getAllRetracedSegs sheet, getSymbolRetraceSegs sheet
