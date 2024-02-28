module SheetBeautifyHelpers

open Optics

open CommonTypes
open DrawModelType
open BusWireT

open Symbol
type Dimension = {
    W: float
    H: float
}

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

/// B1R, B1W RW Low
/// returns the lens of dimensions of a custom component symbol
let getCCSymbolDimension (sym : SymbolT.Symbol) =
    match (sym.HScale, sym.VScale) with
    | (None, None) -> { W = sym.Component.W ; H = sym.Component.H }
    | (Some hScale, Some vScale) -> { W = sym.Component.W * hScale ; H = sym.Component.H * vScale }
    | (Some hScale, None) -> { W = sym.Component.W * hScale ; H = sym.Component.H }
    | (None, Some vScale) -> { W = sym.Component.W ; H = sym.Component.H * vScale }

let setCCSymbolDimension (dim : Dimension) (sym : SymbolT.Symbol) =
    { sym with HScale = Some (dim.W/sym.Component.W); VScale = Some (dim.H/sym.Component.H) }
    // keep the W and H of the component the same, as this is calculated when creating the custom component
    // if the user performs an illegal sizing, we can revert back to this original size

let CCSymbolDimensions_ = Lens.create getCCSymbolDimension setCCSymbolDimension

/// B2W Med
/// The position of a symbol on the sheet
let setSymbolPosOnSheet (sheetModel : SheetT.Model) (symId : ComponentId) (pos : XYPos) : SheetT.Model = 
    let updateSymPos (sym : SymbolT.Symbol) : SymbolT.Symbol = { sym with Pos = pos }
    let newSymModel: SymbolT.Model = SymbolUpdate.updateSymbol updateSymPos symId sheetModel.Wire.Symbol

    sheetModel
    |> Optic.set SheetT.symbol_ newSymModel

/// B3R, B3W RW Med
/// Read/write the order of ports on a specified side of a symbol
/// used Symbols.fs
let getSymPortOrder (sym : SymbolT.Symbol) (side : Edge) : string list =
    sym.PortMaps.Order[side]

let setSymPortOrder (orderedPorts : string list) (side : Edge) (sym : SymbolT.Symbol) : SymbolT.Symbol =
    let newPortOrder = sym.PortMaps.Order |> Map.add side orderedPorts

    { sym with PortMaps = { sym.PortMaps with Order = newPortOrder } }

// A lens is not directly possible as we also need to specify the specific side
// let symPortOrder_ = Lens.create getSymPortOrder setSymPortOrder

/// B4R, B4W RW Low 
/// The reverses state of the inputs of a MUX2
let getMux2IsReversed (sym : SymbolT.Symbol) : bool option =
    sym.ReversedInputPorts

let setMux2IsReversed (sym : SymbolT.Symbol) (reverseState : bool option) : SymbolT.Symbol =
    { sym with ReversedInputPorts = reverseState }

/// B5R R Low 
/// The position of a port on the sheet. It cannot directly be written.
let getPortPosOnSheet (sym : SymbolT.Symbol) (portId : string) : XYPos =
    let port = sym.Component.getPort(PortId portId)
    match port with
    | Some port -> getPortPos sym port
    | None -> failwithf "Port with id %s not found in symbol" portId
    
/// B6R R Low 
/// The Bounding box of a symbol outline (position is contained in this)
let getSymOutlineBoundingBox (sym : SymbolT.Symbol) : BoundingBox =
    getSymbolBoundingBox sym

/// B7R, B7W RW Low 
/// The rotation state of a symbol
let getSymRotation (sym : SymbolT.Symbol) : Rotation =
    sym.STransform.Rotation

let setSymRotation (rotation : Rotation) (sym : SymbolT.Symbol) : SymbolT.Symbol =
    { sym with STransform = { sym.STransform with Rotation = rotation } }

let symRotation_ = Lens.create getSymRotation setSymRotation

/// B8R, B8W RW Low 
/// The flip state of a symbol
let isSymFlip (sym : SymbolT.Symbol) : bool =
    sym.STransform.Flipped

let setSymFlip (isFlipped : bool) (sym : SymbolT.Symbol) : SymbolT.Symbol =
    { sym with STransform = { sym.STransform with Flipped = isFlipped } }

let symFlip_ = Lens.create isSymFlip setSymFlip

/// T1R R Low 
/// The number of pairs of symbols that intersect each other. See Tick3 for a related
/// function. Count over all pairs of symbols.
let countSymIntersectSym (sheetModel : SheetT.Model) : int =
    let wireModel = sheetModel.Wire
    let boxes =
        Map.toList sheetModel.BoundingBoxes
        |> List.mapi (fun n box -> n,box)
    
    List.allPairs boxes boxes 
    |> List.filter (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox (snd box1) (snd box2))
    |> List.length
    

/// T2R R Low 
/// The number of distinct wire visible segments that intersect with one or more
/// symbols. See Tick3.HLPTick3.visibleSegments for a helper. Count over all visible wire
/// segments.
let countWireIntersectSym (sheetModel : SheetT.Model) : int =
    let wireModel = sheetModel.Wire

    wireModel.Wires
    |> Map.filter (fun _ wire -> BusWireRoute.findWireSymbolIntersections wireModel wire <> [])
    |> Map.count

// ------------ Some Extra Helpers ------------

/// helper to get all pairs of elements from a list without repeats
let rec allPairsWithoutRepeats list =
    match list with
    | [] -> []
    | hd::tl -> (List.map (fun elm -> (hd, elm)) tl) @ allPairsWithoutRepeats tl

/// helper to get the input port of a segment from the sheet model
let getInputPortOfSeg (seg : BusWireT.Segment) (sheetModel : SheetT.Model) =
    let wireMap : Map<ConnectionId,Wire> = sheetModel.Wire.Wires
    wireMap[snd seg.GetId].InputPort

/// helper to check if two segments are from the same net in the sheet model
let isSegFromSameNet (seg1 : BusWireT.Segment) (seg2 : BusWireT.Segment) (sheetModel : SheetT.Model) =
    getInputPortOfSeg seg1 sheetModel = getInputPortOfSeg seg2 sheetModel

/// helper to calculate the overlap length of two segments
let calcASegOverlapLength (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) = 
    let min1, min2 = min seg1.Start seg1.End, min seg2.Start seg2.End
    let max1, max2 = max seg1.Start seg1.End, max seg2.Start seg2.End
    
    let vector = min max1 max2 - max min1 min2
    max vector.X vector.Y // assume that the segments are parallel, either X or Y will be cancelled out
    
// --------------------------------------------

/// T3R R Low 
/// The number of distinct pairs of segments that cross each other at right angles. Does
/// not include 0 length segments or segments on same net intersecting at one end, or
/// segments on same net on top of each other. Count over whole sheet.
let countSegmentIntersect (sheetModel : SheetT.Model) : int =
    let wires: Map<ConnectionId,Wire> = sheetModel.Wire.Wires 
    let ASegments = 
        wires
        |> Map.toList
        |> List.map (fun (id, wire) -> wire)
        |> List.collect BlockHelpers.getNonZeroAbsSegments

    let intersectFilter (seg1 : BusWireT.ASegment) (seg2 : BusWireT.ASegment) =
        // we require segments to be on different nets, be orthogonal to each other and overlap
        (isSegFromSameNet seg1.Segment seg2.Segment sheetModel) 
        && (seg1.Orientation <> seg2.Orientation) 
        && BlockHelpers.overlap2D (seg1.Start, seg1.End) (seg2.Start, seg2.End)
    
    ASegments
    |> allPairsWithoutRepeats
    |> List.filter (fun (seg1, seg2) -> intersectFilter seg1 seg2)
    |> List.length

/// T4R R Medium 
/// Sum of visible wiring segment length. When there are N same-net
/// segments overlapping this is counted once. Count over whole sheet.
let calcVisibleWiringLength (sheetModel : SheetT.Model) : float =
    // Summary:
    // Visible Length = (1) + (2)
    // (1) Sum of segment length without any overlap
    // (2) Sum of the maximum overlap length of segments in the N same net
    // Approach for getting (2):
    // Find all segment pairs that overlap
    // They MUST have either start or end points at the same position
    // Group the pairs by their shared point (start/end) => same N net pairs
    // Calculate the max overlap length for all the pairs in each group
    // Sum the above
    let wires: Map<ConnectionId,Wire> = sheetModel.Wire.Wires 
    let ASegments = 
        wires
        |> Map.toList
        |> List.map snd
        |> List.collect BlockHelpers.getNonZeroAbsSegments

    // we require segments to be parallel to each other and overlap 
    let overlapPairs = 
        let overlapFilter (seg1 : BusWireT.ASegment) (seg2 : BusWireT.ASegment) =
            (seg1.Orientation = seg2.Orientation) && BlockHelpers.overlap2D (seg1.Start, seg1.End) (seg2.Start, seg2.End)

        ASegments
        |> allPairsWithoutRepeats
        |> List.filter (fun (seg1, seg2) -> overlapFilter seg1 seg2)

    let segWithoutOverlapLength =
        // identify all segments that overlap
        let allSegsWithOverlap =
            overlapPairs
            |> List.unzip
            |> (fun (seg1, seg2) -> seg1 @ seg2)
            |> List.distinct

        ASegments
        |> List.except allSegsWithOverlap
        |> List.map (fun seg -> seg.Segment.Length)
        |> List.sum
    
    let segInSameNetOverlapLength =
        let findSharedPoint (seg1: BusWireT.ASegment, seg2: BusWireT.ASegment) = 
            seg1.Start // NB: assume start and end points of ASegments are guaranteed to be in order
            // NB: However, use this if the start and end points are not guaranteed to be in order
            // if seg1.Start = seg2.Start then seg1.Start 
            // elif seg1.Start = seg2.End then seg1.Start
            // elif seg1.End = seg2.Start then seg1.End
            // elif seg1.End = seg2.End then seg1.End
            // else failwithf "No shared overlap starting point found."
        overlapPairs
        |> List.filter (fun (seg1, seg2) -> isSegFromSameNet seg1.Segment seg2.Segment sheetModel)
        |> List.groupBy findSharedPoint // group by the shared point of segments
        |> List.map (fun (netId, segs) -> 
            segs
            |> List.map (fun (seg1, seg2) -> calcASegOverlapLength seg1 seg2)
            |> List.max
            ) 
            // calculate the maximum overlap length 
            // this is calculated separately for each group of segments that belong to the same net
        |> List.sum

    segWithoutOverlapLength + segInSameNetOverlapLength

/// T5R R Low 
/// Number of visible wire right-angles counted over whole sheet.
let countWireRightAngles (sheetModel : SheetT.Model) : int =
    let wireModel = sheetModel.Wire
    failwithf "Not implemented"

// T6R R High 
// The zero-length segments in a wire with non-zero segments on either side that have
// Lengths of opposite signs lead to a wire retracing itself. Note that this can also apply
// at the end of a wire (where the zero-length segment is one from the end). This is a
// wiring artifact that should never happen but errors in routing or separation can
// cause it. Count over the whole sheet. Return from one function a list of all the
// segments that retrace, and also a list of all the end of wire segments that retrace so
// far that the next segment (index = 3 or Segments.Length – 4) - starts inside a symbol.
