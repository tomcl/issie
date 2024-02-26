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

// B1R, B1W RW Low
// returns the lens of dimensions of a custom component symbol
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

// B2W Med
// The position of a symbol on the sheet
let setSymbolPosOnSheet (sheetModel : SheetT.Model) (symId : ComponentId) (pos : XYPos) : SheetT.Model = 
    let updateSymPos (sym : SymbolT.Symbol) : SymbolT.Symbol = { sym with Pos = pos }
    let newSymModel: SymbolT.Model = SymbolUpdate.updateSymbol updateSymPos symId sheetModel.Wire.Symbol

    sheetModel
    |> Optic.set SheetT.symbol_ newSymModel

// B3R, B3W RW Med
// Read/write the order of ports on a specified side of a symbol
// used Symbols.fs
let getSymPortOrder (sym : SymbolT.Symbol) (side : Edge) : string list =
    sym.PortMaps.Order[side]

let setSymPortOrder (orderedPorts : string list) (side : Edge) (sym : SymbolT.Symbol) : SymbolT.Symbol =
    let newPortOrder = sym.PortMaps.Order |> Map.add side orderedPorts

    { sym with PortMaps = { sym.PortMaps with Order = newPortOrder } }

// A lens is not directly possible as we also need to specify the specific side
// let symPortOrder_ = Lens.create getSymPortOrder setSymPortOrder

// B4R, B4W RW Low 
// The reverses state of the inputs of a MUX2
let getMux2IsReversed (sym : SymbolT.Symbol) : bool option =
    sym.ReversedInputPorts

let setMux2IsReversed (sym : SymbolT.Symbol) (reverseState : bool option) : SymbolT.Symbol =
    { sym with ReversedInputPorts = reverseState }

// B5R R Low 
// The position of a port on the sheet. It cannot directly be written.
let getPortPosOnSheet (sym : SymbolT.Symbol) (portId : string) : XYPos =
    let port = sym.Component.getPort(PortId portId)
    match port with
    | Some port -> getPortPos sym port
    | None -> failwithf "Port with id %s not found in symbol" portId
    
// B6R R Low 
// The Bounding box of a symbol outline (position is contained in this)
let getSymOutlineBoundingBox (sym : SymbolT.Symbol) : BoundingBox =
    getSymbolBoundingBox sym

// B7R, B7W RW Low 
// The rotation state of a symbol
let getSymRotation (sym : SymbolT.Symbol) : Rotation =
    sym.STransform.Rotation

let setSymRotation (rotation : Rotation) (sym : SymbolT.Symbol) : SymbolT.Symbol =
    { sym with STransform = { sym.STransform with Rotation = rotation } }

let symRotation_ = Lens.create getSymRotation setSymRotation

// B8R, B8W RW Low 
// The flip state of a symbol
let isSymFlip (sym : SymbolT.Symbol) : bool =
    sym.STransform.Flipped

let setSymFlip (isFlipped : bool) (sym : SymbolT.Symbol) : SymbolT.Symbol =
    { sym with STransform = { sym.STransform with Flipped = isFlipped } }

let symFlip_ = Lens.create isSymFlip setSymFlip

// T1R R Low 
// The number of pairs of symbols that intersect each other. See Tick3 for a related
// function. Count over all pairs of symbols.
let countSymIntersectSym (sheetModel : SheetT.Model) : int =
    let wireModel = sheetModel.Wire
    let boxes =
        Map.toList sheetModel.BoundingBoxes
        |> List.mapi (fun n box -> n,box)
    
    List.allPairs boxes boxes 
    |> List.filter (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox (snd box1) (snd box2))
    |> List.length
    

// T2R R Low 
// The number of distinct wire visible segments that intersect with one or more
// symbols. See Tick3.HLPTick3.visibleSegments for a helper. Count over all visible wire
// segments.
let countWireIntersectSym (sheetModel : SheetT.Model) : int =
    let wireModel = sheetModel.Wire

    wireModel.Wires
    |> Map.filter (fun _ wire -> BusWireRoute.findWireSymbolIntersections wireModel wire <> [])
    |> Map.count


// T3R R Low 
// The number of distinct pairs of segments that cross each other at right angles. Does
// not include 0 length segments or segments on same net intersecting at one end, or
// segments on same net on top of each other. Count over whole sheet.
let countCrossingSegments (sheetModel : SheetT.Model) : int =
    let wires: Map<ConnectionId,Wire> = sheetModel.Wire.Wires 
    // first convert to ASegments
    // check for overlap for Horizontal and Vertical segments

    let sheetSegments = 
        wires
        |> Map.toList
        |> List.map (fun (id, wire) -> wire)
        |> List.collect BlockHelpers.getNonZeroAbsSegments

    let filter (seg1 : BusWireT.ASegment) (seg2 : BusWireT.ASegment) (wireMap : Map<ConnectionId,Wire>) =
        let wire1, wire2 = wireMap[snd seg1.GetId], wireMap[snd seg2.GetId]
        // we require segments to be on different nets, be orthogonal to each other and overlap
        (wire1.InputPort <> wire2.InputPort) && (seg1.Orientation <> seg2.Orientation) && BlockHelpers.overlap2D (seg1.Start, seg1.End) (seg2.Start, seg2.End)
    
    sheetSegments
    |> List.allPairs sheetSegments
    |> List.filter (fun (seg1, seg2) -> filter seg1 seg2 wires)
    |> List.length
    

// T4R R Medium 
// Sum of wiring segment length, counting only one when there are N same-net
// segments overlapping (this is the visible wire length on the sheet). Count over whole
// sheet.
let getWireLength (wire : BusWireT.Wire) : float = 
    wire.Segments
    |> List.fold (fun (len: float) (seg: BusWireT.Segment) -> len+seg.Length) 0.0

let sumWireLength (sheetModel : SheetT.Model) : float =
    let folder (curLen : float) (mapElm : ConnectionId*BusWireT.Wire) = 
        curLen + getWireLength (snd mapElm)

    sheetModel.Wire.Wires
    |> Map.toList
    |> List.fold folder 0.0

// T5R R Low 
// Number of visible wire right-angles. Count over whole sheet.
let countWireRightAngles (sheetModel : SheetT.Model) : int =
    let wireModel = sheetModel.Wire

    wireModel.Wires
    |> Map.fold (fun _ wire acc -> acc + BlockHelpers.countRightAngles wire) 0
    0


// T6R R High 
// The zero-length segments in a wire with non-zero segments on either side that have
// Lengths of opposite signs lead to a wire retracing itself. Note that this can also apply
// at the end of a wire (where the zero-length segment is one from the end). This is a
// wiring artifact that should never happen but errors in routing or separation can
// cause it. Count over the whole sheet. Return from one function a list of all the
// segments that retrace, and also a list of all the end of wire segments that retrace so
// far that the next segment (index = 3 or Segments.Length – 4) - starts inside a symbol.