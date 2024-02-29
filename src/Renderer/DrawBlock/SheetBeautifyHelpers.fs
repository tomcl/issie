module SheetBeautifyHelpers

open Optics
open Symbol
open CommonTypes
open DrawModelType
open BlockHelpers


(* ---------------------------------------------------------------------------------------------- *)
(*                                           B-Functions                                          *)
(* ---------------------------------------------------------------------------------------------- *)

// B1R
/// <summary>Get dimensions of a component symbol.</summary>
/// <param name="sym">Target  symbol.</param>
/// <returns>Tuple of floats: (dimension of x, dimension of y).</returns>
let getCustomSymDim (sym: SymbolT.Symbol): float*float =
    let comp = sym.Component
    let stran = sym.STransform
    let xDim = (SymbolT.getScaleF sym.HScale) * comp.W
    let yDim = (SymbolT.getScaleF sym.VScale) * comp.H

    match stran.Rotation with
    | Degree0 | Degree180 -> xDim, yDim
    | Degree90 | Degree270 -> yDim, xDim

// B1W
/// <summary>Set dimensions of a component symbol.</summary>
/// <param name="dim">New dimension to set to symbol.</param>
/// <param name="sym">Target symbol.</param>
/// <returns>Symbol with updated dimension.</returns>
let setCustomSymDim (dim: float * float) (sym: SymbolT.Symbol): SymbolT.Symbol =
    let comp = sym.Component
    let tran = sym.STransform
    let xDim, yDim = dim
    let xDim', yDim' =
        match tran.Rotation with
        | Degree0 | Degree180 -> xDim, yDim
        | Degree90 | Degree270 -> yDim, xDim

    { sym with Component = { comp with W = xDim'; H = yDim' }}

// B1
/// <summary>Lens to read and write the dimension of a symbol.</summary>
let customSymDim_ =
    Lens.create getCustomSymDim setCustomSymDim


// B2W
/// <summary>Set position of a symbol.</summary>
/// <param name="pos">Position to set symbol to.</param>
/// <param name="sym">Target symbol.</param>
/// <returns>Symbol with updated position.</returns>
let setSymPos (pos: XYPos) (sym: SymbolT.Symbol): SymbolT.Symbol =
    Optic.set SymbolT.posOfSym_ pos sym


// B3R
/// <summary>Get port order on a specified side of a symbol.</summary>
/// <param name="edge">Side of a symbol, of type Edge.</param>
/// <param name="sym">Target symbol.</param>
/// <returns>List of ports in order.
/// Empty list if no ports are on specified edge.</returns>
let getSymPortOrder (edge: Edge) (sym: SymbolT.Symbol): List<string> =
    let getPortMap, _ = SymbolT.portMaps_

    (getPortMap sym).Order
    |> Map.tryFind edge
    |> function
        | Some list -> list
        | None -> List.empty

// B3W
/// <summary>Set port order on a specified side of a symbol.</summary>
/// <param name="order">List of ports to update side of symbol to.</param>
/// <param name="edge">Side of a symbol, of type Edge.</param>
/// <param name="sym">Target symbol.</param>
/// <returns>List of ports in order.
/// Empty list if no ports are on specified edge</returns>
let setSymPortOrder (order: List<string>) (edge: Edge) (sym: SymbolT.Symbol) =
    let portMapOrder' =
        (Optic.get SymbolT.portMaps_ sym).Order
        |> Map.add edge order

    Optic.set SymbolT.portMaps_ { (Optic.get SymbolT.portMaps_ sym) with Order = portMapOrder' } sym

// B3
/// <summary>Function to create lens to read and write port order on a specified side of a symbol</summary>
/// <param name="edge">Side of a symbol, of type Edge.</param>
/// <remarks>Not sure if this is useful.
/// Seemed like a missed opportunity to create a lens, but the function signatures don't match.</remarks>
let portMaps_Order_ (edge: Edge) =
    Lens.create (getSymPortOrder edge) (fun order sym -> setSymPortOrder order edge sym)


// B4R
/// <summary>Get input reverse state of a Mux or Demux symbol.</summary>
/// <param name="sym">Target Mux or Demux symbol.</param>
/// <returns>Boolean of reverse state of Mux or Demux symbol.
/// False is returned when reverse state is not set.
/// False is also returned when given symbol is not a Mux or Demux symbol.</returns>
let getMuxReverseState (sym: SymbolT.Symbol): bool =
    match sym.Component.Type with
    | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 ->
        Option.defaultValue false sym.ReversedInputPorts
    | _ ->
        false

// B4W
/// <summary>Set input reverse state of a Mux or Demux symbol.</summary>
/// <param name="state">Reverse state to set symbol to.</param>
/// <param name="sym">Target Mux or Demux symbol.</param>
/// <returns>New symbol with updated reverse set.
/// Original symbol returned if symbol is not of type Mux or Demux.</returns>
let setMuxReverseState (state: bool) (sym: SymbolT.Symbol): SymbolT.Symbol =
    match sym.Component.Type with
    | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 ->
        { sym with ReversedInputPorts = Some state }
    | _ ->
        sym

// B4
/// <summary>Lens to read and write the input reverse state of a Mux or Demux symbol.</summary>
let reversedInputPorts_ =
    Lens.create getMuxReverseState setMuxReverseState


// B5R
/// <summary>Get position of a port on the sheet.</summary>
/// <param name="port">Port to locate, of type Port.</param>
/// <param name="sheet">Model of the whole sheet.</param>
/// <returns>Position of the port on the sheet.</returns>
let getPortPosOnSheet (port: Port) (sheet: SheetT.Model): XYPos =
    let symId = port.HostId
    let sym = sheet.Wire.Symbol.Symbols[ComponentId symId]
    sym.Pos + getPortPos sym port


// B6R
/// <summary>Get bounding box information of a symbol.</summary>
/// <param name="sym">Target symbol.</param>
/// <param name="sheet">Model of the whole sheet, of type SheetT.Model.</param>
/// <returns>Bounding box of symbol. Fail if given symbol is not on the sheet.</returns>
let getSymBoundingBox (sym: SymbolT.Symbol) (sheet: SheetT.Model): BoundingBox =
    let symbolId =
        sheet.Wire.Symbol.Symbols
        |> Map.tryFindKey (fun _ value -> value = sym)
        |> function
            | Some id -> id
            | None -> failwith "getSymBoundingBox: given symbol is not in sheet"

    (Optic.get SheetT.boundingBoxes_ sheet)[symbolId]


// B7R
/// <summary>Get rotation state of a symbol.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>Rotation state, of type Rotation.</returns>
let getSymRotation (sym: SymbolT.Symbol): Rotation =
    sym.STransform.Rotation

// B7W
/// <summary>Set rotation state of a symbol.</summary>
/// <param name="rotate">Rotation state to set to, of type Rotation.</param>
/// <param name="sym">Target symbol.</param>
/// <returns>Symbol with updated rotation state.</returns>
let setSymRotation (rotate: Rotation) (sym: SymbolT.Symbol) =
    { sym with STransform = { sym.STransform with Rotation = rotate } }

// B7
/// <summary>Lens to read and write the rotation state of a symbol.</summary>
let stransform_rotation_ =
    Lens.create getSymRotation setSymRotation


// B8R
/// <summary>Get flipped state of a symbol.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>Flipped state, of bool type.</returns>
let getSymFlipped (sym: SymbolT.Symbol): bool =
    sym.STransform.Flipped

// B8W
/// <summary>Set flipped state of a symbol.</summary>
/// <param name="flip">Flipped state to set to.</param>
/// <param name="sym">Target symbol.</param>
/// <returns>Symbol with updated flipped state.</returns>
let setSymFlipped (flip: bool) (sym: SymbolT.Symbol) =
    { sym with STransform = { sym.STransform with Flipped = flip } }

// B8
/// <summary>Lens to read and write the flipped state of a symbol.</summary>
let stransform_flipped_ =
    Lens.create getSymFlipped setSymFlipped


(* ---------------------------------------------------------------------------------------------- *)
(*                                       T-Function Helpers                                       *)
(* ---------------------------------------------------------------------------------------------- *)

/// <summary>Get all symbol IDs within a sheet.</summary>
/// <param name="sheet">Model of the whole sheet.</param>
/// <returns>List of component IDs of all the symbols on the sheet.</returns>
let getAllSymIds (sheet: SheetT.Model): List<ComponentId> =
    sheet.Wire.Symbol.Symbols
    |> Map.toList
    |> List.map fst

/// <summary>Get all symbol bounding boxes within a sheet.</summary>
/// <param name="sheet">Model of the whole sheet.</param>
/// <returns>List of symbol component IDs with their bounding boxes of all the symbols on the sheet.</returns>
let getAllSymBoundingBoxes (sheet: SheetT.Model): List<ComponentId*BoundingBox> =
    Optic.get SheetT.boundingBoxes_ sheet
    |> Map.filter (fun key _ -> List.exists (fun symId -> key = symId) (getAllSymIds sheet))
    |> Map.toList

/// <summary>D.U. to describe orientation of a segment.</summary>
/// <remarks>Noted that BusWireT.Orientation is similar, but wanted to represent Other and Zero type,
/// although Other is likely not used.</remarks>
type SegOrientation = | Horizontal | Vertical | Other | Zero

/// <summary>Get a segment's orientation from its start and end position.</summary>
/// <param name="segStart">Segment start position.</param>
/// <param name="segEnd">Segment end position.</param>
/// <returns>Orientation, of SegOrientation type.</returns>
let segOrientation (segStart: XYPos) (segEnd: XYPos): SegOrientation =
    match (segEnd.X - segStart.X), (segEnd.Y - segStart.Y) with
    | dx, dy when dx = 0.0 && dy = 0.0 -> Zero
    | dx, dy when dx <> 0.0 && dy = 0.0 -> Horizontal
    | dx, dy when dx = 0.0 && dy <> 0.0 -> Vertical
    | _ -> Other

/// <summary>Get a segment's length from its start and end position.</summary>
/// <param name="segStart">Segment start position.</param>
/// <param name="segEnd">Segment end position.</param>
/// <returns>Length of segment.</returns>
let segLength (segStart: XYPos) (segEnd: XYPos): float =
    let abs (num: float) = if num < 0 then -num else num

    match segOrientation segStart segEnd with
    | Horizontal -> abs (segEnd.X - segStart.X)
    | Vertical -> abs (segEnd.Y - segStart.Y)
    | Zero -> 0.0
    | Other -> ((segEnd.X - segStart.X) ** 2 + (segEnd.Y - segStart.Y) ** 2) ** 0.5

/// <summary>Get segment vectors of a wire.</summary>
/// <param name="wire">Target wire, of type Wire.</param>
/// <returns>List of XYPos that represents the vectors of segments in a wire.</returns>
/// <remarks>Function refactored from TestDrawBlock.HLPTick3.visibleSegments.</remarks>
let getWireSegVectors (wire: BusWireT.Wire): List<XYPos> =
    /// <summary>Helper to match odd and even numbers.</summary>
    let (| IsEven | IsOdd |) (n: int) =
        match n % 2 with
        | 0 -> IsEven
        | _ -> IsOdd

    /// <summary>Helper to convert segment into XYPos vectors.</summary>
    let getSegmentVector (index:int) (seg: BusWireT.Segment) =
        match index, wire.InitialOrientation with
        | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> { X=0.; Y=seg.Length }
        | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> { X=seg.Length; Y=0. }

    wire.Segments
    |> List.mapi getSegmentVector

/// <summary>Get vertices of un-coalesceed segments of a wire.</summary>
/// <param name="wire">Target wire, of type Wire.</param>
/// <returns>List of XYPos that represents the vertices in a wire.</returns>
/// <remarks>Function seems to have same functionality as BlockHelpers.getWireSegmentXY.
/// Rewritten because this is clearer what it does, when compared the the function below.</remarks>
let getWireSegVertices (wire: BusWireT.Wire): List<XYPos> =
    getWireSegVectors wire
    |> List.mapFold (fun currPos segVec -> segVec+currPos, segVec+currPos) wire.StartPos
    |> (fun (vertices, _) -> wire.StartPos :: vertices)

/// <summary>Get vertices of visible coalesceed segments of a wire.</summary>
/// <param name="wire">Target wire, of type Wire.</param>
/// <returns>List of XYPos that represents the visible vertices in a wire.</returns>
/// <remarks>Function refactored from TestDrawBlock.HLPTick3.visibleSegments.</remarks>
let getWireVisibleSegVertices (wire: BusWireT.Wire): List<XYPos> =
    /// <summary>Helper to reduce multiple segments into one.</summary>
    let tryCoalesceAboutIndex (segVecs: List<XYPos>) (index: int)  =
        if index < segVecs.Length - 1 && segVecs[index] = ~ XYPos.zero
        then segVecs[0..index-2] @ [segVecs[index-1] + segVecs[index+1]] @ segVecs[index+2..segVecs.Length - 1]
        else segVecs

    getWireSegVectors wire
    |> (fun segVecs -> (segVecs,[1..segVecs.Length-2]) ||> List.fold tryCoalesceAboutIndex)
    |> List.mapFold (fun currPos segVec -> segVec+currPos, segVec+currPos) wire.StartPos
    |> (fun (vertices, _) -> wire.StartPos :: vertices)

/// <summary>Get all wire IDs within a sheet.</summary>
/// <param name="sheet">Model of the whole sheet.</param>
/// <returns>List of connection IDs of all the wires on the sheet.</returns>
let getAllWireIds (sheet: SheetT.Model): List<ConnectionId> =
    sheet.Wire.Wires |> Map.toList |> List.map fst

/// <summary>Get all wires within a sheet.</summary>
/// <param name="sheet">Model of the whole sheet.</param>
/// <returns>List of all wires on the sheet.</returns>
let getAllWires (sheet: SheetT.Model): List<BusWireT.Wire> =
    sheet.Wire.Wires |> Map.toList |> List.map snd

/// <summary>Get all visible segments of all wires within a sheet.</summary>
/// <param name="sheet">Model of the whole sheet.</param>
/// <returns>List of tuple of segments': wire's InputPortId, start,
/// and end positions of all segments on the sheet.</returns>
let getAllWireVisibleSegs (sheet: SheetT.Model): List<InputPortId*XYPos*XYPos> =
    /// <summary>Helper to create data of InputPortId, start, and end position tuples,
    /// from list of wire vertices.</summary>
    let makeSegData (sourceId: InputPortId, posList: List<XYPos>) =
        List.pairwise posList
        |> List.map (fun (segStart, segEnd) -> (sourceId, segStart, segEnd))

    getAllWireIds sheet
    |> List.map (fun wireId -> sheet.Wire.Wires[wireId])
    |> List.map (fun wire -> (wire.InputPort, getWireVisibleSegVertices wire))
    |> List.map makeSegData
    |> List.collect (fun item -> item)


(* ---------------------------------------------------------------------------------------------- *)
(*                                           T-Functions                                          *)
(* ---------------------------------------------------------------------------------------------- *)

// T1R
/// <summary>Count of all intersecting pairs of symbols on a sheet.</summary>
/// <param name="sheet">Target sheet to check, of type SheetT.Model.</param>
/// <returns>Total count of intersecting symbol pairs.</returns>
let findSymIntersectsSymCount (sheet: SheetT.Model): int =
    let symBoundingBoxes = getAllSymBoundingBoxes sheet

    List.allPairs symBoundingBoxes symBoundingBoxes
    |> List.filter
        (fun ((id1, box1), (id2, box2)) ->
            (id1 <> id2) && (overlap2DBox box1 box2))
    |> List.length
    |> fun count -> count/2 // if using allPairs on same list, will create duplicates


// T2R
/// <summary>Get count of all visible wire segments that intersects any symbols on a sheet.</summary>
/// <param name="sheet">Target sheet to check, of type SheetT.Model.</param>
/// <returns>Total count of symbol-intersecting visible wire segments.</returns>
let findVisibleSegIntersectsSymCount (sheet: SheetT.Model): int =
    let symBoundingBoxes = getAllSymBoundingBoxes sheet
    let wireSegs = getAllWireVisibleSegs sheet

    List.allPairs wireSegs symBoundingBoxes
    |> List.filter
        (fun ((_, segStart, segEnd), (_, boundingBox)) ->
            Option.isSome (segmentIntersectsBoundingBox boundingBox segStart segEnd))
    |> List.length


// T3R
/// <summary>Get count of visible, perpendicular, non-zero-length, non-same-net,
/// and non-consecutive segments pairs, that intersects each other.</summary>
/// <param name="sheet">Target sheet to check, of type SheetT.Model.</param>
/// <returns>Total count of intersecting visible wire segment pairs.</returns>
/// <remarks> Assumed that "segments on same net on top of each other" is a
/// superset of "segments same net intersecting at one end".</remarks>
let findSegIntersectsSegCount (sheet: SheetT.Model): int =
    /// <summary>Helper to identify perpendicular segment pairs.</summary>
    let isPerpSegPair (seg1: InputPortId*XYPos*XYPos, seg2: InputPortId*XYPos*XYPos): bool =
        let _, segStart1, segEnd1 = seg1
        let _, segStart2, segEnd2 = seg2

        match segOrientation segStart1 segEnd1, segOrientation segStart2 segEnd2 with
        | Horizontal, Vertical | Vertical, Horizontal -> true
        | _ -> false

    getAllWireVisibleSegs sheet
    |> List.filter (fun (_, segStart, segEnd) -> segStart <> segEnd) // remove length=0
    |> (fun list -> List.allPairs list list) // make segment pairs
    |> List.filter (fun ((inputId1, _, _), (inputId2, _, _)) -> inputId1 <> inputId2) // remove same-net pairs
    |> List.filter isPerpSegPair // remove non-perpendicular pairs
    |> List.filter
        (fun ((_, segStart1, segEnd1), (_, segStart2, segEnd2)) ->
            overlap2D (segStart1, segEnd1) (segStart2, segEnd2)) // remove non-overlapping pairs
    |> List.length
    |> fun count -> count/2 // if using allPairs on same list, will create duplicates


// T4R
/// <summary>Get total length of visible wire segments on a sheet.
/// Overlapping segments are only counted once.</summary>
/// <param name="sheet">Target sheet to check, of type SheetT.Model.</param>
/// <returns>Total length of visible segments rendered on screen.</returns>
/// <remarks>Function "unions" all visible segments, so if two segments from different nets overlaps,
/// they are only counted once.</remarks>
let findVisibleSegLength (sheet: SheetT.Model): float =
    /// <summary>Helper to union the start and end positions of seg2 into seg1 if possible.</summary>
    /// <param name="seg1">Segment to insert into, tuples of orientation, start, and end position.</param>
    /// <param name="seg2">Segment to be merged, tuples of orientation, start, and end position.</param>
    /// <returns>Union-ed segment if successfully union-ed. None if cannot union.</remarks>
    let segUnion (seg1: SegOrientation*XYPos*XYPos) (seg2: SegOrientation*XYPos*XYPos): Option<SegOrientation*XYPos*XYPos> =
        let orien1, segStart1, segEnd1 = seg1
        let orien2, segStart2, segEnd2 = seg2
        match orien1, orien2 with
        | Horizontal, Horizontal when segStart1.Y = segStart2.Y && overlap1D (segStart1.X, segEnd1.X) (segStart2.X, segEnd2.X) ->
            let allX = [ segStart1.X; segEnd1.X; segStart2.X; segEnd2.X ]
            Some (orien1, { segStart1 with X = List.min allX }, { segEnd1 with X = List.max allX })
        | Vertical, Vertical when segStart1.X = segStart2.X && overlap1D (segStart1.Y, segEnd1.Y) (segStart2.Y, segEnd2.Y) ->
            let allY = [ segStart1.Y; segEnd1.Y; segStart2.Y; segEnd2.Y ]
            Some (orien1, { segStart1 with Y = List.min allY }, { segEnd1 with Y = List.max allY })
        | _ ->
            None

    /// <summary>Helper to union a segment with a list of union-ed segments.</summary>
    /// <param name="union">List of segments to insert into. State in a folder function.</param>
    /// <param name="seg">Segment to be inserted. Value in a foler function.</param>
    /// <returns>List of segments after insertion.</returns>
    /// <remarks>Function checks if segment can be union-ed (merged), if not then it is append to the list.</remarks>
    let segUnionFolder (union: List<SegOrientation*XYPos*XYPos>) (seg: SegOrientation*XYPos*XYPos): List<SegOrientation*XYPos*XYPos> =
        let union', seg' =
            List.mapFold
                (fun segToAdd segFromUnion ->
                    match segUnion segFromUnion segToAdd with
                    | Some segUnioned -> segUnioned, (Zero, { XYPos.X = 0.0; XYPos.Y = 0.0 }, { XYPos.X = 0.0; XYPos.Y = 0.0 })
                    | None -> segFromUnion, segToAdd
                )
                seg
                union

        match seg' with
        | Zero, _, _ -> union'
        | _ -> union' @ [ seg' ]

    // obtain total length
    getAllWireVisibleSegs sheet
    |> List.map (fun (inputId, segStart, segEnd) -> (segOrientation segStart segEnd, segStart, segEnd)) // tag segment info with orientation
    |> List.filter (fun (orientation, _, _) -> match orientation with | Horizontal | Vertical -> true | _ -> false) // remove zero-lengt
    |> List.fold segUnionFolder List.empty // find union of all segments
    |> List.fold (fun length (_, segStart, segEnd) -> length + segLength segStart segEnd) 0.0 // sum length


// T5R
/// <summary>Get count of visible right angles on a sheet by counting coalesced segments.</summary>
/// <param name="sheet">Target sheet to check, of type SheetT.Model.</param>
/// <returns>Total count of right angles.</returns>
/// <remarks>Right angles are counted so because zero-length segments have already been
/// removed within the getWireVisibleSegVertices function.</remarks>
let findRightAngleCount (sheet: SheetT.Model): int =
    getAllWires sheet
    |> List.map getWireVisibleSegVertices
    |> List.map List.length
    |> List.map (fun count -> count-2) // excluding start and end, each vertice is a right angle
    |> List.map (fun count -> if count < 0 then 0 else count) // remove cases where there is no segment
    |> List.fold (+) 0


// T6R
/// <summary>Get a list of retracing segments and a subset list of segments which retraces inside a symbol.</summary>
/// <param name="sheet">Target sheet to check, of type SheetT.Model.</param>
/// <returns>A tuple of two segment lists. First item is all the retracing segments.
/// Second item is all the near-end retracing segments that intersects with a symbol.</returns>
let findRetracingSegs (sheet: SheetT.Model): List<BusWireT.Segment>*List<BusWireT.Segment> =
    /// <summary>Helper to choose segments that are retracing</summary>
    let isRetracing (seg: BusWireT.Segment): bool =
        let segs = sheet.Wire.Wires[seg.WireId].Segments
        match seg.Index with
        | i when i = 0 || i = 1 ->
            false
        | i when 1 < i && i < List.length segs ->
            (segs[i-2].Length > 0 && segs[i-1].Length = 0 && segs[i].Length < 0) ||
            (segs[i-2].Length < 0 && segs[i-1].Length = 0 && segs[i].Length > 0)
        | _ -> false

    /// <summary>Helper to check if segment intersects with any bounding box.</summary>
    let isIntersectingAnyBoundingBox (seg: BusWireT.Segment): bool =
        let segVertices = getWireSegVertices sheet.Wire.Wires[seg.WireId]
        let segStart, segEnd = segVertices[seg.Index], segVertices[seg.Index+1]

        getAllSymBoundingBoxes sheet
        |> List.tryFind (fun (_, bbox) -> Option.isSome (segmentIntersectsBoundingBox bbox segStart segEnd))
        |> Option.isSome

    // filter segements for retracing
    let retracingSegs =
        getAllWires sheet
        |> List.map (fun wire -> wire.Segments)
        |> List.map (fun segs -> List.filter isRetracing segs)
        |> List.collect (fun item -> item)

    // filter normal retracing segments for intersecting and near-end-segments
    let retracingEndSegs =
        retracingSegs
        |> List.filter (fun seg -> seg.Index = 2 || seg.Index = sheet.Wire.Wires[seg.WireId].Segments.Length-5)
        |> List.filter isIntersectingAnyBoundingBox

    retracingSegs, retracingEndSegs
