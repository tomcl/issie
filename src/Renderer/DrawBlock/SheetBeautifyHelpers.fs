module SheetBeautifyHelpers

open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Symbol
open Optics
open Operators
open BlockHelpers
open EEExtensions
open Fable.React
open DrawModelType.SheetT
open Helpers


//-----------------Module for beautify Helper functions--------------------------//
// lists the functions that must be written, R = Read, W = write. Where a value is RW two functions are needed one for Read and one for Write. These should be combined in a Lens (if possible).

/// Identify a port from its component label and number.
/// Usually both an input and output port will mathc this, so
/// the port is only unique if it is known to be input or output.
/// used to specify the ends of wires, since tehee are known to be
/// connected to outputs (source) or inputs (target).
type SymbolPort = { Label: string; PortNumber: int }

/// convenience function to make SymbolPorts
let portOf (label:string) (number: int) =
    {Label=label; PortNumber = number}
///-------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
// B1RW - The dimensions of a custom component symbol
let customComponentDimensionsLens : Lens<Symbol, (float * float)> =
    // Getter function: gets the width and height of a custom component symbol
    let get (sym: Symbol) = (sym.Component.W, sym.Component.H)
    // Setter function: returns a new Symbol with updated width and height
    let set (newDims: (float * float)) (sym: Symbol) =
        { sym with Component = { sym.Component with W = fst newDims; H = snd newDims } }

    Lens.create get set

///-------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
// B2W - The position of a symbol on the sheet
let symbolPositionLens : Lens<Symbol, XYPos> =
    // Getter function: gets the position of a symbol on the sheet
    let get (sym: Symbol) = sym.Pos
    // Setter function: returns a new Symbol with updated position
    let set (newPos: XYPos) (sym: Symbol) = { sym with Pos = newPos }

    Lens.create get set

///-------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
// B3R, B3W - Read and Write the order of ports on a specified side of a symbol
let portOrderLens (side: Edge) : Lens<Symbol, string list option> =
    // Getter function: gets the list of port IDs for a specified side from a Symbol
    let get (symbol: Symbol) =
        Map.tryFind side symbol.PortMaps.Order
    // Setter function: returns a new Symbol with updated port order for the specified side
    let set (newOrderOpt: string list option) (symbol: Symbol) =
        match newOrderOpt with
        | Some newOrder ->
            let updatedOrder = Map.add side newOrder symbol.PortMaps.Order
            { symbol with PortMaps = { symbol.PortMaps with Order = updatedOrder } }
        | None -> symbol  // If None, don't modify the symbol

    Lens.create get set

///-------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
// B4RW - The reverses state of the inputs of a MUX2
let reversedInputPortsLens : Lens<Symbol, bool option> =
    // Getter function: gets the reverse state of the input ports if available from the Symbol's Component
    let get (symbol: Symbol) =
        match symbol.Component.SymbolInfo with
        | Some symInfo -> symInfo.ReversedInputPorts
        | None -> None
    // Setter function: returns a new Symbol with updated reverse state for the input ports of a MUX2
    let set (newReversedState: bool option) (symbol: Symbol) =
        match symbol.Component.Type with
        | Mux2 -> // Only proceed if the component is a Mux2
            let updatedSymbolInfo = 
                match symbol.Component.SymbolInfo with
                | Some symInfo -> 
                    Some { symInfo with ReversedInputPorts = newReversedState }
                | None -> 
                    // If there's no SymbolInfo, create it with the new reversed state; adjust according to your model's requirements
                    Some { LabelBoundingBox = None; LabelRotation = None; STransform = { Rotation = Degree0; Flipped = false }; ReversedInputPorts = newReversedState; PortOrientation = Map.empty; PortOrder = Map.empty; HScale = None; VScale = None }
            let updatedComponent = { symbol.Component with SymbolInfo = updatedSymbolInfo }
            { symbol with Component = updatedComponent }
        | _ -> symbol // If not a Mux2, return the symbol unchanged

    Lens.create get set



///-------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
// B5R - Read the position of a port on the sheet
let readPortPosition (sym: Symbol) (port: Port) : XYPos =
    let addXYPos (pos1: XYPos) (pos2: XYPos) : XYPos =
    // relative to the top left corner of the symbol
        { X = pos1.X + pos2.X; Y = pos1.Y - pos2.Y }
    let TopLeft = sym.Pos
    let offset = getPortPos sym port
    addXYPos TopLeft offset




// Helper function to apply scaling to the width and height if present
let applyScaling (width: float) (height: float) (hScale: float option) (vScale: float option) =
    let w = match hScale with
            | Some(scale) -> width * scale
            | None -> width
    let h = match vScale with
            | Some(scale) -> height * scale
            | None -> height
    (w, h)
///-------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
// B6R - Read the Bounding box of a symbol outline
let readBoundingBox (symbol: Symbol) : BoundingBox =
    let (scaledWidth, scaledHeight) = applyScaling symbol.Component.W symbol.Component.H symbol.HScale symbol.VScale
    {
        TopLeft = symbol.Pos
        W = scaledWidth
        H = scaledHeight
    }
///-------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
// B7RW - The rotation of a symbol
let symbolRotationLens : Lens<Symbol, Rotation> =
    // Getter function: gets the rotation of a symbol
    let get (sym: Symbol) = sym.STransform.Rotation
    // Setter function: returns a new Symbol with updated rotation, aiming for a different structure than updateSymRotationState
    let set (newRotation: Rotation) (sym: Symbol) = 
        // Update SymbolInfo's STransform rotation if it exists, in a more compact form
        let updatedSymbolInfo = sym.Component.SymbolInfo |> Option.map (fun symInfo ->
            { symInfo with STransform = { symInfo.STransform with Rotation = newRotation } })
        // Prepare the updated component with the potentially updated SymbolInfo
        let updatedComponent = { sym.Component with SymbolInfo = updatedSymbolInfo }
        // Update the symbol's STransform rotation directly, using the updated component
        { sym with Component = updatedComponent; STransform = { sym.STransform with Rotation = newRotation } }

    Lens.create get set
///-------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
// B8RW - The flip state of a symbol
let symbolFlipLens : Lens<Symbol, bool> =
    // Getter function: gets the flip state of a symbol
    let get (sym: Symbol) = sym.STransform.Flipped
    // Setter function: returns a new Symbol with updated flip state
    let set (newFlipState: bool) (sym: Symbol) = 
        { sym with STransform = { sym.STransform with Flipped = newFlipState } }

    Lens.create get set
//-----------------------------T-Functions----------------------------------------//
//--------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------//

// function from Tick3.HLPTick3.visibleSegments
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
    let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int) =
        // Check if the segment at index is zero length and has non-zero length segments on either side, changed based on given function
        if index > 0 && index < segVecs.Length - 1 && segVecs[index] =~ XYPos.zero then
            let before = segVecs.[0..index-2]
            let coalesced = segVecs.[index-1] + segVecs.[index+1]
            let after = segVecs.[index+2..]
            before @ [coalesced] @ after  // if not changed it will show index out of range error
        else
            segVecs


    wire.Segments
    |> List.mapi getSegmentVector
    |> (fun segVecs ->
            (segVecs,[1..segVecs.Length-2])
            ||> List.fold tryCoalesceAboutIndex)

///-------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
// T1R - The number of pairs of symbols that intersect each other. Count over all pairs of symbols.
let countSymbolIntersectSymbol (sheet: SheetT.Model) =
    let boxes = mapValues sheet.BoundingBoxes |> Array.toList |> List.indexed
    let countOverlaps (acc: int) ((i, box1): int * BoundingBox) =
        boxes
        |> List.filter (fun (j, _) -> j > i) // Only consider boxes after the current one to avoid duplicate checks
        |> List.fold (fun innerAcc (_, box2) ->
            if overlap2DBox box1 box2 then innerAcc + 1 else innerAcc
        ) acc
    boxes |> List.fold countOverlaps 0

///-------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
// T2R - The number of distinct wire visible segments that intersect with one or moresymbols. Count over all visible wire segments.
let countSymbolIntersectWire (sheet: SheetT.Model) =
    let wireModel = sheet.Wire
    wireModel.Wires
    |> Map.values
    |> Seq.fold (fun acc wire -> 
    // Check if the wire intersects with any symbols
        if BusWireRoute.findWireSymbolIntersections wireModel wire <> [] 
        // if it does, add one to the accumulator
        then acc + 1 
        else acc
    ) 0

///-------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
// T3R - The number of distinct pairs of segments that cross each other at right angles on the whole sheet
type SegVector = {Start: XYPos; Dir: XYPos} // A segment vector with absolute start and direction

// Function to transform wire segments into a map of wire ID to a list of segments with absolute start and direction
let transformWireSegmentsToAbsoluteMap (model: SheetT.Model) =
    model.Wire.Wires
    |> Map.fold (fun accMap wId wire ->
        let origin = wire.StartPos
        let vectors = visibleSegments wId model

        // Transform the list of vectors into a list of segments with absolute start and direction
        let segments, _ = 
            vectors
            |> List.fold (fun (acc: SegVector list, lastPos: XYPos) vector -> 
                let newPos = lastPos + vector
                let segment = { Start = lastPos; Dir = vector }
                (acc @ [segment], newPos)
            ) ([], origin)

        // Add the wire ID and its segments to the map
        Map.add wId segments accMap
    ) Map.empty

// Function to count the number of right angle intersections between all segments
let countRightAngleIntersectionsAll (segments: SegVector list) =
    // Split segments into vertical and horizontal lists based on direction
    let (verticals, horizontals) = 
        List.fold (fun (v, h) seg -> 
            if seg.Dir.X = 0.0 then (seg :: v, h)
            else if seg.Dir.Y = 0.0 then (v, seg :: h)
            else (v, h)
        ) ([], []) segments

    // Function to check if two segments intersect at a right angle
    let segmentsIntersectRightAngle (seg1, seg2) =
        let end1 = { X = seg1.Start.X + seg1.Dir.X; Y = seg1.Start.Y + seg1.Dir.Y }
        let end2 = { X = seg2.Start.X + seg2.Dir.X; Y = seg2.Start.Y + seg2.Dir.Y }
        let isSeg1Vertical = seg1.Dir.X = 0.0
        let seg1Range, seg2Range, seg1Pos, seg2Pos =
            if isSeg1Vertical then 
                ((min seg1.Start.Y end1.Y, max seg1.Start.Y end1.Y), (min seg2.Start.X end2.X, max seg2.Start.X end2.X), seg1.Start.X, seg2.Start.Y)
            else
                ((min seg1.Start.X end1.X, max seg1.Start.X end1.X), (min seg2.Start.Y end2.Y, max seg2.Start.Y end2.Y), seg1.Start.Y, seg2.Start.X)
        seg1Pos > fst seg2Range && seg1Pos < snd seg2Range && seg2Pos > fst seg1Range && seg2Pos < snd seg1Range

    // Calculate the count of right angle intersections
    List.fold (fun acc vSeg ->
        List.fold (fun innerAcc hSeg ->
            if segmentsIntersectRightAngle (vSeg, hSeg) then innerAcc + 1 else innerAcc
        ) acc horizontals
    ) 0 verticals


// Function to count the total number of right angle intersections on the whole sheet
let totalRightAngleIntersect (model: SheetT.Model) =
    let segmentsMap = transformWireSegmentsToAbsoluteMap model
    let allSegments =
        segmentsMap
        |> Map.toList // Convert the map to a list of (key, value) pairs
        |> List.collect snd // Collect all SegVector lists into a single list of segments

    allSegments
    |> countRightAngleIntersectionsAll

///-------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
// T4R - Sum of wiring segment length, counting only one when there are N same-net segments overlapping on the whole sheet
let sumWireLength (model: SheetT.Model) =
    let countSegmentLength (wId: ConnectionId) (model: SheetT.Model) =
        let segments = visibleSegments wId model
        segments
        |> List.fold (fun acc seg -> 
        // Add the length of the segment to the accumulator, but only if the segment is not a zero-length segment
            acc + (if seg.X = 0.0 then abs seg.Y else abs seg.X) // Knowing that visible segments are either horizontal or vertical
        ) 0.0
    model.Wire.Wires
    |> Map.fold (fun acc wId _ -> acc + countSegmentLength wId model) 0.0

///-------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
// T5R - Number of visible wire right-angles. Count over whole sheet.
let countWireRightAngles (wId: ConnectionId) (model: SheetT.Model) =
    let segments = visibleSegments wId model
    let numSegments = List.length segments
    if numSegments > 0 then numSegments - 1 else 0
    // The check `if numSegments > 0 then ... else 0` ensures that wires with no visible segments
    // do not contribute to the right angle count negatively or incorrectly.

/// Sums up the right angles from all wires in the model.
let countTotalRightAngles (model: SheetT.Model) =
    model.Wire.Wires
    |> Map.fold (fun acc wId _ -> acc + countWireRightAngles wId model) 0

///-------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
/// ------------------------------------------------------------------------------------------------------------------------------------------//
// T6R - The zero-length segments in a wire with non-zero segments on either side that have Lengths of opposite signs lead to a wire retracing itself
// Count over the whole sheet. Return from one function a list of all the segments that retrace, and also a list of all the end of wire segments that retrace so far
let countRetraceSegments (model: SheetT.Model) : BusWireT.Segment list * BusWireT.Segment list =
    let wireModel = model.Wire
    wireModel.Wires
    |> Map.fold (fun (accSegs, accEnds) _ wire ->
        let segments = wire.Segments
        let numSegments = List.length segments
        segments
        |> List.indexed
        |> List.fold (fun (accSegs, accEnds) (i, seg) ->
            match i, seg.Length = 0.0 with
            // Check if the segment is a zero-length segment with opposite non-zero segments on either side
            | i, true when i > 0 && i < numSegments - 1 ->
                let prevSeg = segments.[i - 1]
                let nextSeg = segments.[i + 1]
                if prevSeg.Length <> 0.0 && nextSeg.Length <> 0.0 && sign prevSeg.Length = -(sign nextSeg.Length) then
                    (accSegs @ [seg], accEnds)
                else
                    (accSegs, accEnds)
            // Check if the segment is the last segment and the wire has more than one segment
            | i, true when i = numSegments - 1 && numSegments > 1 && segments.[i - 1].Length <> 0.0 -> (accSegs, accEnds @ [seg])
            | _ -> (accSegs, accEnds)
        ) ([], []) // Initial accumulator values
    ) ([], []) // Accumulates across all wires

