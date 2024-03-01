module SheetBeautifyHelpers
// ------- Drawblocks -----------
open BlockHelpers
open BusWire
open BusWireRoute
open BusWireRoutingHelpers
open BusWireSeparate
open BusWireUpdate
open BusWireUpdateHelpers
open PopupHelpers
open RotateScale
open Sheet
open SheetDisplay
open SheetSnap
open SheetUpdateHelpers
open Symbol
open SymbolHelpers
open SymbolPortHelpers
open SymbolReplaceHelpers
open SymbolUpdate
open SymbolView
// --------- common -----------
open CommonTypes
open DrawHelpers
open EEExtensions
open Helpers
open Optics
open Optic
open Operators
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open ModelType 

(*
    This file aims to provide code of two functions that helps read and write the corresponding values in each case. 
    A Lens should also be provided if possible, in order to combine the two values. 
    See https://github.com/dyu18/hlp24-project-issie-team7/tree/indiv-az1821/README-Indiv-notes.md for more detailed documentation. 
*)

// ================================= Draw Block Build Functions ====================================

// ------------------------------------- B1 (Read & write) --------------------------------
/// The dimensions of a custom component symbol
/// input - symbol
/// output - (x y dimension)
let readCustomSymbolDim (sym : Symbol) = 
    (sym.Component.W, sym.Component.H)
let writeCustomSymbolDim ((newDim) : (float * float)) (sym : Symbol) =
    {sym with Component = { sym.Component with W = fst newDim; H = snd newDim }}
let CustomCompDim : Lens<Symbol, (float * float)> =
    Lens (readCustomSymbolDim, writeCustomSymbolDim)

// --------------------------------- B2 (write) -------------------------------------
/// The position of a symbol on the sheet
/// input - new position of symbol
/// output - new symbol at the specific position
let writeSymbolPos (newPos : XYPos) (sym : Symbol)= 
    {sym with Pos = newPos}

// --------------------------------- B3 (read & write) ------------------------------
/// Read & write the order of ports on a specified side of a symbol
/// input - the list of port ids for a specified side from a Symbol
/// output - new symbol with updated port order for the specified side
let readPortOrder (side : Edge) (sym : Symbol) =
    sym.PortMaps.Order |> Map.tryFind side 
let writePortOrder (side : Edge) (newPortOrder : list<string> option) (sym : Symbol) =
    match newPortOrder with
    | Some newPortOrder ->
        let newPorts =  Map.add side newPortOrder sym.PortMaps.Order
        { sym with PortMaps = { sym.PortMaps with Order = newPorts } }
    | _ -> sym 
let portOrderLens (side : Edge) : Lens<Symbol, list<string> option> =
    Lens (readPortOrder side, writePortOrder side)

// --------------------------------- B4 (read & write) -------------------------------- 
/// The reverses state of the inputs of a MUX2
/// input - symbol (look up current state of reversed input ports)
/// output - (update state of reversed input ports)
let readReverseInput (sym : Symbol) = 
    sym.ReversedInputPorts
let writeReverseInput (reversedState : bool option) (sym : Symbol) = 
    let newReverseState =
        match reversedState with
        | Some state -> Some (not state)
        | _ -> Some true
    let newSym = 
        sym.Component.SymbolInfo 
        |> Option.map (fun symInfo -> 
        { symInfo with ReversedInputPorts = newReverseState })
    { sym with 
        Component = { sym.Component with SymbolInfo = newSym }
        ReversedInputPorts = newReverseState 
    }
let reversedInputsMux2_ : Lens<Symbol, bool option> = 
    Lens (readReverseInput, writeReverseInput)

// ------------------------------------ B5 (read) ------------------------------------------
/// The position of a port on the sheet. (It cannot directly be written.)
/// input - port and symbol
/// output - port position 
let readPort (port : Port) (sym : Symbol) : (XYPos) = 
    { X = sym.Pos.X + (getPortPos sym port).X; 
      Y = sym.Pos.Y - (getPortPos sym port).Y }

// TODO : check if look up for port/symbol is needed

// ---------------------------------- B6 (read) ----------------------------------------
/// The Bounding box of a symbol outline (position is contained in this)
/// input - symbol
/// output - the bouding box of the symbol
/// Note : Helper function in symbol.fs have been used to implement this
///        Consider using `getSymBoundingBox` to simplify
let readSymBoundingBox (sym : Symbol) : BoundingBox =
    let h,w = getRotatedHAndW sym
    if sym.Annotation = Some ScaleButton then 
        {TopLeft = sym.Pos - {X = 9.; Y = 9.}; H = 17. ; W = 17.}
    else 
        {TopLeft = sym.Pos; H = float(h) ; W = float(w)}

// ---------------------------------- B7 (read & wrtie) -------------------------------
/// The rotation state of a symbol
/// input - rotation
/// output - new rotated symbol
let readSymRotationState (sym : Symbol) =
    sym.STransform.Rotation
let writeSymRotationState (rotate : Rotation) (sym : Symbol) = 
    {sym with STransform = {sym.STransform with Rotation = rotate}}
let symRotation : Lens<Symbol, Rotation> = 
    Lens (readSymRotationState, writeSymRotationState)

// ------------------------------------ B8 (read & write) -------------------------------
/// The flip state of a symbol
/// input - flip
/// output - new flipped symbol
let readSymFlipState (sym : Symbol) = 
    sym.STransform.Flipped
let writeSymFlipState (flip : bool) (sym : Symbol) = 
    {sym with STransform = {sym.STransform with Flipped = flip}}
let symFlip : Lens<Symbol, bool> = 
    Lens (readSymFlipState, writeSymFlipState)

// TODO - check write rotation / flip

// ================================== Test Helper Functions =============================================

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


// ------------------------------------- T1 (read) ---------------------------------------
/// The number of pairs of symbols that intersect each other. 
/// See Tick3 for a related function. 
/// Count over all pairs of symbols.
let countSymIntersectSym (sheet: SheetT.Model) =
    // Bounding box with index
    sheet.BoundingBoxes
    // Filter boxes to get only those with intersections
    |> Map.filter (fun idx1 box1 ->
        sheet.BoundingBoxes |> Map.exists ( fun idx2 box2 ->
            idx1 <> idx2 && overlap2DBox box1 box2
        )
    )
    // Count the number of filtered boxes
    |> Map.fold (fun acc _ _  -> acc + 1) 0

// ---------------------------------------- T2 (read) ------------------------------------
/// The number of distinct wire visible segments that intersect with one or more symbols. 
/// See Tick3.HLPTick3.visibleSegments for a helper. 
/// Count over all visible wire segments.
let countSymIntersectWire (sheet: SheetT.Model) = 
    // wires -> check for intersection -> count
    sheet.Wire.Wires
    |> Map.filter (fun _ wire -> findWireSymbolIntersections sheet.Wire wire <> [])
    |> Map.fold (fun acc _ _ -> acc + 1) 0

// ---------------------------------------- T3 (read) -------------------------------------
/// The number of *distinct* pairs of segments that *cross* each other at *right angles*. 
/// *Condition 1* - Does not include 0 length segments 
/// *Condition 2* - or segments on same net intersecting at one end, 
/// *Condition 3* - or segments on same net on top of each other. 
/// Count over whole sheet. 
let countWireRightAngleIntersect (sheet : SheetT.Model) =  
    // Helper functions: 
    let isZeroLength (_, wire) =
        wire.Segments.Length > 0 

    let getSegList (start: XYPos) (seg: XYPos list) =
        seg |> List.scan (fun currentPos vector -> { X = currentPos.X + vector.X; Y = currentPos.Y + vector.Y } ) start

    let generatePairs wires =
        let n = List.length wires
        [0 .. n - 2]
        |> List.collect (fun i -> [i + 1 .. n - 1] |> List.map (fun j -> (wires.[i], wires.[j])))

    let getSegOrient (segPos : XYPos * XYPos) =
        let startPos, endPos = segPos
        match startPos.Y = endPos.Y with
        | true -> "Horizontal"
        | false -> "Vertical"

    // checks for right angle intersection
    let isRightAngleIntersect (segPos1 : XYPos * XYPos) (segPos2 : XYPos * XYPos) = 
        let ort1 = getSegOrient segPos1
        let ort2 = getSegOrient segPos2
        (overlap2D segPos1 segPos2) && (ort1 <> ort2)
        
    // Define a function to count intersections between two wires
    let segRightAngleIntersect ((_, wire1), (_, wire2)) =
        List.pairwise wire1
        |> List.collect (fun seg1 -> List.pairwise wire2 |> List.filter (fun seg2 -> isRightAngleIntersect seg1 seg2))
        |> List.length

    // Filter: 1. non-zero  2. distinct  3. right angle intersection
    let segFilter = 
        sheet.Wire.Wires
        |> Map.toList
        |> List.filter isZeroLength
        |> List.map (fun (wId, wire) -> (wId, (getSegList wire.StartPos (visibleSegments wId sheet))))
        |> generatePairs 
        |> List.map segRightAngleIntersect
        |> List.filter (fun x -> x > 0) // no. of intersection
        |> List.distinct

    // Count the filtered segments
    segFilter.Length

// ----------------------------------------- T4 (read) -------------------------------------------
/// Sum of wiring segment length, counting only one when there are N same-net segments overlapping 
/// (this is the visible wire length on the sheet). 
/// Count over whole sheet.
let sumWireSegmentLength (sheet : SheetT.Model) =
    // helper function for length
    // assumption - always has length
    let getLength (xyPos: XYPos) =
        if xyPos.X <> 0. then xyPos.X else xyPos.Y

    sheet.Wire.Wires
    |> Map.keys
    |> Array.map (fun wire ->
        visibleSegments wire sheet
        |> List.fold (fun sum xyPos -> sum + getLength xyPos) 0.
        )
    |> Array.sum

// ----------------------------------------- T5 (read) ------------------------------------------
/// Count the number of visible wire right-angles. 
/// Count over whole sheet.
let countWireRightAngle (sheet : SheetT.Model) =
    let getNum num =
        if num <> 0 then num - 1 else 0

    // 1. visible  2. right angle  3. sum
    sheet.Wire.Wires
    |> Map.keys
    |> Array.map (fun wire -> visibleSegments wire sheet |> List.length)
    |> Array.sumBy getNum

// -------------------------------------- T6 (read) ---------------------------------------
/// The zero-length segments in a wire with non-zero segments on either side 
/// that have Lengths of opposite signs lead to a wire retracing itself. 
/// Note that this can also apply at the end of a wire (where the zero-length segment is one from the end). 
/// This is a wiring artifact that should never happen but errors in routing or separation can cause it. 
/// Count over the whole sheet. 
/// Return from one function a list of all the segments that retrace, 
/// and also a list of all the end of wire segments that retrace so far 
/// that the next segment (index = 3 or Segments.Length – 4) - starts inside a symbol.
let countRetracingWire (sheet : SheetT.Model) : XYPos list * XYPos list =
    // helper functions
    let isOppositeDirection (v1 : XYPos) (v2 : XYPos) =
        (v1.X * v2.X < 0.0) || (v1.Y * v2.Y < 0.0)

    let isZeroVector (v : XYPos) =
        (v.X = 0.0) && (v.Y = 0.0)

    // check for retracing wires
    let isRetracingWire (segPos : XYPos list) : XYPos list =
        segPos
        |> List.indexed
        |> List.choose (fun (index, seg) ->
            if index + 2 < List.length segPos then
                if isZeroVector segPos.[index + 1] && isOppositeDirection seg segPos.[index + 2] 
                then Some segPos.[index + 1]
                else None
            else None
        )

    // call the check function and fold results
    let segPos, _ =
        sheet.Wire.Wires
        |> Map.fold (fun (accXYPos, accSegs) _ wire ->
            let wireSeg = visibleSegments wire.WId sheet
            (wireSeg @ accXYPos, wire.Segments @ accSegs)
        ) ([], [])
    let retraceSegList = isRetracingWire segPos
    (retraceSegList, segPos)

// =================================== End of helper functions =======================================

