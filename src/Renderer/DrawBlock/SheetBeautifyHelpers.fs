module SheetBeautifyHelpers

open Fable.Core
open CommonTypes
open DrawHelpers
open Fable.React
open Optics
open Operators
open Node.ChildProcess
open DrawModelType
open Symbol
open Elmish
open EEExtensions
open Optics
open Optics.Operators
open DrawHelpers
open Helpers
open CommonTypes
open ModelType
open DrawModelType
open Sheet.SheetInterface
open CommonTypes
open DrawModelType.BusWireT
open DrawModelType.SymbolT
open BusWire
open BusWireUpdateHelpers
open BusWireRoutingHelpers
open SymbolReplaceHelpers

let caseInvariantEqual str1 str2 =
        String.toUpper str1 = String.toUpper str2
let getlabel (model:SheetT.Model) (label:string): SymbolT.Symbol option = 
        model.Wire.Symbol.Symbols
        |> Map.values
        |> Seq.tryFind (fun sym -> caseInvariantEqual label sym.Component.Label)

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team


//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
// B1 The dimensions of a custom component symbol

// let getCustomComponentDimensions (symbol: SymbolT.Symbol) : float * float = 
//     let baseWidth = symbol.Component.W
//     let baseHeight = symbol.Component.H
//     let scaledWidth = 
//         match symbol.HScale with
//         | Some(scale) -> baseWidth * scale
//         | None -> baseWidth
//     let scaledHeight = 
//         match symbol.VScale with
//         | Some(scale) -> baseHeight * scale
//         | None -> baseHeight
//     (scaledWidth, scaledHeight)

// let updateCustomComponentDimensions (symLabel: string) (model: SheetT.Model) (desiredDimensions: float * float): Result<SheetT.Model, string> =
//     let desiredWidth, desiredHeight = desiredDimensions
//     match getlabel model symLabel with
//     | Some symbol ->
//         let originalWidth = symbol.Component.W
//         let originalHeight = symbol.Component.H
//         let hScale = desiredWidth / originalWidth
//         let vScale = desiredHeight / originalHeight
//         let updatedSymbol = { symbol with HScale = Some hScale; VScale = Some vScale }
        
//         let symbolModelUpdated = SymbolUpdate.replaceSymbol model.Wire.Symbol updatedSymbol symbol.Id

//         let updatedSheetModel = model |> Optic.set SheetT.symbol_ symbolModelUpdated
        
//         Ok updatedSheetModel
//     | None -> Error (sprintf "Symbol with label '%s' not found." symLabel)


let CustomComponentDimensionsLens (symbol: SymbolT.Symbol) : Lens<SheetT.Model, (float * float)> =

        let originalWidth = symbol.Component.W
        let originalHeight = symbol.Component.H


        let get (model: SheetT.Model) =
            let width = match symbol.HScale with
                            | Some scale -> originalWidth * scale
                            | None -> originalWidth
            let height = match symbol.VScale with
                            | Some scale -> originalHeight * scale
                            | None -> originalHeight
            (width, height)


        let set (newDimensions: (float * float)) (model: SheetT.Model) =
            let (width, height) = newDimensions

            let hScale = width / originalWidth
            let vScale = height / originalHeight
            let updatedSymbol = { symbol with HScale = Some hScale; VScale = Some vScale }
            let symbolModelUpdated = SymbolUpdate.replaceSymbol model.Wire.Symbol updatedSymbol symbol.Id
            let updatedSheetModel = model |> Optic.set SheetT.symbol_ symbolModelUpdated
            
            updatedSheetModel
        (get, set)




//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
// B5R - Read the position of a port on the sheet
let readPortPosition (sym: SymbolT.Symbol) (port: Port) : XYPos =
    let scale = SymbolT.getScaleF
    let symbolWidth = scale sym.HScale * sym.Component.W
    let symbolHeight = scale sym.VScale * sym.Component.H

    match Map.tryFind port.Id sym.PortMaps.Orientation with
    | Some edge ->
        let portOffset = DrawHelpers.calculatePortOffset edge sym.PortMaps sym.Component port.Id
        match edge with
        | Edge.Top -> { X = sym.Pos.X + portOffset; Y = sym.Pos.Y }
        | Edge.Bottom -> { X = sym.Pos.X + portOffset; Y = sym.Pos.Y + symbolHeight }
        | Edge.Left -> { X = sym.Pos.X; Y = sym.Pos.Y + portOffset }
        | Edge.Right -> { X = sym.Pos.X + symbolWidth; Y = sym.Pos.Y + portOffset }
        | _ -> sym.Pos // Fallback to symbol's position if the edge is not recognized
    | None -> sym.Pos // If the port is not found in the orientation map, fallback to the symbol's position


//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
// B7 RW  Low The rotation state of a symbol
// B8 RW  Low The Flipped state of a symbol 

// let getSymbolRotation (symbol: SymbolT.Symbol) : Rotation =
//     symbol.STransform.Rotation

// let updateSymbolRotation (symLabel: string) (desiredRotation: Rotation) (model: SheetT.Model): Result<SheetT.Model, string> =
//     Error "Not implemented yet"

// let getSymbolFlipped (symbol: SymbolT.Symbol) : bool =
//     symbol.STransform.Flipped

// let updateSymbolFlipped (symLabel: string) (model: SheetT.Model) (desiredFlipped: bool) : Result<SheetT.Model, string> =
//     Error "Not implemented yet"

let SymbolRotationLens (symbol: SymbolT.Symbol) : Lens<SheetT.Model, Rotation> =
    let get (model: SheetT.Model) : Rotation = symbol.STransform.Rotation

    //Rotation -> SheetT.Model -> SheetT.Model
    let set (desiredRotation: Rotation) (model: SheetT.Model) : SheetT.Model =
        let updatedSymbol = { symbol with STransform = { symbol.STransform with Rotation = desiredRotation}}
        let symbolModelUpdated = SymbolUpdate.replaceSymbol model.Wire.Symbol updatedSymbol symbol.Id
        let updatedSheetModel = model |> Optic.set SheetT.symbol_ symbolModelUpdated
        
        updatedSheetModel

    (get, set)

let SymbolFlippedLens (symbol: SymbolT.Symbol) : Lens<SheetT.Model, bool> =
    let get (model: SheetT.Model) : bool = symbol.STransform.Flipped

    //Rotation -> SheetT.Model -> SheetT.Model
    let set (desiredFlipped: bool) (model: SheetT.Model) : SheetT.Model =
        let updatedSymbol = { symbol with STransform = { symbol.STransform with Flipped = desiredFlipped}}
        let symbolModelUpdated = SymbolUpdate.replaceSymbol model.Wire.Symbol updatedSymbol symbol.Id
        let updatedSheetModel = model |> Optic.set SheetT.symbol_ symbolModelUpdated
        
        updatedSheetModel

    (get, set)



//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//

// T1R 
// The number of pairs of symbols that intersect each other.
// Count over all pairs of symbols.
let countIntersectingPairs (sheet: SheetT.Model) =
    let wireModel = sheet.Wire
    let boxes =
        mapValues sheet.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> n,box)
    List.allPairs boxes boxes 
    |> List.filter (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
    |> List.length



//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
// T2R
// The number of distinct wire visible segments that intersect with one or more symbols.
let countSymbolIntersectingWire (sheet: SheetT.Model) =
    let wireModel = sheet.Wire
    wireModel.Wires
    |> Map.fold (fun acc _ wire -> 
        if BusWireRoute.findWireSymbolIntersections wireModel wire <> [] then acc + 1 else acc
    ) 0


//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
// Function to convert a wire into a list of segments, useful for T3-6
type SegVector = {Start: XYPos; Dir: XYPos}
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
        if index > 0 && index < segVecs.Length - 1 && segVecs[index] =~ XYPos.zero then
            let before = segVecs.[0..index-2]
            let coalesced = segVecs.[index-1] + segVecs.[index+1]
            let after = segVecs.[index+2..]
            before @ [coalesced] @ after
        else
            segVecs


    wire.Segments
    |> List.mapi getSegmentVector
    |> (fun segVecs ->
            (segVecs,[1..segVecs.Length-2])
            ||> List.fold tryCoalesceAboutIndex)


let wireToSegments (wId: ConnectionId) (model: SheetT.Model) =
    let segmentsXYPos = visibleSegments wId model

    let addXYPos (p1: XYPos) (p2: XYPos) = 
        { X = p1.X + p2.X; Y = p1.Y + p2.Y }

    segmentsXYPos
    |> List.fold (fun (acc: SegVector list, lastPos: XYPos) pos -> 
        match acc with
        | [] -> ([{ Start = lastPos; Dir = pos }], addXYPos lastPos pos)
        | _ -> (acc @ [{ Start = lastPos; Dir = pos }], addXYPos lastPos pos)
    ) ([], { X = 0.0; Y = 0.0 })
    |> fst



//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
// T3R - Number of visible wire Intersecting at right-angles
let totalRightAngleIntersect (model: SheetT.Model) =
    let isCrossingAtRightAngle seg1 seg2 =
        // Determine the end points of each segment
        let end1 = { X = seg1.Start.X + seg1.Dir.X; Y = seg1.Start.Y + seg1.Dir.Y }
        let end2 = { X = seg2.Start.X + seg2.Dir.X; Y = seg2.Start.Y + seg2.Dir.Y }

        // Check if both segments are vertical or horizontal
        if (seg1.Dir.X = 0.0 && seg2.Dir.X = 0.0) || (seg1.Dir.Y = 0.0 && seg2.Dir.Y = 0.0) then
            false
        else
            // Check for vertical seg1 intersecting with horizontal seg2 or vice versa
            let isSeg1Vertical = seg1.Dir.X = 0.0
            let seg1Range = 
                if isSeg1Vertical then 
                    (min seg1.Start.Y end1.Y, max seg1.Start.Y end1.Y) 
                else (min seg1.Start.X end1.X, max seg1.Start.X end1.X)

            let seg2Range = 
                if isSeg1Vertical then 
                    (min seg2.Start.X end2.X, max seg2.Start.X end2.X) 
                else (min seg2.Start.Y end2.Y, max seg2.Start.Y end2.Y)
            let seg1Pos = if isSeg1Vertical then seg1.Start.X else seg1.Start.Y
            let seg2Pos = if isSeg1Vertical then seg2.Start.Y else seg2.Start.X

            // Check if the static position of one segment falls within the range of the other segment's start and end
            seg1Pos > fst seg2Range && seg1Pos < snd seg2Range &&
            seg2Pos > fst seg1Range && seg2Pos < snd seg1Range

    let countRightAngleIntersections segments =
        let verticals = segments |> List.filter (fun seg -> seg.Dir.X = 0.0)
        let horizontals = segments |> List.filter (fun seg -> seg.Dir.Y = 0.0)

        let mutable count = 0
        verticals
        |> List.collect (fun vSeg -> horizontals |> List.filter (isCrossingAtRightAngle vSeg))
        |> List.length



    // Step 1: Extract and process segments for each wire
    let allSegments =
        model.Wire.Wires
        |> Map.toList // Convert the map of wires to a list of (key, value) pairs
        |> List.collect (fun (wId, _) -> wireToSegments wId model) // Use 'List.collect' to flatten the lists of segments

    // Step 2: Merge overlapping segments for the entire list of segments
    // Step 3: Calculate the total length

    allSegments
    |> countRightAngleIntersections

    // let test1 = {Start = {X=1892.500000; Y=1850.000000}; Dir = {X=0.000000; Y = -50.000000}}
    // let test2 = {Start = {X=1892.500000; Y=1800.000000}; Dir = {X = -190.000000; Y = 0.000000}}

    // if isCrossingAtRightAngle test1 test2 then
    //     1
    // else
    //     0



//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
// T4 Sum of wiring segment length, counting only one when there are N same-net 
// segments overlapping (this is the visible wire length on the sheet). Count over whole 
// sheet. 

// Check if two segments are overlapping
let isOverlapping seg1 seg2 =
    let end1 = { X = seg1.Start.X + seg1.Dir.X; Y = seg1.Start.Y + seg1.Dir.Y }
    let end2 = { X = seg2.Start.X + seg2.Dir.X; Y = seg2.Start.Y + seg2.Dir.Y }
    seg1.Start.X = seg2.Start.X && seg1.Start.Y = seg2.Start.Y ||
    (seg1.Start.X <= end2.X && end1.X >= seg2.Start.X) ||
    (seg1.Start.Y <= end2.Y && end1.Y >= seg2.Start.Y)

// Merge two overlapping segments if they are parallel and overlapping
let mergeSegments seg1 seg2 =
    let endX = max (seg1.Start.X + seg1.Dir.X) (seg2.Start.X + seg2.Dir.X)
    let endY = max (seg1.Start.Y + seg1.Dir.Y) (seg2.Start.Y + seg2.Dir.Y)
    { Start = seg1.Start; Dir = { X = endX - seg1.Start.X; Y = endY - seg1.Start.Y } }

// Merge all parallel overlapping segments in a list
let mergeOverlappingSegments segList =
    let rec mergeRecursive acc remaining =
        match remaining with
        | [] -> acc
        | hd :: tl ->
            let overlaps, nonOverlaps = List.partition (isOverlapping hd) tl
            let mergedSegment = List.fold mergeSegments hd overlaps
            mergeRecursive (mergedSegment :: acc) nonOverlaps
    mergeRecursive [] segList |> List.rev

// Calculate sum of segment lengths
let totalVisibleWiringLength (model: SheetT.Model) =

    model.Wire.Wires
    |> Map.toList // Convert the map of wires to a list<SegVector>
    |> List.collect (fun (wId, _) -> wireToSegments wId model) // flatten the lists of segments
    |> mergeOverlappingSegments // Merge overlapping segments
    |> List.sumBy (fun seg -> abs seg.Dir.X + abs seg.Dir.Y) // Sum length
    // Step 2: Merge overlapping segments for the entire list of segments and Calculate the total length
    // let totalLength =
    //     allSegments
        // |> mergeOverlappingSegments
        // |> List.sumBy (fun seg -> abs seg.Dir.X + abs seg.Dir.Y)

    // totalLength



//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
// T5R - Number of visible wire right-angles. Count over whole sheet.

let countWireRightAngles (wId: ConnectionId) (model: SheetT.Model) =
    let segments = visibleSegments wId model

    segments
    |> List.length
    |> (fun numSegments -> if numSegments = 0 then 0 else numSegments - 1)

/// Sums up the right angles from all wires in the model.
let countTotalRightAngles (model: SheetT.Model) =
    model.Wire.Wires
    |> Map.fold (fun acc wId _ -> acc + countWireRightAngles wId model) 0