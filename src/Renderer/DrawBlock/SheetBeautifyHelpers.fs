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


//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team


// Key Type Difficulty Value read or written 
// B1R, B1W RW 
//  Value read or written: The dimensions of a custom component symbol


/// Calculates the final dimensions of a symbol, considering potential scaling.
let caseInvariantEqual str1 str2 =
        String.toUpper str1 = String.toUpper str2
let getlabel (model:SheetT.Model) (label:string): SymbolT.Symbol option = 
        model.Wire.Symbol.Symbols
        |> Map.values
        |> Seq.tryFind (fun sym -> caseInvariantEqual label sym.Component.Label)
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
// B7R, B7W RW Low The rotation state of a symbol 
let getSymbolById (model: SheetT.Model) (id: ComponentId) : Option<SymbolT.Symbol> =
    let symbols = model.Wire.Symbol.Symbols
    symbols |> Map.tryFind id

let getSymbolRotation (symbol: SymbolT.Symbol) : Rotation =
    symbol.STransform.Rotation

let getSymbolFlipped (symbol: SymbolT.Symbol) : bool =
    symbol.STransform.Flipped


//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//

// T1R R Low
// The number of pairs of symbols that intersect each other. See Tick3 for a related function.
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

// T2R R Low 
// The number of distinct wire visible segments that intersect with one or more symbols. See Tic
let countSymbolIntersectingWire (sheet: SheetT.Model) =
    let wireModel = sheet.Wire
    wireModel.Wires
    |> Map.fold (fun acc _ wire -> 
        if BusWireRoute.findWireSymbolIntersections wireModel wire <> [] then acc + 1 else acc
    ) 0


//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
// T4 Sum of wiring segment length, counting only one when there are N same-net 
// segments overlapping (this is the visible wire length on the sheet). Count over whole 
// sheet. 

// Step 1: Convert wire to segments (similar to visibleSegments but simplified for this context)
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
    let wireStart = model.Wire.Wires.[wId].StartPos
    let addXYPos (p1: XYPos) (p2: XYPos) = 
        { X = p1.X + p2.X; Y = p1.Y + p2.Y }

    segmentsXYPos
    |> List.fold (fun (acc: SegVector list, lastPos: XYPos) pos -> 
        match acc with
        | [] -> ([{ Start = lastPos; Dir = pos }], addXYPos lastPos pos)
        | _ -> (acc @ [{ Start = lastPos; Dir = pos }], addXYPos lastPos pos)
    ) ([], wireStart)
    |> fst
    

// Step 2: Detect and remove overlapping segments

// Function to determine if two segments overlap
let isOverlapping seg1 seg2 =
    let end1 = { X = seg1.Start.X + seg1.Dir.X; Y = seg1.Start.Y + seg1.Dir.Y }
    let end2 = { X = seg2.Start.X + seg2.Dir.X; Y = seg2.Start.Y + seg2.Dir.Y }
    seg1.Start.X = seg2.Start.X && seg1.Start.Y = seg2.Start.Y ||
    (seg1.Start.X <= end2.X && end1.X >= seg2.Start.X) ||
    (seg1.Start.Y <= end2.Y && end1.Y >= seg2.Start.Y)

// Function to merge two overlapping segments
let mergeSegments seg1 seg2 =
    let endX = max (seg1.Start.X + seg1.Dir.X) (seg2.Start.X + seg2.Dir.X)
    let endY = max (seg1.Start.Y + seg1.Dir.Y) (seg2.Start.Y + seg2.Dir.Y)
    { Start = seg1.Start; Dir = { X = endX - seg1.Start.X; Y = endY - seg1.Start.Y } }

// Function to merge all overlapping segments in a list
let mergeOverlappingSegments segList =
    let rec mergeRecursive acc remaining =
        match remaining with
        | [] -> acc
        | hd :: tl ->
            let overlaps, nonOverlaps = List.partition (isOverlapping hd) tl
            let mergedSegment = List.fold mergeSegments hd overlaps
            mergeRecursive (mergedSegment :: acc) nonOverlaps
    mergeRecursive [] segList |> List.rev




// Step 3: Calculate sum of segment lengths

let printSegVectorList segList =
    segList
    |> List.iter (fun seg ->
        printfn "Start: (X=%f, Y=%f), Dir: (X=%f, Y=%f)" seg.Start.X seg.Start.Y seg.Dir.X seg.Dir.Y
    )

let totalVisibleWiringLength (model: SheetT.Model) =
    // Step 1: Extract and process segments for each wire
    let allSegments =
        model.Wire.Wires
        |> Map.toList // Convert the map of wires to a list of (key, value) pairs
        |> List.collect (fun (wId, _) -> wireToSegments wId model) // Use 'List.collect' to flatten the lists of segments

    // Step 2: Merge overlapping segments for the entire list of segments
    // Step 3: Calculate the total length
    let totalLength =
        allSegments
        |> mergeOverlappingSegments
        |> List.sumBy (fun seg -> abs seg.Dir.X + abs seg.Dir.Y) // Sum the lengths of all segments (X + Y components

    // printSegVectorList allSegments
    totalLength




//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
// T3R - Number of visible wire right-angles
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

// printSegVectorList verticals
// printfn "-----------------------------------------------------"
// printSegVectorList horizontals
// printfn "-----------------------------------------------------"
// printfn "-----------------------------------------------------"
// printfn "-----------------------------------------------------"
// printfn "-----------------------------------------------------"
// printfn "-----------------------------------------------------"


let countRightAngleIntersections segments =
    let verticals = segments |> List.filter (fun seg -> seg.Dir.X = 0.0)
    let horizontals = segments |> List.filter (fun seg -> seg.Dir.Y = 0.0)

    let mutable count = 0
    verticals
    |> List.collect (fun vSeg -> horizontals |> List.filter (isCrossingAtRightAngle vSeg))
    |> List.length


let totalRightAngleIntersect (model: SheetT.Model) =
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
// T5R - Number of visible wire right-angles. Count over whole sheet.


let countWireRightAngles (wId: ConnectionId) (model: SheetT.Model) =
    let segments = visibleSegments wId model
    // printfn "segments:" 
    let numSegments = List.length segments
    // printfn "numSegments: %A" numSegments
    if numSegments > 0 then numSegments - 1 else 0
    // The check `if numSegments > 0 then ... else 0` ensures that wires with no visible segments
    // do not contribute to the right angle count negatively or incorrectly.

/// Sums up the right angles from all wires in the model.
let countTotalRightAngles (model: SheetT.Model) =
    // printfn "countTotalRightAngles"
    model.Wire.Wires
    |> Map.fold (fun acc wId _ -> acc + countWireRightAngles wId model) 0
    // |> Map.fold (fun acc key _ -> 
    //     let newAcc = acc + countWireRightAngles key model
    //     // Print the updated accumulator value
    //     printfn "Current total right angles: %d" newAcc
    //     newAcc
    // ) 0