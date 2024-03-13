module TestAsserts

open Optics
open CommonTypes
open DrawModelType
open BlockHelpers
open DrawModelType.SymbolT
open DrawModelType.SheetT
open Operators
open SheetBeautifyHelpers
open SheetBeautifyHelpers.EzraHelpers
open BusWidthInferer
open TestDrawBlockHelpers.SimpleSymbol

//----------------------------------------------------------------------------------------------//
//------------------------------------D1T Asserts Functions-------------------------------------//
//----------------------------------------------------------------------------------------------//

let countComponentOverlaps  (sheet: SheetT.Model) : int =
    printfn "Testing countComponentOverlaps"
    let count = SheetBeautifyHelpers.numOfIntersectedSymPairs sheet
    count

/// <summary>
/// Counts the number of straightened wire segments within a given sheet model.
/// </summary>
/// <param name="sample">The sample number being analyzed.</param>
/// <param name="sheet">The sheet model containing wire data.</param>
/// <returns>
/// A string option containing a message about the number of straight wire segments in the sample if any exist; otherwise, None.
/// </returns>
/// <remarks>
/// This function evaluates each wire segment in the sheet model to determine if it is straight (aligned either horizontally or vertically).
/// </remarks>
let countWireStraightInSheet (sheet: SheetT.Model) : int =
    printfn "Testing countWireStraightInSheet"
    let countTransformations (wire: BusWireT.Wire) : int =
        let visibleSegments =
            SheetBeautifyHelpers.SegmentHelpers.visibleSegments wire.WId sheet
        let numberOfVisibleSegments = List.length visibleSegments
        if numberOfVisibleSegments = 1 then
            1
        else
            0

    let numberOfStraightWires =
        sheet
        |> Optic.get wires_
        |> mapValues
        |> List.map countTransformations
        |> List.sum

    numberOfStraightWires

let countWireRoutingLength (sheet: SheetT.Model) : int =
    printfn "Testing countWireRoutingLength"
    let count = SheetBeautifyHelpers.calcVisWireLength sheet
    int count

let isPointCloseToRectangle (point: XYPos) (box: BoundingBox) distanceThreshold =
    let inRange v minV maxV =
        v >= minV - distanceThreshold
        && v <= maxV + distanceThreshold
    inRange point.X box.TopLeft.X (box.TopLeft.X + box.W)
    && inRange point.Y box.TopLeft.Y (box.TopLeft.Y + box.H)

let isSegmentIntersectingOrCloseToBox (segmentStart: XYPos) (segmentEnd: XYPos) (compBox: BoundingBox) =
    let distanceThreshold = -0.001
    isPointCloseToRectangle segmentStart compBox distanceThreshold
    || isPointCloseToRectangle segmentEnd compBox distanceThreshold

let isWireSquashed (wire: BusWireT.Wire) (sheet: SheetT.Model) : bool =
    let componentsBoundingBoxes: BoundingBox list =
        sheet.Wire.Symbol.Symbols
        |> mapValues
        |> List.map SheetBeautifyHelpers.getSymBoundingBox

    let segmentTooCloseToComponent segment =
        componentsBoundingBoxes
        |> List.exists (fun compBox ->
            let segmentStart = wire.StartPos
            let segmentEnd = segmentStart + segment
            isSegmentIntersectingOrCloseToBox segmentStart segmentEnd compBox)

    let wireSegments =
        SheetBeautifyHelpers.SegmentHelpers.visibleSegments wire.WId sheet
    wireSegments
    |> List.exists segmentTooCloseToComponent

let countWireSquashedInSheet (sheet: SheetT.Model) : int =
    printfn "Testing countWireSquashedInSheet"
    let count =
        sheet.Wire.Wires
        |> Map.toList
        |> List.map snd
        |> List.map (fun w -> isWireSquashed w sheet)
        |> List.filter (fun n -> n = true)
        |> List.length
    count


//----------------------------------------------------------------------------------------------//
//------------------------------------D2T Asserts Functions-------------------------------------//
//----------------------------------------------------------------------------------------------//

/// <summary> Checks if two wire segments intersect by evaluating their horizontal and vertical overlap. </summary>
/// <param name="seg1">The first wire segment to check for intersection.</param>
/// <param name="seg2">The second wire segment to check for intersection.</param>
/// <returns>
/// True if the segments intersect; otherwise, false.
/// </returns>
let checkWireCrossing (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) : bool =
    let horizontalOverlap =
        overlap1D (seg1.Start.X, seg1.End.X) (seg2.Start.X, seg2.End.X)
    let verticalOverlap =
        overlap1D (seg1.Start.Y, seg1.End.Y) (seg2.Start.Y, seg2.End.Y)
    horizontalOverlap && verticalOverlap

/// <summary>
/// Counts the number of wire crossings in a given sheet and reports if any are found.
/// </summary>
/// <param name="sample">The sample number being tested, for identification in test results.</param>
/// <param name="sheet">The sheet model containing the wires to be analyzed for crossings.</param>
/// <returns>
/// A string option containing a message if crossings are found, specifying the number of crossings and the sample number; otherwise, None if no crossings are detected.
/// </returns>
/// <remarks>
/// This function evaluates all wire segments within the sheet to determine if any intersect with others. It only counts intersections between different wires or non-adjacent segments of the same wire, ignoring overlaps of directly connected segments. The purpose is to identify potential schematic layout issues where wires cross each other, which could indicate design or routing inefficiencies.
/// </remarks>
let countWiresOverlapInSheet (sheet: SheetT.Model) : int =
    printfn "Testing countWiresOverlapInSheet"
    // This function serves general purpose and will consider any overlapping segments, to count proper crossings use countWiresCrossingInSheet
    let allWires = sheet.Wire.Wires |> Map.toList |> List.map snd
    let allAbsSegments = allWires |> List.collect getAbsSegments
    let segmentPairs = distinctPairs allAbsSegments
    let crossings =
        segmentPairs
        |> List.filter (fun (seg1, seg2) ->
            seg1.Segment.WireId <> seg2.Segment.WireId
            || seg1.Segment.Index <> seg2.Segment.Index)
        |> List.filter (fun (seg1, seg2) -> checkWireCrossing seg1 seg2)
    let numberOfCrossings = crossings.Length / 2
    numberOfCrossings
let countWiresCrossingInSheet  (sheet: SheetT.Model) : int  =
    printfn "Testing countWiresCrossingInSheet"
    let count = visibleWireIntersections sheet
    count

let countWireIntersectsSymbolInSheet (sheet: SheetT.Model) : int =
    printfn "Testing countWireIntersectsSymbolInSheet"
    let count = SheetBeautifyHelpers.numOfIntersectSegSym sheet
    count

//----------------------------------------------------------------------------------------------//
//------------------------------------D3T Asserts Functions-------------------------------------//
//----------------------------------------------------------------------------------------------//

let countBendsInSheet (sheet: SheetT.Model) : int =
    printfn "Testing countBendsInSheet"
    let count = countVisibleRightAngles sheet
    count

//----------------------------------------------------------------------------------------------//
//-------------------------------------Comparison Asserts---------------------------------------//
//----------------------------------------------------------------------------------------------//

/// <summary>
/// If the number of right angles increased by beautification, this is considered a failure.
/// If it's the same, it's not considered a failure; other metrics will be used here.
/// </summary>
/// <param name="sheetBeforeBeautify">The sheet model before beautification.</param>
/// <param name="sheetAfterBeautify">The sheet model after beautification.</param>
/// <remarks>
/// Use with 'countBendsInSheet' metric
/// </remarks>
/// <returns>
/// A failure message if the number of right angles increased in the sample;
/// otherwise, None.
/// </returns>
let failOnBeautifyIncreasesRightAngles (sheetBeforeBeautify: SheetT.Model) (sheetAfterBeautify: SheetT.Model) =
    let rAnglesBefore = countBendsInSheet sheetBeforeBeautify
    let rAnglesAfter = countBendsInSheet sheetAfterBeautify
    match rAnglesAfter > rAnglesBefore with
        | true -> Some $"FAILURE: Beautify increased no. right angles"
        | false -> None

/// <remarks>
/// Use with 'countComponentOverlaps' metric
/// </remarks>
let failOnBeautifyCausesSymbolOverlap (sheetBeforeBeautify: SheetT.Model) (sheetAfterBeautify: SheetT.Model) =
    let overlapAfter = countComponentOverlaps sheetAfterBeautify
    match overlapAfter > 0 with
        | true -> Some $"FAILURE: Beautify caused symbols to overlap"
        | false -> None

/// <remarks>
/// Use with 'countWireIntersectsSymbolInSheet' metric
/// </remarks>
let failOnBeautifyIncreasesSegSymIntersect (sheetBeforeBeautify: SheetT.Model) (sheetAfterBeautify: SheetT.Model) =
    let intersectsBefore = countWireIntersectsSymbolInSheet sheetBeforeBeautify
    let intersectsAfter = countWireIntersectsSymbolInSheet sheetAfterBeautify
    match intersectsAfter > intersectsBefore with
        | true -> Some $"FAILURE: Beautify increased no. wire segment/symbol intersections"
        | false -> None

/// <remarks>
/// Use with 'countWireStraightInSheet' metric
/// </remarks>
let failOnBeautifyDecreasesStraightWires (sheetBeforeBeautify: SheetT.Model) (sheetAfterBeautify: SheetT.Model) =
    let straightWiresBefore = countWireStraightInSheet sheetBeforeBeautify
    let straightWiresAfter = countWireStraightInSheet sheetAfterBeautify
    match straightWiresBefore > straightWiresAfter with
        | true -> Some $"FAILURE: Beautify decreased no. straight wires in sheet"
        | false -> None

/// <remarks>
/// Use with 'countWireRoutingLength' metric
/// </remarks>
let failOnBeautifyIncreasesWireLength (sheetBeforeBeautify: SheetT.Model) (sheetAfterBeautify: SheetT.Model) =
    let lengthBefore = countWireRoutingLength sheetBeforeBeautify
    let lengthAfter = countWireRoutingLength sheetAfterBeautify
    match lengthAfter > lengthBefore with
        | true -> Some $"FAILURE: Beautify increased visible wire length in sheet"
        | false -> None

/// <remarks>
/// Use with 'countWireSquashedInSheet' metric
/// </remarks>
let failOnBeautifyIncreasesSquashedWires (sheetBeforeBeautify: SheetT.Model) (sheetAfterBeautify: SheetT.Model) =
    let squashesBefore = countWireSquashedInSheet sheetBeforeBeautify
    let squashesAfter = countWireSquashedInSheet sheetAfterBeautify
    match squashesAfter > squashesBefore with
        | true -> Some $"FAILURE: Beautify increased no. of squased wires in sheet"
        | false -> None

/// <remarks>
/// Use with 'countWiresCrossingInSheet' metric
/// </remarks>
let failOnBeautifyIncreasesRightAngleCrossings (sheetBeforeBeautify: SheetT.Model) (sheetAfterBeautify: SheetT.Model) =
    let crossingsBefore = countWiresCrossingInSheet sheetBeforeBeautify
    let crossingsAfter = countWiresCrossingInSheet sheetAfterBeautify
    match crossingsAfter > crossingsBefore with
        | true -> Some $"FAILURE: Beautify increased no. of right angle crossings in sheet"
        | false -> None

/// <remarks>
/// Use with 'countWiresOverlapInSheet' metric
/// </remarks>
let failOnBeautifyIncreasesOverlappingWires (sheetBeforeBeautify: SheetT.Model) (sheetAfterBeautify: SheetT.Model) =
    let overlapsBefore = countWiresOverlapInSheet sheetBeforeBeautify
    let overlapsAfter = countWiresOverlapInSheet sheetAfterBeautify
    match overlapsAfter > overlapsBefore with
        | true -> Some $"FAILURE: Beautify increased no. of overlapping wires"
        | false -> None


