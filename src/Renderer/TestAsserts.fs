module Renderer.TestAsserts

open GenerateData
open EEExtensions
open Optics
open CommonTypes
open DrawModelType
open BlockHelpers
open DrawModelType.SymbolT
open DrawModelType.SheetT
open Operators
open System
open SheetBeautifyHelpers.EzraHelpers
open SheetBeautifyHelpers.Constants

//----------------------------------------------------------------------------------------------//
//-----------------------------------Global Asserts Functions-----------------------------------//
//----------------------------------------------------------------------------------------------//

/// Ignore sheet and fail on the specified sample, useful for displaying a given sample
let failOnSampleNumber (sampleToFail: int) (sample: int) _sheet =
    if sampleToFail = sample then
        Some $"Failing forced on Sample {sampleToFail}."
    else
        None
/// Fails all tests: useful to show in sequence all the sheets generated in a test
let failOnAllTests (sample: int) _ = Some <| $"Sample {sample}"
/// Fail when sheet contains a wire segment that overlaps (or goes too close to) a symbol outline
let failOnWireIntersectsSymbol (sample: int) (sheet: SheetT.Model) =
    let wireModel = sheet.Wire
    wireModel.Wires
    |> Map.exists (fun _ wire ->
        BusWireRoute.findWireSymbolIntersections wireModel wire
        <> [])
    |> (function
    | true -> Some $"Wire intersects a symbol outline in Sample {sample}"
    | false -> None)

//----------------------------------------------------------------------------------------------//
//------------------------------------D1T Asserts Functions-------------------------------------//
//----------------------------------------------------------------------------------------------//

let countComponentOverlaps (sample: int) (sheet: SheetT.Model) : string option =
    let count = SheetBeautifyHelpers.numOfIntersectedSymPairs sheet
    if count > 0 then
        Some $"Sample {sample} has {count} component overlaps"
    else
        None

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
let countWireStraightInSheet (sample: int) (sheet: SheetT.Model) : string option =

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
        |> mapValuesToList
        |> List.map countTransformations
        |> List.sum

    if numberOfStraightWires > 0 then
        Some $"Sample {sample} has {numberOfStraightWires} straight wires."
    else
        None

let countWireRoutingLength (sample: int) (sheet: SheetT.Model) : string option =
    let count = SheetBeautifyHelpers.calcVisWireLength sheet
    if count > 0 then
        Some $"Sample {sample} has {count} wire routing lenght"
    else
        None

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
        |> mapValuesToList
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

let countWireSquashedInSheet (sample: int) (sheet: SheetT.Model) : string option =
    let count =
        sheet.Wire.Wires
        |> Map.toList
        |> List.map snd
        |> List.map (fun w -> isWireSquashed w sheet)
        |> List.filter (fun n -> n = true)
        |> List.length

    if count > 0 then
        Some $"Sample {sample} has {count} squashed wires"
    else
        None

//----------------------------------------------------------------------------------------------//
//------------------------------------D2T Asserts Functions-------------------------------------//
//----------------------------------------------------------------------------------------------//

/// <summary>
/// Checks if two wire segments intersect by evaluating their horizontal and vertical overlap.
/// </summary>
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
let countWiresOverlapInSheet (sample: int) (sheet: SheetT.Model) : string option =
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
    if numberOfCrossings > 0 then
        Some $"Sample {sample} has {numberOfCrossings} wire crossings"
    else
        None

let countWiresCrossingInSheet (sample: int) (sheet: SheetT.Model) : string option =
    let count = SheetBeautifyHelpers.numOfWireRightAngleCrossings sheet
    if count > 0 then
        Some $"Sample {sample} has {count} wire crossings"
    else
        None

let countWireIntersectsSymbolInSheet (sample: int) (sheet: SheetT.Model) : string option =
    let count = SheetBeautifyHelpers.numOfIntersectSegSym sheet
    if count > 0 then
        Some $"Sample {sample} has {count} wire intersects symbol"
    else
        None

//----------------------------------------------------------------------------------------------//
//------------------------------------D3T Asserts Functions-------------------------------------//
//----------------------------------------------------------------------------------------------//

let countBendsInSheet (sample: int) (sheet: SheetT.Model) : string option =
    let count = SheetBeautifyHelpers.numOfVisRightAngles sheet
    if count > 0 then
        Some $"Sample {sample} has {count} bends"
    else
        None

module D2TestBuild =
    //----------------------------------------------------------------------------------------------//
    //------------------------------------D3T Asserts Functions-------------------------------------//
    //----------------------------------------------------------------------------------------------//
    open TestDrawBlockD3.D3Testing

    let rand = Random()

    let portInfoByComponentType (compType: ComponentType) : (int * int) = // (numInputs, numOutputs)
        match compType with
        | GateN(gateType, numInputs) -> (numInputs, 1) 
        | Mux2 -> (2, 1)
        | Mux4 -> (4, 1)
        | Mux8 -> (8, 1)
        | Demux2 -> (1, 2)
        | Demux4 -> (1, 4)
        | Demux8 -> (1, 8)
        | DFF
        | DFFE -> (1, 1) // Simplification !!
        | _ -> (0, 0) 

    let randomGateComponentType () =
        [| And; Or; Xor; Nand; Nor; Xnor |]
        |> Array.item (Random().Next(6))

    let randomMuxDemuxType () =
        let types = [| Mux2; Mux4; Mux8; Demux2; Demux4; Demux8 |]
        // Mux8; Demux2; Demux4; Demux8
        types.[Random().Next(types.Length)]

    let randomFlipFlopType () =
        [| DFF; DFFE |] |> Array.item (Random().Next(2))

    let randomComponentType () : ComponentType =
        let choice = Random().Next(100)
        match choice with
        | n when n < 50 -> // 50% for GateN
            GateN(randomGateComponentType (), Random().Next(1, 5)) // Randomly selecting between 1 to 4 inputs
        | n when n < 80 -> // 30% for Mux/Demux
            randomMuxDemuxType ()
        | _ -> // 20% for FlipFlop
            randomFlipFlopType ()

    let randomFlipType () =
        let flips = [| FlipHorizontal; FlipVertical |]
        flips.[rand.Next(flips.Length)]

    let defineGridParameters (maxCoord: float) (numberOfColumns: int) : float list =
        let columnWidth = maxCoord / float numberOfColumns
        List.init numberOfColumns (fun i -> (float i + 0.5) * columnWidth)

    let randomPositionInColumn (maxCoord: float) (columnXPositions: float list) : XYPos =
        let columnIndex = rand.Next(columnXPositions.Length)
        let xPosition = columnXPositions.[columnIndex]
        let yPosition = rand.NextDouble() * maxCoord
        { X = xPosition; Y = yPosition }

    let generateRandomPositionInColumn (maxCoord: float) (numberOfColumns: int) : XYPos =
        let columnXPositions = defineGridParameters maxCoord numberOfColumns
        randomPositionInColumn maxCoord columnXPositions

    let randomPosition (maxCoord: float) =
        { X = rand.NextDouble() * maxCoord; Y = rand.NextDouble() * maxCoord }

    let randomSTransform () =
        { Rotation =
            match Random().Next(4) with
            | 0 -> Degree0
            | 1 -> Degree90
            | 2 -> Degree180
            | _ -> Degree270
          Flipped =
            if Random().Next(2) = 0 then
                true
            else
                false }

    let createSimpleSymbol (id: int) (maxCoord: float) (numberOfColumns: int) : SimpleSymbol =
        let compType = randomComponentType ()
        let numInputs, numOutputs = portInfoByComponentType compType
        let portMaps =
            { Order =
                Map.empty
                    .Add(Top, List.init numInputs (sprintf "In%d"))
                    .Add(Bottom, List.init numOutputs (sprintf "Out%d"))
              Orientation = Map.empty }
        { SymLabel = sprintf "Comp%d" id
          CompType = compType
          Position = generateRandomPositionInColumn maxCoord numberOfColumns
          STransform = randomSTransform ()
          PortMaps = portMaps }
    let getRandomPort (portMaps: PortMaps) (edge: Edge) : int =
        let ports = portMaps.Order.[edge]
        if ports.Length > 0 then
            rand.Next(ports.Length) // Select a random port if available
        else
            0
    let rec generateAndConnectComponents (numComponents: int) (maxCoord: float) (numberOfColumns: int) : TestModel =
        let components =
            List.init numComponents (fun id -> createSimpleSymbol id maxCoord numberOfColumns)

        let connections =
            List.zip components (List.tail components)
            |> List.map (fun (source, target) ->
                { Source = { Label = source.SymLabel; PortNumber = getRandomPort source.PortMaps Bottom }
                  Target = { Label = target.SymLabel; PortNumber = getRandomPort target.PortMaps Top } })

        let testModel = { SimpleSymbols = components; Connections = connections }

        let sheetModel =
            try
                Builder.placeTestModel testModel
            with _ ->
                failwith "Error placing test model on sheet."

        if SheetBeautifyHelpers.numOfIntersectedSymPairs sheetModel > 0 then
            generateAndConnectComponents numComponents maxCoord numberOfColumns
        else
            testModel

    let buildTestCircuit (numComponents: int) (maxCoord: float) (numberOfColumns: int) : SheetT.Model =
        let testModel = generateAndConnectComponents numComponents maxCoord numberOfColumns
        let sheetModel = Builder.placeTestModel testModel
        sheetModel
