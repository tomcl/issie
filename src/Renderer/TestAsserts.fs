module Renderer.TestAsserts

open CommonTypes.JSONComponent
open GenerateData
open EEExtensions
open ModelType
open Optics
open CommonTypes
open DrawModelType
open BlockHelpers
open DrawModelType.SymbolT
open DrawModelType.SheetT
open Operators
open System
open SheetBeautifyHelpers
open SheetBeautifyHelpers.EzraHelpers
open BusWidthInferer
open TestDrawBlockSimpleSymbol.SimpleSymbolTesting

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

let countComponentOverlaps  (sheet: SheetT.Model) : int =
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
    let count = SheetBeautifyHelpers.calcVisWireLength sheet
    printfn "Testing countWireRoutingLength"
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
    let count = SheetBeautifyHelpers.numOfWireRightAngleCrossings sheet
    count

let countWireIntersectsSymbolInSheet (sheet: SheetT.Model) : int =
    let count = SheetBeautifyHelpers.numOfIntersectSegSym sheet
    count

//----------------------------------------------------------------------------------------------//
//------------------------------------D3T Asserts Functions-------------------------------------//
//----------------------------------------------------------------------------------------------//

let countBendsInSheet (sheet: SheetT.Model) : int =
    let count = SheetBeautifyHelpers.numOfVisRightAngles sheet
    count

//----------------------------------------------------------------------------------------------//
//-------------------------------------Comparison Asserts---------------------------------------//
//----------------------------------------------------------------------------------------------//

/// <summary>
/// If the number of right angles increased by beautification, this is considered a failure.
/// If it's the same, it's not considered a failure; other metrics will be used here.
/// </summary>
/// <param name="rAnglesBefore">The number of right angles before beautification.</param>
/// <param name="rAnglesAfter">The number of right angles after beautification.</param>
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


module D2TestBuild =

    open CommonTypes

    let rand = Random()

    let createRandomCustomComponent numInputs =
        {
           Name = "Custom"
           InputLabels = List.init numInputs (fun i -> sprintf "In%d" i, i)
           OutputLabels = ["Out", 0]
           Form = None
           Description = None
        }


    let portInfoByComponentType (compType: ComponentType) : (int * int) = // (numInputs, numOutputs)
        match compType with
        | GateN(_, numInputs) -> (numInputs, 1)
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
        | n when n < 90 -> // 10% for FlipFlop
            randomFlipFlopType ()
        | _ -> // 10% for FlipFlop
            // randomFlipFlopType ()
            Custom (createRandomCustomComponent (Random().Next(1, 4)))

    let randomFlipType () =
        let flips = [| FlipHorizontal; FlipVertical |]
        flips.[rand.Next(flips.Length)]

    let defineGridParameters (gridLength: float) (numberOfColumns: int) : float list =
        let columnWidth = gridLength / float numberOfColumns
        List.init numberOfColumns (fun i -> (float i + 0.5) * columnWidth)

    let randomPositionInColumn (gridLength: float) (columnXPositions: float list) : XYPos =
        let columnIndex = rand.Next(columnXPositions.Length)
        let xPosition = columnXPositions.[columnIndex]
        let yPosition = rand.NextDouble() * gridLength
        { X = xPosition; Y = yPosition }

    let generateRandomPositionInColumn (maxCoord: float) (numberOfColumns: int) : XYPos =
        let columnXPositions = defineGridParameters maxCoord numberOfColumns
        randomPositionInColumn maxCoord columnXPositions

    let deviatePos (minDev: float) (maxDev: float) (pos: XYPos) =
        let rand = System.Random()
        let deviate =  minDev + (maxDev - minDev) * rand.NextDouble()
        { X = pos.X + deviate; Y = pos.Y + deviate }


    let getGridPositions (gridDimension: XYPos) (numberOfColumns: int) (numberOfRows: int) =
        let xPositions = defineGridParameters gridDimension.X numberOfColumns
        let yPositions = defineGridParameters gridDimension.Y numberOfRows
        List.collect (fun y -> List.map (fun x -> { X = x; Y = y }) xPositions) yPositions



    let randomSTransform () =
        { Rotation =
            match Random().Next(4) with
            | 0 -> Degree0
            | 1 -> Degree90
            | 2 -> Degree180
            | _ -> Degree270
          Flipped =
            Random().Next(2) = 0 }


    let getRandomEdge _ : Edge =
        match Random().Next(4) with
        | 0 -> Top
        | 1 -> Bottom
        | 2 -> Left
        | _ -> Right


    let createRandomSimpleSymbol (id: int) (maxCoord: float) (numberOfColumns: int) : SimpleSymbol =
        let compType = randomComponentType ()
        let pos = (generateRandomPositionInColumn maxCoord numberOfColumns)
        createSimpleSymbol (sprintf "Comp%d" id) compType pos (randomSTransform ())


    let getRandomPort (comp: ComponentType) (portType: PortType) : int =
        let inputs, outputs = portInfoByComponentType comp
        match portType with
        | PortType.Output -> rand.Next(outputs)
        | PortType.Input -> rand.Next(inputs)

    let createRandomSimpleConnection source target =
        { Source = { Label = source.SymLabel; PortNumber = getRandomPort source.CompType PortType.Output }
          Target = { Label = string target.SymLabel; PortNumber = getRandomPort target.CompType PortType.Input } }

    let makeConnections (components: SimpleSymbol list) =
        List.zip components (List.tail components)
        |> List.map (fun (source, target) -> createRandomSimpleConnection source target)


    let rec generateAndConnectComponents (numComponents: int) (maxCoord: float) (numberOfColumns: int) : TestModel =
        let components =
            List.init numComponents (fun id -> createRandomSimpleSymbol id maxCoord numberOfColumns)
        let connections = makeConnections components
        let testModel = { SimpleSymbols = components; Connections = connections }

        let sheetModel =
            try Builder.placeTestModel testModel
            with
            | _ -> failwith "Error placing test model on sheet."

        if SheetBeautifyHelpers.numOfIntersectedSymPairs sheetModel > 0 then
            generateAndConnectComponents numComponents maxCoord numberOfColumns
        else
            testModel


    let getRandomSymbol (simSymbols: SimpleSymbol list) =
        List.item (rand.Next(simSymbols.Length)) simSymbols

    let createRandomConnection (simSymbols: SimpleSymbol list) =
        let source = getRandomSymbol simSymbols
        let target = getRandomSymbol simSymbols
        createRandomSimpleConnection source target

    let createNRandomConnections (simSymbols: SimpleSymbol list) (n: int) =
        List.init n (fun _ -> createRandomConnection simSymbols)


    let updateConnections (newConnections: SimpleConnection list) (model: TestModel)  =
        let removeIllegalConnections (connections: SimpleConnection list) (newConnections: SimpleConnection list) =
            connections @ newConnections
            |> List.distinctBy (fun c -> (c.Target))

        Optic.map connections_ (fun connections -> removeIllegalConnections connections newConnections) model

    let rec buildConstrainedCircuit (minDev: float) (maxDev: float) (numberOfRows: int) (numberOfColumns: int) (gridDimension: XYPos) : Model =
        let gridPositions = getGridPositions gridDimension numberOfColumns numberOfRows
        let components =
            gridPositions
            |> List.map (deviatePos minDev maxDev)
            |> List.mapi (fun i pos -> createSimpleSymbol (sprintf "Comp%d" i) (randomComponentType ()) pos {Rotation = Degree0; Flipped = false})

        components
        |> (fun components -> createTestModel components (makeConnections components))
        |> updateConnections (createNRandomConnections components (components.Length / 2))
        |> Builder.placeTestModel
        |> (fun sheet ->
            match SheetBeautifyHelpers.numOfIntersectedSymPairs sheet  with
            | 0 -> sheet
            | _ -> buildConstrainedCircuit minDev maxDev numberOfRows numberOfColumns gridDimension
        )

    let buildTestCircuit (numComponents: int) (maxCoord: float) (numberOfColumns: int) : SheetT.Model =
        let testModel = generateAndConnectComponents numComponents maxCoord numberOfColumns
        let sheetModel = Builder.placeTestModel testModel
        sheetModel




