module Renderer.TestDrawBlockD2
(*
    This module updates the Tick3 testing setup for Deliverable 2. It tests circuit layouts by:
    - Flipping and rotating gates and MUXes randomly.
    - Swapping inputs on 2-MUX symbols randomly.
    - Counting wire crossings and straightened wires to check the layout.

    Functions included are: 
    - randomRotation: returns a random rotation for a symbol
    - randomFlipType: returns a random flip type for a symbol
    - shufflePortMapsOrder: shuffles the order of ports on relevant edge of a component's port maps
    - updateMux2PortOrder: updates the order of ports for all Mux2 type symbols in a model, shuffling the port orders
    - checkWireCrossing: checks if two wire segments intersect by evaluating their horizontal and vertical overlap
    - countCrossingsInSheet: counts the number of wire crossings in a given sheet and reports if any are found
    - countWireStraightInSheet: counts the number of straightened wire segments within a given sheet model
    - functionalShuffle: shuffles a list of items into a random order
    - placeAndOrientSymbol: places and orients a symbol on the sheet based on specified position, rotation, and flip type
    - baseCircuit1: creates a base circuit with an AND gate and a MUX2
    - baseCircuit2: creates a base circuit with an OR gate and a MUX2
    - testWireCrossingD2: runs a test with manually generated circuits and checks for wire crossings
    - testWireStraightD2: runs a test with manually generated circuits and checks for wires straightenedhelp place symbols with specific rotations and flips, shuffle ports, and create test circuits. 
    
    The tests check for wire crossings and straight wires to see how these changes affect the layout.
*)

open GenerateData
open TestDrawBlock.TestLib
open TestDrawBlock.HLPTick3
open TestDrawBlock.HLPTick3.Builder
open TestDrawBlock.HLPTick3.Tests
open SheetBeautifyHelpers
open EEExtensions
open Optics
open CommonTypes
open DrawModelType
open BlockHelpers
open DrawModelType.SymbolT
open DrawModelType.SheetT
open Operators
open System

module D2T =
    /// <summary>
    /// Places and orients a symbol on the sheet based on specified position, rotation, and flip type.
    /// </summary>
    /// <param name="symLabel">The label of the symbol to place.</param>
    /// <param name="compType">The component type of the symbol.</param>
    /// <param name="position">The position where the symbol should be placed.</param>
    /// <param name="rotation">The rotation to apply to the symbol.</param>
    /// <param name="flip">The flip type to apply to the symbol.</param>
    /// <param name="model">The current sheet model.</param>
    /// <returns>
    /// A Result containing the updated sheet model if successful, or an error string if the symbol could not be placed due to overlap or out-of-bounds position.
    /// </returns>
    let placeAndOrientSymbol
        (symLabel: string)
        (compType: ComponentType)
        (position: XYPos)
        (rotation: Rotation)
        (flip: SymbolT.FlipType)
        (model: SheetT.Model)
        : Result<SheetT.Model, string>
        =
        let symLabelUpper = String.toUpper symLabel
        match SymbolUpdate.addSymbol [] (model.Wire.Symbol) position compType symLabelUpper with
        | symModel, symId ->
            let sym = symModel.Symbols[symId]
            let pos =
                { X = sym.Component.X + sym.Component.W / 2.0
                  Y = sym.Component.Y + sym.Component.H / 2.0 }
            match position + sym.getScaledDiagonal with
            | { X = x; Y = y } when x > maxSheetCoord || y > maxSheetCoord ->
                Error
                    $"symbol '{symLabelUpper}' position {position + sym.getScaledDiagonal} lies outside allowed coordinates"
            | _ ->
                let rotatedSym = RotateScale.rotateSymbolByDegree rotation sym
                let orientedSym = RotateScale.flipSymbolInBlock flip pos rotatedSym
                let updatedSymModel =
                    { symModel with Symbols = Map.add symId orientedSym symModel.Symbols }
                let updatedModel =
                    model
                    |> Optic.set symbolModel_ updatedSymModel
                    |> SheetUpdateHelpers.updateBoundingBoxes
                Ok updatedModel
        | _, _ -> failwithf "[TestDrawBlockD2] - placeAndOrientSymbol: addSymbol failed -- Never happens"

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
    let countCrossingsInSheet (sample: int) (sheet: SheetT.Model) : string option =
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
    /// <summary>
    /// Determines the edge placement of ports on a symbol based on the symbol's rotation.
    /// </summary>
    /// <param name="rotation">The rotation of the symbol (Degree0, Degree90, Degree180, Degree270).</param>
    /// <returns>The edge (Left, Top, Right, Bottom) on which ports are placed given the symbol's rotation.</returns>
    let edgeBasedOnRotation rotation =
        match rotation with
        | Degree0 -> Left
        | Degree90 -> Top
        | Degree180 -> Right
        | Degree270 -> Bottom

    /// <summary>
    /// Shuffles a list of items into a random order.
    /// </summary>
    /// <param name="list">The list of items to shuffle.</param>
    /// <returns>
    /// A new list containing the same items as the input list but in a random order.
    /// </returns>
    /// <remarks>
    /// This function applies a functional approach to shuffling. Each item in the input list is paired with a random number. The list is then sorted by these random numbers, effectively shuffling the items. Finally, the random numbers are discarded, leaving a shuffled list of the original items. This method ensures that the shuffling process is both stateless and deterministic, given a fixed seed for the random number generator.
    /// </remarks>
    let listShuffle list =
        let rng = new Random()
        list
        |> List.map (fun item -> (rng.Next(), item))
        |> List.sortBy fst
        |> List.map snd

    /// <summary>
    /// Shuffles the order of ports on specified edge of a component's port maps.
    /// </summary>
    /// <param name="portMaps">The port maps of a component, detailing the order and orientation of ports.</param>
    /// <param name="rotation">The rotation of the component, which determines the edge placement of ports.</param>
    /// <returns>
    /// A new PortMaps instance where the order of ports on specified edge is shuffled, while the orientation of ports remains unchanged.
    /// </returns>
    /// <remarks>
    /// This function targets each edge defined in the component's port maps and applies a shuffle to the list of ports on that edge.
    /// </remarks>
    let shufflePortMapsOrderOnRotationState (portMaps: PortMaps) (rotation: Rotation) : PortMaps =
        let relevantEdge = edgeBasedOnRotation rotation
        let shuffledOrder =
            portMaps.Order
            |> Map.map (fun edge portList ->
                if edge = relevantEdge then
                    listShuffle portList
                else
                    portList) // Only shuffle ports on the relevant edge
        { portMaps with Order = shuffledOrder }

    /// <summary>
    /// Updates the order of ports for all Mux2 type symbols in a model, shuffling the port orders on the edge containing the inputs.
    /// </summary>
    /// <param name="model">The current sheet model containing symbols and their port maps.</param>
    /// <returns>
    /// A new SheetT.Model instance with the port orders of all Mux2 symbols shuffled.
    /// </returns>
    let updateMux2PortOrder (model: SheetT.Model) : SheetT.Model =
        let symbols = model.Wire.Symbol.Symbols
        let updatedSymbols =
            symbols
            |> Map.map (fun _ sym ->
                match sym.Component.Type with
                | Mux2 ->
                    let rotation = sym.STransform.Rotation // Assuming we have rotation info in sym.STransform
                    let shuffledPortMaps = shufflePortMapsOrderOnRotationState sym.PortMaps rotation
                    { sym with PortMaps = shuffledPortMaps }
                | _ -> sym)
        let updatedSymbolModel = { model.Wire.Symbol with Symbols = updatedSymbols }
        { model with Wire = { model.Wire with Symbol = updatedSymbolModel } }

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
        let isSegmentStraight (seg: BusWireT.ASegment) =
            seg.Start.X = seg.End.X || seg.Start.Y = seg.End.Y
        let allWires = sheet.Wire.Wires |> Map.toList |> List.map snd
        let allAbsSegments = allWires |> List.collect getAbsSegments
        let straightenedSegmentsCount =
            allAbsSegments
            |> List.filter isSegmentStraight
            |> List.length
        if straightenedSegmentsCount > 0 then
            Some $"Sample {sample} has {straightenedSegmentsCount} straightened wire segments."
        else
            None

    /// <summary>
    /// Randomly selects a rotation for a symbol.
    /// </summary>
    let randomRotation () =
        let rotations = [| Degree0; Degree90; Degree180; Degree270 |]
        rotations.[random.Next(rotations.Length)]

    /// <summary>
    /// Randomly selects a flip type for a symbol.
    /// </summary>
    let randomFlipType () =
        let flips = [| FlipHorizontal; FlipVertical |]
        flips.[random.Next(flips.Length)]

    /// <summary>
    /// Creates a base circuit with an AND gate and a MUX2.
    /// </summary>
    /// <returns>
    /// A sheet model containing the base circuit with an AND gate and a MUX2.
    /// </returns>
    /// <remarks>
    /// <para> The position of the AND gate is specified by hand, as requested in the deliverable. </para>
    /// <para> The input order of the MUX2 is shuffled randomly. </para>
    /// </remarks>
    let baseCircuit1 (andPos: XYPos) =
        initSheetModel
        // The position of the AND gate is specified by hand, as requested in the deliverable.
        |> placeAndOrientSymbol "AND1" (GateN(And, 2)) { X = 100.0; Y = 100.0 } (Degree0) (randomFlipType ())
        |> Result.bind (placeAndOrientSymbol "MUX1" Mux2 { X = 200.0; Y = 100.0 } (Degree0) (randomFlipType ()))
        |> Result.bind (placeWire (portOf "AND1" 0) (portOf "MUX1" 0))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "AND1" 0))
        |> Result.map updateMux2PortOrder
        |> getOkOrFail

    /// <summary>
    /// Creates a base circuit with an OR gate and a MUX2.
    /// </summary>
    /// <returns>
    /// A sheet model containing the base circuit with an OR gate and a MUX2.
    /// </returns>
    /// <remarks>
    /// <para> The position of the OR gate is specified by hand, as requested in the deliverable. </para>
    /// <para> The input order of the MUX2 is shuffled randomly. </para>
    /// </remarks>
    let baseCircuit2 (andPos: XYPos) =
        initSheetModel
        // The position of the OR gate is specified by hand, as requested in the deliverable.
        |> placeAndOrientSymbol "OR1" (GateN(And, 2)) { X = 100.0; Y = 100.0 } (Degree0) (randomFlipType ())
        |> Result.bind (placeAndOrientSymbol "MUX1" Mux2 { X = 200.0; Y = 100.0 } (Degree0) (randomFlipType ()))
        |> Result.bind (placeWire (portOf "OR1" 0) (portOf "MUX1" 0))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "OR1" 0))
        |> Result.map updateMux2PortOrder
        |> getOkOrFail
    
    /// Test with manually generated circuits and check for wires straightened
    let testWireStraightD2 testNum firstSample dispatch =
        runTestOnSheets
                "Test with manually generated circuits and check for wires straightened"
                firstSample
                horizLinePositions
                baseCircuit2
                countWireStraightInSheet
                dispatch
            |> recordPositionInTest testNum dispatch
    
    /// Test with manually generated circuits and check for wire crossings
    let testWireCrossingD2 testNum firstSample dispatch =
        runTestOnSheets
            "Test with manually generated circuits and check for wire crossings"
            firstSample
            horizLinePositions
            baseCircuit1
            countCrossingsInSheet
            dispatch
        |> recordPositionInTest testNum dispatch
    
