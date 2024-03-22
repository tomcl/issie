module TestDrawBlockD3

open GenerateData
open Elmish

(******************************************************************************************
   This submodule contains a set of functions that enable random data generation
   for property-based testing of Draw Block wire routing functions.
   basic idea.
   1. Generate, in various ways, random circuit layouts
   2. For each layout apply smartautoroute to regenerate all wires
   3. Apply check functions to see if the resulting wire routing obeys "good layout" rules.
   4. Output any layouts with anomalous wire routing
*******************************************************************************************)

open TestDrawBlock
open TestLib
open TestDrawBlock.HLPTick3
open TestDrawBlock.HLPTick3.Asserts
open TestDrawBlock.HLPTick3.Builder
open TestDrawBlock.HLPTick3.Tests

open EEExtensions
open Optics
open Optics.Operators
open BlockHelpers
open Helpers
open CommonTypes
open ModelType
open DrawModelType
open Sheet.SheetInterface
open GenerateData
open SheetBeautifyHelpers
open SheetBeautifyHelpers.SegmentHelpers
open SheetBeautifyD3
open BusWireUpdate
open RotateScale


//------------------------------------------------------------------------------------------------------------------------//
//------------------------------functions to build issue schematics programmatically--------------------------------------//
//------------------------------------------------------------------------------------------------------------------------//

module Builder =
    let segsConnectedToSym (sheet: SheetT.Model) (sym: SymbolT.Symbol) =
        let countVisSegsInWire (wire: BusWireT.Wire) =
            visibleSegments wire.WId sheet |> List.length

        let symPortIds =
            sym.PortMaps.Order
            |> mapValues
            |> Array.toList
            |> List.concat

        sheet.Wire.Wires
        |> Map.filter (fun _ wire ->
            List.contains (string wire.InputPort) symPortIds
            || List.contains (string wire.OutputPort) symPortIds)
        |> Map.toList
        |> List.map (fun (_, wire) -> wire)
        |> List.map countVisSegsInWire
        |> List.sum

    /// Print info needed for reverse circuit generation from sheet
    let printCircuitBuild (sheet: SheetT.Model) = failwithf "Not implemented"

    let rnd = System.Random(42)

    let shuffleA arrayToShuffle : 'a array =
        let tmpA = Array.copy arrayToShuffle
        for i = 0 to tmpA.Length - 1 do
            let r = rnd.Next(i, tmpA.Length)
            (tmpA[i], tmpA[r])
            |> fun (iv, rv: 'a) ->
                tmpA[r] <- iv
                tmpA[i] <- rv
        tmpA
    // Rotate a symbol
    let rotateSymbol (symLabel: string) (rotate: Rotation) (model: SheetT.Model) : (SheetT.Model) =
        let symbolsMap = model.Wire.Symbol.Symbols
        let getSymbol =
            mapValues symbolsMap
            |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
            |> function
                | Some x -> Ok x
                | None -> Error "Can't find symbol with label '{symPort.Label}'"

        match getSymbol with
        | Ok symbol ->
            let rotatedSymbol = SymbolResizeHelpers.rotateSymbol rotate symbol
            let updatedSymbolsMap = Map.add symbol.Id rotatedSymbol symbolsMap
            { model with
                Wire =
                    { model.Wire with
                        Symbol = { model.Wire.Symbol with Symbols = updatedSymbolsMap } } }

        | _ -> model

    // Flip a symbol
    let flipSymbol (symLabel: string) (flip: SymbolT.FlipType option) (model: SheetT.Model) : (SheetT.Model) =
        let symbolsMap = model.Wire.Symbol.Symbols
        let getSymbol =
            mapValues symbolsMap
            |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
            |> function
                | Some x -> Ok x
                | None -> Error "Can't find symbol with label '{symPort.Label}'"

        match flip with
        | None -> model
        | Some f ->
            match getSymbol with
            | Ok symbol ->
                let flippedSymbol = SymbolResizeHelpers.flipSymbol f symbol
                let updatedSymbolsMap = Map.add symbol.Id flippedSymbol symbolsMap
                { model with
                    Wire =
                        { model.Wire with
                            Symbol = { model.Wire.Symbol with Symbols = updatedSymbolsMap } } }

            | _ -> model

    /// Place a symbol with rotation and flip
    let placeSymbol
        (symLabel: string)
        (compType: ComponentType)
        (position: XYPos)
        (rotation: Rotation)
        (flip: SymbolT.FlipType option)
        (model: SheetT.Model)
        : Result<SheetT.Model, string>
        =
        let symLabel = String.toUpper symLabel // make label into its standard casing
        let symModel, symId =
            SymbolUpdate.addSymbol [] (model.Wire.Symbol) position compType symLabel
        let sym = symModel.Symbols[symId]
        match position + sym.getScaledDiagonal with
        | { X = x; Y = y } when x > maxSheetCoord || y > maxSheetCoord ->
            Error $"symbol '{symLabel}' position {position + sym.getScaledDiagonal} lies outside allowed coordinates"
        | _ ->
            model
            |> Optic.set symbolModel_ symModel
            |> SheetUpdateHelpers.updateBoundingBoxes // could optimise this by only updating symId bounding boxes
            |> rotateSymbol symLabel rotation
            |> flipSymbol symLabel flip
            |> Ok

    /// Deselect everything in a model
    let dSelect (model: SheetT.Model) =
        { model with SelectedComponents = []; SelectedWires = [] }

    /// Select everything in a model
    let selectA (model: SheetT.Model) =
        let symbols =
            model.Wire.Symbol.Symbols
            |> Map.toList
            |> List.map fst
        let wires = model.Wire.Wires |> Map.toList |> List.map fst
        { model with SelectedComponents = symbols; SelectedWires = wires }

//--------------------------------------------------------------------------------------------------//
//----------------------------------------Circuit test data generators------------------------------//
//--------------------------------------------------------------------------------------------------//

open Builder

/// small offsets in X&Y axis

let test2Builder =
    let intToRot (x: int) =
        match x with
        | 0 -> Degree90
        | 1 -> Degree90
        | 2 -> Degree180
        | 3 -> Degree270
        | _ -> Degree0
    let ints = GenerateData.map intToRot (randomInt 0 1 3)
    let floats = randomFloat 100 20 200
    List.allPairs (GenerateData.toList floats) (GenerateData.toList ints)
    |> List.toArray
    |> GenerateData.shuffleA
    |> GenerateData.fromArray

let test3Builder = randomFloat 50 20 300

let test1Builder =
    let getRotation x =
        match x with
        | 1 -> Degree0
        | 2 -> Degree90
        | 3 -> Degree180
        | 4 -> Degree270
        | _ -> Degree0

    let getFlip x =
        match x with
        | 1 -> Some SymbolT.FlipHorizontal
        | 2 -> Some SymbolT.FlipVertical
        | _ -> None
    let thing = 0
    let allFlip = List.map getFlip [ 1..3 ]
    let allRot = List.map getRotation [ 1..4 ]
    let combinations lst =
        [ for x in lst do
            for y in lst do
                for z in lst do
                    yield [ x; y; z ] ]
    List.allPairs allRot allFlip
    |> combinations
    |> List.toArray
    |> shuffleA
    |> (fun f -> f[1..25])
    |> fromArray

//--------------------------------------------------------------------------------------------------//
//----------------------------------------Example Test Circuits using Gen<'a> samples---------------//
//--------------------------------------------------------------------------------------------------//


let makeTest1Circuit (ori: list<Rotation * (SymbolT.FlipType option)>) =
    printf "MUX1 rotation: %A" (fst ori[0])
    printf "MUX1 flipType: %A" ((snd ori[0]))
    printf "MUX2 rotation: %A" (fst ori[1])
    printf "MUX2 flipType: %A" ((snd ori[1]))
    printf "MUX3 rotation: %A" (fst ori[2])
    printf "MUX3 flipType: %A" ((snd ori[2]))
    let Mux1Pos = middleOfSheet + { X = 400.; Y = 0. }
    let Mux2Pos = middleOfSheet + { X = 400.; Y = 400. }
    let finalModel =
        initSheetModel
        |> placeSymbol "DM1" Demux4 middleOfSheet (fst ori[0]) (snd ori[0])
        |> Result.bind (placeSymbol "MUX1" Mux4 Mux1Pos (fst ori[1]) (snd ori[1]))
        |> Result.bind (placeWire (portOf "DM1" 0) (portOf "MUX1" 0))
        |> Result.bind (placeWire (portOf "DM1" 1) (portOf "MUX1" 1))
        |> Result.bind (placeWire (portOf "DM1" 2) (portOf "MUX1" 2))
        |> Result.bind (placeWire (portOf "DM1" 3) (portOf "MUX1" 3))
        |> Result.bind (placeSymbol "MUX2" Mux4 Mux2Pos (fst ori[2]) (snd ori[2]))
        |> Result.bind (placeWire (portOf "DM1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "DM1" 1) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "DM1" 2) (portOf "MUX2" 2))
        |> Result.bind (placeWire (portOf "DM1" 3) (portOf "MUX2" 3))
        |> getOkOrFail

    finalModel.Wire.Symbol.Symbols.Values
    |> Seq.cast
    |> List.ofSeq
    |> List.map (fun (x: SymbolT.Symbol) -> printf "%A STransform : %A" x.Component.Label x.STransform)
    |> ignore
    finalModel

let makeTest2Circuit (data: float * Rotation) =
    let rotation = snd data
    let gap = fst data
    printf "Test 2 rotation: %A" rotation
    printf "Test 2 gap: %A" gap
    let Pos1 = middleOfSheet + { X = gap; Y = 0. }
    let Pos2 = Pos1 + { X = gap; Y = 0. }
    let Pos3 = Pos2 + { X = gap; Y = 0. }
    let noWireModel =
        initSheetModel
        |> placeSymbol "C1" (Constant1(Width = 8, ConstValue = 0, DialogTextValue = "0")) middleOfSheet Degree0 None
        |> Result.bind (placeSymbol "SN1" (SplitN(3, [ 2; 3; 3 ], [ 0; 1; 2 ])) Pos1 Degree0 None)
        |> Result.bind (placeSymbol "MN1" (MergeN(3)) Pos2 Degree0 None)
        |> Result.bind (placeSymbol "B" (Output(8)) Pos3 Degree0 None)
        |> getOkOrFail
        |> selectA
    let rotModel =
        match rotation with
        | Degree0 -> noWireModel
        | _ ->
            Optic.set
                symbolModel_
                (rotateBlock noWireModel.SelectedComponents noWireModel.Wire.Symbol rotation)
                noWireModel
    let model =
        rotModel
        |> placeWire (portOf "C1" 0) (portOf "SN1" 0)
        |> Result.bind (placeWire (portOf "SN1" 0) (portOf "MN1" 0))
        |> Result.bind (placeWire (portOf "SN1" 1) (portOf "MN1" 1))
        |> Result.bind (placeWire (portOf "SN1" 2) (portOf "MN1" 2))
        |> Result.bind (placeWire (portOf "MN1" 0) (portOf "B" 0))
        |> getOkOrFail

    { model with Wire = model.Wire |> calculateBusWidths |> fst }
    |> rerouteAllWires
    |> dSelect

let makeTest3Circuit (gap: float) =
    let coeff = gap / 200.
    printf "Test 3 gap: %A" gap
    let Pos1 = middleOfSheet + { X = 0.; Y = 200. * coeff }
    let Pos2 =
        middleOfSheet
        + { X = 200. * coeff; Y = (-60.) * coeff }
    let Pos3 = Pos1 + { X = 200. * coeff; Y = (-60.) * coeff }
    let Pos4 = Pos2 + { X = 200. * coeff; Y = (-50.) * coeff }
    let Pos5 = Pos4 + { X = 0.; Y = 100. * coeff }
    let Pos6 = Pos5 + { X = 0.; Y = 100. * coeff }
    let Pos7 = Pos6 + { X = 0.; Y = 100. * coeff }
    let Pos8 = Pos6 + { X = 150. * coeff; Y = 0. }
    let Pos9 = Pos7 + { X = 250. * coeff; Y = (-14.) * coeff }
    let Pos10 = Pos8 + { X = 80. * coeff; Y = (-100.) * coeff }
    let Pos11 = Pos8 + { X = 180. * coeff; Y = (-35.) * coeff }
    let Pos12 = Pos11 + { X = 180. * coeff; Y = (-100.) * coeff }
    let Pos13 = Pos12 + { X = 180. * coeff; Y = 200. * coeff }

    let noWireModel =
        initSheetModel
        |> placeSymbol "IN1" (Input1(BusWidth = 2, DefaultValue = None)) middleOfSheet Degree0 None
        |> Result.bind (placeSymbol "IN2" (Input1(BusWidth = 2, DefaultValue = None)) Pos1 Degree0 None)
        |> Result.bind (placeSymbol "SW1" (SplitWire(BusWidth = 1)) Pos2 Degree0 None)
        |> Result.bind (placeSymbol "SW2" (SplitWire(BusWidth = 1)) Pos3 Degree0 None)
        |> Result.bind (placeSymbol "G1" (GateN(GateType = And, NumInputs = 2)) Pos4 Degree0 None)
        |> Result.bind (placeSymbol "G2" (GateN(GateType = And, NumInputs = 2)) Pos5 Degree0 None)
        |> Result.bind (placeSymbol "G3" (GateN(GateType = And, NumInputs = 2)) Pos6 Degree0 None)
        |> Result.bind (placeSymbol "G4" (GateN(GateType = And, NumInputs = 2)) Pos7 Degree0 None)
        |> Result.bind (placeSymbol "G5" (GateN(GateType = And, NumInputs = 2)) Pos8 Degree0 None)
        |> Result.bind (placeSymbol "G6" (GateN(GateType = And, NumInputs = 2)) Pos9 Degree0 None)
        |> Result.bind (placeSymbol "G7" (GateN(GateType = Xor, NumInputs = 2)) Pos10 Degree0 None)
        |> Result.bind (placeSymbol "G8" (GateN(GateType = Xor, NumInputs = 2)) Pos11 Degree0 None)
        |> Result.bind (placeSymbol "MN1" (MergeN(NumInputs = 4)) Pos12 Degree0 None)
        |> Result.bind (placeSymbol "OUT" (Output(BusWidth = 4)) Pos13 Degree0 None)
        |> getOkOrFail
    let model =
        noWireModel
        |> placeWire (portOf "IN1" 0) (portOf "SW1" 0)
        |> Result.bind (placeWire (portOf "IN2" 0) (portOf "SW2" 0))
        |> Result.bind (placeWire (portOf "SW1" 0) (portOf "G1" 0))
        |> Result.bind (placeWire (portOf "SW1" 0) (portOf "G2" 0))
        |> Result.bind (placeWire (portOf "SW1" 1) (portOf "G3" 0))
        |> Result.bind (placeWire (portOf "SW1" 1) (portOf "G4" 0))
        |> Result.bind (placeWire (portOf "SW2" 0) (portOf "G2" 1))
        |> Result.bind (placeWire (portOf "SW2" 0) (portOf "G3" 1))
        |> Result.bind (placeWire (portOf "SW2" 1) (portOf "G1" 1))
        |> Result.bind (placeWire (portOf "SW2" 1) (portOf "G4" 1))
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "G5" 0))
        |> Result.bind (placeWire (portOf "G3" 0) (portOf "G5" 1))
        |> Result.bind (placeWire (portOf "G4" 0) (portOf "G6" 1))
        |> Result.bind (placeWire (portOf "G5" 0) (portOf "G6" 0))
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "G7" 0))
        |> Result.bind (placeWire (portOf "G3" 0) (portOf "G7" 1))
        |> Result.bind (placeWire (portOf "G4" 0) (portOf "G8" 0))
        |> Result.bind (placeWire (portOf "G5" 0) (portOf "G8" 1))
        |> Result.bind (placeWire (portOf "G2" 0) (portOf "MN1" 0))
        |> Result.bind (placeWire (portOf "G7" 0) (portOf "MN1" 1))
        |> Result.bind (placeWire (portOf "G8" 0) (portOf "MN1" 2))
        |> Result.bind (placeWire (portOf "G6" 0) (portOf "MN1" 3))
        |> Result.bind (placeWire (portOf "MN1" 0) (portOf "OUT" 0))
        |> getOkOrFail

    { model with Wire = model.Wire |> calculateBusWidths |> fst }

let makeTest4Circuit (data: float) =
    let gap = (data) / 1.
    printf "Test 4 gap: %A" gap
    let Pos1 = middleOfSheet + { X = 100; Y = 150. }
    let noWireModel =
        initSheetModel
        |> placeSymbol "IN" (Constant1(Width = 4, ConstValue = 0, DialogTextValue = "0")) middleOfSheet Degree0 None
        |> Result.bind (placeSymbol "SN1" (SplitN(4, [ 1; 1; 1; 1 ], [ 0; 1; 2; 3 ])) Pos1 Degree0 None)
        |> Result.bind (placeSymbol "MN1" (MergeN(NumInputs = 4)) (Pos1 + { X = 50. + gap; Y = 0. }) Degree0 None)
        |> Result.bind (placeSymbol "OUT" (Output(BusWidth = 4)) (Pos1 + { X = 200. + gap; Y = 0. }) Degree0 None)
        |> Result.bind (
            placeSymbol
                "SEL"
                (BusSelection(OutputWidth = 1, OutputLSBit = 3))
                (middleOfSheet + { X = 100; Y = 0. })
                Degree0
                None
        )
        |> getOkOrFail
    let model =
        noWireModel
        |> placeWire (portOf "IN" 0) (portOf "SN1" 0)
        |> Result.bind (placeWire (portOf "SN1" 1) (portOf "MN1" 1))
        |> Result.bind (placeWire (portOf "SN1" 2) (portOf "MN1" 2))
        |> Result.bind (placeWire (portOf "SN1" 3) (portOf "MN1" 3))
        |> Result.bind (placeWire (portOf "SEL" 0) (portOf "MN1" 0))
        |> Result.bind (placeWire (portOf "IN" 0) (portOf "SEL" 0))
        |> Result.bind (placeWire (portOf "MN1" 0) (portOf "OUT" 0))
        |> getOkOrFail

    { model with Wire = model.Wire |> calculateBusWidths |> fst }
    |> rerouteAllWires
    |> dSelect
//---------------------------------------------------------------------------------------//
//-----------------------------Demo tests on Draw Block code-----------------------------//
//---------------------------------------------------------------------------------------//

module Tests =
    open Asserts
    let D3Test1 testNum firstSample showTargetSheet dispatch =
        runTestOnSheets
            "Mux conected to 2 demux, fail on all"
            firstSample
            test1Builder
            showTargetSheet
            (Some sheetWireLabelSymbol)
            makeTest1Circuit
            (AssertFunc(failOnMetric true))
            Evaluations.nullEvaluator
            dispatch
        |> recordPositionInTest testNum showTargetSheet dispatch

    let D3Test2 testNum firstSample showTargetSheet dispatch =
        runTestOnSheets
            "Mux conected to 2 demux, fail on metric"
            firstSample
            test1Builder
            showTargetSheet
            (Some sheetWireLabelSymbol)
            makeTest1Circuit
            (AssertFunc(failOnMetric false))
            Evaluations.nullEvaluator
            dispatch
        |> recordPositionInTest testNum showTargetSheet dispatch

    let D3Test3 testNum firstSample showTargetSheet dispatch =
        runTestOnSheets
            "Test for label placement, fail on all"
            firstSample
            test2Builder
            showTargetSheet
            (Some sheetWireLabelSymbol)
            makeTest2Circuit
            (AssertFunc(failOnMetric true))
            Evaluations.nullEvaluator
            dispatch
        |> recordPositionInTest testNum showTargetSheet dispatch

    let D3Test4 testNum firstSample showTargetSheet dispatch =
        runTestOnSheets
            "Test for label placement, fail on metric"
            firstSample
            test2Builder
            showTargetSheet
            (Some sheetWireLabelSymbol)
            makeTest2Circuit
            (AssertFunc(failOnMetric false))
            Evaluations.nullEvaluator
            dispatch
        |> recordPositionInTest testNum showTargetSheet dispatch

    let D3Test5 testNum firstSample showTargetSheet dispatch =
        runTestOnSheets
            "General Test on complex circuit"
            firstSample
            test3Builder
            showTargetSheet
            (Some sheetWireLabelSymbol)
            makeTest3Circuit
            (AssertFunc(failOnMetric true))
            Evaluations.nullEvaluator
            dispatch
        |> recordPositionInTest testNum showTargetSheet dispatch

    let D3Test6 testNum firstSample showTargetSheet dispatch =
        runTestOnSheets
            "Test for label placement, fail on all "
            firstSample
            test3Builder
            showTargetSheet
            (Some sheetWireLabelSymbol)
            makeTest4Circuit
            (AssertFunc(failOnMetric true))
            Evaluations.nullEvaluator
            dispatch
        |> recordPositionInTest testNum showTargetSheet dispatch

    let D3Test7 testNum firstSample showTargetSheet dispatch =
        runTestOnSheets
            "Test for label placement, fail on metric "
            firstSample
            test3Builder
            showTargetSheet
            (Some sheetWireLabelSymbol)
            makeTest4Circuit
            (AssertFunc(failOnMetric false))
            Evaluations.nullEvaluator
            dispatch
        |> recordPositionInTest testNum showTargetSheet dispatch

    // ac2021: CAUSED COMPILE ERRORS SO COMMENTED
    // ac2021: I think it was caused by Alina's pr?
    // let D3Test2 testNum firstSample dispatch =
    //     runTestOnSheets
    //         "two custom components with random offset: fail all tests"
    //         firstSample
    //         offsetXY
    //         makeTest2Circuit
    //         Asserts.failOnAllTests
    //         dispatch
    //     |> recordPositionInTest testNum dispatch

    let testsToRunFromSheetMenu: (string * (int -> int -> bool -> Dispatch<Msg> -> Unit)) list =
        // Change names and test functions as required
        // delete unused tests from list
        [ "Test1", D3Test1 // example
          "Test2", D3Test2 // example
          "Test3", D3Test3
          "Test4", D3Test4
          "Test5", D3Test5
          "Test6", D3Test6
          "Test7", D3Test7
          "Toggle Beautify", (fun _ _ _ _ -> printf "Beautify Toggled")
          "Next Test Error",
          fun _ _ _ _ -> printf "Next Error:" ] // Go to the nexterror in a test

    /// Display the next error in a previously started test
    let nextError (testName, testFunc) firstSampleToTest showTargetSheet dispatch =
        let testNum =
            testsToRunFromSheetMenu
            |> List.tryFindIndex (fun (name, _) -> name = testName)
            |> Option.defaultValue 0
        testFunc testNum firstSampleToTest showTargetSheet dispatch

    /// common function to execute any test.
    /// testIndex: index of test in testsToRunFromSheetMenu
    let testMenuFunc (testIndex: int) (dispatch: Dispatch<Msg>) (model: Model) =
        let name, func = testsToRunFromSheetMenu[testIndex]
        printf "%s" name
        match name, model.DrawBlockTestState with
        | "Next Test Error", Some state ->
            nextError
                testsToRunFromSheetMenu[state.LastTestNumber]
                (state.LastTestSampleIndex + 1)
                (state.TargetFunctionApplied)
                dispatch
        | "Next Test Error", None ->
            printf "Test Finished"
            ()
        | "Toggle Beautify", Some state ->
            nextError
                testsToRunFromSheetMenu[state.LastTestNumber]
                (state.LastTestSampleIndex)
                (not state.TargetFunctionApplied)
                dispatch
        | "Toggle Beautify", None ->
            printf "No test started"
            ()
        | _ -> func testIndex 0 true dispatch
