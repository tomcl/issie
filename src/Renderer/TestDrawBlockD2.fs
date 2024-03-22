module TestDrawBlockD2
open GenerateData
open Elmish

(******************************************************************************************
   This submodule contains a set of functions that enables testing of sheet beautify D2.
   basic idea.
   1. Generate manual test circuit to test Muxes, Gates, Custom components flipping, rotating and swapping inputs
   2. Generate random sample tests for the number of wire crossings
   3. Quality measures: Wire crossing, wire squashing, wire bends
*******************************************************************************************)

open SymbolResizeHelpers
open EEExtensions
open Optics
open Optics.Operators
open DrawHelpers
open Helpers
open CommonTypes
open ModelType
open DrawModelType
open Sheet.SheetInterface
open GenerateData
open SheetBeautifyHelpers
open SheetBeautifyD2
open System
open SheetBeautifyHelpers.SegmentHelpers
open BlockHelpers

open TestDrawBlock
open TestDrawBlockD1
open TestDrawBlockD1.Circuit
open TestLib
open TestDrawBlock.HLPTick3
open TestDrawBlock.HLPTick3.Asserts
open TestDrawBlock.HLPTick3.Builder
open TestDrawBlock.HLPTick3.Tests


//------------------------------------------------------------------------------------------------------------------------//
//------------------------------functions to build issue schematics programmatically--------------------------------------//
//------------------------------------------------------------------------------------------------------------------------//
module Builder =  
        type operationChoice = 
            | RotateSymbol90
            | RotateSymbol270
            | FlipSymbol
            | SwapMuxInputs

        // Custom Components for testing
        let testingCC: ComponentType =
            Custom {
                        Name = "TestingCC";
                        InputLabels = [("A", 1); ("B", 1); ("C", 1)];
                        OutputLabels = [("E", 1); ("F", 1); ("G", 1)];
                        Form = Some ProtectedTopLevel
                        Description = None
                }
        let aludeCC: ComponentType =
            Custom {
                        Name = "AludeCC";
                        InputLabels = [("A", 1); ("B", 1)];
                        OutputLabels = [("E", 1); ("F", 1)];
                        Form = Some ProtectedTopLevel
                        Description = None
                }

        // Rotate a symbol
        let rotateSymbol (symLabel: string) (rotate: Rotation) (model: SheetT.Model) : (SheetT.Model) =
            let symbolsMap = model.Wire.Symbol.Symbols
            let getSymbol = 
                mapValues symbolsMap
                |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
                |> function | Some x -> Ok x | None -> Error "Can't find symbol with label '{symPort.Label}'"

            match getSymbol with
            | Ok symbol ->
                let rotatedSymbol = SymbolResizeHelpers.rotateSymbol rotate symbol
                let updatedSymbolsMap = Map.add symbol.Id rotatedSymbol symbolsMap
                { model with Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbolsMap } } }

            | _ -> model

        // Flip a symbol
        let flipSymbol (symLabel: string) (flip: SymbolT.FlipType) (model: SheetT.Model) : (SheetT.Model) =
            let symbolsMap = model.Wire.Symbol.Symbols
            let getSymbol =
                mapValues symbolsMap
                |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
                |> function | Some x -> Ok x | None -> Error "Can't find symbol with label '{symPort.Label}'"

            match getSymbol with
            | Ok symbol ->
                let flippedSymbol = SymbolResizeHelpers.flipSymbol flip symbol
                let updatedSymbolsMap = Map.add symbol.Id flippedSymbol symbolsMap
                { model with Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbolsMap } } }

            | _ -> model
        
        // swap MUX input, return flipped MUX
        let swapMuxInputOrder (mux: SymbolT.Symbol)=
            let edge: Edge option = 
                match mux.Component.Type with
                |Mux2| Mux4| Mux8 -> Some Left
                |Demux2|Demux4|Demux8 -> Some Right
                | _ -> None

            match edge with
            |Some side -> 
                let portOrder = getPortOrder side mux
                let reversePortOrder = List.rev portOrder
                putPortOrder side reversePortOrder mux
            |None -> mux
        
        let swapMuxInputs (symLabel: string) (model: SheetT.Model) : (SheetT.Model) =
            let symbolsMap = model.Wire.Symbol.Symbols
            let getSymbol =
                mapValues symbolsMap
                |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
                |> function | Some x -> Ok x | None -> Error "Can't find symbol with label '{symPort.Label}'"

            match getSymbol with
            | Ok symbol ->
                let swappedSymbol = swapMuxInputOrder symbol
                let updatedSymbolsMap = Map.add symbol.Id swappedSymbol symbolsMap
                { model with Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbolsMap } } }

            | _ -> model

        // Apply rotate or flip operation to a symbol
        let applyOperation (symLabel: String) (choice: operationChoice)=
            match choice with
            | RotateSymbol90 -> rotateSymbol symLabel (Degree90)
            | RotateSymbol270 -> rotateSymbol symLabel (Degree270)
            | FlipSymbol -> flipSymbol symLabel SymbolT.FlipType.FlipVertical
            | SwapMuxInputs -> swapMuxInputs symLabel

    

//--------------------------------------------------------------------------------------------------//
//----------------------------------------Example Test Circuits using Gen<'a> samples---------------//
//--------------------------------------------------------------------------------------------------//

module Circuit = 
    open Builder
        /// Sample data based on 11 equidistant points on a horizontal line

    // randomly generate a test circuit with 4 components and 2 inputs, tests can be repeated
    let makeRandomTestCircuitTest (ranEle: ComponentType list * XYPos list* operationChoice list) = 
        let oprationList = 
            match ranEle with
            | (_, _, third) -> third
        let compList = 
            match ranEle with
            | (first, _, _) -> first
        let posList = 
            match ranEle with
            | (_, second, _) -> second

        initSheetModel
        |> placeSymbol "C2" compList.[1] {X= middleOfSheet.X + 300.; Y= middleOfSheet.Y}
        |> addSym "C1" compList.[0] posList.[0].X posList.[0].Y
        |> addSym "C3" compList.[2] posList.[2].X posList.[2].Y
        |> addSym "C4" Mux2 posList.[1].X posList.[1].Y
        |> addSym "S1" (Input1(1,None)) (-400.) (-25.)
        |> addSym "S2" (Input1(1,None)) (-400.) (100.)
        |> getOkOrFail
        |> applyOperation "C1" oprationList.[0]
        |> applyOperation "C2" oprationList.[1]
        |> applyOperation "C3" oprationList.[2]
        |> placeWire (portOf "C1" 0) (portOf "C4" 0)
        |> addWire ("C2", 0) ("C3", 0)
        |> addWire ("C3", 0) ("C4", 1)
        |> addWire ("S1", 0) ("C2", 0)
        |> addWire ("S2", 0) ("C3", 1)
        |> getOkOrFail

    /// manually generated test circuits where gates and MUXes are randomly flipped, or (2-MUX) inputs swapped.
    /// random sample test circuit that has 3 MUX2 gates and 2 inputs. Two gates are flipped to create two crossing wires.
    let makeTestCircuit1(andPos:XYPos) = 
            initSheetModel
            |> placeSymbol "1MUX2" Mux2 andPos
            |> addSym "2MUX2" Mux2 0 0
            |> addSym "3MUX2" Mux2 100. 0
            |> addSym "S1" (Input1(1,None)) -100. 0
            |> addSym "S2" (Input1(1,None)) -100. -100.
            |> getOkOrFail
            |> placeWire (portOf "1MUX2" 0) (portOf "2MUX2" 1)
            |> addWire ("2MUX2", 0) ("3MUX2", 1)
            |> addWire ("S1", 0) ("1MUX2", 0)
            |> addWire ("S2", 0) ("2MUX2", 0)
            |> getOkOrFail

    // manually generated test circuit that to test Mux2
    let makeTestCircuit2 (andPos:XYPos) = 
            initSheetModel
            |> placeSymbol "S2" (Input1(1,None)) {X = (andPos.X - 100.); Y = andPos.Y}
            |> addSym "G2" (GateN(And,2)) 0 0
            |> addSym "G1" (GateN(And,2)) 0 -100.
            |> addSym "MUX1" Mux2 200. -100.
            |> addSym "MUX2" Mux2 200. 50.
            |> addSym "S1" (Input1(1,None)) -100. -150.
            |> getOkOrFail
            //|> flipSymbol "MUX1" SymbolT.FlipType.FlipVertical
            |> placeWire (portOf "S2" 0) (portOf "G1" 0)
            |> addWire ("G1", 0) ("MUX1", 1)
            |> addWire ("MUX1", 0) ("G1", 1)
            |> addWire ("G2", 0) ("MUX1", 0)
            |> addWire ("G2", 0) ("MUX2", 1)
            |> addWire ("S1", 0) ("MUX1", 2)
            |> getOkOrFail

    // manually generated test circuit that to test Mux2 and And gate
    let makeTestCircuit3 (andPos:XYPos) = 
            initSheetModel
            |> placeSymbol "S1" (Input1(1,None)) {X=middleOfSheet.X - 150.; Y=andPos.Y}
            |> addSym "MUX2" Mux2 0 0
            |> addSym "S2" (Input1(1,None)) -150. 55.
            |> addSym "MUX1" Mux2 -100. -150.
            |> addSym "G1" (GateN(And,2)) 100. -100.
            |> getOkOrFail
            |> flipSymbol "MUX2" SymbolT.FlipType.FlipVertical
            |> placeWire (portOf "S1" 0) (portOf "MUX2" 1)
            |> addWire ("S2", 0) ("MUX2", 2)
            |> addWire ("MUX1", 0) ("MUX2", 0)
            |> addWire ("MUX1", 0) ("G1", 1)
            |> addWire ("MUX2", 0) ("G1", 0)
            |> getOkOrFail

    // manually generated test circuit that to test Custom Component
    let makeTestCCCircuit (andXY: XYPos) = 
        initSheetModel
        |> placeSymbol "AludeCode" aludeCC andXY
        |> addSym "S1" (Input1(1,None)) -200. -50.
        |> addSym "S2" (Input1(1,None)) -200. 50.
        |> addSym "O1" (Output(1)) 200. 200. 
        |> addSym "MUX4" Mux4 300. 0.
        |> addWire ("S1", 0) ("AludeCode", 1)
        |> addWire ("S2", 0) ("AludeCode", 0)
        |> addWire ("AludeCode", 0) ("O1", 0)
        |> addWire ("AludeCode", 1) ("MUX4", 3)
        |> getOkOrFail

    // manually generated test circuit that to test all functions
    let makeTestAllCircuit (andXY: XYPos) = 
        initSheetModel
        |> placeSymbol "CC" testingCC andXY
        |> addSym "S1" (Input1(1,None)) -400. -50.
        |> addSym "S2" (Input1(1,None)) -400. 50.
        |> addSym "MUX21" Mux2 200. -50. 
        |> addSym "MUX22" Mux2 -200. -50.
        |> addSym "XOr" (GateN(Xor,2)) 400. 100.
        |> addWire ("S1", 0) ("MUX22", 1)
        |> addWire ("S2", 0) ("CC", 0)
        |> addWire ("MUX22", 0) ("CC", 2)
        |> addWire ("CC", 1) ("XOr", 0)
        |> addWire ("CC", 2) ("MUX21", 1)
        |> addWire ("MUX21", 0) ("XOr", 1)
        |> getOkOrFail



//------------------------------------------------------------------------------------------------//
//-------------------------Example assertions used to test sheets---------------------------------//
//------------------------------------------------------------------------------------------------//

module Asserts =
        let indevTestWiresCrossing (sample: int) (sheet: SheetT.Model) = 
            let crossingsBeforeD2 = 
                sheet
                |> numSegmentCrossRightAngle
                // |> function | 0 -> Some $"Sample {sample} has no right angle wire crossings"
                //             | n -> Some $"Sample {sample} has {n} right angle wire crossings"
            let crossingsAfterD2 = 
                sheet
                |> sheetOrderFlip
                |> numSegmentCrossRightAngle
            Some $"Sample {sample}: {crossingsBeforeD2 - crossingsAfterD2} crossings is reduced, {crossingsAfterD2} crossings after beautify"


//---------------------------------------------------------------------------------------//
//-----------------------------Evaluation------------------------------------------------//
//---------------------------------------------------------------------------------------//
// Evaluation of the circuit will be calulated after the sheetChecker is run successfully. 
// Each evaluation metric returns score between [0, 1].
// The larger the score, the more 'beautiful' the beautified sheet is 
// relative to ideal beautification.

module Evaluations =
    open TestDrawBlock.HLPTick3.Evaluations

    /// Weights to choose metrics' imprortance.
    /// D1-specific metrics only
    type ConfigD2 =
        {
            wBendWeight: float
            wCrossWeight: float
            wireSquashWeight: float
        }

    /// combined evaluation function for D1
    let evaluateD2 (c: ConfigD2) =
        combEval3 
            wireBendProp c.wBendWeight 
            (float << numOfWireRightAngleCrossings) c.wCrossWeight
            wireSquashProp c.wireSquashWeight
    
    let d2Conf = 
        {
            wBendWeight = 0.75
            wCrossWeight = 1
            wireSquashWeight = 0.5
        }

    let d2Evaluator = 
        {
            EvalFunc = evaluateD2 d2Conf
            Penalty = -1
        }


//---------------------------------------------------------------------------------------//
//-----------------------------TestData--------------------------------------------------//
//---------------------------------------------------------------------------------------//

module TestData = 
    open Builder
    let horizLinePositions =
            fromList [-100..20..100]
            |> map (fun n -> middleOfSheet + {X=float n; Y=(-50.)})

    // used for manual test circuits
    let offset = 
        Array.init 1 (fun x y -> {X=x; Y=y})
        |> Array.map (fun pos -> {X=middleOfSheet.X; Y=middleOfSheet.Y})
        |> fromArray

    let offsetPair: Gen<XYPos>=
        [{X = middleOfSheet.X - 50.; Y = middleOfSheet.Y}; {X = middleOfSheet.X; Y = middleOfSheet.Y}]
        |> fromList

    /// list of all the beautifiable components, custom components and other muxes to be added later
    let d2ComponentTypes: ComponentType list = 
            [Mux2; GateN(And,2); GateN(Or,2); GateN(Xor,3); GateN(Nand,4); GateN(Nor,3); GateN(Xnor,3); Mux2; Mux2; testingCC; aludeCC]

    /// random XYPos data within a grid
    let randomPos: XYPos list list = 
            let range = [-250..100..250] |> List.map float
            let x1Array = range |> Array.ofList |> shuffleA
            let y1Array = range |> Array.ofList |> shuffleA
            let x2Array = range |> Array.ofList |> shuffleA
            let y2Array = range |> Array.ofList |> shuffleA
            let x3Array = range |> Array.ofList |> shuffleA
            let y3Array = range |> Array.ofList |> shuffleA
            let xyPos1 = Array.map2 (fun x y -> {X=float x; Y=float y}) x1Array y1Array
            let xyPos2 = Array.map2 (fun x y -> {X=float x; Y=float y}) x2Array y2Array
            let xyPos3 = Array.map2 (fun x y -> {X=float x; Y=float y}) x3Array y3Array
            //ensure 3 points are unique, avoid duplicate positions, thus avoid overlapping symbols
            let dummyPos = {X=0.; Y=0.}
            let gen12Array = 
                Array.map2 (fun (p1:XYPos) (p2:XYPos) -> 
                    if (p1.X = p2.X) && (p1.Y = p2.Y) then [dummyPos; dummyPos] else [p1 ; p2]
                ) xyPos1 xyPos2 
                //|> Array.choose id
            let gen123Array = 
                Array.map2 (fun (p1:XYPos list) (p2:XYPos) -> 
                if ((p1[0].X = p2.X) && (p1[0].Y = p2.Y)) || ((p1[1].X = p2.X) && (p1[1].Y = p2.Y)) 
                    then (p1 @ [dummyPos]) else (p1 @ [p2])
                ) gen12Array xyPos3
                |> Array.filter (fun pos -> not (pos |> List.exists (fun p -> p.X = 0. && p.Y = 0.)))
                |> Array.toList
            gen123Array

    // Generate random test elements
    // for each list in randomPos, get a list of component type from d2ComponentTypes, in order, store in an array
    // use shuffleA to shuffle the array
    // for each component type, get a list of random operation choice using getRandCompType
    // store the component type, position and operation choice in a tuple list
    let testRandElements: Gen<ComponentType list * XYPos list* operationChoice list>= 
        let compList =
            randomPos
            |> List.mapi (fun index1 posList -> 
                    posList
                    |> List.mapi (fun index2 _ ->
                        let comIndex = ((index2 + 1)* (index1+1)) % d2ComponentTypes.Length
                        List.item comIndex d2ComponentTypes
                    )
                    |> List.toArray
                    |> shuffleA
                    |> Array.toList
            )
        let opList =
            compList
            |> List.mapi (fun index1 comList -> 
                let possibleOperation = [RotateSymbol90; RotateSymbol270; FlipSymbol; SwapMuxInputs]
                comList
                |> List.mapi (fun index2 _ -> 
                    let opIndex = ((index2+1) * (index1+1)) % 4
                    List.item opIndex possibleOperation
                )
                |> List.toArray
                |> shuffleA
                |> Array.toList
            )
        // combine the component type, position and operation choice into a tuple list
        randomPos
        |> List.mapi (fun index pos -> 
            (compList.[index], pos, opList.[index])
        )
        |> fromList
    
    // target function for D2
    // Squashed wire = too small separation between wire segment and symbol edges
    let targetSheetD2 (sheet: SheetT.Model) = 
        let crossings = 
            sheet
            |> numOfWireRightAngleCrossings
        let beautifiedSheet = sheetOrderFlip sheet
        let crossingsAfter = 
            beautifiedSheet
            |> numSegmentCrossRightAngle//numOfWireRightAngleCrossings
        let wireBendBefore = 
            sheet
            |> numVisibleWireRightAngle
        let wireBendAfter =     
            beautifiedSheet
            |> numVisibleWireRightAngle
        let wireSquashBefore = 
            sheet
            |> numOfSquashedWires
        let wireSquashAfter = 
            beautifiedSheet
            |> numOfSquashedWires
        printfn "Crossings before: %d , Crossings after: %d , Crossings reduced: %d" crossings crossingsAfter (crossings - crossingsAfter)
        printfn "Squashed before: %d , Squashed after: %d, Squashed reduced: %d " wireSquashBefore wireSquashAfter (wireSquashBefore - wireSquashAfter)
        printfn "Bends before: %d , Bends after: %d , Bends reduced: %d" wireBendBefore wireBendAfter (wireBendBefore - wireBendAfter)
        beautifiedSheet

//---------------------------------------------------------------------------------------//
//-----------------------------Demo tests on Draw Block code-----------------------------//
//---------------------------------------------------------------------------------------//

module Tests =
        open Asserts
        open Evaluations
        open Circuit
        open TestData

        let test1 testNum firstSample showTargetSheet dispatch =
            runTestOnSheets
                "D2 Test Circuit 1: testing"
                firstSample
                horizLinePositions
                showTargetSheet
                None
                makeTestCircuit1
                (AssertFunc failOnAllTests)
                Evaluations.nullEvaluator
                dispatch
            |> recordPositionInTest testNum showTargetSheet dispatch
        
        let test2 testNum firstSample showTargetSheet dispatch =
            runTestOnSheets
                "D2 Test Circuit 2: Annie's test"
                firstSample
                offset
                showTargetSheet
                (Some targetSheetD2)
                makeTestCircuit2
                (AssertFunc failOnAllTests)
                Evaluations.d2Evaluator
                dispatch
            |> recordPositionInTest testNum showTargetSheet dispatch
        
        let test3 testNum firstSample showTargetSheet dispatch =
            runTestOnSheets
                "D2 Test Circuit 3, Mux2 and And gate"
                firstSample
                offsetPair
                showTargetSheet
                (Some targetSheetD2)
                makeTestCircuit3
                TargetFuncWorse
                { 
                    EvalFunc = wireCrossProp
                    Penalty = -0.5
                }
                dispatch
            |> recordPositionInTest testNum showTargetSheet dispatch
        
        let testRandomD2Eval testNum firstSample showTargetSheet dispatch =
            runTestOnSheets
                "D2 Test Random Circuit Eval"
                firstSample
                testRandElements
                showTargetSheet
                (Some targetSheetD2)
                makeRandomTestCircuitTest
                TargetFuncWorse
                { 
                    EvalFunc = wireCrossProp
                    Penalty = -0.5
                }
                dispatch
            |> recordPositionInTest testNum showTargetSheet dispatch

        let testRandomD2 testNum firstSample showTargetSheet dispatch =
            runTestOnSheets
                "D2 Test Random Circuit"
                firstSample
                testRandElements
                showTargetSheet
                (Some targetSheetD2)
                makeRandomTestCircuitTest
                (AssertFunc failOnAllTests)
                Evaluations.nullEvaluator
                dispatch
            |> recordPositionInTest testNum showTargetSheet dispatch

        let testCC testNum firstSample showTargetSheet dispatch =
            runTestOnSheets
                "D2 Test Custom Component"
                firstSample
                offset
                showTargetSheet
                (Some targetSheetD2)
                makeTestCCCircuit
                (AssertFunc failOnAllTests)
                Evaluations.nullEvaluator
                dispatch
            |> recordPositionInTest testNum showTargetSheet dispatch
        
        let testAll testNum firstSample showTargetSheet dispatch =
            runTestOnSheets
                "D2 Test All D2 functions"
                firstSample
                offset
                showTargetSheet
                (Some targetSheetD2)
                makeTestAllCircuit
                (AssertFunc failOnAllTests)
                Evaluations.nullEvaluator
                dispatch
            |> recordPositionInTest testNum showTargetSheet dispatch
        
        /// List of tests available which can be run ftom Issie File Menu.
        /// The first 9 tests can also be run via Ctrl-n accelerator keys as shown on menu
        let testsToRunFromSheetMenu : (string * (int -> int -> bool -> Dispatch<Msg> -> Unit)) list =
            // Change names and test functions as required
            // delete unused tests from list
            [
                "dev testing", test1                    // test1
                "Mux2", test2                           // test2
                "Mux2 & And", test3                     // test3
                "Random Eval Score", testRandomD2Eval   // test4
                "Random Test", testRandomD2             // test5
                "Custom Component", testCC              // test6
                "Test All Funcs", testAll               // test7
                "Toggle Beautify", fun _ _ _ _ -> printf "Beautify Toggled"
                "Next Test Error", fun _ _ _ _ -> printf "Next Error:" // Go to the nexterror in a test
            ]

        /// Display the next error in a previously started test
        let nextError (testName, testFunc) firstSampleToTest showTargetSheet dispatch =
            let testNum =
                testsToRunFromSheetMenu
                |> List.tryFindIndex (fun (name,_) -> name = testName)
                |> Option.defaultValue 0
            testFunc testNum firstSampleToTest showTargetSheet dispatch

        /// common function to execute any test.
        /// testIndex: index of test in testsToRunFromSheetMenu
        let testMenuFunc (testIndex: int) (dispatch: Dispatch<Msg>) (model: Model) =
            let name,func = testsToRunFromSheetMenu[testIndex] 
            printf "%s" name
            match name, model.DrawBlockTestState with
            | "Next Test Error", Some state ->
                nextError testsToRunFromSheetMenu[state.LastTestNumber] (state.LastTestSampleIndex+1) (state.TargetFunctionApplied) dispatch
            | "Next Test Error", None ->
                printf "Test Finished"
                ()
            | "Toggle Beautify", Some state -> 
                nextError testsToRunFromSheetMenu[state.LastTestNumber] (state.LastTestSampleIndex) (not state.TargetFunctionApplied) dispatch
            | "Toggle Beautify", None ->
                printf "No test started"
                ()
            | _ ->
                func testIndex 0 true dispatch
    


            


