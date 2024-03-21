// D2 Testing
module TestDrawBlockD2
open GenerateData
open Elmish

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

// Test D2 using random sample inputs
// Reduction in wire crossings, other quality measures

// TODO: 
// 1. Generate random sample tests for the number of wire crossings Done
// 2. Test the input before D2 and after D2 is applied Done
// 3. Output the difference in the number of wire crossings Done
// 4. Other quality measures: check if wire intersecting symbols, etc.
// 5. Write evaluation functions for the wire crossing
// 6. Make custom components and test circuit with them
// 7. Create more manual test circuits: different Muxs, Custom components, gates.
 
(******************************************************************************************
   This submodule contains a set of functions that enable random data generation
   for property-based testing of Draw Block wire routing functions.
   basic idea.
   1. Generate, in various ways, random circuit layouts
   2. For each layout apply smartautoroute to regenerate all wires
   3. Apply check functions to see if the resulting wire routing obeys "good layout" rules.
   4. Output any layouts with anomalous wire routing
*******************************************************************************************)

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
                        InputLabels = [("A", 1); ("B", 1)];
                        OutputLabels = [("E", 1); ("F", 1)];
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
        
        let applyOperation (symLabel: String) (choice: operationChoice)=
            match choice with
            | RotateSymbol90 -> rotateSymbol symLabel (Degree90)
            | RotateSymbol270 -> rotateSymbol symLabel (Degree270)
            | FlipSymbol -> flipSymbol symLabel SymbolT.FlipType.FlipVertical
            | SwapMuxInputs -> swapMuxInputs symLabel

    

//--------------------------------------------------------------------------------------------------//
//----------------------------------------Example Test Circuits using Gen<'a> samples---------------//
//--------------------------------------------------------------------------------------------------//

open Builder
    /// Sample data based on 11 equidistant points on a horizontal line
let horizLinePositions =
        fromList [-100..20..100]
        |> map (fun n -> middleOfSheet + {X=float n; Y=(-50.)})
    
let offset = 
    Array.init 1 (fun x y -> {X=x; Y=y})
    |> Array.map (fun pos -> {X=middleOfSheet.X; Y=middleOfSheet.Y})
    |> fromArray
    // let positionsOverlap (sheet: SheetT.Model) :bool = 
    //     // let sheet = makeTest1Circuit andPos
    //     // let wireModel = sheet.Wire
    //     let boxes =
    //         mapValues sheet.BoundingBoxes
    //         |> Array.toList
    //         |> List.mapi (fun n box -> n,box)
    //     List.allPairs boxes boxes 
    //     |> List.exists (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
    //     |> (function | true -> true
    //                  | false -> false)
    // let generateGridAroundMiddle : Gen<XYPos> =
    //     let sampleRange = [-150..40..150] |> List.map float
    //     // Generate the Cartesian product of sampleRange for both X and Y coordinates
    //     let gridGenerator = 
    //         product (fun x y -> { X = middleOfSheet.X + x; Y = middleOfSheet.Y + y })
    //                 (fromList sampleRange)
    //                 (fromList sampleRange)
    //     filter (fun pos -> not (positionsOverlap pos))
    //        gridGenerator

    // type testComponent = {
    //     label: string
    //     compType: ComponentType
    //     position: XYPos   
    // }



/// random XYPos sample data within a grid
let randomPos: Gen<XYPos List> = 
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
        // let gen123Array = 
        //     Array.map2 (fun (p1:XYPos list) (p2:XYPos) -> 
        //     if ((p1[0].X = p2.X) && (p1[0].Y = p2.Y)) || ((p1[1].X = p2.X) && (p1[1].Y = p2.Y)) 
        //         then (p1 @ [dummyPos]) else (p1 @ [p2])
        //     ) gen12Array xyPos3
            |> Array.filter (fun pos -> not (pos |> List.exists (fun p -> p.X = 0. && p.Y = 0.)))
            //|> Array.choose id
        fromArray gen12Array

// let testRandElements = 
//     getTestRand 3

// Generate a seed from current time
// Seed generated & displayed to allow repeatable random testing
//let seed = int (System.DateTime.Now.Ticks % int64 System.Int32.MaxValue)
let randomD2 = 
    //printfn "Random seed: %d" seed
    System.Random(seed)

/// list of all the beautifiable components, custom components and other muxes to be added later
let d2ComponentTypes: ComponentType list = 
        [Mux2; GateN(And,2); GateN(Or,2); GateN(Xor,3); GateN(Nand,4); GateN(Nor,3); GateN(Xnor,3); Mux2; Mux2]
let getRandCompType((rand: Random))(possibleTypes: ComponentType list) = 
        let randomIndex = rand.Next (List.length possibleTypes)
        List.item randomIndex possibleTypes

// let randomCompTypeArray = 
//         Array.init 3 (fun _ -> getRandCompType d2ComponentTypes)
    
let applyRandomOperation (rand: Random) (compType: ComponentType) (label: String) = 
        let possibleOperation = [RotateSymbol90; RotateSymbol270; FlipSymbol; SwapMuxInputs]
        match compType with
        | Mux2 ->
            let randomIndex = rand.Next (4)
            List.item randomIndex possibleOperation
                |> applyOperation label
        | _ -> 
            let randomIndex = rand.Next (3)
            List.item randomIndex possibleOperation
                |> applyOperation label

// let randomOperationArray (label: String) = 
//         randomCompTypeArray
//         |> Array.map (fun compType -> 
//             let possibleOperation = [RotateSymbol90; RotateSymbol270; FlipSymbol; SwapMuxInputs]
//             match compType with
//             | Mux2 ->
//                 let randomIndex = randomD2.Next (4)
//                 List.item randomIndex possibleOperation
//                     |> applyOperation label
//             | _ -> 
//                 let randomIndex = randomD2.Next (3)
//                 List.item randomIndex possibleOperation
//                     |> applyOperation label
//         )

let getRandPort (rand: Random) (symType: ComponentType) = 
        match symType with
        | Mux2 -> 
            rand.Next (3)
        | GateN(_,n) -> 
            rand.Next (n)
        | _ -> 
            0

let makeRandomTestCircuit (posList: XYPos List) = 
    let rand = Random(seed)

    let comp1 = getRandCompType rand d2ComponentTypes
    let comp2 = getRandCompType rand d2ComponentTypes
    let comp3 = getRandCompType rand d2ComponentTypes

    initSheetModel
    |> placeSymbol "C2" comp2 {X= middleOfSheet.X + 300.; Y= middleOfSheet.Y}
    |> addSym "C1" comp1 posList.[0].X posList.[0].Y
    |> addSym "C3" comp3 posList.[1].X posList.[1].Y
    |> addSym "S1" (Input1(1,None)) (-350.) (-25.)
    |> addSym "S2" (Input1(1,None)) (-350.) (-100.)
    |> getOkOrFail
    |> applyRandomOperation rand comp1 "C1"
    |> applyRandomOperation rand comp2 "C2"
    |> applyRandomOperation rand comp3 "C3"
    |> placeWire (portOf "C1" 0) (portOf "C2" (getRandPort rand comp2))
    |> addWire ("C2", 0) ("C3", 0)
    |> addWire ("C3", 0) ("C1", (getRandPort rand comp1))
    |> addWire ("S1", 0) ("C2", 1)
    |> addWire ("S2", 0) ("C3", 1)
    |> getOkOrFail


// /// random sample test circuit that has 2 inputs
// let makeRandomTestCircuit (posList:XYPos List) = 
//         let comp1 = getRandCompType d2ComponentTypes
//         let comp2 = getRandCompType d2ComponentTypes
//         let comp3 = getRandCompType d2ComponentTypes
//         // let comp4 = getRandCompType d2ComponentTypes
//         //let compList = [comp1; comp2; comp3; comp4]
//         initSheetModel
//             |> placeSymbol "C2" comp2 {X= middleOfSheet.X + 300.; Y= middleOfSheet.Y}
//             |> addSym "C1" comp1 posList.[0].X posList.[0].Y
//             |> addSym "C3" comp3 posList.[1].X posList.[1].Y
//             //|> addSym "C4" comp4 posList.[2].X posList.[2].Y
//             |> addSym "S1" (Input1(1,None)) (-350.) (-25.)
//             |> addSym "S2" (Input1(1,None)) (-350.) (-100.)
//             |> getOkOrFail
//             |> applyRandomOperation comp1 "C1"
//             |> applyRandomOperation comp2 "C2"
//             |> applyRandomOperation comp3 "C3"
//             |> placeWire (portOf "C1" 0) (portOf "C2" (getRandPort comp2))
//             |> addWire ("C2", 0) ("C3", 0)
//             //|> addWire ("C3", 0) ("C4", 0)
//             |> addWire ("C3", 0) ("C1", (getRandPort comp1))
//             |> addWire ("S1", 0) ("C2", 1)
//             |> addWire ("S2", 0) ("C3", 1)
//             |> getOkOrFail

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

let makeTestCircuit2 (andPos:XYPos) = 
        initSheetModel
        |> placeSymbol "S2" (Input1(1,None)) andPos
        |> addSym "G2" (GateN(And,2)) 0 0
        |> addSym "G1" (GateN(And,2)) 0 -100.
        |> addSym "MUX1" Mux2 200. -100.
        |> addSym "MUX2" Mux2 200. 50.
        |> addSym "S1" (Input1(1,None)) -100. -150.
        |> getOkOrFail
        |> flipSymbol "MUX1" SymbolT.FlipType.FlipVertical
        |> placeWire (portOf "S2" 0) (portOf "G1" 0)
        |> addWire ("G1", 0) ("MUX1", 1)
        |> addWire ("MUX1", 0) ("G1", 1)
        |> addWire ("G2", 0) ("MUX1", 0)
        |> addWire ("G2", 0) ("MUX2", 1)
        |> addWire ("S1", 0) ("MUX1", 2)
        |> getOkOrFail

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

let makeCCTestingCircuit (andXY: XYPos) = 
    initSheetModel
    |> placeSymbol "AludeCode" aludeCC andXY
    |> addSym "S1" (Input1(1,None)) -200. -50.
    |> addSym "S2" (Input1(1,None)) -200. 50.
    |> addSym "O1" (Output(1)) 100. 100. 
    |> addSym "MUX4" Mux4 200. 0.
    |> addWire ("S1", 0) ("AludeCode", 1)
    |> addWire ("S2", 0) ("AludeCode", 0)
    |> addWire ("AludeCode", 0) ("O1", 0)
    |> addWire ("AludeCode", 1) ("MUX4", 3)
    |> getOkOrFail



//------------------------------------------------------------------------------------------------//
//-------------------------Example assertions used to test sheets---------------------------------//
//------------------------------------------------------------------------------------------------//

// helper functions:
// need to avoid completelt: 
// wire crossing symbols
// reduce for beautify:
// same net crossing, distinct net crossing


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


let targetSheetD2 (sheet: SheetT.Model) = 
    let crossings = 
        sheet
        |> numSegmentCrossRightAngle
    printfn "Crossings before beautify: %d" crossings
    let beautifiedSheet = sheetOrderFlip sheet
    let crossingsAfter = 
        beautifiedSheet
        |> numSegmentCrossRightAngle
    printfn "Crossings after beautify: %d" crossingsAfter
    beautifiedSheet

//---------------------------------------------------------------------------------------//
//-----------------------------Evaluation------------------------------------------------//
//---------------------------------------------------------------------------------------//
// Evaluation of the circuit will be calulated after the sheetChecker is run successfully. 
// Each evaluation metric returns score between [0, 1].
// The larger the score, the more 'beautiful' the beautified sheet is 
// relative to ideal beautification.

module Evaluations =

    /// Calculates the proportion of wire bends compared to the ideal solution.
    /// Same as evaluating the number of visual segments
    let wireBendProp (sheet: SheetT.Model) =
        let wires = mapValues sheet.Wire.Wires
        let symMap = sheet.Wire.Symbol

        // Ideal min turn with no position constraints
        let wireMinTurns (wire: BusWireT.Wire) =
            let inpEdge = getInputPortOrientation symMap wire.InputPort
            let outEdge = getOutputPortOrientation symMap wire.OutputPort
            match inpEdge, outEdge with
            | edge1, edge2 when edge1 = edge2 -> 2
            | Left, Right | Right, Left | Top, Bottom | Bottom, Top -> 0
            | _ -> 1

        let rightAngs = numOfVisRightAngles sheet
        let idealRightAngs =
            wires
            |> Array.map wireMinTurns
            |> Array.sum

        match rightAngs with
        | 0 -> 1.
        | _ -> float idealRightAngs / float rightAngs

    /// Evaluates number of wires compared to number of visual segments
    let visualSegmentProp (sheet: SheetT.Model) =
        failwithf "Not implemented"

    /// Evaluates number of crosses of wires compared to number of wires in sheet
    /// Returns 1 if no wire crosses
    /// Calculates the proportion of wire crossings compared to the total number of wires
    let wireCrossProp (sheet: SheetT.Model) =
        let numCrossing = numOfWireRightAngleCrossings sheet
        let numWires = mapValues sheet.Wire.Wires |> Array.length
        match numCrossing with
        | 0 -> 1.
        | _ -> 1. - (float numCrossing / float numWires)

    /// Evaluates wire squashing between symbols
    let wireSquashProp (sheet: SheetT.Model) =
        failwithf "Not implemented"
        // getWiresInBox

    /// Evaluates length of wires compared to ideal minimum
    let wireLengthProp (sheet: SheetT.Model) =    
        let minWireLen wire =
            BusWireRoute.getWireVertices
        failwithf "Not implemented"

    // For each symbol in sheet
    // evaluates alignment with all other symbols
    // getSymbolPos
    /// Evaluates symbol alignment with all other symbols
    let symCentreAlignmentProp (sheet: SheetT.Model) : float =
        let syms = mapValues sheet.Wire.Symbol.Symbols
        
        /// Scores how aligned two symbols are
        let calcAlignment (symA: SymbolT.Symbol) (symB: SymbolT.Symbol) =
            // getSymBoundingBox
            failwithf "not implemented"

        Array.allPairs syms syms
        |> Array.sumBy (function | (symA,symB) when symA.Id <= symB.Id -> calcAlignment symA symB
                                 | _ -> 0.)
        |> (fun x -> x / (float (Array.length syms))) // Scales to lots of symbols

    type ConfigD1 =
        {
            wireBendWeight: float
            wireCrossWeight: float // numOfWireRightAngleCrossings
            wireSquashWeight: float
            wireLengthWeight: float // calcVisWireLength
            failPenalty: float // -1
        }

    let combEval evalA weightA evalB weightB (sheet: SheetT.Model) =
        weightA * (evalA sheet) + weightB * (evalB sheet)

    /// Combines all evaluations into one score
    let evaluateD1 (c: ConfigD1) (sheet: SheetT.Model) : float =
        c.wireBendWeight * (wireBendProp sheet)
        |> (+) (c.wireCrossWeight * (float (numOfWireRightAngleCrossings sheet)))
        |> (+) (c.wireSquashWeight * (float (wireSquashProp sheet)))
        |> (+) (c.wireSquashWeight * (float (wireSquashProp sheet)))


//---------------------------------------------------------------------------------------//
//-----------------------------Demo tests on Draw Block code-----------------------------//
//---------------------------------------------------------------------------------------//

module Tests =
        open Asserts
        open Evaluations
        
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
                horizLinePositions
                showTargetSheet
                (Some sheetOrderFlip)
                makeTestCircuit2
                (AssertFunc indevTestWiresCrossing)
                Evaluations.nullEvaluator
                dispatch
            |> recordPositionInTest testNum showTargetSheet dispatch
        
        let test3 testNum firstSample showTargetSheet dispatch =
            runTestOnSheets
                "D3 Test Circuit 3"
                firstSample
                horizLinePositions
                showTargetSheet
                (Some targetSheetD2)
                makeTestCircuit3
                (AssertFunc failOnAllTests)
                Evaluations.nullEvaluator
                dispatch
            |> recordPositionInTest testNum showTargetSheet dispatch
        
        let testRandom testNum firstSample showTargetSheet dispatch =
            runTestOnSheets
                "D4 Test Random Circuit"
                firstSample
                randomPos
                showTargetSheet
                (Some sheetOrderFlip)
                makeRandomTestCircuit
                (AssertFunc indevTestWiresCrossing)
                Evaluations.nullEvaluator
                dispatch
            |> recordPositionInTest testNum showTargetSheet dispatch
        
            // TargetFuncWorse
            // { 
            //     EvalFunc = wireBendProp
            //     Penalty = -1
            // }

        let testCC testNum firstSample showTargetSheet dispatch =
            runTestOnSheets
                "D5 Test Custom Component"
                firstSample
                horizLinePositions
                showTargetSheet
                None
                makeCCTestingCircuit
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
                "dev testing", test1
                "Mux2", test2
                "Mux2 & And", test3 
                "Random", testRandom 
                "Custom Component", testCC
                "Test6", fun _ _ _ _ -> printf "Test6"
                "Test7", fun _ _ _ _ -> printf "Test7"
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
        


    


            


