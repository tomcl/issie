module TestDrawBlockD2
// open modules likely to be used
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.BusWireT
open DrawModelType.SheetT
open DrawModelType.SymbolT
open EEExtensions
open Elmish
open GenerateData
open Helpers
open ModelType
open Optics
open Optics.Operators
open Sheet.SheetInterface
open SheetBeautifyHelpers
open SheetUpdateHelpers
open SymbolResizeHelpers
open TestDrawBlock
open TestDrawBlock.HLPTick3
open TestDrawBlock.HLPTick3.Asserts
open TestDrawBlock.HLPTick3.Builder
open TestDrawBlock.HLPTick3.Tests
open TestLib
open SheetBeautifyD2

// Define a new type that combines an XYPos with an optional FlipType
type posFlipPair = {
    Position: XYPos
    Flip: Option<FlipType>
}


//--------------------------------------------------------------------------------------------------------------------------//
//-----------------------------------------------------Helper Functions-----------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//

/// Function to print any value
let print x = printfn "%A" x

/// <summary> Prints metrics related to wire intersections and right angles within a given sheet model. </summary>
/// <param name="model">The sheet model to analyze.</param>
let printMetrics (model : SheetT.Model) : unit =
    // T1
    let symbolPairIntersection : int = numOfIntersectedSymPairs model
    print ($"Number of Symbol and Symbol Intersections: {symbolPairIntersection}")
    
    // T2
    let segmentSymbolIntersections : int = numOfIntersectSegSym model
    print ($"Number of Symbol and Wire Intersections: {segmentSymbolIntersections}")

    // T3
    let wireIntersections : int = numOfWireRightAngleCrossings model
    print ($"Number of Wire Crossings: {wireIntersections}")
    
    // T4
    let totalSegLength : float = calcVisWireLength model
    print ($"Total Segment Length: {totalSegLength}")

    // T5
    let wireRightAngles : int = numOfVisRightAngles model
    print ($"Number of Wire Right angles: {wireRightAngles}")

/// <summary> Prints the difference in metrics between two given sheet models. </summary>
/// <param name="model1">The sheet model before the beautification.</param>
/// <param name="model2">The sheet model after the beautification.</param>
let calcMetricsDiff (model1 : SheetT.Model) (model2 : SheetT.Model) : unit =
    let symbolPairIntersection1 : int = numOfIntersectedSymPairs model1
    let symbolPairIntersection2 : int = numOfIntersectedSymPairs model2
    print ($"Number of Symbol and Symbol Intersections Removed: {symbolPairIntersection1-symbolPairIntersection2}")
    
    // T2
    let segmentSymbolIntersections1 : int = numOfIntersectSegSym model1
    let segmentSymbolIntersections2 : int = numOfIntersectSegSym model2
    print ($"Number of Symbol and Wire Intersections Removed: {segmentSymbolIntersections2 - segmentSymbolIntersections1}")

    // T3
    let wireIntersections1 : int = numOfWireRightAngleCrossings model1
    let wireIntersections2 : int = numOfWireRightAngleCrossings model2
    print ($"Number of Wire Crossings Removed: {wireIntersections2 - wireIntersections1}")
    
    // T4
    let totalSegLength1 : float = calcVisWireLength model1
    let totalSegLength2 : float = calcVisWireLength model2
    print ($"Total Segment Length Reduced: {totalSegLength2 - totalSegLength1}")

    // T5
    let wireRightAngles1 : int = numOfVisRightAngles model1
    let wireRightAngles2 : int = numOfVisRightAngles model2
    print ($"Number of Wire Right angles Removed: {wireRightAngles2 - wireRightAngles1}")


/// <summary> Finds the ID of a symbol within a model by its label. </summary>
/// <param name="model">The sheet model containing the symbols.</param>
/// <param name="targetLabel">The label of the symbol to find.</param>
/// <returns> The ID of the symbol if found, wrapped in an Option type; otherwise, None. </returns>
let findSymbolIdByLabel (model: SheetT.Model) (targetLabel: string) : Option<ComponentId> =
    model.Wire.Symbol.Symbols
    |> Map.toSeq 
    |> Seq.tryFind (fun (_, sym) -> sym.Component.Label.ToUpper() = targetLabel.ToUpper()) 
    |> Option.map fst 


/// <summary> Flips a symbol within the model based on the specified orientation. </summary>
/// <param name="symLabel">The label of the symbol to flip.</param>
/// <param name="orientation">The orientation to flip the symbol to.</param>
/// <param name="model">The current sheet model containing the symbol.</param>
/// <returns> A Result containing the updated model if the symbol is found and flipped, or the original model if the symbol is not found.</returns>
let flipSymbol (symLabel: string) (orientation: FlipType option) (model : SheetT.Model) : Result<SheetT.Model, string> =
    match orientation with
    | None -> Ok model // Return the original model if no flip orientation is provided
    | Some(orientation) ->
        match findSymbolIdByLabel model symLabel with
        | Some(id) ->
            let sym = model.Wire.Symbol.Symbols[id]
            let updatedSym = SymbolResizeHelpers.flipSymbol orientation sym
            let updatedSymbols = Map.add id updatedSym model.Wire.Symbol.Symbols
            let newModel = { model with Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbols } } }
            Ok newModel
        | None -> Ok model

let gridStep = 50.0


/// <summary>Generates a 2x2 grid of XYPos positions based on an offset from the middle of the sheet.</summary>
/// <param name="offsetX">The horizontal offset from the middle of the sheet for the grid's starting position.</param>
/// <param name="offsetY">The vertical offset from the middle of the sheet for the grid's starting position.</param>
/// <returns>A Gen<XYPos> representing a list of positions for a 2x2 grid.</returns>
let makeGrid (offsetX: float) (offsetY: float) =
    let x, y = middleOfSheet.X, middleOfSheet.Y
    [{X = x + offsetX; Y = y + offsetY};
     {X = x + offsetX + gridStep; Y = y + offsetY};
     {X = x + offsetX; Y = y + offsetY + gridStep};
     {X = x + offsetX + gridStep; Y = y + offsetY + gridStep}]
    |> fromList

//--------------------------------------------------------------------------------------------------------------------------//
//----------------------------------------------------Beautify Functions----------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//

// Placeholder for actual implementation
let beautify (model : SheetT.Model) : SheetT.Model = 
    findBestModel model

/// <summary> Beautifies the given sheet within the model then updates and displays the new sheet. Prints metrics before and after changes. </summary>
/// <param name="model">The model containing the sheet to be beautified.</param>
/// <param name="dispatch"> dispatch</param>
let beautifySheet (model : ModelType.Model) (dispatch: Dispatch<Msg>): unit = 
    // Use optic to get sheet
    let sheet = Optic.get sheet_ model
    
    // Print metrics for current Sheet
    print "\nMetrics before beautifying"
    printMetrics sheet
    
    // Beautify sheet and print new metrics
    let newSheet = beautify sheet
    print "\nMetrics after beautifying"
    printMetrics newSheet
    
    print "\nDifference in Metrics"
    calcMetricsDiff sheet newSheet

    // Update the Model with the new sheet and display it
    let newModel = Optic.set sheet_ newSheet model
    showSheetInIssieSchematic newModel.Sheet dispatch



//--------------------------------------------------------------------------------------------------------------------------//
//----------------------------------------Example Test Circuits using Gen<'a> samples---------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//

//----------------------------------------------------- Circuit 1 ---------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//
// Generate Random Flips
let flipTypeGen = fromList [None; Some FlipHorizontal; Some FlipVertical]

// Generate all pairs of FlipType
let flipTypePairs = product (fun a b -> (a, b)) flipTypeGen flipTypeGen

// Generate all triples by combining pairs with single flip types
let flipTypeTriples = product (fun (a, b) c -> (a, b, c)) flipTypePairs flipTypeGen

let flipTypeQuadruples = product (fun (a, b, c) d -> (a, b, c, d)) flipTypeTriples flipTypeGen

// Define the positions of Symbols in the Sheet for Circuit 1
let mux1PosCircuit1 : XYPos = middleOfSheet - { X = 150.0; Y = 175.0 }
let mux2PosCircuit1 : XYPos = middleOfSheet
let input1PosCircuit1 : XYPos = middleOfSheet - { X = 200.0; Y = -28.0 }
let input2PosCircuit1 : XYPos = middleOfSheet - { X = 200.0; Y = -90.0 }
let andPosCircuit1 : XYPos = middleOfSheet - { X = -180.0; Y = 180. }



// Generate a circuit with 2 inputs, 2 Mux2 and one and gate with random flips
let makeCircuit1 (symFlips : Option<FlipType> * Option<FlipType> * Option<FlipType>) =
    let flipMUX1, flipMUX2, flipGate = symFlips 
    let model = 
        initSheetModel
        |> placeSymbol "G1" (GateN(And, 2)) andPosCircuit1
        |> Result.bind (flipSymbol "G1" flipGate)
        |> Result.bind (placeSymbol "MUX1" Mux2 mux1PosCircuit1)
        |> Result.bind (flipSymbol "MUX1" flipMUX1)
        |> Result.bind (placeSymbol "MUX2" Mux2 mux2PosCircuit1)
        |> Result.bind (flipSymbol "MUX2" flipMUX2)
        |> Result.bind (placeSymbol "S1" (Input1(1, Some 1)) input1PosCircuit1)
        |> Result.bind (placeSymbol "S2" (Input1(1, Some 1)) input2PosCircuit1)
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "G1" 1))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "G1" 0))
        |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2))
        |> getOkOrFail
    model
//--------------------------------------------------------------------------------------------------------------------------//


//----------------------------------------------------- Circuit 2 ---------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//
// Define the positions of Symbols in the Sheet for Circuit 2
let notGatePosCircuit2 : XYPos = middleOfSheet - { X = 200.0; Y = 0.0 }
let mux1PosCircuit2 : XYPos = middleOfSheet - { X = 100.0; Y = 100.0 }
let mux2PosCircuit2 : XYPos = middleOfSheet + { X = -100.0; Y = 100.0 }
let orGatePosCircuit2 : XYPos = middleOfSheet + { X = 0.0; Y = 0.0 }
let inputPosCircuit2 : XYPos = middleOfSheet - { X = 300.0; Y = 0.0 }
let outputPosCircuit2 : XYPos = middleOfSheet + { X = 150.0; Y = 0.0 }

// Generate a circuit with an input, two Mux2s, a Not gate, an Or gate, and an output with random flips
let makeCircuit2 (symFlips : Option<FlipType> * Option<FlipType> * Option<FlipType> * Option<FlipType>) =
    let flipMUX1, flipMUX2, flipOr, flipNot = symFlips 
    let model = 
        initSheetModel
        |> placeSymbol "Not1" (Not) notGatePosCircuit2
        |> Result.bind (flipSymbol "Not1" flipNot)
        |> Result.bind (placeSymbol "MUX1" Mux2 mux1PosCircuit2)
        |> Result.bind (flipSymbol "MUX1" flipMUX1)
        |> Result.bind (placeSymbol "MUX2" Mux2 mux2PosCircuit2)
        |> Result.bind (flipSymbol "MUX2" flipMUX2)
        |> Result.bind (placeSymbol "Or1" (GateN(Or, 2)) orGatePosCircuit2)
        |> Result.bind (flipSymbol "Or1" flipOr)
        |> Result.bind (placeSymbol "In1" (Input1(1, Some 1)) inputPosCircuit2)
        |> Result.bind (placeSymbol "Out1" (Output 1) outputPosCircuit2)
        // Wire connections
        |> Result.bind (placeWire (portOf "In1" 0) (portOf "Not1" 0))
        |> Result.bind (placeWire (portOf "In1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "Not1" 0) (portOf "MUX1" 1))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "Or1" 0))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "Or1" 1))
        |> Result.bind (placeWire (portOf "Or1" 0) (portOf "Out1" 0))
        |> getOkOrFail
    model

//--------------------------------------------------------------------------------------------------------------------------//


//----------------------------------------------------- Circuit 3 ---------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//

// Create the generator for Circuit with random positions
let grid1 = makeGrid 0.0 0.0
let grid2 = makeGrid -150.0 -100.0
let grid3 = makeGrid -100.0 50.0

// Combine position and flip into a PosAndFlip generator
let posAndFlipGen1 = product (fun position flip -> { Position = position; Flip = flip }) grid1 flipTypeGen
let posAndFlipGen2 = product (fun position flip -> { Position = position; Flip = flip }) grid2 flipTypeGen
let posAndFlipGen3 = product (fun position flip -> { Position = position; Flip = flip }) grid3 flipTypeGen

// Generate all combinations for three symbols
let posAndFlipPair = product (fun posFlip1 posFlip2 -> (posFlip1, posFlip2)) posAndFlipGen1 posAndFlipGen2
let posAndFlipTriplesGen = product (fun (posFlip1, posFlip2) posFlip3 -> (posFlip1, posFlip2, posFlip3)) posAndFlipPair posAndFlipGen3

// Generate a circuit with an input, two Mux2s, an And gate and an output with random flips and positioning
let makeCircuit3 (symInfo : posFlipPair * posFlipPair * posFlipPair) =
    let (MUX1info, MUX2info, andInfo) = symInfo
    let MUX1pos = MUX1info.Position
    let MUX1flip = MUX1info.Flip
    let MUX2pos = MUX2info.Position
    let MUX2flip = MUX2info.Flip
    let andPos = andInfo.Position
    let andFlip = andInfo.Flip
    let model = 
        initSheetModel
        |> placeSymbol "And1" (GateN(And, 2)) andPos
        |> Result.bind (flipSymbol "And1" andFlip)
        |> Result.bind (placeSymbol "MUX1" Mux2 MUX1pos)
        |> Result.bind (flipSymbol "MUX1" MUX1flip)
        |> Result.bind (placeSymbol "MUX2" Mux2 MUX2pos)
        |> Result.bind (flipSymbol "MUX2" MUX2flip)
        |> Result.bind (placeSymbol "In1" (Input1(1, Some 1)) inputPosCircuit2)
        |> Result.bind (placeSymbol "Out1" (Output 1) outputPosCircuit2)
        // Wire connections
        |> Result.bind (placeWire (portOf "In1" 0) (portOf "And1" 0))
        |> Result.bind (placeWire (portOf "In1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "And1" 0) (portOf "MUX1" 1))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "OUT1" 0))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "MUX1" 0))
        |> getOkOrFail
    model

//--------------------------------------------------------------------------------------------------------------------------//




//--------------------------------------------------------------------------------------------------------------------------//
//------------------------------------------------Demo tests on Draw Block code---------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//
let test1 testNum firstSample dispatch =
    runTestOnSheets
        "Manually Generated: 2 MUXes and 1 Gate With Random Flips"
        firstSample
        flipTypeTriples
        makeCircuit1 
        failOnAllTests
        dispatch
    |> recordPositionInTest testNum dispatch

let test2 testNum firstSample dispatch =
    runTestOnSheets
        "Manually Generated: 2 MUXes and 2 Gates With Random Flips"
        firstSample
        flipTypeQuadruples
        makeCircuit2
        failOnAllTests
        dispatch
    |> recordPositionInTest testNum dispatch

let test3 testNum firstSample dispatch =
    runTestOnSheets
        "Randomly Generated: 2 MUXes and a Gate With Random Flips"
        firstSample
        posAndFlipTriplesGen
        makeCircuit3
        failOnWireIntersectsSymbol
        dispatch
    |> recordPositionInTest testNum dispatch

let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
    // Change names and test functions as required
    // delete unused tests from list
    [
        "Test1", test1
        "Test2", test2
        "Test3", test3
        "Blank", fun _ _ _ -> printf "Test4" // dummy test - delete line or replace by real test as needed
        "Blank", fun _ _ _ -> printf "Test5" // dummy test - delete line or replace by real test as needed
        "Run Beautify Function 1", fun _ _ _ -> printf "Test6"
        "Run Beautify Function 2", fun _ _ _ -> printf "Flipping MUX1"
        "Run Beautify Function", fun _ _ _ -> printf "Running beautify algorithm"
        "Next Test Error", fun _ _ _ -> printf "Next Error:" // Go to the nexterror in a test

    ]

let testMenuFunc (testIndex: int) (dispatch: Dispatch<Msg>) (model: Model) =
            let name,func = testsToRunFromSheetMenu[testIndex] 
            printf "%s" name
            match name, model.DrawBlockTestState with
            | "Next Test Error", Some state ->
                nextError testsToRunFromSheetMenu[state.LastTestNumber] (state.LastTestSampleIndex+1) dispatch
            | "Next Test Error", None ->
                printf "Test Finished"
                ()
            | "Run Beautify Function", _ ->
                beautifySheet model dispatch
                printf "Test Finished"
                ()
            | _ ->
                func testIndex 0 dispatch