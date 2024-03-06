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




//--------------------------------------------------------------------------------------------------------------------------//
//-----------------------------------------------------Helper Functions-----------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//

// Function to print any value
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
    print ($"Number of Wire Intersections: {wireIntersections}")
    
    // T4
    let totalSegLength : float = calcVisWireLength model
    print ($"Total Segment Length: {totalSegLength}")

    // T5
    let wireRightAngles : int = numOfVisRightAngles model
    print ($"Number of Wire Right angles: {wireRightAngles}")


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
let flipSymbol (symLabel: string) (orientation: FlipType) (model : SheetT.Model) : Result<SheetT.Model, string> =
    match findSymbolIdByLabel model symLabel with
    | Some(id) ->
        let sym = model.Wire.Symbol.Symbols[id]
        let updatedSym = SymbolResizeHelpers.flipSymbol orientation sym
        let updatedSymbols = Map.add id updatedSym model.Wire.Symbol.Symbols
        let newModel = { model with Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbols } } }
        Ok newModel
    | None -> Ok model 


// Placeholder for actual implementation
let beautify (model : SheetT.Model) : SheetT.Model = 
    evaluateTransformations model

/// <summary> Beautifies the given sheet within the model then updates and displays the new sheet. Prints metrics before and after changes. </summary>
/// <param name="model">The model containing the sheet to be beautified.</param>
/// <param name="dispatch"> dispatch</param>
let beautifySheet (model : ModelType.Model) (dispatch: Dispatch<Msg>): unit = 
    // Use optic to get sheet
    let sheet = Optic.get sheet_ model
    
    // Print metrics for current Sheet
    print "Metrics before beautifying"
    printMetrics sheet
    
    // Beautify sheet and print new metrics
    let newSheet = beautify sheet
    print "Metrics after beautifying"
    printMetrics newSheet

    // Update the Model with the new sheet and display it
    let newModel = Optic.set sheet_ newSheet model
    showSheetInIssieSchematic newModel.Sheet dispatch



//--------------------------------------------------------------------------------------------------------------------------//
//----------------------------------------Example Test Circuits using Gen<'a> samples---------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//


// Define the positions of Symbols in the Sheet
let mux1PosCircuit1 : XYPos = middleOfSheet - { X = 150.0; Y = 175.0 }
let mux2PosCircuit1 : XYPos = middleOfSheet
let input1PosCircuit1 : XYPos = middleOfSheet - { X = 200.0; Y = -28.0 }
let input2PosCircuit1 : XYPos = middleOfSheet - { X = 200.0; Y = -90.0 }
let andPosCircuit1 : XYPos = middleOfSheet - { X = -180.0; Y = 180. }

// Generate Random Flips
let flipTypeGen = fromList [FlipHorizontal; FlipVertical]

// Generate all pairs of FlipType
let flipTypePairs = product (fun a b -> (a, b)) flipTypeGen flipTypeGen

// Generate all triples by combining pairs with single flip types
let flipTypeTriples = product (fun (a, b) c -> (a, b, c)) flipTypePairs flipTypeGen


// Generate a circuit with 2 inputs, 2 Mux2 and one and gate with random flips
let makeCircuit1 (symFlips : FlipType*FlipType*FlipType) =
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
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "G1" 0))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "G1" 1))
        |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2))
        |> getOkOrFail
    printMetrics model
    model

// let makeCircuit2

//--------------------------------------------------------------------------------------------------------------------------//
//------------------------------------------------Demo tests on Draw Block code---------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//
let test1 testNum firstSample dispatch =
    runTestOnSheets
        "2 MUXes and 1 Gate With Random Flips"
        firstSample
        flipTypeTriples
        makeCircuit1 
        failOnAllTests
        dispatch
    |> recordPositionInTest testNum dispatch



let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
    // Change names and test functions as required
    // delete unused tests from list
    [
        "Test1", test1
        "Test2", fun _ _ _ -> printf "Test2" // dummy test - delete line or replace by real test as needed
        "Test3", fun _ _ _ -> printf "Test3" // dummy test - delete line or replace by real test as needed
        "Test4", fun _ _ _ -> printf "Test4" // dummy test - delete line or replace by real test as needed
        "Test5", fun _ _ _ -> printf "Test5" // dummy test - delete line or replace by real test as needed
        "Test6", fun _ _ _ -> printf "Test6"
        "Test7", fun _ _ _ -> printf "Test7"
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