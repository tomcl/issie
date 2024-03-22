module TestDrawBlock

open SheetBeautify
open SheetBeautifyHelpers
open SheetBeautifyAlign
open SheetBeautifyFlip
open SheetBeautifyWireLabel


open TestDrawBlockAlign.HLPTick3.Tests
open TestDrawBlockFlip.Tests
open TestDrawBlockWireLabel
open TestDrawBlockHelpers.Displays
open TestDrawBlockHelpers.Builder

open DrawModelType
open ModelType
open CommonTypes
open Helpers
open Elmish

/// This test is to apply beautification and calculate metric
let applyBeautify beautifyFunc testNum firstSample dispatch model =
    let beforeCount = numOfWireRightAngleCrossings model.Sheet
    let sheetAfter = beautifyFunc model.Sheet
    let afterCount = numOfWireRightAngleCrossings sheetAfter
    
    metricDisplay displayMetrics sheetAfter model.Sheet
    printfn $"after crossings: {afterCount}, before: {beforeCount}"
    showSheetInIssieSchematic sheetAfter dispatch

/// This test is to display info on the current model as it is in Issie
let displayCurSheet testNum firstSample dispatch model =
    display displayAll model.Sheet
    ()

/// List of tests available which can be run ftom Issie File Menu.
/// The first 9 tests can also be run via Ctrl-n accelerator keys as shown on menu
let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> Model -> Unit)) list =
    // Change names and test functions as required
    // delete unused tests from list
    [
        "Apply Beautify D1", (applyBeautify (sheetAlignScale 3)) // TODO: add actual function
        "Apply Beautify D2", (applyBeautify optimizePortOrder) 
        "Apply Beautify D3", (applyBeautify wireLabelBeautify)
        "Apply Beautify D4", (applyBeautify (SheetBeautify))
        "Build: Random Components", testRandomComp
        "Test: Statistics", TestDrawBlockAlign.HLPTick3.Tests.showTestCircuit5
        "Test: Edge Case", TestDrawBlockAlign.HLPTick3.Tests.runAllTests 
        "Test: Beautify Time Complexity", testBeautifyTimePerformance
        "Next Test Error", fun _ _ _ _ -> printf "Next Error:" // Go to the nexterror in a test
        "D1: Optimize current circuit", showOptimizedCircuit
        "D1: Test Circuit 1", showTestCircuit1
        "D1: Test Circuit 2", showTestCircuit2
        "D1: Test Circuit 3", showTestCircuit3
        "D1: Test Circuit 4", showTestCircuit4
        "D1: Test Circuit 5", showTestCircuit5
        "D1: Auto Place Wires", autoPlaceWires
        "D2: Build Random Components No Rotate", testRandomCompNoRotate 
        "D2: Test Gate/MUX Flip", flipExample
        "D2: Test CC Port Order", portOrderExample 
        "D3: Regular Shifts", testD3Regular
    ]

/// Display the next error in a previously started test
let nextError (testName, testFunc) firstSampleToTest dispatch =
    let testNum =
        testsToRunFromSheetMenu
        |> List.tryFindIndex (fun (name,_) -> name = testName)
        |> Option.defaultValue 0
    testFunc testNum firstSampleToTest dispatch

/// common function to execute any test.
/// testIndex: index of test in testsToRunFromSheetMenu
let testMenuFunc (testIndex: int) (dispatch: Dispatch<Msg>) (model: Model) =
    let name,func = testsToRunFromSheetMenu[testIndex] 
    printf "%s" name
    match name, model.DrawBlockTestState with
    | "Next Test Error", Some state ->
        nextError testsToRunFromSheetMenu[state.LastTestNumber] (state.LastTestSampleIndex+1) dispatch model
    | "Next Test Error", None ->
        printf "Test Finished"
        ()
    | _ ->
        func testIndex 0 dispatch model
