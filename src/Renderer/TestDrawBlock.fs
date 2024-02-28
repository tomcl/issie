module TestDrawBlock
open GenerateData
open Elmish


//-------------------------------------------------------------------------------------------//
// I added some helper functions regarding the team phase at Testing for D1.
//
// Summary of what is added:
// - [line x - y] - Function that calls both the current and D1 algorithm to compare them
// - [line x - y] - Assertion on if the D1 algorithm performs worse than the current one
// - [line x - y] - Info logging about how much improvement the D1 algorithm has over the current one
//
// With these helper functions, it will be a good starting point to tell
// if the D1 algorithm is doing the job.
//-------------------------------------------------------------------------------------------//


//-------------------------------------------------------------------------------------------//
//--------Types to represent tests with (possibly) random data, and results from tests-------//
//-------------------------------------------------------------------------------------------//
module TestLib =

    /// convenience unsafe function to extract Ok part of Result or fail if value is Error
    let getOkOrFail (res: Result<'a,string>) =
        match res with
        | Ok x -> x
        | Error mess ->
            failwithf "%s" mess


    type TestStatus =
            | Fail of string
            | Exception of string

    type Test<'a> = {
        Name: string
        Samples: Gen<'a>
        StartFrom: int
        /// The 1st argument is the test number: allows assertions that fail on a specific sample
        /// to display just one sample.
        /// The return value is None if test passes, or Some message if it fails.
        Assertion: int -> 'a -> string option
        }

    type TestResult<'a> = {
        TestName: string
        TestData: Gen<'a>
        FirstSampleTested: int
        TestErrors: (int * TestStatus) list
    }

    let catchException name func arg =
        try
            Ok (func arg)
        with
            | e ->
                Error ($"Exception when running {name}\n" + e.StackTrace)

 
            
module HLPTick3 =
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
    open TestLib
    open SheetBeautifyHelpers
    open SheetBeautify


    /// create an initial empty Sheet Model 
    let initSheetModel = DiagramMainView.init().Sheet

    /// Optic to access SheetT.Model from Issie Model
    let sheetModel_ = sheet_

    /// Optic to access BusWireT.Model from SheetT.Model
    let busWireModel_ = SheetT.wire_

    /// Optic to access SymbolT.Model from SheetT.Model
    let symbolModel_ = SheetT.symbol_

    /// allowed max X or y coord of svg canvas
    let maxSheetCoord = Sheet.Constants.defaultCanvasSize
    let middleOfSheet = {X=maxSheetCoord/2.;Y=maxSheetCoord/2.}

    /// Used throughout to compare labels since these are case invariant "g1" = "G1"
    let caseInvariantEqual str1 str2 =
        String.toUpper str1 = String.toUpper str2
 

    /// Identify a port from its component label and number.
    /// Usually both an input and output port will mathc this, so
    /// the port is only unique if it is known to be input or output.
    /// used to specify the ends of wires, since tehee are known to be
    /// connected to outputs (source) or inputs (target).
    type SymbolPort = { Label: string; PortNumber: int }

    /// convenience function to make SymbolPorts
    let portOf (label:string) (number: int) =
        {Label=label; PortNumber = number}


    /// Test Metrics for Sheet
    type TestMetrics = {
        /// number of segments in total
        numSegments: int
        /// number of visible segments (excluding zero length segments)
        numVisibleSegments: int
        /// visible length of all segments
        visibleLength: float
        /// right angle crossings
        rightAngleCrossings: int
        /// intersections of wire and symbols
        wireSymbolIntersections: int
        /// retracing segments total
        retracingSegments: int
        /// retracing segments that are inside a symbol
        retracingSegmentsInsideSymbol: int
        /// intersecting symbols
        intersectingSymbols: int
    }


    let getTestMetrics (sheet : SheetT.Model) : TestMetrics =
        {
            numSegments = sheet.Wire.Wires.Values
                          |> Seq.toList
                          |> List.sumBy (fun wire -> List.length wire.Segments)

            // not implemented for now
            numVisibleSegments = sheet.Wire.Wires.Values
                          |> Seq.toList
                          |> List.sumBy (fun wire -> List.length wire.Segments)

            visibleLength = getVisibleWireLength sheet

            rightAngleCrossings = getVisibleWireRightAngles sheet

            wireSymbolIntersections = getWireSymbolIntersectionCount sheet

            retracingSegments = (fst (getRetracingSegments sheet)) |> List.length

            retracingSegmentsInsideSymbol = (snd (getRetracingSegments sheet)) |> List.length

            intersectingSymbols = getIntersectingSymbols sheet
        }


    let TestD1Metrics (sheet : SheetT.Model) =
        // get test metrics for the un-optimized version
        let originalMetrics = getTestMetrics sheet

        // call the D1 algorithm, dummy for now
        let optimizedSheet = sheet

        // get test metrics for the optimized version
        let optimizedMetrics = getTestMetrics optimizedSheet

        // compare the two metrics, we simply log them for now
        printfn "Original Metrics: %A" originalMetrics
        printfn "Optimized Metrics: %A" optimizedMetrics



//---------------------------------------------------------------------------------------//
//----------------------------- tests on Draw Block code-----------------------------//
//---------------------------------------------------------------------------------------//

    module Tests =

        /// Example test: Horizontally positioned AND + DFF: fail on sample 0
        let testD1Metrics testNum firstSample dispatch =
            // TestD1Metircs


        /// List of tests available which can be run ftom Issie File Menu.
        /// The first 9 tests can also be run via Ctrl-n accelerator keys as shown on menu
        let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
            // Change names and test functions as required
            // delete unused tests from list
            [
                "D1 Test", testD1Metrics;
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
                nextError testsToRunFromSheetMenu[state.LastTestNumber] (state.LastTestSampleIndex+1) dispatch
            | "Next Test Error", None ->
                printf "Test Finished"
                ()
            | _ ->
                func testIndex 0 dispatch
        


    


