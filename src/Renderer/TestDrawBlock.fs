module TestDrawBlock
open GenerateData
open Elmish


//-------------------------------------------------------------------------------------------//
// yc3821:
//
// I added some helper functions regarding the team phase at Testing for D1.
//
// Summary of what is done in this script:
// 1. deleted unnecessary code to make it cleaner
// 2. built a test metrics to access the performance of the D1 algorithm and current version
// 3. when performing Ctrl-1, the test metrics will be printed in developer console
//
// These codes will serve as a good starting point for T1.
//
// The reason that I did not use the sheet builder approach is that, I wanted to build some testing
// circuits in Issie first that is targeted to test certain features of the D1 algorithm, and test
// the performance of the D1 algorithm on those circuits.
//-------------------------------------------------------------------------------------------//


module TestLib =
    type Test<'a> = {
        Name: string
        Samples: Gen<'a>
        StartFrom: int
        /// The 1st argument is the test number: allows assertions that fail on a specific sample
        /// to display just one sample.
        /// The return value is None if test passes, or Some message if it fails.
        Assertion: int -> 'a -> string option
        }


 
            
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


    /// Test Metrics for Sheet
    type TestMetrics = {
        /// number of segments in total
        numSegments: int
        /// number of visible segments (excluding zero length segments)
        numVisibleSegments: int
        /// visible length of all segments
        visibleLength: float
        /// number of right angle segments
        rightAngleCount: int
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

            numVisibleSegments = getVisibleSegments sheet

            visibleLength = getVisibleWireLength sheet

            rightAngleCount = getVisibleWireRightAngles sheet

            rightAngleCrossings = getRightAngleCrossings sheet

            wireSymbolIntersections = getWireSymbolIntersectionCount sheet

            retracingSegments = (fst (getRetracingSegments sheet)) |> List.length

            retracingSegmentsInsideSymbol = (snd (getRetracingSegments sheet)) |> List.length

            intersectingSymbols = getIntersectingSymbols sheet
        }


   



//---------------------------------------------------------------------------------------//
//----------------------------- tests on Draw Block code-----------------------------//
//---------------------------------------------------------------------------------------//

    module Tests =

        let TestD1Metrics (sheet : SheetT.Model) =
            // get test metrics for the un-optimized version
            let originalMetrics = getTestMetrics sheet

            // TODO: call the D1 algorithm (yet to be done by partner), dummy for now
            let optimizedSheet = sheet

            // get test metrics for the optimized version
            let optimizedMetrics = getTestMetrics optimizedSheet

            // compare the two metrics, we simply log them for now
            printfn "Original Metrics: %A" originalMetrics
            printfn "Optimized Metrics: %A" optimizedMetrics


        /// List of tests available which can be run ftom Issie File Menu.
        /// The first 9 tests can also be run via Ctrl-n accelerator keys as shown on menu
        let testsToRunFromSheetMenu =
            [
                "D1 Test", TestD1Metrics;   // prints metrics of the current and D1 algorithm
            ]


        /// common function to execute any test.
        /// testIndex: index of test in testsToRunFromSheetMenu
        let testMenuFunc (testIndex: int) (dispatch: Dispatch<Msg>) (model: Model) =
            let name,func = testsToRunFromSheetMenu[testIndex] 
            printf "%s" name

            // execute the test on current sheet
            func model.Sheet
        


    


