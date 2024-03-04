module TestSheetFunctions

open GenerateData
open Elmish
open EEExtensions
open Optics
open Optics.Operators
open DrawHelpers
open Helpers
open CommonTypes
open ModelType
open DrawModelType
open Sheet.SheetInterface
open BeautifySheetHelpers
open Fable.Core

[<Emit("console.log('%c' + $0, 'font-weight: bold; color: blue;')")>]
let consoleLogBlueBold (message: string) : unit = jsNative

let testSheetFunc (dispatch: Dispatch<Msg>) (model: Model) =
    printf ""
    (consoleLogBlueBold "Test Sheet Functions")

    let modelSheet = model.Sheet

    printf "T1R: countIntersectingSymbolPairs %A" (countIntersectingSymbolPairs modelSheet)
    printf "T2R: countVisibleSegsIntersectingSymbols %A" (countVisibleSegsIntersectingSymbols modelSheet)
    printf "T3R: countVisibleSegsPerpendicularCrossings %A" (countVisibleSegsPerpendicularCrossings modelSheet)
    printf "T4R: getApproxVisibleSegmentsLength %A" (getApproxVisibleSegmentsLength modelSheet)
    printf "countVisibleRAngles %A" (countVisibleRAngles modelSheet)
    printf "countUniqRetracingSegmentsAndIntersects %A" (countUniqRetracingSegmentsAndIntersects modelSheet)
