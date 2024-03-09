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
open SheetBeautifyHelpers
open Fable.Core
open TestDrawBlockD1.HLPTick3.Builder
open SheetUpdateHelpers
open CommonTypes
open Sheet
open TestDrawBlockD1.TestLib
open TestDrawBlockD1.HLPTick3

// Javascript to print bold blue text to console.log for better readability
[<Emit("console.log('%c' + $0, 'font-weight: bold; color: blue;')")>]
let consoleLogBlueBold (message: string) : unit = jsNative

let initSheetModel = DiagramMainView.init().Sheet

let UndoList_: Lens<SheetT.Model, SheetT.Model List> =
    Lens.create (fun m -> m.UndoList) (fun w m -> { m with UndoList = w })

let appendUndoListSheetT (undoList: SheetT.Model List) (model_in: SheetT.Model) : SheetT.Model List =
    let rec removeLast inputLst =
        inputLst
        |> List.truncate (max 0 (inputLst.Length - 1))

    match List.length undoList with
    | n when n < 500 -> model_in :: undoList
    | _ -> model_in :: (removeLast undoList)

let test1Circuit =
    let andPos = { X = 80.0; Y = 0.0 } + middleOfSheet
    initSheetModel
    |> placeSymbol "G1" (GateN(And, 2)) andPos
    |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
    |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
    |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0))
    |> getOkOrFail

let makeTestSheet (initSheetModel: SheetT.Model) : SheetT.Model =
    // make circuit here
    initSheetModel

let showSheetInIssieSchematicWithUndo (testSheetModel: SheetT.Model) (dispatch: Dispatch<Msg>) =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    // let newModel =
    //     model
    //     |> Optic.set sheet_ testSheetModel
    dispatch
    <| UpdateModel(Optic.set sheet_ testSheetModel) // set the Sheet component of the Issie model to make a new schematic.
    sheetDispatch <| SheetT.KeyPress SheetT.CtrlW // Centre & scale the schematic to make all components viewable.

let testBeautifyFuncOnSheet
    (dispatch: Dispatch<Msg>)
    (model: Model)
    (sheetChecker: Option<SheetT.Model -> string>)
    (testFunction: SheetT.Model -> SheetT.Model)
    =
    let newSheet = testFunction model.Sheet
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    match sheetChecker with
    | Some checker -> printf "output: %A" (checker newSheet)
    | None -> ()

    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    dispatch
    <| UpdateModel(fun (model: Model) ->
        let newUndoList = appendUndoListSheetT model.Sheet.UndoList model.Sheet
        model
        |> Optic.set sheet_ newSheet
        |> Optic.set (sheet_ >-> UndoList_) (newUndoList))
    sheetDispatch <| SheetT.KeyPress SheetT.CtrlW

let testBeautifyFuncOnSetSheet
    (dispatch: Dispatch<Msg>)
    (model: Model)
    (sheetChecker: Option<SheetT.Model -> string>)
    (testFunction: SheetT.Model -> SheetT.Model)
    =
    let newSheet = test1Circuit |> testFunction

    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    dispatch
    <| UpdateModel(fun (model: Model) ->
        let newUndoList = appendUndoListSheetT model.Sheet.UndoList model.Sheet
        model
        |> Optic.set sheet_ newSheet
        |> Optic.set (sheet_ >-> UndoList_) (newUndoList))
    sheetDispatch <| SheetT.KeyPress SheetT.CtrlW

    // let sheetDispatch sMsg = dispatch (Sheet sMsg)
    match sheetChecker with
    | Some checker -> printf "output: %A" (checker newSheet)
    | None -> ()

    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    dispatch
    <| UpdateModel(fun (model: Model) ->
        let newUndoList = appendUndoListSheetT model.Sheet.UndoList model.Sheet
        model
        |> Optic.set sheet_ newSheet
        |> Optic.set (sheet_ >-> UndoList_) (newUndoList))
    sheetDispatch <| SheetT.KeyPress SheetT.CtrlW

let beautifyFunction (sheet: SheetT.Model) : SheetT.Model = sheet

let testSheetFunc (dispatch: Dispatch<Msg>) (model: Model) =
    printf ""

    // CODE TO PLACE ON SHEET WITH UNDO
    // (consoleLogBlueBold "Placing On Sheet")
    // let sheetDispatch sMsg = dispatch (Sheet sMsg)
    // dispatch
    // <| UpdateModel(fun (model: Model) ->
    //     let newUndoList = appendUndoListSheetT model.Sheet.UndoList model.Sheet
    //     model
    //     |> Optic.set sheet_ test1Circuit
    //     |> Optic.set (sheet_ >-> UndoList_) (newUndoList))
    // sheetDispatch <| SheetT.KeyPress SheetT.CtrlW

    // CODE TO RUN WITH EXISTING SHEET + UNDO
    // (consoleLogBlueBold "Running Beautify On Sheet")
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    dispatch
    <| UpdateModel(fun (model: Model) ->
        let newUndoList = appendUndoListSheetT model.Sheet.UndoList model.Sheet
        model
        |> Optic.set sheet_ (beautifyFunction model.Sheet)
        |> Optic.set (sheet_ >-> UndoList_) (newUndoList))

(*

    Notes to myself:
    Keypress to LOAD circuit from a function and add model to UNDO list

    Keypress to LOAD

    Another keypress to run a function on the circuit and add to UNDO list, + output any metrics

    Another function that creates a circuit but also obtains a symbol to be passed to the function.

    Another prettyprint function for the wire
*)
