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
let beautifyFunction (sheet: SheetT.Model) : SheetT.Model = sheet

let testSheetFunc (dispatch: Dispatch<Msg>) (model: Model) =
    // Code to Run with Existing Sheet + Undo
    (consoleLogBlueBold "Running Function On Sheet (undo with cmd/ctrl + z)")
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    dispatch
    <| UpdateModel(fun (model: Model) ->
        let newUndoList = appendUndoListSheetT model.Sheet.UndoList model.Sheet
        model
        |> Optic.set sheet_ (beautifyFunction model.Sheet)
        |> Optic.set (sheet_ >-> UndoList_) (newUndoList))

let testSheetFuncWithCircuit (dispatch: Dispatch<Msg>) (modelIn: Model) =
    // Code to Run with New Circuit + Undo
    (consoleLogBlueBold "Placing and Running Function On Sheet (undo with cmd/ctrl + z twice)")
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    dispatch
    <| UpdateModel(fun (model: Model) ->
        let newUndoList = appendUndoListSheetT model.Sheet.UndoList modelIn.Sheet
        // load test circuit
        let modelWithCircuit =
            model
            // since cannot keypress ctrlw, manually fit interim circuit
            |> Optic.set sheet_ (fst (fitCircuitToScreenUpdate test1Circuit))
            |> Optic.set (sheet_ >-> UndoList_) (newUndoList)
        // make a new UndoList to keep test circuit before beautified
        let newNewUndoList =
            appendUndoListSheetT modelWithCircuit.Sheet.UndoList modelWithCircuit.Sheet
        // update model with beautified test circuit
        modelWithCircuit
        |> Optic.set sheet_ (beautifyFunction test1Circuit)
        |> Optic.set (sheet_ >-> UndoList_) (newNewUndoList))

    sheetDispatch <| SheetT.KeyPress SheetT.CtrlW
