module TestSheetFunctions

open GenerateData
open Elmish
open EEExtensions
open Optics
open Optics.Operators
open DrawHelpers
open Helpers
open CommonTypes
open DrawModelType
open DrawModelType.BusWireT
open ModelType

open Sheet.SheetInterface
open SheetBeautifyHelpers
open Fable.Core
open TestDrawBlockD1.HLPTick3.Builder
open SheetUpdateHelpers
open CommonTypes
open Sheet
open TestDrawBlockD1.TestLib
open TestDrawBlockD1.HLPTick3
open RotateScale

(*
Purpose of the TestSheetFunctions:
It is more focused on testing heuristics, helper functions, and beautify functions
on manually-created circuits. It is also for sub-functions that might not take in
the whole sheet, but symbols or wires.

TestdrawBlock1 is more for testing  focused on programmatically generating circuits,
for RotateScale, D1B functions, BeautifySheetHelpers- basically running functions on generated
sheets followed by basic tests e.g. wire-wire, symbol-symbol, wire-symbol intersection tests.
Mostly for wire routing tests (from Tick3) and to be used for a majority of D1T.
*)

// Javascript to print bold blue text to console.log for better readability
[<Emit("console.log('%c' + $0, 'font-weight: bold; color: blue;')")>]
let consoleLogBlueBold (message: string) : unit = jsNative

let initSheetModel = DiagramMainView.init().Sheet

// redefining so it will not depend on TestDrawBlockD1
let wire_: Lens<SheetT.Model, BusWireT.Model> =
    Lens.create (fun m -> m.Wire) (fun w m -> { m with Wire = w })

let test1Circuit =
    let andPos = { X = 80.0; Y = 0.0 } + middleOfSheet
    initSheetModel
    |> placeSymbol "G1" (GateN(And, 2)) andPos
    |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
    |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
    |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0))
    |> getOkOrFail

/// Function that will test RotateScale's reSizeSymbolTopLevel
let testRotateScale1 (model: ModelType.Model) : ModelType.Model =
    // check for at least two symbols, take the first and second, run with reSizeSymbolTopLevel
    match model.Sheet.Wire.Symbol.Symbols.Count >= 2 with
    | true ->
        let symbolToSize, otherSymbol =
            match model.Sheet.Wire.Symbol.Symbols |> Map.toList with
            | symbolToSizeInMap :: otherSymbolInMap :: _ -> snd (symbolToSizeInMap), snd (otherSymbolInMap)
            | _ -> failwith "Not enough elements in symbol list" // will never happen
        model
        |> Optic.set (sheet_ >-> wire_) (reSizeSymbolTopLevel model.Sheet.Wire symbolToSize otherSymbol)

    | false -> model

let testBeautifyFunction (model: ModelType.Model) = model.Sheet

let testSheetFunc (dispatch: Dispatch<Msg>) (model: ModelType.Model) =
    // Code to Run with Existing Sheet + Undo
    (consoleLogBlueBold "Running Function On Sheet (undo with cmd/ctrl + z)")
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    dispatch
    <| UpdateModel(fun (model: ModelType.Model) ->
        let newUndoList = appendUndoList model.Sheet.UndoList model.Sheet
        model
        |> Optic.set ModelType.sheet_ (testBeautifyFunction model)
        |> Optic.set (ModelType.sheet_ >-> UndoList_) (newUndoList))

let testSheetFuncWithCircuit (dispatch: Dispatch<Msg>) (modelIn: ModelType.Model) =
    // Code to Run with New Circuit + Undo
    (consoleLogBlueBold "Placing and Running Function On Sheet (undo with cmd/ctrl + z twice)")
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    dispatch
    <| UpdateModel(fun (model: Model) ->
        let newUndoList = appendUndoList model.Sheet.UndoList modelIn.Sheet
        // load test circuit
        let modelWithCircuit =
            model
            // since dispatching once cannot keypress ctrlw halfway, so manually fit interim circuit
            |> Optic.set ModelType.sheet_ (fst (fitCircuitToScreenUpdate test1Circuit))
            |> Optic.set (ModelType.sheet_ >-> UndoList_) (newUndoList)
        // make a new UndoList to keep test circuit before beautified
        let newNewUndoList =
            appendUndoList modelWithCircuit.Sheet.UndoList modelWithCircuit.Sheet
        // update model with beautified test circuit
        modelWithCircuit
        |> Optic.set ModelType.sheet_ (testBeautifyFunction modelWithCircuit)
        |> Optic.set (ModelType.sheet_ >-> UndoList_) (newNewUndoList))

    sheetDispatch <| SheetT.KeyPress SheetT.CtrlW

let testsToRunFromSheetMenu =
    // First text is the label that will be shown in the menu
    // Replace (fun (model: ModelType.Model) -> model) with the function you want to run
    // All functions should take in a model and return a model.
    // Be sure to modify the sheet with the sheet_ optic before returning. Undos will be handled automatically.
    [ "Test 1", testRotateScale1
      "Test 2", (fun (model: ModelType.Model) -> model)
      "Test 3", (fun (model: ModelType.Model) -> model)
      "Test 4", (fun (model: ModelType.Model) -> model)
      "Test 5", (fun (model: ModelType.Model) -> model)
      "Test 6", (fun (model: ModelType.Model) -> model)
      "Test 7", (fun (model: ModelType.Model) -> model)
      "Test 8", (fun (model: ModelType.Model) -> model)
      "Test 9", (fun (model: ModelType.Model) -> model) ]

let testMenuFunc (testIndex: int) (dispatch: Dispatch<Msg>) (model: Model) =
    let name, (func: ModelType.Model -> ModelType.Model) =
        testsToRunFromSheetMenu[testIndex]
    let funcName = nameof func
    (consoleLogBlueBold $"Running {name} {funcName} (undo with cmd/ctrl + z)")
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    dispatch
    <| UpdateModel(fun (model: ModelType.Model) ->
        let newUndoList = appendUndoList model.Sheet.UndoList model.Sheet
        func model
        |> Optic.set (ModelType.sheet_ >-> UndoList_) (newUndoList))
// Run the function on the model
