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
open BusWireSeparate
open BusWireUpdateHelpers
open SheetBeautifyD1
open SheetBeautifyD2
open SheetBeautifyD3

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
//----------------------------------------------------------------------//
// -------------------------Functions/Helpers-------------------------- //
//----------------------------------------------------------------------//

// Javascript to print bold blue text to console.log for better readability
[<Emit("console.log('%c' + $0, 'font-weight: bold; color: blue;')")>]
let consoleLogBlueBold (message: string) : unit = jsNative

let initSheetModel = DiagramMainView.init().Sheet

// redefining so it will not depend on TestDrawBlockD1 when cleaning up the code
let wire_: Lens<SheetT.Model, BusWireT.Model> =
    Lens.create (fun m -> m.Wire) (fun w m -> { m with Wire = w })

//------------------------------------------------------------------//
// -------------------------Test Circuits-------------------------- //
//------------------------------------------------------------------//
let test1Circuit =
    let andPos = { X = 80.0; Y = 0.0 } + middleOfSheet
    initSheetModel
    |> placeSymbol "G1" (GateN(And, 2)) andPos
    |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
    |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
    |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0))
    |> getOkOrFail

//-------------------------------------------------------------//
// ------------------------- Tests --------------------------- //
//-------------------------------------------------------------//

/// Function that will test RotateScale's reSizeSymbolTopLevel
let testRotateScale1 (model: ModelType.Model) : ModelType.Model =
    // check for at least two symbols, take the first and second, run with reSizeSymbolTopLevel
    match model.Sheet.Wire.Symbol.Symbols.Count >= 2 with
    | true ->
        // let symbolToSize, otherSymbol =
        //     match model.Sheet.Wire.Symbol.Symbols |> Map.toList with
        //     | symbolToSizeInMap :: otherSymbolInMap :: _ -> snd (symbolToSizeInMap), snd (otherSymbolInMap)
        //     | _ -> failwith "Not enough elements in symbol list" // will never happen
        let customSymbolsPairs =
            model.Sheet.Wire.Symbol.Symbols
            |> Map.values
            |> Array.toList
            |> List.filter (fun sym ->
                match sym.Component.Type with
                | Custom _ -> true
                | _ -> false)
            |> List.mapi (fun i sym -> (i, sym))
            |> (fun customSymbols ->
                List.allPairs customSymbols customSymbols
                |> List.filter (fun ((i1, _), (i2, _)) -> i1 < i2))

        let newWire =
            customSymbolsPairs
            |> List.fold
                (fun accWModel (pairOne, pairTwo) ->
                    let symbolToSize = snd pairOne
                    let otherSymbol = snd pairTwo
                    (reSizeSymbolTopLevel accWModel symbolToSize otherSymbol))
                model.Sheet.Wire

        model |> Optic.set (sheet_ >-> wire_) (newWire)

    | false -> model

let testResizeSymbolTopLevelImproved (model: ModelType.Model) : ModelType.Model =
    // check for at least two symbols, take the first and second, run with reSizeSymbolTopLevel
    match model.Sheet.Wire.Symbol.Symbols.Count >= 2 with
    | true ->
        // let symbolToSize, otherSymbol =
        //     match model.Sheet.Wire.Symbol.Symbols |> Map.toList with
        //     | symbolToSizeInMap :: otherSymbolInMap :: _ -> snd (symbolToSizeInMap), snd (otherSymbolInMap)
        //     | _ -> failwith "Not enough elements in symbol list" // will never happen

        let customSymbolsPairs =
            model.Sheet.Wire.Symbol.Symbols
            |> Map.values
            |> Array.toList
            |> List.filter (fun sym ->
                match sym.Component.Type with
                | Custom _ -> true
                | _ -> false)
            |> List.mapi (fun i sym -> (i, sym))
            |> (fun customSymbols ->
                List.allPairs customSymbols customSymbols
                |> List.filter (fun ((i1, _), (i2, _)) -> i1 < i2))
            |> List.sortBy (fun ((_, sym1), (_, sym2)) -> (max sym1.Pos.X sym2.Pos.X))
            |> List.sortBy (fun ((_, sym1), (_, sym2)) -> (max sym1.Pos.Y sym2.Pos.Y))
            |> List.map (fun ((i1, sym1), (i2, sym2)) ->
                match sym1.Pos.X >= sym2.Pos.X with
                | true -> ((i1, sym1), (i2, sym2))
                | false -> ((i2, sym2), (i1, sym1)))
            |> List.map (fun ((i1, sym1), (i2, sym2)) ->
                match sym1.Pos.Y >= sym2.Pos.Y with
                | true -> ((i1, sym1), (i2, sym2))
                | false -> ((i2, sym2), (i1, sym1)))

        // let customSymbolsPairs =
        //     model.Sheet.Wire.Symbol.Symbols
        //     |> Map.values
        //     |> Array.toList
        //     |> List.filter (fun sym ->
        //         match sym.Component.Type with
        //         | Custom _ -> true
        //         | _ -> false)
        //     |> List.mapi (fun i sym -> (i, sym))
        //     |> (fun customSymbols ->
        //         List.allPairs customSymbols customSymbols
        //         |> List.filter (fun ((i1, _), (i2, _)) -> i1 < i2))
        //     |> List.sortBy (fun ((_, sym1), (_, sym2)) -> (max sym1.Pos.X sym2.Pos.X))
        //     |> List.map (fun ((i1, sym1), (i2, sym2)) ->
        //         match sym1.Pos.X >= sym2.Pos.X with
        //         | true -> ((i1, sym1), (i2, sym2))
        //         | false -> ((i2, sym2), (i1, sym1)))

        let newWire =
            customSymbolsPairs
            |> List.fold
                (fun accWModel (pairOne, pairTwo) ->
                    let symbolToSize = snd pairOne
                    let otherSymbol = snd pairTwo
                    (reSizeSymbolImprovedTopLevel accWModel symbolToSize otherSymbol))
                model.Sheet.Wire

        model |> Optic.set (sheet_ >-> wire_) (newWire)

    | false -> model

// let testCustomCompResize (model: ModelType.Model) : ModelType.Model =
//     // check for at least two symbols, take the first and second, run with reSizeSymbolTopLevel
//     match model.Sheet.Wire.Symbol.Symbols.Count >= 2 with
//     | true ->
//         let customSymbols =
//             model.Sheet.Wire.Symbol.Symbols
//             |> Map.values
//             |> Array.toList
//             |> List.filter (fun sym ->
//                 match sym.Component.Type with
//                 | Custom _ -> true
//                 | _ -> false)

//         let newWire =
//             customSymbols
//             |> List.fold
//                 (fun accWModel customSymbol ->
//                     (reSizeCustomComponent accWModel customSymbol
//                 model.Sheet.Wire))

//         model |> Optic.set (sheet_ >-> wire_) (newWire)

//     | false -> model

let testD2 (model: ModelType.Model) : ModelType.Model =
    model
    |> Optic.set sheet_ (sheetOrderFlip model.Sheet)

/// Function to test BeautifySheetHelper's removeWireInvisibleSegments
let testRemoveWireInvisibleSegments (model: ModelType.Model) : ModelType.Model =
    model
    |> Optic.set (sheet_ >-> wire_ >-> wires_) (removeWireInvisibleSegments model.Sheet.Wire.Wires)

// Function to test
let testRemoveWireInvisSegsAndRestoreNubs (model: ModelType.Model) : ModelType.Model =
    let connIdList = model.Sheet.Wire.Wires |> Map.keys |> Array.toList
    let newWireModel =
        model.Sheet.Wire.Wires
        |> removeWireInvisibleSegments
        |> makeAllWiresDraggable
        |> (fun wires -> (Optic.set wires_ wires) model.Sheet.Wire)

    model
    |> Optic.set (sheet_ >-> wire_) (newWireModel)

let testBeautifyFunction (model: ModelType.Model) = model.Sheet

let testLongwires (model: ModelType.Model) =
    model
    |> Optic.set sheet_ (sheetWireLabelSymbol model.Sheet)
//-----------------------------------------------------------------------//
// --------------------Top-Level-Calls for Renderer--------------------- //
//-----------------------------------------------------------------------//

/// Run a sheet-based function on the sheet (Cmd/Ctrl + Shift + 1)
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

/// Run a sheet-based function on the sheet with custom circuit defined in TestSheetFunctions (Cmd/Ctrl + Shift + 2)
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

/// Lists of Tests that can be called from the sheet menu or by Alt/Option + 1-9
let testsToRunFromSheetMenu =
    // First text is the label that will be shown in the menu
    // Replace (fun (model: ModelType.Model) -> model) with the function you want to run
    // All functions should take in a model and return a model.
    // Be sure to modify the sheet with the sheet_ optic before returning. Undos will be handled automatically.
    [ "Test 1: testRotateScale1", testRotateScale1
      "Test 2: testRemoveWireInvisibleSegments", testRemoveWireInvisibleSegments
      "Test 3: testRemoveWireInvisSegsAndRestoreNubs", testRemoveWireInvisSegsAndRestoreNubs
      "Test 4", testD2
      "Test 5: cleanUpAlmostStraightSinglyConnWires", cleanUpAlmostStraightSinglyConnWires
      "Test 6: tryGeneralCleanup", tryGeneralCleanUp
      "Test 7: testResizeSymbolTopLevelImproved", testResizeSymbolTopLevelImproved
      "Test 8: long wires", testLongwires
      "Test 9", (fun (model: ModelType.Model) -> model) ]

/// Run any kind of function on the model.ModelType from the menu (Alt/Option + 1-9)
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
