module SheetBeautifyT3

//-----------------------------------------------------------------------------------------//
//----------Module for top-level beautify functions making (mostly) whole-sheet changes----//
//-----------------------------------------------------------------------------------------//

(*
Whole sheet functions are normally applied to whole sheet. In many cases a feature could be to
apply them to currently selected wires or components. That provides users control over what gets beautified.
Ideal beautify code will never make things worse so can be applied to whole sheet always.

Otehr relevant modules:
SheetBeautifyHelpers (helpers)
Generatedata (used to make randomized tests)
TestDrawBlock (used to test the code written here).

*)

// open modules likely to be used
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open SheetBeautifyHelpers
open Optics
open Optics.Operators
open GenerateData
open Elmish
open EEExtensions
open Helpers
open BlockHelpers
open ModelType
open Sheet.SheetInterface
open Symbol
open SymbolUpdate
open SymbolResizeHelpers
open BusWidthInferer
open BusWireSeparate
open RotateScale
open CanvasStateAnalyser

/// constants used by SheetBeautify
module Constants = 
    // () // dummy to make skeleton type check - remove when other content exists
    let wireLabelThreshold = 100.0 

// ------------------------------------ Team work ------------------------------------------
(* 
    This part of the code aims to test the correct usage of labels as described in D3. 
    See https://github.com/dyu18/hlp24-project-issie-team7/tree/indiv-az1821/README-Indiv-notes.md for more documentation. 
*)

/// dummy function to be tested (to avoid error for now)
let sheetWireLabelSymbol (model : SheetT.Model) = 
    Ok (model) // returns the same model, no change in labels

module T3 =
    open TestDrawBlock.TestLib
    open TestDrawBlock.HLPTick3
    open TestDrawBlock.HLPTick3.Asserts
    open TestDrawBlock.HLPTick3.Builder
    open TestDrawBlock.HLPTick3.Tests

    // More helper functions
    let getWireAndPort (sym : Symbol) (model : SheetT.Model) =
        let portOption =
            mapValues model.Wire.Symbol.Ports
            |> List.tryFind (fun port -> port.HostId = sym.Component.Id) // Get ports on the wire label
        match portOption with
        | Some port ->
            let wireOption =
                mapValues model.Wire.Wires
                |> List.tryFind (fun wire -> wire.OutputPort = OutputPortId port.Id || wire.InputPort = InputPortId port.Id)
            match wireOption with
            | Some wire -> Some (sym, wire, port.Id)
            | None -> None
        | None -> None

    /// Simply count number of wire/labels
    let countPorts (model: SheetT.Model) =
        model.Wire.Wires |> Map.count

    // -------------------------- Test data generation -------------------------------------------

    let makeTestCircuit (andPos:XYPos) =
        initSheetModel
        |> placeSymbol "MUX1" Mux2  andPos
        |> Result.bind (placeSymbol "I0" IOLabel (andPos+{X=60.;Y=60.}))
        |> Result.bind (placeSymbol "FF1" DFF (middleOfSheet-{X=0.;Y=100.}))
        |> Result.bind (placeSymbol "FF2" DFF (middleOfSheet))
        |> Result.bind (placeSymbol "FF3" DFF (middleOfSheet+{X=0.;Y=100.}))
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "I0" 0))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "FF2" 0))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "FF3" 0))
        |> Result.bind sheetWireLabelSymbol
        |> getOkOrFail

    // ------------------------------------ Assertions -----------------------------------------
    /// Assert functions to use for the testing of D3 task
    /// The dataset used in this test must pass all the assertion in TestDrawBlocks.fs 
    /// 0. No port is connected to more than 1 label
    /// 1. Wire label placement when wire lengths > threshold.
    /// 2. Wire label removal when wire lengths < threshold.
    /// 3. Wire label correct connection between component ports. 
    /// 4. Wire label positioning adjustment to avoid overlaps.
    module Asserts = 

        /// Fails on test number: show certain test case
        let failOnSampleNumber (sampleToFail : int) (sample: int) _sheet =
            if sampleToFail = sample then
                Some $"Failing forced on Sample {sampleToFail}."
            else
                None

        /// Fails all tests: show all test cases
        let failOnAllTests (sample: int) _ =
            Some $"Sample {sample}"

        /// 0. Each port has no more than 1 label
        ///    and if it has a label, no wire is connected to it
        ///   (for now this is abandoned, could be changed later)
        let failOnWireLabels (model : SheetT.Model) =
            let portLabelCounts =
                model.Wire.Symbol.Ports
                |> Map.toList
                |> List.map (fun (_, port) ->
                    let labelCount =
                        model.Wire.Symbol.Symbols
                        |> Map.filter (fun _ sym -> sym.Component.Type = IOLabel)
                        |> Map.filter (fun _ sym ->
                            match getWireAndPort sym model with
                            | Some (_, _, portId) -> portId = port.Id
                            | None -> false)
                        |> Map.count
                    port.Id, labelCount)

            let failedPorts =
                portLabelCounts
                |> List.filter (fun (_, countLabel) -> countLabel > 1)

            if List.isEmpty failedPorts then
                printfn "Each port has at most 1 label and no wire connected to it."
            else
                printfn "The following ports violate the constraint of having at most 1 label and no wire connected to it:"
                for portId, countLabel in failedPorts do
                    printfn "Port: %s, Label Count: %d" portId countLabel

        /// 1. Check wire -> label placement
        let failOnLabelNotPlaced (model: SheetT.Model) =
            let wiresAboveThreshold =
                mapValues model.Wire.Wires
                |> List.filter (fun wire -> getWireLength wire >= Constants.wireLabelThreshold)

            if List.isEmpty wiresAboveThreshold then
                printfn "All wires above threshold have been replaced by labels."
            else
                printfn "The following wires are still above the threshold length and haven't been replaced by labels:"
                for wire in wiresAboveThreshold do
                    printfn "Wire: %A, Length: %f" wire.WId (getWireLength wire)

        /// 2. Check label -> wire removal
        let failOnLabelNotRemoved (model: SheetT.Model) =
            let labelsWithWires =
                mapValues model.Wire.Symbol.Symbols
                |> List.filter (fun sym -> sym.Component.Type = IOLabel)
                |> List.filter (fun sym ->
                    match getWireAndPort sym model with
                    | Some (_, wire, _) -> getWireLength wire < Constants.wireLabelThreshold
                    | None -> false)

            if List.isEmpty labelsWithWires then
                printfn "All labels corresponding to wires below threshold length have been removed."
            else
                printfn "The following labels still correspond to wires below the threshold length and haven't been removed:"
                for sym in labelsWithWires do
                    printfn "Label: %s" sym.Component.Label

        /// 3. Check port are connected correctly
        /// (the model should be the same before and after sheet beautify)
        let failOnWrongConnection (model : SheetT.Model) = 
            failwithf "Not Implemented"

        // /// whether wire labels are correctly positioned to avoid overlaps with symbols
        // let assertWireLabelPositionAdjustment (sample: int) (sheet: SheetT.Model) =
        //     let wireLabels = Map.toSeq sheet.Wire.Symbol.Symbols |> Seq.filter (fun (_, sym) -> sym.Component.Type = IOLabel)
        //     let symbols = Map.toSeq sheet.Wire.Symbol.Symbols |> Seq.filter (fun (_, sym) -> sym.Component.Type <> IOLabel)
        //     let misplacedLabels =
        //         wireLabels
        //         |> Seq.filter (fun (_, label) ->
        //             symbols |> Seq.exists (fun (_, symbol) -> overlap2DBox (getSymBoundingBox symbol) (getSymBoundingBox label))
        //         )
        //     match Seq.isEmpty misplacedLabels with
        //     | true -> None
        //     | false -> Some $"Wire labels are misplaced due to overlap with symbols in Sample {sample}."

    // ----------------------------------- Test driver -----------------------------------
    /// this is a similar test menu as tick 3
    module Tests = 

        let testWireToLabel testNum firstSample dispatch = 
            // runTestOnSheets
            //     "Test sheetWireLabelSymbol function"
            //     firstSample
            //     sampleSheet // Replace sampleSheet with actual sheet
            //     sheetWireLabelSymbol 
            //     Asserts.failOnWireLabels
            //     dispatch
            // |> recordPositionInTest testNum dispatch
            failwithf "Not Implemented"

        // let testLabelToWire = 
        //     failwithf "Not Implemented"


        let testSheetWireLabelSymbol testNum firstSample dispatch = 
            // runTestOnSheets
            //     "Test sheetWireLabelSymbol function"
            //     firstSample
            //     sampleSheet // Replace sampleSheet with actual sheet
            //     sheetWireLabelSymbol 
            //     Asserts.failOnWireLabels
            //     dispatch
            // |> recordPositionInTest testNum dispatch
            failwithf "Not Implemented"

        let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
            [
                "test 0 : No port connected to more than 1 label", testWireToLabel
                "test 1 : Wire to label", testWireToLabel
                // "test 2 : Label to wire", testLabelToWire
                "(Dummy Test : SheetWireLabelSymbol)", testSheetWireLabelSymbol
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

