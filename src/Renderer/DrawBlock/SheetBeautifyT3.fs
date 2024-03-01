module SheetBeautify

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
open TestDrawBlock

/// constants used by SheetBeautify
module Constants =
    () // dummy to make skeleton type check - remove when other content exists

// ------------------------------------ Team work ------------------------------------------
(* 
    This part of the code aims to use the Figure 1 functions to contribute to the Team phase work. 
    See https://github.com/dyu18/hlp24-project-issie-team7/tree/indiv-az1821/README-Indiv-notes.md for more documentation. 
*)

module T3 =
    // ------------------------------------ Assertions -----------------------------------------
    /// Some assert functions to use for the testing of D3 task
    /// 1. Wire label placement with different wire lengths.
    /// 2. Wire label removal when under the threshold.
    /// 3. Wire label naming based on component and port names.
    /// 4. Wire label positioning adjustment to avoid overlaps.
    /// (fail all tests is copied from TestDrawBlocks)
    module Asserts = 

        /// Ignore sheet and fail on the specified sample, useful for displaying a given sample
        let failOnSampleNumber (sampleToFail : int) (sample: int) _sheet =
            if sampleToFail = sample then
                Some $"Failing forced on Sample {sampleToFail}."
            else
                None

        /// Fails all tests: useful to show in sequence all the sheets generated in a test
        let failOnAllTests (sample: int) _ =
            Some $"Sample {sample}"

        // Fail when wire labels are not added or removed correctly
        let failOnWireLabels (sample: int) (sheet: SheetT.Model) =
            let originalWireCount = Map.countBy (fun _ wire -> wire |> WireT.wireOf).Wires
            let updatedSheet = sheetWireLabelSymbol sheet // Assuming sheetWireLabelSymbol modifies the sheet
            let updatedWireCount = Map.countBy (fun _ wire -> wire |> WireT.wireOf).Wires

            if originalWireCount <> updatedWireCount then
                Some $"Wire labels were not added or removed correctly in Sample {sample}."
            else
                None

        /// whether wire labels are correctly placed based on wire length threshold
        let assertWireLabelPlacement (sample: int) (sheet: SheetT.Model) =
            let wireLabels = Map.toSeq sheet.Wire.Symbol.Symbols |> Seq.filter (fun (_, sym) -> sym.Component.Type = IOLabel)
            
            let misplacedLabels =
                wireLabels
                |> Seq.filter (fun (_, label) ->
                    let wireLength = computeWireLength label
                    let expectedPlacement = if wireLength > Constants.wireLabelThreshold then WireLabelPlacement.InPlace else WireLabelPlacement.Removed
                    match expectedPlacement with
                    | WireLabelPlacement.InPlace -> label.Pos = expectedPositionForLabel label
                    | WireLabelPlacement.Removed -> not (Map.exists (fun _ wire -> wire.Label = label.Component.Label) sheet.Wire.Wires)
                )
            match Seq.isEmpty misplacedLabels with
            | true -> None
            | false -> Some $"Wire labels are misplaced in Sample {sample}."

        /// whether wire labels are removed when under the threshold
        let assertWireLabelRemoval (sample: int) (sheet: SheetT.Model) =
            let wireLabels = Map.toSeq sheet.Wire.Symbol.Symbols |> Seq.filter (fun (_, sym) -> sym.Component.Type = IOLabel)
            
            let removedLabels =
                wireLabels
                |> Seq.filter (fun (_, label) ->
                    let wireLength = computeWireLength label
                    wireLength <= Constants.wireLabelThreshold && not (Map.exists (fun _ wire -> wire.Label = label.Component.Label) sheet.Wire.Wires)
                )
            match Seq.isEmpty removedLabels with
            | true -> None
            | false -> Some $"Wire labels are not removed correctly in Sample {sample}."

        /// whether wire labels are named correctly based on component and port names
        let assertWireLabelNaming (sample: int) (sheet: SheetT.Model) =
            let wireLabels = Map.toSeq sheet.Wire.Symbol.Symbols |> Seq.filter (fun (_, sym) -> sym.Component.Type = IOLabel)
            
            let incorrectNames =
                wireLabels
                |> Seq.filter (fun (_, label) ->
                    let expectedName = generateWireLabelName label
                    label.Component.Label <> expectedName
                )
            match Seq.isEmpty incorrectNames with
            | true -> None
            | false -> Some $"Wire labels have incorrect names in Sample {sample}."

        /// whether wire labels are correctly positioned to avoid overlaps with symbols
        let assertWireLabelPositionAdjustment (sample: int) (sheet: SheetT.Model) =
            let wireLabels = Map.toSeq sheet.Wire.Symbol.Symbols |> Seq.filter (fun (_, sym) -> sym.Component.Type = IOLabel)
            let symbols = Map.toSeq sheet.Wire.Symbol.Symbols |> Seq.filter (fun (_, sym) -> sym.Component.Type <> IOLabel)
            let misplacedLabels =
                wireLabels
                |> Seq.filter (fun (_, label) ->
                    symbols |> Seq.exists (fun (_, symbol) -> overlap2DBox (getSymBoundingBox symbol) (getSymBoundingBox label))
                )
            match Seq.isEmpty misplacedLabels with
            | true -> None
            | false -> Some $"Wire labels are misplaced due to overlap with symbols in Sample {sample}."

    // --------------------------- Test of sheetWireLabelSymbol function -----------------------
    /// this is a similar test menu as tick 3
    let testSheetWireLabelSymbol testNum firstSample dispatch = 
        runTestOnSheets
            "Test sheetWireLabelSymbol function"
            firstSample
            sampleSheet // Replace sampleSheet with actual sheet
            sheetWireLabelSymbol 
            Asserts.failOnWireLabels
            dispatch
        |> recordPositionInTest testNum dispatch

    let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> unit)) list =
        [
            "SheetWireLabelSymbolTest", testSheetWireLabelSymbol
            // more test cases can be added here
        ]

    let testMenuFunc (testIndex: int) (dispatch: Dispatch<Msg>) (model: Model) =
        match name, model.DrawBlockTestState with
        | "SheetWireLabelSymbolTest", _ ->
            testSheetWireLabelSymbol testIndex 0 dispatch
        | _ ->
            printfn "Unknown test."

    // run all D3 tests
    let runD3Tests (firstSample: int) (dispatch: Dispatch<Msg>) (model: Model) =
        let testFunctions = [
            testWireLabelPlacement
        ]
        
        testFunctions
        |> List.mapi (fun i testFunc ->
            printfn "Running D3 test %d..." (i + 1)
            let result = testFunc firstSample model.Sheet
            match result with
            | Some errMsg ->
                printfn "Test %d failed: %s" (i + 1) errMsg
                false
            | None ->
                printfn "Test %d passed" (i + 1)
                true)
        |> List.forall id // Check if all tests passed

