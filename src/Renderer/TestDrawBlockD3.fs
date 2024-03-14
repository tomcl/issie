module TestDrawBlockD3
open GenerateData
open Elmish

(******************************************************************************************
   This submodule contains a set of functions that enable random data generation
   for property-based testing of Draw Block wire routing functions.
   basic idea.
   1. Generate, in various ways, random circuit layouts
   2. For each layout apply smartautoroute to regenerate all wires
   3. Apply check functions to see if the resulting wire routing obeys "good layout" rules.
   4. Output any layouts with anomalous wire routing
*******************************************************************************************)

open TestDrawBlock
open TestLib
open TestDrawBlock.HLPTick3
open TestDrawBlock.HLPTick3.Asserts
open TestDrawBlock.HLPTick3.Builder
open TestDrawBlock.HLPTick3.Tests

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
open SheetBeautifyHelpers

//------------------------------------------------------------------------------------------------------------------------//
//------------------------------functions to build issue schematics programmatically--------------------------------------//
//------------------------------------------------------------------------------------------------------------------------//

module Builder =
    let segsConnectedToSym (sheet: SheetT.Model) (sym: SymbolT.Symbol) =
        let countVisSegsInWire (wire: BusWireT.Wire) =
            visibleSegments wire.WId sheet
            |> List.length

        let symPortIds = 
            sym.PortMaps.Order
            |> mapValues
            |> Array.toList
            |> List.concat
        
        sheet.Wire.Wires
        |> Map.filter (fun _ wire -> List.contains (string wire.InputPort) symPortIds || List.contains (string wire.OutputPort) symPortIds)
        |> Map.toList
        |> List.map (fun (_, wire) -> wire)
        |> List.map countVisSegsInWire
        |> List.sum


    /// Print info needed for reverse circuit generation from sheet
    let printCircuitBuild (sheet: SheetT.Model) =
        failwithf "Not implemented"


//--------------------------------------------------------------------------------------------------//
//----------------------------------------Example Test Circuits using Gen<'a> samples---------------//
//--------------------------------------------------------------------------------------------------//

open Builder

/// small offsets in X&Y axis
let offsetXY =
    let offsetX = randomFloat -2. 0.1 2.
    let offsetY = randomFloat -2. 0.1 2.
    (offsetX, offsetY)
    ||> product (fun (x: float) (y: float) -> {X=x; Y=y})

/// Returns the position in respect to the centre of the sheet
let pos x y = 
    middleOfSheet + {X=float x; Y=float y}

let makeTest1Circuit (x:XYPos)=
    let Mux1Pos = middleOfSheet + {X=300. ; Y=0.}
    let Mux2Pos = middleOfSheet + {X=300. ; Y=300.}
    initSheetModel
    |> placeSymbol "DM1" Demux4 middleOfSheet
    |> Result.bind(placeSymbol "MUX1" Mux4 Mux1Pos)
    |> Result.bind (placeWire (portOf "DM1" 0) (portOf "MUX1" 0))
    |> Result.bind (placeWire (portOf "DM1" 1) (portOf "MUX1" 1))
    |> Result.bind (placeWire (portOf "DM1" 2) (portOf "MUX1" 2))
    |> Result.bind (placeWire (portOf "DM1" 3) (portOf "MUX1" 3))
    |> Result.bind(placeSymbol "MUX2" Mux4 Mux2Pos)
    |> Result.bind (placeWire (portOf "DM1" 0) (portOf "MUX2" 0))
    |> Result.bind (placeWire (portOf "DM1" 1) (portOf "MUX2" 1))
    |> Result.bind (placeWire (portOf "DM1" 2) (portOf "MUX2" 2))
    |> Result.bind (placeWire (portOf "DM1" 3) (portOf "MUX2" 3))
    |> getOkOrFail


//------------------------------------------------------------------------------------------------//
//-------------------------Example assertions used to test sheets---------------------------------//
//------------------------------------------------------------------------------------------------//


module Asserts =
    let failOnAllTests (sample: int) _ =
            Some <| $"Sample {sample}"
//---------------------------------------------------------------------------------------//
//-----------------------------Demo tests on Draw Block code-----------------------------//
//---------------------------------------------------------------------------------------//

module Tests =
    
    let D3Test1 testNum firstSample dispatch =
        runTestOnSheets
            "two custom components with random offset: fail all tests"
            firstSample
            offsetXY
            makeTest1Circuit
            Asserts.failOnAllTests
            dispatch
        |> recordPositionInTest testNum dispatch

    let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
        // Change names and test functions as required
        // delete unused tests from list
        [
            "Test1", D3Test1 // example
            "Test2", D3Test1 // example
        ]
    
    let nextError (testName, testFunc) firstSampleToTest dispatch =
            let testNum =
                testsToRunFromSheetMenu
                |> List.tryFindIndex (fun (name,_) -> name = testName)
                |> Option.defaultValue 0
            testFunc testNum firstSampleToTest dispatch
    
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