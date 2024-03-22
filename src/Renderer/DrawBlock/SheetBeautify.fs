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
open SheetBeautifyD1
open SheetBeautifyD2
open SheetBeautifyD3
open Optics

/// For best performance
/// each elementary beautifier is called 3 times
let sheetBeautify (model: SheetT.Model) = 
    let elementaryBeautify (model: SheetT.Model) = 
        model
        |> optimizeModelILS    // D2
        |> sheetWireLabelSymbol     // D3
        |> alignSinglyConnectedComponents   // D1
    model
    |> elementaryBeautify
    |> elementaryBeautify
    |> elementaryBeautify


    