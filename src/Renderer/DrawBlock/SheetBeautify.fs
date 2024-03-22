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
open SheetBeautifyD1
open SheetBeautifyD3
/// constants used by SheetBeautify
module Constants =
    () // dummy to make skeleton type check - remove when other content exists

let sheetAlignScale (sheet:SheetT.Model) = SheetBeautifyD1.Beautify.sheetAlignScale sheet
    
(*let sheetOrderFlip (sheet:SheetT.Model) = 
    permuteMuxState sheet*)

let sheetWireLabelSymbol (sheet:SheetT.Model) = 
    removeComplexWires sheet

let sheetOrderFlip (sheet:SheetT.Model) = 
    sheet

let beautifySheet (sheet:SheetT.Model) = 
    sheet
    |> sheetAlignScale
    |> sheetOrderFlip
    |> sheetWireLabelSymbol



    
