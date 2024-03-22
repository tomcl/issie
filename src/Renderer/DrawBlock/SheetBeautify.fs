module SheetBeautify

//-----------------------------------------------------------------------------------------//
//----------Module for top-level beautify functions making (mostly) whole-sheet changes----//
//-----------------------------------------------------------------------------------------//

(*
Whole sheet functions are normally applied to whole sheet. In many cases a feature could be to
apply them to currently selected wires or components. That provides users control over what gets beautified.
Ideal beautify code will never make things worse so can be applied to whole sheet always.

Other relevant modules:
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
open SheetBeautifyD2
open SheetBeautifyD3



let beautifySheet (initialModel : SheetT.Model) : SheetT.Model =

    // Adding current model to the undo/redo list
    let newUndoList, newRedoList = SheetUpdateHelpers.appendUndoList initialModel.UndoList initialModel, initialModel.RedoList

    let modelToUpdate = {initialModel with UndoList = newUndoList; RedoList = newRedoList}

    // Calling all the beautify functions in order: D2, D1 and D3
    modelToUpdate
    |> SheetBeautifyD2.sheetOrderFlip
    |> SheetBeautifyD1.sheetAlignScale
    |> SheetBeautifyD3.sheetWireLabelSymbol


/// constants used by SheetBeautify
module Constants =
    () // dummy to make skeleton type check - remove when other content exists




