module SheetBeautifyD4

open SheetBeautifyD1
open SheetBeautifyD2
open SheetBeautifyD3

open DrawModelType
open Optics
open Helpers

let rerouteAllWires (sheet: SheetT.Model) : SheetT.Model=
    let comps = mapKeys sheet.Wire.Symbol.Symbols |> Array.toList
    let newWModel = List.fold (BusWireSeparate.routeAndSeparateSymbolWires) sheet.Wire comps
    Optic.set (SheetT.wire_) newWModel sheet

/// Basic combination of beautify functions
let beautifySheetBasic : SheetT.Model -> SheetT.Model =
    // sheetOrderFlip
    sheetAlignScale
    >> rerouteAllWires
    >> autoGenerateWireLabels

// Applies each beautify function to the sheet.
// Then evaluates which looks best.
// Keep applying till no improvement or maxDepth.
/// Optimal combination of beautify functions
let beautifySheetOptimal 
    (beautifyFuncs: (SheetT.Model -> SheetT.Model) list)
    (maxDepth: int) 
    (scoreFunc : SheetT.Model -> float)
    (sheet: SheetT.Model)
    : SheetT.Model =

    let applyFuncAndScore (sheet: SheetT.Model) (func: (SheetT.Model -> SheetT.Model)) : SheetT.Model * float =
        let sheet' = func sheet
        let score = scoreFunc sheet'
        (sheet', score)

    let rec findBestFunc (sheet: SheetT.Model) (prevScore: float) depth =
        if depth >= maxDepth then sheet else
        beautifyFuncs
        |> List.map (applyFuncAndScore sheet)
        |> List.maxBy (snd)
        |> function
            | sheet', score when score > prevScore -> findBestFunc sheet' score (depth+1)
            | _ -> sheet

    findBestFunc sheet (scoreFunc sheet) 0
