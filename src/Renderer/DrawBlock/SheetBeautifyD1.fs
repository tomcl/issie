module SheetBeautifyD1

open CommonTypes
open DrawModelType
open DrawModelType.BusWireT
open Microsoft.FSharp.Collections
open BlockHelpers
open Optics.Optic
open SheetBeautifyHelpers2


/// Retrieves a list of parallel wires from the provided sheet model.
///
/// <param name="sheet">The sheet model.</param>
/// <returns>A list of parallel wires.</returns>
let getParallelWires (sheet: SheetT.Model) =
    sheet.Wire.Wires
    |> Map.values
    |> Seq.toList
    |> List.filter (fun wire -> (List.length (visibleSegments wire.WId sheet false)) >= 3)

/// Retrieves a list of symbols from the provided sheet model.
///
/// <param name="sheet">The sheet model.</param>
/// <returns>A list of symbols.</returns>
let getSymbolsList (sheet: SheetT.Model) =
    sheet.Wire.Symbol.Symbols
    |> Map.values
    |> Seq.toList

/// Checks if the provided symbol does not intersect with any symbol in the given symbol list.
/// <param name="symbol">The symbol to check.</param>
/// <param name="symbolList">The list of symbols to check against.</param>
/// <returns>True if the symbol does not intersect with any symbol in the list; otherwise, false.</returns>
let noSymbolSymbolListIntersection (symbol: SymbolT.Symbol) (symbolList: List<SymbolT.Symbol>) =
    List.exists
        (fun listSymbol ->
            overlap2DBox (Symbol.getSymbolBoundingBox listSymbol) (Symbol.getSymbolBoundingBox symbol)
            && listSymbol.Id <> symbol.Id)
        symbolList
    |> not

/// Straightens wires once in the provided sheet model.
/// <param name="sheet">The sheet model.</param>
/// <returns>The updated sheet model with straightened wires.</returns>
let straightenWiresOnce (sheet: SheetT.Model) =
    let symbolsList = getSymbolsList sheet
    let parallelWires = getParallelWires sheet
    let sourceSymbols =
        List.map (fun wire -> (getSourceSymbol sheet.Wire wire), wire) parallelWires

    let targetSymbols =
        List.map (fun wire -> (getTargetSymbol sheet.Wire wire), wire) parallelWires

    let sourceTargetConnections =
        List.allPairs sourceSymbols targetSymbols
        |> List.filter (fun ((_s1, w1), (_s2, w2)) -> w1 = w2)
        |> List.map (fun ((s1, w), (s2, _w2)) -> (s1, s2, visibleSegments w.WId sheet false))

    let updatedSymbols =
        List.fold
            (fun sheetSymbols (s1, s2, vl: List<XYPos>) ->
                let newSymbol = moveSymbol (vl[1]) s1
                if (noSymbolSymbolListIntersection newSymbol sheetSymbols) then
                    (List.map
                        (fun (sym: SymbolT.Symbol) ->
                            if sym.Id = newSymbol.Id then
                                newSymbol
                            else
                                sym)
                        sheetSymbols)
                else
                    sheetSymbols)
            symbolsList
            sourceTargetConnections
        |> List.map (fun sym -> (sym.Id, sym))
        |> Map.ofList

    sheet
    |> set SheetT.symbols_ updatedSymbols
    |> map SheetT.wire_ (BusWireSeparate.reRouteWiresFrom (updatedSymbols.Keys |> Seq.toList))

/// Straightens wires in the provided sheet model iteratively until no further improvement can be made.
/// <param name="sheet">The sheet model.</param>
/// <returns>The updated sheet model with straightened wires.</returns>
let sheetAlignScale (sheet: SheetT.Model) =

    let rec straightenWires (sheet: SheetT.Model) =
        if (getParallelWires sheet |> List.length) = 0 then
            sheet
        else
            let newSheet = straightenWiresOnce sheet
            if
                (getParallelWires sheet |> List.length)
                <= (getParallelWires newSheet |> List.length)
            then
                sheet
            else
                straightenWires newSheet

    straightenWires sheet
