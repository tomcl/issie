module SheetBeautifyD1

open EEExtensions
open Optics
open Optics.Operators
open Helpers
open CommonTypes
open DrawModelType
open BlockHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWireUpdateHelpers
open Optics.Optic
open Operators
open Symbol

(*
    Contributor : ya921 - Yazan Ayyoub
    This module contains the implementation of a starter for sheet beautification algorithm D1.
    The algorithm is designed to update the positions of singly connected symbols with parallel constrained wires connected.
    The algorithm is designed to be scalable and can be extended to include more complex cases, helper functions are provided to facilitate this.
*)

///<summary> Determines if a wire is parallel and can be straightened based on the length of its non zero absolute segments. </summary>
/// <param name="wire">The wire to be checked for parallelism.</param>
/// <returns>True if the wire is parallel, false otherwise.</returns>
let isParallel (wire: Wire) : bool = (getNonZeroAbsSegments wire).Length = 5

/// <summary>Filters the list of wires in a sheet model to only include parallel wires.</summary>
/// <param name="sheetModel">The sheet model to be filtered.</param>
/// <returns>A list of parallel wires.</returns>
let getParallelWires (sheetModel: SheetT.Model) : Wire list =
    sheetModel.Wire
    |> getWireList
    |> List.filter isParallel

/// <summary>Retrieves the source and target symbols of a wire in a sheet model and appends wire segments for further processing and scalability.</summary>
/// <param name="sheet">The sheet model to be processed.</param>
/// <param name="wire">The wire to be processed.</param>
/// <returns>The source and target symbols of the wire and the wire segments.</returns>
let getSymbols (sheet: SheetT.Model) (wire: Wire) : Symbol * Symbol * ASegment list =
    let sourceSymbol = getSourceSymbol sheet.Wire wire
    let targetSymbol = getTargetSymbol sheet.Wire wire
    sourceSymbol, targetSymbol, getNonZeroAbsSegments wire

/// <summary>Retrieves a list of singly connected symbols in a sheet model.</summary>
/// <param name="sheet">The sheet model to be processed.</param>
/// <returns>A list of singly connected symbols.</returns>
let getSinglyConnectedSymbols (sheet: SheetT.Model) : Symbol list =
    let wires = getWireList sheet.Wire
    let symbols =
        wires
        |> List.map (getSymbols sheet)
        |> List.collect (fun (a, b, _) -> [ a; b ])
    symbols
    |> List.groupBy id
    |> List.filter (fun (_, l) -> l.Length = 1)
    |> List.map fst

/// <summary>Determines if a symbol is in a list of symbols.</summary>
/// <param name="symbol">The symbol to be checked.</param>
/// <param name="symbols">The list of symbols to be checked against.</param>
/// <returns>True if the symbol is in the list, false otherwise.</returns>
let isSinglyConnected (symbol: Symbol) (symbols: Symbol list) : bool = symbols |> List.exists ((=) symbol)

/// <summary>Updates the position of a symbol based on a list of segments (parallel wire) and returns the updated symbol along with its component ID for further processing.</summary>
/// <param name="symbol">The symbol to be updated.</param>
/// <param name="segments">The list of segments to be used for updating the symbol position.</param>
/// <returns>The component ID and the updated symbol.</returns>
let updateSymbolPosition (symbol: Symbol) (segments: ASegment list) : ComponentId * Symbol =
    let changingDirection = segments.[2]
    let updatedSymbol = setSymbolPos symbol (symbol.Pos + (changingDirection.Start - changingDirection.End))
    symbol.Id, updatedSymbol

/// <summary>Determines if two symbols intersect.</summary>
/// <param name="a">The first symbol to be checked.</param>
/// <param name="b">The second symbol to be checked.</param>
/// <returns>True if the symbols intersect, false otherwise.</returns>
let boundingBoxesIntersect (a: Symbol) (b: Symbol) : bool = overlap2DBox (getSymbolBB a) (getSymbolBB a)

/// <summary>Beautifies a sheet model by updating the positions of singly connected symmbols with parallel constrained wires connected.</summary>
/// <param name="sheet">The sheet model to be beautified.</param>
/// <returns>The beautified sheet model.</returns>
let beautifySheetModelStarter (sheet: SheetT.Model) : SheetT.Model =
    let singlyConnectedSymbols = getSinglyConnectedSymbols sheet
    let updatedComponents =
        getParallelWires sheet
        |> List.map (fun wire ->
            let (a, b, c) = getSymbols sheet wire
            let symbol, segments =
                if isSinglyConnected a singlyConnectedSymbols then
                    a, c
                else
                    b, c
            updateSymbolPosition symbol segments)
    { sheet with
        Wire =
            { sheet.Wire with
                Symbol = { sheet.Wire.Symbol with Symbols = Map.ofList updatedComponents } } }


// todo: expand on beautification algorithm D1 and scale