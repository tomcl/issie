module SheetBeautifyTest

open CommonTypes
open DrawModelType
open Optics
open Operators
open BusWireSeparate
open SheetUpdateHelpers
open SheetBeautifyD2
open SheetBeautify
open SheetBeautifyHelpers2


////////// Parameter/configuration types

/// Sheet evaluation metric hyperparameters
type SheetEvalParamT = {
    WireCrossingsNorm: float;
    VisibleLengthNorm: float;
    RightAnglesNorm: float;
    RetracedSegsNorm: float;
    SegSymIntersectNorm: float;
}

/// Sheet testing configuration.
type SheetTestConfigT =
    | Single
    | Aggregate of int


////////// Adjustable parameters/configuration options

let SheetEvalParam = {
    WireCrossingsNorm = 250_000.0;
    VisibleLengthNorm = 1.0;
    RightAnglesNorm = 15_000.0;
    RetracedSegsNorm = 1_000_000.0;
    SegSymIntersectNorm = 1_500_000.0;
}

let SheetTestConfig = Single // Default config


////////// Helper functions

/// Given a sheet, returns an overall beauty metric.
let evaluateSheet
        (sheet: SheetT.Model)
        : float =

    let wireCrossingsSq = float (SheetBeautifyHelpers2.numRightAngleSegCrossings sheet) ** 2.0
    let visibleLengthSq = float (SheetBeautifyHelpers2.visibleWireLength sheet) ** 2.0
    let rightAnglesSq = float (SheetBeautifyHelpers2.numWireRightAngles sheet) ** 2.0
    let retracedSegsSq = float (numRetracedSegs sheet) ** 2.0
    let segSymIntersectSq = float (numSymsIntersectedBySeg sheet) ** 2.0

    wireCrossingsSq * SheetEvalParam.WireCrossingsNorm +
    visibleLengthSq * SheetEvalParam.VisibleLengthNorm +
    rightAnglesSq * SheetEvalParam.RightAnglesNorm +
    retracedSegsSq * SheetEvalParam.RetracedSegsNorm +
    segSymIntersectSq * SheetEvalParam.SegSymIntersectNorm

let random = System.Random()

// Borrowed from TestDrawBlock
/// Fischer-Yates shuffle algorithm
/// Returns a random shuffled array without changing the input array
let shuffleA arrayToShuffle: 'a array =
    let tmpA = Array.copy arrayToShuffle
    for i = 0 to tmpA.Length - 1 do 
        let r = random.Next(i, tmpA.Length);
        (tmpA[i],tmpA[r])
        |> fun (iv, rv) -> tmpA[r] <- iv;  tmpA[i]  <- rv
    tmpA

/// Shuffles the ports on each edge of a custom component.
let shuffleCustomCompPorts
        (sheet: SheetT.Model)
        (symbol: SymbolT.Symbol)
        : SymbolT.Symbol =
    let edges = [Edge.Left; Edge.Right; Edge.Top; Edge.Bottom]
    let shuffledPorts =
        List.map (fun edge ->
            SheetBeautifyHelpers2.getOrderedPorts sheet.Wire.Symbol symbol edge
            |> List.toArray |> shuffleA |> Array.toList
        ) edges
    (symbol, edges, shuffledPorts)
    |||> List.fold2 SheetBeautifyHelpers2.setOrderedPorts

/// Given a symbol and a sheet, applies a random transformation to the symbol on the sheet
/// for the purposes of testing D2.
let randomTransformSymbol
        (sheet: SheetT.Model)
        (symbol: SymbolT.Symbol)
        : SheetT.Model =
    match symbol.Component.Type with
    | Custom _ -> [shuffleCustomCompPorts sheet symbol, 0.0]
                // Custom component has different behaviour here than in brute-force algorithm.
    | _ -> getSymbolChoices symbol
    |> List.filter (fun (sym, _) -> SheetBeautifyHelpers2.noSymbolIntersections sheet sym)
       // Immediately reject solutions with overlapping symbols.
    |> List.toArray
    |> shuffleA
    |> Array.head
    |> fun (sym, _) -> replaceSymbolAndBB sheet sym

/// Applies a series of random transformations to a sheet for the purposes of testing D2.
let randomTransformSheet
        (sheet: SheetT.Model)
        : SheetT.Model =
    let (compIds, symbols) =
        sheet.Wire.Symbol.Symbols
        |> Map.toList |> List.unzip
    (sheet, symbols)
    ||> List.fold randomTransformSymbol
    |> fun randomSheet ->
        randomSheet.Wire
        |> reRouteWiresFrom compIds
        |> fun wire -> Optic.set SheetT.wire_ wire randomSheet


////////// Top-level

let sheetBeautifyTest
        (sheet: SheetT.Model)
        : SheetT.Model =
    match SheetTestConfig with

    | Single ->

        let randomSheet = randomTransformSheet sheet
        let scoreBefore = evaluateSheet randomSheet
        let optimSheet = beautifySheet randomSheet
        let scoreAfter = evaluateSheet optimSheet
        let scoreDiff = scoreBefore - scoreAfter
        let d3WireBendsBefore = SheetBeautifyHelpers.numOfVisRightAngles randomSheet
        let d3WireBendsAfter = SheetBeautifyHelpers.numOfVisRightAngles sheet
        let d3SymbolIntersectsBefore = SheetBeautifyHelpers.numOfIntersectedSymPairs randomSheet
        let d3SymbolIntersectsAfter = SheetBeautifyHelpers.numOfIntersectedSymPairs sheet
        printf "Score before = %.2e" scoreBefore
        printf "Score after = %.2e" scoreAfter
        printf "Score improvement = %.2e (%.1f%%)" scoreDiff (scoreDiff/scoreBefore * 100.)
        printf $"Wire Bends before = {d3WireBendsBefore}"
        printf $"Wire Bends after = {d3WireBendsAfter}"
        printf $"Symbol Intersects Before = {d3SymbolIntersectsBefore}"
        printf $"Symbol Intersects Before = {d3SymbolIntersectsAfter}"

        let newUndoList =
            appendUndoList sheet.UndoList sheet
            |> fun undoList -> appendUndoList undoList randomSheet
        {optimSheet with UndoList = newUndoList}

    | Aggregate numTests ->

        let testScores =
            ([], [1..numTests])
            ||> List.fold (fun scores _ ->
                let randomSheet = randomTransformSheet sheet
                let scoreBefore = evaluateSheet randomSheet
                let optimSheet = beautifySheet randomSheet
                let scoreAfter = evaluateSheet optimSheet
                scores @ [(scoreBefore, scoreAfter)]
            )
        let scoreDiffAbs =
            testScores
            |> List.map (fun (before, after) -> before - after)
        let scoreDiffRel =
            testScores
            |> List.map (fun (before, after) -> (before - after)/before * 100.)
        printf "Absolute metrics:"
        printf "Min score improvement = %.2e" <| List.min scoreDiffAbs
        printf "Max score improvement = %.2e" <| List.max scoreDiffAbs
        printf "Average score improvement = %.2e" <| List.average scoreDiffAbs
        printf "Relative metrics:"
        printf "Min score improvement = %.1f%%" <| List.min scoreDiffRel
        printf "Max score improvement = %.1f%%" <| List.max scoreDiffRel
        printf "Average score improvement = %.1f%%" <| List.average scoreDiffRel
        sheet
