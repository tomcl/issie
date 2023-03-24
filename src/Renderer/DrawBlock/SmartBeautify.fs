module SmartBeautify

open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Symbol
open Optics
open Optic
open Operators
open SmartHelpers
open SmartPortOrder
open SmartSizeSymbol

module Constants =
    let maxPairsToReorder = 10

/// Optimizes Symbols.
let optimizeSyms
    (helpers: ExternalSmartHelpers)
    (bBoxes: Map<CommonTypes.ComponentId, BoundingBox>)
    (model: BusWireT.Model)
    =

    let updateBBoxes (wModel: BusWireT.Model) (bBoxes: Map<ComponentId, BoundingBox>) =
        bBoxes |> Map.map (fun sId _ -> getBoundingBox wModel.Symbol sId)

    let symsToResize =
        model.Symbol.Symbols
        |> Map.filter (fun id sym ->
            match sym.Component.Type with
            | Custom _ as customComp -> true
            | _ -> false)
        |> Map.toList
        |> List.map snd
        |> Set.ofList

    let folder (currModel, currBB, currSet) _ =
        let sym, newModel =
            Set.toArray currSet
            |> Array.map (fun sym -> sym, optimiseSymbol currModel sym currBB helpers)
            |> Array.minBy (fun (_, m) -> totalLengthOfWires m.Wires)

        newModel, updateBBoxes newModel currBB, Set.remove sym currSet

    let model', _, _ = ((model, bBoxes, symsToResize), symsToResize) ||> Set.fold folder

    model'

/// Reorders Symbol.
let reorderPairs (smartHelpers: ExternalSmartHelpers) (model: BusWireT.Model) =

    // Gets Symbol Pairs to Beautify.
    let getSymsToReorder =
        let keepSymPair (symPair: Symbol * Symbol) =
            match symbolMatch (fst symPair), symbolMatch (snd symPair) with
            | Custom _, Custom _
            | Custom _, And
            | And, Custom _ -> true
            | _ -> false

        getConnSyms model |> List.filter keepSymPair

    // Truncate pairs to reorder based on number of swaps.
    let pairs =
        getSymsToReorder
        |> List.sortByDescending (fun (symA, symB) -> (symReorderPair model symA symB) |> optSwaps |> fst)
        |> List.truncate Constants.maxPairsToReorder

    (model, pairs)
    ||> List.fold (fun model' (symA, symB) ->
        let syms' = model'.Symbol.Symbols
        let symA', symB' = syms'[symA.Id], syms'[symB.Id]
        reOrderPorts model' symA' symB' smartHelpers)

/// Header Function.
let smartBeautify
    (wModel: BusWireT.Model)
    (boundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>)
    (smartHelpers: ExternalSmartHelpers)
    : BusWireT.Model =

    wModel |> reorderPairs smartHelpers |> optimizeSyms smartHelpers boundingBoxes
