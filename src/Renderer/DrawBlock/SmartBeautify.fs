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

module Constants =
    let maxPairsToBeautify = 10

/// Gets Symbol Pairs to Beautify.
let getSymsToReorder (model: BusWireT.Model) =

    let keepSymPair (symPair: Symbol * Symbol) =
        match symbolMatch (fst symPair), symbolMatch (snd symPair) with
        | Custom _, Custom _
        | Custom _, And
        | And, Custom _ -> true
        | _ -> false

    getConnSyms model |> List.filter keepSymPair

/// Beautifies Symbol Pairs.
let beautifyPairs (model: BusWireT.Model) (smartHelpers: ExternalSmartHelpers) =

    let pairs =
        getSymsToReorder model
        |> List.sortByDescending (fun (symA, symB) -> (symReorderPair model symA symB) |> optSwaps |> fst)
        |> List.truncate Constants.maxPairsToBeautify

    (model, pairs)
    ||> List.fold (fun model' (symA, symB) ->
        let syms' = model'.Symbol.Symbols
        let symA', symB' = syms'[symA.Id], syms'[symB.Id]
        reOrderPorts model' symA' symB' smartHelpers)

/// Header Function.
let smartBeautify (wModel: BusWireT.Model) (smartHelpers: ExternalSmartHelpers) : BusWireT.Model =

    printfn "Invoked SmartBeautify Function!"

    beautifyPairs wModel smartHelpers
