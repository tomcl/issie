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
    let maxPairsToBeautify = 2

/// Gets Symbol Pairs to Beautify.
let getSymsToReorder (model: BusWireT.Model) =
    let keepSym (sym: Symbol) =
        match symbolMatch sym with
        | And
        | Custom _ -> true
        | _ -> false

    let keepSymPair (symPair: Symbol * Symbol) =
        match symbolMatch (fst symPair), symbolMatch (snd symPair) with
        | And, And -> false
        | _ -> true

    // Source: https://stackoverflow.com/questions/1222185/most-elegant-combinations-of-elements-in-f
    let rec getCombs (n: int) (syms: Symbol list) =
        match n, syms with
        | 0, _ -> [ [] ]
        | _, [] -> []
        | x, hd :: tl -> List.map ((@) [ hd ]) (getCombs (x - 1) tl) @ getCombs x tl

    let syms =
        model.Symbol.Symbols
        |> Map.filter (fun _ sym -> keepSym sym)
        |> Map.values
        |> Seq.toList

    getCombs 2 syms
    |> List.map (fun syms -> (syms[0], syms[1]))
    |> List.filter keepSymPair

/// Beautifies Symbol Pairs.
let beautifyPairs (model: BusWireT.Model) (smartHelpers: ExternalSmartHelpers) =

    let pairs =
        getSymsToReorder model
        |> List.sortByDescending (fun (symA, symB) -> (getSymReorderPair model symA symB) |> getOptSwaps |> fst)
        |> List.truncate Constants.maxPairsToBeautify

    (model, pairs)
    ||> List.fold (fun model' (symA, symB) -> reOrderPorts model' symA symB smartHelpers)

/// Header Function.
let smartBeautify (wModel: BusWireT.Model) (smartHelpers: ExternalSmartHelpers) : BusWireT.Model =

    printfn "Invoked SmartBeautify Function!"

    beautifyPairs wModel smartHelpers
