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
    let maxPairsToBeautify = 10
    let maxOffset = 0
    let maxScale = { x = 0.2; y = 0.2 }
    

/// Gets Symbol Pairs to Beautify.
let getSymsToReorder (model: BusWireT.Model) =

    let keepSymPair (symPair: Symbol * Symbol) =
        match symbolMatch (fst symPair), symbolMatch (snd symPair) with
        | Custom _, Custom _
        | Custom _, And
        | And, Custom _ -> true
        | _ -> false

    getConnSyms model |> List.filter keepSymPair    

let updateBoundingBoxes (wModel: BusWireT.Model) (boundingBoxes: Map<ComponentId, BoundingBox>) = 
    boundingBoxes |> Map.map (fun sId _ -> getBoundingBox wModel.Symbol sId)
  
/// Extracts Symbols to Resize.
let symsToResize (model: BusWireT.Model) = 
    model.Symbol.Symbols
    |> Map.filter (fun id sym ->         
        match sym.Component.Type with
        | Custom _ as customComp -> true
        | _ -> false)
    |> Map.toList
    |> List.map snd

let beautifyComp (helpers: ExternalSmartHelpers) (boundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>) (model: BusWireT.Model) = 
    ((model, boundingBoxes), symsToResize model)
    ||> List.fold (fun (model', bb') sym -> 
        let sym' = model'.Symbol.Symbols[sym.Id]
        let model' = optimiseSymbol model' sym' bb' helpers
        (model', updateBoundingBoxes model' bb')
    )
    |> fst

/// Beautifies Symbol Pairs.
let beautifyPairs (smartHelpers: ExternalSmartHelpers) (model: BusWireT.Model) =

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
let smartBeautify (wModel: BusWireT.Model) (boundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>) (smartHelpers: ExternalSmartHelpers) : BusWireT.Model =

    printfn "Invoked SmartBeautify Function!"

    wModel
    |> beautifyComp smartHelpers boundingBoxes 
    |> beautifyPairs smartHelpers
