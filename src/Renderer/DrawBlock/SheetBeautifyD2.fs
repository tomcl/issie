module SheetBeautifyD2
// open modules likely to be used
open SheetBeautifyHelpers
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open BlockHelpers
open Optics
open Helpers
open Symbol

/// Permutes gate inputs
let rec permute list =
    match list with
    | [] -> [ [] ]
    | head :: tail ->
        let rec insertEverywhere x list =
            match list with
            | [] -> [ [ x ] ]
            | head :: tail ->
                (x :: list)
                :: [ for rest in insertEverywhere x tail -> head :: rest ]
        let tailPerms = permute tail
        tailPerms |> List.collect (insertEverywhere head)

/// Applies all permutations of gate inputs and MUX inputs and evaluate the transformations to output the best model
let evaluateTransformations (model: Model) : Model =
    let originalCrossings = countRightAngleCrossings model
    let originalRightAngles = countVisibleWireRightAngles model

    // Extracts gates from the model
    let gates =
        model.Wire.Symbol.Symbols
        |> Map.filter (fun _ symbol ->
            match symbol.Component.Type with
            | GateN _ -> true
            | _ -> false)
        |> Map.toList

    // Extracts MUXes from the model
    let muxes =
        model.Wire.Symbol.Symbols
        |> Map.filter (fun _ symbol ->
            match symbol.Component.Type with
            | Mux2 -> true
            | _ -> false)
        |> Map.toList

    // Generates all permutations for gates
    let gatePermutations =
        gates
        |> List.collect (fun (id, symbol) ->
            let inputPerms = permute symbol.Component.InputPorts
            inputPerms
            |> List.map (fun perm -> (id, { symbol with Component = { symbol.Component with InputPorts = perm } })))

    // Generates all flipped MUX models
    let muxPermutations =
        muxes
        |> List.map (fun (id, symbol) ->
            match symbol.ReversedInputPorts with
            | Some state -> (id, Optic.set reverseInputPortsMUX2_ (not state) symbol)
            | None -> failwithf "Reversed Input Ports not found for MUX %A" id)

    // Applies permutations and flip transformations
    let models =
        gatePermutations @ muxPermutations
        |> List.map (fun (id, symbol) ->
            { model with
                Wire =
                    { model.Wire with
                        Symbol =
                            { model.Wire.Symbol with
                                Symbols = model.Wire.Symbol.Symbols |> Map.add id symbol } } })

    // Evaluates each model and select the best one
    let bestModel =
        models
        |> List.minBy (fun m ->
            let crossings = countRightAngleCrossings m
            let rightAngles = countVisibleWireRightAngles m
            if
                crossings < originalCrossings
                && rightAngles <= originalRightAngles
            then
                crossings, -rightAngles
            else
                failwithf "cannot find best model")

    bestModel
/// Flips all MUXes on a sheet. Not implemented yet (to be used for options section)
let sheetFlipMUX (model: Model) : Model =
    let updatedSymbols =
        model.Wire.Symbol.Symbols
        |> Map.toList
        |> List.map (fun (id, symbol) ->
            if symbol.Component.Type = Mux2 then
                (id, Optic.set symbolFlipState_ (not symbol.STransform.Flipped) symbol)
            else
                (id, symbol))
        |> Map.ofList
    { model with
        Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbols } } }