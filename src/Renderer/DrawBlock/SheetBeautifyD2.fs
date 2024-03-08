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
open BusWireRoute

// /// Permutes gate inputs
// let rec permute list =
//     match list with
//     | [] -> [ [] ]
//     | head :: tail ->
//         let rec insertEverywhere x list =
//             match list with
//             | [] -> [ [ x ] ]
//             | head :: tail ->
//                 (x :: list)
//                 :: [ for rest in insertEverywhere x tail -> head :: rest ]
//         let tailPerms = permute tail
//         tailPerms |> List.collect (insertEverywhere head)

// /// Applies all permutations of gate inputs and MUX inputs and evaluate the transformations to output the best model
// let evaluateTransformations (model: Model) : Model =
//     printf "BEGINNING EVALUATE TRANSFORMATIONS -------"
    
//     let originalCrossings = numOfWireRightAngleCrossings model
//     let originalRightAngles = numOfVisRightAngles model

//     // Extracts gates from the model
//     let gates =
//         model.Wire.Symbol.Symbols
//         |> Map.filter (fun _ symbol ->
//             match symbol.Component.Type with
//             | GateN _ -> true
//             | _ -> false)
//         |> Map.toList
//     printfn "Found %d gates" (List.length gates)
//     gates |> List.iter (fun (id, sym) -> printfn "Gate Id: %A, Type: %A" id sym.Component.Type)

//     // Extracts MUXes from the model
//     let muxes =
//         model.Wire.Symbol.Symbols
//         |> Map.filter (fun _ symbol ->
//             match symbol.Component.Type with
//             | Mux2 -> true
//             | _ -> false)
//         |> Map.toList
//     printfn "Found %d MUXes" (List.length muxes)
//     muxes |> List.iter (fun (id, sym) -> printfn "MUX Id: %A, Reversed Inputs: %A" id sym.ReversedInputPorts)
//     muxes
//     |> List.iter (fun (_, symbol) ->
//             symbol.Component.InputPorts
//             |> List.iter (fun port ->
//                 printfn "MUX portId: %A portNumber: %A, portType: %A" port.Id port.PortNumber port.PortType))
    

//     // Generates all permutations for gates
//     let gatePermutations =
//         gates
//         |> List.collect (fun (id, symbol) ->
//             let inputPerms = permute symbol.Component.InputPorts
//             printfn "Gate Id: %A permutations:" id
//             inputPerms |> List.iter (fun perm -> printfn "%A" perm)
//             inputPerms
//             |> List.map (fun perm -> (id, { symbol with Component = { symbol.Component with InputPorts = perm } })))
//     printfn "Generated %d gate permutations" (List.length gatePermutations)

//     // Generates all flipped MUX models
//     let muxPermutations =
//         muxes
//         |> List.map (fun (id, symbol) ->
//             match symbol.ReversedInputPorts with
//             | Some state ->
//                 let flippedSymbol = Optic.set reversedInputPorts_ (Some(not state)) symbol
//                 // Print the flipped state of each MUX
//                 printfn "Flipped MUX: %A" flippedSymbol
//                 (id, flippedSymbol)
//             | None -> failwithf "Reversed Input Ports not found for MUX %A" id)
//     printfn "Generated %d MUX permutations" (List.length muxPermutations)

//     // Applies permutations and flip transformations
//     let models =
//         gatePermutations @ muxPermutations
//         |> List.map (fun (id, symbol) ->
//             { model with
//                 Wire =
//                     { model.Wire with
//                         Symbol =
//                             { model.Wire.Symbol with
//                                 Symbols = model.Wire.Symbol.Symbols |> Map.add id symbol } } })

//     // Evaluates each model and select the best one
//     let bestModel =
//         models
//         |> List.minBy (fun m ->
//             let crossings = numOfWireRightAngleCrossings m
//             let rightAngles = numOfVisRightAngles m
//             if
//                 crossings <= originalCrossings
//                 && rightAngles <= originalRightAngles
//             then
//                 crossings, -rightAngles
//             else
//                 failwithf "cannot find best model")

//     bestModel


// TESTING GATE SWITCHING ---------------------------------------
// --------------------------------------------------------------

/// Applies the smartAutoRoute function to all existing wires connected to a symbol
let rerouteWire (model: SheetT.Model) (symbolId: ComponentId) : SheetT.Model = 
    let wiresToReroute = 
        model.Wire.Wires
        |> Map.filter (fun _ wire ->
            let sourceSymbol = getSourceSymbol model.Wire wire
            let targetSymbol = getTargetSymbol model.Wire wire
            sourceSymbol.Id = symbolId || targetSymbol.Id = symbolId)
        |> Map.toList
        |> List.map snd  

    let rerouteModel (model: SheetT.Model) (wire: Wire): SheetT.Model = 
        let newWire = smartAutoroute model.Wire wire
        let newWires = Map.add wire.WId newWire model.Wire.Wires
        {model with Wire = {model.Wire with Wires = newWires}}

    let updateWireModel = 
        wiresToReroute 
        |> List.fold (fun accModel wire -> rerouteModel accModel wire) model

    updateWireModel

/// Gate permutations (currently just horizontal flip)
/// TODO: Implement permuations for higher port orders & test if horiontal flip is correct for all orientations
let flipGatesHorizontally (model: SheetT.Model) : SheetT.Model =
    let updatedModel, flippedIds = 
        model.Wire.Symbol.Symbols
        |> Map.fold (fun (accModel, accIds) id symbol ->
            match symbol.Component.Type with
            | GateN _ ->
                let accModel = flipSymbol (symbol.Component.Label) FlipVertical accModel 
                accModel, id :: accIds
            | _ -> accModel, accIds) (model, [])

    flippedIds
    |> List.fold rerouteWire updatedModel

/// Find the better performing model based off total wire crossings and right angles
let evaluateModelCrossings (originalCrossings: int) (originalRightAngles: int) (flippedModel: SheetT.Model) (originalModel: SheetT.Model) = 
    let crossings = numOfWireRightAngleCrossings flippedModel
    printfn "NewCrossings: %A" crossings
    let rightAngles = numOfVisRightAngles flippedModel
    printfn "NewRightAngles: %A" rightAngles
    if crossings <= originalCrossings && rightAngles <= originalRightAngles then
        flippedModel
    else
        originalModel

// Find model 
let findBestModel (model: SheetT.Model) = 
    printf "RUNNING EVALUATION------------------------------------------------------------------------"
    let originalCrossings = numOfWireRightAngleCrossings model
    printfn "OriginalCrossings: %A " originalCrossings
    let originalRightAngles = numOfVisRightAngles model
    printfn "OriginalRightAngles: %A " originalRightAngles

    // For now just checking if the two input flip improves model quality
    let flippedModel = flipGatesHorizontally model

    let flippedModelEval = evaluateModelCrossings originalCrossings originalRightAngles flippedModel model

    flippedModelEval

//------------------------------------------------------------
//------------------------------------------------------------







/// Flips all MUXes on a sheet. Not implemented yet (to be used for options section)
// let sheetFlipMUX (model: Model) : Model =
//     let updatedSymbols =
//         model.Wire.Symbol.Symbols
//         |> Map.toList
//         |> List.map (fun (id, symbol) ->
//             if symbol.Component.Type = Mux2 then
//                 (id, Optic.set symbolFlipState_ (not symbol.STransform.Flipped) symbol)
//             else
//                 (id, symbol))
//         |> Map.ofList
//     { model with
//         Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbols } } }

// let reverseAllMuxInputPorts (model: Model) : Model =
//     let reverseInputPort symbol =
//         let newSymbol =
//             match symbol.ReversedInputPorts with
//             | Some state -> Optic.set reversedInputPorts_ (Some(not state)) symbol
//             | None -> Optic.set reversedInputPorts_ (Some true) symbol // Default to true if not set
//         newSymbol.Component.InputPorts
//         |> List.iter (fun port ->
//         newSymbol

//     let updatedSymbols =
//         model.Wire.Symbol.Symbols
//         |> Map.map (fun _ symbol ->
//             match symbol.Component.Type with
//             | Mux2 -> 
//                 reverseInputPort symbol
//             | _ -> symbol) // Skip non-MUX2 symbols

//     { model with Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbols } } }

