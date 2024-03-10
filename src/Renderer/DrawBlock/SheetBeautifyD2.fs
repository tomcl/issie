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

// --------------------------------------------------------------
// --------------------------------------------------------------
// D2 using only horizontal flips, permuting across every option 

/// Function to generate powersets for testing all combinations of flips
let rec powerSet = function
    | [] -> [[]]
    | head :: tail ->
        let tailSubsets = powerSet tail
        tailSubsets @ (tailSubsets |> List.map (fun subset -> head :: subset))

/// Applies the smartAutoRoute function to all existing wires connected to a symbol
let rerouteWire (symbolId: ComponentId) (model: SheetT.Model)  : SheetT.Model = 
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

/// Flips the specified component before rerouting connected wires
let flipAndRerouteComp (model: SheetT.Model) (compId: ComponentId) : SheetT.Model =
    let compLabel =
        match Map.tryFind compId model.Wire.Symbol.Symbols with
        | Some sym -> sym.Component.Label
        | None -> failwith "Component not found in model"
    
    printfn "Flipping component: %A" compId

   
    flipSymbol compLabel FlipVertical model 
    |> rerouteWire compId
    
    

/// Find the better performing model based off total wire crossings and right angles
let evaluateModels (flippedModel: SheetT.Model) (originalModel: SheetT.Model) : SheetT.Model = 
    let originalCrossings = numOfWireRightAngleCrossings originalModel
    let originalRightAngles = numOfVisRightAngles originalModel    
    let newCrossings = numOfWireRightAngleCrossings flippedModel
    let newRightAngles = numOfVisRightAngles flippedModel

    // printfn "Running evaluateModelCrossings................."
    // printfn "originalCrossings: %A" originalCrossings
    // printfn "originalRightAngles: %A" originalRightAngles
    // printfn "NewCrossings: %A" newCrossings
    // printfn "NewRightAngles: %A" newRightAngles

    if newCrossings <= originalCrossings && newRightAngles <= originalRightAngles then    
        if newCrossings = originalCrossings && newRightAngles = originalRightAngles then
            // printfn "Kept original model"
            // printfn "EvaluateModelCrossings complete................"
            originalModel
            
        else
            // printfn "Changed to new model"
            // printfn "EvaluateModelCrossings complete................"
            flippedModel
    else
        // printfn "Kept original model"
        // printfn "EvaluateModelCrossings complete................"
        originalModel

/// Perform exhaustive search across all component flips, finding the model with the least total wire crossings and right angles
let findBestModel (model: SheetT.Model) : SheetT.Model = 
    let components =
        model.Wire.Symbol.Symbols
        |> Map.toList
        |> List.choose (fun (id, sym) ->
            match sym.Component.Type with
            | GateN _ | Mux2 -> Some id
            | _ -> None)

    let flipCombinations = powerSet components
    // printfn "Flip combinations: %A" flipCombinations

    let compareModels (currentBestModel: SheetT.Model) flipCombination = 
        let flippedModel = 
            flipCombination |> List.fold (fun accModel compId ->
                flipAndRerouteComp accModel compId 
            ) currentBestModel

        evaluateModels flippedModel currentBestModel 

    flipCombinations |> List.fold compareModels model






// /// Gate permutations (currently just horizontal flip)
// /// TODO: Implement permuations for higher port orders & test if horiontal flip is correct for all orientations
// let flipGatesHorizontally (model: SheetT.Model) : SheetT.Model =
//     let updatedModel, flippedIds = 
//         model.Wire.Symbol.Symbols
//         |> Map.fold (fun (accModel, accIds) id symbol ->
//             match symbol.Component.Type with
//             | GateN _ | Mux2 ->
//                 let accModel = flipSymbol (symbol.Component.Label) FlipVertical accModel 
//                 accModel, id :: accIds
//             | _ -> accModel, accIds) (model, [])

//     flippedIds
//     |> List.fold rerouteWire updatedModel


//------------------------------------------------------------
//------------------------------------------------------------



