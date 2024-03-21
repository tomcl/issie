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
// Exhaustive Search

type ComponentState =
    { ComponentId: ComponentId
      Flipped: bool
      Rotation: Rotation
      InputsReversed: bool }

/// Function to generate powersets for testing all combinations of flips
let rec powerSet =
    function
    | [] -> [ [] ]
    | head :: tail ->
        let tailSubsets = powerSet tail
        tailSubsets
        @ (tailSubsets
           |> List.map (fun subset -> head :: subset))

/// Generates records for the specified component, for every single state combination
let generateComponentStates (compId: ComponentId) : ComponentState list =
    let flipStates = [ true; false ]
    let rotationStates = [ Degree0; Degree90; Degree180; Degree270 ]
    let inputReversalStates = [ true; false ]
    List.collect
        (fun flip ->
            List.collect
                (fun rotation ->
                    List.map
                        (fun inputReversed ->
                            { ComponentId = compId
                              Flipped = flip
                              Rotation = rotation
                              InputsReversed = inputReversed })
                        inputReversalStates)
                rotationStates)
        flipStates
let rec cartesianProduct =
    function
    | [] -> [ [] ]
    | xs :: xss ->
        [ for x in xs do
              for ys in cartesianProduct xss do
                  yield x :: ys ]

/// Applies the smartAutoRoute function to all existing wires connected to a symbol
let rerouteWire (symbolId: ComponentId) (model: SheetT.Model) : SheetT.Model =
    let wiresToReroute =
        model.Wire.Wires
        |> Map.filter (fun _ wire ->
            let sourceSymbol = getSourceSymbol model.Wire wire
            let targetSymbol = getTargetSymbol model.Wire wire
            sourceSymbol.Id = symbolId
            || targetSymbol.Id = symbolId)
        |> Map.toList
        |> List.map snd

    let rerouteModel (model: SheetT.Model) (wire: Wire) : SheetT.Model =
        let newWire = smartAutoroute model.Wire wire
        let newWires = Map.add wire.WId newWire model.Wire.Wires
        { model with Wire = { model.Wire with Wires = newWires } }

    let updateWireModel =
        wiresToReroute
        |> List.fold (fun accModel wire -> rerouteModel accModel wire) model

    updateWireModel

/// Find a component label from its Id
let findComponent (compId: ComponentId) (model: SheetT.Model) : string =
    match Map.tryFind compId model.Wire.Symbol.Symbols with
    | Some sym -> sym.Component.Label
    | None -> failwith "Component not found in model"

/// Flips the specified component before rerouting connected wires
let flipAndRerouteComp (model: SheetT.Model) (compId: ComponentId) : SheetT.Model =
    let compLabel = findComponent compId model

    // printfn "Flipping component: %A" compId

    flipSymbol compLabel FlipVertical model
    |> rerouteWire compId

/// Rotates the specified component before rerouting connected wires
let rotateAndRerouteComp (model: SheetT.Model) (compId: ComponentId) (rotate: Rotation) : SheetT.Model =
    let compLabel = findComponent compId model

    // printfn "Rotating component: %A by %A" compId rotate

    rotateSymbol compLabel rotate model
    |> rerouteWire compId

/// Reverses inputs of specified component before rerouting connected wires
let reverseInputsAndRerouteComp (model: SheetT.Model) (compId: ComponentId) : SheetT.Model =
    let updateSymbol sym =
        match sym.ReversedInputPorts with
        | Some currState ->
            if sym.Component.Type = Mux2 then
                sym
            else
                { sym with ReversedInputPorts = Some(not currState) }
        | None -> failwith "No state found"

    let updatedSymbols =
        model.Wire.Symbol.Symbols
        |> Map.change compId (Option.map updateSymbol)

    // printfn "Reversing Input Ports of Mux: %A" compId

    let newModel =
        { model with
            Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbols } } }
    newModel |> rerouteWire compId

let applyStateToModel (model: SheetT.Model) (state: ComponentState) : SheetT.Model =
    let updatedFlipModel =
        if state.Flipped = true then
            flipAndRerouteComp model state.ComponentId
        else
            model
    let updatedInputsReversedModel =
        if state.InputsReversed = true then
            reverseInputsAndRerouteComp updatedFlipModel state.ComponentId
        else
            updatedFlipModel
    rotateAndRerouteComp updatedInputsReversedModel state.ComponentId state.Rotation

/// Find the better performing model based off total wire crossings and right angles
let evaluateModels (changedModel: SheetT.Model) (originalModel: SheetT.Model) : SheetT.Model =
    let originalCrossings = numOfWireRightAngleCrossings originalModel
    let originalRightAngles = numOfVisRightAngles originalModel
    let newCrossings = numOfWireRightAngleCrossings changedModel
    let newRightAngles = numOfVisRightAngles changedModel

    // printfn "Running evaluateModelCrossings................."
    // printfn "originalCrossings: %A" originalCrossings
    // printfn "originalRightAngles: %A" originalRightAngles
    // printfn "NewCrossings: %A" newCrossings
    // printfn "NewRightAngles: %A" newRightAngles

    if
        newCrossings <= originalCrossings
        && newRightAngles <= originalRightAngles
    then
        if
            newCrossings = originalCrossings
            && newRightAngles = originalRightAngles
        then
            // printfn "Kept original model"
            // printfn "EvaluateModelCrossings complete................"
            originalModel
        else
            // printfn "Changed to new model"
            // printfn "EvaluateModelCrossings complete................"
            changedModel
    else
        // printfn "Kept original model"
        // printfn "EvaluateModelCrossings complete................"
        originalModel

let evaluateAllComponents (model: SheetT.Model) (components: ComponentId list) : SheetT.Model =
    let allStates = components |> List.map generateComponentStates
    let allStateCombinations = cartesianProduct allStates
    let mutable stateCount = 0

    let applyStatesToModel model states =
        states |> List.fold applyStateToModel model

    let evaluateAndCompare model states =
        let updatedModel = applyStatesToModel model states
        stateCount <- stateCount + 1
        evaluateModels updatedModel model

    let finalModel =
        allStateCombinations
        |> List.fold evaluateAndCompare model

    // Print the total number of state applications
    printfn "Total number of ComponentStates applied: %d" stateCount
    finalModel

let findAllSubCircuitsFunctional (model: SheetT.Model) =
    // Helper to add a component to a sub-circuit, ensuring no duplicates.
    let addToSubCircuit comp subCircuit =
        if List.contains comp subCircuit then
            subCircuit
        else
            comp :: subCircuit

    // Helper to merge two sub-circuits and ensure unique elements.
    let mergeSubCircuits sc1 sc2 = (sc1 @ sc2) |> List.distinct

    let rec processWires wires subCircuits =
        match wires with
        | [] -> subCircuits
        | wire :: remainingWires ->
            let sourceSymbol = getSourceSymbol model.Wire wire
            let targetSymbol = getTargetSymbol model.Wire wire
            let sourceId, targetId = sourceSymbol.Id, targetSymbol.Id

            // Find sub-circuits that contain the source or target component, if any.
            let sourceSubCircuitIndex, targetSubCircuitIndex =
                subCircuits
                |> List.indexed
                |> List.fold
                    (fun (si, ti) (idx, sc) ->
                        let si =
                            if List.contains sourceId sc then
                                Some idx
                            else
                                si
                        let ti =
                            if List.contains targetId sc then
                                Some idx
                            else
                                ti
                        (si, ti))
                    (None, None)

            // Update sub-circuits based on whether source/target components are already in sub-circuits.
            let updatedSubCircuits =
                match sourceSubCircuitIndex, targetSubCircuitIndex with
                | Some si, Some ti when si = ti -> subCircuits
                | Some si, None ->
                    subCircuits
                    |> List.mapi (fun idx sc ->
                        if idx = si then
                            addToSubCircuit targetId sc
                        else
                            sc)
                | None, Some ti ->
                    subCircuits
                    |> List.mapi (fun idx sc ->
                        if idx = ti then
                            addToSubCircuit sourceId sc
                        else
                            sc)
                | Some si, Some ti when si <> ti ->
                    let merged = mergeSubCircuits (List.item si subCircuits) (List.item ti subCircuits)
                    let withoutSi =
                        List.append (List.take si subCircuits) (List.skip (si + 1) subCircuits)
                    let finalSubCircuits =
                        List.append (List.take ti withoutSi) (List.skip (ti + 1) withoutSi)
                    merged :: finalSubCircuits
                | None, None ->

                    List.append subCircuits [ [ sourceId; targetId ] ]
                | _, _ -> failwithf "should not be here"

            processWires remainingWires updatedSubCircuits

    processWires (List.map snd (Map.toList model.Wire.Wires)) []
let findBestModel (model: SheetT.Model) : SheetT.Model =
    // Finds all sub-circuits in the model.
    let subCircuits = findAllSubCircuitsFunctional model

    // Function to evaluate a single sub-circuit and update the model.
    let evaluateSubCircuit (currentModel: SheetT.Model) (subCircuit: ComponentId list) : SheetT.Model =
        // Filter components of the current sub-circuit for the specific types to evaluate.
        let componentsToEvaluate =
            subCircuit
            |> List.choose (fun id ->
                match Map.tryFind id currentModel.Wire.Symbol.Symbols with
                | Some sym ->
                    match sym.Component.Type with
                    | GateN _
                    | Mux2 -> Some id
                    | _ -> None
                | None -> None)

        // Evaluate all components in the current sub-circuit and update the model.
        evaluateAllComponents currentModel componentsToEvaluate

    // Sequentially evaluate each sub-circuit and update the model.
    let updatedModel = List.fold evaluateSubCircuit model subCircuits

    // Return the model updated with evaluations of all sub-circuits.
    updatedModel

// let flipCombinations = powerSet components
// // printfn "Flip combinations: %A" flipCombinations

// let compareModels (currentBestModel: SheetT.Model) flipCombination =
//     let flippedModel =
//         flipCombination
//         |> List.fold (fun accModel compId -> rotateAndRerouteComp accModel compId Degree90) currentBestModel

//     evaluateModels flippedModel currentBestModel

// flipCombinations |> List.fold compareModels model

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

// let evaluateAllCombinations (model: SheetT.Model) (components: ComponentId list) : SheetT.Model =
//     let allStates =
//         components
//         |> List.collect generateComponentStates
//         |> Set.ofList
//         |> Set.toList
//     let powerSetOfStates = powerSet allStates |> Set.ofList |> Set.toList

//     powerSetOfStates
//     |> List.iter (fun subset ->
//         printfn "Subset start"
//         subset
//         |> List.iter (fun state ->
//             printfn
//                 "ComponentId: %A, Flipped: %b, Rotation: %A, InputsReversed: %b"
//                 state.ComponentId
//                 state.Flipped
//                 state.Rotation
//                 state.InputsReversed)
//         printfn "Subset end")

//     powerSetOfStates
//     |> List.fold
//         (fun currentBestModel statesCombination ->
//             let modelForCombination =
//                 statesCombination
//                 |> List.fold applyStateToModel model

//             evaluateModels modelForCombination currentBestModel)
//         model

// /// Perform exhaustive search across all component flips, finding the model with the least total wire crossings and right angles
// let findBestModel (model: SheetT.Model) : SheetT.Model =
//     let components =
//         model.Wire.Symbol.Symbols
//         |> Map.toList
//         |> List.choose (fun (id, sym) ->
//             match sym.Component.Type with
//             | GateN _
//             | Mux2 -> Some id
//             | _ -> None)

//     evaluateAllCombinations model components

//------------------------------------------------------------
//------------------------------------------------------------
// Iterated Local Search (ILS)

/// General framework for Iterated Local Search -> Needs fully implementing once we test more complex circuits
let iteratedLocalSearchSingleComponent (initialModel: SheetT.Model) : SheetT.Model =
    let optimiseComponent (model: SheetT.Model) (compId: ComponentId) =
        let states = generateComponentStates compId
        states
        |> List.fold
            (fun currentBestModel state ->
                let newStateModel = applyStateToModel model state
                evaluateModels newStateModel currentBestModel)
            model

    let rec optimiseAllComponents (model: SheetT.Model) (components: ComponentId list) =
        match components with
        | [] -> model
        | compId :: restComponents ->
            let optimisedModel = optimiseComponent model compId
            optimiseAllComponents optimisedModel restComponents // Continue with the rest components.

    let componentIds =
        initialModel.Wire.Symbol.Symbols
        |> Map.toList
        |> List.choose (fun (id, sym) ->
            match sym.Component.Type with
            | GateN _
            | Mux2 -> Some id
            | _ -> None)

    let optimisedModel = optimiseAllComponents initialModel componentIds
    let doubleOptimisedModel = optimiseAllComponents optimisedModel componentIds
    doubleOptimisedModel
