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

//--------------------------------------------------------------------------------------------------------------------------//

//--------------------------------------------------------------------------------------------------------------------------//
//---------------------------------------------------- Exhaustive Search ---------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//

/// Record that stores potential permutations
type ComponentState =
    { ComponentId: ComponentId
      Flipped: bool
      Rotation: Rotation
      InputsReversed: bool }

/// Generates records for the specified component, for every possible  state combination
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
/// Recursive function to compute the Cartesian product of a list of lists. Used to generate all possible combinations of permutations
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

/// Applies a ComponentState record to the sheet
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

/// Finds the better performing model based off total wire crossings and right angles
let evaluateModels (changedModel: SheetT.Model) (originalModel: SheetT.Model) : SheetT.Model * bool =
    let originalCrossings = numOfWireRightAngleCrossings originalModel
    let originalRightAngles = numOfVisRightAngles originalModel
    let newCrossings = numOfWireRightAngleCrossings changedModel
    let newRightAngles = numOfVisRightAngles changedModel

    if
        newCrossings <= originalCrossings
        && newRightAngles <= originalRightAngles
    then
        if
            newCrossings = originalCrossings
            && newRightAngles = originalRightAngles
        then
            originalModel, false
        else

            changedModel, true
    else
        originalModel, false

/// Generates, applies and evaluates all potential permutations for a specified list of componenents.
/// Returns the best performing model
let evaluateAllComponents (model: SheetT.Model) (components: ComponentId list) : SheetT.Model =
    let allStates = components |> List.map generateComponentStates
    let allStateCombinations = cartesianProduct allStates
    let mutable stateCount = 0

    let applyStatesToModel model states =
        states |> List.fold applyStateToModel model

    let evaluateAndCompare model states =
        let updatedModel = applyStatesToModel model states
        stateCount <- stateCount + 1
        let winningModel, _ = evaluateModels updatedModel model
        winningModel

    let finalModel =
        allStateCombinations
        |> List.fold evaluateAndCompare model

    // Print the total number of state applications
    printfn "Total number of ComponentStates applied: %d" stateCount
    finalModel

/// Partitions the components in to independent connected groups based on if they are directly
/// or indirectly connected
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

            // Updates sub-circuits based on whether source/target components are already in sub-circuits.
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

/// Partitions circuit and applies exhaustive search on each sub-circuit.
/// Returns model with best performing sub-circuits
let findBestModel (model: SheetT.Model) : SheetT.Model =
    let subCircuits = findAllSubCircuitsFunctional model

    // Function to evaluate a single sub-circuit and update the model.
    let evaluateSubCircuit (currentModel: SheetT.Model) (subCircuit: ComponentId list) : SheetT.Model =
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

        evaluateAllComponents currentModel componentsToEvaluate

    let updatedModel = List.fold evaluateSubCircuit model subCircuits

    updatedModel

//--------------------------------------------------------------------------------------------------------------------------//

//--------------------------------------------------------------------------------------------------------------------------//
//-------------------------------------------------- Iterated Local Search -------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//

/// Generates possible permutations for one component at a time, evaluating which permutation is best.
/// Carried out on all components to achieve a local minimum
/// Generates possible permutations for one component at a time, evaluating which permutation is best.
/// Continues to optimize each component until no further improvements can be made.

let iteratedLocalSearchSingleComponent (initialModel: SheetT.Model) : SheetT.Model =
    let optimiseComponent (model: SheetT.Model) (compId: ComponentId) =
        let states = generateComponentStates compId
        states
        |> List.fold
            (fun (accModel, accStateOpt) state ->
                let newStateModel = applyStateToModel accModel state
                let newStateModelImproved, improved = evaluateModels newStateModel accModel
                if improved then
                    (newStateModelImproved, Some state)
                else
                    (accModel, accStateOpt))
            (model, None)

    let rec optimiseAllComponents
        (model: SheetT.Model)
        (components: ComponentId list)
        (bestStates: Map<ComponentId, ComponentState>)
        =
        match components with
        | [] -> model, bestStates
        | compId :: restComponents ->
            let newModel, newStateOpt = optimiseComponent model compId
            let newBestStates =
                match newStateOpt with
                | Some newState -> Map.add compId newState bestStates
                | None -> bestStates
            optimiseAllComponents newModel restComponents newBestStates

    let rec optimisationLoop (model: SheetT.Model) (bestStates: Map<ComponentId, ComponentState>) iterationCount =
        let componentIds = Map.toList bestStates |> List.map fst
        let newModel, newBestStates = optimiseAllComponents model componentIds bestStates
        if newBestStates = bestStates then
            printfn "Total iterations: %d" iterationCount
            model
        else
            optimisationLoop newModel newBestStates (iterationCount + 1)

    let initialBestStates =
        initialModel.Wire.Symbol.Symbols
        |> Map.filter (fun _ sym ->
            match sym.Component.Type with
            | GateN _
            | Mux2 -> true
            | _ -> false)
        |> Map.map (fun id _ ->
            { ComponentId = id
              Flipped = false
              Rotation = Degree0
              InputsReversed = false })

    optimisationLoop initialModel initialBestStates 0
