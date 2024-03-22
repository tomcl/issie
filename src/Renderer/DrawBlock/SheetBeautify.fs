module SheetBeautify

// open modules likely to be used
open SheetBeautifyHelpers
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open SymbolUpdate
open BlockHelpers
open Optics
open Optics.Operators
open Helpers
open Symbol
open BusWireRoute
open Sheet
open EEExtensions
open SegmentHelpers

/// Optic to access SymbolT.Model from SheetT.Model
let symbolModel_ = symbol_

/// Optic to access BusWireT.Model from SheetT.Model
let busWireModel_ = wire_

/// allowed max X or y coord of svg canvas
let maxSheetCoord = Constants.defaultCanvasSize

/// Find the middle of the sheet
let middleOfSheet = { X = maxSheetCoord / 2.; Y = maxSheetCoord / 2. }

//type SymbolPortRec = { Label: string; PortNumber: int }
type SymbolPortRec = { SymbolId: ComponentId; PortNumber: int }

/// Used throughout to compare labels since these are case invariant "g1" = "G1"
let caseInvariantEqual str1 str2 =
    String.toUpper str1 = String.toUpper str2

//--------------------------------------------------------------------------------------------------------------------------//
//----------------------------------------------- D1 Implementation --------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//

/// <summary>
/// Filters parallel wires from the model based on the number of visible segments.
/// </summary>
/// <param name="model">The sheet model containing wires and segments.</param>
/// <returns>List of wires with exactly three visible segments.</returns>
let parallelWires (model: SheetT.Model) =
    Map.values model.Wire.Wires
    |> List.ofSeq
    |> List.filter (fun wire -> List.length (visibleSegments wire.WId model) >= 3)
    |> List.sortBy (fun wire -> wire.StartPos.X)

/// <summary>
/// Gets all single-connected symbols associated with a list of wires in the model.
/// </summary>
/// <param name="model">The sheet model containing wires and symbols.</param>
/// <param name="wires">List of wires to consider for symbol connections.</param>
/// <returns>List of single-connected symbols.</returns>
let getAllSingleConnectedSymbols (model: SheetT.Model) (wires: Wire list) =
    let sourceTargetSymbols =
        List.map (fun wire -> (getSourceSymbol model.Wire wire, getTargetSymbol model.Wire wire, wire)) wires

    let getSingleConnectedSymbols (symbolList: (Symbol * Symbol * Wire) list) =
        symbolList
        |> List.groupBy (fun sym -> sym)
        |> List.filter (fun (_, symList) -> List.length symList = 1)
        |> List.map (fun (sym, _) -> sym)

    let singleConnected = getSingleConnectedSymbols sourceTargetSymbols
    let sourceSymbols = List.map (fun (s, _, _) -> s.Id) singleConnected
    let targetSymbols = List.map (fun (_, t, _) -> t.Id) singleConnected

    let moveSource sl tl s t=
        (not (List.contains s tl) || List.contains t sl)


    singleConnected
    // |> List.map (fun (s, t, w) -> (s, t, w, moveSource sourceSymbols targetSymbols s.Id t.Id))
    // |> List.sortBy (fun (_, _, w, _) -> (w.StartPos.X, w.StartPos.Y))

/// <summary>
/// Gets all symbols associated with parallel wires in the model.
/// </summary>
/// <param name="model">The sheet model containing wires and symbols.</param>
/// <returns>List of symbols connected by parallel wires.</returns>
let getAllSymbols (model: SheetT.Model) =
    getAllSingleConnectedSymbols model (parallelWires model)

/// <summary>
/// Gets the bounding boxes of symbols associated with parallel wires in the model.
/// </summary>
/// <param name="model">The sheet model containing wires and symbols.</param>
/// <returns>List of bounding boxes of symbols connected by parallel wires.</returns>
let getSymbolsBoundingBox (model: SheetT.Model) =
    let symbols = getAllSymbols model
    symbols
    |> List.map (fun (symS, symT, _) -> getSymBoundingBox symS)

/// <summary>
/// Check for overlaps between symbols after they have been moved.
/// </summary>
/// <param name="symbols">The symbol that checks for overlapping.</param>
/// <returns>List of overlaps</returns>
let detectOverlaps (symbols: Symbol list) : (Symbol * Symbol) list =
    let boundingBoxes = symbols |> List.map (fun sym -> (sym, getSymBoundingBox sym))
    let overlaps =
        boundingBoxes
        |> List.collect (fun (sym1, box1) ->
            boundingBoxes
            |> List.filter (fun (sym2, box2) ->
                sym1 <> sym2 && overlap2DBox box1 box2)
            |> List.map (fun (sym2, _) -> (sym1, sym2)))
    overlaps

let resolveOverlaps (overlaps: (Symbol * Symbol) list) : Symbol list -> Symbol list =
    let moveApart (sym1: Symbol, sym2: Symbol) : Symbol * Symbol =
        // Calculate current bounding boxes
        let bb1 = getSymBoundingBox sym1
        let bb2 = getSymBoundingBox sym2

        // Determine the overlap along X and Y
        let overlapX = max 0.0 (bb1.TopLeft.X + bb1.W - bb2.TopLeft.X)
        let overlapY = max 0.0 (bb1.TopLeft.Y + bb1.H - bb2.TopLeft.Y)

        // Determine how much to move the symbols to no longer overlap
        let moveDistanceX = if overlapX > 0.0 then overlapX + 1.0 else 0.0
        let moveDistanceY = if overlapY > 0.0 then overlapY + 1.0 else 0.0

        // Check the direction of movement needed based on your layout preference
        // This example assumes moving sym2 to the right and/or down
        let newPosX = sym2.Pos.X + moveDistanceX
        let newPosY = sym2.Pos.Y + moveDistanceY

        // Move sym2 by updating its position
        let sym2Moved = { sym2 with Pos = { X = newPosX; Y = newPosY } }

        // Return the updated symbols
        (sym1, sym2Moved)

    let updatedSymbols (symbols) = 
        overlaps |> List.fold (fun acc (sym1, sym2) ->
            let (movedSym1, movedSym2) = moveApart(sym1, sym2) 
            // Update the list of symbols with the new positions
            // Ensure that the list remains unique and update only the moved symbols
            acc |> List.map (fun sym ->
                if sym.Id = movedSym1.Id then movedSym1
                elif sym.Id = movedSym2.Id then movedSym2
                else sym)
        ) symbols  // Start with the original list of symbols
    updatedSymbols  // Return the updated list of symbols
    





/// <summary>
/// Aligns all singly connected symbols by their target port.
/// </summary>
/// <param name="model">The sheet model containing wires and symbols</param>
/// <returns>Updated model with the moved symbols</returns>
let alignSingleConnectedSyms (model: SheetT.Model) (syms) =
    // let syms = getAllSingleConnectedSymbols model (parallelWires model)
                //Prefer moving symbols with only one port to have minimal impact on the rest of the sheet
                //Need to modify the delta function to account for different directions when moving source/target
                // |> List.map (fun (s,t,w) ->
                //                 printfn $"{s.PortMaps.Order.Count}, {t.PortMaps.Order.Count}"
                //                 match (s.PortMaps.Order.Count, t.PortMaps.Order.Count) with
                //                 | (1,1) -> (s,w)
                //                 | (_, 1) -> (t,w)
                //                 | (_, _) -> (s,w)
                //             )
    let checkXCoor (s1: Symbol) (s2: Symbol) =
        if abs(s1.Pos.X - s2.Pos.X) < 80 then
            -100.
        else 0
    let delta (wire:Wire) (x: float) =
        match wire.InitialOrientation with
        | Horizontal -> {X=x; Y=wire.EndPos.Y - (wire.StartPos.Y)}
        | Vertical -> {X=wire.EndPos.X - (wire.StartPos.X); Y=0}

    let symbolMap = Optic.get symbols_ model
    let movedSyms = List.map (fun (s,t,w) -> moveSymbol (checkXCoor s t |> delta w) (s)) syms
    let symbols' =  (symbolMap, movedSyms)
                    ||> List.fold (fun s movedSym ->
                        s |> Map.map (fun _ v ->
                            match v.Id with
                            | id when id = movedSym.Id -> movedSym
                            | _ -> v
                        ))


    let overlaps = detectOverlaps (Map.toSeq symbols' |> Seq.map snd |> Seq.toList)
    let nonOverlappingSymbols = resolveOverlaps overlaps (Map.toSeq symbols' |> Seq.map snd |> Seq.toList)



    let NewSymbolModel = 
        Optic.set symbols_ 
            (Map.ofSeq (List.zip (Map.keys symbols' |> Seq.toList) nonOverlappingSymbols)) 
            model


    //Getting Wire map to update to new wire positions based on updated Symbol postions
    let wireMap = Optic.get wires_ model
    let movedWires = wireMap |> Map.values |> List.ofSeq
                    |> List.map (fun w ->
                    // BusWireUpdateHelpers.autoroute NewSymbolModel.Wire w)
                    BusWireRoute.smartAutoroute NewSymbolModel.Wire w)

    let wires' =  (wireMap, movedWires)
                    ||> List.fold (fun w movedWire ->
                        w |> Map.map (fun _ v ->
                            match v.WId with
                            | id when id = movedWire.WId -> movedWire
                            | _ -> v
                        ))
    

    // Updating the model with the new wire positions
    // let intersectingPair = numOfIntersectedSymPairs NewSymbolModel
    // printfn "Number of intersecting pairs: %d" intersectingPair
    Optic.set wires_ wires' NewSymbolModel

// Only scales unrotated components
let scaleCustomSymAlign (sourceSym: Symbol) (targetSym: Symbol) =
    let sourceDims = Optic.get customCompDims_ sourceSym
    let targetDims = Optic.get customCompDims_ targetSym
    let sourcePortCount = List.length sourceSym.Component.InputPorts
    let targetPortCount = List.length targetSym.Component.InputPorts
    let sourceDimPerPort = sourceDims.Y / (float) sourcePortCount
    let targetDimPerPort = targetDims.Y / (float) targetPortCount
    let scale = targetDimPerPort / sourceDimPerPort
    Optic.set customCompDims_ ({ X = targetDims.X; Y = targetDims.Y * scale }) targetSym

// let scaleCustomSyms model =
//     let customSyms = Optic.get symbols_ model
//                      |> Map.filter (fun _ s  -> match s.Component.Type with
//                                                 | Custom _ -> true
//                                                 | _ -> false)
//                      |> Map.filter (fun _ s -> s.Component.InputPorts > 1)
//                      |> Map.filter (fun k s -> BusWireUpdateHelpers.getConnectedWires k model |> List.length > 1)
//                      |> Map.toList

/// <summary>
/// Aligns and scales symbols based on the positions and bounding boxes of connected symbols.
/// </summary>
/// <param name="model">The sheet model containing wires and symbols.</param>
let sheetAlignScale (model: SheetT.Model) =
    let syms = getAllSingleConnectedSymbols model (parallelWires model)
    printfn "Running SheetAlign %A" <| List.map (fun(s,t,w)->(s.Component.Label, t.Component.Label)) syms

    let rec runAlignSingleConnectedSyms model symList count =
        match symList with
        | [] -> model
        | _ ->
            if List.length symList > 0 && count < 10 then //
                let model' = alignSingleConnectedSyms model symList
                let updatedSymList = getAllSingleConnectedSymbols model' (parallelWires model')
                let symbolsOnly1 = List.map (fun (sym, _, _) -> sym) updatedSymList
                let symbolsOnly2 = List.map (fun (_, sym, _) -> sym) updatedSymList
                let overlaps1 = detectOverlaps symbolsOnly1
                let overlaps2 = detectOverlaps symbolsOnly2
                let resolvedSymList1 = resolveOverlaps overlaps1 symbolsOnly1
                let resolvedSymList2 = resolveOverlaps overlaps2 symbolsOnly2
                let mappedResolvedSymList1 = List.map2 (fun (_, sym, wire) resolvedSym -> (resolvedSym, sym, wire)) updatedSymList resolvedSymList1
                let mappedResolvedSymList2 = List.map2 (fun (sym, _, wire) resolvedSym -> (sym, resolvedSym, wire)) mappedResolvedSymList1 resolvedSymList2
                runAlignSingleConnectedSyms model' mappedResolvedSymList2 (count + 1)
            else
                model
    
    runAlignSingleConnectedSyms model syms 0

//--------------------------------------------------------------------------------------------------------------------------//
//----------------------------------------------- D2 Implementation --------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//

//----------------------------------------------- Exhaustive Search --------------------------------------------------------//
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

    flipSymbol compLabel FlipHorizontal model
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
                    | Mux2
                    | Mux4
                    | Mux8
                    | Demux2
                    | Demux4
                    | Demux8
                    | NbitsNot(_)
                    | NbitsAnd(_)
                    | NbitsXor(_)
                    | NbitsOr(_)
                    | NbitsAdder(_)
                    | NbitsAdderNoCin(_)
                    | NbitsAdderNoCout(_)
                    | NbitsAdderNoCinCout(_)
                    | IOLabel
                    | DFF
                    | DFFE
                    | Register(_)
                    | RegisterE(_)
                    | Counter(_)
                    | CounterNoLoad(_)
                    | CounterNoEnable(_)
                    | CounterNoEnableLoad(_)
                    | AsyncROM(_)
                    | ROM1(_)
                    | RAM1(_)
                    | AsyncRAM1(_) -> Some id
                    | _ -> None
                | None -> None)
        evaluateAllComponents currentModel componentsToEvaluate

    let updatedModel = List.fold evaluateSubCircuit model subCircuits

    updatedModel

//--------------------------------------------------------------------------------------------------------------------------//

//--------------------------------------------- Iterated Local Search ------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//

/// Generates possible permutations for one component at a time, evaluating which permutation is best.
/// Carried out on all components to achieve a local minimum
/// Generates possible permutations for one component at a time, evaluating which permutation is best.
/// Continues to optimize each component until no further improvements can be made.

let d2iteratedLocalSearchSingleComponent (initialModel: SheetT.Model) : SheetT.Model =
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

//--------------------------------------------------------------------------------------------------------------------------//
//----------------------------------------------- D3 Implementation --------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//

/// Find the symbols connected to each end of a wire and the respective portIds that the wire is connected to
let getWireSymbolsAndPort (wireId: ConnectionId) (model: SheetT.Model) =
    let wire = model.Wire.Wires.[wireId]
    let sourceSymbol = getSourceSymbol model.Wire wire
    let targetSymbol = getTargetSymbol model.Wire wire

    let targetPortId = InputId wire.InputPort
    let sourcePortId = OutputId wire.OutputPort

    ((sourceSymbol, sourcePortId), (targetSymbol, targetPortId))

/// Places a wire layer symbol given the desired name of that label and position
let placeWireLabelSymbol
    (symLabel: string)
    (model: SheetT.Model)
    (position: XYPos)
    : Result<(Model * ComponentId), string>
    =
    let symLabel = String.toUpper symLabel
    let wlSymModel, wlSymId =
        SymbolUpdate.addSymbol [] (model.Wire.Symbol) position IOLabel symLabel
    let wlSym = wlSymModel.Symbols[wlSymId]
    match position + wlSym.getScaledDiagonal with
    | { X = x; Y = y } when x > maxSheetCoord || y > maxSheetCoord ->
        Error $"symbol '{symLabel}' position {position + wlSym.getScaledDiagonal} lies outside allowed coordinates"
    | _ ->
        let updatedModel =
            model
            |> Optic.set symbolModel_ wlSymModel
            |> updateBoundingBoxes
        Ok(updatedModel, wlSymId)

/// Places a wire given source and target symbol SymbolPortRecs.
/// SymbolPortRecs is a record containing the symbol id and port number of each symbol.
let placeWireX
    (source: SymbolPortRec)
    (target: SymbolPortRec)
    (model: SheetT.Model)
    : Result<SheetT.Model * ConnectionId, string>
    =
    let getPort (symPortRec: SymbolPortRec) (portType: PortType) =
        match
            model.Wire.Symbol.Symbols
            |> Map.tryFind symPortRec.SymbolId
        with
        | Some symbol ->
            let ports =
                match portType with
                | PortType.Input -> symbol.Component.InputPorts
                | PortType.Output -> symbol.Component.OutputPorts
            match ports |> List.tryItem symPortRec.PortNumber with
            | Some port -> Ok port.Id
            | None ->
                Error $"Can't find {portType} port {symPortRec.PortNumber} on symbol with ID {symPortRec.SymbolId}"
        | None -> Error $"Can't find symbol with ID {symPortRec.SymbolId}"

    match getPort source PortType.Output, getPort target PortType.Input with
    | Error e, _
    | _, Error e -> Error e
    | Ok outPort, Ok inPort ->
        let newWire =
            BusWireUpdate.makeNewWire (InputPortId inPort) (OutputPortId outPort) model.Wire
        if
            model.Wire.Wires
            |> Map.exists (fun wid wire ->
                wire.InputPort = newWire.InputPort
                && wire.OutputPort = newWire.OutputPort)
        then
            Error "Can't create wire because a wire already exists between those ports"
        else
            let updatedModel =
                model
                |> Optic.set (busWireModel_ >-> BusWireT.wireOf_ newWire.WId) newWire
            Ok(updatedModel, newWire.WId)

// Delete the wire with the corresponding wireId.
let deleteWire (wireId: ConnectionId) (model: SheetT.Model) =
    let newWires =
        model.Wire.Wires
        |> Map.filter (fun id _ -> id <> wireId)

    { model with Wire = { model.Wire with Wires = newWires } }

/// Ensures the wire label rotation is correct for all sides, orientations and flips of the connected symbol.
/// Takes in the id of both the wire label, connected source/target symbol and the portId on the source/target symbol that the wire label is connected too.
let autoAdjustWLRotation
    (wlSymId: ComponentId)
    (symId: ComponentId)
    (portId: PortId)
    (model: SheetT.Model)
    : SheetT.Model
    =
    let symbolsMap = model.Wire.Symbol.Symbols
    let symLens = symbolOf_ symId
    let sym = Optic.get symLens model
    let symRotation = Optic.get symbol_rotation_ sym
    let symFlip = Optic.get symbol_flipped_ sym

    match symbolsMap |> Map.tryFind wlSymId with
    | Some wireLabel ->
        let portEdge = getPortOrientation model.Wire.Symbol portId
        let adjustedSymbol =
            match portEdge with
            | Top
            | Bottom ->
                let rotationCheck wireLabel =
                    match symRotation with
                    | Degree180
                    | Degree270 -> SymbolResizeHelpers.rotateSymbol Degree270 wireLabel
                    | _ -> SymbolResizeHelpers.rotateSymbol Degree90 wireLabel

                let flipCheck wireLabel =
                    match symFlip with
                    | true -> wireLabel
                    | false -> wireLabel

                wireLabel |> rotationCheck |> flipCheck
            | Left
            | Right ->
                let rotationCheck wireLabel =
                    match symRotation with
                    | Degree180
                    | Degree90 -> SymbolResizeHelpers.rotateSymbol Degree180 wireLabel
                    | _ -> wireLabel

                let flipCheck wireLabel =
                    match symFlip with
                    | true -> SymbolResizeHelpers.rotateSymbol Degree180 wireLabel
                    | false -> wireLabel

                wireLabel |> rotationCheck |> flipCheck
        let updatedSymbols = Map.add wlSymId adjustedSymbol symbolsMap
        { model with
            Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbols } } }
    | None -> model

/// Places and connects a wire label to a desired symbol.
/// <param name="wlName">The name of the wire label being placed.</param>
/// <param name="portId">The id of the port to connect the wire label to.</param>
/// <param name="symId">The id of the symbol the wire label is being connected to.</param>
/// <param name="isSourceWL">Boolean parameter to indicate whether the symbol is a source or a target symbol.</param>
let addWireLabel
    (wlName: string)
    (portId: PortId)
    (symId: ComponentId)
    (isSourceWL: bool)
    (model: SheetT.Model)
    : (SheetT.Model * string)
    =
    let portIdStr = getPortIdStr portId
    let portPos = SheetBeautifyHelpers.getPortPos portIdStr model.Wire.Symbol

    let PortIndex =
        match model.Wire.Symbol.Symbols |> Map.tryFind symId with
        | Some symbol ->
            let allPorts =
                if isSourceWL then
                    symbol.Component.OutputPorts
                else
                    symbol.Component.InputPorts

            allPorts
            |> List.mapi (fun idx port -> (idx, port.Id))
            |> List.tryFind (fun (_, pid) -> pid = portIdStr)
            |> Option.map fst
            |> Option.get
        | None -> failwith "Symbol ID not found in model"

    let wireLabelName =
        if isSourceWL then
            wlName + "/" + string PortIndex
        else
            wlName

    let calcWLPos (portPos: XYPos) =
        let portEdge = getPortOrientation model.Wire.Symbol portId

        match portEdge with
        | Right -> { X = portPos.X + 40.0; Y = portPos.Y }
        | Left -> { X = portPos.X - 40.0; Y = portPos.Y }
        | Bottom -> { X = portPos.X + 15.0; Y = portPos.Y + 40.0 }
        | Top -> { X = portPos.X + 15.0; Y = portPos.Y - 40.0 }

    let placeWlAtPort (model: SheetT.Model) (portPos: XYPos) =
        portPos
        |> calcWLPos
        |> placeWireLabelSymbol wireLabelName model

    match placeWlAtPort model portPos with
    | Ok(updatedModel, wlSymId) ->
        let wlRec = { SymbolId = wlSymId; PortNumber = 0 }
        let symRec = { SymbolId = symId; PortNumber = PortIndex }

        let modelWithAdjustedWL = autoAdjustWLRotation wlSymId symId portId updatedModel

        let wireResult =
            if isSourceWL then
                placeWireX symRec wlRec modelWithAdjustedWL
            else
                placeWireX wlRec symRec modelWithAdjustedWL

        match wireResult with
        | Ok(finalModel, _) ->
            printfn "Wire label name %A" wireLabelName
            (finalModel, wireLabelName)
        | Error wireError ->
            printfn "Error placing wire: %s" wireError
            (updatedModel, wireLabelName)

    | Error errMsg ->
        printfn "Error placing wire label: %s" errMsg
        (model, wireLabelName)

/// High level D3 function that replaces wires that exceed the length threshold, with wire labels.
let replaceLongWiresWithLabels (model: SheetT.Model) (lengthThreshold: float) : SheetT.Model =
    let findLongWires =
        model.Wire.Wires
        |> Map.toList
        |> List.filter (fun (_, wire) ->
            let wireLength = getWireLength wire
            wireLength > lengthThreshold)
        |> List.map fst

    let replaceEachWire (model: SheetT.Model) (wireId: ConnectionId) =
        let (sourceSymbol, sourcePortId), (targetSymbol, targetPortId) =
            getWireSymbolsAndPort wireId model
        printfn "SourceSymbol: %A" sourceSymbol.Component.Label
        printfn "TargetSymbol: %A" targetSymbol.Component.Label
        let (modelWithSourceLabels, newWLName) =
            addWireLabel sourceSymbol.Component.Label sourcePortId sourceSymbol.Id true model
        let (modelWithTargetLabels, _) =
            addWireLabel newWLName targetPortId targetSymbol.Id false modelWithSourceLabels

        deleteWire wireId modelWithTargetLabels

    findLongWires |> List.fold replaceEachWire model

// Checks if a symbol is a label
let isLabel (symbolId: ComponentId) (model: SheetT.Model) =
    match model.Wire.Symbol.Symbols |> Map.tryFind symbolId with
    | Some symbol -> symbol.Component.Type = IOLabel
    | None -> false

// Checks if a wire is connected to any label
let wireConnectedToLabel (wireId: ConnectionId, wire: BusWireT.Wire) (model: SheetT.Model) =
    let (sourceSymbol, _), (targetSymbol, _) = getWireSymbolsAndPort wireId model
    isLabel sourceSymbol.Id model
    || isLabel targetSymbol.Id model

/// Finds wires that are connected at one end to a wire label component
let findWiresConnectedToLabels (model: SheetT.Model) =
    let wires = model.Wire.Wires |> Map.toList

    wires
    |> List.choose (fun (wireId, wire) ->
        if wireConnectedToLabel (wireId, wire) model then
            Some wireId
        else
            None)

/// Finds port index of a specific component. Used for port record formation.
let getPortIndex (portId: PortId) (symId: ComponentId) (model: SheetT.Model) =
    let portIdStr: string = getPortIdStr portId

    let findPortIndexInList (portsList: Port list) =
        portsList
        |> List.mapi (fun idx port -> (idx, port.Id))
        |> List.tryFind (fun (_, pid) -> pid = portIdStr)
        |> Option.map fst

    match model.Wire.Symbol.Symbols |> Map.tryFind symId with
    | Some symbol ->
        let outputPortIndex = findPortIndexInList symbol.Component.OutputPorts
        match outputPortIndex with
        | Some idx -> (idx, false)
        | None ->
            match findPortIndexInList symbol.Component.InputPorts with
            | Some idx -> (idx, true)
            | None -> failwith "Port ID not found in model"
    | None -> failwith "Symbol ID not found in model"

/// Used to get port record for the non wire label component, wire label name and port type (input or output port)
let getNonLabelEndDetails (wireId: ConnectionId) (model: SheetT.Model) =
    let ((sourceSymbol, sourcePortId), (targetSymbol, targetPortId)) =
        getWireSymbolsAndPort wireId model
    let isSourceLabel = sourceSymbol.Component.Type = IOLabel
    let isTargetLabel = targetSymbol.Component.Type = IOLabel
    printf "%b,%b" isSourceLabel isTargetLabel

    match isSourceLabel, isTargetLabel with
    | true, false ->
        printf "type: %A" targetSymbol.Component.Type
        let number, isInputPort = getPortIndex targetPortId targetSymbol.Id model
        Some({ SymbolId = targetSymbol.Id; PortNumber = number }, sourceSymbol.Component.Label, isInputPort)
    | false, true ->
        printf "type: %A" sourceSymbol.Component.Type
        let number, isInputPort = getPortIndex sourcePortId sourceSymbol.Id model
        Some({ SymbolId = sourceSymbol.Id; PortNumber = number }, targetSymbol.Component.Label, isInputPort)
    | _ -> None

/// Helper function to find all possible pairs in a list
let rec allPairs lst =
    match lst with
    | [] -> []
    | hd :: tl -> List.map (fun x -> (hd, x)) tl @ allPairs tl

/// Helper function for pair processing that checks port type label flags and ensures wire labels are the same
let processPairs pairs =
    pairs
    |> List.choose (fun ((end1, label1, flag1), (end2, label2, flag2)) ->
        match flag1, flag2 with
        | false, true when label1 = label2 -> Some((end1, end2), label1)
        | true, false when label1 = label2 -> Some((end2, end1), label1)
        | _ -> None)

/// Replaces Wire labels with wires if potential wire between wire labels is below threshold
let replaceLabelsWithWires (model: SheetT.Model) (lengthThreshold: float) : SheetT.Model =
    let wireEndsAndLabels =
        model.Wire.Wires
        |> Map.toList
        |> List.choose (fun (wireId, _) ->
            match getNonLabelEndDetails wireId model with
            | Some detailsAndLabel -> Some detailsAndLabel
            | None -> None)

    let groupedByLabelAndCombinations =
        wireEndsAndLabels
        |> List.groupBy (fun (_, label, _) -> label)
        |> List.collect (fun (_, group) -> group |> allPairs |> processPairs)

    // let componentsWithLabel (label: string) (model: SheetT.Model) =
    //     model.Wire.Symbol.Symbols
    //     |> Map.toList
    //     |> List.choose (fun (id, sym) ->
    //         if caseInvariantEqual sym.Component.Label label then
    //             Some id
    //         else
    //             None)

    // let deleteSymbols (model: SheetT.Model) compIds =
    //     let newSymbols =
    //         (model.Wire.Symbol.Symbols, compIds)
    //         ||> List.fold (fun prevModel sId -> Map.remove sId prevModel)
    //     { model with Wire.Symbol.Symbols = newSymbols }

    /// Checks each matched pair of ports that are connected by wire label and replaces if wire below threshold.
    let processPair (model: SheetT.Model) (((end1, end2), label): (SymbolPortRec * SymbolPortRec) * string) =
        printf "%A,%A" end1 end2
        match placeWireX end1 end2 model with
        | Ok(updatedModel, wireId) ->
            let wire =
                if Map.containsKey wireId updatedModel.Wire.Wires then
                    Map.find wireId updatedModel.Wire.Wires
                else
                    failwithf "Wire with ID %A not found" wireId
            let wireLength = getWireLength wire
            if wireLength <= lengthThreshold then
                printf "Label: %A" label
                //deleteSymbols updatedModel (componentsWithLabel label updatedModel)
                updatedModel
            else
                let modelWithWireRemoved = deleteWire wireId updatedModel
                modelWithWireRemoved
        | Error errMsg ->
            printfn "Error placing wire: %s" errMsg
            model

    List.fold processPair model groupedByLabelAndCombinations

/// Overall function that implements both replacing long wires with labels and replacing wire labels if a wire between them is below threshould
let d3Function (lengthThreshold: float) (model: SheetT.Model) : SheetT.Model =
    let removedWiresModel = replaceLongWiresWithLabels model lengthThreshold
    replaceLabelsWithWires removedWiresModel lengthThreshold

//--------------------------------------------------------------------------------------------------------------------------//
//------------------------------------------ Top level Beautify Function ---------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//
let integratedBeautify (model: SheetT.Model) (userThreshold: float) : SheetT.Model =
    model
    |> d2iteratedLocalSearchSingleComponent
    |> d3Function userThreshold
    |> sheetAlignScale
