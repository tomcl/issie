module SheetBeautifyD2

open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open SheetBeautifyHelpers
open Optics
open Optics.Operators
open Helpers
open BlockHelpers

let symbolModel_ = SheetT.symbol_

/// Checks if a component type is a MUX or DEMUX type.
let isMuxType (componentType: ComponentType) : bool = 
    [Mux2; Mux4; Mux8; Demux2; Demux4; Demux8] 
    |> List.contains componentType

/// Checks if a component type is a Gate type.
let isGateType (componentType: ComponentType) : bool = 
    match componentType with
    | GateN _ -> true
    | _ -> false

/// Checks if a component type is a Custom Component type.
let isCustomType (componentType: ComponentType) : bool = 
    match componentType with
    | Custom _ -> true
    | _ -> false


/// Returns symbols that are of given component type.
/// where isSymbolType is a function that returns a bool for a given component type.
let getCompTypeSymbols (isSymbolType) (model: SheetT.Model) : Symbol list =
    mapValues model.Wire.Symbol.Symbols
    |> Array.filter (fun sym -> isSymbolType sym.Component.Type)
    |> Array.toList

/// Checks if two symbols are directly connected through any wire.
let areSymbolsConnected (wModel: BusWireT.Model) symA symB =
    wModel.Wires
    |> Map.exists (fun _ wire -> isConnBtwnSyms wire symA symB)


/// Generates all permutations for given number of MUX and Gate components.
/// where MUX components have 4 states, Gate components have 2 states
let generateMuxGatePermutations  (muxCount: int, gateCount: int) =
    let muxStates = [(false, false); (false, true); (true, false); (true, true)]
    let gateStates = [false; true]

    let rec generatePermutations depth states acc =
        match depth with
        | 0 -> acc
        | _ -> 
            let nextAcc = 
                acc |> List.collect (fun accPerm -> 
                    states |> List.map (fun state -> 
                        state :: accPerm))
            generatePermutations (depth - 1) states nextAcc

    let muxPermutations = generatePermutations muxCount muxStates [[]]
    let gatePermutations = generatePermutations gateCount gateStates [[]]

    List.allPairs muxPermutations gatePermutations

//let printMixedPermutations (muxCount: int, gateCount: int) =
//    let mixedPermutations = makeMixedPermutations (muxCount, gateCount)
//    let stateToString (state: bool) = if state then "1" else "0"
//    let muxStateToString (state: (bool * bool)) =
//        let (rev, flip) = state
//        sprintf "[%s,%s]" (stateToString rev) (stateToString flip)
//    mixedPermutations |> List.iter (fun (muxPerm, gatePerm) ->
//        let muxStatesStr = muxPerm |> List.map muxStateToString 
//        let gateStatesStr = gatePerm |> List.map stateToString 

//        printf $"MUX: {muxStatesStr} | Gates: {gateStatesStr}"
//    )


/// Applies permutation of MUX and Gate states to a sheet
let genSheetFromMuxGatePermutation (model: SheetT.Model)
                                   (muxSymbols: Symbol list, gateSymbols: Symbol list)
                                   (muxPermutation: (bool * bool) list, gatePermutation: bool list) =
    let allComponents =
        (muxSymbols @ gateSymbols)
        |> List.map (fun sym -> sym.Id)

    let updateGates accModel (flipped, sym) =
        if flipped then
            SheetBeautifyHelpers.flipSymbol sym.Component.Label FlipVertical accModel
        else accModel

    let updateMuxes accModel ((reversed, flipped), sym) =
        let symReversed = Optic.set reversedInputPorts_ (Some reversed) sym
        if flipped then
            SheetBeautifyHelpers.flipSymbol symReversed.Component.Label FlipVertical accModel
        else accModel

    let updatedGates =
        List.fold updateGates model (List.zip gatePermutation gateSymbols)

    let updatedModel =
        List.fold updateMuxes updatedGates (List.zip muxPermutation muxSymbols)

    let reroutedModel = BusWireSeparate.reRouteWiresFrom allComponents updatedModel.Wire
    { updatedModel with Wire = reroutedModel }


/// Objective function used to evaluate the quality of a sheet
/// Lower is better
let calculateSheetObjective (model: SheetT.Model) : int =
    // TODO: objective function : #wire crossings, #right angles ?
    numOfWireRightAngleCrossings model


/// Exhaustive search through all permutations of MUX and Gate flips to find the sheet with the lowest objective.
let permuteMuxGateState (model: SheetT.Model) (muxSymbols: Symbol list, gateSymbols: Symbol list) : SheetT.Model =
    let muxCount = List.length muxSymbols
    let gateCount = List.length gateSymbols

    let permutations = generateMuxGatePermutations (muxCount, gateCount)
    let originalObjective = calculateSheetObjective model
    printf $"Permutation length: {permutations.Length}\n"

    // Generate all permuted sheets
    // filtering out the ones that have worse objective than original
    let permutedSheets = 
        permutations
        |> List.map (fun (muxPerm, gatePerm) ->
                genSheetFromMuxGatePermutation model (muxSymbols, gateSymbols) (muxPerm, gatePerm))
        |> List.filter (fun updatedSheet ->
                calculateSheetObjective updatedSheet < originalObjective)

    // Return the sheet with the lowest objective, if any
    match permutedSheets with
    | [] -> model
    | _ -> permutedSheets |> List.minBy calculateSheetObjective


/// Groups directly connected gates and muxes for localised permuting.
/// Doesn't produce isolated, distinct groups
let groupConnectedSymbols (model: SheetT.Model) (symbols: Symbol list) : Symbol list list =
    let wModel = model.Wire

    let getConnectedSymbols symA =
        symbols
        |> List.filter (fun symB -> symA <> symB && areSymbolsConnected wModel symA symB)
        |> List.append [symA]
        |> List.sortBy (fun sym -> sym.Id)

    let getSymbolById id =
        symbols |> List.find (fun sym -> sym.Id = id)

    symbols
    |> List.map getConnectedSymbols
    |> List.map (List.map (fun sym -> sym.Id))
    |> List.distinct
    |> List.map (List.map getSymbolById)


// Group by splitting into Symbol Lists of connected gates + muxes (only directly connected)
// Generate all the permutations for group
// Apply permutations to sheet + autoroute + filter to find best group sheet
// Use that local best going forwards for next group - until all groups done
let optimiseGroupedMuxGate (model: SheetT.Model) : SheetT.Model =
    let muxSymbols = getCompTypeSymbols isMuxType model
    let gateSymbols = getCompTypeSymbols isGateType model
    let allSymbols = muxSymbols @ gateSymbols

    let groupedSymbols = groupConnectedSymbols model allSymbols
    //groupedSymbols |> List.iteri (fun i group ->
    //    printfn "Group %d size: %d" (i + 1) (List.length group))

    // Iterate over each group, updating sheet iteratively with the best permutation found for each group
    let finalSheet = 
        groupedSymbols
        |> List.fold (fun currentSheet group ->
            let groupMuxSymbols = 
                List.filter (fun sym -> isMuxType sym.Component.Type) group
            let groupGateSymbols = 
                List.filter (fun sym -> isGateType sym.Component.Type) group
            permuteMuxGateState currentSheet (groupMuxSymbols, groupGateSymbols)
        ) model

    finalSheet


/// Calculates the angle for an edge based on the centre and destination port positions.
let calculateAngleForEdge (centre: XYPos) (destPort: XYPos) (edge: Edge) =
    let deltaX = destPort.X - centre.X
    // Invert deltaY for top and bottom edges
    let deltaY = 
        match edge with
        | Top | Bottom -> centre.Y - destPort.Y  // Invert deltaY for top and bottom edges
        | Left | Right -> destPort.Y - centre.Y 
    
    let angleRadians = atan2 deltaY deltaX
    // printf $"Edge: {edge}, destPort: {destPort}, angle: {angleRadians}"
    angleRadians


/// Gets the centre position of a custom component symbol.
let getCustomCompCentre (sym: Symbol) : XYPos =
    let dims = getCustomCompDims sym
    // calculate centre of custom component symbol
    { X = sym.Pos.X + dims.X / 2.0; Y = sym.Pos.Y + dims.Y / 2.0 }


/// Gets the connected port ID for a given port ID.
let getConnectedPort (model: SheetT.Model) (portID: string) =
    let wires = 
        model.Wire.Wires
        |> Map.filter (fun _ wire -> 
            wire.InputPort = InputPortId portID || wire.OutputPort = OutputPortId portID)
        |> Map.toList

    wires
    |> List.tryPick (fun (_, wire) ->
        match wire.InputPort, wire.OutputPort with
        | InputPortId pid, _ when pid <> portID -> Some pid
        | _, OutputPortId pid when pid <> portID -> Some pid
        | _ -> None
    )
    

/// Reorders the ports of a custom component symbol by angle.
let reorderPortsByAngle (sym: Symbol) (model: SheetT.Model) : Symbol =
    let centre = getCustomCompCentre sym

    // Function to process and reorder ports for a given edge
    let processEdge sym edge =
        let portIds = getPortOrder edge sym

        // Separate connected and unconnected ports
        let connectedPorts, unconnectedPorts =
            portIds
            |> List.partition (fun portId -> getConnectedPort model portId |> Option.isSome)

        let connectedPortAngles =
            connectedPorts
            |> List.map (fun portId ->
                let connectedPortId = getConnectedPort model portId |> Option.get
                let portPos = getPortPos connectedPortId model.Wire.Symbol
                let angle = calculateAngleForEdge centre portPos edge
                (portId, angle))
            |> List.sortBy snd
            |> List.map fst
        
        let sortedPortIds = connectedPortAngles @ unconnectedPorts

        putPortOrder edge sortedPortIds sym

    [Edge.Top; Edge.Bottom; Edge.Left; Edge.Right]
    |> List.fold processEdge sym


/// Applies custom component port reordering to a sheet.
let applyCustomCompReorder (model: SheetT.Model) (customCompSym: Symbol) : SheetT.Model =
    let reorderedCustomComp = reorderPortsByAngle customCompSym model
    let updatedModel = 
        model
        |> Optic.set (symbolModel_ >-> SymbolT.symbolOf_ reorderedCustomComp.Id) reorderedCustomComp
    let reroutedModel = BusWireSeparate.reRouteWiresFrom [customCompSym.Id] updatedModel.Wire

    { updatedModel with Wire = reroutedModel }


/// Iteratively optimise custom component ports by reordering them per edge
let optimiseCustomCompPorts (sheet: SheetT.Model) : SheetT.Model =
    let customCompSymbols = getCompTypeSymbols isCustomType sheet
    let originalObjective = calculateSheetObjective sheet

    let optimisedSheet =
        customCompSymbols
        |> List.fold (fun currentSheet customCompSym ->
            let reorderedSheet = applyCustomCompReorder currentSheet customCompSym
            let reorderedObjective = calculateSheetObjective reorderedSheet
            if reorderedObjective < calculateSheetObjective currentSheet then
                reorderedSheet
            else
                currentSheet
        ) sheet

    if calculateSheetObjective optimisedSheet < originalObjective then
        optimisedSheet
    else
        sheet


/// For testing: Reorder all custom component ports on sheet
/// TODO: Consider custom connected to custom 
let reorderAllCustomCompPorts (sheet: SheetT.Model) : SheetT.Model =
    let customCompSymbols = getCompTypeSymbols isCustomType sheet
    printfn $"Custom Comp Count {List.length customCompSymbols}"
    customCompSymbols
    |> List.fold (fun currentSheet customCompSym ->
        applyCustomCompReorder currentSheet customCompSym
    ) sheet
