module SheetBeautify

//-----------------------------------------------------------------------------------------//
//----------Module for top-level beautify functions making (mostly) whole-sheet changes----//
//-----------------------------------------------------------------------------------------//

(*
Whole sheet functions are normally applied to whole sheet. In many cases a feature could be to
apply them to currently selected wires or components. That provides users control over what gets beautified.
Ideal beautify code will never make things worse so can be applied to whole sheet always.

Otehr relevant modules:
SheetBeautifyHelpers (helpers)
Generatedata (used to make randomized tests)
TestDrawBlock (used to test the code written here).

*)

// open modules likely to be used
open Browser
open Elmish
open Fable.React
open CommonTypes
open ModelType
open DrawHelpers
open DrawModelType
//open DrawModelType.SymbolT
//open DrawModelType.BusWireT
//open DrawModelType.SheetT
open SheetUpdateHelpers
open SheetBeautifyHelpers
open SheetBeautifyHelpers.SegmentHelpers
open Optics
open BlockHelpers
open BusWireRoute
open RotateScale
open Symbol


type Directions = DIR_Left | DIR_Right | DIR_Up | DIR_Down
type SymbolClassification = ClassInput | ClassOutput
type StraightWireDirection = Horizontal | Vertical | Unknown | NotStraight

/// Record type to store the information required to move a symbol
type moveSymInformation =
    {
        SymbolToMove: SymbolT.Symbol
        SymbolRef: SymbolT.Symbol
        Movements: (Directions * float) list
    }

let createMovementSymbolWire (symbolToMove: SymbolT.Symbol) (symbolRef: SymbolT.Symbol) (movements: (Directions * float) list) : moveSymInformation =
    { SymbolToMove = symbolToMove; SymbolRef = symbolRef; Movements = movements }

let checkWiresToBeStraightened (model: SheetT.Model) =
    let wires = model.Wire.Wires

    let checkWireCanBeStraightened (wId, wire: BusWireT.Wire) : XYPos list option =
        let wireVertices =
            visibleSegments wId model
            |> List.filter (fun segs -> not (segs =~ XYPos.zero))
        match wireVertices with
        | [a; b; c] -> Some [a; b; c] // Explicitly match a list with 3 elements
        | [a ;b ; c ;d] -> Some [a; b; c; d]
        | [a; b; c; d; e] -> Some [a; b; c; d; e]
        | _ -> None

    let wireSegments =
        wires
        |> Map.toList
        |> List.choose (fun (wId, wire) -> 
            checkWireCanBeStraightened (wId, wire)
            |> Option.map (fun segments -> (wire, segments)))

    wireSegments

/// symbol singly connected
/// returns true if the symbol is singly connected
/// A symbol is singely connected if it has only one port and from that one port 1 wire connection
let isSymbolSinglyConnected (symbol: SymbolT.Symbol) (model: SheetT.Model) : bool=
    let symbolOutputPorts = symbol.Component.OutputPorts
    let symbolInputPorts = symbol.Component.InputPorts
    match symbolOutputPorts.Length, symbolInputPorts.Length with
    | 1, 0 ->
        let outputPort = symbolOutputPorts.[0]
        let connectedWires = model.Wire.Wires |> Map.filter (fun _ wire -> wire.OutputPort = OutputPortId outputPort.Id)
        match connectedWires |> Map.count with
        | 1 -> true
        | _ -> false
    | 0, 1 -> 
        let inputPort = symbolInputPorts.[0]
        let connectedWires = model.Wire.Wires |> Map.filter (fun _ wire -> wire.InputPort = InputPortId inputPort.Id)
        match connectedWires |> Map.count with
        | 1 -> true
        | _ -> false
    | _ -> false

// TODO: should only return a symbol if labeled to do so
/// Finds the source port of the wire and returns a tuple of the wire, segments and the symbol
let findOutputSymbol (model: SheetT.Model) (wire: BusWireT.Wire, segments:XYPos list) : (BusWireT.Wire * XYPos list * SymbolT.Symbol*SymbolClassification) Option =
    // need to identif the symbol or component and move it by the amount stated by the second segment
    let symbolMap = model.Wire.Symbol.Symbols
        // Assuming symbolMap is a Map<componentId, Symbol>
        // Find the symbol whose component's output port list contains the wire's OutputPortId
    let matchingSymbolOption =
        symbolMap
        |> Map.toList
        |> List.tryFind (fun (_, symbol) ->
            symbol.Component.OutputPorts |> List.exists (fun port -> OutputPortId port.Id = wire.OutputPort) )

    match matchingSymbolOption with
    | Some (_, symbol) ->
        Some (wire, segments, symbol, ClassOutput)
    | None -> None
// not combined with the above function as later in the code only one of these function is called whilst in other places both are called
/// Finds the destination port of the wire and returns a tuple of the wire, segments and the symbol
let findInputSymbol (model: SheetT.Model) (wire: BusWireT.Wire, segments:XYPos list) : (BusWireT.Wire * XYPos list * SymbolT.Symbol*SymbolClassification) Option =
    // need to identif the symbol or component and move it by the amount stated by the second segment
    let symbolMap = model.Wire.Symbol.Symbols
        // Assuming symbolMap is a Map<componentId, Symbol>
        // Find the symbol whose component's output port list contains the wire's OutputPortId
    let matchingSymbolOption =
        symbolMap
        |> Map.toList
        |> List.tryFind (fun (_, symbol) ->
             symbol.Component.InputPorts |> List.exists (fun port -> InputPortId port.Id = wire.InputPort))

    match matchingSymbolOption with
    | Some (_, symbol) ->
        Some (wire, segments, symbol, ClassInput)
    | None -> None

// TODO: Need to redo this
let simplifyMovements (movements: (Directions * float) list) =
    // Count the occurrences of each direction
    let counts = 
        movements 
        |> List.fold (fun (left, right, up, down) (direction, _) ->
            match direction with
            | DIR_Left -> (left + 1, right, up, down)
            | DIR_Right -> (left, right + 1, up, down)
            | DIR_Up -> (left, right, up + 1, down)
            | DIR_Down -> (left, right, up, down + 1)
        ) (0, 0, 0, 0)

    let (countLeft, countRight, countUp, countDown) = counts

    // Check for equal counts in opposing directions
    if countLeft = countRight || countUp = countDown then // anedge case that can result in error if simplified 
        movements
    else
        let summedMovements = 
            movements
            |> List.fold (fun (left, right, up, down) (direction, distance) ->
                match direction with
                | DIR_Left -> (left + distance, right, up, down)
                | DIR_Right -> (left, right + distance, up, down)
                | DIR_Up -> (left, right, up + distance, down)
                | DIR_Down -> (left, right, up, down + distance)
            ) (0.0, 0.0, 0.0, 0.0)
        
        let (sumLeft, sumRight, sumUp, sumDown) = summedMovements
        
        // Combine movements in the same direction and subtract opposing movements
        [
            if sumLeft > sumRight then Some (DIR_Left, sumLeft - sumRight) else None
            if sumRight > sumLeft then Some (DIR_Right, sumRight - sumLeft) else None
            if sumUp > sumDown then Some (DIR_Up, sumUp - sumDown) else None
            if sumDown > sumUp then Some (DIR_Down, sumDown - sumUp) else None
        ]
        |> List.choose id

/// Checks that the input and output ports of the wire are opposite to each other and returns true if they are
let checkInOutPortOpp (model: SheetT.Model) (wire: BusWireT.Wire) =
    // find the input symbol
    let inputSymbol =
        match findInputSymbol model (wire, visibleSegments wire.WId model) with
        | Some (wire, segments, symbol, symbolType) -> Some symbol
        | _ -> None
    // find the output symbol
    let outputSymbol =
        match findOutputSymbol model (wire, visibleSegments wire.WId model) with
        | Some (wire, segments, symbol, symbolType) -> Some symbol
        | _ -> None
    match inputSymbol, outputSymbol with
    | Some inputSymbol, Some outputSymbol ->
        let inputPort, outputPort = getPortAB  model.Wire {SymA = inputSymbol; SymB = outputSymbol; Wire = wire}

        let inputPortOrientation = getSymbolPortOrientation inputSymbol inputPort
        let outputPortOrientation = getSymbolPortOrientation outputSymbol outputPort

        match (inputPortOrientation: Edge), (outputPortOrientation: Edge) with
        | Left , Right -> true
        | Right, Left -> true
        | Top, Bottom -> true
        | Bottom, Top -> true
        | _ ->
            printfn "Symbol IN: %A, and Symbol OUT: %A Input and output ports are not opposite" inputSymbol.Component.Label outputSymbol.Component.Label
            false
    | _, _ ->   false 


/// calculates the movements required to straighten the wire
/// returns the steps required to straighten the wire in a list of tuples of direction and distance as well as the symbol and wire
/// Returns an option type
let calcMovementsToStraightenWire (model: SheetT.Model) (wire: BusWireT.Wire, segments: XYPos list, symbol: SymbolT.Symbol, symbolType:SymbolClassification): (SymbolT.Symbol *  BusWireT.Wire * (Directions*float) list) Option =
    let absSegments = segments |> List.scan (fun acc relativePos -> acc + relativePos) wire.StartPos
    let middleAbsSegments = absSegments[1..(absSegments.Length - 2)]

    match checkInOutPortOpp model wire with
    | false ->
        None
    | true ->
        // Function to determine direction and magnitude of movement between two points
        let determineMovement (startPos: XYPos) (endPos: XYPos) =
            let deltaX = endPos.X - startPos.X
            let deltaY = endPos.Y - startPos.Y
            match symbolType with
            | ClassOutput ->
                if abs deltaX > abs deltaY then
                    if deltaX > 0.0 then (DIR_Right, abs deltaX) else (DIR_Left, abs deltaX)
                else
                    if deltaY > 0.0 then (DIR_Down, abs deltaY) else (DIR_Up, abs deltaY)
            | ClassInput ->
                if abs deltaX > abs deltaY then
                    if deltaX > 0.0 then (DIR_Left, abs deltaX) else (DIR_Right, abs deltaX)  // Reversed directions for deltaX
                else
                    if deltaY > 0.0 then (DIR_Up, abs deltaY) else (DIR_Down, abs deltaY)  // Reversed directions for deltaY


        // Use List.pairwise to get pairs of points (segments) and then fold to accumulate movements
        let movements =
            middleAbsSegments
            |> List.pairwise
            |> List.fold (fun acc (startPos, endPos) ->
                let move = determineMovement startPos endPos
                if List.isEmpty acc || fst (List.last acc) <> fst move then acc @ [move]  // Avoid duplicating direction if consecutive segments suggest the same movement
                else 
                    let lastMove = List.last acc
                    let updatedLastMove = (fst lastMove, snd lastMove + snd move)  // Combine movements in the same direction
                    List.init (List.length acc - 1) (fun i -> List.item i acc) @ [updatedLastMove]
            ) []
            //|> simplifyMovements
            
        match simplifyMovements movements with
        | [] -> None
        | _ -> Some (symbol, wire, simplifyMovements movements)


let singlyConnectedSymbols (model: SheetT.Model) =
    let outputWiresToBeStraightened =
        checkWiresToBeStraightened model
        |> List.choose (fun wireAndSegments -> 
            match findOutputSymbol model wireAndSegments with
            | Some (wire, segments, symbol, symbolType) when isSymbolSinglyConnected symbol model -> 
                Some (wire, segments, symbol, symbolType)
            | _ -> None
        )

    let inputWiresToBeStraightened = 
        checkWiresToBeStraightened model
        |> List.choose (fun wireAndSegments -> 
            match findInputSymbol model wireAndSegments with
            | Some (wire, segments, symbol, symbolType) when isSymbolSinglyConnected symbol model -> 
                Some (wire, segments, symbol, symbolType)
            | _ -> None
        )

    let wiresToBeStraightened = outputWiresToBeStraightened @ inputWiresToBeStraightened

    let processedSymbols =
        wiresToBeStraightened 
        |> List.map (fun (wire, segments, symbol, symbolType) ->
            match calcMovementsToStraightenWire model (wire, segments, symbol, symbolType) with
            | Some (symbol, wire, movements)  ->
                    match List.length movements with
                    | 1 -> Some (symbol, movements)
                    | 2 -> Some (symbol, movements)
                    | 3 -> Some (symbol, movements)
                    | _ -> None

            | _ -> None
        )
        |> List.choose id // Properly flatten the list by removing None

    processedSymbols

  
let nonSinglyConnectedSymbols (model: SheetT.Model): moveSymInformation list =
    let outputWiresToBeStraightened =
        checkWiresToBeStraightened model
        |> List.choose (fun wireAndSegments -> 
            match findOutputSymbol model wireAndSegments with
            | Some (wire, segments, symbol, symbolType) when not (isSymbolSinglyConnected symbol model) ->
                match findInputSymbol model wireAndSegments with
                | Some (wireIN, segmentsIN, symbolIN, symbolTypeIN) ->
                    Some (wire, segments, symbol, symbolType, symbolIN)
                | _ -> None
            | _ -> None
        )

    // removed from code for now as it is untested
    let inputWiresToBeStraightenedUnchecked =
        checkWiresToBeStraightened model
        |> List.choose (fun wireAndSegments -> 
            match findInputSymbol model wireAndSegments with
            | Some (wire, segments, symbol, symbolType) when not (isSymbolSinglyConnected symbol model) ->
                match findOutputSymbol model wireAndSegments with
                | Some (wireOUT, segmentsOUT, symbolOUT, symbolTypeOUT) ->
                    Some (wire, segments, symbol, symbolType, symbolOUT)
                | _ -> None
            | _ -> None
        )
    // Extract Wire IDs from outputWiresToBeStraightened
    let outputWireIDs = outputWiresToBeStraightened |> List.map (fun (wire, segments, symbol, symbolType, sym2)-> wire.WId)

    // Filter inputWiresToBeStraightened to exclude wires present in outputWiresToBeStraightened
    let inputWiresToBeStraightened =
        inputWiresToBeStraightenedUnchecked
        |> List.filter (fun (wire, _, _, _,_) -> not (List.contains wire.WId outputWireIDs))
    let wiresToBeStraightened = outputWiresToBeStraightened //@ inputWiresToBeStraightened

    let filteredWires = 
        wiresToBeStraightened
        |> List.map (fun (wire, segments, symbol, symbolType, symIn) ->
            match calcMovementsToStraightenWire model (wire, segments, symbol, symbolType) with
            | Some (symbol, wire, movements) ->
                match List.length movements with
                | 1 -> Some (createMovementSymbolWire symbol symIn movements)
                | 2 -> Some (createMovementSymbolWire symbol symIn movements)
                | 3 -> Some (createMovementSymbolWire symbol symIn movements)
                | _ -> None
            | _ -> None
        )
        |> List.choose id // Properly flatten the list by removing None
    //printfn "Filtered Wires: %A" filteredWires
    filteredWires
    //|> List.map (fun (symbol, symbolIn, movements) -> createMovementSymbolWire symbol symbolIn movements)



/// based on the movements required to straighten the wire calculate the new coordinates of the symbol
let calculateNewCoordinates (symbol: SymbolT.Symbol, movementList: (Directions*float) list) : XYPos =
    let symOriginalPos: XYPos = symbol.Pos
    movementList |> List.fold (fun (acc: XYPos) (dir, dist) ->
        match dir with
        | DIR_Up -> { acc with Y = acc.Y  - dist }
        | DIR_Down -> { acc with Y = acc.Y + dist }
        | DIR_Left -> { acc with X = acc.X - dist }
        | DIR_Right -> { acc with X = acc.X + dist }
        | _ -> acc
    ) symOriginalPos



/// Function that checks the number of right angles and symbol intersecions
let evaluateSchematic (model: SheetT.Model): (int*int*int) =
    let rightAngles = countWireRightAngles model
    let symOverlap = numOfIntersectedSymPairs model
    let wireIntersections = numOfWireRightAngleCrossings model
    (rightAngles, symOverlap, wireIntersections)


// iterate through each movementInfo entry check the number of ports of each symbol at this together and 
let setMovementPriority (movementInfo: moveSymInformation list): moveSymInformation list =
    let numberOfPorts (symbol: SymbolT.Symbol) =
        let outputPorts = symbol.Component.OutputPorts
        let inputPorts = symbol.Component.InputPorts
        List.length outputPorts + List.length inputPorts

    // Function to calculate the total number of ports for a movementSymbolWire entry
    let totalPortsAffected (msw: moveSymInformation) =
        numberOfPorts msw.SymbolToMove + numberOfPorts msw.SymbolRef

    // Sorting the movementInfo list by the total number of ports affected, highest first
    movementInfo
    |> List.sortByDescending totalPortsAffected


/// Invert the Directions of the movementts to be used when switching the symbol and symbolRef
let invertDirections movements =
    movements |> List.map (function
        | (DIR_Left, dist) -> (DIR_Right, dist)
        | (DIR_Right, dist) -> (DIR_Left, dist)
        | (DIR_Up, dist) -> (DIR_Down, dist)
        | (DIR_Down, dist) -> (DIR_Up, dist))

/// Called whenever a symbol is moved, if it is moved then reverse the order of SymbolToMove and SymbolRef
let switchMoveRefSymbols (movementSymbolWireList: moveSymInformation list) (symMoved: SymbolT.Symbol) (move: moveSymInformation): moveSymInformation list =
    movementSymbolWireList
    |> List.map (fun msw ->
        if msw.SymbolToMove.Id = symMoved.Id then
            // swap symboltomove with symbolref, reverse and invert directions in movements
            let inversetedMovements = invertDirections msw.Movements
            let newSimplifiedMovements = simplifyMovements (inversetedMovements @ move.Movements)

            { SymbolToMove = msw.SymbolRef; SymbolRef = msw.SymbolToMove; Movements = newSimplifiedMovements }
        else
            msw
      )
let switchMoveRefSymbolsAndMergeMovements (movementSymbolWireList: moveSymInformation list) (move: moveSymInformation): moveSymInformation =
    let matchedMovements = 
        movementSymbolWireList
        |> List.tryFind (fun msw -> msw.SymbolToMove.Id = move.SymbolToMove.Id)
        |> Option.map (fun msw -> 
            // If a match is found, invert and append move's movements to the matched movements
            let invertedMovements = invertDirections move.Movements

            let combinedMovements = invertedMovements @ msw.Movements
            // Apply simplifyMovements to the combined movements
            printfn "switchMoveRefSymbolsAndMergeMovements: %A, Movements: %A, Length of movements: %A" move.SymbolRef.Component.Label  (simplifyMovements combinedMovements) (List.length combinedMovements)
            simplifyMovements combinedMovements
        )
    
    match matchedMovements with
    | Some(simplifiedMovements) ->
        // If there was a match and movements were combined and simplified, construct a new movementSymbolWire
        { SymbolToMove = move.SymbolRef; SymbolRef = move.SymbolToMove; Movements = simplifiedMovements }
    | None ->
        // If no match was found, return the move as-is
        move


/// Updates the model with the movements of the symbols that are not singly connected
/// Ensures that there are no symbol intersections and that the number of right angles is minimized
/// Ensures that the number o fwire intersection are not increased (rewards reducing wire intersections)
/// Returns the updated model
let updateModelWithNonSinglyConnected (initialModel: SheetT.Model) (symbolMovements: moveSymInformation list): SheetT.Model =

    let applyMovmentsTo model movementsToBeCompleted =
        movementsToBeCompleted |>
        List.fold (fun (currentModel, listOfMovedSyms) msw ->
            let movementList = msw.Movements
            let verifiedMovements = switchMoveRefSymbolsAndMergeMovements listOfMovedSyms msw
            let symbol = verifiedMovements.SymbolToMove
            let symbolRef = verifiedMovements.SymbolRef
            
            let newCoordinatesSymbol = calculateNewCoordinates (symbol, verifiedMovements.Movements)
            let (numRightAngles, numIntersections, numWireIntersections) = evaluateSchematic currentModel

            // Try moving SymbolToMove
            let trialModelSymbolMove =  updateSymPosInSheet symbol.Id newCoordinatesSymbol currentModel
            let updatedWireModel =  BusWireSeparate.routeAndSeparateSymbolWires trialModelSymbolMove.Wire symbol.Id
            let updateOtherSymm =  BusWireSeparate.routeAndSeparateSymbolWires updatedWireModel symbolRef.Id
            let symbolModel = { trialModelSymbolMove with Wire = updateOtherSymm }

            //let (newRightAngles, newIntersections) =

                

            // TODO: create a function for this instead this is bad :(
            match evaluateSchematic symbolModel with
            | newRightAngles ,0, newWireIntersections when (((newRightAngles < numRightAngles) && (newWireIntersections <= numWireIntersections)) || ((newWireIntersections < numWireIntersections) && (newRightAngles <= numRightAngles)))->
                
                symbolModel, listOfMovedSyms @ [verifiedMovements]
            | _ ->

                // Try moving SymbolRef in the opposite direction if SymbolToMove fails
                let invertedMovements = invertDirections movementList

                let newCoordinatesSymbolRef = calculateNewCoordinates (symbolRef, invertedMovements)

                let trialModelSymbolRefMove =  updateSymPosInSheet symbolRef.Id newCoordinatesSymbolRef currentModel
                let updatedRefWireModel =  BusWireSeparate.routeAndSeparateSymbolWires trialModelSymbolRefMove.Wire symbolRef.Id
                let updateOtherSymm =  BusWireSeparate.routeAndSeparateSymbolWires updatedRefWireModel symbol.Id

                let symbolRefModel = { trialModelSymbolRefMove with Wire = updateOtherSymm }

                match evaluateSchematic symbolRefModel with
                | newRefRightAngles, 0, newRefWireIntersections  when (((newRefRightAngles < numRightAngles) && (newRefWireIntersections <= numWireIntersections)) || ((newRefRightAngles <= numRightAngles) && (newRefWireIntersections < numWireIntersections)) )-> 
                    symbolRefModel, listOfMovedSyms @ [createMovementSymbolWire symbolRef symbol invertedMovements]
                |newRefRightAngles, intersections, newRefWireIntersections -> 
                    currentModel, listOfMovedSyms // Return the original model if both attempts fail
        ) (model, ([]: moveSymInformation list))


    let newModel, _ = applyMovmentsTo initialModel symbolMovements
    newModel
     

/// Applies the movements to the model and checks for intersections for singly connected symbols
/// Ensures no overlap when making the movement
let updateModelWithSinglyConnected (model: SheetT.Model) (symbolMovements: (SymbolT.Symbol * (Directions * float) list) list) : SheetT.Model =
    symbolMovements |> List.fold (fun currentModel (symbol, movementList) ->
        let newCoordinates = calculateNewCoordinates (symbol, movementList)
        

        let updatedModel = updateSymPosInSheet symbol.Id newCoordinates currentModel
        let updatedModelWires = BusWireSeparate.routeAndSeparateSymbolWires updatedModel.Wire symbol.Id
        let symbolModel = { updatedModel with Wire = updatedModelWires }

        match numOfIntersectedSymPairs symbolModel with
        | 0 -> 
            symbolModel
        | _ -> 
            currentModel
        
    ) model



 /// Function to print the details of the movementSymbolWire list
 /// Used for debugging
let printMovementSymbolWireDetails (movementsList: moveSymInformation list) =
    movementsList |> List.iter (fun msw ->
        printfn "SymbolToMove: %s, SymbolRef: %s, Movements: %A" 
            msw.SymbolToMove.Component.Label 
            msw.SymbolRef.Component.Label 
            msw.Movements
    )

/// Top-level function to straightenWires on the sheet
let straightenWiresByMovingSyms (model: SheetT.Model) : SheetT.Model =
    let nonSinglyConnected = setMovementPriority (nonSinglyConnectedSymbols model)

    //printMovementSymbolWireDetails nonSinglyConnected
    match nonSinglyConnected.Length with
    | 0 ->
        printfn "No non singly connected symbols to align"
        model
    | _ ->
        printfn "Non singly connected symbols to align"

        let nonSinglyCnnectedModel = updateModelWithNonSinglyConnected model nonSinglyConnected
        let newSinglyConnected =  singlyConnectedSymbols nonSinglyCnnectedModel // cals singly connected symbols again to check if any wires have been unstraightened
        updateModelWithSinglyConnected nonSinglyCnnectedModel newSinglyConnected


