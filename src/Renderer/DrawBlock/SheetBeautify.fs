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
/// constants used by SheetBeautify
//module Constants =
//    () // dummy to make skeleton type check - remove when other content exists


// TODO: Define the different types required
// - direction ie left, right, up, down
// - symbolClassification ie input, output

//TODO: add more types to prevent long tuples
type Directions = DIR_Left | DIR_Right | DIR_Up | DIR_Down
type SymbolClassification = ClassInput | ClassOutput
type StraightWireDirection = Horizontal | Vertical | Unknown | NotStraight

type movementSymbolWire =
    {
        SymbolToMove: SymbolT.Symbol
        SymbolRef: SymbolT.Symbol
        Movements: (Directions * float) list
    }

type PriorityDir = PriorityLeft | PriorityRight | PriorityUp | PriorityDown



let createMovementSymbolWire (symbolToMove: SymbolT.Symbol) (symbolRef: SymbolT.Symbol) (movements: (Directions * float) list) : movementSymbolWire =
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
    if countLeft = countRight || countUp = countDown then
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
        //printfn "Symbol: %A, Movements: %A, Length of movements: %A" symbol.Component.Label  movements (List.length movements)
        //printfn "Symbol: %A, Movements: %A, Length of simplified movements: %A" symbol.Component.Label  (simplifyMovements movements) (List.length (simplifyMovements movements))
    
        match simplifyMovements movements with
        | [] -> None
        | _ -> Some (symbol, wire, simplifyMovements movements)

/// Checks if the symbol is singly connected 
//let singlyConnectedSymbols (model: SheetT.Model) =
//    let outputWiresToBeStraightened =
//        checkWiresToBeStraightened model
//        |> List.choose (fun wireAndSegments -> 
//            match findOutputSymbol model wireAndSegments with
//            | Some (wire, segments, symbol, symbolType) when isSymbolSinglyConnected symbol model -> 
//                Some (wire, segments, symbol, symbolType)
//            | _ -> None
//        )
//    let inputWiresToBeStraightened =
//        checkWiresToBeStraightened model
//        |> List.choose (fun wireAndSegments -> 
//            match findInputSymbol model wireAndSegments with
//            | Some (wire, segments, symbol, symbolType) when isSymbolSinglyConnected symbol model -> 
//                Some (wire, segments, symbol, symbolType)
//            | _ -> None
//        )

//    let wiresToBeStraightened = outputWiresToBeStraightened @ inputWiresToBeStraightened

//    let temp = wiresToBeStraightened |> List.map (fun (wire, segments, symbol, symbolType) -> calcMovementsToStraightenWire model (wire, segments, symbol, symbolType))
//    temp |> List.choose (fun ans ->
//        match ans with
//        | Some (symbol, wire, movements) ->
//            printfn "Symbol: %A, Movements: %A, Length of movements: %A" symbol.Component.Label  movements (List.length movements)
//            Some (symbol, movements)
//        | None -> None
//    )

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




/// required function for straighening non singly connected symbols
/// Checks other wires from that originate from the same port, if they are straight then the wires movements are restricted to the same direction
/// If the other wires are not straight then the wire can be straightened in any direction
/// returns true if the wire can be straightened
let decideOnStraightening (symbol: SymbolT.Symbol, wireRef:BusWireT.Wire, movementList: (Directions*float) list) (model: SheetT.Model) : bool =
    // check that the wire to be straightened is not connected to a port with an already straight wire
    let wires = model.Wire.Wires
    // Find wires that are connected to the same port as the wire to be straightened
    let connectedWires =
            wires
            |> Map.filter (fun _ wire -> wire.OutputPort = wireRef.OutputPort && wire.WId <> wireRef.WId)  // Exclude the wire to be straightened

    // Collect directions for all wires in connectedWires
    let allDirections = 
        connectedWires
        |> Map.fold (fun acc _ wire ->
            let absSegments =
                visibleSegments wire.WId model
                |> List.filter (fun segs -> not (segs =~ XYPos.zero))
                |> List.scan (fun acc relativePos -> acc + relativePos) wire.StartPos
            let directions =
                absSegments
                |> List.pairwise // Get pairs of consecutive segment ends to determine direction
                |> List.map (fun (prevPos, currPos) ->
                    if prevPos.X = currPos.X then Vertical
                    else if prevPos.Y = currPos.Y then Horizontal
                    else Unknown)
            acc @ [directions]) []  // Accumulate directions from all wires

    let checkForStraightLines allDirections:StraightWireDirection list =
        allDirections
        |> List.map (fun directions ->
            match directions with
            | [Vertical] -> Vertical
            | [Horizontal] -> Horizontal
            | _ ->
                let isStraightLine = 
                    List.forall (fun direction -> direction = List.head directions) directions
                if isStraightLine then List.head directions
                else NotStraight)

    let straightDirectionList = checkForStraightLines allDirections

    match List.exists (fun dir -> dir = Horizontal) straightDirectionList, List.exists (fun dir -> dir = Vertical) straightDirectionList with
    | true , true ->
        printfn "Symbol: %A, Wire: %A, Movements: %A, Wire cannot be straightened" symbol.Component.Label wireRef.WId movementList
        false
    | true, false ->
        let ans = movementList |> List.forall (fun (dir, _) -> dir = DIR_Left || dir = DIR_Right)
        printfn "Symbol: %A, Wire: %A, Movements: %A, Wire can be straightened" symbol.Component.Label wireRef.WId ans
        ans
    | false, true ->
        let ans = movementList |> List.forall (fun (dir, _) -> dir = DIR_Up || dir = DIR_Down)
        printfn "Symbol: %A, Wire: %A, Movements: %A, Wire can be straightened" symbol.Component.Label wireRef.WId ans
        ans
    | false, false ->
        printfn "Symbol: %A, Wire: %A, Movements: %A, Wire can be straightened" symbol.Component.Label wireRef.WId movementList
        true

  


// TODO commented code 
/// Function to find and process to straighten non singly connected symbols
/// returns a list of symbols and the movements required to straighten the wire that will not unstraighten other wires
//let nonSinglyConnectedSymbols (model: SheetT.Model) =
//    let outputWiresToBeStraightened =
//        checkWiresToBeStraightened model
//        |> List.choose (fun wireAndSegments -> 
//            match findOutputSymbol model wireAndSegments with
//            | Some (wire, segments, symbol, symbolType) when not (isSymbolSinglyConnected symbol model) -> 
//                Some (wire, segments, symbol, symbolType)
//            | _ -> None
//        )

//    // removed from code for now as it is untested
//    //let inputWiresToBeStraightenedUnchecked =
//    //    checkWiresToBeStraightened model
//    //    |> List.choose (fun wireAndSegments -> 
//    //        match findInputSymbol model wireAndSegments with
//    //        | Some (wire, segments, symbol, symbolType) when not (isSymbolSinglyConnected symbol model) -> 
//    //            Some (wire, segments, symbol, symbolType)
//    //        | _ -> None
//    //    )
//    //// Extract Wire IDs from outputWiresToBeStraightened
//    //let outputWireIDs = outputWiresToBeStraightened |> List.map (fun (wire, _, _, _) -> wire.WId)

//    //// Filter inputWiresToBeStraightened to exclude wires present in outputWiresToBeStraightened
//    //let inputWiresToBeStraightened =
//    //    inputWiresToBeStraightenedUnchecked
//    //    |> List.filter (fun (wire, _, _, _) -> not (List.contains wire.WId outputWireIDs))



//    let wiresToBeStraightened = outputWiresToBeStraightened// @ inputWiresToBeStraightened


//    let filteredWires = 
//        wiresToBeStraightened
//        |> List.choose (fun (wire, segments, symbol, symbolType) ->
//            let movements = calcMovementsToStraightenWire model (wire, segments, symbol, symbolType) // Adjusted based on expected output
//            match movements with
//            | Some (_, _, movementsList) when decideOnStraightening (symbol, wire, movementsList) model
//                 -> Some (symbol, movementsList)
//            | _  -> None
//            //match movements with
//            //| Some (symbol, wire, movementsList)  -> Some (symbol, movementsList)
//            //| _  -> None
//        )

//    filteredWires

let nonSinglyConnectedSymbols (model: SheetT.Model): movementSymbolWire list =
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

//    // removed from code for now as it is untested
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

/// based on the movements required to straighten the wire calculate the new coordinates of the symbol
let calculateNewCoordinatesTest (symbol: SymbolT.Symbol, movementList: (Directions*float) list) : XYPos =
    let symOriginalPos: XYPos = symbol.Pos
    movementList |> List.fold (fun (acc: XYPos) (dir, dist:float) ->
        match dir with
        | DIR_Up -> { acc with Y = acc.Y - dist  }
        | DIR_Down -> { acc with Y = acc.Y + dist }
        | DIR_Left -> { acc with X = acc.X - dist }
        | DIR_Right -> { acc with X = acc.X + dist }
        | _ -> acc
    ) symOriginalPos

// TODO Adjust to allow maximum number of movements to be applied to the symbol
/// Function to apply the movements to the model and check for intersections for non singly connected symbols
/// Completes extra processing as there maybe mulitple suggested movements for a symbol. Selects the first one that does not intersect with any other symbol
let updateModelWithNonSinglyConnected2 (model: SheetT.Model) (symbolMovements: (SymbolT.Symbol * (Directions * float) list) list) : SheetT.Model =
    printfn "Updating model with non singly connected symbols!!!!!"
    // Step 1: Group movements by symbol
    let movementsBySymbol =
        let test1 =
            symbolMovements
            |> List.groupBy fst
        let test2 =
            test1
            |> List.map (fun (symbol, groupedMovements) -> 
                symbol, List.collect snd groupedMovements)
        test2

    // Function to attempt applying movements and check intersections
    let tryMovementsForSymbol (symbol, movements) =
        movements
        |> List.map (fun movement -> 
            let trialModel = 
                let newCoordinates = calculateNewCoordinatesTest (symbol, [movement])
                let updatedModel = Optic.set (SheetT.symbolOf_ symbol.Id) { symbol with Pos = newCoordinates } model
                let updatedModelWires = BusWireSeparate.routeAndSeparateSymbolWires updatedModel.Wire symbol.Id
                let symbolModel = { updatedModel with Wire = updatedModelWires }
                symbolModel
            let intersections = numOfIntersectedSymPairs trialModel
            (trialModel, intersections))
        |> List.sortBy snd // Sort by number of intersections
        |> List.tryFind (fun (_, intersections) -> intersections = 0) // Find the first movement set with 0 intersections
    // Step 2: Attempt movements for each symbol and record intersections
    let modelsWithMovements = 
        movementsBySymbol
        |> List.choose tryMovementsForSymbol // Only keep successful movement attempts
    printfn "Movements by symbol: %A" (modelsWithMovements)

    // Step 3: Select best movement set and update model accordingly
    modelsWithMovements
    |> List.fold (fun accModel (newModel, _) -> newModel) model // Apply successful movements to the model

/// Function that checks the number of right angles and symbol intersecions
let evaluateSchematic (model: SheetT.Model): (int*int) =
    let rightAngles = countWireRightAngles model
    let symOverlap = numOfIntersectedSymPairs model
    (rightAngles, symOverlap)

/// Find the output symbols in the model
let findOutputSymbols (model: SheetT.Model) =
    let outputSymbols = 
        model.Wire.Symbol.Symbols
        |> Map.toList
        |> List.choose (fun (_, symbol) ->
           match symbol.Component.Type with
           | Output _ -> Some symbol
           | _ -> None)

    outputSymbols

// use with findInputSymbol
/// find the wire connected to the output port
let findWireConnectedToOutputPort (model: SheetT.Model) (portId: OutputPortId) =
    let wires = model.Wire.Wires
    let connectedWires = wires |> Map.filter (fun _ wire -> wire.OutputPort = portId)
    connectedWires

// use with findOutputSymbol
/// find the wire connected to the input port
let findWireConnectedToInputPort (model: SheetT.Model) (portId: InputPortId) =
    let wires = model.Wire.Wires
    let connectedWires = wires |> Map.filter (fun _ wire -> wire.InputPort = portId)
    connectedWires



//let updateModelWithNonSinglyConnected (initialModel: SheetT.Model) (symbolMovements: movementSymbolWire list): SheetT.Model =
//    printfn "Updating model with non singly connected symbols!!!!!"



    //// Evaluate initial model
    //let (initialRightAngles, initialSymOverlap) = evaluateSchematic initialModel

    //// Process each movementSymbolWire and update model if it improves right angles without causing overlaps
    //let processMovement (currentModel: SheetT.Model) (movementSymbolWire: movementSymbolWire) =
    //    // Calculate new coordinates for the symbol using the entire list of movements
    //    let symTomove = movementSymbolWire.SymbolToMove
    //    let symRef = movementSymbolWire.SymbolRef
    //    let symbolMovements = movementSymbolWire.Movements
    //    //let newCoordinates = calculateNewCoordinates (symTomove, symbolMovements)
    //    //let updatedSymbol = { movementSymbolWire.SymbolToMove with Pos = newCoordinates }
    //    //let trialModel = Optic.set (SheetT.symbolOf_ movementSymbolWire.SymbolToMove.Id) updatedSymbol currentModel

    //    let trialModel = 
    //        let newCoordinates = calculateNewCoordinatesTest (symTomove, symbolMovements)
    //        let updatedModel = Optic.set (SheetT.symbolOf_ symTomove.Id) { symTomove with Pos = newCoordinates } currentModel
    //        let updatedModelWires = BusWireSeparate.routeAndSeparateSymbolWires updatedModel.Wire symTomove.Id
    //        let symbolModel = { updatedModel with Wire = updatedModelWires }
    //        symbolModel

    //    // Evaluate the trial model
    //    let (trialRightAngles, trialSymOverlap) = evaluateSchematic trialModel
    //    printfn "Right Angles: %A, Symbol Overlap: %A" trialRightAngles trialSymOverlap

    //    // Check the conditions for accepting the movements
    //    //if trialRightAngles < initialRightAngles && trialSymOverlap = 0 then
    //    //    printfn "Movement applied successfully to symbol: %A" symTomove.Component.Label 
    //    //    trialModel // Accept the updated model
    //    //else
    //    //    printfn "Movement resulted in intersections, not applied to symbol: %A" symTomove.Component.Label
    //    //    currentModel // Reject the update and return the original model
    //    trialModel

    //// Iterate over all symbol movements and update the model
    //symbolMovements
    //|> List.fold processMovement initialModel
// iterate through each movementInfo entry check the number of ports of each symbol at this together and 
let setMovementPriority (movementInfo: movementSymbolWire list): movementSymbolWire list =
    let numberOfPorts (symbol: SymbolT.Symbol) =
        let outputPorts = symbol.Component.OutputPorts
        let inputPorts = symbol.Component.InputPorts
        List.length outputPorts + List.length inputPorts

    // Function to calculate the total number of ports for a movementSymbolWire entry
    let totalPortsAffected (msw: movementSymbolWire) =
        numberOfPorts msw.SymbolToMove + numberOfPorts msw.SymbolRef

    // Sorting the movementInfo list by the total number of ports affected, highest first
    movementInfo
    |> List.sortByDescending totalPortsAffected



let invertDirections movements =
    movements |> List.map (function
        | (DIR_Left, dist) -> (DIR_Right, dist)
        | (DIR_Right, dist) -> (DIR_Left, dist)
        | (DIR_Up, dist) -> (DIR_Down, dist)
        | (DIR_Down, dist) -> (DIR_Up, dist))

/// Called whenever a symbol is moved, if it is moved then reverse the order of SymbolToMove and SymbolRef
/// Add the m
let switchMoveRefSymbols (movementSymbolWireList: movementSymbolWire list) (symMoved: SymbolT.Symbol) (move: movementSymbolWire): movementSymbolWire list =
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
let switchMoveRefSymbolsAndMergeMovements (movementSymbolWireList: movementSymbolWire list) (move: movementSymbolWire): movementSymbolWire =
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


//let updateModelWithNonSinglyConnected (initialModel: SheetT.Model) (symbolMovements: movementSymbolWire list): SheetT.Model =
//    printfn "Updating model with non singly connected symbols!!!!!"

//    symbolMovements |> List.fold (fun currentModel msw ->
//        let symbol = msw.SymbolToMove
//        let symbolRef = msw.SymbolRef
//        let movementList = msw.Movements
//        let newCoordinates = calculateNewCoordinates (symbol, movementList)
        
//        // Print out the symbol and movements being applied.
//        printfn "Applying movements to symbol: %A " symbol.Component.Label
//        movementList |> List.iter (fun (direction, distance) ->
//            printfn "  Movement: %A, Distance: %f" direction distance)
//        let updatedSymbol = updateSymPos newCoordinates symbol
//        // use this function!!!!
//        let updatedModel = updateSymPosInSheet symbol.Id newCoordinates currentModel
//        //let updatedModel = Optic.set (SheetT.symbolOf_ symbol.Id) updatedSymbol currentModel
//        let updatedModelWires = BusWireSeparate.routeAndSeparateSymbolWires updatedModel.Wire symbol.Id
//        let trialModel = { updatedModel with Wire = updatedModelWires }

//        match numOfIntersectedSymPairs trialModel with
//        | 0 -> 
//            printfn "Movement applied successfully to symbol: %A" symbol.Component.Label
//            printfn "Number of intersections %A" (numOfIntersectedSymPairs trialModel)

//            trialModel
//        | _ -> 
//            printfn "Movement resulted in intersections, not applied to symbol: %A" symbol.Component.Label
//            currentModel
//        //symbolModel
//    ) initialModel

let updateModelWithNonSinglyConnected (initialModel: SheetT.Model) (symbolMovements: movementSymbolWire list): SheetT.Model =
    printfn "Updating model with non singly connected symbols!!!!!"

    let applyMovmentsTo model movementsToBeCompleted =
        movementsToBeCompleted |>
        List.fold (fun (currentModel, listOfMovedSyms) msw ->
            let movementList = msw.Movements
            let verifiedMovements = switchMoveRefSymbolsAndMergeMovements listOfMovedSyms msw
            let symbol = verifiedMovements.SymbolToMove
            let symbolRef = verifiedMovements.SymbolRef
            
            let newCoordinatesSymbol = calculateNewCoordinates (symbol, verifiedMovements.Movements)
            let (numRightAngles, numIntersections) = evaluateSchematic currentModel

            //match symbolRef.Component.Type with
            //| Output _ -> 
            //             //Print out the symbol and movements being app
            //            printfn "output symbol not moving"
            //            currentModel, listOfMovedSyms
            //| _ ->
            printfn "Applying movements to symbol: %A " symbol.Component.Label
            movementList |> List.iter (fun (direction, distance) ->
                printfn "  Movement: %A, Distance: %f" direction distance)

            printfn "Applying VERIFIED movements to symbol: %A " symbol.Component.Label
            verifiedMovements.Movements |> List.iter (fun (direction, distance) ->
                printfn "  VERIFIED Movement: %A, Distance: %f" direction distance)

            // Try moving SymbolToMove
            let trialModelSymbolMove =  updateSymPosInSheet symbol.Id newCoordinatesSymbol currentModel
            let updatedWireModel =  BusWireSeparate.routeAndSeparateSymbolWires trialModelSymbolMove.Wire symbol.Id
            let symbolModel = { trialModelSymbolMove with Wire = updatedWireModel }

            //let (newRightAngles, newIntersections) =

                


            match evaluateSchematic symbolModel with
            | newRightAngles ,0 when newRightAngles < numRightAngles ->
                printfn "Movement applied successfully to symbol: %A" symbol.Component.Label
                printfn "Symbol: %A, Right Angles: %A, Intersections: %A, Original Right anges: %A " symbolRef.Component.Label newRightAngles 0 numRightAngles
                
                symbolModel, listOfMovedSyms @ [verifiedMovements]
            | _ ->
                printfn "Movement resulted in intersections, not applied to symbol: %A" symbol.Component.Label
                // Try moving SymbolRef in the opposite direction if SymbolToMove fails
                let invertedMovements = invertDirections movementList

                let newCoordinatesSymbolRef = calculateNewCoordinates (symbolRef, invertedMovements)
            
                printfn "Trying to move reference symbol: %A in the opposite direction" symbolRef.Component.Label
                invertedMovements |> List.iter (fun (direction, distance) ->
                    printfn "  Inverted Movement: %A, Distance: %f" direction distance)

                let trialModelSymbolRefMove =  updateSymPosInSheet symbolRef.Id newCoordinatesSymbolRef currentModel
                let updatedRefWireModel =  BusWireSeparate.routeAndSeparateSymbolWires trialModelSymbolRefMove.Wire symbolRef.Id
                let symbolRefModel = { trialModelSymbolRefMove with Wire = updatedRefWireModel }

                match evaluateSchematic symbolRefModel with
                | newRefRightAngles, 0  when newRefRightAngles < numRightAngles-> 
                    printfn "Movement applied successfully to reference symbol: %A" symbolRef.Component.Label
                    printfn "Symbol: %A, Right Angles: %A, Intersections: %A, Original Right anges: %A " symbolRef.Component.Label newRefRightAngles 0 numRightAngles
                    symbolRefModel, listOfMovedSyms @ [createMovementSymbolWire symbolRef symbol invertedMovements]
                |newRefRightAngles, intersections -> 
                    printfn "Movement resulted in intersections, not applied to reference symbol: %A" symbolRef.Component.Label
                    printfn "Symbol: %A, Right Angles: %A, Intersections: %A, Original Right anges: %A " symbol.Component.Label newRefRightAngles intersections numRightAngles
                    currentModel, listOfMovedSyms // Return the original model if both attempts fail
        ) (model, ([]: movementSymbolWire list))


    let newModel, _ = applyMovmentsTo initialModel symbolMovements
    newModel





   

/// Applies the movements to the model and checks for intersections for singly connected symbols
/// Ensures no overlap when making the movement
let updateModelWithSinglyConnected (model: SheetT.Model) (symbolMovements: (SymbolT.Symbol * (Directions * float) list) list) : SheetT.Model =
    symbolMovements |> List.fold (fun currentModel (symbol, movementList) ->
        let newCoordinates = calculateNewCoordinates (symbol, movementList)
        
        // Print out the symbol and movements being applied.
        printfn "Applying movements to symbol: %A " symbol.Component.Label
        movementList |> List.iter (fun (direction, distance) ->
            printfn "  Movement: %A, Distance: %f" direction distance)

        let updatedModel = Optic.set (SheetT.symbolOf_ symbol.Id) { symbol with Pos = newCoordinates } currentModel
        let updatedModelWires = BusWireSeparate.routeAndSeparateSymbolWires updatedModel.Wire symbol.Id
        let symbolModel = { updatedModel with Wire = updatedModelWires }

        match numOfIntersectedSymPairs symbolModel with
        | 0 -> 
            printfn "Movement applied successfully to symbol: %A" symbol.Component.Label
            symbolModel
        | _ -> 
            printfn "Movement resulted in intersections, not applied to symbol: %A" symbol.Component.Label
            currentModel
        
    ) model


// checks if there are a pair of custom compoenents that are connected to each other
//let checkCustomComponents (model: SheetT.Model) =
    
let printMovementSymbolWireDetails (movementsList: movementSymbolWire list) =
    movementsList |> List.iter (fun msw ->
        printfn "SymbolToMove: %s, SymbolRef: %s, Movements: %A" 
            msw.SymbolToMove.Component.Label 
            msw.SymbolRef.Component.Label 
            msw.Movements
    )

 /// Function to align the symbols and wires in the sheet
/// Returns the updated model
let alignSymbols (model: SheetT.Model) : SheetT.Model =
    printfn "Here we go again -> ALIGN SYMBOLS!!!!!!!!!!!!!!!!!!"
    let singlyConnected = singlyConnectedSymbols model
    let nonSinglyConnected = setMovementPriority (nonSinglyConnectedSymbols model)
    //printfn "singlyConnected: %A" singlyConnected
    //printfn "nonSinglyConnected: %A" nonSinglyConnected
    //let firstNonSinglyConnected = nonSinglyConnected



    printMovementSymbolWireDetails nonSinglyConnected



    //let modelWithAlignedSinglyConnected = updateModelWithSinglyConnected model singlyConnected
    //modelWithAlignedSinglyConnected
    match nonSinglyConnected.Length with
    | 0 ->
        printfn "No non singly connected symbols to align"
        //modelWithAlignedSinglyConnected   
        model
    | _ ->
        printfn "Non singly connected symbols to align"

        let nonSinglyCnnectedModel = updateModelWithNonSinglyConnected model nonSinglyConnected
        ////let nonsinglyConnected2 = updateModelWithNonSinglyConnected nonSinglyCnnectedModel (nonSinglyConnectedSymbols nonSinglyCnnectedModel)
        let newSinglyConnected =  singlyConnectedSymbols nonSinglyCnnectedModel // cals singly connected symbols again to check if any wires have been unstraightened
        updateModelWithSinglyConnected nonSinglyCnnectedModel newSinglyConnected
        //nonSinglyCnnectedModel

