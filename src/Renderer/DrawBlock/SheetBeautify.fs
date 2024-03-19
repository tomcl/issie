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

type Directions = DIR_Left | DIR_Right | DIR_Up | DIR_Down
type SymbolClassification = ClassInput | ClassOutput
type StraightWireDirection = Horizontal | Vertical | Unknown | NotStraight


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
    | true , true -> false
    | true, false -> movementList |> List.forall (fun (dir, _) -> dir = DIR_Left || dir = DIR_Right)
    | false, true -> movementList |> List.forall (fun (dir, _) -> dir = DIR_Up || dir = DIR_Down)
    | false, false -> true

  


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

let nonSinglyConnectedSymbols (model: SheetT.Model) =
    let outputWiresToBeStraightened =
        checkWiresToBeStraightened model
        |> List.choose (fun wireAndSegments -> 
            match findOutputSymbol model wireAndSegments with
            | Some (wire, segments, symbol, symbolType) when not (isSymbolSinglyConnected symbol model) -> 
                Some (wire, segments, symbol, symbolType)
            | _ -> None
        )

//    // removed from code for now as it is untested
    //let inputWiresToBeStraightenedUnchecked =
    //    checkWiresToBeStraightened model
    //    |> List.choose (fun wireAndSegments -> 
    //        match findInputSymbol model wireAndSegments with
    //        | Some (wire, segments, symbol, symbolType) when not (isSymbolSinglyConnected symbol model) -> 
    //            Some (wire, segments, symbol, symbolType)
    //        | _ -> None
    //    )
    //// Extract Wire IDs from outputWiresToBeStraightened
    //let outputWireIDs = outputWiresToBeStraightened |> List.map (fun (wire, _, _, _) -> wire.WId)

    // Filter inputWiresToBeStraightened to exclude wires present in outputWiresToBeStraightened
    //let inputWiresToBeStraightened =
    //    inputWiresToBeStraightenedUnchecked
    //    |> List.filter (fun (wire, _, _, _) -> not (List.contains wire.WId outputWireIDs))
    let wiresToBeStraightened = outputWiresToBeStraightened //@ inputWiresToBeStraightened

    let filteredWires = 
        wiresToBeStraightened
        |> List.map (fun (wire, segments, symbol, symbolType) ->
            match calcMovementsToStraightenWire model (wire, segments, symbol, symbolType) with
            | Some (symbol, wire, movements) ->
                match List.length movements with
                | 1 -> Some (symbol, movements)
                | 2 -> Some (symbol, movements)
                | 3 -> Some (symbol, movements)
                | _ -> None
            | _ -> None
        )
        |> List.choose id // Flatten the list by removing None values

    filteredWires


/// based on the movements required to straighten the wire calculate the new coordinates of the symbol
let calculateNewCoordinates (symbol: SymbolT.Symbol, movementList: (Directions*float) list) : XYPos =
    let symOriginalPos: XYPos = symbol.Pos
    movementList |> List.fold (fun (acc: XYPos) (dir, dist) ->
        match dir with
        | DIR_Up -> { acc with Y = acc.Y - dist }
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
let updateModelWithNonSinglyConnected (model: SheetT.Model) (symbolMovements: (SymbolT.Symbol * (Directions * float) list) list) : SheetT.Model =
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

    // Step 3: Select best movement set and update model accordingly
    modelsWithMovements
    |> List.fold (fun accModel (newModel, _) -> newModel) model // Apply successful movements to the model

   

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
    


 /// Function to align the symbols and wires in the sheet
/// Returns the updated model
let alignSymbols (model: SheetT.Model) : SheetT.Model =
    let singlyConnected = singlyConnectedSymbols model
    let nonSinglyConnected = nonSinglyConnectedSymbols model
    //printfn "singlyConnected: %A" singlyConnected
    //printfn "nonSinglyConnected: %A" nonSinglyConnected
    let firstNonSinglyConnected = nonSinglyConnected

    let modelWithAlignedSinglyConnected = updateModelWithSinglyConnected model singlyConnected
    //modelWithAlignedSinglyConnected
    match nonSinglyConnected.Length with
    | 0 -> modelWithAlignedSinglyConnected
    | _ -> let nonSinglyCnnectedModel = updateModelWithNonSinglyConnected modelWithAlignedSinglyConnected firstNonSinglyConnected
           //let nonsinglyConnected2 = updateModelWithNonSinglyConnected nonSinglyCnnectedModel (nonSinglyConnectedSymbols nonSinglyCnnectedModel)
           let newSinglyConnected =  singlyConnectedSymbols nonSinglyCnnectedModel // cals singly connected symbols again to check if any wires have been unstraightened
           updateModelWithSinglyConnected nonSinglyCnnectedModel newSinglyConnected
           //nonSinglyCnnectedModel

