module TestDrawBlockD1
open EEExtensions
open CommonTypes
open DrawModelType
open Optics
open SheetBeautifyHelpers
open TestDrawBlock
open TestDrawBlock.HLPTick3
open Symbol
open RotateScale
open DrawModelType.SymbolT
open GenerateData


type FailureType =
    | WiresNotStraightened of string
    | SymbolIntersections of string
    | WireSymbolIntersections of string


type TestResult = | Success of string | Failure of FailureType


type SheetAlignCircuitType =
    | UnstraightenedSimpleMUX of SheetT.Model
    | MultipleConnections of SheetT.Model
    | GateScaling of SheetT.Model
    | MultipleMUX of SheetT.Model
    | CustomComponentScaling of SheetT.Model

        
/// Empty function that will hold the completed sheetAlignScale function
let sheetAlignScale (model: SheetT.Model) =
    model


/// Rotates a symbol by a specified amount
///
/// label - The label of the symbol to be rotated
///
/// rotation - The amount to rotate the symbol by
///
/// sheet - The sheet model on which symbol rotation needs to be changed.
let rotateSymbol (rotation: Rotation) (label: string) (sheet: SheetT.Model): SheetT.Model =
    let symMap = sheet.Wire.Symbol.Symbols
    let symbol =
        (Map.toList >> List.map snd) symMap
        |> List.find (fun sym -> sym.Component.Label = label)

    let symCentre = getRotatedSymbolCentre symbol // If not rotated, just returns the correct centre, but accounts for rotated case.
    let symId = ComponentId symbol.Component.Id

    let rotatedSymbol =
        symMap
        |> Map.tryFind symId
        |> Option.map (fun sym -> rotateSymbolInBlock rotation symCentre sym) // The symbol is the only one in the block

    match rotatedSymbol with
    | Some (sym) ->
        let newSymbols =
            (symId, sym)
            |> symMap.Add
        Optic.set SheetT.symbols_ newSymbols sheet
        |> SheetUpdateHelpers.updateBoundingBoxes // Need to recalculate bounding boxes because rotation changes them
    | None ->
        printf $"Given symbol {symbol.Component.Label} does not exist on the sheet. Returning sheet before change."
        sheet


/// Given a symbol on a sheet and its flip orientation, flips the symbol.
///
/// symbol - The symbol the be flipped
///
/// flip - The new flip orientation
///
/// sheet - The sheet model on which symbol flip state needs to be changed.
let flipSymbol (label: string) (flip: FlipType) (sheet: SheetT.Model): SheetT.Model =
    let symMap = sheet.Wire.Symbol.Symbols
    let symbol =
        (Map.toList >> List.map snd) symMap
        |> List.find (fun sym -> sym.Component.Label = label)

    let symCentre = getRotatedSymbolCentre symbol // If not rotated, just returns the correct centre, but accounts for rotated case.
    let symId = ComponentId symbol.Component.Id

    let flippedSymbol =
        symMap
        |> Map.tryFind symId
        |> Option.map (fun sym -> flipSymbolInBlock flip symCentre sym) // The symbol is the only one in the block

    match flippedSymbol with
    | Some (sym) ->
        let newSymbols =
            (symId, sym)
            |> symMap.Add
        Optic.set SheetT.symbols_ newSymbols sheet
    | None ->
        printf $"Given symbol {symbol.Component.Label} does not exist on the sheet. Returning sheet before change."
        sheet


/// Implements an arbitrary flip or rotate on both components in the sheet
/// and gets a random flip and rotate value.
let arbitraryFlipRotate =
    let rSeed = randomInt 0 1 3
    let fSeed = randomInt 0 1 2
    let rotate =
        match rSeed.Data 0 with
        | 0 -> None // According to https://edstem.org/us/courses/51379/discussion/4281774: Rotation needs to be patched. Please patch before testing!!
        | 1 -> Some Rotation.Degree90
        | 2 -> Some Rotation.Degree180 // Patch of using pos in adjustPosForRotation did not seem to work.
        | 3 -> Some Rotation.Degree270
        | errVal -> failwithf $"Seed ranges from 0-3, no value outside this range should be generated: {errVal}"
    let flip =
        match fSeed.Data 0 with
        | 0 -> None
        | 1 -> Some SymbolT.FlipHorizontal
        | errVal -> failwithf $"Seed ranges from 0-3, no value outside this range should be generated: {errVal}"
    printfn $"Rotation: {rotate}, Flip: {flip}"
    {|rotateSeed = rotate; flipSeed = flip|}


/// Given a model, applies the completed sheetAlignScale and judges if visual quality
/// has improved or not
///
/// model - The sheet to be beautified and checked.
let evaluateBeautification (model: SheetT.Model) =
    let straightWires (model: SheetT.Model) =
        model.Wire.Wires
        |> Map.toList
        |> List.map fst
        |> List.fold (fun wires wId -> if List.length (HLPTick3.visibleSegments wId model) = 1 then wires + 1 else wires) 0

    let beautifiedModel = sheetAlignScale model

    let initialStraightWires = straightWires model
    let finalStraightWires = straightWires beautifiedModel

    let percentWiresStraightened = int (100.0 * float (finalStraightWires - initialStraightWires) / float initialStraightWires)
    let symbolIntersectionCount = T1.intersectingSymbolPairs beautifiedModel
    let wireSymbolIntersectionCount = T2.visSegsIntersectSymbol beautifiedModel

    match (percentWiresStraightened, symbolIntersectionCount, wireSymbolIntersectionCount) with
    | (wires, _, _) when wires < 0 ->
        $">> sheetAlignScale failed - Initial Straight Wires: {initialStraightWires} | Final Straight Wires: {finalStraightWires}"
        |> WiresNotStraightened
        |> Failure
    | (_, count, _) when count > 0 ->
        $">> sheetAlignScale failed - Pairs of intersecting symbols: {count}"
        |> SymbolIntersections
        |> Failure
    | (_, _, count) when count > 0 ->
        $">> sheetAlignScale failed - Wires intersecting symbols: {count}"
        |> WireSymbolIntersections
        |> Failure
    | _ -> Success ($">> {percentWiresStraightened}" + "% wires straightened")


/// Given a sheet, deconstructs all the information present in the circuit.
///
/// model - Sheet to be replicated.
let deconstructCircuit (model: SheetT.Model) =

    /// Finds mapping between portId and symbol for all ports on the sheet.
    let portSymbolMap =
        (Map.toList >> List.map snd) model.Wire.Symbol.Symbols
        |> List.collect (fun sym ->
            sym.PortMaps.Orientation
            |> Map.toList
            |> List.map (fun (portId, _) -> portId, sym))
        |> Map.ofList

    /// For all symbols on the sheet, breaks them down into label, type and position.
    let symbolsInfo =
        // Similar to the functions above, this function can be fed into Builder.placeSymbol to
        // conveniently place symbols on the sheet.
        model.Wire.Symbol.Symbols
        |> (Map.toList >> List.map snd)
        |> List.map (fun sym ->
            let actualPos = {X = sym.Pos.X + float sym.Component.W / 2.0; Y = sym.Pos.Y + float sym.Component.H / 2.0}
            // Need to get the actual X and Y position back. See Symbol.createNewSymbol for why.
            sym.Component.Label, sym.Component.Type, actualPos)

    /// For a given port ID and port type, returns the label of the symbol
    /// the port is attached to, and the index associated with the port on
    /// that symbol.
    let findPortSymbolIndex (portId: string) (portType: PortType) =
        // This function is crucial in generating information about the connections on the sheet.
        // The output of this function can be directly fed to the Builder.placeWire function (label, port index)
        // This function does not perform error handling when trying to find the symbol attached to a particular
        // port ID, because it is assumed said port ID will always exist (since this function is used in conjunction)
        // with the port ID:symbol mapping produced earlier.
        let sym = Map.find portId portSymbolMap

        match portType with
            | PortType.Input -> sym.Component.InputPorts
            | PortType.Output -> sym.Component.OutputPorts
        |> List.tryFindIndex (fun (port:Port) -> port.Id = portId)
        |> Option.defaultWith (fun _ ->
                                printfn $"The given port ID: {portId} does not exist on the sheet"
                                0)
        |> (fun portNum -> sym.Component.Label, portNum)

    /// Generates a list of all of the symbol(port) <-> (port)symbol connections on the sheet.
    let connectionsInfo =
        // This function uses findPortSymbolIndex to construct a list of all of the said connections
        // on the sheet.
        model.Wire.Wires
        |> (Map.toList >> List.map snd)
        |> List.map (fun wire -> match wire.InputPort, wire.OutputPort with
                                        | InputPortId iPort, OutputPortId oPort ->
                                            findPortSymbolIndex oPort PortType.Output,
                                            // Output port needs to be first - this serves as the source port for the wire.
                                            findPortSymbolIndex iPort PortType.Input)
        |> List.map (fun (symInfo1, symInfo2) -> symInfo1 ||> portOf, symInfo2 ||> portOf)

    printfn $"Symbols: {symbolsInfo}"
    printfn $"Connections: {connectionsInfo}"

    {| Symbols = symbolsInfo; Connections = connectionsInfo |} // Anonymous record.


/// Given data from a deconstructed circuit, reconstructs it and returns the sheet.
let reconstructCircuit
    (data: {| Symbols: List<string * ComponentType * XYPos>; Connections: List<SymbolPort * SymbolPort> |}) =

    (Ok initSheetModel, data.Symbols)
    ||> List.fold (fun sheet (label, compType, pos) ->
                        Result.bind (Builder.placeSymbol label compType pos) sheet)
    |> (fun connections sheet ->
            (sheet, connections)
            ||> List.fold (fun sheet (sourceSymbolPort, targetSymbolPort) ->
                                Result.bind(Builder.placeWire sourceSymbolPort targetSymbolPort) sheet)) data.Connections
    |> TestLib.getOkOrFail


let verticalLinePositions =
    fromList [-42.0..7.0..42.0]


// Initial test circuit with 1 MUX, crossed wires for input symbol and an output symbol with offset.
let simpleMuxTest (inputPos: XYPos) =
    initSheetModel
    |> Builder.placeSymbol "I1" (Input1(1, None)) {X = inputPos.X; Y = inputPos.Y + 40.0}
    |> Result.bind (Builder.placeSymbol "I2" (Input1(1, None)) {X = inputPos.X; Y = inputPos.Y - 40.0})
    |> Result.bind (Builder.placeSymbol "MUX" (Mux2) middleOfSheet)
    |> Result.bind (Builder.placeSymbol "S1" (Input1(1, None)) {X = middleOfSheet.X - 20.0; Y = middleOfSheet.Y + 150.0})
    |> Result.bind (Builder.placeSymbol "O1" (Output(1)) {X = middleOfSheet.X + 100.0; Y = middleOfSheet.Y + 20.0})
    |> TestLib.getOkOrFail
    |> rotateSymbol Degree90 "S1"
    |> Builder.placeWire (portOf "I1" 0) (portOf "MUX" 0)
    |> Result.bind (Builder.placeWire (portOf "S1" 0) (portOf "MUX" 2))
    |> Result.bind (Builder.placeWire (portOf "I2" 0) (portOf "MUX" 1))
    |> Result.bind (Builder.placeWire (portOf "MUX" 0) (portOf "O1" 0))
    |> TestLib.getOkOrFail
    |> deconstructCircuit
    |> reconstructCircuit // Tested by deconstructing and reconstructing the circuit.
    |> UnstraightenedSimpleMUX


// Simple yet important case - what to do when there are two non straight wires coming out of a
// singly connected component? Also highlights 4 visible segments for the select input in certain orientations.
// 4 visible segments != nearly straight, but it CAN be straightened. Extension work.
let multipleConnectionsTest (offsetFromCentre: float) =
    initSheetModel
    |> Builder.placeSymbol "I1" (Input1(1, None)) {X = middleOfSheet.X; Y = middleOfSheet.Y + offsetFromCentre}
    |> Result.bind (Builder.placeSymbol "S" (Input1(1, None)) {X = middleOfSheet.X; Y = middleOfSheet.Y - offsetFromCentre})
    |> Result.bind (Builder.placeSymbol "MUX" (Mux2) middleOfSheet)
    |> Result.bind (Builder.placeSymbol "O1" (Output(1)) {X = middleOfSheet.X + 100.0; Y = middleOfSheet.Y + 20.0})
    |> Result.bind (Builder.placeWire (portOf "I1" 0) (portOf "MUX" 0))
    |> Result.bind (Builder.placeWire (portOf "I1" 0) (portOf "MUX" 1))
    |> Result.bind (Builder.placeWire (portOf "S" 0) (portOf "MUX" 2))
    |> Result.bind (Builder.placeWire (portOf "MUX" 0) (portOf "O1" 0))
    |> TestLib.getOkOrFail
    |> MultipleConnections


// Circuit demonstrating enough degrees of freedom to straighten non singly constrained components.
let makeMultipleMux (_: XYPos) =
    initSheetModel
    |> Builder.placeSymbol "MUX2" Mux2 middleOfSheet
    |> Result.bind (Builder.placeSymbol "MUX1" Mux2 {X = middleOfSheet.X - 175.0; Y = middleOfSheet.Y - 28.0})
    |> Result.bind (Builder.placeSymbol "A" (Input1(1, None)) {X = middleOfSheet.X - 300.0; Y = middleOfSheet.Y - 56.0})
    |> Result.bind (Builder.placeSymbol "B" (Input1 (1, None)) {X = middleOfSheet.X - 300.0; Y = middleOfSheet.Y})
    |> Result.bind (Builder.placeSymbol "S2" (Input1 (1, None)) {X = middleOfSheet.X - 250.0; Y = middleOfSheet.Y + 100.0})
    |> Result.bind (Builder.placeSymbol "C" (Output(1)) {X = middleOfSheet.X + 150.0; Y = middleOfSheet.Y})
    |> Result.bind (Builder.placeWire (portOf "A" 0) (portOf "MUX1" 0))
    |> Result.bind (Builder.placeWire (portOf "B" 0) (portOf "MUX1" 1))
    |> Result.bind (Builder.placeWire (portOf "S2" 0) (portOf "MUX2" 2))
    |> Result.bind (Builder.placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
    |> Result.bind (Builder.placeWire (portOf "MUX2" 0) (portOf "C" 0))
    |> TestLib.getOkOrFail


// Circuit emulating results from orderFlip, for compatibility with the other beautify algorithms.
let orderFlipTest (_: XYPos) =
    initSheetModel
    |> Builder.placeSymbol "S1" (Input1(1, None)) {X = middleOfSheet.X - 275.0; Y = middleOfSheet.Y - 40.0}
    |> Result.bind (Builder.placeSymbol "S2" (Input1(1, None)) {X = middleOfSheet.X - 275.0; Y = middleOfSheet.Y + 60.0})
    |> Result.bind (Builder.placeSymbol "MUX2" (Mux2) middleOfSheet)
    |> Result.bind (Builder.placeSymbol "MUX1" (Mux2) {X = middleOfSheet.X - 150.0; Y = middleOfSheet.Y - 150.0})
    |> Result.bind (Builder.placeSymbol "G1" (GateN(And, 2)) {X = middleOfSheet.X + 150.0; Y = middleOfSheet.Y - 160.0})
    |> Result.bind (Builder.placeWire (portOf "S1" 0) (portOf "MUX2" 1))
    |> Result.bind (Builder.placeWire (portOf "S2" 0) (portOf "MUX2" 2))
    |> Result.bind (Builder.placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
    |> Result.bind (Builder.placeWire (portOf "MUX1" 0) (portOf "G1" 0))
    |> Result.bind (Builder.placeWire (portOf "MUX2" 0) (portOf "G1" 1))
    |> TestLib.getOkOrFail

