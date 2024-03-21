module TestDrawBlockProject


open CommonTypes
open DrawModelType
open DrawModelType.BusWireT
open DrawModelType.SheetT
open DrawModelType.SymbolT
open Elmish
open Fable
open Fable.React
open GenerateData
open ModelType
open Optics
open PopupHelpers
open SheetBeautifyHelpers
open TestDrawBlock
open TestDrawBlock.HLPTick3
open TestDrawBlock.HLPTick3.Asserts
open TestDrawBlock.HLPTick3.Builder
open TestDrawBlock.HLPTick3.Tests
open TestLib
open SheetBeautifyD1
open SheetBeautifyD2
open SheetBeautifyD3



//--------------------------------------------------------------------------------------------------------------------------//
//-----------------------------------------------------Type Definitions-----------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//
// Define a new type that combines an XYPos with an Port Connections and Component type
type posConnectionsComponent = {
    Position: XYPos
    Connections: int * int
    Component: ComponentType
}

type posConnectionsPair = {
    Position: XYPos
    Connections: int * int
}

// Define a new type that combines an XYPos with an optional FlipType
type posFlipPair = {
    Position: XYPos
    Flip: Option<FlipType>
}

type posFlipComponent = {
    Position: XYPos
    Flip: Option<FlipType>
    Component: ComponentType
}

// Define new Type for Integrated Tests
type posFlipComponentConnections = {
    Position: XYPos
    Flip: Option<FlipType>
    Connections: int * int
    Component: ComponentType
}


//--------------------------------------------------------------------------------------------------------------------------//
//-----------------------------------------------------Helper Functions-----------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//

/// Function to print any value
let print x = printfn "%A" x

/// <summary> Prints metrics related to wire intersections and right angles within a given sheet model. </summary>
/// <param name="model">The sheet model to analyze.</param>
let printMetrics (model : SheetT.Model) : unit =
    // T1
    let symbolPairIntersection : int = numOfIntersectedSymPairs model
    print ($"Number of Symbol and Symbol Intersections: {symbolPairIntersection}")
    
    // T2
    let segmentSymbolIntersections : int = numOfIntersectSegSym model
    print ($"Number of Symbol and Wire Intersections: {segmentSymbolIntersections}")

    // T3
    let wireIntersections : int = numOfWireRightAngleCrossings model
    print ($"Number of Wire Crossings: {wireIntersections}")
    
    // T4
    let totalSegLength : float = calcVisWireLength model
    print ($"Total Segment Length: {totalSegLength}")

    // T5
    let wireRightAngles : int = numOfVisRightAngles model
    print ($"Number of Wire Right angles: {wireRightAngles}")

    let wireLabelIntersections: int = countWireLabelIntersections model
    print ($"Number of Wire Label Intersections: {wireLabelIntersections}")

/// <summary> Prints the difference in metrics between two given sheet models. </summary>
/// <param name="model1">The sheet model before the beautification.</param>
/// <param name="model2">The sheet model after the beautification.</param>
let calcMetricsDiff (model1 : SheetT.Model) (model2 : SheetT.Model) : unit =
    let symbolPairIntersection1 : int = numOfIntersectedSymPairs model1
    let symbolPairIntersection2 : int = numOfIntersectedSymPairs model2
    print ($"Number of Symbol and Symbol Intersections Removed: {symbolPairIntersection1-symbolPairIntersection2}")
    
    // T2
    let segmentSymbolIntersections1 : int = numOfIntersectSegSym model1
    let segmentSymbolIntersections2 : int = numOfIntersectSegSym model2
    print ($"Number of Symbol and Wire Intersections Removed: {segmentSymbolIntersections1 - segmentSymbolIntersections2}")

    // T3
    let wireIntersections1 : int = numOfWireRightAngleCrossings model1
    let wireIntersections2 : int = numOfWireRightAngleCrossings model2
    print ($"Number of Wire Crossings Removed: {wireIntersections1 - wireIntersections2}")
    
    // T4
    let totalSegLength1 : float = calcVisWireLength model1
    let totalSegLength2 : float = calcVisWireLength model2
    print ($"Total Segment Length Reduced: {totalSegLength1 - totalSegLength2}")

    // T5
    let wireRightAngles1 : int = numOfVisRightAngles model1
    let wireRightAngles2 : int = numOfVisRightAngles model2
    print ($"Number of Wire Right angles Removed: {wireRightAngles1 - wireRightAngles2}")
    
    let wireLabelIntersections1 : int = countWireLabelIntersections model1
    let wireLabelIntersections2 : int = countWireLabelIntersections model2
    print ($"Number of Wire Label Intersections Removed: {wireLabelIntersections1 - wireLabelIntersections2}")


/// <summary> Finds the ID of a symbol within a model by its label. </summary>
/// <param name="model">The sheet model containing the symbols.</param>
/// <param name="targetLabel">The label of the symbol to find.</param>
/// <returns> The ID of the symbol if found, wrapped in an Option type; otherwise, None. </returns>
let findSymbolIdByLabel (model: SheetT.Model) (targetLabel: string) : Option<ComponentId> =
    model.Wire.Symbol.Symbols
    |> Map.toSeq 
    |> Seq.tryFind (fun (_, sym) -> sym.Component.Label.ToUpper() = targetLabel.ToUpper()) 
    |> Option.map fst


/// <summary> Flips a symbol within the model based on the specified orientation. </summary>
/// <param name="symLabel">The label of the symbol to flip.</param>
/// <param name="orientation">The orientation to flip the symbol to.</param>
/// <param name="model">The current sheet model containing the symbol.</param>
/// <returns> A Result containing the updated model if the symbol is found and flipped, or the original model if the symbol is not found.</returns>
let flipSymbol (symLabel: string) (orientation: FlipType option) (model : SheetT.Model) : Result<SheetT.Model, string> =
    match orientation with
    | None -> Ok model // Return the original model if no flip orientation is provided
    | Some(orientation) ->
        match findSymbolIdByLabel model symLabel with
        | Some(id) ->
            let sym = model.Wire.Symbol.Symbols[id]
            let updatedSym = SymbolResizeHelpers.flipSymbol orientation sym
            let updatedSymbols = Map.add id updatedSym model.Wire.Symbol.Symbols
            let newModel = { model with Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbols } } }
            Ok newModel
        | None -> Ok model


/// <summary>Generates a 2x2 grid of XYPos positions based on an offset from the middle of the sheet.</summary>
/// <param name="offsetX">The horizontal offset from the middle of the sheet for the grid's starting position.</param>
/// <param name="offsetY">The vertical offset from the middle of the sheet for the grid's starting position.</param>
/// <returns>A Gen<XYPos> representing a list of positions for a 2x2 grid.</returns>
let makeGrid (offsetX: float) (offsetY: float) =
    let gridStep = 50.0
    let x, y = middleOfSheet.X, middleOfSheet.Y
    [{X = x + offsetX; Y = y + offsetY};
     {X = x + offsetX + gridStep; Y = y + offsetY};
     {X = x + offsetX; Y = y + offsetY + gridStep};
     {X = x + offsetX + gridStep; Y = y + offsetY + gridStep}]
    |> fromList

let verticalLinePositions midPos stepSize =
    fromList [-30..stepSize..30]
    |> map (fun n -> midPos + { X = 0.; Y = float n })
let horizontallLinePositions midPos stepSize =
    fromList [-150..stepSize..50]
    |> map (fun n -> midPos + { X = float n; Y = 0. })

let randomXPositions =
    [-300.0..15.0..300.0]
    |> List.map (fun xpos -> {X=xpos; Y=0})
    |> fromList

let failOnLabelWireIntersection (sample:int) model =
    let intersections = countWireLabelIntersections model
    if intersections = 0 then
        None
    else
        Some $"Wire intersects a wire label in Sample {sample}, at least {intersections} intersections"

let failOnOversizeWires (sample:int) (model:SheetT.Model) : string option =
    let threshold = 200.0 // Need to find a good threshold
    let wireLengths = SegmentHelpers.allWireNets model
                      |> List.collect (fun (_, net) -> SegmentHelpers.getVisualSegsFromNetWires true model net)
                      |> List.map( fun (startP,endP) -> euclideanDistance startP endP)
    let maxWireLength = List.max wireLengths
    if maxWireLength > threshold then
        Some $"Wire length exceeds threshold in Sample {sample}, max length: {maxWireLength}"
    else
        None



let beautifyOnOddTests tests =
    tests
    |> toList
    |> List.collect (fun t -> [(t,false); (t,true)])
    |> fromList    

//--------------------------------------------------------------------------------------------------------------------------//
//----------------------------------------------------Beautify Functions----------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//

// Placeholder for actual implementation
let beautify (model : SheetT.Model) : SheetT.Model = 
    replaceLongWiresWithLabels model 250

///<summary>Displays a popup dialog for setting a user-defined threshold for beautifying a circuit.</summary>
///<param name="model">The model containing the circuit data.</param>
///<param name="func">A function to beautify the circuit, which takes the current model and returns the updated model.</param>
///<param name="dispatch">Dispatch</param>
let userThresholdPopUp (model: ModelType.Model) (func: SheetT.Model -> float -> SheetT.Model) (dispatch: Dispatch<Msg>) =
    let title = $"Beautify circuit"
    let beforeInt = fun _ -> str "Maximum wire length before replacing with wire label"
    let intDefault = 200
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Beautify"
    let buttonAction =
        fun (model': Model) ->
            // Use optic to get sheet
            let sheet = Optic.get sheet_ model

            // Print metrics for current Sheet
            print "\nMetrics before beautifying"
            printMetrics sheet
            let dialogData = model'.PopupDialogData
            let outputInt = getInt dialogData
            let sheet = Optic.get sheet_ model
            let newSheet = func sheet (float outputInt)
            print "\nMetrics after beautifying"
            printMetrics newSheet
            print "\nDifference in Metrics"
            calcMetricsDiff sheet newSheet
            let newModel = Optic.set sheet_ newSheet model
            showSheetInIssieSchematic newModel.Sheet dispatch
            dispatch ClosePopup
    let isDisabled = fun (model': Model) -> getInt model'.PopupDialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


/// <summary> Beautifies the given sheet within the model then updates and displays the new sheet. Prints metrics before and after changes. </summary>
/// <param name="model">The model containing the sheet to be beautified.</param>
/// <param name="dispatch"> dispatch</param>
let beautifySheet (model : ModelType.Model) (func: SheetT.Model -> SheetT.Model) (dispatch: Dispatch<Msg>): unit = 
    // Use optic to get sheet
    let sheet = Optic.get sheet_ model
    
    // Print metrics for current Sheet
    print "\nMetrics before beautifying"
    printMetrics sheet
    
    // Beautify sheet and print new metrics
    let newSheet = func sheet
    print "\nMetrics after beautifying"
    printMetrics newSheet
    
    print "\nDifference in Metrics"
    calcMetricsDiff sheet newSheet

    // Update the Model with the new sheet and display it
    let newModel = Optic.set sheet_ newSheet model
    showSheetInIssieSchematic newModel.Sheet dispatch



//--------------------------------------------------------------------------------------------------------------------------//
//----------------------------------------Example Test Circuits using Gen<'a> samples---------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//


//------------------------------------------------------- Circuit 1 --------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//


let input1PosCircuit1 = middleOfSheet + { X = -140; Y = 176 }
let input2PosCircuit1 = middleOfSheet + { X = -230; Y = 100 }
let mux1PosCircuit1 = middleOfSheet + { X = -150; Y = -90 }
let mux2PosCircuit1 = middleOfSheet - { X = 0; Y = 80 }

let verticalLinePairs = product (fun a b -> (a, b)) (verticalLinePositions mux1PosCircuit1 15) (verticalLinePositions mux2PosCircuit1 15)


let makeCircuit1 (symPosInfo : XYPos * XYPos) =
    let (mux1Pos, mux2Pos) = symPosInfo
    initSheetModel
    |> placeSymbol "MUX1" Mux2 mux1Pos
    |> Result.bind (placeSymbol "MUX2" Mux2 mux2Pos)
    |> Result.bind (placeSymbol "A" (Input1 (1, None)) (middleOfSheet + { X = -350; Y = -130 }))
    |> Result.bind (placeSymbol "B" (Input1 (1, None)) (middleOfSheet + { X = -350; Y = -50 }))
    |> Result.bind (placeSymbol "C" (Output 1) (middleOfSheet + { X = 150; Y = -75 }))
    |> Result.bind (placeSymbol "S1" (Input1 (1, None)) input1PosCircuit1)
    |> Result.bind (placeSymbol "S2" (Input1 (1, None)) input2PosCircuit1)
    |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
    |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "C" 0))
    |> Result.bind (placeWire (portOf "A" 0) (portOf "MUX1" 0))
    |> Result.bind (placeWire (portOf "B" 0) (portOf "MUX1" 1))
    |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX1" 2))
    |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 1))
    |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2))
    |> getOkOrFail

//--------------------------------------------------------------------------------------------------------------------------//


//----------------------------------------------------- Circuit 2 ---------------------------------------------------------//
//-------------------------------------------------------------------------------------------------------------------------//

let randomComponents1Circuit2 = fromList [Mux4; GateN(And, 5)]
let randomComponents2Circuit2 = fromList [Not; DFF]
let randomComponents3Circuit2 = fromList [Mux2; NbitsAdder(2)]

let posGen1Circuit2 = verticalLinePositions (middleOfSheet - { X = 150.0; Y = 150.0 }) 30
let posGen2Circuit2 = verticalLinePositions (middleOfSheet + { X = 60.0; Y = -130.0 }) 30
let posGen3Circuit2 = verticalLinePositions (middleOfSheet + { X = 50.0; Y = 15.0 }) 30

let connections1Circuit2 = fromList [(0, 0); (1, 0); (3, 0); (4, 0);]
let connections2Circuit2 = fromList [(0, 0)]
let connections3Circuit2 = fromList [(0, 0); (1, 0); (3, 0);]

let posAndConnectionGen1 = product (fun (connections: int * int) (position: XYPos) -> { Position = position; Connections = connections }) connections1Circuit2 posGen1Circuit2 
let posAndConnectionGen2 = product (fun (connections: int * int) (position: XYPos) -> { Position = position; Connections = connections }) connections2Circuit2 posGen2Circuit2
let posAndConnectionGen3 = product (fun (connections: int * int) (position: XYPos) -> { Position = position; Connections = connections }) connections3Circuit2 posGen3Circuit2 

let posConnectionCompGen1 = product (fun (posConnections : posConnectionsPair) comp -> { Position = posConnections.Position; Connections = posConnections.Connections; Component = comp }) posAndConnectionGen1 randomComponents1Circuit2
let posConnectionCompGen2 = product (fun (posConnections : posConnectionsPair) comp -> { Position = posConnections.Position; Connections = posConnections.Connections; Component = comp }) posAndConnectionGen2 randomComponents2Circuit2
let posConnectionCompGen3 = product (fun (posConnections : posConnectionsPair) comp -> { Position = posConnections.Position; Connections = posConnections.Connections; Component = comp }) posAndConnectionGen3 randomComponents3Circuit2

let posConnectionCompGenPair = product (fun gen1 gen2 -> (gen1, gen2)) posConnectionCompGen1 posConnectionCompGen2
let posConnectionCompGenTriple = product (fun (gen1, gen2) gen3 -> (gen1, gen2, gen3)) posConnectionCompGenPair posConnectionCompGen3


let makeCircuit2 (symInfo : posConnectionsComponent * posConnectionsComponent * posConnectionsComponent) =
    let (sym1Info, sym2Info, sym3Info) = symInfo
    let sym1pos = sym1Info.Position
    let (sym1InputPort, sym1OutputPort) = sym1Info.Connections
    let sym1comp = sym1Info.Component
    let sym2pos = sym2Info.Position
    let (sym2InputPort, sym2OutputPort) = sym2Info.Connections
    let sym2comp = sym2Info.Component
    let sym3pos = sym3Info.Position
    let (sym3InputPort, sym3OutputPort) = sym3Info.Connections
    let sym3comp = sym3Info.Component
    initSheetModel
    |> placeSymbol "Sym1" sym1comp sym1pos
    |> Result.bind (placeSymbol "Sym2" sym2comp sym2pos)
    |> Result.bind (placeSymbol "Sym3" sym3comp sym3pos)
    |> Result.bind (placeSymbol "S1" (Input1 (1, None)) (middleOfSheet + { X = -350; Y = -130 }))
    |> Result.bind (placeSymbol "S2" (Input1 (1, None)) (middleOfSheet + { X = -350; Y = -50 }))
    |> Result.bind (placeSymbol "S3" (Input1 (1, None)) (middleOfSheet + { X = -300; Y = +10 }))
    |> Result.bind (placeSymbol "OUT1" (Output 1) (middleOfSheet + { X = 150; Y = -125 }))
    |> Result.bind (placeSymbol "OUT2" (Output 1) (middleOfSheet + { X = 150; Y = -5 }))
    |> Result.bind (placeWire (portOf "Sym1" sym1OutputPort) (portOf "Sym2" sym2InputPort))
    |> Result.bind (placeWire (portOf "Sym2" sym2OutputPort) (portOf "OUT1" 0))
    |> Result.bind (placeWire (portOf "S1" 0) (portOf "Sym1" sym1InputPort))
    |> Result.bind (placeWire (portOf "S2" 0) (portOf "Sym1" ((sym1InputPort + 3) % 4)))
    |> Result.bind (placeWire (portOf "S3" 0) (portOf "Sym3" sym3InputPort))
    |> Result.bind (placeWire (portOf "Sym3" sym3OutputPort) (portOf "OUT2" 0))
    // |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 1))
    // |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2))
    |> getOkOrFail

//--------------------------------------------------------------------------------------------------------------------------//


//----------------------------------------------------- Circuit 3 ---------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//
// Generate Random Flips
let flipTypeGen = fromList [None; Some FlipHorizontal; Some FlipVertical]

// Generate all pairs of FlipType
let flipTypePairs = product (fun a b -> (a, b)) flipTypeGen flipTypeGen

// Generate all triples by combining pairs with single flip types
let flipTypeTriples = product (fun (a, b) c -> (a, b, c)) flipTypePairs flipTypeGen

let flipTypeQuadruples = product (fun (a, b, c) d -> (a, b, c, d)) flipTypeTriples flipTypeGen

// Define the positions of Symbols in the Sheet for Circuit 1
let mux1PosCircuit3 : XYPos = middleOfSheet - { X = 150.0; Y = 175.0 }
let mux2PosCircuit3 : XYPos = middleOfSheet
let input1PosCircuit3 : XYPos = middleOfSheet - { X = 200.0; Y = -28.0 }
let input2PosCircuit3 : XYPos = middleOfSheet - { X = 200.0; Y = -90.0 }
let andPosCircuit3 : XYPos = middleOfSheet - { X = -180.0; Y = 180. }



// Generate a circuit with 2 inputs, 2 Mux2 and one and gate with random flips
let makeCircuit3 (symFlips : Option<FlipType> * Option<FlipType> * Option<FlipType>) =
    let flipMUX1, flipMUX2, flipGate = symFlips 
    let model = 
        initSheetModel
        |> placeSymbol "G1" (GateN(And, 2)) andPosCircuit3
        |> Result.bind (flipSymbol "G1" flipGate)
        |> Result.bind (placeSymbol "MUX1" Mux2 mux1PosCircuit3)
        |> Result.bind (flipSymbol "MUX1" flipMUX1)
        |> Result.bind (placeSymbol "MUX2" Mux2 mux2PosCircuit3)
        |> Result.bind (flipSymbol "MUX2" flipMUX2)
        |> Result.bind (placeSymbol "S1" (Input1(1, Some 1)) input1PosCircuit3)
        |> Result.bind (placeSymbol "S2" (Input1(1, Some 1)) input2PosCircuit3)
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "G1" 1))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "G1" 0))
        |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2))
        |> getOkOrFail
    model
//--------------------------------------------------------------------------------------------------------------------------//


//----------------------------------------------------- Circuit 4 ---------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//
// Define the positions of Symbols in the Sheet for Circuit 4
let notGatePosCircuit4 : XYPos = middleOfSheet - { X = 200.0; Y = 0.0 }
let mux1PosCircuit4 : XYPos = middleOfSheet - { X = 100.0; Y = 100.0 }
let mux2PosCircuit4 : XYPos = middleOfSheet + { X = -100.0; Y = 100.0 }
let orGatePosCircuit4 : XYPos = middleOfSheet + { X = 0.0; Y = 0.0 }
let inputPosCircuit4 : XYPos = middleOfSheet - { X = 300.0; Y = 0.0 }
let outputPosCircuit4 : XYPos = middleOfSheet + { X = 150.0; Y = 0.0 }

// Generate a circuit with an input, two Mux2s, a Not gate, an Or gate, and an output with random flips
let makeCircuit4 (symFlips : Option<FlipType> * Option<FlipType> * Option<FlipType> * Option<FlipType>) =
    let flipMUX1, flipMUX2, flipOr, flipNot = symFlips 
    let model = 
        initSheetModel
        |> placeSymbol "Not1" (Not) notGatePosCircuit4
        |> Result.bind (flipSymbol "Not1" flipNot)
        |> Result.bind (placeSymbol "MUX1" Mux2 mux1PosCircuit4)
        |> Result.bind (flipSymbol "MUX1" flipMUX1)
        |> Result.bind (placeSymbol "MUX2" Mux2 mux2PosCircuit4)
        |> Result.bind (flipSymbol "MUX2" flipMUX2)
        |> Result.bind (placeSymbol "Or1" (GateN(Or, 2)) orGatePosCircuit4)
        |> Result.bind (flipSymbol "Or1" flipOr)
        |> Result.bind (placeSymbol "In1" (Input1(1, Some 1)) inputPosCircuit4)
        |> Result.bind (placeSymbol "Out1" (Output 1) outputPosCircuit4)
        // Wire connections
        |> Result.bind (placeWire (portOf "In1" 0) (portOf "Not1" 0))
        |> Result.bind (placeWire (portOf "In1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "Not1" 0) (portOf "MUX1" 1))
        |> Result.bind (placeWire (portOf "Not1" 0) (portOf "MUX2" 2))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "Or1" 1))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "Or1" 0))
        |> Result.bind (placeWire (portOf "Or1" 0) (portOf "Out1" 0))
        |> getOkOrFail
    model

//--------------------------------------------------------------------------------------------------------------------------//


//----------------------------------------------------- Circuit 5 ---------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//

// Create the generator for Circuit with random positions
let grid1 = makeGrid 0.0 0.0
let grid2 = makeGrid -150.0 -100.0
let grid3 = makeGrid -100.0 50.0

// Combine position and flip into a PosAndFlip generator
let posAndFlipGen1 = product (fun position flip -> { Position = position; Flip = flip }) grid1 flipTypeGen
let posAndFlipGen2 = product (fun position flip -> { Position = position; Flip = flip }) grid2 flipTypeGen
let posAndFlipGen3 = product (fun position flip -> { Position = position; Flip = flip }) grid3 flipTypeGen

// Generate all combinations for three symbols
let posAndFlipPair = product (fun posFlip1 posFlip2 -> (posFlip1, posFlip2)) posAndFlipGen1 posAndFlipGen2
let posAndFlipTriplesGen = product (fun (posFlip1, posFlip2) posFlip3 -> (posFlip1, posFlip2, posFlip3)) posAndFlipPair posAndFlipGen3

// Generate a circuit with an input, two Mux2s, an And gate and an output with random flips and positioning
let makeCircuit5 (symInfo : posFlipPair * posFlipPair * posFlipPair) =
    let (MUX1info, MUX2info, andInfo) = symInfo
    let MUX1pos = MUX1info.Position
    let MUX1flip = MUX1info.Flip
    let MUX2pos = MUX2info.Position
    let MUX2flip = MUX2info.Flip
    let andPos = andInfo.Position
    let andFlip = andInfo.Flip
    let model = 
        initSheetModel
        |> placeSymbol "And1" (GateN(And, 2)) andPos
        |> Result.bind (flipSymbol "And1" andFlip)
        |> Result.bind (placeSymbol "MUX1" Mux2 MUX1pos)
        |> Result.bind (flipSymbol "MUX1" MUX1flip)
        |> Result.bind (placeSymbol "MUX2" Mux2 MUX2pos)
        |> Result.bind (flipSymbol "MUX2" MUX2flip)
        |> Result.bind (placeSymbol "In1" (Input1(1, Some 1)) inputPosCircuit4)
        |> Result.bind (placeSymbol "Out1" (Output 1) outputPosCircuit4)
        // Wire connections
        |> Result.bind (placeWire (portOf "In1" 0) (portOf "And1" 0))
        |> Result.bind (placeWire (portOf "In1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "And1" 0) (portOf "MUX1" 1))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "OUT1" 0))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "MUX1" 0))
        |> getOkOrFail
    model

//--------------------------------------------------------------------------------------------------------------------------//


//------------------------------------------------------- Circuit 6 --------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//

let randomComponents1 = fromList [Mux2; GateN(And, 3)]
let randomComponents2 = fromList [Not; DFF]
let randomComponents3 = fromList [Mux2; NbitsAdder(2)]

let posGen4 = fromList [middleOfSheet - { X = 10.0; Y = 150.0 }; middleOfSheet + { X = 40.0; Y = 150.0 } ]
let posGen5 = fromList [middleOfSheet - { X = 130.0; Y = -130.0 }; middleOfSheet + { X = -70.0; Y = 10.0 } ]
let posGen6 = fromList [middleOfSheet + { X = 50.0; Y = 15.0 }; middleOfSheet + { X = 50.0; Y = -50.0 } ]

let posAndFlipGen4 = product (fun position flip -> { Position = position; Flip = flip }) posGen4 flipTypeGen
let posAndFlipGen5 = product (fun position flip -> { Position = position; Flip = flip }) posGen5 flipTypeGen
let posAndFlipGen6 = product (fun position flip -> { Position = position; Flip = flip }) posGen6 flipTypeGen


// Combine position and flip into a PosAndFlip generator
let posFlipCompGen1 = product (fun (posFlip : posFlipPair) comp -> { Position = posFlip.Position; Flip = posFlip.Flip; Component = comp }) posAndFlipGen4 randomComponents1
let posFlipCompGen2 = product (fun (posFlip : posFlipPair) comp -> { Position = posFlip.Position; Flip = posFlip.Flip; Component = comp }) posAndFlipGen5 randomComponents2
let posFlipCompGen3 = product (fun (posFlip : posFlipPair) comp -> { Position = posFlip.Position; Flip = posFlip.Flip; Component = comp }) posAndFlipGen6 randomComponents3
let posFlipCompGenPair = product (fun gen1 gen2 -> (gen1, gen2)) posFlipCompGen1 posFlipCompGen2
let posFlipCompGenTriple = product (fun (gen1, gen2) gen3 -> (gen1, gen2, gen3)) posFlipCompGenPair posFlipCompGen3


let makeCircuit6 (symInfo : posFlipComponent * posFlipComponent * posFlipComponent) =
    let (sym1Info, sym2Info, sym3Info) = symInfo
    let sym1pos = sym1Info.Position
    let sym1flip = sym1Info.Flip
    let sym1comp = sym1Info.Component
    let sym2pos = sym2Info.Position
    let sym2flip = sym2Info.Flip
    let sym2comp = sym2Info.Component
    let sym3pos = sym3Info.Position
    let sym3flip = sym3Info.Flip
    let sym3comp = sym3Info.Component
    let model = 
        initSheetModel
        |> placeSymbol "Sym3" sym3comp sym3pos
        |> Result.bind (flipSymbol "sym3" sym3flip)
        |> Result.bind (placeSymbol "sym1" sym1comp sym1pos)
        |> Result.bind (flipSymbol "sym1" sym1flip)
        |> Result.bind (placeSymbol "sym2" sym2comp sym2pos)
        |> Result.bind (flipSymbol "sym2" sym2flip)
        |> Result.bind (placeSymbol "In1" (Input1(1, Some 1)) inputPosCircuit4)
        |> Result.bind (placeSymbol "Out1" (Output 1) outputPosCircuit4)
        // Wire connections
        |> Result.bind (placeWire (portOf "In1" 0) (portOf "sym1" 0))
        |> Result.bind (placeWire (portOf "In1" 0) (portOf "sym2" 0))
        |> Result.bind (placeWire (portOf "sym1" 0) (portOf "Sym3" 1))
        |> Result.bind (placeWire (portOf "Sym3" 0) (portOf "OUT1" 0))
        |> Result.bind (placeWire (portOf "sym2" 0) (portOf "Sym3" 0))
        |> Result.bind (placeWire (portOf "sym2" 0) (portOf "sym1" 2))
        |> getOkOrFail
    model


//--------------------------------------------------------------------------------------------------------------------------//

//------------------------------------------------------- Circuit 7 --------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//
let makeCircuit7 (muxDistance : XYPos) =
    let mux2Offset = (muxDistance.X * 2.0) % 200.0
    let muxDistance2 = { X = mux2Offset; Y = 300}
    let model = 
        initSheetModel
        |> placeSymbol "DMUX1" (Demux4) (middleOfSheet + muxDistance)
        |> Result.bind (placeSymbol "MUX1" Mux4 middleOfSheet)
        |> Result.bind (placeSymbol "MUX2" Mux4 (middleOfSheet + muxDistance2))
        |> Result.bind (placeWire (portOf "DMUX1" 0) (portOf "MUX1" 0))
        |> Result.bind (placeWire (portOf "DMUX1" 1) (portOf "MUX1" 1))
        |> Result.bind (placeWire (portOf "DMUX1" 2) (portOf "MUX1" 2))
        |> Result.bind (placeWire (portOf "DMUX1" 3) (portOf "MUX1" 3))
        |> Result.bind (placeWire (portOf "DMUX1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "DMUX1" 1) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "DMUX1" 2) (portOf "MUX2" 2))
        |> Result.bind (placeWire (portOf "DMUX1" 3) (portOf "MUX2" 3))
        |> getOkOrFail
    model

//--------------------------------------------------------------------------------------------------------------------------//


//------------------------------------------------------- Circuit 8 --------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//

// Define Component Positions

// Sym1 definitions
let sym1PosCircuit8 = middleOfSheet + {X = -150; Y = -230}
let posGen1Circuit8 = horizontallLinePositions sym1PosCircuit8 100
let sym1Flips : FlipType option list = [None]
let sym1FlipsGen = fromList sym1Flips
let sym1Connections = fromList [(1,0); (1, 1); (1, 3)]
let sym1Components = fromList [Demux4]
let sym1PosFlipGen = product (fun pos flip -> {Position = pos; Flip =  flip}) posGen1Circuit8 sym1FlipsGen
let sym1PosFlipComponentGen = product (fun (posFlip: posFlipPair) comp -> {Position = posFlip.Position; Flip = posFlip.Flip; Component = comp}) sym1PosFlipGen sym1Components
let sym1PosFlipComponentConnectionGen = product (fun (posFlipComponent : posFlipComponent) connections -> {Position = posFlipComponent.Position; 
                                                                            Flip = posFlipComponent.Flip; 
                                                                            Component = posFlipComponent.Component;
                                                                            Connections = connections}) sym1PosFlipComponentGen sym1Connections


// Sym2 definitions
let sym2PosCircuit8 = middleOfSheet + {X = 0; Y = -230}
let posGen2Circuit8 = verticalLinePositions sym2PosCircuit8 15
let sym2Flips : FlipType option list = [None]
let sym2FlipsGen = fromList sym1Flips
let sym2Connections = fromList [(0,0);]
let sym2Components = fromList [Mux4; GateN(And, 5)]
let sym2PosFlipGen = product (fun pos flip -> {Position = pos; Flip =  flip}) posGen2Circuit8 sym2FlipsGen
let sym2PosFlipComponentGen = product (fun (posFlip: posFlipPair) comp -> {Position = posFlip.Position; Flip = posFlip.Flip; Component = comp}) sym2PosFlipGen sym2Components
let sym2PosFlipComponentConnectionGen = product (fun (posFlipComponent : posFlipComponent) connections -> {Position = posFlipComponent.Position; 
                                                                            Flip = posFlipComponent.Flip; 
                                                                            Component = posFlipComponent.Component;
                                                                            Connections = connections}) sym2PosFlipComponentGen sym2Connections

// Sym3 definitions
let posGen3Circuit8 = makeGrid 0 0
let sym3Flips : FlipType option list = [None]
let sym3FlipsGen = fromList sym1Flips
let sym3Connections = fromList [(0,0); (2,0)]
let sym3Components = fromList [Mux2]
let sym3PosFlipGen = product (fun pos flip -> {Position = pos; Flip =  flip}) posGen3Circuit8 sym3FlipsGen
let sym3PosFlipComponentGen = product (fun (posFlip: posFlipPair) comp -> {Position = posFlip.Position; Flip = posFlip.Flip; Component = comp}) sym3PosFlipGen sym3Components
let sym3PosFlipComponentConnectionGen = product (fun (posFlipComponent : posFlipComponent) connections -> {Position = posFlipComponent.Position; 
                                                                            Flip = posFlipComponent.Flip; 
                                                                            Component = posFlipComponent.Component;
                                                                            Connections = connections}) sym3PosFlipComponentGen sym3Connections

// combine all sym info
let sym1And2 = product (fun sym1 sym2 -> (sym1, sym2)) sym1PosFlipComponentConnectionGen sym2PosFlipComponentConnectionGen
let allSymInfoCircuit8 = product (fun (sym1, sym2) sym3 -> (sym1, sym2, sym3)) sym1And2 sym3PosFlipComponentConnectionGen

let integratedTestCircuit (symInfo : posFlipComponentConnections * posFlipComponentConnections * posFlipComponentConnections) =
    let (sym1Info, sym2Info, sym3Info) = symInfo
    let sym1pos = sym1Info.Position
    let sym1flip = sym1Info.Flip
    let sym1comp = sym1Info.Component
    let (sym1InputPort, sym1OutputPort) = sym1Info.Connections
    let sym2pos = sym2Info.Position
    let sym2flip = sym2Info.Flip
    let sym2comp = sym2Info.Component
    let (sym2InputPort, sym2OutputPort) = sym2Info.Connections
    let sym3pos = sym3Info.Position
    let sym3flip = sym3Info.Flip
    let sym3comp = sym3Info.Component
    let (sym3InputPort, sym3OutputPort) = sym3Info.Connections
    initSheetModel
    |> placeSymbol "Sym3" sym3comp sym3pos
    |> Result.bind (flipSymbol "Sym3" sym3flip)
    |> Result.bind (placeSymbol "Sym2" sym2comp sym2pos)
    |> Result.bind (placeSymbol "Sym1" sym1comp sym1pos)
    |> Result.bind (placeSymbol "S1" (Input1 (1, None)) (middleOfSheet + { X = -400; Y = -160 }))
    |> Result.bind (placeSymbol "S2" (Input1 (1, None)) (middleOfSheet + { X = -350; Y = -10 }))
    |> Result.bind (placeSymbol "OUT1" (Output 1) (middleOfSheet + { X = 150; Y = -125 }))
    |> Result.bind (placeSymbol "OUT2" (Output 1) (middleOfSheet + { X = 150; Y = -5 }))
    |> Result.bind (placeWire (portOf "S1" 0) (portOf "Sym1" sym1InputPort))
    |> Result.bind (placeWire (portOf "S2" 0) (portOf "Sym3" sym3InputPort))
    |> Result.bind (placeWire (portOf "Sym1" sym1OutputPort) (portOf "Sym2" sym2InputPort))
    |> Result.bind (placeWire (portOf "Sym1" ((sym1InputPort + 2) % 4)) (portOf "Sym2" ((sym2InputPort + 3) % 4)))
    |> Result.bind (placeWire (portOf "Sym2" sym2OutputPort) (portOf "OUT1" 0))
    |> Result.bind (placeWire (portOf "Sym3" sym3OutputPort) (portOf "OUT2" 0))
    |> Result.bind (placeWire (portOf "Sym1" 3) (portOf "Sym3" 1))
    |> getOkOrFail

//--------------------------------------------------------------------------------------------------------------------------//


//--------------------------------------------------------------------------------------------------------------------------//
//---------------------------------------------- Demo tests on Draw Block code ---------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//
let test1 testNum firstSample dispatch =
    runTestOnSheets
        "2 muxes single connected and 2 gates connected"
        firstSample
        verticalLinePairs
        makeCircuit1
        failOnAllTests
        dispatch
    |> recordPositionInTest testNum dispatch
    
let test2 testNum firstSample dispatch =
    runTestOnSheets
        "2 muxes and Viewers connected as per example Fig A1"
        firstSample
        posConnectionCompGenTriple
        makeCircuit2
        failOnAllTests
        dispatch
    |> recordPositionInTest testNum dispatch

let test3 testNum firstSample dispatch =
    runTestOnSheets
        "Manually Generated: 2 MUXes and 1 Gate With Random Flips"
        firstSample
        flipTypeTriples
        makeCircuit3 
        failOnAllTests
        dispatch
    |> recordPositionInTest testNum dispatch

let test4 testNum firstSample dispatch =
    runTestOnSheets
        "Manually Generated: 2 MUXes and 2 Gates With Random Flips"
        firstSample
        flipTypeQuadruples
        makeCircuit4
        failOnAllTests
        dispatch
    |> recordPositionInTest testNum dispatch

let test5 testNum firstSample dispatch =
    runTestOnSheets
        "Randomly Generated: 2 MUXes and a Gate With Random Flips"
        firstSample
        posAndFlipTriplesGen
        makeCircuit5
        failOnWireIntersectsSymbol
        dispatch
    |> recordPositionInTest testNum dispatch

let test6 testNum firstSample dispatch =
    runTestOnSheets
        "Randomly Generated Components, Flips and Positions"
        firstSample
        posFlipCompGenTriple
        makeCircuit6
        failOnAllTests
        dispatch
    |> recordPositionInTest testNum dispatch

let test7 testNum firstSample dispatch =
    runTestOnSheets
        "2 MUXes With Random Distance"
        firstSample
        randomXPositions
        makeCircuit7
        failOnAllTests
        dispatch
    |> recordPositionInTest testNum dispatch

let test8 testNum firstSample dispatch =
    runTestOnSheets
        "Integrated Test"
        firstSample
        allSymInfoCircuit8
        integratedTestCircuit
        failOnAllTests
        dispatch
    |> recordPositionInTest testNum dispatch


let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
    // Change names and test functions as required
    // delete unused tests from list
    [
        // D1
        "Test1", test1
        "Test2", test2
        
        // D2
        "Test3", test3
        "Test4", test4
        "Test5", test5
        "Test6", test6
        
        // D3
        "Test7", test7

        // Integrated Test
        "Test8", test8
        "Next Test Error", fun _ _ _ -> printf "Next Error:" // Go to the next error in a test

    ]

let testMenuFunc (testIndex: int) (dispatch: Dispatch<Msg>) (model: Model) =
            let name,func = testsToRunFromSheetMenu[testIndex] 
            printf "%s" name
            match name, model.DrawBlockTestState with
            | "Next Test Error", Some state ->
                nextError testsToRunFromSheetMenu[state.LastTestNumber] (state.LastTestSampleIndex+1) dispatch
            | "Next Test Error", None ->
                printf "Test Finished"
                ()
            | "Run Beautify Function", _ ->
                beautifySheet model beautify dispatch
                printf "Test Finished"
                ()
            | _ ->
                func testIndex 0 dispatch

let beautifyFunctionsMenu : (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
    [
        "D1 Beautify Function", fun _ _ _ -> printf "D1 Beautify Function"
        "D2 Beautify Function", fun _ _ _ -> printf "D2 Beautify Function"
        "D3 Beautify Function", fun _ _ _ -> printf "D3 Beautify Function"
        "Integrated Beautify Function", fun _ _ _ -> printf "Integrated Beautify Function"
    ]

let beautifyMenuFunc (testIndex: int) (dispatch: Dispatch<Msg>) (model: Model) =
    let name, _ = beautifyFunctionsMenu[testIndex]
    printf "%s" name
    match name, model.DrawBlockTestState with
    | "D1 Beautify Function", _ -> 
        beautifySheet model sheetAlignScale dispatch
    | "D2 Beautify Function", _ -> 
        beautifySheet model findBestModel dispatch
    | "D3 Beautify Function", _ -> 
        userThresholdPopUp model replaceLongWiresWithLabels dispatch
    | "Integrated Beautify Function", _ ->
        userThresholdPopUp model replaceLongWiresWithLabels dispatch
    | _ -> failwithf "Shouldn't happen buddy"