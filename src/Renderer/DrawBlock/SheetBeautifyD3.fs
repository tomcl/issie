module SheetBeautifyD3

open CommonTypes
open DrawModelType.SymbolT
open DrawModelType
open BlockHelpers
open SheetBeautifyHelpers



(*
//////////////////////////////////////// Algorithm FlowChart //////////////////////////////////////////////////

    Program Flow:
        - sheetWireLabelSymbol
                 |
                 '---> getLabelModeModel
                        |
                        |---> identifyWiresToRemove
                        |         |
                        |         |---> getWireLength
                        |         |---> countRightAngleBendsInWire
                        |         |     |
                        |         |     '---> isRightAngleBend
                        |         |
                        |         '---> countCrossingsOnWire
                        |        
                        '---> removeWires
                        |
                        |---> placeWireLabels
                        |         |
                        |         |---> generateLabelText
                        |         |---> addLabelBesidePort
                        |                  |
                        |                  |---> computeLabelPos
                        |                  |---> generateLabelText
                        |                  '---> rotateLabelSymbol
                        |
                        '---> placeSmallWire

////////////////////////////////////////////////////////////////////////////////////////////////////////////
*)



// Record to hold the properties that define wires to be removed 
type wireProperties = {
    minLength : float
    minRightAngleBends : int
    minWireCrossings : int
    }

/// <summary>
/// Calculates the total length of a wire by summing the lengths of its segments.
/// </summary>
/// <param name="wire">The wire for which to calculate the length.</param>
/// <returns>The total length of the wire.</returns>
let getWireLength (wire : BusWireT.Wire) : float =
    wire.Segments
    |> List.sumBy (fun (segment: BusWireT.Segment) -> abs(segment.Length))

/// <summary>
/// Determines if two wire segments form a right angle.
/// </summary>
/// <param name="seg1">The first segment, defined by its start and end positions.</param>
/// <param name="seg2">The second segment, defined by its start and end positions.</param>
/// <returns>True if the segments form a right angle; otherwise, false.</returns>
let isRightAngleBend (seg1: XYPos*XYPos) (seg2: XYPos*XYPos) =
    let orientation1 = SegmentHelpers.visSegOrientation seg1
    let orientation2 = SegmentHelpers.visSegOrientation seg2
    match orientation1, orientation2 with
    | BusWireT.Orientation.Horizontal, BusWireT.Orientation.Vertical 
    | BusWireT.Orientation.Vertical, BusWireT.Orientation.Horizontal -> true
    | _ -> false

/// <summary>
/// Counts the number of right-angle bends in a wire.
/// </summary>
/// <param name="model">The sheet model containing the wire.</param>
/// <param name="wire">The wire to examine for right-angle bends.</param>
/// <returns>The count of right-angle bends in the wire.</returns>
let countRightAngleBendsInWire (model: SheetT.Model) (wire: BusWireT.Wire) =
    let segments = SegmentHelpers.visibleSegsWithVertices wire model 
    segments
    |> List.pairwise 
    |> List.filter (fun (seg1, seg2) -> isRightAngleBend seg1 seg2) 
    |> List.length 

/// <summary>
/// Counts the number of crossings on a wire with other wires.
/// </summary>
/// <param name="model">The sheet model containing the wires.</param>
/// <param name="wire">The wire to examine for crossings.</param>
/// <returns>The count of wire crossings.</returns>
let countCrossingsOnWire (model: SheetT.Model) (wire: BusWireT.Wire): int = 
    model.Wire.Wires
    |> Map.values
    |> Seq.filter (fun otherWire -> otherWire <> wire && otherWire.OutputPort <> wire.OutputPort)
    |> Seq.collect (fun otherWire ->
        let wireSegments = getNonZeroAbsSegments wire
        let otherWireSegments = getNonZeroAbsSegments otherWire
        List.allPairs wireSegments otherWireSegments |> List.filter (fun (seg1, seg2) ->
            let direction1 = seg1.End - seg1.Start
            let direction2 = seg2.End - seg2.Start
            let dotProduct = direction1.X * direction2.X + direction1.Y * direction2.Y
            dotProduct = 0.0 && overlap1D (seg1.Start.X, seg1.End.X) (seg2.Start.X, seg2.End.X) && overlap1D (seg1.Start.Y, seg1.End.Y) (seg2.Start.Y, seg2.End.Y)))
    |> Seq.distinct
    |> Seq.length

/// <summary>
/// Identifies wires that should be removed based on specified bad wire properties.
/// </summary>
/// <param name="model">The bus wire model containing the wires.</param>
/// <param name="properties">The properties used to determine which wires to remove.</param>
/// <param name="sheetModel">The sheet model for additional context.</param>
/// <returns>A list of wires that should be removed.</returns>
let identifyWiresToRemove (model: BusWireT.Model) (properties : wireProperties) (sheetModel: SheetT.Model): BusWireT.Wire list =
    model.Wires
    |> Map.values
    |> Seq.filter (fun wire ->
        let wireLength = getWireLength wire
        let rightAngleCount = countRightAngleBendsInWire sheetModel wire
        let crossingCount = countCrossingsOnWire sheetModel wire
        wireLength >= properties .minLength || rightAngleCount >= properties .minRightAngleBends || crossingCount >= properties .minWireCrossings)
    |> Seq.toList

/// <summary>
/// Removes specified bad wires from the model.
/// </summary>
/// <param name="model">The sheet model from which wires will be removed.</param>
/// <param name="wires">The list of wires to be removed.</param>
/// <returns>The sheet model after removal of the specified wires.</returns>
let removeWires (model: SheetT.Model) (wires: BusWireT.Wire list) : SheetT.Model =
    wires
    |> List.map (fun wire -> wire.WId)   
    |> Set.ofList                         
    |> fun wiresToRemove ->
        model.Wire.Wires
        |> Map.filter (fun key _ -> not (Set.contains key wiresToRemove)) 
        |> fun updatedWires -> 
            let updatedWireModel = { model.Wire with Wires = updatedWires }
            let finalWireModel, _ = BusWireUpdate.calculateBusWidths updatedWireModel
            { model with Wire = finalWireModel }

/// <summary>
/// Computes the position and edge orientation for placing a label next to a port.
/// </summary>
/// <param name="portID">The ID of the port next to which the label will be placed.</param>
/// <param name="portPos">The position of the port.</param>
/// <param name="sym">The symbol associated with the port.</param>
/// <param name="model">The model containing the symbol and port.</param>
/// <returns>The position for the label and the edge orientation relative to the port.</returns>
let computeLabelPos (portID : string) (portPos : XYPos) (sym : Symbol) (model: SymbolT.Model) : XYPos * Edge =
    let orientation = BlockHelpers.getPortOrientationFrmPortIdStr model portID

    match orientation with
    | Top -> { X = portPos.X + 15.0; Y = portPos.Y - 60.0 }, Top
    | Bottom -> { X = portPos.X + 15.0; Y = portPos.Y + 60.0}, Bottom
    | Left -> { X = portPos.X - 60.0; Y = portPos.Y + 0.0 }, Left
    | Right -> { X = portPos.X + 60.0; Y = portPos.Y + 0.0 }, Right

/// <summary>
/// Generates the text for a label associated with a port.
/// </summary>
/// <param name="portID">The ID of the port for which the label text is generated.</param>
/// <param name="model">The model containing the port and associated symbol.</param>
/// <returns>The generated label text.</returns>
let generateLabelText (portID : string) (model : SymbolT.Model) : string=
    let port = BlockHelpers.getPort model portID
    let sym = BlockHelpers.getSymbol model portID

    match port.PortNumber with
    | Some number -> sym.Component.Label + "_" + (string number) 
    | None -> sym.Component.Label + "_0"

/// <summary>
/// Rotates a label symbol based on the port orientation.
/// </summary>
/// <param name="labelCompId">The component ID of the label symbol to rotate.</param>
/// <param name="portOrientation">The orientation of the port next to which the label is placed.</param>
/// <param name="model">The model containing the label symbol.</param>
/// <returns>The model with the rotated label symbol.</returns>
let rotateLabelSymbol (labelCompId : ComponentId) (portOrientation : Edge) (model : Model) : Model =
    let labelSymbol = model.Symbols.[labelCompId] 

    let rotatedLabelSymbol = 
        match portOrientation with
        | Top -> SymbolResizeHelpers.rotateSymbol Degree270 labelSymbol
        | Bottom -> SymbolResizeHelpers.rotateSymbol Degree90 labelSymbol
        | _ -> labelSymbol

    let updatedSymbols = Map.add labelCompId rotatedLabelSymbol model.Symbols 

    {model with Symbols = updatedSymbols} 

/// <summary>
/// Adds a label next to a port and returns the updated model and port pairs.
/// </summary>
/// <param name="portID">The ID of the port next to which the label will be added.</param>
/// <param name="labelText">The text of the label to add.</param>
/// <param name="model">The model to which the label will be added.</param>
/// <param name="oldPortPairs">Existing port pairs before adding the new label.</param>
/// <param name="portType">The type of the port ("Target" or other).</param>
/// <returns>The updated model and the list of port pairs including the new label.</returns>
let addLabelBesidePort (portID : string) (labelText : string) (model : Model) (oldPortPairs : List<(string * string)>) (portType : string) : (Model * List<(string * string)>) =
    let symbol = BlockHelpers.getSymbol model portID
    let portPos = SheetBeautifyHelpers.getPortPos portID model
    let labelPos, portOrientation = computeLabelPos portID portPos symbol model
    let labelAddedModel, labelCompId = SymbolUpdate.addSymbol [] model labelPos IOLabel labelText

    let updatedLabelModel = rotateLabelSymbol labelCompId portOrientation labelAddedModel

    let labelComponent = updatedLabelModel.Symbols
                            |> Map.find labelCompId 

    let labelPortId = 
        match portType with
        | "Target" -> labelComponent.Component.OutputPorts
                        |> List.head
                        |> fun port -> port.Id
        | _ -> labelComponent.Component.InputPorts
                 |> List.head
                 |> fun port -> port.Id

    let pair =
        match portType with
        | "Target" -> (portID, labelPortId)
        | _ -> (labelPortId, portID)

    (updatedLabelModel, pair :: oldPortPairs)

/// <summary>
/// Places labels for a wire, updating the model with label symbols next to the wire's ports.
/// </summary>
/// <param name="wire">The wire for which labels will be placed.</param>
/// <param name="sheetModel">The sheet model to update with the wire labels.</param>
/// <param name="placedSourceLabels">A set of source port IDs for which labels have already been placed.</param>
/// <param name="portPairs">The list of port pairs to be updated with the new labels.</param>
/// <returns>The updated sheet model, the updated set of placed source labels, and the updated list of port pairs.</returns>
let placeWireLabels (wire: BusWireT.Wire) (sheetModel: SheetT.Model) (placedSourceLabels: Set<string>) (portPairs: List<(string * string)>): (SheetT.Model * Set<string> * List<(string * string)>) =
    let model = sheetModel.Wire.Symbol
    let sourcePortID = BlockHelpers.getOutputPortIdStr wire.OutputPort
    let targetPortID = BlockHelpers.getInputPortIdStr wire.InputPort
    let labelText = generateLabelText sourcePortID model

    let targetModel, targetPairs = addLabelBesidePort targetPortID labelText model portPairs "Target"
    let updatedModel = { sheetModel with Wire = { sheetModel.Wire with Symbol = targetModel } }

    let finalModel, finalPairs =
        if Set.contains sourcePortID placedSourceLabels then
            updatedModel, targetPairs
        else
            let sourceModel, sourcePairs = addLabelBesidePort sourcePortID labelText targetModel targetPairs "Source"
            { updatedModel with Wire = { updatedModel.Wire with Symbol = sourceModel } }, sourcePairs

    let updatedPlacedSourceLabels = Set.add sourcePortID placedSourceLabels

    (finalModel, updatedPlacedSourceLabels, finalPairs)

/// <summary>
/// Places the small wire connecting the port of the symbol, and the port of the wire-label placed.
/// </summary>
/// <param name="model">The sheet model to be updated with the new wire.</param>
/// <param name="inputPortStr">The input port ID as a string.</param>
/// <param name="outputPortStr">The output port ID as a string.</param>
/// <returns>The updated model with the new wire added.</returns>
let placeSmallWire (model: SheetT.Model) (inputPortStr, outputPortStr) =
    let inputPortId = InputPortId inputPortStr
    let outputPortId = OutputPortId outputPortStr

    let newWire = BusWireUpdate.makeNewWire inputPortId outputPortId model.Wire

    let updatedWires = Map.add newWire.WId newWire model.Wire.Wires

    { model with Wire = { model.Wire with Wires = updatedWires } }

/// <summary>
/// Updates the model by removing long wires based on specified properties, then places labels and small connecting wires.
/// </summary>
/// <param name="initialModel">The initial model containing all wires and components.</param>
/// <param name="properties">Criteria for identifying wires to remove (length, right angle bends, wire crossings).</param>
/// <returns>The updated model, with bad wires replaced by labels and connecting wires, after modifications.</returns>
let modelWithWireLabels (initialModel : SheetT.Model) (properties : wireProperties) : SheetT.Model =

    let badWires = identifyWiresToRemove initialModel.Wire properties initialModel

    let modelWithNoBadWires = removeWires initialModel badWires

    let modelWithLabelsPlaced, _, allSymbolPortPairs = badWires
                                                    |> List.fold (fun (model, sourceLabels, portPairs) wire ->
                                                        let updatedModel, updatedSourceLabels, updatedPortPairs = placeWireLabels wire model sourceLabels portPairs
                                                        updatedModel, updatedSourceLabels, updatedPortPairs) (modelWithNoBadWires, Set.empty, [])

    let newModel = allSymbolPortPairs
                       |> List.fold placeSmallWire modelWithLabelsPlaced

    newModel

/// <summary>
/// Main function for enhancing the model by removing complex wires that are too long,
/// has too many bends, or too many wire crossings.
/// </summary>
/// <param name="initialModel">The starting sheet model for processing.</param>
/// <returns>The processed model with improved clarity and organization.</returns>
let sheetWireLabelSymbol (initialModel: SheetT.Model): SheetT.Model =

    // Adding initial model to the undo list
    let newUndoList, newRedoList = SheetUpdateHelpers.appendUndoList initialModel.UndoList initialModel, initialModel.RedoList

    let modelToUpdate = {initialModel with UndoList = newUndoList; RedoList = newRedoList}

    // Set properties that define a bad wire
    let badWireProperties = { minLength = 1000; minRightAngleBends = 3; minWireCrossings = 4 }  

    modelWithWireLabels modelToUpdate badWireProperties
