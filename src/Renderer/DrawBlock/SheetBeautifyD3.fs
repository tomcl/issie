module SheetBeautifyD3

open CommonTypes
open DrawModelType.SymbolT
open DrawModelType
open Optics
open Optics.Operators
open Symbol
open BlockHelpers
open Helpers
open SheetBeautifyHelpers
open BusWireUpdate

(*
//////////////////////////////////////// Planned Approach //////////////////////////////////////////////////

    Program Flow:
        - sheetWireLabelSymbol
                 |
                 '---> checkToggleMode <------------------------;-----------------------------;
                        |           |                           |                             |
             LABEL_MODE |           |WIRE_MODE                  |                             |
                        |           |                           |                             |
                        |           '---> savedWireModeModel--->'                             |
                        |                                                                     |
                        |                                                                     |
                        '---> getLabelModeModel --------------------------------------------->'
                                |            |                                             
                                |            '-> placeWireLabels      
                                |                           |
                                |                           '---> computeLabelPos
                                |                                    |
                                |                                    '---> generateLabelText
                                |                                                             
                                '-> identifyLongWires                                         
                                        |                                                     
                                        '-> removeLongWires                                   

    Methodology:
        1) Save a copy of the current wire segments
        2) Check toggle mode to choose from LABEL_MODE or WIRE_MODE:
            2.1) For LABEL_MODE:
                    a) Identify long wires (len > 300 units for now)
                    b) Remove current visible long wire segemnts
                    c) Create wire labels for both input and output ends of the wire:
                          i) Make the label text from source symbol label + port number
                    d) Position labels on both input and output ports (keeping 2 unit wire segments, then placing the wire label)
            2.2) For WIRE_MODE:
                    a) Load saved copy of wire segments (from step 1) 
                                                                            
   Notes:
    1) Size of each grid: 30.0 x 30.0 units
    2) WireLabel type is IOLabel


    TO-DOs:
        1) Need to place a small segment of wire in between teh port on the symbol and the placed wire label.
        2) Need to flip the placed wire label's orientation based on the symbol's oreintation.

   
////////////////////////////////////////////////////////////////////////////////////////////////////////////
*)




/// Optic to access BusWireT.Model from SheetT.Model
let busWireModel_ = SheetT.wire_

/// Identify a port from its component label and number.
/// Usually both an input and output port will mathc this, so
/// the port is only unique if it is known to be input or output.
/// used to specify the ends of wires, since tehee are known to be
/// connected to outputs (source) or inputs (target).
type SymbolPort = { Label: string; PortNumber: int }

/// convenience function to make SymbolPorts
let portOf (label:string) (number: int) =
    {Label=label; PortNumber = number}

/// Add a (newly routed) wire, source specifies the Output port, target the Input port.
/// Return an error if either of the two ports specified is invalid, or if the wire duplicates and existing one.
/// The wire created will be smart routed but not separated from other wires: for a nice schematic
/// separateAllWires should be run after  all wires are added.
/// source, target: respectively the output port and input port to which the wire connects.
let placeWire (source: SymbolPort) (target: SymbolPort) (model: SheetT.Model) : Result<SheetT.Model,string> =
    let symbols = model.Wire.Symbol.Symbols
    let getPortId (portType:PortType) symPort =
        mapValues symbols
        |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symPort.Label)
        |> function | Some x -> Ok x | None -> Error "Can't find symbol with label '{symPort.Label}'"
        |> Result.bind (fun sym ->
            match portType with
            | PortType.Input -> List.tryItem symPort.PortNumber sym.Component.InputPorts
            | PortType.Output -> List.tryItem symPort.PortNumber sym.Component.OutputPorts
            |> function | Some port -> Ok port.Id
                        | None -> Error $"Can't find {portType} port {symPort.PortNumber} on component {symPort.Label}")
            
    match getPortId PortType.Input target, getPortId PortType.Output source with
    | Error e, _ | _, Error e -> Error e
    | Ok inPort, Ok outPort ->
        let newWire = BusWireUpdate.makeNewWire (InputPortId inPort) (OutputPortId outPort) model.Wire
        if model.Wire.Wires |> Map.exists (fun wid wire -> wire.InputPort=newWire.InputPort && wire.OutputPort = newWire.OutputPort) then
                // wire already exists
                Error "Can't create wire from {source} to {target} because a wire already exists between those ports"
        else
                model
                |> Optic.set (busWireModel_ >-> BusWireT.wireOf_ newWire.WId) newWire
                |> Ok











/// Computes total length of a single wire by summing the lengths of all its segments.
/// Takes as input a wire.
let getWireLength (wire : BusWireT.Wire) : float =
    wire.Segments
    |> List.sumBy (fun (segment: BusWireT.Segment) -> abs(segment.Length))

/// Gives the list of all wires in the model which can be cosidered as long wires.
/// Takes as input the model, and threshold value to check if long or not.
let identifyLongWires (model : BusWireT.Model) (longLen : float) : BusWireT.Wire list =
    model.Wires
    |> Map.toList
    |> List.map (fun (_, wire) -> wire) 
    |> List.filter (fun wire -> getWireLength wire > longLen)

/// Produces a new SheetT.model with the wires identified as long wires removed.
/// Takes as input the model, the list of all wires identified as long wires.
let removeWires (model: SheetT.Model) (wires: BusWireT.Wire list) : SheetT.Model =
    let connectionIDs = wires
                        |> List.map (fun wire -> wire.WId)

    let wiresToRemove = Set.ofList connectionIDs

    let updatedWires = model.Wire.Wires
                        |> Map.filter (fun key _ -> not (Set.contains key wiresToRemove))

    let updatedModel = { model with Wire = { model.Wire with Wires = updatedWires } }

    let finalModel, _ = BusWireUpdate.calculateBusWidths updatedModel.Wire

    { updatedModel with Wire = finalModel }










let computeLabelPos (portID : string) (portPos : XYPos) (sym : Symbol) (model: SymbolT.Model) : XYPos =
    let orientation = BlockHelpers.getPortOrientationFrmPortIdStr model portID

    // relative positioning based on which side of the symbol the port resides on, so as not to overlap the symbol
    match orientation with
    | Top -> { portPos with Y = portPos.Y + 30.0 }          
    | Bottom -> { portPos with Y = portPos.Y - 30.0 }
    | Left -> { portPos with X = portPos.X - 30.0 }
    | Right -> { portPos with X = portPos.X + 30.0 }



let generateLabelText (portID : string) (model : SymbolT.Model)=
    let port = BlockHelpers.getPort model portID
    let sym = BlockHelpers.getSymbol model portID

    match port.PortNumber with
    | Some number -> sym.Component.Label + "_" + (string number) 
    | None -> sym.Component.Label + "_0"



let placeWireLabels (wire : BusWireT.Wire) (sheetModel : SheetT.Model) (placedSourceLabels : Set<string>) (portPairs : List<SymbolPort*SymbolPort>) : SheetT.Model * Set<string> * List<SymbolPort*SymbolPort> = 

    let model = sheetModel.Wire.Symbol

    let addLabelBesidePort portID labelText model oldPortPairs portType =
        let symbol = BlockHelpers.getSymbol model portID
        let portPos = getPortPos portID model
        let labelPos = computeLabelPos portID portPos symbol model
        let labelAddedModel, _ = SymbolUpdate.addSymbol [] model labelPos IOLabel labelText

        let symbolPort = portOf labelText 0

        let labelPort =
            let sourcePort = getPort model portID
            let sourcePortNumber =  match sourcePort.PortNumber with       
                                    | Some number -> number
                                    | None -> 0
            portOf symbol.Component.Label sourcePortNumber

        let updatedPortPairs =
            if portType = "Target" then   
                printfn $"T: {labelPort}{symbolPort}"
                oldPortPairs @ [(labelPort, symbolPort)]
            else
                printf $"S: {labelPort}{symbolPort}"
                oldPortPairs @ [(symbolPort, symbolPort)]

  
        labelAddedModel, updatedPortPairs


    let sourcePortID = BlockHelpers.getOutputPortIdStr wire.OutputPort
    let targetPortID = BlockHelpers.getInputPortIdStr wire.InputPort
    let labelText = generateLabelText sourcePortID model

    let targetSideLabelAdded, updatedPortPairs = addLabelBesidePort targetPortID labelText model portPairs "Target"
    
    let sourceSideLabelAdded, finalPortPairs = if Set.contains sourcePortID placedSourceLabels then
                                                   targetSideLabelAdded, updatedPortPairs
                                               else
                                                   addLabelBesidePort sourcePortID labelText targetSideLabelAdded updatedPortPairs "Source"



    let updatedPlacedSourceLabels = Set.add sourcePortID placedSourceLabels

    let updatedSymbolModel = sourceSideLabelAdded
    let updatedBusWireTModel = { sheetModel.Wire with Symbol = updatedSymbolModel }
    let updatedSheetTModel = { sheetModel with Wire = updatedBusWireTModel }

    (updatedSheetTModel, updatedPlacedSourceLabels, finalPortPairs)







/// Returns the final model with all long wires removed and WireLabels placed.
/// Takes as input the initial SheetT.Model.
let getLabelModeModel (initialModel : SheetT.Model) : SheetT.Model =

    let longWires = identifyLongWires initialModel.Wire 0.0

    let modelLongWiresRemoved = removeWires initialModel longWires

    let modelWithLabels, _, allSymbolPortPairs = longWires
                                                 |> List.fold (fun (updatedModel, updatedSourcePortIDs, updatedPortPairs) wire ->
                                                      placeWireLabels wire updatedModel updatedSourcePortIDs updatedPortPairs)
                                                  (modelLongWiresRemoved, Set.empty, [])

    printf $"### PortPairs {allSymbolPortPairs}"
(*
    let modelWithSmallWires, _ =
                                allSymbolPortPairs
                                |> List.fold (fun (updatedModel, _) (sourcePort, targetPort) ->
                                    match placeWire sourcePort targetPort updatedModel with
                                    | Ok newModel -> (newModel, ())
                                    | Error errorMessage -> failwith errorMessage)
                                    (modelWithLabels, ())*)
    modelWithLabels
                                       








  
// The Main Function
let sheetWireLabelSymbol (initialModel : SheetT.Model) : SheetT.Model =

    let updatedModel = getLabelModeModel initialModel 

    updatedModel
    

































(*let placeWireLabels (wire: BusWireT.Wire) (sheetModel: SheetT.Model) (placedSourceLabels: Set<string>): (SheetT.Model * Set<string> * Set<(InputPortId * OutputPortId)>) =
    let model = sheetModel.Wire.Symbol

    let addLabelBesidePort portID labelText theme symbolsMap pairs =
        let symbol = BlockHelpers.getSymbol model portID
        let portPos = SheetBeautifyHelpers.getPortPos portID model 
        let labelPos = computeLabelPos portID portPos symbol model
        
        // Since addSymbol returns a new model and the ID of the added symbol, you must handle those here.
        // Assume that IOLabel is the ComponentType for the labels, and loadedComponents is available in this context.
        let newModel, labelSymbolId = SymbolUpdate.addSymbol [] model labelPos IOLabel labelText
        let labelSymbol = newModel.Symbols |> Map.find labelSymbolId
        
        // Retrieve the InputPortId from the added symbol (label) assuming the first port is the relevant one.
        let labelPortID = labelSymbol.Component.InputPorts |> List.head |> fun port -> port.Id
        let inputPort = InputPortId(labelPortID)
        let outputPort = OutputPortId(portID)

        newModel, Set.add (inputPort, outputPort) pairs

    let sourcePortID = BlockHelpers.getOutputPortIdStr wire.OutputPort
    let targetPortID = BlockHelpers.getInputPortIdStr wire.InputPort
    let labelText = generateLabelText sourcePortID model

    // Initial symbols and pairs from the model
    let initialSymbols = model.Symbols
    let initialPairs = Set.empty

    // Determine if a label needs to be added to the source side
    let newModelSource, sourcePairs =
        if Set.contains sourcePortID placedSourceLabels then
            model, initialPairs
        else
            addLabelBesidePort sourcePortID labelText model.Theme initialSymbols initialPairs

    // Add label to the target side
    let newModelTarget, targetPairs = addLabelBesidePort targetPortID labelText model.Theme newModelSource.Symbols sourcePairs

    // Update the placed source labels set
    let updatedPlacedSourceLabels = Set.add sourcePortID placedSourceLabels

    // Assemble the final model to return
    let updatedSymbolModel = { newModelTarget with Symbols = newModelTarget.Symbols }
    let updatedBusWireTModel = { sheetModel.Wire with Symbol = updatedSymbolModel }
    let updatedSheetTModel = { sheetModel with Wire = updatedBusWireTModel }

    updatedSheetTModel, updatedPlacedSourceLabels, targetPairs


// Function to convert a list of InputPortId*OutputPortId pairs to a list of string*string pairs
let convertPortPairsToStringPairs portPairs =
    portPairs |> List.map (fun (inputPort, outputPort) ->
        (BlockHelpers.inputPortStr inputPort, BlockHelpers.outputPortStr outputPort))


let placeSmallWires (pairs: Set<(InputPortId * OutputPortId)>) (initialModel: SheetT.Model): SheetT.Model =
    let processPair (accModel: BusWireT.Model) (portID, labelPortID) =
        // Assuming newWire function signature: 
        // newWire : InputPortId -> OutputPortId -> BusWireT.Model -> BusWireT.Model * BusWireT.Msg option
        let inputPortId = portID // Convert portID to InputPortId
        let outputPortId = labelPortID // Convert labelPortID to OutputPortId
        let updatedModel, _ = BusWireUpdate.newWire inputPortId outputPortId accModel
        updatedModel

    let busWireModel = initialModel.Wire // Extract BusWireT.Model from the initial SheetT.Model
    
    // Convert the set of pairs to a list for iteration
    let pairsList = Set.toList pairs

   
    // Go over each of the pairs and update the BusWireT.Model with new small wires
    let updatedBusWireModel = List.fold processPair busWireModel pairsList
    
    // Place the updated BusWireT.Model back into the SheetT.Model
    { initialModel with Wire = updatedBusWireModel }




let getLabelModeModel (initialModel : SheetT.Model) : (SheetT.Model) =
    let longWires = identifyLongWires initialModel.Wire 0.0

    let modelLongWiresRemoved = removeWires initialModel longWires

    // Process each long wire to place labels and accumulate the pairs of portID and labelPortID.
    let finalModel, _, finalPairs = longWires
                                        |> List.fold (fun (accModel, accPlacedSourceLabels, accPairs) wire ->
                                            let updatedModel, updatedPlacedSourceLabels, updatedPairs = placeWireLabels wire accModel accPlacedSourceLabels
                                            updatedModel, updatedPlacedSourceLabels, Set.union accPairs updatedPairs) 
                                        (modelLongWiresRemoved, Set.empty, Set.empty)

    //let completedModel = placeSmallWires finalPairs finalModel

    finalModel

*)
