module SheetBeautifyD3

open CommonTypes
open DrawModelType.SymbolT
open DrawModelType
open Optics
open Symbol
open BlockHelpers
open Helpers
open SheetBeautifyHelpers

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



/// Computes total length of a single wire by summing the lengths of all its segments.
/// Takes as input a wire.
let getWireLength (wire : BusWireT.Wire) : float =
    wire.Segments
    |> List.sumBy (fun (segment: BusWireT.Segment) -> segment.Length)

/// Gives the list of all wires in the model which can be cosidered as long wires.
/// Takes as input the model, and threshold value to check if long or not.
let identifyLongWires (model : BusWireT.Model) (longLen : float) : BusWireT.Wire list =
    model.Wires
    |> Map.toList
    |> List.map (fun (_, wire) -> wire) 
    |> List.filter (fun wire -> getWireLength wire > longLen)

/// Produces a new SheetT.model with the wires identified as long wires removed.
/// Takes as input the model, the list of all wires identified as long wires.
let removeLongWires (model : SheetT.Model) (longWires : BusWireT.Wire list) : SheetT.Model =
    let longWireIDs = longWires
                      |> List.map (fun wire -> wire.WId)
                      |> Set.ofList

    let removeSegments (wire : BusWireT.Wire) =
        if Set.contains wire.WId longWireIDs then // checks to see if wire has been identified as Long-Wire
            { wire with Segments = [] } // if so, remove the segemnts so that they do not exist in the model
        else
            wire // or else just return existing wire if not a long wire

    let updatedWires = model.Wire.Wires
                       |> Map.map (fun _ wire -> removeSegments wire)

    { model with Wire = { model.Wire with Wires = updatedWires } }
     


/// Computes the absolute position of the WireLabel to be placed.
/// Takes as input the port ID, the port's absolute position, the symbol on which the port resides, and the full model.
// TODO:
// Need to check for available room before placing the label.
let computeLabelPos (portID : string) (portPos : XYPos) (sym : Symbol) (model: SymbolT.Model) : XYPos =
    let orientation = BlockHelpers.getPortOrientationFrmPortIdStr model portID

    // relative positioning based on which side of the symbol the port resides on, so as not to overlap the symbol
    match orientation with
    | Top -> { portPos with Y = portPos.Y + 30.0 }          
    | Bottom -> { portPos with Y = portPos.Y - 30.0 }
    | Left -> { portPos with X = portPos.X - 30.0 }
    | Right -> { portPos with X = portPos.X + 30.0 }

/// Generates the appropiate label text for the WireLabels to be placed.
/// Takes as input the Source port ID and the model.
/// Uses the symbol on which the source port resides' name, and the port number of the port, to make the label.
let generateLabelText (portID : string) (model : SymbolT.Model)=
    let port = BlockHelpers.getPort model portID
    let sym = BlockHelpers.getSymbol model portID

    match port.PortNumber with
    | Some number -> sym.Component.Label + "_" + (string number) // uses symbol's label + port number of the symbol
    | None -> sym.Component.Label + "_0"

/// Produces the model with WireLables placed (on both ends) for a single wire.
/// Takes as input a wire, and the sheet model.
/// Adds the labels to both beside the source port and the target port.
let placeWireLabels (wire : BusWireT.Wire) (sheetModel : SheetT.Model) : SheetT.Model =

    let model = sheetModel.Wire.Symbol

    // sub-function to perform the labl placing operation
    let addLabelBesidePort portID labelText theme symbolsMap =
        let symbol = BlockHelpers.getSymbol model portID
        let portPos = getPortPos portID model
        let labelPos = computeLabelPos portID portPos symbol model
        let labelSymbol = Symbol.createNewSymbol [] labelPos IOLabel labelText theme
        Map.add labelSymbol.Id labelSymbol symbolsMap

    let sourcePortID = BlockHelpers.getOutputPortIdStr wire.OutputPort
    let targetPortID = BlockHelpers.getInputPortIdStr wire.InputPort

    let labelText = generateLabelText sourcePortID model

    let sourceSideLabelAdded = addLabelBesidePort sourcePortID labelText model.Theme model.Symbols

    let targetSideLabelAdded = addLabelBesidePort targetPortID labelText model.Theme sourceSideLabelAdded

    let updatedSymbolModel = { model with Symbols = targetSideLabelAdded } // updated SymbolT.Model
    let updatedBusWireTModel = { sheetModel.Wire with Symbol = updatedSymbolModel } // updated BusWireT.Model

    // updated final SheetT.Model
    { sheetModel with Wire = updatedBusWireTModel} 


/// Returns the final model with all long wires removed and WireLabels placed.
/// Takes as input the initial SheetT.Model.
let getLabelModeModel (initialModel : SheetT.Model) : SheetT.Model =
    let longWires = identifyLongWires initialModel.Wire 300.0

    let modelLongWiresRemoved = removeLongWires initialModel longWires

    let modelWithLabels = longWires
                        |> List.fold (fun updatedModel wire ->
                            placeWireLabels wire updatedModel
                         ) modelLongWiresRemoved

    modelWithLabels

  
// The Main Function
/// Switches between WIRE_MODE and LABEL_MODE models based on the toggleMode user input (will be updated later)
let sheetWireLabelSymbol (initialModel : SheetT.Model) (toggleMode: bool) : SheetT.Model =
    let savedWireModeModel = initialModel

    match toggleMode with
    | true -> getLabelModeModel initialModel // LABEL_MODE
    | false -> savedWireModeModel // WIRE_MODE
