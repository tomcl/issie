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
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open Symbol
open SymbolUpdate
open SheetBeautifyHelpers
open Optics
open Optics.Operators
open ModelType
open BlockHelpers
open BusWidthInferer

/// constants used by SheetBeautify
module Constants =
    // user-variable threshold
    // for classifying a single wire as long enough to be replaced by wire labels
    let longSingleWireLength = 100.

    // minimum distance between two adjacent symbols
    let minCompDistance = 7.
    let numInputsIOLabel, numOutputsIOLabel, heightIOLabel, widthIOLabel = getComponentProperties IOLabel "I"


/// Optic to access SheetT.Model from Issie Model
let sheetModel_ = sheet_

/// Optic to access BusWireT.Model from SheetT.Model
let busWireModel_ = SheetT.wire_

/// Optic to access SymbolT.Model from SheetT.Model
let symbolModel_ = SheetT.symbol_


// --------------------------------------------------------- D3 ---------------------------------------------------------
let generateWireLabel (model: SheetT.Model) (defaultName:string) : string =
// code adapted from SymbolUpdate.generateIOLabel
    let symList = List.map snd (Map.toList model.Wire.Symbol.Symbols)
    let newCompBaseName, newCompNo = extractIOPrefix defaultName []
    //printfn "s %s , n%i" newCompBaseName newCompNo
    let existingNumbers =
        symList
        |> List.collect (fun sym ->
            match sym.Component.Type with
            | IOLabel -> 
                let baseName,no = extractIOPrefix sym.Component.Label []
                if baseName = newCompBaseName then
                    [no]
                else []
            | _ -> []
        )
    match existingNumbers with
    | [] -> defaultName
    | [-1] ->
        match newCompNo with
        | -1 -> defaultName+"1"
        | _  -> defaultName
    | lst -> 
        let maxNo = List.max existingNumbers
        match List.exists (fun x -> x=newCompNo) lst with
        | true -> newCompBaseName + (string (maxNo+1)) 
        | false -> defaultName

let addIOLabelComp (pos:XYPos) (defaultLabel:string) (model:SheetT.Model) = 
    let labelName = generateWireLabel model defaultLabel
    let newSymbolModel, compId = addSymbol [] model.Wire.Symbol pos IOLabel labelName
    {model with Wire={model.Wire with Symbol = newSymbolModel}},compId

let deleteSingleWire (wId:ConnectionId) (model:SheetT.Model) =
// code adapted from BusWireUpdate.update (DeleteWires)
    // deletes wires from model, then runs bus inference
    // Issie model is not affected but will extract connections from wires
    // at some time.
    let newWires =
        model.Wire.Wires
        |> Map.remove wId
    {model with Wire={ model.Wire with Wires = newWires ; ErrorWires = [wId]}}

let placeSingleWire (sourcePortId: OutputPortId) (targetPortId: InputPortId) (model: SheetT.Model) =
// code adapted from TestDrawBlock.HLPTick3.Builder.placeWire
    let newWire = BusWireUpdate.makeNewWire targetPortId sourcePortId model.Wire
    // if model.Wire.Wires |> Map.exists (fun wid wire -> wire.InputPort=newWire.InputPort && wire.OutputPort = newWire.OutputPort) 
    // then  // wire already exists
    //     Error "Can't create wire from {source} to {target} because a wire already exists between those ports"
    // else
    model
    |> Optic.set (busWireModel_ >-> BusWireT.wireOf_ newWire.WId) newWire

let wireToWireLabels (targetWire:Wire) (model:SheetT.Model) = 
    let label = "I"
    let startPort = 
        match targetWire.OutputPort with
        | OutputPortId str -> str
    let startPos = getPortPosOnSheet startPort model
    let endPort = 
        match targetWire.InputPort with
        | InputPortId str -> str
    let endPos = getPortPosOnSheet endPort model
    
    // create IOLabel components for each port
    let startIOPos: XYPos = startPos+{X=startPos.X+Constants.minCompDistance; Y=startPos.Y-Constants.heightIOLabel/2.} // not taking flips/rotations into account
    let model,startIOLabelId =  addIOLabelComp startIOPos label model
    let startIOLabelLeftPortId = InputPortId (model.Wire.Symbol.Symbols[startIOLabelId].PortMaps.Order[Left][0])

    let endIOPos: XYPos = endPos+{X=endPos.X-Constants.minCompDistance-Constants.widthIOLabel; Y=endPos.Y-Constants.heightIOLabel/2.} // not taking flips/rotations into account
    let model,endIOLabelId =  addIOLabelComp endIOPos label model
    let endIOLabelRightPortId = OutputPortId (model.Wire.Symbol.Symbols[endIOLabelId].PortMaps.Order[Right][0])

    // remove original wire, then add new wirings
    model
    |> deleteSingleWire targetWire.WId
    |> placeSingleWire (OutputPortId startPort) startIOLabelLeftPortId
    |> placeSingleWire endIOLabelRightPortId (InputPortId endPort)
    

/// Automatically identify long wires and transform them into wire labels.
let autoReplaceLongWiresWithWireLabels (model:SheetT.Model) = 
    mapValues model.Wire.Wires
    |> List.filter (fun wire -> (getWireLength wire) > Constants.longSingleWireLength)
    // TODO: filter out the ones with available room
    |> List.fold (fun model wire -> wireToWireLabels wire model) model


/// Add or remove wire labels (swapping between long wires and wire labels) to reduce wiring complexity.
let sheetWireLabelSymbol (model:SheetT.Model) = 
    ()