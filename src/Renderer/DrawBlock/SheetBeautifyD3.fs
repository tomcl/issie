module SheetBeautifyD3

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
open SymbolResizeHelpers
open SheetBeautifyHelpers
open Optics
open Optics.Operators
open ModelType
open BlockHelpers
open BusWidthInferer
open BusWireSeparate
open RotateScale

/// constants used by SheetBeautify
module Constants =
    // user-variable threshold
    // for classifying a single wire as long enough to be replaced by wire labels
    let longSingleWireLength = 100.

    // minimum distance between two adjacent symbols
    let minCompDistance = 14.
    let numInputsIOLabel, numOutputsIOLabel, heightIOLabel, widthIOLabel = getComponentProperties IOLabel "I"


/// Optic to access SheetT.Model from Issie Model
let sheetModel_ = sheet_

/// Optic to access BusWireT.Model from SheetT.Model
let busWireModel_ = SheetT.wire_

/// Optic to access SymbolT.Model from SheetT.Model
let symbolModel_ = SheetT.symbol_


// ---------------------------------- D3 ---------------------------------------------------------

/// Return an automatically created WireLabel name (string) with a default basename
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
    match newCompNo with
    | -1 -> match existingNumbers with
            | [] -> defaultName+"0"
            | lst ->
                let maxNo = List.max lst
                newCompBaseName + (string (maxNo+1))
    | _ -> defaultName


// function below copied from SimulationView.getPosRotNextToPort (due to compile order restriction)
/// get the position and rotation for inserting a new component next to the given port
/// at a given distance
/// the rotation is such that the original left side of the component (input side)
/// faces the given port
/// returns None if another symbol is in the way
let getPosRotNextToPort (port: Port) (model: SymbolT.Model) (dist: float) =
    let isPosInBoundingBox  (pos: XYPos) (boundingBox: BoundingBox) =
        (pos.X > boundingBox.TopLeft.X && pos.X < boundingBox.TopLeft.X + boundingBox.W &&
        pos.Y > boundingBox.TopLeft.Y && pos.Y < boundingBox.TopLeft.Y + boundingBox.H)
    
    let sym =
        model.Symbols
        |> Map.toList
        |> List.tryFind (fun (_, sym) -> sym.Component.Id = port.HostId)
        |> function
            | Some (_, sym) -> sym
            | None -> failwithf "The given component should be in the list of symbols"

    let edge = sym.PortMaps.Orientation[port.Id]
    let portPos = Symbol.getPortPos sym port
    let pos, rot =
        match edge with
        | Right ->
            {X = sym.Pos.X + portPos.X + dist; Y = sym.Pos.Y + portPos.Y},
            Degree0
        | Top ->
            {X = sym.Pos.X + portPos.X; Y = sym.Pos.Y + portPos.Y - dist},
            Degree90
        | Left ->
            {X = sym.Pos.X + portPos.X - dist; Y = sym.Pos.Y + portPos.Y},
            Degree180
        | Bottom ->
            {X = sym.Pos.X + portPos.X; Y = sym.Pos.Y + portPos.Y + dist},
            Degree270

    model.Symbols
    |> Map.toList
    |> List.map (fun (_, sym) -> Symbol.getSymbolBoundingBox sym)
    |> List.exists (isPosInBoundingBox pos)
    |> function
        | true -> None
        | false -> Some (pos, rot)


/// Add a IOLabel (wire label) component to the sheet, return the updated model, the added symbol, and its label name
let addIOLabelComp (pos:XYPos) (rot:Rotation) (defaultLabel:string) (model:SheetT.Model) = 
    let labelName = generateWireLabel model defaultLabel
    let newSymbolModel, compId = addSymbol [] model.Wire.Symbol pos IOLabel labelName
    let newSymbolModelWithRot = rotateBlock [compId] newSymbolModel rot
    let newModel = 
        model
        |> Optic.set symbol_ newSymbolModelWithRot
        |> updateBoundingBoxes
    newModel, newSymbolModelWithRot.Symbols[compId], labelName


/// Remove the given wire from the sheet
let deleteSingleWire (wId:ConnectionId) (model:SheetT.Model) =
// code adapted from BusWireUpdate.update (DeleteWires)
    // deletes wires from model, then runs bus inference
    // Issie model is not affected but will extract connections from wires
    // at some time.
    let newWires =
        model.Wire.Wires
        |> Map.remove wId
    {model with Wire={ model.Wire with Wires = newWires ; ErrorWires = [wId]}}


/// Place a wire from a source port to a target port, return the updated model
let placeSingleWire (sourcePortId: OutputPortId) (targetPortId: InputPortId) (model: SheetT.Model) =
// code adapted from TestDrawBlock.HLPTick3.Builder.placeWire
    let newWire = BusWireUpdate.makeNewWire targetPortId sourcePortId model.Wire
    // if model.Wire.Wires |> Map.exists (fun wid wire -> wire.InputPort=newWire.InputPort && wire.OutputPort = newWire.OutputPort) 
    // then  // wire already exists
    //     Error "Can't create wire from {source} to {target} because a wire already exists between those ports"
    // else
    model
    |> Optic.set (busWireModel_ >-> BusWireT.wireOf_ newWire.WId) newWire


/// (Helper function for moveSymToNonOverlap)
/// Move the target symbol away (out of the bounding box) from the symbol it's overlapping with (the symbol to avoid)
let adjustPosFromOverlap (targetCompId:ComponentId) (targetSymBoundingBox:BoundingBox) (avoidSymBoundingBox:BoundingBox) (model:SheetT.Model) = 
    match overlap2DBox targetSymBoundingBox avoidSymBoundingBox with  // check if they indeed intersect first
    | false -> model
    | true ->
        let moveOffset = 
            match avoidSymBoundingBox.Centre(),targetSymBoundingBox.Centre() with
            | cA,cT when cA.X<cT.X ->  // avoidSym is on the left of targetSym
                if (cA.Y<cT.Y) then  // avoidSym is above targetSym
                    avoidSymBoundingBox.BottomRight() - targetSymBoundingBox.TopLeft
                else  // avoidSym is below targetSym
                    {X = avoidSymBoundingBox.RightEdge(); Y = avoidSymBoundingBox.TopLeft.Y} - targetSymBoundingBox.TopLeft
            | cA,cT when cA.X>=cT.X -> // avoidSym is on the right of targetSym
                if (cA.Y<cT.Y) then  // avoidSym is above targetSym
                    avoidSymBoundingBox.TopLeft - targetSymBoundingBox.BottomRight()
                else  // avoidSym is below targetSym
                    {X = avoidSymBoundingBox.TopLeft.X; Y = avoidSymBoundingBox.BottomEdge()} - targetSymBoundingBox.BottomRight()
            | _ -> {X=0.;Y=0.}  // should not happen, just to make compiler happy
        let adjustedDistanceToMove = moveOffset + {X = float(sign(moveOffset.X))*Constants.minCompDistance; Y = float(sign(moveOffset.Y))*Constants.minCompDistance}
        let newSymbolModel = SymbolUpdate.moveSymbols model.Wire.Symbol [targetCompId] adjustedDistanceToMove
        Optic.set symbol_ newSymbolModel model


/// Try moving the given symbol around if it overlaps with other existing symbols, no change if no overlapping
let moveSymToNonOverlap (sym: Symbol) (model:SheetT.Model) = 
    let symBoundingBox = getBoundingBox model.Wire.Symbol sym.Id
    let overlappingBoundingBoxes = 
        model.BoundingBoxes
        |> Map.filter (fun sId boundingBox -> overlap2DBox boundingBox (getSymBoundingBox sym) && sym.Id <> sId)
        |> mapValues
    match List.length overlappingBoundingBoxes with
    | 0 -> model 
    | _ -> 
        // printf $"adjusting postion of {sym.Component.Label} at {sym.Pos}"   // debug
        (model,overlappingBoundingBoxes)
        ||> List.fold (fun m bbox -> adjustPosFromOverlap sym.Id symBoundingBox bbox m) 
    // TODO:
    // because the adjusted position can still be overlapping with something else,
    // maybe consider applying this function multiple times to reach an effect of recursion.
    // or, change this function to a recursive one: stop until no overlapping exists.

let updateWiring (sym:Symbol) (model:SheetT.Model) = 
    Optic.set wire_ (routeAndSeparateSymbolWires model.Wire sym.Id) model

/// Replace a given wire with wire labels, return the updated model
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
    
    // create and place IOLabel components for each port
    let startIOPos, startRot = 
        let posRotOption = getPosRotNextToPort model.Wire.Symbol.Ports[startPort] model.Wire.Symbol (3.*Constants.minCompDistance)
        match posRotOption with
        | Some (pos,rot) -> pos,rot
        | None -> 
            printf "no good start pos, using default pos"    // debug
            {X=startPos.X+3.*Constants.minCompDistance; Y=startPos.Y},Degree0
    let model,startIOLabelSym, newLabel =  addIOLabelComp startIOPos startRot label model
    let startIOLabelLeftPortId = InputPortId (startIOLabelSym.PortMaps.Order[Left][0])

    let endIOPos, endRot = 
        let posRotOption = getPosRotNextToPort model.Wire.Symbol.Ports[endPort] model.Wire.Symbol (3.*Constants.minCompDistance)
        match posRotOption with
        | Some (pos,rot) -> pos,rot
        | None -> 
            printf "no good end pos, using default pos" // debug
            {X=endPos.X-Constants.minCompDistance-Constants.widthIOLabel; Y=endPos.Y},Degree0
    let model,endIOLabelSym,_ =  addIOLabelComp endIOPos endRot newLabel model
    let endIOLabelRightPortId = OutputPortId (endIOLabelSym.PortMaps.Order[Right][0])

    // remove original wire, then add new wirings
    model
    |> deleteSingleWire targetWire.WId
    |> moveSymToNonOverlap startIOLabelSym  // reduce overlap with other symbols
    |> moveSymToNonOverlap endIOLabelSym
    |> moveSymToNonOverlap startIOLabelSym  // applying twice to mirror the effect of recursion
    |> moveSymToNonOverlap endIOLabelSym
    // TODO: reduce overlap with wire
    |> placeSingleWire (OutputPortId startPort) startIOLabelLeftPortId
    |> placeSingleWire endIOLabelRightPortId (InputPortId endPort)
    |> updateWiring startIOLabelSym
    |> updateWiring endIOLabelSym
    

/// Automatically identify long wires and transform them into wire labels
let autoWiresToWireLabels (model:SheetT.Model) = 
    mapValues model.Wire.Wires
    |> List.filter (fun wire -> (getWireLength wire) > Constants.longSingleWireLength)
    |> List.fold (fun m wire -> wireToWireLabels wire m) model
    

/// Add or remove wire labels (swapping between long wires and wire labels) to reduce wiring complexity
let sheetWireLabelSymbol (model:SheetT.Model) = 
    ()