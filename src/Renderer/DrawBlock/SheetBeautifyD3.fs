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
open CanvasStateAnalyser

/// constants used by SheetBeautify
module Constants =
    // user-variable threshold
    // for classifying a single wire as long enough to be replaced by wire labels
    let longSingleWireLength = 200.

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

/// Check whether the given output port is already connected to a wire label, if true, return the label name and its InputPortId
let checkOutputPortConnectedToWireLabel (portId: OutputPortId) (model:SheetT.Model) : Option<string*InputPortId> =
    let tryFindWireLabelWithInputPort (portId:InputPortId) = 
        let portIdStr =
            match portId with
            | InputPortId str -> str
        let port = getPort  model.Wire.Symbol portIdStr
        let sym = getSymbol model.Wire.Symbol portIdStr
        match sym.Component.Type with
        | IOLabel -> Some (sym.Component.Label,portId)
        | _ -> None
        
    let wireOption =
        mapValues model.Wire.Wires
        |> List.filter (fun wire -> wire.OutputPort = portId)   // all wires going out from the given port
        |> List.tryFind (fun wire ->    // try find a wire whose other end connects to a wire label
            match tryFindWireLabelWithInputPort wire.InputPort with
            | Some _ -> true
            | None -> false
            )
    match wireOption with 
    | Some wire -> tryFindWireLabelWithInputPort wire.InputPort
    | None -> None


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


// function below adapted from SimulationView.getPosRotNextToPort (due to compile order restriction)
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
            Degree180
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
let addIOLabelComp (pos:XYPos) (rot:Rotation) (labelName:string) (model:SheetT.Model) = 
    // let labelName = generateWireLabel model label
    let newSymbolModel, compId = addSymbol [] model.Wire.Symbol pos IOLabel labelName
    let newSymbolModelWithRot = rotateBlock [compId] newSymbolModel rot
    let newModel = 
        model
        |> Optic.set symbol_ newSymbolModelWithRot
        |> updateBoundingBoxes
    newModel, newSymbolModelWithRot.Symbols[compId], labelName


/// Remove the given wire from the sheet
let deleteSingleWire (wire:Wire) (model:SheetT.Model) =
// code adapted from BusWireUpdate.update (DeleteWires)
    // deletes wires from model, then runs bus inference
    // Issie model is not affected but will extract connections from wires
    // at some time.
    let newWires =
        model.Wire.Wires
        |> Map.remove wire.WId
    {model with Wire={model.Wire with Wires = newWires ; ErrorWires = [wire.WId]}}

/// Remove the given list of wires from the sheet
let deleteWires (wireList:list<Wire>) (model:SheetT.Model) =
    (model,wireList)
    ||> List.fold (fun m wire -> deleteSingleWire wire m)


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
    |> Optic.map wire_ (updateWireSegmentJumpsAndSeparations [newWire.WId])

/// Place wires for a list of (source port, target port) tuples, return the updated model
let placeWires (portsList: list<string*string>) (model: SheetT.Model) =
    (model,portsList)
    ||> List.fold (fun m (srcPort,tgtPort) -> placeSingleWire (OutputPortId srcPort) (InputPortId tgtPort) m)


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


/// Create and place IOLabel components for a given port, return the updated model and the added wire label's name
let addIOLabelAtPortAndPlaceWire (portIdStr:string) (portPos:XYPos) (isStart:bool) (label:string) (model:SheetT.Model) =
    let ioLabelPos, ioLabelRot = 
        let posRotOption = getPosRotNextToPort model.Wire.Symbol.Ports[portIdStr] model.Wire.Symbol (3.*Constants.minCompDistance)
        match posRotOption with
        | Some (pos,rot) -> pos,rot
        | None -> 
            printf "no good end pos, using default pos" // debug
            if isStart then {X=portPos.X+3.*Constants.minCompDistance; Y=portPos.Y},Degree180
            else {X=portPos.X-Constants.minCompDistance-Constants.widthIOLabel; Y=portPos.Y},Degree180

    let modelPlacedSym,ioLabelSym,ioLabelName =  addIOLabelComp ioLabelPos ioLabelRot label model
    
    let ioLabelInputEdge,ioLabelOutputEdge =
    // note: IOLabel with Degree180 rotation is in the "normal" orientation (input on the left, output on the right)
        match ioLabelRot with
        | Degree0 -> Right,Left
        | Degree90 -> Top,Bottom
        | Degree180 -> Left,Right
        | Degree270 -> Bottom,Top
    let sourcePort, targetPort =
        match isStart with
        | true -> portIdStr, ioLabelSym.PortMaps.Order[ioLabelInputEdge][0]
        | false -> ioLabelSym.PortMaps.Order[ioLabelOutputEdge][0], portIdStr

    let modelPlacedWire =
        modelPlacedSym
        |> moveSymToNonOverlap ioLabelSym   // reduce overlap with other symbols
        |> placeSingleWire (OutputPortId sourcePort) (InputPortId targetPort)
        |> updateWiring ioLabelSym
    
    ioLabelName, modelPlacedWire

/// Get the list of wires belonging to the same net with the given OutputPortId
let getSameNetWires (sourcePortId) (model:SheetT.Model) =
    mapValues model.Wire.Wires
    |> List.filter (fun wire -> wire.OutputPort = sourcePortId)

/// Derive wire label names from names of driving components and ports
let deriveWireLabelFromSrcPort (sourcePortStr:string) (model:SheetT.Model) = 
    let port = getPort model.Wire.Symbol sourcePortStr
    let sym = getSymbol model.Wire.Symbol sourcePortStr
    let symLabel = sym.Component.Label
    let portLabel = getPortName sym.Component port
    match portLabel with
    | "" -> symLabel+"_"+"OUT"   // TODO: add support for "SplitWire" & "SplitN" (multiple possible outputs)
    | _ -> symLabel+"_"+portLabel

/// Replace the net of wires corresponding to the given source port with a set of wire labels, return the updated model
let sameNetWiresToWireLabels (sourcePortId:OutputPortId) (model:SheetT.Model) = 
    let srcPortStr, srcPos = 
        match sourcePortId with
        | OutputPortId str -> str, getPortPosOnSheet str model

    let sameNetWires = 
        getSameNetWires sourcePortId model

    let givenLabel = deriveWireLabelFromSrcPort srcPortStr model
    let newSameNetWires, labelName, modelWithSrcWireLabel = 
        match (checkOutputPortConnectedToWireLabel sourcePortId model) with
        | Some (label,wireLabelPortId) ->   // a source wire label already exists
            let sameNetWires = 
                sameNetWires
                |> List.filter (fun wire -> wire.InputPort<>wireLabelPortId) // remove the wire connecting to the wire label from list
            sameNetWires,label,model
        | None ->   // add the source wire label
            let label,model = 
                (givenLabel,model)
                ||> addIOLabelAtPortAndPlaceWire srcPortStr srcPos true // add the source wire label
            sameNetWires,label,model
    
    (modelWithSrcWireLabel, newSameNetWires)
    ||> List.fold (fun model  wire ->
        let tgtPortStr, tgtPos = 
            match wire.InputPort with
            | InputPortId str -> str, getPortPosOnSheet str model
        model
        |> deleteSingleWire wire
        |> addIOLabelAtPortAndPlaceWire tgtPortStr tgtPos false labelName   // add a target wire label
        |> snd)


// Assume that if users would like to manually apply the following function (e.g. by clicking on an menu item),
// they would tend to include the long wires that they would like to convert into wire labels 
// in their selection.
// Hence, here we apply the conversion standard (i.e., longer than threshold) only to the selection,
// not to the entire nets of the selected wires.
// This function can apply to a set of selected wires, when called from an electron edit menu item,
// or to a single selected wire, when called from a right-click context menu item.

/// Identify long wires from selected wires and transform them into wire labels
let selectedWiresToWireLabels (wireIdList: list<ConnectionId>) (model: SheetT.Model) = 
    wireIdList
    |> List.map (fun wireId -> model.Wire.Wires[wireId])    // all selected wires
    |> List.filter (fun wire -> (getWireLength wire) > Constants.longSingleWireLength)  // filter out the long ones
    |> List.distinctBy (fun wire -> wire.OutputPort)    // get wires with distinct output ports here
    |> List.fold (fun m wire -> sameNetWiresToWireLabels wire.OutputPort m) model
    

/// Automatically identify long wires and transform them into wire labels (applies to the whole sheet)
let autoWiresToWireLabels (model:SheetT.Model) = 
    let allWireIds = mapKeys model.Wire.Wires    // get the list of ConnectionId of all wires on sheet
    selectedWiresToWireLabels allWireIds model
    

//-----------------------------------------------------------------------------------------------------------------------------------------------


/// Try to find the wire connected to the given port and return them both, return None if not found
let tryFindWireWithPort (portId:string) (model:SheetT.Model) = 
    let wireOption =
        mapValues model.Wire.Wires
        |> List.tryFind (fun wire -> wire.OutputPort = OutputPortId portId || wire.InputPort = InputPortId portId)
    match wireOption with
    | Some wire -> Some (wire,portId)
    | None -> None


/// Return the wire connected to the wire label and the port on the symbol connected to the wire,
/// assume sym is of type IOLabel and it is connected to one wire only
let getWireAndPort (sym:Symbol) (model:SheetT.Model) = 
    let wire,portOnWireLabel = 
        mapValues model.Wire.Symbol.Ports
        |> List.filter (fun port -> port.HostId = sym.Component.Id)     // get ports on the wire label
        |> List.choose (fun port -> tryFindWireWithPort port.Id model)  // get the port that has wire connected, and the wire itself
        |> List.last    // the list should only contain one element
    sym,wire,portOnWireLabel


/// Return the absolute Manhattan distance between two ports on the sheet
/// (i.e.) the potential wire length if placed between them
let getAbsDistanceBetweenPorts (port1:string) (port2:string) (model:SheetT.Model) = 
    let pos1 = getPortPosOnSheet port1 model
    let pos2 = getPortPosOnSheet port2 model
    let distance = pos1-pos2
    abs distance.X + abs distance.Y


/// Return a list of wire labels belonging to the same net (have the same label name)
let getSameNetWireLabelsFromName (labelName: string) (model:SheetT.Model) = 
    mapValues model.Wire.Symbol.Symbols
    |> List.filter (fun sym -> sym.Component.Type = IOLabel && sym.Component.Label = labelName)
        

/// For a given wire label name (string),
/// get the list of wire labels belonging to the same net;
/// check whether ALL of them are below the threshold of converting into wires;
/// if ture, perform the conversion to the entire net and return the updated model, otherwise keep the model unchanged.
let sameNetWireLabelsToWire (wireLabelName: string) (model:SheetT.Model) = 
    // 1. get all same-net wire labels
    let wireLabelList = getSameNetWireLabelsFromName wireLabelName model
    printf $"wire label name: {wireLabelName}"

    // 2. check condition
    let wireLabelsMap =
        wireLabelList
        |> List.map (fun sym -> getWireAndPort sym model)
        |> List.groupBy (fun (sym,wire,portId) ->
                wire.InputPort = InputPortId portId     // the source wire label
            )
        |> Map.ofList
    let sourceWireLabel, sourceWire, _ = wireLabelsMap[true][0]        // should only have one
    let targetWireLabelInfoList = wireLabelsMap[false]

    let targetWireLabels,targetWires, _ = List.unzip3 targetWireLabelInfoList

    // stuff to (potentially) remove
    let wireLabelsInNet = sourceWireLabel::targetWireLabels
    let wiresInNet = sourceWire::targetWires

    let sourceSymPort = 
        match sourceWire.OutputPort with
        | OutputPortId portIdStr -> portIdStr

    let targetSymPortList = 
        targetWires
        |> List.map (fun targetWire ->
            match targetWire.InputPort with
            | InputPortId portIdStr -> portIdStr)

    let potentialWiresPortPairs = List.allPairs [sourceSymPort] targetSymPortList
    let cond =
        potentialWiresPortPairs     // ports of potential wires
        |> List.forall (fun (srcPort,tgtPort) ->
            (getAbsDistanceBetweenPorts srcPort tgtPort model) < Constants.longSingleWireLength)
    

    // 3. if meet condition, perform wire-labels-to-wires replacement
    let deleteWireLabels (wireLabelList:list<Symbol>) (model:SheetT.Model) = 
        let newSymbolModel =
            wireLabelList
            |> List.map (fun sym -> sym.Id)
            |> deleteSymbols model.Wire.Symbol
        Optic.set symbol_ newSymbolModel model

    match cond with
    | false -> model
    | true -> 
        let newModel = 
            model
            // delete old wires (deleteSingleWire)
            |> deleteWires wiresInNet
            // delete old wire labels
            |> deleteWireLabels wireLabelsInNet
            // add new wires (placeSingleWire)
            |> placeWires potentialWiresPortPairs
        newModel


// This function can apply to a set of selected wire labels, when called from an electron edit menu item,
// or to a single selected wire label, when called from a right-click context menu item.

/// Identify selected wire labels' nets that score below threshold and transform them into wires,
/// all wire labels in the same net as those being selected are taken into consideration
/// (i.e., transformation takes place on a per-net level)
let selectedWireLabelsToWires (compIdList: list<ComponentId>) (model: SheetT.Model) = 
    compIdList
    |> List.map (fun compId -> model.Wire.Symbol.Symbols[compId])   // get the symbols in the list
    |> List.filter (fun sym -> sym.Component.Type = IOLabel)    // get the wire labels
    |> List.distinctBy (fun sym -> sym.Component.Label)     // get symbols with distinct wire label names
    |> List.fold (fun m sym -> sameNetWireLabelsToWire sym.Component.Label m) model 


/// Automatically identify same-net wire labels that score below threshold and transform them into wires
let autoWireLabelsToWires (model:SheetT.Model) = 
    let allCompIds = mapKeys model.Wire.Symbol.Symbols    // get the list of ComponentId of all symbols on sheet
    selectedWireLabelsToWires allCompIds model
    

/// Add or remove wire labels (swapping between long wires and wire labels) to reduce wiring complexity
let sheetWireLabelSymbol (model:SheetT.Model) = 
    ()