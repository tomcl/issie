module SheetBeautifyD3

open Optics
open Optics.Operators
open DrawHelpers
open Helpers
open CommonTypes
open ModelType
open DrawModelType
open SymbolUpdate
open BlockHelpers
open Symbol
open SheetBeautifyHelpers
open EEExtensions
open Sheet.SheetInterface
open BusWireT

/// Optic to access BusWireT.Model from SheetT.Model
let busWireModel_ = SheetT.wire_

/// Optic to access SymbolT.Model from SheetT.Model
let symbolModel_ = SheetT.symbol_

let getWireListFromSheet (sheet: SheetT.Model) = sheet.Wire.Wires |> Map.toList
// |> List.map snd

/// Return a list of wires that are long which can potentially be replaced with wire labels
let getLongWires (wireLengthlimit: float) (wireList: List<ConnectionId * BusWireT.Wire>) =
    wireList
    |> List.filter (fun (id, wire) -> (getWireLength wire > wireLengthlimit))

let findWireLabelRoomAtSource (wire: BusWireT.Wire ) (distance: float)= 
    let asegList =  wire |> getAbsSegments 
    let firstSeg = asegList |> List.head
    match firstSeg.Orientation with
    | BusWireT.Horizontal -> 
        if firstSeg.End.X - firstSeg.Start.X < 0 
        then {firstSeg.End with X = firstSeg.End.X - distance; Y = firstSeg.End.Y }
        else { firstSeg.End with X = firstSeg.End.X + distance; Y = firstSeg.End.Y }
    | BusWireT.Vertical -> 
        if firstSeg.End.Y - firstSeg.Start.Y < 0 
        then {firstSeg.End with X = firstSeg.End.X; Y = firstSeg.End.Y - distance}
        else { firstSeg.End with X = firstSeg.End.X; Y = firstSeg.End.Y + distance}

let findWireLabelRoomAtTarget (wire: BusWireT.Wire ) (distance: float) = 
    let asegList =  wire |> getAbsSegments 
    let lastSeg = asegList |> List.last
    match lastSeg.Orientation with
    | BusWireT.Horizontal -> 
        if lastSeg.End.X - lastSeg.Start.X < 0 
        then {lastSeg.Start with X = lastSeg.Start.X + distance; Y = lastSeg.Start.Y }
        else { lastSeg.Start with X = lastSeg.Start.X - distance; Y = lastSeg.Start.Y }
    | BusWireT.Vertical -> 
        if lastSeg.End.Y - lastSeg.Start.Y < 0 
        then {lastSeg.Start with X = lastSeg.Start.X; Y = lastSeg.Start.Y + distance}
        else { lastSeg.Start with X = lastSeg.Start.X; Y = lastSeg.Start.Y + distance}

let getMovedSymbolBB (move: XYPos) (sym: SymbolT.Symbol) : BoundingBox =
    {sym.LabelBoundingBox with
        TopLeft =  sym.LabelBoundingBox.TopLeft + move}
    

let adjustWireLabelPos (wireLabelSym: SymbolT.Symbol) (sheet: SheetT.Model) = 
    let originalPos = wireLabelSym.Pos
    let adjustmentAmount = 70.0
    let boxes =
        sheet.BoundingBoxes
        |> Map.toList
        |> List.filter (fun (compId, box) -> compId <> wireLabelSym.Id)
        |> List.map snd
    let checkIfIntersect (symbolBB : BoundingBox)= 
        boxes
        |> List.exists (fun box -> BlockHelpers.overlap2DBox box symbolBB)

    let tryMoveWireLabel moveAmount =
        let newWireLabelSymBB = getMovedSymbolBB moveAmount wireLabelSym
        not (checkIfIntersect newWireLabelSymBB)

    let tryAdjust =
        if tryMoveWireLabel {X = 0.; Y = adjustmentAmount} then
            Some {X=0.; Y=adjustmentAmount}
        elif tryMoveWireLabel {X = 0.; Y = -adjustmentAmount} then
            Some {X=0.; Y = -adjustmentAmount}
        // if tryMoveWireLabel {X = adjustmentAmount; Y = 0.} then
        //     Some {X=adjustmentAmount; Y = 0.}
        // elif tryMoveWireLabel {X = -adjustmentAmount; Y = 0.} then
        //     Some {X=adjustmentAmount; Y = 0.}
        else
            None
    let newPos (moveAmount: XYPos) =
        {originalPos with X = originalPos.X + moveAmount.X; Y = originalPos.Y + moveAmount.Y}

    if checkIfIntersect wireLabelSym.SymbolBoundingBox
    then 
        match tryAdjust with
        | Some moveAmount -> Some (newPos moveAmount)
        | None -> None
    else 
        Some originalPos


/// Return a map of wires grouped by net (multiple wires with same source port).
/// And a list of single wires that are too long.
/// Both need wire labels generated.
let getWiresNeedLabels (sheet: SheetT.Model) (wireLengthlimit: float) =
    let flattenList = List.collect id >> List.distinct
    let wireInNet, singleWires =
        getWireListFromSheet sheet
        |> List.groupBy (fun (id, wire) -> wire.InputPort)
        |> List.map snd
        |> List.partition (fun wireList -> wireList.Length > 1)
    let longWires =
        singleWires
        |> flattenList
        |> getLongWires wireLengthlimit
    
    wireInNet 
    |> flattenList
    |> List.append longWires

let deleteWire (wire: ConnectionId) (sheet: SheetT.Model) =
    let newWires =
        sheet.Wire.Wires
        |> Map.filter (fun id _ -> not (id = wire))
    { sheet with Wire = { sheet.Wire with Wires = newWires } }

/// generate wire label for a wire connected between source symbol and target symbol
/// or keep the wires if not enough room for label
let generateWireLabel (sheet: SheetT.Model) (wireWithCID: ConnectionId * BusWireT.Wire) =
    let connectionID, wire = wireWithCID
    let startSym = getSourceSymbol sheet.Wire wire
    let sourceSymPort = getSourcePort sheet.Wire wire
    let inputPortNumber = sourceSymPort.PortNumber
    let wireLabelName =
        startSym.Component.Label
        + "OUT"
        + string inputPortNumber

    let inputPort, outputPort = wire.InputPort, wire.OutputPort

    let wireLabelSourcePos = findWireLabelRoomAtSource wire 40.0
    let wireLabelTargetPos = findWireLabelRoomAtTarget wire 40.0

    let addWireLabelAndConnectWire
        (pos)
        (labelName)
        (compType)
        (isAtSource: bool)
        (fromLabelToPortID: InputPortId)
        (toLabelFromPortID: OutputPortId)
        (sheet: SheetT.Model)
        =
        let labelModel, labelID =
            SymbolUpdate.addSymbol [] (sheet.Wire.Symbol) pos compType labelName
        let labelSym = labelModel.Symbols[labelID]
        let inputPortIDstr, outputPortIdstr =
            if isAtSource then
                (InputPortId labelModel.Symbols[labelID].Component.InputPorts.[0].Id), toLabelFromPortID
            else
                fromLabelToPortID, (OutputPortId labelModel.Symbols[labelID].Component.OutputPorts.[0].Id)
        let sheetWithWireLabelAdded = 
            sheet
            |> Optic.set symbolModel_ labelModel
            |> SheetUpdateHelpers.updateBoundingBoxes

        let adjustLabelOnSheet = 
            match adjustWireLabelPos labelSym sheetWithWireLabelAdded with
            | Some newPos -> 
                printf "newpos %.2f, %.2f " newPos.X newPos.Y
                printf "oldpos %.2f, %.2f" pos.X pos.Y
                updateSymPosInSheet labelID newPos sheetWithWireLabelAdded
            | None -> 
                printf "no room"
                sheetWithWireLabelAdded

        let newWireModel, _ =
            BusWireUpdate.newWire (inputPortIDstr) (outputPortIdstr) adjustLabelOnSheet.Wire

        adjustLabelOnSheet
        |> Optic.set busWireModel_ newWireModel

    let sheetCheckedForNetWires = 
        if sheet.Wire.Symbol.Symbols
            |> Map.exists (fun _ sym -> caseInvariantEqual sym.Component.Label wireLabelName) // if wire in a net then this would be true
        then sheet // Don't want to duplicate wirel Label at net inputPort
        else sheet
            |> addWireLabelAndConnectWire wireLabelSourcePos wireLabelName IOLabel true inputPort outputPort
    sheetCheckedForNetWires
    |> addWireLabelAndConnectWire wireLabelTargetPos wireLabelName IOLabel false inputPort outputPort
    |> deleteWire connectionID

// let generateWireLabel (sheet: SheetT.Model) (wireWithCID: ConnectionId * BusWireT.Wire) =
//     let connectionID, wire = wireWithCID
//     let startSym = getSourceSymbol sheet.Wire wire
//     let sourceSymPort = getSourcePort sheet.Wire wire
//     let inputPortNumber = sourceSymPort.PortNumber
//     let wireLabelName =
//         startSym.Component.Label
//         + "OUT"
//         + string inputPortNumber

//     let inputPort, outputPort = wire.InputPort, wire.OutputPort

//     let wireLabelSourcePos = findWireLabelRoomAtSource wire 30.0
//     let wireLabelTargetPos = findWireLabelRoomAtTarget wire 30.0

//     let addWireLabelAndConnectWire
//         (pos)
//         (labelName)
//         (compType)
//         (isAtSource: bool)
//         (fromLabelToPortID: InputPortId)
//         (toLabelFromPortID: OutputPortId)
//         (sheet: SheetT.Model)
//         =
//         let labelModel, labelID =
//             SymbolUpdate.addSymbol [] (sheet.Wire.Symbol) pos compType labelName
//         let inputPortIDstr, outputPortIdstr =
//             if isAtSource then
//                 (InputPortId labelModel.Symbols[labelID].Component.InputPorts.[0].Id), toLabelFromPortID
//             else
//                 fromLabelToPortID, (OutputPortId labelModel.Symbols[labelID].Component.OutputPorts.[0].Id)
//         let newWireModel, _ =
//             BusWireUpdate.newWire (inputPortIDstr) (outputPortIdstr) { sheet.Wire with Symbol = labelModel }
//         sheet
//         |> Optic.set symbolModel_ labelModel
//         |> SheetUpdateHelpers.updateBoundingBoxes
//         |> Optic.set busWireModel_ newWireModel

//     let sheetCheckedForNetWires = 
//         if sheet.Wire.Symbol.Symbols
//             |> Map.exists (fun _ sym -> caseInvariantEqual sym.Component.Label wireLabelName) // if wire in a net then this would be true
//         then sheet // Don't want to duplicate wirel Label at net inputPort
//         else sheet
//             |> addWireLabelAndConnectWire wireLabelSourcePos wireLabelName IOLabel true inputPort outputPort
//     sheetCheckedForNetWires
//     |> addWireLabelAndConnectWire wireLabelTargetPos wireLabelName IOLabel false inputPort outputPort
//     |> deleteWire connectionID

let autoGenerateWireLabels (sheet: SheetT.Model) =
    let limit = 50. // need clarify how long a wire is considered too long
    let wiresNeedLabels = getWiresNeedLabels sheet limit
    (sheet, wiresNeedLabels)
    ||> List.fold (fun sheet wire -> generateWireLabel sheet wire)
    |> Ok
