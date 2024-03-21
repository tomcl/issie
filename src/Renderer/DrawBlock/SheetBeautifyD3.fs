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
let getLongWires (wireLengthlimit: float) (wireList: List<BusWireT.Wire>) =
    wireList
    |> List.filter (fun wire -> (getWireLength wire > wireLengthlimit))

let findWireLabelRoomAtSource (wire: BusWireT.Wire ) (distance: float) = 
    let asegList =  wire |> getAbsSegments 
    let firstSeg = asegList |> List.head
    match firstSeg.Orientation with
    | BusWireT.Horizontal -> 
        if firstSeg.End.X - firstSeg.Start.X < 0 
        then 
            let pos = {firstSeg.End with X = firstSeg.End.X - distance; Y = firstSeg.End.Y }
            pos, Degree180
        else 
            let pos = { firstSeg.End with X = firstSeg.End.X + distance; Y = firstSeg.End.Y }
            pos, Degree0
    | BusWireT.Vertical -> 
        if firstSeg.End.Y - firstSeg.Start.Y < 0 
        then 
            let pos = {firstSeg.End with X = firstSeg.End.X; Y = firstSeg.End.Y - distance}
            pos, Degree90
        else 
            let pos = { firstSeg.End with X = firstSeg.End.X; Y = firstSeg.End.Y + distance}
            pos, Degree270

// let findWireLabelRoomAtTarget (wire: BusWireT.Wire) (symModel: SymbolT.Model) (sheet) (distance: float) = 
//     let inputPort = wire.InputPort
//     let portPos = getInputPortLocation None symModel inputPort
//     let asegList =  sheet |> SegmentHelpers.visibleSegsWithVertices wire
//     let lastSeg = asegList |> List.last
//     let segStart, segEnd = lastSeg
//     match SegmentHelpers.visSegOrientation lastSeg with
//     | BusWireT.Horizontal -> 
//         if segEnd.X - segStart.X < 0 
//         then 
//             let pos = {segEnd with X = segEnd.X + distance; Y = segEnd.Y }
//             pos, Degree180
//         else 
//             let pos = { segEnd with X = segEnd.X - distance; Y = segEnd.Y }
//             pos, Degree0
//     | BusWireT.Vertical -> 
//         if segEnd.Y - segStart.Y < 0 
//         then 
//             let pos = {segEnd with X = segEnd.X; Y = segEnd.Y + distance}
//             printf "portpos %.2f, %.2f " portPos.X portPos.Y
//             pos, Degree90
//         else 
//             printf "portpos %.2f, %.2f " portPos.X portPos.Y
//             let pos = { portPos with X = segEnd.X; Y = segEnd.Y - distance}
//             pos, Degree270
let findWireLabelRoomAtTarget (wire: BusWireT.Wire) (symModel: SymbolT.Model) (distance: float) = 
    let inputPort = wire.InputPort
    let portPos = getInputPortLocation None symModel inputPort
    let asegList =  wire |> getAbsSegments 
    let lastSeg = asegList |> List.last
    match lastSeg.Orientation with
    | BusWireT.Horizontal -> 
        if lastSeg.End.X - lastSeg.Start.X < 0 
        then 
            let pos = {lastSeg.End with X = lastSeg.End.X + distance; Y = lastSeg.End.Y }
            pos, Degree180
        else 
            let pos = { lastSeg.End with X = lastSeg.End.X - distance; Y = lastSeg.End.Y }
            pos, Degree0
    | BusWireT.Vertical -> 
        if lastSeg.End.Y - lastSeg.Start.Y < 0 
        then 
            let pos = {lastSeg.Start with X = lastSeg.Start.X; Y = lastSeg.Start.Y + distance}
            printf "portpos %.2f, %.2f " lastSeg.Start.X (lastSeg.Start.Y - distance)
            pos, Degree90
        else 
            printf "portpos %.2f, %.2f " lastSeg.Start.X (lastSeg.Start.Y - distance)
            let pos = { lastSeg.Start with X = lastSeg.Start.X; Y = lastSeg.Start.Y - distance}
            pos, Degree270

let getMovedSymbolBB (move: XYPos) (sym: SymbolT.Symbol) : BoundingBox =
    {sym.LabelBoundingBox with
        TopLeft =  sym.LabelBoundingBox.TopLeft + move}
    

let adjustWireLabelPos (wireLabelSym: SymbolT.Symbol) (sheet: SheetT.Model) = 
    let originalPos = wireLabelSym.Pos
    let adjustmentAmount = 40.0
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

    let rec tryAdjust adjustmentAmount attemptCounter =
        if attemptCounter >= 10 then
            None  // Terminate recursion after 10 tries
        elif tryMoveWireLabel {X = 0.; Y = adjustmentAmount} then
            Some {X = 0.; Y = adjustmentAmount}
        elif tryMoveWireLabel {X = 0.; Y = -adjustmentAmount} then
            Some {X = 0.; Y = -adjustmentAmount}
        else
            tryAdjust (adjustmentAmount + 40.) (attemptCounter + 1)

    let newPos (moveAmount: XYPos) =
        {originalPos with X = originalPos.X + moveAmount.X; Y = originalPos.Y + moveAmount.Y}

    if checkIfIntersect wireLabelSym.SymbolBoundingBox
    then 
        match tryAdjust adjustmentAmount 0 with
        | Some moveAmount -> 
            //Some (newPos moveAmount)
            Some originalPos
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
        |> List.map snd
        |> getLongWires wireLengthlimit
    
    wireInNet 
    |> flattenList
    |> List.map snd
    |> List.append longWires

let deleteWire (wireCID: ConnectionId) (sheet: SheetT.Model) =
    let newWires =
        sheet.Wire.Wires
        |> Map.filter (fun id wire -> not (wire.WId = wireCID))
    { sheet with Wire = { sheet.Wire with Wires = newWires } }

/// generate wire label for a wire connected between source symbol and target symbol
/// or keep the wires if not enough room for label
let generateWireLabel (wire: BusWireT.Wire) (sheet: SheetT.Model) =
    let connectionID = wire.WId
    let startSym = getSourceSymbol sheet.Wire wire
    let endSym = getTargetSymbol sheet.Wire wire
    let symbolModel = sheet.Wire.Symbol

    let sourceSymPort = getSourcePort sheet.Wire wire
    let inputPortNumber = sourceSymPort.PortNumber
    let wireLabelName =
        startSym.Component.Label
        + "OUT"
        + string inputPortNumber

    let inputPort, outputPort = wire.InputPort, wire.OutputPort

    let wireLabelSourcePos, rotationSource = findWireLabelRoomAtSource wire 40.0
    let wireLabelTargetPos, rotationTarget = findWireLabelRoomAtTarget wire symbolModel 40.0

    let addWireLabelAndConnectWire
        (pos)
        (rotation)
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
        let rotatedLabelModel = updateSymbol (SymbolResizeHelpers.rotateSymbol rotation) labelID labelModel 
        let inputPortIDstr, outputPortIdstr =
            if isAtSource then
                (InputPortId rotatedLabelModel.Symbols[labelID].Component.InputPorts.[0].Id), toLabelFromPortID
            else
                fromLabelToPortID, (OutputPortId rotatedLabelModel.Symbols[labelID].Component.OutputPorts.[0].Id)
        let sheetWithWireLabelAdded = 
            sheet
            |> Optic.set symbolModel_ rotatedLabelModel
            |> SheetUpdateHelpers.updateBoundingBoxes
            //|> updateSymPosInSheet labelID pos

        let adjustLabelOnSheet = 
            match adjustWireLabelPos labelSym sheetWithWireLabelAdded with
            | Some newPos -> 
                // printf "newpos %.2f, %.2f " newPos.X newPos.Y
                printf "labelpos %s, %.2f, %.2f" labelName labelSym.Component.X labelSym.Component.Y
                let adjustedSheet = updateSymPosInSheet labelID newPos sheetWithWireLabelAdded
                let newWire = 
                    BusWireUpdate.makeNewWire (inputPortIDstr) (outputPortIdstr) adjustedSheet.Wire
                adjustedSheet
                |> Optic.set (busWireModel_ >-> wireOf_ newWire.WId) newWire
            | None -> 
                // printf "no room"
                sheet
        adjustLabelOnSheet
    
    let sheetCheckedForNetWires = 
        if sheet.Wire.Symbol.Symbols
            |> Map.exists (fun _ sym -> caseInvariantEqual sym.Component.Label wireLabelName) // if wire in a net then this would be true
        then sheet // Don't want to duplicate wirel Label at net inputPort
        else 
            sheet
            |> addWireLabelAndConnectWire wireLabelSourcePos rotationSource wireLabelName IOLabel true inputPort outputPort
    sheetCheckedForNetWires
    |> addWireLabelAndConnectWire wireLabelTargetPos rotationTarget wireLabelName IOLabel false inputPort outputPort 
    |> deleteWire connectionID

let autoGenerateWireLabels (sheet: SheetT.Model) =
    let wireLengthlimit = 120. // User can decide what is considered long wire
    let wiresNeedLabels = getWiresNeedLabels sheet wireLengthlimit
    (sheet, wiresNeedLabels)
    ||> List.fold (fun sheet wire -> generateWireLabel wire sheet)

/// Find the wire connected to input port of a wire label.
/// Returns an option of (connectionID of the wire, output port of the wire)
let findWireByLabelInputPort (wires: Map<ConnectionId,Wire>) (label: SymbolT.Symbol) = 
    let labelInputPortID = InputPortId label.Component.InputPorts[0].Id
    let wireList = 
        wires 
        |> Map.filter (fun id wire -> wire.InputPort = labelInputPortID)
        |> Map.toList

    if List.isEmpty wireList then None
    else 
        let ouputPortId = 
            wireList
            |> (List.head >> snd)
            |> (fun wire -> wire.OutputPort)
        let connectionID = 
            wireList
            |> List.head
            |> fst
        Some (connectionID, ouputPortId)

/// Find the wire connected to output port of a wire label.
/// Returns an option of (connectionID of the wire, input port of the wire)
let findWireByLabelOutputPort  (wires: Map<ConnectionId,Wire>) (label: SymbolT.Symbol) = 
    let labelOutputPortID = OutputPortId label.Component.OutputPorts[0].Id
    let wireList = 
        wires 
        |> Map.filter (fun id wire -> wire.OutputPort = labelOutputPortID)
        |> Map.toList

    if List.isEmpty wireList then None
    else 
        let inputPortID = 
            wireList
            |> (List.head >> snd)
            |> (fun wire -> wire.InputPort)
        let connectionID = 
            wireList
            |> List.head
            |> fst
        Some (connectionID, inputPortID)

let createNewWireAndUpdateSheet (inputPortID: InputPortId) (outputPortID: OutputPortId) (sheet: SheetT.Model)  =
    let newWire = BusWireUpdate.makeNewWire inputPortID outputPortID sheet.Wire
    sheet
    |> Optic.set (busWireModel_ >-> wireOf_ newWire.WId) newWire

let deleteSymbolsAndUpdateSheet (symbols: list<ComponentId>) (sheet: SheetT.Model) = 
    sheet
    |> Optic.set (symbolModel_) (deleteSymbols sheet.Wire.Symbol symbols)

let turnWireLabelsToWires (wireLabel: SymbolT.Symbol) (sheet: SheetT.Model) = 
    let wires = sheet.Wire.Wires    
    match wireLabel.Component.Type with
    | IOLabel ->
        let wireLabelName = wireLabel.Component.Label
        let wireOutputInfo = findWireByLabelInputPort wires wireLabel
        let wireInputInfo = findWireByLabelOutputPort wires wireLabel
        /// Other wire labels with matching name as given wire label.
        /// A wire label at source symbol could have a list of matching labels 
        /// at target symbols if in a net.
        /// A wire label at target symbol only has 1 matching labels at the source.
        // let matchingWireLabels = 
        //     mapValues sheet.Wire.Symbol.Symbols
        //     |> Array.filter (fun sym -> (sym.Component.Label = wireLabelName) && sym.Id <> wireLabel.Id)
        //     |> Array.toList
        let matchingWireLabels = 
            mapValues sheet.Wire.Symbol.Symbols
            |> Array.filter (fun sym -> (sym.Component.Label = wireLabelName) && sym.Id <> wireLabel.Id)
            |> Array.toList

        if not (List.isEmpty matchingWireLabels)
        then 
            let allWireLabelsCompID = 
                matchingWireLabels
                |> List.append [wireLabel]
                |> List.map (fun sym -> sym.Id)
            match wireInputInfo, wireOutputInfo with
            | None, Some wireInfo->
                let wiresNeedDelete, outputPortID = wireInfo
                let matchingWireLabelsWires = 
                    matchingWireLabels
                    |> List.choose (fun sym -> findWireByLabelOutputPort wires sym)

                let inputPorts = 
                    matchingWireLabelsWires
                    |> List.map snd

                let deleteWireLabelWires (sheet: SheetT.Model) = 
                    let wireList = 
                        matchingWireLabelsWires
                        |> List.map (fst)
                        |> List.append [wiresNeedDelete]

                    (sheet, wireList)
                    ||> List.fold (fun updatedSheet wireID -> deleteWire wireID updatedSheet)
                    
                let updatedSheet = 
                    (sheet,inputPorts)
                    ||> List.fold (fun newSheet inputPortID ->
                        newSheet
                        |> deleteWireLabelWires
                        |> deleteSymbolsAndUpdateSheet allWireLabelsCompID
                        |> createNewWireAndUpdateSheet inputPortID outputPortID )
                updatedSheet

            | Some wireInfo, None  ->
                let wireNeedsDelete, inputPortID = wireInfo
                let matchingWireLabelsWires = 
                    matchingWireLabels
                    |> List.choose (fun sym -> findWireByLabelInputPort wires sym)
                let outputPortID = matchingWireLabelsWires |> (List.head >> snd)
                let deleteWireLabelWiresAndLabels (sheet: SheetT.Model) = 
                    if matchingWireLabels.Length > 1 
                    then 
                        sheet
                        |> deleteWire wireNeedsDelete 
                        |> deleteSymbolsAndUpdateSheet [wireLabel.Id]
                        
                    else 
                        let wireList = 
                            matchingWireLabelsWires
                            |> List.map (fst)
                            |> List.append [wireNeedsDelete]

                        (sheet, wireList)
                        ||> List.fold (fun updatedSheet wireID -> deleteWire wireID updatedSheet)
                        |> deleteSymbolsAndUpdateSheet allWireLabelsCompID
                        
                sheet 
                |> deleteWireLabelWiresAndLabels        
                |> createNewWireAndUpdateSheet inputPortID outputPortID

            | _, _ -> sheet

        else sheet

    | _ -> sheet

let autoConvertWireLabelsToWires (sheet: SheetT.Model) = 
    let symbolMap = mapValues sheet.Wire.Symbol.Symbols 
    (sheet, symbolMap)
    ||> Array.fold (fun sheet sym -> turnWireLabelsToWires sym sheet)       



    
