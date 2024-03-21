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

/// Run the global wire separation algorithm (should be after all wires have been placed and routed)
let separateAllWires (model: SheetT.Model) : SheetT.Model =
    model
    |> Optic.map busWireModel_ (BusWireSeparate.updateWireSegmentJumpsAndSeparations (model.Wire.Wires.Keys |> Seq.toList))

let rerouteAllWires (sheet: SheetT.Model) : SheetT.Model=
    let comps = mapKeys sheet.Wire.Symbol.Symbols |> Array.toList
    let newWModel = List.fold (BusWireSeparate.routeAndSeparateSymbolWires) sheet.Wire comps
    Optic.set (SheetT.wire_) newWModel sheet
let getWireListFromSheet (sheet: SheetT.Model) = sheet.Wire.Wires |> Map.toList

/// Return a list of wires that are long and are not straight which can potentially be replaced with wire labels
let getLongWires (wireLengthlimit: float) (sheet: SheetT.Model) (wireList: List<BusWireT.Wire>)  =
    wireList
    |> List.filter (fun wire -> (getWireLength wire > wireLengthlimit))
    |> List.filter (fun wire -> (SegmentHelpers.visibleSegsWithVertices wire sheet).Length >1)

/// Return a map of wires grouped by net (multiple wires with same source port).
/// And a list of single wires that are too long and complex.
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
        |> getLongWires wireLengthlimit sheet
    
    wireInNet 
    |> flattenList
    |> List.map snd
    |> List.append longWires

let deleteWire (wireCID: ConnectionId) (sheet: SheetT.Model) =
    let newWires =
        sheet.Wire.Wires
        |> Map.filter (fun id wire -> not (wire.WId = wireCID))
    { sheet with Wire = { sheet.Wire with Wires = newWires } }

/// Returns appropriate position and rotation of the Wire Label placed at the source symbol
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

/// Returns appropriate position and rotation of the Wire Label placed at the target symbol
let findWireLabelRoomAtTarget (wire: BusWireT.Wire) (distance: float) = 
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
            pos, Degree90
        else 
            printf "portpos %.2f, %.2f " lastSeg.Start.X (lastSeg.Start.Y - distance)
            let pos = { lastSeg.Start with X = lastSeg.Start.X; Y = lastSeg.Start.Y - distance}
            pos, Degree270

let getMovedSymbolBB (move: XYPos) (sym: SymbolT.Symbol) : BoundingBox =
    {sym.LabelBoundingBox with
        TopLeft =  sym.LabelBoundingBox.TopLeft + move}
    
/// Check if the default position of a Wire Label is good, else
/// adjust the position so it's placed at a location
/// where it doesn't intersect with other symbols on the sheet.
/// Returns an Option of XYPos if such location is found within
/// a grid range of original position
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
        |> List.exists (fun box -> BlockHelpers.overlap2DBox symbolBB box)

    let tryMoveWireLabel moveAmount =
        let newWireLabelSymBB = getMovedSymbolBB moveAmount wireLabelSym
        not (checkIfIntersect newWireLabelSymBB)

    let gridPositions = 
        let offsets = [0.;1.;-1.];
        offsets 
        |> List.allPairs offsets 
        |> List.tail
        |> List.map (fun (offsetX, offsetY) -> {X = offsetX*adjustmentAmount; Y=offsetY*adjustmentAmount})
        |> List.sortBy (euclideanDistance {X=0.; Y=0.})

    let scalePosition (multiplier: float) (pos: XYPos) : XYPos =
        { X = pos.X * multiplier; Y = pos.Y * multiplier }

    let rec tryAdjust (gridPos: XYPos List) (attemptCounter: float) =  
        if attemptCounter > 2. then
            None  // Terminate recursion after 2 tries
        else 
            match gridPos |> List.tryFind (tryMoveWireLabel) with
            | Some offset -> Some offset
            | None -> 
                let furtherGridPositions = 
                    gridPositions
                    |> List.map (scalePosition (attemptCounter+1.) )
                printf "%.2f: " attemptCounter
                tryAdjust furtherGridPositions (attemptCounter+1.)

    let newPos (moveAmount: XYPos) =
        {originalPos with X = originalPos.X + moveAmount.X; Y = originalPos.Y + moveAmount.Y}

    if checkIfIntersect wireLabelSym.SymbolBoundingBox
    then 
        match tryAdjust gridPositions 1. with
        | Some moveAmount -> 
            printfn "Moved"
            Some (newPos moveAmount)
            //Some originalPos
        | None -> None
    else 
        printfn "Original position"
        Some originalPos
        

/// Generate Wire Label for a wire connected between source symbol and target symbol
/// or keep the wire if not enough room for label at either source or target symbol
let generateWireLabel (wire: BusWireT.Wire) (sheet: SheetT.Model) =
    let connectionID = wire.WId
    let startSym = getSourceSymbol sheet.Wire wire
    let sourceSymPort = getSourcePort sheet.Wire wire
    let inputPortNumber = sourceSymPort.PortNumber
    let wireLabelName =
        startSym.Component.Label
        + "OUT"
        + string inputPortNumber

    let inputPort, outputPort = wire.InputPort, wire.OutputPort

    let wireLabelSourcePos, rotationSource = findWireLabelRoomAtSource wire 40.0
    let wireLabelTargetPos, rotationTarget = findWireLabelRoomAtTarget wire 40.0

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

        let adjustLabelOnSheet = 
            match adjustWireLabelPos labelSym sheetWithWireLabelAdded with
            | Some newPos -> 
                // printf "newpos %.2f, %.2f " newPos.X newPos.Y
                //printf "labelpos %s, %.2f, %.2f" labelName labelSym.Component.X labelSym.Component.Y
                let adjustedSheet = updateSymPosInSheet labelID newPos sheetWithWireLabelAdded
                let newWire = 
                    BusWireUpdate.makeNewWire (inputPortIDstr) (outputPortIdstr) adjustedSheet.Wire
                let newSheet = 
                    adjustedSheet
                    |> Optic.set (busWireModel_ >-> wireOf_ newWire.WId) newWire
                Some newSheet
            | None -> 
                printf "no room"
                None
        adjustLabelOnSheet
    
    let tryAddWireLabelAtSource = 
        if sheet.Wire.Symbol.Symbols
            |> Map.exists (fun _ sym -> caseInvariantEqual sym.Component.Label wireLabelName) // if wire in a net then this would be true
        then Some sheet // Don't want to duplicate wirel Label at net inputPort
        else 
            sheet
            |> addWireLabelAndConnectWire wireLabelSourcePos rotationSource wireLabelName IOLabel true inputPort outputPort

    match tryAddWireLabelAtSource with
    | Some addedWireLabelOnSheet -> 
        let tryAddWireLabelAtTarget = 
            addedWireLabelOnSheet
            |> addWireLabelAndConnectWire wireLabelTargetPos rotationTarget wireLabelName IOLabel false inputPort outputPort 
        match tryAddWireLabelAtTarget with
        | Some  addedWireLabelOnSheet -> 
            addedWireLabelOnSheet
            |> deleteWire connectionID
        |   None -> sheet
    | None -> sheet

/// Automatically generate Wire Labels for all wires on a sheet
let sheetWireLabelSymbol (sheet: SheetT.Model) =
    let wireLengthlimit = 120. // User can decide what is considered long wire
    let wiresNeedLabels = getWiresNeedLabels sheet wireLengthlimit
    (sheet, wiresNeedLabels)
    ||> List.fold (fun sheet wire -> generateWireLabel wire sheet)
    |> rerouteAllWires

/// Find the wire connected to input port of a Wire Label (i.e the Wire Label
/// is at the source symbol).
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

/// Find the wire connected to output port of a wire label. (i.e the Wire Label
/// is at the target symbol).
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
    // Ignores symbol if not a Wire Label
    match wireLabel.Component.Type with
    | IOLabel ->
        let wires = sheet.Wire.Wires    
        let wireLabelName = wireLabel.Component.Label
        let wireOutputInfo = findWireByLabelInputPort wires wireLabel
        let wireInputInfo = findWireByLabelOutputPort wires wireLabel

        /// Other wire labels with matching name as given wire label.
        let matchingWireLabels = 
            sheet.Wire.Symbol.Symbols
            |> Map.filter (fun _ sym -> (sym.Component.Label = wireLabelName) && sym.Id <> wireLabel.Id)
            |> Map.toList
            |> List.map snd

        if not (List.isEmpty matchingWireLabels) then 
            let allWireLabelsCompID = 
                matchingWireLabels
                |> List.append [wireLabel]
                |> List.map (fun sym -> sym.Id)
 
            match wireInputInfo, wireOutputInfo with
            // When the Wire Label is at source symbol, delete all matching Wire Labels 
            // and the wires connected to them. Add wires back between souce and target symbols
            | None, Some wireInfo ->
                let wireNeedsDelete, outputPortID = wireInfo
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
                        |> List.append [wireNeedsDelete]

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

            // When the Wire Label is at target symbol, if not in a net, delete the pair of Wire Labels
            // and the wires connected to them; else only delete the Wire Label itself and its wire.
            // Add wire back between the souce and target symbol 
            | Some wireInfo, None  ->
                let wireNeedsDelete, inputPortID = wireInfo
                let matchingWireLabelsWire, outputPortID = 
                    matchingWireLabels
                    |> List.choose (fun sym -> findWireByLabelInputPort wires sym)
                    |> List.head

                let deleteWireLabelWiresAndLabels (sheet: SheetT.Model) = 
                    if matchingWireLabels.Length > 1 
                    then 
                        sheet
                        |> deleteWire wireNeedsDelete 
                        |> deleteSymbolsAndUpdateSheet [wireLabel.Id]
                        
                    else 
                        let wireList = [matchingWireLabelsWire; wireNeedsDelete]
                        (sheet, wireList)
                        ||> List.fold (fun updatedSheet wireID -> deleteWire wireID updatedSheet)
                        |> deleteSymbolsAndUpdateSheet allWireLabelsCompID
                        
                sheet 
                |> deleteWireLabelWiresAndLabels        
                |> createNewWireAndUpdateSheet inputPortID outputPortID

            | _, _ -> sheet

        else sheet

    | _ -> sheet

/// Automatically convert all Wire Labels on the sheet to wires between
/// corresponding inputs and outputs
let autoConvertWireLabelsToWires (sheet: SheetT.Model) = 
    let symbolMap = mapValues sheet.Wire.Symbol.Symbols 
    (sheet, symbolMap)
    ||> Array.fold (fun sheet sym -> turnWireLabelsToWires sym sheet)       



    
