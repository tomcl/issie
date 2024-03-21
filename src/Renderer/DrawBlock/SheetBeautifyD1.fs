module SheetBeautifyD1

open CommonTypes
open DrawModelType
open BlockHelpers
open BusWire
open SymbolT
open SheetBeautifyHelpers
open DrawModelType
open Optics

// this is the module for team phase work D1 

/// Given a map, convert all map values to a list
let mapValuesToList map = 
    map
    |> Helpers.mapValues
    |> Array.toList

/// Given a XYPos, return the orientation of the segment
let getOrientation (pos: XYPos): BusWireT.Orientation = 
    if pos.X = 0.0 then BusWireT.Vertical else BusWireT.Horizontal

/// Given a wireId, determine whether it is a parallel wire, if so, return the vector of the snd segment of the wire
let isParallelWire (wireId: ConnectionId) (model: SheetT.Model): XYPos option= 
    let segsLst = SegmentHelpers.visibleSegments wireId model
    match segsLst.Length with
        | 3 ->  match getOrientation segsLst.[0] = getOrientation segsLst.[2] && getOrientation segsLst.[0] <> getOrientation segsLst.[1] &&
                segsLst.[0].X * segsLst.[2].X >= 0 && segsLst.[0].Y * segsLst.[2].Y >= 0  with //lengths of seg1 and seg3 need to have the same sign
                | true -> Some segsLst.[1]
                | false -> None
        | _ -> None

/// Given a symbol, return how many wires are connected to it
let connectedWiresCount (sym: Symbol) (sheetModel: SheetT.Model): int = 
    let portsLst = 
        sym.PortMaps.Order 
        |> Map.toList 
        |> List.collect snd

    let wiresModel = 
        sheetModel.Wire.Wires 
        |> mapValuesToList

    let connectedWires = 
        portsLst
        |> List.collect (fun portId ->
            wiresModel
            |> List.filter (fun wire -> 
                wire.InputPort = InputPortId portId || wire.OutputPort = OutputPortId portId))

    connectedWires.Length

/// Given a XYPos, return the negative XYPos
let negXYPos (pos: XYPos) : XYPos = {X = -pos.X; Y = -pos.Y}


let getFstOfSndTuple (tuple : ('a * ('b * 'c))) = 
    match tuple with
    | (_, snd) -> fst snd


let getSndOfSndTuple (tuple : ('a * ('b * 'c))) = 
    match tuple with
    | (_, snd') -> snd snd'


/// Update one symbol with a given offset
let updateOneSym (sym: Symbol) (sheetModel: SheetT.Model) ((offset, portType): (XYPos * PortType)) (intersectSymPairCount): Symbol option= 
    printfn "updating one sym with offset %s" (pXY offset)
    let newSym = 
        match portType with
        | PortType.Output -> moveSymbol  offset sym
        | PortType.Input -> moveSymbol  (negXYPos offset) sym
    let newSheet = 
        sheetModel
        |> Optic.set SheetT.symbol_ (SymbolUpdate.replaceSymbol sheetModel.Wire.Symbol newSym newSym.Id)
        |> SheetUpdateHelpers.updateBoundingBoxes
    // if the new symbol intersects with any other symbol, return None
    printfn "intersected symbols after beautify: %d" (numOfIntersectedSymPairs newSheet)
    printfn "intersected symbols before beautify: %d" intersectSymPairCount
    match (numOfIntersectedSymPairs newSheet) > intersectSymPairCount with
    | false -> Some newSym
    | _ -> None

/// Find all wires that are parallel in a sheet which is not in the stubbornWiresLst 
let getParallelWiresLst (sheetModel: SheetT.Model) (wiresCannotStraighten: ConnectionId list): BusWireT.Wire list= 
    sheetModel.Wire.Wires 
    |> mapValuesToList 
    |> List.collect (fun wire -> 
        match isParallelWire wire.WId sheetModel with
            | Some seg -> [wire]
            | None -> [])
    |> List.filter (fun wire -> 
        not (List.exists (fun wireCannotStraighten -> wire.WId = wireCannotStraighten) wiresCannotStraighten))

/// Given a portId and a symbol, return the portType of the port
let getPortType (portId: string) (sym: Symbol): PortType = 
    let port = 
        sym.Component.InputPorts @ sym.Component.OutputPorts
        |> List.find (fun port -> port.Id = portId)
    port.PortType

/// Determine whether a symbol is movable
let isMovable (sym: Symbol) (portId: string) (parallelWirePortsLst : string list): bool= 
    printfn "checking if the symbol %s is movable" (pXY sym.Pos)
    let edge = sym.PortMaps.Orientation[portId]
    //ports on given edge
    sym.PortMaps.Order[edge]
    //if the port is connected to only one parallel wire in one edge, it is movable
    |> List.map (fun portId -> List.exists (fun parallelWirePortId -> parallelWirePortId = portId) parallelWirePortsLst)
    |> List.length = 1

/// Given a symbol, return all the ports of the symbol
let getSymPorts (sym: Symbol) =
    sym.Component.InputPorts @ sym.Component.OutputPorts
    |> List.map (fun port -> port.Id)

/// Given two symbols, return the wires between them
let getWiresBetweenTwoSymbol (sym1 : Symbol) (sym2 : Symbol) (sheetModel : SheetT.Model): BusWireT.Wire list = 
    let sym1Ports = getSymPorts sym1
    let sym2Ports = getSymPorts sym2
    let wires = sheetModel.Wire.Wires |> mapValuesToList
    wires
    |> List.filter (fun wire -> 
        let inputPortId =  wire.InputPort.ToString()
        let outputPortId =  wire.OutputPort.ToString()
        ((List.exists (fun portId -> portId = inputPortId) sym1Ports) && (List.exists (fun portId -> portId = outputPortId) sym2Ports)) ||
        ((List.exists (fun portId -> portId = outputPortId) sym1Ports) && (List.exists (fun portId -> portId = inputPortId) sym2Ports)))


/// Return true if the component type is custom
let isCustomComponentType (componentType: ComponentType) =
    match componentType with
    | Custom customType -> true
    | _ -> false

/// Extract all Custom symbols from the sheetModel
let getCustomSyms (sheetModel : SheetT.Model) : Symbol list = 
    sheetModel.Wire.Symbol.Symbols
    |> mapValuesToList
    |> List.filter (fun sym -> isCustomComponentType sym.Component.Type)

/// Choose which custom symbol to move
let chooseCSymToMove (sym1 : Symbol) (edge1 : Edge) (sym2 : Symbol) (edge2 : Edge): Symbol =
    let sym1Ports = sym1.PortMaps.Order.[edge1].Length
    let sym2Ports = sym2.PortMaps.Order.[edge2].Length
    if sym1Ports > sym2Ports 
    then
        printfn "sym to change dim : %s" sym2.Component.Label
        sym2
    else 
        printfn "sym to change dim : %s" sym1.Component.Label
        sym1


/// Get the port gap of a custom symbol in certain edge
let getPortGapOfCSym (sym : Symbol) (edge : Edge) : float = 
    let gap = Symbol.getPortPosEdgeGap sym.Component.Type
    let portDimension = float sym.PortMaps.Order.[edge].Length - 1.0
    match edge with
    | Left | Right -> sym.Component.H / (portDimension + 2.0 * gap)
    | Top | Bottom -> sym.Component.W / (portDimension + 2.0 * gap)
    | _ -> failwith "not implement yet"


/// Given destred port gap, calculate the dimension of the custom symbol
let calculateDimension (sym : Symbol) (edge : Edge) (desiredPortGap : float) : XYPos = 
    let portDimension = float sym.PortMaps.Order.[edge].Length - 1.0
    let gap = Symbol.getPortPosEdgeGap sym.Component.Type
    let h = 
        match edge with
        | Left -> desiredPortGap * (portDimension + 2.0 * gap)
        | Right -> desiredPortGap * (portDimension + 2.0 * gap)
        | _ -> 
            printfn "top or bottom side are not implement yet"
            sym.Component.H
    {X = sym.Component.W; Y = h}

/// Move custom symbols whose dimension was changed, to straighten the wire
let moveCSym (wires : BusWireT.Wire list) (sheetModel : SheetT.Model): SheetT.Model =
    let wiresInfo = 
        wires
        |> List.collect (fun wire -> 
            match isParallelWire wire.WId sheetModel with
            | Some offset -> [wire, offset]
            | None -> [])

    let symsToMove = 
        wiresInfo
        |> List.map (fun (wire, offset) -> getSourceSymbol sheetModel.Wire wire, offset)
        |> List.distinct
    
    let newSyms =
        symsToMove
        |> List.map (fun (sym, offset) -> 
            let newSym = moveSymbol offset sym
            newSym)
    
    let newWireModel =
        updateModelWires (updateModelSymbols sheetModel.Wire newSyms) []
        |> BusWireSeparate.updateWireSegmentJumpsAndSeparations []

    let newWireModel' =
        BusWireRoute.updateWires newWireModel (newSyms |> List.map (fun sym -> sym.Id)) {X = 0.0; Y = 0.0}
    
    let newSheetModel =
        sheetModel
        |> Optic.set SheetT.wire_ (newWireModel')
        |> SheetUpdateHelpers.updateBoundingBoxes
    
    newSheetModel

/// Modify custom symbols' scale and position to straighten wires
let alignCSyms (sheetModel : SheetT.Model) : SheetT.Model= 
    sheetModel
    // printfn "aligning custom symbols................"
    // let cSyms = getCustomSyms sheetModel
    // let cSymPairs =  cSyms |> List.pairwise
    // let cSymPairsToAlign = 
    //     cSymPairs
    //     |> List.filter (fun (sym1, sym2) -> 
    //         let wires = getWiresBetweenTwoSymbol sym1 sym2 sheetModel
    //         let parallelWires = wires |> List.filter (fun wire -> isParallelWire wire.WId sheetModel |> Option.isSome)
    //         // we can have at most 1 straight wire between two symbols
    //         wires.Length - parallelWires.Length < 2)

    // let cSymPairsToAlignInfo = 
    //     cSymPairsToAlign
    //     |> List.map (fun (sym1,sym2) -> 
    //         let wires = getWiresBetweenTwoSymbol sym1 sym2 sheetModel
    //         let wire = wires |> List.head
    //         let sourceSym = getSourceSymbol sheetModel.Wire wires.[0]
    //         let targetSym = getTargetSymbol sheetModel.Wire wires.[0]

    //         let (sourceEdge, targetEdge) = (sourceSym.PortMaps.Orientation.[wire.OutputPort.ToString()], targetSym.PortMaps.Orientation.[wire.InputPort.ToString()])

    //         if chooseCSymToMove sourceSym sourceEdge targetSym targetEdge = sourceSym then
    //             let desiredPortGap = getPortGapOfCSym targetSym targetEdge
    //             let dimension = calculateDimension sourceSym sourceEdge desiredPortGap
    //             (sourceSym, dimension, wire)
    //         else
    //             let desiredPortGap = getPortGapOfCSym sourceSym sourceEdge
    //             let dimension = calculateDimension targetSym targetEdge desiredPortGap
    //             (targetSym, dimension, wire))

    // let newCSyms = 
    //     cSymPairsToAlignInfo
    //     |> List.map (fun (sym, dimension, wire) -> putCustomCompDims dimension sym)

    // let wiresToStraighten = 
    //     cSymPairsToAlignInfo
    //     |> List.map (fun (sym, dimension, wire) -> wire)

    // let newCsymIds = 
    //     newCSyms
    //     |> List.map (fun sym -> sym.Id)

    // let newWireModel = 
    //     updateModelWires (updateModelSymbols sheetModel.Wire newCSyms ) []
    //     |> BusWireSeparate.updateWireSegmentJumpsAndSeparations [] 

    // let newWireModel' = 
    //     BusWireRoute.updateWires newWireModel newCsymIds { X = 0.0; Y = 0.0 } 

    // let newSheetModel = 
    //     sheetModel
    //     |> Optic.set SheetT.wire_ (newWireModel')
    //     |> SheetUpdateHelpers.updateBoundingBoxes // could optimise this by only updating symId bounding boxes

    // newSheetModel
    // |> moveCSym wiresToStraighten


/// Straighten parallel wires in the sheetModel, once
let rec sheetAlignScaleOnce (sheetModel: SheetT.Model) (stubbornWiresLst : ConnectionId list) (intersectSymPairCount : int)=
    printfn "%s" "aligning and scaling start!"
    printfn "stubborn wires: %d" stubbornWiresLst.Length
    let parallelWiresLst = getParallelWiresLst sheetModel stubbornWiresLst
    printfn "parallel wires: %d" parallelWiresLst.Length
    // if there is no parallel wire, return the original sheetModel
    match parallelWiresLst.Length with
    | 0 -> 
        printfn "no parallel wire found, aligning and scaling finished"
        printfn "intersected symbols found: %d" (numOfIntersectedSymPairs sheetModel)
        sheetModel
    | _ ->
        let parallelWirePortsLst=
            parallelWiresLst
            |> List.collect (fun (wire: BusWireT.Wire) -> [wire.InputPort.ToString(); wire.OutputPort.ToString()])
        
        // filter out the wires that are not movable
        let newNotMovableWires = 
            parallelWiresLst
            |> List.filter (fun wire -> 
                let sourceSym = getSourceSymbol sheetModel.Wire wire
                let targetSym = getTargetSymbol sheetModel.Wire wire
                let inputPortId =  wire.InputPort.ToString()
                let outputPortId =  wire.OutputPort.ToString()
                not (isMovable sourceSym outputPortId parallelWirePortsLst || isMovable targetSym inputPortId parallelWirePortsLst))
            |> List.map (fun wire -> wire.WId)

        // update the stubbornWiresLst
        let newStubbonWiresLst = 
            stubbornWiresLst @ newNotMovableWires
            |> List.distinct

        // Given a wire, return how many wires are connected to the source and target symbols of the wire
        let getRelateWiresCount (wire : BusWireT.Wire) : int =
            connectedWiresCount (getSourceSymbol sheetModel.Wire wire) sheetModel
            + connectedWiresCount (getTargetSymbol sheetModel.Wire wire) sheetModel

        /// Given a wire and a sheetModel, return the wire to move
        let wireToStraighten : BusWireT.Wire option = 
            parallelWiresLst
            |> List.filter (fun wire -> not (List.contains wire.WId newNotMovableWires))
            |> List.map (fun wire -> (wire, getRelateWiresCount wire))
            |> List.sortByDescending snd
            |> List.tryHead // move the wire that has the most connected wires
            |> Option.map fst

        match wireToStraighten with
        //none happens when all the parallel wires are not movable, thus return the original sheetModel
        | None -> 
            printfn "%s" "no wire to move, continue to align and scale"
            sheetModel
        // if there is a wire to move, move the symbol and update the sheetModel and call the function recursively
        | Some wireToMove ->
            let offset = isParallelWire wireToMove.WId sheetModel |> Option.defaultValue XYPos.zero
            let sourceSym = getSourceSymbol sheetModel.Wire wireToMove
            let targetSym = getTargetSymbol sheetModel.Wire wireToMove
            let moveInfo = 
                match isMovable sourceSym (wireToMove.OutputPort.ToString()) parallelWirePortsLst, isMovable targetSym (wireToMove.InputPort.ToString()) parallelWirePortsLst with
                | true, false -> [sourceSym, (PortType.Output, offset)]
                | false, true -> [targetSym, (PortType.Input, offset)]
                | true, true -> 
                    if connectedWiresCount sourceSym sheetModel < connectedWiresCount targetSym sheetModel 
                    then [targetSym, (PortType.Input, offset); sourceSym, (PortType.Output, offset)]
                    else [sourceSym, (PortType.Output, offset); targetSym, (PortType.Input, offset)]
                | false, false -> failwith "impossible" // this should not happen

            let newSym =
                match moveInfo.Length with
                | 1 -> 
                    updateOneSym (fst moveInfo.[0]) sheetModel (getSndOfSndTuple moveInfo.[0], getFstOfSndTuple moveInfo.[0]) intersectSymPairCount
                | 2 ->
                    match updateOneSym (fst moveInfo.[0]) sheetModel (getSndOfSndTuple moveInfo.[0], getFstOfSndTuple moveInfo.[0]) intersectSymPairCount with
                    | Some sym -> Some sym
                    | None -> 
                        printfn "%s" "one end sym failed to move, move the other end sym instead"
                        updateOneSym (fst moveInfo.[1]) sheetModel (getSndOfSndTuple moveInfo.[1], getFstOfSndTuple moveInfo.[1]) intersectSymPairCount
                | _ -> failwith "impossible" // this should not happen
            match newSym with
            | None -> 
                printfn "%s" "intersected symbols found, add new wire to stubbornWiresLst" 
                sheetAlignScaleOnce sheetModel (wireToMove.WId::newStubbonWiresLst) (numOfIntersectedSymPairs sheetModel) 
            | Some newSym ->
                let newWireModel = 
                    updateModelWires (updateModelSymbols sheetModel.Wire [newSym] ) [parallelWiresLst |> List.head]
                    |> BusWireSeparate.updateWireSegmentJumpsAndSeparations [(parallelWiresLst |> List.head).WId] 

                let newWireModel' = 
                    BusWireRoute.updateWires newWireModel [newSym.Id] { X = 0.0; Y = 0.0 } 

                let newSheetModel = 
                    sheetModel
                    |> Optic.set SheetT.wire_ (newWireModel')
                    |> SheetUpdateHelpers.updateBoundingBoxes
                
                printfn "%s" "straighten one wire, continue to align and scale"
                sheetAlignScaleOnce newSheetModel newStubbonWiresLst (numOfIntersectedSymPairs newSheetModel)

/// Straighten parallel wires in the sheetModel, multiple times
let rec sheetAlignScale (runTimes : int) (sheetModel: SheetT.Model) =
    let newSheet = alignCSyms sheetModel
    if runTimes = 0 then alignCSyms newSheet
    else
        printfn "runTimes: %d" runTimes
        let newSheetModel = sheetAlignScaleOnce newSheet [] (numOfIntersectedSymPairs newSheet)
        sheetAlignScale (runTimes - 1) newSheetModel