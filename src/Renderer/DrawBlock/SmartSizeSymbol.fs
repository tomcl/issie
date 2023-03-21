module SmartSizeSymbol

open Elmish
open Fable.React.Props
open CommonTypes
open Fable.React
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Symbol
open Optics
open Optic
open Operators
open SmartHelpers
open BusWire

(* 
    HLP23: This module will normally be used exclusively by team member doing the "smart resize symbol" 
    part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    Normally it will update multiple wires and one symbols in the BusWire model so could use the SmartHelper 
    function for the wires.
*)

/// record containing all the information required to calculate the position of a port on the sheet
type PortInfo =
    { port: Port
      sym: Symbol
      side: Edge
      ports: string list
      gap: float
      topBottomGap: float
      portDimension: float
      h: float
      w: float
      portGap: float }

/// TODO: this is mostly copy pasted code from Symbol.getPortPos, perhaps abstract out the existing code there to use makePortInfo.
/// Could not simply use getPortPos because more data (side, topBottomGap, etc.) is needed to caclulate the new dimensions of the resized symbol
let makePortInfo (sym: Symbol) (port: Port) =
    let side = getSymbolPortOrientation sym port
    let ports = sym.PortMaps.Order[side] //list of ports on the same side as port
    let gap = getPortPosEdgeGap sym.Component.Type
    let topBottomGap = gap + 0.3 // extra space for clk symbol
    let portDimension = float ports.Length - 1.0
    let h, w = getRotatedHAndW sym

    let portGap =
        match side with
        | Left
        | Right -> float h / (portDimension + 2.0 * gap)
        | Bottom
        | Top -> float w / (portDimension + 2.0 * topBottomGap)

    { port = port
      sym = sym
      side = side
      ports = ports
      gap = gap
      topBottomGap = topBottomGap
      portDimension = portDimension
      h = h
      w = w
      portGap = portGap }

type wireSymbols =
    { symA: Symbol
      symB: Symbol
      wire: Wire }

let getPortAB wModel wireSyms =
    let ports = portsOfWires wModel [ wireSyms.wire ]
    let portA = filterPortBySym ports wireSyms.symA |> List.head
    let portB = filterPortBySym ports wireSyms.symB |> List.head
    portA, portB

/// Try to get two ports that are on opposite edges
let getOppEdgePortInfo
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    : (PortInfo * PortInfo) option =
    let wires = wiresBtwnSyms wModel symbolToSize otherSymbol

    let tryGetOppEdgePorts wireSyms =
        let portA, portB = getPortAB wModel wireSyms
        let edgeA = getSymbolPortOrientation wireSyms.symA portA
        let edgeB = getSymbolPortOrientation wireSyms.symB portB

        match edgeA = edgeB.Opposite with
        | true -> Some(makePortInfo wireSyms.symA portA, makePortInfo wireSyms.symB portB)
        | _ -> None

    wires
    |> List.tryPick (fun w ->
        tryGetOppEdgePorts{ symA = symbolToSize; symB = otherSymbol; wire = w })

let alignPortsOffset (movePInfo: PortInfo) (otherPInfo: PortInfo) =
    let getPortRealPos pInfo =
        getPortPos pInfo.sym pInfo.port + pInfo.sym.Pos

    let movePortPos = getPortRealPos movePInfo
    let otherPortPos = getPortRealPos otherPInfo
    let posDiff = otherPortPos - movePortPos

    match movePInfo.side with
    | Top
    | Bottom -> { X = posDiff.X; Y = 0.0 }
    | Left
    | Right -> { X = 0.0; Y = posDiff.Y }

let alignSymbols
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    (smartHelpers: ExternalSmartHelpers)
    : BusWireT.Model =

    // only attempt to align symbols if they are connected by ports on parallel edges
    match getOppEdgePortInfo wModel symbolToSize otherSymbol with
    | None -> wModel
    | Some(movePortInfo, otherPortInfo) ->
        let offset = alignPortsOffset movePortInfo otherPortInfo
        let symbol' = moveSymbol offset symbolToSize
        let model' = set (symbolOf_ symbolToSize.Id) symbol' wModel
        smartHelpers.UpdateSymbolWires model' symbolToSize.Id


/// HLP23: To test this, it must be given two symbols interconnected by wires. It then resizes symbolToSize
/// so that the connecting wires are exactly straight
/// HLP23: It should work out the interconnecting wires (wires) from
////the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has adjustable size).
/// HLP23: when this function is written replace teh XML comment by something suitable concisely
/// stating what it does.
/// Given two connected symbols, resize the selected symbol to make the port gaps on both symbols the same.
/// Translate the selected symbol so that the ports on both symbols align and the connecting wires become straight.
let reSizeSymbol (wModel: BusWireT.Model) (symbolToSize: Symbol) (otherSymbol: Symbol) : (Symbol * (XYPos * ScaleFactor)) =
    let wires = wiresBtwnSyms wModel symbolToSize otherSymbol

    // try to get two ports that are on opposite edges, if none found just use any two ports
    let resizePortInfo, otherPortInfo =
        match getOppEdgePortInfo wModel symbolToSize otherSymbol with
        | None ->
            let pA, pB = getPortAB wModel{ symA = symbolToSize; symB = otherSymbol; wire = wires[0] }

            makePortInfo symbolToSize pA, makePortInfo symbolToSize pB
        | Some(pIA, pIB) -> (pIA, pIB)

    let h, w =
        match resizePortInfo.side with
        | Left
        | Right ->
            otherPortInfo.portGap * (resizePortInfo.portDimension + 2.0 * resizePortInfo.gap), resizePortInfo.w
        | Top
        | Bottom ->
            resizePortInfo.h, otherPortInfo.portGap * (resizePortInfo.portDimension + 2.0 * resizePortInfo.topBottomGap)

    let scale = { x = h / symbolToSize.Component.H; y = w / symbolToSize.Component.W;}
    let scaledSymbol = setCustomCompHW h w symbolToSize
    let scaledInfo = makePortInfo scaledSymbol resizePortInfo.port
    let offset = alignPortsOffset scaledInfo otherPortInfo
    (moveSymbol offset scaledSymbol, (offset, scale))

/// For UI to call ResizeSymbol
let reSizeSymbolTopLevel
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    (smartHelpers: ExternalSmartHelpers)
    : BusWireT.Model =
    printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"

    let scaledSymbol, _ = reSizeSymbol wModel symbolToSize otherSymbol

    let model' = set (symbolOf_ symbolToSize.Id) scaledSymbol wModel
    smartHelpers.UpdateSymbolWires model' symbolToSize.Id

// for each edge of the symbol, store a count of how many connections it has to other symbols
type SymConnDataT =
    { ConnMap: Map<ComponentId * Edge, int> }


let wireSymEdge wModel wire sym = 
    let sPort, tPort = getSourcePort wModel wire, getTargetPort wModel wire
    let sEdge = Map.tryFind sPort.Id sym.PortMaps.Orientation
    let tEdge = Map.tryFind tPort.Id sym.PortMaps.Orientation

    match sEdge, tEdge with
    | Some e, None -> e
    | None, Some e -> e
    | _ -> Top // Shouldn't happen.

/// for a wire and a symbol, return the edge of the symbol that the wire is connected to
let tryWireSymEdge (wModel: Model) (wire: Wire) (sym: Symbol) (otherSym: Symbol) =
    let symEdge = wireSymEdge wModel wire sym
    let otherSymEdge = wireSymEdge wModel wire otherSym

    match symEdge = otherSymEdge.Opposite with
    | true -> Some symEdge
    | _ -> None

let updateOrInsert (symConnData: SymConnDataT) (edge: Edge) (cid: ComponentId) =
    let m = symConnData.ConnMap
    let count = Map.tryFind (cid, edge) m |> Option.defaultValue 0 |> (+) 1
    { ConnMap = Map.add (cid, edge) count m }

// TODO: this is copied from Sheet.notIntersectingComponents. It requires SheetT.Model, which is not accessible from here
let noSymbolOverlap (boxesIntersect: BoundingBox -> BoundingBox -> bool) boundingBoxes sym =
    let symBB = getSymbolBoundingBox sym

    boundingBoxes
    |> Map.filter (fun sId boundingBox -> boxesIntersect boundingBox symBB && sym.Id <> sId)
    |> Map.isEmpty

/// Finds the optimal size and position for the selected symbol w.r.t. to its surrounding symbols
///
let optimiseSymbol
    (wModel: BusWireT.Model)
    (symbol: Symbol)
    (boundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>)
    (smartHelpers: ExternalSmartHelpers)
    : BusWireT.Model =

    let updateData (symConnData: SymConnDataT) (connId: ConnectionId) (wire: Wire) =
        let symS, symT = getSourceSymbol wModel wire, getTargetSymbol wModel wire

        let otherSymbol =
            match symS, symT with
            | _ when (symS.Id <> symbol.Id) && (symT.Id = symbol.Id) ->
                printfn $"{symS.Component.Label}"
                Some symS
            | _ when (symS = symbol) && (symT <> symbol) ->
                printfn $"{symT.Component.Label}"
                Some symT
            | _ -> None

        match otherSymbol with
        | Some otherSym ->
            let edge = tryWireSymEdge wModel wire symbol otherSym

            match edge with
            | Some e -> updateOrInsert symConnData e otherSym.Id
            | None -> symConnData // should not happen
        | None -> symConnData

    // look through all wires to build up SymConnDataT
    let symConnData = ({ ConnMap = Map.empty }, wModel.Wires) ||> Map.fold updateData

    let tryResize (symCount: ((ComponentId * Edge) * int) array) (sym: Symbol) =
        let alignSym (sym: Symbol) (otherSym: Symbol) =
            let resizedSym, resizeData = reSizeSymbol wModel sym otherSym
            let noOverlap = noSymbolOverlap smartHelpers.BoxesIntersect boundingBoxes resizedSym

            match noOverlap with
            | true -> true, (resizedSym, Some resizeData) 
            | _ -> false, (sym, None)

        let folder (hAligned, vAligned, sym) ((cid, edge), count) =
            let otherSym = get (symbolOf_ cid) wModel
            match hAligned, vAligned with
            | false, _ when edge = Top || edge = Bottom ->
                let hAligned', (resizedSym, resizeData) = alignSym sym otherSym
                (hAligned', vAligned, resizedSym)
            | _, false when edge = Left || edge = Right ->
                let vAligned', (resizedSym, resizeData) = alignSym sym otherSym
                (hAligned, vAligned', resizedSym)
            | _ -> (hAligned, vAligned, sym)

        let (_, _, sym') = ((false, false, sym), symCount) ||> Array.fold folder
        sym'

    let scaledSymbol =
        let symCount =
            Map.toArray symConnData.ConnMap
            |> Array.filter (fun (_, count) -> count > 1)
            |> Array.sortByDescending snd

        tryResize symCount symbol

    let model' = set (symbolOf_ symbol.Id) scaledSymbol wModel
    smartHelpers.UpdateSymbolWires model' symbol.Id
