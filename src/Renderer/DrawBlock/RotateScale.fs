module RotateScale

open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open SymbolUpdate
open Symbol
open Optics
open Operators
open BlockHelpers
open SymbolResizeHelpers


(* ---------------------------------------------------------------------------------------------- *)
(*                                             Helpers                                            *)
(* ---------------------------------------------------------------------------------------------- *)

/// <summary>Get list of symbols from list of component IDs.</summary>
/// <param name="compIds">List of component IDs to fetch.</param>
/// <param name="model">Symbol model.</param>
/// <returns>List of symbols. If ComponentId is not present in symbol model, it will be omitted from output.</returns>
let getSymsFromIds (compIds: List<ComponentId>) (model: SymbolT.Model): List<Symbol> =
    compIds
    |> List.map (fun compId -> Map.tryFind compId model.Symbols)
    |> List.choose id

/// <summary>Apply mapping to some symbols, specified by <c>compIds</c>, within a symbol model.</summary>
/// <param name="mapping">Mapping function to transform symbol.</param>
/// <param name="compIds">List of specified symbol's component IDs to map over.</param>
/// <param name="model">Symbol model.</param>
/// <returns>Updated symbol models with mapping applied to selected symbols.</returns>
let mapSelectedSymsInModel (mapping: Symbol->Symbol) (compIds: List<ComponentId>) (model: SymbolT.Model): SymbolT.Model =
    getSymsFromIds compIds model
    |> List.map (fun sym -> mapping sym)
    |> List.fold (fun map sym -> Map.add sym.Id sym map) model.Symbols
    |> (fun updatedSymMap -> { model with Symbols = updatedSymMap })

/// <summary>Get a symbol's true dimension in vector format.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>Symbol dimension in <c>XYPos</c> vector format.</returns>
let getRotatedSymDim (sym: Symbol): XYPos =
    let h, w = getRotatedHAndW sym
    { X = w; Y = h }

/// <summary>Get the 4 outermost symbols by centre of a selected list.</summary>
/// <param name="syms">List of symbols to check.</param>
/// <returns>Tuple of symbols, in the order of: max x, min x, max y, min y.</returns>
let getMaxMinSymInBlock2D (syms: List<Symbol>): Symbol*Symbol*Symbol*Symbol =
    /// <summary>Helper to get the outermost symbols by centre of a selected list in 1 direction.</summary>
    /// <param name="dimSel">Dimension selector, either do <c>.X</c> or <c>.Y</c>.</param>
    /// <param name="syms">List of symbols to check.</param>
    /// <returns>Tuple of symbols: in the order of: max, min.</returns>
    let getMaxMinSymInBlock1D (dimSel: XYPos->float) (syms: List<Symbol>): Symbol*Symbol =
        let maxSym = syms |> List.maxBy (fun sym -> dimSel sym.Pos + dimSel (getRotatedSymDim sym))
        let minSym = syms |> List.minBy (fun sym -> dimSel sym.Pos)

        maxSym, minSym

    let maxXSym, minXSym = getMaxMinSymInBlock1D (fun vec -> vec.X) syms
    let maxYSym, minYSym = getMaxMinSymInBlock1D (fun vec -> vec.Y) syms

    maxXSym, minXSym, maxYSym, minYSym

/// <summary>Apply x- and y-mappings separately to tuple of x-y-max-min-symbols.
/// Used to obtain float values from the symbols.</summary>
/// <param name="xMapping">Mapping function for max and min in x-direction symbols.</param>
/// <param name="yMapping">Mapping function for max and min in y-direction symbols.</param>
/// <remarks>To be used in conjunction with <c>getMaxMinSymInBlock2D</c>.</remarks>
let mapMaxMinSyms2D
        (xMapping: Symbol->float)
        (yMapping: Symbol->float)
        (maxMinSyms: Symbol*Symbol*Symbol*Symbol)
            : float*float*float*float =
    let maxXSym, minXSym, maxYSym, minYSym = maxMinSyms

    xMapping maxXSym, xMapping minXSym, yMapping maxYSym, yMapping minYSym

/// <summary>Get the bounding box of a list of symbols.</summary>
/// <param name="syms">List of symbols.</param>
/// <returns>Bounding box of symbols.</returns>
/// <remarks>Author: Ismagilov, HLP23.</remarks>
let getBlockBBox (syms: List<Symbol>): BoundingBox =
    /// <summary>Helper to find new outermost position of a set of max min symbols in 1 dimension.</summary>
    /// <param name="dimSel">Function to access 1 dimension of <c>XYPos</c>.</param>
    let getMaxMinPos1D (dimSel: XYPos->float) (maxSym: Symbol) (minSym: Symbol): float*float =
        dimSel maxSym.Pos + dimSel (getRotatedSymDim maxSym), dimSel minSym.Pos

    let maxXSym, minXSym, maxYSym, minYSym = getMaxMinSymInBlock2D syms
    let maxX, minX = getMaxMinPos1D (fun pos -> pos.X) maxXSym minXSym
    let maxY, minY = getMaxMinPos1D (fun pos -> pos.Y) maxYSym minYSym

    { TopLeft = { X = minX; Y = minY }
      W = maxX - minX
      H = maxY - minY }


(* ------------------------------------------ Interface ----------------------------------------- *)

/// <summary>[Deprecated] Get list of symbols from list of ComponentIds. Use <c>getSymsFromIds</c> instead.</summary>
/// <param name="compList">List of component IDs to fetch.</param>
/// <param name="model">Symbol model.</param>
/// <returns>List of symbols.</returns>
/// <remarks>Interface to external codebase.</remarks>
let findSelectedSymbols (compList: List<ComponentId>) (model: SymbolT.Model): List<Symbol> =
    getSymsFromIds compList model

/// <summary>[Deprecated] Apply function to selected symbols within a symbol model. Use <c>mapSelectedSymsInModel</c> instead.</summary>
/// <param name="compList">List of selected component IDs to apply function to.</param>
/// <param name="model">Symbol model.</param>
/// <param name="selectedSymbols">[NOT USED] List of symbols to apply function to.</param>
/// <param name="modifySymbolFunc">Mapping function to transform symbol.</param>
/// <returns>Updated symbol models with mapping applied to selected symbols.</returns>
/// <remarks>Interface to external codebase.</remarks>
let groupNewSelectedSymsModel
        (compList: List<ComponentId>)
        (model: SymbolT.Model)
        (selectedSymbols: List<Symbol>)
        (modifySymbolFunc: Symbol->Symbol)
            : SymbolT.Model =
    mapSelectedSymsInModel modifySymbolFunc compList model


(* ---------------------------------------------------------------------------------------------- *)
(*                                         Port Operations                                        *)
(* ---------------------------------------------------------------------------------------------- *)

(* ----------------------------------------- Info Types ----------------------------------------- *)

/// <summary>Record type containing info to calculate the position of a port on the sheet.</summary>
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

/// <summary>Record type to note a wire between two symbols.</summary>
type WireSymbols =
    { SymA: Symbol
      SymB: Symbol
      Wire: Wire }


(* --------------------------------------- Implemetations --------------------------------------- *)

// TODO:
// This is mostly copy pasted code from Symbol.getPortPos, perhaps abstract out the existing code
// there to use makePortInfo. Could not simply use getPortPos because more data (side, topBottomGap,
// etc.) is needed to caclulate the new dimensions of the resized symbol.
/// <summary>Calculates layout information for a port within a symbol, including orientation and dimensions.</summary>
/// <param name="sym">Target symbol.</param>
/// <param name="port">Target port to calculate layout for.</param>
/// <returns><c>PortInfo</c> record with layout details based on symbol's orientation and size.</returns>
let makePortInfo (sym: Symbol) (port: Port): PortInfo =
    let side = getSymbolPortOrientation sym port
    let ports = sym.PortMaps.Order[side] // list of ports on the same side as port
    let gap = getPortPosEdgeGap sym.Component.Type
    let topBottomGap = gap + 0.3 // extra space for clk symbol
    let portDimension = float ports.Length - 1.
    let h, w = getRotatedHAndW sym
    let portGap =
        match side with
        | Left | Right -> h / (portDimension + 2.0 * gap)
        | Bottom | Top -> w / (portDimension + 2.0 * topBottomGap)

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

/// <summary>Gets the ports at either end of the connection, given a wire between two symbols.</summary>
/// <param name="wModel">Wire model.</param>
/// <param name="wireSyms">Record containing symbol-wire-symbol connection info, of type <c>WireSymbols</c>.</param>
/// <returns>Tuple of ports.</returns>
let getWirePorts (wModel: BusWireT.Model) (wireSyms: WireSymbols): Port*Port =
    let ports = portsOfWires wModel [ wireSyms.Wire ]
    let portA = filterPortBySym ports wireSyms.SymA |> List.head
    let portB = filterPortBySym ports wireSyms.SymB |> List.head
    portA, portB

/// <summary>Find two ports on opposite edges between two symbols.</summary>
/// <param name="wModel">Wire model.</param>
/// <param name="sym1">First symbol to check.</param>
/// <param name="sym2">Second symbol to check.</param>
/// <returns>An option type containing a tuple of PortInfo records for two ports on opposite edges if found, otherwise None.</returns>
let tryFindOppEdgePortInfo (wModel: BusWireT.Model) (sym1: Symbol) (sym2: Symbol): Option<PortInfo*PortInfo> =
    let tryGetOppEdgePorts (wireSyms: WireSymbols) =
        let portA, portB = getWirePorts wModel wireSyms
        let edgeA = getSymbolPortOrientation wireSyms.SymA portA
        let edgeB = getSymbolPortOrientation wireSyms.SymB portB

        match edgeA = edgeB.Opposite with
        | true -> Some(makePortInfo wireSyms.SymA portA, makePortInfo wireSyms.SymB portB)
        | _ -> None

    wiresBtwnSyms wModel sym2 sym1
    |> List.tryPick (fun w -> tryGetOppEdgePorts { SymA = sym2; SymB = sym1; Wire = w })

/// <summary>Finds the offset between two ports that leads to alignment.</summary>
/// <param name="refPInfo"><c>PortInfo</c> of port to reference.</param>
/// <param name="targetPInfo"><c>PortInfo</c> of port to move.</param>
/// <returns><c>XYPos</c> of port to move by.</returns>
let findPortAlignOffset (refPInfo: PortInfo) (targetPInfo: PortInfo) : XYPos =
    let getPortRealPos pInfo = getPortPos pInfo.sym pInfo.port + pInfo.sym.Pos

    let movePortPos = getPortRealPos targetPInfo
    let otherPortPos = getPortRealPos refPInfo
    let posDiff = otherPortPos - movePortPos

    match targetPInfo.side with
    | Top | Bottom -> { X = posDiff.X; Y = 0.0 }
    | Left | Right -> { X = 0.0; Y = posDiff.Y }


(* ---------------------------------------------------------------------------------------------- *)
(*                                        Symbol Operations                                       *)
(* ---------------------------------------------------------------------------------------------- *)

(* ----------------------------------------- Info Types ----------------------------------------- *)

/// <summary>Record type to note the number of connections on each edge of a symbol</summary>
type SymConnDataT = { ConnMap: Map<ComponentId*Edge,int> }


(* --------------------------------------- Implementation --------------------------------------- *)

/// <summary>Align a symbol with another reference symbol, if they are connected by wires on parallel edges.</summary>
/// <param name="wModel">Wire model of the sheet.</param>
/// <param name="refSym">Reference symbol to align to.</param>
/// <param name="targetSym">Target symbol to operate on.</param>
/// <returns>Updated wire model if aligned, ortherwise original model.</returns>
let alignSymToRefSym (wModel: BusWireT.Model) (refSym: Symbol) (targetSym: Symbol): BusWireT.Model =
    // Only attempt to align symbols if they are connected by ports on parallel edges.
    match tryFindOppEdgePortInfo wModel targetSym refSym with
    | None ->
        wModel
    | Some (targetPortInfo, refPortInfo) ->
        let offset = findPortAlignOffset targetPortInfo refPortInfo
        let symbol' = moveSymbol offset targetSym
        let model' = Optic.set (symbolOf_ targetSym.Id) symbol' wModel
        BusWireSeparate.routeAndSeparateSymbolWires model' targetSym.Id

/// <summary>Resizes a symbol with another reference symbol, if the target symbol is a <c>Custom</c> symbol.</summary>
/// <param name="wModel">Wire model of the sheet.</param>
/// <param name="refSym">Reference symbol to resize to.</param>
/// <param name="targetSym">Target symbol to operate on.</param>
/// <returns>Updated wire model if resized, ortherwise original model.</returns>
let resizeSymToRefSym (wModel: BusWireT.Model) (targetSym: Symbol) (refSym: Symbol): Symbol =
    let wires = wiresBtwnSyms wModel targetSym refSym

    // Try to get two ports that are on opposite edges, if none found just use any two ports.
    let resizePortInfo, otherPortInfo =
        match tryFindOppEdgePortInfo wModel targetSym refSym with
        | None ->
            let pA, pB = getWirePorts wModel { SymA = targetSym; SymB = refSym; Wire = wires[0] }
            makePortInfo targetSym pA, makePortInfo targetSym pB
        | Some(pIA, pIB) ->
            (pIA, pIB)

    let h, w =
        match resizePortInfo.side with
        | Left | Right ->
            otherPortInfo.portGap * (resizePortInfo.portDimension + 2.0 * resizePortInfo.gap), resizePortInfo.w
        | Top | Bottom ->
            resizePortInfo.h, otherPortInfo.portGap * (resizePortInfo.portDimension + 2.0 * resizePortInfo.topBottomGap)

    match targetSym.Component.Type with
    | Custom _ ->
        let scaledSymbol = setCustomCompHW h w targetSym
        let scaledInfo = makePortInfo scaledSymbol resizePortInfo.port
        let offset = findPortAlignOffset scaledInfo otherPortInfo
        moveSymbol offset scaledSymbol
    | _ ->
        targetSym

/// <summary>Top level function for UI to call <c>resizeSymToRefSym</c>.
/// Resizes a symbol with another reference symbol, if the target symbol is a <c>Custom</c> symbol.</summary>
/// <param name="wModel">Wire model of the sheet.</param>
/// <param name="refSym">Reference symbol to resize to.</param>
/// <param name="targetSym">Target symbol to operate on.</param>
/// <returns>Updated wire model if resized, ortherwise original model.</returns>
let resizeSymToRefSymTopLevel (wModel: BusWireT.Model) (targetSym: Symbol) (refSym: Symbol): BusWireT.Model =
    printfn $"resizeSymToRefSymTopLevel: target: {targetSym.Component.Label}, reference: {refSym.Component.Label}"

    let scaledSymbol = resizeSymToRefSym wModel targetSym refSym
    let model' = Optic.set (symbolOf_ targetSym.Id) scaledSymbol wModel

    BusWireSeparate.routeAndSeparateSymbolWires model' targetSym.Id

/// <summary>Find the <c>ComponentId</c> and <c>Edge</c> of the other symbol,
/// if a wire between a target symbol and another symbol connects opposite edges</summary>
/// <param name="wModel">Wire model of the sheet.</param>
/// <param name="sym">Target symbol.</param>
/// <param name="wire">Target wire.</param>
let tryFindWireSymOppEdgeInfo (wModel: BusWireT.Model) (sym: Symbol) (wire: Wire): Option<ComponentId*Edge> =
    let tryFindWireSymOppEdge (otherSym: Symbol): Option<Edge> =
        let symEdge, otherSymEdge =
            wireSymEdge wModel wire sym, wireSymEdge wModel wire otherSym

        match symEdge = otherSymEdge.Opposite with
        | true -> Some symEdge
        | _ -> None

    let symS, symT =
        getSourceSymbol wModel wire, getTargetSymbol wModel wire

    let otherSymbol =
        match symS, symT with
        | _ when (symS.Id <> sym.Id) && (symT.Id = sym.Id) -> Some symS
        | _ when (symS = sym) && (symT <> sym) -> Some symT
        | _ -> None

    let edge =
        otherSymbol
        |> Option.bind tryFindWireSymOppEdge

    match otherSymbol, edge with
    | Some otherSym, Some edge -> Some (otherSym.Id, edge)
    | _ -> None

/// <summary>Finds the optimal size and position for the selected symbol with respect to its surrounding symbols.</summary>
/// <param name="wModel">Wire model of the sheet.</param>
/// <param name="bboxMap">Map of bounding box of the whole sheet.</param>
/// <param name="sym">Target symbol to optimise.</param>
/// <returns>Updated wire model if optimised, else original model.</returns>
let optimiseSym (wModel: BusWireT.Model) (bboxMap: Map<ComponentId,BoundingBox>) (sym: Symbol): BusWireT.Model =
    /// <summary>Increment counnection count for given <c>componentId</c> and <c>edge</c>.
    /// If there are no connections, value will be set to 1.</summary>
    let incrementSymConnData (symConnData: SymConnDataT) (cId: ComponentId, edge: Edge): SymConnDataT =
        let map = symConnData.ConnMap
        let count = Map.tryFind (cId, edge) map |> Option.defaultValue 0 |> (+) 1
        { ConnMap = Map.add (cId, edge) count map }

    let symConnData =
        wModel.Wires
        |> Map.values
        |> List.ofSeq
        |> List.map (tryFindWireSymOppEdgeInfo wModel sym)
        |> List.filter Option.isSome
        |> List.map Option.get
        |> List.fold incrementSymConnData { ConnMap = Map.empty }

    // TODO:
    // This is copied from Sheet.notIntersectingComponents.
    // It requires SheetT.Model, which is not accessible from here. Maybe refactor it.
    /// <summary>Checks if there is no overlap to anything within <c>bboxMap</c>.</summary>
    let isNoSymOverlapBBoxMap (bboxMap: Map<ComponentId,BoundingBox>) (sym: Symbol): bool =
        let symBbox = getSymbolBoundingBox sym

        bboxMap
        |> Map.exists (fun symId bbox -> DrawHelpers.boxesIntersect bbox symBbox && sym.Id <> symId)
        |> not

    /// <summary>Resizes symbol, if possible.</summary>
    let resize (sym: Symbol) (symCount: array<((ComponentId*Edge)*int)>): Symbol =

        let alignSym (sym: Symbol) (otherSym: Symbol) =
            let resizedSym = resizeSymToRefSym wModel sym otherSym
            let noOverlap = isNoSymOverlapBBoxMap bboxMap resizedSym

            match noOverlap with
            | true -> true, resizedSym
            | _ -> false, sym

        let folder (hAligned, vAligned, sym) ((cid, edge), _) =
            let otherSym = Optic.get (symbolOf_ cid) wModel

            let isAligned, newSym = alignSym sym otherSym

            match hAligned, vAligned with
            | false, _ when edge = Top || edge = Bottom ->
                (isAligned, vAligned, newSym)
            | _, false when edge = Left || edge = Right ->
                (hAligned, isAligned, newSym)
            | _ -> (hAligned, vAligned, sym)

        let (_, _, sym') = ((false, false, sym), symCount) ||> Array.fold folder

        sym'

    let scaledSymbol =
        Map.toArray symConnData.ConnMap
        |> Array.filter (fun (_, count) -> count > 1)
        |> Array.sortByDescending snd
        |> resize sym

    let model' =
        Optic.set (symbolOf_ sym.Id) scaledSymbol wModel

    BusWireSeparate.routeAndSeparateSymbolWires model' sym.Id


(* ---------------------------------------------------------------------------------------------- *)
(*                                             Rotate                                             *)
(* ---------------------------------------------------------------------------------------------- *)

/// <summary>Rotate a point with respect to a centre point.</summary>
/// <param name="degree">Degree of rotation.</param>
/// <param name="centre">Centre point to flipped about.</param>
/// <param name="point">Point to rotate.</param>
/// <returns>Position of rotate point.</returns>
/// <remarks>Author: Ismagilov, HLP23.</remarks>
let rotatePointAboutBlockCentre (rotation: Rotation) (centre: XYPos) (point: XYPos): XYPos =
    point
    |> (fun pt -> pt - centre) // obtain vector from centre to point
    |> (fun vec ->
        match rotation with
        | Degree0 -> vec
        | Degree90 -> { X = vec.Y; Y = -vec.X }
        | Degree180 -> { X = -vec.X; Y = -vec.Y }
        | Degree270 -> { X = -vec.Y; Y = vec.X }) // rotate
    |> (fun vec -> centre - vec) // convert vector back to pos

/// <summary>Get top-left corner of a symbol after it has been rotated.</summary>
/// <param name="degree">Degree of rotation of symbol.</param>
/// <param name="h">Original height of symbol.</param>
/// <param name="w">Original width of symbol.</param>
/// <param name="pos">Original top-left corner.</param>
/// <returns>New top-left corner after rotation.</returns>
/// <remarks>Author: Ismagilov, HLP23.</remarks>
let adjustPosForBlockRotation (rotation: Rotation) (h: float) (w: float) (pos: XYPos): XYPos =
    match rotation with
    | Degree0 -> { X = 0.; Y = 0. }
    | Degree90 -> { X = h; Y = 0. }
    | Degree180 -> { X = w; Y = -h }
    | Degree270 -> { X = 0.; Y = w }
    |> (-) pos

/// <summary>Rotate a symbol in its selection block.</summary>
/// <param name="degree">Degree of rotation of symbol.</param>
/// <param name="blockBBox">Bounding box of selection block.</param>
/// <param name="sym">Symbol to be rotated.</param>
/// <returns>Updated symbol after rotated about block centre.</returns>
/// <remarks>Author: Ismagilov, HLP23.</remarks>
let rotateSymInBlock (degree: Rotation) (blockBBox: BoundingBox) (sym: Symbol): Symbol =
    let rotatedH, rotatedW =
        getRotatedHAndW sym

    let newTopLeft =
        rotatePointAboutBlockCentre (invertRotation degree) (blockBBox.Centre()) sym.Pos
        |> adjustPosForBlockRotation (invertRotation degree) rotatedH rotatedW

    let newComponent =
        { sym.Component with X = newTopLeft.X; Y = newTopLeft.Y }

    let newSTransform =
        match sym.STransform.Flipped with
        | true ->
            { sym.STransform with Rotation = combineRotation (invertRotation degree) sym.STransform.Rotation }
        | _->
            { sym.STransform with Rotation = combineRotation degree sym.STransform.Rotation }

    let newPortMaps =
        rotatePortInfo degree sym.PortMaps

    { sym with
        Pos = newTopLeft
        Component = newComponent
        STransform = newSTransform
        PortMaps = newPortMaps
        LabelHasDefaultPos = true
    }
    |> calcLabelBoundingBox

/// <summary>Rotates a symbol on its own.</summary>
/// <param name="degree">Degree to rotate symbol by.</param>
/// <param name="sym">Symbol to be rotated.</param>
/// <remarks>Author: Klapper, HLP23.</remarks>
let rotateSym (degree: Rotation) (sym: Symbol): Symbol =
    rotateSymInBlock degree (getBlockBBox [ sym ]) sym

/// <summary>Rotates a block of symbols.</summary>
/// <param name="degree">Degree to rotate block by.</param>
/// <param name="compIds">List of ComponentIds of block components.</param>
/// <param name="model">Target symbol model.</param>
/// <returns>Returns updated symbol model, of type <c>SymbolT.Model</c>.</returns>
/// <remarks>Refactored top-level function. Apostrophe to be removed after codebase is refactored.</remarks>
let rotateBlock' (degree: Rotation) (compIds: List<ComponentId>) (model: SymbolT.Model): SymbolT.Model =
    let selectedSymsBBox = getSymsFromIds compIds model |> getBlockBBox

    mapSelectedSymsInModel (rotateSymInBlock (invertRotation degree) selectedSymsBBox) compIds model


(* ------------------------------------------ Interface ----------------------------------------- *)

/// <summary>[Deprecated] Rotates a block of symbols. Use <c>rotateBlock'</c> instead.</summary>
/// <param name="compList">List of <c>ComponentId</c>'s of selected components.</param>
/// <param name="model">Current symbol model.</param>
/// <param name="rotation">Type of rotation to do.</param>
/// <returns>Returns updated symbol model, of type <c>SymbolT.Model</c>.</returns>
/// <remarks>Interface to existing codebase.</remarks>
let rotateBlock (compList: List<ComponentId>) (model: SymbolT.Model) (rotation:Rotation): SymbolT.Model =
    rotateBlock' (rotation) (compList) (model)


(* ---------------------------------------------------------------------------------------------- *)
(*                                              Flip                                              *)
(* ---------------------------------------------------------------------------------------------- *)

/// <summary>Flip a point with respect to a centre point.</summary>
/// <param name="flip">Flip type, either <c>FlipHorizontal</c> or <c>FlipVertical</c>.</param>
/// <param name="centre">Centre point to flipped about.</param>
/// <param name="point">Point to flip.</param>
/// <returns>Position of flipped point.</returns>
/// <remarks>Author: Ismagilov, HLP23.</remarks>
let flipPointAboutBlockCentre (flip: FlipType) (centre: XYPos) (point: XYPos) : XYPos =
    match flip with
    | FlipHorizontal -> { X = centre.X - (point.X - centre.X); Y = point.Y }
    | FlipVertical -> { X = point.X; Y = centre.Y - (point.Y - centre.Y) }

/// <summary>Get top-left corner of a symbol after it has been flipped.</summary>
/// <param name="flip">Flip type, either <c>FlipHorizontal</c> or <c>FlipVertical</c>.</param>
/// <param name="h">Original height of symbol.</param>
/// <param name="w">Original width of symbol.</param>
/// <param name="pos">Original top-left corner.</param>
/// <returns>Position of top-left corner after flip.</returns>
/// <remarks>Author: Ismagilov, HLP23.</remarks>
let adjustPosForBlockFlip (flip:FlipType) (h: float) (w:float) (pos: XYPos): XYPos =
    match flip with
    | FlipHorizontal -> { X = w; Y = 0. }
    | FlipVertical -> { X = 0.; Y = h }
    |> (-) pos

/// <summary>Flip a symbol in a selection block.</summary>
/// <param name="flip">Flip type, either <c>FlipHorizontal</c> or <c>FlipVertical</c>.</param>
/// <param name="blockBBox">Bounding box of selection block.</param>
/// <param name="sym">Symbol to be flipped.</param>
/// <returns>Updated symbol after flipped about block centre.</returns>
/// <remarks>Author: Ismagilov, HLP23.</remarks>
let flipSymInBlock (flip: FlipType) (blockBBox: BoundingBox) (sym: Symbol): Symbol =
    let rotatedH, rotatedW = // h and w not updated during normal rotate
        getRotatedHAndW sym

    let newTopLeft = // update pos of block
        flipPointAboutBlockCentre flip (blockBBox.Centre()) sym.Pos
        |> adjustPosForBlockFlip flip rotatedH rotatedW

    let newPortOrientation =
        sym.PortMaps.Orientation
        |> Map.map (fun _ side -> flipSideHorizontal side)

    let newPortOrder =
        sym.PortMaps.Order
        |> Map.toList
        |> List.map (fun (side, _) -> side) // get all sides
        |> List.fold
            (fun currPortOrder side -> Map.add (flipSideHorizontal side) sym.PortMaps.Order[side] currPortOrder)
            Map.empty
        |> Map.map (fun _ order -> List.rev order)

    { sym with
        Component = { sym.Component with X = newTopLeft.X; Y = newTopLeft.Y }
        PortMaps = { Order = newPortOrder; Orientation = newPortOrientation }
        STransform = { sym.STransform with Flipped = not sym.STransform.Flipped }
        LabelHasDefaultPos = true
        Pos = newTopLeft
    }
    |> calcLabelBoundingBox
    |> (fun sym ->
        match flip with
        | FlipHorizontal ->
            sym
        | FlipVertical ->
            let symBBox = getBlockBBox [ sym ]
            sym
            |> rotateSymInBlock Degree270 symBBox
            |> rotateSymInBlock Degree270 symBBox)

/// <summary>Flips a block of symbols.</summary>
/// <param name="flip">Type of flip to do.</param>
/// <param name="compIds">List of <c>ComponentId</c>s of block components.</param>
/// <param name="model">Target symbol model.</param>
/// <returns>Returns updated symbol model, of type <c>SymbolT.Model</c>.</returns>
/// <remarks>Refactored top-level function. Apostrophe to be removed after codebase is refactored.</remarks>
let flipBlock' (flip: FlipType) (compIds :ComponentId list) (model:SymbolT.Model): SymbolT.Model =
    let selectedSymsBBox = getSymsFromIds compIds model |> getBlockBBox

    mapSelectedSymsInModel (flipSymInBlock flip selectedSymsBBox) compIds model


(* ------------------------------------------ Interface ----------------------------------------- *)

/// <summary>[Deprecated] Flips a block of symbols. Use <c>flipBlock'</c> instead.</summary>
/// <param name="compList">List of <c>ComponentId</c>s of block components.</param>
/// <param name="model">Target symbol model.</param>
/// <param name="flip">Type of flip to do.</param>
/// <returns>Returns updated symbol model, of type <c>SymbolT.Model</c>.</returns>
/// <remarks>Interface to existing codebase. Author: Ismagilov, HLP23.</remarks>
let flipBlock (compList: ComponentId list) (model: SymbolT.Model) (flip: FlipType) =
    flipBlock' flip compList model


(* ---------------------------------------------------------------------------------------------- *)
(*                                              Scale                                             *)
(* ---------------------------------------------------------------------------------------------- *)

/// <summary>Scales a symbol up or down in a selection box.</summary>
/// <param name="scale"><c>ScaleUp</c> or <c>ScaleDown</c>.</param>
/// <param name="blockBBox">Bounding box of selected components.</param>
/// <param name="sym">Symbol to be scaled.</param>
/// <returns>Updated symbol after scaled about block centre.</returns>
/// <remarks>Scaling distance is constant. Author: Ismagilov, HLP23.</remarks>
let scaleSymInBlock (scale: ScaleType) (blockBBox: BoundingBox) (sym: Symbol): Symbol =
    // scaling distant constants
    let blockPosDiff = 5.
    let blockSideDiff = 10.

    // scaling info in XYPos vector format
    let rotatedCentre = getRotatedSymbolCentre sym
    let rotatedHW = getRotatedSymDim sym
    let blockPos = blockBBox.TopLeft
    let blockHW = { X = blockBBox.W; Y = blockBBox.H }

    /// <summary>Helper to find new top-left position of a symbol of 1 dimension.</summary>
    /// <param name="dimSel">Function to access 1 dimension of <c>XYPos</c>.</param>
    let findNewPos1D (dimSel: XYPos->float): float =
        let prop = // proportion of symbol to block
            (dimSel rotatedCentre) - (dimSel blockPos) / (dimSel blockHW)
        let centre = // centre of symbol
            match scale with
            | ScaleUp -> (dimSel blockPos - blockPosDiff) + (dimSel blockHW + blockSideDiff) * prop
            | ScaleDown -> (dimSel blockPos + blockPosDiff) + (dimSel blockHW - blockSideDiff) * prop
        let pos = // top-left pos of symbol
            centre - (dimSel rotatedHW)/2.

        pos

    let newPosX = findNewPos1D (fun vec -> vec.X)
    let newPosY = findNewPos1D (fun vec -> vec.Y)

    { sym with
        Pos = { X = newPosX; Y = newPosY }
        Component = { sym.Component with X = newPosX; Y = newPosY }
        LabelHasDefaultPos = true
    }

/// <summary>Find scaling coefficients for 1 dimension.</summary>
/// <param name="min">Min of original range.</param>
/// <param name="matchMin">Min of range to scale to.</param>
/// <param name="max">Max of original range.</param>
/// <param name="matchMax">Max of range to scale to.</param>
/// <returns>Tuple of coefficients, in the order of: scaling factor and offset centre.</returns>
/// <remarks>No original docstring, educated guess used.</remarks>
let getScalingCoeffs1D (min: float) (matchMin: float) (max: float) (matchMax: float): float*float =
    let scaleFact =
        if min = max || matchMax <= matchMin
        then 1.
        else (matchMin - matchMax) / (min - max)

    let offsetCentre =
        if scaleFact = 1. then 0.
        else (matchMin - min * scaleFact) / (1.-scaleFact)

    (scaleFact, offsetCentre)

/// <summary>Return set of floats that define how a group of components is scaled.</summary>
/// <param name="matchBBoxMin">Min corner of bounding box to scale to.</param>
/// <param name="matchBBoxMax">Max corner of bounding box to scale to.</param>
/// <returns>Tuple of tuple of scaling coefficients, in the order of x-dimension scaling factor and offset centre,
/// and then the y-dimension.</returns>
/// <remarks>Original docstring not complete, educated guess used for params.</remarks>
let getScalingCoeffsGroup
        (matchBBoxMin: XYPos)
        (matchBBoxMax: XYPos)
        (selectedSyms: List<Symbol>)
            : ((float*float)*(float*float)) =

    let maxXSym, minXSym, maxYSym, minYSym =
        getMaxMinSymInBlock2D selectedSyms

    let oldMaxX, oldMinX, oldMaxY, oldMinY =
        mapMaxMinSyms2D
            (fun xSym -> (getRotatedSymbolCentre xSym).X)
            (fun ySym -> (getRotatedSymbolCentre ySym).Y)
            (maxXSym, minXSym, maxYSym, minYSym)

    let newMaxX = matchBBoxMax.X - ((getRotatedSymDim maxXSym).X)/2.
    let newMinX = matchBBoxMin.X + ((getRotatedSymDim minXSym).X)/2.
    let newMaxY = matchBBoxMax.Y - ((getRotatedSymDim maxYSym).Y)/2.
    let newMinY = matchBBoxMin.Y + ((getRotatedSymDim minYSym).Y)/2.

    let xScalingCoeffs = getScalingCoeffs1D oldMinX newMinX oldMaxX newMaxX
    let yScalingCoeffs = getScalingCoeffs1D oldMinY newMinY oldMaxY newMaxY

    (xScalingCoeffs, yScalingCoeffs)

/// <summary>Alter the position of a symbol as needed in a scaling operation.</summary>
/// <param name="xyScalingCoeffs">Scaling coefficients, in the form of the output of <c>getScalingCoeffsGroup</c>.</param>
/// <param name="sym">Target symbol.</param>
/// <returns>Updated symbol its position is updated.</returns>
/// <remarks>Original docstring not complete, educated guess used for params.</remarks>
let scaleSymbolPos (xyScalingCoeffs: (float*float)*(float*float)) (sym: Symbol): Symbol =
    let symCentre = getRotatedSymbolCentre sym
    let xScalingCoeffs, yScalingCoeffs = xyScalingCoeffs

    /// <summary>Helper function to update coordinate based on scaling coefficients.</summary>
    let translateFunc (scalingCoeff: float*float) (coordinate: float): float =
        let scaleFact, offsetCentre = scalingCoeff
        (coordinate - offsetCentre) * scaleFact + offsetCentre

    let newX = translateFunc xScalingCoeffs symCentre.X
    let newY = translateFunc yScalingCoeffs symCentre.Y

    let newPos = { X = newX; Y = newY } - (getRotatedSymDim sym) * (1./2.)
    let newComponent = { sym.Component with X = newPos.X; Y = newPos.Y }

    { sym with
        Pos = newPos
        Component = newComponent
        LabelHasDefaultPos = true
    }


(* ------------------------------------------ Interface ----------------------------------------- *)

/// <summary>[Deprecated] Return set of floats that define how a group of components is scaled.
/// Use <c>getScalingCoeffsGroup</c> instead.</summary>
/// <param name="matchBBMin">Min corner of bounding box to scale to.</param>
/// <param name="matchBBMax">Max corner of bounding box to scale to.</param>
/// <returns>Tuple of tuple of scaling coefficients, in the order of x-dimension scaling factor and offset centre,
/// and then the y-dimension.</returns>
/// <remarks>Interface to existing codebase. Original docstring not complete, educated guess used for params.</remarks>
let getScalingFactorAndOffsetCentreGroup
        (matchBBMin: XYPos)
        (matchBBMax: XYPos)
        (selectedSyms: List<Symbol>)
            : ((float*float)*(float*float)) =
    getScalingCoeffsGroup matchBBMin matchBBMax selectedSyms

/// <summary>[Deprecated] Alter the position of a symbol as needed in a scaling operation.
/// Use <c>scaleSymbolPos</c> instead.</summary>
/// <param name="xYSC">Scaling coefficients, in the form of the output of <c>getScalingCoeffsGroup</c>.</param>
/// <param name="sym">Target symbol.</param>
/// <returns>Updated symbol its position is updated.</returns>
/// <remarks>Interface to existing codebase.
/// Original docstring not complete, educated guess used for params.</remarks>
let scaleSymbol
        (xYSC: ((float*float)*(float*float)))
        (sym: Symbol)
            : Symbol =
    scaleSymbolPos xYSC sym


(* ---------------------------------------------------------------------------------------------- *)
(*                              Update Scaling Box (Not Refactor-ed)                              *)
(* ---------------------------------------------------------------------------------------------- *)

/// <summary>Updates the model and commands after the scaling box has been changed.</summary>
/// <param name="model">Current sheet model.</param>
/// <param name="cmd">Current command list.</param>
/// <returns>Tuple of updated sheet model and updated command list</returns>
/// <remarks>
/// * _Less than two selected components and no scaling box_: returns the original model and the original command list.
/// * _Less than two selected components and a scaling box_: removes the scaling box and updates the
/// bounding boxes, returning the updated model and the combined command list with the deletion and update commands.
/// * _Two or more selected components_: checks if the scaling box bound matches the current selected
/// components' symbols' bounding boxes. Updates the symbols and the scaling box, deleting the old
/// buttons and symbols, creating new ones, and setting the new scaling box. Returns the updated
/// model, the updated command list with the deletion, creation, and update commands, and the new command list.
/// </remarks>
let postUpdateScalingBox (model: SheetT.Model, cmd) =
    let symbolCmd (msg: SymbolT.Msg) = Elmish.Cmd.ofMsg (ModelType.Msg.Sheet (SheetT.Wire (BusWireT.Symbol msg)))
    let sheetCmd (msg: SheetT.Msg) = Elmish.Cmd.ofMsg (ModelType.Msg.Sheet msg)

    let updatedSymbols (updatedBoundingBox: BoundingBox) =
        let topleft = updatedBoundingBox.TopLeft
        let rotateDeg90OffSet: XYPos = {X = updatedBoundingBox.W+57.; Y = (updatedBoundingBox.H/2.)-12.5}
        let rotateDeg270OffSet: XYPos = {X = -69.5; Y = (updatedBoundingBox.H/2.)-12.5}
        let buttonOffSet: XYPos = {X = updatedBoundingBox.W + 47.5; Y = -47.5}
        let dummyPos = (topleft + buttonOffSet)

        let makeButton = SymbolUpdate.createAnnotation ThemeType.Colourful
        let buttonSym = {makeButton ScaleButton dummyPos with Pos = (topleft + buttonOffSet)}
        let makeRotateSym sym = {sym with Component = {sym.Component with H = 25.; W=25.}}
        let rotatedSymbol = [makeButton (RotateButton Degree90) (topleft + rotateDeg90OffSet);
                                {makeButton (RotateButton Degree270) (topleft + rotateDeg270OffSet) with SymbolT.STransform = {Rotation=Degree90 ; Flipped=false}}]
                                |> List.map makeRotateSym
        let rotateDeg90Sym, rotateDeg270Sym = rotatedSymbol.[0], rotatedSymbol.[1]

        buttonSym, rotateDeg90Sym, rotateDeg270Sym

    let updatedMapAndBox (newBoxBound : BoundingBox) =
        let buttonSym, rotate90Sym, rotate270Sym = updatedSymbols newBoxBound
        let newSymbolMap =
            model.Wire.Symbol.Symbols
            |> Map.add buttonSym.Id buttonSym
            |> Map.add rotate270Sym.Id rotate270Sym
            |> Map.add rotate90Sym.Id rotate90Sym

        let initScalingBox : SheetT.ScalingBox = {
            ScalingBoxBound = newBoxBound;
            ScaleButton = buttonSym;
            RotateDeg90Button = rotate90Sym;
            RotateDeg270Button = rotate270Sym;
            ButtonList = [buttonSym.Id; rotate270Sym.Id; rotate90Sym.Id];
            }

        newSymbolMap, initScalingBox

    match model.SelectedComponents.Length, model.ScalingBox with
    | count, None when count < 2 ->
        model, cmd
    | count, _ when count < 2 ->
        { model with ScalingBox = None },
            [symbolCmd (SymbolT.DeleteSymbols (model.ScalingBox.Value).ButtonList);
            sheetCmd SheetT.UpdateBoundingBoxes]
            |> List.append [cmd]
            |> Elmish.Cmd.batch
    | _ ->
        let newBoxBound =
            model.SelectedComponents
            |> List.map (fun id -> Map.find id model.Wire.Symbol.Symbols)
            |> getBlockBBox

        match model.ScalingBox with
        | Some value when value.ScalingBoxBound = newBoxBound ->
            model, cmd
        | _ ->
            let updateSymbolMap, updatedScalingBox = updatedMapAndBox newBoxBound
            let newCmd =
                match model.ScalingBox with
                | Some _ ->
                    [symbolCmd (SymbolT.DeleteSymbols (model.ScalingBox.Value).ButtonList);
                    sheetCmd SheetT.UpdateBoundingBoxes]
                    |> List.append [cmd]
                    |> Elmish.Cmd.batch
                | None ->
                    cmd

            model
            |> Optic.set SheetT.scalingBox_ (Some updatedScalingBox)
            |> Optic.set SheetT.symbols_ updateSymbolMap,
            newCmd


(* ---------------------------------------------------------------------------------------------- *)
(*                                              Misc                                              *)
(* ---------------------------------------------------------------------------------------------- *)

/// <summary>Checks whether a symbol spans across a list of selected symbols in either dimension.</summary>
/// <param name="selectedSyms">List of symbols to check.</param>
/// <returns>True if a symbol bounds both edges.</returns>
let oneCompBoundsBothEdges (selectedSyms: List<Symbol>): bool =
    let maxXCentreX, minXCentreX, maxYCentreY, minYCentreY =
        mapMaxMinSyms2D
            (fun xSym -> (getRotatedSymbolCentre xSym).X)
            (fun ySym -> (getRotatedSymbolCentre ySym).Y)
            (getMaxMinSymInBlock2D selectedSyms)

    maxXCentreX = minXCentreX || maxYCentreY = minYCentreY
