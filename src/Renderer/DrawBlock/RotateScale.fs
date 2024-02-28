module RotateScale

(*

    Contribution Statement - Samuel Wang - sw2521

    Three team members were assigned to the same section. I was assigned the later section of the file, from lines 370 
    to the end of the file. The whole section was improved and refactored, with the exception of the postUpdateScalingBox
    function, with the majority of the work being: a) reordering functions, b) extracting helper functions, and c) 
    updating function signals. See below for a comprehensive list of improvements.
    
    Major Improvements - 

    1.  Reordered and sectioned top-level function by subject. 
        The following categories were created: Helpers, Rotate, Flip, Scaling, Update Scaling Box, and Misc. This was 
        done to allow helper functions to be more easily extracted (easier to see duplicate code in similar functinos) 
        and to allow library functions to be more easily found.
    
    2.  Extracted duplicate code into helper functions. See extracted function and uses below:
        a.  getSymsFromIds, mapSelectedSymsInModel: 
            extracted from rotateBlock and flipBlock, which is of same functionality of findSelectedSymbols and 
            groupNewSelectedSymsModel.
        b.  getMaxMinSymInBlock2D, mapMaxMinSyms2D: 
            extracted from getScalingFactorAndOffsetCentreGroup and oneCompBoundsBothEdges, which also reduces duplicate 
            code across x- and y-dimensions.
        c. getRotatedSymDim: 
            function to map symbol's true height and width to a XYPos vector. Used across scaling functions.
    
    3.  Renamed top-level functions and updated its signal. 
        Renaming was done for function names to be shorter and representative of what they do. Signals were updated for 
        the function to be curried more easily, usually in the form of putting the Symbol or the SymbolT.Model it is 
        operating on last, e.g. rotateBlock. Signals were also unified across similar functions, e.g. rotateSymInBlock, 
        flipSymInBlock, and scaleSymInBlock, allowing easier use. Interface functions were created when functions were 
        used by external code, where parameter names were kept the same but docs updated.

    4.  Removed unsafe and unnecessary code in rotateBlock and flipBlock. 
        The original code first obtains a list of selected symbols and a map of unselected symbols, then updates the 
        selected symbol by adding back to the map of unselected symbols. The first operation was unsafe as it uses 
        Map.find, a implementation with Map.tryFind is better. The second operation was unnecessary as Map.add updates 
        a map's value if a key was present, a simpler implementation to just add in the updated symbols in the symbol 
        map was given in the mapSelectedSymsInModel helper.

    5.  Used XYPos to represent vector data. 
        This was done as the ".X" and ".W" accesses can be parameterized (with just ".X"), which allowed for reduction 
        of repeated code. This was also done so that the XYPos overloaded operator can be used to simplify operations. 
        E.g. scaleSymbolPos.
    
    Minor Fixes -

    6.  Added comprehensive XML documentation to all functions refactored. 
        This included summary, parameters, and its returns. Some complicated local helpers have doc-strings as well.

    7.  Renamed function, variable, and parameter names for them to be more succinct or obvious. See below:
        a. Reduce: Symbol -> Sym; ScalingFactorAndOffsetCentre -> ScalingCoeffs
        b. Clarify: compList -> compIds; Block -> BlockBBox; BB -> BBox; OffsetC -> OffsetCentre

    (Content: Ln 41, Col 120)
*)

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

(* 
    This module contains the code that rotates and scales blocks of components.
    It was collected from HLP work in 2023 and has some technical debt and also unused functions.
    It requires better documentation of teh pasrts now used.
*)

/// Record containing all the information required to calculate the position of a port on the sheet.
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

/// Type used to record a wire between two symbols.
type WireSymbols =
    { SymA: Symbol
      SymB: Symbol
      Wire: Wire }

/// TODO: this is mostly copy pasted code from Symbol.getPortPos, perhaps abstract out the existing code there to use makePortInfo.
/// Could not simply use getPortPos because more data (side, topBottomGap, etc.) is needed to caclulate the new dimensions of the resized symbol.
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

let getPortAB wModel wireSyms =
    let ports = portsOfWires wModel [ wireSyms.Wire ]
    let portA = filterPortBySym ports wireSyms.SymA |> List.head
    let portB = filterPortBySym ports wireSyms.SymB |> List.head
    portA, portB

/// Try to get two ports that are on opposite edges.
let getOppEdgePortInfo
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    : (PortInfo * PortInfo) option =
    let wires = wiresBtwnSyms wModel symbolToSize otherSymbol

    let tryGetOppEdgePorts wireSyms =
        let portA, portB = getPortAB wModel wireSyms
        let edgeA = getSymbolPortOrientation wireSyms.SymA portA
        let edgeB = getSymbolPortOrientation wireSyms.SymB portB

        match edgeA = edgeB.Opposite with
        | true -> Some(makePortInfo wireSyms.SymA portA, makePortInfo wireSyms.SymB portB)
        | _ -> None

    wires
    |> List.tryPick (fun w ->
        tryGetOppEdgePorts
            { SymA = symbolToSize
              SymB = otherSymbol
              Wire = w })

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
    : BusWireT.Model =

    // Only attempt to align symbols if they are connected by ports on parallel edges.
    match getOppEdgePortInfo (wModel:BusWireT.Model) symbolToSize otherSymbol with
    | None -> wModel
    | Some(movePortInfo, otherPortInfo) ->
        let offset = alignPortsOffset movePortInfo otherPortInfo
        let symbol' = moveSymbol offset symbolToSize
        let model' = Optic.set (symbolOf_ symbolToSize.Id) symbol' wModel
        BusWireSeparate.routeAndSeparateSymbolWires model' symbolToSize.Id

/// HLP23: To test this, it must be given two symbols interconnected by wires. It then resizes symbolToSize
/// so that the connecting wires are exactly straight
/// HLP23: It should work out the interconnecting wires (wires) from
/// the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has adjustable size).
let reSizeSymbol (wModel: BusWireT.Model) (symbolToSize: Symbol) (otherSymbol: Symbol) : (Symbol) =
    let wires = wiresBtwnSyms wModel symbolToSize otherSymbol

    // Try to get two ports that are on opposite edges, if none found just use any two ports.
    let resizePortInfo, otherPortInfo =
        match getOppEdgePortInfo wModel symbolToSize otherSymbol with
        | None ->
            let pA, pB = getPortAB wModel { SymA = symbolToSize; SymB = otherSymbol; Wire = wires[0] }
            makePortInfo symbolToSize pA, makePortInfo symbolToSize pB
        | Some(pIA, pIB) -> (pIA, pIB)

    let h, w =
        match resizePortInfo.side with
        | Left | Right ->
            otherPortInfo.portGap * (resizePortInfo.portDimension + 2.0 * resizePortInfo.gap), resizePortInfo.w
        | Top | Bottom ->
            resizePortInfo.h, otherPortInfo.portGap * (resizePortInfo.portDimension + 2.0 * resizePortInfo.topBottomGap)

    match symbolToSize.Component.Type with
    | Custom _ ->
        let scaledSymbol = setCustomCompHW h w symbolToSize
        let scaledInfo = makePortInfo scaledSymbol resizePortInfo.port
        let offset = alignPortsOffset scaledInfo otherPortInfo
        moveSymbol offset scaledSymbol
    | _ ->
        symbolToSize

/// For UI to call ResizeSymbol.
let reSizeSymbolTopLevel
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    : BusWireT.Model =
    printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"

    let scaledSymbol = reSizeSymbol wModel symbolToSize otherSymbol

    let model' = Optic.set (symbolOf_ symbolToSize.Id) scaledSymbol wModel
    BusWireSeparate.routeAndSeparateSymbolWires model' symbolToSize.Id

/// For each edge of the symbol, store a count of how many connections it has to other symbols.
type SymConnDataT =
    { ConnMap: Map<ComponentId * Edge, int> }

/// If a wire between a target symbol and another symbol connects opposite edges, return the edge that the wire is connected to on the target symbol 
let tryWireSymOppEdge (wModel: Model) (wire: Wire) (sym: Symbol) (otherSym: Symbol) =
    let symEdge = wireSymEdge wModel wire sym
    let otherSymEdge = wireSymEdge wModel wire otherSym

    match symEdge = otherSymEdge.Opposite with
    | true -> Some symEdge
    | _ -> None

let updateOrInsert (symConnData: SymConnDataT) (edge: Edge) (cid: ComponentId) =
    let m = symConnData.ConnMap
    let count = Map.tryFind (cid, edge) m |> Option.defaultValue 0 |> (+) 1
    { ConnMap = Map.add (cid, edge) count m }

// TODO: this is copied from Sheet.notIntersectingComponents. It requires SheetT.Model, which is not accessible from here. Maybe refactor it.
let noSymbolOverlap (boxesIntersect: BoundingBox -> BoundingBox -> bool) boundingBoxes sym =
    let symBB = getSymbolBoundingBox sym

    boundingBoxes
    |> Map.filter (fun sId boundingBox -> boxesIntersect boundingBox symBB && sym.Id <> sId)
    |> Map.isEmpty

/// Finds the optimal size and position for the selected symbol w.r.t. to its surrounding symbols.
let optimiseSymbol
    (wModel: BusWireT.Model)
    (symbol: Symbol)
    (boundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>)
    : BusWireT.Model =

    // If a wire connects the target symbol to another symbol, note which edge it is connected to
    let updateData (symConnData: SymConnDataT) _ (wire: Wire) =
        let symS, symT = getSourceSymbol wModel wire, getTargetSymbol wModel wire

        let otherSymbol =
            match symS, symT with
            | _ when (symS.Id <> symbol.Id) && (symT.Id = symbol.Id) -> Some symS
            | _ when (symS = symbol) && (symT <> symbol) -> Some symT
            | _ -> None

        match otherSymbol with
        | Some otherSym ->
            let edge = tryWireSymOppEdge wModel wire symbol otherSym

            match edge with
            | Some e -> updateOrInsert symConnData e otherSym.Id
            | None -> symConnData // should not happen
        | None -> symConnData 

    // Look through all wires to build up SymConnDataT.
    let symConnData = ({ ConnMap = Map.empty }, wModel.Wires) ||> Map.fold updateData

    let tryResize (symCount: ((ComponentId * Edge) * int) array) sym =

        let alignSym (sym: Symbol) (otherSym: Symbol) =
            let resizedSym = reSizeSymbol wModel sym otherSym
            let noOverlap = noSymbolOverlap DrawHelpers.boxesIntersect boundingBoxes resizedSym

            match noOverlap with
            | true -> true, resizedSym
            | _ -> false, sym

        let folder (hAligned, vAligned, sym) ((cid, edge), _) =
            let otherSym = Optic.get (symbolOf_ cid) wModel       

            match hAligned, vAligned with
            | false, _ when edge = Top || edge = Bottom ->
                let hAligned', resizedSym = alignSym sym otherSym
                (hAligned', vAligned, resizedSym)
            | _, false when edge = Left || edge = Right ->
                let vAligned', resizedSym = alignSym sym otherSym
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

    let model' = Optic.set (symbolOf_ symbol.Id) scaledSymbol wModel
    BusWireSeparate.routeAndSeparateSymbolWires model' symbol.Id

/// <summary>HLP 23: AUTHOR Ismagilov - Get the bounding box of multiple selected symbols</summary>
/// <param name="symbols"> Selected symbols list</param>
/// <returns>Bounding Box</returns>
let getBlock 
        (symbols:Symbol List) :BoundingBox = 

    let maxXsym = (List.maxBy (fun (x:Symbol) -> x.Pos.X+(snd (getRotatedHAndW x))) symbols)
    let maxX = maxXsym.Pos.X + (snd (getRotatedHAndW maxXsym))

    let minX = (List.minBy (fun (x:Symbol) -> x.Pos.X) symbols).Pos.X

    let maxYsym = List.maxBy (fun (x:Symbol) -> x.Pos.Y+(fst (getRotatedHAndW x))) symbols
    let maxY = maxYsym.Pos.Y + (fst (getRotatedHAndW maxYsym))

    let minY = (List.minBy (fun (x:Symbol) -> x.Pos.Y) symbols).Pos.Y

    {TopLeft = {X = minX; Y = minY}; W = maxX-minX; H = maxY-minY}


/// <summary>HLP 23: AUTHOR Ismagilov - Takes a point Pos, a centre Pos, and a rotation type and returns the point flipped about the centre</summary>
/// <param name="point"> Original XYPos</param>
/// <param name="center"> The center XYPos that the point is rotated about</param>
/// <param name="rotation"> Clockwise or Anticlockwise </param>
/// <returns>New flipped point</returns>
let rotatePointAboutBlockCentre 
            (point:XYPos) 
            (centre:XYPos) 
            (rotation:Rotation) = 
    let relativeToCentre = (fun x-> x - centre)
    let rotateAboutCentre (pointIn:XYPos) = 
        match rotation with 
        | Degree0 -> 
            pointIn
        | Degree90 ->
            {X = pointIn.Y ; Y = -pointIn.X}
        | Degree180 -> 
            {X = -pointIn.X ; Y = - pointIn.Y}
        | Degree270 ->
            {X = -pointIn.Y ; Y = pointIn.X}
           
    let relativeToTopLeft = (fun x-> centre - x)

    point
    |> relativeToCentre
    |> rotateAboutCentre
    |> relativeToTopLeft

/// <summary>HLP 23: AUTHOR Ismagilov - Takes a point Pos, a centre Pos, and a flip type and returns the point flipped about the centre</summary>
/// <param name="point"> Original XYPos</param>
/// <param name="center"> The center XYPos that the point is flipped about</param>
/// <param name="flip"> Horizontal or Vertical flip</param>
/// <returns>New flipped point</returns>
let flipPointAboutBlockCentre 
    (point:XYPos)
    (center:XYPos)
    (flip:FlipType) = 
    match flip with
    | FlipHorizontal-> 
        {X = center.X - (point.X - center.X); Y = point.Y} 
    | FlipVertical -> 
        {X = point.X; Y = center.Y - (point.Y - center.Y)}

/// <summary>HLP 23: AUTHOR Ismagilov - Get the new top left of a symbol after it has been rotated</summary>
/// <param name="rotation"> Rotated CW or AntiCW</param>
/// <param name="h"> Original height of symbol (Before rotation)</param>
/// <param name="w"> Original width of symbol (Before rotation)</param>
/// <param name="sym"> Symbol</param>
/// <returns>New top left point of the symbol</returns>
let adjustPosForBlockRotation
        (rotation:Rotation) 
        (h: float)
        (w:float)
        (pos: XYPos)
         : XYPos =
    let posOffset =
        match rotation with
        | Degree0 -> {X = 0; Y = 0}
        | Degree90 -> {X=(float)h ;Y=0}
        | Degree180 -> {X= (float)w; Y= -(float)h}
        | Degree270 -> { X = 0 ;Y = (float)w }
    pos - posOffset

/// <summary>HLP 23: AUTHOR Ismagilov - Get the new top left of a symbol after it has been flipped</summary>
/// <param name="flip">  Flipped horizontally or vertically</param>
/// <param name="h"> Original height of symbol (Before flip)</param>
/// <param name="w"> Original width of symbol (Before flip)</param>
/// <param name="sym"> Symbol</param>
/// <returns>New top left point of the symbol</returns>
let adjustPosForBlockFlip
        (flip:FlipType) 
        (h: float)
        (w:float)
        (pos: XYPos) =
    let posOffset =
        match flip with
        | FlipHorizontal -> {X=(float)w ;Y=0}
        | FlipVertical -> { X = 0 ;Y = (float)h }
    pos - posOffset



(* ---------------------------------------------------------------------------------------------- *)
(*                                             Helpers                                            *)
(* ---------------------------------------------------------------------------------------------- *)

(* ------------------------------------ Refactor-ed Functions ----------------------------------- *)

/// <summary>Get list of symbols from list of component IDs.</summary>
/// <param name="compIds">List of component IDs to fetch.</param>
/// <param name="model">Symbol model.</param>
/// <returns>List of symbols. If ComponentId is not present in symbol model, it will be omitted from output.</returns>
let getSymsFromIds (compIds: List<ComponentId>) (model: SymbolT.Model): List<Symbol> =
    compIds 
    |> List.map (fun compId -> Map.tryFind compId model.Symbols)
    |> List.choose id

/// <summary>Apply mapping to some symbols, specified by compIds, within a symbol model.</summary>
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
/// <returns>Symbol dimension in XYPos vector format.</returns>
let getRotatedSymDim (sym: Symbol): XYPos =
    let h, w = getRotatedHAndW sym
    { X = w; Y = h }

/// <summary>Get the 4 outermost symbols by centre of a selected list.</summary>
/// <param name="syms">List of symbols to check.</param>
/// <returns>Tuple of symbols, in the order of: max x, min x, max y, min y.</returns>
let getMaxMinSymInBlock2D (syms: List<Symbol>): Symbol*Symbol*Symbol*Symbol =
    /// <summary>Helper to get the outermost symbols by centre of a selected list in 1 direction.</summary>
    /// <param name="dimSel">Dimension selector, either do ".X" or ".Y".</param>
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
/// <remarks>To be used in conjunction with getMaxMinSymInBlock2D.</remarks>
let mapMaxMinSyms2D
        (xMapping: Symbol->float) 
        (yMapping: Symbol->float)
        (maxMinSyms: Symbol*Symbol*Symbol*Symbol)
            : float*float*float*float =
    let maxXSym, minXSym, maxYSym, minYSym = maxMinSyms
    
    xMapping maxXSym, xMapping minXSym, yMapping maxYSym, yMapping minYSym
    

(* ------------------------------------------ Interface ----------------------------------------- *)

/// <summary>[Deprecated] Get list of symbols from list of ComponentIds.</summary>
/// <param name="compList">List of component IDs to fetch.</param>
/// <param name="model">Symbol model.</param>
/// <returns>List of symbols.</returns>
/// <remarks>Interface layer for external codebase. Use getSymsFromIds instead.</remarks>
let findSelectedSymbols (compList: List<ComponentId>) (model: SymbolT.Model): List<Symbol> =
    getSymsFromIds compList model

/// <summary>[Deprecated] Apply function to selected symbols within a symbol model.</summary>
/// <param name="compList">List of selected component IDs to apply function to.</param>
/// <param name="model">Symbol model.</param>
/// <param name="selectedSymbols">[NOT USED] List of symbols to apply function to.</param>
/// <param name="modifySymbolFunc">Mapping function to transform symbol.</param>
/// <returns>Updated symbol models with mapping applied to selected symbols.</returns>
/// <remarks>Interface layer for external codebase. Use mapSelectedSymsInModel instead.</remarks>
let groupNewSelectedSymsModel 
        (compList: List<ComponentId>) 
        (model: SymbolT.Model) 
        (selectedSymbols: List<Symbol>) 
        (modifySymbolFunc: Symbol->Symbol)
            : SymbolT.Model = 
    mapSelectedSymsInModel modifySymbolFunc compList model


(* ---------------------------------------------------------------------------------------------- *)
(*                                             Rotate                                             *)
(* ---------------------------------------------------------------------------------------------- *)

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
        rotatePointAboutBlockCentre sym.Pos (blockBBox.Centre()) (invertRotation degree)
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
    rotateSymInBlock degree (getBlock [ sym ]) sym

/// <summary>Rotates a block of symbols.</summary>
/// <param name="degree">Degree to rotate block by.</param>
/// <param name="compIds">List of ComponentIds of block components.</param>
/// <param name="model">Target symbol model.</param>
/// <returns>Returns updated symbol model, of type SymbolT.Model.</returns>
/// <remarks>Refactored top-level function. Apostrophe to be removed after codebase is refactored.</remarks>
let rotateBlock' (degree: Rotation) (compIds: List<ComponentId>) (model: SymbolT.Model): SymbolT.Model =
    let selectedSymsBBox = getSymsFromIds compIds model |> getBlock
    
    mapSelectedSymsInModel (rotateSymInBlock (invertRotation degree) selectedSymsBBox) compIds model


(* ------------------------------------------ Interface ----------------------------------------- *)

/// <summary>[Deprecated] Rotates a block of symbols.</summary>
/// <param name="compList">List of ComponentId's of selected components.</param>
/// <param name="model">Current symbol model.</param>
/// <param name="rotation">Type of rotation to do.</param>
/// <returns>Returns updated symbol model, of type SymbolT.Model.</returns>
/// <remarks>Interface layer for existing codebase. Use rotateBlock' instead.</remarks>
let rotateBlock (compList: List<ComponentId>) (model: SymbolT.Model) (rotation:Rotation): SymbolT.Model =
    rotateBlock' (rotation) (compList) (model)


(* ---------------------------------------------------------------------------------------------- *)
(*                                              Flip                                              *)
(* ---------------------------------------------------------------------------------------------- *)

/// <summary>Flip a symbol in a selection block.</summary>
/// <param name="flip">Direction to flip, either FlipHorizontal or FlipVertical.</param>
/// <param name="blockBBox">Bounding box of selection block.</param>
/// <param name="sym">Symbol to be flipped.</param>
/// <returns>Updated symbol after flipped about block centre.</returns>
/// <remarks>Author: Ismagilov, HLP23.</remarks>
let flipSymInBlock (flip: FlipType) (blockBBox: BoundingBox) (sym: Symbol): Symbol =
    let rotatedH, rotatedW = // h and w not updated during normal rotate
        getRotatedHAndW sym

    let newTopLeft = // update pos of block
        flipPointAboutBlockCentre sym.Pos (blockBBox.Centre()) flip 
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
            let symBBox = getBlock [ sym ]
            sym 
            |> rotateSymInBlock Degree270 symBBox 
            |> rotateSymInBlock Degree270 symBBox)

/// <summary>Flips a block of symbols.</summary>
/// <param name="flip">Type of flip to do.</param>
/// <param name="compIds">List of ComponentIds of block components.</param>
/// <param name="model">Target symbol model.</param>
/// <returns>Returns updated symbol model, of type SymbolT.Model.</returns>
/// <remarks>Refactored top-level function. Apostrophe to be removed after codebase is refactored.</remarks>
let flipBlock' (flip: FlipType) (compIds :ComponentId list) (model:SymbolT.Model): SymbolT.Model = 
    let selectedSymsBBox = getSymsFromIds compIds model |> getBlock

    mapSelectedSymsInModel (flipSymInBlock flip selectedSymsBBox) compIds model


(* ------------------------------------------ Interface ----------------------------------------- *)

/// <summary>[Deprecated] Flips a block of symbols.</summary>
/// <param name="compList">List of ComponentIds of block components.</param>
/// <param name="model">Target symbol model.</param>
/// <param name="flip">Type of flip to do.</param>
/// <returns>Returns updated symbol model, of type SymbolT.Model.</returns>
/// <remarks>Interface layer for existing codebase. Use flipBlock' instead. Author: Ismagilov, HLP23.</remarks>
let flipBlock (compList: ComponentId list) (model: SymbolT.Model) (flip: FlipType) = 
    flipBlock' flip compList model


(* ---------------------------------------------------------------------------------------------- *)
(*                                              Scale                                             *)
(* ---------------------------------------------------------------------------------------------- *)

/// <summary>Scales a symbol up or down in a selection box.</summary>
/// <param name="scale">ScaleUp or ScaleDown.</param>
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
    /// <param name="dim">Function to access 1 dimension of XYPos.</param>
    let findNewPos1D (dim: XYPos->float): float =
        let prop = // proportion of symbol to block 
            (dim rotatedCentre) - (dim blockPos) / (dim blockHW)
        let centre = // centre of symbol
            match scale with
            | ScaleUp -> (dim blockPos - blockPosDiff) + (dim blockHW + blockSideDiff) * prop
            | ScaleDown -> (dim blockPos + blockPosDiff) + (dim blockHW - blockSideDiff) * prop
        let pos = // top-left pos of symbol
            centre - (dim rotatedHW)/2.
        
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
/// <remarks>No original docstring. Educated guess used.</remarks>
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
/// <remarks>Original docstring not complete. Educated guess used for params.</remarks>
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

    let newMaxX = matchBBoxMax.X - (snd (getRotatedHAndW maxXSym))/2.
    let newMinX = matchBBoxMin.X + (snd (getRotatedHAndW minXSym))/2.
    let newMaxY = matchBBoxMax.Y - (fst (getRotatedHAndW maxYSym))/2.
    let newMinY = matchBBoxMin.Y + (fst (getRotatedHAndW minYSym))/2.
    
    let xScalingCoeffs = getScalingCoeffs1D oldMinX newMinX oldMaxX newMaxX
    let yScalingCoeffs = getScalingCoeffs1D oldMinY newMinY oldMaxY newMaxY

    (xScalingCoeffs, yScalingCoeffs)

/// <summary>Alter the position of a symbol as needed in a scaling operation.</summary>
/// <param name="xyScalingCoeffs">Scaling coefficients, in the form of the output of getScalingCoeffsGroup.</param>
/// <param name="sym">Target symbol.</param>
/// <returns>Updated symbol its position is updated.</returns>
/// <remarks>Original docstring not complete. Educated guess used for params.</remarks>
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

/// <summary>[Deprecated] Return set of floats that define how a group of components is scaled.</summary>
/// <param name="matchBBMin">Min corner of bounding box to scale to.</param>
/// <param name="matchBBMax">Max corner of bounding box to scale to.</param>
/// <returns>Tuple of tuple of scaling coefficients, in the order of x-dimension scaling factor and offset centre, 
/// and then the y-dimension.</returns>
/// <remarks>Interface layer for existing codebase. Use getScalingCoeffsGroup instead.
/// Original docstring not complete. Educated guess used for params.</remarks>
let getScalingFactorAndOffsetCentreGroup 
        (matchBBMin: XYPos) 
        (matchBBMax: XYPos) 
        (selectedSyms: List<Symbol>)
            : ((float*float)*(float*float)) =  
    getScalingCoeffsGroup matchBBMin matchBBMax selectedSyms

/// <summary>[Deprecated] Alter the position of a symbol as needed in a scaling operation.</summary>
/// <param name="xYSC">Scaling coefficients, in the form of the output of getScalingCoeffsGroup.</param>
/// <param name="sym">Target symbol.</param>
/// <returns>Updated symbol its position is updated.</returns>
/// <remarks>Interface layer for existing codebase. Use scaleSymbolPos instead.
/// Original docstring not complete. Educated guess used for params.</remarks>
let scaleSymbol 
        (xYSC: ((float*float)*(float*float)))
        (sym: Symbol)
            : Symbol = 
    scaleSymbolPos xYSC sym


(* ---------------------------------------------------------------------------------------------- *)
(*                                       Update Scaling Box                                       *)
(* ---------------------------------------------------------------------------------------------- *)

/// After every model update this updates the "scaling box" part of the model to be correctly
/// displayed based on whetehr multiple components are selected and if so what is their "box"
/// In addition to changing the model directly, cmd may contain messages that make further changes.
let postUpdateScalingBox (model: SheetT.Model, cmd) = 
    
    let symbolCmd (msg: SymbolT.Msg) = Elmish.Cmd.ofMsg (ModelType.Msg.Sheet (SheetT.Wire (BusWireT.Symbol msg)))
    let sheetCmd (msg: SheetT.Msg) = Elmish.Cmd.ofMsg (ModelType.Msg.Sheet msg)

    if (model.SelectedComponents.Length < 2) then 
        match model.ScalingBox with 
        | None ->  model, cmd
        | _ -> {model with ScalingBox = None}, 
                [symbolCmd (SymbolT.DeleteSymbols (model.ScalingBox.Value).ButtonList);
                 sheetCmd SheetT.UpdateBoundingBoxes]
                |> List.append [cmd]
                |> Elmish.Cmd.batch
    else 
        let newBoxBound = 
            model.SelectedComponents
            |> List.map (fun id -> Map.find id model.Wire.Symbol.Symbols)
            |> getBlock
        match model.ScalingBox with 
        | Some value when value.ScalingBoxBound = newBoxBound -> model, cmd
        | _ -> 
            let topleft = newBoxBound.TopLeft
            let rotateDeg90OffSet: XYPos = {X = newBoxBound.W+57.; Y = (newBoxBound.H/2.)-12.5}
            let rotateDeg270OffSet: XYPos = {X = -69.5; Y = (newBoxBound.H/2.)-12.5}
            let buttonOffSet: XYPos = {X = newBoxBound.W + 47.5; Y = -47.5}
            let dummyPos = (topleft + buttonOffSet)

            let makeButton = SymbolUpdate.createAnnotation ThemeType.Colourful
            let buttonSym = {makeButton ScaleButton dummyPos with Pos = (topleft + buttonOffSet)}
            let makeRotateSym sym = {sym with Component = {sym.Component with H = 25.; W=25.}}
            let rotateDeg90Sym = 
                makeButton (RotateButton Degree90) (topleft + rotateDeg90OffSet)
                |> makeRotateSym
            let rotateDeg270Sym = 
                {makeButton (RotateButton Degree270) (topleft + rotateDeg270OffSet) 
                    with SymbolT.STransform = {Rotation=Degree90 ; Flipped=false}}
                |> makeRotateSym

            let newSymbolMap = model.Wire.Symbol.Symbols 
                                                        |> Map.add buttonSym.Id buttonSym 
                                                        |> Map.add rotateDeg270Sym.Id rotateDeg270Sym 
                                                        |> Map.add rotateDeg90Sym.Id rotateDeg90Sym
            let initScalingBox: SheetT.ScalingBox = {
                ScalingBoxBound = newBoxBound;
                ScaleButton = buttonSym;
                RotateDeg90Button = rotateDeg90Sym;
                RotateDeg270Button = rotateDeg270Sym;
                ButtonList = [buttonSym.Id; rotateDeg270Sym.Id; rotateDeg90Sym.Id];
            }
            let newCmd =
                match model.ScalingBox with
                | Some _ -> [symbolCmd (SymbolT.DeleteSymbols (model.ScalingBox.Value).ButtonList);
                             sheetCmd SheetT.UpdateBoundingBoxes]
                            |> List.append [cmd]
                            |> Elmish.Cmd.batch
                | None -> cmd
            model
            |> Optic.set SheetT.scalingBox_ (Some initScalingBox)
            |> Optic.set SheetT.symbols_ newSymbolMap, 
            newCmd


(* ---------------------------------------------------------------------------------------------- *)
(*                                              Misc                                              *)
(* ---------------------------------------------------------------------------------------------- *)

/// <summary>Checks whether a symbol spans across a list of selected symbols in either dimension.</summary>
/// <param name="selectedSyms">List of symbols to check.</param>
/// <returns>Boolean, whether a symbol bounds both edges.</returns>
let oneCompBoundsBothEdges (selectedSyms: List<Symbol>): bool =
    let maxXCentreX, minXCentreX, maxYCentreY, minYCentreY = 
        mapMaxMinSyms2D 
            (fun xSym -> (getRotatedSymbolCentre xSym).X)
            (fun ySym -> (getRotatedSymbolCentre ySym).Y)
            (getMaxMinSymInBlock2D selectedSyms)
    
    maxXCentreX = minXCentreX || maxYCentreY = minYCentreY
