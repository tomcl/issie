// Summary of <sk1421>'s improvements to RotateScale:
// 1. Line 261 onwards [updateData]: Match case compression (bringing common logic before the match),
// better use of monadic library functions (bind).
// 2. Line 280 onwards [tryResize]: Ordering - Changed the order of sym and symCount (sym is more static
// parameter) to facilitate currying.
// 3. Line 298 onwards [tryResize]: Match case compression - brought common logic before the match, to
// calculate one variable whose value could be used in the horizontal or vertical alignment.
// 4. Line 315 onwards [scaledSymbol]: Better use of pipelines - this was facilitated by switching the order of
// arguments in the function tryResize
// 5. Better XML docs: Lines 87 [getWirePorts] onwards, 124 [alignPortsOffset] onwards, 145 [alignSymbols] onwards, 227 [updateOrInsert] onwards - streamlined XML
// documentation, ensuring line breaks and explanation of the parameters of each function.
// 6. Changed function name and variables [getWirePorts]: getPortsAB renamed to getWirePorts -> More descriptive, the function is meant to get the source and target ports of a
// wire from the two symbols connected to it.
// 7. Lines 367 onwards [rotatePointAboutBlockCentre]: Better use of anonymous functions to make the pipeline readable, and more functional.
// Change function names to make them more descriptive and extract similar functionality from the same
// function (one function just used the opposite sign for the same value - more in the description).
// 8. Lines 411 onwards [adjustPosForBlockRotation]: Used anonymous function at the end of the pipeline to subtract offset
// from the position. Also removed redundant helper function within the function.
// 9. Lines 435 onwards [adjustPosForBlockFlip]: Same as above, implemented the same functionality except for flip state.
// Also removed redundant helper function within the function.

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

// <sk1421's addition: Better XML docs
/// Given a wire between two symbols (A and B), gets the ports
/// at either end (input and output) of the connection
///
/// wModel - The wire model
///
/// wireSyms - Record containing symbol - wire - symbol connection.
let getWirePorts wModel wireSyms =
    let ports = portsOfWires wModel [ wireSyms.Wire ]
    let sourcePort = filterPortBySym ports wireSyms.SymA |> List.head
    let targetPort = filterPortBySym ports wireSyms.SymB |> List.head
    sourcePort, targetPort

/// Try to get two ports that are on opposite edges.
let getOppEdgePortInfo
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    : (PortInfo * PortInfo) option =
    let wires = wiresBtwnSyms wModel symbolToSize otherSymbol

    let tryGetOppEdgePorts wireSyms =
        let portA, portB = getWirePorts wModel wireSyms
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

// <sk1421's addition>: Better XML documentation
/// Given the port info for two ports, finds the offset
/// between them that leads to alignment when covered.
///
/// movePInfo - PortInfo for port to be moved.
///
/// otherPInfo - PortInfo for the other port.
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

// <sk1421>'s addition: Better XML Docs
/// Given a symbol to align with another symbol, attempts to
/// align them.
///
/// wModel - The wire model on the sheet
///
/// symbolToSize - Symbol to align
///
/// otherSymbol - The symbol with respect to which the alignment should occur.
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
            let pA, pB = getWirePorts wModel { SymA = symbolToSize; SymB = otherSymbol; Wire = wires[0] }
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

// <sk1421>'s addition: Better XML docs
/// Given an edge, component ID and the connection data for a symbol,
/// adds a connection to that edge on the symbol. If there are no connections,
/// initializes the map with 1 (returns a new map).
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

        // <sk1421>'s addition:
        // Transform 6 & 1: Monadic operations & Match case compression
        // Issie coding guidelines - moving common code outside the match
        // Initially, in this function, edge was being calculated inside a match statement
        // There was a match to extract the otherSymbol from the option and then use that to
        // find an edge option. There was a redundant case, wherein if the edge was none
        // (which should never happen) symConnData was returned, but this was the default
        // return anyway for a None case in otherSymbol.
        let edge =
            otherSymbol
            |> Option.bind (tryWireSymOppEdge wModel wire symbol)

        match otherSymbol, edge with
        | Some otherSym, Some edge -> updateOrInsert symConnData edge otherSym.Id
        | _ -> symConnData 

    // Look through all wires to build up SymConnDataT.
    let symConnData = ({ ConnMap = Map.empty }, wModel.Wires) ||> Map.fold updateData

    // <sk1421>'s addition:
    // Transform 2: Ordering
    // Re-ordered sym, moved it ahead of symCount to implement RSN,
    // this is immediately useful below (lines 319 onwards) as it allows good use of pipelining
    // in scaledSymbol
    let tryResize sym (symCount: ((ComponentId * Edge) * int) array) =

        let alignSym (sym: Symbol) (otherSym: Symbol) =
            let resizedSym = reSizeSymbol wModel sym otherSym
            let noOverlap = noSymbolOverlap DrawHelpers.boxesIntersect boundingBoxes resizedSym

            match noOverlap with
            | true -> true, resizedSym
            | _ -> false, sym

        let folder (hAligned, vAligned, sym) ((cid, edge), _) =
            let otherSym = Optic.get (symbolOf_ cid) wModel       

            // <sk1421> 's addition:
            // Transform 1: Match case compression
            // Extracted common code at the beginning of the match, outside of it,
            // slightly more descriptive variable names.
            // Initially there was a let definition using the same value in the first two match cases.
            let isAligned, newSym = alignSym sym otherSym

            match hAligned, vAligned with
            | false, _ when edge = Top || edge = Bottom ->
                (isAligned, vAligned, newSym)
            | _, false when edge = Left || edge = Right ->
                (hAligned, isAligned, newSym)
            | _ -> (hAligned, vAligned, sym)

        let (_, _, sym') = ((false, false, sym), symCount) ||> Array.fold folder
        sym'

    // <sk1421> 's addition:
    // Transform 3 & 1: Making (extending) pipeline + Structural Abstraction [This was facilitated
    // by the transform 2 above.] Better use of pipelines here, avoiding unnecessary let by applying
    // tryResize to symbol Which was achieved by swapping parameters of tryResize. This allowed the pipeline to feed into the curried tryResize.
    let scaledSymbol =
        Map.toArray symConnData.ConnMap
        |> Array.filter (fun (_, count) -> count > 1)
        |> Array.sortByDescending snd
        |> tryResize symbol

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

    // <sk1421> 's addition:
    // Transform 3: Anonymous function at the end (and between)
    // Replaced unnecessary anonymous function declarations. Also
    // created function distanceFromCentre (below) which binds the
    // +/- functionality of the previous relativeToCentre and
    // relativeToTopLeft anonymous functions which performed the same
    // subtraction but with different signs (relativeToCentre = - relativeToTopLeft). 

    let distanceFromCentre (pointIn: XYPos) = centre - pointIn

    point
    |> distanceFromCentre
    |> (-) XYPos.zero // This just gets - distanceFromCentre
    |> rotateAboutCentre
    |> distanceFromCentre

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

    // <sk1421> 's addition:
    // Transform 1: Structural Abstraction with pipelining
    // Pipeline adds more readability. Anonymous function at the end of the pipeline to
    // achieve subtraction of the newly calculated symbol boundaries (based on the degree of rotation)
    // from the position of the block.
    match rotation with
    | Degree0 -> {X = 0; Y = 0}
    | Degree90 -> {X=(float)h ;Y=0}
    | Degree180 -> {X= (float)w; Y= -(float)h}
    | Degree270 -> { X = 0 ;Y = (float)w }
    |> (-) pos

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

    // <sk1421> 's addition:
    // Transform 1: Structural Abstraction with pipelining
    // Pipeline adds more readability. Anonymous function at the end of the pipeline to
    // achieve subtraction of the newly calculated symbol boundaries (based on the flip orientation)
    // from the position of the block.
    match flip with
    | FlipHorizontal -> {X=(float)w ;Y=0}
    | FlipVertical -> { X = 0 ;Y = (float)h }
    |> (-) pos

/// <summary>HLP 23: AUTHOR Ismagilov - Rotate a symbol in its block.</summary>
/// <param name="rotation">  Clockwise or Anticlockwise rotation</param>
/// <param name="block"> Bounding box of selected components</param>
/// <param name="sym"> Symbol to be rotated</param>
/// <returns>New symbol after rotated about block centre.</returns>
let rotateSymbolInBlock 
        (rotation: Rotation) 
        (blockCentre: XYPos)
        (sym: Symbol)  : Symbol =
      
    let h,w = getRotatedHAndW sym

    let newTopLeft = 
        rotatePointAboutBlockCentre sym.Pos blockCentre (invertRotation rotation)
        |> adjustPosForBlockRotation (invertRotation rotation) h w

    let newComponent = { sym.Component with X = newTopLeft.X; Y = newTopLeft.Y}
    
    let newSTransform = 
        match sym.STransform.Flipped with
        | true -> 
            {sym.STransform with Rotation = combineRotation (invertRotation rotation) sym.STransform.Rotation}  
        | _-> 
            {sym.STransform with Rotation = combineRotation rotation sym.STransform.Rotation}

    { sym with 
        Pos = newTopLeft;
        PortMaps = rotatePortInfo rotation sym.PortMaps
        STransform = newSTransform 
        LabelHasDefaultPos = true
        Component = newComponent
    } |> calcLabelBoundingBox 


/// <summary>HLP 23: AUTHOR Ismagilov - Flip a symbol horizontally or vertically in its block.</summary>
/// <param name="flip">  Flip horizontally or vertically</param>
/// <param name="block"> Bounding box of selected components</param>
/// <param name="sym"> Symbol to be flipped</param>
/// <returns>New symbol after flipped about block centre.</returns>
let flipSymbolInBlock
    (flip: FlipType)
    (blockCentre: XYPos)
    (sym: Symbol) : Symbol =

    let h,w = getRotatedHAndW sym
    //Needed as new symbols and their components need their Pos updated (not done in regular flip symbol)
    let newTopLeft = 
        flipPointAboutBlockCentre sym.Pos blockCentre flip
        |> adjustPosForBlockFlip flip h w

    let portOrientation = 
        sym.PortMaps.Orientation |> Map.map (fun id side -> flipSideHorizontal side)

    let flipPortList currPortOrder side =
        currPortOrder |> Map.add (flipSideHorizontal side ) sym.PortMaps.Order[side]

    let portOrder = 
        (Map.empty, [Edge.Top; Edge.Left; Edge.Bottom; Edge.Right]) ||> List.fold flipPortList
        |> Map.map (fun edge order -> List.rev order)       

    let newSTransform = 
        {Flipped= not sym.STransform.Flipped;
        Rotation= sym.STransform.Rotation} 

    let newcomponent = {sym.Component with X = newTopLeft.X; Y = newTopLeft.Y}

    { sym with
        Component = newcomponent
        PortMaps = {Order=portOrder;Orientation=portOrientation}
        STransform = newSTransform
        LabelHasDefaultPos = true
        Pos = newTopLeft
    }
    |> calcLabelBoundingBox
    |> (fun sym -> 
        match flip with
        | FlipHorizontal -> sym
        | FlipVertical -> 
            let newblock = getBlock [sym]
            let newblockCenter = newblock.Centre()
            sym
            |> rotateSymbolInBlock Degree270 newblockCenter 
            |> rotateSymbolInBlock Degree270 newblockCenter)

/// <summary>HLP 23: AUTHOR Ismagilov - Scales selected symbol up or down.</summary>
/// <param name="scaleType"> Scale up or down. Scaling distance is constant</param>
/// <param name="block"> Bounding box of selected components</param>
/// <param name="sym"> Symbol to be rotated</param>
/// <returns>New symbol after scaled about block centre.</returns>
let scaleSymbolInBlock
    //(Mag: float)
    (scaleType: ScaleType)
    (block: BoundingBox)
    (sym: Symbol) : Symbol =

    let symCenter = getRotatedSymbolCentre sym

    //Get x and y proportion of symbol to block
    let xProp, yProp = (symCenter.X - block.TopLeft.X) / block.W, (symCenter.Y - block.TopLeft.Y) / block.H

    let newCenter = 
        match scaleType with
            | ScaleUp ->
                {X = (block.TopLeft.X-5.) + ((block.W+10.) * xProp); Y = (block.TopLeft.Y-5.) + ((block.H+10.) * yProp)}
            | ScaleDown ->
                {X= (block.TopLeft.X+5.) + ((block.W-10.) * xProp); Y=  (block.TopLeft.Y+5.) + ((block.H-10.) * yProp)}

    let h,w = getRotatedHAndW sym
    let newPos = {X = (newCenter.X) - w/2.; Y= (newCenter.Y) - h/2.}
    let newComponent = { sym.Component with X = newPos.X; Y = newPos.Y}

    {sym with Pos = newPos; Component=newComponent; LabelHasDefaultPos=true}


/// HLP 23: AUTHOR Klapper - Rotates a symbol based on a degree, including: ports and component parameters.

let rotateSymbolByDegree (degree: Rotation) (sym:Symbol)  =
    let pos = {X = sym.Component.X + sym.Component.W / 2.0 ; Y = sym.Component.Y + sym.Component.H / 2.0 }
    match degree with
    | Degree0 -> sym
    | _ ->  rotateSymbolInBlock degree pos sym
    

/// <summary>HLP 23: AUTHOR Ismagilov - Rotates a block of symbols, returning the new symbol model</summary>
/// <param name="compList"> List of ComponentId's of selected components</param>
/// <param name="model"> Current symbol model</param>
/// <param name="rotation"> Type of rotation to do</param>
/// <returns>New rotated symbol model</returns>
let rotateBlock (compList:ComponentId list) (model:SymbolT.Model) (rotation:Rotation) = 

    printfn "running rotateBlock"
    let SelectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList
    let UnselectedSymbols = model.Symbols |> Map.filter (fun x _ -> not (List.contains x compList))

    //Get block properties of selected symbols
    let block = getBlock SelectedSymbols

    //Rotated symbols about the center
    let newSymbols = 
        List.map (fun x -> rotateSymbolInBlock (invertRotation rotation) (block.Centre()) x) SelectedSymbols 

    //return model with block of rotated selected symbols, and unselected symbols
    {model with Symbols = 
                ((Map.ofList (List.map2 (fun x y -> (x,y)) compList newSymbols)
                |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols)
    )}

let oneCompBoundsBothEdges (selectedSymbols: Symbol list) = 
    let maxXSymCentre = 
            selectedSymbols
            |> List.maxBy (fun (x:Symbol) -> x.Pos.X + snd (getRotatedHAndW x)) 
            |> getRotatedSymbolCentre
    let minXSymCentre =
            selectedSymbols
            |> List.minBy (fun (x:Symbol) -> x.Pos.X)
            |> getRotatedSymbolCentre
    let maxYSymCentre = 
            selectedSymbols
            |> List.maxBy (fun (y:Symbol) -> y.Pos.Y+ fst (getRotatedHAndW y))
            |> getRotatedSymbolCentre
    let minYSymCentre =
            selectedSymbols
            |> List.minBy (fun (y:Symbol) -> y.Pos.Y)
            |> getRotatedSymbolCentre
    (maxXSymCentre.X = minXSymCentre.X) || (maxYSymCentre.Y = minYSymCentre.Y)
    

let findSelectedSymbols (compList: ComponentId list) (model: SymbolT.Model) = 
    List.map (fun x -> model.Symbols |> Map.find x) compList

let getScalingFactorAndOffsetCentre (min:float) (matchMin:float) (max:float) (matchMax:float) = 
    let scaleFact = 
        if min = max || matchMax <= matchMin then 1. 
        else (matchMin - matchMax) / (min - max)
    let offsetC = 
        if scaleFact = 1. then 0.
        else (matchMin - min * scaleFact) / (1.-scaleFact)
    (scaleFact, offsetC)

/// Return set of floats that define how a group of components is scaled
let getScalingFactorAndOffsetCentreGroup
    (matchBBMin:XYPos)
    (matchBBMax:XYPos)
    (selectedSymbols: Symbol list) : ((float * float) * (float * float)) = 
    //(compList: ComponentId list)
    //(model: SymbolT.Model)

    //let selectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList

    let maxXSym = 
            selectedSymbols
            |> List.maxBy (fun (x:Symbol) -> x.Pos.X + snd (getRotatedHAndW x)) 

    let oldMaxX = (maxXSym |> getRotatedSymbolCentre).X
    let newMaxX = matchBBMax.X - (snd (getRotatedHAndW maxXSym))/2.

    let minXSym =
            selectedSymbols
            |> List.minBy (fun (x:Symbol) -> x.Pos.X)

    let oldMinX = (minXSym |> getRotatedSymbolCentre).X
    let newMinX = matchBBMin.X + (snd (getRotatedHAndW minXSym))/2.
    
    let maxYSym = 
            selectedSymbols
            |> List.maxBy (fun (y:Symbol) -> y.Pos.Y+ fst (getRotatedHAndW y))

    let oldMaxY = (maxYSym |> getRotatedSymbolCentre).Y
    let newMaxY = matchBBMax.Y - (fst (getRotatedHAndW maxYSym))/2.

    let minYSym =
            selectedSymbols
            |> List.minBy (fun (y:Symbol) -> y.Pos.Y)

    let oldMinY = (minYSym |>  getRotatedSymbolCentre).Y
    let newMinY = matchBBMin.Y + (fst (getRotatedHAndW minYSym))/2.
    
    let xSC = getScalingFactorAndOffsetCentre oldMinX newMinX oldMaxX newMaxX
    let ySC = getScalingFactorAndOffsetCentre oldMinY newMinY oldMaxY newMaxY
    (xSC, ySC)

/// Alter position of one symbol as needed in a scaling operation
let scaleSymbol
        (xYSC: (float * float) * (float * float))
        (sym: Symbol)
        : Symbol = 
    let symCentre =  getRotatedSymbolCentre sym
    let translateFunc scaleFact offsetC coordinate = (coordinate - offsetC) * scaleFact + offsetC
    let xSC = fst xYSC
    let ySC = snd xYSC
    let newX = translateFunc (fst xSC) (snd xSC) symCentre.X
    let newY = translateFunc (fst ySC) (snd ySC) symCentre.Y

    let symCentreOffsetFromTopLeft = {X = (snd (getRotatedHAndW sym))/2.; Y = (fst (getRotatedHAndW sym))/2.}
    let newTopLeftPos = {X = newX; Y = newY} - symCentreOffsetFromTopLeft
    let newComp = {sym.Component with X = newTopLeftPos.X; Y = newTopLeftPos.Y}

    {sym with Pos = newTopLeftPos; Component = newComp; LabelHasDefaultPos = true}

/// Part of the rotate and scale code       
let groupNewSelectedSymsModel
    (compList:ComponentId list) 
    (model:SymbolT.Model) 
    (selectedSymbols: Symbol list)
    (modifySymbolFunc) = 

    //let SelectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList
    let UnselectedSymbols = model.Symbols |> Map.filter (fun x _ -> not (List.contains x compList))

    // let block = getBlock SelectedSymbols
    // printfn "bbCentreX:%A" (block.Centre()).X

    // let newSymbols = List.map (modifySymbolFunc (block.Centre())) SelectedSymbols
    let newSymbols = List.map (modifySymbolFunc) selectedSymbols

    {model with Symbols = 
                ((Map.ofList (List.map2 (fun x y -> (x,y)) compList newSymbols)
                |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols)
    )}


/// <summary>HLP 23: AUTHOR Ismagilov - Flips a block of symbols, returning the new symbol model</summary>
/// <param name="compList"> List of ComponentId's of selected components</param>
/// <param name="model"> Current symbol model</param>
/// <param name="flip"> Type of flip to do</param>
/// <returns>New flipped symbol model</returns>
let flipBlock (compList:ComponentId list) (model:SymbolT.Model) (flip:FlipType) = 
    //Similar structure to rotateBlock, easy to understand
    let SelectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList
    let UnselectedSymbols = model.Symbols |> Map.filter (fun x _ -> not (List.contains x compList))
    
    let block = getBlock SelectedSymbols
  
    let newSymbols = 
        List.map (fun x -> flipSymbolInBlock flip (block.Centre()) x ) SelectedSymbols

    {model with Symbols = 
                ((Map.ofList (List.map2 (fun x y -> (x,y)) compList newSymbols)
                |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols)
    )}

/// After every model update this updates the "scaling box" part of the model to be correctly
/// displayed based on whetehr multiple components are selected and if so what is their "box"
/// In addition to changing the model directly, cmd may contain messages that make further changes.
let postUpdateScalingBox (model:SheetT.Model, cmd) = 
    
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

