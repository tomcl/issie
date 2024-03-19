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


// yc3521:
// ---------------------------- improvement part 1 ----------------------------

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

//*yc3521: Improved by functional abstraction 
//         introduced a hypothetical calculatePortGap function
//         increased readability and reuse of code

/// TODO: this is mostly copy pasted code from Symbol.getPortPos, perhaps abstract out the existing code there to use makePortInfo.
/// Could not simply use getPortPos because more data (side, topBottomGap, etc.) is needed to caclulate the new dimensions of the resized symbol.

//*yc3521: Helper function to abstract the match side expressions
let calculateBasedOnSide side calculateForHorizontal calculateForVertical =
    match side with
    | Left | Right -> calculateForHorizontal()
    | Top | Bottom -> calculateForVertical()

    
let makePortInfo (sym: Symbol) (port: Port) =
    let side = getSymbolPortOrientation sym port
    let ports = sym.PortMaps.Order[side] //list of ports on the same side as port
    let gap = getPortPosEdgeGap sym.Component.Type
    let topBottomGap = gap + 0.3 // extra space for clk symbol
    let portDimension = float ports.Length - 1.0
    let h, w = getRotatedHAndW sym
    let portGap = calculateBasedOnSide side
                    (fun () -> float h / (portDimension + 2.0 * gap))       // For Left or Right
                    (fun () -> float w / (portDimension + 2.0 * topBottomGap)) // For Top or Bottom

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

//*yc3521: inline wireSyms record Construction
//         defined a helper function to get the edge of a wire connected to a symbol
//         making pipelline more clear and readable
/// Try to get two ports that are on opposite edges.
let getOppEdgePortInfo
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    : (PortInfo * PortInfo) option =
    let wire = wiresBtwnSyms wModel symbolToSize otherSymbol

    let tryGetOppEdgePorts (wire: Wire) =
        let wireSyms = {
            SymA = symbolToSize;
            SymB = otherSymbol;
            Wire = wire 
            }
        let portA, portB = getPortAB wModel wireSyms
        let edgeA = getSymbolPortOrientation wireSyms.SymA portA
        let edgeB = getSymbolPortOrientation wireSyms.SymB portB

        match edgeA = edgeB.Opposite with
        | true -> Some(makePortInfo wireSyms.SymA portA, makePortInfo wireSyms.SymB portB)
        | _ -> None

    wire
    |> List.tryPick tryGetOppEdgePorts //*yc3521: making the pipeline more succinct

let alignPortsOffset (movePInfo: PortInfo) (otherPInfo: PortInfo) =
    let getPortRealPos pInfo =
        getPortPos pInfo.sym pInfo.port + pInfo.sym.Pos

    let movePortPos = getPortRealPos movePInfo
    let otherPortPos = getPortRealPos otherPInfo
    let posDiff = otherPortPos - movePortPos

    calculateBasedOnSide movePInfo.side
        (fun () -> { X = posDiff.X; Y = 0.0 })  // For Top or Bottom
        (fun () -> { X = 0.0; Y = posDiff.Y })  // For Left or Right

//*yc3521: break down the logic of alignSymbols and encapsulate in attemptAlighSymbols
//         Seperate Handling logic from the calculation logic, abstracting two logics
//         renameing: movedSymbol and updatedModel to indicate their actual purpose
//         Define a helper function to encapsulate updating the model and it can be used in following functions to avoid repetition
let updateModel (model: BusWireT.Model) (symbolId: ComponentId) (movedSymbol: Symbol) : BusWireT.Model =
    let updatedModel = Optic.set (symbolOf_ symbolId) movedSymbol model
    BusWireSeparate.routeAndSeparateSymbolWires updatedModel symbolId
let alignSymbols
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    : BusWireT.Model =

    let AlignAndRouteSymbols movePortInfo otherPortInfo =
        let offset = alignPortsOffset movePortInfo otherPortInfo
        let movedSymbol = moveSymbol offset symbolToSize
        updateModel wModel symbolToSize.Id movedSymbol

    let handleAlignmentResult = function
        | None -> wModel //*yc3521: No alignment needed, return original model
        | Some(portInfoPair) -> AlignAndRouteSymbols (fst portInfoPair) (snd portInfoPair)

    //*yc3521: Retrieve port information and handle the alignment result
    getOppEdgePortInfo wModel symbolToSize otherSymbol
    |> handleAlignmentResult



/// HLP23: To test this, it must be given two symbols interconnected by wires. It then resizes symbolToSize
/// so that the connecting wires are exactly straight
/// HLP23: It should work out the interconnecting wires (wires) from
/// the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has adjustable size).

// *yc3521: seperate the calculation logic(get port and move symbol) from the main logic

let reSizeSymbol (wModel: BusWireT.Model) (symbolToSize: Symbol) (otherSymbol: Symbol) : (Symbol) =
    let wires = wiresBtwnSyms wModel symbolToSize otherSymbol

    // Try to get two ports that are on opposite edges, if none found just use any two ports.
    // *yc3521: Function to extract port information based on availability of opposite edges
    let getResizeAndOtherPortInfo () =
        match getOppEdgePortInfo wModel symbolToSize otherSymbol with
        | None ->
            let pA, pB = getPortAB wModel { SymA = symbolToSize; SymB = otherSymbol; Wire = wires.Head }
            (makePortInfo symbolToSize pA, makePortInfo otherSymbol pB)
        | Some(pIA, pIB) -> (pIA, pIB)

    //*yc3521: Function to resize and potentially move the symbol based on its type
    let resizeAndMoveSymbol (resizePortInfo, otherPortInfo) =

        let h, w = calculateBasedOnSide resizePortInfo.side
                    (fun () -> otherPortInfo.portGap * (resizePortInfo.portDimension + 2.0 * resizePortInfo.gap), resizePortInfo.w)
                    (fun () -> resizePortInfo.h, otherPortInfo.portGap * (resizePortInfo.portDimension + 2.0 * resizePortInfo.topBottomGap))

        match symbolToSize.Component.Type with
        | Custom _ ->
            let scaledSymbol = setCustomCompHW h w symbolToSize
            let scaledInfo = makePortInfo scaledSymbol resizePortInfo.port
            let offset = alignPortsOffset scaledInfo otherPortInfo
            moveSymbol offset scaledSymbol
        | _ -> symbolToSize

    //*yc3521: Main logic flow
    getResizeAndOtherPortInfo ()
    |> resizeAndMoveSymbol

//*yc3521: use of updateModel function accroding to DRY Principle
/// For UI to call ResizeSymbol.
let reSizeSymbolTopLevel
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    : BusWireT.Model =
    printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"

    let scaledSymbol = reSizeSymbol wModel symbolToSize otherSymbol

    updateModel wModel symbolToSize.Id scaledSymbol



// jz921:
// ---------------------------- improvement part 2 ----------------------------

/// For each edge of the symbol, store a count of how many connections it has to other symbols.
type SymConnDataT =
    { ConnMap: Map<ComponentId * Edge, int> }

/// If a wire between a target symbol and another symbol connects opposite edges, return the edge that the wire is connected to on the target symbol 
let tryWireSymOppEdge (wModel: Model) (wire: Wire) (sym: Symbol) (otherSym: Symbol) =
    let symEdge = wireSymEdge wModel wire sym
    let otherSymEdge = wireSymEdge wModel wire otherSym

    if symEdge = otherSymEdge.Opposite then Some symEdge else None

/// Updates the connection count for a given edge and component ID in the symbol connection data. 
let updateOrInsert (symConnData: SymConnDataT) (edge: Edge) (cid: ComponentId) =
    let updateCount = function
    | Some count -> Some (count + 1)
    | None -> Some 1

    { ConnMap = Map.change (cid, edge) updateCount symConnData.ConnMap }

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


// hp921:
// ---------------------------- improvement part 3 ----------------------------

// Reduce Code Duplication 
// The getRotatedHAndW function is called twice for both maxXsym and maxYsym. It's more efficient to call it once and store the result.
// Abstract the repetitive logic of finding min/max X/Y values into reusable functions or inline expressions.
/// <summary> 
/// Calculates the bounding box for a list of symbols, considering their rotations.
/// </summary>
/// <param name="symbols">The list of symbols to calculate the bounding box for.</param>
/// <returns>A BoundingBox structure containing the top left corner, width, and height.</returns>

let getBlock (symbols: Symbol list): BoundingBox =
    let calculateBounds (acc: float * float * float * float) (symbol: Symbol) =
        let (height, width) = getRotatedHAndW symbol
        let maxX, minX, maxY, minY = acc
        let newMaxX = max maxX (symbol.Pos.X + width)
        let newMinX = min minX symbol.Pos.X
        let newMaxY = max maxY (symbol.Pos.Y + height)
        let newMinY = min minY symbol.Pos.Y
        (newMaxX, newMinX, newMaxY, newMinY)

    let initialBounds = (System.Double.MinValue, System.Double.MaxValue, System.Double.MinValue, System.Double.MaxValue)
    let (maxX, minX, maxY, minY) = List.fold calculateBounds initialBounds symbols

    { TopLeft = { X = minX; Y = minY }; W = maxX - minX; H = maxY - minY }



// This function was already clean and consize
/// <summary>
/// Rotates a point about a specified center point by a given rotation angle.
/// </summary>
/// <param name="point">The point to rotate.</param>
/// <param name="center">The center point to rotate about.</param>
/// <param name="rotation">The rotation angle (0, 90, 180, 270 degrees).</param>
/// <returns>The rotated point as a new XYPos structure.</returns>
let rotatePointAboutBlockCentre (point: XYPos) (center: XYPos) (rotation: Rotation) =
    let toRelativeCentre = (fun point -> point - center)
    let rotateAroundCenter (point: XYPos) =
        match rotation with
        | Degree0 -> point
        | Degree90 -> { X = point.Y; Y = -point.X }
        | Degree180 -> { X = -point.X; Y = -point.Y }
        | Degree270 -> { X = -point.Y; Y = point.X }
    let relativeToTopLeft = (fun p -> center - p)

    point
    |> toRelativeCentre
    |> rotateAroundCenter
    |> relativeToTopLeft



// Removed Unnecessary Parentheses: by simplified mathematical expressions center.X - (point.X - center.X) and center.Y - (point.Y - center.Y)
/// <summary>
/// Flips a point about a specified center point horizontally or vertically.
/// </summary>
/// <param name="point">The point to flip.</param>
/// <param name="center">The center point to flip about.</param>
/// <param name="flip">The type of flip to apply (horizontal or vertical).</param>
/// <returns>The flipped point as a new XYPos structure.</returns>
let flipPointAboutBlockCentre 
    (point:XYPos) 
    (center:XYPos) 
    (flip:FlipType) = 
    match flip with
    | FlipHorizontal -> 
        { X = center.X - point.X + center.X; Y = point.Y }
    | FlipVertical -> 
        { X = point.X; Y = center.Y + center.Y - point.Y }


// Parameter Naming: For clarity, Renamed h to height and w to width. Renamed pos to position for consistency.
// change 0 to 0.0 to reduce replication of (float).
/// <summary>
/// Adjusts the position of a point based on a specified rotation, considering its dimensions.
/// </summary>
/// <param name="rotation">The rotation to apply (0, 90, 180, 270 degrees).</param>
/// <param name="height">The height of the block before rotation.</param>
/// <param name="width">The width of the block before rotation.</param>
/// <param name="position">The original top left position of the block.</param>
/// <returns>The new top left position of the block after rotation.</returns>
let adjustPosForBlockRotation 
    (rotation: Rotation) 
    (height: float) 
    (width: float) 
    (position: XYPos)
    : XYPos =
    let offset =
        match rotation with
        | Degree0 -> { X = 0.0; Y = 0.0 }
        | Degree90 -> { X = height; Y = 0.0 }
        | Degree180 -> { X = width; Y = -height }
        | Degree270 -> { X = 0.0; Y = width }
    position - offset




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

// jw3621:
// ---------------------------- improvement part 4 ----------------------------
/// <summary>Rotate a symbol around the block centre.</summary>
/// <param name="rotation">Clockwise or Anticlockwise rotation</param>
/// <param name="blockCentre">Centre of the block</param>
/// <param name="sym">Symbol to rotate</param>
/// <returns>Symbol after rotation, with updated position and transformation.</returns>

// improvements:
// 1. Use pipelines for readability.

let rotateSymbolInBlock 
        (rotation: Rotation) 
        (blockCentre: XYPos)
        (sym: Symbol)  : Symbol =
      
    let h,w = getRotatedHAndW sym

    let newTopLeft = 
        rotation
        |> invertRotation
        |> rotatePointAboutBlockCentre sym.Pos blockCentre 
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

// 1. Use pipelines for readability.
// 2. Function Decomposition: Extracted the applyFlip function to improve readability
let flipSymbolInBlock
    (flip: FlipType)
    (blockCentre: XYPos)
    (sym: Symbol) : Symbol =

    let h,w = getRotatedHAndW sym
    //Needed as new symbols and their components need their Pos updated (not done in regular flip symbol)
    let newTopLeft = 
        flip
        |> flipPointAboutBlockCentre sym.Pos blockCentre
        |> adjustPosForBlockFlip flip h w

    let portOrientation = 
        sym.PortMaps.Orientation |> Map.map (fun id side -> flipSideHorizontal side)

    let flipPortList currPortOrder side =
        currPortOrder |> Map.add (flipSideHorizontal side ) sym.PortMaps.Order[side]

    let portOrder = 
        (Map.empty, [Edge.Top; Edge.Left; Edge.Bottom; Edge.Right]) 
        ||> List.fold flipPortList
        |> Map.map (fun edge order -> List.rev order)       

    let newSTransform = 
        {Flipped= not sym.STransform.Flipped;
        Rotation= sym.STransform.Rotation} 

    let newcomponent = {sym.Component with X = newTopLeft.X; Y = newTopLeft.Y}

    let applyFlip (flip:FlipType) (sym: Symbol) =
        match flip with
        | FlipHorizontal -> sym
        | FlipVertical -> 
            let newblock = getBlock [sym]
            let newblockCenter = newblock.Centre()
            sym
            |> rotateSymbolInBlock Degree270 newblockCenter 
            |> rotateSymbolInBlock Degree270 newblockCenter

    { sym with
        Component = newcomponent
        PortMaps = {Order=portOrder;Orientation=portOrientation}
        STransform = newSTransform
        LabelHasDefaultPos = true
        Pos = newTopLeft
    }
    |> calcLabelBoundingBox
    |> applyFlip flip


/// <summary>HLP 23: AUTHOR Ismagilov - Scales selected symbol up or down.</summary>
/// <param name="scaleType"> Scale up or down. Scaling distance is constant</param>
/// <param name="block"> Bounding box of selected components</param>
/// <param name="sym"> Symbol to be rotated</param>
/// <returns>New symbol after scaled about block centre.</returns>

// 1. define variables on different line for readability
let scaleSymbolInBlock
    //(Mag: float)
    (scaleType: ScaleType)
    (block: BoundingBox)
    (sym: Symbol) : Symbol =

    let symCenter = getRotatedSymbolCentre sym

    //Get x and y proportion of symbol to block
    let xProp = (symCenter.X - block.TopLeft.X) / block.W
    let yProp = (symCenter.Y - block.TopLeft.Y) / block.H

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

let rotateSymbolByDegree (degree: Rotation) (sym:Symbol)  =
    let pos = {X = sym.Component.X + sym.Component.W / 2.0 ; Y = sym.Component.Y + sym.Component.H / 2.0 }
    match degree with
    | Degree0 -> sym
    | _ ->  rotateSymbolInBlock degree pos sym

    
// dy321:
// ---------------------------- improvement part 5 ----------------------------

//*dy321: improved namings (to represent meanings);
//        simplified and compressed some functions and expressions, removed unnecessary ones;
//        more readable formatting (with pipelines).
/// <summary>HLP 23: AUTHOR Ismagilov - Rotates a block of symbols, returning the new symbol model</summary>
/// <param name="compList"> List of ComponentId's of selected components</param>
/// <param name="model"> Current symbol model</param>
/// <param name="rotation"> Type of rotation to do</param>
/// <returns>New rotated symbol model</returns>
let rotateBlock (compIdList:ComponentId list) (model:SymbolT.Model) (rotation:Rotation) = 

    printfn "running rotateBlock"
    let selectedSymbols =
        compIdList 
        |> List.map (fun compId -> model.Symbols[compId])

    //Get block properties of selected symbols
    let block = getBlock selectedSymbols

    //Rotated symbols about the center
    let newSelectedSymbolsMap = 
        selectedSymbols
        |> List.map (fun sym -> rotateSymbolInBlock (invertRotation rotation) (block.Centre()) sym) 
        |> List.zip compIdList

    //Return updated model with block of rotated selected symbols
    {model with Symbols = 
                (model.Symbols, newSelectedSymbolsMap)
                ||> List.fold (fun prevMap (compId,sym) -> Map.add compId sym prevMap)
    }

//*dy321: add a helper function to be used in oneCompBoundsBothEdges and getScalingFactorAndOffsetCentreGroup.
/// Return the requested symbol boundary (4 cases: left-x, right-x, top-y, bottom-y)
let getSymBounds (dirIsX:bool) (wHOffset:float) (sym: Symbol) = 
        let height,width = getRotatedHAndW sym
        match dirIsX with
        | true -> sym.Pos.X + wHOffset*width
        | false -> sym.Pos.Y + wHOffset*height

//*dy321: reduced repetition using "function wrapping". (but perhaps less readable?)
//        (The original code here contained four highly repetitive assignments,
//         which have been shrinked into one single subfunction being utilised twice.)
/// Check whether there exists a single component that is on the boundaries of two opposite edges of selected area.
let oneCompBoundsBothEdges (selectedSymbols: Symbol list) = 
    let checkIfTwoBoundsSameComp (dirIsX:bool) =
        let maxCentre =
            selectedSymbols
            |> List.maxBy (getSymBounds dirIsX 1.) 
            |> getRotatedSymbolCentre
        let minCentre =
            selectedSymbols
            |> List.minBy (getSymBounds dirIsX 0.) 
            |> getRotatedSymbolCentre
        match dirIsX with
        | true -> maxCentre.X = minCentre.X
        | false -> maxCentre.Y = minCentre.Y
    (checkIfTwoBoundsSameComp true) || (checkIfTwoBoundsSameComp false)

//*dy321: improved namings (to represent meanings);
//        simplified expressions;
//        more readable formatting (with pipelines).
/// Return symbols corresponding to the input ComponentId list
let findSelectedSymbols (compList: ComponentId list) (model: SymbolT.Model) = 
    compList
    |> List.map (fun compId -> model.Symbols[compId])

//*dy321: changed if-then-else to match to represent different cases clearer.
/// Calculate scaling factor and offset centre using provided float values
let getScalingFactorAndOffsetCentre (min:float) (matchMin:float) (max:float) (matchMax:float) = 
    let scaleFact = 
        match min = max || matchMax <= matchMin with
        | true -> 1. 
        | false -> (matchMin - matchMax) / (min - max)
    let offsetC = 
        match scaleFact with 
        | 1. -> 0.
        | _  -> (matchMin - min * scaleFact) / (1.-scaleFact)
    (scaleFact, offsetC)

//*dy321: reduced repetition using "function wrapping";
//        improved namings (to represent meanings).
//        (The original code here contained four sets of highly repetitive assignments,
//         which have been shrinked into one single subfunction being utilised twice.)
/// Return set of floats that define how a group of components is scaled
let getScalingFactorAndOffsetCentreGroup
    (matchBBMin:XYPos)
    (matchBBMax:XYPos)
    (selectedSymbols: Symbol list) : ((float * float) * (float * float)) = 

    let getScaleAndOffsetInDir dirIsX =
        let hOrW =
            match dirIsX with
            | true -> snd
            | false -> fst
        let xOrY (xYPos:XYPos) =
            match dirIsX with
            | true -> xYPos.X
            | false -> xYPos.Y

        let maxSym = 
            selectedSymbols
            |> List.maxBy (getSymBounds dirIsX 1.) 
        let oldMax = xOrY (getRotatedSymbolCentre maxSym)
        let newMax = xOrY matchBBMax - (hOrW (getRotatedHAndW maxSym))/2.

        let minSym =
            selectedSymbols
            |> List.minBy (getSymBounds dirIsX 0.)
        let oldMin = xOrY (getRotatedSymbolCentre minSym)
        let newMin = xOrY matchBBMin + (hOrW (getRotatedHAndW minSym))/2.

        getScalingFactorAndOffsetCentre oldMin  newMin oldMax newMax
    
    (getScaleAndOffsetInDir true, getScaleAndOffsetInDir false)


// az1821:
// ---------------------------- improvement part 6 ----------------------------
(* 
    This part of the code is analysed and mainly improved on layout & pipelines. 
    Further improvements could be done by changing data type but would have to modify the code structure a lot. 
    See https://github.com/dyu18/hlp24-project-issie-team7/tree/indiv-az1821/README-Indiv-notes.md for more detailed documentation. 
*)

/// Alter position of one symbol as needed in a scaling operation
let scaleSymbol
        (xYSC: (float * float) * (float * float))
        (sym: Symbol)
        : Symbol = 
    let symCentre = sym |> getRotatedSymbolCentre    // improved readability
    let translateFunc scaleFact offsetC coordinate = (coordinate - offsetC) * scaleFact + offsetC
    let xSC, ySC = xYSC
    let newX = symCentre.X |> translateFunc (fst xSC) (snd xSC)
    let newY = symCentre.Y |> translateFunc (fst ySC) (snd ySC)

    let symCentreOffsetFromTopLeft = { X = (snd (getRotatedHAndW sym))/2.; 
                                                Y = (fst (getRotatedHAndW sym))/2.}
    let newTopLeftPos = {X = newX; Y = newY} - symCentreOffsetFromTopLeft
    let newComp = {sym.Component with X = newTopLeftPos.X;  Y = newTopLeftPos.Y}

    {sym with Pos = newTopLeftPos; Component = newComp; LabelHasDefaultPos = true}

/// Part of the rotate and scale code       
let groupNewSelectedSymsModel
    (compList:ComponentId list) 
    (model:SymbolT.Model) 
    (selectedSymbols: Symbol list)
    (modifySymbolFunc) = 

    let UnselectedSymbols = model.Symbols |> Map.filter (fun x _ -> not (List.contains x compList))
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
/// 
/// 1. Used pattern matching for clarity and readability where applicable.
/// 2. Simplified variable naming for better readability.
let postUpdateScalingBox (model: SheetT.Model, cmd) = 

    let symbolCmd (msg: SymbolT.Msg) = Elmish.Cmd.ofMsg (ModelType.Msg.Sheet (SheetT.Wire (BusWireT.Symbol msg)))
    let sheetCmd (msg: SheetT.Msg) = Elmish.Cmd.ofMsg (ModelType.Msg.Sheet msg)

    match model.SelectedComponents.Length with
    | length when length < 2 ->
        match model.ScalingBox with 
        | None -> model, cmd
        | _ -> 
            let deleteSymbolsCmd = 
                model.ScalingBox.Value.ButtonList 
                |> SymbolT.DeleteSymbols 
                |> symbolCmd 
            let cmds =
                [ deleteSymbolsCmd
                  sheetCmd SheetT.UpdateBoundingBoxes
                ]
                |> List.append [cmd]
                |> Elmish.Cmd.batch
            { model with ScalingBox = None }, cmds
    | _ ->
        let newBoxBound = 
            model.SelectedComponents
            |> List.map (fun id -> model.Wire.Symbol.Symbols |> Map.find id)
            |> getBlock

        match model.ScalingBox with 
        | Some value when value.ScalingBoxBound = newBoxBound -> model, cmd
        | _ -> 
            let topleft = newBoxBound.TopLeft
            let rotateDeg90OffSet: XYPos = { X = newBoxBound.W + 57.0; Y = (newBoxBound.H / 2.0) - 12.5 }
            let rotateDeg270OffSet: XYPos = { X = -69.5; Y = (newBoxBound.H / 2.0) - 12.5 }
            let buttonOffSet: XYPos = { X = newBoxBound.W + 47.5; Y = -47.5 }
            let dummyPos = topleft + buttonOffSet

            let makeButton = SymbolUpdate.createAnnotation ThemeType.Colourful
            let buttonSym = 
                makeButton ScaleButton dummyPos
                |> fun sym -> { sym with Pos = dummyPos }
            let makeRotateSym sym = 
                { sym with Component = { sym.Component with H = 25.0; W = 25.0 } }
            let rotateDeg90Sym = 
                makeButton (RotateButton Degree90) (topleft + rotateDeg90OffSet)
                |> makeRotateSym
            let rotateDeg270Sym = 
                makeButton (RotateButton Degree270) (topleft + rotateDeg270OffSet) 
                |> fun sym -> 
                    { sym with 
                        STransform = { Rotation = Degree90; Flipped = false } 
                    }
                |> makeRotateSym

            let newSymbolMap = 
                model.Wire.Symbol.Symbols 
                |> Map.add buttonSym.Id buttonSym 
                |> Map.add rotateDeg270Sym.Id rotateDeg270Sym 
                |> Map.add rotateDeg90Sym.Id rotateDeg90Sym

            let initScalingBox: SheetT.ScalingBox = 
                { ScalingBoxBound = newBoxBound
                  ScaleButton = buttonSym
                  RotateDeg90Button = rotateDeg90Sym
                  RotateDeg270Button = rotateDeg270Sym
                  ButtonList = [ buttonSym.Id; rotateDeg270Sym.Id; rotateDeg90Sym.Id ]
                }

            let newCmd =
                match model.ScalingBox with
                | Some _ -> 
                    [ symbolCmd (SymbolT.DeleteSymbols (model.ScalingBox.Value.ButtonList))
                      sheetCmd SheetT.UpdateBoundingBoxes
                    ]
                    |> List.append [cmd]
                    |> Elmish.Cmd.batch
                | None -> cmd
            model
            |> Optic.set SheetT.scalingBox_ (Some initScalingBox)
            |> Optic.set SheetT.symbols_ newSymbolMap, 
            newCmd