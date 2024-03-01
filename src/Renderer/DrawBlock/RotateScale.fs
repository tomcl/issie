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
    It requires better documentation of the parts now used.
*)
(*
//IMPROVEMENTS TO ROTATESCALE:
//IMPROVEMENTS TO FUNCTIONS:
    - alignPortsOffset:
        Functional Abstraction, separated the original function into smaller functions for better readability and maintainability
        Pipelines, used pipelines to make reading easier.
    - getBlock:
        Helper functions, created helper function getMaxMin to avoid duplicate calculations within the function.
        Functional Abstraction, helper functions allowed for minimising computation within the getBlock function for better readability.
        Discriminated Union, created a DU type MaxMinType so it is easier to manage Max and Min calculations
        Match Case Compression, used match cases to allow for simpler and more intuitive viewing of the calculations
    (The 3 below functions are outside my allocated section of rotateScale,
     but I tried them anyway because there were not enough I could change in my section, and they were similar to getBlock)
    - oneCompoundsBothEdges:
        Helper Functions, findMaxMin function avoids duplicated calculations within the function
        Functional Abstraction, same as previous function
        Match Case Compression, same as previous function
    - getScaleFactorAndOffsetCentre:
        Match (using guards), used match cases to avoid using if. More visually appealing, and follows the Issie Guidelines
    - getScaleFactorAndOffsetCentreGroup
        Helper Functions
        Functional Abstraction
        Match Case Compression
//FUNCTION NAME CHANGES:
    - getScalingFactorAndOffsetCentre -> getScaleFactorAndOffsetCentre // consistency with scaleFact variable within the function
    - getScalingFactorAndOffsetCentreGroup -> getScaleFactorAndOffsetCentreGroup //consistency with above
    - oneCompBoundsBothEdges -> selectedSymbolsBoundedByBothEdges (suggested, not actually implemeted in the code as it is referenced multiple times in other files)
//VARIABLE NAME CHANGES:
    - wModel -> wireModel, minor change, but a lot clearer, does not hurt to change. Referenced throughout rotateScale
    - symbolToResize -> sourceSymbol, original name was not quite clear. Referenced throughout rotateScale
    - (updateOrInsert) m -> map, minor change, but does not hurt
//XML COMMENTS:
    - alignPortsOffset
    - alignSymbols
    - tryWireSymOppEdge
    - updateOrInsert
    - getMaxMin
    - findMaxMinForScale
    - findNewMaxMin
    added more context to these functions


    
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

let getPortAB wireModel wireSyms =
    let ports = portsOfWires wireModel [ wireSyms.Wire ]
    let portA = filterPortBySym ports wireSyms.SymA |> List.head
    let portB = filterPortBySym ports wireSyms.SymB |> List.head
    portA, portB

/// Try to get two ports that are on opposite edges.
let getOppEdgePortInfo
    (wireModel: BusWireT.Model)
    (sourceSymbol: Symbol)
    (otherSymbol: Symbol)
    : (PortInfo * PortInfo) option =
    let wires = wiresBtwnSyms wireModel sourceSymbol otherSymbol

    let tryGetOppEdgePorts wireSyms =
        let portA, portB = getPortAB wireModel wireSyms
        let edgeA = getSymbolPortOrientation wireSyms.SymA portA
        let edgeB = getSymbolPortOrientation wireSyms.SymB portB

        match edgeA = edgeB.Opposite with
        | true -> Some(makePortInfo wireSyms.SymA portA, makePortInfo wireSyms.SymB portB)
        | _ -> None

    wires
    |> List.tryPick (fun w ->
        tryGetOppEdgePorts
            { SymA = sourceSymbol
              SymB = otherSymbol
              Wire = w })

//CHANGEs TO alignPortsOffset:
///Helper function to calculate real position of port on a Symbol
///Adds the position of the port relative to symbol's original position
let calculatePortRealPos (pInfo: PortInfo) =
    getPortPos pInfo.sym pInfo.port + pInfo.sym.Pos

///Helper function to calculate offset between two port positions
let calculateOffset (otherPortPos: XYPos) (movePortPos: XYPos) =
    { X = otherPortPos.X - movePortPos.X; Y = otherPortPos.Y - movePortPos.Y }

///Helper function to determine direction of the offset based on side of symbol where port is located
let determineOffsetDirection (side: Edge) (posDiff: XYPos) =
    match side with
    | Top | Bottom -> { X = posDiff.X; Y = 0.0 }
    | Left | Right -> { X = 0.0; Y = posDiff.Y }

///calculates the offset needed to move the moving port to align it with the other port
let alignPortsOffset (movePInfo: PortInfo) (otherPInfo: PortInfo) =
    calculatePortRealPos movePInfo
    |> calculateOffset (calculatePortRealPos otherPInfo)
    |> determineOffsetDirection movePInfo.side

///Aligns symbols in a circuit diagram, ensuring correct position and wire connections
let alignSymbols
    (wireModel: BusWireT.Model)
    (sourceSymbol: Symbol)
    (otherSymbol: Symbol)
    : BusWireT.Model =

    // Only attempt to align symbols if they are connected by ports on parallel edges.
    match getOppEdgePortInfo (wireModel:BusWireT.Model) sourceSymbol otherSymbol with
    | None -> wireModel
    | Some(movePortInfo, otherPortInfo) ->
        let offset = alignPortsOffset movePortInfo otherPortInfo
        let symbol' = moveSymbol offset sourceSymbol
        let model' = Optic.set (symbolOf_ sourceSymbol.Id) symbol' wireModel
        BusWireSeparate.routeAndSeparateSymbolWires model' sourceSymbol.Id


/// HLP23: To test this, it must be given two symbols interconnected by wires. It then resizes symbolToSize
/// so that the connecting wires are exactly straight
/// HLP23: It should work out the interconnecting wires (wires) from
/// the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has adjustable size).
let reSizeSymbol (wireModel: BusWireT.Model) (sourceSymbol: Symbol) (otherSymbol: Symbol) : (Symbol) =
    let wires = wiresBtwnSyms wireModel sourceSymbol otherSymbol

    // Try to get two ports that are on opposite edges, if none found just use any two ports.
    let resizePortInfo, otherPortInfo =
        match getOppEdgePortInfo wireModel sourceSymbol otherSymbol with
        | None ->
            let pA, pB = getPortAB wireModel { SymA = sourceSymbol; SymB = otherSymbol; Wire = wires[0] }
            makePortInfo sourceSymbol pA, makePortInfo sourceSymbol pB
        | Some(pIA, pIB) -> (pIA, pIB)

    let h, w =
        match resizePortInfo.side with
        | Left | Right ->
            otherPortInfo.portGap * (resizePortInfo.portDimension + 2.0 * resizePortInfo.gap), resizePortInfo.w
        | Top | Bottom ->
            resizePortInfo.h, otherPortInfo.portGap * (resizePortInfo.portDimension + 2.0 * resizePortInfo.topBottomGap)

    //possibly pipeline? 
    match sourceSymbol.Component.Type with
    | Custom _ ->
        let scaledSymbol = setCustomCompHW h w sourceSymbol
        let scaledInfo = makePortInfo scaledSymbol resizePortInfo.port
        let offset = alignPortsOffset scaledInfo otherPortInfo
        moveSymbol offset scaledSymbol
    | _ ->
        sourceSymbol


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

/// If a wire between a target symbol and another symbol connects opposite edges,
/// return the edge that the wire is connected to on the target symbol 
let tryWireSymOppEdge (wireModel: Model) (wire: Wire) (sym: Symbol) (otherSym: Symbol) =
    let symEdge = wireSymEdge wireModel wire sym
    let otherSymEdge = wireSymEdge wireModel wire otherSym

    match symEdge = otherSymEdge.Opposite with
    | true -> Some symEdge
    | _ -> None


///Update or Insert an entry into the connection map
let updateOrInsert (symConnData: SymConnDataT) (edge: Edge) (cid: ComponentId) =
    let map = symConnData.ConnMap
    let count = Map.tryFind (cid, edge) map |> Option.defaultValue 0 |> (+) 1
    { ConnMap = Map.add (cid, edge) count map }

// TODO: this is copied from Sheet.notIntersectingComponents. It requires SheetT.Model, which is not accessible from here. Maybe refactor it.
let noSymbolOverlap (boxesIntersect: BoundingBox -> BoundingBox -> bool) boundingBoxes sym =
    let symBB = getSymbolBoundingBox sym

    boundingBoxes
    |> Map.filter (fun sId boundingBox -> boxesIntersect boundingBox symBB && sym.Id <> sId)
    |> Map.isEmpty



/// Finds the optimal size and position for the selected symbol w.r.t. to its surrounding symbols.
let optimiseSymbol
    (wireModel: BusWireT.Model)
    (symbol: Symbol)
    (boundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>)
    : BusWireT.Model =

    // If a wire connects the target symbol to another symbol, note which edge it is connected to
    let updateData (symConnData: SymConnDataT) _ (wire: Wire) =
        let symS, symT = getSourceSymbol wireModel wire, getTargetSymbol wireModel wire

        let otherSymbol =
            match symS, symT with
            | _ when (symS.Id <> symbol.Id) && (symT.Id = symbol.Id) -> Some symS
            | _ when (symS = symbol) && (symT <> symbol) -> Some symT
            | _ -> None

        match otherSymbol with
        | Some otherSym ->
            let edge = tryWireSymOppEdge wireModel wire symbol otherSym

            match edge with
            | Some e -> updateOrInsert symConnData e otherSym.Id
            | None -> symConnData // should not happen
        | None -> symConnData 

    // Look through all wires to build up SymConnDataT.
    let symConnData = ({ ConnMap = Map.empty }, wireModel.Wires) ||> Map.fold updateData

    let tryResize (symCount: ((ComponentId * Edge) * int) array) sym =

        let alignSym (sym: Symbol) (otherSym: Symbol) =
            let resizedSym = reSizeSymbol wireModel sym otherSym
            let noOverlap = noSymbolOverlap DrawHelpers.boxesIntersect boundingBoxes resizedSym

            match noOverlap with
            | true -> true, resizedSym
            | _ -> false, sym

        let folder (hAligned, vAligned, sym) ((cid, edge), _) =
            let otherSym = Optic.get (symbolOf_ cid) wireModel       

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

    let model' = Optic.set (symbolOf_ symbol.Id) scaledSymbol wireModel
    BusWireSeparate.routeAndSeparateSymbolWires model' symbol.Id


//IMPROVEMENTS TO getBlock

///DU to help find max and min
type MaxMinType =
    | Max
    | Min

/// Helper function to find the maximum and minimum X and Y coord of symbols
let getMaxMin (symbols: Symbol list) (maxMinType: MaxMinType) =
    match maxMinType with
    | Max ->
        let maxX =
            symbols
            |> List.maxBy (fun sym -> sym.Pos.X + snd (getRotatedHAndW sym))
            |> (fun maxXsym -> maxXsym.Pos.X + snd (getRotatedHAndW maxXsym))

        let maxY =
            symbols
            |> List.maxBy (fun sym -> sym.Pos.Y + fst (getRotatedHAndW sym))
            |> (fun maxYsym -> maxYsym.Pos.Y + fst (getRotatedHAndW maxYsym))
        maxX, maxY

    | Min ->
        let minX = (List.minBy (fun (x:Symbol) -> x.Pos.X) symbols).Pos.X
        let minY = (List.minBy (fun (x:Symbol) -> x.Pos.Y) symbols).Pos.Y
        minX, minY



/// <summary>HLP 23: AUTHOR Ismagilov - Get the bounding box of multiple selected symbols</summary>
/// <param name="symbols"> Selected symbols list</param>
/// <returns>Bounding Box</returns>
let calcSymbolBoundingBox
        (symbols:Symbol List) :BoundingBox = 

    let maxX, maxY = getMaxMin symbols Max
    let minX, minY = getMaxMin symbols Min

    {TopLeft = {X = minX; Y = minY}; W = maxX-minX; H = maxY-minY}
    //getBLOCK simplified
    //reasons - a lot of repeated computations
    //more readable this way
    // name changed getBlock not an apt name for a function that returns bounding box
    // altered all instances of getBlock


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
        (w: float)
        (pos: XYPos)
         : XYPos =
    let posOffset =
        match rotation with
        | Degree0 -> {X = 0; Y = 0}
        | Degree90 -> {X=(float)h ;Y=0}
        | Degree180 -> {X= (float)w; Y= -(float)h}
        | Degree270 -> { X = 0 ;Y = (float)w }
    pos - posOffset


//--------------------------------------------------------------------------------------------------------------------------------------//
//-----------------------------------------------------------------TALHA PART END-------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------------------//


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
            let newblock = calcSymbolBoundingBox [sym]
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
    let block = calcSymbolBoundingBox SelectedSymbols

    //Rotated symbols about the center
    let newSymbols = 
        List.map (fun x -> rotateSymbolInBlock (invertRotation rotation) (block.Centre()) x) SelectedSymbols 

    //return model with block of rotated selected symbols, and unselected symbols
    {model with Symbols = 
                ((Map.ofList (List.map2 (fun x y -> (x,y)) compList newSymbols)
                |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols)
    )}


//CHANGES MADE TO OneCompoundsBothEdges
/// Helper functions to find Max Min, oldMax, oldMin for oneComBoundsBothEdges
let findMaxMinForScale (symbols: Symbol list) (maxMinType: MaxMinType) =
    match maxMinType with
    |Max -> let maxX =
                symbols
                |> List.maxBy (fun sym -> sym.Pos.X + snd (getRotatedHAndW sym)) 
                |> getRotatedSymbolCentre
            let oldMaxX = (maxX).X
            let maxY =
                symbols
                |> List.maxBy (fun (y:Symbol) -> y.Pos.Y+ fst (getRotatedHAndW y)) //maxYSym
                |> getRotatedSymbolCentre
            let oldMaxY = (maxY).Y
            maxX, oldMaxX, maxY, oldMaxY

    |Min -> let minX =
                symbols
                |> List.minBy (fun (x:Symbol) -> x.Pos.X)
                |> getRotatedSymbolCentre
            let oldMinX = (minX).X
            let minY =
                symbols
                |> List.minBy (fun (y:Symbol) -> y.Pos.Y)
                |> getRotatedSymbolCentre
            let oldMinY = (minY).Y
            minX, oldMinX, minY, oldMinY

//Simplified to avoid duplicate calculations
//name change suggestion: selectedSymbolsBoundedByBothEdges
let oneCompBoundsBothEdges (selectedSymbols: Symbol list) = 
    let maxXSymCentre, _, maxYSymCentre, _ = findMaxMinForScale selectedSymbols Max
    let minXSymCentre, _, minYSymCentre, _ = findMaxMinForScale selectedSymbols Min

    (maxXSymCentre.X = minXSymCentre.X) || (maxYSymCentre.Y = minYSymCentre.Y)


let findSelectedSymbols (compList: ComponentId list) (model: SymbolT.Model) = 
    List.map (fun x -> model.Symbols |> Map.find x) compList

//replaced if with match
//rename scaling to scale to match scaleFact subfunction
let getScaleFactorAndOffsetCentre (min: float) (matchMin: float) (max: float) (matchMax: float) = 
    let scaleFact =
        match min, max, matchMin, matchMax with
        | _, _, _, _ when min = max || matchMax <= matchMin -> 1.0
        | _, _, _, _ -> (matchMin - matchMax) / (min - max)
    
    let offsetC =
        match scaleFact with
        | 1.0 -> 0.0
        | _ -> (matchMin - min * scaleFact) / (1.0 - scaleFact)
    
    (scaleFact, offsetC)

/// Helper function to find newMax and newMin for getScaleFactorAndOffsetCentreGroup
let findNewMaxMin (symbols: Symbol list) (maxMinType: MaxMinType) (bb: XYPos)  =
    match maxMinType with
    | Max ->
        let maxXSym =
            symbols
            |> List.maxBy (fun (x: Symbol) -> x.Pos.X + snd (getRotatedHAndW x))
        let newMaxX = bb.X - (snd (getRotatedHAndW maxXSym)) / 2.
        let maxYSym = 
            symbols
            |> List.maxBy (fun (y:Symbol) -> y.Pos.Y+ fst (getRotatedHAndW y))
        let newMaxY = bb.Y - (fst (getRotatedHAndW maxYSym))/2. 
        newMaxX, newMaxY

    | Min ->
        let minXSym =
            symbols
            |> List.minBy (fun (x:Symbol) -> x.Pos.X)
        let newMinX = bb.X + (snd (getRotatedHAndW minXSym))/2.
        let minYSym =
            symbols
            |> List.minBy (fun (y:Symbol) -> y.Pos.Y)
        let newMinY = bb.Y + (fst (getRotatedHAndW minYSym))/2.
        newMinX, newMinY

/// Return set of floats that define how a group of components is scaled
//renamed to scale for consistency
let getScaleFactorAndOffsetCentreGroup
    (matchBBMin:XYPos)
    (matchBBMax:XYPos)
    (selectedSymbols: Symbol list) : ((float * float) * (float * float)) = 

    let _, oldMaxX, _, oldMaxY = findMaxMinForScale selectedSymbols Max
    let _, oldMinX, _, oldMinY = findMaxMinForScale selectedSymbols Min
    let newMaxX, newMaxY = findNewMaxMin selectedSymbols Max matchBBMax 
    let newMinX, newMinY = findNewMaxMin selectedSymbols Min matchBBMin
    
    let xSC = getScaleFactorAndOffsetCentre oldMinX newMinX oldMaxX newMaxX
    let ySC = getScaleFactorAndOffsetCentre oldMinY newMinY oldMaxY newMaxY
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
    
    let block = calcSymbolBoundingBox SelectedSymbols
  
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
            |> calcSymbolBoundingBox
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

