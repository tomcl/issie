module RotateScale
open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open SymbolUpdate
open Symbol
open Optics
open Operators
open Symbol
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
type SymbolWireConnections =
    { SymA: Symbol
      SymB: Symbol
      Wire: Wire }
    
/// For each edge of the symbol, store a count of how many connections it has to other symbols.
type SymConnDataT =
    { ConnMap: Map<ComponentId * Edge, int> }

/// Creates a SymbolWireConnections record
let createSymbolWireConnections (symA: Symbol) (symB: Symbol) (wire: Wire) : SymbolWireConnections =
    { SymA = symA
      SymB = symB
      Wire = wire }

// TODO: this is mostly copy pasted code from Symbol.getPortPos, perhaps abstract out the existing code there to use makePortInfo.
// Could not simply use getPortPos because more data (side, topBottomGap, etc.) is needed to caclulate the new dimensions of the resized symbol.
/// Returns PortInfo for a given symbol and port.
let makePortInfo  (port: Port) (sym: Symbol) : PortInfo =
    let side = getSymbolPortOrientation sym port
    let ports = sym.PortMaps.Order[side] //list of ports on the same side as port
    let gap = getPortPosEdgeGap sym.Component.Type
    let topBottomGap = gap + 0.3 // extra space for clk symbol
    let portDimension = float ports.Length - 1.0
    let h, w = getRotatedHAndW sym
    // more readable
    let portGap =
        match side with
        | Left   | Right -> float h / (portDimension + 2.0 * gap)
        | Bottom | Top -> float w / (portDimension + 2.0 * topBottomGap)

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

/// Returns a tuple of two connected ports (portA, portB) for a specified wire that links two symbols within a model.
let getPortsABConnectedByWire (wModel:Model) (wireSyms:SymbolWireConnections) : (Port*Port) option =
    let ports = portsOfWires wModel [ wireSyms.Wire ]
    //TODO decide whether to use List.tryFind or List.head.
    let findPortForSymbol sym = 
        ports |> List.tryFind (fun port -> ComponentId port.HostId = sym.Id)
    //let portA = filterPortBySym ports wireSyms.SymA |> List.head
    //let portB = filterPortBySym ports wireSyms.SymB |> List.head
    match findPortForSymbol wireSyms.SymA, findPortForSymbol wireSyms.SymB with
    | Some portA, Some portB -> Some (portA, portB)
    | _ -> None


/// Trys to return a tuple of portInfo for 2 ports that are on opposite edges of two symbols.
/// Returns a (PortInfo * PortInfo) option
let getOppEdgePortInfo
    (wModel: BusWireT.Model)
    (symbolA: Symbol) // renamed symbolToSize to symbolA
    (symbolB: Symbol) // renamed otherSymbol to symbolB
    : (PortInfo * PortInfo) option =
    // moved (wires = wiresBtwnSyms wModel symbolToSize otherSymbol) into a pipeline

    let tryGetOppEdgePorts wireSyms =
        match getPortsABConnectedByWire wModel wireSyms with
            | Some (portA, portB) -> 
                let edgeA = getSymbolPortOrientation wireSyms.SymA portA
                let edgeB = getSymbolPortOrientation wireSyms.SymB portB
                match edgeA = edgeB.Opposite with
                | true -> Some(makePortInfo portA wireSyms.SymA , makePortInfo portB wireSyms.SymB )
                | _ -> None
            | _ -> None
    // changed to createSymbolWireConnections to allow pipelining and prevent code duplication
    wiresBtwnSyms wModel symbolA symbolB
    |> List.tryPick (fun w ->
            createSymbolWireConnections symbolA symbolB w
            |> tryGetOppEdgePorts)

// used inline small function called one after another should a small performance benefit
// renamed to calcPortsOffset felt alignPortsOffset was a bit misleading
/// Calculates the offset required to align two ports on their respective symbols based on their positions and orientations.
/// First arguement is the reference port, second is the port to be moved.
let calcPortsOffset (refPInfo: PortInfo) (movePInfo: PortInfo) : XYPos =
    let inline getPortRealPos (pInfo:PortInfo) :XYPos =
        getPortPos pInfo.sym pInfo.port + pInfo.sym.Pos

    let movePortPos = getPortRealPos movePInfo
    let otherPortPos = getPortRealPos refPInfo
    let posDiff = otherPortPos - movePortPos

    // more readable
    match movePInfo.side with 
    | Top  | Bottom -> { X = posDiff.X; Y = 0.0 }
    | Left | Right -> { X = 0.0; Y = posDiff.Y }

// renamed to symbolToMove felt symbolToSize was not descriptive enough
/// Aligns two symbols based on their connection to ports on parallel edges within the given model.
/// Only attempts to align symbols if they are connected by  ports on parallel edges else returns the model unchanged.
let alignSymbols (wModel: BusWireT.Model) (symbolToMove: Symbol) (refSymbol: Symbol): BusWireT.Model =
    // Only attempt to align symbols if they are connected by ports on parallel edges.
    match getOppEdgePortInfo wModel symbolToMove refSymbol with
    | None -> wModel
    | Some(movePortInfo, refPortInfo) ->
        let symbol' =
            calcPortsOffset  refPortInfo movePortInfo
            |> (fun offset -> moveSymbol  offset symbolToMove)
        let model' = Optic.set (symbolOf_ symbolToMove.Id) symbol' wModel
        BusWireSeparate.routeAndSeparateSymbolWires model' symbolToMove.Id


/// Returns a resized symbol such that the wires connecting it to another symbol are straight.
/// If the symbol is not a Custom component, it returns the symbol unchanged.
/// Must provide symbols that are interconnected by wires
let reSizeCustomSymbol (wModel: BusWireT.Model) (symbolToReSize: Symbol) (refSymbol: Symbol) : (Symbol) =
    
    // Try to get two ports that are on opposite edges, if none found just use any two ports.
    let (optionResizePortInfo, optionRefPortInfo) =
        match getOppEdgePortInfo wModel symbolToReSize refSymbol with
        | None ->
            match wiresBtwnSyms wModel symbolToReSize refSymbol |> List.tryHead with
            | Some wire ->
                match( wire |>createSymbolWireConnections symbolToReSize refSymbol |>getPortsABConnectedByWire wModel) with
                | Some(pA, pB) ->
                        Some (makePortInfo pA symbolToReSize), Some (makePortInfo pB symbolToReSize)
                | None -> None, None
            | None -> None, None
        | Some(pIA, pIB) -> Some pIA,  Some pIB


    match symbolToReSize.Component.Type, optionResizePortInfo, optionRefPortInfo with
    | Custom _, Some resizePortInfo, Some  refPortInfo->
        let h, w =
            match resizePortInfo.side with
            | Left | Right ->
                refPortInfo.portGap * (resizePortInfo.portDimension + 2.0 * resizePortInfo.gap), resizePortInfo.w
            | Top | Bottom ->
                resizePortInfo.h, refPortInfo.portGap * (resizePortInfo.portDimension + 2.0 * resizePortInfo.topBottomGap)
        let scaledSymbol = setCustomCompHW h w symbolToReSize
        // added pipeline for better readability
        // changed functions to allow this

        scaledSymbol
        |> makePortInfo  resizePortInfo.port
        |> calcPortsOffset  refPortInfo
        |> (fun offset ->  moveSymbol offset scaledSymbol)
    | _ ->
        symbolToReSize
        
/// For UI to call ResizeSymbol.
let reSizeSymbolTopLevel (wModel: BusWireT.Model) (symbolToSize: Symbol) (otherSymbol: Symbol) : BusWireT.Model =
    printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"
    let scaledSymbol = reSizeCustomSymbol wModel symbolToSize otherSymbol
    let model' = Optic.set (symbolOf_ symbolToSize.Id) scaledSymbol wModel
    BusWireSeparate.routeAndSeparateSymbolWires model' symbolToSize.Id

/// <summary>If a wire between a target symbol and another symbol connects opposite edges, return the edge that the wire is connected to on the target symbol. Else: None </summary>
/// <param name="wModel"> Wire model</param>
/// <param name="wire"> Wire connected between the two symbols</param>
/// <param name="sym"> Target symbol</param>
/// <param name="otherSym"> Other symbol connected by the wire</param>
/// <returns>Some Edge or None</returns>
let tryWireSymOppEdge (wModel: Model) (wire: Wire) (sym: Symbol) (otherSym: Symbol) =
    let symEdge = wireSymEdge wModel wire sym
    let otherSymEdge = wireSymEdge wModel wire otherSym

    match symEdge = otherSymEdge.Opposite with // if the Edges are opposite
    | true -> Some symEdge
    | false -> None

/// <summary>Update Symbol Connection Data record with ComponentID->Edge mapping</summary>
/// <param name="symConnData">SymConnData record to update</param>
/// <param name="edge">Symbol Edge</param>
/// <param name="cid">Symbol Component ID</param>
/// <returns>Updated Symbol Connection Data record</returns>
let updateSymConnData (symConnData: SymConnDataT) (edge: Edge) (cid: ComponentId) =
    let m = symConnData.ConnMap
    let count = Map.tryFind (cid, edge) m |> Option.defaultValue 0 |> (+) 1
    { ConnMap = Map.add (cid, edge) count m }

// TODO: this is copied from Sheet.notIntersectingComponents. It requires SheetT.Model, which is not accessible from here. Maybe refactor it.
let noSymbolOverlap (boxesIntersect: BoundingBox -> BoundingBox -> bool) boundingBoxes sym =
    let symBB = getSymbolBoundingBox sym
    let boxesIntersectsBox sId boundingBox =
        sym.Id <> sId && (boxesIntersect boundingBox symBB)
    boundingBoxes
    |> Map.filter boxesIntersectsBox
    |> Map.isEmpty

/// <summary>Finds the optimal size and position for the selected bus wire symbol w.r.t. to its surrounding symbols.</summary>
/// <param name="wModel">Bus Wire model</param>
/// <param name="symbol">Target symbol to optimise</param>
/// <param name="boundingBoxes">Map of component IDs,BoundingBox for surrounding symbols /param>
/// <returns>Optimised BusWire model</returns>
let optimiseSymbol
    (wModel: BusWireT.Model)
    (symbol: Symbol)
    (boundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>)
    : BusWireT.Model =

    /// If a wire connects the target symbol to another symbol, note which edge it is connected to
    let updateData (symConnData: SymConnDataT) _ (wire: Wire) =
        let symS, symT = getSourceSymbol wModel wire, getTargetSymbol wModel wire
        /// Determine the other symbol connected by to this wire
        let otherSymbol =
            match symS, symT with
            | _ when (symS.Id <> symbol.Id) && (symT.Id = symbol.Id) -> Some symS
            | _ when (symS = symbol) && (symT <> symbol) -> Some symT
            | _ -> None

        match otherSymbol with
        | Some otherSym ->
            let edge = tryWireSymOppEdge wModel wire symbol otherSym
            match edge with
            | Some e -> updateSymConnData symConnData e otherSym.Id
            | None -> symConnData // should not happen
        | None -> symConnData 

    /// Look through all wires to build up SymConnDataT.
    let symConnData = ({ ConnMap = Map.empty }, wModel.Wires) ||> Map.fold updateData

    /// Resize the symbol based on the number of connections it has to other symbols.
    let tryResize (symCount: ((ComponentId * Edge) * int) array) sym =
        let alignSym (sym: Symbol) (otherSym: Symbol) =
            let resizedSym = reSizeCustomSymbol wModel sym otherSym
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
/// Change: Initialize bounding box calculation with the first symbol's dimension, thus avoiding the need for separate max/min
/// functions for each coordinate. Fold operation is used to efficiently iterate through the symbols list
let getBlock (symbols: Symbol List) : BoundingBox =
    let initialSymbol = List.head symbols
    let initialDims = getRotatedHAndW initialSymbol
    let initialMinX, initialMinY = initialSymbol.Pos.X, initialSymbol.Pos.Y
    let initialMaxX, initialMaxY = initialMinX + (snd initialDims), initialMinY + (fst initialDims)

    let (minX, maxX, minY, maxY) =
        symbols
        |> List.fold (fun (minX, maxX, minY, maxY) symbol ->
            let (height, width) = getRotatedHAndW symbol
            let adjustedX, adjustedY = symbol.Pos.X + width, symbol.Pos.Y + height
            (min minX symbol.Pos.X, max maxX adjustedX, min minY symbol.Pos.Y, max maxY adjustedY)
        ) (initialMinX, initialMaxX, initialMinY, initialMaxY)
    { TopLeft = { X = minX; Y = minY }; W = maxX - minX; H = maxY - minY }

let getSymbolsBoundingBox 
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
/// Change: removed unnessary Lambda functions.Directly applied transformations to the point relative to the center
let rotatePointAboutBlockCentre
            (point:XYPos) 
            (centre:XYPos) 
            (rotation:Rotation): XYPos= 
    let relativeToCentre = point - centre
    let rotatedPoint = 
        match rotation with 
        | Degree0 -> 
            relativeToCentre 
        | Degree90 ->
            {X = relativeToCentre.Y ; Y = -relativeToCentre.X}
        | Degree180 -> 
            {X = -relativeToCentre.X ; Y = - relativeToCentre.Y}
        | Degree270 ->
            {X = -relativeToCentre.Y ; Y = relativeToCentre.X}
    rotatedPoint + centre

/// <summary>HLP 23: AUTHOR Ismagilov - Takes a point Pos, a centre Pos, and a flip type and returns the point flipped about the centre</summary>
/// <param name="point"> Original XYPos</param>
/// <param name="center"> The center XYPos that the point is flipped about</param>
/// <param name="flip"> Horizontal or Vertical flip</param>
/// <returns>New flipped point</returns>
/// Change: Directly calculate the new position using a more straightforward formula, reducing complexity
let flipPointAboutBlockCentre 
    (point:XYPos)
    (center:XYPos)
    (flip:FlipType): XYPos = 
    match flip with
    | FlipHorizontal-> 
        {X = 2.0 * center.X - point.X; Y = point.Y} 
    | FlipVertical -> 
        {X = point.X; Y = 2.0 * center.Y - point.Y}

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
/// <remarks>
/// HLP24: Anlan Qiu - furtherRotationIfVertical subfunction added. Defined before the pipeline
///                    and replaces the anonymous function.
/// </remarks>
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

    // Added subfunction (HLP24: Anlan Qiu)
    let furtherRotationIfVertical (sym:Symbol):Symbol =
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
    |> furtherRotationIfVertical

/// <summary>HLP 23: AUTHOR Ismagilov - Scales selected symbol up or down.</summary>
/// <remarks>
/// HLP24: Anlan Qiu - Removed the obsolete and commented out argument //(Mag: float)
///                  - 2 New subfunctions getNewPos and getNewComponent added.
///                    newPos and newComp now computed slightly differently with these
///                    new subfunctions.
/// </remarks>
/// <param name="scaleType"> Scale up or down. Scaling distance is constant</param>
/// <param name="block"> Bounding box of selected components</param>
/// <param name="sym"> Symbol to be rotated</param>
/// <returns>New symbol after scaled about block centre.</returns>
let scaleSymbolInBlock
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

    let getNewPos (heightWidthTuple:float*float):XYPos =
        let h = fst heightWidthTuple
        let w = snd heightWidthTuple
        {X = (newCenter.X) - w/2.; Y= (newCenter.Y) - h/2.}

    let getNewComponent (newPos:XYPos):Component =
        { sym.Component with X = newPos.X; Y = newPos.Y}

    let newPos =
        getRotatedHAndW sym |> getNewPos

    let newComponent = getNewComponent newPos

    {sym with Pos = newPos; Component=newComponent; LabelHasDefaultPos=true}


/// HLP 23: AUTHOR Klapper - Rotates a symbol based on a degree, including: ports and component parameters.
/// <remarks>
/// HLP24: Anlan Qiu - pos definiton moved from the first line of the function to inside the 
///                    second match case.
/// </remarks>
let rotateSymbolByDegree (degree: Rotation) (sym:Symbol)  =
    match degree with
    | Degree0 -> sym
    | _ ->  
        let pos = {X = sym.Component.X + sym.Component.W / 2.0 ; Y = sym.Component.Y + sym.Component.H / 2.0 }
        rotateSymbolInBlock degree pos sym
    

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


/// <summary>Merge two maps, prioritising the second map's entries</summary>
let mergeMaps map1 map2 =
    Map.fold (fun acc k v -> Map.add k v acc) map1 map2

/// Alter position of one symbol as needed in a scaling operation
let scaleSymbol
        (xYSC: (float * float) * (float * float))
        (sym: Symbol)
        : Symbol = 

    let transformationOffset = {XYPos.X = (xYSC |> fst |> fst); Y = (xYSC |> snd |> fst)}
    let transformationScale = {XYPos.X = (xYSC |> fst |> snd); Y = (xYSC |> snd |> snd)}

    let symbol = sym

    let symbolCenter =  getRotatedSymbolCentre symbol
    let symbolHandW = (getRotatedHAndW symbol)

    // Finds elementwise/Hadamard product
    let elementWiseProduct (a:XYPos) (b:XYPos) = {XYPos.X=a.X*b.X; Y=a.Y*b.Y}

    let newCenter = (elementWiseProduct (symbolCenter - transformationOffset) transformationScale) + transformationOffset
    let symCentreOffsetFromTopLeft = {X = (snd symbolHandW)/2.; Y = (fst symbolHandW)/2.}

    let newTopLeftPos = newCenter - symCentreOffsetFromTopLeft
    let newComponent = {symbol.Component with X = newTopLeftPos.X; Y = newTopLeftPos.Y}

    {symbol with Pos = newTopLeftPos; Component = newComponent; LabelHasDefaultPos = true}
    
/// Part of the rotate and scale code       
let groupNewSelectedSymsModel
    (model: SymbolT.Model) 
    (selectedComponentIds: ComponentId list)
    (symbolMapper: (Symbol -> Symbol)) = 
    
    let (selectedSymbols, unselectedSymbols) =
        model.Symbols |> Map.partition (fun x _ -> (List.contains x selectedComponentIds))

    let selectedSymbols' =
        selectedSymbols |> Map.map (fun k symbol -> symbolMapper symbol)

    {model with Symbols = mergeMaps selectedSymbols' unselectedSymbols}


/// <summary>HLP 23: AUTHOR Ismagilov - Flips a block of symbols, returning the new symbol model</summary>
/// <param name="compList"> List of ComponentId's of selected components</param>
/// <param name="model"> Current symbol model</param>
/// <param name="flip"> Type of flip to do</param>
/// <returns>New flipped symbol model</returns>
let flipBlock (compList:ComponentId list) (model:SymbolT.Model) (flip:FlipType) = 
    let (selectedSymbols, unselectedSymbols) =
        model.Symbols |> Map.partition (fun x _ -> (List.contains x compList))
    
    let boundingBoxCentre =
        (selectedSymbols
        |> Helpers.mapValues
        |> Array.toList
        |> getSymbolsBoundingBox).Centre()
  
    let flippedSymbols = 
        selectedSymbols
        |> Map.map (fun _ symbol -> flipSymbolInBlock flip boundingBoxCentre symbol ) 

    {model with Symbols = mergeMaps flippedSymbols unselectedSymbols}

/// After every model update this updates the "scaling box" part of the model to be correctly
/// displayed based on whetehr multiple components are selected and if so what is their "box"
/// In addition to changing the model directly, cmd may contain messages that make further changes.
let postUpdateScalingBox (model:SheetT.Model, cmd) = 
    
    let makeSymbolCmd (msg: SymbolT.Msg) = Elmish.Cmd.ofMsg (ModelType.Msg.Sheet (SheetT.Wire (BusWireT.Symbol msg)))
    let makeSheetCmd (msg: SheetT.Msg) = Elmish.Cmd.ofMsg (ModelType.Msg.Sheet msg)

    if (model.SelectedComponents.Length <= 1) then 
        match model.ScalingBox with 
        | None -> model, cmd
        | Some _ -> {model with ScalingBox = None}, 
                    Elmish.Cmd.batch [cmd;
                     makeSymbolCmd (SymbolT.DeleteSymbols model.ScalingBox.Value.ButtonList);
                     makeSheetCmd SheetT.UpdateBoundingBoxes]
    else 
        let newBoundingBox =
            model.SelectedComponents
            |> List.map (fun id -> Map.find id model.Wire.Symbol.Symbols)
            |> getSymbolsBoundingBox

        match model.ScalingBox with 
        | Some scalingBox when scalingBox.ScalingBoxBound = newBoundingBox -> (model, cmd)
        | _ ->

            let bbTopLeft = newBoundingBox.TopLeft
            let buttonOffset: XYPos = {X = newBoundingBox.W + 47.5; Y = -47.5}

            // Dummy position to pass to makeButton, 'dummy' since we are changing
            // Pos again immediately afterwards
            let dummyButtonPos = (bbTopLeft + buttonOffset)

            let makeButton = SymbolUpdate.createAnnotation ThemeType.Colourful

            let buttonSym = {makeButton ScaleButton dummyButtonPos with Pos = (bbTopLeft + buttonOffset)}
            let makeRotateButtonSymbol symbol = {symbol with Component = {symbol.Component with H = 25; W=25}}

            let rotateDeg90Offset: XYPos = {X = newBoundingBox.W+57.; Y = (newBoundingBox.H/2.)-12.5}
            let rotateDeg90ButtonSymbol = 
                makeButton (RotateButton Degree90) (bbTopLeft + rotateDeg90Offset)
                |> makeRotateButtonSymbol

            let rotateDeg270Offset: XYPos = {X = -69.5; Y = (newBoundingBox.H/2.)-12.5}
            let rotateDeg270ButtonSymbol = 
                {makeButton (RotateButton Degree270) (bbTopLeft + rotateDeg270Offset) 
                    with SymbolT.STransform = {Rotation=Degree90 ; Flipped=false}}
                |> makeRotateButtonSymbol

            let newSymbolMap = model.Wire.Symbol.Symbols 
                                |> Map.add buttonSym.Id buttonSym 
                                |> Map.add rotateDeg270ButtonSymbol.Id rotateDeg270ButtonSymbol 
                                |> Map.add rotateDeg90ButtonSymbol.Id rotateDeg90ButtonSymbol

            let initScalingBox: SheetT.ScalingBox = {
                ScalingBoxBound = newBoundingBox;
                ScaleButton = buttonSym;
                RotateDeg90Button = rotateDeg90ButtonSymbol;
                RotateDeg270Button = rotateDeg270ButtonSymbol;
                ButtonList = [buttonSym.Id; rotateDeg270ButtonSymbol.Id; rotateDeg90ButtonSymbol.Id];
            }

            let cmdWithUpdates =
                match model.ScalingBox with
                | Some _ -> [cmd;
                             makeSymbolCmd (SymbolT.DeleteSymbols model.ScalingBox.Value.ButtonList);
                             makeSheetCmd SheetT.UpdateBoundingBoxes]
                            |> Elmish.Cmd.batch
                | None -> cmd

            model
            |> Optic.set SheetT.scalingBox_ (Some initScalingBox)
            |> Optic.set SheetT.symbols_ newSymbolMap,
            cmdWithUpdates
