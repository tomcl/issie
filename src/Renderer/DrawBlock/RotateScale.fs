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
open DrawHelpers
open System

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
      sameSidePorts: string list
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

/// Updates model with new symbol & reroutes wires
let editSymInModelAndReroute (editedSym: Symbol) (wModel: BusWireT.Model) =
    let model' = Optic.set (symbolOf_ editedSym.Id) editedSym wModel
    BusWireSeparate.routeAndSeparateSymbolWires model' editedSym.Id

/// TODO: this is mostly copy pasted code from Symbol.getPortPos, perhaps abstract out the existing code there to use makePortInfo.
/// Could not simply use getPortPos because more data (side, topBottomGap, etc.) is needed to caclulate the new dimensions of the resized symbol.
let makePortInfo (sym: Symbol) (port: Port) =
    let side = getSymbolPortOrientation sym port
    let sameSidePorts = sym.PortMaps.Order[side]
    let gap = getPortPosEdgeGap sym.Component.Type
    let topBottomGap = gap + 0.3 // extra space for clk symbol
    let portDimension = float sameSidePorts.Length - 1.0
    let h, w = getRotatedHAndW sym

    let portGap =
        match side with
        | Left | Right -> float h / (portDimension + 2.0 * gap)
        | Bottom | Top -> float w / (portDimension + 2.0 * topBottomGap)

    { port = port
      sym = sym
      side = side
      sameSidePorts = sameSidePorts
      gap = gap
      topBottomGap = topBottomGap
      portDimension = portDimension
      h = h
      w = w
      portGap = portGap }

// ac2021: had simplification for this but not sure if it works
// TODO: check if it works
/// Get ports connected to a wire
let getPortAB (wModel: Model) wireSyms =
    // let ports = portsOfWires wModel [ wireSyms.Wire ]
    // let wire = wireSyms.Wire
    // let portA = ports[string wire.InputPort]
    // let portB = ports[string wire.OutputPort]
    // portA, portB
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
        | false -> None

    wires
    |> List.tryPick (fun w ->
        tryGetOppEdgePorts
            { SymA = symbolToSize
              SymB = otherSymbol
              Wire = w })

/// Offset vector between two ports
/// in axis perpendicular to side
let alignPortsOffset (movePInfo: PortInfo) (otherPInfo: PortInfo) =
    let getPortRealPos pInfo =
        getPortPos pInfo.sym pInfo.port + pInfo.sym.Pos

    let movePortPos = getPortRealPos movePInfo
    let otherPortPos = getPortRealPos otherPInfo
    let posDiff = otherPortPos - movePortPos

    match movePInfo.side with
    | Top | Bottom -> { X = posDiff.X; Y = 0.0 }
    | Left | Right -> { X = 0.0; Y = posDiff.Y }

/// moves `symbolToSize` to align connection ports
let alignSymbols
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    (wModel: BusWireT.Model)
    : BusWireT.Model =

    // Only attempt to align symbols if they are connected by ports on parallel edges.
    match getOppEdgePortInfo wModel symbolToSize otherSymbol with
    | None -> wModel
    | Some (movePortInfo, otherPortInfo) ->
        let offset = alignPortsOffset movePortInfo otherPortInfo
        let symbol' = moveSymbol offset symbolToSize
        editSymInModelAndReroute  symbol' wModel

/// HLP23: To test this, it must be given two symbols interconnected by wires. 
/// It then resizes symbolToSize so that the connecting wires are exactly straight
/// HLP23: It should work out the interconnecting wires from
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

    editSymInModelAndReroute scaledSymbol wModel

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

/// Increments ConnMap count or initialises at 1 if new connection
let updateOrInsert (symConnData: SymConnDataT) (edge: Edge) (cid: ComponentId) =
    let map = symConnData.ConnMap
    let count = 
        Map.tryFind (cid, edge) map 
        |> Option.defaultValue 0 
        |> (+) 1
    { ConnMap = Map.add (cid, edge) count map }

// TODO: this is copied from Sheet.notIntersectingComponents. It requires SheetT.Model, which is not accessible from here. Maybe refactor it.
// imported boxesIntersect from DrawHelpers
let noSymbolOverlap boundingBoxes sym =
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

    // Update symConnData with wire connection information
    let updateData symConnData _ wire =
        let symS, symT = getSourceSymbol wModel wire, getTargetSymbol wModel wire

        let otherSymbol =
            match symS, symT with
            | _ when (symS.Id <> symbol.Id) && (symT.Id = symbol.Id) -> Some symS
            | _ when (symS = symbol) && (symT <> symbol) -> Some symT
            | _ -> None

        match otherSymbol with
        | Some otherSym ->
            let edge = tryWireSymOppEdge wModel wire symbol otherSym
            Option.fold (fun scd e -> updateOrInsert scd e otherSym.Id) symConnData edge
        | None -> symConnData 

    // Look through all wires to build up SymConnDataT.
    let symConnData = 
        ({ ConnMap = Map.empty }, wModel.Wires) 
        ||> Map.fold updateData 

    // Resize the symbol based on its connections
    let tryResize symCount sym =
        let alignSym sym otherSym =
            let resizedSym = reSizeSymbol wModel sym otherSym
            let noOverlap = noSymbolOverlap boundingBoxes resizedSym
            match noOverlap with
            | true -> true, resizedSym
            | _ -> false, sym

        let folder (hAligned, vAligned, sym) ((cid, edge), _) =
            let otherSym = Optic.get (symbolOf_ cid) wModel
            match hAligned, vAligned, edge with
            | false, _, (Top | Bottom) ->
                let hAligned', resizedSym = alignSym sym otherSym
                hAligned', vAligned, resizedSym
            | _, false, (Left | Right) ->
                let vAligned', resizedSym = alignSym sym otherSym
                hAligned, vAligned', resizedSym
            | _ -> hAligned, vAligned, sym

        let _, _, sym' =
            Array.fold folder (false, false, sym) symCount

        sym'
        
    // Retrieve symConnData and resize the symbol
    let scaledSymbol =
        let symCount =
            symConnData.ConnMap
            |> Map.toArray
            |> Array.filter (fun (_, count) -> count > 1)
            |> Array.sortByDescending snd

        tryResize symCount symbol
    
    // Update the model with the resized symbol and route wires
    editSymInModelAndReroute scaledSymbol wModel

/// <summary> Improve by az1221 - Obtain the bounding box of multiple selected symbols</summary>
/// <param name="symbols"> Selected symbols list</param>
/// <returns>Bounding Box</returns> 
let getBlock (symbols:Symbol List) :BoundingBox = 
    let getPos (sym: Symbol) = sym.Pos

    let minX, maxX, minY, maxY =
        symbols
        |> List.fold (fun (minX, maxX, minY, maxY) sym ->
            let pos = getPos sym
            let rotatedH,rotatedW = getRotatedHAndW sym
            let maxXsym = pos.X + rotatedW
            let maxYsym = pos.Y + rotatedH
            (min minX (pos.X), max maxXsym maxX, min minY (pos.Y), max maxYsym maxY)
        ) (Double.MaxValue, Double.MinValue, Double.MaxValue, Double.MinValue)

    {TopLeft = {X = minX; Y = minY}; W = maxX-minX; H = maxY-minY}

/// <summary>Improve by az1221 - Takes a point Pos, a centre Pos, and a rotation type and returns the point flipped about the centre</summary>
/// <param name="point"> Original XYPos</param>
/// <param name="center"> The center XYPos that the point is rotated about</param>
/// <param name="rotation"> Clockwise or Anticlockwise </param>
/// <returns>New rotated point</returns> 
let rotatePointAboutBlockCentre (point:XYPos) (centre:XYPos) (rotation:Rotation) = 
    
    match rotation with 
    | Degree0 -> 
        point
    | Degree90 ->
        centre - {X =(point - centre).Y; Y = -(point - centre).X}
    | Degree180 -> 
        centre - {X =(point - centre).X; Y = (point - centre).Y}
    | Degree270 ->
        centre - {X = -(point - centre).Y ; Y = (point - centre).X}

/// <summary>Improved by az1221 - Takes a point Pos, a centre Pos, and a flip type and returns the point flipped about the centre</summary>
/// <param name="point"> Original XYPos</param>
/// <param name="center"> The center XYPos that the point is flipped about</param>
/// <param name="flip"> Horizontal or Vertical flip</param>
/// <returns>New flipped point</returns>  
let flipPointAboutBlockCentre (point:XYPos) (center:XYPos) (flip:FlipType) = 
    match flip with
    | FlipHorizontal-> 
        {X = center.X - (point.X - center.X); Y = point.Y} 
    | FlipVertical -> 
        {X = point.X; Y = center.Y - (point.Y - center.Y)}

/// <summary> Improved by az1221 - Get the new top left of a symbol after it has been rotated</summary>
/// <param name="rotation"> Rotated CW or AntiCW</param>
/// <param name="h"> Original height of symbol (Before rotation)</param>
/// <param name="w"> Original width of symbol (Before rotation)</param>
/// <param name="sym"> Symbol</param>
/// <returns>New top left point of the symbol</returns>  
let adjustPosForBlockRotation (rotation:Rotation) (h: float) (w:float)(pos: XYPos): XYPos =
    match rotation with
    | Degree0 -> pos - {X = 0; Y = 0}
    | Degree90 -> pos - {X=(float)h ;Y=0}
    | Degree180 -> pos - {X= (float)w; Y= -(float)h}
    | Degree270 -> pos - { X = 0 ;Y = (float)w }

/// <summary> Improved by az1221 - Get the new top left of a symbol after it has been flipped</summary>
/// <param name="flip">  Flipped horizontally or vertically</param>
/// <param name="h"> Original height of symbol (Before flip)</param>
/// <param name="w"> Original width of symbol (Before flip)</param>
/// <param name="sym"> Symbol</param>
/// <returns>New top left point of the symbol</returns> 
let adjustPosForBlockFlip (flip:FlipType) (h: float) (w:float)(pos: XYPos): XYPos =
    match flip with
    | FlipHorizontal -> pos - {X=(float)w ;Y=0}
    | FlipVertical -> pos - { X = 0 ;Y = (float)h }

type flipOrRot =
    | Flip of FlipType
    | Rot of Rotation
let newTopLeftFlipOrRot (blockCentre: XYPos) (transform: flipOrRot) (sym: Symbol) : XYPos =
    let h,w = getRotatedHAndW sym 
    match transform with 
    | Flip x -> flipPointAboutBlockCentre sym.Pos blockCentre x
                            |>adjustPosForBlockFlip x h w 
    | Rot x -> rotatePointAboutBlockCentre sym.Pos blockCentre (invertRotation x)
                            |>adjustPosForBlockRotation (invertRotation x) h w 
    

/// <summary>HLP 23: AUTHOR Ismagilov - Rotate a symbol in its block.</summary>
/// <param name="rotation">  Clockwise or Anticlockwise rotation</param>
/// <param name="block"> Bounding box of selected components</param>
/// <param name="sym"> Symbol to be rotated</param>
/// <returns>New symbol after rotated about block centre.</returns>
let rotateSymbolInBlock 
        (rotation: Rotation) 
        (blockCentre: XYPos)
        (sym: Symbol)  : Symbol =
      
    let newTopLeft = newTopLeftFlipOrRot blockCentre (Rot rotation) sym

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

    let newTopLeft = newTopLeftFlipOrRot blockCentre (Flip flip) sym

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
    

let findSelectedSymbols (compList: ComponentId list) (model: SymbolT.Model) = 
    List.map (fun x -> model.Symbols |> Map.find x) compList

/// <summary>HLP 23: AUTHOR Ismagilov - Rotates a block of symbols, returning the new symbol model</summary>
/// <param name="compList"> List of ComponentId's of selected components</param>
/// <param name="model"> Current symbol model</param>
/// <param name="rotation"> Type of rotation to do</param>
/// <returns>New rotated symbol model</returns>
let rotateBlock (compList:ComponentId list) (model:SymbolT.Model) (rotation:Rotation) = 
    let selectedSymbols = findSelectedSymbols compList model
    let block = getBlock selectedSymbols

    let newSymbolsMap = 
        selectedSymbols
        |> List.map (rotateSymbolInBlock (invertRotation rotation) (block.Centre()))  
        |> List.zip compList
        |> Map.ofList
    
    let newSymbols = 
        (model.Symbols , newSymbolsMap)
        ||> Map.fold (fun acc k v -> Map.add k v acc)

    {model with Symbols = newSymbols}

let getMaxXSym (selectedSymbols: Symbol list) = 
    selectedSymbols
    |> List.maxBy (fun (x:Symbol) -> x.Pos.X + snd (getRotatedHAndW x)) 
let getMinXSym (selectedSymbols: Symbol list) = 
    selectedSymbols
    |> List.minBy (fun (x:Symbol) -> x.Pos.X)
let getMaxYSym (selectedSymbols: Symbol list)  = 
    selectedSymbols
    |> List.maxBy (fun (y:Symbol) -> y.Pos.Y+ fst (getRotatedHAndW y))
let getMinYSym (selectedSymbols: Symbol list) = 
    selectedSymbols
    |> List.minBy (fun (y:Symbol) -> y.Pos.Y)
let oneCompBoundsBothEdges (selectedSymbols: Symbol list) = 
    let maxXSymCentre = selectedSymbols |> getMaxXSym |> getRotatedSymbolCentre
    let minXSymCentre = selectedSymbols |> getMinXSym |> getRotatedSymbolCentre
    let maxYSymCentre = selectedSymbols |> getMaxYSym |> getRotatedSymbolCentre
    let minYSymCentre = selectedSymbols |> getMinYSym |> getRotatedSymbolCentre
    (maxXSymCentre.X = minXSymCentre.X) || (maxYSymCentre.Y = minYSymCentre.Y)

// ac2021: reduce need for consfusing (float * float) * (float * float).
// ac2021: as intermedited helper functions work together,
// ac2021: changing intermediary type causes no problems is code (compiles)
/// Scaling factor & offset from centre
type scalingOffset =
    {
        ScalingF: float
        OffsetC: float
    }

let getScalingFactorAndOffsetCentre (min:float) (matchMin:float) (max:float) (matchMax:float) = 
    let isInvalid = min = max || matchMax <= matchMin
    let scaleFact =
        match isInvalid with
        | true -> 1.
        | false -> (matchMin - matchMax) / (min - max)
    let offsetC =
        match scaleFact with
        | 1. -> 0.
        | _ -> (matchMin - min * scaleFact) / (1.-scaleFact)
    {
        ScalingF = scaleFact
        OffsetC = offsetC
    }

/// Return set of scalingOffset that define how a group of components is scaled
let getScalingFactorAndOffsetCentreGroup
    (matchBBMin:XYPos)
    (matchBBMax:XYPos)
    (selectedSymbols: Symbol list) : (scalingOffset * scalingOffset) = 

    let maxXSym = getMaxXSym selectedSymbols
    let oldMaxX = (maxXSym |> getRotatedSymbolCentre).X
    let newMaxX = matchBBMax.X - (snd (getRotatedHAndW maxXSym))/2.

    let minXSym = getMinXSym selectedSymbols
    let oldMinX = (minXSym |> getRotatedSymbolCentre).X
    let newMinX = matchBBMin.X + (snd (getRotatedHAndW minXSym))/2.
    
    let maxYSym = getMaxYSym selectedSymbols
    let oldMaxY = (maxYSym |> getRotatedSymbolCentre).Y
    let newMaxY = matchBBMax.Y - (fst (getRotatedHAndW maxYSym))/2.

    let minYSym = getMinYSym selectedSymbols
    let oldMinY = (minYSym |>  getRotatedSymbolCentre).Y
    let newMinY = matchBBMin.Y + (fst (getRotatedHAndW minYSym))/2.
    
    let xSC = getScalingFactorAndOffsetCentre oldMinX newMinX oldMaxX newMaxX
    let ySC = getScalingFactorAndOffsetCentre oldMinY newMinY oldMaxY newMaxY
    (xSC, ySC)

/// Alter position of one symbol as needed in a scaling operation
let scaleSymbol
        (xSC, ySC)
        (sym: Symbol)
        : Symbol = 
    let symCentre =  getRotatedSymbolCentre sym
    let translateFunc scaleFact offsetC coordinate = (coordinate - offsetC) * scaleFact + offsetC
    let newX = translateFunc (xSC.ScalingF) (xSC.OffsetC) symCentre.X
    let newY = translateFunc (ySC.ScalingF) (ySC.OffsetC) symCentre.Y

    let newTopLeftPos = 
        {X = newX - (snd (getRotatedHAndW sym))/2. 
         Y = newY - (fst (getRotatedHAndW sym))/2.} 
    let newComp = {sym.Component with X = newTopLeftPos.X; Y = newTopLeftPos.Y}

    {sym with Pos = newTopLeftPos; Component = newComp; LabelHasDefaultPos = true}

// ac2021: in implementations, these are used together
/// Get scaled symbol from desired characteristics
let scaleSymbolToBB
        (matchBBMin:XYPos)
        (matchBBMax:XYPos)
        (selectedSymbols: Symbol list)
        (sym: Symbol)
        : Symbol =
    let xySC = getScalingFactorAndOffsetCentreGroup matchBBMin matchBBMax selectedSymbols
    scaleSymbol xySC sym

/// Modify a selected subset of symbols found in a Symbol model according to a supplied function.
/// Returns the updated Symbol model.
let groupNewSelectedSymsModel
    (compList:ComponentId list)  
    (model:SymbolT.Model) 
    (selectedSymbols: Symbol list)
    (modifySymbolFunc) =

    let newSymbolMap = 
        selectedSymbols
        |> List.map modifySymbolFunc
        |> List.map2 (fun x y -> (x,y)) compList
        |> Map.ofList
    {
        model with Symbols = (model.Symbols, newSymbolMap)
                ||> Map.fold (fun acc k v -> Map.add k v acc)
    }

/// <summary>HLP 23: AUTHOR Ismagilov - Flips a block of symbols, returning the new symbol model</summary>
/// <param name="compList"> List of ComponentId's of selected components</param>
/// <param name="model"> Current symbol model</param>
/// <param name="flip"> Type of flip to do</param>
/// <returns>New flipped symbol model</returns>
let flipBlock (compList:ComponentId list) (model:SymbolT.Model) (flip:FlipType) = 
    let selectedSymbols = 
        compList
        |> List.map (fun cid -> Map.tryFind cid model.Symbols)
        |> List.filter (fun sym -> not sym.IsNone)
        |> List.map (fun sym -> sym.Value) // Should never throw an error, as Nones were filtered out
    
    let block = getBlock selectedSymbols
    let modifier = flipSymbolInBlock flip (block.Centre())

    groupNewSelectedSymsModel compList model selectedSymbols modifier 
    

/// After every model update this updates the "scaling box" part of the model to be correctly
/// displayed based on whether multiple components are selected and if so what is their "box"
/// In addition to changing the model directly, cmd may contain messages that make further changes.
let postUpdateScalingBox (model:SheetT.Model, cmd) = 
    let symbolCmd (msg: SymbolT.Msg) = Elmish.Cmd.ofMsg (ModelType.Msg.Sheet (SheetT.Wire (BusWireT.Symbol msg)))
    let sheetCmd (msg: SheetT.Msg) = Elmish.Cmd.ofMsg (ModelType.Msg.Sheet msg)

    let returnModelAndCommand (oldModel:SheetT.Model) (newModel: SheetT.Model) = 
        match model.ScalingBox with 
        | None -> oldModel,cmd
        | _ -> newModel, [symbolCmd (SymbolT.DeleteSymbols (model.ScalingBox.Value).ButtonList);
                        sheetCmd SheetT.UpdateBoundingBoxes; cmd]
                        |> Elmish.Cmd.batch           

    if (model.SelectedComponents.Length < 2) then 
        returnModelAndCommand model {model with ScalingBox=None}
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
            let dummyPos = (topleft + {X = newBoxBound.W + 47.5; Y = -47.5})

            // Helper to create a button
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