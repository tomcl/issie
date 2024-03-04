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
/// Creates a PortInfo record for a given symbol and port. </summary>
/// <param name="sym"> sym: The symbol the port is on. </param>
/// <param name="port"> port: The port to create the record for. </param>
/// <returns> A PortInfo record containing all the information required to calculate the position of a port on the sheet.</returns>
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

/// <summary> Gets Port A and B for the given symbol. </summary>
/// <param name="wModel"> The wire model. </param>
/// <param name="wireSyms"> The symbols connected by the wire. </param>
/// <returns> Tuple of ports A and B for the given symbol. </returns>
let getPortAB (wModel : BusWireT.Model) (wireSyms: WireSymbols) : (Port * Port) =
    let ports = portsOfWires wModel [ wireSyms.Wire ]
    let portA = filterPortBySym ports wireSyms.SymA |> List.head
    let portB = filterPortBySym ports wireSyms.SymB |> List.head
    portA, portB

/// <summary> Try to get two ports that are on opposite edges. </summary>
/// <param name="wModel"> The wire model. </param>
/// <param name="firstSymbol"> The first symbol. </param>
/// <param name="otherSymbol"> The other symbol. </param>
/// <returns> Tuple of ports A and B for the given symbol. </returns>
let getOppEdgePortInfo
    (wModel: BusWireT.Model)
    (firstSymbol: Symbol)
    (otherSymbol: Symbol)
    : (PortInfo * PortInfo) option =
    let wires = wiresBtwnSyms wModel firstSymbol otherSymbol

    let tryGetOppEdgePorts (wireSyms: WireSymbols) =
        let portA, portB = getPortAB wModel wireSyms
        let edgeA = getSymbolPortOrientation wireSyms.SymA portA
        let edgeB = getSymbolPortOrientation wireSyms.SymB portB
        match edgeA = edgeB.Opposite with
        | true -> Some(makePortInfo wireSyms.SymA portA, makePortInfo wireSyms.SymB portB)
        | _ -> None
    wires
    |> List.tryPick (fun w ->
        tryGetOppEdgePorts
            { SymA = firstSymbol
              SymB = otherSymbol
              Wire = w })

/// <summary> Aligns ports based on the offset between two ports. </summary>
/// <param name="movePInfo"> The port to move. </param>
/// <param name="otherPInfo"> The other port. </param>
/// <returns> The new offset XYPos for the port. </returns>
let alignPortsOffset (movePInfo: PortInfo) (otherPInfo: PortInfo) : (XYPos) =
    let getPortRealPos pInfo =
        getPortPos pInfo.sym pInfo.port + pInfo.sym.Pos

    let movePortPos = getPortRealPos movePInfo
    let otherPortPos = getPortRealPos otherPInfo
    let posDiff = otherPortPos - movePortPos

    match movePInfo.side with
    | Top  | Bottom -> { X = posDiff.X; Y = 0.0 }
    | Left | Right -> { X = 0.0; Y = posDiff.Y }


/// <summary> Aligns two symbols if they are connected by ports on parallel edges. </summary>
/// <param name="wModel"> The wire model. </param>
/// <param name="symbolToSize"> The symbol to size. </param>
/// <param name="otherSymbol"> The other symbol. </param>
/// <returns> The updated wire model. </returns>
let alignSymbols
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    : BusWireT.Model =

    // Only attempt to align symbols if they are connected by ports on parallel edges.
    match getOppEdgePortInfo wModel symbolToSize otherSymbol with
    | None -> 
        // If no parallel ports found, return the original model
        wModel
    | Some(movePortInfo, otherPortInfo) ->
        // Calculate the offset for alignment
        let offset = alignPortsOffset movePortInfo otherPortInfo
        // Move the symbol to the new position
        let symbol' = moveSymbol offset symbolToSize
        // Update the symbol in the model
        let model' = Optic.set (symbolOf_ symbolToSize.Id) symbol' wModel
        // Route and separate the symbol wires in the model
        BusWireSeparate.routeAndSeparateSymbolWires model' symbolToSize.Id

/// <summary> Helper function to calculate the new height and width of a symbol after resizing. 
/// Good to have for better code maintainability and useability in other parts of the code base. </summary>
/// <param name="resizePortInfo"> The port to resize. </param>
/// <param name="otherPortInfo"> The other port. </param>
/// <returns> The new height and width of the symbol. </returns>
let calculateHW (resizePortInfo: PortInfo) (otherPortInfo:PortInfo) : (float * float) = 
    match resizePortInfo.side with
    | Left | Right ->
        otherPortInfo.portGap * (resizePortInfo.portDimension + 2.0 * resizePortInfo.gap), resizePortInfo.w
    | Top | Bottom ->
        resizePortInfo.h, otherPortInfo.portGap * (resizePortInfo.portDimension + 2.0 * resizePortInfo.topBottomGap)


///<summary> HLP23: AUTHOR Ismagilov - Resizes a symbol so that the connecting wires are exactly straight.
/// HLP23: To test this, it must be given two symbols interconnected by wires. It then resizes symbolToSize
/// so that the connecting wires are exactly straight
/// HLP23: It should work out the interconnecting wires (wires) from
/// the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has adjustable size).  </summary>
/// <param name="wModel"> The wire model. </param>
/// <param name="symbolToSize"> The symbol to size. </param>
/// <param name="otherSymbol"> The other symbol. </param>
/// <returns> The updated symbol. </returns>
let reSizeSymbol (wModel: BusWireT.Model) (symbolToSize: Symbol) (otherSymbol: Symbol) : (Symbol) =
    let wires = wiresBtwnSyms wModel symbolToSize otherSymbol

    // Try to get two ports that are on opposite edges, if none found just use any two ports.
    let resizePortInfo, otherPortInfo =
        match getOppEdgePortInfo wModel symbolToSize otherSymbol with
        | None ->
            let pA, pB = getPortAB wModel { SymA = symbolToSize; SymB = otherSymbol; Wire = wires.Head }
            makePortInfo symbolToSize pA, makePortInfo symbolToSize pB
        | Some(pIA, pIB) -> (pIA, pIB)

    let h, w = calculateHW resizePortInfo otherPortInfo

    match symbolToSize.Component.Type with
    | Custom _ ->
        let scaledSymbol = setCustomCompHW h w symbolToSize
        let scaledInfo = makePortInfo scaledSymbol resizePortInfo.port
        let offset = alignPortsOffset scaledInfo otherPortInfo
        moveSymbol offset scaledSymbol
    | _ -> symbolToSize

/// For UI to call ResizeSymbol.
let reSizeSymbolTopLevel
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    : BusWireT.Model =
    printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"

    let scaledSymbol = reSizeSymbol wModel symbolToSize otherSymbol

    wModel
    |> Optic.set (symbolOf_ symbolToSize.Id) scaledSymbol
    |> fun updatedModel -> BusWireSeparate.routeAndSeparateSymbolWires updatedModel symbolToSize.Id

/// For each edge of the symbol, store a count of how many connections it has to other symbols.
type SymConnDataT =
    { ConnMap: Map<ComponentId * Edge, int> }

/// If a wire between a target symbol and another symbol connects opposite edges, return the edge that the wire is connected to on the target symbol 
//Changed:Simplify unnecessary matching
let tryWireSymOppEdge (wModel: Model) (wire: Wire) (sym: Symbol) (otherSym: Symbol) =
    let symEdge = wireSymEdge wModel wire sym
    let otherSymEdge = wireSymEdge wModel wire otherSym

    if symEdge = otherSymEdge.Opposite then Some symEdge else None

let updateOrInsert (symConnData: SymConnDataT) (edge: Edge) (cid: ComponentId) =
    let m = symConnData.ConnMap
    let count = Map.tryFind (cid, edge) m |> Option.defaultValue 0 |> (+) 1
    { ConnMap = Map.add (cid, edge) count m }

// TODO: this is copied from Sheet.notIntersectingComponents. It requires SheetT.Model, which is not accessible from here. Maybe refactor it.

/// Checks if the given symbol's bounding box does not overlap with any other symbol's bounding box.
//Changed: Simplify Map.filter and Map.isEmpty to Map.exists
let noSymbolOverlap (boxesIntersect: BoundingBox -> BoundingBox -> bool) boundingBoxes (sym: Symbol) =
    let symBB = getSymbolBoundingBox sym

    not (Map.exists (fun sId boundingBox -> sId <> sym.Id && boxesIntersect boundingBox symBB) boundingBoxes)


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
        //Changed: Simplified match by combining cases and using tuple patterns.
        let folder (hAligned, vAligned, sym) ((cid, edge), _) =
            match hAligned, vAligned, edge with
            | false, _, (Top | Bottom) | _, false, (Left | Right) ->
                let otherSym = Optic.get (symbolOf_ cid) wModel       
                let aligned, resizedSym = alignSym sym otherSym
                match edge with
                | Top | Bottom -> (aligned, vAligned, resizedSym)
                | _ -> (hAligned, aligned, resizedSym)
            | _ -> (hAligned, vAligned, sym)

        let (_, _, sym') = ((false, false, sym), symCount) ||> Array.fold folder
        sym'

    let scaledSymbol =
        let symCount =
            symConnData.ConnMap
            |> Map.toArray 
            |> Array.filter (fun (_, count) -> count > 1)
            |> Array.sortByDescending snd

        tryResize symCount symbol
    //Changed: Combine last 2 lines to pipeline. Make it more readable.
    let updatedModel = 
        Optic.set (symbolOf_ symbol.Id) scaledSymbol wModel
        |> fun updatedWModel -> BusWireSeparate.routeAndSeparateSymbolWires updatedWModel symbol.Id
    //Changed: Add a return function.
    updatedModel

/// <summary>HLP 23: AUTHOR Ismagilov - Get the bounding box of multiple selected symbols</summary>
/// <param name="symbols"> Selected symbols list</param>
/// <returns>Bounding Box</returns>
let getBlock 
        (symbols:Symbol List) :BoundingBox = 

    let getRightmostX sym =
        let _, w = getRotatedHAndW sym
        sym.Pos.X + w
    
    let getBottommostY sym =
        let h, _ = getRotatedHAndW sym
        sym.Pos.Y + h

    let maxX = symbols |> List.maxBy getRightmostX |> getRightmostX
    let minX = symbols |> List.minBy (fun sym -> sym.Pos.X) |> fun sym -> sym.Pos.X
    let maxY = symbols |> List.maxBy getBottommostY |> getBottommostY
    let minY = symbols |> List.minBy (fun sym -> sym.Pos.Y) |> fun sym -> sym.Pos.Y

    { TopLeft = { X = minX; Y = minY }; W = maxX - minX; H = maxY - minY }



/// <summary>HLP 23: AUTHOR Ismagilov - Takes a point Pos, a centre Pos, and a rotation type and returns the point flipped about the centre</summary>
/// <param name="point"> Original XYPos</param>
/// <param name="center"> The centre XYPos that the point is rotated about</param>
/// <param name="rotation"> Clockwise or Anticlockwise </param>
/// <returns>New flipped point</returns>
let rotatePointAboutBlockCentre (point: XYPos) (centre: XYPos) (rotation: Rotation) =
    let toCentre = point - centre
    let rotated =
        match rotation with
        | Degree0 -> toCentre
        | Degree90 -> { X = toCentre.Y; Y = -toCentre.X }
        | Degree180 -> { X = -toCentre.X; Y = -toCentre.Y }
        | Degree270 -> { X = -toCentre.Y; Y = toCentre.X }
    rotated + centre

/// <summary>HLP 23: AUTHOR Ismagilov - Takes a point Pos, a centre Pos, and a flip type and returns the point flipped about the centre</summary>
/// <param name="point"> Original XYPos</param>
/// <param name="centre"> The center XYPos that the point is flipped about</param>
/// <param name="flip"> Horizontal or Vertical flip</param>
/// <returns>New flipped point</returns>
let flipPointAboutBlockCentre (point: XYPos) (centre: XYPos) (flip: FlipType) =
    match flip with
    | FlipHorizontal -> { X = (centre.X * 2.0) - point.X; Y = point.Y }
    | FlipVertical -> { X = point.X; Y = (centre.Y * 2.0) - point.Y }

/// <summary>HLP 23: AUTHOR Ismagilov - Get the new top left of a symbol after it has been rotated</summary>
/// <param name="rotation"> Rotated CW or AntiCW</param>
/// <param name="h"> Original height of symbol (Before rotation)</param>
/// <param name="w"> Original width of symbol (Before rotation)</param>
/// <param name="sym"> Symbol</param>
/// <returns>New top left point of the symbol</returns>
let adjustPosForBlockRotation (rotation: Rotation) (h: float) (w: float) (pos: XYPos) : XYPos =
    let posOffset =
        match rotation with
        | Degree0 -> { X = 0; Y = 0 }
        | Degree90 -> { X = h; Y = 0 }
        | Degree180 -> { X = w; Y = -h }
        | Degree270 -> { X = 0; Y = w }
    pos - posOffset

/// <summary>HLP 23: AUTHOR Ismagilov - Get the new top left of a symbol after it has been flipped</summary>
/// <param name="flip">  Flipped horizontally or vertically</param>
/// <param name="h"> Original height of symbol (Before flip)</param>
/// <param name="w"> Original width of symbol (Before flip)</param>
/// <param name="sym"> Symbol</param>
/// <returns>New top left point of the symbol</returns>
let adjustPosForBlockFlip (flip: FlipType) (h: float) (w: float) (pos: XYPos) =
    let posOffset =
        match flip with
        | FlipHorizontal -> { X = w; Y = 0 }
        | FlipVertical -> { X = 0; Y = h }
    pos - posOffset

/// <summary>HLP 23: AUTHOR Ismagilov - Rotate a symbol in its block.</summary>
/// <param name="rotation">  Clockwise or Anticlockwise rotation</param>
/// <param name="block"> Bounding box of selected components</param>
/// <param name="sym"> Symbol to be rotated</param>
/// <returns>New symbol after rotated about block centre.</returns>
let rotateSymbolInBlock (rotation: Rotation) (blockCentre: XYPos) (sym: Symbol) : Symbol =

    let h, w = getRotatedHAndW sym

    let newTopLeft =
        rotatePointAboutBlockCentre sym.Pos blockCentre (invertRotation rotation)
        |> adjustPosForBlockRotation (invertRotation rotation) h w

    let newComponent = { sym.Component with X = newTopLeft.X; Y = newTopLeft.Y }

    let newSTransform =
        match sym.STransform.Flipped with
        | true ->
            { sym.STransform with
                Rotation = combineRotation (invertRotation rotation) sym.STransform.Rotation }
        | _ ->
            { sym.STransform with
                Rotation = combineRotation rotation sym.STransform.Rotation }

    { sym with
        Pos = newTopLeft
        PortMaps = rotatePortInfo rotation sym.PortMaps
        STransform = newSTransform
        LabelHasDefaultPos = true
        Component = newComponent }
    |> calcLabelBoundingBox


/// <summary>HLP 23: AUTHOR Ismagilov - Flip a symbol horizontally or vertically in its block.</summary>
/// <param name="flip">  Flip horizontally or vertically</param>
/// <param name="blockCentre"> Centre Position of Block </param>
/// <param name="sym"> Symbol to be flipped</param>
/// <returns>New symbol after flipped about block centre.</returns>
let flipSymbolInBlock
    (flip: FlipType)
    (blockCentre: XYPos)
    (sym: Symbol) : Symbol =

    let h,w = getRotatedHAndW sym
    //Needed as new symbols and their components need their Pos updated (not done in regular flip symbol)
    
    // Changed: add pipeline
    let updatedTopLeft = 
        flip 
        |> flipPointAboutBlockCentre sym.Pos blockCentre
        |> adjustPosForBlockFlip flip h w
    
    let flippedPortOrientation = 
        sym.PortMaps.Orientation 
        |> Map.map (fun id side -> flipSideHorizontal side)

    // Function to get the flipped port order
    let flipPortList currPortOrder side =
        currPortOrder 
        |> Map.add (flipSideHorizontal side ) sym.PortMaps.Order[side]

    let flippedPortOrder = 
        (Map.empty, [Edge.Top; Edge.Left; Edge.Bottom; Edge.Right]) ||> List.fold flipPortList
        |> Map.map (fun edge order -> List.rev order)   
  
    let updatedSTransform = { sym.STransform with Flipped = not sym.STransform.Flipped }

    let updatedComponent = { sym.Component with X = updatedTopLeft.X; Y = updatedTopLeft.Y }

    
    // Define new symbol with updated STransform and PortMaps
    let updatedSym = 
        { sym with
            Component = updatedComponent;
            PortMaps = { Order = flippedPortOrder; Orientation = flippedPortOrientation };
            STransform = updatedSTransform;
            LabelHasDefaultPos = true;
            Pos = updatedTopLeft }
        |> calcLabelBoundingBox

    let applyFlip flip updatedSym =
        match flip with
        | FlipHorizontal -> 
            updatedSym
        | FlipVertical ->
            let newBlock = getBlock [updatedSym]
            let newBlockCenter = newBlock.Centre()
            updatedSym
            |> rotateSymbolInBlock Degree270 newBlockCenter 
            |> rotateSymbolInBlock Degree270 newBlockCenter

    applyFlip flip updatedSym


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
    let xProp = (symCenter.X - block.TopLeft.X) / block.W
    let yProp = (symCenter.Y - block.TopLeft.Y) / block.H

    let scaleOffset = match scaleType with
                      | ScaleUp -> -5.0, 10.0
                      | ScaleDown -> 5.0, -10.0

    let newCenterX = block.TopLeft.X + fst scaleOffset + (block.W + snd scaleOffset) * xProp
    let newCenterY = block.TopLeft.Y + fst scaleOffset + (block.H + snd scaleOffset) * yProp
    let newCenter = { X = newCenterX; Y = newCenterY }

    let h,w = getRotatedHAndW sym
    let newPos = { X = newCenter.X - w / 2.0; Y = newCenter.Y - h / 2.0 }
    let newComponent = { sym.Component with X = newPos.X; Y = newPos.Y}

    { sym with Pos = newPos; Component = newComponent; LabelHasDefaultPos = true }


// Change: Add XML documentation (DUI)
/// <summary> HLP 23: AUTHOR Klapper - Rotates a symbol based on a degree, including: ports and component parameters. </summary>
/// <param name="degree">The rotation degree (Degree0, Degree90, Degree180, Degree270).</param>
/// <param name="sym">The symbol to rotate.</param>
/// <returns>The rotated symbol, or the original symbol if Degree0 is specified.</returns>
let rotateSymbolByDegree (degree: Rotation) (sym:Symbol)  =
    let symbolCenterPos = 
        { X = sym.Component.X + sym.Component.W / 2.0; 
          Y = sym.Component.Y + sym.Component.H / 2.0 }
    
    match degree with
    | Degree0 -> sym
    | _ ->  rotateSymbolInBlock degree symbolCenterPos sym
    

/// <summary>HLP 23: AUTHOR Ismagilov - Rotates a block of symbols, returning the new symbol model</summary>
/// <param name="compList"> List of ComponentId's of selected components</param>
/// <param name="model"> Current symbol model</param>
/// <param name="rotation"> Type of rotation to do</param>
/// <returns>New rotated symbol model</returns>
let rotateBlock (compList:ComponentId list) (model:SymbolT.Model) (rotation:Rotation) = 
    let selectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList
    let unselectedSymbols = model.Symbols |> Map.filter (fun x _ -> not (List.contains x compList))

    //Get block properties of selected symbols
    let block = getBlock selectedSymbols
    
    let invertedRotation = invertRotation rotation
    
    //Rotated symbols about the center
    let newSymbols = 
        List.map (fun x -> rotateSymbolInBlock invertedRotation (block.Centre()) x) selectedSymbols 

    let updatedSymbols = 
        newSymbols
        |> List.map2 (fun x y -> (x,y)) compList
        |> Map.ofList
        |> Map.fold (fun acc k v -> Map.add k v acc) unselectedSymbols
    
    //return model with block of rotated selected symbols, and unselected symbols
    { model with Symbols = updatedSymbols }

/// <summary>
/// Identifies the edge symbols within a list of symbols, along both X and Y axes
/// </summary>
/// <param name="selectedSymbols">The list of symbols to examine.</param>
/// <returns>
/// A tuple of four symbols which have the largest X, smallest X, largest Y, and smallest Y positions, respectively.
/// </returns>
let findOutermostSymbols (selectedSymbols: Symbol list) = 
    let symbolWithLargestEdge symbolEdge = 
        selectedSymbols |> List.maxBy symbolEdge
    
    let symbolWithSmallestEdge symbolEdge = 
        selectedSymbols |> List.minBy symbolEdge
    
    let maxXSym = symbolWithLargestEdge (fun sym -> sym.Pos.X + snd (getRotatedHAndW sym))
    let minXSym = symbolWithSmallestEdge (fun sym -> sym.Pos.X)
    let maxYSym = symbolWithLargestEdge (fun sym -> sym.Pos.Y + fst (getRotatedHAndW sym))
    let minYSym = symbolWithSmallestEdge (fun sym -> sym.Pos.Y)

    (maxXSym, minXSym, maxYSym, minYSym)
    
/// <summary>
/// Determines whether one symbol bounds both edges of a given list of symbols along a particular direction, either horizontally or vertically.
/// </summary>
/// <param name="selectedSymbols">List of symbols for evaluation.</param>
/// <returns>True if any symbol bounds both edges of all others along either axis, otherwise, false</returns>
let oneCompBoundsBothEdges (selectedSymbols: Symbol list) : bool = 
    let (maxXSym, minXSym, maxYSym, minYSym) = selectedSymbols |> findOutermostSymbols

    let maxXSymCentre = maxXSym |> getRotatedSymbolCentre
    let minXSymCentre = minXSym |> getRotatedSymbolCentre
    let maxYSymCentre = maxYSym |> getRotatedSymbolCentre
    let minYSymCentre = minYSym |> getRotatedSymbolCentre

    (maxXSymCentre.X = minXSymCentre.X) || (maxYSymCentre.Y = minYSymCentre.Y)

/// Retrieves a list of symbols from a model given a list of component ids.
let findSelectedSymbols (compList: ComponentId list) (model: SymbolT.Model) = 
    compList
    |> List.map (fun x -> model.Symbols |> Map.tryFind x) 
    |> List.choose id


/// <summary>
/// Calculates the scaling factor and offset centre based on given min/max values and the corresponding values to meet.
/// </summary>
/// <param name="originalMin">The original minimum value.</param>
/// <param name="targetMin">The target minimum value to match.</param>
/// <param name="originalMax">The original maximum value.</param>
/// <param name="targetMax">The target maximum value to match.</param>
/// <returns>A tuple consisting of the scaling factor and offset centre, respectively.</returns>
let getScalingFactorAndOffsetCentre (min:float) (matchMin:float) (max:float) (matchMax:float) = 
    let scaleFact = 
        if min = max || matchMax <= matchMin then 1. 
        else (matchMin - matchMax) / (min - max)     
    let offsetC =                                   
        if scaleFact = 1. then 0.
        else (matchMin - min * scaleFact) / (1.-scaleFact)  
    (scaleFact, offsetC)

/// Calculates the scaling factors and offsets required to fit a group of symbols within a specific bound box.
let getScalingFactorAndOffsetCentreGroup
    (matchBBMin:XYPos)
    (matchBBMax:XYPos)
    (selectedSymbols: Symbol list) : ((float * float) * (float * float)) = 
   
    
    let (maxXSym, minXSym, maxYSym, minYSym) = selectedSymbols |> findOutermostSymbols 
 
    let oldMaxX = (maxXSym |> getRotatedSymbolCentre).X
    let newMaxX = matchBBMax.X - (snd (getRotatedHAndW maxXSym))/2.

    let oldMinX = (minXSym |> getRotatedSymbolCentre).X
    let newMinX = matchBBMin.X + (snd (getRotatedHAndW minXSym))/2.

    let oldMaxY = (maxYSym |> getRotatedSymbolCentre).Y
    let newMaxY = matchBBMax.Y - (fst (getRotatedHAndW maxYSym))/2.
   
    let oldMinY = (minYSym |>  getRotatedSymbolCentre).Y
    let newMinY = matchBBMin.Y + (fst (getRotatedHAndW minYSym))/2.
    
    let xScalingAndOffset = getScalingFactorAndOffsetCentre oldMinX newMinX oldMaxX newMaxX
    let yScalingAndOffset = getScalingFactorAndOffsetCentre oldMinY newMinY oldMaxY newMaxY
    (xScalingAndOffset, yScalingAndOffset) 



/// Alter position and size of one symbol based on the scaling factors and offset in both X and Y directions
let scaleSymbol
        (xyScalingAndOffset: (float * float) * (float * float))
        (sym: Symbol)
        : Symbol = 
    let symCentre =  getRotatedSymbolCentre sym
    let translateFunc scaleFact offsetC coordinate = (coordinate - offsetC) * scaleFact + offsetC
    let (xScale, xOffset) = fst xyScalingAndOffset
    let (yScale, yOffset) = snd xyScalingAndOffset
    let newX = translateFunc (xScale) (xOffset) symCentre.X
    let newY = translateFunc (yScale) (yOffset) symCentre.Y

    let symCentreOffsetFromTopLeft = {X = (snd (getRotatedHAndW sym))/2.; Y = (fst (getRotatedHAndW sym))/2.}
    let newTopLeftPos = {X = newX; Y = newY} - symCentreOffsetFromTopLeft
    let newComp = {sym.Component with X = newTopLeftPos.X; Y = newTopLeftPos.Y}

    {sym with Pos = newTopLeftPos; Component = newComp; LabelHasDefaultPos = true}

/// Part of the rotate and scale code       
/// Part of the rotate and scale code
let groupNewSelectedSymsModel
    (compList:ComponentId list)
    (model:SymbolT.Model)
    (selectedSymbols: Symbol list)
    (modifySymbolFunc) =

    // hng21: Could use findSelectedSymbols again here, further eliminating duplicated code,
    // but this would require refactoring some of SheetUpdateHelper.mDragUpdate

    //let SelectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList
    let UnselectedSymbols = model.Symbols |> Map.filter (fun x _ -> not (List.contains x compList))

    // let block = getBlock SelectedSymbols
    // printfn "bbCentreX:%A" (block.Centre()).X

    // let newSymbols = List.map (modifySymbolFunc (block.Centre())) SelectedSymbols
    let newSymbols = List.map (modifySymbolFunc) selectedSymbols

    {model with Symbols =
                 // hng21: Replaced list.map2 with list.zip
                ((Map.ofList (List.zip compList newSymbols)
                |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols)
    )}


/// <summary>HLP 23: AUTHOR Ismagilov - Flips a block of symbols, returning the new symbol model</summary>
/// <param name="compList"> List of ComponentId's of selected components</param>
/// <param name="model"> Current symbol model</param>
/// <param name="flip"> Type of flip to do</param>
/// <returns>New flipped symbol model</returns>
let flipBlock (compList:ComponentId list) (model:SymbolT.Model) (flip:FlipType) =
    //Similar structure to rotateBlock, easy to understand

    // hng21: DRY - Use existing findSelectedSymbols function
    let SelectedSymbols = findSelectedSymbols compList model
    let block = getBlock SelectedSymbols
    let modifyFun = flipSymbolInBlock flip (block.Centre())

    // hng21: DRY - de duplicated code by using existing function
    groupNewSelectedSymsModel compList model SelectedSymbols modifyFun


/// After every model update this updates the "scaling box" part of the model to be correctly
/// displayed based on whether multiple components are selected and if so what is their "box"
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
            // hng21: Renamed variable to avoid confusion with the id function
            |> List.map (fun compId -> Map.find compId model.Wire.Symbol.Symbols)
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

            // hng21: Changed indenting to better align pipe
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
