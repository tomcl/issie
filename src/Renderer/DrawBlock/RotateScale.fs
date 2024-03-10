// Due to reformating in merge, line numbers will not match the code in desciption

(*Part 1: hj1021 
    <improvement summary>
    Changes are made to to WireSymbols, getOppEdgePortInfo, alignPortsOffset, alignSymbols and reSizeSymbol
    And add XML comments to each function

    WireSymbols:
    - Changed the names of the fields to sourceSym and targetSym to make it more readable.
    - And change all its references to the new names.

    alignPortsOffset:
    -Extracted the getPortRealPos function from alignPortsOffset and used it as a helper function.
    -Shortened match cases and increased readability.

    alignSymbols:
    -No changes made, as it is already well structured and readable.
    -Add some comments describ how well it is structured and readable.

    reSizeSymbol:
    -No changes made, as it is already well structured and readable.
    -Add some comments describ how well it is structured and readable. 
*)

(* Part 2 yc3821:  the improvement is at line 131 - 194 (or search yc3821 to see all improved code)
    Summary:
    - Created a DU instead of separate variables for the port pair
    - Extracted the function getPortInfoPair to improve readability and reusability
    - improved some variable names
    - minor pipeline improvements to enhance readability
*)

(* Part 3: Improveing RotateScale module - ec1221
    Split lines: 175 - 268
    - Changed nested match cases to make it more readable
    - Removed some intermediate variables
    - Changing match statements to reduce clutter
    - Changed layout of function to group functions and main body
    - Changed function parameters to allow better pipeline of functions
    - Added to function optimizeSymbol name to make it more clear what is being optimized
    - refactoring of code to remove code duplication (noSymbolOverlap and Sheet.notIntersectingComponents)
    - comments

    This module contains the code that rotates and scales blocks of components.
    It was collected from HLP work in 2023 and has some technical debt and also unused functions.
    It requires better documentation of teh pasrts now used.
*)

(* Part 4: hn621
    List of Improvements (partition lines 266-524):
    - 275: Renamed "getBlock" function into "getSymbolBlockBoundingBox", the notion of block's type is not clear
    - 275-294: Applied transformation 1 (functional abstraction) to the "getSymbolBlockBoundingBox" function, reduced the repeated use of List.maxBy and List.minBy and made the code more readable
    - 506-523: in "rotateBlock", removed the redundant calculation of unselected symbols
    - 518: changed "newSymbols" to output Map<cid,sym> type (as this is the type of model.Symbols), initially was List<Symbol> which can cause confusion
    - 522: by doing the above, also simplified the record update line making it much more readable
    - Changed parameter name in annonymous function to be more meaningful, e.g. (fun x:Symbol -> ...) , changed to (fun sym:Symbol -> ...), Examples in line 509, 517
    - Changed the documentation of rotation functions: the documentation of rotation states direction can be CW or AntiCW, but the code only supports CW.
    - Added parameter isClockwise in the "rotatePointAboutBlockCentre", so that the function can support both CW and AntiCW rotation as specified
*)

(*
    Part 5: LL3621 improvement summary: My part goes from line 474 to 686
    Main changes made by ll3621:
    Improve readability by seprating complex nested functions into a pipeline
    Reduce the complexity of the functions by using more suitable functions/types
    Function and variable renaming to make more sense and stick with their type
    Some changes to function signatures to make sure they are consistant with other functions and easier to use
    changes to each line are commented after the line changed and stated the reason
    good functions are remained unchanged with a comment saying why I havent changed it.
*)

(*Part 6: rl3721
changes are made to the functions flipblock and postUpdateScalingBox

flipBlock:
- changed the type of SelectedSymbols to map to be consistent with UnselectedSymbols
- added a pipeline to find the boundingBox block to support the above change of type to map
- renamed newSymbols to newFlippedSymbols to be more intuitive
- rewrote return logic with additional pipeline to be more readable

postUpdateScalingBox:
- a new pipeline for the return model, to make the return logic more readable
- reorderd subfunctions to be more readable
- rewrote the makeRotateSym function with optics
- removed use of a dummy variable: dummyPos
- renamed ButtonSym, rotateDeg90Sym and rotateDeg270Sym to
    scaleButtonSym, rotateDegButton90Sym and rotateDeg270ButtonSym, to give more information on the purpose
    also to make the names more "parallel" with each other as they logically are in the code
- changed vertical allignment of the code to be more readable
- commented on some ugly use of float literals in the code, unable to tell how the literals are infered
*)



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



//--------------------------------------start of hj1021 section ----------------------------------------//

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
    // hj1021 In my point of view symA and symB are bad names
    // better change them to sourceSym and targetSym
    { sourceSym: Symbol
      targetSym: Symbol
      Wire: Wire }

/// TODO: this is mostly copy pasted code from Symbol.getPortPos, perhaps abstract out the existing code there to use makePortInfo.
/// Could not simply use getPortPos because more data (side, topBottomGap, etc.) is needed to caclulate the new dimensions of the resized symbol.
let makePortInfo (sym: Symbol) (port: Port) =
    // hj1021 good illustration of input wrapping
    let side = getSymbolPortOrientation sym port
    let ports = sym.PortMaps.Order[side] //list of ports on the same side as port
    let gap = getPortPosEdgeGap sym.Component.Type
    let topBottomGap = gap + 0.3 // extra space for clk symbol
    let portDimension = float ports.Length - 1.0
    let h, w = getRotatedHAndW sym

    //hj1021 shorten the code and increase readability
    let portGap =
        match side with
        | Left   | Right -> float h / (portDimension + 2.0 * gap)
        | Bottom | Top   -> float w / (portDimension + 2.0 * topBottomGap)

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

// hj1021 rename it from get PortAB to getSourceTargetPorts
/// Get source and target port of a wire
let getSourceTargetPorts wModel wireSyms =
    let ports = portsOfWires wModel [ wireSyms.Wire ]
    let sourcePort = filterPortBySym ports wireSyms.sourceSym |> List.head
    let targetPort = filterPortBySym ports wireSyms.targetSym |> List.head
    sourcePort, targetPort

/// Try to get two ports that are on opposite edges.
let getOppEdgePortInfo
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    : (PortInfo * PortInfo) option =
    let wires = wiresBtwnSyms wModel symbolToSize otherSymbol

    let tryGetOppEdgePorts wireSyms =
        let sourcePort, targetPort = getSourceTargetPorts wModel wireSyms
        let edgeA = getSymbolPortOrientation wireSyms.sourceSym sourcePort
        let edgeB = getSymbolPortOrientation wireSyms.targetSym targetPort

        match edgeA = edgeB.Opposite with
        | true -> Some(makePortInfo wireSyms.sourceSym sourcePort, makePortInfo wireSyms.targetSym targetPort)
        | _ -> None

    wires
    |> List.tryPick (fun w ->
        tryGetOppEdgePorts
            { sourceSym = symbolToSize
              targetSym = otherSymbol
              Wire = w })

// hj1021 Functional (and let value definition) abstraction: 
// Extract this function from the alignPortsOffset as it seems like a useful helper function
// that has the potential to be used several times. (DRY)
let getPortRealPos pInfo =
        getPortPos pInfo.sym pInfo.port + pInfo.sym.Pos


/// Calculate the position offset needed to align potrs
let alignPortsOffset (movePInfo: PortInfo) (otherPInfo: PortInfo) =

    let movePortPos = getPortRealPos movePInfo
    let otherPortPos = getPortRealPos otherPInfo
    let posDiff = otherPortPos - movePortPos

    //hj1021 shorten the code and increase readability
    match movePInfo.side with
    | Top  | Bottom -> { X = posDiff.X; Y = 0.0 }
    | Left | Right  -> { X = 0.0; Y = posDiff.Y }

/// Align symbols by moving it by an offset
let alignSymbols
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    : BusWireT.Model =

    // Only attempt to align symbols if they are connected by ports on parallel edges.
    match getOppEdgePortInfo (wModel:BusWireT.Model) symbolToSize otherSymbol with
    | None -> wModel
    | Some(movePortInfo, otherPortInfo) ->
        //hj1021 Good illustration of input wrapping
        let offset = alignPortsOffset movePortInfo otherPortInfo
        let symbol' = moveSymbol offset symbolToSize
        let model' = Optic.set (symbolOf_ symbolToSize.Id) symbol' wModel
        BusWireSeparate.routeAndSeparateSymbolWires model' symbolToSize.Id
        
//--------------------------------------end of hj1021 section ----------------------------------------//
// ----------------------- yc3821: improved parts start here -----------------------



/// HLP23: To test this, it must be given two symbols interconnected by wires. It then resizes symbolToSize
/// so that the connecting wires are exactly straight
/// HLP23: It should work out the interconnecting wires (wires) from
/// the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has adjustable size).
type ResizePortPairInfo = { ResizePortInfo: PortInfo; OtherPortInfo: PortInfo }

/// yc3821: Try to get two ports that are on opposite edges, if none found just use any two ports. (Improvement on "Type" and "Function")
let getPortInfoPair (wModel: BusWireT.Model) (symbolToSize: Symbol) (otherSymbol: Symbol) : ResizePortPairInfo =
    match getOppEdgePortInfo wModel symbolToSize otherSymbol with
    | None ->
        // yc3821: moved wires variable here to enhance readability
        let wires = wiresBtwnSyms wModel symbolToSize otherSymbol
        let pA, pB = getPortAB wModel { SymA = symbolToSize; SymB = otherSymbol; Wire = wires[0] }  // yc3821: wire[0] is arbitrary
        {ResizePortInfo = makePortInfo symbolToSize pA; OtherPortInfo = makePortInfo symbolToSize pB }
    | Some(pIA, pIB) -> { ResizePortInfo = pIA; OtherPortInfo = pIB }


let reSizeSymbol (wModel: BusWireT.Model) (symbolToSize: Symbol) (otherSymbol: Symbol) : (Symbol) =


    // yc3821: get the ResizePortPairInfo
    let portInfoPair = getPortInfoPair wModel symbolToSize otherSymbol

    // yc3821: better naming for h and w
    let newHeight, newWidth =
        match portInfoPair.ResizePortInfo.side with

        | Left | Right ->
            portInfoPair.OtherPortInfo.portGap * (portInfoPair.ResizePortInfo.portDimension + 2.0 * portInfoPair.ResizePortInfo.gap), portInfoPair.ResizePortInfo.w
        | Top | Bottom ->
            portInfoPair.ResizePortInfo.h, portInfoPair.OtherPortInfo.portGap * (portInfoPair.ResizePortInfo.portDimension + 2.0 * portInfoPair.ResizePortInfo.topBottomGap)

    match symbolToSize.Component.Type with
    | Custom _ ->

        // yc3821: added pipeline to enhance readability
        let scaledSymbol = symbolToSize |> setCustomCompHW newHeight newWidth 
        let scaledInfo = makePortInfo scaledSymbol portInfoPair.ResizePortInfo.port
        let offset = alignPortsOffset scaledInfo portInfoPair.OtherPortInfo

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

    let model' = Optic.set (symbolOf_ symbolToSize.Id) scaledSymbol wModel
    BusWireSeparate.routeAndSeparateSymbolWires model' symbolToSize.Id



// ----------------------- yc3821: improved parts end here -----------------------
//--------------------------------------start of ec1221 section ----------------------------------------//

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
// ec1221 - adjusted function to be usable in Sheet.notIntersectingComponents to remove code duplication
let noSymbolOverlap boundingBoxes box inputId =
    boundingBoxes
    |> Map.filter (fun sId boundingBox -> DrawHelpers.boxesIntersect boundingBox box && inputId <> sId)
    |> Map.isEmpty

/// Finds the optimal size and position for the selected symbol w.r.t. to its surrounding symbols.
let optimiseSymbolDimensions
    (wModel: BusWireT.Model)
    (symbol: Symbol)
    (boundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>)
    : BusWireT.Model =

    // If a wire connects the target symbol to another symbol, note which edge it is connected to
    let updateData (symConnData: SymConnDataT) _ (wire: Wire) =
        let symS, symT = getSourceSymbol wModel wire, getTargetSymbol wModel wire
        // symbolSource, symbolTarget

        match symS, symT with
        | _ when (symS.Id <> symbol.Id) && (symT.Id = symbol.Id) -> Some symS
        | _ when (symS = symbol) && (symT <> symbol) -> Some symT
        | _ -> None

        // ec1221 - changed nested match statements dealing with option variables to use built in Option functions to
        // reduce clutter and lines. Removed intermediate variable assignments that were not necessary 
        |> Option.bind (fun otherSym ->
            tryWireSymOppEdge wModel wire symbol otherSym
            |> Option.map (fun edge ->
                updateOrInsert symConnData edge otherSym.Id))
        |> Option.defaultValue symConnData

    // ec1221 - swapped parameter order to allow better pipelining
    let tryResize (sym: Symbol) (symCount: ((ComponentId * Edge) * int) array) =

        // align symbol according to other symbols and resize when no overlap
        let alignSym (sym: Symbol) (otherSym: Symbol) =
            let resizedSym = reSizeSymbol wModel sym otherSym
            let symBB = getSymbolBoundingBox resizedSym
            let noOverlap = noSymbolOverlap boundingBoxes symBB resizedSym.Id
            // ec1221 - changed match to if statement as variable names are good and makes code more readable
            if noOverlap then (true, resizedSym) else (false, sym)

        // Function to fold over symCount and adjust alignment and size
        let folder (hAligned, vAligned, sym) ((cid, edge), _) =
            let otherSym = Optic.get (symbolOf_ cid) wModel       

            // ec1221 - added 'edge' to match condition
            match edge, hAligned, vAligned with
            | (Top | Bottom), false, _ ->
                let hAligned', resizedSym = alignSym sym otherSym
                (hAligned', vAligned, resizedSym)
            | (Left | Right), _, false ->
                let vAligned', resizedSym = alignSym sym otherSym
                (hAligned, vAligned', resizedSym)
            | _ -> (hAligned, vAligned, sym)

        let (_, _, sym') = ((false, false, sym), symCount) ||> Array.fold folder
        sym'

    // ec1221 - moved it down to the rest of the main body
    // Look through all wires to build up SymConnDataT.
    let symConnData = ({ ConnMap = Map.empty }, wModel.Wires) ||> Map.fold updateData

    // Get the count of connections and resize the symbol
    let scaledSymbol =
        Map.toArray symConnData.ConnMap
        |> Array.filter (fun (_, count) -> count > 1)
        |> Array.sortByDescending snd
        |> tryResize symbol

    // Update the model with the resized symbol and separate wires
    let model' = Optic.set (symbolOf_ symbol.Id) scaledSymbol wModel
    BusWireSeparate.routeAndSeparateSymbolWires model' symbol.Id


//--------------------------------------end of ec1221 section ----------------------------------------//
//--------------------------------------start of hn621 section ----------------------------------------//


/// <summary>HLP 23: AUTHOR Ismagilov - Get the bounding box of multiple selected symbols</summary>
/// <param name="symbols"> Selected symbols list</param>
/// <returns>Bounding Box</returns>
let getSymbolBlockBoundingBox (symbols:Symbol List) :BoundingBox = 
    let getMaxPosByAxis (selectAxis:XYPos->float) (selectDimension:float*float->float) (symbols: Symbol list) = 
        symbols
        |> List.map (fun (sym:Symbol) -> selectAxis sym.Pos + selectDimension (getRotatedHAndW sym)) 
        |> List.max
        
    let getMinPosByAxis (selectAxis:XYPos->float) (symbols: Symbol list) = 
        symbols
        |> List.map (fun (sym:Symbol) -> selectAxis sym.Pos)
        |> List.min

    let selectX = (fun (pos: XYPos) -> pos.X)
    let selectY = (fun (pos: XYPos) -> pos.Y)

    let maxX = getMaxPosByAxis selectX snd symbols
    let minX = getMinPosByAxis selectX symbols
    let maxY = getMaxPosByAxis selectY fst symbols
    let minY = getMinPosByAxis selectY symbols

    {TopLeft = {X = minX; Y = minY}; W = maxX-minX; H = maxY-minY}

/// <summary>HLP 23: AUTHOR Ismagilov - Takes a point Pos, a centre Pos, and a rotation type and returns the point flipped about the centre</summary>
/// <param name="point"> Original XYPos</param>
/// <param name="center"> The center XYPos that the point is rotated about</param>
/// <param name="rotation"> Clockwise or Anticlockwise </param>
/// <returns>New flipped point</returns>
let rotatePointAboutBlockCentre 
            (point:XYPos) 
            (centre:XYPos) 
            (rotation:Rotation) 
            (isClockwise:bool) = 
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
    |> match isClockwise with
        | true -> id 
        | false -> (fun x -> {X = -x.X; Y = -x.Y}) // MOD: allow anti-clockwise rotation
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
/// <param name="rotation"> Rotated CW</param>
/// <param name="h"> Original height of symbol (Before rotation)</param>
/// <param name="w"> Original width of symbol (Before rotation)</param>
/// <param name="pos"> XYPos</param>
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
        | Degree90 -> {X = h; Y =0}
        | Degree180 -> {X = w; Y = -h}
        | Degree270 -> {X = 0; Y = w}
    pos - posOffset

/// <summary>HLP 23: AUTHOR Ismagilov - Get the new top left of a symbol after it has been flipped</summary>
/// <param name="flip">  Flipped horizontally or vertically</param>
/// <param name="h"> Original height of symbol (Before flip)</param>
/// <param name="w"> Original width of symbol (Before flip)</param>
/// <param name="pos"> XYPos</param>
/// <returns>New top left point of the symbol</returns>
let adjustPosForBlockFlip
        (flip:FlipType) 
        (h: float)
        (w:float)
        (pos: XYPos) =
    let posOffset =
        match flip with
        | FlipHorizontal -> {X=w ;Y=0}
        | FlipVertical -> {X=0 ;Y=h}
    pos - posOffset

/// <summary>HLP 23: AUTHOR Ismagilov - Rotate a symbol in its block.</summary>
/// <param name="rotation">  Clockwise or Anticlockwise rotation</param>
/// <param name="blockCentre"> Bounding box centre of selected components</param>
/// <param name="sym"> Symbol to be rotated</param>
/// <returns>New symbol after rotated about block centre.</returns>
let rotateSymbolInBlock 
        (rotation: Rotation) 
        (blockCentre: XYPos)
        (sym: Symbol)  : Symbol =
      
    let h,w = getRotatedHAndW sym

    let newTopLeft = 
        rotatePointAboutBlockCentre sym.Pos blockCentre (invertRotation rotation) true
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
    //Needed as new symbols need their Pos and components updated (not done in regular flip symbol)
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

    { sym with
        Component = {sym.Component with X=newTopLeft.X; Y=newTopLeft.Y}
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
            let newblock = getSymbolBlockBoundingBox [sym]
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
    let newPos: XYPos = {X = (newCenter.X) - w/2.; Y= (newCenter.Y) - h/2.}
    let newComponent = { sym.Component with X = newPos.X; Y = newPos.Y}

    {sym with Pos = newPos; Component=newComponent; LabelHasDefaultPos=true}

//--------------------------------------end of hn621 section ----------------------------------------//
//--------------------------------------start of ll3621 section ----------------------------------------//

///<summary> Main changes made by ll3621:
/// Improve readability by seprating complex nested functions into a pipeline
/// Reduce the complexity of the functions by using more suitable functions/types
/// Function and variable renaming to make more sense or stick with their type
/// changes to each line are commented after the line changed and stated the reason</summary>

/// HLP 23: AUTHOR Klapper - Rotates a symbol based on a degree, including: ports and component parameters.
/// 

let rotateSymbolByDegree (degree: Rotation) (sym:Symbol)  =

    (*old implementation*)
    // let pos = {X = sym.Component.X + sym.Component.W / 2.0 ; Y = sym.Component.Y + sym.Component.H / 2.0 }
    
    (*new implementation*)
    let centerPos= sym.CentrePos 
    // this is a easier implementation then above, and as the type for pos is blockcenter, centerpos would be better name
    match degree with
    | Degree0 -> sym
    | _ ->  rotateSymbolInBlock degree centerPos sym
    

/// <summary>HLP 23: AUTHOR Ismagilov - Rotates a block of symbols, returning the new symbol model</summary>
/// <param name="compList"> List of ComponentId's of selected components</param>
/// <param name="model"> Current symbol model</param>
/// <param name="rotation"> Type of rotation to do</param>
/// <returns>New rotated symbol model</returns>
let rotateBlock (compList:ComponentId list) (model:SymbolT.Model) (rotation:Rotation) = 


    // printfn "running rotateBlock"
    let SelectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList
    let UnselectedSymbols = model.Symbols |> Map.filter (fun x _ -> not (List.contains x compList))


    //Get block properties of selected symbols
    let block = getSymbolBlockBoundingBox SelectedSymbols

    //Rotated symbols about the center

    (*old implementation*)
    // let newSymbols = 
    //     List.map (fun x -> rotateSymbolInBlock (invertRotation rotation) (block.Centre()) x) SelectedSymbols 

    // //return model with block of rotated selected symbols, and unselected symbols
    // {model with Symbols = 
    //             ((Map.ofList (List.map2 (fun x y -> (x,y)) compList newSymbols)
    //             |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols)
    // )}

    (*new implementation*)
    let modifiedSymbolList = 
        SelectedSymbols 
        |>List.map (fun x -> rotateSymbolInBlock (invertRotation rotation) (block.Centre()) x)  
        // changed names to match with type , and clearer pipeline
    let newSymbols= //gave newsymbol's name to this variable because the Symbol in model type has this signature
        modifiedSymbolList   //split the long assigning symbols as a seperate variablea nd pipeline to increase readability
        |> List.map2 (fun x y -> (x,y)) compList 
        |> Map.ofList
        |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols
    {model with Symbols=newSymbols}



let oneCompBoundsBothEdges (selectedSymbols: Symbol list) = 
    (*comment: this function is well written, clear and pipelined with good naems, no change*)
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
    (*comment:
    don't really understand why matchMin is called matchMin, what's the match about it? not clear but don't know what to change*)
    (*old implementation*)
    // let scaleFact = 
    //     if min = max || matchMax <= matchMin then 1. 
    //     else (matchMin - matchMax) / (min - max)
    // let offsetC = 
    //     if scaleFact = 1. then 0.
    //     else (matchMin - min * scaleFact) / (1.-scaleFact)
    // (scaleFact, offsetC)

    (*new implementation*)
    let scaleFactor = //changed name from scalefact to scaleFactor, because scalefact doesn't makesense and scalefactor is not much longer
        if min = max || matchMax <= matchMin then 1. 
        else (matchMin - matchMax) / (min - max)
    let offsetCenter = //same as above, offsetCenter is not much longer but much clearer than offsetC
        if scaleFactor = 1. then 0.
        else (matchMin - min * scaleFactor) / (1.-scaleFactor)
    (scaleFactor, offsetCenter)

/// Return set of floats that define how a group of components is scaled
let getScalingFactorAndOffsetCentreGroup
    (matchBBMin:XYPos)
    (matchBBMax:XYPos)

    (selectedSymbols: Symbol list) : ((float * float) * (float * float)) = 
    (*comment: this is done relatively neat as there are a lot of different variables, instead of putting them into 4 giant pipelines
    it is better to leave them out like this, no changes needed*)

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


// Alter position of one symbol as needed in a scaling operation
// 
(*old implementation*)
// let scaleSymbol
//         (xYSC: (float * float) * (float * float))
//         (sym: Symbol)
//         : Symbol = 
//     let symCentre =  getRotatedSymbolCentre sym
//     let translateFunc scaleFact offsetC coordinate = (coordinate - offsetC) * scaleFact + offsetC
//     let xSC = fst xYSC
//     let ySC = snd xYSC
//     let newX = translateFunc (fst xSC) (snd xSC) symCentre.X
//     let newY = translateFunc (fst ySC) (snd ySC) symCentre.Y

//     let symCentreOffsetFromTopLeft = {X = (snd (getRotatedHAndW sym))/2.; Y = (fst (getRotatedHAndW sym))/2.}
//     let newTopLeftPos = {X = newX; Y = newY} - symCentreOffsetFromTopLeft
//     let newComp = {sym.Component with X = newTopLeftPos.X; Y = newTopLeftPos.Y}

//     {sym with Pos = newTopLeftPos; Component = newComp; LabelHasDefaultPos = true}


(*new implementation: *)
let scaleSymbol 
        ((xSC,ySC): (float * float) * (float * float)) 
        // changed xySC to xSC,ySC tuple to save the need of giving them a value, less lines, also does better with the function above

        (sym: Symbol)
        : Symbol =
    let symCentre =  getRotatedSymbolCentre sym
    let translateFunc (scaleFact,offsetC) coordinate = (coordinate - offsetC) * scaleFact + offsetC 
    // grouped first 2 inputs of translateFunc in a tuple so they are used more easily with the tuple input xSC and ySC
    let newX = translateFunc xSC symCentre.X //shorter and more readable compared to above
    let newY = translateFunc ySC symCentre.Y
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

    let UnselectedSymbols = model.Symbols |> Map.filter (fun x _ -> not (List.contains x compList))
    
    

    (*old implementation*)
    // let newSymbols = List.map (modifySymbolFunc) selectedSymbols
    // {model with Symbols = 
    //             ((Map.ofList (List.map2 (fun x y -> (x,y)) compList newSymbols)
    //             |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols)
    // )}

    (*new implementation *)
    let modifiedSymbolList = List.map (modifySymbolFunc) selectedSymbols // changed names to match with type 
    let newSymbols= //gave newsymbol's name to this variable because the Symbol in model type has this signature
        modifiedSymbolList   //split the long assigning symbols as a seperate variablea nd pipeline to increase readability
        |> List.map2 (fun x y -> (x,y)) compList 
        |> Map.ofList
        |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols
    {model with Symbols=newSymbols}





//--------------------------------------end of ll3621 section ----------------------------------------//
//--------------------------------------start of rl3721 section ----------------------------------------//

(*Old implementation*) 
// let flipBlock (compList:ComponentId list) (model:SymbolT.Model) (flip:FlipType) = 
//     //Similar structure to rotateBlock, easy to understand
//     let SelectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList
//     let UnselectedSymbols = model.Symbols |> Map.filter (fun x _ -> not (List.contains x compList))
    
//     let block = getBlock SelectedSymbols
  
//     let newSymbols = 
//         List.map (fun x -> flipSymbolInBlock flip (block.Centre()) x ) SelectedSymbols

//     {model with Symbols = 
//                 ((Map.ofList (List.map2 (fun x y -> (x,y)) compList newSymbols)
//                 |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols)
//     )}


/// <summary>HLP 23: AUTHOR Ismagilov - Flips a block of symbols, returning the new symbol model</summary>
/// <param name="compList"> List of ComponentId's of selected components</param>
/// <param name="model"> Current symbol model</param>
/// <param name="flip"> Type of flip to do</param>
/// <returns>New flipped symbol model</returns>
let flipBlock (compList:ComponentId list) (model:SymbolT.Model) (flip:FlipType) = 
    let SelectedSymbols = model.Symbols |> Map.filter (fun x _ -> List.contains x compList) (*changed type to map, consistent with UnselectedSymbols to be more intuitive*)
    let UnselectedSymbols = model.Symbols |> Map.filter (fun x _ -> not (List.contains x compList))
    
    let block =
        SelectedSymbols
        |> Map.toList
        |> List.map snd
        |> getSymbolBlockBoundingBox

  
    let newFlippedSymbols = (*rewrote the code with better allignment in pipeline form to be more readable*)
        SelectedSymbols
        |> Map.map (fun key x -> flipSymbolInBlock flip (block.Centre()) x )
        |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols

    {model with Symbols = newFlippedSymbols}


(*Old implementation*)
// let postUpdateScalingBox (model:SheetT.Model, cmd) = 
    
//     let symbolCmd (msg: SymbolT.Msg) = Elmish.Cmd.ofMsg (ModelType.Msg.Sheet (SheetT.Wire (BusWireT.Symbol msg)))
//     let sheetCmd (msg: SheetT.Msg) = Elmish.Cmd.ofMsg (ModelType.Msg.Sheet msg)

//     if (model.SelectedComponents.Length < 2) then 
//         match model.ScalingBox with 
//         | None ->  model, cmd
//         | _ -> {model with ScalingBox = None}, 
//                 [symbolCmd (SymbolT.DeleteSymbols (model.ScalingBox.Value).ButtonList);
//                  sheetCmd SheetT.UpdateBoundingBoxes]
//                 |> List.append [cmd]
//                 |> Elmish.Cmd.batch
//     else 
//         let newBoxBound = 
//             model.SelectedComponents
//             |> List.map (fun id -> Map.find id model.Wire.Symbol.Symbols)
//             |> getBlock
//         match model.ScalingBox with 
//         | Some value when value.ScalingBoxBound = newBoxBound -> model, cmd
//         | _ -> 
//             let topleft = newBoxBound.TopLeft
//             let rotateDeg90OffSet: XYPos = {X = newBoxBound.W+57.; Y = (newBoxBound.H/2.)-12.5}
//             let rotateDeg270OffSet: XYPos = {X = -69.5; Y = (newBoxBound.H/2.)-12.5}
//             let buttonOffSet: XYPos = {X = newBoxBound.W + 47.5; Y = -47.5}
//             let dummyPos = (topleft + buttonOffSet)

//             let makeButton = SymbolUpdate.createAnnotation ThemeType.Colourful
//             let buttonSym = {makeButton ScaleButton dummyPos with Pos = (topleft + buttonOffSet)}
//             let makeRotateSym sym = {sym with Component = {sym.Component with H = 25.; W=25.}}
//             let rotateDeg90Sym = 
//                 makeButton (RotateButton Degree90) (topleft + rotateDeg90OffSet)
//                 |> makeRotateSym
//             let rotateDeg270Sym = 
//                 {makeButton (RotateButton Degree270) (topleft + rotateDeg270OffSet) 
//                     with SymbolT.STransform = {Rotation=Degree90 ; Flipped=false}}
//                 |> makeRotateSym

//             let newSymbolMap = model.Wire.Symbol.Symbols 
//                                                         |> Map.add buttonSym.Id buttonSym 
//                                                         |> Map.add rotateDeg270Sym.Id rotateDeg270Sym 
//                                                         |> Map.add rotateDeg90Sym.Id rotateDeg90Sym
//             let initScalingBox: SheetT.ScalingBox = {
//                 ScalingBoxBound = newBoxBound;
//                 ScaleButton = buttonSym;
//                 RotateDeg90Button = rotateDeg90Sym;
//                 RotateDeg270Button = rotateDeg270Sym;
//                 ButtonList = [buttonSym.Id; rotateDeg270Sym.Id; rotateDeg90Sym.Id];
//             }
//             let newCmd =
//                 match model.ScalingBox with
//                 | Some _ -> [symbolCmd (SymbolT.DeleteSymbols (model.ScalingBox.Value).ButtonList);
//                              sheetCmd SheetT.UpdateBoundingBoxes]
//                             |> List.append [cmd]
//                             |> Elmish.Cmd.batch
//                 | None -> cmd
//             model
//             |> Optic.set SheetT.scalingBox_ (Some initScalingBox)
//             |> Optic.set SheetT.symbols_ newSymbolMap, 
//             newCmd


(*changed xml comment*)
/// <summary> Updates the scaling box of the symbol model</summary>, update commands that make further changes
/// <param name="model"> current sheet model</param>
/// <param name="model"> current command</param>
/// <returns>New model with scaling box updated, new command that contain messages to make additional changes</returns>
let postUpdateScalingBox (model:SheetT.Model, cmd) = 
    
    let symbolCmd (msg: SymbolT.Msg) = Elmish.Cmd.ofMsg (ModelType.Msg.Sheet (SheetT.Wire (BusWireT.Symbol msg)))
    let sheetCmd (msg: SheetT.Msg) = Elmish.Cmd.ofMsg (ModelType.Msg.Sheet msg)

    if (model.SelectedComponents.Length < 2) then 
        match model.ScalingBox with 
        | None ->  model, cmd
        | _ -> (*changed vertical allignment slightly*)
            let newCmd =
                [symbolCmd (SymbolT.DeleteSymbols (model.ScalingBox.Value).ButtonList); sheetCmd SheetT.UpdateBoundingBoxes]
                |> List.append [cmd]
                |> Elmish.Cmd.batch
            {model with ScalingBox = None}, newCmd
    else 
        let newBoxBound = 
            model.SelectedComponents
            |> List.map (fun id -> Map.find id model.Wire.Symbol.Symbols)
            |> getSymbolBlockBoundingBox
        match model.ScalingBox with 
        | Some value when value.ScalingBoxBound = newBoxBound -> model, cmd
        | _ -> 
            let topleft = newBoxBound.TopLeft

            (*The float constants used in the following 3 lines are very confusing. It is hard to tell where the values are infered From.
            1: In the ugly case they are values imperially tested for the function to work temporarily, comments should be written on how it was tested, and noting how it can be changed from 
                the temporary implementation to a more general one.
            2: In the bad case that these values are infered from parameters such as default symbol size or boundingBox dimensions, they should be included in the
                correspounding upper level module and be referenced here*)
            let rotateDeg90OffSet: XYPos = {X = newBoxBound.W+57.; Y = (newBoxBound.H/2.)-12.5}
            let rotateDeg270OffSet: XYPos = {X = -69.5; Y = (newBoxBound.H/2.)-12.5}
            let scaleButtonOffSet: XYPos = {X = newBoxBound.W + 47.5; Y = -47.5}

            let makeButton = SymbolUpdate.createAnnotation ThemeType.Colourful
            let makeRotateSym sym = Optic.set component_ {sym.Component with H = 25.; W=25.} sym (*rewritten with optics*)

            let scaleButtonSym = (*renamed buttonOffSet to scaleButtonOffSet to give more insight into its purpose*)
                makeButton ScaleButton (topleft + scaleButtonOffSet) (*removed a dummyPos variable which is not used anywhere else*)
            let rotateDeg90ButtonSym = 
                makeButton (RotateButton Degree90) (topleft + rotateDeg90OffSet)
                |> makeRotateSym
            let rotateDeg270ButtonSym = 
                {makeButton (RotateButton Degree270) (topleft + rotateDeg270OffSet) 
                    with SymbolT.STransform = {Rotation=Degree90 ; Flipped=false}}
                |> makeRotateSym

            let newSymbolMap = (*changed vertical allignment slightly*)
                model.Wire.Symbol.Symbols 
                    |> Map.add scaleButtonSym.Id scaleButtonSym 
                    |> Map.add rotateDeg270ButtonSym.Id rotateDeg270ButtonSym 
                    |> Map.add rotateDeg90ButtonSym.Id rotateDeg90ButtonSym

            let initScalingBox: SheetT.ScalingBox = {
                ScalingBoxBound = newBoxBound;
                ScaleButton = scaleButtonSym;
                RotateDeg90Button = rotateDeg90ButtonSym;
                RotateDeg270Button = rotateDeg270ButtonSym;
                ButtonList = [scaleButtonSym.Id; rotateDeg270ButtonSym.Id; rotateDeg90ButtonSym.Id];
            }

            let newModel = (*a new function to help resolve formating for output, more readable*)
                model
                |> Optic.set SheetT.scalingBox_ (Some initScalingBox)
                |> Optic.set SheetT.symbols_ newSymbolMap
            let newCmd =
                match model.ScalingBox with
                | Some _ -> [symbolCmd (SymbolT.DeleteSymbols (model.ScalingBox.Value).ButtonList);
                             sheetCmd SheetT.UpdateBoundingBoxes]
                            |> List.append [cmd]
                            |> Elmish.Cmd.batch
                | None -> cmd

            newModel, newCmd

//--------------------------------------end of ec1221 section ----------------------------------------//