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

let setTopBottomGap (gap : float) (clkSpace : float) : float = 
    gap + clkSpace

/// TODO: this is mostly copy pasted code from Symbol.getPortPos, perhaps abstract out the existing code there to use makePortInfo.
/// Could not simply use getPortPos because more data (side, topBottomGap, etc.) is needed to caclulate the new dimensions of the resized symbol.
/// Constructs detailed information of a port's position on a symbol, considering the symbol's orientation and dimensions.
/// Given a symbol and a specific port on that symbol, it returns a record containing all the information required to calculate the position of the port on the sheet
/// when symbol is resized.
let makePortInfo (sym: Symbol) (port: Port) : PortInfo =
    let side = getSymbolPortOrientation sym port
    let ports = sym.PortMaps.Order[side] 
    let gap = getPortPosEdgeGap sym.Component.Type
    let topBottomGap = gap + 0.3 // extra space for clk symbol
    let portDimension = float ports.Length - 1.0
    let h, w = getRotatedHAndW sym

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

/// <summary>
/// Identifies and returns the ports of two symbols connected by a wire.
/// </summary>
/// <param name="model">The model containing information about wires and symbols.</param>
/// <param name="wireSyms">A record type of type WireSymbols, which includes two symbols (SymA and SymB) and the connecting wire (Wire) between them.</param>
/// <returns>A tuple containing two ports (Port * Port), where each port corresponds to one of teh symbols specified in wireSym.</returns>
let getPortAB (model : Model) (wireSyms : WireSymbols) : Port * Port =
    let ports = portsOfWires model [ wireSyms.Wire ]
    let portA = filterPortBySym ports wireSyms.SymA |> List.head
    let portB = filterPortBySym ports wireSyms.SymB |> List.head
    portA, portB 

// The proposed fucntion which gets rid of the runtime exception by using Option type to return the result.
// This causes changes in the getOppEdgePortInfo function and a new type signature need to me made, plus
// a match expression needs to be used to get the values from getPortAB. 
// Subsequent functions like alignSymbols and reSizeSymbol need to be changed to accomodate the changes in the fucntion getPortAB,
// due to type signatures.

// let getPortAB (model : Model) (wireSyms : WireSymbols) : Option<Port * Port> =
//     let ports = portsOfWires model [ wireSyms.Wire ]
//     let portA = filterPortBySym ports wireSyms.SymA |> List.tryHead
//     let portB = filterPortBySym ports wireSyms.SymB |> List.tryHead
//     match portA, portB with
//     | Some pA, Some pB -> Some (pA, pB)
//     | _ -> None

/// <summary>
/// Attempts to identify and return information for two ports located on opposite edges of two symbols connected by a wire.
/// </summary>
/// <param name="model">The model representing the current state of wires and symbols.</param>
/// <param name="sym1">The first symbol to be considered.</param>
/// <param name="sym2">The second symbol to be considered.</param>
/// <returns>
/// An option type that contains a tuple of PortInfo records for two ports IF thay are on opposite edges, otherwise None.
/// </returns>
let getOppEdgePortInfo ( model: BusWireT.Model ) ( sym1: Symbol ) ( sym2: Symbol ) : (PortInfo * PortInfo) option =
    let wires = wiresBtwnSyms model sym1 sym2

    let tryGetOppEdgePorts ( wireSyms : WireSymbols ) : Option<PortInfo * PortInfo>=
        let portA, portB = getPortAB model wireSyms
        let edgeA = getSymbolPortOrientation wireSyms.SymA portA
        let edgeB = getSymbolPortOrientation wireSyms.SymB portB

        match edgeA = edgeB.Opposite with
        | true -> Some ( makePortInfo wireSyms.SymA portA, makePortInfo wireSyms.SymB portB )
        | _ -> None

            

    wires
    |> List.tryPick (fun wire ->
        tryGetOppEdgePorts
            { SymA = sym1
              SymB = sym2
              Wire = wire })

/// <summary>
/// Calculates the offset required to align the ports of two symbols based on their current positiona and sides
/// </summary>
/// <param name="movingPortI">PortInfo for the first port which is considered the moving port in the alignment process.</param>
/// <param name="stationaryPortI">PortInfo for the second port, against which the first port is to be aligned.</param>
/// <returns>
/// An XYPos record representing the offset needed to align the moving port with the stationary port.
/// </returns>
let alignPortsOffset (movingPortI : PortInfo) (stationaryPortI : PortInfo) : XYPos =
    let getPortRealPos (pInfo : PortInfo) : XYPos =
        getPortPos pInfo.sym pInfo.port + pInfo.sym.Pos
    let movePortPos , otherPortPos = getPortRealPos movingPortI, getPortRealPos stationaryPortI 
    let posDiff = otherPortPos - movePortPos

    match movingPortI.side with
    | Top  | Bottom -> { X = posDiff.X; Y = 0.0 }
    | Left | Right -> { X = 0.0; Y = posDiff.Y }

/// <summary>
/// Aligns two symbols based on the positions of their connected ports, ensuring they are on parallel edges.
/// </summary>
/// <param name="model">The current model of the workspace, containing all symbols and wires..</param>
/// <param name="movingSymb">The symbol to be moved for alignment purposes.</param>
/// <param name="stationarySymb">The reference symbol, which movingSymb will align against.</param>
/// <returns>
/// Returns an updated model with the position of `movingSymb` potentially adjusted to achieve alignment.
/// </returns>
let alignSymbols (model: BusWireT.Model) (movingSymb: Symbol) (stationarySymb: Symbol) : BusWireT.Model =

    // Only attempt to align symbols if they are connected by ports on parallel edges.
    match getOppEdgePortInfo (model: BusWireT.Model) movingSymb stationarySymb with
    | None -> model
    | Some(movePortInfo, otherPortInfo) ->
        let offset = alignPortsOffset movePortInfo otherPortInfo
        let symbol' = moveSymbol offset movingSymb
        let model' = Optic.set (symbolOf_ movingSymb.Id) symbol' model
        BusWireSeparate.routeAndSeparateSymbolWires model' movingSymb.Id

// Helper function to calculate new dimentions 
let calculateNewDims (portInfoA : PortInfo) (portInfoB : PortInfo) : float * float = 
    match portInfoA.side with
    | Left | Right -> portInfoB.portGap * (portInfoA.portDimension + 2.0 * portInfoA.gap), portInfoA.w
    | Top  | Bottom -> portInfoA.h, portInfoB.portGap * (portInfoA.portDimension + 2.0 * portInfoA.topBottomGap)

// Refactored fucntion using helper fucntion 
let reSizeSymbol2 (model : BusWireT.Model) (movingSymb : Symbol) (stationarySymb : Symbol) : Symbol =

    let wires = wiresBtwnSyms model movingSymb stationarySymb
    let resizePortInfo, otherPortInfo =
        match getOppEdgePortInfo model movingSymb stationarySymb with
        | None ->
            let pA, pB = getPortAB model { SymA = movingSymb; SymB = stationarySymb; Wire = wires[0] }
            makePortInfo movingSymb pA, makePortInfo movingSymb pB
        | Some(pIA, pIB) -> (pIA, pIB)
    let h, w = calculateNewDims resizePortInfo otherPortInfo

    match movingSymb.Component.Type with
    | Custom _ ->
        let scaledSymbol = setCustomCompHW h w movingSymb
        let scaledInfo = makePortInfo scaledSymbol resizePortInfo.port
        let offset = alignPortsOffset scaledInfo otherPortInfo
        moveSymbol offset scaledSymbol
    | _ -> movingSymb
    

/// HLP23: To test this, it must be given two symbols interconnected by wires. It then resizes symbolToSize
/// so that the connecting wires are exactly straight
/// HLP23: It should work out the interconnecting wires (wires) from
/// the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has adjustable size).
/// <summary>
/// Resized a symbol based on the positions and sides of connected ports to ensure straight wire connection between symbols
/// </summary>
/// <param name="model">Model representing the current state of wires and symbols in sheet.</param>
/// <param name="movingSymb">The symbol to be resized/moved.</param>
/// <param name="stationarySymb">The stationary symbol connected to the symbol being resized, used to determinethe optimal size for straight connections.</param>
/// <returns>
/// the resized symbol with adjusted height (h) and weight (w) to align its ports for straight wire connections.
/// </returns>
let reSizeSymbol (model: BusWireT.Model) (movingSymb: Symbol) (stationarySymb: Symbol) : Symbol =
    let wires = wiresBtwnSyms model movingSymb stationarySymb

    // Attempts to find ports on opposite edges for optimal resizing. If unavbailable, default to any two ports.
    let resizePortInfo, otherPortInfo =
        match getOppEdgePortInfo model movingSymb stationarySymb with
        | None ->
            let pA, pB =
                getPortAB model { SymA = movingSymb; SymB = stationarySymb; Wire = wires[0] }
            makePortInfo movingSymb pA, makePortInfo movingSymb pB
        | Some(pIA, pIB) -> (pIA, pIB)

    let h, w =
        match resizePortInfo.side with
        | Left | Right ->
            otherPortInfo.portGap *
            (resizePortInfo.portDimension + 2.0 * resizePortInfo.gap),
            resizePortInfo.w
        | Top  | Bottom ->
            resizePortInfo.h,
            otherPortInfo.portGap *
            (resizePortInfo.portDimension + 2.0 * resizePortInfo.topBottomGap)

    match movingSymb.Component.Type with
    | Custom _ ->
        let scaledSymbol = setCustomCompHW h w movingSymb
        let scaledInfo = makePortInfo scaledSymbol resizePortInfo.port
        let offset = alignPortsOffset scaledInfo otherPortInfo
        moveSymbol offset scaledSymbol
    | _ ->
        movingSymb

// ae621 : Moved parameters on the same line for this function so that it is consistent with the rest of the code and added XML comments
///  <summary>
/// Resizes the specified top-level symbol within the given BusWireT.Model.
/// </summary>
/// <param name="wModel">The BusWireT.Model containing symbols and their connections.</param>
/// <param name="symbolToSize">The symbol to be resized.</param>
/// <param name="otherSymbol">The reference symbol used for resizing calculations.</param>
/// <returns>
/// A new BusWireT.Model with the resized symbol and updated connections.
/// </returns>
let reSizeSymbolTopLevel (model: BusWireT.Model) (movingSymb: Symbol) (stationarySymb: Symbol) : BusWireT.Model =
    printfn $"ReSizeSymbol: ToResize:{movingSymb.Component.Label}, Other:{stationarySymb.Component.Label}"

    let scaledSymbol = reSizeSymbol model movingSymb stationarySymb

    let model' = Optic.set (symbolOf_ movingSymb.Id) scaledSymbol model
    BusWireSeparate.routeAndSeparateSymbolWires model' movingSymb.Id

/// For each edge of the symbol, store a count of how many connections it has to other symbols.
type SymConnDataT =
    { ConnMap: Map<ComponentId * Edge, int> }

// ae621: renamed function and added XML comments
/// <summary>
/// Tries to find the edge connected to the target symbol when a wire connects opposite edges.
/// </summary>
/// <param name="wModel">The model containing the symbols and wires.</param>
/// <param name="wire">The wire between the symbols.</param>
/// <param name="targetSymbol">The target symbol.</param>
/// <param name="otherSymbol">The other symbol connected by the wire.</param>
/// <returns>
/// If the wire connects opposite edges, returns the edge connected to the target symbol;
/// otherwise, returns <c>None</c>.
///  </returns>
let tryFindEdgeForOppositeWire (wModel: Model) (wire: Wire) (sym: Symbol) (otherSym: Symbol) =
    let symEdge = wireSymEdge wModel wire sym
    let otherSymEdge = wireSymEdge wModel wire otherSym

    match symEdge = otherSymEdge.Opposite with
    | true -> Some symEdge
    | _ -> None

//added XML comments and renamed function to make it clearer removed TODO as it as already been done
/// <summary>
/// Updates or inserts a connection count for a specific edge and component ID in the symmetric connection data.
/// </summary>
/// <param name="symConnData">The symmetric connection data to update.</param>
/// <param name="edge">The edge involved in the connection.</param>
/// <param name="cid">The component ID related to the connection.</param>
/// <returns>
/// The updated symmetric connection data with the modified connection count.
/// </returns>
let updateOrInsertConnectionCount (symConnData: SymConnDataT) (edge: Edge) (cid: ComponentId) =
    let m = symConnData.ConnMap
    let count =
        Map.tryFind (cid, edge) m
        |> Option.defaultValue 0
        |> (+) 1
    { ConnMap = Map.add (cid, edge) count m }

// TODO: this is copied from Sheet.notIntersectingComponents. It requires SheetT.Model, which is not accessible from here. Maybe refactor it.
//added XML comments
/// <summary>
/// Checks if a symbol does not overlap with any other symbols based on the provided bounding box intersection function.
/// </summary>
/// <param name="boxesIntersect">The function to determine if two bounding boxes intersect.</param>
/// <param name="boundingBoxes">A map of symbol IDs to their corresponding bounding boxes.</param>
/// <param name="symbol">The symbol to check for overlap with other symbols.</param>
/// <returns>
/// Returns true if the symbol does not overlap with any other symbols; otherwise, returns false.
/// </returns>
let noSymbolOverlap (boxesIntersect: BoundingBox -> BoundingBox -> bool) boundingBoxes sym =
    let symBB = getSymbolBoundingBox sym

    boundingBoxes
    |> Map.filter (fun sId boundingBox -> boxesIntersect boundingBox symBB && sym.Id <> sId)
    |> Map.isEmpty

(*
In the refactoring process, several changes were made to enhance the modularity and correctness of the code.
The calculateSymConnData function was
introduced to encapsulate the logic for computing symbolic connection data,
 while the alignSym, tryResize, and scaleSymbolForOptim functions were created to isolate the symbol alignment and resizing operations.
 These changes not only improve code readability but also contribute to a more organized and maintainable codebase.
*)
let updateData (wModel: BusWireT.Model) (symbol: Symbol) (symConnData: SymConnDataT) _ (wire: Wire) =
    let symS, symT = getSourceSymbol wModel wire, getTargetSymbol wModel wire

    let otherSymbol =
        match symS, symT with
        | _ when symS.Id <> symbol.Id && symT.Id = symbol.Id -> Some symS
        | _ when symS = symbol && symT <> symbol -> Some symT
        | _ -> None

    match otherSymbol with
    | Some otherSym ->
        let edge = tryFindEdgeForOppositeWire wModel wire symbol otherSym

        match edge with
        | Some e -> updateOrInsertConnectionCount symConnData e otherSym.Id
        | None -> symConnData
    | None -> symConnData

let calculateSymConnData (wModel: BusWireT.Model) (symbol: Symbol) : SymConnDataT =
    let resizeWithAlign (symConnData: SymConnDataT) _ wire =
        updateData wModel symbol symConnData () wire
    Map.fold resizeWithAlign { ConnMap = Map.empty } wModel.Wires

let alignSym (wModel: BusWireT.Model) (sym: Symbol) (otherSym: Symbol) : bool * Symbol =
    let resizedSym = reSizeSymbol wModel sym otherSym
    let boundingBoxesID = wModel.Symbol.Symbols |> Map.keys |> Seq.toList
    let boundingBoxes =
        List.map (fun (bbID) -> (bbID, getBoundingBox wModel.Symbol bbID)) boundingBoxesID
        |> Map.ofList
    let noOverlap = noSymbolOverlap DrawHelpers.boxesIntersect boundingBoxes resizedSym

    match noOverlap with
    | true -> true, resizedSym
    | _ -> false, sym

let tryResize (wModel: BusWireT.Model) (symCount: ((ComponentId * Edge) * int) array) (sym: Symbol) : Symbol =
    let alignSym (hAligned, vAligned, sym) ((cid, edge), _) =
        let otherSym = Optic.get (symbolOf_ cid) wModel

        match hAligned, vAligned, edge with
        | false, _, (Top | Bottom) ->
            let hAligned', resizedSym = alignSym wModel sym otherSym
            (hAligned', vAligned, resizedSym)
        | _, false, (Left | Right) ->
            let vAligned', resizedSym = alignSym wModel sym otherSym
            (hAligned, vAligned', resizedSym)
        | _ -> (hAligned, vAligned, sym)

    let (_, _, sym') = Array.fold alignSym (false, false, sym) symCount
    sym'

let scaleSymbolForOptim (wModel: BusWireT.Model) (symConnData: SymConnDataT) (symbol: Symbol) : Symbol =
    let symCount =
        symConnData.ConnMap
        |> Map.toArray
        |> Array.filter (fun (_, count) -> count > 1)
        |> Array.sortByDescending snd

    tryResize wModel symCount symbol

let optimiseSymbol (wModel: BusWireT.Model) (symbol: Symbol) : BusWireT.Model =
    let symConnData = calculateSymConnData wModel symbol
    let scaledSymbol = scaleSymbolForOptim wModel symConnData symbol
    let model' = Optic.set (symbolOf_ symbol.Id) scaledSymbol wModel
    BusWireSeparate.routeAndSeparateSymbolWires model' symbol.Id


/// a helper module to deal, specifically with tuples of size 2
module Tuple2 = 
    /// <summary>maps a given function across a 2-tuple,
    /// where both elements are of the same type</summary>
    /// <param name="function">the function to apply across the 2-tuple</param>
    /// <param name="tuple (a, b)">the 2-tuple to be transformed</param>
    /// <returns>2-tuple of transformed values</returns>
    /// <author>HLP24: Pierce Wiegerling</author>
    let map (f: 'a -> 'b) (a: 'a, b: 'a) = (f a, f b)


/// <summary>HLP 23: AUTHOR Ismagilov - Get the bounding box of multiple selected symbols\
/// HLP 24: modified by Pierce Wiegerling</summary>
/// <param name="symbols"> Selected symbols list</param>
/// <returns>Bounding Box</returns>
let getBlock (symbols: Symbol List) : BoundingBox =    
    symbols
    |> List.map getSymbolBoundingBox
    |> List.map (fun bb -> (bb.TopLeft, bb.BottomRight()))
    |> List.map (fun (tlVal, brVal) -> 
        [tlVal.X; brVal.X], [tlVal.Y; brVal.Y]  // split into X, Y components
    )
    |> List.unzip  // separate into X and Y lists
    |> Tuple2.map (List.collect id)  // collecting all the BB sublists
    |> fun (xList, yList) -> 
        let minX = List.min xList
        let minY = List.min yList
        let maxX = List.max xList
        let maxY = List.max yList

        {TopLeft = {X = minX; Y = minY}; W = maxX - minX; H = maxY - minY}

/// <summary>HLP 23: AUTHOR Ismagilov - Takes a point Pos, a centre Pos, and a rotation type and returns the point flipped about the centre</summary>
/// <param name="point"> Original XYPos</param>
/// <param name="center"> The center XYPos that the point is rotated about</param>
/// <param name="rotation"> Clockwise or Anticlockwise </param>
/// <returns>New flipped point</returns>
let rotatePointAboutBlockCentre 
    (point: XYPos) 
    (centre: XYPos) 
    (rotation: Rotation) = 
    let relativeToCentre = (fun x -> x - centre)
    let rotateAboutCentre pointIn = 
        match rotation with 
        | Degree0 -> 
            pointIn
        | Degree90 ->
            { X = pointIn.Y ; Y = -pointIn.X }
        | Degree180 -> 
            { X = -pointIn.X ; Y = -pointIn.Y }
        | Degree270 ->
            { X = -pointIn.Y ; Y = pointIn.X }
           
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
    (point: XYPos)
    (center: XYPos)
    (flip: FlipType) = 
    match flip with
    | FlipHorizontal-> 
        { X = center.X - (point.X - center.X); Y = point.Y } 
    | FlipVertical -> 
        { X = point.X; Y = center.Y - (point.Y - center.Y) }

/// <summary>HLP 23: AUTHOR Ismagilov - Get the new top left of a symbol after it has been rotated</summary>
/// <param name="rotation"> Rotated CW or AntiCW</param>
/// <param name="h"> Original height of symbol (Before rotation)</param>
/// <param name="w"> Original width of symbol (Before rotation)</param>
/// <param name="sym"> Symbol</param>
/// <returns>New top left point of the symbol</returns>
let adjustPosForBlockRotation
        (rotation: Rotation) 
        (h: float) (w: float)
        (pos: XYPos)
         : XYPos =
    let posOffset =
        match rotation with
        | Degree0 -> { X = 0.0; Y = 0.0 }
        | Degree90 -> { X = h ; Y = 0.0 }
        | Degree180 -> { X = w; Y= -h }
        | Degree270 -> { X = 0.0 ; Y = w }
    pos - posOffset

/// <summary>HLP 23: AUTHOR Ismagilov - Get the new top left of a symbol after it has been flipped</summary>
/// <param name="flip">  Flipped horizontally or vertically</param>
/// <param name="h"> Original height of symbol (Before flip)</param>
/// <param name="w"> Original width of symbol (Before flip)</param>
/// <param name="sym"> Symbol</param>
/// <returns>New top left point of the symbol</returns>
let adjustPosForBlockFlip
        (flip: FlipType) 
        (h: float) (w: float)
        (pos: XYPos) =
    let posOffset =
        match flip with
        | FlipHorizontal -> { X = w ; Y = 0.0 }
        | FlipVertical -> { X = 0.0 ; Y = h }
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
      
    // HLP 2024 - Ahmæd reoganised code
    // took invRotation out as a new variable over calling (invertRotation rotation) 3 times
    // used if statement for newRotation and newSTransform over match with true and false for readability and consistnecy  
    let h, w = getRotatedHAndW sym
    let invRotation = invertRotation rotation // ah121
    let newTopLeft = 
        rotatePointAboutBlockCentre sym.Pos blockCentre invRotation
        |> adjustPosForBlockRotation invRotation h w
    let newComponent = { sym.Component with X = newTopLeft.X; Y = newTopLeft.Y}
    let newRotation = if sym.STransform.Flipped then invRotation else rotation //ah121
    let newSTransform = {sym.STransform with Rotation = combineRotation newRotation sym.STransform.Rotation} //ah121
    
    { sym with 
        Pos = newTopLeft;
        PortMaps = rotatePortInfo rotation sym.PortMaps
        STransform = newSTransform
        LabelHasDefaultPos = true
        Component = newComponent
    }
    |> calcLabelBoundingBox 


/// <summary>HLP 23: AUTHOR Ismagilov - Flip a symbol horizontally or vertically in its block.</summary>
/// <param name="flip">  Flip horizontally or vertically</param>
/// <param name="block"> Bounding box of selected components</param>
/// <param name="sym"> Symbol to be flipped</param>
/// <returns>New symbol after flipped about block centre.</returns>
let flipSymbolInBlock
    (flip: FlipType)
    (blockCentre: XYPos)
    (sym: Symbol) : Symbol =

    let h, w = getRotatedHAndW sym
    //Needed as new symbols and their components need their Pos updated (not done in regular flip symbol)
    let newTopLeft = 
        flipPointAboutBlockCentre sym.Pos blockCentre flip
        |> adjustPosForBlockFlip flip h w

    let portOrientation = 
        sym.PortMaps.Orientation
        |> Map.map (fun id side -> flipSideHorizontal side)

    let flipPortList currPortOrder side =
        currPortOrder
        |> Map.add (flipSideHorizontal side) sym.PortMaps.Order[side]

    let portOrder = 
        (Map.empty, [Edge.Top; Edge.Left; Edge.Bottom; Edge.Right]) 
        ||> List.fold flipPortList
        |> Map.map (fun _edge order -> List.rev order) //_edge ah121

    let newSTransform = 
        {Flipped = not sym.STransform.Flipped;
         Rotation = sym.STransform.Rotation} 

    let newcomponent = {sym.Component with X = newTopLeft.X; Y = newTopLeft.Y}

    { sym with
        Component = newcomponent
        PortMaps = {Order = portOrder; Orientation = portOrientation}
        STransform = newSTransform
        LabelHasDefaultPos = true
        Pos = newTopLeft }
    |> calcLabelBoundingBox
    |> (fun sym -> 
        // HLP 2024 - Ahmæd reoganised code
        // used rotateAntiClockByAng over rotateSymbolInBlock
        // unnecessary need to use get and use newBlockCenter 
        // unnecessary need to rotateSymbol twice by 270 so just once by 180
        // Further Improvements given more time would be to rewrite it without 
        // flips and only adjust the ports and edges
        match flip with
        | FlipHorizontal -> sym 
        | FlipVertical -> sym |> rotateAntiClockByAng Degree180) // ah121

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
    let xProp, yProp =
        (symCenter.X - block.TopLeft.X) / block.W, (symCenter.Y - block.TopLeft.Y) / block.H

    let newCenter = 
        match scaleType with
        | ScaleUp ->
            {X = (block.TopLeft.X-5.) + ((block.W+10.) * xProp);
             Y = (block.TopLeft.Y-5.) + ((block.H+10.) * yProp)}
        | ScaleDown ->
            {X = (block.TopLeft.X+5.) + ((block.W-10.) * xProp);
             Y =  (block.TopLeft.Y+5.) + ((block.H-10.) * yProp)}

    let h, w = getRotatedHAndW sym
    let newPos = {X = (newCenter.X) - w/2.; Y= (newCenter.Y) - h/2.}
    let newComponent = { sym.Component with X = newPos.X; Y = newPos.Y}

    {sym with Pos = newPos; Component = newComponent; LabelHasDefaultPos = true}

/// <summary> HLP 23: AUTHOR Klapper - Rotates a symbol based on a degree, including: ports and component parameters.
/// The rotation degree can only be a fixed multiple of 90 degrees. </summary>
/// <param name="degree"> The amount of clockwise rotation for the symbol</param>
/// <param name="sym"> The symbol that is rotated</param>
/// <returns>New rotated symbol model</returns>
let rotateSymbolByDegree (degree: Rotation) (sym:Symbol)  =
    let pos =
        {X = sym.Component.X + sym.Component.W / 2.0;
         Y = sym.Component.Y + sym.Component.H / 2.0}
    match degree with
    | Degree0 -> sym
    | _ -> rotateSymbolInBlock degree pos sym

let oneCompBoundsBothEdges (selectedSymbols: Symbol list) = 
    let maxXSymCentre = 
            selectedSymbols
            |> List.maxBy (fun (sym: Symbol) -> sym.Pos.X + snd (getRotatedHAndW sym)) 
            |> getRotatedSymbolCentre
    let minXSymCentre =
            selectedSymbols
            |> List.minBy (fun (sym: Symbol) -> sym.Pos.X)
            |> getRotatedSymbolCentre
    let maxYSymCentre = 
            selectedSymbols
            |> List.maxBy (fun (sym: Symbol) -> sym.Pos.Y+ fst (getRotatedHAndW sym))
            |> getRotatedSymbolCentre
    let minYSymCentre =
            selectedSymbols
            |> List.minBy (fun (sym: Symbol) -> sym.Pos.Y)
            |> getRotatedSymbolCentre
    (maxXSymCentre.X = minXSymCentre.X) || (maxYSymCentre.Y = minYSymCentre.Y)

/// <summary>Finds a specific list of symbols on a sheet from ComponentIds</summary>
/// <param name="compList"> The list of ComponentId's of those symbols</param>
/// <param name="model"> The current symbol model</param>
/// <returns>List of symbol types</returns>
let findSelectedSymbols (compList: ComponentId list) (model: SymbolT.Model) = 
    List.map (fun x -> model.Symbols |> Map.find x) compList

let getScalingFactorAndOffsetCentre (min:float) (matchMin:float) (max:float) (matchMax:float) = 
    let scaleFact = 
        if min = max || matchMax <= matchMin then
            1. 
        else
            (matchMin - matchMax) / (min - max)
    let offsetC = 
        if scaleFact = 1. then
            0.
        else
            (matchMin - min * scaleFact) / (1. - scaleFact)
    (scaleFact, offsetC)

// It is difficult to understand the functionality and high-level purpose of this code without comments
/// <summary>For a group of symbols to be scaled, get the scaling factor and offset from centre</summary>
/// <param name="matchBBMin"> Match minimum Bounding Box of mouse selection block</param>
/// <param name="matchBBMax"> Match maximum Bounding Box of mouse selection block</param>
/// <param name="selectedSymbols"> The group of symbols selected in the block</param>
/// <returns>Set of floats that define how a group of components is scaled</returns>
let getScalingFactorAndOffsetCentreGroup
    (matchBBMin:XYPos)
    (matchBBMax:XYPos)
    (selectedSymbols: Symbol list) 
    : ((float * float) * (float * float)) = 

    let maxXSym = 
        selectedSymbols
        |> List.maxBy (fun (sym: Symbol) -> sym.Pos.X + snd (getRotatedHAndW sym)) 
    let minXSym =
        selectedSymbols
        |> List.minBy (fun (sym: Symbol) -> sym.Pos.X)

    let maxYSym = 
        selectedSymbols
        |> List.maxBy (fun (sym: Symbol) -> sym.Pos.Y+ fst (getRotatedHAndW sym))
    let minYSym =
        selectedSymbols
        |> List.minBy (fun (sym: Symbol) -> sym.Pos.Y)

    let oldMaxX = (maxXSym |> getRotatedSymbolCentre).X
    let newMaxX = matchBBMax.X - (snd (getRotatedHAndW maxXSym))/2.
    
    let oldMinX = (minXSym |> getRotatedSymbolCentre).X
    let newMinX = matchBBMin.X + (snd (getRotatedHAndW minXSym))/2.
    
    let oldMaxY = (maxYSym |> getRotatedSymbolCentre).Y
    let newMaxY = matchBBMax.Y - (fst (getRotatedHAndW maxYSym))/2.

    let oldMinY = (minYSym |>  getRotatedSymbolCentre).Y
    let newMinY = matchBBMin.Y + (fst (getRotatedHAndW minYSym))/2.
    
    let xSC = getScalingFactorAndOffsetCentre oldMinX newMinX oldMaxX newMaxX
    let ySC = getScalingFactorAndOffsetCentre oldMinY newMinY oldMaxY newMaxY
    (xSC, ySC)

/// Alter position of one symbol as needed in a scaling operation.
/// xYSC has meaning ((XScale, XOffset), (YScale, YOffset)).
let scaleSymbol
        (xYSC: (float * float) * (float * float))
        (sym: Symbol)
        : Symbol = 

    //ra2520 (1/4/5) input wrapping + anon record + pattern matching
    let transformInfo =
        match xYSC with
        | ((xScale, xOffset), (yScale, yOffset)) -> {|XScale = xScale; XOffset = xOffset;
                                                      YScale = yScale; YOffset = yOffset|}

    //ra2520 (1) structural abstraction
    let translate1D scale offset coord = (coord - offset) * scale + offset
    let translate2D (pos: XYPos) : XYPos =
        {X = translate1D transformInfo.XScale transformInfo.XOffset pos.X;
         Y = translate1D transformInfo.YScale transformInfo.YOffset pos.Y}
    
    let halfDims =
        getRotatedHAndW sym
        |> fun (h, w) -> {X = w/2.; Y = h/2.}

    //ra2520 (3) pipelining
    let newTopLeftPos =
        getRotatedSymbolCentre sym
        |> translate2D
        |> fun pos -> pos - halfDims

    let newComponent = {sym.Component with X = newTopLeftPos.X; Y = newTopLeftPos.Y}

    {sym with Pos = newTopLeftPos; Component = newComponent; LabelHasDefaultPos = true}

/// Apply a function to the selected symbols, and return the new model.
/// compList and selectedSymbols refer to pairs of matching component ids and symbols.
/// modifySymbolFunc is applied to each selected symbol to get a new symbol.
//ra2520 (1/2/3) helper function + currying + pipelines used throughout
let transformSelectedSymbols
        (compList: ComponentId list) 
        (selectedSymbols: Symbol list)
        (modifySymbolFunc: Symbol -> Symbol)
        (model: SymbolT.Model) 
        : SymbolT.Model =

    // Zip together the existing component id list and new symbol list.
    let zipSymbols (newSymbols: Symbol list) : Map<ComponentId, Symbol> =
        List.map2 (fun x y -> (x, y)) compList newSymbols
        |> Map.ofList

    let newSelectedSymMap =
        selectedSymbols
        |> List.map modifySymbolFunc
        |> zipSymbols

    let unselectedSymMap =
        model.Symbols
        |> Map.filter (fun x _ -> not <| List.contains x compList)

    let addMaps map1 map2 =
        Map.fold (fun acc k v -> Map.add k v acc) map1 map2

    {model with Symbols = addMaps unselectedSymMap newSelectedSymMap}

/// An alias for transformSelectedSymbols, since this function is used by other modules.
/// Apply a function to the selected symbols, and return the new model.
/// compList and selectedSymbols refer to pairs of matching component ids and symbols.
/// modifySymbolFunc is applied to each selected symbol to get a new symbol.
//ra2520 (1) function wrapping (just changing order of parameters)
let groupNewSelectedSymsModel
        (compList: ComponentId list) 
        (model: SymbolT.Model) 
        (selectedSymbols: Symbol list)
        (modifySymbolFunc)
        : SymbolT.Model =
    transformSelectedSymbols compList selectedSymbols modifySymbolFunc model

/// A wrapper for transformSelectedSymbols, specifically for block functions.
/// modifySymbolFunc takes a block and a symbol as input, and returns the transformed symbol.
//ra2520 (1/2) helper function + currying (order of modifySymbolFunc parameters)
let transformBlock
        (compList: ComponentId list)
        (modifySymbolFunc: BoundingBox -> Symbol -> Symbol)
        (model: SymbolT.Model)
        : SymbolT.Model =
    let selectedSymbols = findSelectedSymbols compList model
    let block = getBlock selectedSymbols
    transformSelectedSymbols compList selectedSymbols (modifySymbolFunc block) model

/// Rotates a block of symbols, returning the new symbol model.
/// The selected block is given by compList.
let rotateBlock
        (compList:ComponentId list)
        (model:SymbolT.Model)
        (rotation:Rotation)
        : SymbolT.Model =
    printfn "running rotateBlock"
    let rotateFunc (block: BoundingBox) (symbol: Symbol) : Symbol =
        rotateSymbolInBlock (invertRotation rotation) (block.Centre()) symbol
    transformBlock compList rotateFunc model

/// Flips a block of symbols, returning the new symbol model.
/// The selected block is given by compList.
let flipBlock
        (compList:ComponentId list)
        (model:SymbolT.Model)
        (flip:FlipType)
        : SymbolT.Model =
    let flipFunc (block: BoundingBox) (symbol: Symbol) : Symbol =
        flipSymbolInBlock flip (block.Centre()) symbol
    transformBlock compList flipFunc model

/// After every model update this updates the "scaling box" part of the model to be correctly
/// displayed based on whether multiple components are selected, and if so, what is their "box".
/// In addition to changing the model directly, cmd may contain messages that make further changes.
//ra2520 lots of renaming/reordering things for clarity
let postUpdateScalingBox (model:SheetT.Model, cmd) = 
    
    let symbolCmd (msg: SymbolT.Msg) =
        Elmish.Cmd.ofMsg (ModelType.Msg.Sheet (SheetT.Wire (BusWireT.Symbol msg)))
    let sheetCmd (msg: SheetT.Msg) =
        Elmish.Cmd.ofMsg (ModelType.Msg.Sheet msg)
    //ra2520 (1) helper function
    let removeOldScalingBoxCmd (model: SheetT.Model) =
        [model.ScalingBox.Value.ButtonList |> SymbolT.DeleteSymbols |> symbolCmd;
         sheetCmd SheetT.UpdateBoundingBoxes]
        |> List.append [cmd]
        |> Elmish.Cmd.batch

    if model.SelectedComponents.Length <= 1 then 
        match model.ScalingBox with 
        | None -> model, cmd
        | _ -> {model with ScalingBox = None}, removeOldScalingBoxCmd model
    else 
        let newBoxBound = 
            model.SelectedComponents
            |> List.map (fun compId -> Map.find compId model.Wire.Symbol.Symbols)
            |> getBlock
        match model.ScalingBox with 
        | Some value when value.ScalingBoxBound = newBoxBound -> model, cmd
        | _ ->
            //ra2520 renamed/reordered things for clarity
            let topLeft = newBoxBound.TopLeft
            let scaleOffset: XYPos = {X = newBoxBound.W + 47.5; Y = -47.5}
            let rotateDeg90Offset: XYPos = {X = newBoxBound.W+57.; Y = (newBoxBound.H/2.)-12.5}
            let rotateDeg270Offset: XYPos = {X = -69.5; Y = (newBoxBound.H/2.)-12.5}

            let makeButton = SymbolUpdate.createAnnotation ThemeType.Colourful
            let makeRotateSymbol sym = {sym with Component = {sym.Component with H = 25.; W=25.}}

            let scaleButton = {makeButton ScaleButton (topLeft + scaleOffset) with Pos = (topLeft + scaleOffset)}
            let rotateDeg90Button = 
                makeButton (RotateButton Degree90) (topLeft + rotateDeg90Offset)
                |> makeRotateSymbol
            let rotateDeg270Button = 
                {makeButton (RotateButton Degree270) (topLeft + rotateDeg270Offset) 
                    with SymbolT.STransform = {Rotation=Degree90 ; Flipped=false}}
                |> makeRotateSymbol

            let newSymbolMap =
                model.Wire.Symbol.Symbols 
                |> Map.add scaleButton.Id scaleButton 
                |> Map.add rotateDeg270Button.Id rotateDeg270Button 
                |> Map.add rotateDeg90Button.Id rotateDeg90Button
            let initScalingBox: SheetT.ScalingBox = {
                ScalingBoxBound = newBoxBound;
                ScaleButton = scaleButton;
                RotateDeg90Button = rotateDeg90Button;
                RotateDeg270Button = rotateDeg270Button;
                ButtonList = [scaleButton.Id; rotateDeg270Button.Id; rotateDeg90Button.Id];
            }
            let newCmd =
                match model.ScalingBox with
                | Some _ -> removeOldScalingBoxCmd model
                | None -> cmd
            model
            |> Optic.set SheetT.scalingBox_ (Some initScalingBox)
            |> Optic.set SheetT.symbols_ newSymbolMap, 
            newCmd

