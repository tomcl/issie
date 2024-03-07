(*
  - Changed getConnectionPorts. Used List.head, which is not recommended.
  The implmentation relied upon narrowing the list of ports down, first by wire,
  then by Symbol. This would be expected to result in a single element list with the required
  port as the only element. List.head would select it.
  Now, uses TryFind, and if None is found, then failswithf (should not occur).

  - Noticed that a symbol and its associated port were often passed together, modified
  makePortInfo and getConnectionPorts to take a tuple of Symbol and Port. Considered making a record,
  but it being tuple allows me to use <|| (scandalous!) to pass the pairs of parameters to functions
  defined in the Symbol file without modifying them.

  - Renamed WireSymbols to WireConnection to better highlight the role of the record in characterising connections.

  - Adjusted match cases to align as per rest of file and other slight improvements to aid legibility
  by, for example, removing intermediate variables which did not contribute much to clarity.

  - Added and adjusted XML comments

  Added comments to explain specific implementation.
  Would have like to remove the big block in MakePortInfo which just copies info, but without being able
  to change getPortPos
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

(*
    This module contains the code that rotates and scales blocks of components.
    It was collected from HLP work in 2023 and has some technical debt and also unused functions.
    It requires better documentation of the parts now used.
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

// renamed type from WireSymbols to WireConnection to better highlight the purpose of the record
// as representing the connection from symbol A to symbol B.
// I noticed that the symbol is often passed with the port (getSymbolPortOrientation, makePortInfo)
// I would have liked to also include the ports in such a way so that it would be easier to pass these parameters
// However, given I cannot change getSymbolPortOrientation, this change would add more noise than clarity.
/// Type used to record a wire between two symbols.
type WireConnection =
    { SymA: Symbol
      SymB: Symbol
      Wire: Wire }

/// <summary>Get and package all the required info to determine the position of a port on the sheet</summary>
/// <remarks>TODO: this is mostly copy pasted code from Symbol.getPortPos, perhaps abstract out the existing code there to use makePortInfo.
/// Could not simply use getPortPos because more data (side, topBottomGap, etc.) is needed to caclulate the new dimensions of the resized symbol.</remarks>
let makePortInfo (symbAndPort: Symbol * Port) =
    let side = getSymbolPortOrientation <|| symbAndPort
    let ports = (fst symbAndPort).PortMaps.Order[side] //list of ports on the same side as port
    let gap = getPortPosEdgeGap (fst symbAndPort).Component.Type
    let topBottomGap = gap + 0.3 // extra space for clk symbol
    let portDimension = float ports.Length - 1.0
    let h, w = getRotatedHAndW (fst symbAndPort)

    let portGap =
        match side with
        | Left | Right -> float h / (portDimension + 2.0 * gap)
        | Bottom | Top -> float w / (portDimension + 2.0 * topBottomGap)

    // really hate this here, but given I cannot change symbol to refactor the getPortPos function,
    // it is not possible to get the required info without duplicating the function
    { port = snd symbAndPort
      sym = fst symbAndPort
      side = side
      ports = ports
      gap = gap
      topBottomGap = topBottomGap
      portDimension = portDimension
      h = h
      w = w
      portGap = portGap }

// Modifications: Previous implementation used functions which resulted in a
// list of a single port corresponding to the desired one, which was subsequently
// selected with List.head
// Using List.head is not recommended, so this implmentation uses tryFind.
/// <summary>Find the ports at the extremities of a wire</summary>
let getConnectionPorts wModel connection =
  let ports = portsOfWires wModel [ connection.Wire ]

  let portA = List.tryFind (fun port -> ComponentId port.HostId = connection.SymA.Id) ports
  let portB = List.tryFind (fun port -> ComponentId port.HostId = connection.SymB.Id) ports

  match portA, portB with
  | Some pA, Some pB -> (connection.SymA, pA), (connection.SymB, pB)
  // should not be reachable as wire always connects two ports together
  | _ -> failwithf "could not find wire connection ports!"

/// <summary>Try to get portInfo for two ports that are on opposite edges of two symbols</summary>
let getOppEdgePortInfo
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    : (PortInfo * PortInfo) option =
    let wires = wiresBtwnSyms wModel symbolToSize otherSymbol

    let tryGetOppEdgePorts connection =
        let symbAndPortA, symbAndPortB = getConnectionPorts wModel connection
        let edgeA = getSymbolPortOrientation <|| symbAndPortA
        let edgeB = getSymbolPortOrientation <|| symbAndPortB

        match edgeA = edgeB.Opposite with
        | true -> Some (makePortInfo symbAndPortA, makePortInfo symbAndPortB)
        | _ -> None

    wires
    |> List.tryPick (fun w ->
        tryGetOppEdgePorts
            { SymA = symbolToSize
              SymB = otherSymbol
              Wire = w })

// reduce intermediate variables, and align matches as per rest of file.
/// <summary>Compute the offset between the two wires, if offset is zero then
/// a straight wire can potientially connect both ports</summary>
let alignPortsOffset (movePInfo: PortInfo) (otherPInfo: PortInfo) =
    let getPortRealPos pInfo =
        getPortPos pInfo.sym pInfo.port + pInfo.sym.Pos

    let portPosDiff = (getPortRealPos otherPInfo) - (getPortRealPos movePInfo)

    match movePInfo.side with
    | Top | Bottom -> { X = portPosDiff.X; Y = 0.0 }
    | Left | Right -> { X = 0.0; Y = portPosDiff.Y }

/// <summary>Move symbol to align interconnecting wires</summary>
let alignSymbols
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    : BusWireT.Model =

    // Only attempt to align symbols if they are connected by ports on parallel edges.
    match getOppEdgePortInfo (wModel:BusWireT.Model) symbolToSize otherSymbol with
    | None -> wModel
    | Some (movePortInfo, otherPortInfo) ->
        let offset = alignPortsOffset movePortInfo otherPortInfo
        let symbol' = moveSymbol offset symbolToSize
        let model' = Optic.set (symbolOf_ symbolToSize.Id) symbol' wModel
        BusWireSeparate.routeAndSeparateSymbolWires model' symbolToSize.Id

/// <summary>Change custom symbol size to try and make interconnecting wires straight</summary>
/// <remarks>HLP23: To test this, it must be given two symbols interconnected by wires. It then resizes symbolToSize
/// so that the connecting wires are exactly straight
/// HLP23: It should work out the interconnecting wires (wires) from
/// the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has adjustable size).</remarks>
let reSizeSymbol (wModel: BusWireT.Model) (symbolToSize: Symbol) (otherSymbol: Symbol) : (Symbol) =
    let wires = wiresBtwnSyms wModel symbolToSize otherSymbol

    // Try to get two ports that are on opposite edges, if none found just use any two ports.
    let resizePortInfo, otherPortInfo =
        match getOppEdgePortInfo wModel symbolToSize otherSymbol with
        | None ->
            let symPortA, symPortB = getConnectionPorts wModel { SymA = symbolToSize; SymB = otherSymbol; Wire = wires[0] }
            makePortInfo symPortA, makePortInfo symPortB
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
        let scaledInfo = makePortInfo (scaledSymbol, resizePortInfo.port)
        let offset = alignPortsOffset scaledInfo otherPortInfo
        moveSymbol offset scaledSymbol
    | _ ->
        symbolToSize

// End of Deigo's changes


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

// CHANGE AND EXPLANATION
// Update the SynConn Type from a record to a simple map, the record contained a single map field and nothing else, defeating the point of it being a record
// The advantage in readability provided by a record can just as easily be obatained by simple assigning a type name to the partiuclar map type.

/// For each edge of the symbol, store a count of how many connections it has to other symbols.
type SymConnMapT = Map<ComponentId * Edge, int>

/// If a wire between a target symbol and another symbol connects opposite edges, return the edge that the wire is connected to on the target symbol
let tryWireSymOppEdge (wModel: Model) (wire: Wire) (sym: Symbol) (otherSym: Symbol) =
    let symEdge = wireSymEdge wModel wire sym
    let otherSymEdge = wireSymEdge wModel wire otherSym

    match symEdge = otherSymEdge.Opposite with
    | true -> Some symEdge
    | _ -> None

// CHANGE AND EXPLANATION
// updated the function to use the new SynnConnMapT type instead of the record
// add a short XML comment to briefly describe the functionality

/// Return an updated map with the count for an edge component tuple incremented
let updateOrInsert (symConnData: SymConnMapT) (edge: Edge) (cid: ComponentId) =
    let count = Map.tryFind (cid, edge) symConnData |> Option.defaultValue 0 |> (+) 1
    Map.add (cid, edge) count symConnData

// CHANGE AND EXPLANATION
// refactor most of the code to remove unneccessary temporary variables and streamline everything into a pipeline
//      this is achieved by rewriting some of the interal helper functions to accept the variables in the pipeline in the correct order
//      the final section where lenses are used to update the model state use anonymous functions to maintaing the pipeline

/// Finds the optimal size and position for the selected symbol w.r.t. to its surrounding symbols.
let optimiseSymbol
    (wModel: BusWireT.Model)
    (symbol: Symbol)
    (boundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>)
    : BusWireT.Model =

    // CHANGE AND EXPLANATION (noSymbolOverlap)
    // remove a boxes intersect parameter which was being set to DrawHelpers.boxesIntersect
    // replaced that parameter with overlap2DBox which is a function that checks if two bounding boxes overlap, and seems to be a more recent and more widely used
    // library function for this particular purpose
    // function moved inside of the optimiseSymbol as it is the only place it is called in
    //      by moving it inside of the function, the parameter Bounding boxes can be removed since the value being passed in was not changing

    /// Check if the bounding box of sym overlaps with any of bounding boxes in boundingBoxes
    let noSymbolOverlap sym =
        let symBB = getSymbolBoundingBox sym

        boundingBoxes
        |> Map.filter (fun sId boundingBox -> sym.Id <> sId && overlap2DBox boundingBox symBB )
        |> Map.isEmpty

    // If a wire connects the target symbol to another symbol, note which edge it is connected to
    let updateData (symConnData: SymConnMapT) _ (wire: Wire) =
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

    let tryResize sym (symCount: ((ComponentId * Edge) * int) array)=
        let alignSym (sym: Symbol) (otherSym: Symbol) =
            let resizedSym = reSizeSymbol wModel sym otherSym

            if noSymbolOverlap resizedSym
            then true, resizedSym
            else false, sym

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

    (Map.empty, wModel.Wires)
    ||> Map.fold updateData
    |> Map.toArray
    |> Array.filter (fun (_, count) -> count > 1)
    |> Array.sortByDescending snd
    |> tryResize symbol
    |> (fun scaledSymbol ->
        Optic.set (symbolOf_ symbol.Id) scaledSymbol wModel)
    |> (fun model' ->
        BusWireSeparate.routeAndSeparateSymbolWires model' symbol.Id)

// End of Luigi's changes


// ############################################################################################################
//                                      ----- Ezra Reich -- er121 -----
// ############################################################################################################
// broken, needs changing

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


// ############################################################################################################
//                                      ----- Ezra Reich -- er121 -----
// ############################################################################################################

(*
    - symHeight, symWidth - changed names to more accurately represent what they are

    - Input wrapping - Created clockwiseRotation to make it more clearly identify what value is
                        and to prevent (invertRotation rotation) from being called multiple times in function

    - XML
        - Updated summary to be more accurate
        - Change rotation description to represent what it really is - degrees anticlockwise
        - Corrected block to blockCentre and updated description
        - Added remark to describe usage
        - Fixed return grammar error

    - Fixed indentation at end

    - Made spacing consistent in function parameters
*)
/// <summary>HLP 23: AUTHOR Ismagilov - Rotate a symbol about the centre of a bounding box</summary>
/// <param name="rotation">  The rotation in degrees anticlockwise</param>
/// <param name="blockCentre"> The centre of bounding box which symbol is rotated about</param>
/// <param name="sym"> The symbol to be rotated</param>
/// <returns> New symbol after rotation about block centre</returns>
/// <remarks> Used when rotating a block of symbols together</remarks>
let rotateSymbolInBlock
    (rotation: Rotation)
    (blockCentre: XYPos)
    (sym: Symbol) : Symbol =

    let symHeight,symWidth = getRotatedHAndW sym
    let clockwiseRotation = invertRotation rotation

    let newTopLeft =
        rotatePointAboutBlockCentre sym.Pos blockCentre clockwiseRotation
        |> adjustPosForBlockRotation clockwiseRotation symHeight symWidth

    let newComponent = {sym.Component with X = newTopLeft.X; Y = newTopLeft.Y}

    let newSTransform =
        match sym.STransform.Flipped with
        | true ->
            {sym.STransform with Rotation = combineRotation clockwiseRotation sym.STransform.Rotation}
        | _->
            {sym.STransform with Rotation = combineRotation rotation sym.STransform.Rotation}

    { sym with
        Pos = newTopLeft
        PortMaps = rotatePortInfo rotation sym.PortMaps
        STransform = newSTransform
        LabelHasDefaultPos = true
        Component = newComponent }
    |> calcLabelBoundingBox


(*
    - symHeight, symWidth - changed names to more accurately represent what they are

    - XML
        - Updated summary to be more accurate
        - Corrected block to blockCentre and updated description
        - Added remark to describe usage
        - Changed return wording
        - Improved wording of flip parameter

    - Moved flipPortList inside of newPortOrder since it is only called in there

    - Added 'new' to some of the names of calculated values to clearly represent what they are

    - Removed redundant Edge qualifier in newPortOrder function

    - Made variable names consistent, e.g camelCase for newComponent

    - Changed some pipelines to be vertical so more readable

    - Changed some spacing throughout

    - Created rotateFlipped180 function to clean up final pipeline and to clearly dictate what is happening

    - Also changed sym to horizontalFlippedSym in pipeline to show what it is
*)
/// <summary>HLP 23: AUTHOR Ismagilov - Flip a symbol horizontally or vertically about the centre of a bounding box</summary>
/// <param name="flip">  Specifies whether to flip horizontally or vertically.</param>
/// <param name="blockCentre"> The centre of bounding box which symbol is flipped about</param>
/// <param name="sym"> The symbol to be flipped</param>
/// <returns> New symbol after being flipped about block centre</returns>
/// <remarks> Used when flipping a block of symbols together</remarks>
let flipSymbolInBlock
    (flip: FlipType)
    (blockCentre: XYPos)
    (sym: Symbol) : Symbol =

    let symHeight,symWidth = getRotatedHAndW sym

    // Required as new symbols and their components need their Pos updated (not done in regular symbol flip)
    let newTopLeft =
        flipPointAboutBlockCentre sym.Pos blockCentre flip
        |> adjustPosForBlockFlip flip symHeight symWidth

    let newPortOrientation =
        sym.PortMaps.Orientation
        |> Map.map (fun id side -> flipSideHorizontal side)

    let newPortOrder =
        let flipPortList currPortOrder side =
            currPortOrder
            |> Map.add (flipSideHorizontal side) sym.PortMaps.Order[side]

        (Map.empty, [Top; Left; Bottom; Right])
        ||> List.fold flipPortList
        |> Map.map (fun edge order -> List.rev order)

    let newSTransform =
        { Flipped = not sym.STransform.Flipped;
          Rotation = sym.STransform.Rotation }

    let newComponent = {sym.Component with X = newTopLeft.X; Y = newTopLeft.Y}

    let rotateFlipped180 (symbol: Symbol) =
        let newblock = getBlock [symbol]
        let newblockCenter = newblock.Centre()
        symbol
        |> rotateSymbolInBlock Degree270 newblockCenter
        |> rotateSymbolInBlock Degree270 newblockCenter

    { sym with
        Pos = newTopLeft
        PortMaps = {Order=newPortOrder; Orientation=newPortOrientation}
        STransform = newSTransform
        LabelHasDefaultPos = true
        Component = newComponent }
    |> calcLabelBoundingBox
    |> (fun horizontalFlippedSym ->
            match flip with
            | FlipHorizontal -> horizontalFlippedSym
            | FlipVertical -> rotateFlipped180 horizontalFlippedSym)


(*
    - Function not used in issie, not sure it works correctly.
      Has been replaced by newer code to calculate symbol postion upon block scaling.
      Should probably be removed but I have made improvements and left in for now.

    - Removed unnecessary comment of parameter in first line

    - Moved xProp/yProp onto separate lines and improved comment

    - Changed name 'newCenter' to 'newSymCentre' to be more representative
    - Created scale function to avoid repetition, and to simplify newSymCentre function

    - Cleaned up syntax of symbol return at end

    - symHeight, symWidth - changed names to more accurately represent what they are

    - Change name newPos to newTopLeft for clarity, also fixed error calculating y of top left

    - Spaced code to be more readable, e.g newTopLeft now on 2 lines

    - XML
        - Updated summary to be more accurate
        - Updated description of all three parameters to better describe what they are
        - Updated description of return
*)
/// <summary>HLP 23: AUTHOR Ismagilov - Adjusts position of a symbol when block containing it is scaled</summary>
/// Scales selected symbol up or down centre of a bounding box
/// <param name="scaleType"> Specifies whether block is scaled up or down. Scaling distance is constant</param>
/// <param name="block"> Bounding box of block containing symbol</param>
/// <param name="sym"> Symbol to have position adjusted</param>
/// <returns> New symbol with position adjusted for block scaling </returns>
let scaleSymbolInBlock
    (scaleType: ScaleType)
    (block: BoundingBox)
    (sym: Symbol) : Symbol =

    let symCentre = getRotatedSymbolCentre sym

    // Get x and y proportion of symbol centre position relative to block size
    let xProp = (symCentre.X - block.TopLeft.X) / block.W
    let yProp = (symCentre.Y - block.TopLeft.Y) / block.H

    let scale (topLeftShift: float) (blockDimensionDelta: float) =
        {X = (block.TopLeft.X + topLeftShift) + ((block.W + blockDimensionDelta) * xProp)
         Y = (block.TopLeft.Y + topLeftShift) + ((block.H + blockDimensionDelta) * yProp)}

    let newSymCentre =
        match scaleType with
            | ScaleUp -> scale -5. 10.
            | ScaleDown -> scale 5. -10.

    let symHeight,symWidth = getRotatedHAndW sym
    let newTopLeft = { X = (newSymCentre.X) - symWidth/2.
                       Y = (newSymCentre.Y) + symHeight/2. }

    let newComponent = {sym.Component with X = newTopLeft.X; Y = newTopLeft.Y}

    { sym with
        Pos = newTopLeft
        Component = newComponent
        LabelHasDefaultPos = true }


//------------------------------------------------------------------------------------------------//
//----------------------------------------End of Changes------------------------------------------//
//------------------------------------------------------------------------------------------------//


// Yazan Section Broken

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


// End of Yazans section

// ---------------------------------------------------------------------- //
// -------------------    Omar Alkhatib -- oa321    --------------------- //
// ---------------------------------------------------------------------- //
(*
ChangesMade:
    1)  Cleaned up scaleSymbol to reuse functions rather than repeat code

    2)  Added a combineMaps function to combine two maps with the same Key-Value types
        This makes the code more readable and easier to understand, since it allows it to be
        used in a pipeline. Previously this was done using a fold, which was less readable.

    3)  groupNewSelectedSymsModel, and flipBlock previously seperated the symbols into selected, and unselected symbols
        and then combined them at the end. This was changed to combine the modified map the the main map (model.Symbols) directly.
        This can be done since Map.add will add the element if it doesnt exist (which will not happen since selected symbols
        will exist in the map), and if it does exist it will update the value.

    4) PostUpdateScalingBox was originally one single massive If-Else statement, which was hard to read and understand.
        This was changed to use pattern matching, and used the Issie transforms to move repeated code outside the match statement.
        The If-Else statement is replaced with a single match statement at the bottom which applies the same logic and returns
        the updated model and commands.
*)




/// <summary> Scales and offsets a symbol along the X and Y axis, returns the updated symbol</summary>
/// <param name="xYSC"> (X-scalar, X-Offset), (Y-Scalar, Y-Offset) </param>
/// <param name="sym"> Current symbol </param>
/// <returns>Scaled symbol</returns>
let scaleSymbol
        (xYSC: (float * float) * (float * float))
        (sym: Symbol)
        : Symbol =

    let translateFunc scaleFact offsetC coordinate =
            (coordinate - offsetC) * scaleFact + offsetC

    let symCentreOffsetFromTopLeft =
        sym
        |> getRotatedHAndW
        |> (fun (x, y) ->
                {X = x; Y = y} * (1./2.)
            )

    let transformCoordinate (pos: XYPos) : XYPos =
        let  ({X=x; Y=y}: XYPos) = pos
        let xScalar, xOffset = fst xYSC
        let yScalar, yOffset = snd xYSC

        {
            X = translateFunc xScalar xOffset x
            Y = translateFunc yScalar yOffset y
        }

    let newCoordinates =
        sym
        |> getRotatedSymbolCentre
        |> transformCoordinate
        |> (fun coords -> coords - symCentreOffsetFromTopLeft)

    sym
    |> Optic.map component_ (fun comp ->
        {comp with X = newCoordinates.X; Y = newCoordinates.Y})
    |> (fun sym ->
        {sym with Pos = newCoordinates; LabelHasDefaultPos = true})

/// <summary>Combine Two maps with the same Key-Value types</summary>
/// <returns>The combined map</returns>
let combineMaps mapA mapB =
    (mapA, mapB)
    ||> Map.fold (fun acc k v -> Map.add k v acc)

/// <summary> Applies modifier onto input symbols list, and update them in the model </summary>
/// <param name="compList"> ComponentId's of the symbols being modified </param>
/// <param name="model"> Current Symbol Model </param>
/// <param name="symbols"> Symbols to Modify </param>
/// <param name="modifySymbolFunc"> Symbol Modifier Function </param>
/// <returns> Updated Model with Modified Selected Symbols </returns>
// TODO: This function is only used once in sheetUpdateHelpers and its one pipeline, maybe consider removing ?
// TODO: Or rename to modifySymbolsInModel, I have not changed the name since its called in another file
let groupNewSelectedSymsModel
    (compList:ComponentId list)
    (model:SymbolT.Model)
    (symbols: Symbol list)
    modifySymbolFunc =

    symbols
    |> List.map modifySymbolFunc
    |> (List.zip compList >> Map.ofList)
    |> combineMaps model.Symbols
    |> (fun newMap -> Optic.set symbols_ newMap model)

/// <summary>Flips a block of symbols, returning the new symbol model</summary>
/// <param name="compList"> List of ComponentId's of selected components</param>
/// <param name="model"> Current symbol model</param>
/// <param name="flip"> Type of flip to do</param>
/// <returns>New flipped symbol model</returns>
let flipBlock (compList:ComponentId list) (model:SymbolT.Model) (flip:FlipType) =
    model
    |> Optic.map symbols_ (fun map ->
        let selectedSymbols =
            map
            |> Map.filter (fun cId _ -> List.contains cId compList)
            |> (fun map -> Seq.toList map.Values)

        let blockCentre =
            selectedSymbols
            |> getBlock
            |> (fun block -> block.Centre())

        selectedSymbols
        |> List.map (flipSymbolInBlock flip blockCentre)
        |> (List.zip compList >> Map.ofList)
        |> combineMaps  map
    )


/// After every model update this updates the "scaling box" part of the model to be correctly
/// displayed based on whether multiple components are selected and if so what is their "box"
/// In addition to changing the model directly, cmd may contain messages that make further changes.

/// <summary>Updates the Scaling box of the Sheet Model</summary>
/// <param name="model"> Current symbol model</param>
/// <param name="cmd"> Current commands </param>
/// <returns>Updated Symbol Model, New commands</returns>

let postUpdateScalingBox (model:SheetT.Model, cmd) =

    let symbolCmd (msg: SymbolT.Msg) = Elmish.Cmd.ofMsg (ModelType.Msg.Sheet (SheetT.Wire (Symbol msg)))
    let sheetCmd (msg: SheetT.Msg) = Elmish.Cmd.ofMsg (ModelType.Msg.Sheet msg)

    let selectedComponents = model.SelectedComponents
    let scalingBox = model.ScalingBox

    let makeButton = createAnnotation ThemeType.Colourful
    let makeRotateSym sym =
        sym
        |> Optic.map component_ (fun comp ->
            {comp with H = 25.; W=25.}
        )

    let makeRotation (rotation: Rotation) (offset: XYPos) (topLeft: XYPos) =
        makeButton (RotateButton rotation) (topLeft + offset)
        |>  (fun sym ->
            match rotation with
            | Degree270 -> {sym with SymbolT.STransform = {Rotation=Degree90 ; Flipped=false}}
            | _ -> sym
            )
        |> makeRotateSym

    let updateSymbolMap (symList: Symbol list) =
        (Optic.get SheetT.symbols_ model, symList)
        ||> List.fold (fun map sym -> Map.add sym.Id sym map)

    let makeNewScalingBox (newBoxBound: BoundingBox) : SheetT.ScalingBox =
        let topLeft = newBoxBound.TopLeft
        let dummyPos = (topLeft + {X = newBoxBound.W + 47.5; Y = -47.5})
        let buttonSym = {makeButton ScaleButton dummyPos with Pos = dummyPos}
        let rotateDeg90Sym = makeRotation Degree90 topLeft {X = newBoxBound.W+57.; Y = (newBoxBound.H/2.)-12.5}
        let rotateDeg270Sym = makeRotation Degree270 topLeft {X = -69.5; Y = (newBoxBound.H/2.)-12.5}

        {
            ScalingBoxBound = newBoxBound;
            ScaleButton = buttonSym;
            RotateDeg90Button = rotateDeg90Sym;
            RotateDeg270Button = rotateDeg270Sym;
            ButtonList = [buttonSym.Id; rotateDeg270Sym.Id; rotateDeg90Sym.Id];
        }

    let newBoxBound : BoundingBox option=
        model
        |> Optic.get SheetT.symbols_
        |> Map.filter (fun cId _ -> List.contains cId selectedComponents)
        |> (fun map -> Seq.toList map.Values)
        |> (fun symbols ->
            match symbols with
            | [] | [_]  -> None
            | _ ->
                Some (getBlock symbols)
        )
    let newCommands =
        match model.ScalingBox with
        | Some _ -> [symbolCmd (SymbolT.DeleteSymbols (model.ScalingBox.Value).ButtonList);
                     sheetCmd SheetT.UpdateBoundingBoxes]
                    |> List.append [cmd]
                    |> Elmish.Cmd.batch
        | None -> cmd


    match newBoxBound, scalingBox with
    | None, None -> model, cmd
    | Some newBB, Some scaleBox when scaleBox.ScalingBoxBound = newBB -> model, cmd
    | None, Some _ ->  {model with ScalingBox = None}, newCommands
    | Some newBB, _ ->

       let initScalingBox = makeNewScalingBox newBB
       let newCmd = if scalingBox = None then cmd else newCommands
       let newSymbolMap = updateSymbolMap [initScalingBox.ScaleButton;initScalingBox.RotateDeg270Button;initScalingBox.RotateDeg90Button]

       model
       |> Optic.set SheetT.scalingBox_ (Some initScalingBox)
       |> Optic.set SheetT.symbols_ newSymbolMap, newCmd
