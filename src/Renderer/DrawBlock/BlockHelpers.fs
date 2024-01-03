module BlockHelpers
open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Optics
open Optics.Operators


//-----------------------------------------------------------------------------------------------//
//---------------------------HELPERS FOR SMART DRAW BLOCK ADDITIONS------------------------------//
//-----------------------------------------------------------------------------------------------//
module Constants =
    let intervalTolerance = 0.0001

open Constants
/// Update BusWire model with given symbols. Can also be used to add new symbols.
/// This uses a fold on the Map to add symbols which makes it fast in the case that the number
/// of symbols added is very small.
let updateModelSymbols (model: BusWireT.Model) (symbols: Symbol list) : BusWireT.Model =
    // HLP23: note on fold implementation. symMap is both argument and result of the
    // fold function => sequential set of updates. In thsi case much more efficient than Map.map
    // over all symbols.
    // HLP23 - see also similar updateModelWires
    let symbols' =
        (model.Symbol.Symbols, symbols)
        ||> List.fold (fun symMap symToAdd -> Map.add symToAdd.Id symToAdd symMap)

    Optic.set (symbol_ >-> symbols_) symbols' model

/// Update BusWire model with given wires. Can also be used to add new wires.
/// This uses a fold on the Map to add wires which makes it fast in the case that the number
/// of wires added is small.
let updateModelWires (model: BusWireT.Model) (wiresToAdd: Wire list) : BusWireT.Model =
    model
    |> Optic.map wires_ (fun wireMap ->
        (wireMap, wiresToAdd)
        ||> List.fold (fun wireMap wireToAdd -> Map.add wireToAdd.WId wireToAdd wireMap))


//------------------------------------------------------------------//
//---------------- Symbol Helper functions -------------------------//
//------------------------------------------------------------------//

let moveSymbol (offset:XYPos) (sym:Symbol) :Symbol =
    let newPos = sym.Pos + offset
    let comp' = {sym.Component with X = newPos.X; Y = newPos.Y}
    {sym with 
        Component = comp'
        Pos = newPos
        LabelBoundingBox = {sym.LabelBoundingBox with TopLeft = sym.LabelBoundingBox.TopLeft + offset}
    }

let moveSymbols  (offset: XYPos) (model:SymbolT.Model) =
    {model with
        Symbols = 
            model.Symbols
            |> Map.map (fun _ symbol -> moveSymbol offset symbol)
    }

let inline inputPortStr (InputPortId s) = s
let inline outputPortStr (OutputPortId s) = s

/// Returns true if two 1D line segments intersect
/// HLP23: Derek Lai (ddl20)
let overlap1D ((a1, a2): float * float) ((b1, b2): float * float) : bool =
    let a_min, a_max = min a1 a2, max a1 a2
    let b_min, b_max = min b1 b2, max b1 b2
    a_max >= b_min && b_max >= a_min


/// Converts a segment list into a list of vertices to store inside Connection
let segmentsToIssieVertices (segList:Segment list) (wire:Wire) = 
    ((wire.StartPos, wire.InitialOrientation, false),segList)
    ||> List.scan(fun (currPos, currOrientation, _) seg ->
        let (nextPos, nextOrientation) =
            match currOrientation with
            | Horizontal -> { currPos with X = currPos.X + seg.Length}, Vertical
            | Vertical -> { currPos with Y = currPos.Y + seg.Length}, Horizontal
        let manual = (seg.Mode = Manual)
        (nextPos,nextOrientation,manual))
    |> List.map ( fun (pos,_,manual) -> pos.X,pos.Y,manual)

/// Returns true if two Boxes intersect, where each box is passed in as top right and bottom left XYPos tuples
/// HLP23: Derek Lai (ddl20)
let overlap2D ((a1, a2): XYPos * XYPos) ((b1, b2): XYPos * XYPos) : bool =
    (overlap1D (a1.X, a2.X) (b1.X, b2.X)) && (overlap1D (a1.Y, a2.Y) (b1.Y, b2.Y))

/// Returns true if two Boxes intersect, where each box is passed in as a BoundingBox
/// HLP23: Derek Lai (ddl20)
let overlap2DBox (bb1: BoundingBox) (bb2: BoundingBox) : bool =
    let bb1Coords =
        { X = bb1.TopLeft.X; Y = bb1.TopLeft.Y },
        { X = bb1.TopLeft.X + bb1.W
          Y = bb1.TopLeft.Y + bb1.H }

    let bb2Coords =
        { X = bb2.TopLeft.X; Y = bb2.TopLeft.Y },
        { X = bb2.TopLeft.X + bb2.W
          Y = bb2.TopLeft.Y + bb2.H }

    overlap2D bb1Coords bb2Coords


/// Returns an XYPos shifted by length in an X or Y direction defined by orientation.
let inline addLengthToPos (position: XYPos) orientation length =
    match orientation with
    | Horizontal -> { position with X = position.X + length }
    | Vertical -> { position with Y = position.Y + length }

/// Returns the opposite orientation of the input orientation. (i.e. Horizontal becomes Vertical and vice-versa)
let inline switchOrientation orientation =
    match orientation with
    | Horizontal -> Vertical
    | Vertical -> Horizontal

/// <summary> Applies a function which requires the segment start and end positions to the segments in a wire, 
/// threading an accumulator argument through the computation. Essentially a List.fold applied to the list of segments of a wire, but with access to each segment's absolute positions. </summary>
/// <remarks> This is used in cases where absolute segment positions are required. 
/// These positions are computed on the fly and passed to the folder function. </remarks>
/// <param name="folder"> The function to update the state given the segment start and end positions, current state and segment itself.</param>
/// <param name="state"> The initial state.</param>
/// <param name="wire"> The wire containing the segment list we are folding over.</param>
/// <returns> The final state value </returns>
let inline foldOverSegs folder state wire =
    let initPos = wire.StartPos
    let initOrientation = wire.InitialOrientation
    ((state, initPos, initOrientation), wire.Segments)
    ||> List.fold (fun (currState, currPos, currOrientation) seg -> 
        let nextPos = addLengthToPos currPos currOrientation seg.Length
        let nextOrientation = switchOrientation currOrientation
        let nextState = folder currPos nextPos currState seg
        (nextState, nextPos, nextOrientation))
    |> (fun (state, _, _) -> state)

/// <summary> Applies a function which requires the segment start and end positions to the non-zero-length segments in a wire, 
/// threading an accumulator argument through the computation. Essentially a List.fold applied to the list of segments of a wire, but with access to each segment's absolute positions. </summary>
/// <remarks> This is used in cases where absolute segment positions are required. 
/// These positions are computed on the fly and passed to the folder function. </remarks>
/// <param name="folder"> The function to update the state given the segment start and end positions, current state and segment itself.</param>
/// <param name="state"> The initial state.</param>
/// <param name="wire"> The wire containing the segment list we are folding over.</param>
/// <returns> The final state value </returns>
let inline foldOverNonZeroSegs folder state wire =
    let initPos = wire.StartPos
    let initOrientation = wire.InitialOrientation
    ((state, initPos, initOrientation), wire.Segments)
    ||> List.fold (fun (currState, currPos, currOrientation) seg -> 
        let nextOrientation = switchOrientation currOrientation
        if seg.IsZero then 
            (currState, currPos, nextOrientation)
        else
            let nextPos = addLengthToPos currPos currOrientation seg.Length
            let nextState = folder currPos nextPos currState seg
            (nextState, nextPos, nextOrientation))
    |> (fun (state, _, _) -> state)

/// Return absolute segment list from a wire.
/// NB - it is often more efficient to use various fold functions (foldOverSegs etc)
let getAbsSegments (wire: Wire) : ASegment list =
    let convertToAbs ((start,dir): XYPos*Orientation) (seg: Segment) =
        {Start=start; End = addLengthToPos start dir seg.Length; Segment = seg}
    (((wire.StartPos,wire.InitialOrientation),[]), wire.Segments)
    ||> List.fold (fun (posDir, aSegL) seg -> 
            let nextASeg = convertToAbs posDir seg
            let posDir' = nextASeg.End, switchOrientation (snd posDir)
            posDir', (nextASeg :: aSegL))
    |> snd
    |> List.rev


/// Return absolute segment list from a wire.
/// NB - it is often more efficient to use various fold functions (foldOverSegs etc)
let getNonZeroAbsSegments (wire: Wire) : ASegment list =
    let convertToAbs ((start,dir): XYPos*Orientation) (seg: Segment) =
        {Start=start; End = addLengthToPos start dir seg.Length; Segment = seg}
    (((wire.StartPos,wire.InitialOrientation),[]), wire.Segments)
    ||> List.fold (fun (posDir, aSegL) seg -> 
            let nextASeg = convertToAbs posDir seg
            let posDir' = nextASeg.End, switchOrientation (snd posDir)
            if not <| seg.IsZero then
                posDir', (nextASeg :: aSegL)
            else
                posDir', aSegL)                
    |> snd
    |> List.rev

/// Return filtered absolute segment list from a wire.
/// includeSegment determines whether a given segment is included in the output list.
/// NB this is more efficient than generating the whole lits and then filtering.
let getFilteredAbsSegments includeSegment (wire: Wire) : ASegment list =
    let convertToAbs ((start,dir): XYPos*Orientation) (seg: Segment) =
        {Start=start; End = addLengthToPos start dir seg.Length; Segment = seg}
    (((wire.StartPos,wire.InitialOrientation),[]), wire.Segments)
    ||> List.fold (fun ((pos,ori), aSegL) seg -> 
            let nextASeg = convertToAbs (pos,ori) seg
            let posDir' = nextASeg.End, switchOrientation ori
            match includeSegment ori seg with 
            | true -> posDir', (nextASeg :: aSegL)
            | false -> posDir', aSegL)                
    |> snd
    |> List.rev

type Wire with 
        member inline this.EndOrientation =
            match this.Segments.Length % 2, this.InitialOrientation with 
            | 1, _ -> this.InitialOrientation
            | _, Vertical -> Horizontal
            | _, Horizontal -> Vertical

        member inline this.EndPos =
            (this.StartPos, this)
            ||> foldOverSegs (fun startP endP _ _ -> endP)


/// Retrieves XYPos of every vertex in a wire
/// HLP23: Derek Lai (ddl20)
let getWireSegmentsXY (wire: Wire) =
    let tupToXY (l: (float * float)) : XYPos = { X = fst l; Y = snd l }

    segmentsToIssieVertices wire.Segments wire
    |> List.map (fun (x, y, _) -> (x, y))
    |> List.map tupToXY

/// Retrieves all wires which intersect an arbitrary bounding box & the index
/// of the segment which intersects the box
/// HLP23: Derek Lai (ddl20)
let getWiresInBox (box: BoundingBox) (model: Model) : (Wire * int) list =
    let wires = (List.ofSeq (Seq.cast model.Wires.Values))

    let bottomRight =
        {   X = box.TopLeft.X + box.W
            Y = box.TopLeft.Y + box.H
        }

    // State Tuple - (overlapping: bool, overlapping_wire_index: int)
    let checkOverlapFolder (startPos: XYPos) (endPos: XYPos) (state: bool * int) (segment: Segment) : bool * int =
        let overlap = overlap2D (startPos, endPos) (box.TopLeft, bottomRight)
        (fst state || overlap), if overlap then segment.Index else snd state

    List.map (fun w -> foldOverNonZeroSegs checkOverlapFolder (false, -1) w, w) wires
    |> List.filter (fun l -> fst (fst l))
    |> List.map (fun ((_, index), w) -> w, index)

/// Used to fix bounding box with negative width and heights
/// HLP23: Derek Lai (ddl20)
let fixBoundingBox (box: BoundingBox): BoundingBox =
    let x = min (box.TopLeft.X + box.W) box.TopLeft.X
    let y = min (box.TopLeft.Y + box.H) box.TopLeft.Y
    {TopLeft = {X = x; Y = y}; W = abs box.W; H = abs box.H}


let partitionWiresIntoNets (model:Model) =
    model.Wires
    |> Map.toList
    |> List.groupBy (fun (_,wire) -> wire.OutputPort)

//------------------------------------------------------------------------------//
//----------------------------------Helper functions----------------------------//
//------------------------------------------------------------------------------//

/// Returns true if x lies in the open interval (a,b). Endpoints are avoided by a tolerance parameter
let inline inMiddleOf a x b = 
    let e = intervalTolerance
    a + e < x && x < b - e

/// Returns true if a lies in the closed interval (a,b). Endpoints are included by a tolerance parameter
let inline inMiddleOrEndOf a x b = 
    let e = intervalTolerance
    a - e < x && x < b + e
   
let inline getSourcePort (model:Model) (wire:Wire) =
    let portId = outputPortStr wire.OutputPort
    let port = model.Symbol.Ports[portId]
    port

let inline getTargetPort (model:Model) (wire:Wire) =
    let portId = inputPortStr wire.InputPort
    let port = model.Symbol.Ports[portId]
    port

let inline getSourceSymbol (model:Model) (wire:Wire) =
    let portId = outputPortStr wire.OutputPort
    let port = model.Symbol.Ports[portId]
    let symbol = model.Symbol.Symbols[ComponentId port.HostId]
    symbol

let inline getTargetSymbol (model:Model) (wire:Wire) =
    let portId = inputPortStr wire.InputPort
    let port = model.Symbol.Ports[portId]
    let symbol = model.Symbol.Symbols[ComponentId port.HostId]
    symbol

/// Moves a wire by the XY amounts specified by displacement
let moveWire (wire: Wire) (displacement: XYPos) =
    { wire with
          StartPos = wire.StartPos + displacement
    }


let moveWires (offset: XYPos)  (model: Model)  =
    let wires' =
        model.Wires
        |> Map.map (fun _ wire -> moveWire wire offset)
    {model with Wires = wires'}


//--------------------- GETTING PORTS AND THEIR LOCATIONS INTERFACE FUNCTIONS-------------------------------
// Helpers
/// Returns the center coordinates of a Symbol
let getSymbolPos (symbolModel: SymbolT.Model) compId = //makes sense or should we have getSymbol?
    let symbol = Map.find compId symbolModel.Symbols
    symbol.Pos

/// Interface function to get componentIds of the copied symbols
let getCopiedSymbols (symModel: SymbolT.Model) : (ComponentId list) =
    symModel.CopiedSymbols
    |> Map.toList
    |> List.map fst


/// Returns the port object associated with a given portId
let inline getPort (symModel: SymbolT.Model) (portId: string) =
    symModel.Ports.[portId]

let inline getSymbol (model: SymbolT.Model) (portId: string) =
    let port = getPort model portId
    model.Symbols.[ComponentId port.HostId]

let inline getCompId (model: SymbolT.Model) (portId: string) =
    let symbol = getSymbol model portId
    symbol.Id

/// Returns the string of a PortId
let inline getPortIdStr (portId: PortId) = 
    match portId with
    | InputId (InputPortId id) -> id
    | OutputId (OutputPortId id) -> id

let inline getInputPortIdStr (portId: InputPortId) = 
    match portId with
    | InputPortId s -> s

let inline getOutputPortIdStr (portId: OutputPortId) = 
    match portId with
    | OutputPortId s -> s

/// HLP23: AUTHOR dgs119
let inline getPortOrientationFrmPortIdStr (model: SymbolT.Model) (portIdStr: string) : Edge = 
    let port = model.Ports[portIdStr]
    let sId = ComponentId port.HostId
    model.Symbols[sId].PortMaps.Orientation[portIdStr]

/// returns what side of the symbol the port is on
let inline getPortOrientation (model: SymbolT.Model)  (portId: PortId) : Edge =
    let portIdStr = getPortIdStr portId
    getPortOrientationFrmPortIdStr model portIdStr

let inline getInputPortOrientation (model: SymbolT.Model) (portId: InputPortId): Edge =
    getPortOrientation model (InputId portId)

let inline getOutputPortOrientation (model: SymbolT.Model) (portId: OutputPortId): Edge =
    getPortOrientation model (OutputId portId)





//-------------------------------------------------------------------------------------------------------------------------//
//-----------------------------Miscellaneous one-off helpers - should be put into use modules------------------------------//
//-------------------------------------------------------------------------------------------------------------------------//

/// Get the start and end positions of a wire.
/// HLP23: AUTHOR Jian Fu Eng (jfe20)
let getStartAndEndWirePos (wire: Wire) : XYPos * XYPos =
    let wireVertices =
        segmentsToIssieVertices wire.Segments wire
        |> List.map (fun (x, y, _) -> { X = x; Y = y })

    let currentStartPos = wireVertices.Head
    let currentEndPos = wireVertices[wireVertices.Length - 2]

    currentStartPos, currentEndPos

/// Returns length of wire
/// HLP23: AUTHOR Jian Fu Eng (jfe20)
let getWireLength (wire: Wire) : float =
    (0., wire.Segments) ||> List.fold (fun acc seg -> acc + (abs seg.Length))

/// Gets total length of a set of wires.
/// HLP23: AUTHOR dgs119
let totalLengthOfWires (conns: Map<ConnectionId, Wire>) = 
    conns
    |> Map.map(fun _ wire -> getWireLength wire)
    |> Map.toList
    |> List.map snd
    |> List.sum

/// Checks if a wire is part of a net.
/// If yes, return the netlist. Otherwise, return None
/// HLP23: AUTHOR Jian Fu Eng (jfe20)
let isWireInNet (model: Model) (wire: Wire) : (OutputPortId * (ConnectionId * Wire) list) option =
    let nets = partitionWiresIntoNets model

    nets
    |> List.tryFind (fun (outputPortID, netlist) -> wire.OutputPort = outputPortID && netlist |> List.exists (fun (connID, w) -> connID <> wire.WId))

/// Checks if a port is part of a Symbol.
/// HLP23: AUTHOR dgs119
let isPortInSymbol (portId: string) (symbol: Symbol) : bool =
    symbol.PortMaps.Orientation |> Map.containsKey portId

/// Get pairs of unique symbols that are connected to each other.
/// HLP23: AUTHOR dgs119
let getConnSyms (wModel: BusWireT.Model) =
    wModel.Wires
    |> Map.values
    |> Seq.toList
    |> List.map (fun wire -> (getSourceSymbol wModel wire, getTargetSymbol wModel wire))
    |> List.filter (fun (symA, symB) -> symA.Id <> symB.Id)
    |> List.distinctBy (fun (symA, symB) -> Set.ofList [ symA; symB ])

/// Checks if wire is connected to two given symbols.
/// Returns false if two Symbols are the same.
/// HLP23: AUTHOR dgs119
let isConnBtwnSyms (wire: Wire) (symA: Symbol) (symB: Symbol) : bool =
    let inId, outId =
        getInputPortIdStr wire.InputPort, getOutputPortIdStr wire.OutputPort

    match inId, outId with
    | _ when (isPortInSymbol inId symA) && (isPortInSymbol outId symB) -> true
    | _ when (isPortInSymbol inId symB) && (isPortInSymbol outId symA) -> true
    | _ -> false

/// Gets connections between symbols.
/// HLP23: AUTHOR dgs119
let connsBtwnSyms (wModel: BusWireT.Model) (symA: Symbol) (symB: Symbol) : Map<ConnectionId, Wire> =
    wModel.Wires |> Map.filter (fun _ wire -> isConnBtwnSyms wire symA symB)

/// Gets Wires between symbols.
/// HLP23: AUTHOR dgs119
let wiresBtwnSyms (wModel: BusWireT.Model) (symA: Symbol) (symB: Symbol) : Wire list =
    connsBtwnSyms wModel symA symB |> Map.toList |> List.map snd

/// Filters Ports by Symbol.
/// HLP23: AUTHOR dgs119
let filterPortBySym (ports: Port list) (sym: Symbol) =
    ports |> List.filter (fun port -> ComponentId port.HostId = sym.Id)

/// Gets Ports From a List of Wires.
/// HLP23: AUTHOR dgs119
let portsOfWires (model: BusWireT.Model) (wires: Wire list) =
    wires
    |> List.map (fun wire ->
        [ getPort model.Symbol (getInputPortIdStr wire.InputPort)
          getPort model.Symbol (getOutputPortIdStr wire.OutputPort) ])
    |> List.concat
    |> List.distinct

/// Groups Wires by the net they belong to.
/// HLP23: AUTHOR dgs119
let groupWiresByNet (conns: Map<ConnectionId, Wire>) =
    conns
    |> Map.toList
    |> List.groupBy (fun (_, wire) -> wire.OutputPort)
    |> List.map (snd >> List.map snd)

/// Scales a symbol so it has the provided height and width.
/// HLP23: AUTHOR BRYAN TAN
let setCustomCompHW (h: float) (w: float) (sym: Symbol) =
    let hScale = w / sym.Component.W
    let vScale = h / sym.Component.H

    { sym with
        HScale = Some hScale
        VScale = Some vScale }

/// For a wire and a symbol, return the edge of the symbol that the wire is connected to.
/// /// HLP23: AUTHOR BRYAN TAN
let wireSymEdge wModel wire sym =
    let sPort, tPort = getSourcePort wModel wire, getTargetPort wModel wire
    let sEdge = Map.tryFind sPort.Id sym.PortMaps.Orientation
    let tEdge = Map.tryFind tPort.Id sym.PortMaps.Orientation

    match sEdge, tEdge with
    | Some e, None -> e
    | None, Some e -> e
    | _ -> Top // Shouldn't happen.


//-------------------------- types and functiond related to BusWireRouting -------------//
//-------------------------segmentIntersectsBoundingBox---------------------------------//

/// Type used to simplify BoundingBox intersection calculations
type Rectangle = {
    TopLeft: XYPos
    BottomRight: XYPos
}

/// Returns the X-value of an XYPos
let inline toX (pos: XYPos) = pos.X

/// Returns the Y-value of an XYPos
let inline toY (pos: XYPos) = pos.Y

/// Returns the X and Y fields of an XYPos as a pair of floats
let inline getXY (pos: XYPos) = pos.X, pos.Y

/// Returns pos with the X and Y fields scaled by factor (I didn't like the order of parameters for the * operator in XYPos)
let inline scalePos (factor: float) (pos: XYPos) : XYPos =
    { X = factor * pos.X; Y = factor * pos.Y}

/// Returns true if p1 is less than or equal to p2 (has both smaller X and Y values
let inline lThanEqualPos (p1: XYPos) (p2: XYPos) : bool =
    p1.X <= p2.X && p1.Y <= p2.Y

/// Returns the dot product of 2 XYPos
let inline dotProduct (p1: XYPos) (p2: XYPos) : float = 
    p1.X * p2.X + p1.Y * p2.Y

/// Returns the squared distance between 2 points using Pythagoras
let inline squaredDistance (p1: XYPos) (p2: XYPos) = 
    let diff = p1 - p2
    dotProduct diff diff

/// Checks if 2 rectangles intersect
let rectanglesIntersect (rect1: Rectangle) (rect2: Rectangle) =
    /// Checks if there is an intersection in the X or Y dimension
    let intersect1D (xOrY: XYPos -> float): bool =
        let qHi = min (xOrY rect1.BottomRight) (xOrY rect2.BottomRight)
        let qLo = max (xOrY rect1.TopLeft) (xOrY rect2.TopLeft)
        qLo <= qHi

    (intersect1D toX) && (intersect1D toY)

let findPerpendicularDistance (segStart:XYPos) (segEnd:XYPos) (point:XYPos) =
    match abs (segStart.X - segEnd.X) > abs (segStart.Y - segEnd.Y) with
    | true -> abs (segStart.Y - point.Y)
    | false -> abs (segStart.X - point.X)

/// Checks if a segment intersects a bounding box using the segment's start and end XYPos
/// return how close teh segment runs to the box centre, if it intersects
let segmentIntersectsBoundingBox (box: BoundingBox) segStart segEnd =
    let toRect p1 p2 =
        let topLeft, bottomRight =
            if lThanEqualPos p1 p2 then
                p1, p2
            else
                p2, p1

        { TopLeft = topLeft
          BottomRight = bottomRight }

    let bbBottomRight =
        { X = box.TopLeft.X + box.W
          Y = box.TopLeft.Y + box.H }

    let bbRect = toRect box.TopLeft bbBottomRight
    let segRect = toRect segStart segEnd

    if rectanglesIntersect bbRect segRect then
        Some <| findPerpendicularDistance segStart segEnd ((box.TopLeft + bbBottomRight) * 0.5)
    else
        None
