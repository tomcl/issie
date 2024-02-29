module SheetBeautifyHelpers

open CommonTypes
open Elmish
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open Optics
open Optic
open Sheet
open SheetSnap
open Symbol
open Helpers
open DrawHelpers
open BusWireRoutingHelpers
open BlockHelpers
open Browser
open Optics
open Operators

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

/// Return whether a symbol is on the sheet, useful when checking for exceptions
let checkSymOnSheet (symId: ComponentId) (model: SheetT.Model) : bool =
    let symOpt = Map.tryFind symId model.Wire.Symbol.Symbols
    match symOpt with
    | Some _ -> true
    | None -> false

/// Update a symbol on the sheet, return the updated model
let updateSymOnSheet (symId: ComponentId) (newSym: Symbol) (model: SheetT.Model) : SheetT.Model =
    let newSymbolsMap = Map.add symId newSym model.Wire.Symbol.Symbols
    set symbols_ newSymbolsMap model


// B1R
/// Get the dimensions of a custom component symbol, return (Width, Height)
let getCustomSymWH (sym: Symbol) = 
    let comp = sym.Component
    let getScale = Option.defaultValue 1.0
    let xDim, yDim =  getScale sym.HScale*comp.W, getScale sym.VScale*comp.H
    match sym.STransform.Rotation with
        | Degree0 | Degree180 -> xDim, yDim
        | Degree90 | Degree270 -> yDim, xDim

// B1W
/// Set the dimensions (Width, Height) of a custom component symbol
let updateCustomSymWH ((w,h): float*float) (sym: Symbol) =
    match sym.STransform.Rotation with
    | Degree0 | Degree180 -> setCustomCompHW h w sym
    | Degree90 | Degree270 -> setCustomCompHW w h sym
    
// B1RW
/// Lens to operate on the dimensions of a custom component symbol
let customSymWH_ = Lens.create getCustomSymWH updateCustomSymWH


// B2W
/// Set the position of a symbol on the sheet
let updateSymXYPos (newPos: XYPos) (sym: Symbol)= 
    sym
    |> set posOfSym_ newPos


// B3R
/// Get the order of ports on a specified side of a symbol
let getPortOrderOnSide (side: Edge) (sym: Symbol) = 
    sym.PortMaps.Order
    |> Map.find side        // given that a PortMap.Order map should contain all sides

// B3W
/// Set the order of ports on a specified side of a symbol
let updatePortOrderOnSide (side: Edge) (newPortOrder: list<string>) (sym: Symbol) = 
    let newPortMapsOrder = 
        sym.PortMaps.Order
        |> Map.add side newPortOrder
    {sym with PortMaps = {sym.PortMaps with Order = newPortMapsOrder}}

// B3RW (not sure if this is a correct way of combining them in a Lens)
/// Lens to operate on the order of ports on a specified side of a symbol
let portOrderOnSide_ side = Lens.create (getPortOrderOnSide side) (updatePortOrderOnSide side)


// B4R
/// Get the reverses state of the inputs of a MUX2
let getReversedInputsMux2 (sym: Symbol) = 
    sym.ReversedInputPorts

// B4W
/// Set the reverses state of the inputs of a MUX2
let updateReversedInputsMux2 (reversedState: Option<bool>) (sym: Symbol) = 
    let newReversedState =
        match reversedState with
        | Some false -> Some true
        | Some true -> Some false
        | None -> Some true
    let newSymbolInfo = 
        match sym.Component.SymbolInfo with
        | Some symInfo -> Some {symInfo with ReversedInputPorts = newReversedState}
        | None -> None
    {sym with Component = {sym.Component with SymbolInfo = newSymbolInfo}; ReversedInputPorts = newReversedState} 

// B4RW
/// Lens to operate on the reverses state of the inputs of a MUX2
let reversedInputsMux2_ = Lens.create getReversedInputsMux2 updateReversedInputsMux2


// B5R
/// Get the position of a port on the sheet.
/// (Changed from returning Result into returning XYPos, for more convenient usage in SheetBeautifyB3.fs)
let getPortPosOnSheet (portId: string) (model: SheetT.Model) = 
// code adapted from Symbol.getPortLocation
    // get Port from portId
    let port = Map.find portId model.Wire.Symbol.Ports
    // get Symbol from portId
    let sym = Map.find (ComponentId port.HostId) model.Wire.Symbol.Symbols
    (getPortPos sym port) + sym.Pos

// the original version that returns a Result (considers exceptions):
let getPortPosOnSheetResult (portId: string) (model: SheetT.Model) = 
// code adapted from Symbol.getPortLocation
    // get Port from portId
    let portOpt = Map.tryFind portId model.Wire.Symbol.Ports
    // get Symbol from portId
    let symOpt = 
        portOpt
        |> Option.map (fun port ->  ComponentId port.HostId)
        |> Option.bind (fun symbolId -> Map.tryFind symbolId model.Wire.Symbol.Symbols)
    match symOpt, portOpt with
    | Some sym,  Some port -> Ok ((getPortPos sym port) + sym.Pos)
    | _, None -> Error $"Error: can't find Port='{portId}' on the sheet."
    | None, _ -> Error $"Error: can't find Symbol with Port='{portId}' on the sheet."


// B6R
/// Get the Bounding box of a symbol outline (position is contained in this)
let getSymBoundingBox (sym: Symbol) =
    getSymbolBoundingBox sym


// B7R
/// Get the rotation state of a symbol
let getSymRotationState (sym: Symbol) =
    (getSTransformWithDefault sym.Component.SymbolInfo).Rotation

// B7W
/// Set the rotation state of a symbol
let updateSymRotationState (rotationState: Rotation) (sym: Symbol) = 
    let newSymbolInfo =
        match sym.Component.SymbolInfo with
                | Some symInfo -> Some {symInfo with STransform = {symInfo.STransform with Rotation=rotationState}}
                | None -> None
    let newComp = {sym.Component with SymbolInfo = newSymbolInfo}
    let newSTransform = {sym.STransform with Rotation=rotationState}
    {sym with Component=newComp; STransform=newSTransform}

// B7RW
/// Lens to operate on the rotation state of a symbol
let symRotationState_ = Lens.create getSymRotationState updateSymRotationState


// B8R
/// Get the flip state of a symbol
let getSymFlipState (sym: Symbol) =
    (getSTransformWithDefault sym.Component.SymbolInfo).Flipped

// B8W
/// Set the flip state of a symbol
let updateSymFlipState (flipState: bool) (sym: Symbol) = 
    let newSymbolInfo =
        match sym.Component.SymbolInfo with
                | Some symInfo -> Some {symInfo with STransform = {symInfo.STransform with Flipped=flipState}}
                | None -> None
    let newComp = {sym.Component with SymbolInfo = newSymbolInfo}
    let newSTransform = {sym.STransform with Flipped=flipState}
    {sym with Component=newComp; STransform=newSTransform}

// B8RW
/// Lens to operate on the flip state of a symbol
let symFlipState_ = Lens.create getSymFlipState updateSymFlipState


// -------------------------------------------------------------------------------------------------------


// T1R
/// Count the number of pairs of symbols that intersect each other. Count over all pairs of symbols.
let countSymIntersectSym (sheet: SheetT.Model)  =
    let boxes =
        mapValues sheet.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> n,box)
    List.allPairs boxes boxes 
    |> List.filter (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
    |> List.length     


// (Copied from TestDrawBlock.HLPTick3.visibleSegments as a helper.)
/// The visible segments of a wire, as a list of vectors, from source end to target end.
/// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed 
/// which if present causes the two segments on either side of it to coalesce into a single visible segment.
/// A wire can have any number of visible segments - even 1.
let visibleSegments (wId: ConnectionId) (model: SheetT.Model): XYPos list =

    let wire = model.Wire.Wires[wId] // get wire from model

    /// helper to match even and off integers in patterns (active pattern)
    let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

    /// Convert seg into its XY Vector (from start to end of segment).
    /// index must be the index of seg in its containing wire.
    let getSegmentVector (index:int) (seg: BusWireT.Segment) =
        // The implicit horizontal or vertical direction  of a segment is determined by 
        // its index in the list of wire segments and the wire initial direction
        match index, wire.InitialOrientation with
        | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> {X=0.; Y=seg.Length}
        | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> {X=seg.Length; Y=0.}

    /// Return a list of segment vectors with 3 vectors coalesced into one visible equivalent
    /// if this is possible, otherwise return segVecs unchanged.
    /// Index must be in range >= 1
    let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int)  =
        if index < segVecs.Length - 1 && segVecs[index] =~ XYPos.zero
        then
            segVecs[0..index-2] @
            [segVecs[index-1] + segVecs[index+1]] @
            segVecs[index+2..segVecs.Length - 1]
        else
            segVecs

    wire.Segments
    |> List.mapi getSegmentVector
    |> (fun segVecs ->
            (segVecs,[1..segVecs.Length-2])
            ||> List.fold tryCoalesceAboutIndex)

// T2R
/// Count the number of distinct wire visible segments that intersect with one or more symbols. Count over all visible wire segments.
let countWireSegIntersectSym (sheet: SheetT.Model) = 
    let getSegStartEndPos (wire: Wire) (segXYPosList: list<XYPos>) = 
        // code adapted from BlockHelpers.segmentsToIssieVertices 
        let segVertices =
            (wire.StartPos, segXYPosList)
            ||> List.scan(fun currPos segXYPos -> currPos+segXYPos)
        let segStartEndPosList = 
            let lstStartPos = 
                match (List.rev segVertices) with
                | [] -> []
                | _hd::tl -> List.rev tl
            let lstEndPos = 
                match segVertices with
                | [] -> []
                | _hd::tl -> tl
            List.zip lstStartPos lstEndPos
        segStartEndPosList

    let boundingBoxs = 
        // code adapted from BusWireRoute.findWireSymbolIntersections 
        mapValues sheet.BoundingBoxes
        |> Array.map (fun boundingBox ->
            (
                {
                    W = boundingBox.W + Constants.minWireSeparation * 2.
                    H = boundingBox.H + Constants.minWireSeparation * 2.
                    TopLeft =
                    boundingBox.TopLeft
                    |> updatePos Left_ Constants.minWireSeparation
                    |> updatePos Up_ Constants.minWireSeparation
                }
            ))

    Map.toArray sheet.Wire.Wires
    |> Array.map (fun (wId,wire) -> (wire, visibleSegments wId sheet))  // all visible segments
    |> Array.collect (fun (wire,segXYPosList) ->    // get startPos and endPos of visible segments
            segXYPosList
            |> getSegStartEndPos wire
            |> List.toArray
        )
    |> Array.filter (fun (startPos, endPos) ->  // distinct segments that intersect with at least one symbol
        boundingBoxs
        |> Array.exists ( fun boundingBox ->
            match segmentIntersectsBoundingBox boundingBox startPos endPos with // do not consider the symbols that the wire is connected to
                | Some _ -> true // segment intersects bounding box
                | None -> false // no intersection
            )
        )
    |> Array.distinct  // should already be distinct but just to guarantee it
    |> Array.length


// T3R
/// Count the number of distinct pairs of segments that cross each other at right angles.
/// Does not include 0 length segments or segments on same net intersecting at one end,
/// or segments on same net on top of each other. Count over whole sheet.
let countWireSegRightAngleIntersect (sheet: SheetT.Model) =  

    /// Converts a Segment list into a list of start and end XYPos of each segment
    let getSegStartEndPosList (wire,segList) = 
        // adapted from BlockHelpers.getWireSegmentsXY
        let tupToXY (l: (float * float)) : XYPos = { X = fst l; Y = snd l }
        let segVertices = 
            segmentsToIssieVertices segList wire
            |> List.map (fun (x, y, _) -> (x, y))
            |> List.map tupToXY
        let segStartEndPosList = 
            let lstStartPos = 
                match (List.rev segVertices) with
                | [] -> []
                | _hd::tl -> List.rev tl
            let lstEndPos = 
                match segVertices with
                | [] -> []
                | _hd::tl -> tl
            List.zip lstStartPos lstEndPos
        wire, segStartEndPosList

    /// Checks whether two wires belong to different nets
    let differentNetWires wire1 wire2 =
        wire1.OutputPort <> wire2.OutputPort

    /// Filters out 0 length segments of a wire
    let filterZeroLenSeg (wire:Wire) =
        wire.Segments
        |> List.filter (fun seg -> seg.Length<>0)

    /// Checks whether two segments intersect at one end
    let segsIntersectOneEnd (segPos1:XYPos*XYPos) (segPos2:XYPos*XYPos) = // startPos or endPos are the same (4 cases)
        let XYPosToFloat (pos:XYPos) = (pos.X,pos.Y)
        let start1,end1 = segPos1
        let start2,end2 = segPos2
        List.allPairs [start1;end1] [start2;end2]
        |> List.exists (fun (pos1,pos2) -> overlap1D (XYPosToFloat pos1) (XYPosToFloat pos2))

    /// Return the orientation of a segment from the positions of its two ends
    let getOrientFromSegPos (segPos:XYPos*XYPos) =
        match segPos with
        | (startPos,endPos) when startPos.Y=endPos.Y -> Horizontal
        | _ -> Vertical                      // startPos.X=endPos.X

    /// Checks whether two segments lie partly or wholly on top of each other
    let segsOverlay (segPos1:XYPos*XYPos) (segPos2:XYPos*XYPos) = 
        let ort1 = getOrientFromSegPos segPos1
        let ort2 = getOrientFromSegPos segPos2
        match segPos1,segPos2 with
        | _ when not (overlap2D segPos1 segPos2) -> false   // don't overlap
        | _ when ort1<>ort2 -> false                        // not the same orientation
        | _ -> true                                         // overlap AND same orientation => must overlay

    /// Checks whether two segments cross each other at right angle
    let segsCrossRightAngle (segPos1:XYPos*XYPos) (segPos2:XYPos*XYPos) = 
        let ort1 = getOrientFromSegPos segPos1
        let ort2 = getOrientFromSegPos segPos2
        (overlap2D segPos1 segPos2) && (ort1<>ort2)

    let wireSegArray =
        mapValues sheet.Wire.Wires
        // filter out 0 length segments for each wire
        |> Array.map (fun wire -> wire,(filterZeroLenSeg wire))
        |> Array.map getSegStartEndPosList
        |> Array.collect (fun (wire, segStartEndPosList) -> 
            segStartEndPosList
            |> List.map (fun seg -> {|Wire=wire; Pos=seg|})
            |> List.toArray
            )  // distribute wire into the segment list

    wireSegArray
    |> Array.allPairs wireSegArray
    // filter out segments on same net on top of each other
    |> Array.filter (fun (seg1,seg2) -> differentNetWires seg1.Wire seg2.Wire && segsOverlay seg1.Pos seg2.Pos)
    // filter out segments on same net intersecting at one end
    |> Array.filter (fun (seg1,seg2) -> differentNetWires seg1.Wire seg2.Wire && segsIntersectOneEnd seg1.Pos seg2.Pos)
    // find segments that cross each other at right angles
    |> Array.filter (fun (seg1,seg2) -> segsCrossRightAngle seg1.Pos seg2.Pos)
    |> Array.distinct  // should already be distinct but just to guarantee it
    |> Array.length


// T4R
/// Sum of wiring segment length, counting only one when there are N same-net segments overlapping 
/// (the visible wire length on the sheet). Count over whole sheet.
let totalWireSegmentLength (sheet:SheetT.Model) = 
    let segXYPosToLength (segXYPos: XYPos) =
        match segXYPos with
        | {X=0.; Y=len} -> abs len
        | {X=len; Y=0.} -> abs len
        | _ -> 0.   // should never match

    mapKeys sheet.Wire.Wires
    |> Array.map (fun wId -> visibleSegments wId sheet)  // all visible segments
    |> Array.map (List.fold (fun (sum:float) (segXYPos: XYPos) -> sum + (segXYPosToLength segXYPos)) 0.)
    |> Array.fold (fun (sum:float) (len:float) -> sum+len) 0. 


// T5R
/// Count the number of visible wire right-angles. Count over whole sheet.
let countVisibleWireRightAngles (sheet:SheetT.Model) =
    let numRightAnglesPerWire wId =
        let numOfSegs =
            visibleSegments wId sheet
            |> List.length
        match numOfSegs with
        | n when n>1 -> n-1
        | _ -> 0

    (0, mapKeys sheet.Wire.Wires)
    ||> Array.fold (fun (sum: int) wId -> sum + (numRightAnglesPerWire wId)) 


// T6R
// The zero-length segments in a wire with non-zero segments on either side that have 
// lengths of opposite signs (directions) lead to a wire retracing itself. 
// Note that this can also apply at the end of a wire (where the zero-length segment is one from the end). 
// This is a wiring artifact that should never happen but errors in routing or separation can cause it.

/// Returns a list of all the segments that retrace,
/// and also a list of all the end of wire segments that retrace so far
/// that the next segment (index = 3 or Segments.Length – 4) - starts inside a symbol.
/// Count over the whole sheet.
let getRetracingSegs (sheet:SheetT.Model) = 
    let getRetracingSegsOfWire (wire:Wire) =
        let segList = wire.Segments
        let checkRetraceAtIndex (retraceList, endOfWireList) (idx:int) =
            match segList[idx].Length with
            | 0. -> (
                    let currSeg = segList[idx]
                    match idx with
                    | nextIdx when nextIdx = segList.Length-1 -> // the zero-segment is already end-of-wire
                        (currSeg::retraceList, endOfWireList)
                    | _ ->                                            // the zero-segment is not end-of-wire
                        let prevSeg = segList[idx-1]
                        let nextSeg = segList[idx+1]
                        match (sign prevSeg.Length = -(sign nextSeg.Length)) with     // check directions of neighbour segments
                        | true ->  // opposite direction: add segments into lists
                            match idx+1 with
                            | nextIdx when nextIdx = segList.Length-1 -> // the next segment is end-of-wire
                                (nextSeg::retraceList, nextSeg::endOfWireList)
                            | _ -> (nextSeg::retraceList, endOfWireList)       // the next segment is not end-of-wire
                        | false -> (retraceList, endOfWireList) // same direction: lists unchanged
                )
            | _ -> (retraceList, endOfWireList)
        (([],[]),[1..segList.Length-1])
        ||> List.fold checkRetraceAtIndex

    mapValues sheet.Wire.Wires
    |> Array.map getRetracingSegsOfWire
    |> Array.fold (fun (li1,li2) (segLi1,segLi2) -> (List.append li1 segLi1, List.append li2 segLi2)) ([],[])

