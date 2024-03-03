module SheetBeautifyHelpers

open EEExtensions
open CommonTypes
open DrawModelType.SymbolT
open DrawModelType
open Optics
open Symbol
open BusWireRoute
open Helpers
open BlockHelpers

//------------------------------------------------------------------------------------------------------------------------//
//------------------------------Helpers used in TestDrawBlockD3 and in SheetBeautifyHelpers-------------------------------//
//------------------------------------------------------------------------------------------------------------------------//
let visibleSegments (wId: ConnectionId) (model: SheetT.Model): XYPos list =

    let wire = model.Wire.Wires.[wId] // get wire from model

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
    /// Index must be in range 1..segVecs
    let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int)  =
        if segVecs.[index] =~ XYPos.zero
        then
            segVecs.[0..index-2] @
            [segVecs.[index-1] + segVecs.[index+1]] @
            segVecs.[index+2..segVecs.Length - 1]
        else
            segVecs

    wire.Segments
    |> List.mapi getSegmentVector
    |> (fun segVecs ->
            (segVecs,[1..segVecs.Length-2])
            ||> List.fold tryCoalesceAboutIndex)

let caseInvariantEqual str1 str2 =
    String.toUpper str1 = String.toUpper str2

/// Rotate the symbol given by symLabel by an amount rotate.
/// Takes in a symbol label, a rotate fixed amount, and a sheet containing the symbol.
/// Return the sheet with the rotated symbol.
let rotateSymbol (symLabel: string) (rotate: Rotation) (model: SheetT.Model) : (SheetT.Model) =

    let symbolsMap = model.Wire.Symbol.Symbols
    let getSymbol = 
        mapValues symbolsMap
        |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
        |> function | Some x -> Ok x | None -> Error "Can't find symbol with label '{symPort.Label}'"

    match getSymbol with
    | Ok symbol ->
        let rotatedSymbol = SymbolResizeHelpers.rotateSymbol rotate symbol
        let updatedSymbolsMap = Map.add symbol.Id rotatedSymbol symbolsMap
        { model with Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbolsMap } } }

    | _ -> model

/// Flip the symbol given by symLabel by an amount flip.
/// Takes in a symbol label, a flip fixed amount, and a sheet containing the symbol.
/// Return the sheet with the flipped symbol.
let flipSymbol (symLabel: string) (flip: SymbolT.FlipType) (model: SheetT.Model) : (SheetT.Model) =

    let symbolsMap = model.Wire.Symbol.Symbols
    let getSymbol =
        mapValues symbolsMap
        |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
        |> function | Some x -> Ok x | None -> Error "Can't find symbol with label '{symPort.Label}'"

    match getSymbol with
    | Ok symbol ->
        let flippedSymbol = SymbolResizeHelpers.flipSymbol flip symbol
        let updatedSymbolsMap = Map.add symbol.Id flippedSymbol symbolsMap
        { model with Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbolsMap } } }

    | _ -> model


// B1 RW
/// A lens for reading and writing the dimensions of a custom component symbol
/// Returns a lens: i) gets the symbol's dimensions as a tuple. ii) sets the symbol's dimensions by taking in updated dimensions.
let customCompDimsLens = Lens.create (fun customSym -> customSym.Component.W, customSym.Component.H) (fun (updateW, updateH) customSym -> {customSym with Component = {customSym.Component with W = updateW; H = updateH} })

// B2 W 
/// Updates the posiiton of a symbol on the sheet.
/// Takes a new X,Y position and the symbol object.
/// Returns the symbol with its position updated.
let updateSymPos (updatePos : XYPos) (sym: Symbol)  =
                      fun updatePos symbol -> {symbol with Pos = { symbol.Pos with X=updatePos.X; Y=updatePos.Y}}

//B3 R
/// Reads the order of ports on a specified side of a symbol.
/// Takes the symbol and the edge to read from.
/// Returns the order of ports as a list.
let readPortOrder symbol edge =
    let orderMap = symbol.PortMaps.Order
    orderMap.[edge]

//B3 W
/// Writes the order of ports on a specified side of a symbol.
/// Takes the symbol, the edge to rewrite the ports of and the new list of ports.
/// Returns the updated symbol with the new port order.
let updatePortOrder edge newList symbol =
    let updatedOrder = Map.add edge newList symbol.PortMaps.Order
    let updatedPortMaps = { symbol.PortMaps with Order = updatedOrder }
    { symbol with PortMaps = updatedPortMaps }

//B4 RW
/// A lens for reading and writing the reversed input ports state of a MUX2.
/// Returns a lens: i) gets the symbol's reversed input port state. ii) sets the symbol's new reversed input port state.
let revInpPortLens = Lens.create (fun symbol -> symbol.ReversedInputPorts) (fun newState symbol -> {symbol with ReversedInputPorts = newState})

//B5 R
/// Reads the position of a port on a symbol.
/// Takes the symbol, the sheet and the portID.
/// Returns the X,Y position of the port.
let getPortPos (symbol: Symbol) (model: Model) (portId: string) =
    let optionPos = Some symbol.Pos
    Symbol.getPortLocation optionPos model portId

//B6 R
/// Calculates the bounding box of a symbol outline using the symbol's dimensions and top left coord.
/// Takes the symbol object.
/// Returns a bounding box of a symbol outline.
let getSymBoundingBox (symbol : Symbol) =
                     let boundingBox = { TopLeft = symbol.Pos; W = symbol.Component.W; H = symbol.Component.H }
                     boundingBox

//B7 RW
/// A lens for reading and writing the rotation state of a symbol.
/// Returns a lens: i) gets the symbol's rotation state. ii) sets the symbol's rotation state by taking in newState.
let symRotStateLens = Lens.create (fun symbol -> symbol.STransform.Rotation) (fun newState symbol -> {symbol with STransform = {symbol.STransform with Rotation =newState}})

//B8 RW
/// A lens for reading and writing the flip state of a symbol.
/// Returns a lens: i) gets the symbol's flip state. ii) sets the symbol's flip state by taking in newState.
let symFlipStateLens = Lens.create (fun sym -> sym.STransform.Flipped) (fun newState sym -> {sym with STransform = {sym.STransform with Flipped =newState}})

//T1 R
/// Counts the number of pairs of symbols that intersect each other in the sheet.
/// Takes in a sheet.
/// Returns a count of intersecting symbol pairs. 
let numOfIntersectSymPairs (sheet: SheetT.Model) =
    let boxes =
        mapValues sheet.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> n,box)
    List.allPairs boxes boxes 
    |> List.fold (fun count ((n1,box1),(n2,box2)) -> if ((n1 <> n2) && BlockHelpers.overlap2DBox box1 box2) then
                                                         count + 1 
                                                      else
                                                         count) 0

//T2 R
/// Counts the number of distinct wire visible segments that intersect with one or more symbols in the sheet.
/// Takes in a sheet.
/// Returns a count of the distinct visible wire segments that intersect with at least one symbol.
let numOfIntersectSegSym (model: SheetT.Model) : int =
    let allWires = model.Wire.Wires
                    |> Map.toList

    let wiresStartPos = allWires
                            |> List.map (fun (_, wire) -> wire.StartPos)

    let segmentsPos =
        allWires
        |> List.map (fun (connectId, _) -> visibleSegments connectId model)
        |> List.zip wiresStartPos
        |> List.map (fun (beginPos, segXYPos) ->
                            segXYPos
                            |> List.fold (fun (acc, lstPos) oneSeg ->
                                let absPos = lstPos + oneSeg
                                (acc @ [(lstPos, absPos)], absPos)
                            ) ([], beginPos)
                            |> fst)
   
    let boundBoxes = model.BoundingBoxes
                     |> Map.toList
                     |> List.map (fun (id, box) -> box)

    let countIntersections =
        segmentsPos
        |> List.map (fun wireSegments ->
            wireSegments
            |> List.map (fun (startPos, endPos) -> 
                boundBoxes
                |> List.choose (fun box -> segmentIntersectsBoundingBox box startPos endPos))
            |> List.map List.length
            |> List.sum)
            |> List.sum

    countIntersections

// T3R R Low The number of distinct pairs of segments that cross each other at right angles. Does not include 0 length segments or segments on same net intersecting at one end, or segments on same net on top of each other. Count over whole sheet.
//T3 R
/// Count the number of distinct visible wire segments that cross each other at right angles in a sheet.
/// Takes in a sheet.
/// Returns the number right angle intersections between wire segments.
let numOfWireRightAngles (model: SheetT.Model) : int =

    let extractWireSegments = 
        model.Wire.Wires
        |> Map.toList
        |> List.collect (fun (connectId, wire) ->
            let startPos = wire.StartPos
            visibleSegments connectId model
            |> List.scan (fun (lastPos, _) seg -> (lastPos + seg, lastPos)) (startPos, startPos)
            |> List.tail
            |> List.map (fun (endPos, startPos) -> (startPos, endPos, wire.OutputPort))
        )

    let isRightAngleIntersection ((start1, end1, _): XYPos * XYPos * OutputPortId) ((start2, end2, portId2): XYPos * XYPos * OutputPortId) : bool =
        let direction1 = { X = end1.X - start1.X; Y = end1.Y - start1.Y }
        let direction2 = { X = end2.X - start2.X; Y = end2.Y - start2.Y }
        let dotProduct = direction1.X * direction2.X + direction1.Y * direction2.Y
        dotProduct = 0.0 && overlap2D (start1, end1) (start2, end2)

    let countRightAngles =
        extractWireSegments
        |> List.collect (fun segmentA -> extractWireSegments |> List.map (fun segmentB -> (segmentA, segmentB)))
        |> List.filter (fun ((_, _, portIdA), (_, _, portIdB)) -> portIdA <> portIdB)
        |> List.countBy id
        |> List.filter (fun ((segmentA, segmentB), _) -> isRightAngleIntersection segmentA segmentB)
        |> List.length

    countRightAngles / 2

//T4 R
/// Sum the wiring segments length of all wires in the sheet, only counting once when N wire segments of the same-net are overlapping.
/// Takes in a sheet.
/// Returns the total visible wiring segment length over the whole sheet.
let calcVisWireLength (model:SheetT.Model) : float=
    let netsToCheck (model:SheetT.Model) =
        partitionWiresIntoNets model.Wire
        |> List.map (fun (_, wires) -> match wires.Length with
                                             | 1 -> []
                                             | _ -> wires)

    let wiresStartPos (currNetList : (ConnectionId * BusWireT.Wire) list) = currNetList
                                                                            |> List.map (fun (_, wire) -> wire.StartPos)

    let segmentsPos (currNetList : (ConnectionId * BusWireT.Wire) list) (model : SheetT.Model) : ((XYPos * XYPos) list) list=
            currNetList //wires for in a particular net list
            |> List.map (fun (connectId, _) -> visibleSegments connectId model)
            |> List.zip (wiresStartPos currNetList) 
            |> List.map (fun (startPos, segXYPos) ->
                                segXYPos
                                |> List.fold (fun (acc, lastPos) oneSeg ->
                                    let newAbsPos = lastPos + oneSeg
                                    (acc @ [(lastPos, newAbsPos)], newAbsPos)
                                ) ([], startPos)
                                |> fst)

    let mergeSegments ((a1, a2): XYPos * XYPos) ((b1, b2): XYPos * XYPos) : (XYPos * XYPos) =
        let startX = min (min a1.X a2.X) (min b1.X b2.X)
        let startY = min (min a1.Y a2.Y) (min b1.Y b2.Y)
        let endX = max (max a1.X a2.X) (max b1.X b2.X)
        let endY = max (max a1.Y a2.Y) (max b1.Y b2.Y)
        ({X = startX; Y = startY}, {X = endX; Y = endY})

    let iterSegments (segmentsListforNet : ((XYPos * XYPos) list) list) : (XYPos * XYPos) list =
        let unionList = List.empty
        let flattenSegments = List.concat segmentsListforNet

        let rec mergeWithUnionList segment unionList =
            match unionList with
            | [] -> [segment]
            | head :: tail ->
                if overlap2D segment head then
                    let mergedSegment = mergeSegments segment head
                    mergeWithUnionList mergedSegment tail
                else
                    head :: (mergeWithUnionList segment tail)
    
        let finalUnionList = List.fold (fun acc segment -> mergeWithUnionList segment acc) [] flattenSegments

        finalUnionList

    let getTotalLengths (finalUnionList : (XYPos * XYPos) list) : float =
        finalUnionList
        |> List.map (fun (startPos, endPos) ->
            let diff = endPos - startPos
            sqrt(diff.X ** 2.0 + diff.Y ** 2.0))
        |> List.sum

    netsToCheck model 
    |> List.map (fun net ->
       segmentsPos net model
       |> iterSegments
          |> getTotalLengths)
    |> List.sum

//T5 R
/// Counts the visible wire right-angles over the entire sheet.
/// Takes in a sheet.
/// Returns the number of visible wire right-angles.
let numOfVisRightAngles (model: SheetT.Model) : int =
    let isRightAngle (v1: XYPos) (v2: XYPos) : bool =
        (v1.X * v2.X + v1.Y * v2.Y) = 0.0

    let numOfWireRightAngles (wireVectors: XYPos list) : int =
        let rec countHelper vectors count =
            match vectors with
            | v1::v2::vs when isRightAngle v1 v2 -> countHelper (v2::vs) (count + 1)
            | _::vs -> countHelper vs count
            | [] -> count
        countHelper wireVectors 0

    model.Wire.Wires
    |> Seq.map (fun kvp -> let wId, _ = kvp.Key, kvp.Value in visibleSegments wId model)
    |> Seq.map numOfWireRightAngles
    |> Seq.sum

//T6 R
/// Analyzes wire retracing cases in wire segments over the entire sheet and retracing segments that intersect with symbols for edge cases.
/// Takes in a sheet.
/// Returns a tuple of 2 lists, the left tuple containing the retracing segments and right tuple containing edge case symbol intersections.
let findRetraceCases (model : SheetT.Model) =

    let absSegments = Map.toList model.Wire.Wires
                        |> List.map (fun (_, wire ) -> wire)
                        |> List.map getAbsSegments

    let allSegments = Map.toList model.Wire.Wires
                        |> List.map (fun (_, wire ) -> wire.Segments)
                       
    let getBoundingBoxes = model.BoundingBoxes
                                  |> Map.toList
                                  |> List.map (fun (_, box) -> box)

    let checkSegIntersectSymbol (aSeg: BusWireT.ASegment) =
        getBoundingBoxes
        |> List.exists (fun currentBox ->
            match (segmentIntersectsBoundingBox currentBox aSeg.Start aSeg.End) with
            | Some _ -> true
            | None -> false) 

    let detectEdgeCaseRetracings =
        absSegments
        |> List.collect (fun segmentList ->
            let segCount = List.length segmentList
            segmentList
            |> List.indexed
            |> List.fold (fun accumulated (index, currentSeg) ->
                if segCount > 4 then
                    match index with
                    | 2 when checkSegIntersectSymbol currentSeg ->
                        let precedingSeg = segmentList.[index - 1]
                        let followingSeg = segmentList.[index + 1]
                        accumulated @ [[precedingSeg.Segment; currentSeg.Segment; followingSeg.Segment]]
                    | i when (i, segCount - 3) |> fst = i && checkSegIntersectSymbol currentSeg ->
                        let precedingSeg = segmentList.[index - 1]
                        let followingSeg = segmentList.[index + 1]
                        accumulated @ [[precedingSeg.Segment; currentSeg.Segment; followingSeg.Segment]]
                    | _ -> accumulated
                else accumulated
            ) [])

    let accumulateRetracingSegments =
        allSegments
        |> List.collect (fun segmentGroup -> // Use collect for integrated flattening
            segmentGroup
            |> List.indexed
            |> List.fold (fun accumulated (idx, currentSeg) ->
                match currentSeg.Length, idx with
                | 0.0, index when index > 0 && index < List.length segmentGroup - 1 ->
                    let previousSeg = segmentGroup.[idx - 1]
                    let followingSeg = segmentGroup.[idx + 1]
                    if (previousSeg.Length > 0.0 <> (followingSeg.Length > 0.0)) then
                        accumulated @ [[previousSeg; currentSeg; followingSeg]]
                    else accumulated
                | _ -> accumulated
            ) [])

    (accumulateRetracingSegments, detectEdgeCaseRetracings)
