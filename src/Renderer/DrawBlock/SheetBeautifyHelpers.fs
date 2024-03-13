﻿
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

open Optics.Operators // for >-> operator

//----------------------------------------------------------------------------------------------------//
//----------------------------------------RotateScale-------------------------------------------------//
//----------------------------------------------------------------------------------------------------//


let getBlockAbstracted (symbols: Symbol list) : BoundingBox =
    /// Sample implementation to show how abstraction can capture XY regularity
    /// It is debatable whetehr this on its own is worthwhile because of the
    /// cognitive burden understanding what it is doing. But with more consistent use
    /// of XYPos (see TODO) and a few standard helper functions it looks better.
    /// This implementation captures more readably the "real content" of the function.

    /// Return height and width of symbol in XYPos form
    /// TODO - refactor rotatScaleHW to do this!
    let getWH sym =
        getRotatedHAndW sym
        |> (fun (h, w) -> {X=w;Y=h})

    /// Convenience function to turn its two curries arguments into an XYPos
    /// TODO: put this in DrawHelpers with other low-level stuff.
    let toXY x y : XYPos = {X=x;Y=y}

    /// apply f to a list to genrate a list of XYPos. Extract lists of X and y coordinates.
    /// Apply total to make a float from each coordinate-list, return X,Y results as an XYPos.
    let listMapXY total (f: 'a -> XYPos) (xyL: 'a list) =
            toXY (total (List.map (fun xy -> (f xy).X) xyL))
                 (total (List.map (fun xy -> (f xy).Y) xyL))
    
    // Calculate the maximum X & Y position of a symbol edge in the bounding box plus its rotated width
    let maxXY = symbols |> listMapXY List.max (fun sym -> sym.Pos + getWH sym)

    // Calculate the minimum X & Y  position of a symbol edge in the bounding box
    let minXY = symbols |> listMapXY List.min (fun sym -> sym.Pos)

    // used to generate result
    let sizeXY = maxXY - minXY

    // TODO: make W,H into an XYPos chnaging types. In addition,
    // move W,H (now called Size) from Component into Symbol so that
    // all the non-electrical component stuff is part of Symbol not Component.
    // Do a similar transform of HScale,VScale to Scale : XYPos.
    { TopLeft = minXY; W = sizeXY.X; H = sizeXY.Y }

//----------------------------------------------------------------------------------------------------//
//----------------------------------------General Helpers---------------------------------------------//
//----------------------------------------------------------------------------------------------------//

//----------------------------------------------------------------------------------------------------//
//----------------------------------------General Helpers---------------------------------------------//
//----------------------------------------------------------------------------------------------------//

module Constants =
    /// determines how close segment starting positions must be for them to be in the same bucket
    /// segment overlaps are determined by checking are segment starts in the same bucket
    /// this is faster than clustering based in euclidean distance
    /// Two very close segments will sometimes map to different buckets if on a bucket boundary
    /// for the use here this potential error is likley very unusual and deemed OK
    let bucketSpacing = 0.1


/// Compare two strings ignoring case
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

//----------------------------------------------------------------------------------------------------//
//------------------------------Helpers functions assesed in Individual Phase-------------------------//
//----------------------------------------------------------------------------------------------------//



// B1 R
/// get the outline dimensions of a custom component
let getCustomCompDims (sym: Symbol) =
    (SymbolHelpers.getCustomSymCorners sym)[2] // compensates for rotation & scaling

// B1 W
/// Write the outline dimensions of a custom component
let putCustomCompDims  (newDims: XYPos) (sym: Symbol) =
    // changing H & W does not work well because these are recalculated by autoScaleHandW
    // symbol is resized by changing HScale, VScale
    let {X=w; Y=h} = getCustomCompDims sym // old dimensions
    let ws, hs = Option.defaultValue 1. sym.HScale, Option.defaultValue 1. sym.VScale
    let ws', hs' = // HScale, VScale affect h,w before rotation, so we need to compensate for that
        match sym.STransform.Rotation with
        | Degree0 | Degree180 -> ws * newDims.X / w, hs * newDims.Y / h // non-rotated case
        | Degree90 | Degree270 -> hs * newDims.Y / w, ws * newDims.X / h // rotated case: swap w,h
    {sym with HScale = Some ws'
              VScale = Some hs'}
   
/// A lens from a symbol to its bounding box dimensions (as a width X height XYPos)
let customCompDims_ = Lens.create getCustomCompDims putCustomCompDims

// B2 W (a)
/// Updates the posiiton of a symbol on the sheet.
/// Takes a new X,Y position and the symbol.
/// Returns the symbol with its position updated.
/// WARNING - this does not update SheetT.Model BoundingBoxes
let updateSymPos (newPos : XYPos) (sym: Symbol)  =
    {sym with Pos = newPos}
    |> calcLabelBoundingBox // recalculate the label bounding box in the symbol


// B2 W (b)
/// Updates the position of a symbol on the sheet.
/// Takes a new X,Y position and the sheet Model.
/// Returns the Model with the symbol's position updated in Sheet BoundingBoxes
let updateSymPosInSheet (symId: ComponentId) (newPos : XYPos) (sheet: SheetT.Model) =
    Optic.map (SheetT.symbolOf_ symId) (updateSymPos newPos) sheet // update symbol & its label bounding box
    |> Optic.set (SheetT.boundingBoxes_ >-> Optics.Map.valueForce_ "Bounding box not found" symId >-> topLeft_ ) newPos



//B3 R
/// Reads the order of ports on a specified side of a symbol.
/// Takes the symbol and the edge to read from.
/// Returns the order of ports as a list.
let getPortOrder edge symbol  =
    symbol.PortMaps.Order.[edge]


//B3 W
/// Writes the order of ports on a specified side of a symbol.
/// Takes the symbol, the edge to rewrite the ports of and the new list of ports.
/// Returns the updated symbol with the new port order.
let putPortOrder edge newList symbol =
    symbol
    |> Optic.map (portMaps_ >-> order_) (Map.add edge newList)

//B4 RW
/// A lens for accessing the reversed input ports state of a MUX2 from its symbol
let reversedInputPorts_ =
    Lens.create (fun symbol -> symbol.ReversedInputPorts)
                (fun newState symbol -> {symbol with ReversedInputPorts = newState})


//B5 R
/// Reads the position of a port from SymbolT.Model
let getPortPos (portId: string) (model: SymbolT.Model)=
    Symbol.getPortLocation None model portId

//B6 R
/// Returns a bounding box of a symbol outline.
let getSymBoundingBox (symbol : Symbol) = symbol.SymbolBoundingBox
                     

//B7 RW
/// A lens for accessing the rotation state of a symbol.
/// NB after writing this, the symbol label box, and the sheet-level symbol
/// bounding box will chnage. Arguably this function should do those things.
let symbol_rotation_ =
    Lens.create (fun symbol -> symbol.STransform.Rotation)
                (fun newState symbol -> {symbol with STransform = {symbol.STransform with Rotation = newState}})

//B8 RW
/// A lens for accessing the flip state of a symbol.
let symbol_flipped_ =
    Lens.create (fun sym -> sym.STransform.Flipped)
                (fun newState sym -> {sym with STransform = {sym.STransform with Flipped =newState}})


//----------------------------------------------------------------------------------------------//
//-----------------------------------SegmentHelpers Submodel------------------------------------//
//----------------------------------------------------------------------------------------------//

/// Helpers to work with visual segments and nets
/// Includes functions to remove overlapping same-net segments
/// We can assume different-net segments never overlap.
module SegmentHelpers =

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

        /// Return the list of segment vectors with 3 vectors coalesced into one visible equivalent
        /// wherever this is possible
        let rec coalesce (segVecs: XYPos list)  =
            match List.tryFindIndex (fun segVec -> segVec =~ XYPos.zero) segVecs[1..segVecs.Length-2] with          
            | Some zeroVecIndex ->
                let index = zeroVecIndex + 1 // base index as it should be on full segVecs
                segVecs[0..index-2] @
                [segVecs[index-1] + segVecs[index+1]] @
                segVecs[index+2..segVecs.Length - 1]
                |> coalesce
            | None -> segVecs
     
        wire.Segments
        |> List.mapi getSegmentVector
        |> coalesce


    (* These functions make ASSUMPTIONS about the wires they are used on:
       - Distinct net segments never overlap
       - Same-net segments overlap from source onwards and therefore overlapping segments
         must have same start position
       - Overlap determination may very occasionally fail, so that overlapped
         wires are seen as not overlapped. This allows a much faster overlap check
    *)

    open BusWireT // so that Orientation D.U. members do not need qualification

    /// visible segments in a wire as a pair (start,end) of vertices.
    /// start is the segment end nearest the wire Source.
    let visibleSegsWithVertices (wire:BusWireT.Wire) (model: SheetT.Model) =
        visibleSegments wire.WId model
        |> List.map (fun segV -> wire.StartPos, wire.StartPos + segV)

    /// Input must be a pair of visula segment vertices (start, end).
    /// Returns segment orientation
    let visSegOrientation ((vSegStart, vSegEnd): XYPos * XYPos) =
        match abs (vSegStart.X - vSegEnd.X) > abs (vSegStart.Y - vSegEnd.Y) with
        | true -> Horizontal
        | false -> Vertical


    /// Filter visSegs so that if they overlap with common start only the longest is kept.
    /// ASSUMPTION: in a connected Net this will remove all overlaps
    let distinctVisSegs (visSegs: (XYPos * XYPos) list) =
        /// convert float to integer buckt number
        let pixBucket (pixel:float) = int(pixel / Constants.bucketSpacing)

        /// convert XYPos to pair of bucket numbers
        let posBucket (pos:XYPos) = pixBucket pos.X, pixBucket pos.Y

        visSegs
        // first sort segments so longest (which we want to keep) are first
        |> List.sortByDescending (fun (startOfSeg, endOfSeg) -> euclideanDistance startOfSeg endOfSeg)
        // then discard duplicates (the later = shorter ones will be discarded)
        // Two segments are judged the same if X & y starting coordinates map to the same "buckets"
        // This will very rarely mean that very close but not identical position segments are viewed as different
        |> List.distinctBy (fun ((startOfSeg, _) as vSeg) -> posBucket startOfSeg, visSegOrientation vSeg)

    /// Filter visSegs so that if they overlap with common start only the longest is kept.
    /// More accurate version of distinctVisSegs.
    /// Use if the accuracy is needed.
    let distinctVisSegsPrecision (visSegs: (XYPos * XYPos) list) =
        // This implementation clusters the segments, so cannot go wrong
        // It still uses the assumption that overlapped segments have common start position.
        // Without that, the code is slower and longer

        /// Turn segs into a distinctSegs list, losing shorter overlapped segments.
        /// All of segs must be the same orientation.
        let clusterSegments segs =

            /// Add a segment to distinctSegs unless it overlaps.
            /// In that case replace seg in distinctSegs if seg is longer than the segment it overlaps.
            /// If seg overlaps and is shorter, there is no change to distinctSegs.
            /// seg and all segments in distinctSegs must have same orientation.
            let addOrientedSegmentToClusters (distinctSegs:(XYPos*XYPos) list) (seg:XYPos*XYPos) =
                let len (seg: XYPos*XYPos) = euclideanDistance (fst seg) (snd seg)
                let segStart = fst seg
                distinctSegs
                |> List.tryFindIndex (fun dSeg -> euclideanDistance (fst dSeg) segStart < Constants.bucketSpacing / 2.)
                |> function
                        | Some index when len distinctSegs[index] < len seg ->
                            List.updateAt index seg distinctSegs
                        | Some index ->
                            distinctSegs // can do nothing
                        | _ ->
                            seg :: distinctSegs // add seg to the list of distinct (non-overlapped) segments

            ([], segs)
            ||> List.fold addOrientedSegmentToClusters
        visSegs
        |> List.partition (visSegOrientation >> (=) Horizontal) // separate into the two orientations
        |> (fun (hSegs, vSegs) -> clusterSegments hSegs @ clusterSegments vSegs) // cluster each orientation separately


    /// input is a list of all the wires in a net.
    /// output a list of the visual segments.
    /// isDistinct = true => remove overlapping shorter segments
    let getVisualSegsFromNetWires (isDistinct: bool) (model: SheetT.Model) netWires =
        netWires
        |> List.collect (fun wire -> visibleSegsWithVertices wire model)
        |> (if isDistinct then distinctVisSegs else id) // comment this to test the preision implementation
        //|> (if isDistinct then distinctVisSegsPrecision else id) // uncomment this to test the preision implementation


    /// Returns true if two segments (seg1, seg2) cross in the middle (e.g. not a T junction).
    /// Segment crossings very close to being a T junction will be counted. That however should not happen?
    /// Seg1, seg2 are represented as pair of start and end vertices
    let isProperCrossing (seg1: XYPos*XYPos) (seg2: XYPos*XYPos) =
        /// return true if mid is in between a & b, where the order of a & b does not matter.
        /// this is an open interval: if mid is close to an endpoint return false.
        // rewrite inMiddleOf here with larger tolerance if this is needed.
        let isBetween a mid b =
            match a > b with
            | true -> inMiddleOf b mid a
            | false -> inMiddleOf a mid b

        let properCrossingHV (hSeg:XYPos*XYPos) (vSeg:XYPos*XYPos) =
            let startH, endH = hSeg
            let startV, endV = vSeg
            isBetween startH.X startV.X endH.X &&
            isBetween startV.Y startH.Y endV.Y

        match visSegOrientation seg1, visSegOrientation seg2 with
        | BusWireT.Orientation.Horizontal,Vertical -> properCrossingHV seg1 seg2
        | Vertical,Horizontal -> properCrossingHV seg2 seg1
        | _ -> false


    /// visible segments in a Net defined as a pair (start,end) of vertices.
    /// source: the source port driving the Net
    /// start is the segment end nearest the wire Source.
    /// isDistinct = true => filter visible segments so they do not overlap
    /// where segments overlap only the longest is taken
    /// ASSUMPTION: all overlaps are on segments with same starting point
    let visibleSegsInNetWithVertices (isDistinct: bool) (source: OutputPortId) (model: SheetT.Model) =
        let wModel = model.Wire
        let wires = wModel.Wires
        let netWires =
            wires
            |> Map.filter (fun wid netWire -> netWire.OutputPort = source) // source port is same as wire
            |> Map.toList
            |> List.map snd

        netWires
        |> getVisualSegsFromNetWires isDistinct model

    /// return a list of all the wire Nets in the model
    /// Each element has form (source port Id, list of wires driven by port)   
    let allWireNets (model: SheetT.Model) =
        model.Wire.Wires
        |> Map.values
        |> Array.toList
        |> List.groupBy (fun wire -> wire.OutputPort)

    /// return a lits of all the distinct visible segments
    /// visible segments in a Net are defined as a pair (start,end) of vertices.
    /// Filter visible segments so they do not overlap
    /// where segments overlap only the longest is taken
    /// ASSUMPTION: all overlaps are on segments with same starting point
    let distinctVisibleSegsInNet = visibleSegsInNetWithVertices true

//--------------------------------end of SegmentHelpers----------------------------------//



open SegmentHelpers

//T1 R
/// Counts the number of pairs of symbols that intersect each other in the sheet.
/// uses sheet Model bounding boxes.
let numOfIntersectedSymPairs (sheet: SheetT.Model) =
    let boxes = Map.toList sheet.BoundingBoxes
    List.allPairs boxes boxes
    |> List.sumBy (function | ((id1, _),(id2, _)) when id1 <= id2 -> 0
                            | ((_, box1),(_, box2)) when BlockHelpers.overlap2DBox box1 box2 -> 1
                            | _ -> 0)


//T2 R
/// The Number of distinct wire visible segments that intersect with one or more symbols in the sheet.
/// Counts each such segment even if they overlap (which is not likely)
/// assumes that within one wire, at most one segment crosses a symbol boundary
/// although this is not always true, it is fine for a metric.
let numOfIntersectSegSym (model: SheetT.Model) : int =
    let wModel = model.Wire
    let allWires = model.Wire.Wires
                   |> Map.values
    allWires
    |> Array.map (findWireSymbolIntersections wModel)
    |> Array.sumBy (function [] -> 0 | _ -> 1)


// T3R
/// The number of pairs of distinct visible wire segments that cross each other at right angles in a sheet.
/// Returns the number right angle intersections between wire segments.
/// Does not include crossings that are "T junction"
/// counts segments that overlap only once
/// ASSUMPTION: overlapping segments are in same Net and have same starting point.
let numOfWireRightAngleCrossings (model: SheetT.Model)  =

    let nets = allWireNets model
    let distinctSegs =
        nets
        |> List.collect (fun (_, net) -> getVisualSegsFromNetWires true model net)
    List.allPairs distinctSegs distinctSegs
    |> List.filter (fun (seg1,seg2) -> seg1 > seg2 && isProperCrossing seg1 seg2)
    |> List.length
     

//T4 R
/// Sum the wiring length of all wires in the sheet, only counting once
/// when N wire segments of the same-net are overlapping.
/// Returns the total visible wiring segment length over the whole sheet.
/// ASSUMPTION: as in SegmentHelpers
let calcVisWireLength (model:SheetT.Model) : float =
    allWireNets model
    |> List.collect (fun (_, net) -> getVisualSegsFromNetWires true model net)
    |> List.sumBy( fun (startP,endP) -> euclideanDistance startP endP)

// T5 R
/// Counts the visible wire right-angles (bends) over the entire sheet.
/// Where same-net wires overlap a bend is counted only once
/// Returns the number of visible wire right-angles.
/// ASSUMPTIONS: right-angles come from two adjacent visible segments
/// ASSUMPTION: segment overlaps as SegmentHelpers
let numOfVisRightAngles (model: SheetT.Model) : int =
    let nets = allWireNets model
    let numWires = nets |> List.sumBy (fun (source,wires) -> wires.Length)
    let distinctSegs =
        nets
        |> List.collect (fun (_, net) -> getVisualSegsFromNetWires true model net)
    // every visual segment => right-angle bend except for the first (or last) in a wire
    distinctSegs.Length - numWires
    

//T6 R
/// Returns the retracing segments, and those which intersect symbols.
/// a segment seg is retracing if the segment before it is zero-length and
/// the segment two segments before has opposite sign length
let findRetracingSegments (model : SheetT.Model) =
    /// Return any segemnts in the wire which are retracing.
    let getRetracingSegments (segs: BusWireT.ASegment list) =
        /// the two segments go in opposite directions so retrace if separted by zero segmnet
        let hasOppositeDir (seg1:BusWireT.ASegment) (seg2:BusWireT.ASegment) =
            System.Math.Sign seg1.Segment.Length <> System.Math.Sign seg2.Segment.Length
        segs[2..segs.Length-1] // take all but first two segments - those cannot retrace
        |> List.mapi (fun n seg -> n+2, seg) // index (n+2) is correct for lookup in segs
        |> List.filter (fun (n,seg) -> segs[n-1].IsZero && hasOppositeDir segs[n-2] seg)
        |> List.map snd

    /// list of all the segments that are retracing
    let retracingSegs = 
        model.Wire.Wires
        |> Map.values
        |> Array.toList
        |> List.collect (getAbsSegments >> getRetracingSegments)

    /// list of all the symbol bounding boxes from sheet model
    let symbolBoundingBoxes =
        model.BoundingBoxes
        |> Map.toList
        |> List.map (fun (_, box) -> box)

    /// return true if the segments intersects any symbol
    let checkSegIntersectsAnySymbol (aSeg: BusWireT.ASegment) =
        symbolBoundingBoxes
        |> List.exists (fun box ->
            segmentIntersectsBoundingBox box aSeg.Start aSeg.End
            |> Option.isSome)

    let retracingSegsInsideSymbol = retracingSegs |> List.filter checkSegIntersectsAnySymbol

    {| RetraceSegs =retracingSegs;
       RetraceSegsInSymbol = retracingSegsInsideSymbol|}
