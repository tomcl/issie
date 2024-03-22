module SheetBeautifyD2

//-----------------------------------------------------------------------------------------//
//----------Module for top-level beautify functions making (mostly) whole-sheet changes----//
//-----------------------------------------------------------------------------------------//

(*
Whole sheet functions are normally applied to whole sheet. In many cases a feature could be to
apply them to currently selected wires or components. That provides users control over what gets beautified.
Ideal beautify code will never make things worse so can be applied to whole sheet always.

Otehr relevant modules:
SheetBeautifyHelpers (helpers)
Generatedata (used to make randomized tests)
TestDrawBlock (used to test the code written here).

*)

// open modules likely to be used
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open SheetBeautifyHelpers
open RotateScale
open Optics
open Operators

/// constants used by SheetBeautify
module Parameters =
    type PermutePenalty = {
        VerticalFlip : float;
        HorizontalFlip: float;
        Rotation90: float;
    }

    let MuxPenalty = {
        VerticalFlip = 1.1;
        HorizontalFlip = 2.5;
        Rotation90 = 3.0;
    }

    let GateNPenalty = {
        VerticalFlip = 1.1;
        HorizontalFlip = 2.5;
        Rotation90 = 3.0;
    }

    /// Scale the number of wire crossing caused by a symbol, where a higher number means the symbol is "beautified first"
    let scaleSymbolOrder ( sym : SymbolT.Symbol ) = 
        match sym.Component.Type with
        | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 -> 3.0
        | GateN _ -> 1.5
        | Custom _ -> 2.0
        | _ -> 1.0

module mySheetBeautifyHelpers = 
    /// <summary>
    /// This function takes a wire and returns a list of visible segments by coalescing all zero length segments.
    /// Each segment is represented as a tuple of start and end positions (XYPos * XYPos).
    /// </summary>
    /// <param name="wire">The wire for which to find visible segments.</param>
    /// <returns>A list of tuples representing the visible segments of the wire.</returns>
    let visibleSegments (wire : BusWireT.Wire): (XYPos * XYPos) list =

        let tryCoalesceAboutIndex ((remainingSegs : ASegment List, coalescedSegs : (XYPos * XYPos) list)) (_)  =
            match remainingSegs with
            | prev::z1::next::z2::tail when z1.IsZero && z2.IsZero -> ([{prev with End=next.End};z2] @ tail, coalescedSegs)
            | prev::curr::next::tail -> 
                if curr.IsZero
                then 
                    let coalescedSeg = (prev.Start, next.End)
                    (tail, coalescedSegs @ [coalescedSeg])
                else        
                    ([curr;next] @ tail, coalescedSegs @ [(prev.Start,prev.End)])
                    
            | seg1::seg2::tail when tail = [] -> ([], coalescedSegs @ [(seg1.Start,seg1.End);(seg2.Start,seg2.End)])
            | _ -> ([], coalescedSegs)

        wire
        |> BlockHelpers.getAbsSegments
        |> (fun segVecs ->
                ((segVecs,[]),segVecs)
                ||> List.fold tryCoalesceAboutIndex)
        |> snd

    /// <summary>
    /// Given a list, returns all unique pairings of its elements, ignoring ordering.
    /// </summary>
    /// <param name="lst">The list for which to find all distinct pairs.</param>
    /// <returns>A list of tuples representing all distinct pairs in the input list.</returns>
    let rec allDistinctPairs lst =
        match lst with
        | [] -> []
        | h::t -> List.allPairs [h] t @ allDistinctPairs t

    // adapted from findWireSymbolIntersection
    /// <summary>
    /// Retrieves the bounding boxes of all symbols in a given sheet.
    /// </summary>
    /// <param name="sheet">The sheet containing symbols.</param>
    /// <returns>A list of tuples where each tuple contains the component type and the bounding box of the symbol.</returns>
    let allSymbolBBoxInSheet ( sheet : SheetT.Model) =
        sheet.Wire.Symbol.Symbols
        |> Map.values
        |> Seq.toList
        |> List.map (fun s -> (s.Component.Type, Symbol.getSymbolBoundingBox s))

    type IntresectionType = 
        | CrossJunction 
        | TJunction

    /// Returns an option that indicated whether there is an intersection, and the position of said intersection
    /// Expects two ASegments of opposite orientation
    let getIntersectionOpt (seg1 : ASegment) (seg2: ASegment) = 
        let hori, vert = match seg1.Orientation  with | Horizontal -> (seg1,seg2) | Vertical -> (seg2,seg1)
        
        let xmin, xmax = min hori.Start.X hori.End.X, max hori.Start.X hori.End.X 
        let ymin, ymax = min vert.Start.Y vert.End.Y, max vert.Start.Y vert.End.Y

        let isTJunction = 
            hori.Start.Y = ymax 
            || hori.Start.Y = ymin 
            || vert.Start.X = xmin 
            || vert.Start.X = xmax

        let intersectRightAngle = 
            (vert.Start.X <= xmax) && (vert.Start.X >= xmin)
            &&
            (hori.Start.Y <= ymax) && (hori.Start.Y >= ymin)

        (match intersectRightAngle with
        | false -> None
        | true -> 
            if isTJunction 
            then Some TJunction
            else Some CrossJunction
        , {X=vert.Start.X;Y=hori.Start.Y})

    let countSegmentPairIntersections ( segPairs : (OutputPortId * ASegment) list ) =
        allDistinctPairs segPairs
        |> List.collect (fun ((netId1,seg1), (netId2,seg2)) ->
            if seg2.Orientation <> seg1.Orientation // to cross at right angles orientation must be opposite
            then
                match getIntersectionOpt seg1 seg2 with
                | None, _ -> []
                | Some TJunction, intPos -> if netId1 <> netId2 then [intPos] else [] 
                | Some CrossJunction, intPos -> [intPos]
            else
                []
        )
        // remove any overlapping intersections
        |> List.distinct
        |> List.length

    /// <summary>
    /// Counts the number of distinct wire segments that intersect orthogonally on a sheet.
    /// </summary>
    /// <param name="sheet">The sheet containing wire segments.</param>
    /// <returns>
    /// An integer representing the count of distinct wire segments that intersect at right angles.
    /// </returns>
    /// <remarks>
    /// <para>
    /// The IntersectionType D.U. is used to distinguish between different types of intersections: CrossJunction and TJunction.
    /// If a TJunction is found within segments belonging to the same net it is ignored.
    /// </para>
    /// </remarks>
    let countVisibleSegmentIntersection ( sheet : SheetT.Model) = 
        // get a list of segments which intersect at right angles
        // for each segment obtain asbolute start and end position
        // check orthogonality by checking each distinct segment pair to ensure they have opposite orientation and are within range of each other
        let allSegments = 
            sheet.Wire.Wires
            |> Map.toList
            |> List.collect (fun (wId, wire) -> BlockHelpers.getNonZeroAbsSegments wire |> List.map (fun v -> (wire.OutputPort, v)))
            |> List.distinctBy (fun (netid, seg) -> (netid,seg.Start,seg.End))


        countSegmentPairIntersections allSegments

    /// <summary>
    /// Counts the number of distinct wire segments that intersect orthogonally on a sheet excluding 
    /// those belonging to any wire connected to the provided symbol.
    /// </summary>
    /// <param name="sheet">The sheet containing wire segments.</param>
    /// <param name="sym">The symbol whose wires are to be ignored.</param>
    /// <returns>
    /// An integer representing the count of distinct wire segments that intersect at right angles.
    /// </returns>
    /// <remarks>
    /// <para>
    /// The IntersectionType D.U. is used to distinguish between different types of intersections: CrossJunction and TJunction.
    /// If a TJunction is found within segments belonging to the same net it is ignored.
    /// </para>
    /// </remarks>
    let countVisibleSegmentIntersectionWithoutSymbol ( sheet : SheetT.Model) (sym : SymbolT.Symbol) = 
        let inputPortIds =  Optic.get (component_ >-> inputPorts_) sym |> List.map (fun port -> port.Id)
        let outputPortIds =  Optic.get (component_ >-> outputPorts_) sym |> List.map (fun port -> port.Id)


        let allSegments = 
            sheet.Wire.Wires
            |> Map.toList
            |> List.filter (fun (wid, wire) -> 
                match List.tryFind (fun x -> x = string wire.InputPort) inputPortIds, List.tryFind (fun x -> x = string wire.OutputPort) outputPortIds with
                | None, None -> true
                | _ -> false
            )
            |> List.collect (fun (wId, wire) -> BlockHelpers.getNonZeroAbsSegments wire |> List.map (fun v -> (wire.OutputPort, v)))
            |> List.distinctBy (fun (netid, seg) -> (netid,seg.Start,seg.End))

        countSegmentPairIntersections allSegments


open mySheetBeautifyHelpers
open Parameters

module D2Helpers = 

    /// Given a symbol, returns the symbol with its' inputs reversed
    /// (Main application is flipping the location of the select symbol in a mux)
    let changeReversedInputs symbol =
        let newValue =
            match symbol.ReversedInputPorts with
            |Some false -> Some true
            |Some true -> Some false
            |None -> Some true
        let newSymbolInfo = 
            match symbol.Component.SymbolInfo with
            |Some si -> Some {si with ReversedInputPorts = newValue}
            |None -> None
        let newcompo = {symbol.Component with SymbolInfo = newSymbolInfo }
        {symbol with Component = newcompo; ReversedInputPorts = newValue}
    
    /// Given a scaling factor, weight and symbol 
    /// returns a pair containing the unchanged symbol and weight and the permuted symbol with corresponding weight
    let rotationPermute scalingFactor ( scale, symbol : SymbolT.Symbol )  =
        let centre = getBlock [symbol] |> (fun block -> block.Centre())
        [scale, symbol; (scale * scalingFactor), rotateSymbolInBlock Degree90 centre symbol]

    /// Given a flip type, scaling factor, weight and symbol 
    /// returns a pair containing the unchanged symbol and weight and the permuted symbol with corresponding weight
    let flipPermute flip (scalingFactor : float) (scale,sym : SymbolT.Symbol)  =
        let centre = getBlock [sym] |> (fun block -> block.Centre())
        [(scale, sym) ; ((scale * scalingFactor),flipSymbolInBlock flip centre sym)]

    /// Given a list, returns a list containing all possible permutations of the input list
    let rec permute lst  =
        lst
        |> List.mapi (fun n xn ->
            let lst' = List.removeAt n lst
            permute lst'
            |> List.map (fun permL -> xn :: permL))
        |> List.concat

    /// Given a set of penalties and a list of symbols with some weighting, 
    /// applies vertical and horizontal flip and 90 degree rotation permutations returning a list containing all permutations
    let permuteMux ( penalty : PermutePenalty ) ( symbolL : (float * SymbolT.Symbol) list ) = 
        symbolL
        |> List.collect (flipPermute FlipVertical penalty.VerticalFlip)
        |> List.collect (flipPermute FlipHorizontal penalty.HorizontalFlip)
        |> List.collect (rotationPermute penalty.Rotation90)

    /// Given a sheet and a symbol-componentId pair applies the symbol into the list, updating the relevant wires
    let updateSymbolInSheet sheet (cid,newSym) = 
        sheet
        |> Optic.map (symbols_) (Map.add cid newSym)
        |> Optic.map SheetT.wire_ (BusWireSeparate.reRouteWiresFrom [cid])

    /// Given a Symbol and Sheet Exhaustively search through all permutations of the symbol to find the configuration which minimises the wire crossing heuristic
    let optimisePermuteSymbol ( sheet: SheetT.Model ) cid symL penalties  =
        let prevCrossings = countVisibleSegmentIntersection sheet
        symL
        |> permuteMux penalties
        |> List.map (fun (scale,newSym) -> 
            let newSheet = updateSymbolInSheet sheet (cid,newSym)

            let newCrossings = countVisibleSegmentIntersection newSheet

            let diff = float (newCrossings - prevCrossings)
            // printf "diff:%A w:%A" (newCrossings - prevCrossings) weight;
            (diff, scale, newSheet))
        |> (fun lst -> 
            List.unzip3 lst
            |> (fun (diffs, scales, sheets) ->
                let minDiff = List.min diffs
                let biasedDiffs = List.map ((+) (minDiff * -1.0 + 1.0)) diffs
                // printf "%A" biasedDiffs;
                let weights = List.zip biasedDiffs scales |> List.map (fun (x,y) -> x*y);
                // printf "%A" weights;
                (weights, sheets)
                ||> List.zip
            )
        )
        // |> List.map (fun (scale, sheet) -> printf "%A %A" scale (scale * (float (countVisibleSegmentIntersection sheet) + 1.0)); (scale,sheet))
        |> List.minBy fst
        |> snd
        
    /// Sort a symbol list by the number of wire crossings each symbol causes, considering the scaling factor based on the symbol type  
    let sortSymbolsByCrossings sheet syms = 
        let initWireCrossings = countVisibleSegmentIntersection sheet

        syms
        |> List.map (fun (cid, sym) -> 
            let crossingsWithoutSym = countVisibleSegmentIntersectionWithoutSymbol sheet sym
            ((cid,sym),float (initWireCrossings - crossingsWithoutSym) * (scaleSymbolOrder sym))
        )
        |> List.sortByDescending (fun (_, crossings) -> crossings)
        |> List.unzip 
        |> fst

    /// Given a Symbol and a Sheet return a Sheet where the symbol has been modified to reduce the number of wire crossings
    /// Customised are applied to specific component types to encourage/discourage rotation behaviour
    let reduceWireCrossings ( sheet: SheetT.Model ) ((cid: ComponentId,sym : SymbolT.Symbol)) = 
        match sym.Component.Type with
        | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 -> optimisePermuteSymbol sheet cid [1.0, sym; 1.0, changeReversedInputs sym] MuxPenalty
        | GateN _ -> optimisePermuteSymbol sheet cid [1.0, sym] GateNPenalty
        | _ -> optimisePermuteSymbol sheet cid [1.0, sym] {VerticalFlip=100.0; HorizontalFlip=100.0; Rotation90=100.0}

    /// Given a sheet and a list of sorted symbols, traverses the list by finding the optimal symbol and recalculating the symbol list
    let rec beautifySheet sheet syms = 
        match syms with
        | [] -> sheet
        | [sym] -> reduceWireCrossings sheet sym
        | h::t ->
            let optimisedSheet = reduceWireCrossings sheet h

            sortSymbolsByCrossings optimisedSheet t
            |> beautifySheet optimisedSheet
            

open D2Helpers

let sheetOrderFlip ( sheet : SheetT.Model ) = 
    sheet.Wire.Symbol.Symbols
    |> Map.toList
    |> sortSymbolsByCrossings sheet
    |> beautifySheet sheet
