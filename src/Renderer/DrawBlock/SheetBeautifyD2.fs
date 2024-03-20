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
module Constants =
    () // dummy to make skeleton type check - remove when other content exists

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

    // function 11 : The number of distinct pairs of segments that cross each other at right angles. 
    // Does not include 0 length segments or segments on same net intersecting at one end, or segments on same net on top of each other. Count over whole sheet.
    // T3R
    // This function was tested by running it in issie and visually inspecting behaviour at various corner cases
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

module D2Helpers = 

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

    let rotationPermute scalingFactor ( scale, symbol : SymbolT.Symbol )  =
        let centre = getBlock [symbol] |> (fun block -> block.Centre())
        [scale, symbol; (scale * scalingFactor), rotateSymbolInBlock Degree90 centre symbol]

    let flipPermute flip (scalingFactor : float) (scale,sym : SymbolT.Symbol)  =
        let pos = {X = sym.Pos.X + sym.Component.W / 2.0 ; Y = sym.Pos.Y + sym.Component.H / 2.0 }
        let centre = getBlock [sym] |> (fun block -> block.Centre())
        printf "%A" centre;
        [(scale, sym) ; ((scale * scalingFactor),flipSymbolInBlock flip centre sym)]

    let rec permute lst  =
        lst
        |> List.mapi (fun n xn ->
            let lst' = List.removeAt n lst
            permute lst'
            |> List.map (fun permL -> xn :: permL))
        |> List.concat

    // stolen from findWireSymbolIntersections in BusWireRoute
    // let componentIsMux (comp:Component) =
    //     match comp.Type with
    //     | Mux2 | Mux4 | Mux8 -> true
    //     | _ -> false

    let permuteMux ( symbolL : (float * SymbolT.Symbol) list ) = 
        symbolL
        |> List.collect (flipPermute FlipVertical 1.1)
        |> List.collect (flipPermute FlipHorizontal 2.5)
        |> List.collect (rotationPermute 3.0)

    // /// combine a list of symbol permutations into a list of all possible symbol permutations with each other
    // let combinePermutations ( allPerms ) = 
    //     ([], allPerms)
    //     ||> Map.fold (fun (combinedPerms) cid perms -> 
    //         match combinedPerms with
    //         | [] -> perms |> List.map (fun sym -> Map.empty |> Map.add cid sym)
    //         | c -> 
    //             List.allPairs c perms
    //             |> List.map (fun (oldmap, newsym) -> Map.add cid newsym oldmap))

    // let updateSymbolsInSheet sheet newSyms = 
    //     let oldSymbols = Optic.get symbols_ sheet

    //     let newSymbols = Map.fold (fun old cid sym -> Map.add cid sym old) oldSymbols newSyms

    //     sheet
    //     |> Optic.set symbols_ newSymbols 
    //     |> Optic.map SheetT.wire_ (BusWireSeparate.reRouteWiresFrom (newSymbols.Keys |> Seq.toList))

    let updateSymbolInSheet sheet (cid,newSym) = 
        sheet
        |> Optic.map (symbols_) (Map.add cid newSym)
        |> Optic.map SheetT.wire_ (BusWireSeparate.reRouteWiresFrom [cid])

    // let evaluateFlip ( sheet : SheetT.Model ) ( newSyms : Map<ComponentId,Symbol> ) = 
    //     updateSymbolsInSheet sheet newSyms
    //     |> countVisibleSegmentIntersection

    /// Given a Symbol and Sheet Exhaustively search through all permutations of the symbol to find the configuration which minimises the wire crossing heuristic
    let optimisePermuteSymbol ( sheet: SheetT.Model ) cid symL  =
        let prevCrossings = countVisibleSegmentIntersection sheet
        symL
        |> permuteMux
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
        

    /// Scale the number of wire crossing caused by a symbol, where a higher number means the symbol is "beautified first"
    let scaleSymbolOrder ( sym : SymbolT.Symbol ) = 
        match sym.Component.Type with
        | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 -> 3.0
        | GateN _ -> 1.5
        | Custom _ -> 2.0
        | _ -> 1.0

    /// Given a Symbol and a Sheet return a Sheet where the symbol has been modified to reduce the number of wire crossings
    let reduceWireCrossings ( sheet: SheetT.Model ) ((cid,sym : SymbolT.Symbol)) = 
        match sym.Component.Type with
        | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 -> optimisePermuteSymbol sheet cid [1.0, sym; 1.0, changeReversedInputs sym]
        | GateN _ -> optimisePermuteSymbol sheet cid [1.0, sym]
        | _ -> sheet

open D2Helpers

let sheetOrderFlip ( sheet : SheetT.Model ) = 
    // Basic Mux Implementation:
    // Obtain all of the muxes in the sheet
    // for each one obtain all permutations of the mux 
    // combine all permutations to obtain all possible permutation of all muxes
    // minimise over the number of wire crossings
    
    // sheet.Wire.Symbol.Symbols
    // |> Map.filter (fun _cid sym -> componentIsMux <| Optic.get component_ sym)
    // |> Map.map (fun cid sym -> 
    //     let perms = permutateMux sym
        
    //     let print = 
    //         perms |> List.map (fun sym ->
    //         printfn "pos %A rev %A flip %A rot %A" 
    //             (Optic.get posOfSym_ sym)
    //             (Optic.get reversedInputPorts_ sym)
    //             (Optic.get symbol_flipped_ sym)
    //             (Optic.get symbol_rotation_ sym));
        
    //     perms)
    // |> combinePermutations
    // |> List.mapi (fun i newSyms -> 
    //     let newSheet = updateSymbolsInSheet sheet newSyms
    //     let numCrossing = countVisibleSegmentIntersection newSheet
    //     printfn "%d crossings num: %d"  i numCrossing;
    //     (numCrossing,newSheet))
    // |> List.minBy (fun (num, sheet) -> num)
    // |> snd
    // |> updateSymbolsInSheet sheet

    let initWireCrossings = countVisibleSegmentIntersection sheet

    sheet.Wire.Symbol.Symbols
    |> Map.map (fun cid sym -> 
        let crossingsWithoutSym = countVisibleSegmentIntersectionWithoutSymbol sheet sym
        printf "sym: %A bef: %A after: %A diff: %A" sym.Component.Label initWireCrossings crossingsWithoutSym (initWireCrossings - crossingsWithoutSym);
        (sym,(initWireCrossings - crossingsWithoutSym))
    )
    |> Map.map (fun cid (sym, crossings) -> 
        (sym, float crossings * (scaleSymbolOrder sym))
    )
    |> Map.toList
    |> List.sortByDescending (fun (cid, (sym, crossings)) -> crossings)
    |> List.map (fun (cid, (sym, crossings)) -> (cid,sym))
    // |> List.map (fun (cid, (sym, crossings)) ->
    //     printf "%A %A %A" sym.Component.Label sym.Component.Type crossings
    // )
    |> List.fold reduceWireCrossings sheet

    // sheet
