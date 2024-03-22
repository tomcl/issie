module SheetBeautify

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
open Optics
open BusWireUpdateHelpers
open Optics.Optic
open Operators
open BlockHelpers
open RotateScale

/// constants used by SheetBeautify
module Constants =
    () // dummy to make skeleton type check - remove when other content exists


module SheetBeautifyD1 =

    /// The visible segments of a wire, as a list of vectors, from source end to target end.
    /// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed
    /// which if present causes the two segments on either side of it to coalesce into a single visible segment.
    /// A wire can have any number of visible segments - even 1.
    let visibleSegments (wId: ConnectionId) (model: SheetT.Model) : XYPos list =

        let wire = model.Wire.Wires[wId] // get wire from model

        /// helper to match even and off integers in patterns (active pattern)
        let (|IsEven|IsOdd|) (n: int) =
            match n % 2 with
            | 0 -> IsEven
            | _ -> IsOdd

        /// Convert seg into its XY Vector (from start to end of segment).
        /// index must be the index of seg in its containing wire.
        let getSegmentVector (index: int) (seg: BusWireT.Segment) =
            // The implicit horizontal or vertical direction  of a segment is determined by
            // its index in the list of wire segments and the wire initial direction
            match index, wire.InitialOrientation with
            | IsEven, BusWireT.Vertical
            | IsOdd, BusWireT.Horizontal -> { X = 0.; Y = seg.Length }
            | IsEven, BusWireT.Horizontal
            | IsOdd, BusWireT.Vertical -> { X = seg.Length; Y = 0. }

        /// Return the list of segment vectors with 3 vectors coalesced into one visible equivalent
        /// wherever this is possible
        let rec coalesce (segVecs: XYPos list) =
            match List.tryFindIndex (fun segVec -> segVec =~ XYPos.zero) segVecs[1 .. segVecs.Length - 2] with
            | Some zeroVecIndex ->
                let index = zeroVecIndex + 1 // base index as it should be on full segVecs
                segVecs[0 .. index - 2]
                @ [ segVecs[index - 1] + segVecs[index + 1] ]
                @ segVecs[index + 2 .. segVecs.Length - 1]
                |> coalesce
            | None -> segVecs

        wire.Segments
        |> List.mapi getSegmentVector
        |> coalesce

    ///<summary> Determines if a wire is parallel and can be straightened based on the length of its non zero absolute segments. </summary>
    /// <param name="wire">The wire to be checked for parallelism.</param>
    /// <returns>True if the wire is parallel, false otherwise.</returns>
    let isParallel (sheet: SheetT.Model) (wire: Wire) : bool =
        (visibleSegments wire.WId sheet).Length = 3

    /// <summary>Filters the list of wires in a sheet model to only include parallel wires.</summary>
    /// <param name="sheetModel">The sheet model to be filtered.</param>
    /// <returns>A list of parallel wires.</returns>
    let getParallelWires (sheetModel: SheetT.Model) : Wire list =
        sheetModel.Wire
        |> getWireList
        |> List.filter (isParallel sheetModel)

    /// <summary>Retrieves the source and target symbols of a wire in a sheet model and appends wire segments for further processing and scalability.</summary>
    /// <param name="sheet">The sheet model to be processed.</param>
    /// <param name="wire">The wire to be processed.</param>
    /// <returns>The source and target symbols of the wire and the wire segments.</returns>
    let getSymbols (sheet: SheetT.Model) (wire: Wire) : Symbol * Symbol * (XYPos list) =
        let sourceSymbol = getSourceSymbol sheet.Wire wire
        let targetSymbol = getTargetSymbol sheet.Wire wire
        sourceSymbol, targetSymbol, (visibleSegments wire.WId sheet)

    /// <summary>Retrieves a list of connected symbols in a sheet model.</summary>
    /// <param name="sheet">The sheet model to be processed.</param>
    /// <returns>A list of singly connected symbols.</returns>
    let getConnectedSymbols (sheet: SheetT.Model) (wireList: Wire List) =
        let symbols = wireList |> List.map (getSymbols sheet)
        symbols

    /// <summary>Updates the position of a symbol based on a list of segments (parallel wire) and returns the updated symbol along with its component ID for further processing.</summary>
    /// <param name="symbol">The symbol to be updated.</param>
    /// <param name="segments">The list of segments to be used for updating the symbol position.</param>
    /// <returns>The component ID and the updated symbol.</returns>
    let updateSymbolPosition (symbol: Symbol) (change: XYPos) = moveSymbol (change) symbol

    /// <summary>Determines if two symbols intersect.</summary>
    /// <param name="a">The first symbol to be checked.</param>
    /// <param name="b">The second symbol to be checked.</param>
    /// <returns>True if the symbols intersect, false otherwise.</returns>
    let boundingBoxesIntersect (a: Symbol) (b: Symbol) : bool =
        overlap2DBox (getSymBoundingBox a) (getSymBoundingBox b)

    /// <summary>Updates the position of a symbol if it does not intersect with any other symbols in the sheet model.</summary>
    /// <param name="symbol">The symbol to be updated.</param>
    /// <param name="change">The change in position to be applied to the symbol.</param>
    /// <param name="allSymbols">The list of all symbols in the sheet model.</param>
    /// <returns>The updated symbol.</returns>
    let moveSymbolIfNoIntersection (symbol: Symbol) (change: XYPos) (allSymbols: Symbol list) : Symbol =
        let newSymbol = updateSymbolPosition symbol change
        let otherSymbols = List.filter ((<>) symbol) allSymbols
        if List.exists (boundingBoxesIntersect newSymbol) otherSymbols then
            symbol
        else
            newSymbol

    /// <summary>Aligns the symbols onnce in a sheet model based on the parallel wires in the sheet model.</summary>
    /// <param name="sheet">The sheet model to be processed.</param>
    /// <returns>The sheet model with aligned symbols.</returns>
    let sheetAlignOnce (sheet: SheetT.Model) : SheetT.Model =
        let singlyConstrainedConnectedSymbols =
            getConnectedSymbols sheet (getParallelWires sheet)
        let allSymbols = get SheetT.symbols_ sheet
        let updatedSymbolMap =
            singlyConstrainedConnectedSymbols
            |> List.fold
                (fun (symbolMap: Map<ComponentId, Symbol>) (a, _, c) ->
                    let newSymbol = moveSymbolIfNoIntersection a c[1] (symbolMap.Values |> List.ofSeq)
                    Map.add newSymbol.Id newSymbol symbolMap)
                allSymbols

        sheet
        |> set SheetT.symbols_ updatedSymbolMap
        |> map SheetT.wire_ (BusWireSeparate.reRouteWiresFrom (updatedSymbolMap.Keys |> Seq.toList))

    /// <summary>Resizes a symbol based on the position of another symbol.</summary>
    /// <param name="sheet">The sheet model to be processed.</param>
    /// <returns>The sheet model with resized symbols.</returns>
    let scaleCustomComponents (sheet: SheetT.Model) =
        let newWire =
            getConnectedSymbols sheet (getParallelWires sheet)
            |> List.fold (fun sheet (comp1, comp2, _) -> (reSizeSymbolTopLevel sheet comp1 comp2)) sheet.Wire

        sheet |> set SheetT.wire_ (newWire)

    /// <summary>Completely aligns the symbols in a sheet model based on the parallel wires in the sheet model without symbol overlaps.</summary>
    /// <param name="sheet">The sheet model to be processed.</param>
    /// <returns>The sheet model with aligned symbols.</returns>
    let rec alignSymbols (sheet: SheetT.Model) =
        match getParallelWires sheet |> List.length with
        | 0 -> sheet
        | initialParallelWiresCount ->
            let newSheet = sheetAlignOnce sheet
            match getParallelWires newSheet |> List.length with
            | newParallelWiresCount when initialParallelWiresCount <= newParallelWiresCount -> sheet
            | _ -> alignSymbols newSheet

    /// <summary>Beautifies a sheet model by updating the positions and scale of symbols.</summary>
    /// <param name="sheet">The sheet model to be beautified.</param>
    /// <returns>The beautified sheet model.</returns>
    let sheetAlignScale (sheet: SheetT.Model) =
        sheet |> alignSymbols |> scaleCustomComponents


let visibleWireNetLength (wModel: BusWireT.Model) (wire: BusWireT.Wire) =
  let thisNet = 
    wModel.Wires
    |> Helpers.mapValues
    |> Seq.toList
    |> List.filter (fun w -> w.OutputPort = wire.OutputPort)

  thisNet
  |> List.map BlockHelpers.getNonZeroAbsSegments
  |> List.concat
  |> List.distinctBy (fun seg -> seg.Start, seg.End) // remove obvious overlaps
  |> (fun segs -> 
    List.fold (fun length (seg: BusWireT.ASegment) -> 
      segs
      |> List.tryFind (fun seg' ->
        // do not double count near T junction
        seg'.Segment.GetId <> seg.Segment.GetId 
        && seg'.Segment.Length < seg.Segment.Length
        && (seg'.Start = seg.Start
          || seg'.End = seg.End))
      |> function
      | Some dupl -> length + (abs seg.Segment.Length) - (abs dupl.Segment.Length)
      | None -> length + (abs seg.Segment.Length)
    ) 0.0 segs
  )

let getLongWires (sheet: SheetT.Model) threshold =
  let totalWireLength = calcVisWireLength sheet

  sheet.Wire.Wires
  |> Helpers.mapValues
  |> Seq.toList
  |> List.filter (fun w -> 
    let score = (BlockHelpers.getWireLength w) / (totalVisibleWireLength sheet.Wire * (float)(Map.count sheet.Wire.Wires))
    printfn $"this ratio {score}"
     
    score > threshold // criterion: relative length
    // visibleWireNetLength sheet.Wire w > threshold // criterion: absolute length
  )

  // visibleWireNetsLength sheet.Wire (sheet.Wire.Wires |> Helpers.mapValues |> Seq.head)
let beautifyD3 (sheet: SheetT.Model) =
//   getLongWires sheet 0.5
  getLongWires sheet 0.005
  |> List.fold (fun previousSheet wire -> replaceWireWithLabel wire previousSheet) sheet
        
