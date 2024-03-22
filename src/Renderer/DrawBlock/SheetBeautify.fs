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

/// constants used by SheetBeautify
module Constants =
    () // dummy to make skeleton type check - remove when other content exists

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
  |> printListInline2 (fun wire -> visibleWireNetLength sheet.Wire wire)
//   |> List.map 
  |> List.filter (fun w -> 
    printfn $"this ratio {(visibleWireNetLength sheet.Wire w) / totalWireLength}"
    (visibleWireNetLength sheet.Wire w) / (totalVisibleWireLength sheet.Wire) |> (<) threshold // criterion: relative length
    // visibleWireNetLength sheet.Wire w > threshold // criterion: absolute length
  )

  // visibleWireNetsLength sheet.Wire (sheet.Wire.Wires |> Helpers.mapValues |> Seq.head)
let beautifyD3 (sheet: SheetT.Model) =
  getLongWires sheet 0.1
  |> List.fold (fun previousSheet wire -> replaceWireWithLabel wire previousSheet) sheet
    
