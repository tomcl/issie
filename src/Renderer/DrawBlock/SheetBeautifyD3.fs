module SheetBeautifyD3

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
open BlockHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open SheetBeautifyHelpers
open Optics
open Optics.Operators

// my plan is to modify the ports to contain a flag as to whether the wires are labeled or not
// let portWireLabeled_ (outputPortId: OutputPortId) : Lens<SheetT.Model, bool> =
let portWireLabeled_ (outputPortId: OutputPortId) : Lens<SheetT.Model, Port> =
  let port_ = symbol_ >-> ports_

  let getPort' (sheet: SheetT.Model) =
    Optic.get port_ sheet
    // port should be there, if this is a concern we can use prism
    |> Map.find (getOutputPortIdStr outputPortId)

  // let setPort wireLabeled outputId  =
  let setPort curPort (sheet: SheetT.Model) =

    // let curPort = getPort' outputId
    let curMap = Optic.get port_ sheet
    
    // Optic.set port_ (Map.add (getOutputPortIdStr outputId) { curPort with IsWireLabeled = wireLabeled } curMap) sheet
    Optic.set port_ (Map.add (getOutputPortIdStr outputPortId) curPort curMap) sheet

  getPort', setPort


let toggleWireLabel (sheet: SheetT.Model) (outputPort: OutputPortId) =
  let myLens_ = portWireLabeled_ sheet
  ()
  // Optic.set myLens_ 
let visibleWireNetLength (wModel: BusWireT.Model) (wire: BusWireT.Wire) =
  let thisNet = 
    wModel.Wires
    |> Helpers.mapValues
    |> Seq.toList
    |> List.filter (fun w -> w.OutputPort = wire.OutputPort)

  thisNet
  |> List.map getNonZeroAbsSegments
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
  sheet.Wire.Wires
  |> Helpers.mapValues
  |> Seq.toList
  |> List.filter (fun w -> 
    // (visibleWireNetLength sheet.Wire w) / (totalVisibleWireLength sheet.Wire) |> (<) threshold // criterion: relative length
    visibleWireNetLength sheet.Wire w > 100.0 // criterion: absolute length
  )

  // visibleWireNetsLength sheet.Wire (sheet.Wire.Wires |> Helpers.mapValues |> Seq.head)
let beautifyD3 (sheet: SheetT.Model) =
  printf $"{getLongWires sheet 0.1}"