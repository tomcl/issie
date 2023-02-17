module SmartChannel

open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers

open Optics
open Operators

(* 
    HLP23: This module will normally be used exclusively by team member doing the "smart channel route" 
    part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    Normally it will update multiple wires in the BusWire model so could use the SmartHelper function for
    this purpose.
*)


module Constants =
    let ChannelPolygon = { defaultPolygon with StrokeWidth = "1px"; Fill = "red"; FillOpacity = 0.2 }

///
/// HLP23: suggested initial smartChannel top-level function
/// to be tested, it must be given a channel in through which to route wires nicely
/// Normally the channel will come from symbol edges.
/// The function must identify all wires with segments going through the channel and space them
/// This function routes the middle segment of all 7 segment wires by moving them perpendicular to its direction.
/// It is expected that all wires provided are 7 segment with middle segment in the same direction
/// wires not so will be ignored.
/// The messiness of when to call it, what to do with different types of wire, which wires to call it with
/// could be left till later.
/// For this simple routing only one dimension of the channel is needed (determined by orientation).
/// The Wires going through the channel must be returned as an updated Wires map in model.
// HLP23: need to update XML doc comments when this function is fully worked out.

let overlap1D ((a1, a2): float * float) ((b1, b2): float * float): bool =
    let a_min, a_max = min a1 a2, max a1 a2
    let b_min, b_max = min b1 b2, max b1 b2
    a_max >= b_min && b_max >= a_min

let overlap2D ((a1, a2): XYPos * XYPos) ((b1, b2): XYPos * XYPos): bool =
    overlap1D (a1.X, a2.X) (b1.X, b2.X) &&
    overlap1D (a1.Y, a2.Y) (b1.Y, b2.Y)

let intersectChannel (orien: Orientation) (topLeft: XYPos) (len: float) (wirel: Wire list) =

    let border = topLeft, {topLeft with X = topLeft.X + len}
    let coordList =
        List.map (fun w -> (segmentsToIssieVertices w.Segments w)[3..4]) wirel
        |> List.map ( List.map (fun (x, y, _) -> (x, y) ) )
        |> List.map ( fun [x;y] -> (x, y) )
    List.filter (overlap2D border) coordList

    match orien with
    | Vertical -> 
        let line =
            (segmentsToIssieVertices wire.Segments wire)[3..4]
            |> List.map (fun (x,y,man) -> x,y)
        let cross = fst line[0] 
        []
    
    | Horizontal -> []

let smartChannelRoute 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (model:Model) 
            :Model =
    let tl = channel.TopLeft
    let len = 
        match channelOrientation with
        | Vertical -> channel.W
        | Horizontal -> channel.H

    // identify all wires going through channel
    let getWires =
        List.map (intersectChannel Vertical tl len) (List.ofSeq (Seq.cast model.Wires.Values))
    printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"
    model
