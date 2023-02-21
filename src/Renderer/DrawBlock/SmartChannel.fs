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
    (overlap1D (a1.X, a2.X) (b1.X, b2.X)) &&
    (overlap1D (a1.Y, a2.Y) (b1.Y, b2.Y))

let getWiresInChannel (channel: BoundingBox) (wires: Wire list): ConnectionId list =
    
    let listToTup (l: (float * float) list): XYPos * XYPos =
        {X = fst l[0]; Y = snd l[0]}, {X = fst l[1]; Y = snd l[1]}
    
    let is7Seg (wire: Wire): bool =
        wire.Segments.Length = 7
    
    let wireCoordList =
        List.filter is7Seg wires
        |> List.map (fun w -> (segmentsToIssieVertices w.Segments w)[3..4], w.WId)
        |> List.map (fun (verts, wid) ->
            List.map (fun (x, y, _) -> (x, y) ) verts |> listToTup, wid )
    
    
    
    let bottomRight =
        { channel.TopLeft with X = channel.TopLeft.X + channel.W; Y = channel.TopLeft.Y + channel.H }
    
    List.filter (fun x -> overlap2D (channel.TopLeft, bottomRight) (fst x)) wireCoordList
    |> List.map snd


let smartChannelRoute 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (model:Model) 
            :Model =
    // channel.TopLeft is actually channel.TopRight
    let tl = {channel.TopLeft with X = channel.TopLeft.X - channel.W}
    let _channel = {channel with TopLeft = tl}

    let getWires =
        getWiresInChannel _channel (List.ofSeq (Seq.cast model.Wires.Values))
        |> printfn "Wires In Channel: %A"
        
    printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"
    model
