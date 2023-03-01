module SmartChannel
open BusWireUpdate
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

// Authored by Harshil Shah
let smartChannelRoute 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (model:Model) 
            :Model =
    let tl = channel.TopLeft
    printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"
  

    let wireInChannelPredicate (connId,wire) :bool = 
        //7 seg wires allowed in horizontal channel ONLY
        //9 seg wires allowed in vertical channel ONLY
        if not (List.length wire.Segments = 7 && channelOrientation = Vertical 
                || List.length wire.Segments = 9 && channelOrientation = Horizontal ) then 
            false
        else
            match wireIntersectsBoundingBox wire channel with
            | None -> false
            | Some x -> true

    let allWiresList = 
        model.Wires
        |> Map.toList
        |> List.partition wireInChannelPredicate 

    //List of wires in channel sorted by X coord of starting pos
    let channelWiresList = 
        fst allWiresList
        |> List.sortBy (fun (x,y) -> y.StartPos.X)

    //Calculates current deviation of each wire's mid segment from desired position, then calls moveSegment to correct it
    let shiftedWiresList =

        match channelOrientation with 
        | Vertical -> 
            //Setup desired wire positions
            let wireSpacing = 0.8*channel.W/(float channelWiresList.Length)
            let spacedWirePos = 
                [1..channelWiresList.Length]
                |> List.map (fun i -> tl.X  + float(i) * wireSpacing)


            channelWiresList
            |> List.map (fun (cid,wire) -> getAbsoluteSegmentPos wire 3)
            |> List.map (fun x -> (fst x).X)
            |> List.map2 (fun autoPosX absPos  -> autoPosX - absPos) spacedWirePos //list of adjustments
            |> List.map2 (fun (cid,wire) adj -> (cid, moveSegment model wire.Segments[3] adj)) channelWiresList

        | Horizontal -> 
            let wireSpacing = 0.8*channel.H/(float channelWiresList.Length)
            let spacedWirePos = 
                [1..channelWiresList.Length]
                |> List.map (fun i -> tl.Y  + float(i) * wireSpacing)

            channelWiresList
            |> List.map (fun (cid,wire) -> getAbsoluteSegmentPos wire 4)
            |> List.map (fun x -> (fst x).Y)
            |> List.map2 (fun autoPosY absPos  -> autoPosY - absPos) spacedWirePos //list of adjustments
            |> List.map2 (fun (cid,wire) adj -> (cid, moveSegment model wire.Segments[4] adj)) channelWiresList

    let allWiresMap = 
        shiftedWiresList @ (snd allWiresList) 
        |> Map.ofList

    {model with Wires = allWiresMap}
    