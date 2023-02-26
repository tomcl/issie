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
let smartChannelRoute 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (model:Model) 
            :Model =
    let tl = channel.TopLeft
    //printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"

    printfn "p"   
    //must return a model with an updated Wires:Map<ConnectionId,Wire>
    //determine which wires are inside the bounding box given?
    let wireInChannelPredicate ConnId wire :bool = 
        //is middle segment in bounding box?

        //let midSegPos = getAbsoluteSegmentPos wire 3
        //geta
        //let midSeg = getAbsSegments wire [3]
        //let segStartPos, segEndPos = fst midSegPos, snd midSegPos

        //match segmentIntersectsBoundingBox channel segStartPos segEndPos with
        //| None -> false
        //| Some x -> true
        match wireIntersectsBoundingBox wire channel with
        | None -> false
        | Some x -> true

    let shiftWire wire adj = 
            let a,b = wire.Segments[2].Length, wire.Segments[4].Length
            wire.Segments
            |> List.updateAt 2 {wire.Segments[2] with Length = a + adj}
            |> List.updateAt 4 {wire.Segments[4] with Length = b - adj}
            |> (fun x -> {wire with Segments = x})

    let channelWiresList = 
        Map.filter wireInChannelPredicate model.Wires
        |> Map.toList
        |> List.sortBy (fun (x,y) -> y)

    let wireSpacing =
        match channelOrientation with 
        | Vertical -> 0.8*channel.W/(float channelWiresList.Length)
        | Horizontal -> 0.8*channel.H/(float channelWiresList.Length)

    //At this point we have the wires, and the spacing
    //we need to shift the wires to the right positions now
    //do i need absolute wires, how do i shift?

    

    let spacedWirePos = 
        [1..channelWiresList.Length]
        |> List.map (fun x -> tl.X  + float(x) * wireSpacing)

    printfn $"{tl}"
        
    let shiftedWires = 
        channelWiresList
        |> List.map (fun (cid,wire) -> getAbsoluteSegmentPos wire 3) //gives absoulte pos of each mid wire segment
        //|>List.map (fun x -> printf $"{x}  ")
        |> List.map (fun x -> (fst x).X) //extract X coord of middle segment
        |> List.map2 (fun autoPosX absPos  -> autoPosX - absPos) spacedWirePos //list of adjustments
        //|> List.map (fun x -> printf $"{x}  ")
        |> List.map2 (fun (cid,wire) adj -> (cid, shiftWire wire adj)) channelWiresList
        |> Map.ofList
        


    //match channelOrientation with 
    //    | Vertical -> 
    //            let spacing = channel.W/(float channelWiresList.Length)
    //            ()
    //            //sort wires by
    //    | Horizontal -> 
    //            let spacing = channel.H/(float channelWires.Length)
    //            ()
    model
    {model with Wires = shiftedWires}
    