module SmartChannel

open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers
open SmartHelpers
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

let pipePrint x = 
    printfn "%A" x
    x






let replaceSegments (model : Model) (bounds : BoundingBox) (orientation : Orientation) (wireList : List<List<SegmentId * XYPos>>): Model =
    match orientation with
    | Vertical ->   let sortedLst = wireList |> List.map (fun x -> List.head x) |> List.sortBy (fun x -> (snd x).Y) 
                    let folder (lst : List<Wire>, counter: int) (next : SegmentId * XYPos) : List<Wire>*int = 
                        segmentShifterHelper model (fst next) (bounds.TopLeft.X - bounds.W + (bounds.W / float(wireList.Length + 1) * float(counter)) - (snd next).X)
                        |> function
                        | Some wire -> (lst @ [wire]), (counter + 1) 
                        | None -> lst, (counter + 1)
                    (([], 1), sortedLst) ||> List.fold folder
                    |> fst 
                    |> updateModelWires model  
    | Horizontal -> model
                        
    
    
                

let segmentsInBounds (bounds: BoundingBox) (model:Model) : Option<List<List<SegmentId * XYPos>>> =
    let isInBound (state:List<List<SegmentId * XYPos>>) (key:ConnectionId)(wire : Wire) =
        let cornerFolder ((pos : XYPos , pseg : Segment), orientation: Orientation) (seg : Segment) =
            if orientation = Horizontal then (({pos with X = pos.X + seg.Length}, seg), Vertical)
                                      else (({pos with Y = pos.Y + seg.Length}, seg), Horizontal)
        let corners: ((XYPos * Segment) * Orientation) list = 
            (((wire.StartPos, List.head wire.Segments),wire.InitialOrientation),wire.Segments) ||> List.scan cornerFolder
            |> List.tail 
        let folder (state : List<SegmentId * XYPos>) ((pos: XYPos, seg:Segment) , orientation:Orientation) =
            match orientation with 
            | Horizontal -> match (bounds.TopLeft.X > pos.X, bounds.TopLeft.X - bounds.W < pos.X, (pos.Y < bounds.TopLeft.Y) <> (pos.Y - seg.Length < bounds.TopLeft.Y)) with
                            | true,true,true -> state @ [(seg.Index, seg.WireId), pos]
                            | _ -> state
            | Vertical -> match (bounds.TopLeft.Y < pos.Y, bounds.TopLeft.Y + bounds.H > pos.Y, (pos.X < bounds.TopLeft.X) <> (pos.X - seg.Length < bounds.TopLeft.X)) with
                            | true,true,true -> state @ [(seg.Index, seg.WireId), pos]
                            | _ -> state
        let lst = ([], corners) ||> List.fold folder 
        match lst.Length with 
        | 0 -> state
        | _ -> lst :: state
    let ret = ([],model.Wires) ||> Map.fold isInBound
    if ret.Length = 0 then None
                      else Some ret
    
        
                          
        
        
        



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
    printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"
    match segmentsInBounds channel model with
        | Some (channelSeg: List<List<SegmentId * XYPos>>) -> channelSeg |> replaceSegments model channel channelOrientation
        | None -> model
