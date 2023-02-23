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


 //getAbsoluteSegmentPos   
let selectSegmentsIntersectingBoundingBox (bounds: BoundingBox) (wires: List<Wire>) : List<List<Segment>> =
    let selectSegments (wire: Wire) =
        ([],wire) ||>
        foldOverSegs
            (fun startPos endPos state seg -> state @ [segmentIntersectsBoundingBox bounds startPos endPos])
        |> List.zip wire.Segments
        |> List.filter (fun element -> Option.isSome (snd element))
        |> List.map fst 
    wires |> List.map selectSegments


type ChannelRelation = PassThrough | Originates | Terminates | LivesIn

let categoriseWireSegments (segList : List<List<Segment>>) =
    let mapOverWireSegment (segments : List<Segment>) =
        match segments.Length with
        | 1 -> segments[0],PassThrough
        | 4 when segments[0].Index = 0 -> segments[3], Originates
        | 4 -> segments[0], Terminates
        | 7 -> segments[0],LivesIn
    segList |> List.map mapOverWireSegment

let generateChannelOrder (orientation : Orientation) (model : Model) (lst: List<Segment*ChannelRelation>) =
    let segList =
        lst
        |> List.map (fun (segment, orient) -> (segment, getAbsoluteSegmentPos model.Wires[segment.WireId] segment.Index), orient)
    let PassThroughList = 
        segList 
        |> List.filter (fun element -> snd element = PassThrough) 
        |> List.map fst
        |> List.sortBy (fun element -> if orientation = Vertical then (snd (snd element)).Y else (snd (snd element)).X )
    let OriginateList = segList |> List.filter (fun element -> snd element = Originates) |> List.map fst
    let TerminatesList = segList |> List.filter (fun element -> snd element = Terminates) |> List.map fst
    OriginateList @ PassThroughList @ TerminatesList
    

let createShiftedWires (orientation : Orientation) (model : Model) (bounds: BoundingBox) (segList : List<Segment*(XYPos*XYPos)>)  = 
    let converterFolder (lst ,counter) (seg, (startPos:XYPos, endPos)) = 
        match orientation with
        | Vertical -> lst@[moveSegment model seg -(startPos.X - (bounds.TopLeft.X - bounds.W + counter *(bounds.W / float (segList.Length + 1))))], counter + 1.0
        | Horizontal -> lst@[moveSegment model seg -(startPos.Y - (bounds.TopLeft.Y + counter *(bounds.H / float (segList.Length + 1))))], counter + 1.0
    (([], 1.0), segList)||> List.fold converterFolder 
    |> fst 
     

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
    let correctBounds = {channel with TopLeft = {channel.TopLeft with X = channel.TopLeft.X - channel.W}}
    printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"
    model |> getWireList 
    |> selectSegmentsIntersectingBoundingBox correctBounds 
    |> categoriseWireSegments
    |> generateChannelOrder channelOrientation model 
    |> createShiftedWires channelOrientation model channel
    |> updateModelWires model 
    
    
