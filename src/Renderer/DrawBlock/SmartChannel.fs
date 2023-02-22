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


// Finds if two segments in 1D intersect
let overlap1D ((a1, a2): float * float) ((b1, b2): float * float): bool =
    let a_min, a_max = min a1 a2, max a1 a2
    let b_min, b_max = min b1 b2, max b1 b2
    a_max >= b_min && b_max >= a_min

// Finds if two boxes in 2D intersect
let overlap2D ((a1, a2): XYPos * XYPos) ((b1, b2): XYPos * XYPos): bool =
    (overlap1D (a1.X, a2.X) (b1.X, b2.X)) &&
    (overlap1D (a1.Y, a2.Y) (b1.Y, b2.Y))

// Retrieves XYPos of every vertex in a wire
let getWireSegmentsXY (wire: Wire) =
    
    let tupToXY (l: (float * float)): XYPos =
        {X = fst l; Y = snd l}

    segmentsToIssieVertices wire.Segments wire
    |> List.map (fun (x, y, _) -> (x, y))
    |> List.map tupToXY    

// Retrieves all wireId's which intersect an arbitrary bounding box
let getWiresInBox (box: BoundingBox) (model: Model): ConnectionId list =
    
    let wires = (List.ofSeq (Seq.cast model.Wires.Values))
    
    let listToTup (l: (float * float) list): XYPos * XYPos =
        {X = fst l[0]; Y = snd l[0]}, {X = fst l[1]; Y = snd l[1]}
    
    let is7Seg (wire: Wire): bool =
        wire.Segments.Length = 7
        
    let wireCoordList =
        List.filter is7Seg wires
        |> List.map (fun w -> getWireSegmentsXY w, w.WId)
        |> List.map (fun (posL, wid) -> (posL[3], posL[4]), wid)
    
    let bottomRight =
        { box.TopLeft with X = box.TopLeft.X + box.W; Y = box.TopLeft.Y + box.H }
    
    List.filter (fun x -> overlap2D (box.TopLeft, bottomRight) (fst x)) wireCoordList
    |> List.map snd


let extractWire (model: Model) (wId: ConnectionId): Wire = model.Wires[wId]

let smartChannelRoute 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (model: Model) 
            : Model =
                
    // channel.TopLeft is actually channel.TopRight
    let tl = {channel.TopLeft with X = channel.TopLeft.X - channel.W}
    let _channel = {channel with TopLeft = tl}

    let sortWires (wireIds: ConnectionId list) =
        
        let wire23Pos =
            List.map (fun wid -> wid, extractWire model wid) wireIds
            |> List.map (fun (wid, w) -> wid, getWireSegmentsXY w)
            |> List.map (fun (wid, l) -> wid, l[2..3])
        
        // let channelMid = tl.X + (channel.W / 2.0)
        
        // let separatedWires = wire23Pos |> List.partition (fun (_, pos) -> pos.X < channelMid)
        // printfn "Separated wires %A" separatedWires
        let sorted =
            wire23Pos
            |> List.sortByDescending (fun (_, pos) -> pos[0].Y) 
            // ( (snd separatedWires) |> List.sortByDescending (fun (_, pos) -> pos.Y) )
            |> List.map (fun (wid, pos) -> wid, pos[1].X)
        printfn "Sorted wires %A" sorted

        sorted
        // List.sortBy snd wire3rdXs
        // |> List.map (fun (id, x) -> printfn $"id: {id}, x: {x}")

    let moveWires (model: Model) (sortedWires: (ConnectionId * float) list) =
        
        let wireChannels =
            [_channel.TopLeft.X..
                (_channel.W / (float (sortedWires.Length+1)))
            .._channel.TopLeft.X+_channel.W][1..sortedWires.Length]
    
        // printfn "Old Pos: %A" (List.map snd sortedWires)
        printfn "Old Pos: %A" sortedWires
        printfn "New Pos: %A" wireChannels
        
        // In a 7 segment wire, seg index 2,3,4 are the ones which control the bend
        let adjustWire (model: Model) (newPos: float) ((wid, oldPos): ConnectionId * float) =
            // shorten or lengthen seg 2 & 4
            let currentSegments = (extractWire model wid).Segments
            let newWire =
                {
                    extractWire model wid with
                        Segments =
                            currentSegments[0..1] @
                            [{currentSegments[2] with Length = currentSegments[2].Length + (newPos - oldPos)}] @
                            [currentSegments[3]] @
                            [{currentSegments[4] with Length = currentSegments[4].Length - (newPos - oldPos)}] @ 
                            currentSegments[5..6]
                }
            
            { model with Wires = Map.add wid newWire model.Wires }

        (model, wireChannels, sortedWires) |||> List.fold2 adjustWire

    
    let sortedWires =
        getWiresInBox _channel model
        |> fun x -> printfn "Wires In Channel: %A" x; x
        |> sortWires
    
    printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"
    
    moveWires model sortedWires
    
