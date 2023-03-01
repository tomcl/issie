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

/// Automatically moves all 7-seg wires which intersect the channel
/// 1. Detects all 7-seg wires in the model which pass through the channel
/// 2. Sorts them in the ideal order so that wires are separated in the channel, if possible
/// 3. Moves wires so that they are equidistant to one another in the channel
let smartChannelRoute 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (model: Model) 
            : Model =
    
    let tl = channel.TopLeft
    let extractWire (wId: ConnectionId): Wire = model.Wires[wId]
    
    // Sorts all wire IDs so that all the wires through the channel will be separated if possible
    let sortWires7Seg (wires: Wire list) =
        // Get two XY positions: one at the start of the wire (after the nub and 0 len) and the first bend.
        let wire23Pos =
            List.map (fun wire -> wire.WId, wire) wires
            |> List.map (fun (wid, w) -> wid, getWireSegmentsXY w)
            |> List.map (fun (wid, l) -> wid, l[2..3]) 
        
        let sorted (wire23Pos: (ConnectionId * XYPos list) list) =
            match channelOrientation with
            | Vertical ->
                // Sort by the vertical positioning of the wire starts
                wire23Pos
                |> List.sortByDescending (fun (_: ConnectionId, pos: XYPos list) -> pos[0].Y)
                |> List.map (fun (wid, pos) -> wid, pos[1].X)
            | Horizontal ->
                // Sort by the horizontal positioning of the wire starts
                wire23Pos
                |> List.sortByDescending (fun (_: ConnectionId, pos: XYPos list) -> pos[0].X)
                |> List.map (fun (wid, pos) -> wid, pos[1].Y)
                
        sorted wire23Pos

    // Moves all the wire IDs which are passed in from their old position to the new position in the channel
    let moveWires (model: Model) (sortedWires: (ConnectionId * float) list) =
        let wireChannels =
            match channelOrientation with
            | Vertical ->
                [channel.TopLeft.X .. (channel.W / (float (sortedWires.Length+1)))
                .. channel.TopLeft.X + channel.W][1..sortedWires.Length]
            | Horizontal ->
                [channel.TopLeft.Y .. (channel.H / (float (sortedWires.Length+1)))
                .. channel.TopLeft.Y + channel.H][1..sortedWires.Length]
        
        printfn "New Channels: %A" wireChannels
        
        // In a 7 segment wire, seg index 2,3,4 are the ones which control the bend
        let adjustWire (model: Model) (newPos: float) ((wid, oldPos): ConnectionId * float) =
            // shorten or lengthen seg 2 & 4
            let currentWire = extractWire wid
            let currentSegments = currentWire.Segments
            let newWire =
                {
                    extractWire wid with
                        Segments =
                            currentSegments[0..1] @
                            [{currentSegments[2]  with Length = currentSegments[2].Length + (newPos - oldPos)}] @
                            [currentSegments[3]] @
                            [{currentSegments[4] with Length = currentSegments[4].Length - (newPos - oldPos)}] @ 
                            currentSegments[5..6]
                }
            { model with Wires = Map.add wid newWire model.Wires }
            
        (model, wireChannels, sortedWires) |||> List.fold2 adjustWire
    
    printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"
    
    let sorted7SegWires =
        getWiresInBox channel model
        |> List.filter (fun w -> w.Segments.Length = 7)
        |> sortWires7Seg
    
    sorted7SegWires
    |> moveWires model
    
