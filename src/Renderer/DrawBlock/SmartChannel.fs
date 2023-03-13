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


/// TODO:
/// Dont move wires which have two segments in the channel

/// Automatically moves all wires which intersect the channel
/// 1. Detects all wires in the model which pass through the channel
/// 2. Sorts them in the ideal order so that wires are separated in the channel, if possible
/// 3. Moves wires so that they are equidistant to one another in the channel
let smartChannelRoute 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (model: Model) 
            : Model =
    
    let tl = channel.TopLeft
    let extractWire (wId: ConnectionId): Wire = model.Wires[wId]
    
    let sortWires (wires: (Wire * int) list) =
        // Get two XY positions: one at the first bend before the wire crosses the channel
        // as well as the one before that.
        let wirePos =
            List.map (fun (wire, index) ->
                (wire.WId, index), (getWireSegmentsXY wire)[index-1..index]) wires
    
        let sorted (wirePos: ((ConnectionId * int) * XYPos list) list) =
            match channelOrientation with
            | Vertical ->
                // Sort by the vertical positioning of the wire starts
                wirePos
                |> List.sortByDescending (fun (_, pos: XYPos list) -> pos[0].Y)
                |> List.map (fun ((wid, ind), pos) -> (wid, ind), pos[1].X)
            | Horizontal ->
                // Sort by the horizontal positioning of the wire starts
                wirePos
                |> List.sortBy (fun (_, pos: XYPos list) -> pos[0].Y)
                |> List.map (fun ((wid, ind), pos) -> (wid, ind), pos[1].Y)
                
        sorted wirePos

    // Moves all the wire IDs which are passed in from their old position to the new position in the channel
    let moveWires (model: Model) (sortedWires: ((ConnectionId * int) * float) list) =
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
        let adjustWire (model: Model) (newPos: float) (((wid, index), oldPos): (ConnectionId * int) * float) =
            // shorten or lengthen seg 2 & 4
            let currentWire = extractWire wid
            let currentSegments = currentWire.Segments
            let newWire =
                {
                    extractWire wid with
                        Segments =
                            currentSegments
                            |> List.updateAt (index-1)
                                   {currentSegments[index-1] with Length = currentSegments[index-1].Length + (newPos - oldPos)}
                            |> List.updateAt (index+1)
                                   {currentSegments[index+1] with Length = currentSegments[index+1].Length - (newPos - oldPos)}
                }
            { model with Wires = Map.add wid newWire model.Wires }
            
        (model, wireChannels, sortedWires) |||> List.fold2 adjustWire
    
    printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"
    
    getWiresInBox channel model
    |> sortWires
    |> moveWires model
    
