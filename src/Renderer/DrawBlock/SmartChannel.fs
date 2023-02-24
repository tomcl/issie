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
    
    // let listToTup (l: (float * float) list): XYPos * XYPos =
    //     {X = fst l[0]; Y = snd l[0]}, {X = fst l[1]; Y = snd l[1]}
    
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
    
    let tl = channel.TopLeft

    let sortWires (wireIds: ConnectionId list) =
        let wire23Pos =
            List.map (fun wid -> wid, extractWire model wid) wireIds
            |> List.map (fun (wid, w) -> wid, getWireSegmentsXY w)
            |> List.map (fun (wid, l) -> wid, l[2..3])
        
        let sorted (wire23Pos: (ConnectionId * XYPos list) list) =
            match channelOrientation with
            | Vertical ->
                wire23Pos
                |> List.sortByDescending (fun (_: ConnectionId, pos: XYPos list) -> pos[0].Y)
                |> List.map (fun (wid, pos) -> wid, pos[1].X)
            | Horizontal ->
                wire23Pos
                |> List.sortByDescending (fun (_: ConnectionId, pos: XYPos list) -> pos[0].X)
                |> List.map (fun (wid, pos) -> wid, pos[1].Y)
                
        sorted wire23Pos

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
            let currentWire = extractWire model wid
            let currentSegments = currentWire.Segments
            // printfn "Current Seg Len %A" currentSegments
            // printfn "New pos %A, Old Pos %A" newPos oldPos
            // printfn "Adjustment %A" (newPos - oldPos)
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
        getWiresInBox channel model
        |> sortWires
        // |> fun x -> printfn "Wires In Channel: %A" x; x
    
    printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"
    
    moveWires model sortedWires
    
