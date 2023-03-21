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

/// WireAdjustInfo contains all of the data essential for each wire for the smartChannelRoute algorithm
type WireAdjustInfo = {
    WId: ConnectionId
    AdjustIndex: int
    OldPosition: float
    POI: XYPos list
    Net: OutputPortId
}

/// This type contains all of the data required to do the smartChannelRoute algorithm
type SmartWireChannel = {
    Wires: WireAdjustInfo list
    NumberChannels: int
    ChannelToNetMap: Map<OutputPortId, int>
}

/// Automatically moves all wires which intersect the channel
/// 1. Detects all wires in the model which pass through the channel
/// 2. Sorts them in the ideal order so that wires are separated in the channel, if possible
/// 3. Moves wires so that they are equidistant to one another in the channel (groups wires from the same net)
let smartChannelRoute 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (model: Model) 
            : Model =
    
    let extractWire (wId: ConnectionId): Wire = model.Wires[wId]
    
    /// Takes a list of wire infos and returns a SmartWireChannel object with all
    /// of the fields populated.
    let getChannelDescriptor (wireInfos: WireAdjustInfo list): SmartWireChannel =
        let groupNets = List.groupBy (fun x -> x.Net) wireInfos
        
        let assignedWireList =
            groupNets
            |> List.collect (fun (netId, wireInfoList) ->
                wireInfoList |> List.map (fun wireInfo -> { wireInfo with Net = netId }))

        let assignedOldPosition =
            match channelOrientation with
            | Vertical ->
                assignedWireList
                |> List.sortByDescending (fun w -> w.POI[0].Y)
                |> List.map (fun w -> { w with OldPosition = w.POI[1].X })
            | Horizontal ->
                assignedWireList
                |> List.sortBy (fun w -> w.POI[0].Y)
                |> List.map (fun w -> { w with OldPosition = w.POI[1].Y })
                
        let channelIndexToNetMap =
            let folder (ind, m) (ele: WireAdjustInfo) =
                if Map.containsKey ele.Net m then
                    ind, m
                else
                    ind + 1, Map.add ele.Net ind m
                
            ((0, Map.empty), assignedOldPosition) ||> List.fold folder
            |> snd
        
        { Wires = assignedOldPosition; NumberChannels = groupNets.Length; ChannelToNetMap = channelIndexToNetMap }

    /// Takes the current model and modifies it based on the contents of the SmartWireChannel type
    let moveWires (model: Model) (channelDescriptor: SmartWireChannel) =
        let wireChannels =
            match channelOrientation with
            | Vertical ->
                [channel.TopLeft.X .. (channel.W / (float (channelDescriptor.NumberChannels+1)))
                .. channel.TopLeft.X + channel.W][1..channelDescriptor.NumberChannels]
            | Horizontal ->
                [channel.TopLeft.Y .. (channel.H / (float (channelDescriptor.NumberChannels+1)))
                .. channel.TopLeft.Y + channel.H][1..channelDescriptor.NumberChannels]
    
        printfn "New Channels: %A" wireChannels
        
        let adjustSingleWire (model: Model) (wireInfo: WireAdjustInfo) =
            let currentWire = extractWire wireInfo.WId
            let net = currentWire.OutputPort
            let segments = currentWire.Segments
            let newPos = wireChannels[Map.find net channelDescriptor.ChannelToNetMap]
            let newWire =
                {
                    currentWire with
                        Segments =
                            segments
                            |> List.updateAt (wireInfo.AdjustIndex-1)
                                   {segments[wireInfo.AdjustIndex-1] with Length = segments[wireInfo.AdjustIndex-1].Length + (newPos - wireInfo.OldPosition)}
                            |> List.updateAt (wireInfo.AdjustIndex+1)
                                   {segments[wireInfo.AdjustIndex+1] with Length = segments[wireInfo.AdjustIndex+1].Length - (newPos - wireInfo.OldPosition)}
                }
                
            { model with Wires = Map.add wireInfo.WId newWire model.Wires }
    
        (model, channelDescriptor.Wires) ||> List.fold adjustSingleWire

    // Take the generic function which gets wires in a box and map it to WireAdjustInfo type
    // so that it can be used in the smart channel pipeline
    getWiresInBox channel model
    |> List.map (fun (wire, index) -> {
        WId = wire.WId
        AdjustIndex = index
        OldPosition = 0.0
        Net = wire.OutputPort
        POI = (getWireSegmentsXY wire)[index-1..index]
    })
    |> getChannelDescriptor
    |> moveWires model
