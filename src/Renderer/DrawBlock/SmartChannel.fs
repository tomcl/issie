module SmartChannel

open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.SheetT
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



//---------------------------------------------------------------------------------------------------------------------------------------------------------------
//                                                              SELECTING SEGMENTS INSIDE THE CHANNEL
//---------------------------------------------------------------------------------------------------------------------------------------------------------------
//Function which returns a list of the segments of a wire intersecting a bounding box, for all wires in the sheet
let selectSegmentsIntersectingBoundingBox (bounds: BoundingBox) (wires: List<Wire>) : List<List<Segment>> =
    let selectSegments (wire: Wire) =
        ([],wire) ||>
        foldOverSegs
            (fun startPos endPos state seg -> state @ [segmentIntersectsBoundingBox bounds startPos endPos])
        |> List.zip wire.Segments
        |> List.filter (fun element -> Option.isSome (snd element))
        |> List.map fst 
    wires |> List.map selectSegments



//---------------------------------------------------------------------------------------------------------------------------------------------------------------
//                                                              CATEGORISING THE WIRES INSIDE THE CHANNEL
//---------------------------------------------------------------------------------------------------------------------------------------------------------------

//These are the types the wires get broken down into:
//PassThrough : The wire crosses the channel with a straight single segment vertically 
//ZiggZagg cross: The wire crosses the channel sideways, while having a 'Z' shape
//Originates : The wire originitas from the channel
//Terminates : The wire terminates in the channel
//StraightCross: The wire crosses the channel sideways originating from and terminating in the channel, while being straight
//HarryPotter : The wire crosses the channel vertically while making a 'HarryPotter' shape
//LShape : The wire enters the channel sideways then takes a turn and leaves vertically
//TooDifficult: Any other wire types which are too difficult too route in a readable sense
type ChannelRelation = PassThrough | ZigZagCross | Originates | Terminates | StraightCross | HarryPotter | LShape | TooDifficult 

//Helper function to further categorise wire, which are fully contained within the channel
let process7Segment (seg: Segment) =
    match seg.Index with 
    | 3 -> if seg.Length = 0 then Some (seg, StraightCross) else Some (seg, ZigZagCross)

//Given the output of selectSegmentsIntersectingBoundingBox, it reduces the list in a list into a single List containing one segment per wire, and its categorisation
let categoriseWireSegments (model:Model ) (bounds: BoundingBox) (segList : List<List<Segment>>) =
    
    let mapOverWireSegment (segments : List<Segment>) =
        match segments.Length with
        | 0 -> None
        | 1 when segments[0].Index = 3 -> Some (segments[0],PassThrough)
        | 1 -> Some (segments[0], StraightCross)
        | 2 -> Some (getMiddleSegment model segments[0].WireId, LShape)
        | 3 -> if segments[1].Length <> 0 then Some (segments[0], HarryPotter) else Some (segments[0], StraightCross)
        | 4 when isWireConnectedOnLeft model bounds segments[0].WireId -> Some (getMiddleSegment model segments[0].WireId , Originates)
        | 4 -> Some(getMiddleSegment model segments[0].WireId, Terminates )
        | 5 -> Some (getMiddleSegment model segments[0].WireId ,ZigZagCross)
        | 7 -> getMiddleSegment model segments[0].WireId |> process7Segment
        | _ -> Some (getMiddleSegment model segments[0].WireId, TooDifficult)
    segList |> List.map mapOverWireSegment 
    |> List.filter Option.isSome
    |> List.map (fun option -> match option with Some value -> value)

//---------------------------------------------------------------------------------------------------------------------------------------------------------------
//                                                              SOURTING OUT THE ORDER OF THE WIRES INSIDE THE CHANNEL
//---------------------------------------------------------------------------------------------------------------------------------------------------------------

//Type to identify if the given wire is travelling upwards or downwards
type UpDown = Up | Down | Neither

//Helper function to categorise whether the segment travels up or down
let categoriseUpdown (model: Model) (seg : Segment) =
    let wire = model.Wires[seg.WireId]
    let starPortOrientation = Symbol.getOutputPortOrientation model.Symbol wire.OutputPort
    let endPortOrientation = Symbol.getInputPortOrientation model.Symbol wire.InputPort
    let initialPositon = match seg.Length with
                            | x when x > 0 -> Down
                            | x when x < 0 -> Up
                            | _ -> Neither
    match initialPositon, (starPortOrientation = Left || endPortOrientation = Right) with
    | initialPositon, false -> initialPositon
    | Down, true -> Up
    | Up, true -> Down
    | Neither, true -> Neither
//Function to sort out the wires within any category based on travelling up or down
let sortUpDown (model : Model) (orientation : Orientation) (segs : List<Segment*(XYPos*XYPos)>) :  List<List<Segment*(XYPos*XYPos)>>=
    let categorisedList = 
        segs 
        |> List.groupBy (fun element -> categoriseUpdown model (fst element))
    let upLst = 
        categorisedList 
        |> List.filter (fun element -> fst element = Up)
        |> List.collect snd
        |> List.sortBy (fun element -> if orientation = Vertical then (snd (snd element)).Y else (fst (snd element)).X )
        |> List.groupBy (fun (seg, posTuple) -> model.Wires[seg.WireId].OutputPort)
        |> List.map snd
    let downList =
        categorisedList 
        |> List.filter (fun element -> fst element = Down)
        |> List.collect snd
        |> List.sortByDescending (fun element -> if orientation = Vertical then (snd (snd element)).Y else (snd (snd element)).X )
        |> List.groupBy (fun (seg, posTuple) -> model.Wires[seg.WireId].OutputPort)
        |> List.map snd
    downList @ upLst

//Specialised helper function to return the vertical segments of a HarryPotter wire
let createHarryPair (model : Model) (segment : Segment, posTuple) =
    match segment.Length with 
    | x when  x >= 0.0 -> ((getSegmentFromId model (segment.Index - 1, segment.WireId),
                            getAbsoluteSegmentPos model.Wires[segment.WireId] (segment.Index - 1)),

                            (getSegmentFromId model (segment.Index + 1, segment.WireId),
                            getAbsoluteSegmentPos model.Wires[segment.WireId] (segment.Index + 1)))

    | x when x < 0.0 ->  ((getSegmentFromId model (segment.Index + 1, segment.WireId),
                            getAbsoluteSegmentPos model.Wires[segment.WireId] (segment.Index + 1)),

                            (getSegmentFromId model (segment.Index - 1, segment.WireId), 
                            getAbsoluteSegmentPos model.Wires[segment.WireId] (segment.Index - 1)))


//Function which sorts out the orderin of the wires within a channel, returns the list of segments in the order which they have to be shifted
let generateChannelOrder (orientation : Orientation) (model : Model) (lst: List<Segment*ChannelRelation>) =
    let segList =
        lst
        |> List.map (fun (segment, orient) -> (segment, getAbsoluteSegmentPos model.Wires[segment.WireId] segment.Index), orient)
    let leftHarry, rightHarry =
        segList
        |> List.filter (fun element -> snd element = HarryPotter)
        |> List.map fst
        |> List.map (createHarryPair model)
        |> List.unzip
    let ZiggZaggList =
        segList
        |> List.filter (fun element -> snd element = ZigZagCross)
        |> List.map fst
        
    let PassThroughList = 
        segList 
        |> List.filter (fun element -> snd element = PassThrough) 
        |> List.map fst
        |> sortUpDown model orientation
    let OriginateList = 
        segList 
        |> List.filter (fun element -> snd element = Originates) 
        |> List.map fst
        |> sortUpDown model orientation
    let TerminatesList = 
        segList 
        |> List.filter (fun element -> snd element = Terminates) 
        |> List.map fst
       
    let LList =
        segList 
        |> List.filter (fun element -> snd element = LShape) 
        |> List.map fst
        |> sortUpDown model orientation
    let TooDifficult = lst |> List.filter (fun element -> snd element = TooDifficult) |> List.map fst
    (leftHarry|> sortUpDown model orientation) @ OriginateList @ PassThroughList  @ LList @ (ZiggZaggList  @ TerminatesList |> sortUpDown model orientation) @ (rightHarry|> sortUpDown model orientation), TooDifficult
    

//---------------------------------------------------------------------------------------------------------------------------------------------------------------
//                                                          SHIFTING THE WIRES TO THEIR CORRESPONDING POSITION IN THE CHANNEL
//---------------------------------------------------------------------------------------------------------------------------------------------------------------

//Returns a bool whether the wire is shifted to minimal possible position
let isWireShiftedToMinDistance (wire : Wire) (index : int) =
    let prevSeg = wire.Segments[index - 1]
    let nextSeg = wire.Segments[index + 1]
    match (abs prevSeg.Length < Constants.nubLength + 2.0, abs nextSeg.Length < Constants.nubLength + 2.0) with
    | false, false -> false
    | _ -> true

//Filters the wires which could not be shifted to the desired location
let filterShiftedWires (lst : List<Wire* Segment>) =
    let checkedList =
        lst
        |> List.groupBy (fun (wire, seg) -> isWireShiftedToMinDistance wire seg.Index)
    let safeList = 
        checkedList
        |> List.filter (fun element -> fst element = false)
        |> List.collect snd 
        |> List.map fst
    let unsafeList = 
        checkedList
        |> List.filter (fun element -> fst element = true)
        |> List.collect snd 
        |> List.map fst
    unsafeList, safeList

//Shifts wires to the correct position
let createShiftedWires (orientation : Orientation) (model : Model) (bounds: BoundingBox) (segList : List<List<Segment*(XYPos*XYPos)>>)  = 
    let innerSegFolder (increment : float ) (lst) (seg, (startPos:XYPos, endPos)) = 
        match orientation with
        | Vertical -> lst@[moveSegment model seg -(startPos.X - (bounds.TopLeft.X - bounds.W + increment *(bounds.W / float (segList.Length + 1)))) , seg]
        | Horizontal -> lst@[moveSegment model seg -(startPos.Y - (bounds.TopLeft.Y + increment *(bounds.H / float (segList.Length + 1)))), seg]
    let outerSegFolder (stateLst, counter) (sgLst) =
        let updatedList = (stateLst, sgLst) ||> List.fold (innerSegFolder counter)
        updatedList, counter + 1.0 
    (([], 1.0), segList)||> List.fold outerSegFolder 
    |> fst 
     



/// <summary>HLP 23: AUTHOR Klapper - Routes the wires in a nice readable way in a channel, replaces wires with labels if it fails to route them in a neet way</summary>
/// <param name="channelOrientation">Orientation of the channel</param>
/// <param name="channel">Boundingbox of the channel (Note: instead of TopLeft it uses TopRight)</param>
/// <param name="model">BusWireT.Model</param>
/// <returns>The modified BusWireT.Model</returns>
let rec smartChannelRoute 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (model: DrawModelType.BusWireT.Model) 
            : DrawModelType.BusWireT.Model =

    let tl = channel.TopLeft
    printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"
    
    let correctBounds = {channel with TopLeft = {channel.TopLeft with X = channel.TopLeft.X - channel.W}}
    
    let sortedWires, diffictulWires = 
        model 
        |> getWireList 
        |> List.filter (fun element -> not(isWireConnectedToLabel model element))
        |> selectSegmentsIntersectingBoundingBox correctBounds
        |> categoriseWireSegments model correctBounds
        |> pipePrint
        |> generateChannelOrder channelOrientation model 
    let moreDiffictultWires ,wireListToModify =
        sortedWires
        |> createShiftedWires channelOrientation model channel
        |> filterShiftedWires
    
    let updatedWireModel : DrawModelType.BusWireT.Model = 
        wireListToModify
        |> updateModelWires model 
    let finalModel = ((1,updatedWireModel),moreDiffictultWires |> List.append (diffictulWires |> List.map (fun element -> model.Wires[element.WireId])))
                    ||> List.fold replaceWireWithLabel
                    |> snd
    match moreDiffictultWires.Length with
    | 0 -> finalModel
    | _ -> smartChannelRoute channelOrientation channel finalModel
    
    


    
    
