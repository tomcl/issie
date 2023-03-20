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

//---------------------------------------------------------------------------------------------------------------------------------------------------------------
//                                                                      HLP23: AUTHOR Klapper
//---------------------------------------------------------------------------------------------------------------------------------------------------------------


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
//NShape : The wire crosses the channel vertically while making a '-, shape
//LShape : The wire enters the channel sideways then takes a turn and leaves vertically
//TooDifficult: Any other wire types which are too difficult too route in a readable sense
type ChannelRelation = PassThrough | ZigZagCross | Originates | Terminates | StraightCross | NShape | LShape | TooDifficult 

//Helper function to further categorise wire, which are fully contained within the channel
let process7Segment (seg: Segment) =
    match seg.Index with 
    | 3 -> if seg.Length = 0 then Some (seg, StraightCross) else Some (seg, ZigZagCross)

//Given the output of selectSegmentsIntersectingBoundingBox, it reduces the list in a list into a single List containing one segment per wire, and its categorisation
let categoriseWireSegments (model:Model ) (bounds: BoundingBox) (orientation : Orientation) (segList : List<List<Segment>>)  =
    
    let mapOverWireSegment (segments : List<Segment>) =
        match segments.Length with
        | 0 -> None
        | 1 when segments[0].Index = 3 -> Some (segments[0],PassThrough)
        | 1 -> Some (segments[0], StraightCross)
        | 2 -> Some (getMiddleSegment model segments[0].WireId, LShape)
        | 3 -> if segments[1].Length <> 0 then Some (segments[0], NShape) else Some (segments[0], StraightCross)
        | 4 when isWireConnectedOnLeft model bounds segments[0].WireId orientation -> Some (getMiddleSegment model segments[0].WireId , Originates)
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
let categoriseUpdown (model: Model) (orientation) (seg : Segment) =
    match seg.Length with
    | x when x > 0 -> Down
    | x when x < 0 -> Up
    | _ -> Neither

//Function to sort out the wires within any category based on travelling up or down
let sortUpDown (model : Model) (orientation : Orientation) (segs : List<Segment*(XYPos*XYPos)>) :  List<List<Segment*(XYPos*XYPos)>> =
    let sorter ((seg : Segment, upDown: UpDown),(fstPost: XYPos, sndPos: XYPos)) =
        let wire = model.Wires[seg.WireId]
        let starPortOrientation = Symbol.getOutputPortOrientation model.Symbol wire.OutputPort
        let endPortOrientation = Symbol.getInputPortOrientation model.Symbol wire.InputPort
        match orientation with 
        | Vertical -> match upDown, (starPortOrientation = Left || endPortOrientation = Right) with
                        | Up,  true -> fstPost.Y 
                        | Up,  false -> -fstPost.Y 
                        | Down,  true-> -sndPos.Y
                        | Down,  false-> sndPos.Y
                        | _ -> fstPost.Y
        | Horizontal -> match upDown, (starPortOrientation = Bottom || endPortOrientation = Top) with
                        | Up, true -> -sndPos.X
                        | Up, false -> sndPos.X
                        | Down, true -> fstPost.X
                        | Down, false -> -fstPost.X
                        | _ -> fstPost.Y

    segs |> List.map (fun (x,y) -> (x, categoriseUpdown model orientation x), y)
    |> List.sortByDescending sorter
    |> List.map (fun (x,y) -> fst x,y)
    |> List.groupBy (fun (seg, posTuple) -> model.Wires[seg.WireId].OutputPort)
    |> List.map snd

    
//Specialised helper function to return the vertical segments of a NShape wire
let createNShapePair (model : Model) (segment : Segment, posTuple) =
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
    let folder inLst outLst _type =
        inLst 
        |> List.filter (fun element -> snd element = _type)
        |> List.map fst
        |> sortUpDown model orientation
        |> List.append outLst

    let segList =
        lst
        |> List.map (fun (segment, orient) -> (segment,
                                             getAbsoluteSegmentPos model.Wires[segment.WireId] segment.Index),
                                              orient)
    let leftNList, rightNList =
        segList
        |> List.filter (fun element -> snd element = NShape)
        |> List.map fst
        |> List.map (createNShapePair model)
        |> List.unzip
        |> (fun (a , b) -> sortUpDown model orientation a, sortUpDown model orientation b)

    let TooDifficult = lst |> List.filter (fun element -> snd element = TooDifficult) |> List.map fst
    
    rightNList
    |> List.append (([], [Originates; PassThrough; LShape; ZigZagCross; Terminates]) ||> List.fold (folder segList))
    |> List.append leftNList
    |> function
        | x -> x,TooDifficult
    

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
    let filterWires (safe:bool) (lt) = 
        lt
        |> List.filter (fun element -> fst element = (not <| safe))
        |> List.collect snd 
        |> List.map fst

    lst
    |> List.groupBy (fun (wire, seg) -> isWireShiftedToMinDistance wire seg.Index)
    |> function 
        | x -> filterWires false x, filterWires true x

//Shifts wires to the correct position
let createShiftedWires (orientation : Orientation) (model : Model) (bounds: BoundingBox) (segList : List<List<Segment*(XYPos*XYPos)>>)  = 
    let innerSegFolder (increment : float ) (lst) (seg, (startPos:XYPos, endPos)) = 
        match orientation with
        | Vertical -> lst@[moveSegment model seg -(startPos.X - (bounds.TopLeft.X + increment *(bounds.W / float (segList.Length + 1)))) , seg]
        | Horizontal -> lst@[moveSegment model seg -(startPos.Y - (bounds.TopLeft.Y + increment *(bounds.H / float (segList.Length + 1)))), seg]
    let outerSegFolder (stateLst, counter) (sgLst) =
        let updatedList = (stateLst, sgLst) ||> List.fold (innerSegFolder counter)
        updatedList, counter + 1.0 
    (([], 1.0), segList)||> List.fold outerSegFolder 
    |> fst 
     
//---------------------------------------------------------------------------------------------------------------------------------------------------------------
//                                                                           MAIN PIPELINE
//---------------------------------------------------------------------------------------------------------------------------------------------------------------


///HLP 23: AUTHOR Klapper - Routes the wires in a nice readable way in a channel, replaces wires with labels if it fails to route them in a neet way
let rec smartChannelRoute 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (fullModel: DrawModelType.SheetT.Model) 
            : DrawModelType.SheetT.Model =
    let model = fullModel.Wire
    let tl = channel.TopLeft
    printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"
    
    
    let sortedWires, diffictulWires = 
        model 
        |> getWireList
        |> List.filter (fun element -> (List.contains element.WId fullModel.SelectedWires) |> not)
        |> List.filter (fun element -> not(isWireConnectedToLabel model element))
        |> selectSegmentsIntersectingBoundingBox channel
        |> categoriseWireSegments model channel channelOrientation
        |> generateChannelOrder channelOrientation model 
    let moreDiffictultWires ,wireListToModify =
        sortedWires
        |> createShiftedWires channelOrientation model channel
        |> filterShiftedWires
    
    let updatedWireModel : DrawModelType.BusWireT.Model = 
        wireListToModify
        |> updateModelWires model 
    let wireToReplaceWithLabel = 
        diffictulWires
        |> List.map (fun element -> model.Wires[element.WireId])
        |> List.append moreDiffictultWires
        |> List.map (fun element -> element.WId)
    
    let finalModel = {fullModel with Wire = updatedWireModel; SelectedWires = fullModel.SelectedWires @ wireToReplaceWithLabel}
    match wireToReplaceWithLabel.Length with
    | 0 -> finalModel
    | _ -> smartChannelRoute channelOrientation channel finalModel
    
    


    
    
