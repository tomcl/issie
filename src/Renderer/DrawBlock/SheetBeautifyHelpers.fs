

module SheetBeautifyHelpers

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team



open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open Optics
open Optic
open Symbol
open Helpers
open BusWireRoute
open BlockHelpers
open System

module constants=
    ()

//get a custom input symbol and returns the height and width of the symbol in a xypos format
//B1R
let readCustomSymbolSize (sym:Symbol)=
    // let symPos = get posOfSym_ sym
    let comp = sym.Component 
    let compBox = {X=comp.W; Y=comp.H}
    compBox
//returns a symbol with dimension of the input
//B1W
let writeCustomSymbolSize (HW:XYPos) (sym: Symbol)=
    let W,H= HW.X,HW.Y
    let newSym={sym with Component= {sym.Component with H=H; W=W}}
    newSym

let RWCustomSymbolSize=Lens.create (readCustomSymbolSize) (writeCustomSymbolSize)

//B4R
let readMux2ReverseState (sym:Symbol)=
    sym.ReversedInputPorts
//B4W
let writeMux2ReverseState(state:option<bool>)(sym:Symbol)=
    {sym with ReversedInputPorts = state}

let RWMux2ReverseState= Lens.create (readMux2ReverseState) (writeMux2ReverseState)

//B2W
let writeSymbolPosition (pos:XYPos) (symId:ComponentId) (sheet : SheetT.Model)=
    let boundingBoxes= sheet.BoundingBoxes
    let symBoundingBox=boundingBoxes[symId]
    let newsymBox={symBoundingBox with TopLeft =pos-{X=symBoundingBox.W/2.; Y=symBoundingBox.H/2.} }
    let newSymBoundingBoxes= Map.change symId (fun stringsOpt -> Some newsymBox) boundingBoxes
    {sheet with BoundingBoxes=newSymBoundingBoxes}

//B3R

let readPortOrientation (sym:Symbol) (edge:Edge)=
    let portmap=sym.PortMaps
    let order= portmap.Order

    match Map.tryFind edge order with
    | Some keys -> keys
    | None -> []
    
// B3W
let WritePortOrientation (newOrder:list<string>)  (edge:Edge) (sym:Symbol)=
    let portmap=sym.PortMaps
    let order= portmap.Order
    let newOrderMap=Map.change edge (fun stringsOpt -> Some newOrder) order
    let newportmap= {portmap with Order = newOrderMap}
    {sym with PortMaps = portmap}


//B5R
let readPortPos(sym:Symbol) (port : Port)=
    getPortPos sym port

//B6R
let readSymbolBoundingBox (sym:Symbol) =
    sym.SymbolBoundingBox

//B7R
let readSymbolRotationState (sym:Symbol) =
    sym.STransform.Rotation
//B7W
let writeSymbolRotationState (rotation:Rotation) (sym:Symbol) =
    {sym with STransform= { sym.STransform with Rotation=rotation}}
let RWSymbolRotationState=Lens.create  (readSymbolRotationState) (writeSymbolRotationState)

//B8R 
let readSymbolFlipState(sym:Symbol) =
    sym.STransform.Flipped
//B8W
let writeSymbolFlipState (flipped:bool) (sym:Symbol) =
    {sym with STransform= { sym.STransform with Flipped=flipped}}
let RWSymbolFlipState= Lens.create  (readSymbolFlipState) (writeSymbolFlipState)

//T1R

let ReadSymbolIntersectsSymbolPairs  (sheet: SheetT.Model) =
            
            let boxes =
                mapValues sheet.BoundingBoxes
                |> Array.toList
                |> List.mapi (fun n box -> n,box)
            List.allPairs boxes boxes 
            |> List.fold (fun num ((n1,box1),(n2,box2))  -> if ((n1 <> n2) && BlockHelpers.overlap2DBox box1 box2) then num+1 else num) 0

        

let visibleSegments (wId: ConnectionId) (model: SheetT.Model): XYPos list =

        let wire = model.Wire.Wires[wId] // get wire from model

        /// helper to match even and off integers in patterns (active pattern)
        let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

        /// Convert seg into its XY Vector (from start to end of segment).
        /// index must be the index of seg in its containing wire.
        let getSegmentVector (index:int) (seg: BusWireT.Segment) =
            // The implicit horizontal or vertical direction  of a segment is determined by 
            // its index in the list of wire segments and the wire initial direction
            match index, wire.InitialOrientation with
            | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> {X=0.; Y=seg.Length}
            | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> {X=seg.Length; Y=0.}

        /// Return a list of segment vectors with 3 vectors coalesced into one visible equivalent
        /// if this is possible, otherwise return segVecs unchanged.
        /// Index must be in range >= 1
        let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int)  =
            if index < segVecs.Length - 1 && segVecs[index] =~ XYPos.zero
            then
                segVecs[0..index-2] @
                [segVecs[index-1] + segVecs[index+1]] @
                segVecs[index+2..segVecs.Length - 1]
            else
                segVecs

        wire.Segments
        |> List.mapi getSegmentVector
        |> (fun segVecs ->
                (segVecs,[1..segVecs.Length-2])
                ||> List.fold tryCoalesceAboutIndex)


// let failOnWireIntersectsSymbol (sample: int) (sheet: SheetT.Model) =
//             let wireModel = sheet.Wire
//             wireModel.Wires
//             |> Map.exists (fun _ wire -> BusWireRoute.findWireSymbolIntersections wireModel wire <> [])
//             |> (function | true -> Some $"Wire intersects a symbol outline in Sample {sample}"
//                          | false -> None)
let ReadSegmentIntersectSymbolNum ( sheet: SheetT.Model)=
    let wires= sheet.Wire.Wires
    // let keysList= wires |> Map.toList  |> List.map fst
    let wiresList= wires |> Map.toList  |> List.map snd
    let absSegments=List.collect id (List.map (fun wire ->getNonZeroAbsSegments wire) wiresList )
    let boundingBoxes= sheet.BoundingBoxes |> Map.toList  |> List.map snd

    List.allPairs absSegments boundingBoxes
    |> List.map (fun  (segment, boundingbox)-> segmentIntersectsBoundingBox boundingbox segment.Start segment.End)
    |> List.fold (fun num (intersectRes:option<float>) ->
        match intersectRes with
        | Some _ -> num+1
        | None->num) 0
    // List.map ( fun key-> visibleSegments key sheet) keysList##

//T3R

let readSegmentRightIntersectionPair(sheet: SheetT.Model)=
    let wires= sheet.Wire.Wires
    // let keysList= wires |> Map.toList  |> List.map fst
    let wiresList= wires |> Map.toList  |> List.map snd
    let absSegments= (List.map (fun wire ->getNonZeroAbsSegments wire) wiresList )
    let half num=num/2
    let isZero (num:float)=
        match num with
        | 0.0 -> true
        |_ -> false
    let checkSegmentRightAngleIntersect (segmentA:ASegment) (segmentB:ASegment)=
        let segmentAVector:XYPos= segmentA.Start-segmentA.End
        let segmentBVector:XYPos= segmentB.Start-segmentB.End
        (segmentA <> segmentB) && (overlap2D (segmentA.Start,segmentA.End)(segmentB.Start,segmentB.End)) && (isZero (dotProduct segmentAVector segmentBVector))
        // they intersect right angle if dot product is 0, and they overlap. 
    List.allPairs absSegments absSegments
    |> List.map (fun (segmentListA, segmentListB) -> List.allPairs segmentListA segmentListB) //pair wires with each other
    |> List.collect id 
    |> List.map (fun (segmentA, segmentB)-> checkSegmentRightAngleIntersect segmentA segmentB)
    |> List.fold ( fun num bool -> 
    match bool with
    |true -> num+1
    |_ -> num ) 0
    |> half // checked twice because same list pair with themselves
    

type WirePosDict =
    {
        Horizontal:Map<float,List<float*float>>
        Vertical:Map<float,List<float*float>>
    } //the intermediate data store to store the wire positions
//T4R

let readTotalWireLengthsheet (sheet : SheetT.Model)=
    
   
    let wirePosDict = {
        Horizontal = Map.empty
        Vertical = Map.empty  
    }
    //make Asegment datatype into the following format: (orientation, xy<pos> where x= start.)


   
    let updateOrAddEntries (key:float) (segStartCord:float) (segEndcord:float) (map:Map<float,(float*float) list>):Map<float,(float*float) list> =
        let hasKey= map.ContainsKey key
        let largecord= max segEndcord segStartCord
        let smallcord= min segEndcord segStartCord
        
        if hasKey then 
            let lis= map[key]
            let containedEntry=List.filter (fun (a, b) -> ((b >= segStartCord) && segStartCord >= b) || (b >= segEndcord && segEndcord >= a)) lis

            if List.isEmpty containedEntry  then Map.add key ((largecord,smallcord)::lis) map
            else 
                let fsts= segStartCord::(containedEntry |> List.map fst)
                let snds= segEndcord::(containedEntry |> List.map snd)
                let newRange = (List.min fsts, List.max snds) 
                Map.add key (newRange::(lis |> List.except containedEntry)) map
           

        else
            Map.add key [(smallcord,largecord)] map

        // this function addes entries to wireposdict, if there is an overlap, update the xypos to the corrected overlapped length
        //if no overlap, just adds the segment to the wireposdict

    let makeWirePosDict (lis:List<ASegment>)=
        lis
        |> List.fold (fun (wirePosDict:WirePosDict) (aSegment)-> 
        match aSegment.Orientation with
        | Horizontal ->{ wirePosDict with Horizontal= updateOrAddEntries aSegment.Start.Y aSegment.Start.X aSegment.End.X wirePosDict.Horizontal}
        | Vertical -> { wirePosDict with Vertical= updateOrAddEntries aSegment.Start.X aSegment.Start.Y aSegment.End.Y wirePosDict.Vertical}) wirePosDict

    let calcLenFromWirePosDict (wirePosDict:WirePosDict)=
        let verti= wirePosDict.Vertical
        let hori= wirePosDict.Horizontal
        let calcLen (map: Map<float,List<float*float>>)= 
            map |> Map.toList |>  List.map snd |> List.collect id
            |> List.fold (fun num (startv,endv) -> num+ (endv-startv)) 0.
        calcLen verti + calcLen hori
        //calculates the length of the wires from wireposdict


    let wires= sheet.Wire.Wires
    // let keysList= wires |> Map.toList  |> List.map fst
    let wiresList= wires |> Map.toList  |> List.map snd

    let samenetWiresEmpty: Map<Port ,Wire list>= Map.empty

    let samenetWires= List.fold (fun (samenetWires:Map<Port ,Wire list>) (wire:Wire)-> 
        let wireport= getSourcePort sheet.Wire wire
        if (samenetWires.ContainsKey wireport)
        then
            let lis=samenetWires[wireport]
            Map.add wireport (wire::lis) samenetWires
        else Map.add wireport [wire] samenetWires ) samenetWiresEmpty wiresList
    
    let absSegmentsListListList= samenetWires |> Map.toList |> List.map snd |> List.map (fun wireList-> List.map (fun wire ->getNonZeroAbsSegments wire) wireList)
    // list of Asegment lists from lists of wires with the same port

    let absSegments= List.collect id absSegmentsListListList

    absSegments
    |> List.map makeWirePosDict
    |> List.fold (fun num wireposdict ->num+(calcLenFromWirePosDict wireposdict)) 0.0




  



//T5R
let readVisibleRightAngleNum (sheet:SheetT.Model)=
    let wires= sheet.Wire.Wires
    let IdList= wires |> Map.toList  |> List.map fst

    let visibleSegmentsList:list<list<XYPos>> = List.map (fun cId-> visibleSegments cId sheet) IdList
    visibleSegmentsList 
    |> List.fold (fun num segmentList-> num+ (List.length segmentList)-1) 0


//T6R


let floatEqual (x:float) y=
    let epsilon = 1e-6
    Math.Abs(x - y) < epsilon
let readRetracedWire(sheet:SheetT.Model)=
    let wiresMap= sheet.Wire.Wires
    // let w= sheet.Wire
    let symbols=wiresMap|> Map.toList |>List.map snd |> List.map(fun (wire:Wire)->getSourceSymbol sheet.Wire wire)
    let asegmentsList= wiresMap|> Map.toList |>List.map snd |> List.map(fun (wire:Wire)->getAbsSegments wire)
    let symbolsAndasegments= List.zip symbols asegmentsList
    // let emptyRetraceList:Segment list= [] 

    let differentSigns x y = x * y < 0.0



    
    let segmentsRetraceInWire (symbolAseg:(Symbol*List<ASegment>))= // this function finds all retracing segments in the wire
        let asegments= snd symbolAseg
        let segments= List.map (fun asegment -> asegment.Segment) asegments
        
        let symbolBoundingBox= getSymbolBoundingBox (fst symbolAseg )
        let symbolXYPosCords= symbolBoundingBox.TopLeft, symbolBoundingBox.BottomRight()
        let startPos=asegments[0].Start
        let calcEndPos (startPos:XYPos) (asegment:ASegment) :XYPos=
            match asegment.Orientation with
            | Vertical -> {X=startPos.X ;Y= startPos.Y+asegment.Segment.Length}
            | Horizontal -> {X=startPos.X+asegment.Segment.Length; Y=startPos.Y}
        

        segments
        |> List.fold (fun (retraceList,startpos,reTraceTooFarList, lastIndexRetraced) segment ->
            
            // go through all the elements in the list to get the lists
            let newRetraceList, newRetracedIndex= //this function finds the segments that retrace and puts them into a list
                if (( segment.IsZero) && (segment.Index<>0)) //if index==0 and not at start
                then 
                    if ((segment.Index <> segments.Length-1) && (differentSigns segments[segment.Index-1].Length segments[segment.Index+1].Length ) ) || (segment.Index = segments.Length-1)then 
                    //retrace if between 2 opposite sign segments or at the end
                        segment::retraceList, segment.Index
                    else retraceList, lastIndexRetraced
                        
                else retraceList,lastIndexRetraced
            let newReTraceTooFarList= // this function finds the segments that retracted too far that they intersect with their original symbol
                if  (segment.Index = lastIndexRetraced+2) then 
                    if (not segment.IsZero ) && (overlap2D symbolXYPosCords (startpos,startpos) ) 
                        then segments[segment.Index-1]::reTraceTooFarList // if this segment starts inside the wire, then the segment retraced too far is the one before
                        else reTraceTooFarList
                else reTraceTooFarList

            let endpos= calcEndPos startpos asegments[segment.Index] //update the end position after retracing, which is the next segment's starting position
            (newRetraceList, endpos, newReTraceTooFarList, newRetracedIndex)
            )  ([], startPos,[],0)
           
    symbolsAndasegments
    |> List.fold (fun (retracedSegList:Segment list,retracedTooFarSegList: Segment list) symbolAsegments-> 
        let res= (segmentsRetraceInWire symbolAsegments) 
        // let wireRetracedSegList=   res
        let (a,b,c,d)=res
        (a @ retracedSegList), (c @ retracedTooFarSegList)
        ) ([],[])

