module SheetBeautifyHelpers

//-----------------Module for beautify Helper functions--------------------------//
//written by ll3621
//What each function does is from the xml comments, more detailed description is after each line when needed



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




///get a custom input symbol and returns the height and width of the symbol in a xypos format
///B1R
let readCustomSymbolSize (sym:Symbol)=
    // let symPos = get posOfSym_ sym
    let comp = sym.Component 
    let compBox = {X=comp.W; Y=comp.H}
    compBox

///returns a symbol with dimension of the input
///B1W
let writeCustomSymbolSize (HW:XYPos) (sym: Symbol)=
    let W,H= HW.X,HW.Y
    let newSym={sym with Component= {sym.Component with H=H; W=W}}
    newSym
/// their combined lens
let RWCustomSymbolSize_=Lens.create (readCustomSymbolSize) (writeCustomSymbolSize)



///B2W
/// changes the position of a symbol within a sheet to user input
let writeSymbolPosition (pos:XYPos) (symId:ComponentId) (sheet : SheetT.Model)=
    let boundingBoxes= sheet.BoundingBoxes
    let symBoundingBox=boundingBoxes[symId]
    let newsymBox={symBoundingBox with TopLeft =pos-{X=symBoundingBox.W/2.; Y=symBoundingBox.H/2.} }
    let newSymBoundingBoxes= Map.change symId (fun stringsOpt -> Some newsymBox) boundingBoxes
    {sheet with BoundingBoxes=newSymBoundingBoxes}

///B3R
/// returns the ids of the ports of a symbols on a specified edge
let readPortOrder (sym:Symbol) (edge:Edge)=
    let portmap=sym.PortMaps
    let order= portmap.Order

    match Map.tryFind edge order with
    | Some keys -> keys
    | None -> []
    
/// B3W
/// changes the ori
let WritePortOrder (newOrder:list<string>)  (edge:Edge) (sym:Symbol)=
    let portmap=sym.PortMaps
    let order= portmap.Order
    let newOrderMap=Map.change edge (fun stringsOpt -> Some newOrder) order
    let newportmap= {portmap with Order = newOrderMap}
    {sym with PortMaps = newportmap}


///B4R
   /// returns the reversesState of any symbol (especially a mux2)
let readMux2ReverseState (sym:Symbol)=
    sym.ReversedInputPorts
///B4W
///changes the reversesate of a symbol to specified state input
let writeMux2ReverseState(state:option<bool>)(sym:Symbol)=
    {sym with ReversedInputPorts = state}

let RWMux2ReverseState= Lens.create (readMux2ReverseState) (writeMux2ReverseState)



///B5R
/// get the position of the a port in the sheet
let readPortPos(sheet:SheetT.Model) (port : Port)=
    let sym =
        sheet.Wire.Symbol.Symbols
        |> Map.toList
        |> List.tryFind (fun (_, sym) -> sym.Component.Id = port.HostId)
        |> function
            | Some (_, sym) -> sym
            | None -> failwithf "The given component should be in the list of symbols"
    getPortPos sym port

///B6R
/// get the bounding box of a symbol
let readSymbolBoundingBox (sym:Symbol) =
    sym.SymbolBoundingBox

///B7R
/// get the rotation state of a symbol
let readSymbolRotationState (sym:Symbol) =
    sym.STransform.Rotation
///B7W
///set the rotation state of a symbol
let writeSymbolRotationState (rotation:Rotation) (sym:Symbol) =
    {sym with STransform= { sym.STransform with Rotation=rotation}}
let RWSymbolRotationState=Lens.create  (readSymbolRotationState) (writeSymbolRotationState)

///B8R 
///get the flipped state of a symbol
let readSymbolFlipState(sym:Symbol) =
    sym.STransform.Flipped
///B8W
/// overwrite the flip state of a symbol
let writeSymbolFlipState (flipped:bool) (sym:Symbol) =
    {sym with STransform= { sym.STransform with Flipped=flipped}}
let RWSymbolFlipState= Lens.create  (readSymbolFlipState) (writeSymbolFlipState)

///T1R
///number of pairs of symbols that intersect with each other
let readSymIntersectsSymPairs  (sheet: SheetT.Model) =
            
            let boxes =
                mapValues sheet.BoundingBoxes
                |> Array.toList
                |> List.mapi (fun i box -> i,box)
            List.allPairs boxes boxes 
            |> List.fold (fun num ((n1,box1),(n2,box2))  ->
                 if ((n1 <> n2) && BlockHelpers.overlap2DBox box1 box2) then num+1 else num) 0
            |> (fun num -> num/2) // for all pairs each pair is counted twice

        
///visible segments function provided in tick3
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

///The number of distinct wire visible segments that intersect with one or more symbols
/// get all non zero segments and see if they intersect with every symbol
let ReadSegmentIntersectSymbolNum ( sheet: SheetT.Model)=
    let wires= sheet.Wire.Wires

    let wiresList= wires |> Map.toList  |> List.map snd
    let absSegments=List.collect id (List.map (fun wire ->getNonZeroAbsSegments wire) wiresList )
    let boundingBoxes= sheet.BoundingBoxes |> Map.toList  |> List.map snd

    List.allPairs absSegments boundingBoxes
    |> List.map (fun  (segment, boundingbox)-> segmentIntersectsBoundingBox boundingbox segment.Start segment.End)
    |> List.fold (fun num (intersectRes:option<float>) ->
        match intersectRes with
        | Some _ -> num+1
        | None->num) 0


///T3R
///The number of distinct pairs of segments that cross each other at right angles. 
///counted the segment pairs that are not in the same net, and perpendicularly intersected

let readSegRightIntersectPair(sheet: SheetT.Model)=
    let wires= sheet.Wire.Wires
    // let keysList= wires |> Map.toList  |> List.map fst
    let wiresList= wires |> Map.toList  |> List.map snd
    let samenetWires= List.fold (fun (samenetWires:Map<Port ,Wire list>) (wire:Wire)-> 
        let wireport= getSourcePort sheet.Wire wire
        if (samenetWires.ContainsKey wireport)
        then
            let lis=samenetWires[wireport]
            Map.add wireport (wire::lis) samenetWires
        else Map.add wireport [wire] samenetWires ) Map.empty wiresList

    let absSegmentsListListList= samenetWires |> Map.toList |> List.map snd |> List.map (fun wireList-> List.map (fun wire ->getNonZeroAbsSegments wire) wireList)
    // list of Asegment lists from lists of wires with the same port

    let absSegments= List.collect id absSegmentsListListList
    // let absSegments= (List.map (fun wire ->getNonZeroAbsSegments wire) wiresList )
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
    |> half // checked twice because all list returl a,b and b,a
    
///helper type used to compute length of visable wire postions in T4R
type WirePosDict =
    {
        Horizontal:Map<float,List<float*float>>
        Vertical:Map<float,List<float*float>>
    } //the intermediate data store to store the wire positions
///T4R
///Sum of wiring segment length, counting only one when there are N same-net segments overlapping
///Groups same net wires into the same group, then compute a wirePosDict to resolve overlapping for wires in the same net
/// then calculate the length, and finally added together
let readTotalWireLength (sheet : SheetT.Model)=
    
   
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
    let wiresList= wires |> Map.toList  |> List.map snd
    let samenetWires= List.fold (fun (samenetWires:Map<Port ,Wire list>) (wire:Wire)-> 
        let wireport= getSourcePort sheet.Wire wire
        if (samenetWires.ContainsKey wireport)
        then
            let lis=samenetWires[wireport]
            Map.add wireport (wire::lis) samenetWires
        else Map.add wireport [wire] samenetWires ) Map.empty wiresList
    
    let absSegmentsListListList= samenetWires |> Map.toList |> List.map snd |> List.map (fun wireList-> List.map (fun wire ->getNonZeroAbsSegments wire) wireList)
    // list of Asegment lists from lists of wires with the same port

    let absSegments= List.collect id absSegmentsListListList

    absSegments
    |> List.map makeWirePosDict
    |> List.fold (fun num wireposdict ->num+(calcLenFromWirePosDict wireposdict)) 0.0




  



///T5R
///Number of visible wire right-angles
///counts the number of visablesegments for each wire, because visible segments combines the 0 length segments 3 into 1
///it must mean that all adjacent visible segments are perpendicular and >0 length, so right angle num= sum of (visible segment num in each wire-1)
let readVisibleRightAngleNum (sheet:SheetT.Model)=
    let wires= sheet.Wire.Wires
    let IdList= wires |> Map.toList  |> List.map fst

    let visibleSegmentsList:list<list<XYPos>> = List.map (fun cId-> visibleSegments cId sheet) IdList
    visibleSegmentsList 
    |> List.fold (fun num segmentList-> num+ (List.length segmentList)-1) 0



///T6R
///Return from one function a list of all the segments that retrace, and also a list of all the end of wire segments that retrace so far that the next segment starts inside symbol
/// finds the retrace segment by identifying if they are 0 length have segments of opposite orientation, (or they are last one)
/// find the segments that retrace too far by seeing if their previous segment retraces, and the end of this segment ends inside the symbol it started
/// or it ends inside the symbol the wire is going to. Either way, they have retraced too far to end inside a symbol.
let readRetracedWire(sheet:SheetT.Model)=
    let wiresMap= sheet.Wire.Wires

    let wires=wiresMap|> Map.toList |>List.map snd

    let differentSigns x y = x * y < 0.0

    let segmentsRetraceInWire (wire:Wire)= // this function finds all retracing segments in the wire

        let asegments= getAbsSegments wire
        let sourceSymbol= getSourceSymbol sheet.Wire wire
        let targetSymbol= getTargetSymbol sheet.Wire wire
        let segments= List.map (fun asegment -> asegment.Segment) asegments
        
        let sourceSymbolBoundingBox= getSymbolBoundingBox sourceSymbol
        let targetSymbolBoundingBox= getSymbolBoundingBox targetSymbol
        let sourceSymbolXYPosCords= sourceSymbolBoundingBox.TopLeft, sourceSymbolBoundingBox.BottomRight()
        let targetSymbolXYPosCords= targetSymbolBoundingBox.TopLeft, targetSymbolBoundingBox.BottomRight()
        

        segments
        |> List.fold (fun (retraceList,reTraceTooFarList, lastIndexRetraced) segment ->
            
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
                if  (segment.Index = lastIndexRetraced+1) then 
                    if (not segment.IsZero ) && ((overlap2D sourceSymbolXYPosCords (asegments[segment.Index].End,asegments[segment.Index].End) ||(overlap2D targetSymbolXYPosCords (asegments[segment.Index].End,asegments[segment.Index].End))) ) 
                        then segment::reTraceTooFarList // if this segment ends inside the source symbol or target symbol, then the segment has retraced too far that the next segment will start inside it
                        else reTraceTooFarList
                else reTraceTooFarList

            (newRetraceList,  newReTraceTooFarList, newRetracedIndex)
            )  ([],[],0)
        |> (fun (a,b,c) -> a,b)
           
    wires
    |> List.fold (fun (retracedSegList:Segment list,retracedTooFarSegList: Segment list) symbolAsegments-> 
        let res= (segmentsRetraceInWire symbolAsegments) 
        ((fst res) @ retracedSegList), ((snd res) @ retracedTooFarSegList)
        ) ([],[])

