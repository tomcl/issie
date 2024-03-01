module SheetBeautifyHelpers
open Symbol
open Fable.React
open Fable.React.Props
open Elmish
open Optics
open Helpers

open CommonTypes
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType
open DrawModelType.SheetT
open BlockHelpers
open DrawModelType.BusWireT




type Dimensions2D =
    {
        W : float
        H : float
    }
type Seg =
    {
        Start : XYPos
        End : XYPos
    }

//B1Lens
///Read or write dimensions of a custom component symbol
let customSymDimoensions_ = 
    Lens.create 
        (fun (sym:Symbol) -> { W = sym.Component.W; H = sym.Component.H }) 
        (fun (dimensions: Dimensions2D) (sym:Symbol) -> { sym with Component = { sym.Component with W = dimensions.W; H = dimensions.H }})
//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//ADDITIONAL COMMENTS FOR MARKING - these will be deleted after the individual project marking is done
//Since I am using quite short names for width and height, I decided to use the name 'dimensions' for easier readability.
//I don't check if the symbol is of type custom because it is impossible to check that without throwing an error or fail the function which can cause a lot of issues later (as we were advised).
//If needed here is my previous function before I made that decision:

////B1
////B1R
//let readCustomSymDimensions (sym: Symbol) =
//    match sym.Component.Type with
//    | Custom _-> 
//        // Return the dimensions of the Custom Symbol
//        { W = sym.Component.W; H = sym.Component.H }
//    | _ -> failwith "Symbol is not of type Custom"
//
////B1W
//let writeCustomSymDimensions (dimensions: Dimensions2D) (sym: Symbol) =
//    match sym.Component.Type with
//    | Custom _ -> 
//        // Create a new Symbol with updated dimensions
//        let updatedComponent = { sym.Component with W = dimensions.W; H = dimensions.H }
//        { sym with Component = updatedComponent }
//    | _ -> failwith "Symbol is not of type Custom"
//
////B1Lens
//let CustomSymDimensions_old = Lens.create readCustomSymDimensions writeCustomSymDimensions
//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


//B2W
///Writes the position of a symbol on the sheet,where CompId is the Symbol's ID
let writeSymPosition (compID: ComponentId) (pos: XYPos) (model: SheetT.Model) : (SheetT.Model) =
    //rotModel is the model with updated Wire field 
    let rotModel =
        { model with
            Wire = { model.Wire with
                        Symbol = { model.Wire.Symbol with
                                    //the position of Symbol with compID in Symbols is updated with pos
                                    Symbols = model.Wire.Symbol.Symbols 
                                    |> Map.add 
                                        compID 
                                        { (model.Wire.Symbol.Symbols |> Map.find compID) with Pos = pos }
                                 }
                   }
        }
    rotModel
    |> SheetUpdateHelpers.updateBoundingBoxes

//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//ADDITIONAL COMMENTS FOR MARKING - these will be deleted after the individual project marking is done
// For this function I decided to update the position on the sheet insted of just the Symbol becuse the sheet is mentioned in the instuctions 'The position of a symbol on the sheet'
//The input related to the Symbol is of type ComponentID because sometimes we represent the Symbols by 
//their ID or we can get easily the componentId of the Symbol just by Symbol.Id
//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

 

//B3
//B3R
///Read the order of ports on a specified side of a symbol
let readPortOrderOfSide (side: Edge) (sym: Symbol) =
    //Returns the order(the list of ports on that side) of the ports of a side of a Symbol
    sym.PortMaps.Order[side] 
    
//B3W
///Write the order of ports on a specified side of a symbol
let writePortOrderOfSide (side: Edge) (order:string list) (sym: Symbol) =
    // Create a new Symbol with updated port order
    let updatedOrder = 
        sym.PortMaps.Order 
        |> Map.add side order

    { sym with PortMaps = { sym.PortMaps with Order = updatedOrder } }
    
//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//ADDITIONAL COMMENTS FOR MARKING - these will be deleted after the individual project marking is done
//I decided not to use lens here because it would be weird to combine side and symbol in one type since side is key of a map which is subfield of symbol.
//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


//B4
//B4Lens
///Lens for the reverses state of the inputs of a MUX2
let inputsState_ = Lens.create (fun (sym:Symbol) -> sym.ReversedInputPorts) (fun (inputStates: option<bool>) (sym:Symbol) -> { sym with ReversedInputPorts = inputStates})
//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//ADDITIONAL COMMENTS FOR MARKING - these will be deleted after the individual project marking is done
//I don't check if the symbol is of type MUX2 because it is impossible to check that without throwing an error or fail the function which can cause a lot of issues later (as we were advised).
//If needed here is my previous function before I made that decision:

////B4R
////read state of the inputs of a MUX2
//let readStateOfInputsOfMUX2(sym: Symbol) =
//    match sym.Component.Type with
//    | Mux2 -> sym.ReversedInputPorts
//    | _ -> failwith "Symbol is not of type Mux2"
//
//    
////B4W
//let writeStateOfInputsOfMUX2 (inputStates: option<bool>) (sym: Symbol) =
//
//    match sym.Component.Type with
//    | Mux2 -> { sym with ReversedInputPorts = inputStates }
//    | _ -> failwith "Symbol is not of type Mux2"
//    
////B4Lens
//let StateOfInputsOfMUX2_old = Lens.create readStateOfInputsOfMUX2 writeStateOfInputsOfMUX2

//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

//B5R
///Reads the position of a port on the sheet.
let readPortPosOnSheet  (model: SheetT.Model) (port: Port): XYPos = 
    let sym = model.Wire.Symbol.Symbols[ComponentId port.HostId]
    getPortPos sym port + sym.Pos 
    


//B6R
///Reads the Bounding box of a symbol outline (position is contained in this),
///where ComponentId is the Symbol's ID
let getBoundingboxOfSym (compID: ComponentId) (model: SheetT.Model) : (BoundingBox) = 
    model.BoundingBoxes[compID]

//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//ADDITIONAL COMMENTS FOR MARKING - these will be deleted after the individual project marking is done
//The input related to the Symbol is of type ComponentID because sometimes we represent the Symbols by 
//their ID or we can get easily the componentId of the Symbol just by Symbol.Id
//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        

//B7
//B7Lens
///Lens that raads or writes the rotation state of a symbol
let symRotationState_ =  Lens.create (fun (sym:Symbol) -> sym.STransform.Rotation) (fun (r:Rotation) (sym:Symbol) -> { sym with STransform = { sym.STransform with Rotation = r }})

//B8
//B8Lens
///Lens that reads or writes the flip state of a symbol
let symFlipState_ =  Lens.create (fun (sym:Symbol) -> sym.STransform.Flipped) (fun (f:bool) (sym:Symbol) -> { sym with STransform = { sym.STransform with Flipped = f }})


//T1R
///The number of pairs of symbols that intersect each other. Counted over all pairs of symbols on the sheet.
let countIntersectedSymbols (sheet: SheetT.Model) =
    let boxes =
        mapValues sheet.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> n,box)
    let countPairs =
        List.allPairs boxes boxes 
        |> List.filter (fun ((n1, box1), (n2, box2)) -> n1 <> n2 && BlockHelpers.overlap2DBox box1 box2)
        |> List.length
    countPairs



//helper function copied from TestDrawBlock
/// The visible segments of a wire, as a list of vectors, from source end to target end.
/// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed 
/// which if present causes the two segments on either side of it to coalesce into a single visible segment.
/// A wire can have any number of visible segments - even 1.
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
//helper function
//Kept the wId name for consistency with the other modules
///Creates list of tuples where fst is the segStart and snd is the segEnd for all visible Segments
let SegStartAndEnd (wId: ConnectionId) (sheet: SheetT.Model) =
        //Gets the wire from the ConnectionId
        let wire = sheet.Wire.Wires[wId]
        //Gets the start position of the wire
        let wireStart = wire.StartPos
        //helper function needed for the mapFold, adds the start position of the wire to the relative position 
        //of the segment nodes in order to get the position of the node on the sheet
        let addWirePos (stateWirePos:XYPos) (segNode: XYPos) =
            let transformedElement = 
                { 
                X =  segNode.X + stateWirePos.X;
                Y = segNode.Y + stateWirePos.Y 
                }
            let stateWirePos = transformedElement
            (transformedElement, stateWirePos)
        //get the list of segment nodes  
        let segNodes, firstSegNode = 
            visibleSegments wId sheet 
            |> List.mapFold addWirePos wireStart 
        //prepends the node that is skiped by visibleSegments 
        let allSegNodes = wireStart::segNodes        
        allSegNodes   
        |> List.windowed 2
        |> List.map (fun arr -> (arr.[0], arr.[1]))

//T2R
///Calculates the number of distinct wire visible segments that intersect with one or more symbols. Counts over all visible wire segments on a sheet.
let countSegmentsIntersectSymbols (sheet: SheetT.Model) =
    ///Creates a list of all the BoundingBoxes on the sheet
    let boxes =
        mapValues sheet.BoundingBoxes
        |> Array.toList

    ///Creates a list of all the ConnectionIDs on the sheet
    let connectionIDsL = Map.fold (fun keys key _ -> key::keys) [] sheet.Wire.Wires

    ///Creates lists for all connectionIDs, with their segments start and end XYPos
    let segLists = connectionIDsL |> List.map (fun wId -> SegStartAndEnd wId sheet)

    ///Returns True if the segment intersects the BoundingBox    
    let IfSegIntersectsBoundingBox seg (box: BoundingBox) =
        match segmentIntersectsBoundingBox box (fst seg) (snd seg) with
        | Some(result) -> true
        | None -> false
    ///Returns True if the segment intersects with any of the BoundingBoxes
    let IfSegIntersectsBoundingBoxes seg (boxes: list<BoundingBox>) =
        boxes |> List.exists (fun box -> IfSegIntersectsBoundingBox seg box)
    
    let countIntersectioningSeg (segLists: list<list<XYPos*XYPos>>) (boxes: list<BoundingBox>) =
        ///Stores the results of the check IfSegIntersectsBoundingBoxes of all the segments
        let results = List.map (fun segList -> List.map (fun seg -> IfSegIntersectsBoundingBoxes seg boxes) segList) segLists
        let countTrueValues (boolLists: list<list<bool>>) =
            let countTrueInList boolList =
                List.fold (fun acc b -> if b then acc + 1 else acc) 0 boolList
            List.map countTrueInList boolLists
            |> List.sum
        countTrueValues results

    countIntersectioningSeg segLists boxes
// NOTE added later: I could have used List.collect in order to avoid the type of segLists: list<list<XYPos*XYPos>> and instead have a better, easier to manipulate type list<XYPos*XYPos>.



//T3R Helper functions

/// Function that finds the orientation of a segment (so whether it is vertical or horizontal) 
let segOrientation (startPos : XYPos , endPos : XYPos) =
    if abs(startPos.X - endPos.X) <= XYPos.epsilon
    then  Vertical
    else Horizontal


///Returns true if the input segments are orthogonal and intersect
let orthogonalyIntersectedPairs  (firstSeg:XYPos*XYPos) (secSeg:XYPos*XYPos)=
            let fstSeg={
                Start = fst firstSeg;
                End = snd firstSeg
            }
            let sndSeg={
                Start = fst secSeg;
                End = snd secSeg
            }
            match segOrientation firstSeg, segOrientation secSeg with
            | ( Vertical), ( Horizontal) ->
            //The X coordinate of the vertical segment is the same everywhere along the segment and it needs to be between the Start X and End X coordinates of the horizontal segment 
                (( min sndSeg.Start.X  sndSeg.End.X < (fstSeg.Start.X)) && ((fstSeg.Start.X) < max sndSeg.Start.X  sndSeg.End.X) &&
            // The Y coordinate of the horizontal segment is the same everywhere along the segment and it needs to be between the Start Y and End Y coordinates of the vertical segment
                ( min fstSeg.Start.Y  fstSeg.End.Y < (sndSeg.Start.Y)) && ((sndSeg.Start.Y) < max fstSeg.Start.Y  fstSeg.End.Y))         
            | ( Horizontal), (Vertical) -> 
                (( min fstSeg.Start.X  fstSeg.End.X < (sndSeg.Start.X)) && ((sndSeg.Start.X) < max fstSeg.Start.X  fstSeg.End.X) &&
                ( min sndSeg.Start.Y  sndSeg.End.Y < (fstSeg.Start.Y)) && ((fstSeg.Start.Y) < max sndSeg.Start.Y  sndSeg.End.Y))           
            | _ -> false  

//T3R main function
///Counts the number of distinct pairs of segments that cross each other at right angles. Does not include 0 length segments or 
/// segments on same net intersecting at one end, or segments on same net on top of each other. Counts over whole sheet.
let countSegCrosses (sheet: SheetT.Model) =
    //Creates a list of all connectionIDs on the sheet
    let connectionIDsL = Map.fold (fun keys key _ -> key::keys) [] sheet.Wire.Wires
    //Creates a list of all segments on the sheet
    let segLists = 
        connectionIDsL 
        |> List.collect (fun wId -> SegStartAndEnd wId sheet)
    //Creates a list of all segment pairs on the sheet
    let allSegPairs (segLists:List<XYPos*XYPos>) =  List.collect (fun x -> List.map (fun y -> (x,y)) segLists) segLists
    //Creates a list of all pairs that are orthogonal 
    let orthogonalyIntersectedSegPairs = 
        allSegPairs segLists 
        |> List.filter (fun pair -> orthogonalyIntersectedPairs (fst pair) (snd pair))
    orthogonalyIntersectedSegPairs |> List.length
    

//T4R helper function
///If the segments overlap it returns the length of the overlaping part otherwise returns 0.
let overlapLenthSegPairs  (firstSeg:XYPos*XYPos) (secSeg:XYPos*XYPos)=
            let fstSeg={
                Start = fst firstSeg;
                End = snd firstSeg
            }
            let sndSeg={
                Start = fst secSeg;
                End = snd secSeg
            }

            match segOrientation firstSeg, segOrientation secSeg with
            | ( Vertical), ( Vertical) -> 
                if abs(fstSeg.Start.X - sndSeg.Start.X) < XYPos.epsilon
                then
                    let minFirst = min fstSeg.Start.Y fstSeg.End.Y
                    let minSec = min sndSeg.Start.Y sndSeg.End.Y 
                    let maxMin = max minFirst minSec

                    let maxFirst = max fstSeg.Start.Y fstSeg.End.Y
                    let maxSec = max sndSeg.Start.Y sndSeg.End.Y
                    let minMax = min maxFirst maxSec
                    abs(minMax - maxMin)                  
                else 0
            | ( Horizontal), (Horizontal) -> 
                if abs(fstSeg.Start.Y - sndSeg.Start.Y) < XYPos.epsilon
                    then
                        let minFirst = min fstSeg.Start.X fstSeg.End.X
                        let minSec = min sndSeg.Start.X sndSeg.End.X 
                        let maxMin = max minFirst minSec

                        let maxFirst = max fstSeg.Start.X fstSeg.End.X
                        let maxSec = max sndSeg.Start.X sndSeg.End.X
                        let minMax = min maxFirst maxSec
                        abs(minMax - maxMin)                  
                    else 0
                           
            | _ -> 0



///Visible wire length
let visibleLengthOfWires (sheet: SheetT.Model) =
    let nets = groupWiresByNet sheet.Wire.Wires
    let totalLengthOfAllWires = totalLengthOfWires sheet.Wire.Wires
    //totalLengthOverlap calculates the total length of overlaps on the sheet (by using the helper function I wrote above -> overlapLenthSegPairs) and in the next step subtracts that from the total lenght of wires on the sheet
    //let visibleTotalLengthOfWires = totalLengthOfAllWires - totalLengthOverlap
    //visibleTotalLengthOfWires
    ()
    
    



//T5R
/// Calculates the number of visible wire right-angles. Counts over whole sheet.
let countWireRightAngles (sheet: SheetT.Model) =
    // List of all connectionIds on the sheet
    let connectionIDsL = Map.fold (fun keys key _ -> key::keys) [] sheet.Wire.Wires
    //every visible segmet creates a right-angle with the following visible segment. -1 is for the last segment that is not followed by another segment
    let rightAngleNumber(wId: ConnectionId) (sheet: SheetT.Model) = 
        (visibleSegments wId sheet |> List.length) - 1
    connectionIDsL |> List.map (fun wId -> rightAngleNumber wId sheet) |> List.reduce (fun fst snd -> fst + snd)


//________________________________________________________________________________________________________________________
//TEAM  DELIVERABLES
//D1. sheetAlignScale ASB
//________________________________________________________________________________________________________________________

// Align all singly-connected components to eliminate wire bends in parallel wires

//----> helper functions for the algorithm

//I am using parallel as "wire that is straight, or a candidate for straightening".
//Wires, for example, with 4 visual segments, are not parallel, and  are more complex. You could ignore them.

//A component is single-connected (in a given direction) when it has a single straightenable 3-visual-segment 
//wire connecting it to another component in that direction. In that case you can be sure that moving the component 
//can straighten that connection without unstraightening any other connections.
//Straightening: turn 3 visual segments into one
//Unstraightening: turning one visual segment into 3

//A *single-constrained wire* is one which can be straightened without unstraightening any other wires: in other 
//words one of its ends is a single-connected component. - 







///Returns True if a wire is streightened.
let streightenedWire (wire: Wire) (sheet: SheetT.Model) = 
    let wId = wire.WId
    (visibleSegments wId sheet |> List.length) = 1
///Returns True if a wire has potential to be streightened.
let straighteningPotentialWire (wire: Wire) (sheet: SheetT.Model) =
    let wId = wire.WId
    (visibleSegments wId sheet |> List.length) = 3


// Need to calculate the sign of the offset and return it as the second element of the tuple with this function
// Can use PortMaps.Orientation - This is in the Symbol Record
let checkIfSinglePortComponent (sym: Symbol) =
    let numInputPorts= sym.Component.InputPorts |> List.length 
    let numOutputPorts= sym.Component.OutputPorts |> List.length
    (numInputPorts + numOutputPorts) = 1


let endOfWire (wire: BusWireT.Wire) (sheet: SheetT.Model) = 
    visibleSegments wire.WId sheet
    |> List.fold (fun start segEnd -> start + segEnd) wire.StartPos

let changeOffsetSign (sym: Symbol) (portId: string) =
    match Map.tryFind portId sym.PortMaps.Orientation with
    | Some Left -> -1.0
    | _ -> 1.0



//BlockHelpers.getSourcePort
    //let inline getSourcePort (model:Model) (wire:Wire)
//BlockHelpers.moveSymbol
    //moveSymbol (offset:XYPos) (sym:Symbol)
//BusWireRoute.updateWires
    //let updateWires (model : Model) (compIdList : ComponentId list) (diff : XYPos) = 
    // list opf moved componnts

//symbol.
    //getPortLocation (defPos: XYPos option) (model: Model) (portId : string) : XYPos=



//Should be used just for wires with 3 visible segments
// Calculates the offSet based on the middle segmant of a potential wire 
let calculateOffset (wire: Wire) (sheet: SheetT.Model) =
    wire.WId
    |> (fun wId -> visibleSegments wId sheet)
    |> (fun visSegs -> if List.length visSegs = 3 then visSegs[1] else XYPos.zero)

//let calculateOffset_old (wire: Wire) (sheet: SheetT.Model) =
//    let wId = wire.WId
//    let nodeslist = visibleSegments wId sheet
//    let firstNode = List.item 0 nodeslist
//    let secondNode = List.item 1 nodeslist
//    {
//        X = firstNode.X - secondNode.X
//        Y = firstNode.Y - secondNode.Y
//    }


///Returns a list of the potential wire if there are no straight wires already.
let listPotentialWires (wires: list<Wire> ) (sheet: SheetT.Model) =
    let streightWires = wires |> List.exists (fun wire -> streightenedWire wire sheet)
    if streightWires = false
    then wires |> List.filter (fun wire -> straighteningPotentialWire wire sheet)
    else []


//Write a  function that changes the position of the symbol according rhe first potential wire
let getPotentialWireOffset (sheet: SheetT.Model) (wires: list<Wire> ) =
    if wires = []
    then XYPos.zero
    else
        let firstWire = List.item 0 wires
        calculateOffset firstWire sheet
    






    
        


let getWiresFromPort (sheet: SheetT.Model) (port: Port) (wireInputPort: bool) =
            sheet.Wire.Wires
            |> Map.toList
            |> List.map snd
            |> List.filter (fun wire -> if wireInputPort = true
                                        then match wire.InputPort with
                                                | InputPortId id when id = port.Id -> true
                                                | _ -> false
                                        else match wire.OutputPort with
                                                | OutputPortId id when id = port.Id -> true
                                                | _ -> false)


///Get all the wires from a Symbol that has strictly just one Port
let allWires1PortSym (sym: Symbol) (sheet: SheetT.Model)=
        //let i = List.item 0 sym.Component.InputPorts 
        // let o = List.item 0  sym.Component.OutputPorts
        if sym.Component.OutputPorts = []
        then
            let i = List.item 0 sym.Component.InputPorts
            let wires = getWiresFromPort sheet i true
            wires
        else
            let o = List.item 0  sym.Component.OutputPorts
            let wires = getWiresFromPort sheet o false
            wires
        
/// VERY IMPORTANT THAT YOU CHECK THAT THE SYMBOL HAS ONLY ONE PORT
let align1PortSymbol (onePortSym: Symbol) (sheet: SheetT.Model)= // (offsetSign: +/-)
    let portId = Map.toList onePortSym.PortMaps.Orientation |> List.item 0 |> fst
    let offsetSign = changeOffsetSign onePortSym portId
    let potentialList = 
        sheet
        |> allWires1PortSym onePortSym
        |> listPotentialWires 
    sheet
    |> potentialList 
    |> getPotentialWireOffset sheet
    |> (fun offset -> BlockHelpers.moveSymbol {X = offset.X * offsetSign; Y = offset.Y * offsetSign} onePortSym)
    // |> (fun newSym -> Optic.set (SheetT.symbolOf_ onePortSym.Id) newSym sheet)
    // |> SheetUpdateHelpers.updateBoundingBoxes

/// After all the symbols have been moved, update the wiring on the entire sheet.
/// 
/// newcIdList -> List of cIds of all symbols that have moved
/// 
/// sheet -> The sheet to be changed
/// 
/// symbolMovedBy -> Take as 0 for now, not sure what this does, needed in updateWires
let update1PortWires (newcIdList: List<ComponentId>) (symbolMovedBy: XYPos) (sheet: SheetT.Model) = 
    BusWireRoute.updateWires sheet.Wire newcIdList symbolMovedBy
    |> (fun newWireModel -> Optic.set SheetT.wire_ newWireModel sheet)

let scaleSymbol (newVertical: float option) (newHorizontal: float option) (symbol: Symbol) (sheet: SheetT.Model) =
        let symbols = sheet.Wire.Symbol.Symbols

        let newSymbol = {symbol with VScale = newVertical; HScale = newHorizontal}

        let newSymbols = Map.add symbol.Id newSymbol symbols

        Optic.set SheetT.symbols_ newSymbols sheet
        |> SheetUpdateHelpers.updateBoundingBoxes
        //fix the function Update boundingBoxes for scaling!!!

//Not used but might be needed later
//let offestPortPos (firstPort:Port) (secondPort:Port) (sheet: SheetT.Model)=
//    let firstPortPos = readPortPosOnSheet sheet firstPort
//    let secPortPos = readPortPosOnSheet sheet secondPort
//    let Xdiff  = abs(firstPortPos.Y - secPortPos.Y)
//    let Ydiff = abs(firstPortPos.X - secPortPos.X)
//    let diff = max Xdiff Ydiff
//    diff

///Calculates port offset between two consecutive ports of same type (input or output).
let calcPortOffset (sym: SymbolT.Symbol) (portType: PortType) =
        let portList =
            match portType with
            | PortType.Input -> sym.Component.InputPorts
            | PortType.Output -> sym.Component.OutputPorts
        if List.length portList < 2
        then None
        else 
            (Symbol.getPortPos sym portList[1]) - (Symbol.getPortPos sym portList[0])
            |> (fun pos -> Some (max pos.X pos.Y))

///Calculates the ratio that is needed for scaleSymbol (newVertical and newHorizontal)
let calcPortRatio (outSym: SymbolT.Symbol) (inSym: SymbolT.Symbol) =
        printfn $"{calcPortOffset outSym PortType.Output}"
        match calcPortOffset outSym PortType.Output, calcPortOffset inSym PortType.Input with
        | Some (outOff), Some (inOff) -> outOff/inOff
        | _ -> 1.0



 

let firstPhaseStraightening (sheet: SheetT.Model) =
    let initialOverlap = countIntersectedSymbols sheet
    let singlePortComponents = 
        sheet.Wire.Symbol.Symbols
        |> Map.toList 
        |> List.filter (fun (_, sym) -> checkIfSinglePortComponent sym)


    let changedSymbolList = 
        singlePortComponents
        |> List.map (fun (_, sym) -> align1PortSymbol sym sheet)
    changedSymbolList
    |> List.fold (fun sheet newSym -> 
                        let newSheet = Optic.set (SheetT.symbolOf_ newSym.Id) newSym sheet
                        if countIntersectedSymbols newSheet > initialOverlap
                        then sheet
                        else newSheet) sheet
    |> SheetUpdateHelpers.updateBoundingBoxes
    |> update1PortWires (List.map (fun sym -> sym.Id) changedSymbolList) XYPos.zero
    


    
    //get the actual position of the input port and allign the output port  with the input
    //getPortPosOnSheet  

//BlockHelpers.getSourcePort
    //let inline getSourcePort (model:Model) (wire:Wire)
//BlockHelpers.moveSymbol
    //moveSymbol (offset:XYPos) (sym:Symbol)
//BusWireRoute.updateWires
    //let updateWires (model : Model) (compIdList : ComponentId list) (diff : XYPos) = 
    // list opf moved componnts

//for custom components I want to make the port distance equal on both sides
//if I have two symbols and the input symbol is not singly connected then just scale the input symbol
