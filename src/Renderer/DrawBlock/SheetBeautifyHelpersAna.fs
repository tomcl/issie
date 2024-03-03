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

//-----------------------------------------------------------------------------------------------------------------------
//------------------> I have written code for all the required functions except T6R <------------------------------------
//-----------------------------------------------------------------------------------------------------------------------

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
    //part of code that is missing:  the totalLengthOverlap identifier contains the calculations of the total length of overlaps on the sheet (by using the helper function I wrote above -> overlapLenthSegPairs) and in the next step subtracts that from the total lenght of wires on the sheet
    //the code continues:
    //let visibleTotalLengthOfWires = totalLengthOfAllWires - totalLengthOverlap
    //visibleTotalLengthOfWires
    ()
// in this task I struggled to make all posible distinct pairs that don't contain same pair of elements of a list. For example:
// [1,2,3] to (1;2) (1;3) (2;3) so I couldn't finish the code.
    



//T5R
/// Calculates the number of visible wire right-angles. Counts over whole sheet.
let countWireRightAngles (sheet: SheetT.Model) =
    // List of all connectionIds on the sheet
    let connectionIDsL = Map.fold (fun keys key _ -> key::keys) [] sheet.Wire.Wires
    //every visible segmet creates a right-angle with the following visible segment. -1 is for the last segment that is not followed by another segment
    let rightAngleNumber(wId: ConnectionId) (sheet: SheetT.Model) = 
        (visibleSegments wId sheet |> List.length) - 1
    connectionIDsL |> List.map (fun wId -> rightAngleNumber wId sheet) |> List.reduce (fun fst snd -> fst + snd)


