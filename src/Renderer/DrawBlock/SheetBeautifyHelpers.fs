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

//B1Lens
///Read or write dimensions of a custom component symbol
let CustomSymDimoensions_ = 
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
    //newModel is the model with updated BoundingBoxes field and it is used as final model
    //**** let newModel = {rotModel with BoundingBoxes = Symbol.getBoundingBoxes rotModel.Wire.Symbol}
    //**** newModel
//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//ADDITIONAL COMMENTS FOR MARKING - these will be deleted after the individual project marking is done
// For this function I decided to update the position on the sheet insted of just the Symbol becuse the sheet is mentioned in the instuctions 'The position of a symbol on the sheet'
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
///The reverses state of the inputs of a MUX2
let InputsState_ = Lens.create (fun (sym:Symbol) -> sym.ReversedInputPorts) (fun (inputStates: option<bool>) (sym:Symbol) -> { sym with ReversedInputPorts = inputStates})
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
//The position of a port on the sheet.
let readPosOfPortOnSheet  (model: SheetT.Model) (port: Port): XYPos = 
    let sym = model.Wire.Symbol.Symbols[ComponentId port.HostId]
    getPortPos sym port + sym.Pos 
    


//B6R
//The Bounding box of a symbol outline (position is contained in this)
///where ComponentId is the Symbol's ID
let getBoundingboxOfSym (compID: ComponentId) (model: SheetT.Model) : (BoundingBox) = 
    model.BoundingBoxes[compID]
        

//B7
//B7Lens
let SymRotationState_ =  Lens.create (fun (sym:Symbol) -> sym.STransform.Rotation) (fun (r:Rotation) (sym:Symbol) -> { sym with STransform = { sym.STransform with Rotation = r }})

//B8
//B8Lens
let SymFlipState_ =  Lens.create (fun (sym:Symbol) -> sym.STransform.Flipped) (fun (f:bool) (sym:Symbol) -> { sym with STransform = { sym.STransform with Flipped = f }})
// ask if the bool type is fine *****************************

//T1R
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


//T2R
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

///The number of distinct wire visible segments that intersect with one or more symbols. Count over all visible wire segments.
let countSegmentsIntersectSymbols (sheet: SheetT.Model) =
    ///Creates a list of all the BoundingBoxes on the sheet
    let boxes =
        mapValues sheet.BoundingBoxes
        |> Array.toList

    ///Creates a list of all the ConnectionIDs on the sheet
    let connectionIDsL = Map.fold (fun keys key _ -> key::keys) [] sheet.Wire.Wires
    
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

//T3R

// Function that finds the orientation of a segment (so whether it is vertical or horizontal) 
// SegStartAndEnd gives you start and end coordinates of a segment - if x coordinate is the same => vertical segment, else horiz segment
// Now check for two segments : 
// 1. They have opposite orientation (so vert/horiz or horiz/vert)
// 2. For the horiz seg, check if the unchanging coordinate (y coordinate is same at start and end) => should be between the start and end y coordinates of the vertical wire
// 3. For the vert seg, check if the unchanging coordinate (x coordinate is same at start and end) => should be between the start and end x coodrinates of the horizontal wire
// If all conditions hold, then return 1 otherwise return 0
// Then need to check this condition for all pairs of segments on the sheet => Can use allPairs but try not to - try find better solutions using ChatGPT
// You need to List.fold () and add all of the 1s and 0s

//T5R
//Number of visible wire right-angles. Count over whole sheet.
let countWireRightAngles (sheet: SheetT.Model) =
    let connectionIDsL = Map.fold (fun keys key _ -> key::keys) [] sheet.Wire.Wires
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

//symbol.getPortLocation
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

//et updateWires (model : Model) (compIdList : ComponentId list) (diff : XYPos)

// To do:
//checkIfthereIsOverlap 

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
    


    
    
