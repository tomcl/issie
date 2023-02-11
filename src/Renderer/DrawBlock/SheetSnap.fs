module SheetSnap

open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SheetT
open Optics
open Operators
open Sheet


//----------------------------------------------------------------------------------------//
//-----------------------------------SNAP helper functions--------------------------------//
//----------------------------------------------------------------------------------------//

let snapIndicatorLine = 
    { defaultLine with Stroke = "Red"; StrokeWidth = "1px"; StrokeDashArray = "5, 5" }   

let snapLineHorizontal wholeCanvas y = makeLine 0. y wholeCanvas y  snapIndicatorLine
let snapLineVertical wholeCanvas x = makeLine x 0. x wholeCanvas snapIndicatorLine

/// a vertical line marking the position of the current symbol or segment snap, if one exists
let snapIndicatorLineX model wholeCanvas =
    match model.SnapSymbols.SnapX.SnapOpt, model.SnapSegments.SnapX.SnapOpt with
    | Some snap, _
    | None, Some snap ->
        [ makeLine snap.SnapIndicatorPos 0.0 snap.SnapIndicatorPos wholeCanvas snapIndicatorLine ]
    | None, None ->
        []

/// a horizontal line marking the position of the current symbol or segment snap, if one exists
let snapIndicatorLineY model wholeCanvas =
    match model.SnapSymbols.SnapY.SnapOpt, model.SnapSegments.SnapY.SnapOpt with
    | Some snap, _
    | None, Some snap ->
        [ makeLine 0. snap.SnapIndicatorPos wholeCanvas snap.SnapIndicatorPos  snapIndicatorLine ]
    | None, None ->
        []

// projection functions from XYPos to its elements

/// select X coordinate
let posToX (p:XYPos) = p.X
/// select Y coordinate
let posToY (p:XYPos) = p.Y

/// Helper function to create 1D static data for use by snapping functions based on 
/// a single coordinate array of snap points.
/// points: array of points to snap to.
/// limit: max distance from a snap point at which snap will happen.
let makeSnapBounds 
        (limit:float) 
        (points: {|Pos: float; IndicatorPos:float|} array) 
            : SnapData array =
    let points = points |> Array.sortBy (fun pt -> pt.Pos)
    points
    |> Array.mapi (fun i point -> 
        let (xBefore, xAfter) = Array.tryItem (i - 1) points, Array.tryItem (i + 1) points
        let lower = 
            xBefore
            |> Option.map (fun xb -> max ((point.Pos + xb.Pos)/2.) (point.Pos - limit))
            |> Option.defaultValue (point.Pos - limit)
        let upper = 
             xAfter
             |> Option.map (fun xa -> min ((point.Pos + xa.Pos)/2.) (point.Pos + limit))
             |> Option.defaultValue (point.Pos + limit)
        {
            LowerLimit = lower
            UpperLimit = upper
            Snap = point.Pos
            IndicatorPos = point.IndicatorPos
        })



/// initial empty snap data which disables snapping
let emptySnap: SnapXY = 
    let emptyInfo:SnapInfo = {SnapData = [||]; SnapOpt = None}
    {
        SnapX = emptyInfo
        SnapY = emptyInfo
    }

/// Map Component types to values which partition them into
/// sets of components which should be treated as same type
/// in snap operations. e.g. Input, Output, IOLabel are all
/// treated as same.
/// Usage: symbolMatch s1 = symbolMatch s2 // true if s1 and s2
/// snap to each other.
let symbolMatch (symbol: SymbolT.Symbol) =
    match symbol.Component.Type with
    // legacy component - to be deleted
    | Input _
    | Input1 _ | Output _| IOLabel -> 
        Input1 (0, None)

    | BusCompare _
    | BusSelection _ -> 
        BusCompare (0,0u)

    | Constant _
    | Constant1 _ -> 
        Constant (0, 0L)

    | NbitsAdder _ | NbitsXor _ -> 
        NbitsAdder 0

    | MergeWires | SplitWire _ -> 
        MergeWires

    | DFF | DFFE | Register _ | RegisterE _ -> 
        DFF

    | AsyncROM x | ROM x | RAM x -> RAM x 
    | AsyncROM1 x | ROM1 x | RAM1 x | AsyncRAM1 x -> 
        ROM1 { x with Data = Map.empty}

    | otherCompType -> 
        otherCompType

//----------------------------------------------------------------------------------------//
//--------------------------MAIN SNAP DATA DEFINITIONS------------------------------------//
//----------------------------------------------------------------------------------------//

(*
    HLP23 - this module is assumed controlled by the SmartSnap team student: 
    functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    HLP23 - the Smart Snap part of the individual work should update these definitions and
    (if needed, advanced) also update the functions in the next section taht implement snaps.
    Helper functions can be added to the snap helper section above, or to SmartHelpers if they
    might be of use to others.

    HLP23 - Much of the challenge here is understanding the (well written) existing code. 
    One way to make progress is to leave this code as-is and pipeline its result into your 
    own function which modifies the calculated snap data - adding or deleting snaps.

    HLP23 - optionally you could go through all the snap code and and improve it - e.g.
    rewrite using optics.
*)

/// Extracts static snap data used to control a symbol snapping when being moved.
/// Called at start of a symbol drag.
/// model: schematic positions are extracted from here.
/// movingSymbol: the symbol which moved.
let getNewSymbolSnapInfo 
        (model: Model) 
        (movingSymbol: SymbolT.Symbol) 
            : SnapXY =
    /// xOrY: which coordinate is processed.
    /// create snap points on the centre of each other same type symbol
    /// this will align (same type) symbol centres with centres.
    let otherSimilarSymbolData (xOrY: XYPos -> float) = 
        let movingSymbolMatch = symbolMatch movingSymbol
        Map.values model.Wire.Symbol.Symbols 
        |> Seq.filter (fun (sym:SymbolT.Symbol) -> 
                            sym.Id <> movingSymbol.Id && symbolMatch sym = movingSymbolMatch)
        |> Seq.toArray
        |> Array.map (fun sym -> xOrY (Symbol.getRotatedSymbolCentre sym))
        |> Array.map (fun x -> {|Pos=x;IndicatorPos=x|})

    let movingSymbolCentre = Symbol.getRotatedSymbolCentre movingSymbol

    /// for each port on moving symbol, work out whetehr it is connected to another port on opposite edge.
    /// if so add a snap when the two port locations are at same level (on line perpendicular to edges).
    /// This snap will snap symbol movement to no wire kink on connecting 3 segment wires.
    let portSnapData =
        let ports = movingSymbol.PortMaps.Orientation
        let wires = model.Wire.Wires
        let portMap = model.Wire.Symbol.Ports
        let symbolMap = model.Wire.Symbol.Symbols
                    
        let getAllConnectedPorts (pId:string) =
            wires
            |> Map.toArray 
            |> Array.collect (fun (_,wire) -> 
                if wire.OutputPort = OutputPortId pId then 
                    match wire.InputPort with | InputPortId pId' -> [|portMap[pId']|]
                elif wire.InputPort = InputPortId pId then
                    match wire.OutputPort with | OutputPortId pId' -> [|portMap[pId']|]
                else
                    [||])
        ports
        |> Map.toArray
        |> Array.collect (fun (pId,edge) ->
            let portLocOffset = Symbol.getPortLocation None model.Wire.Symbol pId - movingSymbolCentre
            getAllConnectedPorts pId
            |> Array.collect (fun port ->
                let symbol = symbolMap[ComponentId port.HostId]
                let otherPortLoc = Symbol.getPortLocation None model.Wire.Symbol port.Id
                match symbol.PortMaps.Orientation[port.Id], edge with
                | Edge.Left, Edge.Right | Edge.Right, Edge.Left -> 
                    let locDataY = {|Pos = otherPortLoc.Y - portLocOffset.Y; IndicatorPos = otherPortLoc.Y|}
                    [|BusWireT.Horizontal, locDataY|]
                | Edge.Top, Edge.Bottom | Edge.Bottom, Edge.Top -> 
                    let locDataX = {|Pos = otherPortLoc.X - portLocOffset.X; IndicatorPos = otherPortLoc.X|}
                    [|BusWireT.Vertical, locDataX|]
                | _ -> [||]))
        |> Array.partition (fun (ori,_) -> ori = BusWireT.Horizontal)
        |> (fun (horizL,vertL) ->
            let makeSnap =  Array.map snd             
            {| YSnaps = makeSnap horizL; XSnaps = makeSnap vertL |})            

    {
        SnapX = {
                    SnapData = 
                        Array.append (otherSimilarSymbolData posToX) portSnapData.XSnaps
                        |> makeSnapBounds Constants.symbolSnapLimit
                    SnapOpt = None}
        SnapY = { 
                    SnapData = 
                        Array.append (otherSimilarSymbolData posToY) portSnapData.YSnaps
                        |> makeSnapBounds Constants.symbolSnapLimit
                    SnapOpt = None}
    }
   


    

/// Extracts static snap data used to control a segment snapping when being dragged.
/// Called at start of a segment drag.
/// xOrY: which coordinate is processed.
/// model: segment positions are extracted from here.
/// movingSegment: the segment which moved.
/// See SnapXY definition for output.
let getNewSegmentSnapInfo  
        (model: Model) 
        (movingSegment: BusWireT.ASegment) 
            : SnapXY =

    /// Is seg Horizontal or Vertical? Returns None if segments is zero length
    let getDir (seg: BusWireT.ASegment) = BusWire.getSegmentOrientationOpt seg.Start seg.End

    let thisWire = model.Wire.Wires[movingSegment.Segment.WireId]
    let thisSegId = movingSegment.Segment.GetId()
    let orientation = getDir movingSegment
    let snapBounds = 
        match orientation with
        | None ->
            [||] // probably this should never happen, since we cannot move 0 length segments by dragging
        | Some ori ->
            model.Wire.Wires
            |> Map.filter (fun wid otherWire -> otherWire.OutputPort = thisWire.OutputPort)
            |> Map.toArray
            |> Array.map snd
            |> Array.collect (BusWire.getNonZeroAbsSegments >> List.toArray)
            |> Array.collect (function | aSeg when getDir aSeg = Some ori  && aSeg.Segment.GetId() <> thisSegId-> 
                                            [|BusWire.getFixedCoord aSeg|] 
                                       | _ -> 
                                            [||])
            |> Array.map (fun x -> {|Pos=x; IndicatorPos=x|})
            |> makeSnapBounds Constants.segmentSnapLimit

    let snapInDirection snapOrientation =   
        {
            SnapData = (if orientation = Some snapOrientation then snapBounds else [||]); 
            SnapOpt = None
        }       
        
    {
        SnapX = snapInDirection BusWireT.Vertical
        SnapY = snapInDirection BusWireT.Horizontal        
    }

//-----------------------------------------------------------------------------------------------//
//-------------------------------------Snap Functions--------------------------------------------//
//-----------------------------------------------------------------------------------------------//

/// The main snap function which is called every update that drags a symbol or segment.
/// This function porocesses one coordinate (X or Y) and therefore is called twice.
/// autoscrolling: if true switch off snapping (and unsnap if needed).
/// pos.ActualPosition: input the actual position on schematic of the thing being dragged.
/// pos.MouseDelta: mouse position change between this update and the last one.
/// snapI: static (where can it snap) and dynamic (if it is snapped) info controlling the snapping process.
let snap1D 
        (autoScrolling: bool) 
        (pos:{|MouseDelta:float; ActualPosition:float|}) 
        (snapI:SnapInfo) : SnapInfo * float  =

    /// return the snap info if pos should be snapped to d, otehrwise None
    let mustSnap (pos: float) (d: SnapData) =
        if (not autoScrolling) && pos > d.LowerLimit && pos < d.UpperLimit then 
            Some {UnSnapPosition = pos; SnapPosition = d.Snap; SnapIndicatorPos = d.IndicatorPos}
        else
            None        
    /// the position to which the symbol should move if it is unsnapped
    /// based on the snap info snapI.
    /// if no snap keep current position.
    let unSnapPosition = 
        snapI.SnapOpt
        |> Option.map (fun snap -> snap.UnSnapPosition)
        |> Option.defaultValue pos.ActualPosition 
    let data = snapI.SnapData
    let newUnSnapPosition = unSnapPosition + pos.MouseDelta

    let newSnap, newPosition =
        match Array.tryPick (mustSnap newUnSnapPosition) data with
        | Some snapPos -> Some snapPos, snapPos.SnapPosition
        | None -> None, newUnSnapPosition

    {snapI with SnapOpt = newSnap}, newPosition - pos.ActualPosition
 
/// Determine how a dragged symbol snaps. Returns updated snap info and offset to add to symbol position.
/// Symbol position is not updated here, the offset is given to a MoveSymbol message.
/// Called every mouse movement update in a symbol drag.
let snap2DSymbol 
        (autoScrolling:bool) 
        (mousePos: XYPos) 
        (symbol: SymbolT.Symbol) 
        (model:Model) : SnapXY * XYPos =
    let centrePos = Symbol.getRotatedSymbolCentre symbol
    let (snapIX,deltaX) = snap1D 
                            autoScrolling 
                            {|MouseDelta = mousePos.X - model.LastMousePos.X ; ActualPosition = centrePos.X|} 
                            model.SnapSymbols.SnapX
    let (snapIY,deltaY) = snap1D 
                            autoScrolling 
                            {|MouseDelta = mousePos.Y - model.LastMousePos.Y ; ActualPosition = centrePos.Y|} 
                            model.SnapSymbols.SnapY
    let snapXY = {SnapX = snapIX; SnapY = snapIY}
    snapXY, {X=deltaX; Y=deltaY}

/// Determine how a dragged segment snaps. Returns updated snap info and offset to add to ssegment position.
/// Segment position is not updated here, the offset is given to a MoveSegment message.
/// NB every segment can only be dragged in one cordinate - perpendicular to its orientation.
/// Called every mouse movement update in a segment drag.
let snap2DSegment 
        (autoScrolling:bool) 
        (mousePos: XYPos) 
        (aSegment: BusWireT.ASegment) 
        (model:Model) : SnapXY * XYPos =
    let ori = BusWire.getSegmentOrientation aSegment.Start aSegment.End
    let fixedCoord = BusWire.getFixedCoord aSegment
    let deltaXY = mousePos - model.LastMousePos
    let snapXY = model.SnapSegments

    let newSnapXY, newDeltaXY =
        match ori with
        | BusWireT.Horizontal -> 
            let snapY,delta = snap1D autoScrolling {|MouseDelta = deltaXY.Y; ActualPosition = fixedCoord|} snapXY.SnapY
            {snapXY with SnapY=snapY}, {deltaXY with Y = delta}
        | BusWireT.Vertical -> 
            let snapX,delta = snap1D autoScrolling {|MouseDelta = deltaXY.X; ActualPosition = fixedCoord|} snapXY.SnapX
            {snapXY with SnapX=snapX}, {deltaXY with X = delta}
    newSnapXY, newDeltaXY
    
    
