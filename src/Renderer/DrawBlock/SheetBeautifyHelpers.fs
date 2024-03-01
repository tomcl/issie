module SheetBeautifyHelpers

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open CommonTypes
open BlockHelpers
open CommonTypes
open SymbolPortHelpers
open Symbol
open BusWire

//Tick3 helper
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

//B1R
let readCustCompDim (sym: Symbol) =
    (sym.Component.H, sym.Component.W)

//B1W
let writeCustCompDim (h: float) (w: float) (symbol: Symbol) =
    let updatedComponent = { symbol.Component with H = h; W = w }
    { symbol with Component = updatedComponent }

//B2W
let writeSymbolPos (symbolModel: SymbolT.Model) (x: float) (y: float) (compId: ComponentId) =
    match Map.tryFind compId symbolModel.Symbols with
    | Some symbol ->
        let updatedPos = { X = x; Y = y }
        let updatedSymbol = { symbol with Pos = updatedPos }
        { symbolModel with Symbols = symbolModel.Symbols.Add(compId, updatedSymbol) }
    | None -> symbolModel 


//B3R
let getPortOrderOnSide (symbol: Symbol) (edge: Edge) : string list option =
    match Map.tryFind edge symbol.PortMaps.Order with
    | Some portOrder -> Some portOrder
    | None -> None


//B3W

let setPortOrderOnSide (symbol: Symbol) (edge: Edge) (portOrder: string list) : Symbol =
    { symbol with PortMaps = { symbol.PortMaps with Order = Map.add edge portOrder symbol.PortMaps.Order } }




//B4R
let isInputPortsReversed (symbol: Symbol) : bool option =
    symbol.ReversedInputPorts

//B4W

(*
let ReverseMuxInputs (symbol: Symbol) (symbolModel: SymbolT.Model) (compId: ComponentId) =
    let reverse = isInputPortsReversed symbol
    match Map.tryFind compId symbolModel.Symbols with
    | Some symbol ->
        match symbol.Component.Type with
        | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 ->
            match reverse with
            | Some true ->
                // Reverse the connections of the symbol's input ports
                let reversedInputPorts =
                    List.rev symbol.Component.InputPorts
                let updatedComponent =
                    { symbol.Component with
                        InputPorts = reversedInputPorts }
                Some { symbol with
                        Component = updatedComponent }
            | _ -> Some symbol // No need to reverse inputs
        | _ -> Some symbol // Component type doesn't need input reversal
    | None -> None // Symbol not found in the model
*)

let writeReverseMuxInputs (symbol: Symbol) (reverse: bool option) : Symbol =
    match symbol.Component.Type with
        | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 -> {symbol with ReversedInputPorts = reverse}
        | _ -> symbol

            


//B5R

(*
let getPortPosition (symbol: Symbol) (portId: string) : XYPos option =
    let component = symbol.Component
    // Find the edge to which the port belongs
    let edge = symbol.PortMaps.Orientation.[portId]
    // Determine the position along the edge based on the port order
    let portOrder = symbol.PortMaps.Order |> Map.find edge
    let portIndex = List.findIndex (fun p -> p = portId) portOrder
    let portCount = List.length portOrder
    // Calculate the position based on the dimensions of the component and port order
    let portPosition =
        match edge with
        | Top -> // Calculate position along the top edge
        | Bottom -> // Calculate position along the bottom edge
        | Left -> // Calculate position along the left edge
        | Right -> // Calculate position along the right edge
    Some portPosition
*)

let getPortPos (port: Port) (model: SymbolT.Model) : XYPos =
    let portId = port.Id
    getPortLocation None model portId

//B6
let getBoundingBox (symbol: Symbol) : BoundingBox =
    symbol.SymbolBoundingBox

//B7R
let checkRotateState (symbol: Symbol) : Rotation =
    symbol.STransform.Rotation
//B7W
let writeRotateState (symbol: Symbol) (rotate: Rotation) : Symbol =
    let updatedSTransform = {symbol.STransform with Rotation = rotate}
    let updatedSymbol = {symbol with STransform = updatedSTransform}
    updatedSymbol
        

//B8R
let checkFlipState (symbol: Symbol) : bool =
    symbol.STransform.Flipped

//B8W
let writeFlipState (symbol: Symbol) (flip: bool) : Symbol =
    let updatedSTransform = {symbol.STransform with Flipped = flip}
    let updatedSymbol = {symbol with STransform = updatedSTransform}
    updatedSymbol

//T1R
let getSymbolOverlapNum (sheet: SheetT.Model): int =
    // Retrieve the bounding boxes of all components from the model
    let boundingBoxes = sheet.BoundingBoxes |> Map.toList

    // Define a function to count overlapping symbols
    let countOverlappingSymbols (box1: BoundingBox) (acc: int) ((_, box2): CommonTypes.ComponentId * BoundingBox) =
        if overlap2DBox box1 box2 then acc + 1 else acc

    // Use List.fold to accumulate the count of overlapping symbols
    boundingBoxes
    |> List.fold (fun acc (_, box1) -> List.fold (countOverlappingSymbols box1) acc boundingBoxes) 0

//T2R
let getWireIntersectSymbolNum (sheet: SheetT.Model) : int =
    let intersectingSegmentCount (wire: Wire) : int =
        let segments = visibleSegments wire.WId sheet
        let wireStartPos = wire.StartPos
        let rec countIntersectingSegments count startPos = function
            | [] -> count
            | segment :: rest ->
                let endPos = startPos + segment // Compute end position by adding segment vector to start position
                let symbolBoundingBoxes =
                    sheet.BoundingBoxes
                    |> Map.toSeq
                    |> Seq.map snd
                    |> Seq.toList
                let intersects =
                    symbolBoundingBoxes
                    |> List.exists (fun box ->
                        match segmentIntersectsBoundingBox box startPos endPos with
                        | Some _ -> true
                        | None -> false)
                let newCount = if intersects then count + 1 else count
                countIntersectingSegments newCount endPos rest // Process the remaining segments recursively
        countIntersectingSegments 0 wireStartPos segments

    sheet.Wire.Wires
    |> Map.fold (fun acc _ wire -> acc + intersectingSegmentCount wire) 0






//T5R
let getNumRightAngles (sheet: SheetT.Model) (wire: Wire) : int =
    let segments = visibleSegments wire.WId sheet
    let wireStartPos = wire.StartPos
    let rec loop count segStart index =
        if index >= segments.Length - 1 then
            count
        else
            let seg = segments.[index]
            let segEnd = segStart + seg
            match getSegmentOrientationOpt segStart segEnd with
            | Some Vertical when seg <> XYPos.zero ->
                let nextSeg = segments.[index + 1]
                if nextSeg <> XYPos.zero then
                    let segEndNext = segEnd + nextSeg
                    match getSegmentOrientationOpt segEnd segEndNext with
                    | Some Horizontal ->
                        loop (count + 1) segEnd (index + 1)
                    | _ -> loop count segEnd (index + 1)
                else
                    loop count segEnd (index + 1)
            | _ -> loop count segEnd (index + 1)
    loop 0 wireStartPos 0









