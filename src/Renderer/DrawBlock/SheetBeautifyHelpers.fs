module SheetBeautifyHelpers

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team
open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BlockHelpers
open Symbol
open Helpers

//TestDrawBlock
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
        /// Index must be in range 1..segVecs
        let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int)  =
            if segVecs[index] =~ XYPos.zero
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
let getCustomDimensions (sym: Symbol) : float*float =
    (sym.Component.H, sym.Component.W)

//B1W
let updateCustomDimensions (h: float) (w:float) (sym: Symbol) : Symbol =
    let updatedComponent = {sym.Component with H = h; W = w}
    {sym with Component = updatedComponent}

//B2W
let updateSymbolPos (sym: Symbol) (pos: XYPos): Symbol =
    let newPos = pos
    let comp' = {sym.Component with X = newPos.X; Y = newPos.Y}
    {sym with 
        Component = comp'
        Pos = newPos
        LabelBoundingBox = {sym.LabelBoundingBox with TopLeft = sym.LabelBoundingBox.TopLeft + pos}
    }

//B3R
let getSidePorts (sym:Symbol) (side: Edge) : string list =
    match sym.PortMaps.Order.TryFind(side) with
    | Some(ports) -> ports
    | None -> []

//B3W
let updateSidePorts (sym:Symbol) (side: Edge) (ports: string list) : Symbol =
    let updatedOrder = Map.add side ports sym.PortMaps.Order
    let updatedOrientation = 
        ports
        |> List.fold (fun accMap port -> Map.add port side accMap) sym.PortMaps.Orientation
    let updatedPortMaps = { Order = updatedOrder; Orientation = updatedOrientation }
    { sym with PortMaps = updatedPortMaps }

//B4R
let getReverseStateMux2 (sym: Symbol): bool option =
    match sym.Component.Type with
    | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 -> sym.ReversedInputPorts
    | _ -> None

//B4W
let updateReverseStateMux2 (sym: Symbol) (state: bool option): Symbol =
    match sym.Component.Type with
    | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 -> {sym with ReversedInputPorts = state}
    | _ -> sym

//B5
let getPortPos (port: Port) (model: SymbolT.Model) : XYPos =
    let portId = port.Id
    getPortLocation None model portId

//B6
let getSymbolOutlineBoundingBox (sym:Symbol): BoundingBox =
    sym.SymbolBoundingBox

//B7R
let getRotationState (sym:Symbol): Rotation =
    sym.STransform.Rotation

//B7W
let updateRotationState (sym: Symbol) (rotation: Rotation): Symbol =
    match rotation with
    | Degree0 | Degree90 | Degree180 | Degree270 ->
        let updatedTransform = {sym.STransform with Rotation = rotation}
        {sym with STransform = updatedTransform}
    | _ -> sym

//B8R
let getFlipState (sym:Symbol): bool =
    sym.STransform.Flipped

//B8W
let updateFlipState (sym: Symbol) (flip: bool): Symbol =
    let updatedTransform = {sym.STransform with Flipped = flip}
    {sym with STransform = updatedTransform}

//T1R
let getSymbolOverlapCount (sym: Symbol) (sheet: SheetT.Model): int =
    let wireModel = sheet.Wire
    let boxes =
        mapValues sheet.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> n,box)
    let overlappingPairs =
        List.allPairs boxes boxes
        |> List.filter (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
    List.length overlappingPairs

//T2R
let getWireOveerlapCount (sheet: SheetT.Model): int =
    let allWires = sheet.Wire.Wires

    let countIntersectingSegments wireCount wire =
        let segments = visibleSegments wireCount sheet
        let symbolBoundingBoxes = BusWireRoute.findWireSymbolIntersections sheet.Wire wire
        let isSegmentIntersectingSymbol (segment: XYPos) =
            symbolBoundingBoxes |> List.exists (fun symbolBox -> BlockHelpers.overlap2DBox symbolBox segment)
        segments |> List.filter isSegmentIntersectingSymbol |> List.length

    Map.fold (fun acc wireCount wire ->
        acc + countIntersectingSegments wireCount wire) 0 allWires

//T3R

//T4R   


    

