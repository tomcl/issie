module SheetBeautifyHelpers

open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open SymbolUpdate
open Symbol
open Optics
open Operators
open BlockHelpers
open SymbolResizeHelpers
open EEExtensions
open DrawHelpers
open ModelType
open Sheet.SheetInterface
open Fable.React
open Fable.React.Props
open Elmish

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team
// B3, B5, B6 T2 still trying
// lists the functions that must be written, R = Read, W = write. Where a value is RW two functions are needed one for Read and one for Write. These should be combined in a Lens (if possible).
// B3R, B3W RW Medium Read/write the order of ports on a specified side of a symbol
// B5R R Low The position of a port on the sheet. It cannot directly be written.
// B6R R Low The Bounding box of a symbol outline (position is contained in this)
// T2R R Low The number of distinct wire visible segments that intersect with one or more symbols. See Tick3.HLPTick3.visibleSegments for a helper. Count over all visible wire segments.

/// Identify a port from its component label and number.
/// Usually both an input and output port will mathc this, so
/// the port is only unique if it is known to be input or output.
/// used to specify the ends of wires, since tehee are known to be
/// connected to outputs (source) or inputs (target).
type SymbolPort = { Label: string; PortNumber: int }

/// convenience function to make SymbolPorts
let portOf (label:string) (number: int) =
    {Label=label; PortNumber = number}

// B1RW - The dimensions of a custom component symbol
let customComponentDimensionsLens : Lens<Symbol, (float * float)> =
    // Getter function: gets the width and height of a custom component symbol
    let get (sym: Symbol) = (sym.Component.W, sym.Component.H)
    // Setter function: returns a new Symbol with updated width and height
    let set (newDims: (float * float)) (sym: Symbol) =
        { sym with Component = { sym.Component with W = fst newDims; H = snd newDims } }

    (get, set)

// B2W - The position of a symbol on the sheet
let symbolPositionLens : Lens<Symbol, XYPos> =
    // Getter function: gets the position of a symbol on the sheet
    let get (sym: Symbol) = sym.Pos
    // Setter function: returns a new Symbol with updated position
    let set (newPos: XYPos) (sym: Symbol) = { sym with Pos = newPos }

    (get, set)


// B3R, B3W - Read and Write the order of ports on a specified side of a symbol
let portOrderLens (side: Edge) : Lens<PortMaps, string list option> =
    // Getter function: gets the list of port IDs for a specified side
    let get (portMaps: PortMaps) =
        Map.tryFind side portMaps.Order
    // Setter function: returns a new PortMaps with updated port order for the specified side
    let set (newOrderOpt: string list option) (portMaps: PortMaps) =
        match newOrderOpt with
        | Some newOrder ->
            let updatedOrder = Map.add side newOrder portMaps.Order
            { portMaps with Order = updatedOrder }
        | None -> portMaps  // If None, don't modify the portMaps

    (get, set)


// B4RW - The reverses state of the inputs of a MUX2
let reversedInputPortsLens : Lens<Component, bool option> =
    // Getter function: gets the reverse state of the input ports if available
    let get (comp: Component) =
        match comp.SymbolInfo with
        | Some symInfo -> symInfo.ReversedInputPorts
        | None -> None
    // Setter function: returns a new Component with updated reverse state for the input ports of a MUX2
    let set (newReversedState: bool option) (comp: Component) =
        match comp.Type with
        | Mux2 -> // Only proceed if the component is a Mux2
            let updatedSymbolInfo = 
                match comp.SymbolInfo with
                | Some symInfo -> 
                    Some { symInfo with ReversedInputPorts = newReversedState }
                | None -> 
                    // If there's no SymbolInfo, create it with the new reversed state; adjust according to your model's requirements
                    Some { LabelBoundingBox = None; LabelRotation = None; STransform = { Rotation = Degree0; Flipped = false }; ReversedInputPorts = newReversedState; PortOrientation = Map.empty; PortOrder = Map.empty; HScale = None; VScale = None }
            { comp with SymbolInfo = updatedSymbolInfo }
        | _ -> comp // If not a Mux2, return the component unchanged

    (get, set)



// B5R - Read the position of a port on the sheet
let readPortPosition (sym: Symbol) (port: Port) : XYPos =
    let addXYPos (pos1: XYPos) (pos2: XYPos) : XYPos =
    // relative to the top left corner of the symbol
        { X = pos1.X + pos2.X; Y = pos1.Y - pos2.Y }
    let TopLeft = sym.Pos
    let offset = getPortPos sym port
    addXYPos TopLeft offset




// Helper function to apply scaling to the width and height if present
let applyScaling (width: float) (height: float) (hScale: float option) (vScale: float option) =
    let w = match hScale with
            | Some(scale) -> width * scale
            | None -> width
    let h = match vScale with
            | Some(scale) -> height * scale
            | None -> height
    (w, h)

// B6R - Read the Bounding box of a symbol outline
let readBoundingBox (symbol: Symbol) : BoundingBox =
    let (scaledWidth, scaledHeight) = applyScaling symbol.Component.W symbol.Component.H symbol.HScale symbol.VScale
    {
        TopLeft = symbol.Pos
        W = scaledWidth
        H = scaledHeight
    }

// B7RW - The rotation of a symbol
let symbolRotationLens : Lens<Symbol, Rotation> =
    // Getter function: gets the rotation of a symbol
    let get (sym: Symbol) = sym.STransform.Rotation
    // Setter function: returns a new Symbol with updated rotation, aiming for a different structure than updateSymRotationState
    let set (newRotation: Rotation) (sym: Symbol) = 
        // Update SymbolInfo's STransform rotation if it exists, in a more compact form
        let updatedSymbolInfo = sym.Component.SymbolInfo |> Option.map (fun symInfo ->
            { symInfo with STransform = { symInfo.STransform with Rotation = newRotation } })
        // Prepare the updated component with the potentially updated SymbolInfo
        let updatedComponent = { sym.Component with SymbolInfo = updatedSymbolInfo }
        // Update the symbol's STransform rotation directly, using the updated component
        { sym with Component = updatedComponent; STransform = { sym.STransform with Rotation = newRotation } }

    (get, set)

// B8RW - The flip state of a symbol
let symbolFlipLens : Lens<Symbol, bool> =
    // Getter function: gets the flip state of a symbol
    let get (sym: Symbol) = sym.STransform.Flipped
    // Setter function: returns a new Symbol with updated flip state
    let set (newFlipState: bool) (sym: Symbol) = 
        { sym with STransform = { sym.STransform with Flipped = newFlipState } }

    (get, set)

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
    let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int) =
        if index > 0 && index < segVecs.Length - 1 && segVecs[index] =~ XYPos.zero then
            let before = segVecs.[0..index-2]
            let coalesced = segVecs.[index-1] + segVecs.[index+1]
            let after = segVecs.[index+2..]
            before @ [coalesced] @ after
        else
            segVecs


    wire.Segments
    |> List.mapi getSegmentVector
    |> (fun segVecs ->
            (segVecs,[1..segVecs.Length-2])
            ||> List.fold tryCoalesceAboutIndex)

// module T3R =
//     /// Helper function transforms a list of relative vectors into a list of segments with absolute coordinates.
//     let transformVectorsToCoordinates (startCoord: XYPos) (vectors: XYPos list) =
//         // Auxiliary function to recursively compute the coordinates
//         let rec computeCoords acc currentCoord vectors =
//             match vectors with
//             | [] -> List.rev acc // Reverse to maintain the original order
//             | hd :: tl ->
//                 // Calculate new coordinate by adding the vector to the current coordinate
//                 let newCoord = currentCoord + hd
//                 computeCoords (newCoord :: acc) newCoord tl
//         computeCoords [startCoord] startCoord vectors

//     /// Transforms a list of relative vectors into a list of segments with absolute coordinates.
//     let transformWireSegmentsToAbsoluteMap (model: SheetT.Model) =
//         model.Wire.Wires
//         |> Map.fold (fun accMap (wId: ConnectionId) (wire: Wire) ->
//             let origin = wire.StartPos
//             let vectors = visibleSegments wId model // Assuming this returns the relative vectors
//             let absoluteSegments = transformVectorsToCoordinates origin vectors
//             // Add the wire ID and its segments to the map
//             Map.add wId absoluteSegments accMap
//         ) Map.empty

type SegVector = {Start: XYPos; Dir: XYPos}
let isCrossingAtRightAngle seg1 seg2 =
    // Determine the end points of each segment
    let end1 = { X = seg1.Start.X + seg1.Dir.X; Y = seg1.Start.Y + seg1.Dir.Y }
    let end2 = { X = seg2.Start.X + seg2.Dir.X; Y = seg2.Start.Y + seg2.Dir.Y }

    // Check if both segments are vertical or horizontal
    if (seg1.Dir.X = 0.0 && seg2.Dir.X = 0.0) || (seg1.Dir.Y = 0.0 && seg2.Dir.Y = 0.0) then
        false
    else
        // Check for vertical seg1 intersecting with horizontal seg2 or vice versa
        let isSeg1Vertical = seg1.Dir.X = 0.0
        let seg1Range = 
            if isSeg1Vertical then 
                (min seg1.Start.Y end1.Y, max seg1.Start.Y end1.Y) 
            else (min seg1.Start.X end1.X, max seg1.Start.X end1.X)

        let seg2Range = 
            if isSeg1Vertical then 
                (min seg2.Start.X end2.X, max seg2.Start.X end2.X) 
            else (min seg2.Start.Y end2.Y, max seg2.Start.Y end2.Y)
        let seg1Pos = if isSeg1Vertical then seg1.Start.X else seg1.Start.Y
        let seg2Pos = if isSeg1Vertical then seg2.Start.Y else seg2.Start.X

        // Check if the static position of one segment falls within the range of the other segment's start and end
        seg1Pos > fst seg2Range && seg1Pos < snd seg2Range &&
        seg2Pos > fst seg1Range && seg2Pos < snd seg1Range

let countRightAngleIntersections segments =
    let verticals = segments |> List.filter (fun seg -> seg.Dir.X = 0.0)
    let horizontals = segments |> List.filter (fun seg -> seg.Dir.Y = 0.0)
    let mutable count = 0
    for vSeg in verticals do
        for hSeg in horizontals do
            if isCrossingAtRightAngle vSeg hSeg then
                count <- count + 1
    count

let wireToSegments (wId: ConnectionId) (model: SheetT.Model) =
    let segmentsXYPos = visibleSegments wId model
    let wireStart = model.Wire.Wires.[wId].StartPos

    segmentsXYPos
    |> List.fold (fun (acc: SegVector list, lastPos: XYPos) pos -> 
        match acc with
        | [] -> ([{ Start = lastPos; Dir = pos }], (+) lastPos pos)
        | _ -> (acc @ [{ Start = lastPos; Dir = pos }], (+) lastPos pos)
    ) ([], wireStart)
    |> fst

let totalRightAngleIntersect (model: SheetT.Model) =
    let allSegments =
        model.Wire.Wires
        |> Map.toList // Convert the map of wires to a list of (key, value) pairs
        |> List.collect (fun (wId, _) -> wireToSegments wId model)


    allSegments
    |> countRightAngleIntersections

// T5R - Number of visible wire right-angles. Count over whole sheet.
let countWireRightAngles (wId: ConnectionId) (model: SheetT.Model) =
    // printfn "countWireRightAngles: wId: %A" wId
    let segments = visibleSegments wId model
    // printfn "segments: %A" segments
    let numSegments = List.length segments
    // printfn "numSegments: %A" numSegments
    if numSegments > 0 then numSegments - 1 else 0
    // The check `if numSegments > 0 then ... else 0` ensures that wires with no visible segments
    // do not contribute to the right angle count negatively or incorrectly.

/// Sums up the right angles from all wires in the model.
let countTotalRightAngles (model: SheetT.Model) =
    // printfn "countTotalRightAngles"
    model.Wire.Wires
    |> Map.fold (fun acc wId _ -> acc + countWireRightAngles wId model) 0
    // |> Map.fold (fun acc key _ -> 
    //     let newAcc = acc + countWireRightAngles key model
    //     // Print the updated accumulator value
    //     printfn "Current total right angles: %d" newAcc
    //     newAcc
    // ) 0

