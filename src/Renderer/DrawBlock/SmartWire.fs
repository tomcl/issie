module SmartWire

open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers

open Optics
open Operators

(* HLP23

    This module will normally be used exclusively by team member doing the "smart autoroute on single
    wire creation" part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    It does not need specific "get started" test code since is called whenever a new wire is created 
    or (not usual) a symbol is moved so far that the topology of a wire chnages and it is autorouted 
    again from scratch.

*)


/// Returns a list of all the bounding boxes of all the symbols in the current sheet
let getAllSymbolBoundingBoxes (model: Model) : BoundingBox list =
    let componentIDs = model.Symbol.Symbols.Keys |> List.ofSeq

    /// Takes in componentId and returns the bounding box of the corresponding symbol
    let getSymbolBoundingBox (model: Model) (componentId: ComponentId) : BoundingBox =
        let symbol = model.Symbol.Symbols[componentId]

        let symbolHeight =
            match symbol.VScale with
            | Some vScale -> symbol.Component.H * vScale
            | None -> symbol.Component.H

        let symbolWidth =
            match symbol.HScale with
            | Some hScale -> symbol.Component.W * hScale
            | None -> symbol.Component.W

        { H = symbolHeight
          W = symbolWidth
          TopLeft = symbol.Pos }

    componentIDs |> List.map (getSymbolBoundingBox model)


/// Checks if a wire intersects any symbol or not. Returns number of symbols intersected by wire
let checkWireIntersectSymbols (model: Model) (wireVertices: XYPos list) : int =
    let allSymbolBoundingBoxes = getAllSymbolBoundingBoxes model
    // printfn "num of symbols: %A" allSymbolBoundingBoxes.Length

    let segVertices = List.pairwise wireVertices[2..wireVertices.Length - 3] // do not consider the nubs
    // printfn "num of segments: %A" segVertices.Length

    let numBoxesIntersectedBySegment startPos endPos =
        allSymbolBoundingBoxes
        |> List.mapi (fun i boundingBox ->
            match segmentIntersectsBoundingBox boundingBox startPos endPos with
            | Some _ -> 
                printfn "Segment index %A" i 
                1 // segment intersects bounding box
            | None -> 0 // no intersection
        )
        |> List.sum

    segVertices
    |> List.map (fun (startPos, endPos) -> numBoxesIntersectedBySegment startPos endPos)
    |> List.sum

/// An improved version of makeInitialSegmentsList which takes into account intersection with symbols
/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, as well as the orientation of the final port
/// this function returns a list of Segment(s).
let makeSmartInitialSegmentsList
    (hostId: ConnectionId)
    (startPos: XYPos)
    (endPos: XYPos)
    (portOrientation: Edge)
    (model: Model)
    : list<Segment> =
    let wireVertices = makeInitialWireVerticesList startPos endPos portOrientation

    let numIntersections = checkWireIntersectSymbols model wireVertices
    printfn "num of intersections %A" numIntersections |> ignore

    wireVertices |> xyVerticesToSegments hostId

/// top-level function which replaces autoupdate and implements a smarter version of same
/// it is called every time a new wire is created, so is easily tested.
let smartAutoroute (model: Model) (wire: Wire) : Wire =
    let segmentInfo =
        wire.Segments |> List.map (fun (seg: Segment) -> seg.Length, seg.Mode)

    // printfn
    //     "%s"
    //     $"Wire: Number of Segments={wire.Segments.Length} \nInitial Orientation={wire.InitialOrientation}\nSegments={segmentInfo}\nWidth={wire.Width}"
    // |> ignore
    // printfn "%A" wire.Segments.Length |> ignore
    let destPos, startPos =
        Symbol.getTwoPortLocations (model.Symbol) (wire.InputPort) (wire.OutputPort)

    let destEdge = Symbol.getInputPortOrientation model.Symbol wire.InputPort

    let startEdge = Symbol.getOutputPortOrientation model.Symbol wire.OutputPort

    let startPort = genPortInfo startEdge startPos
    let destPort = genPortInfo destEdge destPos

    // Normalise the routing problem to reduce the number of cases in makeInitialSegmentsList
    let normStart, normEnd = rotateStartDest CommonTypes.Right (startPort, destPort)

    let initialSegments =
        makeSmartInitialSegmentsList wire.WId normStart.Position normEnd.Position normEnd.Edge model

    let segments =
        {| edge = CommonTypes.Right
           segments = initialSegments |}
        |> rotateSegments startEdge // Rotate the segments back to original orientation
        |> (fun wire -> wire.segments)

    { wire with
        Segments = segments
        InitialOrientation = getOrientationOfEdge startEdge
        StartPos = startPos }
