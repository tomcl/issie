module SheetBeautifyHelpers

open DrawModelType
open CommonTypes
open DrawModelType.SymbolT
open DrawModelType.SheetT
open DrawModelType.BusWireT
open Optics
open Optics.Operators
open Helpers
open SymbolHelpers
open BlockHelpers
open Symbol
open BusWireRoute
open BusWire

// --------------------------------------------------- //
//                      Helpers                        //
// --------------------------------------------------- //

// lens to set the symbolMap in the model
let symbolMap_: Lens<SheetT.Model, Map<ComponentId, Symbol>> =
    Lens.create (fun m -> m.Wire.Symbol.Symbols) (fun v m ->
        let updatedSymbol = { m.Wire.Symbol with Symbols = v }
        { m with Wire = { m.Wire with Symbol = updatedSymbol } })

/// <summary>
/// visibleSegments helper provided by Professor.
/// The visible segments of a wire, as a list of vectors, from source end to target end.
/// Note that a zero length segment one from either end of a wire is allowed which if present
/// causes the end three segments to coalesce into a single visible segment.
/// </summary>
/// <param name="wId">The ID of the wire for which to calculate the visible segments.</param>
/// <param name="model">The current model containing all wires.</param>
/// <returns>A list of positions representing the visible segments of the wire.</returns>
let visibleSegments (wId: ConnectionId) (model: SheetT.Model) : XYPos list =

    let wire = model.Wire.Wires[wId] // get wire from model

    /// helper to match even and odd integers in patterns (active pattern)
    let (|IsEven|IsOdd|) (n: int) =
        match n % 2 with
        | 0 -> IsEven
        | _ -> IsOdd

    /// Convert seg into its XY Vector (from start to end of segment).
    /// index must be the index of seg in its containing wire.
    let getSegmentVector (index: int) (seg: BusWireT.Segment) =
        // The implicit horizontal or vertical direction  of a segment is determined by
        // its index in the list of wire segments and the wire initial direction
        match index, wire.InitialOrientation with
        | IsEven, BusWireT.Vertical
        | IsOdd, BusWireT.Horizontal -> { X = 0.; Y = seg.Length }
        | IsEven, BusWireT.Horizontal
        | IsOdd, BusWireT.Vertical -> { X = seg.Length; Y = 0. }

    /// Return a list of segment vectors with 3 vectors coalesced into one visible equivalent
    /// if this is possible, otherwise return segVecs unchanged.
    /// Index must be in range 1..segVecs
    let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int) =
        if
            index < segVecs.Length - 1
            && segVecs[index] =~ XYPos.zero
        then
            segVecs[0 .. index - 2]
            @ [ segVecs[index - 1] + segVecs[index + 1] ]
            @ segVecs[index + 2 .. segVecs.Length - 1]
        else
            segVecs

    wire.Segments
    |> List.mapi getSegmentVector
    |> (fun segVecs ->
        (segVecs, [ 1 .. segVecs.Length - 2 ])
        ||> List.fold tryCoalesceAboutIndex)

// --------------------------------------------------- //
//                     B1 Functions                    //
// --------------------------------------------------- //

/// <summary>
/// B1R: Return the dimensions of a custom component symbol.
/// Utilises getCustomSymCorners to find the dimensions
/// </summary>
/// <param name="sym">The symbol to get the dimensions of.</param>
/// <returns>The dimensions of the custom component symbol.</returns>
let getCustomComponentSymbolDims (sym: Symbol) : XYPos =
    match sym.Component.Type with
    | Custom _ ->
        // directly return the third element from the result of getCustomSymCorners
        // format: [|{X=0.0;Y=0.0}; {X=0.0;Y=yDim'}; {X=xDim';Y=yDim'}; {X=xDim';Y=0.0}|]
        (getCustomSymCorners sym).[2]
    | _ -> failwithf "Symbol is not a custom component"

/// <summary> B1W: Set the dimensions of a custom component symbol </summary>
/// <param name="sym">The symbol to set the dimensions of.</param>
/// <param name="dimensions">The new dimensions of the symbol.</param>
let setCustomComponentSymbolDims (sym: Symbol) (dimensions: XYPos) : Symbol =
    // Note: do not modify h and w as they are the default dimensions of the component.
    // We only scale them as needed
    let hScale = dimensions.X / sym.Component.H
    let vScale = dimensions.Y / sym.Component.W
    { sym with HScale = Some hScale; VScale = Some vScale }

// --------------------------------------------------- //
//                     B2 Functions                    //
// --------------------------------------------------- //

/// <summary> B2W: Modifies to the symbol's position. </summary>
/// <param name="sym">The symbol to set the position of.</param>
/// <param name="newPos">The new position of the symbol.</param>
let setSymbolPos (sym: Symbol) (newPos: XYPos) : Symbol =
    let comp' = { sym.Component with X = newPos.X; Y = newPos.Y }
    { sym with
        Component = comp'
        Pos = newPos
        LabelBoundingBox =
            { sym.LabelBoundingBox with
                TopLeft = sym.LabelBoundingBox.TopLeft - sym.Pos + newPos } }

/// <summary> B2W: The position of a symbol on the sheet. This modifies the sheet's SymbolMap </summary>
/// <param name="symID">The ID of the symbol to set the position of.</param>
/// <param name="pos">The new position of the symbol.</param>
/// <param name="model">The SheetT.Model to modify.</param>
let setSymbolPosOnSheet (symID: ComponentId) (pos: XYPos) (model: SheetT.Model) : SheetT.Model =
    let symbolMap = model.Wire.Symbol.Symbols
    let sym: Symbol =
        match symbolMap.TryFind(symID) with
        | Some s -> s
        | None -> failwithf "Symbol with id %A not found in model" symID

    let newSym = moveSymbol pos sym // let newSym be the symbol with the new position
    let newSymbolMap = Map.remove symID symbolMap |> Map.add symID newSym
    // return the model with the new symbolMap
    model |> Optic.set symbolMap_ newSymbolMap

// --------------------------------------------------- //
//                     B3 Functions                    //
// --------------------------------------------------- //

/// <summary> B3R: Read the order of ports on a specified side of a symbol. </summary>
/// <param name="sym">The symbol to get the port order of.</param>
/// <param name="side">The Edge of the symbol to get the port order of.</param>
/// <returns>The order of ports on the specified side of the symbol.</returns>
let getPortMapsOrder
    (sym: Symbol)
    (side: Edge)
    // get Symbol.PortMaps.Order which is Map<Edge, string list>
    // return all strings for given Edge
    =
    sym.PortMaps.Order.TryFind(side)
    |> Option.defaultValue []

/// <summary> B3W: Write the order of ports on a specified side of a symbol. </summary>
/// <param name="sym">The symbol to set the port order of.</param>
/// <param name="newSide">The Edge of the symbol to set the port order of.</param>
/// <param name="newOrder">The new order sequence of ports to be set.</param>
/// <returns>The symbol with the new port order set in the Symbol.PortMaps</returns>
let setPortMapsOrder (sym: Symbol) (newSide: Edge) (newOrder: string list) : Symbol =
    let portMaps = sym.PortMaps
    let newPortMaps =
        { portMaps with Order = portMaps.Order |> Map.add newSide newOrder }
    { sym with PortMaps = newPortMaps }

// --------------------------------------------------- //
//                     B4 Functions                    //
// --------------------------------------------------- //

/// <summary> B4R: Read the reversed state of the inputs of a MUX2 </summary>
/// <param name="sym">The symbol to get the reversed state of the inputs of.</param>
/// <returns>The reversed state of the inputs of the MUX2</returns>
let getMUX2ReversedInput (sym: Symbol) : bool =
    match sym.Component.Type with
    | Mux2 ->
        sym.ReversedInputPorts
        |> Option.defaultValue false
    | _ -> failwithf "Symbol is not a MUX2"

/// <summary>
/// B4W: The reversed state of the inputs of a MUX2
/// There are two variants of this helper: one toggles the state, and the other sets the state to a predefined value.
/// setToggleMUX2ReversedInput toggles the state of the MUX2.
/// </summary>
/// <param name="sym">The symbol to toggle the reversed state of the inputs of.</param>
/// <returns>The symbol with the reversed state of the inputs of the MUX2 toggled.</returns>
let setToggleMUX2ReversedInput (sym: Symbol) : Symbol =
    let toggle (state: bool option) =
        match state with
        | Some s -> Some(not s)
        | None -> None
    match sym.Component.Type with
    | Mux2 -> { sym with ReversedInputPorts = toggle sym.ReversedInputPorts }
    | _ -> failwithf "Symbol is not a MUX2"

/// B4W: The reversed state of the inputs of a MUX2
/// There are two variants of this helper: one toggles the state, and the other sets the state to a predefined value.
/// setMUX2ReversedInput sets the state of the MUX2 to a predefined value
/// </summary>
/// <param name="sym">The symbol to toggle the reversed state of the inputs of.</param>
/// <param name="state">The new bool state of the MUX2.</param>
/// <returns>The symbol with the reversed state of the inputs of the MUX2 toggled.</return
let setMUX2ReversedInput (sym: Symbol) (state: bool) : Symbol =
    let stateOption = Some state
    match sym.Component.Type with
    | Mux2 -> { sym with ReversedInputPorts = stateOption }
    | _ -> failwithf "Symbol is not a MUX2"

// --------------------------------------------------- //
//                     B5 Function                     //
// --------------------------------------------------- //

/// <summary> B5R: The position of a port on the sheet. It cannot directly be written. </summary>
/// <param name="sym">The symbol to get the position of the port of.</param>
/// <param name="port">The port to get the position of.</param>
/// <returns>The position of the port on the sheet as XYPos.</returns>
let getPortPosOnSheet (sym: Symbol) (port: Port) =
    // get the position of the symbol, and relative position of the port
    // add the two positions together and return the result

    let portPosOnSymbol = getPortPos (sym: Symbol) (port: Port)
    sym.Pos + portPosOnSymbol

// --------------------------------------------------- //
//                     B6 Function                     //
// --------------------------------------------------- //

/// <summary> B6R: The Bounding box of a symbol outline (position is contained in this) </summary>
/// <param name="sym">The symbol to get the bounding box of.</param>
/// <returns>The bounding box of the symbol outline as BoundingBox.</returns>
let getSymbolOutlineBoundingBox (sym: Symbol) : BoundingBox =
    let h, w = getRotatedHAndW sym
    if sym.Annotation = Some ScaleButton then
        { TopLeft = sym.Pos - { X = 9.; Y = 9. }; H = 17.; W = 17. }
    else
        { TopLeft = sym.Pos; H = float (h); W = float (w) }

// --------------------------------------------------- //
//                     B7 Functions                    //
// --------------------------------------------------- //

/// <summary> B7R: Read he rotation state of a symbol </summary>
/// <param name="sym">The symbol to get the rotation state of.</param>
/// <returns>The rotation state of the symbol as Rotation.</returns>
let getSymbolRotation (sym: Symbol) : Rotation = sym.STransform.Rotation

/// <summary> B7W: Write the rotation state of a symbol </summary>
/// <param name="sym">The symbol to set the rotation state of.</param>
/// <param name="newRot">The new rotation state of the symbol.</param>
/// <returns>The symbol with the new rotation state set.</returns>
let setSymbolRotation (sym: Symbol) (newRot: Rotation) : Symbol =
    let sTransform = sym.STransform
    let newSTransform = { sTransform with Rotation = newRot }
    { sym with STransform = newSTransform }

// --------------------------------------------------- //
//                     B8 Functions                    //
// --------------------------------------------------- //

/// <summary> B8R: Read the flip state of a symbol </summary>
/// <param name="sym">The symbol to get the flip state of.</param>
/// <returns>The flip state of the symbol as bool.</returns>
let getSymbolFlip (sym: Symbol) : bool = sym.STransform.flipped

/// <summary> B8W: Write the flip state of a symbol </summary>
/// <param name="sym">The symbol to set the flip state of.</param>
/// <param name="newFlip">The new flip state of the symbol.</param>
/// <returns>The symbol with the new flip state set.</returns>
let setSymbolFlip (sym: Symbol) (newFlip: bool) : Symbol =
    let sTransform = sym.STransform
    let newSTransform = { sTransform with flipped = newFlip }
    { sym with STransform = newSTransform }

// --------------------------------------------------- //
//                     T1R Function                    //
// --------------------------------------------------- //

/// <summary> T1R: The number of pairs of symbols that intersect each other. See Tick3 for a related function. Count over all pairs of symbols. </summary>
/// <param name="model">The model to count the intersecting symbol pairs of.</param>
/// <returns>The number of pairs of symbols that intersect each other.</returns>
let countIntersectingSymbolPairs (model: SheetT.Model) =
    let boxes =
        mapValues model.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> n, box)
    List.allPairs boxes boxes
    |> List.filter (fun ((n1, box1), (n2, box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
    |> List.length

// --------------------------------------------------- //
//                     T2R Functions                   //
// --------------------------------------------------- //

///<summary>
/// T2R, T3R Helper.
/// Remove all invisible segments from wires on a sheet.Wire.Wires.
/// </summary>
/// <param name="wires">Map of wires indexed by ConnectionID to remove invisible segments from.</param>
/// <returns>Map of wires indexed by ConnectionID with invisible segments removed.</returns>
// visibleSegments would've worked, but outputs an XYPos list, which is a format that isn't well accepted by the other functions and types.
// This is achieved by utilising existing helper function segmentsToIssieVertices to convert all segments to a list of vertices.
// It is then very easy to remove duplicate vertices.
// We can utilise another helper function issieVerticesToSegments to convert vertices back to segments, and create new wires.
let removeWireInvisibleSegments (wires: Map<ConnectionId, Wire>) =
    wires
    |> Map.map (fun connId wire ->
        let uniqueVertices =
            segmentsToIssieVertices wire.Segments wire
            |> List.distinctBy (fun (x, y, _) -> (x, y))
        // segmentsToIssieVertices returns <float * float * bool> list
        // get rid of duplicate vertices sharing the same float values
        // later, we convert uniqueVertices back to segments

        let newSegments = issieVerticesToSegments connId uniqueVertices
        // for each wire, set the segments to the new segments
        wire |> Optic.set segments_ newSegments)

/// <summary>
/// T2R: The number of distinct wire visible segments that intersect with one or more symbols. See Tick3.HLPTick3.visibleSegments for a helper. Count over all visible wire segments.
/// </summary>
/// <param name="model">The model to count the intersecting wire segments of.</param>
/// <returns>The number of distinct wire visible segments that intersect with one or more symbols.</returns>
let countVisibleSegsIntersectingSymbols (model: SheetT.Model) =
    let wireModel = model.Wire
    wireModel.Wires
    |> removeWireInvisibleSegments
    |> Map.fold
        // For each wire, define a function that takes an accumulator and a wire
        (fun acc _ wire ->
            // Add to the accumulator the count of intersections between the wire and symbols
            // The count is obtained by calling BusWireRoute.findWireSymbolIntersections, which returns a list of intersections,
            // and then using List.length to get the count of intersections
            acc
            + (BusWireRoute.findWireSymbolIntersections wireModel wire
               |> List.length))
        // Initialize the accumulator to 0
        0

// --------------------------------------------------- //
//                     T3R Functions                   //
// --------------------------------------------------- //

/// <summary> T3R helper: Returns true if two 1D line segments intersect at a 90º angle. Takes in two segments described as point-to-point.
/// A variant of overlap2D in BlockHelpers.fs </summary>
/// <param name="a1">The first segment.</param>
/// <param name="a2">The second segment.</param>
let perpendicularOverlap2D ((a1, a2): XYPos * XYPos) ((b1, b2): XYPos * XYPos) : bool =
    let overlapX = overlap1D (a1.X, a2.X) (b1.X, b2.X)
    let overlapY = overlap1D (a1.Y, a2.Y) (b1.Y, b2.Y)
    (overlapX || overlapY)
    && not (overlapX && overlapY)

/// <summary>
/// T3R: The number of distinct pairs of segments that cross each other at right angles.
/// Does not include 0 length segments or segments on same net intersecting at one end, or
/// segments on same net on top of each other. Count over whole sheet.
/// This can be potentially expanded to include more cases by modifying List.filter with more conditions
/// </summary>
/// <param name="model">The SheetT.Model to count the right angle crossings of.</param>
/// <returns>The number of distinct pairs of segments that cross each other at right angles.</returns>
// Corner cases where this won't work: any L shaped crossing that results in a  + style configuration, even though wires do not actually cross at right angles
let countVisibleSegsPerpendicularCrossings (model: SheetT.Model) =
    let wireModel = model.Wire // Get all the wires from the Wire model
    let wires = Map.toList wireModel.Wires

    // Get all the absolute segments from each wire
    let segments =
        wires
        |> List.collect (fun (_, wire) ->
            getAbsSegments wire
            |> List.map (fun seg -> (wire, seg)))

    // Generate all pairs of segments
    let allPairs =
        segments
        |> List.mapi (fun i seg1 ->
            segments
            |> List.skip (i + 1)
            |> List.map (fun seg2 -> (seg1, seg2)))
        |> List.concat

    // Filter the pairs to get only those that belong to wires that do not share the same InputPortId or OutputPortId
    // and intersect at a 90º angle
    let filteredPairs =
        allPairs
        |> List.filter (fun ((wire1, seg1), (wire2, seg2)) ->
            wire1.InputPort <> wire2.InputPort
            && wire1.OutputPort <> wire2.OutputPort
            && overlap2D (seg1.Start, seg1.End) (seg2.Start, seg2.End))
    // Return the count of filtered pairs
    List.length filteredPairs

// --------------------------------------------------- //
//                     T4R Functions                   //
// --------------------------------------------------- //

///<summary>
/// T4R High-Level Helper: For N wires in the same-net, starting from the same port, count the total length
/// of segments that all overlap together multiplied by (N-1).
/// This value (scaled overlap count) is used to offset/substract from total number of segments. Count over whole sheet.
/// At the bottom of the function definition is an in-depth explanation of the algorithm.
/// See T4R getApproxVisibleSegmentsLength function for more information.
///
///             visible_Segments_Length
///             ≈ getApproxVisibleSegmentsLength
///             = totalSegmentsLength - (N-1) * sharedNetOverlapLength
///             = totalSegmentsLength - getSharedNetOverlapOffsetLength
///
/// </summary>
/// <param name="model">The model to calculate the shared net overlap offset length of.</param>
/// <returns>The shared net overlap offset length of the model.</returns>
let getSharedNetOverlapOffsetLength (model: SheetT.Model) =

    //####################### HELPER FUNCTIONS #######################

    /// Helper for T4R that takes in a group of wires and returns the length of the wire with the least number of segments
    let getMinSegments (wireGroup: (Wire * ASegment list) list) =
        wireGroup
        |> List.minBy (fun (_, absSegments) -> List.length absSegments)
        |> snd
        |> List.length

    /// Helper for T4R that outputs the partial overlapping distance for a list of segments share either the same start or end position,
    /// Accepts a list of absolute segments. Function assumes that all segments either share the same start or end position. If no overlap, returns 0.0
    // Algorithm: The function determines if all the segments are moving horizontally or vertically, by checking ASegment.orientation
    // Then it checks if if every Asegment.Segment.Length is the same sign, then return the minimum absolute value
    // if not, return 0.0 (no segments with shared orientations will overlap if they are travelling in opposite magnitudes)
    let partialOverlapDistance (absSegments: ASegment list) : float =
        let (allAligned: bool) =
            absSegments
            |> List.map (fun absSegment -> absSegment.Orientation)
            |> List.distinct
            |> List.length = 1
        let (allSameSign: bool) =
            absSegments
            |> List.map (fun absSegment -> absSegment.Segment.Length >= 0.0)
            |> List.distinct
            |> List.length = 1
        if allAligned && allSameSign then
            // get minimum absolute value of all the segment lengths
            absSegments
            |> List.map (fun absSegment -> absSegment.Segment.Length)
            |> List.map abs
            |> List.min
        else
            0.0

    /// Helper for T4R that find the diverging index of a group of wires, d, for a list of (Wire * ASegment List),
    /// and also determines if d is accessible in the shortest wire of the group.
    /// Returns a tuple of (d, dExists).
    /// All wires's segments from index 0 up to index d-1 are identical, i.e. have the same start and end points.
    /// At index d, this is when all the wires' segments at index d starts to have different end points, i.e. they diverge
    let findDivergingIndex (wireGroup: (Wire * ASegment list) list) (isOutput) =
        // FSharp doesn't support negative indexing, so this is an implementation
        let inline accessSegment (absSegments: ASegment list) (index: int) isOutput =
            if (isOutput) then
                absSegments.[index]
            else
                absSegments.[absSegments.Length - index - 1]

        let minSegments = getMinSegments wireGroup
        let d =
            // iterate up d
            [ 0 .. minSegments - 1 ]
            |> Seq.tryFindIndex (fun d ->
                let startsAreSame =
                    wireGroup
                    |> List.map (fun (_, absSegments) -> (accessSegment absSegments d isOutput).Start) // get a list of all dth segment's start points
                    |> List.distinct
                    |> List.length = 1
                // check if all are the same, i.e. a list of unique start points will have length 1 since all are the same
                let endsAreSame =
                    wireGroup
                    |> List.map (fun (_, absSegments) -> (accessSegment absSegments d isOutput).End) // get a list of all dth segment's end points
                    |> List.distinct
                    |> List.length = 1
                // check if all are the same, i.e. a list of unique start points will have length 1 since all are the same
                not (startsAreSame && endsAreSame))
        // We will find the first index, d, where all wires' 0th to (d-1)th segments share the same start and end points
        // but the dth segments have different end points
        match d with
        | Some d -> (d, (d <= minSegments - 1))
        | None -> minSegments - 1, true

    //####################### START OF FUNCTION #######################

    // Get wires and their absolute segments
    let wires = Map.toList (removeWireInvisibleSegments (model.Wire.Wires))
    let wiresWithAbsSegments =
        wires
        |> List.map (fun (_, wire) -> (wire, getAbsSegments wire))

    // get and group wires that share the same input and output ports
    let outputGroupedWires =
        wiresWithAbsSegments
        |> List.groupBy (fun (wire, _) -> (wire.OutputPort))
        // get rid of InputPortId groups that have less than or equal to 1 wire
        |> List.filter (fun (_, wireGroup) -> List.length wireGroup > 1)
        |> List.map (fun (_, wireGroup) -> wireGroup)

    let inputGroupedWires =
        wiresWithAbsSegments
        |> List.groupBy (fun (wire, _) -> (wire.InputPort))
        // get rid of InputPortId groups that have less than or equal to 1 wire
        |> List.filter (fun (_, wireGroup) -> List.length wireGroup > 1)
        |> List.map (fun (_, wireGroup) -> wireGroup)

    /// T4R helper to find the overlapping segment lengths of a group of wires.
    /// Also has an input isOutput to check if we are calculating that share an output or input port, as the list will be accessed from different ends.
    /// This is done by finding how many segments are identical (in start and end positions) amongst wires and calculating the lengths of these overlaps.
    /// After a sequence of identical segments, they will diverge, but might partially overlap. This partial overlapping is calculated at the dth index using partialOverLapDistance
    let calculateOverlapLengths (wireGroups: (Wire * ASegment list) list) isOutput =
        if wireGroups = [] then //to prevent errors
            0.0
        else
            let d, dExists = findDivergingIndex wireGroups isOutput
            printf "D: %A" d
            let distanceTravelledByIdenticalSegs =
                // if starting from output, take first wire, calculate length of segments in 0th to (d-1)th index
                // if starting from input, take first wire, calculate length of segments from the last to (last+1-d)th index

                match d > 0, isOutput with
                | false, _ -> 0.0
                | true, true ->
                    wireGroups[0]
                    |> snd
                    // cut the wire to the (d-1)th segment, take the first (d) elements
                    |> List.take (d)
                    |> List.fold (fun acc segment -> acc + euclideanDistance segment.Start segment.End) 0.0
                | true, false ->
                    wireGroups[0]
                    |> snd
                    // take the last (d) elements of the wire, slice out
                    |> List.skip ((snd wireGroups[0]).Length - d)
                    |> List.fold (fun acc segment -> acc + euclideanDistance segment.Start segment.End) 0.0

            let distanceTravelledByDthSegs =
                // get all wires' (dth)th segments
                if dExists then
                    wireGroups
                    |> List.map (fun wireGroup ->
                        match (snd wireGroup) |> List.tryItem d with
                        | Some item -> item
                        | None -> failwith "Index out of range") // will never happen
                    // find the distances travelled by the dth segments
                    // we use Euclidean distance, although we know that the segments are either horizontal or vertical
                    |> partialOverlapDistance
                else
                    0.0
            // return the shared overlap length
            (distanceTravelledByIdenticalSegs
             + distanceTravelledByDthSegs)
            * (float (List.length wireGroups - 1))

    // Calculate the shared overlap lengths for both input and output grouped wires, and sum them
    let outputOverlapLengths =
        let outputOverlaps =
            outputGroupedWires
            |> List.map (fun wireGroup -> calculateOverlapLengths wireGroup true)
        let inputOverlaps =
            inputGroupedWires
            |> List.map (fun wireGroup -> calculateOverlapLengths wireGroup false)
        inputOverlaps @ outputOverlaps |> List.sum

    outputOverlapLengths
(*
    Algorithm: find the diverging index, or dth index, where segments of all wires up to index d-1th are identical, i.e. have the same start and end points.
    The dth segments are when the wires start to diverge.
    If wires net is at the input ports, the wires share a same end point,
    the dth segment starting from the end, is the first instance of the segments not sharing a start point

    Examples with 3 wires sharing an output port
    Example 1:  [(0,0, 0,1)], [(0,0 to 0,3)], [(0,0 to 0,5)],
    Shared overlap length is 1.

    Example 2: [(0,0 to 0,-1)], [(0,0 to 0,3)], [(0,0 to 0,5)],
    Shared overlap length is 0.

    Example 3: [(0,0 to 0,3), (0,3 to 1,3)], [(0,0 to 0,2), (0,3 to 5,3)], [(0,0 to 0,3), (0,3 to 7,3)],
    Shared overlap length is 2.

    Example 4: [(0,0 to 0,3), (0,3 to 1,3)], [(0,0 to 0,3), (0,3 to 5,3)], [(0,0 to 0,3), (0,3 to 7,3)],
    Shared overlap length is 3 + 1 = 4.

    Example 5 consists of 2 wires sharing an input port (and share the end point)
    Don't get confused! Input points share the same end
    [(4,5 to 4,3), (4,3 to 9,3), (9,3 to 9,5)],
                  [(4,3 to 9,3), (9,3 to 9,5)]
    Starting from the end, shared overlap length is 2 + 5 = 7

    If wires net is at the output ports, the wires share a same start point,
    the dth segment from the start, the first instance of the segments not sharing an end point
    Calculate the distance travelled by the kth segments for all wires (they will be the same for all, so use the first wire).

    The (k+1)th segment of each wires will have a same starting point and same travel direction, but different end points.
    The shared overlap length is the minimum of the end points of the (k+1)th segments of all wires.
    *)

/// <summary> T4R High-Level Helper: Get total length of wire segments in a model </summary>
/// <param name="model">The model to calculate the total length of wire segments of.</param>
/// <returns>The total length of wire segments in the model, even counting overlaps.</returns>
let getTotalSegmentsLength (model: SheetT.Model) =
    let wireModel = model.Wire
    wireModel.Wires
    |> removeWireInvisibleSegments
    |> Map.fold
        (fun acc _ wire ->
            wire.Segments
            |> List.sumBy (fun seg -> abs (seg.Length)))
        0.0

/// <summary>
/// T4R Top-Level function. Returns the sum of wiring segment length, counting only one wire when there are N same-net
/// segments overlapping (this is the visible wire length on the sheet). Count overthe  whole sheet.
/// It is approximated by (for n wires in a net):
///
///              totalSegmentsLength - (N-1) * sharedNetOverlapLength
///           => totalSegmentsLength - getSharedNetOverlapOffsetLength
///
/// To calculate the exact length accounting for overlaps is extremely complex (it is actively researched in computational geometry)
/// and becomes even more computational if considering x overlaps of 1..x..n
/// </summary>
/// <param name="model">The model to calculate the visible wire length of.</param>
/// <returns>The approximate visible wire length of the model.</returns>
/// <remarks>
/// Limitations: This function uses an approximation, it only considers overlaps from one 'patch' of contiguous segments of wires that start from the same port
/// It will not count partial overlaps of less than N wires, nor subsequent overlappings after the first patch of contiguous segments (in the case wires diverge and later return to overlap)
/// So this value is not always exact. But since we assume most overlaps happen once with all wires starting from a shared port,
/// the approximation below is good enough, since it is likely to be used as a heuristic for wiring complexity.
/// </remarks>
let getApproxVisibleSegmentsLength (model: SheetT.Model) =
    (getTotalSegmentsLength model)
    - (getSharedNetOverlapOffsetLength model)

// --------------------------------------------------- //
//                     T5R Function                    //
// --------------------------------------------------- //

///<summary> T5R: Number of visible wire right-angles. Count over whole sheet. Note that this also counts wires that overlap </summary>
/// <param name="model">The model to count the visible wire right-angles of.</param>
/// <returns>The number of visible wire right-angles.</returns>
let countVisibleRAngles (model: SheetT.Model) =
    let wireModel = model.Wire
    wireModel.Wires
    |> removeWireInvisibleSegments
    // for each wire, get its wire.Segments, list, find its length minus 1
    |> Map.fold
        (fun acc _ wire ->
            wire.Segments.Length - 1
            // add the length of the segments list minus 1 to the accumulator
            + acc)
        // initialise the accumulator to 0
        0

// --------------------------------------------------- //
//                     T6R Functions                   //
// --------------------------------------------------- //

/// <summary>
/// T6R Function: The zero-length segments in a wire with non-zero segments on either side that have lengths of opposite signs lead to a wire retracing itself.
/// </summary>
/// <param name="model">The model to get the retracing segments and intersections of.</param>
/// <return>
/// Returns a list tuple with:
/// 1. a list of all segments that retrace
/// 2. a list that includes adjacent segments to the retracing that starts(intersects) a symbol
/// </return>
/// <remarks>
/// Algorithm:
/// Repeat for every wire on the sheet:
/// 1. Get the absolute segments of the wire
/// 2. Look for a 3-segment pattern of a non-zero segment followed by a zero segment followed by a non-zero segment that has opposite signs to the previous
/// 3. Add them as a list to a list of retracing segments
/// 4. Add them as a list to a second retracing segments, ALSO including the segment before and after the pattern if it exists and is nonzero (max 5 segments in total)
/// 5. For the 2nd list, run them through the function findSymbolIntersections
///
/// Note that this function returns lists of segment lists, using a list of segments to represent a 'group' of retracing segments
/// To fully satisfy T6R (it also asks for a count), we need to get a function that returns all unique segments within the list.
/// See function <c>countUniqRetracingSegmentsAndIntersects</c> where this is implemented.
/// </remarks>
let getRetracingSegmentsAndIntersections (model: SheetT.Model) =
    let wireModel = model.Wire
    wireModel.Wires
    |> Map.fold
        (fun (retracingSegments_acc, retracingSegmentsWithIntersections_acc) _ wire ->
            let segments = wire.Segments

            // run through a window size of 3 segments, and look for the pattern
            let retracingSegments =
                segments
                |> List.windowed 3
                |> List.filter (fun segs ->
                    let (seg1, seg2, seg3) = (segs.[0], segs.[1], segs.[2])
                    (seg1.Length <> 0.0)
                    && (seg2.Length = 0.0)
                    && (seg3.Length <> 0.0)
                    && (seg1.Length * seg3.Length < 0.0)) // nonzero segments must have opposite signs

            // run through window size of 3, and try to find the segment before and after the pattern if it exists and is nonzero
            let retracingSegmentsWithIntersections =
                segments
                |> List.windowed 3
                |> List.mapi (fun i segs ->
                    let (seg1, seg2, seg3) = (segs.[0], segs.[1], segs.[2])
                    if
                        (seg1.Length <> 0.0) // look for a pattern if NONZERO, ZERO, NONZERO
                        && (seg2.Length = 0.0)
                        && (seg3.Length <> 0.0)
                        && (seg1.Length * seg3.Length < 0.0) // nonzero segments must have opposite signs
                    then // look for the segment before and after the pattern if it exists and is nonzero
                        // Identify some NONZERO, (NONZERO, ZERO, NONZERO), some NONZERO (pattern in brackets)
                        let prev =
                            match (i > 0 && segments.[i - 1].Length <> 0.0) with
                            | true -> [ segments.[i - 1] ]
                            | false -> []

                        let next =
                            match
                                (i < List.length segments - 3
                                 && segments.[i + 3].Length <> 0.0)
                            with
                            | true -> [ segments.[i + 3] ]
                            | false -> []
                        Some(prev @ segs @ next)
                    else
                        None)
                |> List.choose id // filter out None values
                |> List.filter (fun segs ->
                    // get the first segment since all of them share the same WId, lookup in wireModel and get the corresponding wire,
                    // create a test wire with the retracing segments + adjacent segments if they exist
                    let testWire = { wireModel.Wires[segs[0].WireId] with Segments = segs }
                    // now check if that testwire intersects with any symbols by using findSymbolIntersections, make sure its output list is non-empty
                    // if non empty, then filter condition evals to true, and the segment list is added to the accumulator retracingSegmentsWithIntersections_acc
                    testWire
                    |> findWireSymbolIntersections wireModel
                    |> List.isEmpty
                    |> not)

            (retracingSegments @ retracingSegments_acc,
             retracingSegmentsWithIntersections
             @ retracingSegmentsWithIntersections_acc))
        ([], [])
(*
 Note that this can also apply
 at the end of a wire (where the zero-length segment is one from the end). This is a
 wiring artifact that should never happen but errors in routing or separation can
 cause it. Count over the whole sheet. Return from one function a list of all the
 segments that retrace, and also a list of all the end of wire segments that retrace so
 far that the next segment (index = 3 or Segments.Length – 4) - starts inside a symbol.
*)

/// <summary>
/// T6R Function Variant.
/// The original T6R function, <c>getRetracingSegmentsAndIntersections</c> returns a tuple with:
/// 1. a list of all segments that retrace
/// 2. a list that includes adjacent segments to the retracing that starts(intersects) a symbol
/// </summary>
/// <remarks>
/// This variant function, <c>countUniqRetracingSegmentsAndIntersects</c>, counts the unique segments where this occurs in a sheet.
/// This is a helpful heuristic to count wiring artefacts, to test routing algorithms
/// </remarks>
/// <param name="model">The model to count the retracing segments and intersections of.</param>
/// <returns>
/// The number of unique segments that retrace, and the number of unique segments in a group that retrace and intersect with a symbol.
/// </returns>
let countUniqRetracingSegmentsAndIntersects (model: SheetT.Model) =
    let (retracingSegments, retracingSegmentsWithIntersections) =
        getRetracingSegmentsAndIntersections model

    let uniqRetracingSegments =
        retracingSegments
        |> List.collect id
        |> List.distinct
        |> List.length
    let uniqRetracingSegmentsWithIntersections =
        retracingSegmentsWithIntersections
        |> List.collect id
        |> List.distinct
        |> List.length

    (uniqRetracingSegments, uniqRetracingSegmentsWithIntersections)
