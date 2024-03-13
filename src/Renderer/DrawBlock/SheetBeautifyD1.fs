module SheetBeautifyD1

open DrawModelType
open CommonTypes
open DrawModelType.SymbolT
open DrawModelType.SheetT
open DrawModelType.BusWireT
open ModelType
open Optics
open Optics.Operators
open Helpers
open SymbolHelpers
open SheetBeautifyHelpers
open BlockHelpers
open Symbol
open BusWireRoute
open BusWire
open BusWireUpdateHelpers
open SheetUpdateHelpers

// --------------------------------------------------- //
//                         INFO!                       //
// --------------------------------------------------- //
// Some of these functions are duplicates of BeautifySheetHelpers since they are used B1-8 and T1-6
// They have been included here for assessment purposes since I developed them to be useful in other parts of the project

///<summary>
/// T2R, T3R Helper, but can be used to minimise invisible segments for D1.
/// Remove all invisible segments from wires on a sheet.Wire.Wires.
/// </summary>
/// <param name="wires">Map of wires indexed by ConnectionID to remove invisible segments from.</param>
/// <returns>Map of wires indexed by ConnectionID with invisible segments removed.</returns>
// visibleSegments would've worked, but outputs an XYPos list, which is a format that isn't well accepted by the other functions and types.
// This is achieved by utilising existing helper function segmentsToIssieVertices to convert all segments to a list of vertices.
// It is then very easy to remove duplicate vertices.
// We can utilise another helper function issieVerticesToSegments to convert vertices back to segments, and create new wires.
let removeWireInvisibleSegments' (wires: Map<ConnectionId, Wire>) =
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
/// T3R helper, but useful for calculations of wire crossings:
/// Returns true if two 1D line segments intersect at a 90º angle. Takes in two segments described as point-to-point.
/// A variant of overlap2D in BlockHelpers.fs
/// </summary>
/// <param name="a1">The first segment.</param>
/// <param name="a2">The second segment.</param>
let perpendicularOverlap2D' ((a1, a2): XYPos * XYPos) ((b1, b2): XYPos * XYPos) : bool =
    let overlapX = overlap1D (a1.X, a2.X) (b1.X, b2.X)
    let overlapY = overlap1D (a1.Y, a2.Y) (b1.Y, b2.Y)
    (overlapX || overlapY)
    && not (overlapX && overlapY)

(*

//-----------Notes for D1 From Ed-----------//
----------Dealing With Separation---------

Typically one problem humans find when trying to tidy up schematics is that there is not enough room in some parts of the sheet,
with components too close,  and everything needs to be rearranged.  Doing this automatically is problematic because changing relative
positions of components (except for a small amount) is multiply constrained.

Still there is one easy transform in line with "do no harm" which is to scale the whole sheet - already done by rotatescale if you select all components.

What might require scaling the sheet - uniformly magnifying the distances between all components?
If the "correct" positions of components to straighten connections cause symbol overlaps

If the separation between (parallel) wire segments and component edges, or segments and segments, is too small.
The wire separation algorithm should ensure that component-wire separation will be equally a problem is wire-wire separation is a problem -
so you probably need to consider only that. Scaling the whole sheet can make it difficult to see everything - if too much scaling is done -
so some sort of cap would maybe be necessary.

----------Dealing With Separation---------
2 quality metrics: no overlapping symbols, try to straighten wires as much as possible

The no overlapping symbols requirement is an extreme form of an enough separation requirement where for two adjacent parallel
symbol edges separated by distance d with n wires running through. The gap between them the separation  is:

d/(n+1)

Separations close to a defined minimum can be included as penalties in the objective function. Obviously separations below some
minimum value should not allowed, so a nonlinear dependence of objective function on separation could make sense and allow optimisation
techniques to be used.

*)

/// Constant that decides if a wire is classified as almost-straight, if its longest segment in the minority direction is shorter than this length
let maxDeviationLengthThreshold = 30.0
/// Constant that decides if a wire is classified as almost-straight, if its overall displacement in the minority direction is shorter than this length
let maxMinorityDisplacementThreshold = 300
// initially wanted to use a ratio, but then it would not help fix shorter wires

/// Helper function to split a list of segments into odd and even segments
let unzipIntoOddAndEvenSegments (segments: Segment list) =
    segments
    |> List.mapi (fun i x -> (i % 2 = 0, x))
    |> List.partition fst
    |> fun (odd, even) -> (List.map snd odd, List.map snd even)

/// Function to flip the orientation of a wire
// should be integrated into Issie with the not operator
let flipOrientation (orientation: Orientation) =
    match orientation with
    | Horizontal -> Vertical
    | Vertical -> Horizontal

/// Function that detects if a wire is almost straight
let checkAlmostStraightWire (wire: BusWireT.Wire) =
    // Get list of even segments and odd segments of the wire. Note: we get rid of invisible segments
    let wireWithInvisSegmentsRemoved = (removeSingleWireInvisibleSegments wire)
    let oddList, evenList =
        wire
        |> removeSingleWireInvisibleSegments
        |> (fun wire -> unzipIntoOddAndEvenSegments wire.Segments)
    let oddDisplacement =
        oddList
        |> List.sumBy (fun segment -> segment.Length)
    let evenDisplacement =
        evenList
        |> List.sumBy (fun segment -> segment.Length)

    let majorityDisplacement, isMajorityDirectionHoriz =
        if oddDisplacement >= evenDisplacement then
            oddDisplacement, wire.InitialOrientation = Horizontal
        // if odd segments majority and wire starts from horiz (which is odd), then horiz is majority
        else
            evenDisplacement, wire.InitialOrientation = Vertical
    // if even segments majority and wire starts from vert (which is odd), then horiz is majority

    // can't be straightened if there are less than 2 segments OR segment length is even
    if
        (wireWithInvisSegmentsRemoved.Segments.Length < 2)
        || (wireWithInvisSegmentsRemoved.Segments.Length % 2 = 0)
    then
        false
    else
        // maxDeviationLength is the longest segment in the minority direction
        // maxMinorityDisplacement is the overall displacement in the minority direction
        match wire.InitialOrientation, isMajorityDirectionHoriz with
        | Horizontal, true -> // first seg horiz, majority horiz, will deviate vertically, which will be the even segments
            let maxDeviationLength =
                (evenList
                 |> List.maxBy (fun segment -> abs (segment.Length)))
                    .Length
            // let ratio = (abs (oddDisplacement) / abs (maxDeviationLength))
            // printf "Ratio: %A" ratio
            // ratio > straightenRatioTolerance
            // || (maxDeviationLength < 30 && oddDisplacement < 100)
            maxDeviationLength < maxDeviationLengthThreshold
            && evenDisplacement < maxDeviationLengthThreshold
        | Vertical, false -> // first seg vertical, majority vertical, will deviate horizontally, which will be the even segments
            let maxDeviationLength =
                (evenList
                 |> List.maxBy (fun segment -> abs (segment.Length)))
                    .Length
            // let ratio = (abs (oddDisplacement) / abs (maxDeviationLength))
            // printf "Ratio: %A" ratio
            // ratio > straightenRatioTolerance
            // || (maxDeviationLength < 30 && oddDisplacement < 100)
            maxDeviationLength < maxDeviationLengthThreshold
            && evenDisplacement < maxDeviationLengthThreshold
        | _, _ -> false

/// To show on DeveloperModeView SheetStats
let countAlmostStraightWiresOnSheet (sheetModel: SheetT.Model) =
    let straightWires =
        sheetModel.Wire.Wires
        |> Map.filter (fun _ wire -> checkAlmostStraightWire wire)
    straightWires.Count

(*  Cases for checkAlmostStraightWire
                                                     __________            __________
            ______                                  |         | No        |         |
    _______|     |________ almost straight          |         |           |         |
                                                                 _________|         |__________
                    __________________                          |                             | No
    _______________|      almost straight

    Todo: make sure it won't detect edge cases, such as a tall user-generated stepladder wire that moves upwards in small increments.
    this can be done by checking the length of the list of minority segments

    |               |
    |               |                                                               _________
    |               ____                     ____                                           |
    |_                 |                         |  No            __________                |
      |                |   almost straight       |                         |                |
      |                |                         |                         | No             |___________No
      |            ––––                      ––––
      |            |
      |            |
almost straight

    algo: check whether the wire travels furthest horizontally or vertically. This is the called the majority direction,
    and the other direction is the minority direction.

    * Not to be confused with the number of segments, e.g. there can be more vertical segments than horizontal segments, but if
    the horizontal segments cover a greater displacement, the majority direction is horizontal.

    Next, calculate the majority displacement, which is the sum of the segment lengths in the majority direction.

    After determining majority direction, ake sure the first and last segments are also travelling
    in the majority direction. Can be done by checking initialOrientation, and then making sure the length is odd.
    (in odd length, first and last segments will be in majority direction)
    If even, discard, can't straighten a wire that is 'diagonal/L' shaped.

    Then, check for the maximum deviation in the minority direction. If the deviation-to-majority-displacement
    ratio is less than a certain threshold, then the wire is almost straight.

*)

/// Helper that to get a Symbol from PortId. Will search the sheet
// Quite surprising this wasn't created already!
let getSymbolFromPortID (portId: string) (model: SheetT.Model) =
    // PortId type is either an InputPortId or an OutputPortId
    let portIdString = portId.ToString()
    model.Wire.Symbol.Symbols
    |> Map.values
    |> Seq.toList
    |> List.tryFind (fun symbol ->
        symbol.PortMaps.Orientation.Keys
        |> Seq.exists (fun key -> key = portIdString))

/// Function helper that checks if a symbol is of a component type that has a single port
let checkIfSinglePortComponent (symbol: Symbol) =
    match symbol.Component.Type with
    | Input1 _
    | Input _ // legacy type, still used?
    | Output _
    | Viewer _
    | Constant1 _
    | NotConnected -> true
    | _ -> false

/// Function helper that checks if a port is of a net that has a single wire connected to it
let checkSingleNet (portId: OutputPortId) (sheetModel: SheetT.Model) : bool =
    //model.Wire.Symbol.OutputPortsConnected[portId] = 1
    // use TryFind
    // printf "symModel.OutputPortsConnected: %A" symModel.OutputPortsConnected
    // match Map.tryFind portId symModel.OutputPortsConnected with
    // | Some 1 -> true
    // | _ -> false

    let nets = partitionWiresIntoNets sheetModel.Wire

    nets
    |> List.tryFind (fun (outputPortID, netlist) -> portId = outputPortID)
    |> Option.map (fun (outputPortID, netlist) -> netlist.Length = 1)
    |> Option.defaultValue false

/// Function helper that checks if the wire is connected to at least 1 symbol that has a single port.
/// Symbols in question: Inputs, Outputs, Viewers, Constants, WireLabels, Not Connected
let checkIfSingularlyConnected (wire: Wire) (model: SheetT.Model) =
    let inputSymbol = getSymbolFromPortID (wire.InputPort.ToString()) model
    let outputSymbol = getSymbolFromPortID (wire.OutputPort.ToString()) model

    // if either Symbol.Component.ComponentType is of Input1, Output, Viewer, Constant1, NotConnected
    // then the wire is singly connected
    match inputSymbol, outputSymbol with
    | Some inputSymbol, Some outputSymbol ->
        // printf "cond 1: %A" (checkIfSinglePortComponent inputSymbol
        if
            checkIfSinglePortComponent inputSymbol
            || (checkIfSinglePortComponent outputSymbol
                && (checkSingleNet wire.OutputPort model))
        then
            true
        else
            false
    | Some inputSymbol, _ -> checkIfSinglePortComponent inputSymbol
    | _, Some outputSymbol ->
        (checkIfSinglePortComponent outputSymbol
         && (checkSingleNet wire.OutputPort model))
    | _ -> false
(* algo steps

get all wires in the model
get the two symbols that the wire is connected to by InputPortId and OutputPortId

draft:
- To ensure InputPortId is singly connected: get the symbol that the symbol port is connected to.
    - Assume that InputPorts only have a single wire. Then check if the symbol with this inputport
     is singly connected. Do this by looking up the symbol's other portid in the SymbolT.model's
     InputPortsConnected and OutputPortsConnected. If they do not exist then the wire links to a symbol's
     input that is singly connected.
- To ensure OutputPortId is singly connected:
    - use isWireInNet in BlockHelpers to check if the net has more than one wire. If so, output port wire is
    connected to is not singly connected. Can skip the other conditions
    - If the net is singular, then check the symbol of the output port– make sure all its ports are not connected anywhere
    else. Check the portids in the SymbolT.model's InputPortsConnected and OutputPortsConnected, same as above

shortcut:
- Note: these cases above apply to partially-done circuits where not all ports are being utilised by wires in the sheet!
- This will help with beautifying a partially-done sheet. Otherwise, it's much faster immediately check symbols that
   only have one port. Symbols in question: Inputs, Outputs, Viewers, Constants, WireLabels, Not Connected
- After all, completed sheets will have all ports connected to wires (or will have used Not Connected Stopper )
*)
/// Helper to find if a wire is singly connected to symbols that have a single port, and returns the symbols in a list.
/// Note that list can be empty
let findSinglyConnectedSymsByWire (wire: Wire) (sheetModel: SheetT.Model) =
    // will return (symbol, true) if the symbol is an input, (symbol, false) if the symbol is an output
    // it is important to distingush between the two, because when correcting the wire bend, inputs are
    // moved in opposite direction to outputs, and startpos is modified instead of endpos
    let inputSymbol = getSymbolFromPortID (wire.InputPort.ToString()) sheetModel
    let outputSymbol = getSymbolFromPortID (wire.OutputPort.ToString()) sheetModel

    // if either Symbol.Component.ComponentType is of Input1, Output, Viewer, Constant1, NotConnected
    // then the wire is singly connected
    match inputSymbol, outputSymbol with
    | Some inputSymbol, Some outputSymbol ->
        match
            checkIfSinglePortComponent inputSymbol,
            (checkIfSinglePortComponent outputSymbol
             && (checkSingleNet wire.OutputPort sheetModel))
        with
        | true, true -> [ (inputSymbol, true); (outputSymbol, false) ]
        | true, false -> [ (inputSymbol, true) ]
        | false, true -> [ (outputSymbol, false) ]
        | _ -> []
    | Some inputSymbol, _ ->
        if (checkIfSinglePortComponent inputSymbol) then
            [ (inputSymbol, true) ]
        else
            []
    | _, Some outputSymbol ->
        if
            (checkIfSinglePortComponent outputSymbol
             && (checkSingleNet wire.OutputPort sheetModel))
        then
            [ (outputSymbol, false) ]
        else
            []
    | _ -> []

/// Test function to show on Sheet Stats' developer mode
let countSinglyConnectedWires (model: SheetT.Model) =
    model.Wire.Wires
    |> Map.filter (fun _ wire -> checkIfSingularlyConnected wire model)
    |> Map.count

/// Function to find the minority displacement of wire (the displacment in the direction that not travelled the furthest by the wire)
let getMinorityWireDisplacementAndOrientation (wire: Wire) =

    let wireWithInvisSegmentsRemoved = (removeSingleWireInvisibleSegments wire)
    let oddList, evenList =
        wire
        |> removeSingleWireInvisibleSegments
        |> (fun wire -> unzipIntoOddAndEvenSegments wire.Segments)
    let oddDisplacement =
        oddList
        |> List.sumBy (fun segment -> segment.Length)
    let evenDisplacement =
        evenList
        |> List.sumBy (fun segment -> segment.Length)

    let minorityDisplacement, MinorityDirection =
        if oddDisplacement >= evenDisplacement then
            evenDisplacement, flipOrientation wire.InitialOrientation
        // if odd segments majority and wire starts from horiz (which is odd), then horiz is majority, vert is minority
        else
            oddDisplacement, wire.InitialOrientation
    // if even segments majority and wire starts from vert (which is odd), then horiz is majority, vert is minority

    minorityDisplacement, MinorityDirection

/// Function to find the majority displacement of wire (the displacment in the direction that travelled the furthest by the wire)
let getMajorityDisplacementWireAndOrientation (wire: Wire) =
    let wireWithInvisSegmentsRemoved = (removeSingleWireInvisibleSegments wire)
    let oddList, evenList =
        wire
        |> removeSingleWireInvisibleSegments
        |> (fun wire -> unzipIntoOddAndEvenSegments wire.Segments)
    let oddDisplacement =
        oddList
        |> List.sumBy (fun segment -> segment.Length)
    let evenDisplacement =
        evenList
        |> List.sumBy (fun segment -> segment.Length)

    let majorityDisplacement, MajorityDirection =
        if oddDisplacement >= evenDisplacement then
            oddDisplacement, wire.InitialOrientation
        // if odd segments majority and wire starts from horiz (which is odd), then horiz is majority
        else
            evenDisplacement, flipOrientation wire.InitialOrientation
    // if even segments majority and wire starts from vert (which is odd), then horiz is majority

    majorityDisplacement, MajorityDirection

/// optic to access the initial orientation of a wire
let initialOrientation_: Lens<Wire, Orientation> =
    Lens.create (fun m -> m.InitialOrientation) (fun s m -> { m with InitialOrientation = s })

/// optic to access the StartPos of a wire
let startpos_: Lens<Wire, XYPos> =
    Lens.create (fun m -> m.StartPos) (fun s m -> { m with StartPos = s })

/// when straightening/cleaning up wires singly connected to symbols, we need to keep track of:
/// the symbol to be moved,
/// the wire that is connected to the symbol,
/// the offset that needs to be moved by,
/// and if the connected port is an input. This will determine how to shift the StartPos of the wire
type CleanUpRecord =
    { Symbol: Symbol
      Wire: Wire
      Offset: XYPos
      IsPortInput: bool
      MajorityDisplacementOffset: float }

/// Function to move a symbol according to its cleanUpRecord
//  smartAutoroute still causes small artefacts so this function manually creates a straight wire, recycling existing WId
let moveSymbolWireCleanUpRecord (cleanUpRecord: CleanUpRecord) =
    let majorityDisplacement, MajorityDirection =
        (getMajorityDisplacementWireAndOrientation cleanUpRecord.Wire)

    // just to invert the direction
    let directionChangeMultiplier =
        (if cleanUpRecord.IsPortInput then
             -1.0
         else
             1.0)

    let newPortStartPos =
        if cleanUpRecord.IsPortInput then
            cleanUpRecord.Wire.StartPos
        else
            cleanUpRecord.Wire.StartPos + cleanUpRecord.Offset

    let newWire =
        cleanUpRecord.Wire
        |> Optic.set
            (segments_)
            // assume that there are at least two segments, we can discard the rest.
            (cleanUpRecord.Wire.Segments // make a wire that consists of one big segment in the majority direction, then make nubs
             |> List.mapi (fun i segment ->
                 match MajorityDirection, i = 0, i = 1 with
                 | Horizontal, true, false ->
                     Some
                         { segment with
                             Length =
                                 (majorityDisplacement
                                  + cleanUpRecord.MajorityDisplacementOffset) }
                 | Vertical, false, true ->
                     Some
                         { segment with
                             Length =
                                 (majorityDisplacement
                                  + cleanUpRecord.MajorityDisplacementOffset) }
                 | _, _, _ -> None)
             |> List.choose id
             |> makeEndsDraggable)
        |> Optic.set initialOrientation_ MajorityDirection
        |> Optic.set startpos_ newPortStartPos // update startpos ONLY if is input

    let newSymbol =
        moveSymbol (cleanUpRecord.Offset * directionChangeMultiplier) cleanUpRecord.Symbol

    newWire, newSymbol
// newSymbol

/// Find the bounding box of a segment intersecting a bounding box
let getSegmentIntersectBBox (box: BoundingBox) segStart segEnd =
    let topLeft =
        if lThanEqualPos segStart segEnd then
            segStart
        else
            segEnd

    let segBBox =
        match abs ((segStart - segEnd).X), abs ((segStart - segEnd).Y) with
        | x, y when x <= XYPos.epsilon -> Some { TopLeft = topLeft; W = 0.0; H = y }
        | x, y when y <= XYPos.epsilon -> Some { TopLeft = topLeft; W = x; H = 0.0 }
        | _, _ -> None // we don't do this for zero length segments

    match segBBox with
    | Some segBBox -> overlapArea2DBox box segBBox
    | _ -> None

/// Find the bounding box of of anything that intersects with a symbol
/// try determine the source of the intersection
// if it is caused by the newSymbol intersecting another symbol, we can easily move the newSymbol to another place and try again
// but if it is caused by the newSymbol intersecting another wire, this is a lot more computationally difficult. Existing helpers
// can help us find if a wire intersects a symbol, and not the other way round. We will have to iterate thru every wire and see if that the bounding
// box returned by the findWireSymbolIntersections matches our newSymbol's bounding box.
// then we will have determined the bbox of the intersection, and can move the newSymbol to another place and try again
let findAllBoundingBoxOfSymbolIntersection (symbol: Symbol) (model: SheetT.Model) =

    let symbolBoundingBox = getBoundingBox model.Wire.Symbol symbol.Id
    let wModel = (updateBoundingBoxes model).Wire // just in case

    let intersectingWiresBbBoxes =
        wModel.Wires
        |> Map.values
        // findWireSymbolIntersections returns a list of bounding boxes of symbols intersected by wire.
        |> Seq.map (fun wire -> (wire, (findWireSymbolIntersections wModel wire)))
        // we have (Wire * BoundingBox list) seq. Now to look through every tuple and get any wire whose bbox list os equal to symbolBoundingBox
        // we might get more than one wire – so get a list
        |> Seq.choose (fun (wire, bboxes) ->
            if
                bboxes
                |> List.exists (fun box -> symbolBoundingBox = box)
            then
                Some wire
            else
                None)
        |> Seq.toList
        |> List.collect (fun wire ->
            // for each wire, we go thru every segment and find the bounding box of the segment that intersects with the symbol
            let wireVertices =
                segmentsToIssieVertices wire.Segments wire
                |> List.map (fun (x, y, _) -> { X = x; Y = y })

            // taken from findWireSymbolIntersections so there is an extra index ignored as _ in List.choose (last line)
            // might need a condition to disqualify the first and last segments (Done)

            let indexes = List.init ((List.length wireVertices) - 2) (fun i -> i + 1)

            let segVertices =
                List.pairwise wireVertices.[1 .. wireVertices.Length - 2]
                |> List.zip indexes // do not consider the nubs
            segVertices

            |> List.choose (fun (i, (segStart, segEnd)) ->
                if ((i = 0) || (i = segVertices.Length - 1)) then
                    None
                else
                    (getSegmentIntersectBBox symbolBoundingBox segStart segEnd)))

    let intersectingSymbolBBoxes =
        model.BoundingBoxes
        |> Map.values
        |> Seq.toList
        // get all boundingBoxes in model not equal to symbolBoundingBox
        |> List.filter (fun (box) -> not (box =~ symbolBoundingBox))
        // see if they overlap with the symbolBoundingBox
        |> List.choose (fun box -> (overlapArea2DBox symbolBoundingBox box))

    intersectingWiresBbBoxes
    @ intersectingSymbolBBoxes

/// Function to clean up almost straight singly connected wires
let cleanUpAlmostStraightSinglyConnWires (model: ModelType.Model) =
    // check if wire is singly connected
    // then check if it is almost straight

    let almostStraightSinglyConnectedWires =
        model.Sheet.Wire.Wires
        |> Map.filter (fun _ wire ->
            checkIfSingularlyConnected wire model.Sheet
            && checkAlmostStraightWire wire)

    /// Produce a list of CleanUpRecords
    /// Find possible wires and symbols to be straightened out, calculate their offset to fix them, and also keep track if the connections
    /// occur at an input port or output port
    let symbolsWireOffsetUpdates: CleanUpRecord list =
        almostStraightSinglyConnectedWires
        |> Map.values
        |> Seq.toList
        |> List.collect (fun wire -> // should I collect, or should I map and choose id
            let (symbolsToMove: list<Symbol * bool>) =
                (findSinglyConnectedSymsByWire wire model.Sheet)
            // will be of length 0, 1, or 2. We just take the first value since moving
            // symbolsToMove consist of tuples (Symbol, bool) where bool is true if the symbol is an input
            if symbolsToMove.Length = 0 then
                []
            else
                match getMinorityWireDisplacementAndOrientation wire with // if deviating vertically, offset y. Negative if input and positive if output
                | y, Vertical ->
                    let offset = { X = 0.0; Y = y }
                    [ { Symbol = symbolsToMove[0] |> fst
                        Wire = wire
                        Offset = offset
                        IsPortInput = symbolsToMove[0] |> snd
                        MajorityDisplacementOffset = 0.0 } ]
                | x, Horizontal -> // if deviating horizontally, offset x. Negative if it's an input port and positive if it output
                    let offset = { X = x; Y = 0.0 }
                    [ { Symbol = symbolsToMove[0] |> fst
                        Wire = wire
                        Offset = offset
                        IsPortInput = symbolsToMove[0] |> snd
                        MajorityDisplacementOffset = 0.0 } ])

    /// helper to update the sheet's existing symbol with a new symbol
    let updateSheetSymWithNewSym (symbol: Symbol) (sheetModel: SheetT.Model) =
        sheetModel
        |> Optic.set (wire_ >-> symbolOf_ symbol.Id) symbol
        |> SheetUpdateHelpers.updateBoundingBoxes // update the bounding boxes for accurate intersection checking

    /// helper to update the sheet's existing wire with a new wire
    let updateSheetWireWithNewWire (wire: Wire) (sheetModel: SheetT.Model) =
        sheetModel
        |> Optic.set (wire_ >-> wires_) (sheetModel.Wire.Wires |> Map.add wire.WId wire)

    let checkIfGainedIntersections (currentModel: SheetT.Model) (newModel: SheetT.Model) =
        ((countIntersectingSymbolPairs newModel)
         <= (countIntersectingSymbolPairs currentModel))
        && ((countVisibleSegsIntersectingSymbols newModel)
            <= (countVisibleSegsIntersectingSymbols currentModel))

    let updatedSheetModel: SheetT.Model =
        symbolsWireOffsetUpdates
        |> List.fold // better way to do this?
            (fun currentSheetModel (cleanUpRecord: CleanUpRecord) ->
                // create a new straight wire
                let newStraightWire, newMovedSymbol = moveSymbolWireCleanUpRecord cleanUpRecord
                let newSheetModel =
                    currentSheetModel
                    |> updateSheetSymWithNewSym newMovedSymbol
                    // |> (fun modelWithNewSymb ->
                    //     updateSheetWireWithNewWire
                    //         (smartAutoroute modelWithNewSymb.Wire cleanUpRecord.Wire)
                    //         modelWithNewSymb)
                    |> updateSheetWireWithNewWire newStraightWire

                // check for intersections. If there are none, then return the new model else return the old model
                if (checkIfGainedIntersections currentSheetModel newSheetModel) then
                    newSheetModel
                else
                    printf "another pass with symbol id %A" cleanUpRecord.Symbol.Id
                    // we do another pass.
                    let intersectingBBoxes =
                        findAllBoundingBoxOfSymbolIntersection newMovedSymbol newSheetModel
                    let newOffset, majorityDisplacementOffset =
                        match cleanUpRecord.IsPortInput, cleanUpRecord.Wire.InitialOrientation with
                        | true, Horizontal ->
                            let addedXOffset =
                                intersectingBBoxes
                                |> List.minBy (fun box -> box.TopLeft.X)
                                |> (fun box -> newMovedSymbol.Pos.X - box.TopLeft.X)
                            // can add another condition to set to zero, cancelling the operatio if we have to move the symbol
                            // too far back
                            // { X = 10.0; Y = 0.0 }, -10.0
                            { X = addedXOffset; Y = 0.0 }, -addedXOffset
                        | false, Horizontal ->
                            let addedXOffset =
                                intersectingBBoxes
                                |> List.maxBy (fun box -> box.TopLeft.X + box.W)
                                |> (fun box -> box.TopLeft.X - newMovedSymbol.Pos.X)
                            { X = -addedXOffset; Y = 0.0 }, addedXOffset

                        | true, Vertical ->
                            let addedYOffset =
                                intersectingBBoxes
                                |> List.minBy (fun box -> box.TopLeft.Y)
                                |> (fun box -> newMovedSymbol.Pos.Y - box.TopLeft.Y)
                            { X = 0.0; Y = addedYOffset }, -addedYOffset
                        | false, Vertical ->
                            let addedYOffset =
                                intersectingBBoxes
                                |> List.maxBy (fun box -> box.TopLeft.Y)
                                |> (fun box -> box.TopLeft.Y - newMovedSymbol.Pos.Y)
                            { X = 0.0; Y = -addedYOffset }, addedYOffset
                    let newCleanUpRecord =
                        { cleanUpRecord with
                            Offset = newOffset
                            Symbol = newMovedSymbol
                            Wire = newStraightWire
                            MajorityDisplacementOffset = majorityDisplacementOffset }
                    let newNewStraightWire, newNewMovedSymbol =
                        moveSymbolWireCleanUpRecord newCleanUpRecord
                    newSheetModel
                    |> updateSheetSymWithNewSym newNewMovedSymbol
                    |> updateSheetWireWithNewWire newNewStraightWire

            // first, calculate how many bends we are saving by straightening CleanUpRecord.Wire
            // before running currentSheetModel, try determine the source of the intersection

            // if it is caused by the newSymbol intersecting another symbol, we can easily move the newSymbol to another place and try again

            // but if it is caused by the newSymbol intersecting another wire, this is a lot more computationally difficult. Existing helpers
            // can help us find if a wire intersects a symbol, and not the other way round. We will have to iterate thru every wire and see if that the bounding
            // box returned by the findWireSymbolIntersections matches our newSymbol's bounding box.
            // then we will have determined the bbox of the intersection, and can move the newSymbol to another place and try again

            )

            model.Sheet

    model |> Optic.set (sheet_) (updatedSheetModel)

(*  More notes: Cases for detecting Straightenable Wires (note we consider more cases than AlmostStraightWires )
                                                         __________
            ______                                      |         | probably not, but is an edge case where ports are vertical (only for custom comps),
    _______|     |________ can be straightened          |         | plus the first and last segment travel in the minority direction.

                                                          ________________
                                                         |               |
                                                         |               |
                                                _________|               |__________
                                                |                                   |
                                                possible!
                                                This is the case where the initial segments are in minority direction and that its final
                                                minority displacement is small.

                                                draft algorithm:

    |
    |
    ____                                     ____
        |                                        |  probably not,           __________            __________
        |   can be straightened                  |                                   |                     |
        |   uncommon case with vertical          |                                   | impossible          |          _______
    ––––    ports of custom comps            ––––                                                          |__________|
    |
    |                                                                                                       possible!



    Easy heuristic: get vertices of all segements and locate the vertices that are farthest away from the startpos and endpos.
    One way to do this is to measure perpendicular distance of each vertice from the 'optimal wire' which is a straight line drawn between startpos
    and endpos. Obviously this straight line isn't an actual wire
*)
