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
open RotateScale
open BusWireRoutingHelpers.Constants
open BusWireRoutingHelpers

// --------------------------------------------------- //
//                      Constants                      //
// --------------------------------------------------- //

module Constants =
    /// Constant that decides if a wire is classified as almost-straight, if its longest segment in the minority direction is shorter than this length
    let maxDeviationLengthThreshold = 30.0
    /// Constant that decides if a wire is classified as almost-straight, if its overall displacement in the minority direction is shorter than this length
    let maxMinorityDisplacementThreshold = 300

// I initially wanted to use a ratio, but then it would not help fix shorter wires. So absolute values instead of relative measurements

// --------------------------------------------------- //
//                      Helpers!                      //
// ---------------------------------------------------//
open Constants

//--------- To Do: Migrate from SheetBeautifyHelpers to D1----------//
///<summary>
/// T2R, T3R Helper, but can be used to minimise invisible segments for D1.
/// Remove all invisible segments from wires on a sheet.Wire.Wires.
/// Author: tdc21/Tim
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
/// Author: tdc21/Tim
/// </summary>
/// <param name="a1">The first segment.</param>
/// <param name="a2">The second segment.</param>
let perpendicularOverlap2D' ((a1, a2): XYPos * XYPos) ((b1, b2): XYPos * XYPos) : bool =
    let overlapX = overlap1D (a1.X, a2.X) (b1.X, b2.X)
    let overlapY = overlap1D (a1.Y, a2.Y) (b1.Y, b2.Y)
    (overlapX || overlapY)
    && not (overlapX && overlapY)

/// <summary>
/// Helper function to split a list of segments into odd and even segments
/// Author: tdc21/Tim
/// </summary>
let unzipIntoOddAndEvenSegments (segments: Segment list) =
    segments
    |> List.mapi (fun i x -> (i % 2 = 0, x))
    |> List.partition fst
    |> fun (odd, even) -> (List.map snd odd, List.map snd even)

/// <summary>
/// Helper function that checks if a wire is completely straight
/// Author: tdc21/Tim
/// </summary>
let checkIfStraightWire (wire: BusWireT.Wire) =
    // remove all nubs and invisible segments, should be left with a single segment
    let wireWithInvisSegmentsRemoved = (removeSingleWireInvisibleSegments wire)
    match wireWithInvisSegmentsRemoved.Segments.Length with
    | 1 -> true
    | _ -> false

/// <summary>
/// Helper function to count the number of straight wires on a sheet. A heuristic
/// Author: tdc21/Tim
/// </summary>
let countStraightWiresOnSheet (sheetModel: SheetT.Model) =
    let straightWires =
        sheetModel.Wire.Wires
        |> Map.filter (fun _ wire -> checkIfStraightWire wire)
    straightWires.Count

/// <summary>
/// Function that detects if a wire is almost straight.
/// Author: tdc21/Tim
/// </summary>
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

    let majorityDisplacement, isWireTravellingInMajority =
        // a wire must be initially travelling in its majority direction.
        // otherwise, it is not straightenable. See the cases below

        match oddDisplacement >= evenDisplacement, wire.InitialOrientation with
        | true, Horizontal -> oddDisplacement, true
        | true, Vertical -> oddDisplacement, false
        | false, Horizontal -> evenDisplacement, false
        | false, Vertical -> evenDisplacement, true

    // can't be straightened if there are less than 2 segments OR segment length is even
    if
        (wireWithInvisSegmentsRemoved.Segments.Length < 2)
        || (wireWithInvisSegmentsRemoved.Segments.Length % 2 = 0)
    then
        false
    else
        // maxDeviationLength is the longest segment in the minority direction
        // maxMinorityDisplacement is the overall displacement in the minority direction
        match wire.InitialOrientation, isWireTravellingInMajority with
        | Horizontal, true -> // first seg horiz, majority horiz, will deviate vertically, which will be the even segments
            let maxDeviationLength =
                (evenList
                 |> List.maxBy (fun segment -> abs (segment.Length)))
                    .Length
            abs (maxDeviationLength) < maxDeviationLengthThreshold
        | Vertical, true -> // first seg vertical, majority vertical, will deviate horizontally, which will be the even segments
            let maxDeviationLength =
                (evenList
                 |> List.maxBy (fun segment -> abs (segment.Length)))
                    .Length
            abs (maxDeviationLength) < maxDeviationLengthThreshold
        | _, _ -> false

(*  Cases for checkAlmostStraightWire             (Not travelling in majority direction)
                                               |    ___________           ___________
            ______                             |    |         | No        |         |
    _______|     |________ almost straight     |    |         |           |         |
                                               |                 _________|         |__________
                    __________________         |                |                             | No
    _______________|      almost straight      |

    Todo: make sure it won't detect edge cases, such as a tall user-generated stepladder wire that moves upwards in small increments.
    Edge case: https://github.com/timothycdc/hlp_team/commit/5460fb1a0cfe80fdecde22362221a07a3234ca93#commitcomment-139775204
    (Update, this 'edge' case actually can be fixed by the cleanup, or when not possible, when updateWires is called by general cleanup,
    the stepladder vanishes)

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
    Another way to reframe this: any wire (odd segment length) must have an initial orientation that is the same as the majority direction.

    (in odd length, first and last segments will be in majority direction)
    If even length, discard, can't straighten a wire that is 'diagonal/L' shaped. We assume all wires have no invisible segments since we've removed them

    Then, check for the maximum deviation in the minority direction. If the deviation-to-majority-displacement
    ratio is less than a certain threshold, then the wire is almost straight.

*)

/// <summary>
/// Function that counts the number of almost straight wires on a sheet. A heuristic to show on DeveloperModeView SheetStats
/// Author: tdc21/Tim
/// </summary>
let countAlmostStraightWiresOnSheet (sheetModel: SheetT.Model) =
    let straightWires =
        sheetModel.Wire.Wires
        |> Map.filter (fun _ wire -> checkAlmostStraightWire wire)
    straightWires.Count

/// <summary>
/// Helper that returns the wires connected to a symbol's output ports
/// Should go into blockhelpers
/// Author: tdc21/Tim
/// </summary>
let getWiresConnectedToSymbolOutput (symbol: Symbol) (model: BusWireT.Model) =

    let modelWireOutputPorts =
        model.Wires
        |> Map.values
        |> Seq.toList
        |> List.collect (fun wire -> [ (wire.OutputPort.ToString(), wire) ])
        |> Map.ofList

    let wiresConnected =
        symbol.PortMaps.Orientation.Keys
        |> Seq.toList
        |> Seq.collect (fun key ->
            match (Map.tryFind key modelWireOutputPorts) with
            | Some wire -> [ wire ]
            | _ -> [])
        |> Seq.toList

    wiresConnected
/// <summary>
/// Helper that returns the wires connected to a symbol's input ports
/// Should go into blockhelpers
/// Author: tdc21/Tim
/// </summary>
let getWiresConnectedToSymbolInput (symbol: Symbol) (model: BusWireT.Model) =

    let modelWireInputPorts =
        model.Wires
        |> Map.values
        |> Seq.toList
        |> List.collect (fun wire -> [ (wire.InputPort.ToString(), wire) ])
        |> Map.ofList

    let wiresConnected =
        symbol.PortMaps.Orientation.Keys
        |> Seq.toList
        |> Seq.collect (fun key ->
            match (Map.tryFind key modelWireInputPorts) with
            | Some wire -> [ wire ]
            | _ -> [])
        |> Seq.toList

    wiresConnected

/// <summary>
/// Helper that counts the number of all the wires connected to a given symbol's ports
/// Should go into blockhelpers
/// Author: tdc21/Tim
/// </summary>
let getWiresCountConnectedToSym (symbol: Symbol) (model: BusWireT.Model) =
    let wiresConnectedToInput = getWiresConnectedToSymbolInput symbol model
    let wiresConnectedToOutput = getWiresConnectedToSymbolOutput symbol model
    (List.length wiresConnectedToInput)
    + (List.length wiresConnectedToOutput)

/// <summary>
/// Helper that to get a Symbol from PortId. Needs to be given a SheetT.Model to search
/// Author: tdc21/Tim
/// </summary>
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

/// <summary>
/// Function helper that checks if a symbol is of a component type that has a single port.
/// Symbols in question: Inputs, Outputs, Viewers, Constants, WireLabels, Not Connected, IOLabels
/// IOLabels are given the exception since they are generally used with a single port/ are used as stoppers
/// Author: tdc21/Tim
/// </summary>
let checkIfSinglePortComponent (symbol: Symbol) =
    match symbol.Component.Type with
    | Input1 _
    | Input _ // legacy type, still used?
    | Output _
    | Viewer _
    | Constant1 _
    | IOLabel
    | NotConnected -> true
    | _ -> false

/// <summary>
/// Function helper that checks if a port is of a net that has a single wire connected to it.
/// Author: tdc21/Tim
/// </summary>
// Mostly used for D1 purposes
let checkSingleNet (portId: OutputPortId) (sheetModel: SheetT.Model) : bool =
    let nets = partitionWiresIntoNets sheetModel.Wire

    nets
    |> List.tryFind (fun (outputPortID, netlist) -> portId = outputPortID)
    |> Option.map (fun (outputPortID, netlist) -> netlist.Length = 1)
    |> Option.defaultValue false

/// <summary>
/// Function helper that checks if a given wire is connected to at least 1 symbol where the symbol as a single port.
/// Symbols in question: Inputs, Outputs, Viewers, Constants, WireLabels, Not Connected
/// Author: tdc21/Tim
/// </summary>
// Mostly used for D1 purposes. The goal is to find 'lone' components with a single port that can be easily moved without causing intersections
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
(* algo steps:
    For each wire in a model, get the two symbols that the wire is connected to by InputPortId and OutputPortId

    draft:
    - For a symbol's input ports, since they can't be part of a net list, make sure the symbol is singly connected by
    looking up the symbol's other ports to ensure no other wires are connected to them.
    - Note: in my testing InputPortsConnected and OutputPortsConnected does not work! There are no references I can find that update it
    - checkSingleNet is a better way to check if a port is singly connected

    - To ensure OutputPortId is singly connected:
    - use isWireInNet in BlockHelpers to check if the net has more than one wire. If so, output port is not singly connected. Skip
    - If the net is singular, then check the symbol of the output port– make sure all its ports are not connected anywhere else

    shortcut:
    - Note: these cases above apply to partially-done circuits where not all ports are being utilised by wires in the sheet!
    - Otherwise, it's much faster to immediately check symbols that only have one port.
    - Symbols in question: Inputs, Outputs, Viewers, Constants, WireLabels, Not Connected
    - After all, completed sheets will have all ports connected to wires (or will have used Not Connected Stopper )
*)

/// <summary>
/// Helper to find if a wire is singly connected to symbols that have a single port or are (IOLabels), and returns the symbols
/// Returns a list<Symbol * bool> where bool is true if the symbol is an input. List length is either 0, 1, or 2 (max num of wire ends is 2)
/// Author: tdc21/Tim
/// </summary>
// mostly used for D1 purposes
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

/// <summary>
/// Helper that returns the symbols on both ends of a wire, if they exist.
/// Returns a list<Symbol * bool> where bool is true if the symbol is an input. List length is either 0, 1, or 2 (max num of wire ends is 2)
/// Author: tdc21/Tim
/// </summary>
let findConnectedSymsByWire (wire: Wire) (sheetModel: SheetT.Model) =
    // will return (symbol, true) if the symbol is an input, (symbol, false) if the symbol is an output
    // it is important to distingush between the two, because when correcting the wire bend, inputs are
    // moved in opposite direction to outputs, and startpos is modified instead of endpos
    let inputSymbol = getSymbolFromPortID (wire.InputPort.ToString()) sheetModel
    let outputSymbol = getSymbolFromPortID (wire.OutputPort.ToString()) sheetModel

    // if either Symbol.Component.ComponentType is of Input1, Output, Viewer, Constant1, NotConnected
    // then the wire is singly connected
    match inputSymbol, outputSymbol with
    | Some inputSymbol, Some outputSymbol -> [ (inputSymbol, true); (outputSymbol, false) ]
    | Some inputSymbol, _ -> [ (inputSymbol, true) ]
    | _, Some outputSymbol -> [ (outputSymbol, false) ]
    | _ -> []

/// <summary>
/// Test function to show on Sheet Stats' developer mode
/// Author: tdc21/Tim
/// </summary>
let countSinglyConnectedWires (model: SheetT.Model) =
    model.Wire.Wires
    |> Map.filter (fun _ wire -> checkIfSingularlyConnected wire model)
    |> Map.count

/// <summary>
/// (Helper?) Function to find the minority displacement of wire (the displacment in the direction that not travelled the furthest by the wire)
/// Author: tdc21/Tim
/// </summary>
// Mostly used for D1 purposes
let getMinorityWireDisplacementAndOrientation (wire: Wire) =
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

    match abs (oddDisplacement) <= abs (evenDisplacement), wire.InitialOrientation with
    | true, Horizontal -> oddDisplacement, Horizontal
    | true, Vertical -> oddDisplacement, Vertical
    | false, Horizontal -> evenDisplacement, Vertical
    | false, Vertical -> evenDisplacement, Horizontal
// if even segments majority and wire starts from vert (which is odd), then horiz is majority, vert is minority

/// <summary>
/// (Helper?) Function to find the majority displacement of wire (the displacment in the direction that travelled the furthest by the wire)
/// Author: tdc21/Tim
/// </summary>
// Mostly used for D1 purposes
let getMajorityDisplacementWireAndOrientation (wire: Wire) =
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

    match abs (oddDisplacement) >= abs (evenDisplacement), wire.InitialOrientation with
    | true, Horizontal -> oddDisplacement, Horizontal
    | true, Vertical -> oddDisplacement, Vertical
    | false, Horizontal -> evenDisplacement, Vertical
    | false, Vertical -> evenDisplacement, Horizontal

/// optic to access the initial orientation of a wire
let initialOrientation_: Lens<Wire, Orientation> =
    Lens.create (fun m -> m.InitialOrientation) (fun s m -> { m with InitialOrientation = s })

/// optic to access the StartPos of a wire
let startpos_: Lens<Wire, XYPos> =
    Lens.create (fun m -> m.StartPos) (fun s m -> { m with StartPos = s })

/// <summary>
/// A record used  when straightening/cleaning up wires singly connected to symbols. Contains:
/// the symbol to be moved,
/// the wire that is connected to the symbol,
/// the offset that needs to be moved by,
/// whether the connected port is an input (the input/output nature will determine how to shift the StartPos of the wire)
/// the majorityDisplacement offset. Used in tryGeneralCleanup's 2nd pass, when we want to shift wire/symbols in the majority direction to avoid an intersection
/// Author: tdc21/Tim
/// </summary>
// Mostly used for D1 purposes
type CleanUpRecord =
    { Symbol: Symbol
      Wire: Wire
      Offset: XYPos
      IsPortInput: bool
      MajorityDisplacementOffset: float }

/// <summary>
/// Function to move a symbol according to its cleanUpRecord
/// smartAutoroute still causes small artefacts so this function manually creates a straight wire, recycling existing WId
/// Author: tdc21/Tim
/// </summary>
// Mostly used for D1 purposes
let moveSymbolWireCleanUpRecord (cleanUpRecord: CleanUpRecord) =
    let majorityDisplacement, MajorityDirection =
        (getMajorityDisplacementWireAndOrientation cleanUpRecord.Wire)

    // to invert the direction if the port is an input
    let directionChangeMultiplier =
        (if cleanUpRecord.IsPortInput then
             -1.0
         else
             1.0)

    // if the port is an output, we will have to shift the Wire's startpos
    let newPortStartPos =
        if cleanUpRecord.IsPortInput then
            cleanUpRecord.Wire.StartPos
        else
            cleanUpRecord.Wire.StartPos + cleanUpRecord.Offset

    // smartautoroute gives artifacts when rerouting almost straight wires to become straight
    // this function manually creates a straight wire from a given wire by discarding all segments except the first, recycling existing WId
    // nubs are created later using makeEndsDraggable, so wires respect the spec
    let newWire =
        cleanUpRecord.Wire
        |> Optic.set
            (segments_)
            // assume that there are at least two segments, we can discard the rest.
            (cleanUpRecord.Wire.Segments // make a wire that consists of one big segment in the majority direction, then make nubs
             // initially I was worried we might need more than the first segment so we use List.mapi and check the index i
             // better to refactor this to use List.head?
             |> List.mapi (fun i segment ->
                 match MajorityDirection, i = 0 with
                 | Horizontal, true ->
                     Some
                         { segment with
                             Length =
                                 (majorityDisplacement
                                  + cleanUpRecord.MajorityDisplacementOffset) }
                 | Vertical, true ->
                     Some
                         { segment with
                             Length =
                                 (majorityDisplacement
                                  + cleanUpRecord.MajorityDisplacementOffset) }
                 | _, _ -> None)
             |> List.choose id
             |> makeEndsDraggable)
        |> Optic.set initialOrientation_ MajorityDirection
        |> Optic.set startpos_ newPortStartPos // update startpos ONLY if is input

    let newSymbol =
        moveSymbol (cleanUpRecord.Offset * directionChangeMultiplier) cleanUpRecord.Symbol

    newWire, newSymbol

/// <summary>
/// Helper function that returns the bounding box of the intersection between a boundingBox and a segment
/// similar to BlockHelpers.segmentIntersectsBoundingBox but uses boundingBoxes instead of rectangles, and returns the bounding box of the intersection
/// Author: tdc21/Tim
/// </summary>
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

/// <summary>
/// A very useful helper function that returns a bounding boxes list of a symbol's intersections with other symbols and wires segments on the model
/// For wire intersections, it is segment-specific to be more exact.
/// Returns a list of bounding boxes of symbols and wires that intersect with the symbol
/// Author: tdc21/Tim
/// </summary>
// used by generalCleanup: we try and move a symbol to fix a bend. If this move causes interesections, this function allows us to
// locate these intersections as boundingboxes so we can easily move the symbol to another place and try again
let findAllBoundingBoxesOfSymIntersections (symbol: Symbol) (model: SheetT.Model) =

    // To find where a symbol intersects a wire, is a lot more computationally difficult than a symbol-symbol intersection.
    // Existing helpers can help us find if a wire intersects a symbol, and not the other way round.

    // We will have to iterate thru every wire, run findWireSymbolIntersections to get a list of intersected symbol bounding boxes,
    // and see if the list contains our newSymbol's bounding box. We use the =~ operator to check if two bounding boxes are equal.
    // Similar to XYPos, I overloaded the =~operator to allow for tiny floating point differences
    let symbolBoundingBox = getBoundingBox model.Wire.Symbol symbol.Id
    let wModel = (updateBoundingBoxes model).Wire // just in case

    let intersectingWiresBbBoxes =
        wModel.Wires
        |> Map.values
        // findWireSymbolIntersections returns a list of bounding boxes of symbols intersected by wire.
        |> Seq.map (fun wire -> (wire, (findWireSymbolIntersections wModel wire)))
        // we have (Wire * BoundingBox list) seq. Now to look through every tuple and get any wire whose bbox list is equal to symbolBoundingBox
        // we might get more than one wire – so get a list
        |> Seq.choose (fun (wire, bboxes) ->
            if
                bboxes
                |> List.exists (fun box ->

                    // findWireSymbolIntersections returns bounding boxes that have been enlarged with minWireSeparation. Is this a bug or feature?
                    // we need to correct the bounding box to the original size
                    let correctedBox =
                        { W = box.W - minWireSeparation * 2.
                          H = box.H - minWireSeparation * 2.
                          TopLeft =
                            box.TopLeft
                            |> updatePos Right_ minWireSeparation
                            |> updatePos Down_ minWireSeparation }

                    symbolBoundingBox =~ correctedBox)
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

            // similar to findWireSymbolIntersections, we disqualify the first and last segments

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

    // unlike finding the bboxes of intersecting wire segments, finidng the bboxes of intersecting symbols is straightforward
    // we get all symbol bounding boxes in the model (except the symbol we are comparing), and compare it with the given symbol
    // check for an overlap area
    let intersectingSymbolBBoxes =
        model.BoundingBoxes
        |> Map.values
        |> Seq.toList
        // get all boundingBoxes in model not equal to symbolBoundingBox
        |> List.filter (fun (box) -> not (box =~ symbolBoundingBox))
        // see if they overlap with the symbolBoundingBox
        |> List.choose (fun box -> (overlapArea2DBox symbolBoundingBox box))

    // return the list of intersecting bounding boxes
    intersectingWiresBbBoxes
    @ intersectingSymbolBBoxes

/// <summary>
/// Helper to update the sheet's existing symbol with a new symbol
/// Author: tdc21/Tim
/// </summary>
let updateSheetSymWithNewSym (symbol: Symbol) (sheetModel: SheetT.Model) =
    sheetModel
    |> Optic.set (wire_ >-> symbolOf_ symbol.Id) symbol
    |> SheetUpdateHelpers.updateBoundingBoxes // update the bounding boxes for accurate intersection checking

/// <summary>
/// Helper to update the sheet's existing wire with a new wire
/// Author: tdc21/Tim
/// </summary>
let updateSheetWireWithNewWire (wire: Wire) (sheetModel: SheetT.Model) =
    sheetModel
    |> Optic.set (wire_ >-> wires_) (sheetModel.Wire.Wires |> Map.add wire.WId wire)

/// <summary>
/// D1 Helper to compare two an old or new sheetT.Model, and see if it has gained maintained or reduced the number of sym-wym or wire-sym intersections.
/// Author: tdc21/Tim
/// </summary>
let checkIfGainedOrMaintainedIntersections (currentModel: SheetT.Model) (newModel: SheetT.Model) =
    ((countIntersectingSymbolPairs newModel)
     <= (countIntersectingSymbolPairs currentModel))
    && ((countVisibleSegsIntersectingSymbols newModel)
        <= (countVisibleSegsIntersectingSymbols currentModel))

/// <summary>
/// Helper to find the opposite edge. Code was from symbolReplaceHelpers but was not accessible as a helper, so I have added one here
/// Author: tdc21/Tim
/// </summary>
let findOpposite (edge: Edge) =
    match edge with
    | Right -> Left
    | Top -> Bottom
    | Left -> Right
    | Bottom -> Top

//                                                    //
//                                                    //
//                                                    //
// ---------------------------------------------------//
//               Deliverable 1 Code                   //
// ---------------------------------------------------//
//                                                    //
//                                                    //
//                                                    //

/// <summary>
/// D1 Spec 1,2: Function to clean up almost straight singly connected wires. Takes in and outputs a model
/// Author: tdc21/Tim
/// </summary>
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

                if (checkIfGainedOrMaintainedIntersections currentSheetModel newSheetModel) then
                    newSheetModel
                else
                    printf "another pass with symbol id %A" cleanUpRecord.Symbol.Id
                    // we do another pass. Although it would be great to increase the number of passes
                    let intersectingBBoxes =
                        findAllBoundingBoxesOfSymIntersections newMovedSymbol newSheetModel
                    let newOffset, majorityDisplacementOffset =
                        match
                            cleanUpRecord.IsPortInput,
                            cleanUpRecord.Wire.InitialOrientation,
                            (intersectingBBoxes.Length > 0)
                        with
                        | true, Horizontal, true ->
                            let addedXOffset = // get the x coordinate of the x bounding box furthest away from the symbol and adjust symbol to avoid it
                                intersectingBBoxes
                                |> List.minBy (fun box -> box.TopLeft.X)
                                |> (fun box -> newMovedSymbol.Pos.X - box.TopLeft.X - 10.0)
                            // can add another condition to set to zero, cancelling the operatio if we have to move the symbol
                            // too far back
                            // { X = 10.0; Y = 0.0 }, -10.0
                            { X = addedXOffset; Y = 0.0 }, -addedXOffset
                        | false, Horizontal, true ->
                            let addedXOffset = // get the x coordinate of the x bounding box furthest away from the symbol and adjust symbol to avoid it
                                intersectingBBoxes
                                |> List.maxBy (fun box -> box.TopLeft.X)
                                |> (fun box -> box.TopLeft.X - newMovedSymbol.Pos.X + 10.0)
                            { X = -addedXOffset; Y = 0.0 }, addedXOffset

                        | true, Vertical, true ->
                            let addedYOffset = // get the y coordinate of the y bounding box furthest away from the symbol and adjust symbol to avoid it
                                intersectingBBoxes
                                |> List.minBy (fun box -> box.TopLeft.Y + box.H)
                                |> (fun box -> newMovedSymbol.Pos.Y - box.TopLeft.Y - box.H)
                            { X = 0.0; Y = addedYOffset }, -addedYOffset
                        | false, Vertical, true ->
                            let addedYOffset =
                                intersectingBBoxes
                                |> List.maxBy (fun box -> box.TopLeft.Y + box.H)
                                |> (fun box -> box.TopLeft.Y + box.H - newMovedSymbol.Pos.Y)
                            { X = 0.0; Y = -addedYOffset }, addedYOffset
                        | _, _, _ -> cleanUpRecord.Offset, cleanUpRecord.MajorityDisplacementOffset
                    let newCleanUpRecord =
                        { cleanUpRecord with
                            Offset = newOffset
                            Symbol = newMovedSymbol
                            Wire = newStraightWire
                            MajorityDisplacementOffset = majorityDisplacementOffset }
                    let newNewStraightWire, newNewMovedSymbol =
                        moveSymbolWireCleanUpRecord newCleanUpRecord
                    let newNewSheetModel =
                        newSheetModel
                        |> updateSheetSymWithNewSym newNewMovedSymbol
                        |> updateSheetWireWithNewWire newNewStraightWire
                    if (checkIfGainedOrMaintainedIntersections currentSheetModel newSheetModel) then
                        newNewSheetModel
                    else
                        printf "second pass unsuccessful"
                        currentSheetModel

            // first, calculate how many bends we are saving by straightening CleanUpRecord.Wire
            // before running currentSheetModel, try determine the source of the intersection

            // if it is caused by the newSymbol intersecting another symbol, we can easily move the newSymbol to another place and try again once more
            )

            model.Sheet

    model |> Optic.set (sheet_) (updatedSheetModel)

/// <summary>
/// D1 spec 3,5,6,7: Try to clean up all bends on a sheet
/// Author: tdc21/Tim
/// </summary>
let tryGeneralCleanUp (model: ModelType.Model) =
    // check if wire is singly connected
    // then check if it is almost straight
    // todo: perhaps use a different constant/tolerance to allow for more general straightening instead of 'almost straight' wires

    let almostStraightWires =
        model.Sheet.Wire.Wires
        |> Map.filter (fun _ wire -> checkAlmostStraightWire wire)

    /// Produce a list of CleanUpRecords
    /// Find possible wires and symbols to be straightened out, calculate their offset to fix them, and also keep track if the connections
    /// occur at an input port or output port
    let symbolsWireOffsetUpdates: CleanUpRecord list =
        almostStraightWires
        |> Map.values
        |> Seq.toList
        |> List.collect (fun wire -> // should I collect, or should I map and choose id
            let (symbolsToMove: list<Symbol * bool>) =
                (findConnectedSymsByWire wire model.Sheet)
            // will be of length 0, 1, or 2.
            // symbolsToMove consist of tuples (Symbol, bool) where bool is true if the symbol is an input
            if symbolsToMove.Length = 0 then
                []
            else
                match getMinorityWireDisplacementAndOrientation wire with // if deviating vertically, offset y. Negative if input and positive if output
                | y, Vertical ->
                    let offset = { X = 0.0; Y = y }
                    symbolsToMove
                    |> List.minBy (fun (symbol, _) -> getWiresCountConnectedToSym symbol model.Sheet.Wire)
                    |> (fun (symbol, isInput) ->
                        [ { Symbol = symbol
                            Wire = wire
                            Offset = offset
                            IsPortInput = isInput
                            MajorityDisplacementOffset = 0.0 } ])

                | x, Horizontal -> // if deviating horizontally, offset x. Negative if it's an input port and positive if it output
                    let offset = { X = x; Y = 0.0 }
                    symbolsToMove
                    |> List.minBy (fun (symbol, _) -> getWiresCountConnectedToSym symbol model.Sheet.Wire)
                    |> (fun (symbol, isInput) ->
                        [ { Symbol = symbol
                            Wire = wire
                            Offset = offset
                            IsPortInput = isInput
                            MajorityDisplacementOffset = 0.0 } ]))

    let continueConditionCheck (currentModel: SheetT.Model) (newModel: SheetT.Model) =
        ((countIntersectingSymbolPairs newModel)
         <= (countIntersectingSymbolPairs currentModel))
        && ((countVisibleSegsIntersectingSymbols newModel)
            <= (countVisibleSegsIntersectingSymbols currentModel))
        && ((countStraightWiresOnSheet newModel)
            >= (countStraightWiresOnSheet currentModel))

    // let maxPasses = 3
    // let maxOffset = 200.0
    let perturbAllSymbols (perturbation: float) (model: BusWireT.Model) =

        let perturbedSymbols =
            model.Symbol.Symbols
            |> Map.map (fun symbolId symbol -> (moveSymbol { X = perturbation; Y = perturbation } symbol))

        let modelWithPerturbed =
            model
            |> Optic.set (symbol_ >-> SymbolT.symbols_) perturbedSymbols

        let newWires =
            modelWithPerturbed.Wires
            |> Map.map (fun wireId wire -> updateWire modelWithPerturbed wire true)

        modelWithPerturbed
        |> Optic.set (wires_) (newWires)

    let updatedSheetModel: SheetT.Model =
        printf "length of symbolsWireOffsetUpdates: %A" symbolsWireOffsetUpdates.Length
        symbolsWireOffsetUpdates
        |> List.fold // better way to do this?
            (fun currentSheetModel (cleanUpRecord: CleanUpRecord) ->
                printf "doing a pass with symbol id %A" cleanUpRecord.Symbol.Id
                // create a new straight wire
                let _, newMovedSymbol = moveSymbolWireCleanUpRecord cleanUpRecord
                let newSheetModelBeforeReroute =
                    currentSheetModel
                    |> updateSheetSymWithNewSym newMovedSymbol
                    |> (fun modelWithNewSymb ->
                        updateSheetWireWithNewWire
                            (smartAutoroute modelWithNewSymb.Wire cleanUpRecord.Wire)
                            modelWithNewSymb)
                let routedWires =
                    newSheetModelBeforeReroute.Wire
                    |> perturbAllSymbols 100.0
                    |> (fun interimModel ->
                        BusWireSeparate.updateWireSegmentJumpsAndSeparations
                            (interimModel.Wires.Keys |> Seq.toList)
                            interimModel)
                    |> perturbAllSymbols -100.0
                    |> (fun interimModel ->
                        BusWireSeparate.updateWireSegmentJumpsAndSeparations
                            (interimModel.Wires.Keys |> Seq.toList)
                            interimModel)

                let newSheetModel =
                    newSheetModelBeforeReroute
                    |> Optic.set wire_ routedWires

                if (continueConditionCheck currentSheetModel newSheetModel) then
                    newSheetModel
                else
                    printf "another pass with symbol id %A" cleanUpRecord.Symbol.Id
                    // we do another pass.
                    let intersectingBBoxes =
                        findAllBoundingBoxesOfSymIntersections newMovedSymbol newSheetModel
                    let newOffset, majorityDisplacementOffset =
                        match
                            cleanUpRecord.IsPortInput,
                            cleanUpRecord.Wire.InitialOrientation,
                            (intersectingBBoxes.Length > 0)
                        with
                        | true, Horizontal, true ->
                            let addedXOffset = // get the x coordinate of the x bounding box furthest away from the symbol and adjust symbol to avoid it
                                intersectingBBoxes
                                |> List.minBy (fun box -> box.TopLeft.X)
                                |> (fun box -> newMovedSymbol.Pos.X - box.TopLeft.X - 10.0)
                            // can add another condition to set to zero, cancelling the operation if we have to move the symbol
                            // too far back
                            // { X = 10.0; Y = 0.0 }, -10.0
                            { X = addedXOffset; Y = 0.0 }, -addedXOffset
                        | false, Horizontal, true ->
                            let addedXOffset = // get the x coordinate of the x bounding box furthest away from the symbol and adjust symbol to avoid it
                                intersectingBBoxes
                                |> List.maxBy (fun box -> box.TopLeft.X)
                                |> (fun box -> box.TopLeft.X - newMovedSymbol.Pos.X + 10.0)
                            { X = -addedXOffset; Y = 0.0 }, addedXOffset

                        | true, Vertical, true ->
                            let addedYOffset = // get the y coordinate of the y bounding box furthest away from the symbol and adjust symbol to avoid it
                                intersectingBBoxes
                                |> List.minBy (fun box -> box.TopLeft.Y + box.H)
                                |> (fun box -> newMovedSymbol.Pos.Y - box.TopLeft.Y - box.H)
                            { X = 0.0; Y = addedYOffset }, -addedYOffset
                        | false, Vertical, true ->
                            let addedYOffset =
                                intersectingBBoxes
                                |> List.maxBy (fun box -> box.TopLeft.Y + box.H)
                                |> (fun box -> box.TopLeft.Y + box.H - newMovedSymbol.Pos.Y)
                            { X = 0.0; Y = -addedYOffset }, addedYOffset
                        | _, _, _ -> cleanUpRecord.Offset, cleanUpRecord.MajorityDisplacementOffset
                    let newCleanUpRecord =
                        { cleanUpRecord with
                            Offset = newOffset
                            Symbol = newMovedSymbol
                            // Wire = Wire
                            MajorityDisplacementOffset = majorityDisplacementOffset }
                    let _, newNewMovedSymbol = moveSymbolWireCleanUpRecord newCleanUpRecord
                    let newNewSheetModel =
                        newSheetModel
                        |> updateSheetSymWithNewSym newNewMovedSymbol
                        |> (fun modelWithNewSymb ->
                            updateSheetWireWithNewWire
                                (smartAutoroute modelWithNewSymb.Wire cleanUpRecord.Wire)
                                modelWithNewSymb)

                    if (continueConditionCheck currentSheetModel newSheetModel) then
                        newNewSheetModel
                    else
                        printf "second pass unsuccessful"
                        currentSheetModel

            (*
                        Wishful Thinking: if there are still too many intersections, use findNearbyComponents to get a list of symbols and manually resize?
                    *)

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

/// When trying to straighten wires by rescaling custom components, we want to find the longest contiguous sequence of connections between two custom components A and B
/// This type keeps track of the longest sequence found so far and its start points on both symbols
type ContiguousSequenceRecord =
    { StartPointA: int
      StartPointB: int
      SequenceLength: int

    }

/// <summary>
/// D1 spec 4: An improved version of reSizeSymbol that identifies the port-pairs to be straightened in a way that maximises the reduction in bends,
/// by targeting the longest contiguous sequence of port connections between two symbols.
/// Author: tdc21/Tim
/// </summary>
let reSizeSymbolImproved (wModel: BusWireT.Model) (symbolToSize: Symbol) (otherSymbol: Symbol) =

    let wires: Wire list = wiresBtwnSyms wModel symbolToSize otherSymbol

    // create a double-length map of connections between ports, for quick lookup
    // using string to allow for InputPortId and OutputPortId to be used as keys
    let connectionMap =
        wires
        |> List.collect (fun wire ->
            [ (wire.InputPort.ToString(), wire.OutputPort.ToString())
              (wire.OutputPort.ToString(), wire.InputPort.ToString()) ])
        |> Map.ofList

    // find longest contiguous sequence of connections between both symbols
    // example case. Denote symbol A as the symbol to resize, and symbol B as the other symbol, and -> for a connection
    // A's output ports (on the right edge) are connected to B's input ports (on the left edge)
    // A.1 refers to A's first port from the top down, and B.1 refers to B's first port from the top down
    // If (A.1 -> B.1), (A.2 -> B.2), (A.3 -> B.3), return [(A.1 -> B.1), (A.2 -> B.2), (A.3 -> B.3)]
    // If (A.1 -> B.2), (A.3 -> B.1), return [(A.1 -> B.2)]. This is because A.2 broke the sequence
    // If (A.1 -> B.2), (A.3 -> B.3), (A.4 -> B.4), (A.5 -> B.5), return [(A.3 -> B.3), (A.4 -> B.4), (A.5 -> B.5)]. This is because A.2 broke the sequence but we found a longer one after

    // note. we iterate throughh ports from L to R or Top-Down. In PortMaps, the top and right edges' order is reversed.
    // algorithm: start with port A.1 and get its connected port, B.X. Then check A.2 -> B.(X+1), A.3 -> B(X+2, ...) and keep track until when this sequence breaks.
    // The first sequence lasting k ports would have stopped at A(k+1). Continue checking from A(k+2) onwards
    // Keep track of the sequence with the most ports

    let rec findLongestContiguousSequence
        (maxData: ContiguousSequenceRecord)
        (currentData: ContiguousSequenceRecord)
        (currentPointA: int)
        (currentPointB: int)
        (endPointA: int)
        (endPointB: int)
        (portListA: string list)
        (portListB: string list)
        (connectionMap: Map<string, string>)
        : ContiguousSequenceRecord
        =

        match
            (currentPointA > endPointA)
            || (currentPointB > endPointB)
        with
        | true -> maxData
        | false ->
            // for our given port on currentPointA index of edge A, we see if it's connected to a port on edge B using connectionMap.
            // connectionMap keeps track of all connections between ports on symbol A and B
            // if the port does not exist in connectionMap, it does not connect to any port on edge B
            // if the port does exist in connectionMap, we get the index of the connected port on edge B
            let tryFindBIndex =
                Map.tryFind (portListA.[currentPointA]) connectionMap
                |> Option.defaultValue ""
                |> (fun (portAConnectedId) -> List.tryFindIndex (fun port -> port = portAConnectedId) portListB)

            match tryFindBIndex with
            | Some bIndex when currentData.SequenceLength = 0 -> // the current port on edge A is connected to the port on edge B. We have started a new sequence
                let newCurrentData =
                    { StartPointA = currentPointA; StartPointB = bIndex; SequenceLength = 1 }
                let newMaxData =
                    match maxData.SequenceLength < newCurrentData.SequenceLength with
                    | true -> newCurrentData
                    | false -> maxData
                findLongestContiguousSequence
                    (newMaxData)
                    (newCurrentData)
                    (currentPointA + 1)
                    (bIndex + 1)
                    (endPointA)
                    (endPointB)
                    (portListA)
                    (portListB)
                    (connectionMap)
            | Some bIndex when (currentPointB = bIndex) -> // the current port on edge A is connected to the port on edge B, continuing from a previous sequence.
                let newCurrentData =
                    { currentData with SequenceLength = currentData.SequenceLength + 1 }
                let newMaxData =
                    match maxData.SequenceLength < newCurrentData.SequenceLength with
                    | true -> newCurrentData
                    | false -> maxData
                findLongestContiguousSequence
                    (newMaxData)
                    (newCurrentData)
                    (currentPointA + 1)
                    (currentPointB + 1)
                    (endPointA)
                    (endPointB)
                    (portListA)
                    (portListB)
                    (connectionMap)
            | _ -> // the current port on edge A is not connected to any port on edge B. the sequence has ended
                findLongestContiguousSequence
                    (maxData)
                    ({ StartPointA = -1; StartPointB = -1; SequenceLength = 0 })
                    (currentPointA + min (currentData.SequenceLength) 1)
                    (currentPointB)
                    (endPointA)
                    (endPointB)
                    (portListA)
                    (portListB)
                    (connectionMap)

    let longestContiguousSequencesByEdge =
        [ Top; Bottom; Left; Right ]
        |> List.map (fun edge ->
            let symToSizeEdgePorts, otherSymbolEdgePorts =

                match edge with
                | Top ->
                    (List.rev (Map.find Top symbolToSize.PortMaps.Order)), (Map.find Bottom otherSymbol.PortMaps.Order)
                | Bottom ->
                    (Map.find Bottom symbolToSize.PortMaps.Order), (List.rev (Map.find Top otherSymbol.PortMaps.Order))
                | Left ->
                    (Map.find Left symbolToSize.PortMaps.Order), (List.rev (Map.find Right otherSymbol.PortMaps.Order))
                | Right ->
                    (List.rev (Map.find Right symbolToSize.PortMaps.Order)), (Map.find Left otherSymbol.PortMaps.Order)
            // printf "symToSizeEdgePorts: %A" symToSizeEdgePorts
            // printf "otherSymbolEdgePorts: %A" otherSymbolEdgePorts

            edge,
            findLongestContiguousSequence
                ({ StartPointA = -1; StartPointB = -1; SequenceLength = 0 })
                ({ StartPointA = -1; StartPointB = -1; SequenceLength = 0 })
                0
                0
                (symToSizeEdgePorts.Length - 1)
                (otherSymbolEdgePorts.Length - 1)
                symToSizeEdgePorts
                otherSymbolEdgePorts
                connectionMap)

    let ((longestVertSequenceEdge, longestContiguousVerticalSequence),
         (longestHorizSequenceEdge, longestContiguousHorizontalSequence)) =
        match
            (snd longestContiguousSequencesByEdge[0]).SequenceLength > (snd longestContiguousSequencesByEdge[1])
                .SequenceLength,
            (snd longestContiguousSequencesByEdge[2]).SequenceLength > (snd longestContiguousSequencesByEdge[3])
                .SequenceLength
        with
        | true, true -> longestContiguousSequencesByEdge[0], longestContiguousSequencesByEdge[2] //Top and Left
        | true, false -> longestContiguousSequencesByEdge[0], longestContiguousSequencesByEdge[3] //Top and Right
        | false, true -> longestContiguousSequencesByEdge[1], longestContiguousSequencesByEdge[2] //Bottom and Left
        | false, false -> longestContiguousSequencesByEdge[1], longestContiguousSequencesByEdge[3] //Bottom and Right
    // printf "symbolToSize: %A" symbolToSize.Id
    // printf "longestContiguousVerticalSequence: %A" longestContiguousVerticalSequence
    // printf "longestContiguousHorizontalSequence: %A" longestContiguousHorizontalSequence

    let resizePortIdHoriz =
        (Map.find longestHorizSequenceEdge symbolToSize.PortMaps.Order)[longestContiguousHorizontalSequence.StartPointA]
    let otherPortIdHoriz =
        (Map.find (findOpposite longestHorizSequenceEdge) otherSymbol.PortMaps.Order)[longestContiguousHorizontalSequence.StartPointB]

    let resizePortInfoHoriz =
        match Map.tryFind resizePortIdHoriz wModel.Symbol.Ports with
        | Some port -> makePortInfo symbolToSize port
        | None -> failwith "Port not found"

    let otherPortInfoHoriz =
        match Map.tryFind otherPortIdHoriz wModel.Symbol.Ports with
        | Some port -> makePortInfo otherSymbol port
        | None -> failwith "Port not found"

    let h, w = calculateResizedDimensions resizePortInfoHoriz otherPortInfoHoriz

    match symbolToSize.Component.Type with
    | Custom _ ->
        let scaledSymbol = setCustomCompHW h w symbolToSize
        let scaledInfo = makePortInfo scaledSymbol resizePortInfoHoriz.port
        let offset = alignPortsOffset scaledInfo otherPortInfoHoriz
        moveSymbol offset scaledSymbol
    | _ -> symbolToSize

// let h, w = calculateResizedDimensions

// set the symbolToSize's vscale to match otherSymbol's vscale

// note that we can only resize H and W. So we choose between considering satisfying the top or bottom edge,

/// <summary>
/// D1 spec 4: High level call for reSizeSymbolImproved
/// Author: tdc21/Tim
/// </summary>
let reSizeSymbolImprovedTopLevel (wModel: BusWireT.Model) (symbolA: Symbol) (symbolB: Symbol) : BusWireT.Model =
    // printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"

    // resize the symbol with the least wires connected to it
    let symbolAWireCount, symbolBWireCount =
        getWiresCountConnectedToSym symbolA wModel, getWiresCountConnectedToSym symbolB wModel
    let symbolToSize, otherSymbol =
        match symbolAWireCount >= symbolBWireCount with
        | true -> symbolB, symbolA
        | false -> symbolA, symbolB

    let scaledSymbol2 = reSizeSymbol wModel symbolToSize otherSymbol
    let scaledSymbol = reSizeSymbolImproved wModel symbolToSize otherSymbol

    wModel
    |> Optic.set (symbolOf_ symbolToSize.Id) scaledSymbol
    |> (fun model' -> BusWireSeparate.routeAndSeparateSymbolWires model' symbolToSize.Id)

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

// // count if
// let symbolToSizeInputPorts =
//     wires // count if each wire's InputPort.toString() is in symbolToSizeAllPorts
//     |> List.filter (fun wire -> symbolToSizeAllPorts |> List.contains (wire.InputPort.ToString()))
//     |> List.map (fun wire -> wire.InputPort.ToString())

// let symbolToSizeOutputPorts =
//     wires // count if each wire's OutputPort.toString() is in symbolToSizeAllPorts
//     |> List.filter (fun wire -> symbolToSizeAllPorts |> List.contains (wire.OutputPort.ToString()))
//     |> List.map (fun wire -> wire.OutputPort.ToString())

// let otherSymbolInputPorts =
//     wires // count if each wire's InputPort.toString() is in otherSymbolAllPorts
//     |> List.filter (fun wire -> otherSymbolAllPorts |> List.contains (wire.InputPort.ToString()))
//     |> List.map (fun wire -> wire.InputPort.ToString())

// let otherSymbolOutputPorts =
//     wires // count if each wire's OutputPort.toString() is in otherSymbolAllPorts
//     |> List.filter (fun wire -> otherSymbolAllPorts |> List.contains (wire.OutputPort.ToString()))
//     |> List.map (fun wire -> wire.OutputPort.ToString())

// let symbolToSizeEdgePorts, otherSymbolEdgePorts =
//     match (List.length(symbolToSizeInputPorts)) > (wires.Length / 2) with
//     | true -> // a majority of the symbolToSize's ports are input ones. Get those that share a majority edge
//         let symToSizeEdgePorts =
//             symbolToSizeInputPorts
//             |> List.groupBy (fun port -> symbolToSize.PortMaps.Orientation.[port])
//             //  get the ports on the edge that is shared by the majority of the ports
//             |> List.maxBy (fun (orientation, ports) -> ports.Length)
//             |> snd
//             // thus, a majority of otherSymbol's ports are output ones. Get those that share a majority edge
//         let otherSymbolEdgePorts =
//             otherSymbolOutputPorts
//             |> List.groupBy (fun port -> otherSymbol.PortMaps.Orientation.[port])
//             //  get the ports on the edge that is shared by the majority of the ports
//             |> List.maxBy (fun (orientation, ports) -> ports.Length)
//             |> snd
//         symToSizeEdgePorts, otherSymbolEdgePorts
//     | false -> // a majority of the symbolToSize's ports are output ones. Get those that share a majority edge
//         let symToSizeEdgePorts  =
//             symbolToSizeOutputPorts
//             |> List.groupBy (fun port -> symbolToSize.PortMaps.Orientation.[port])
//             //  get the ports on the edge that is shared by the majority of the ports
//             |> List.maxBy (fun (orientation, ports) -> ports.Length)
//             |> snd
//             // thus, a majority of otherSymbol's ports are input ones. Get those that share a majority edge
//         let otherSymbolEdgePorts =
//             otherSymbolInputPorts
//             |> List.groupBy (fun port -> otherSymbol.PortMaps.Orientation.[port])
//             //  get the ports on the edge that is shared by the majority of the ports
//             |> List.maxBy (fun (orientation, ports) -> ports.Length)
//             |> snd
//         symToSizeEdgePorts, otherSymbolEdgePorts

// algo: take in two symbols
// find which symbol has least ports/wires connected, that is the symbolToSize, the other is called otherSymbol.
// use existing helpers to get a list of ports on symbolToSize connected to ports on the other, then sort by edge
// ideally they should all share one edge, but if not, choose only ports on the edge that is shared by the majority of the ports
// Get the ports on otherSymbol as well, that are connected to symbolToSize, and that are on the same majority edge

// sort the ports by the edge
// find the longest contiguous segment of ports on the edge of a and b that are connected to each other
// if that segment length is just one, then just align and skip (call reSizeSymbol)
// get the port distances on symbolToSize, call it x
// get the port distances on otherSymbol, call it y

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

// let rec processSheetModel
//     passCount
//     maxPasses
//     currentSheetModel
//     cleanUpRecord
//     newSheetModel
//     newMovedSymbol
//     newStraightWire
//     =
//     if passCount > maxPasses then
//         printfn "Maximum passes reached"
//         currentSheetModel
//     elif checkIfGainedOrMaintainedIntersections currentSheetModel newSheetModel then
//         newSheetModel
//     else
//         printfn "Pass %d with symbol id %A" passCount cleanUpRecord.Symbol.Id
//         let intersectingBBoxes =
//             findAllBoundingBoxesOfSymIntersections newMovedSymbol newSheetModel // find intersecting boxes
//         let newOffset, majorityDisplacementOffset =
//             match
//                 cleanUpRecord.IsPortInput,
//                 cleanUpRecord.Wire.InitialOrientation,
//                 (intersectingBBoxes.Length > 0)
//             with
//             | true, Horizontal, true ->
//                 let addedXOffset = // get the x coordinate of the x bounding box furthest away from the symbol and adjust symbol to avoid it
//                     intersectingBBoxes
//                     |> List.minBy (fun box -> box.TopLeft.X)
//                     |> (fun box -> newMovedSymbol.Pos.X - box.TopLeft.X - 10.0)
//                 // can add another condition to set to zero, cancelling the operatio if we have to move the symbol
//                 // too far back
//                 // { X = 10.0; Y = 0.0 }, -10.0
//                 { X = addedXOffset; Y = 0.0 }, -addedXOffset
//             | false, Horizontal, true ->
//                 let addedXOffset = // get the x coordinate of the x bounding box furthest away from the symbol and adjust symbol to avoid it
//                     intersectingBBoxes
//                     |> List.maxBy (fun box -> box.TopLeft.X)
//                     |> (fun box -> box.TopLeft.X - newMovedSymbol.Pos.X + 10.0)
//                 { X = -addedXOffset; Y = 0.0 }, addedXOffset

//             | true, Vertical, true ->
//                 let addedYOffset = // get the y coordinate of the y bounding box furthest away from the symbol and adjust symbol to avoid it
//                     intersectingBBoxes
//                     |> List.minBy (fun box -> box.TopLeft.Y + box.H)
//                     |> (fun box -> newMovedSymbol.Pos.Y - box.TopLeft.Y - box.H)
//                 { X = 0.0; Y = addedYOffset }, -addedYOffset
//             | false, Vertical, true ->
//                 let addedYOffset =
//                     intersectingBBoxes
//                     |> List.maxBy (fun box -> box.TopLeft.Y + box.H)
//                     |> (fun box -> box.TopLeft.Y + box.H - newMovedSymbol.Pos.Y)
//                 { X = 0.0; Y = -addedYOffset }, addedYOffset
//             | _, _, _ -> cleanUpRecord.Offset, cleanUpRecord.MajorityDisplacementOffset
//         let newCleanUpRecord =
//             if abs (majorityDisplacementOffset) > maxOffset then
//                 printfn "Offset too large"
//                 cleanUpRecord
//             else
//                 { cleanUpRecord with
//                     Offset = newOffset
//                     Symbol = newMovedSymbol
//                     Wire = newStraightWire
//                     MajorityDisplacementOffset = majorityDisplacementOffset }
//         let newNewStraightWire, newNewMovedSymbol =
//             moveSymbolWireCleanUpRecord newCleanUpRecord
//         let newNewSheetModel =
//             newSheetModel
//             |> updateSheetSymWithNewSym newNewMovedSymbol
//             |> updateSheetWireWithNewWire newNewStraightWire
//         processSheetModel
//             (passCount + 1)
//             maxPasses
//             newSheetModel
//             newCleanUpRecord
//             newNewSheetModel
//             newNewMovedSymbol
//             newNewStraightWire

// // Call the function with initial values
// processSheetModel
//     0
//     maxPasses
//     currentSheetModel
//     cleanUpRecord
//     newSheetModel
//     newMovedSymbol
//     newStraightWire
//
// check for intersections. If there are none, then return the new model else return the old model

// for every port on the symbol to size, try find if it is connected to a port on the other symbol
// let sortedEdgeConnections : (Edge * (PortId * PortId) list ) list =
//     let edgeConnections =
//         symbolToSizeAllPorts
//             |> List.groupBy (fun portId -> symbolToSize.PortMaps.Orientation[portId.ToString()])
//             |> List.map (fun (edge, symToSizeEdgePorts) ->
//                 // for each port on the symToSize, count it if it is connected to a port on the other symbol that is on the opposite edge
//                 // find other symbol's ports using the connection map
//                 let oppositeEdgePortPairs =
//                     symToSizeEdgePorts
//                     |> List.map (fun symToSizeEdgePort ->
//                         symToSizeEdgePort, (connectionMap |> Map.tryFind symToSizeEdgePort))
//                     |> List.choose (fun (symToSizeEdgePort, otherSymbolPortOption) ->
//                         match otherSymbolPortOption with
//                         | Some otherSymbolPort ->
//                             if
//                                 otherSymbolAllPorts
//                                 |> List.contains otherSymbolPort
//                             then
//                                 Some(symToSizeEdgePort, otherSymbolPort)
//                             else
//                                 None
//                         | None -> None)
//                     |> List.filter (fun (_, otherPort) ->
//                         otherSymbol.PortMaps.Orientation[(otherPort.ToString())] = findOpposite edge)
//                 (edge, oppositeEdgePortPairs))
//     edgeConnections
//         |> List.sortByDescending (fun (_, oppositeEdgePortPairs) -> oppositeEdgePortPairs.Length)
