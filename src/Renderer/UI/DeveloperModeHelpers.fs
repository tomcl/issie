module DeveloperModeHelpers

open DrawModelType
open CommonTypes
open DrawModelType.SheetT
open DrawModelType.BusWireT
open Optics
open Helpers
open BlockHelpers
open Symbol
open BusWireRoute
open BusWire
open BusWireUpdateHelpers
open ModelType
open BusWireRoutingHelpers


// --------------------------------------------------- //
//                      Constants                      //
// --------------------------------------------------- //

module Constants =
    /// Constant that decides if a wire is classified as almost-straight, if its longest segment in the minority direction is shorter than this length
    let maxDeviationLengthThresholdAlmostStraight = 30.0
    let maxMinorityDisplacementThreshold = 300

open Constants

// --------------------------------------------------- //
//                      Helpers                        //
// --------------------------------------------------- //

// Return a list of segment lengths with 3 lengths coalesced into 1, if 0.0 length appear,
// otherwise return segment lengths unchanged.
let coalesceWire (wire: Wire) : float list =
    let rec coalesceSegLengths (segLengths: float list) =
        match segLengths with
        | l1 :: 0.0 :: l2 :: rest -> coalesceSegLengths ((l1 + l2) :: rest)
        | l :: rest -> l :: (coalesceSegLengths rest)
        | [] -> []
    wire.Segments
    |> List.map (fun seg -> seg.Length)
    |> coalesceSegLengths

// Take a list of wires and group them into their "nets" (i.e same Output Port).
// Returns a List of nets (where a net = List of wires)
let groupWiresByNet (wireList: Wire list) : Wire list list =
    wireList
    |> List.groupBy (fun w -> w.OutputPort) // Group wires by same Net
    |> List.map snd // Don't need the key in fst, just the wires grouped in snd


// Take a sheet and return all Wires on it in a list
let getAllWires (model: SheetT.Model) : Wire list =
    model.Wire.Wires |> Map.toList |> List.map snd



/// overlap2DBoxvariant from BlockHelpers. Returns a bounding box of an overlap area between two bounding boxes
// Used in DeveloperModeView
let overlapArea2DBox (bb1: BoundingBox) (bb2: BoundingBox) : BoundingBox option =
    let xOverlap =
        max
            0.0
            (min (bb1.TopLeft.X + bb1.W) (bb2.TopLeft.X + bb2.W)
             - max bb1.TopLeft.X bb2.TopLeft.X)
    let yOverlap =
        max
            0.0
            (min (bb1.TopLeft.Y + bb1.H) (bb2.TopLeft.Y + bb2.H)
             - max bb1.TopLeft.Y bb2.TopLeft.Y)

    if xOverlap > 0.0 && yOverlap > 0.0 then
        let overlapTopLeft =
            { X = max bb1.TopLeft.X bb2.TopLeft.X; Y = max bb1.TopLeft.Y bb2.TopLeft.Y }
        Some { TopLeft = overlapTopLeft; W = xOverlap; H = yOverlap }
    else
        None

/// For DeveloperModeView. Similar to countIntersectingSymbolPairs but prints out the boxes that intersect
let countIntersectingSymbolPairsWithOverlapArea (model: SheetT.Model) =
    let boxes =
        mapValues model.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> n, box)

    let bBoxes =
        List.allPairs boxes boxes
        |> List.filter (fun ((n1, box1), (n2, box2)) -> (n1 < n2) && BlockHelpers.overlap2DBox box1 box2)
        |> List.map (fun ((n1, box1), (n2, box2)) -> overlapArea2DBox box1 box2)
        |> List.choose id

    bBoxes
    // |> List.map (fun box -> printf "Box: %A" box)
    |> List.length


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
// ########
// Important Note!!!: this function should only be used for calculations and not modifying the sheet. This is because it causes all wires to lose their nubs
// (10-length + zero-length) segment pairs, runing a functionality where the wire can no 'grow' additional segments when dragged.
// see BusWireUpdateHelper for more details. If you must use this, the helper function below  makeAllWiresDraggable to restore the nubs.
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

// note: propose name change to team to have removeWireInvisibleSegments above be renamed with 'wires' instead of 'wire', and
// rename removeSingleWireInvisibleSegments to just removeWireInvisibleSegments
let removeSingleWireInvisibleSegments (wire: Wire) =
    let uniqueVertices =
        segmentsToIssieVertices wire.Segments wire
        |> List.distinctBy (fun (x, y, _) -> (x, y))
    let newSegments = issieVerticesToSegments wire.WId uniqueVertices
    // for each wire, set the segments to the new segments
    wire |> Optic.set segments_ newSegments

let makeAllWiresDraggable (wires: Map<ConnectionId, Wire>) =
    wires
    |> Map.map (fun connId wire ->
        wire
        |> Optic.set segments_ (makeEndsDraggable wire.Segments))


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
/// Function that detects if a wire is almost straight.
/// Author: tdc21/Tim
/// </summary>
let checkAlmostStraightWire (wire: BusWireT.Wire) (deviationLengthThreshold: float) =
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
            abs (maxDeviationLength) < deviationLengthThreshold
        | Vertical, true -> // first seg vertical, majority vertical, will deviate horizontally, which will be the even segments
            let maxDeviationLength =
                (evenList
                 |> List.maxBy (fun segment -> abs (segment.Length)))
                    .Length
            abs (maxDeviationLength) < deviationLengthThreshold
        | _, _ -> false

(*  Cases for checkAlmostStraightWire             (Not travelling in majority direction)
                                               |    ___________           ___________
            ______                             |    |         | No        |         |
    _______|     |________ almost straight     |    |         |           |         |
                                               |                 _________|         |__________
                    __________________         |                |                             | No
    _______________|      almost straight      |

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

    algo: check whether the wire travels furthest horizontally or vertically. This is the called the majority direction, and the other direction is the minority direction.

    * Not to be confused with the number of segments, e.g. there can be more vertical segments than horizontal segments, but if
    the horizontal segments cover a greater displacement, the majority direction is horizontal.

    Next, calculate the majority displacement, which is the sum of the segment lengths in the majority direction.

    After determining majority direction, ake sure the first and last segments are also travelling
    in the majority direction. Can be done by checking initialOrientation, and then making sure the length is odd.
    Another way to reframe this: any wire (odd segment length) must have an initial orientation that is the same as the majority direction.
    (in odd length, first and last segments will be in majority direction)

    If even length, discard, can't straighten a wire that is 'diagonal/L' shaped. Assume no invisible segments since we've removed them

    Then, check for the maximum deviation in the minority direction. If the deviation-to-majority-displacement ratio is less than a certain threshold, then the wire is almost straight.
*)

// --------------------------------------------------- //
//            Metrics for DeveloperModeView            //
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
    |> List.filter (fun ((n1, box1), (n2, box2)) -> (n1 < n2) && BlockHelpers.overlap2DBox box1 box2)
    |> List.length


/// <summary>
/// T2R: The number of distinct wire visible segments that intersect with one or more symbols. See Tick3.HLPTick3.visibleSegments for a helper. Count over all visible wire segments.
/// Assumes that within one wire at most one segment crosses a symbol boundary. Not always true, but works for a metric
/// </summary>
/// <param name="model">The model to count the intersecting wire segments of.</param>
/// <returns>The number of distinct wire visible segments that intersect with one or more symbols.</returns>
let countVisibleSegsIntersectingSymbols (model: SheetT.Model) =

    // SheetUpdateHelpers has not implemented updateBoundingBoxes yet on master
    let wModel =
        model
        |> Optic.set boundingBoxes_ (Symbol.getBoundingBoxes model.Wire.Symbol)
        |> Optic.map symbols_ (Map.map (fun _ sym -> Symbol.calcLabelBoundingBox sym)) // just in case
        |> fun model -> model.Wire

    wModel.Wires
    |> Map.values
    |> Seq.map (fun wire -> (findWireSymbolIntersections wModel wire))
    |> Seq.sumBy (function
        | [] -> 0
        | _ -> 1)


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
    let wireMap = removeWireInvisibleSegments model.Wire.Wires // Get all the wires from the Wire model
    let wires = Map.toList wireMap

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

// T4R Author: MHC21

// ---- INITIAL FUNCTION : countVisibleSegmentLength -------------------------------------
// (1) First sort all wires, on the sheet, into their nets giving a "Segment list list list"
//     where "a net = Segment list list ".
// (2) List.fold over each net which is fed into a recursive function to find the visible
//     segment length of the net.

// ---- RECURSIVE FUNCTION : netVisibleSegmentLength -------------------------------------
// (3) The recursive function starts and checks the length of "index 0 Segment" of each
//     wire in the net.
// (4.1) As all the "positive length index 0 Segments" overlap, you take the max (or 0 if
//       "max < 0") (which is added to running total).
// (4.2) As all the "negative length index 0 Segments" overlap, you take the min (or 0 if
//       "min > 0")(which is added to running total).
// (5) Next you need to form the subNets, which is done by grouping wires by their "index
//     0 Segment length", ( Because this length tells you where the "wire split-off" or
//     "corner" will occur for the next index (i.e 1) of that wire. If the 2 wires have
//     this same length then they will "corner off" at the same point). You then take the
//     tail of each wire/Segment List (so only index 1..n of the Segment list).
// (6) Finally you recursively call the function on the subNets which return a float to sum
//     to the running total.
// (7) Base case: When subNet only has 1 Wire. Hence just sum the rest of the wire and
//     return the float.

let rec netVisibleSegmentLength (net: Segment list list) : float =
    match net.Length with // Number of wires in the net
    | 1 -> List.sumBy (fun seg -> abs seg.Length) net[0] // If 1 wire left in subnet, sum rest of the segments in the only wire/subnet
    | n ->
        let fstSegLengths = List.map (fun (wire: list<Segment>) -> wire[0].Length) net // Grab first/next segment for each wire in Net/subNet
        let longestPosSeg = fstSegLengths |> List.max |> max 0.0
        let longestNegSeg = fstSegLengths |> List.min |> min 0.0 |> abs
        let subNets =
            net
            |> List.groupBy (fun wire -> wire[0].Length) // Group wires into subNets
            |> List.map snd // Don't need the length in fst, just the wires grouped in snd
            |> List.map (fun wireList -> List.map (fun segList -> List.tail segList) wireList) // Take the tail of each Segment list (1..n)
        longestPosSeg
        + longestNegSeg
        + List.fold (fun totalLength wireList -> totalLength + (netVisibleSegmentLength wireList)) 0.0 subNets

let countVisibleSegmentLength (model: SheetT.Model) : float =
    // Get all wires on sheet and sort them into their "nets" (same InputPort)
    // (1) netList = list of nets (2) net = list of wires (3) wire = list of segments
    let netList =
        model
        |> getAllWires
        |> groupWiresByNet
        |> List.map (fun lst -> List.map (fun (w: Wire) -> w.Segments) lst)
    List.fold (fun totalLength wireList -> totalLength + (netVisibleSegmentLength wireList)) 0.0 netList


//T5R Author: MHC21
// ---- INITIAL FUNCTION : countVisibleBends -------------------------------------------
// (1) First sort all wires, on the sheet, into their nets giving a "float list list list"
//     where "a net = float list list ". It's a "float list list" due to the "coalesceWire"
//     helper function which converts and coalesces the segments to their lengths.
// (2) List.fold over each net which is fed into a recursive function to find the visible
//     segment length of the net.

// ---- RECURSIVE FUNCTION : netVisibleBends -------------------------------------------
// (3) The recursive function starts and checks the "index 0 Segment Length" of each wire
//     in the net.
// (4) Next you need to form the subNets, which is done by grouping wires by their "index 0
//     Segment Length", ( Because this length tells you where the "wire split-off" or
//     "corner" will occur for the next index (i.e 1) of that wire. If the 2 wires have this
//     same length then they will "corner off" at the same point). You then take the tail of
//     each wire/Segment List (so only index 1..n of the Segment list).
// (5) The length of the subNets (how many subNets/splitOffs/corners made is the number of
//     Bends for that iteration).
// (6) Finally you recursively call the function on the subNets which return an int to sum
//     to the running total.
// (7) Base case: When subNet only has 1 Wire. Hence just "sum the rest of the Segments of
//     the wire" (List.length) and subtract 1 for the rest of the Bends.

let rec netVisibleBends (net: float list list) : int =
    match net.Length with
    | 1 -> (List.length net[0]) - 1
    | n ->
        let subNets =
            net
            |> List.groupBy (fun segList -> segList[0]) // Group wires into subNets
            |> List.map snd // Don't need the length in fst, just the wires grouped in snd
            |> List.map (fun wireList -> List.map (fun segList -> List.tail segList) wireList) // Take the tail of each Segment list (1..n)

        (List.length subNets)
        + List.fold (fun totalBends wireList -> totalBends + (netVisibleBends wireList)) 0 subNets

let countVisibleBends (model: SheetT.Model) : int =
    // Get all wires on sheet and sort them into their "nets" (same InputPort)
    // (1) netList = list of nets (2) net = list of wires (3) wire = list of floats/lengths
    let netList =
        model
        |> getAllWires
        |> groupWiresByNet
        |> List.map (fun lst -> List.map (fun w -> coalesceWire w) lst)
    List.fold (fun totalBends wireList -> totalBends + (netVisibleBends wireList)) 0 netList

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
/// Function that counts the number of almost straight wires on a sheet. A heuristic to show on DeveloperModeView SheetStats
/// Author: tdc21/Tim
/// </summary>
let countAlmostStraightWiresOnSheet (sheetModel: SheetT.Model) =
    let straightWires =
        sheetModel.Wire.Wires
        |> Map.filter (fun _ wire -> checkAlmostStraightWire wire maxDeviationLengthThresholdAlmostStraight)
    straightWires.Count
