module SheetBeautifyD1

open DrawModelType
open CommonTypes
open DrawModelType.SymbolT
open DrawModelType.SheetT
open DrawModelType.BusWireT
open Optics
open Optics.Operators
open Helpers
open SymbolHelpers
open SheetBeautifyHelpers
open BlockHelpers
open Symbol
open BusWireRoute
open BusWire

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

/// Constant that decides if a wire can be straightened. This ratio is the majority distance over the maximum minority deviation length.
let straightenRatioTolerance = 9.0

let detectAlmostStraightWire (wire: BusWireT.Wire) =
    // Get list of even segments and odd segments of the wire. Note: we get rid of invisible segments
    let wireWithInvisSegmentsRemoved = (removeSingleWireInvisibleSegments wire)
    let oddList, evenList =
        wireWithInvisSegmentsRemoved.Segments
        |> List.mapi (fun i x -> (i % 2 = 0, x))
        |> List.partition fst
        |> fun (odd, even) -> (List.map snd odd, List.map snd even)

    let oddDistance =
        oddList
        |> List.sumBy (fun segment -> segment.Length)
    let evenDistance =
        evenList
        |> List.sumBy (fun segment -> segment.Length)

    let majorityDistance, isMajorityDirectionHoriz =
        if oddDistance >= evenDistance then
            oddDistance, true
        else
            evenDistance, false

    // can't be straightened if there are less than 2 segments OR segment length is even
    if
        (wireWithInvisSegmentsRemoved.Segments.Length < 2)
        || (wireWithInvisSegmentsRemoved.Segments.Length % 2 = 0)
    then
        false
    else
        match wire.InitialOrientation, isMajorityDirectionHoriz with
        | horizontal, true -> // first seg horiz, majority horiz, will deviate vertically, which will be the even segments
            let maxDeviationLength =
                (evenList
                 |> List.maxBy (fun segment -> abs (segment.Length)))
                    .Length
            let ratio = (abs (oddDistance) / abs (maxDeviationLength))
            printf "Ratio: %A" ratio
            ratio > straightenRatioTolerance
        | _ -> false

/// To show on DeveloperModeView SheetStats
let countAlmostStraightWiresOnSheet (sheetModel: SheetT.Model) =
    let straightWires =
        sheetModel.Wire.Wires
        |> Map.filter (fun _ wire -> detectAlmostStraightWire wire)
    straightWires.Count

(*  Cases for detectAlmostStraightWire
                                                     __________            __________
            ______                                  |         | No        |         |
    _______|     |________ almost straight          |         |           |         |
                                                                 _________|         |__________
                    __________________                          |                             | No
    _______________|      almost straight

    make sure it won't detect edge cases, such as a tall user-generated stepladder wire that moves upwards in small increments.
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
    the horizontal segments travel a greater distance, the majority direction is horizontal.

    Next, calculate he majority distance, which is the sum of the segments in the majority direction.

    After determining majority direction, ake sure the first and last segments are also travelling
    in the majority direction. Can be done by checking initialOrientation, and then making sure the length is odd.
    If even, discard, can't straighten a wire that is L-shaped .

    Then, check for the maximum deviation in the minority direction. If the deviation-to-majority-distance
    ratio is less than a certain threshold, then the wire is almost straight.

*)
// check if InitialOrientation is horizontal or vertical. If horizontal, check every even segment of wire.Segments and find the max abs length

// calculate endpos of wire, compare to startpos

//Todo: A helper for the project spec D1 Basic Wire Straightening Algo spec4-5:
// get wires that are almost straight with a single component at one end that is singly connected.
// (i.e. the comp. has one port only that is connected to that wire)

(*  Cases for detecting Straightenable Wires (note we consider more cases than AlmostStraightWires )
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


    algo: check whether the wire travels furthest horizontally or vertically. If vertically

*)
