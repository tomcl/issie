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
