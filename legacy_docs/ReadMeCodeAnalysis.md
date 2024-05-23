# DrawBlock Analysis <!-- omit in toc -->

# Table of Contents <!-- omit in toc -->

- [Sheet](#sheet)
  - [Symbol Alignment](#symbol-alignment)
- [Buswire](#buswire)
  - [Manual routing](#manual-routing)
  - [Auto-route](#auto-route)
    - [Initial Segment List](#initial-segment-list)
  - [Partial Auto-routing](#partial-auto-routing)
  - [Smart auto-routing](#smart-auto-routing)
  - [Radial wires](#radial-wires)
- [Symbols](#symbols)
  - [Auto-sizing](#auto-sizing)
  - [Port placement](#port-placement)
  - [Symbol Drawing](#symbol-drawing)
- [New ISSIE Components](#new-issie-components)

# Sheet

## Symbol Alignment

This allows symbol edges to stick to other symbol edges when moving them in ISSIE. As it stands it calculates the edges of all the symbols in the sheet then the margins between the these edges and the clicked component. These margins are added to the legacy code that snaps to grid lines. There is an obvious question / issue about the performance especially as the number of symbols increases as well as the usability of the program if there are continuous snappings whenever a component is moved. To reduce this the margin for snapping is reduced (also eliminating an issue where a component would snap with a large gap between itself and other edge) and only distinct edges are used to calculate margins. The critical section in the algorithm is the `checkForSnap1D` process, previously O(1) as only grid lines were used, now O(n) with n being the number of distinct symbol edges. Our potential performance issue therefore could be reduced by limiting the number of margins added to the function which could be done by only taking nearby components. There is also room to scale this for moving multiple components by using the furthest edges on each side for the unit as a whole.

# Buswire

## Manual routing

Manual routing has been fully reworked to have the following behaviour:

- A **non-binding segment** can be dragged anywhere
- A **binding segment** cannot be dragged past the **nubLength** of the wire, which is the minimum length of the nubs coming out of the ports (defined as `static member nubLength = 8.0` in `Wire` type)

A segment is defined as binding for a particular port if it is the first segment of non-zero length perpendicular to the port's nub. See the diagram below for examples:

![binding_segments_examples](img/analysis/bindingSegment.svg)

## Auto-route

Auto-routing is done in 3 stages:

1. Normalise the routing problem so that the output port is facing right
2. Generate the [initial segment list](#initial-segment-list)
3. Rotate the problem back to it's in the original orientation

### Initial Segment List

Segments are generated based off of the two ports for each wire, with the assumption that the output port is always facing right. The orientation of the input port is checked as well as its relative position to the output port, allowing us to generate an initial segment list. This segment list consists of a small “nub” segment immediately joining the input and output port. These are followed by 0 length segments in order to facilitate previous functionality of ISSIE where we could drag wires fully. After these 0 length segments we create the remaining segments to link the two ports. These distances are either set to halfway between the two ports, or a small distance in order to get past the boundaries of a symbol.

## Partial Auto-routing

Partial auto-routing attempts to preserve any manual routing when symbols (and their ports) are moved. The conditions for this preservation is as follows:

- For a given port being moved, the **fixed point** is defined as the end of the first manual segment, traversing the wire **from the port being moved**.
- If the moved port is in the same quadrant relative to the **fixed point**, manual routing is preserved by scaling the segment containing the fixed point and the one preceding it by the appropriate XY offset.
- Else, we revert to full auto-routing.

See the diagram below for an example of the region where partial auto-routing will be applied:

![partial_autoroute_example](img/analysis/partialAutoroute.svg)

## Smart auto-routing

Following the discussion during the project demonstration, the following section has been added to detail the design philosophy around smart auto-routing. Implementing smart auto-routing was considered, and after some exploratory discussions was ultimately rejected. It is of critical importance that a smart auto-routing implementation does not cause un-intutive behaviour for routing problems (i.e. either it works well or it does nothing). Although a heuristic based approach could work well for simple cases, and could be disabled for complex ones, despite this however, we felt as though this approach was not forward thinking. A complete routing solution, involving a pathfinding algorithm like A\* or Dijkstra's, was considered to be the ideal approach. This would require completely rewriting the current routing logic, which is non-trivial, particularly for a project of this scope. We felt as though building on the complexity of the current implementation would make re-writing the autorouting more difficult for future maintenance and updates of the code.

## Radial wires

Radial wires are one of the wire types selectable in the view menu, which shows if wire are connected by having a curve. The default radius for these curves is defined as `static member radius = 5.0` in `Wire` type. However, for very small wires, this radius is changed to prevent visual bugs. When drawing radii, the length of the smallest segment the curve connects to is checked, and if its length is <5, the radius is shrunk to match it. Due to the limitations of drawing Arcs in SVG, these radii can only be integer valued, leading to small inconsistencies when a segment is a small non integer value (i.e. 1.5).

# Symbols

## Auto-sizing

Custom components are dynamically resized depending on their port configuration. The minimum distance distance between 2 ports is set as `GridSize = 30`, which is defined as a `[<Literal>]` at the top of `Symbol.fs`. The dimensions of a component are determined as follows:

- **Height:** Determined purely from the maximum number of ports on the left or right edge (n), setting the height to (n+1)`GridSize`, and spreading the ports GridSize apart.
- **Width:** The distance between ports is determined by the maximum between the largest port label and `GridSize`. This ensures that the labels of the ports cannot overlap, whilst still being a minimum distance apart. The width can be calculated for both the Top and Bottom edges using the same approach as above using the maximum value, and taking the largest of these 2 values. In addition, the length of the longest labels from the Left and Right sides, as well as the component label (displayed in the center of the component) are added up and compared to the width obtained from the previous calculation. By selecting the component width as the maximum of these values, it can be ensured that labels associated with the component / ports do not overlap.

Custom components' ports can be placed to different edges on the Symbol by pressing Ctrl and dragging the port.
When a port is dragged onto a different edge, the width and height of the component is automatically resized. The ports on one edge are always equidistant. The height of the component is determined purely from the number of ports on the left or right edge, depending on which one has more ports. The width of the component also considers the lengths of the ports on the top and bottom edges. The distance between ports on the top and bottom edges is big enough, such that it can fit the longest portlabel on the edge, but never smaller than 1 gridsize. The necessary width of the top/bottom edge is determined from this distance and from the number of ports on this edge. The width of the component is given by either the top or bottom edge width, whichever is bigger.

## Port placement

The ordering of the ports is represented by their index in the list associated to a particular edge of the Symbol (Top, Left, Bottom, Right). We allow ports to be moved for custom components by clicking and dragging the port while holding down the `Ctrl` key. When a port is dropped, the target index is determined from it's position along an edge, (in a process inverse to how the position of the port is calculated based on its index). The port is then removed from its old list, and inserted into the target index of the new list. This also allows the order of ports on the same edge to be re-arranged, by simply removing it and inserting it into the same list.

## Symbol Drawing

The function `rotatePoints` is a useful function that allowed us to avoid the hassle of endless match statements when drawing the react element for the shape outline. It can take a set of points (which usually map to the points needed to draw the shape) and rotate them about the centre of a shape depending on the transformations applied to the shape (flipped / rotated) without additional storage for the current points / transformation history. To achieve this it uses a small hack so that while a shape is flipped a rotateLeft cmd will tell the points to rotate right. This is a limitation of the function as it always performs any rotation then flipping first. This was needed because a flip then rotation 90&deg; clockwise is not necessarily equivalent to a rotation 90&deg; clockwise then flip. However a flip then rotation 90&deg; clockwise is equivalent to a rotation 90&deg; anti-clockwise then flip.

# New ISSIE Components

The Mux4, Mux8, Demux4 and Demux8 components have been implemented as new ISSIE components. A few issues were encountered when doing this:

- Rendering issues: The new components have different shapes compared to Mux2 and Demux2 so `getMuxSelOffset` was modified to return correct offset depending on the component.
- Select bit issues: Originally, the `extractBit` function has an assertion to ensure the select input only has a width of 1. However, there is now a possibility of select inputs of width 2 or 3, so a `busWidth` parameter was added for the width size of select.
- Logic issues: The match statements for all the different widths need to be covered, outputting an error if there is a width mismatch for the inputs.

The new components were tested in 2 ways:

- New components were generated and checked if they are rendered properly by applying a combination of flips and rotates on the components.
- Different combinations of inputs were tested in simulation, ensuring that the output produces the correct value. Input combinations were not tested exhaustively due to the sheer number of combinations (particularly for the MUX/DEMUX8), but sufficient tests were performed until a high degree of confidence was achieved.
