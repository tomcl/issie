---
category: Documentation
categoryindex: 1
index: 4
---

# Schematic Editor Features

### Copy and Paste

The on-screen copy & paste buttons can duplicate single components, or sets of components. They can also be used to copy components from one design sheet to a new one.

### Symbol rotation

Individual symbols can be rotated or flipped using the right-click menu or the Issie edit menu.


### Symbol Alignment

This allows symbol edges to stick to other symbol edges when moving them in ISSIE. As it stands it calculates the edges of all the symbols in the sheet then the margins between the these edges and the clicked component. These margins are added to the legacy code that snaps to grid lines. There is an obvious question / issue about the performance especially as the number of symbols increases as well as the usability of the program if there are continuous snappings whenever a component is moved. To reduce this the margin for snapping is reduced (also eliminating an issue where a component would snap with a large gap between itself and other edge) and only distinct edges are used to calculate margins. The critical section in the algorithm is the `checkForSnap1D` process, previously O(1) as only grid lines were used, now O(n) with n being the number of distinct symbol edges. Our potential performance issue therefore could be reduced by limiting the number of margins added to the function which could be done by only taking nearby components. There is also room to scale this for moving multiple components by using the furthest edges on each side for the unit as a whole.


### Manual wire routing

Manual routing has been fully reworked to have the following behaviour:

- A **non-binding segment** can be dragged anywhere
- A **binding segment** cannot be dragged past the **nubLength** of the wire, which is the minimum length of the nubs coming out of the ports (defined as `static member nubLength = 8.0` in `Wire` type)

A segment is defined as binding for a particular port if it is the first segment of non-zero length perpendicular to the port's nub. See the diagram below for examples:

![binding_segments_examples](img/analysis/bindingSegment.svg)

### Wire Auto-routing

Auto-routing is done in 4 stages:

1. Normalise the routing problem so that the output port is facing right
2. Generate the [initial segment list](#initial-segment-list)
3. Rotate the problem back to it's in the original orientation
4. Run a whole-sheet segment separation algorithm that spreads wires out evenly.

#### Initial Segment List

Segments are generated based off of the two ports for each wire, with the assumption that the output port is always facing right. The orientation of the input port is checked as well as its relative position to the output port, allowing us to generate an initial segment list. This segment list consists of a small “nub” segment immediately joining the input and output port. These are followed by 0 length segments in order to facilitate previous functionality of ISSIE where we could drag wires fully. After these 0 length segments we create the remaining segments to link the two ports. These distances are either set to halfway between the two ports, or a small distance in order to get past the boundaries of a symbol.


### Radial wires

Radial wires are one of the wire types selectable in the view menu, which shows if wire are connected by having a curve. The default radius for these curves is defined as `static member radius = 5.0` in `Wire` type. However, for very small wires, this radius is changed to prevent visual bugs. When drawing radii, the length of the smallest segment the curve connects to is checked, and if its length is <5, the radius is shrunk to match it. Due to the limitations of drawing Arcs in SVG, these radii can only be integer valued, leading to small inconsistencies when a segment is a small non integer value (i.e. 1.5).


### Symbol Auto-sizing

Custom components (symbols) are dynamically resized depending on their port configuration. The minimum distance distance between 2 ports is set as `GridSize = 30`, which is defined as a `[<Literal>]` at the top of `Symbol.fs`. The dimensions of a component are determined as follows:

- **Height:** Determined purely from the maximum number of ports on the left or right edge (n), setting the height to (n+1)`GridSize`, and spreading the ports GridSize apart.
- **Width:** The distance between ports is determined by the maximum between the largest port label and `GridSize`. This ensures that the labels of the ports cannot overlap, whilst still being a minimum distance apart. The width can be calculated for both the Top and Bottom edges using the same approach as above using the maximum value, and taking the largest of these 2 values. In addition, the length of the longest labels from the Left and Right sides, as well as the component label (displayed in the center of the component) are added up and compared to the width obtained from the previous calculation. By selecting the component width as the maximum of these values, it can be ensured that labels associated with the component / ports do not overlap.

Custom components' ports can be placed to different edges on the Symbol by pressing Ctrl and dragging the port.
When a port is dragged onto a different edge, the width and height of the component is automatically resized. The ports on one edge are always equidistant. The height of the component is determined purely from the number of ports on the left or right edge, depending on which one has more ports. The width of the component also considers the lengths of the ports on the top and bottom edges. The distance between ports on the top and bottom edges is big enough, such that it can fit the longest portlabel on the edge, but never smaller than 1 gridsize. The necessary width of the top/bottom edge is determined from this distance and from the number of ports on this edge. The width of the component is given by either the top or bottom edge width, whichever is bigger.

The same UI can be used manually to override auto-sizing and make the cuctom symbol a desired size.

### Port placement

The ordering of the ports on  acustom symbol is represented by its index in the list associated to a particular edge of the Symbol (Top, Left, Bottom, Right). We allow ports to be moved for custom components by clicking and dragging the port while holding down the `Ctrl` key. 

### Schematic rotation and scaling

Any **set of components** on the schematic can be highlighted, and then rotated and/or scaled.
