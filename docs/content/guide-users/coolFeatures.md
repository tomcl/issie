---
title: Schematic Editor Features
category: Documentation
categoryindex: 1
index: 3
---

# Summary of Schematic Editor Operations

ISSIE features developed over several years to help you create readable schematics very quickly. These are summarised in the table below:

| Feature | How | Explanation   |
| :----:      |    :----:   |    :----:     |
| **Size-to-fit** | `Ctrl-W` | Most often used keyboard shortcut to zoom and centre schematic so it is all fitted onto the screen at the correct zoom.
| **Change port location on Custom Components** | Press `CTRL` and use your mouse to drag a port to another position on the outline of the symbol | Custom components may end up having a large number of ports. You can move them to your desired location on the outline to create nicer schematics |
| **Resizing of Custom Components** | Change `Width Scale` and `Height Scale` on the properties pane | If default sizing makes port legends overlap you can scale custom component width and height in Properties |
| **Move component's label** | Simply drag the component's label to your desired location on the canvas | |
| **Rotate and flip components** | `Ctrl + left arrow`: <br> `Ctrl + right arrow`:  <br> `Ctrl + up arrow`:  <br> `Ctrl + down arrow`: | Rotate clockwise <br> Rotate anti-clockwise <br> Flip vertically <br> Flip horizontally |
| **Truth Table for combinational logic** | `Simulations` -> `Truth Table` | View the truth table for a combinational logic circuit. This can be either a full sheet or a sub-set by selecting the components you want to be included in the truth table |  
| **Add a description on your sheets** | De-select all components and click on properties. Click the `Add Description` button| Sheet description will appear on the sheet list as an &#9432; button and will be displayed on properties when that sheet is used as a custom component |
| **Themes** | `View` -> `Theme` | Choose between the 3 supported themes: `Grayscale`, `Light` or `Colourful`  |
| **Grid** | `View` -> `Toggle grid` | Choose whether you want a grid to appear on your sheets  |
| **Wire Type** | `View` -> `Wire Type` | Choose between the 3 supported wire types: `Jump`, `Radiussed` or `Modern` wires  |
| **Wire Arrows** | `View` -> `Toggle Wire Arrows` | Choose whether you want arrows at the end-points of your wires to show the direction of signals |
| **Auto-routing with fixed segments** |     | Issie will **nearly always** route and separate all schematic connections neatly with no manual routing required. Any wire segment can be manually dragged to a desired position and "fixed" with other segments and wires auto-routed around it.|
| **Right-click context menus** | Right-click on any schematic component or the background sheet will give you useful actions |
| **Keyboard shortcuts** | Most actions have shortcuts documented from the main **info** button or individually on menus. |

<br><br>

# Details of wire routing and symbol alignment

### Snapping

This allows symbol edges to stick to other symbol edges when moving them in ISSIE, or symbols to stick to positions that make wires straight.

### Explicit arrangement

Selections of same-type components can be aligned vertically or horizontally, or distributed with equal spacing. Differing type components are omitted from the operation even if selected.

### Manual wire routing

Manual routing has been reworked to have the following behaviour:

- A **non-binding segment** can be dragged anywhere
- A **binding segment** cannot be dragged past the **nubLength** of the wire, which is the minimum length of the nubs coming out of the ports (defined as `static member nubLength` in `Wire` type)

A segment is defined as binding for a particular port if it is the first segment of non-zero length perpendicular to the port's nub. 

### Wire auto-routing

Auto-routing is done in 5 stages:

1. Normalise the routing problem so that the output port is facing right
2. Generate the [initial segment list](#initial-segment-list)
3. Rotate the problem back to it's in the original orientation
4. Run an iterative autorouting algorithm that attempts to create a valid route not overlapping any symbol by moving initial segments.
5. Run a whole-sheet segment separation algorithm that spreads wires out evenly.

#### Initial segment List

Segments are generated based off of the two ports for each wire, with the assumption that the output port is always facing right. The orientation of the input port is checked as well as its relative position to the output port, allowing us to generate an initial segment list. This segment list consists of a small “nub” segment immediately joining the input and output port. These are followed by 0 length segments in order to facilitate previous functionality of ISSIE where we could drag wires fully. After these 0 length segments we create the remaining segments to link the two ports. These distances are either set to halfway between the two ports, or a small distance in order to get past the boundaries of a symbol.

<br><br>

# Details of Operations

### Copy and paste

The on-screen copy & paste buttons can duplicate single components, or selected sets of components (see selection below). They can also be used to copy components from one design sheet to a new one.

### Canvas zooming

The canvas can be zoomed in or out, or auto-zoomed and panned to fit the whole circuit using `Ctrl-W` (`Cmd-W`).

### Symbol rotation

Individual symbols can be rotated or flipped using the right-click menu or the Issie edit menu.

### Wire display types

Wires can be displayed as radial, modern, or old-style jump. Switching display types can be done at any time and does not chnage the schematic.

Radial wires are the mots interesting (and readable) form of wire display. Wire bends have small quadrant connections thus distinguishing between wires that cross and a wire joining two perpendicular wires. Visually, radial display makes connectivity easier to follow. The default radius for the wire quadrants is defined as `static member radius = 5.0` in `Wire` type. However, for very small wires, this radius is changed to prevent visual bugs. When drawing radii, the length of the smallest segment the curve connects to is checked, and if its length is <5, the radius is shrunk to match it. Due to the limitations of drawing Arcs in SVG, these radii can only be integer valued, leading to small inconsistencies when a segment is a small non integer value (i.e. 1.5).


### Symbol auto-sizing

Custom components (symbols) are dynamically resized depending on their port configuration. The minimum distance distance between 2 ports is set as `GridSize = 30`, which is defined as a `[<Literal>]` at the top of `Symbol.fs`. The dimensions of a component are determined as follows:

- **Height:** Determined purely from the maximum number of ports on the left or right edge (n), setting the height to (n+1)`GridSize`, and spreading the ports GridSize apart.
- **Width:** The distance between ports is determined by the maximum between the largest port label and `GridSize`. This ensures that the labels of the ports cannot overlap, whilst still being a minimum distance apart. The width can be calculated for both the Top and Bottom edges using the same approach as above using the maximum value, and taking the largest of these 2 values. In addition, the length of the longest labels from the Left and Right sides, as well as the component label (displayed in the center of the component) are added up and compared to the width obtained from the previous calculation. By selecting the component width as the maximum of these values, it can be ensured that labels associated with the component / ports do not overlap.

Custom components' ports can be placed to different edges on the Symbol by pressing `Ctrl` (`Cmd`) and dragging the port.
When a port is dragged onto a different edge, the width and height of the component is automatically resized. The ports on one edge are always equidistant. The height of the component is determined purely from the number of ports on the left or right edge, depending on which one has more ports. The width of the component also considers the lengths of the ports on the top and bottom edges. The distance between ports on the top and bottom edges is big enough, such that it can fit the longest portlabel on the edge, but never smaller than 1 gridsize. The necessary width of the top/bottom edge is determined from this distance and from the number of ports on this edge. The width of the component is given by either the top or bottom edge width, whichever is bigger.

The same UI (dragging component corners) can be used manually to override auto-sizing and make the custom symbol a desired size.

### Port placement

The ordering of the ports on  a custom symbol is represented by its index in the list associated to a particular edge of the Symbol (Top, Left, Bottom, Right). We allow ports to be moved for custom components by clicking and dragging the port while holding down the `Ctrl` key. 

### Schematic rotation and scaling

Any **group of components** on the schematic can be selected, and then rotated and/or scaled. 

* Dragging the mouse will select a rectangle. 
* Shift-click will add or remove a component from the current  selection.
* A selected block of components will show icons for intuitive rotating and scaling.



