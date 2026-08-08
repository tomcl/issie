---
title: Schematic Editor Features
category: Documentation
categoryindex: 1
index: 5
---

# Summary of Schematic Editor Operations

ISSIE features developed over several years to help you create readable schematics very quickly.

> Keys below are given for Windows and Linux. On macOS `Ctrl` is usually `Cmd`, and a few chords
> differ. **The authoritative list for your platform is generated from the code**: press **Info**
> → *Keyboard Shortcuts*. The same actions are also on the **Edit** and **View** menus, and on the
> right-click menus, each labelled with its key.

## Placing and editing components

| Feature | How | Explanation |
| :---- | :---- | :---- |
| **Place a component** | Click it in the Catalogue, then click the canvas — or **drag it straight from the Catalogue** and drop it where you want it | While you drag, a ghost of the real symbol follows the cursor. A drop onto space already occupied by another symbol is refused rather than overlapping them |
| **Component tooltips** | Hover any Catalogue entry, or any field label in **Properties** | Every component explains what it is for, and every field explains what it sets and what people get wrong about it |
| **Find a component** | Type in the Catalogue's search box | Matches the explanations as well as the names, so "subtract" finds the N bits XOR. Sections holding a match open themselves |
| **Size-to-fit** | `Ctrl-W` | Most often used keyboard shortcut: zooms and centres the schematic so it all fits on screen |
| **Zoom / pan the canvas** | `Alt`+`Up` / `Alt`+`Down`; `Ctrl`+mouse wheel; `Shift`+drag (or two-finger scroll) | Zoom in, zoom out, and pan |
| **Rotate and flip components** | `Ctrl`+`Right`: rotate clockwise <br> `Ctrl`+`Left`: rotate anti-clockwise <br> `Ctrl`+`Up`: flip vertically <br> `Ctrl`+`Down`: flip horizontally | Also on the Edit menu and on a component's right-click menu |
| **Undo / redo** | `Ctrl-Z` / `Ctrl-Y`, or the on-screen buttons | |
| **Copy / paste** | `Ctrl-C` / `Ctrl-V`, or the on-screen buttons | Duplicates keep the label with an incremented number. Works across sheets |
| **Move a component's label** | Drag the label; `Ctrl-Shift-Right` rotates it | |
| **Change port location on custom components** | Hold `Ctrl` and drag a port to another position on the outline — or use *Move ports* on the component's right-click menu | Custom components can have a lot of ports; move them to make a readable symbol |
| **Resize a custom component** | Hold `Ctrl` and drag a corner, use *Resize symbol* on its right-click menu, or set `Width Scale` / `Height Scale` in Properties | Auto-sizing keeps port legends from overlapping; override it when you want a particular shape |
| **Change anything about a component** | Select it and use the **Properties** tab | Labels, bus widths, number of gate inputs, MUX input order, optional adder/counter ports, memory contents, Verilog source |
| **Align / distribute** | `Ctrl-Shift-A` / `Ctrl-Shift-D` | Same-type components in the selection are aligned or evenly spaced; other types are left alone |
| **Rotate or scale a block** | Drag a selection rectangle, then use the handles on the selection box | `Shift`-click adds or removes one component from the selection |

## Wires and connections

| Feature | How | Explanation |
| :---- | :---- | :---- |
| **Auto-routing with fixed segments** | Automatic | Issie will **nearly always** route and separate all schematic connections neatly with no manual routing required. Any wire segment can be manually dragged to a desired position and "fixed", with other segments and wires auto-routed around it |
| **Unfix a wire** | Right-click the wire → *Unfix Wire* | Returns a hand-routed wire to auto-routing |
| **Re-separate / re-route** | Edit menu, or *Reroute all wires* on the canvas right-click menu | Applies the whole-sheet tidy-up to the selection, or to everything |
| **Wire type** | `View` → `Wire Type` | `Jump`, `Radiussed` or `Modern` wires. Purely a display choice — it does not change the design |
| **Wire arrows** | `View` → `Toggle Wire Arrows` | Show the direction of signals at wire end-points |
| **Net labels** | Catalogue → `Input / Output` → `Net Label` | Every net label with the same name is one net, joined without wires. Use for long connections and high fan-out. Exactly one label in a same-name set must be driven |
| **Terminate an unused output** | `Not Connected` component, or a `Viewer` | Issie will otherwise report the dangling output — and offer to insert the `Not Connected` for you |

## Sheets, hierarchy and the project

| Feature | How | Explanation |
| :---- | :---- | :---- |
| **Custom components** | Catalogue → `This project` | Any design sheet can be placed in another sheet, any number of times |
| **Design hierarchy tree** | The **Sheet** menu | The whole project drawn as a tree with connector lines, showing which sheet contains which. The same tree appears in the waveform simulator |
| **Sheet right-click menu** | Right-click a sheet in that tree | *Rename*, *Duplicate*, *Delete*, *Set as top*, *Save as library component*, *Write design as Verilog* |
| **Add a description to a sheet** | De-select everything, open **Properties**, click `Add Description` | The description appears against the sheet as an &#9432; button, and is shown in Properties wherever the sheet is used as a custom component |
| **Sheet parameters** | **Properties** with nothing selected → `Add Parameter` | Named integer parameters, used in arithmetic expressions for bus widths, constants and memory sizes. Each instance of the sheet supplies its own values. See [Parameter System](parameterSystem.html) |
| **Component libraries** | Catalogue → `Library` | Ready-made parameterised components. Choosing one copies its sheet into your project and asks for its parameter values, so it stays an ordinary editable sheet |
| **Import a sheet** | `Sheet` → `Import Sheet` | Copy a sheet in from another project |
| **Project browser** | `Project` → `New project` / `Open project` | An in-app file browser: the projects inside a folder are listed and can be opened with the arrow keys and `Enter` |
| **Automatic backups** | Automatic | Every sheet is continuously snapshotted into a `backup/` subdirectory of the project |

## Simulation, appearance and help

| Feature | How | Explanation |
| :---- | :---- | :---- |
| **Step simulation** | `Simulations` → `Step Simulation` | Set inputs and read outputs immediately. `Viewer` components expose signals from any subsheet |
| **Truth table for combinational logic** | `Simulations` → `Truth Tables` | For a full sheet, or for just the components you select. Reduce it with input constraints, hidden columns, redundancy removal, or algebraic inputs |
| **Waveform simulation** | `Simulations` → `Wave Simulation` | See [Features](features.html) for what it can do |
| **Add waveforms from the schematic** | Right-click a component while a wave simulation is running → *Add waveforms to viewer* | |
| **Read a value off the schematic** | Rest the mouse on a wire while either simulator is running | The value that wire carries appears beside the pointer — at the waveform cursor's cycle, or at the step simulator's current clock tick — in that simulator's radix. Nothing is shown for a wire on a sheet the simulation holds more than one copy of, since there would be no single answer |
| **Themes** | `View` → `Theme` | `Grayscale`, `Light` or `Colourful` |
| **Grid** | `View` → `Toggle grid` | |
| **Right-click context menus** | Right-click a component, a custom component, a wire, the canvas, a sheet in the tree, or the project path | Each offers exactly the actions that apply there, labelled with their shortcuts |
| **Keyboard shortcuts** | **Info** → *Keyboard Shortcuts* | Generated from the same table the app dispatches keys with, for your platform, so it is always correct |
| **Errors** | Automatic | Every error names what is wrong and how to correct it, highlights the components and connections responsible on the canvas, and where the fix is unambiguous offers a button that applies it and restarts the simulation |

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

The canvas can be zoomed in or out, or auto-zoomed and panned to fit the whole circuit using
`Ctrl-W` (macOS: `Cmd-Alt-0`).

### Symbol rotation

Individual symbols can be rotated or flipped using the right-click menu or the Issie edit menu.

### Wire display types

Wires can be displayed as radial, modern, or old-style jump. Switching display types can be done at any time and does not change the schematic.

Radial wires are the most interesting (and readable) form of wire display. Wire bends have small quadrant connections thus distinguishing between wires that cross and a wire joining two perpendicular wires. Visually, radial display makes connectivity easier to follow. The default radius for the wire quadrants is defined as `static member radius = 5.0` in `Wire` type. However, for very small wires, this radius is changed to prevent visual bugs. When drawing radii, the length of the smallest segment the curve connects to is checked, and if its length is <5, the radius is shrunk to match it. Due to the limitations of drawing Arcs in SVG, these radii can only be integer valued, leading to small inconsistencies when a segment is a small non integer value (i.e. 1.5).


### Symbol auto-sizing

Custom components (symbols) are dynamically resized depending on their port configuration. The minimum distance between 2 ports is set as `GridSize = 30`, which is defined as a `[<Literal>]` at the top of `Symbol.fs`. The dimensions of a component are determined as follows:

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



