---
title: Schematic Editor Features
description: A reference for every schematic editor operation in ISSIE: placing components, wiring, sheets and hierarchy, simulation, and the keys for each.
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
| **Component tooltips** | Hover any Catalogue entry, or any field label in **Properties** | Every component explains what it is for, and every field explains what it sets, and gives you immediate explanatory error feedback|
| **Find a component** | Type in the Catalogue's search box | Matches the explanations as well as the names, so "subtract" finds the N bits XOR. Sections holding a match open themselves |
| **Size-to-fit** | `Ctrl-0` | Most often used keyboard shortcut: zooms and centres the schematic so it all fits on screen |
| **Zoom the canvas** | `Ctrl`+`+` / `Ctrl`+`-`; `Ctrl`+mouse wheel | `Ctrl` with `+` `-` `0` zooms whatever you are looking at — the schematic, or the waveforms in the wave simulator. Add `Alt` to zoom the whole application instead |
| **Pan the canvas** | `Space`+drag or `Shift`+drag (or two-finger scroll) | |
| **Rotate and flip components** | `Ctrl`+`Right`: rotate clockwise <br> `Ctrl`+`Left`: rotate anti-clockwise <br> `Ctrl`+`Up`: flip vertically <br> `Ctrl`+`Down`: flip horizontally | Also on the Edit menu and on a component's right-click menu |
| **Undo / redo** | `Ctrl-Z` / `Ctrl-Y`, or the on-screen buttons | |
| **Copy / paste** | `Ctrl-C` / `Ctrl-V`, or the on-screen buttons | Duplicates keep the label with an incremented number. Works across sheets |
| **Paste as array** | on-screen button or `Edit` menu | Quickly create horizontal or vertical copies of any selected circuitry |
| **Move a component's label** | Drag the label; `Ctrl-Shift-Right` rotates it | |
| **Change port location on custom components** | Use *Move ports* on the component's right-click menu and drag ports to rearrange | Custom components can have a lot of ports; move them to make a readable symbol |
| **Resize a custom component** | Use *Resize symbol* on its right-click menu and drag a corner, or set `Width Scale` / `Height Scale` in Properties | Auto-sizing keeps port legends from overlapping; override it when you want a particular shape |
| **Change anything about a component** | Select it and use the **Properties** tab | Labels, bus widths, number of gate inputs, MUX input order, optional adder/counter ports, memory contents, Verilog source |
| **Align / distribute** | `Ctrl-Shift-A` / `Ctrl-Shift-D`; rotate a label with `Ctrl-Shift-R` | Same-type components in the selection are aligned or evenly spaced; other types are left alone |
| **Rotate or scale a block** | Drag a selection rectangle, then use the handles on the selection box | `Shift`-click adds or removes one component from the selection |
| **Snapping** | Automatic while dragging | A symbol sticks to the edges of other symbols, and to the positions that make its wires straight |

## Wires and connections

| Feature | How | Explanation |
| :---- | :---- | :---- |
| **Auto-routing with fixed segments** | Automatic | Issie will **nearly always** route and separate all schematic connections neatly with no manual routing required. Any wire segment can be manually dragged to a desired position and "fixed", with other segments and wires auto-routed around it |
| **Unfix a wire** | Right-click the wire → *Unfix Wire* | Returns a hand-routed wire to auto-routing |
| **Separate manual routing / redraw all** | Edit menu | Applies the auto-routing and separation to everything, or keeps manual routing
and spreads it out evenly |
| **Wire type** | `View` → `Wire Type` | `Jump`, `Radiussed` or `Modern` wires. Purely a display choice — choose what you prefer |
| **Wire arrows** | `View` → `Toggle Wire Arrows` | Show the direction of signals at wire end-points |
| **Net labels** | Catalogue → `Input / Output` → `Net Label` | Every net label with the same name is one net, joined without wires. Use for long connections and high fan-out. Exactly one label in a same-name set must be driven - all other labels follow it |
| **Terminate an unused output** | `Not Connected` component, or a `Viewer` | Issie will otherwise report the dangling output — and offer to insert the `Not Connected` for you |

## Sheets, hierarchy and the project

| Feature | How | Explanation |
| :---- | :---- | :---- |
| **Custom components** | Catalogue → `This project` | Any design sheet can be placed in another sheet, any number of times |
| **Design hierarchy tree** | The **Sheet** menu | The whole project drawn as a tree with connector lines, showing which sheet contains which. The same tree appears in the waveform simulator |
| **Sheet right-click menu** | Right-click a sheet in that tree | *Rename*, *Duplicate*, *Delete*, *Save as library component*, *Write design as Verilog* — and *Set as top* only where two designs share a sheet and disagree about its parameter values, which is the one case where the choice settles anything |
| **Add a description to a sheet** | De-select everything, open **Properties**, click `Add Description` | The description appears against the sheet as an &#9432; button, and is shown in Properties wherever the sheet is used as a custom component |
| **Sheet parameters** | **Properties** with nothing selected → `Add Parameter` | Named integer parameters, used in arithmetic expressions for bus widths and constants. Each instance of the sheet supplies its own values. Memory address and word widths are not yet parameterisable. See [Parameter System](parameterSystem.html) |
| **Component libraries** | Catalogue → `Library` | Ready-made parameterised components. Choosing one copies its sheet into your project and asks for its parameter values, so it stays an ordinary editable sheet |
| **Array components** | Catalogue → `Array components` | *Advanced feature*. Write a design sheet that is copied as an array component. Special components create carry chains, multiplexed or concatenated outputs, etc. The equivalent of a Verilog for loop. |
| **Export a library** | Right-click a library in Catalogue → `Library` | Copies it to a folder you choose, as a subdirectory named after the library — created if it is not there, brought up to date if it is. What lands is the library: a component you have since deleted goes from the copy too |
| **Create or use a library** | `Open project`, then the library's folder | A folder of `.ldgm` components opens as a project, each component a sheet — a multi-sheet component brings all of its sheets — and saving writes back into the library in place. Keep the library you are working on in a folder of your own: the shipped ones, and the store your saved and imported libraries land in, are not edited where they are |
| **Import a sheet** | `Sheet` → `Import Sheet` | Copy a sheet, or set of sheets, in from another project |
| **Project browser** | `Project` → `New project` / `Open project` | An in-app file browser: the projects inside a folder are listed and can be opened with the arrow keys and `Enter` |
| **Automatic backups** | Automatic | Every sheet is continuously snapshotted into a `backup/` subdirectory of the project |

## Simulation, appearance and help

| Feature | How | Explanation |
| :---- | :---- | :---- |
| **Step simulation** | `Simulations` → `Step Simulation` | Set inputs and read outputs immediately. `Viewer` components expose signals from any subsheet |
| **Truth table for combinational logic** | `Simulations` → `Truth Table` | For a full sheet, or for just the components you select. Reduce it with input constraints, hidden columns, redundancy removal, or algebraic inputs |
| **Waveform simulation** | `Simulations` → `Wave Simulation` | See [Features](features.html) for what it can do |
| **Add waveforms from the schematic** | Right-click a component while a wave simulation is running → *Add waveforms to viewer* | |
| **Read a value off the schematic** | Rest the mouse on a wire while either simulator is running | The value that wire carries appears beside the pointer — at the waveform cursor's cycle, or at the step simulator's current clock tick — in that simulator's radix. Nothing is shown for a wire on a sheet the simulation holds more than one copy of, since there would be no single answer |
| **Themes** | `View` → `Theme` | `Grayscale`, `Light` or `Colourful` |
| **Grid** | `View` → `Toggle grid` | |
| **Right-click context menus** | Right-click a component, a custom component, a wire, the canvas, a sheet in the tree, or the project path | Each offers exactly the actions that apply there, labelled with their shortcuts |
| **Keyboard shortcuts** | **Info** → *Keyboard Shortcuts* | Generated from the same table the app dispatches keys with, for your platform, so it is always correct |
| **Errors** | Automatic | Every error names what is wrong and how to correct it, highlights the components and connections responsible on the canvas, and where the fix is unambiguous offers a button that applies it and restarts the simulation |


## Why the wire display types look different

Wires can be drawn as **radial**, **modern** or old-style **jump**; switching between them at any
time changes nothing about the design. Radial is usually the most readable: a bend is drawn as a
small quadrant, which distinguishes a wire *crossing* another from a wire *joining* it, so
connectivity can be followed by eye. A bend is squared off only where the straight run beside it is
too short to fit the radius.

How wires are routed and then separated across the sheet — and why a segment has a signed length
and no position — is described in
[How a wire gets its shape](dev/wireRouting.html).

## Why symbols are the size they are

A custom component is sized from its ports: its height comes from whichever of the left and right
edges carries more ports, and its width from the longest port labels on the top and bottom edges
together with the component's own label. Ports on one edge are always equally spaced. This is what
keeps port legends from overlapping as you move ports between edges. Use *Resize symbol* on the
right-click menu, or the `Width Scale` and `Height Scale` fields in Properties, to override it.
