/// The words Issie says to the user, where they are long enough to be worth reading as prose.
///
/// WHAT IS HERE. The Info window's four written tabs, the waveform simulator's five help panels,
/// the memory help, the twenty field explanations in the Properties pane, and the confirmation
/// popups whose body is several sentences rather than one - thirty-five messages, about 25,000
/// characters. They were spread through the view functions that show them, written as React
/// element trees with the words threaded between `str` and `bSpan` and `li` - fine to render and
/// very hard to read, which is a problem for text whose only job is to read well. Written as
/// markdown here, the whole of Issie's long-form help can be read end to end and reviewed as
/// writing.
///
/// WHAT IS NOT HERE, and should not be moved in.
///
/// - Short labels, button captions and headings. A caption is easier to judge beside the button it
///   sits on than in a list of thirty other strings.
/// - The Catalogue's component tooltips. They are short, they are one per component, and each sits
///   beside the component type it describes, which is what makes them easy to keep true.
/// - One-line confirmation bodies - "The current sheet has unsaved changes." A sentence that short
///   belongs with the buttons it is explaining.
/// - `failwithf` and `Log` messages. They are addressed to whoever is maintaining Issie, not to
///   the user, and belong at the point that fails.
/// - Anything assembled from a design: the components in a combinational loop, the ports of a
///   mismatched instance. The sentence around such a list can live here; the list cannot.
/// - The Keyboard Shortcuts tab, which is generated from the shortcut table so that it cannot
///   drift from the keys that actually fire.
///
/// THE RISK THIS RUNS. Text kept away from the code that shows it goes stale - Issie has had that
/// exact bug, a help menu item whose name had drifted from the panel it opened, so that choosing
/// it said "Feature not explained". What limits it is that only the words moved: which message to
/// show, and when, stays at the point that shows it, so a message cannot be reached by a route
/// this module knows nothing about. Every entry also carries a comment saying what it is and where
/// it appears, which is what makes an entry that no longer matches its use site noticeable while
/// reading.
///
/// That is weaker than a check. `MarkdownTests` reads every message here and fails on one that
/// will not render, but nothing yet fails on an entry that has quietly stopped being used, or on a
/// comment that has stopped being true. Both would be worth adding - the first is a source scan
/// for each entry's name, of the kind `SourceHygiene` already does for printf.
///
/// Markdown is rendered by `Markdown.render` - see that module for the subset supported. Tooltips
/// are the exception: they are drawn by CSS from a `data-tooltip` attribute, which can hold text
/// and nothing else, so tooltip entries are plain sentences with no markup.
module AppMessages

//---------------------------------------------------------------------------------------------//
//------------------------------------------INFO WINDOW-----------------------------------------//
//---------------------------------------------------------------------------------------------//

/// The tabs of the Info window, which opens from the Info button and from "New to Issie? Start
/// here" on the startup menu. Its Keyboard Shortcuts tab is not here: it is generated.
module Info =

    /// Tab 1, and what the window opens on. The first five minutes in order, then what happens
    /// when something is wrong, then the two facts about Issie that cannot be worked out by
    /// looking at the screen. It says what to DO before it says what Issie is: somebody who has
    /// just launched an application wants to get somewhere.
    let gettingStarted = """
# The first few minutes

1. Press **Open demo project** and pick one. Five worked designs ship with Issie, from a full
   adder to a CPU running a program. They reset every time you open them, so nothing you do to
   them can be broken. Or press **New project** and start your own.
2. Place components from the **Catalogue** on the right: drag one onto the sheet, or click it and
   click where you want it. Hover any of them to read what it does, or type in the search box to
   find one by what it is for.
3. Wire them up by dragging from one port to another. Issie routes and tidies the wires itself;
   drag a wire segment if you want it somewhere else.
4. Select a component and open **Properties** to set its width, its label and its options. Every
   field there explains itself when you hover its name.
5. Press **Simulations** → **Step Simulation** to set inputs and read outputs, or **Wave
   Simulation** to watch a clocked design over time.

# If something is wrong, Issie will say so

You do not have to find mistakes yourself. Every error names what is wrong and how to correct it,
highlights the components and wires responsible on the schematic, and where the correction is
unambiguous offers a button that makes it for you.

# Two things worth knowing early

- Designs are hierarchical. Any sheet can be used inside another as a 'custom component', any
  number of times - they are in the 'This project' section of the Catalogue. The Sheet menu draws
  the whole hierarchy as a tree.
- Every clocked component (drawn with a blue fill) uses the same clock, Clk. You never wire a
  clock up: all Clk ports are connected together automatically. In the waveform viewer the
  vertical lines are the active clock edges, one per cycle.

# Where to read more

- [User Tutorial](userGuide.html) — one page you can follow at the keyboard, building a design
  from an AND gate up to a clocked circuit with a waveform simulation
- [Features](features.html) — what Issie can do, in one page
- [Schematic Editor Features](coolFeatures.html) — every editing operation and the key that does it
- [Parameter System](parameterSystem.html) — building one sheet that works at any bus width
- [Verilog Components](verilogComp.html) — writing a component's logic in SystemVerilog instead of
  drawing it

The other tabs of this window are worth a minute of your time: **Tips & Features** lists the things
people most often do not find, and **Keyboard Shortcuts** is generated from the keys this build
actually uses, for this platform.
"""

    /// Tab 2. The things people most often do not find. A table so that the name of each is
    /// scannable down the left without reading the explanations.
    let tips = """
| | |
|---|---|
| Right-Click Menus | Explore the Right-Click Menus to find context-dependent operations |
| Search the Catalogue | The Catalogue's search box matches what a component is for as well as its name, so 'subtract' finds the N bits XOR and 'invert' finds Not |
| Hover a Properties field | Every field in the Properties tab explains what it sets, and what people get wrong about it, when you hover its name |
| Drag from the Catalogue | Drag a component out of the Catalogue and drop it where you want it, rather than clicking twice |
| Probe a wire | With either simulator running, rest the mouse on any wire of the schematic to read the value it carries - at the waveform cursor's cycle, or at the step simulator's current clock tick |
| Waveforms and the schematic | Hover a waveform's name to light up its component and wires on the schematic; the button beside the name opens the sheet it lives on. Right-click a component on the schematic to add its waveforms to the viewer |
| Zoom keys | Ctrl with + or - zooms whatever you are looking at - the schematic, or the waveforms. Ctrl-0 fits the whole sheet on the screen, which is the one to reach for most. Add Alt to any of the three to zoom the application itself instead |
| Sheet descriptions | Add short descriptions to your design sheets |
| Copy, Paste | Use copy and one or more Pastes (keys or on-screen buttons) to make duplicate components with the same name and increasing numbers. Copy multiple items onto the same sheet or a new sheet |
| Undo, Redo | From onscreen buttons or keys - use them, they work well! |
| Ctrl-drag | Ctrl-drag ports on custom components to a new position on any side. Change the component height, width in properties if it is the wrong size. |
| 2-MUX properties | Swap 0/1 inputs in properties if this makes a neater diagram |
| Counters, Adders | Hide inputs/outputs you do not need from properties |
| Set Default input values | Set the input values you want in the step simulator and 'click set default inputs', or set individually in input properties. This will remember the values for both step simulator and waveform viewer |
| Use properties | Use properties to change labels, bus widths, etc of all components. |
| Use radix for constant values | Enter constant values for constants and bus comparators in the radix which makes most sense - they will dispaly as you have entered it. |
| Position labels, rotate and flip components | Drag or rotate (key) labels, reposition, rotate or flip components, drag wires, as needed to get a neat schematic. You can select and reposition multiple components |
"""

    /// Tab 3. What to send when reporting a bug, in the order it is useful to collect it.
    let bugReport = """
If you think Issie is not working it is very helpful if you can give us details: we usually answer
and fix bugs, if they exist, very quickly. Before you contact us, look at the list below and answer
as much as possible to make your Bug Report (sometimes it is not all possible, send what you can).

1. Which version of Issie (Info tab, About Issie)
2. Which platform (Windows, Macos)
3. What did you do that led to unexpected behaviour?
4. What result did you expect?
5. What result did you get?
6. What project files caused this, the top-level sheet? Enclose project as zipped file deleting the
   maybe large backup directory when you zip.
7. If you can reproduce the bug yourself, try opening dev tools (Ctrl-Shift-I). You can do this
   after the bug happens. 2/3 of problems result in error messages displayed there. Screenshot the
   error and its backtrace and send it.
8. What precise actions (if you know them) led to the bug after loading this project
"""

    /// Tab 4. Version, who wrote Issie, and what it is written in. Fourth because nobody opened
    /// this window for it. Takes the version because that is a fact about the build, not text.
    let about (version: string) = $"""
# Version

{version}

# Acknowledgments

ISSIE was created in 2020 by Marco Selvatici (EIE 3rd year) as his BEng final year project. The
original waveform viewer was created by Edoardo Santi (EEE 3rd year) during Summer UROP work. The
new F# schematic editor was written as 2021 coursework by HLP students in EEE, and particularly
Team 4. The new editor was integrated by Jo Merrick (EIE 3rd year) for her BEng final year project.
In Spring 2022 the HLP class implemented a draw block with component rotation and much better
routing. In Summer 2022 Jason Zheng rewrote the waveform simulator, Aditya Despande wrote the truth
table generator, and Archontis Pantelopoulos spent all Summer on a UROP writing the Verilog entry
block and making many improvements. In 2023 HLP students implemented intelligent routing, Yujie
Wang made the simulator faster, and Petra Ratkai implemented a much better Verilog compiler. in
2025 the HLP class innovated the first parameter system. In 2026 Samuel Wang updated the build
system with autogenerated binaries. Beth Cham improved the Verilog compiler.

# Technology

ISSIE is written in [F#](https://fsharp.org/) compiled to Javascript by
[FABLE](https://fable.io/) and running under the [Electron](https://www.electronjs.org/) framework
"""

//---------------------------------------------------------------------------------------------//
//---------------------------------WAVEFORM SIMULATOR HELP--------------------------------------//
//---------------------------------------------------------------------------------------------//

//---------------------------------------------------------------------------------------------//
//------------------------------------CONFIRMATION POPUPS---------------------------------------//
//---------------------------------------------------------------------------------------------//

/// The bodies of popups that stop and explain before doing something. Only the ones that are
/// several sentences of explanation are here. A one-line body - "The current sheet has unsaved
/// changes." - stays beside the buttons it belongs to, where it is easier to judge than in a list.
///
/// The buttons, and what they do, stay in the code: what moves is the paragraph that has to
/// persuade the user which button to press.
module Confirm =

    /// Before adding a parameter to a sheet for the first time. The first parameter is the moment
    /// to explain what the feature is for, because until then nothing in the pane hints at it -
    /// and it is a feature a design can perfectly well never use.
    let usingParameters = """
A named property is a value a sheet is built around - a width, a count - so that one sheet can
serve a family of designs. Defining one needs a name, a description, and a default value.

Properties have integer values set separately in each instance of this sheet, and take the default
value when there are no instances. So the same sheet can appear at several sizes in one design.

This is an advanced feature: designs that do not need it are unaffected by it.
"""

    /// Before duplicating a sheet. Duplication is usually the wrong tool: what people want is
    /// several instances of one sheet, which is what a custom component already is.
    let duplicateSheet = """
Duplicating a sheet is only necessary if you intend to implement similar but different versions of
the sheet. If you want copies of sheet hardware you can add the sheet multiple times as a component
from this Project in the Catalog.
"""

    /// Opening a folder that holds sheets but no `.dprj` marker. Issie can open it either way, so
    /// this offers to put the marker back rather than refusing the folder or writing to it
    /// uninvited.
    let missingProjectFile (folder: string) = $"""
'{folder}' holds Issie sheets but no .dprj project file, which is what marks a folder as an Issie
project.

Issie can open it either way. Adding {folder}.dprj lets it be recognised as a project in future.
"""

//---------------------------------------------------------------------------------------------//
//---------------------------------PROPERTIES PANE FIELDS---------------------------------------//
//---------------------------------------------------------------------------------------------//

/// What each field in the Properties pane means, shown when its label is hovered.
///
/// PLAIN TEXT, not markdown. These are drawn by CSS from a `data-tooltip` attribute, which holds
/// characters and nothing else - there is nowhere for a `<b>` to go. Keep each to what the field
/// does and, where there is one, the thing people get wrong.
///
/// Keyed by the label the field displays, which is what makes a field acquire its explanation by
/// being labelled: no call site passes anything, the same label reads the same way wherever it
/// appears, and a label with no entry here renders with no tooltip rather than the wrong one.
/// A label that is reworded loses its explanation, which is the safe direction to fail in.
module Fields =

    /// Explanations, keyed by the exact label text the field shows.
    ///
    /// Labels are written once, in the module that builds the field, and matched here verbatim: a
    /// label that is reworded loses its tooltip rather than showing the wrong one, which is the safe
    /// direction for a mismatch to fail in.
    let tips: Map<string, string> =
        Map [
            // ---- identity ----
            "Name",
                "The label drawn on the symbol and used for this component everywhere else: in error \
                 messages, in the waveform viewer, and in generated Verilog. It must be unique on this \
                 sheet."
            "Instance name",
                "This copy's own label. The sheet it is an instance of keeps its own name - renaming \
                 here renames only this copy."

            // ---- widths ----
            "Width (bits)",
                "How many bits wide this component's bus is. Widths must agree at both ends of every \
                 wire, so changing this here is usually the fix for a 'wrong wire width' error. It can \
                 be an expression in the sheet's parameters, such as WIDTH or WIDTH+1."
            "Output width (bits)",
                "How many bits the output bus has. The single input bit is copied onto every one of \
                 them."
            "Top (LSB) output width (bits)",
                "How many of the input's bits go to the top output. The rest go to the bottom one, so \
                 the two together always add up to the input width. Flip the component vertically \
                 (Ctrl+Down) if you want the least significant bits at the bottom instead."
            "Width",
                "How many bits this output takes from the input bus."
            "LSB",
                "The bit of the input bus this output starts at, counting from 0 at the least \
                 significant end."
            "Least Significant Bit number selected: lsb",
                "The bit this selection starts at, counting from 0 at the least significant end. With \
                 a width of 4 and an LSB of 8 the output is bits 11 down to 8 of the input."
            "Compare with",
                "The output is 1 when the input bus equals this value and 0 otherwise. Write it in \
                 decimal, or with an 0x or 0b prefix for hex or binary."
            "Default value if input is undriven",
                "The value this input takes in simulation when nothing drives it - which is the case \
                 for the top sheet's own inputs in the waveform viewer. Both simulators use it, so it \
                 is the place to set the input values a waveform simulation should start from."

            // ---- shape ----
            "Number of inputs",
                "How many input ports this component has. Reducing it deletes the wires on the ports \
                 that go away, so this is a change to the schematic and cannot be set by a parameter."
            "Number of outputs",
                "How many output ports this component has. Each gets its own width and starting bit \
                 below. Reducing it deletes the wires on the ports that go away."
            "Optional Ports",
                "Ports you do not need can be removed rather than tied off. An unticked Cin behaves as \
                 0; an unticked Cout simply is not there, so nothing has to be connected to it."
            "Optional Inputs",
                "Ports you do not need can be removed rather than tied off. Without Load the counter \
                 only counts; without Enable it counts every clock cycle."
            "Ports",
                "The inputs and outputs this instance has, with their widths, taken from the sheet it \
                 is an instance of. Hold Ctrl and drag a port to move it to another edge of the symbol."

            // ---- appearance ----
            "Width Scale",
                "Stretches the symbol horizontally. Issie sizes a custom component to fit its port \
                 labels; set this only when you want a particular shape."
            "Height Scale",
                "Stretches the symbol vertically. Issie sizes a custom component to fit its ports; set \
                 this only when you want a particular shape."

            // ---- values ----
            "Enter constant value in decimal, hex, or binary:",
                "The value this component drives, written however is clearest: 42, 0x2a or 0b101010. \
                 It is redisplayed in the form you typed it."
            "Enter bus compare value in decimal, hex, or binary:",
                "The output is 1 when the input bus equals this value and 0 otherwise. Write it \
                 however is clearest: 42, 0x2a or 0b101010."

            // ---- the sheet itself ----
            "Sheet Description",
                "A sentence about what this sheet does. It is shown against the sheet in the Sheet \
                 menu, and in this pane wherever the sheet is used as a custom component - so it is \
                 read by whoever uses your sheet, not only by you."
        ]

/// Why the Truth Table tab will not make a table, shown by its "Why is there no table?" button.
///
/// Plain sentences with no markup, like the field explanations above: these go to
/// `Notifications.errorPropsNotification`, which draws its text into a Bulma notification and
/// holds characters and nothing else.
///
/// Neither message names the components at fault, which is the rule this module states: a list
/// assembled from a design cannot live here. They are highlighted on the schematic instead, which
/// says the same thing better - the reader looks at the circuit rather than matching labels - and
/// it is what lets these two stay one sentence each. The two conditions are tested in order, so
/// each message may assume the one before it has already been ruled out.
module TruthTable =

    /// First: a table of a circuit with state has no meaning, whatever its widths.
    let notCombinational =
        "A truth table lists an output for every combination of the inputs, so it can only be made \
         for combinational logic. The clocked components are highlighted on the schematic. To \
         tabulate the combinational part, select the components you want and use 'Truth Table for \
         selected logic'; to see the whole sheet working over time, use the Wave Simulation tab."

    /// Second, once the logic is combinational: it must also be narrow enough to read. The limit
    /// is passed in rather than written here, so that it cannot drift from the one the simulator
    /// enforces - TruthTableTypes.Constants.maxTruthTableBusWidth.
    let busTooWide (maxWidth: int) =
        sprintf
            "A truth table lists an output for every combination of the inputs, so it is made only \
             for logic up to %d bits wide. The components carrying wider buses are highlighted on \
             the schematic. To tabulate a narrower part, select the components you want and use \
             'Truth Table for selected logic'; to see a wide design working, use the Wave \
             Simulation tab."
            maxWidth

/// The collapsible "Expression syntax" note under the Properties pane, shown only on a sheet that
/// declares properties. Every numeric box in the pane accepts an expression, but until a sheet has
/// a property there is nothing to write in one except the number the box already holds, so the
/// note would be advertising a feature with no use. See ParameterView.expressionSyntaxHelp.
module Expressions =

    let title = "Expression syntax"

    /// Kept to what can be typed. Why one would - a sheet at several sizes in one design - is
    /// explained where properties are declared, and repeating it here would bury the grammar,
    /// which is the one thing that cannot be guessed.
    let syntax = """
Any numeric box in this pane takes an expression as well as a number. It is worked out whenever a
property changes, so a width written as `w*2` follows `w`.

**Values**: whole numbers of any size, and the name of any property this sheet declares. A name
starts with a letter and continues with letters and digits.

**Operators**: `+`, `-`, `*`, `/`, `%`, and brackets. Division and remainder are whole-number:
`7/2` is `3`. `*`, `/` and `%` bind tighter than `+` and `-`.

**Functions**: `clog2(n)` is the number of bits needed to count `n` things - the address width of
an `n`-word memory. `min(a,b)` and `max(a,b)` take the smaller and the larger. Their names may be
written in any case, and none of them can be used as a property name.

A value that will not parse, or that breaks the box's own limits, is shown in red and left where it
is: nothing reaches the design until it makes sense.
"""

/// The Memories info button, beside a RAM or ROM's properties. How initial data works, which is
/// the thing people get wrong about memories.
module Memories =

    let title = "Issie Memories: how RAM and ROM data works"

    let help = """
- RAMs and ROMs need to have initial data contents defined. For RAMs the fixed initial data is
  reset for clock cycle 0 whenever a simulation is started, the RAM data can change during
  simulation.
- The default initial data is all 0s. Initial data is stored with the design sheet and may be
  viewed or modified with the memory editor from properties. The editor can change locations
  numbered higher than 15 by entering a number in the 'first location displayed' box.
- During the Step or Waveform Viewer simulation RAM data can be viewed, but not manually changed.
  RAM data may change as the result of writes. These changes don't affect the initial data.
- When using external tools like an assembler it is useful to enter RAM or ROM initial data from a
  text file. Memory data can be written to a file with extension '.ram'. If a '.ram' file is placed
  in the project directory a RAM or ROM component can be linked to the file, or unlinked, by
  selecting it from the properties page.
- Linked memories will have initial data updated to latest file contents, if they change. Update is
  automatic when a new simulation is started and otherwise will happen if needed when the Issie
  screen refreshes.
"""

/// The waveform simulator's help panels. Three of these names come from the "WaveSimHelp"
/// right-click menu in ContextMenus.fs and must stay spelled the same as the items there; the
/// other two come from the viewer's own Info button. The names are matched in
/// UIPopups.viewWaveInfoPopup, which is where that correspondence is enforced.
module WaveHelp =

    /// "Getting Started" - the viewer's Info button opens on this.
    let gettingStarted = """
- The waveform viewer can show waveforms selected from **any sheet** in the design being simulated.
- Choose the top sheet you want to simulate and press **Start**. The top sheet's own inputs and
  outputs are shown straight away. Press the **Select Waves** button to change which waveforms are
  viewed - any signal on any sheet can be shown. See the selection popup info button for more info.
- Use **Ctrl/+** and **Ctrl/-** to show fewer or more clock cycles. The same two keys zoom the
  schematic when the schematic has the keyboard: they zoom whatever you are looking at. Add **Alt**
  to zoom the whole application instead. The **Info** button lists every key for your platform
  under **Keyboard Shortcuts**.
- Drag the **grey horizontal divider bar** to make the waveform viewer wider.
"""

    /// "Viewing Waveforms" - from the viewer's right-click help menu.
    let viewingWaveforms = """
- Hover on a waveform name to see component and connections highlighted in editor.
- Drag waveform names to reorder waveforms; Click the x button to delete a waveform.
- Click on waveforms to change the highlighted clock cycle: see values of signals for this cycle on
  the righthand side.
- Use the right-hand input box to move to a new highlighted cycle number, or the arrows to change
  current cycle.
- Drag the scrollbar to scroll. When the thumb is at the righthand side drag it more to extend the
  simulated cycles.
- Use the lefthand zoom buttons to zoom out or in. Use the radix buttons to change display radix.
"""

    /// "Waveform and RAM selection" - from the viewer's right-click help menu.
    let selection = """
- The waveform viewer can view signals on **any sheet** in the design being simulated.
- Use 'select waves' window to select which waveforms are viewed. The search box allows them to be
  selected by part of name. Alternatively, expand groups to explore design and find components and
  ports.
- The waveforms you view can be changed whenever the simulation is running. It is good practice to
  delete waveforms you are not using, and order waveforms logically.
- Use 'select RAM' to view RAMs showing contents, read and write location, in the current (cursor)
  cycle.
- Selected waveforms are preserved from one simulation to the next.
"""

    /// "Instructions" - the long one, from the viewer's own Info button.
    ///
    /// The scroll bullet used to draw the zoom-out and zoom-in icons inline, from DiagramStyle.
    /// Markdown holds text, so it names the controls instead - which also survives the icons being
    /// redrawn.
    let instructions = """
- Hover mouse over a waveform name in the viewer to see it highlighted on the current schematic
  sheet.
- Change schematic sheet to view or alter components on subsheets.
- Drag names to reorder waveforms, use delete icon to delete, use **Select Waves** to add or delete.
- Scroll or use **Scrollbar arrows** and the **zoom out and zoom in** controls to show which cycles
  to display.
- Move the coloured **cursor clock cycle** using **a click on the waveforms,** the **cursor box
  number,** **box controls,** or the **Left/Right arrow keys** once you have clicked anywhere in
  this pane. They step the cursor until you click the schematic or type in a box.
- The column to the right of the waveforms shows signal values in the cursor cycle
- Drag the **grey vertical divider** to alter the screen space used by waveforms
- Waveforms will scroll vertically if you select more than will fit on the screen.
- Use **Select RAM** to view RAM contents for the current cycle.
- Use **Bin Hex uDec sDec** buttons to change the display radix.
- **Ctrl/+** and **Ctrl/-** show fewer or more clock cycles. Add **Alt** to zoom the whole
  application instead.
"""

    /// "Miscellaneous" - from the viewer's right-click help menu.
    let miscellaneous = """
- During a simulation you can move to any sheet and view or edit the design. When any part of the
  design, or linked memory contents files, changes the green update button will be enabled allowing
  update to the newer design.
- You can change default values for sheet inputs in Input component property boxes. The top sheet
  inputs of the simulation are given these values throughout the simulation. Adjustable values
  anywhere else in the design can be implemented using constants.
- The waveform radix can be changed. When waveforms are too small to fit binary this will be
  changed to hex. Numeric values not displayed on the waveform can be viewed using the cursor and
  the righthand panel.
"""

    /// Shown when a help panel is asked for by a name nothing here answers to - which can only
    /// happen if the menu in ContextMenus.fs and the names matched in UIPopups have drifted apart.
    /// Says what to do next rather than leaving the user in the help system with nothing.
    let noHelpFor (feature: string) = $"""
There is no help written for '{feature}' - this is a fault in Issie, and we would like to know about
it (Info -> Bug Reports).

The Info button at the top of the waveform viewer has instructions covering everything it can do.
"""
