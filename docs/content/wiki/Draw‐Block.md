# Introduction

The Issie Draw Block consists of many source files in the `DrawBlock` directory, and divides into:

* `DrawModelType`. Types common to one or more draw block modules. Contains submodules: `SheetT`, `BusWireT`, `SymbolT` with types for the relevant blocks.
* `Symbol`. Block split into many similarly named source files that implements **symbols**
* `BusWire`. Block split into many similarly named source files which implements **wires**
* `Sheet`. Block split into many similarly named modules which implements top-level draw block operations, e.g. mouse and key actions.
* *Miscellaneous files*
   * `BlockHelpers` - general helper functions for drawblock
   * `PopupDrawingView` - hacky way to implement popups in drawblock using a copy of the Issie popup code `PopupView`. Should be cleaned up.
   * `RotateScale` - advanced operatiosn using symbol and buswire that rotate and scale multiple symbols

![VS project explorer drawblock list of source files](https://github.com/tomcl/issie/blob/master/docs/img/drawblock-files-july-2023.png)

All persistent state in the draw block is contained in the Issie Elmish `model` as nested records one for each block. Each `model` has associated with it a set of Messages that can be dispatched to change model state (type `Msg`).




```
model: Model (Issie to level)
    model.Sheet: SheetT.Model (Sheet)
        model.Sheet.Wire : BusWireT.Model (Buswire)
        --> contains field Wires: Map<ConnectionId,Wire>
            model.Sheet.Wire.Symbol : SymbolT.Model (Symbol)
            --> contains field Symbols; Map<ComponentId, Symbol>
```
# Symbol

The symbol block displays Issie circuit components (`Component`) on the DrawBlock schematic as schematic *symbols* (`Symbol`). These are usually displayed as rectangular boxes. In addition Symbol keeps track of the input and output *ports* (`Port`) of each symbol. Every symbol is associated with its corresponding Issie component, however symbols can contain extra information to make operation more performant.

## Key Operations

* Symbol rendering
* Symbol movement
* Port position and orientation calculation

## Features

### Symbol rendering

Every distinct component type must be displayed differently, as determined by its `ComponentType`. A good implementation of the possible variations, capturing regularity, would make it much easier to add new components with different symbols. The current code tries to do this but is a mess. cleaning this up would be worthwhile

### Performance

The view function must be fast in the case that no or few symbols are moved.

### Symbol rotation and flipping

* All symbols except custom components can be rotated (multiples of 90 degrees) and flipped by a Transform.
* Custom Components cannot be rotated by a Transform. However, their ports can be arbitrarily re-ordered and moved. Rotating a custom component rotates the component ports around its edges and is similar to otehr component rotation - except that the movement of ports changes the symbol dimensions.

### Port Orientation

Rotation changes all port orientations. Another way to do this is by allowing ports from top or bottom side of symbols. This could be by symbol. For example a MUX select input should be at the bottom of the symbol, not the LHS as now. Or customisable (with a suitable drag-and-drop UI) on custom component symbols, where the ability to move some ports to any side of the symbol would be good.

### Port Arrangement

On custom components it would be nice to add space between different sets of ports on one side to distinguish different logical groupings of signals.




# BusWire

The BusWire block is the part of the DrawBlock code in Issie which implements a drag and drop schematic editor. Buswire implements the wires (or busses) on the schematic. Electrical connections between components are represented on the schematic by lines: every connection has an associated width (in bits). Connection widths are inferred automatically on correct circuits by function `inferConnectionsWidth` code in `common/widthinferrer.fs`. A bus is simply a wire with inferred width > 1.

* Every wire is defined by an ordered list of connected segments, where each segment is vertical or horizontal.
* Wire segments strictly alternate between Vertical and Horizontal, the `Segment` definition of segments allows segment endpoints position to be worked out relative to the start of the Wire by calculation from the Wire segment lengths. It is *relative*. For many operations we transform segments into an absolute form (`ASegment`) which contains for each segment the start and end coordinates.
* In some cases a *zero-length segment* is used to make two adjacent (visible) segments have the same orientation.
* The structure of segments is variable. All wires have an initial fixed length "nub" segment at either end which provides distance from the symbol body. Some then have  a zero length segment followed by another variable length segment which extends the nub for however long the first visible segment of the wire needs to be. However some wires have a non-zero length segment straight after the nub (they double-back on themselves in the third from end segment).
* Every wire connects between an output port on a symbol and an input port on a symbol, with the source of a wire being the output leading to a destination input
* Every wire is uniquely associated with an instance of the corresponding Issie `Connection` datatype in `Common\CommonTypes.fs` and provides extra data used by Buswire. The vertices of the schematic wire are defined in the corresponding Connection datatype used by the non-DrawBlock part of Issie. This is read and written to design files by Issie.
    * In order to maintain compatibility with Issie files wires can be completely reconstructed from the corresponding connection. Extra information is inferred, or got by coding vertex coordinates as positive or negative. All coordinates on the SVG are positive, so a negative vertex coordinate can code one bit of information about that vertex. Currently negative vertices are used to represent manually routed segments than therefore do not autoroute when symbols are moved.
* Buswire uses the Wire datatype to represent schematic connections and translates to/from the Connection datatype on load or save. Wire is optimised to make DrawBlock operations: rendering a wire as an SVG figure, changing wires when editing, as simple and performant as possible.


## Key Internal Operations

* `singleWireView`. Each wire is (independently of other wires) *rendered* in the view function to make an SVG shape. Wires render as appropriate lines and text representing width annotations. React is used to cache the rendered state of each wire so that when the view function is re-evaluated any wire with identical `WireRenderProps` does not need to get re-rendered. This allows performant animated drag-and-drop of single wires, or small groups of wires.
* `updateWire` (in `BusWireUpdateHelper.fs`). Adjusts the position of a wire after its endpoints (symbol ports) have been moved, with partial autorouting being prioritised over full autorouting.
   * `autoroute` (in `BusWireUpdateHelper.fs`). Recreates an automatically routed wire between two ports on symbols. This is called repeatedly when symbols are dragged to update their wires.
   * `partialAutoRoute` (in `BusWireUpdateHelper.fs`). The same as `autoroute` but deals with the case where a wire has segments manually routed and therefore which should stay fixed when the end segments are moved to keep the wire attached to a moved symbol. If the wire endpoint moves too far this may no longer be possible, causing the wire to change shape, in which case the wire is autorouted and the manual adjustment is lost.
   * `reverseWire` (in `BusWireUpdateHelper.fs`). The wire is directional, with the tail of wire being joined to the input and the head of wire being joined to the output of an electrical component. `reverseWire` allows the wire to be processed in the opposite way.
* `moveSegment` (in `BusWireUpdateHelper.fs`). Implements manual dragging of individual segments on a wire. Segments drag in a direction perpendicular to the segment. An auto-routed segment becomes manually routed when dragged.
* In order for Sheet to implement drag operations on segments, and select single or multiple wires, each segment must have a bounding box used to make it clickable.
* `makeAllJumps` (in `BusWireUpdateHelper.fs`). When wires cross the horizontal wire must be rendered with a *jump*. Jump positions are expensive to calculate and this is done by after any operation that changes wires finished (not in the middle of a drag-and-drop). This can be seen in `ResetJumps` and `MakeJumps` in `BusWireUpdate.fs`, which only recomputes the jump positions after the drag operation is complete. The jump positions are held in the wire data structures and used when rendering the wire to render visible half-circle jumps.

## Key Interfaces

BusWire interfaces with Sheet and Symbol. Sheet collects user operations (from mouse events and UI operations) and drives Buswire to implement them. It also interrogates Buswire for bounding box information where necessary to decide what on the schematic has been clicked. Symbol hold the current positions of symbols, and hence ports (the endpoints of wires). This information must be used when autorouting.

See the Elmish `Update` function for the exact coding of messages that change the Buswire Model.

## Code


#### Wires and Segments

Wire info is stored as a start position and a list of segments (this is typically 7 but can be more). Segments can be zero length in specific cases. Undraggable segments at each end of a wire "nubs" serve the function of ensuring that wires cannot bend 90 degrees too close to the edge of a symbol.


### Wire jumps versus dots versus radiussed wires

Wire jumps are the old-fashioned way to indicate crossing wires. The more modern way is to use *dots* black disks as join points on T junctions, and not allow 4 segments on the same electrical net to join at a single point - so that all wire crossings are guaranteed not to be wires in two distinct nets. Moving from jumps to black circles would mean some new algorithm development but should be possible and actually faster than the existing jumps, both to render, and to update. Circles are probably a visual improvement over jumps, though one might keep both an allow user choice.

One more readable (unconventional) option is to use radiussed wires.

Currently (2023) all 3 methods are implemented within the `makeAllJumps` operation.

### Wire autoroute

Autoroute currently implements snap-to-position, it should implement symbol avoidance, and auto-space from other wires to reduce the need for finicky manual adjustment. This has the same aim is interactive snap-to-position, but would be done when recreating a wire that did not previously exist.

Segments are represented in a compact rotation and translation invariant form where the wire has an initial orientation determined by which edge of a symbol it connects to. The segment list alternates orientations from this along the list, with the length being positive or negative according to the change in the relevant coordinate.

Segments can be `Auto` or `Manual`. `Auto` segments are rerouted whenever a wire end changes position. `Manual` segments are fixed in place. Partial autorouting will match up segments as best it can until a manual segment is found. Dragging a segment manually marks it manual so such positioning does not get changed. Moving a wire end enough to change the wire topology will reset all its segments to `Auto`. This is a very intuitive interface!

### Segment and wire interactive snap-to-position

Currently there is a *snap-to-position* function that makes it easy to align segments and components.

## Interaction with the MVU structure

`BusWireT.model` is nested within `SheetT.model`, look to the Sheet-MVU section for the diagram. This means that the BusWire model needs to interact both with higher levels and lower levels.

### Update 

```fsharp
let Update.update  // main Issie update function
      (msg : ModelType.Msg) // the oldModel (for all issie) is changed based on the message received
      (oldModel : ModelType.Model) =
      // function body not shown

let BusWireUpdate.update 
      (msg : DrawModelType.BusWireT.Msg) // incoming message to be processed on the Sheet level
      (issieModel : ModelType.Model) // current state of the issie model, to be updated 
      : ModelType.Model*Cmd<ModelType.Msg> = 

      let model = issieModel.Sheet.Wire
      // rest of function body not shown
```

Note that the update function acts upon `BusWireT.Msg` messages that can be sent from any other level of the model. It can also send messages to any other level of the model by making use of some adapter functions. These functions are defined in `BusWire.fs`. Each of these functions sends a command of the highest level (i.e. an issie command), the message is wrapped inside the command. On each Msg level there is one message case that indicates a message that needs to be passed to a lower level and this is what allows us to wrap a Symbol message in an issie message.

These are some of the adapter functions that are used in BusWire to handle all the different types of messages that may need to be sent:

```fsharp
    let withNoMsg (model: ModelType.Model) = model, Cmd.none

    /// Command to issie level
    let withIssieMsg (msg: ModelType.Msg) (model: ModelType.Model) = model, Cmd.ofMsg msg
    
    /// Command to Sheet level
    let withSheetMsg (msg: DrawModelType.SheetT.Msg) (model: ModelType.Model) = model, Cmd.ofMsg (ModelType.Msg.Sheet msg)
    
    /// Command to BusWire level
    let withMsg (msg: DrawModelType.BusWireT.Msg) (model: ModelType.Model) = 
        model,Cmd.ofMsg(ModelType.Msg.Sheet(DrawModelType.SheetT.Msg.Wire msg))
    
    /// Command to Symbol level
    let withSymbolMsg (msg: DrawModelType.SymbolT.Msg) (model: ModelType.Model) = 
        model, Cmd.ofMsg (ModelType.Msg.Sheet (DrawModelType.SheetT.Msg.Wire (DrawModelType.BusWireT.Symbol msg)))

    /// Command with multiple messages on BusWire level
    let withMsgs (msgs: Msg list) (model : Model) =
        let wireMsg msg = Cmd.ofMsg (ModelType.Msg.Sheet (DrawModelType.SheetT.Msg.Wire msg))
        model, Cmd.batch (List.map wireMsg msgs)
```

### View

```fsharp

let MainView.displayView
      (model : Model) // issie model and issie message
      (dispatch : Msg -> unit) =
      // function body not shown

let SheetDisplay.view
      (model : Model) // SheetT.Model
      (dispatch : Msg -> unit) // function that dispatches messages of type BusWireT.Msg
            : ReactElement =
       // function body not shown
```

# Sheet

Sheet is the top-level block in the DrawBlock code implementing an interactive schematic editor. It is driven by commands from the rest of Issie, or user mouse and key events in the schematic pane. In turn it drives Symbol and BusWire blocks.

Sheet does a lot of work and is generally well written and complete at the moment.

At a structural level the DrawBlock code is implemented strictly so that Sheet -> BusWire -> Symbol. Because each module has its own Model type, the Symbol code cannot use the BusWire defined Wire type. Arguably this strict division is not helpful. it would be possible, and perhaps nicer, to put all three `Model` types, and all three `Msg` types, together in one single DrawBlock type definition file before any of the DrawBlock code and visible to all of it. That would allow more flexibility in the types of interfaces allowed between blocks, and allow slightly less cumbersome passing of messages.

The current structure works quite well so no need has been seen for a change as yet. That might change.

## Screen coordinates, zoom, and scrolling

1. Symbol and wire use object coordinates on the schematic sheet that are fixed (regardless of zoom) and in the range [0..Sheet.model.CanvasSize]. 
2. `XYPos` is used uniformly as type for float XY coordinates. Int coordsa re converted to float internally in DrawBlock.
2. CanvasSize is set to 3500 by default, and can be extended if needed. In order to add a usable border round circuits, when anything moves too near the current edge of the canvas the entire circuit has coordinates rebased so its bounding box is in the middle of this range (both X and Y). This rebasing is transparent to the user, except it alters how far they can scroll.
3. **Screen** coordinates (pixels) are related to **DrawBlock** coordinates by by panning offset and a zoom factor. Coordinates are stored in Model, and also presented as fields in `mMsg: DrawHelpers.MouseT` for every mouse event. See documentation on this type.
    * Coordinate field names are prepended with `Screen` to indicate that they are pixel based (after zoom). 
    * To get from Drawblock coords to screen coords it is necessary to *multiply* by `Sheet.model.Zoom`.
    * `page: ScreenPage` is related to `mousePos: MousePos` by mousePos = (`Sheet.model.ScreenScroll` + `page`)/ `Sheet.model.Zoom`

## Scrolling the Schematic

The schematic sheet can be scrolled:

1. via HTML and the build-in scrollbars or single-finger scroll on touchscreen
2. via automatic scrolling when mouse is near the edge of the editor window: `Sheet.Msg.CheckAutomaticScrolling` implements this.
3. As part of circuit centre-and-fit in Ctrl-W or (using Ctrl-W) when automatic scrolling near to the edge of the SVG canvas.

## Synchronising HTML scrolling and message-based scrolling

This is currently not well sorted out. When `ScreenScrollPos` changes we want to HTML scroll (`canvasDiv.scrollX`, `canvasDiv.scrollY`) to change. Also, when the HTML scroll coordinates change we want the corresponding model `ScreenScrollPos`  to change. How this is done in a way that works is not very clear ATM, even though it works. TODO: sort this out.

Related info:
    * `UpdateScreenScrollPos` - message that updates `ScreenScrollPos` field and the HTML scroll position
    * `writeCanvasScroll` - function that writes the HTML scroll position
    * `UpdateScrollPosFromCanvas` - message that updsates `Sheet.model.ScreenScrollPos` from the current HTML scroll position. This is called by the `OnScroll` callback.

## Snapping

Symbol and Wire **snapping** to preferred poistions drawing segment or symbol drag is implemented in a uniform way via `Snap2DSegment` and `Sheet:snap2DSymbol`. These use `Sheet.model.SnapSegments` and `Sheet.model.SnapSymbols`.

The basic idea is that:

* At the start of a drag operation, based on current circuit and what is dragged, a set of snap positions are precomputed and stored. 
* During the drag, if the dragged thing position hits a precalculated set of limits around the snap position then the object snaps to the snap position and stays there until the drag position it would have been in without snapping exits the limits. 
* Limits and snap positions apply in X and Y directions, independently.
* Snap positions close to the mouse position are highlighted as dotted horizontal or vertical red lines.

## Interaction with the MVU structure

Sheet has its own model and hence it must have a `view` and `update` function. These can be found in `SheetDisplay` and `SheetUpdate` respectively. 

Issie MVU has a Model structure:

```fsharp
(Issie) Model -> 
(Sheet)     Model.Sheet -> 
(BusWire)        Model.Sheet.Wire -> 
(Symbol)             Model.Sheet.Wire.Symbol
```

Sheet is thus top level component of the DrawBlock:  `DrawModelType.SheetT.Model` contains `DrawModelType.BusWireT.Model` which in turn contains `DrawModelType.SymbolT.Model`. 

Information from top level models is not available to lower level models. Information from upper levels is still available to lower levels, but naming discourages its use. The message types used are:

```fsharp

module SheetT =
    type Msg = // Sheet message D.U.
        | Wire of BusWireT.Msg // BusWire messages

type Msg = // Issie message D.U.
   | Sheet of SheetT.Msg // Sheet messages
   ...  // other messages
```

### Update

```fsharp
let Update.update  // main Issie update function
      (msg : ModelType.Msg) // the oldModel (for all issie) is changed based on the message received
      (oldModel : ModelType.Model) =
      // function body not shown

let SheetUpdate.update 
      (msg : DrawModelType.SheetT.Msg) // incoming message to be processed on the Sheet level
      (issieModel : ModelType.Model) // current state of the issie model, to be updated 
      : ModelType.Model*Cmd<ModelType.Msg> = 

      let model = issieModel.Sheet
      // rest of function body not shown
```

The peculiarity here is that the function that updates the Sheet, takes the whole issie model as input. This is done in case some of the information in the issie model is needed in the Sheet `update` function, hence the whole model is passed to it as an input. In the code of the `update` function, the `SheetT.Model` is extracted from the `issieModel` (and named `model`) so that both models are available by name to the rest of the code.

The message carried by the output command of Sheet.update is of type `ModelType.Msg`, meaning it is a message that can be sent to any module in issie. This is done so that the Sheet can communicate any changes to its model globally.

### View

```fsharp

let MainView.displayView
      (model : Model) // issie model and issie message
      (dispatch : Msg -> unit) =
      // function body not shown

let SheetDisplay.view
      (model : Model) // SheetT.Model
      (headerHeight : float) // self explanatory
      (style : CSSProp list) // ...
      (dispatch : Msg -> unit) // function that dispatches messages of type SheetT.Msg
            : ReactElement =
       // function body not shown
```

It is not necessary for the view function to access any information outside of the `SheetT.Model` nor in any messages that are not related to the Sheet. The output of every view function is a ReactElement that is displayed on screen.

### Adapter Functions

In `Sheet.fs` there are 3 functions that enable commands to be made to any level of the DrawBlock from Sheet.

```fsharp

/// Creates a command to Symbol
let symbolCmd (msg: SymbolT.Msg) = Cmd.ofMsg (ModelType.Msg.Sheet (Wire (BusWireT.Symbol msg)))

/// Creates a command to BusWire
let wireCmd (msg: BusWireT.Msg) = Cmd.ofMsg (ModelType.Msg.Sheet (Wire msg))

/// The 2 functions above wrap a SymbolT.Msg or a BusWireT.Msg in a Sheet.Msg and then make a sheet command out of it. These commands will trigger the respective update function, thus allowing Sheet to update BusWire and Symbol models.

/// This is self explanatory
let sheetCmd (msg: SheetT.Msg) = Cmd.ofMsg (ModelType.Msg.Sheet msg)

```


