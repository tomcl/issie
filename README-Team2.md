# Smart Custom Symbol Sizing 
Bryan Tan (bet20)

## 1) Auto
Given two connected symbols, resize one to make the port gaps on both symbols equal so wires are straight. Test using the button in the menu (Edit > TestResize)

Changed files: 
- `SmartSizeSymbol.fs` - all the logic
- `SmartHelpers.fs` - some helpers

## 2) Manual
Hold `Ctrl` near a custom component to highlight its four corners. Drag a corner in any direction to resize or mirror the component. 

Changed files: 
- `DrawModelType.fs` - added new message types required for resizing
- `Common/DrawHelpers` - added new type of circle to display the corners of components (following pattern for `portCircle`)
- `DrawModelType.fs` - added new messages to indicate resize operation and whether to display corners, added new cursor style
- `Sheet.fs` - added function to detect if mouse is over a custom component corner, integrated with `mouseOn`
- `SheetUpdate.fs` - added new commands to show/hide custom component corners when `Ctrl` held down
- `SheetUpdateHelpers.fs` - modified mouse update functions to run the correct functions during symbol resizing
- `SymbolUpdate.fs` - match the new messages added and call the right functions during symbol resizing
- `SymbolView.fs` - added code to draw circles on the conrners of custom components
- `SymbolHelpers.fs` - new file, contains helpers used in `SymbolView.fs` (couldn't go in `SmartHelpers.fs`)
- `SymbolUpdateResizeHelpers.fs` - new file, contains manual component resize logic (followed pattern set by `SymbolUpdatePortHelpers.fs`)
- `Renderer/Renderer.fsproj` - added the new files to the project

# Smart Port Reordering
Dharmil Shah (dgs119)

## Changed Files:
- `CommonTypes.fs`
    - Added a helper to the `Edge` Type to find an opposite  `Edge`.
- `SheetUpdate.fs`
    - Passed helpers from `BusWireUpdate` to `SmartPortOrder` function.
- `SmartHelpers.fs`
    - Added helpers. Helpers are tagged with `HLP23: AUTHOR dgs119'.
- `SmartPortOrder.fs`
    - Module that implements port reordering algorithm.
- `Symbol.fs`
    - Adds helper `getPortOrientationFrmPortIdStr`.

# Smart Wire Autoroute
Jian Fu Eng (jfe20)

## Smart Routing Algorithm:
For ease of understanding, the following algorithm and variable names used in the codebase are all explained in the simple case of no rotated symbols. However, the code implemented supports rotated symbols as well.

1)  Check if initial autorouted wire has any intersections with symbols. 
        If yes, calculate the bounding boxes of all the intersected symbols.
2)  Attempt to shift the vertical seg of the wire to buffer amount left of the left most
bound of the intersected symbols. 
If there are still intersections, try shifting to the right most bound + buffer.
3)  If there are still intersections, recursively try to shift the horizontal seg of the wire to either the top or bottom most bound of the intersected symbols. 
If both shifted wires still result in an intersection, compute the vertical distances between 
the start/end pos of the wire and the top/bottom bound of the intersected symbols. 
Using the 4 vertical distances computed, decide whether to try shifting the wire up or down 
depending on which results in a wire with shorter vertical distance.

A max recursion depth is defined for step 3 so that Issie will not break when there are physically 
no possible routes that will not intersect any symbol (eg when dragging a symbol around such that 
the dragged symbol is within another symbol) or when there are special corner cases that have not 
been implemented yet (eg symbol A is in top left quadrant with input port facing up, connected to
symbol B in bottom right quadrant with output port facing down, with other symbols in between the
2 symbols).

## Changed Files:
- `SmartHelpers.fs`
    - `getSymbolBoundingBox`: Takes in `ComponentId` and returns the bounding box of the corresponding symbol. This function takes into account the rotation of the Symbol as well.
    - `getAllSymbolBoundingBoxes`: Returns a list of the bounding boxes of all symbols in the current sheet.
    - `findWireSymbolIntersections`: Returns list of bounding boxes of symbols intersected by wire.
    - `getStartAndEndWirePos`: Return the start and end positions of a wire.
- `SmartWire.fs`
    - Module that implements smart wire autoroute algorithm.
# Smart Channel
Derek Lai (ddl20)

## Changed Files:
- `SmartChannel.fs` - To implement smart channels.
- `SmartHelpers.fs` - To add helpers which detect overlap.
- `SheetUpdate.fs` - To add horizontal channel generation.
- `SheetUpdateHelpers.fs` - To add horizontal channel generation.
