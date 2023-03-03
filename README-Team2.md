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
