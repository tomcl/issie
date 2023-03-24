# Changes To Original Code
See [PR](https://github.com/dharmilshah99/hlp23-team2/pull/40/files) for detailed changes.

## `src/Renderer/Common/CommonTypes.fs`
- Added [helper](https://github.com/dharmilshah99/hlp23-team2/blob/3d7b9095c238fc0871c6f800913f4db0c78a4d17/src/Renderer/Common/CommonTypes.fs#L261) to type `Edge` to get an opposite `Edge`.
- Added type to represent `Clockwise` and `Anticlockwise` directions.
## `src/Renderer/DrawBlock/DrawHelpers.fs`
- Added new type of circle to display the corners of components (following pattern for `portCircle`)
## `src/Renderer/DrawBlock/DrawModelType.fs`
- Added new messages to indicate resize operation and whether to display corners, added new cursor style
## `src/Renderer/DrawBlock/Sheet.fs`
- Added function to detect if mouse is over a custom component corner, integrated with `mouseOn`
## `src/Renderer/DrawBlock/SheetUpdate.fs`
- Added new messages to show/hide custom component corners when `Ctrl` held down
- Processing additional message `BeautifySheet` which reorders and aligns components on a sheet (see [line](https://github.com/dharmilshah99/hlp23-team2/blob/3d7b9095c238fc0871c6f800913f4db0c78a4d17/src/Renderer/DrawBlock/SheetUpdate.fs#LL804C6-L804C6)).
## `src/Renderer/DrawBlock/SheetUpdateHelpers.fs`
- Modified mouse update functions to run the correct functions during symbol resizing 
## `src/Renderer/DrawBlock/SymbolUpdate.fs`
- Match the new messages added and call the right functions during symbol resizing
## `src/Renderer/DrawBlock/SymbolView.fs`
- Added code to draw circles on the conrners of custom components
## [`src/Renderer/DrawBlock/SmartBeautify.fs`](https://github.com/dharmilshah99/hlp23-team2/blob/3d7b9095c238fc0871c6f800913f4db0c78a4d17/src/Renderer/DrawBlock/SmartBeautify.fs#L1)
- New module that contains logic for `BeautifySheet`.

## [`src/Renderer/DrawBlock/SmartHelpers.fs`](https://github.com/dharmilshah99/hlp23-team2/blob/3d7b9095c238fc0871c6f800913f4db0c78a4d17/src/Renderer/DrawBlock/SmartHelpers.fs#LL48)
- Added useful helpers and types shared between Smart modules.

## [`src/Renderer/DrawBlock/SmartPortOrder.fs`](https://github.com/dharmilshah99/hlp23-team2/blob/3d7b9095c238fc0871c6f800913f4db0c78a4d17/src/Renderer/DrawBlock/SmartPortOrder.fs#L1)
- Contains logic for reordering ports on two selected Symbols and flipping `Sel` of Mux/Demux components.

## [`src/Renderer/DrawBlock/SmartWire.fs`](https://github.com/dharmilshah99/hlp23-team2/blob/3d7b9095c238fc0871c6f800913f4db0c78a4d17/src/Renderer/DrawBlock/SmartWire.fs)
- Refactored code to integrate more symmetries.
- Contains logic for smart routing of wires to snap to net and avoid intersections with symbols.

## `src/Renderer/DrawBlock/Symbol.fs`
- Created new function [`getPortOrientationFrmPortIdStr`](https://github.com/dharmilshah99/hlp23-team2/blob/3d7b9095c238fc0871c6f800913f4db0c78a4d17/src/Renderer/DrawBlock/Symbol.fs#L846) that gets a port's orientation from a PortId string.  

## `src/Renderer/DrawBlock/SymbolHelpers.fs`
- New file, contains helpers used in `SymbolView.fs` (couldn't go in `SmartHelpers.fs`)

## `src/Renderer/DrawBlock/SymbolUpdateResizeHelpers.fs`
- New file, contains manual component resize logic (followed pattern set by `SymbolUpdatePortHelpers.fs`)

## `src/Renderer/Renderer.fs`
- Added additional menu [items](https://github.com/dharmilshah99/hlp23-team2/blob/3d7b9095c238fc0871c6f800913f4db0c78a4d17/src/Renderer/Renderer.fs#L239) that perform Smart functions (eg. Beautify, Reorder, Optimize Symbol, etc).

## `src/Renderer/Renderer.fsproj`
- Added new files in compilation order (eg. [`SymbolUpdateResizeHelpers.fs`](https://github.com/dharmilshah99/hlp23-team2/blob/3d7b9095c238fc0871c6f800913f4db0c78a4d17/src/Renderer/Renderer.fsproj#L63)).
