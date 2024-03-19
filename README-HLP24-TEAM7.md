## Team7 Wiki Page
https://github.com/dyu18/hlp24-project-issie-team7/wiki

## About Beautify Function
(statement of how beautify function is invoked and any other user info about how to run it)

The function `sheetBeautify`, which unifies D1, D2, D3, can be invoked by users clicking on the "Beautify Current Sheet" item in the Edit dropdown menu, or by right-clicking on the canvas and selecting "Beautify current sheet" in the context menu. The actions can be undone with `Ctrl+Z` and redone with `Ctrl+Y`.


### D1: sheetAlignScale


### D2: sheetOrderFlip


### D3: sheetWireLabelSymbol
The functions `autoWiresToWireLabels` and `autoWireLabelsToWires` are called in `sheetBeautify`, that is when automatic judgement on wires and wire labels would be made, and corresponding actions automatically taken.

The functions `selectedWiresToWireLabels` and `selectedWireLabelsToWires` can be invoked by users clicking on the "Replace Selected Wire Nets by Wire Labels" and "Replace Selected Wire Label Sets by Wires" items in the Edit dropdown menu, or by right-clicking on selected elements and clicking on "Same-Net Wires to Wire Labels" and "Same-Name Wire Labels to Wires" in the context menu. The actions can be undone with `Ctrl+Z` and redone with `Ctrl+Y`. (These are mandatory commands where the invoked transformation must be performed, regardless of the wiring complexity. Useful when users would like to perform transformations manually.)


## List of Modules Containing Our Code
| Modules |
| --- |
| module SheetBeautifyHelpers |
| module SheetBeautify |
| module SheetBeautifyD1 |
| module SheetBeautifyD2 |
| module SheetBeautifyD3 |
| module TestDrawBlock |


## Changes to Original Code
- `SymbolView.fs`: improvements to symbol rendering in D3.
- `Renderer.fs`: additions to Edit menu items.
- `ContextMenu.fs` & `UpdateHelpers.fs`: additions to right-click menu items.

