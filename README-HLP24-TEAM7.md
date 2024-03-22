## Team7 Wiki Page
https://github.com/dyu18/hlp24-project-issie-team7/wiki

## About Beautify Function
(statement of how beautify function is invoked and any other user info about how to run it)

The function `sheetBeautify`, which unifies D1, D2, D3, can be invoked by users clicking on the "Beautify Current Sheet" item in the Edit dropdown menu, or by right-clicking on the canvas and selecting "Beautify current sheet" in the context menu. The actions can be undone with `Ctrl+Z` and redone with `Ctrl+Y`.


### D1: sheetAlignScale


### D2: sheetOrderFlip
  
Helper datatypes and functions are created to perform Exhaustive Search Algorithm mentioned in the project requirement

**Exhaustive Search Algorithm**


Two new data types, `symbolScript` and `modelScript`, are created, to describe the configuration of a `SheetT.Model`:

- A `symbolScript` contains all parameters that can be changed for a single symbol.
- A `modelScript` is a list of `symbolScript`, with each entry corresponding to one symbol in the model.

### Functions

<!-- - **`generateModelScript`**: Generates all possible `symbolScript` instances for one symbol. This function is pivotal in exploring the configuration space of individual symbols within the model.

- **`generateAllModelScripts`**: Produces all possible `modelScript` configurations for the entire model. By iterating through every symbol and applying `generateModelScript`, it constructs a comprehensive list of model configurations.

- **`optimizeFlipForComponents`**: Conducts an evaluation across all generated model configurations, selecting and returning the one with the lowest number of overlaps. This function is crucial for identifying the optimal model configuration with minimized component overlap. -->


<!-- ## 2. **Heuristic Approach** -->

### D3: sheetWireLabelSymbol
The functions `autoWiresToWireLabels` and `autoWireLabelsToWires` are called in `sheetBeautify`, that is when automatic judgement on wires and wire labels would be made, and corresponding actions automatically taken.

The functions `selectedWiresToWireLabels` and `selectedWireLabelsToWires` can be invoked by users clicking on the "Replace Selected Wire Nets by Wire Labels" and "Replace Selected Wire Label Sets by Wires" items in the Edit dropdown menu, or by right-clicking on selected elements and clicking on "Same-Net Wires to Wire Labels" and "Same-Name Wire Labels to Wires" in the context menu. The actions can be undone with `Ctrl+Z` and redone with `Ctrl+Y`. (These are mandatory commands where the invoked transformation must be performed, regardless of the wiring complexity. Useful when users would like to perform transformations manually.)

### Tests
The file `SheetBeautifyTest.fs` integrates all the tests for the above features, while keeping a same basic structure as in Tick 3. For each beautify strategy, specific test cases were generated to test features in D1, D2 and D3 (as mentioned above), these test cases are then passed to the test driver, where the beautifiers are applied. 

## List of Modules Containing Our Code
| Modules |
| --- |
| module SheetBeautifyHelpers |
| module SheetBeautify |
| module SheetBeautifyD1 |
| module SheetBeautifyD2 |
| module SheetBeautifyD3 |
| module SheetBeautifyTest |
| module TestDrawBlock |


## Changes to Original Code
- `SymbolView.fs`: improvements to symbol rendering in D3.
- `Renderer.fs`: additions to Edit menu items.
- `ContextMenu.fs` & `UpdateHelpers.fs`: additions to right-click menu items.

