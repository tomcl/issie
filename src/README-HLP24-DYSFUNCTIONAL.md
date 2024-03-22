# Dysfunctional# HLP 2024 work

Wiki page: https://github.com/BlueCP/issie-2024/wiki

The beautify function can be invoked using Ctrl+B, or by selecting the 'Beautify' option from the edit menu.

Modules that contain the bulk of our work:

- SheetBeautifyHelpers
- SheetBeautifyHelpers2
- SheetBeautifyD1
- SheetBeautifyD2
- SheetBeautifyD3
- SheetBeautify
- SheetBeautifyTest
- TestDrawBlock
- RotateScale

(SheetBeautifyHelpers is redundant and could be removed - we use SheetBeautifyHelpers2)

In addition, we made changes to the following modules:

- SheetUpdate, in order to execute the beautify function on Ctrl+B or clicking the menu option.
- Renderer, to add the option to invoke the beautify command as described.
- SymbolResizeHelpers: remove the print statement used when running rotatePortInfo to greatly speed up execution time.

There are some things in this repo that were not shown during the demo:

- A testing framework that takes the current sheet in the editor and tests the beautify function on it (Ctrl+T). The first option is to apply a series of random pertubations to get a single sheet, which is beautified to compare before and after. The second option is to apply random perturbation to get many random sheets, which are then beautified and information about sheet scores is aggregated to get an idea of how the beautify function performs overall.
- Another testing framework that uses a DSL to create a set of simple test circuits, using TestDrawBlock. Each of D1, D2, and D3 can be run individually, and their metrics compared. The test circuits can also have random perturbations applied to them (position, rotation, flip).