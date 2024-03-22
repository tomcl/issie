# Beautifying functions by Team 4

### Wiki Page: (insert link)

(README has statement of how beautify function is invoked and any other user info about how to run it.)
add below

### D1 beautifying function (sheet->sheet)

### D2 beautifying function (sheet->sheet)

### D3 beautifying function 
`sheetWireLabelSymbol (sheet: SheetT.Model) : SheetT.Model`: 
this outputs a beautified sheet that automatically replace wires by wire labels where appropriate, user can change heuristic of what consider to be wire label worthy wires. 

`autoConvertWireLabelsToWires (sheet: SheetT.Model) : SheetT.Model`: 
this outputs a sheet after automatically converting all Wire Labels on the sheet to wires between corresponding inputs and outputs

`generateWireLabel (isForIndivWire: bool) (wire: BusWireT.Wire) (sheet: SheetT.Model) : (sheet: SheetT.Model)`: 
used in right-hand click menu. Convert a wire on a sheet to a wire label. isForIndivWire = true when user want to use this to convert selected single wire; isForIndivWire = false when user wants to use this to convert a selected bunch of wires. User can change choose wire condition for both inside the function. 

`turnWireLabelsToWires (wireLabel: SymbolT.Symbol) (sheet: SheetT.Model) :(sheet: SheetT.Model)`: 
used in right-hand click menu. Convert a selected wire component on a sheet to wire, ignore if not a wire label.

`convertSelectedWiresIntoWireLabels (comps: ComponentId list) (model: Model) (sheet: SheetT.Model): (sheet: SheetT.Model)`: 
used in edit menu, to bulk convert selected wires into labels.

`convertSelectedWiresLabelsIntoWires (comps: ComponentId list) (model: Model) (sheet: SheetT.Model) : (sheet: SheetT.Model)`:
used in edit menu, to bulk convert selected wire labels back to wires, ignore non-wire-labels components.


### Module list

### Existing source files changed:
- BlockHelpers -> getSymbolPos: changed sym.pos to sym.centre to do what XML comment said
- Add manhattan distance to XYPos inline
- Renderer.fs: added edit menu items for convert selected wires into wire labels and vice versa.
- ContextMenu.fs and UpdateHelper.fs: added right-hand click menu for convert a wire into a wire label and vice versa

(if needed) README has statement of anything that has changed in repo since demo and why.

(if needed) README has statement of anything important to be considered about functionality not shown in demo.

(If needed). README has rationale for any existing source files in Issie that your code changes other than the expected ones: TestDrawBlock, SheetBeautify, SheetBeautifyHelpers, any module you add.