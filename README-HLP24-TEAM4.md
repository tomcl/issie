# Beautifying functions by Team 4

### Wiki Page: 
Analysis of beautify function through testing is shown [here](https://github.com/AdvikChitre/issie/wiki/Beautify-Analysis)


<!-- 
(README has statement of how beautify function is invoked and any other user info about how to run it.)
add below -->

### D1 beautifying function 
- `sheetAlignScale` _ `[SheetT.Model -> SheetT.Model]`
This aligns all components on a sheet and scales custom components to reduce wire bends. Invoked automatically on D1 test cases (Alt1-7) and toggled with Alt-8. 
### D2 beautifying function 

### D3 beautifying function 
- `sheetWireLabelSymbol` _`[SheetT.Model -> SheetT.Model]`_  
this outputs a beautified sheet that automatically replace wires by wire labels where appropriate, user can change heuristic of what consider to be wire label worthy wires. 

- `autoConvertWireLabelsToWires` _`[SheetT.Model -> SheetT.Model]`_  
this outputs a sheet after automatically converting all Wire Labels on the sheet to wires between corresponding inputs and outputs

- `generateWireLabel` _`[bool -> BusWireT.Wire -> SheetT.Model -> SheetT.Model]`_  
used in right-hand click menu. Convert a wire on a sheet to a wire label. isForIndivWire = true when user want to use this to convert selected single wire; isForIndivWire = false when user wants to use this to convert a selected bunch of wires. User can change choose wire condition for both inside the function. 

- `turnWireLabelsToWires` _`[SymbolT.Symbol -> sheet: SheetT.Model -> SheetT.Model]`_  
used in right-hand click menu. Convert a selected wire component on a sheet to wire, ignore if not a wire label.

- `convertSelectedWiresIntoWireLabels` _`[ComponentId list -> model: Model -> SheetT.Model -> SheetT.Model]`_  
used in edit menu, to bulk convert selected wires into labels.

- `convertSelectedWiresLabelsIntoWires` _`[ComponentId list -> Model -> SheetT.Model -> SheetT.Model]`_  
used in edit menu, to bulk convert selected wire labels back to wires, ignore non-wire-labels components.


### Module list
- SheetBeautifyD1
- SheetBeautifyD2
- SheetBeautifyD3
- SheetBeautifyHelpers
- GenerateData
- TestDrawBlock
- TestDrawBlock1
- TestDrawBlock2
- TestDrawBlock3
- TestDrawBlock4

### Existing source files changed:
- BlockHelpers -> getSymbolPos: changed sym.pos to sym.centre to do what XML comment said
- Add manhattan distance to XYPos inline
- Renderer.fs: added edit menu items for convert selected wires into wire labels and vice versa.
- ContextMenu.fs and UpdateHelper.fs: added right-hand click menu for convert a wire into a wire label and vice versa
<!-- 
(if needed) README has statement of anything that has changed in repo since demo and why.

(if needed) README has statement of anything important to be considered about functionality not shown in demo. -->
