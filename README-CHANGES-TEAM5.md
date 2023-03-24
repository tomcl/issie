## Smart Wire Autoroute: 

D.U. return type for SmartAutoroute function (SmartAutorouteResult): 

* BusWireUpdate: updateWire, updateWires, updateSymbolWires, newWire  

* Implemented to handle D.U. return type implemented for smartAutoroute function, and gives you the choice to automatically replace wires with wire label (if you want to remove popup feature prompt).  

* Reference commit (since this commit and modified continuously in later commits): https://github.com/hlp-team-5/hlp23-team5/commit/7fadcaccb2d98f2af6878c2c3c050ead2abdb1bc  



Wire Label Popups: 
* BusWireUpdate: WireLabelReplacement calling wire label function, DrawModelType: new type in WireLabelReplacement 

* Reference commit: https://github.com/hlp-team-5/hlp23-team5/commit/91386ea9c478b67d7e71d34d72bc256d2c17d946 

 

## Smart Port Order 

* Updated Renderer.fs, SheetUpdate.fs, DrawModelType.fs with `SmartPortOrder.singleReOrder` this was to enable functionality on the Edit section of the UI. Added a Tick3 style helper to SheetUpdate.fs to when function is called to enable lower compile order files to be made available to SmartPortOrder 

 

## Smart Channel Routing: 

* Added a Tick3 style helper to SheetUpdate to work around the compilation order 

 

## Smart Autosizing: 

* Added a Tick3 style helper to SheetUpdate to work around the compilation order 

 

## Smart Port Arrange: 

* This is a new file added to the DrawBlock folder 

* Added a line to Renderer.fsproj under SmartSizeSymbol so that it compiles correctly 

* Added a line to Renderer.fs so that TestPortArrange appears in the edit menu 

## Smart Rendering:

* Mainly edited SymbolView.fs to render new components, change legend positions, render rotations and scaling.
* Renderer.fs : added old gates and new gates option in viewMenu. Defined 
* DrawHelpers.fs : Added helper functions for SVG generation and replaced makeAnyPath -> makeAnyPathWithTransform and makePathFromAttr ->        makePathFromAttrWithTransform to allow SVG transformations for a path.
* CommonTypes.fs : Changed gates to n-input gates in ComponentType and defined binaryGate functions that match gates according to their input n (default = 2)
* DrawModelType.fs : Changed Theme type to have new gates and old gates. Defined a new type "GateStyle" and added a GateStyle option to the AppearanceT type.
* Symbol.fs : updated getPortPos and getPortPosToRender to differentiate new gates from old style gates. Also updated instances of Appearance to now gate a Gatetype option
* Style.fs - updated pattern matching in canvasVisibleStyleList to include new and old gates for Theme type.
* TruthtableView.fs : updated to N-input gates (e.g. and -> and n). There are warnings that I kept as requested by Dr Clarke for the N-input logic in WaveSimulation.


 

 

 
