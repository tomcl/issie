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

 

 

 
