* Changed getChannel -> now requires orientation because its more convinient, also works with vertical channels
* Changed TestSmartChannel (also renamed it to FormSmartChannel) to have type of Orientation -> now autofinds channel based on the orientation
* Added functions for channel formation in SheetUpdateHelpers
* Added functions to generate popupfunction in PopupView

* Change SheetUpdate -> Rotate & Flip messages. Now replaced with smart rotate functionality so working with single and multiple components.
* SymbolView symbol re-rendering detects changes in 'StyleType' (for curvy shapes)
* Added Style Type in drawmodeltype 
* Changed the function "validateTwoSelectedSymbols" to also validate 1 symbol. Needed for smartResizingDraggable in <SheetUpdateHelpers.fs>
* Added type OrientationS to <Symbol.fs>
* Changed TestPortPosition in <SheetUpdate.fs>

What we changed for Scaling Box :
* We took a function from Symbol (createSymbol) and put it in Smart Helpers. We edited it a bit to fit our code.
* SymbolView - Added code to draw the rotation and scaling buttons
* Sheet Display - Added code to draw the Box outline of selected components
* Sheet update - Added scaling option for drag 
               - Changes in move symbol and initialise movement to make sure the box moves as well
* Added 'Scaling' Action to models when user has clicked down on the scale button
* Added three types scaling box (sheet model type), scaling button (component type), rotate button (component type)
* Added box and button list (kkeps track of the ids of the buttons) to type model  
* Sheet update - Deleting the buttons (that are symbols) when they should, in Mouse up and mouse down

