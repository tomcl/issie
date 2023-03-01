This is a readme of our individual coding tasks

Bartek's work:
* add helper functions, (XML comments are added in SmartHelpers.fs)
* add one type (OrientationS) to Symbol.fs
* changed SmartSymbolSizing.fs

Timur (AUTHOR Ismagilov) : 
* Added SmartHelper functions
* Full SmartRotate.fs, does block rotation, flipping & scaling (added ScaleType in drawModelType)
* Initial SmartRendering in SymbolView.fs (added StyleType in drawModelType). Working robustly with And and Or gates.
* Functionalities added in Symbol.fs, SheetUpdate, drawHelpers, Renderer.fs, explained at top level SmartRotate and SymbolView Comments

Zsombor Klapper's work: 
* Added SmartHelpert functions (XML contents discribes each, most important: replaceWireWithLabel)
* Full of SmartChannel.fs (Works with every kind of arrangement in vertical channels, not yet fully implemented for horizontal channels)

Rhea Khoury's work:
* Added SmartHelper functions 
* Full Smart port Reorder (works for all edges of two components)
* Implemented MUX flipping (Added some of these functions in Smart Helpers)
* Implemented smart port Reorder for multiple components (Added an extra test for that) (Second half of SmartPortReorder.fs)
* Added keyboard shortcuts for test in Renderer.fs and Sheet Update.
