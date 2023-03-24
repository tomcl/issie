# Features Implemented: 

## Smart Channel: 
* Wires are neatly placed with least amount of crossings 
* Works in every direction, however you need to use two different shortcuts (CTRL + H) for horizontal (CTRL + J) for vertical 
    * This was done to make auto detect better
* Works with flipped/rotated components
* feature - can auto detect channels out of any components 
    * will try to place in the middle and have the biggest area without components inside
* feature - offers a choice to replace wires which have been too difficult to route
* feature - highlights components that form the channel
* feature - routes wires originating from the same port together (same net wires)
* limitation - least amount of crossing may be unachiaveble due to routing same net wires together 
* limitation - auto detect works only if the channel makes sense, if it doesnt it will still from a channel but it will cause undisiralbe effects

## Wire replacement with labels:
* Offers choice to replace selected wire(s) with labels
* Works in any orientation, flipped/rotated components
* feature - Creates popup window to enter name / confirm the replacement (CTRL + L)
* feature - Can be forced to skip popup and just replace wire(s) with auto generated name(s) (CTRL + SHIFT + L)
* feature - If only one wire is selected it can be named by user or can use generated name if no imputs have been made
* feature - For clarity reason cannot replace wire already connected to a label

## Smart Rotate:
* Created UI to perform rotation and scaling on sheet using buttons. 
* Blocks of selected symbols can be Rotated, Flipped or Scaled about the block center. Wires autorouted too.
* Handles invalid placements for every operation, as well as state changes after clicking elsewhere.
* Original keypress / edit menu for these operations kept (as in individual smart rotate). UI uses a slightly modified scaling function.
* feature - UI created for scaling and rotation when more than 1 symbol selected.
* feature - Correct handling of invalid placement and operations with highlighting / drag and drop states.
* limitation - original individual scaling keypress (Ctrl + U, Ctrl + I) does not affect the UI Box created
* limitation - on sudden quick movements when moving symbols, UI box is misaligned.

## Smart Resizing
* By clicking Ctrl+E (or Cmd+E), the two selected components are resized
* feature - if a resized symbol overlaps with another symbol in the canvas, placement is not allowed. Instead, the user must drag the symbol to an empty location and drop
* feature - works with flipped channels
* feature - a symbol can be dragged along the canvas and the Custom symbol closest to it will get selected for resizing. Actually, it is the symbol being dragged, that will get resized, according to the standard rules (like Ctrl+E). The user still has to click Ctrl+E in order to perform the resizing action. We found it gives the user more control.
* feature - components can be resized even without any connections between them. However if the user wants the component to get shifted, there must me at least one connection between components.
* limitation - dragging symbol across the canvas always highlights the closest Custom component - no CtrlDown is required.
* limitation - does not always work with rotated symbols.
