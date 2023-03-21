# Features Implemented: 

## Smart Channel: 
* Wires are neatly placed with least amount of crossings 
* Works in every direction, however you need to use two different shortcuts (CTRL + H) for horizontal (CTRL + J) for vertical 
    * This was done to make auto detect better
* Works with flipped/rotated components
* feature - can auto detect channels out of any components 
    * will try to place in the middle and have the biggest area without components inside
* feature - offers a choice to replace wires which have been too difficult to route
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
