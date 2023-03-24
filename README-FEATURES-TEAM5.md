## Smart Wire Autoroute: 

 

* wires avoid any number of components 

* wires avoid any number of components 
* works for all orientations segment destination, source RIGHT/LEFT/TOP/BOTTOM (not between TOP and BOTTOM) 

* works for 2 and 3 segment wires (not for 2-segment wires with output port on LEFT) 

* feature - complex routes offer optional wire label replacement for new and updated wires 

* feature - wire label replacement comes with a Popup that asks user if they want it replaced 

* feature - wire label replacement Popup feature lets you select name of wire label or generate random name 

* feature – same symbol routing (wire connected from and to same symbol) overlaps existing segments same net 
* feature – same symbol routing (wire connected from and to same symbol) segments are separated from parallel segments different net 
 
 

 

## Smart Autosize: 

 

* translate and scale both standard and custom symbols so that any slightly misaligned wires are straightened 

* wires are only straightened if they are within a threshold of 11 pixels 

* feature works in vertical and horizontal orientations and with rotated symbols 

 

## Tidy Smart Port 

* Re-Orders ports on selected component to tidy up ports 
* feature – Works for all orientation of edge 
* feature – ReOrder all ports connected to 1 symbol 
* feature – ReOrders all ports from different edges although the algorithm as a limitation does nothing when given non-contiguous wire and mixing inputs/outputs within contiguous 
 

## Smart Port Arrange: 

 

* combines resizing and port reordering and applies it to every symbol in the sheet connected by a wire 

* doesn’t always produce clean wires since a symbol could be adjusted multiple times for one set of wires and be scaled out of alignment for another set 

 

## Smart Channel Routing: 

 

* adjusts wire segments so that the wires are equally spaced in between the channel boundaries 

* does not route around additional symbols within the channel 

* if wires already exist when loading sheet, a symbol must be moved before routing will work 
