## Team-phase work by yc3521
D1 build: in SheetBeautifyD1.fs.
Objective: Reducing total wire segments without increasing wire crossings by scaling the Symbols and translation on the sheet.


### Summary of how objective can be achieved:
1. We need to find the arrangement such that the number of input and output ports that are either horizontally or vertically on the same line is maximized, because this can make sure the number of straight lines connecting inputs and outputs are max.

2. To achieve this arrangement, we need to determine how many input-output ports are on opposite sides of two symbols. Then find out the opposite sides that has the most input-output wire that are either on the same horizontal or vertical line by scaling the symbols(for more than two input-output paris, we can at least align two pairs by scaling and translation).

3. Use the concept of two symbols arrangemengt, search all pairs of symbols on the sheet and rank the pairs with highest being the pair of symbols with the side that has the most same line input-output ports.

### Implemented functionalities useful in D3 Build:


