## Team-phase work by yc3521
D1 build: in SheetBeautifyD1.fs.
Objective: Reducing total wire segments without increasing wire crossings by scaling the Symbols and translation on the sheet.


### Summary of how objective can be achieved:
We need to find the arrangement such that the number of input and output ports that are either horizontally or vertically on the same line is maximized

1. This can be achieved by first identifying singly connected components. For single connection component between symbols, their wires can always be straighten without changing or crossing other wires. We only need to shift the components across the sheet.

2. For not singly connected components, such as, two symbols that have more than one wires connected. We need to enlarge the components to fit the spaces between ports with one another and it will be achieved by shifting + enlarging.


### Implemented functionalities useful in D3 Build:

1. Singly connected Component
    1. Function to check if a wire is parrallel(can be straighten)
    2. Function to find out all the singly connected component returning a list of component ID as string
They are useful because it helps us identifying the single line that can be straighten

2. Multiple Connected Component(still working on)
    1. Function that return a model enlarging component to align two ports(still need to change position to fully align)
