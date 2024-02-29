## Team-phase work by dy321
D3 build: in SheetBeautifyB3.fs (module SheetBeautifyB3)

Contribution to team-phase work: implemented some fundamental functionalities required for D3.

### Implemented functionalities useful in D3 Build:
1. Replace a single given wire with wire labels.
    1. function to get starting and ending positions of original wire
    2. defined user-variable threshold (above which the wires would be converted into wire labels)
    3. defined minimum separation between a component and a wire label (in module Constants)
    4. function to add IOLabel (wire label) component
    5. function to place new wires between components and wire labels
    6. function to remove the original wire
2. Get total length of a wire and filter out the long ones.


### Remaining work in D3 to be done:
1. Filter out the wires that have available room for wire label replacement, choose appropriate positions of wire labels (e.g. make sure there's no overlapping within a pre-defined neighbourhood).
2. Remove wire labels and change into single wires (reverse operation).
3. Improve symbol rendering.

