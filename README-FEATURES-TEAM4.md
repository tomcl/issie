# Implemented Functionality

## sheetAlignScale





## sheetOrderFlip
- Exhuastive serach algorithm to reduce number of wire crossings by first trying every combination of swapping MUXes inputs, flipping MUXes and Gates, changing orientation of MUXes and gates, finding the combination which makes the wire crossings to be minimum. Then, update the sheet by applying the combination to symbols and re route the wires, on top of the updated sheet, further reduce wires by reverse port order of left edge or right edge or both edges of custom components. Note that it will not consider a combination which increases number of wire bends even if it reduces number of wire crossings.

    - Limitations: custom components re-order currently only works for left and right edges. 



## sheetWireLabelSymbol

- Heuristic based algorithm to identify 'wire label worthy' wires and automatically replace identified wires with appropriate wire labels, or vice versa. An optimal position and orientation is determineed for each wire labels, aligning them with connected port as much as possible. If not enough room to place the wire label, it automatically adjusts wire label position to minimize intersection with other symbols, by searching within a limited grid range for alternative positions

- Provide context menu itemd for converting individual wires to wire labels and back, can be used manually to undo the wire label beautification if this is not wanted

- Provide Electron edit menu options for conversion of selected wire nets to wire labels, and vice versa.


 