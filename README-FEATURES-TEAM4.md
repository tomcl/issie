# Implemented Functionality

## sheetAlignScale





## sheetOrderFlip




## sheetWireLabelSymbol

- Heuristic based algorithm to identify 'wire label worthy' wires and automatically replace identified wires with appropriate wire labels, or vice versa. An optimal position and orientation is determineed for each wire labels, aligning them with connected port as much as possible. If not enough room to place the wire label, it automatically adjusts wire label position to minimize intersection with other symbols, by searching within a limited grid range for alternative positions

- Provide context menu itemd for converting individual wires to wire labels and back, can be used manually to undo the wire label beautification if this is not wanted

- Provide Electron edit menu options for conversion of selected wire nets to wire labels, and vice versa.


 