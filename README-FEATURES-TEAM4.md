# Implemented Functionality

## sheetAlignScale (D1)





## sheetOrderFlip (D2)




## sheetWireLabelSymbol (D3)

- Heuristic based algorithm to identify 'wire label worthy' wires and automatically replace identified wires with appropriate wire labels, or vice versa. An optimal position and orientation is determineed for each wire labels, aligning them with connected port as much as possible. If not enough room to place the wire label, it automatically adjusts wire label position to minimize intersection with other symbols, by searching within a limited grid range for alternative positions

- Provide context menu itemd for converting individual wires to wire labels and back, can be used manually to undo the wire label beautification if this is not wanted

- Provide Electron edit menu options for conversion of selected wire nets to wire labels, and vice versa.



## Combination (D4)
- <u>Basic</u> - applies D2 then D1 then D3 (order chosen for better solution)
- <u>Optimal</u> - although longer execution time, takes a list of functions (D1,D2,D3) and applies each recursively, keeping the best until no improvement or max depth


## Evaluation metrics
- <u>Changing assertions</u> to stop execution on first fail (sped up testing execution significantly)
- <u>*Execution*</u> of evaluation functions, allowing for: 
    - failing if function makes the sheet worse  
    - applying penalty on failing assertion
    - averaging all test data to produce score
- <u>Toggleable beautify</u> functions for visual testing
- <u>Popup</u> on test completion displaying score


## Randomised testing
- fully randomised circuit generation to find edge cases