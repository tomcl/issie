# Implemented Functionality

## sheetAlignScale (D1)
- Scaling and alignment of custom components such that ports are aligned and wires between ports are straight. Works both horizontally and vertically (on rotated components). This is done by finding the separation between ports to find a scaling factor by which HScale or VScale must be multiplied to ensure port separation, then aligning the topmost or leftmost components correctly.

- Alignment of all components to reduce the number of turns in the wire. This is done both for singly connected and multiply connected components, where multiply connected components are split into three cases in order to align them more effectively.

- Heuristic alignment of arrays of components. The heuristic used is a case of multiply connected components, where one component that is connected to several components aligns the components that it is connected to. 

- In all cases, symbols are ensured not to overlap by reverting them to previously made sheets.

- Code can be found in SheetBeautifyD1.


## sheetOrderFlip (D2)
- Exhuastive serach algorithm to reduce number of wire crossings. Start by grouping components into 2 sets: MUXes + gates, and custom components. Firstly, try every combination of swapping MUXes inputs, flipping MUXes and Gates, changing orientation of MUXes and gates, finding the combination which makes the wire crossings to be minimum. Then, update the sheet by applying the combination to symbols and re route the wires, on top of the updated sheet, further reduce wires by reverse port order of left edge or right edge or both edges of custom components. Note that it will not prefer a combination which increases number of wire bends even if it reduces number of wire crossings.

    - Limitation: custom components re-order currently only works for left and right edges. 
    - Limitation: beutify function not scalable for large circuits and execution time will be long, so if total components number exceeds a threshold, beautify function will not be applied and return original sheet instead, to avoid screen freeze.



## sheetWireLabelSymbol (D3)

- Heuristic based algorithm to identify 'wire label worthy' wires and automatically replace identified wires with appropriate wire labels, or vice versa. An optimal position and orientation is determineed for each wire labels, aligning them with connected port as much as possible. If not enough room to place the wire label, it automatically adjusts wire label position to minimize intersection with other symbols, by searching within a limited grid range for alternative positions

- Provide context menu itemd for converting individual wires to wire labels and back, can be used manually to undo the wire label beautification if this is not wanted

- Provide Electron edit menu options for conversion of selected wire nets to wire labels, and vice versa.

## D3 Testing

- Implemented multiple specific and general tests:
  - Test 1: simple test for wire label replacement
  - Test 2: general test with cramped components, lots of wires and rotations/flips
  - Test 3: general test with a realistic circuit and overlapping wires
  - Test 4: test for label placement adjustment when there is not enough space
- Each test has randomised and shuffled parameters to make it easier to identify edge cases. These are initialised by seed so consistency can be achieved between tests
- Tests can be run from the D3 Tests submenu in the File menu
- Created a metric for marking each test based on:
  - ISP: number of intersecting symbol pairs
  - ISS: number of wire segment intersections
  - ISR: number of wire intersections
  - wireWaste: actual wire length divided my minimum possible wire length
- Added option to fail a test if the marking metric detects a problem, allows for semi-automated identification of edge cases    

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