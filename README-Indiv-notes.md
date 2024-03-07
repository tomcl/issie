# Individual Project Documentation

## SheetBeatifyHelper Functions

Write a required set of build and testing functions (Figure 1) that will be useful in Team phase work. 

| | Key | Type | Difficulty | Value read or written |
|-| --- | ---- | ---------- | --------------------- |
|x| B1R, B1W | RW | Low | The dimensions of a custom component symbol |
|x| B2W | W | Medium | The position of a symbol on the sheet |
|x| B3R, B3W | RW | Medium | Read/write the order of ports on a specified side of a symbol |
|x| B4R, B4W | RW | Low | The reverses state of the inputs of a `MUX2` |
|?| B5R | R | Low | The position of a port on the sheet. It cannot directly be written. |
|x| B6R | R | Low | The Bounding box of a symbol outline (position is contained in this) |
|?| B7R, B7W | RW | Low | The rotation state of a symbol |
|x| B8R, B8W | RW | Low | The flip state of a symbol |
|x| T1R | R | Low | The number of pairs of symbols that intersect each other. See Tick3 for a related function. Count over all pairs of symbols. |
|x| T2R | R | Low | The number of distinct wire visible segments that intersect with one or more symbols. See Tick3.HLPTick3.visibleSegments for a helper. Count over all visible wire segments. |
|x| T3R | R | Low | The number of distinct pairs of segments that cross each other at right angles. Does not include 0 length segments or segments on same net intersecting at one end, or segments on same net on top of each other. Count over whole sheet. |
|x| T4R | R | Medium | Sum of wiring segment length, counting only one when there are N same-net segments overlapping (this is the visible wire length on the sheet). Count over whole sheet. |
|x| T5R | R | Low | Number of visible wire right-angles. Count over whole sheet. |
|?| T6R | R | High | The zero-length segments in a wire with non-zero segments on either side that have Lengths of opposite signs lead to a wire retracing itself. Note that this can also apply at the end of a wire (where the zero-length segment is one from the end). This is a wiring artifact that should never happen but errors in routing or separation can cause it. Count over the whole sheet. Return from one function a list of all the segments that retrace, and also a list of all the end of wire segments that retrace so far that the next segment (index = 3 or Segments.Length – 4) - starts inside a symbol. |

One helper function has been written in TestDrawBlock.HLPTick3 for you to use.

| Key | Type | Difficulty | Value read or written |
| --- | ---- | ---------- | --------------------- |
| n/a | R | High difficulty | See `TestDrawBlock.HLPTick3.visibleSegments` The visible segments of a wire, as a list of vectors, from source end to target end. Note that a zero length segment one from either end of a wire is allowed which if present causes the end three segments to coalesce into a single visible segment. |

A short dive into the provided helper function `TestDrawBlock.HLPTick3.visibleSegments`: 

The function calculates the visible segments of a wire within a sheet model.
Give the ID of the wire and the model, a list of XY vectors representing the visible segments of the wire would be returned. Each vector indicates the direction and length of a segment. Invisible segments are merged with adjacent segments to ensure that the resulting list contains only visible segments.

---

## RotateScale Improvement

Improve part of the RotateScale code making it comply better with the [Issie Coding Guidelines](https://github.com/tomcl/issie/wiki/1---Coding-guidelines-for-ISSIE), stating as a set of bullet points how you believe it is improved. 

This part of code consists of the following functions: 

1. `scaleSymbol`

    This function is responsible for scaling a symbol based on a scaling factor and an offset. It takes a tuple `(float * float) * (float * float)` representing scaling factors for X and Y axes, along with the symbol to be scaled. The function first calculates the new position of the symbol after scaling, then updates the position and component of the symbol accordingly. 

    This function shows well written documentation, only minor pipelining have been applied to the code to improve its readability. 

2. `groupNewSelectedSymsModel`

    This function groups newly selected symbols in the model, applying a modification function (`modifySymbolFunc`) to each selected symbol. It takes a list of component IDs of selected components, the current symbol model, a list of selected symbols, and the modification function. It filters out unselected symbols, then applies the modification function to each selected symbol, and updates the model accordingly.

3. `flipBlock`

    This function flips a block of symbols (specified by a list of component IDs) within the model based on a flip type (horizontal or vertical). It retrieves selected symbols, performs the flip operation, and updates the model with the new symbols.

4. `postUpdateScalingBox`

    This function is involved in updating the "scaling box" part of the model after every model update. It handles cases where either one or multiple components are selected. It checks if a scaling box already exists and updates it accordingly. If multiple components are selected, it creates a new scaling box with buttons for scaling and rotating the selected components.

    Match cases have been used to improve the function layout, minor pipelining have also been used in its helper functions. 

5. Other helper functions
    - `symbolCmd`: Generates Elmish commands for symbol-related messages.
    - `sheetCmd`: Generates Elmish commands for sheet-related messages.
    - `makeButton`: Creates a button symbol based on a theme and position.
    - `makeRotateSym`: Modifies a symbol to include rotation properties.

This part of code uses precise naming, no need to improve. 

---

## Team phase work

Using the Figure 1 functions as needed, make a coding contribution to your deliverables of the Team phase work. For example: some tests, some useful helper functions, a partly working beautify function. The functionality you provide, and how it addresses a Team deliverable, must be specified and submitted as comments in the code. 

| Function | Adjust on sheet | Primary Optimization | Test using random sample inputs |
| -------- | --------------- | -------------------- | ------------------------------- |
| D3. sheetWireLabelSymbol | Add or remove wire labels (swapping between long wires and wire labels). Improve symbol rendering. | Reduce wiring complexity (symbol rendering is judged manually on appearance) | Test ability to add labels without overlap in presence of adjacent components with visually helpful placement. |

`https://github.com/dyu18/hlp24-project-issie-team7/tree/indiv-az1821/src/Renderer/DrawBlock/SheetBeautifyT3.fs`

The test of this part could be divided into some assertion functions and a test model. To contribute to this part, a test skeleton code have been built and some basic assertion functions are also added. 

### Assertion cases

For now, the following assertion cases are attemped: 

- Wire label placement with different wire lengths.
- Wire label removal when under the threshold.
- Wire label naming based on component and port names.
- Wire label positioning adjustment to avoid overlaps.

The run test function before and after label manipulation is still due to testing, more assertion functions can also be added in the future. 

### Test structure 

The test of label placement and removal is splitted into three steps: A test driver would call a dataset generation function first, which provides a set of data of different component position and connection cases; Then, the `sheetWireLabelSymbol` function would be called to beautify the sheet; Finally, the assert functions would be used to check if the beautifier function has done the job as expected, those that fail to pass the assertion would be filtered out. 

1. Test dataset generation
    - 
    - All the tests must pass all the position asserts in `TestDrawBlock.fs` first, before they are used to test the `sheetWireLabelSymbol` function, in order to make sure these cases are legal and meaningful. 

2. Assert functions
    - Label placement : wire lengths > threshold
    - Label removal : wire lengths < threshold
    - Correct connection 
    - No overlaps

3. Test driver
    - dataset generation
    - `sheetWireLabelSymbol`
    - assertion and results filtering
    
4. Improvements

---

## Work Diary 

- Feb 21st `B1RW` `B7RW` `B8RW`
    - Finished two functions for read and write (not combined). 
    - Basic read (?) to be corrected. 
    - [x] TODO: "These should be combined in a Lens (if possible)." 

- Feb 24th `B2RW`
    - Read and write position 
    - Corrected read functions of B7 and B8

- Feb 25th `B5R` `B6R` `B3R` `B4R`
    - [x] TODO: B5 - check if look up for port/symbol is needed

- Feb 27th `B3W` `B4W` `T1R` `T2R` `T3R` `T4R` `T5R`
    - Finished write functions of the order of ports and reverse of states
    - "when will the test functions be called?"
    - (rough work) test function helpers 

- Feb 28th `RotateScale.fs` analysis and improvement
    - Finished analysing the code and purpose of each function. 
    - Improve on library functions & naming/layout & pipelines. 
    - [x] TODO: check for data type improvements

- Feb 29th: organise notes and documentation. 
    - [x] TODO: team phase
    - [x] TODO: XML comments

- Mar 1st
    - Fixed error in type match
    - cleaned up code

- Mar 4th
    - Built the basic structure of test driver
    - Show T3 tests in issie file menu
    - Basic assert functions (case 1 & 2)

- Mar 6th
    - Basic assert functions (case 3)
    - Documentation of assertion function 
    - [ ] TODO: assertion function case 4
    - [ ] TODO: test cases with wire labels


