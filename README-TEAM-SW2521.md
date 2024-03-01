# Team Contribution Statement - Samuel Wang - sw2521

Assigned to D2 `sheetOrderFlip` implementation, which can be found in [SheetBeautifyD2.fs](src/Renderer/DrawBlock/SheetBeautifyD2.fs).

_Helpers_

1.  Permutation algorithm completed.

    Helpers to count and find all orientations and permutations of ports were written. Counting functions are useful as we can identify the components which will take too much time by trying all permutations, and either skip them or use the clockface algorithm on them.

2.  Clockface algorithm completed, integration pending.

    Clockface algorithm to obtain the optimal port order of a component has been implemented. It has not been integrated into the beautify function as obtaining the correct rendered port order proved to be difficult. Once the rendered port order can be obtained, the clockface algorithm will be very useful in reducing number of permutations to try.

_Main Beautify Function_

1.  Starter objective completed.

    Beautify function is able to find all muxes, demuxes, and gates to perform reordering. Symbols with more wires connected to it are operated on first, as this is likely to reduce dependancy for future operations. Muxes and demuxes are first operated on, and then Gates. Clockface algorithm was yet to be implemented.

_Tests_

1.  Two tests were written to test the basic `sheetOrderFlip` function.

    These are are `makeOFTestCircuitDemo1` and `makeOFTestCircuitDemo2` in [TestDrawBlock.fs](src/Renderer/TestDrawBlock.fs). A Gen object of type bool was passed into the tests, with the first input being false and the second being true. Inputs specifies whether `sheetOrderFlip` was run. The tests also fails on all cases, which meant we can see the effect of `sheetOrderFlip` going to the next test. The circuit being drawn are as the following:

    *   Demo Circuit #1 (`test5`): Given circuit in Figure B1.

    *   Demo Circuit #2 (`test6`): A circuit which includes 2x Muxes, 1x Demux, 2x Gates. Used to demonstrate re-ordering of port across multiple components.

(Content: Ln 14)
