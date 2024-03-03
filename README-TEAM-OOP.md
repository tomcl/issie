# Team OOP - Draft

## D1 Build (Ana)

The code I have written in SheetBeautifyD1.fs can

* Align all singly-connected components to eliminate wire bends in parallel wires
* Makes sure the new wireing doesn't cause symbol intersect
* Scale custom symbols to reduce wire bends in parallel wires between two custom components 
* Scale custom symbols to reduce wire bends in parallel wires between two NON - custom components
* Ensure wires route ok, symbols do not overlap
  
The code has two main functions:

* `firstPhaseStraightening` - where all singly-connected components are aligned and their wires are streightend. From (Figure A2) to (Figure A1) from the project brief. (without the streightening of the wire between the muxes)
* `secondPhaseStraightening` - where scaling and moving is done to a pair of multiport symbols (works for custom and non-custom) in order to streighten the wires between them. From (Figure A4) to (Figure A5) from the project brief.

## D1 Test (Arnav)

The group contribution can be found [here](src/Renderer/TestDrawBlockD1.fs). Some notable progress has been made:

1. Added deconstructor which takes a sheet and finds the symbols as well as connections among symbols for ease of prototyping. Will be integrated into a menu making test circuit construction considerably easier for the rest of the team.
2. Added constructor which constructs a circuit given a list of symbols and their connections. Can be used in conjunction with the function above by the testers on the team.
3. Added evaluation of beautification metrics. Added assertions for failure, and a type for success. Ensuring D1 does not regress changes by D2 and D3.
4. Added basic circuit that tests basic implementation of the algorithm. The circuit has a flipped select and some non-straight wires connected to singly connected components.
5. Added more complex case based on *muxes* with enough degrees of freedom to also be straightened. Testing for the more advanced algorithm.
6. Added multiple connections from single port circuit.
7. Added one test case from the `sheetOrderFlip` examples. For cross algorithm compatibility and better team cohesion.
8. Added flip and rotate based on symbol label. Added random seed generators useful for random flips and rotates. Useful for generating random circuits for the rest of the team.

## D2 Build (Samuel)

Assigned to D2 `sheetOrderFlip` implementation, which can be found in [SheetBeautifyD2.fs](src/Renderer/DrawBlock/SheetBeautifyD2.fs).

_Helpers_

1. Permutation algorithm completed.

    Helpers to count and find all orientations and permutations of ports were written. Counting functions are useful as we can identify the components which will take too much time by trying all permutations, and either skip them or use the clockface algorithm on them.

2. Clockface algorithm completed, integration pending.

    Clockface algorithm to obtain the optimal port order of a component has been implemented. It has not been integrated into the beautify function as obtaining the correct rendered port order proved to be difficult. Once the rendered port order can be obtained, the clockface algorithm will be very useful in reducing number of permutations to try.

_Main Beautify Function_

1. Starter objective completed.

    Beautify function is able to find all muxes, demuxes, and gates to perform reordering. Symbols with more wires connected to it are operated on first, as this is likely to reduce dependancy for future operations. Muxes and demuxes are first operated on, and then Gates. Clockface algorithm was yet to be implemented.

_Tests_

1. Two tests were written to test the basic `sheetOrderFlip` function.

    These are are `makeOFTestCircuitDemo1` and `makeOFTestCircuitDemo2` in [TestDrawBlock.fs](src/Renderer/TestDrawBlock.fs). A Gen object of type bool was passed into the tests, with the first input being false and the second being true. Inputs specifies whether `sheetOrderFlip` was run. The tests also fails on all cases, which meant we can see the effect of `sheetOrderFlip` going to the next test. The circuit being drawn are as the following:

    * Demo Circuit #1 (`test5`): Given circuit in Figure B1.

    * Demo Circuit #2 (`test6`): A circuit which includes 2x Muxes, 1x Demux, 2x Gates. Used to demonstrate re-ordering of port across multiple components.

## D2 Test (Bharathaan)

For the team phase:

* I created the test circuits and
* The data generator

However, I could not get it to interface with

## D3 Build (Alvi)

Starting with the `sheetWireLabelSymbol` function I crafted, it basically toggles between two views in our D3 beautification process: one with wire labels (`LABEL_MODE`) and the other just the plain wire layout (`WIRE_MODE`). The cool part is it's all controlled by a simple toggle, giving us the flexibility to switch views on the fly.

Right now, it's pretty slick. In `LABEL_MODE`, it zeroes in on those lengthy wires, essentially giving them a cloak of invisibility, but it doesn't stop there. It then assigns labels at both ends of these wires, clearly marking which symbol and port they connect to. It's a neat way to keep things organized and understandable at a glance.

However, I've got plans to refine it further. The goal is to enhance the label placement logic, ensuring labels are positioned where there's ample space, and adjusting their orientation based on the symbol's direction. This way, readability is maximized, no matter the layout. It's a step towards making the tool not just functional but also intuitive and clean.

## D3 Test (Gavin)

The team-phase work is in [TestDrawBlockD3] (src/Renderer/TestDrawBlockD3.fs). I created several helper functions and test circuits:

1. Added a better circuit generation DSL - which takes a list of components and a list of connections to generate circuits that have at least user-specified threshold distance between components. Also used Gen<> to apply small random position changes
to each component.
2. The circuit generation DSL includes another helper function that only takes a list of components and connections are randomly generated (by shuffling components and connecting them pairwise). This implementation offers more non-deterministic testing.
These helper functions will allow the sheetWireLabelSymbol creator to quickly and easily generate tests by providing a simpler, more intuitive DSL.
3. Created a random set of "should work/easy" tests, specifically the example test provided in sheetWireLabelSymbol (Figure C1, C2). Also used Gen<> to apply small random position changes, random rotations and flips to components.
4. Created a random set of "likely to fail/hard" tests. These tests have 2-3 overlapping wires, multiple wires from the same output port to different target ports. Components in these tests were also a user-specified threshold distance apart, meaning
sheetWireLabelSymbol could be applied.
These tests are useful in evaluating how well sheetWireLabelSymbol performs across a range of tests with varying difficulties. All these tests also have user-specified threshold distances between components, this allows the sheetWireLabelSymbol
creator to evaluate the function's performance across different thresholds. The user can also specify a low threshold distance for the tests, and evaluate whether their function detects that threshold distances between components is too low and therefore
doesn't try to beautify them.
5. Added flip and rotate helper functions to SheetBeautifyHelpers as it offers more possible tests.
6. Created a metrics counter that when given a circuit schematic counts: the number of symbol overlaps, number of wires intersecting symbols, number of wire bends. "Should-never-happen" cases i.e. number of symbol overlaps > 0 and number of wire intersecting symbols > 0
are made into an assertion case.
7. Another helper function: collectMetricsOfTests leverages the metrics counter by determining metrics of a sheet before and after applying sheetWireLabelSymbol (id is used as stub function).
These helper functions for metrics allows the sheetWireLabelSymbol creator to numerically evaluate the function's performance.
