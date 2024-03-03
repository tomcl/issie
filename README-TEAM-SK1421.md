# sk1421's individual contribution: TestDrawBlockD1

The group contribution can be found [here](src/Renderer/TestDrawBlockD1.fs). Some notable progress has been made:

1. Added deconstructor which takes a sheet and finds the symbols as well as connections among symbols for ease of prototyping. Will be integrated into a menu making test circuit construction considerably easier for the rest of the team.
2. Added constructor which constructs a circuit given a list of symbols and their connections. Can be used in conjunction with the function above by the testers on the team.
3. Added evaluation of beautification metrics. Added assertions for failure, and a type for success. Ensuring D1 does not regress changes by D2 and D3.
4. Added basic circuit that tests basic implementation of the algorithm. The circuit has a flipped select and some non-straight wires connected to singly connected components.
5. Added more complex case based on *muxes* with enough degrees of freedom to also be straightened. Testing for the more advanced algorithm.
6. Added multiple connections from single port circuit.
7. Added one test case from the `sheetOrderFlip` examples. For cross algorithm compatibility and better team cohesion.
8. Added flip and rotate based on symbol label. Added random seed generators useful for random flips and rotates. Useful for generating random circuits for the rest of the team.
