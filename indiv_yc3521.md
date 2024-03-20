## Individual Contribution Statement

As a member of group 7, my role was pivotal in implementing functionality within the D1 sheetAlignScale module to optimize the electronic schematic's layout in  ISSIE and small amount of individual function testing.

### Key Contributions (main design of optimisation):

- **Wire Parallelism Detection**: Developed the `isParallel` function to intelligently determine the parallelism of wire segments, serving as the foundation for further wire optimizations.

- **Single Connection Component Analysis**: Created the `findSinglyConnectedComponents` function to identify components with a singular connection of parallel wire, allowing for targeted layout improvements.

- **Simplification Shift Calculation**: Introduced the `calculateShiftForSimplification` function to calculate essential positional shifts and helps the maping of wire id with translation simpler.

- **Alignment Mapping**: Executed the `findSinglyConnectedWiresAndShifts` function to map out parallel wire id to shifts needed to align wires.

- **Component Position Adjustment**: Finalized the component optimization process with the `alignSinglyConnectedComponents` function, which applies the computed shifts to strategically reposition symbols for the whole model.

My contributions were focused on not only the whole implementation but also written some small tests(small test for individual functions using Tick3) to make the testing process of D1 tester at the start without any bugs. I have successfully implemented the singly connected components optimisation and working on beautifying multiple connected components. 

From what I know, the team member that I work more closely with (D1 tester) have implemented some complete circuits for testing such as those on the project PDF. He was also working on producing random generated testing and working towards devloping metrics to quantify the performance of my beautification.

