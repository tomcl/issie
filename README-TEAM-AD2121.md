The code I have written in SheetBeautifyD1.fs can 
* Align all singly-connected components to eliminate wire bends in parallel wires
* Makes sure the new wireing doesn't cause symbol intersect
* Scale custom symbols to reduce wire bends in parallel wires between two custom components 
* Scale custom symbols to reduce wire bends in parallel wires between two NON - custom components
* Ensure wires route ok, symbols do not overlap
  
The code has two main functions:
* `firstPhaseStraightening` - where all singly-connected components are aligned and their wires are streightend. From (Figure A2) to (Figure A1) from the project brief. (without the streightening of the wire between the muxes)
* `secondPhaseStraightening` - where scaling and moving is done to a pair of multiport symbols (works for custom and non-custom) in order to streighten the wires between them. From (Figure A4) to (Figure A5) from the project brief.
