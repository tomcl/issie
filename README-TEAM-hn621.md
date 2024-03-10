# Readme - Team-phase Work
## Organisation
File containing team-phase work: TestDrawBlockD2 (module D2Test). This is modified from TestDrawBlock done in Tick3. I added two additional modules Metrics and Displays.
### Metric Module
Contains two helper that calculate the metric performance before and after beautifying (difference for metrics that return integer and percentage difference for metrics that return float)

Main metrics to be used:
- numberOfWireStraightened
- numberOfWireIntersection

Some additional metrics that I defined that might be useful in testing the performance:
- intersectionPerSegment: the relative frequency of intersection occuring with respect to number of segments
- rightAnglePerWire: wire having right angles is a necessary condition for intersection
- countVisibleSegments: we don't want to significantly increase the number of visible segments, can set a threshold that is some scaled number of the original
### Display Module
This module is useful for debugging and testing, as it provides a way for various metrics/circuit information being printed out in an organised format. When running test in "runTestOnSheets", we pass in an extra parameter (list of display function) which can display required information on test fail.
### Test Circuit Builder
I made a few fixed test circuits, and one test circuit with randomly flipping components using random generator.