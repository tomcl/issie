# Contribution to team phase work

As the team member assigned to the building part of D2, I have implemented all 3 algorithms (brute force, clock face, and heuristic-based partitioning) for the D2 deliverables, as well as a top-level function using these algorithms. My contribution can be found in src/Renderer/DrawBlock/SheetBeautifyD2.fs.

## Algorithm 1 - brute force

I implemented an algorithm for brute-forcing combinations of flips along component axes (e.g. vertical flip for component rotated 0 or 180 degrees) and MUX input order reversal, to find the best configuration. No rotations because in most cases, users have an idea of the direction of flow of information for their components, which I don't want to change. The algorithm optimises a score, which takes into account the number of wire crossings, total visible wire length, and number of wire right-angles (appropriate normalisation factors still need to be determined). The algorithm could be extended to brute-force order of ports on custom components, but only for small numbers of ports (factorial scaling).

## Algorithm 2 - clock face

I implemented the clock face algorithm to rearrange ports on each side of a custom component. This is a much more viable option for more than a few ports. I chose not to allow the algorithm to move ports to different edges, since there is usually some meaning behind the edge a port is on. Currently, this algorithm only rearranges ports on custom components, but I could extend it to also determine MUX orientations, if using brute force for all MUXes is too costly.

## Algorithm 3 - heuristic partitioning

I implemented a graph partition heuristic using breadth-first search. Details can be found in code comments. Briefly, BFS has some nice properties, such as (generally) choosing proximal clusters, and identifying connected components.

## Top level

Currently, the algorithm splits the circuit into partitions, brute-force optimises logic gates in each partition, uses clock-face on custom components, and accumulates solutions for each partition into a total solution. Some more thought could be given to the order of operations here, and when exactly to update wiring.