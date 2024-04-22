# Helpers in BlockHelpers

## Proposed Additions 
| Function Name       | Use                                                                                      | Input                                          | Output                  | Comments |
|---------------------|------------------------------------------------------------------------------------------|------------------------------------------------|-------------------------|----------|
| `boundingBoxToRect` | Helper to convert a `BoundingBox` to a `Rectangle`. This function is defined separately due to a type name clash between `ScaleAdjustment` and `Orientation` in `CommonTypes`. | `boundingBox: BoundingBox`                    | `Rectangle`             | When `CommonTypes` is fixed and imported back, can phase out and replace with in-built methods for `Bounding Box` and `Rectangle`         |
| `rectToBoundingBox` | Helper to convert a `Rectangle` to a `BoundingBox`. This function is defined separately due to a type name clash between `ScaleAdjustment` and `Orientation` in `CommonTypes`. | `rect: Rectangle`                             | `BoundingBox`           | When `CommonTypes` is fixed and imported back, can phase out and replace with in-built methods for `Bounding Box` and `Rectangle`         |
| `segmentToRect`     | Converts a segment (defined by its start and end points) to a Rectangle. If the segment travels in both directions, an error is raised.                        | `segStart: XYPos`, `segEnd: XYPos`            | `Rectangle`             |          |
| `overlap1DInfo`     | Returns the range of overlap between two 1D ranges if they intersect, else None.           | `(a1, a2): float pair`, `(b1, b2): float pair` | `(float * float) option`|  Designed to pair with `overlap1D` which already exists  |
| `overlap2DInfo`     | Returns the area of the overlap between two rectangles if they intersect, else None.      | `rect1: Rectangle`, `rect2: Rectangle`        | `Rectangle option`      |Designed to pair with `overlap2D` which already exists|
| `overlap2DBoxInfo`  | Returns a bounding box of intersection area between two bounding boxes if they intersect, else None. | `bb1: BoundingBox`, `bb2: BoundingBox`       | `BoundingBox option`    |Designed to pair with `overlap2DBox` which already exists|
| `segmentIntersectsSegment`| Returns true if two 1D line segments intersect in 2D space. | `(a1, a2): XYPos * XYPos`, `(b1, b2): XYPos * XYPos` | `bool` | |
| `segmentIntersectsSegmentInfo`| Returns the rectangle of the overlap between two 1D line segments in 2D space. | `(a1, a2): XYPos * XYPos`, `(b1, b2): XYPos * XYPos` | `Rectangle option` | |
| `segmentIntersectsBoundingBox'` | Returns true if a segment intersects a bounding box using the segment's start and end `XYPos`.  | `box : BoundingBox`, `segStart : XYPos`, `segEnd : XYPos` | `bool` | Will be used to phase out `segmentIntersectsBoundingBox` which already exists. |
| `segmentIntersectsBoundingBoxInfo` | Returns the BoundingBox of the overlap between a segment and a bounding box. | `box : BoundingBox`, `segStart : XYPos`, `segEnd : XYPos` | `BoundingBox option` | |


## Smart Draw Block 'Additions' (from HLP23)

| Function Name                  | Use                                                         | Input                                         | Output          | Comments |
|--------------------------------|-------------------------------------------------------------|-----------------------------------------------|-----------------|----------|
| `updateModelSymbols`           | Updates BusWire model with given symbols, can also add new symbols | `model: BusWireT.Model`, `symbols: Symbol list` | `BusWireT.Model` |          |
| `updateModelWires`             | Updates BusWire model with given wires, can also add new wires | `model: BusWireT.Model`, `wiresToAdd: Wire list` | `BusWireT.Model` |          |

## Symbol Helper Functions
| Function Name                  | Use                                                         | Input                                         | Output          | Comments |
|--------------------------------|-------------------------------------------------------------|-----------------------------------------------|-----------------|----------|
| `moveSymbol`                   | Moves a symbol by a given offset                             | `offset: XYPos`, `symbol: Symbol`           | `Symbol`        |          |
| `moveSymbols`                  | Moves all symbols by a given offset in the model              | `offset: XYPos`, `model: SymbolT.Model`     | `SymbolT.Model` |          |
| `inputPortStr`                 | Convert an `InputPortId` to a string                          | `s: InputPortId`                            | `string`        |          |
| `outputPortStr`                | Convert an `OutputPortId` to a string                         | `s: OutputPortId`                           | `string`        |          |
| `overlap1D`                    | Returns true if two 1D line segments intersect                | `(a1, a2): float * float`, `(b1, b2): float * float` | `bool`          |Add a new function `overlap1DInfo` to return a segment line of intersection| 
| `segmentsToIssieVertices`     | Converts a list of segments to a list of Issie vertices to store inside Connection | `segList: Segment list`, `wire: Wire`      | `(float * float * bool) list` |          |
| `overlap2D`                    | Returns true if two boxes intersect, where each box is passed in as top right and bottom left XYPos tuples | `(a1, a2): XYPos * XYPos`, `(b1, b2): XYPos * XYPos` | `bool`          | Modify to accept a Rectangle type instead. Also add a new function `overlap2DInfo` to return a rectangle of the intersection  |
| `overlap2DBox`                 | Returns true if two boxes intersect, where each box is passed in as BoundingBox | `bb1: BoundingBox`, `bb2: BoundingBox`    | `bool`          |  This is simply a wrapper over `overlap2D`, that accepts Bounding Boxes, converts to tuples of `XYPos`, and feeds into `overlap2D`. Change this so it converts to Rect instead and passes it into the new `overlap2D`. Also create new `overlap2DBoxInfo` that returns a bounding box of the intersection (It converts the output of `overlap2DInfo` from rectangle to boundibg box) |
| `addLengthToPos`               | Returns an XYPos shifted by length in an X or Y direction defined by orientation | `position: XYPos`, `orientation: Orientation`, `length: float` | `XYPos`         |          |
| `switchOrientation`            | Returns the opposite orientation of the input orientation     | `orientation: Orientation`                  | `Orientation`   |          |
| `foldOverSegs`                 | Applies a function which requires the segment start and end positions to the segments in a wire, threading an accumulator argument through the computation | `folder: Function`, `state: Initial state`, `wire: Wire` | `Final state value` |          |
| `foldOverNonZeroSegs`          | Applies a function which requires the segment start and end positions to the non-zero-length segments in a wire, threading an accumulator argument through the computation | `folder: Function`, `state: Initial state`, `wire: Wire` | `Final state value` |          |
| `getAbsSegments`               | Return absolute segment list from a wire                       | `wire: Wire`                                | `ASegment list` |          |
| `getNonZeroAbsSegments`        | Return absolute segment list from a wire, excluding zero-length segments | `wire: Wire`                                | `ASegment list` |          |
| `getFilteredAbsSegments`       | Return filtered absolute segment list from a wire. includeSegment determines whether a given segment is included in the output list | `includeSegment: Function`, `wire: Wire`    | `ASegment list` |          |
| `getWireSegmentsXY`            | Retrieves XYPos of every vertex in a wire                     | `wire: Wire`                                | `List of XYPos` |          |
| `getWiresInBox`                | Retrieves all wires which intersect an arbitrary bounding box & the index of the segment which intersects the box | `box: BoundingBox`, `model: Model`          | `List of tuples (Wire, int)` |          |
| `fixBoundingBox`               | Used to fix bounding box with negative width and heights      | `box: BoundingBox`                          | `BoundingBox`   |          |
| `partitionWiresIntoNets`       | Partitions wires into nets based on their output port         | `model: Model`                              | `List of tuples (OutputPort, List of Wires)` |          |
| `inMiddleOf`                   | Returns true if x lies in the open interval (a,b)             | `a: float`, `x: float`, `b: float`         | `bool`          |          |
| `inMiddleOrEndOf`              | Returns true if a lies in the closed interval (a,b)          | `a: float`, `x: float`, `b: float`         | `bool`          |          |
| `getSourcePort`                | Retrieves the source port of a wire from a model              | `model: Model`, `wire: Wire`                | `Port`          |          |
| `getTargetPort`                | Retrieves the target port of a wire from a model              | `model: Model`, `wire: Wire`                | `Port`          |          |
| `getSourceSymbol`              | Retrieves the source symbol of a wire from a model           | `model: Model`, `wire: Wire`                | `Symbol`        |          |
| `getTargetSymbol`              | Retrieves the target symbol of a wire from a model           | `model: Model`, `wire: Wire`                | `Symbol`        |          |
| `moveWire`                     | Moves a wire by the XY amounts specified by displacement     | `wire: Wire`, `displacement: XYPos`         | `Wire`          |          |
| `moveWires`                    | Moves all wires in a model by the XY amounts specified by offset | `offset: XYPos`, `model: Model`            | `Model`         |          |

## Helpers to get Ports and Locations
| Function Name                  | Use                                                         | Input                                         | Output          | Comments |
|--------------------------------|-------------------------------------------------------------|-----------------------------------------------|-----------------|----------|
| `getSymbolPos`                 | Returns the center coordinates of a Symbol                    | `symbolModel: SymbolT.Model`, `compId: ComponentId` | `XYPos`         |          |
| `getCopiedSymbols`             | Interface function to get componentIds of the copied symbols | `symModel: SymbolT.Model`                   | `List of ComponentId` |          |
| `getPort`                      | Returns the port object associated with a given portId       | `symModel: SymbolT.Model`, `portId: string` | `Port`          |          |
| `getSymbol`                    | Returns the symbol associated with a given portId             | `model: SymbolT.Model`, `portId: string`   | `Symbol`        |          |
| `getCompId`                    | Returns the component id associated with a given portId       | `model: SymbolT.Model`, `portId: string`   | `ComponentId`   |          |
| `getPortIdStr`                 | Returns the string of a PortId                                | `portId: PortId`                           | `string`        |          |
| `getInputPortIdStr`            | Returns the string of an InputPortId                          | `portId: InputPortId`                      | `string`        |          |
| `getOutputPortIdStr`           | Returns the string of an OutputPortId                         | `portId: OutputPortId`                     | `string`        |          |
| `getPortOrientationFrmPortIdStr` | Returns the orientation of a port from a portId string     | `model: SymbolT.Model`, `portIdStr: string` | `Edge`          |          |
| `getPortOrientation`           | Returns what side of the symbol the port is on               | `model: SymbolT.Model`, `portId: PortId`   | `Edge`          |          |
| `getInputPortOrientation`      | Returns the orientation of an input port                      | `model: SymbolT.Model`, `portId: InputPortId` | `Edge`       |          |
| `getOutputPortOrientation`     | Returns the orientation of an output port                     | `model: SymbolT.Model`, `portId: OutputPortId` | `Edge`      |          |

## Miscellaneous 1-Off Helper Functions, should be put into use modules
| Function Name                  | Use                                                         | Input                                         | Output          | Comments |
|--------------------------------|-------------------------------------------------------------|-----------------------------------------------|-----------------|----------|
| `getStartAndEndWirePos`        | Returns the start and end positions of a wire                 | `wire: Wire`                                | `XYPos * XYPos` |          |
| `getWireLength`                | Returns length of wire                                       | `wire: Wire`                                | `float`         |          |
| `totalLengthOfWires`           | Gets total length of a set of wires                          | `conns: Map<ConnectionId, Wire>`           | `float`         |          |
| `isWireInNet`                  | Checks if a wire is part of a net                            | `model: Model`, `wire: Wire`                | `(OutputPortId * (ConnectionId * Wire) list) option` |          |
| `isPortInSymbol`               | Checks if a port is part of a Symbol                         | `portId: string`, `symbol: Symbol`          | `bool`          |          |
| `getConnSyms`                  | Get pairs of unique symbols that are connected to each other | `wModel: BusWireT.Model`                    | `List of pairs of Symbols` |          |
| `isConnBtwnSyms`               | Checks if wire is connected to two given symbols             | `wire: Wire`, `symA: Symbol`, `symB: Symbol` | `bool`       |          |
| `connsBtwnSyms`                | Gets connections between symbols                              | `wModel: BusWireT.Model`, `symA: Symbol`, `symB: Symbol` | `Map<ConnectionId, Wire>` |          |
| `wiresBtwnSyms`                | Gets Wires between symbols                                   | `wModel: BusWireT.Model`, `symA: Symbol`, `symB: Symbol` | `List of Wire` |          |
| `filterPortBySym`              | Filters Ports by Symbol                                      | `ports: Port list`, `sym: Symbol`          | `List of Ports` |          |
| `portsOfWires`                 | Gets Ports From a List of Wires                              | `model: BusWireT.Model`, `wires: Wire list` | `List of Ports` |          |
| `groupWiresByNet`              | Groups Wires by the net they belong to                       | `conns: Map<ConnectionId, Wire>`           | `List of List of Wire` |          |
| `setCustomCompHW`              | Scales a symbol so it has the provided height and width      | `h: float`, `w: float`, `sym: Symbol`      | `Symbol`        |          |
| `wireSymEdge`                  | For a wire and a symbol, return the edge of the symbol that the wire is connected to | `wModel: BusWireT.Model`, `wire: Wire`, `sym: Symbol` | `Edge`    |          |

## segmentIntersectsBoundingBox
| Function Name                  | Use                                                         | Input                                         | Output          | Comments |
|--------------------------------|-------------------------------------------------------------|-----------------------------------------------|-----------------|----------|
| `Rectangle`                    | Type used to simplify BoundingBox intersection calculations. It has two fields: `TopLeft` and `BottomRight`, both of type `XYPos`. |                                               |                 |          |
| `toX`                          | Returns the X-value of an `XYPos`                             | `pos: XYPos`                                | `float`         |          |
| `toY`                          | Returns the Y-value of an `XYPos`                             | `pos: XYPos`                                | `float`         |          |
| `getXY`                        | Returns the X and Y fields of an `XYPos` as a pair of floats  | `pos: XYPos`                                | `float pair`    |          |
| `scalePos`                     | Returns pos with the X and Y fields scaled by factor         | `factor: float`, `pos: XYPos`              | `XYPos`         |          |
| `lThanEqualPos`                | Returns true if p1 is less than or equal to p2                | `p1: XYPos`, `p2: XYPos`                   | `bool`          |          |
| `dotProduct`                   | Returns the dot product of 2 `XYPos`                          | `p1: XYPos`, `p2: XYPos`                   | `float`         |          |
| `squaredDistance`              | Returns the squared distance between 2 points using Pythagoras | `p1: XYPos`, `p2: XYPos`                   | `float`         |          |
| `rectanglesIntersect`          | Checks if 2 rectangles intersect                             | `rect1: Rectangle`, `rect2: Rectangle`     | `bool`          |          |
| `findPerpendicularDistance`    | Finds the perpendicular distance from a point to a line segment | `segStart: XYPos`, `segEnd: XYPos`, `point: XYPos` | `float`   |          |
| `segmentIntersectsBoundingBox` | Checks if a segment intersects a bounding box using the segment's start and end `XYPos` | `box: BoundingBox`, `segStart: XYPos`, `segEnd: XYPos` | `Option of float` | |







