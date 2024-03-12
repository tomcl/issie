module SheetBeautifyD1

open DrawModelType
open CommonTypes
open DrawModelType.SymbolT
open DrawModelType.SheetT
open DrawModelType.BusWireT
open ModelType
open Optics
open Optics.Operators
open Helpers
open SymbolHelpers
open SheetBeautifyHelpers
open BlockHelpers
open Symbol
open BusWireRoute
open BusWire
open BusWireUpdateHelpers

// --------------------------------------------------- //
//                         INFO!                       //
// --------------------------------------------------- //
// Some of these functions are duplicates of BeautifySheetHelpers since they are used B1-8 and T1-6
// They have been included here for assessment purposes since I developed them to be useful in other parts of the project

///<summary>
/// T2R, T3R Helper, but can be used to minimise invisible segments for D1.
/// Remove all invisible segments from wires on a sheet.Wire.Wires.
/// </summary>
/// <param name="wires">Map of wires indexed by ConnectionID to remove invisible segments from.</param>
/// <returns>Map of wires indexed by ConnectionID with invisible segments removed.</returns>
// visibleSegments would've worked, but outputs an XYPos list, which is a format that isn't well accepted by the other functions and types.
// This is achieved by utilising existing helper function segmentsToIssieVertices to convert all segments to a list of vertices.
// It is then very easy to remove duplicate vertices.
// We can utilise another helper function issieVerticesToSegments to convert vertices back to segments, and create new wires.
let removeWireInvisibleSegments' (wires: Map<ConnectionId, Wire>) =
    wires
    |> Map.map (fun connId wire ->
        let uniqueVertices =
            segmentsToIssieVertices wire.Segments wire
            |> List.distinctBy (fun (x, y, _) -> (x, y))
        // segmentsToIssieVertices returns <float * float * bool> list
        // get rid of duplicate vertices sharing the same float values
        // later, we convert uniqueVertices back to segments

        let newSegments = issieVerticesToSegments connId uniqueVertices
        // for each wire, set the segments to the new segments
        wire |> Optic.set segments_ newSegments)

/// <summary>
/// T3R helper, but useful for calculations of wire crossings:
/// Returns true if two 1D line segments intersect at a 90º angle. Takes in two segments described as point-to-point.
/// A variant of overlap2D in BlockHelpers.fs
/// </summary>
/// <param name="a1">The first segment.</param>
/// <param name="a2">The second segment.</param>
let perpendicularOverlap2D' ((a1, a2): XYPos * XYPos) ((b1, b2): XYPos * XYPos) : bool =
    let overlapX = overlap1D (a1.X, a2.X) (b1.X, b2.X)
    let overlapY = overlap1D (a1.Y, a2.Y) (b1.Y, b2.Y)
    (overlapX || overlapY)
    && not (overlapX && overlapY)

(*

//-----------Notes for D1 From Ed-----------//
----------Dealing With Separation---------

Typically one problem humans find when trying to tidy up schematics is that there is not enough room in some parts of the sheet,
with components too close,  and everything needs to be rearranged.  Doing this automatically is problematic because changing relative
positions of components (except for a small amount) is multiply constrained.

Still there is one easy transform in line with "do no harm" which is to scale the whole sheet - already done by rotatescale if you select all components.

What might require scaling the sheet - uniformly magnifying the distances between all components?
If the "correct" positions of components to straighten connections cause symbol overlaps

If the separation between (parallel) wire segments and component edges, or segments and segments, is too small.
The wire separation algorithm should ensure that component-wire separation will be equally a problem is wire-wire separation is a problem -
so you probably need to consider only that. Scaling the whole sheet can make it difficult to see everything - if too much scaling is done -
so some sort of cap would maybe be necessary.

----------Dealing With Separation---------
2 quality metrics: no overlapping symbols, try to straighten wires as much as possible

The no overlapping symbols requirement is an extreme form of an enough separation requirement where for two adjacent parallel
symbol edges separated by distance d with n wires running through. The gap between them the separation  is:

d/(n+1)

Separations close to a defined minimum can be included as penalties in the objective function. Obviously separations below some
minimum value should not allowed, so a nonlinear dependence of objective function on separation could make sense and allow optimisation
techniques to be used.

*)

/// Constant that decides if a wire is classified as almost-straight, if its longest segment in the minority direction is shorter than this length
let maxDeviationLengthThreshold = 30.0
/// Constant that decides if a wire is classified as almost-straight, if its overall displacement in the minority direction is shorter than this length
let maxMinorityDisplacementThreshold = 300
// initially wanted to use a ratio, but then it would not help fix shorter wires

/// Helper function to split a list of segments into odd and even segments
let unzipIntoOddAndEvenSegments (segments: Segment list) =
    segments
    |> List.mapi (fun i x -> (i % 2 = 0, x))
    |> List.partition fst
    |> fun (odd, even) -> (List.map snd odd, List.map snd even)

/// Function to flip the orientation of a wire
// should be integrated into Issie with the not operator
let flipOrientation (orientation: Orientation) =
    match orientation with
    | Horizontal -> Vertical
    | Vertical -> Horizontal

/// Function that detects if a wire is almost straight
let checkAlmostStraightWire (wire: BusWireT.Wire) =
    // Get list of even segments and odd segments of the wire. Note: we get rid of invisible segments
    let wireWithInvisSegmentsRemoved = (removeSingleWireInvisibleSegments wire)
    let oddList, evenList =
        wire
        |> removeSingleWireInvisibleSegments
        |> (fun wire -> unzipIntoOddAndEvenSegments wire.Segments)
    let oddDisplacement =
        oddList
        |> List.sumBy (fun segment -> segment.Length)
    let evenDisplacement =
        evenList
        |> List.sumBy (fun segment -> segment.Length)

    let majorityDisplacement, isMajorityDirectionHoriz =
        if oddDisplacement >= evenDisplacement then
            oddDisplacement, wire.InitialOrientation = Horizontal
        // if odd segments majority and wire starts from horiz (which is odd), then horiz is majority
        else
            evenDisplacement, wire.InitialOrientation = Vertical
    // if even segments majority and wire starts from vert (which is odd), then horiz is majority

    // can't be straightened if there are less than 2 segments OR segment length is even
    if
        (wireWithInvisSegmentsRemoved.Segments.Length < 2)
        || (wireWithInvisSegmentsRemoved.Segments.Length % 2 = 0)
    then
        false
    else
        // maxDeviationLength is the longest segment in the minority direction
        // maxMinorityDisplacement is the overall displacement in the minority direction
        match wire.InitialOrientation, isMajorityDirectionHoriz with
        | Horizontal, true -> // first seg horiz, majority horiz, will deviate vertically, which will be the even segments
            let maxDeviationLength =
                (evenList
                 |> List.maxBy (fun segment -> abs (segment.Length)))
                    .Length
            // let ratio = (abs (oddDisplacement) / abs (maxDeviationLength))
            // printf "Ratio: %A" ratio
            // ratio > straightenRatioTolerance
            // || (maxDeviationLength < 30 && oddDisplacement < 100)
            maxDeviationLength < maxDeviationLengthThreshold
            && evenDisplacement < maxDeviationLengthThreshold
        | Vertical, false -> // first seg vertical, majority vertical, will deviate horizontally, which will be the even segments
            let maxDeviationLength =
                (evenList
                 |> List.maxBy (fun segment -> abs (segment.Length)))
                    .Length
            // let ratio = (abs (oddDisplacement) / abs (maxDeviationLength))
            // printf "Ratio: %A" ratio
            // ratio > straightenRatioTolerance
            // || (maxDeviationLength < 30 && oddDisplacement < 100)
            maxDeviationLength < maxDeviationLengthThreshold
            && evenDisplacement < maxDeviationLengthThreshold
        | _, _ -> false

/// To show on DeveloperModeView SheetStats
let countAlmostStraightWiresOnSheet (sheetModel: SheetT.Model) =
    let straightWires =
        sheetModel.Wire.Wires
        |> Map.filter (fun _ wire -> checkAlmostStraightWire wire)
    straightWires.Count

(*  Cases for checkAlmostStraightWire
                                                     __________            __________
            ______                                  |         | No        |         |
    _______|     |________ almost straight          |         |           |         |
                                                                 _________|         |__________
                    __________________                          |                             | No
    _______________|      almost straight

    make sure it won't detect edge cases, such as a tall user-generated stepladder wire that moves upwards in small increments.
    this can be done by checking the length of the list of minority segments

    |               |
    |               |                                                               _________
    |               ____                     ____                                           |
    |_                 |                         |  No            __________                |
      |                |   almost straight       |                         |                |
      |                |                         |                         | No             |___________No
      |            ––––                      ––––
      |            |
      |            |
almost straight

    algo: check whether the wire travels furthest horizontally or vertically. This is the called the majority direction,
    and the other direction is the minority direction.

    * Not to be confused with the number of segments, e.g. there can be more vertical segments than horizontal segments, but if
    the horizontal segments cover a greater displacement, the majority direction is horizontal.

    Next, calculate the majority displacement, which is the sum of the segment lengths in the majority direction.

    After determining majority direction, ake sure the first and last segments are also travelling
    in the majority direction. Can be done by checking initialOrientation, and then making sure the length is odd.
    (in odd length, first and last segments will be in majority direction)
    If even, discard, can't straighten a wire that is 'diagonal/L' shaped.

    Then, check for the maximum deviation in the minority direction. If the deviation-to-majority-displacement
    ratio is less than a certain threshold, then the wire is almost straight.

*)

/// Helper that to get a Symbol from PortId. Will search the sheet
// Quite surprising this wasn't created already!
let getSymbolFromPortID (portId: string) (model: SheetT.Model) =
    // PortId type is either an InputPortId or an OutputPortId
    let portIdString = portId.ToString()
    model.Wire.Symbol.Symbols
    |> Map.values
    |> Seq.toList
    |> List.tryFind (fun symbol ->
        symbol.PortMaps.Orientation.Keys
        |> Seq.exists (fun key -> key = portIdString))

/// Function helper that checks if a symbol is of a component type that has a single port
let checkIfSinglePortComponent (symbol: Symbol) =
    match symbol.Component.Type with
    | Input1 _
    | Input _ // legacy type, still used?
    | Output _
    | Viewer _
    | Constant1 _
    | NotConnected -> true
    | _ -> false

/// Function helper that checks if a port is of a net that has a single wire connected to it
let checkSingleNet (portId: OutputPortId) (sheetModel: SheetT.Model) : bool =
    //model.Wire.Symbol.OutputPortsConnected[portId] = 1
    // use TryFind
    // printf "symModel.OutputPortsConnected: %A" symModel.OutputPortsConnected
    // match Map.tryFind portId symModel.OutputPortsConnected with
    // | Some 1 -> true
    // | _ -> false

    let nets = partitionWiresIntoNets sheetModel.Wire

    nets
    |> List.tryFind (fun (outputPortID, netlist) -> portId = outputPortID)
    |> Option.map (fun (outputPortID, netlist) -> netlist.Length = 1)
    |> Option.defaultValue false

/// Function helper that checks if the wire is connected to at least 1 symbol that has a single port.
/// Symbols in question: Inputs, Outputs, Viewers, Constants, WireLabels, Not Connected
let checkIfSingularlyConnected (wire: Wire) (model: SheetT.Model) =
    let inputSymbol = getSymbolFromPortID (wire.InputPort.ToString()) model
    let outputSymbol = getSymbolFromPortID (wire.OutputPort.ToString()) model

    // if either Symbol.Component.ComponentType is of Input1, Output, Viewer, Constant1, NotConnected
    // then the wire is singly connected
    match inputSymbol, outputSymbol with
    | Some inputSymbol, Some outputSymbol ->
        // printf "cond 1: %A" (checkIfSinglePortComponent inputSymbol
        if
            checkIfSinglePortComponent inputSymbol
            || (checkIfSinglePortComponent outputSymbol
                && (checkSingleNet wire.OutputPort model))
        then
            true
        else
            false
    | Some inputSymbol, _ -> checkIfSinglePortComponent inputSymbol
    | _, Some outputSymbol ->
        (checkIfSinglePortComponent outputSymbol
         && (checkSingleNet wire.OutputPort model))
    | _ -> false
(* algo steps

get all wires in the model
get the two symbols that the wire is connected to by InputPortId and OutputPortId

draft:
- To ensure InputPortId is singly connected: get the symbol that the symbol port is connected to.
    - Assume that InputPorts only have a single wire. Then check if the symbol with this inputport
     is singly connected. Do this by looking up the symbol's other portid in the SymbolT.model's
     InputPortsConnected and OutputPortsConnected. If they do not exist then the wire links to a symbol's
     input that is singly connected.
- To ensure OutputPortId is singly connected:
    - use isWireInNet in BlockHelpers to check if the net has more than one wire. If so, output port wire is
    connected to is not singly connected. Can skip the other conditions
    - If the net is singular, then check the symbol of the output port– make sure all its ports are not connected anywhere
    else. Check the portids in the SymbolT.model's InputPortsConnected and OutputPortsConnected, same as above

shortcut:
- Note: these cases above apply to partially-done circuits where not all ports are being utilised by wires in the sheet!
- This will help with beautifying a partially-done sheet. Otherwise, it's much faster immediately check symbols that
   only have one port. Symbols in question: Inputs, Outputs, Viewers, Constants, WireLabels, Not Connected
- After all, completed sheets will have all ports connected to wires (or will have used Not Connected Stopper )
*)
/// Helper to find if a wire is singly connected to symbols that have a single port, and returns the symbols in a list.
/// Note that list can be empty
let findSinglyConnectedSymsByWire (wire: Wire) (sheetModel: SheetT.Model) =
    // will return (symbol, true) if the symbol is an input, (symbol, false) if the symbol is an output
    // it is important to distingush between the two, because when correcting the wire bend, inputs are
    // moved in opposite direction to outputs
    let inputSymbol = getSymbolFromPortID (wire.InputPort.ToString()) sheetModel
    let outputSymbol = getSymbolFromPortID (wire.OutputPort.ToString()) sheetModel

    // if either Symbol.Component.ComponentType is of Input1, Output, Viewer, Constant1, NotConnected
    // then the wire is singly connected
    match inputSymbol, outputSymbol with
    | Some inputSymbol, Some outputSymbol ->
        match
            checkIfSinglePortComponent inputSymbol,
            (checkIfSinglePortComponent outputSymbol
             && (checkSingleNet wire.OutputPort sheetModel))
        with
        | true, true -> [ (inputSymbol, true); (outputSymbol, false) ]
        | true, false -> [ (inputSymbol, true) ]
        | false, true -> [ (outputSymbol, false) ]
        | _ -> []
    | Some inputSymbol, _ ->
        if (checkIfSinglePortComponent inputSymbol) then
            [ (inputSymbol, true) ]
        else
            []
    | _, Some outputSymbol ->
        if
            (checkIfSinglePortComponent outputSymbol
             && (checkSingleNet wire.OutputPort sheetModel))
        then
            [ (outputSymbol, false) ]
        else
            []
    | _ -> []

/// Test function to show on Sheet Stats' developer mode
let countSinglyConnectedWires (model: SheetT.Model) =
    model.Wire.Wires
    |> Map.filter (fun _ wire -> checkIfSingularlyConnected wire model)
    |> Map.count

/// Function to find the minority displacement of wire (the displacment in the direction that not travelled the furthest by the wire)
let getMinorityWireDisplacementAndOrientation (wire: Wire) =

    let wireWithInvisSegmentsRemoved = (removeSingleWireInvisibleSegments wire)
    let oddList, evenList =
        wire
        |> removeSingleWireInvisibleSegments
        |> (fun wire -> unzipIntoOddAndEvenSegments wire.Segments)
    let oddDisplacement =
        oddList
        |> List.sumBy (fun segment -> segment.Length)
    let evenDisplacement =
        evenList
        |> List.sumBy (fun segment -> segment.Length)

    let minorityDisplacement, MinorityDirection =
        if oddDisplacement >= evenDisplacement then
            evenDisplacement, flipOrientation wire.InitialOrientation
        // if odd segments majority and wire starts from horiz (which is odd), then horiz is majority, vert is minority
        else
            oddDisplacement, wire.InitialOrientation
    // if even segments majority and wire starts from vert (which is odd), then horiz is majority, vert is minority

    minorityDisplacement, MinorityDirection

/// Function to find the majority displacement of wire (the displacment in the direction that travelled the furthest by the wire)
let getMajorityDisplacementWireAndOrientation (wire: Wire) =
    let wireWithInvisSegmentsRemoved = (removeSingleWireInvisibleSegments wire)
    let oddList, evenList =
        wire
        |> removeSingleWireInvisibleSegments
        |> (fun wire -> unzipIntoOddAndEvenSegments wire.Segments)
    let oddDisplacement =
        oddList
        |> List.sumBy (fun segment -> segment.Length)
    let evenDisplacement =
        evenList
        |> List.sumBy (fun segment -> segment.Length)

    let majorityDisplacement, MajorityDirection =
        if oddDisplacement >= evenDisplacement then
            oddDisplacement, wire.InitialOrientation
        // if odd segments majority and wire starts from horiz (which is odd), then horiz is majority
        else
            evenDisplacement, flipOrientation wire.InitialOrientation
    // if even segments majority and wire starts from vert (which is odd), then horiz is majority

    majorityDisplacement, MajorityDirection

/// optic to access the initial orientation of a wire
let initialOrientation_: Lens<Wire, Orientation> =
    Lens.create (fun m -> m.InitialOrientation) (fun s m -> { m with InitialOrientation = s })

/// optic to access the StartPos of a wire
let startpos_: Lens<Wire, XYPos> =
    Lens.create (fun m -> m.StartPos) (fun s m -> { m with StartPos = s })

/// Function to manually route a wire from a (recently-moved) singly connected symbol
let rerouteStraightWire (wire: Wire) (symbol: Symbol) (offset: XYPos) =
    let majorityDisplacement, MajorityDirection =
        (getMajorityDisplacementWireAndOrientation wire)

    wire
    |> Optic.set
        (segments_)
        (wire.Segments // make a wire that consists of one big segment in the majority direction, then make nubs
         |> List.mapi (fun i segment ->
             match MajorityDirection, i = 0, i = 1 with
             | Horizontal, true, false -> Some { segment with Length = majorityDisplacement }
             | Vertical, false, true -> Some { segment with Length = majorityDisplacement }
             | _, _, _ -> None)
         |> List.choose id
         |> makeEndsDraggable)
    |> Optic.set initialOrientation_ MajorityDirection
    |> Optic.set startpos_ (wire.StartPos + offset)

/// Function to clean up almost straight singly connected wires
let cleanUpAlmostsStraightSinglyConnWires (model: ModelType.Model) =
    // check if wire is singly connected
    // then check if it is almost straight

    let almostStraightSinglyConnectedWires =
        model.Sheet.Wire.Wires
        |> Map.filter (fun _ wire ->
            checkIfSingularlyConnected wire model.Sheet
            && checkAlmostStraightWire wire)

    let symbolsWireOffsetUpdates =
        almostStraightSinglyConnectedWires
        |> Map.values
        |> Seq.toList
        |> List.collect (fun wire -> // should I collect, or should I map and choose id
            let (symbolsToMove: list<Symbol * bool>) =
                (findSinglyConnectedSymsByWire wire model.Sheet)
            // will be of length 0, 1, or 2. We just take the first value since moving
            // symbolsToMove consist of tuples (Symbol, bool) where bool is true if the symbol is an input
            if symbolsToMove.Length = 0 then
                []
            else
                match getMinorityWireDisplacementAndOrientation wire with // if deviating vertically, offset y. Negative if input and positive if output
                | y, Vertical ->
                    let offset =
                        { X = 0.0
                          Y =
                            (if symbolsToMove[0] |> snd then
                                 -y
                             else
                                 y) }
                    let newSymbol = symbolsToMove[0] |> fst |> moveSymbol offset
                    [ (newSymbol, wire, offset) ]
                | x, Horizontal -> // if deviating horizontally, offset x. Negative if input and positive if output
                    let offset =
                        { X =
                            (if symbolsToMove[0] |> snd then
                                 -x
                             else
                                 x)
                          Y = 0.0 }
                    let newSymbol = symbolsToMove[0] |> fst |> moveSymbol offset
                    [ (newSymbol, wire, offset) ])

    let updatedBusWireTModel: BusWireT.Model =
        symbolsWireOffsetUpdates
        |> List.fold // better way to do this?
            (fun currentModel (symbolToMove, wire, offset) ->
                // create a new straight wire
                let newStraightWire = (rerouteStraightWire wire symbolToMove offset)

                // check for intersections. If there are none, then update the wire and symbol
                if
                    (findWireSymbolIntersections currentModel wire)
                    |> List.isEmpty
                then
                    currentModel
                    |> Optic.set (symbolOf_ symbolToMove.Id) symbolToMove
                    |> Optic.set
                        (wires_)
                        (currentModel.Wires
                         |> Map.add wire.WId newStraightWire)
                else
                    currentModel)
            // Note: initially tried to use the below:
            // BusWireSeparate.routeAndSeparateSymbolWires model' symbolToMove.Id // somehow causes artefacts.
            // Better to just replace a wire with a manually drawn straight one
            // BusWireSeparate.reRouteWiresFrom [ symbolToMove.Id ] model' also doesn't work
            model.Sheet.Wire

    model
    |> Optic.set (sheet_ >-> wire_) (updatedBusWireTModel)

(*  More notes: Cases for detecting Straightenable Wires (note we consider more cases than AlmostStraightWires )
                                                         __________
            ______                                      |         | probably not, but is an edge case where ports are vertical (only for custom comps),
    _______|     |________ can be straightened          |         | plus the first and last segment travel in the minority direction.

                                                          ________________
                                                         |               |
                                                         |               |
                                                _________|               |__________
                                                |                                   |
                                                possible!
                                                This is the case where the initial segments are in minority direction and that its final
                                                minority displacement is small.

                                                draft algorithm:

    |
    |
    ____                                     ____
        |                                        |  probably not,           __________            __________
        |   can be straightened                  |                                   |                     |
        |   uncommon case with vertical          |                                   | impossible          |          _______
    ––––    ports of custom comps            ––––                                                          |__________|
    |
    |                                                                                                       possible!




*)
