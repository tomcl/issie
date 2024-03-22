module SheetBeautifyD2

open CommonTypes
open DrawModelType
open Optics
open Operators
open RotateScale
open BusWireSeparate
open SheetUpdateHelpers
open Helpers
open System



////////// Helper functions //////////

/// Optimisation parameters.
type D2ParamsT = {
    NumCrossingsNorm: float;
    VisibleLengthNorm: float;
    RightAnglesNorm: float;
    Rotate90Penalty: float;
    Rotate180Penalty: float;
    k: int;
    ILSIterations: int
}

let D2Params = {
    NumCrossingsNorm = 250_000.0;
    VisibleLengthNorm = 1.0;
    RightAnglesNorm = 15_000.0;
    Rotate90Penalty = 50_000.0;
    Rotate180Penalty = 100_000.0;
    k = 1;
    ILSIterations = 3
}

// TODO consider moving these to SheetBeautifyHelpers (if useful to others?)

/// Replacing a symbol in the sheet with the new symbol.
/// Does not update bounding boxes or wiring.
let replaceSymbol
        (sheet: SheetT.Model)
        (symbol: SymbolT.Symbol)
        : SheetT.Model =
    sheet.Wire.Symbol.Symbols
    |> Map.add symbol.Id symbol
    |> fun symbolMap -> Optic.set SheetT.symbols_ symbolMap sheet

/// Replace a symbol in the sheet with the new symbol, also updating it's bounding box.
/// Does not update wiring.
let replaceSymbolAndBB
        (sheet: SheetT.Model)
        (symbol: SymbolT.Symbol)
        : SheetT.Model =
    Symbol.calcLabelBoundingBox symbol
    |> replaceSymbol sheet
    |> Optic.set SheetT.boundingBoxes_ (Map.add symbol.Id (Symbol.getSymbolBoundingBox symbol) sheet.BoundingBoxes)

// Used for finetuning hyperparameters through testing.
let mutable totalCrossings = 0.
let mutable totalLength = 0.
let mutable totalAngles = 0.
let mutable totalIts = 0.

/// Returns a score for the given sheet, based on how well it satisfies the given
/// beautify criteria e.g. fewer wire crossings, fewer right-angles etc.
/// Lower is better.
let sheetScore
        (sheet: SheetT.Model)
        : float =

    let numCrossingsSq = float (SheetBeautifyHelpers2.numRightAngleSegCrossings sheet) ** 2.0
    let visibleLengthSq = float (SheetBeautifyHelpers2.visibleWireLength sheet) ** 2.0
    let numRightAnglesSq = float (SheetBeautifyHelpers2.numWireRightAngles sheet) ** 2.0
    totalCrossings <- totalCrossings + numCrossingsSq
    totalLength <- totalLength + visibleLengthSq
    totalAngles <- totalAngles + numRightAnglesSq
    totalIts <- totalIts + 1.

    // Square and add metrics to encourage optimisation algorithms to make them all moderately small,
    // as opposed to sacrificing one in order to make the others very small.
    // Same concept as L1 vs L2 regularisation in neural networks (?).
    numCrossingsSq * D2Params.NumCrossingsNorm +
    visibleLengthSq * D2Params.VisibleLengthNorm +
    numRightAnglesSq * D2Params.RightAnglesNorm

let inline (%!) a b = (a % b + b) % b

/// Returns true if the given symbol does not intersect any other symbol in the sheet.
let noSymbolIntersections
        (sheet: SheetT.Model)
        (symbol: SymbolT.Symbol)
        : bool =
    let symbolBox = Symbol.getSymbolBoundingBox symbol
    sheet.BoundingBoxes
    |> Map.exists (fun compId box -> compId <> symbol.Id && BlockHelpers.overlap2DBox box symbolBox)
    |> not

type CircuitBoxT = {
    TopLeft: XYPos;
    BottomRight: XYPos
}

/// Adapted from BusWireRoute, reason: checking within +/- minwireseparation is undesirable as it allows
/// the algorithm to reduce the number of segments within +/- min separation at the cost of introducing
/// new segments that fully intersect a symbol. Therefore, we should be strict in that only segments that
/// fully intersect symbols should be counted.
/// Checks if a wire intersects any symbol.
/// Returns list of bounding boxes of symbols intersected by wire.
let findWireSymbolIntersections (model: BusWireT.Model) (wire: BusWireT.Wire) : BoundingBox list =
    let allSymbolsIntersected =
        model.Symbol.Symbols
        |> Map.values
        |> Seq.toList
        |> List.filter (fun s -> s.Annotation = None)
        |> List.map (fun s -> (s.Component.Type, Symbol.getSymbolBoundingBox s))


    let wireVertices =
        BlockHelpers.segmentsToIssieVertices wire.Segments wire
        |> List.map (fun (x, y, _) -> { X = x; Y = y })

    let indexes = List.init ((List.length wireVertices)-2) (fun i -> i+1)

    let segVertices = List.pairwise wireVertices.[1 .. wireVertices.Length - 2] |> List.zip indexes // do not consider the nubs

    let inputCompId = model.Symbol.Ports.[string wire.InputPort].HostId
    let outputCompId = model.Symbol.Ports.[string wire.OutputPort].HostId

    let componentIsMux (comp:Component) =
        match comp.Type with
        | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 -> true
        | _ -> false

    // this was added to fix MUX SEL port wire rooting bug, it is irrelevant in other cases
    let inputIsSelect =
        let inputSymbol = model.Symbol.Symbols.[ComponentId inputCompId]
        let inputCompInPorts = inputSymbol.Component.InputPorts
        
        componentIsMux inputSymbol.Component && (inputCompInPorts.[List.length inputCompInPorts - 1].Id = string wire.InputPort)

    let inputCompRotation =
        model.Symbol.Symbols.[ComponentId inputCompId].STransform.Rotation

    let outputCompRotation =
        model.Symbol.Symbols.[ComponentId outputCompId].STransform.Rotation

    let isConnectedToSelf = inputCompId = outputCompId


    let boxesIntersectedBySegment (lastSeg:bool) startPos endPos =
        allSymbolsIntersected
        //|> List.map (fun (compType, boundingBox) ->
        //    (
        //        compType,
        //        {
        //            W = boundingBox.W + BusWireRoutingHelpers.Constants.minWireSeparation * 2.
        //            H = boundingBox.H + BusWireRoutingHelpers.Constants.minWireSeparation * 2.
        //            TopLeft =
        //            boundingBox.TopLeft
        //            |> BusWireRoutingHelpers.updatePos BusWireRoutingHelpers.Left_ BusWireRoutingHelpers.Constants.minWireSeparation
        //            |> BusWireRoutingHelpers.updatePos BusWireRoutingHelpers.Up_ BusWireRoutingHelpers.Constants.minWireSeparation
        //        }
        //    ))
        |> List.filter (fun (compType, boundingBox) ->
            // don't check if the final segments of a wire that connects to a MUX SEL port intersect with the MUX bounding box
            match compType, lastSeg with
            | Mux2, true | Mux4, true | Mux8, true | Demux2, true | Demux4, true | Demux8, true -> false
            | _, _ ->
                 match BlockHelpers.segmentIntersectsBoundingBox boundingBox startPos endPos with // do not consider the symbols that the wire is connected to
                 | Some _ -> true // segment intersects bounding box
                 | None -> false // no intersection
        )
        |> List.map (fun (compType, boundingBox) -> boundingBox)


    segVertices
    |> List.collect (fun (i, (startPos, endPos)) -> boxesIntersectedBySegment (i > List.length segVertices - 2 && inputIsSelect) startPos endPos)
    |> List.distinct

/// Returns the number of symbols that are intersected by a wire.
/// A more useful metric than the number of wire segments intersecting a symbol.
let numSymsIntersectedBySeg
        (sheet: SheetT.Model)
        : int =
    sheet.Wire.Wires
    |> Map.toList
    |> List.map (fun (_, wire) -> wire)
    |> List.map (findWireSymbolIntersections sheet.Wire)
    |> (List.collect id >> List.distinct >> List.length)

/// Returns the number of retraced segments in the sheet.
let numRetracedSegs
        (sheet: SheetT.Model)
        : int =
    SheetBeautifyHelpers2.getAllRetracedSegs sheet |> List.length



////////// Algorithm 1: Brute force //////////

/// Takes a symbol and returns the two possible flips of that symbol.
/// Penalty weights are zero.
let generateFlips
        (symbol: SymbolT.Symbol, penalty: float)
        : (SymbolT.Symbol*float) list =
    match symbol.STransform.Rotation with
    | Rotation.Degree0 | Rotation.Degree180 ->
        [symbol, penalty + 0.0;
         SheetBeautifyHelpers2.flipSymbol SymbolT.FlipVertical symbol, penalty + 0.0]
    | _ ->
        [symbol, penalty + 0.0;
         SheetBeautifyHelpers2.flipSymbol SymbolT.FlipHorizontal symbol, penalty + 0.0]

/// Takes a symbol and returns all possible rotations of that symbol,
/// with associated penalty weights.
let generateAllRotations
        (symbol: SymbolT.Symbol, penalty: float)
        : (SymbolT.Symbol*float) list =
    [symbol, penalty + 0.0;
     SheetBeautifyHelpers2.rotateSymbol Degree90 symbol, penalty + D2Params.Rotate90Penalty;
     SheetBeautifyHelpers2.rotateSymbol Degree180 symbol, penalty + D2Params.Rotate180Penalty;
     SheetBeautifyHelpers2.rotateSymbol Degree270 symbol, penalty + D2Params.Rotate90Penalty;
    ]

/// Takes a symbol and returns all possible rotations of that symbol except for 180 degrees,
/// with associated penalty weights.
/// This is mostly for performance reasons - may become necessary in future.
let generateLimitedRotations
        (symbol: SymbolT.Symbol, penalty: float)
        : (SymbolT.Symbol*float) list =
    [symbol, penalty + 0.0;
     SheetBeautifyHelpers2.rotateSymbol Degree90 symbol, penalty + D2Params.Rotate90Penalty;
     SheetBeautifyHelpers2.rotateSymbol Degree270 symbol, penalty + D2Params.Rotate90Penalty;
    ]

/// Takes a MUX and returns the two possible symbols from choosing whether to reverse inputs.
/// Penalty weights are zero.
let generateRevInputs
        (symbol: SymbolT.Symbol, penalty: float)
        : (SymbolT.Symbol*float) list =
    [symbol, penalty + 0.0;
     SheetBeautifyHelpers2.toggleReversedInputs symbol, penalty + 0.0]

/// Contains the result of one path through the search space for brute-force optimisation.
type OptimResult = {
    OptimSheet: SheetT.Model
    Score: float option
}

/// Given a symbol, returns a list of all possible symbol choices considered by D2
/// and their associated penalties.
let getSymbolChoices
        (symbol: SymbolT.Symbol)
        : (SymbolT.Symbol * float) list =

    match symbol.Component.Type with

        // MUXes
        | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 ->
            [symbol, 0.0]
            |> List.collect generateFlips
            |> List.collect generateRevInputs
            |> List.collect generateAllRotations

        // Flip and rotate
        | GateN _ | MergeWires | SplitWire _ ->
            [symbol, 0.0]
            |> List.collect generateFlips
            |> List.collect generateAllRotations

        // Rotate-only (typically single input / single output)
        | Not | Viewer _ | BusCompare1 _ | BusSelection _ | Constant1 _ | NbitSpreader _ ->
            [symbol, 0.0]
            |> List.collect generateAllRotations

        // Flip-only (typically larger units like adders)
        | NbitsAdder _ | NbitsAdderNoCin _ | NbitsAdderNoCout _ | NbitsAdderNoCinCout _
        | NbitsXor _ | NbitsAnd _ | NbitsOr _ | NbitsNot _ | MergeN _ | SplitN _
        | DFF | DFFE | Register _ | RegisterE _ | Counter _ | CounterNoLoad _
        | CounterNoEnable _ | CounterNoEnableLoad _ | AsyncROM1 _ | ROM1 _ | RAM1 _
        | AsyncRAM1 _ ->
            [symbol, 0.0]
            |> List.collect generateFlips

        // Everything else: do not transform (includes legacy components)
        // Non-legacy components shown for clarity
        | Input1 _ | Output _ | Custom _ | _ ->
            [symbol, 0.0]

/// Takes a sheet and a list of symbols to manipulate (flip, reverse inputs and reorder ports),
/// and returns a new sheet with the changed symbols (optimising for the beautify function),
/// and the score of this sheet.
/// compIds acts as a reference and remains constant for each function call.
/// Uses brute force - exponential time complexity. Only use for small groups of symbols.
/// This algorithm will never increase the number of symbols intersected by a segment, or the
/// number of retraced segments.
let rec bruteForceOptimise
        (compIds: ComponentId list)
        (initSymsIntersected: int)
        (initRetraced: int)
        (symbols: SymbolT.Symbol list)
        (sheet: SheetT.Model)
        : OptimResult =

    match symbols with
    | sym::remSyms ->

        getSymbolChoices sym
        |> List.filter (fun (sym, _) -> noSymbolIntersections sheet sym)
           // Immediately reject solutions with overlapping symbols.
        |> List.map (fun (sym, penalty) -> replaceSymbolAndBB sheet sym, penalty)
        |> List.map (fun (sheet, penalty) ->
            bruteForceOptimise compIds initSymsIntersected initRetraced remSyms sheet, penalty)
        |> fun lst ->
            match List.filter (fun (s, _) -> Option.isSome s.Score) lst with
            | [] -> List.head lst
            | l -> List.minBy (fun (s, p) -> Option.get s.Score + p) l
            |> fun (s, _) -> s

    | [] ->

        let newSheet =
            sheet.Wire
            |> reRouteWiresFrom compIds
            |> fun wire -> Optic.set SheetT.wire_ wire sheet
        let newSymsIntersected = numSymsIntersectedBySeg newSheet
        let newRetraced = numRetracedSegs newSheet
        match newSymsIntersected <= initSymsIntersected && newRetraced <= initRetraced with
        | true -> {OptimSheet = newSheet; Score = Some (sheetScore newSheet)}
        | false -> {OptimSheet = newSheet; Score = None}



////////// Algorithm 2: Clock face //////////

/// Returns a list of ports connected to a given port.
let getConnectedPorts
        (model: BusWireT.Model)
        (port: Port)
        : Port list =
    model.Wires
    |> Map.toList
    |> List.map snd
    |> List.collect (fun wire ->
        let (OutputPortId outId) = wire.OutputPort
        let (InputPortId inId) = wire.InputPort
        if port.Id = outId then [model.Symbol.Ports[inId]]
        elif port.Id = inId then [model.Symbol.Ports[outId]]
        else []
    )

/// Get the angle of a vector relative to the +x axis (anticlockwise).
let getVectorAngle
        (v: XYPos)
        : float =
    let delta = {v with Y = (-v.Y)} // Switch to +y = up coord system
    let angle = Math.Atan2 (delta.Y, delta.X)
    angle %! (2.0 * Math.PI)

/// Get the normalised vector difference in position between a symbol and a port.
let getNormPortDiff
        (model: SymbolT.Model)
        (symbol: SymbolT.Symbol)
        (port: Port)
        : XYPos =
    let pos1 = Symbol.getRotatedSymbolCentre symbol
    let pos2 = SheetBeautifyHelpers2.getPortSheetPos model port
    let delta = pos2 - pos1
    let mag = sqrt ((delta.X**2) + (delta.Y**2))
    {X = delta.X/mag; Y = delta.Y/mag}

/// Returns the average angle between a symbol and a list of ports,
/// relative to the +x axis (anticlockwise).
let getAveragePortAngle
        (model: SymbolT.Model)
        (symbol: SymbolT.Symbol)
        (portList: Port list)
        : float =
    portList
    |> List.map (getNormPortDiff model symbol)
    |> List.fold (+) XYPos.zero
    |> getVectorAngle

/// Offsets a port angle depending on which edge it's on to allow the clock face algorithm
/// to be applied.
let offsetPortAngle
        (edge: Edge)
        (angle: float)
        : float =
    match edge with
    | Top -> (angle + Math.PI/2.) %! (2.*Math.PI)
    | Left -> angle
    | Bottom -> (angle - Math.PI/2.) %! (2.*Math.PI)
    | Right -> (angle + Math.PI) %! (2.*Math.PI)

/// Reorder the ports of a custom component along a single edge
/// according to the clock face algorithm.
let reorderCustomPorts
        (model: BusWireT.Model)
        (symbol: SymbolT.Symbol)
        (edge: Edge)
        : Port list =
    SheetBeautifyHelpers2.getOrderedPorts model.Symbol symbol edge
    |> List.map (fun port -> port, getConnectedPorts model port)
    |> List.map (fun (port, lst) -> port, getAveragePortAngle model.Symbol symbol lst)
    |> List.map (fun (port, angle) -> port, offsetPortAngle edge angle)
    |> List.sortBy snd
    |> List.map fst

/// Take a custom component symbol and reorder its ports along each edge
/// according to the clock face algorithm.
let customCompClockFace
        (model: BusWireT.Model)
        (symbol: SymbolT.Symbol)
        : SymbolT.Symbol =
    let edges = [Edge.Left; Edge.Right; Edge.Top; Edge.Bottom]
    let reorderedPorts = List.map (reorderCustomPorts model symbol) edges
    (symbol, edges, reorderedPorts)
    |||> List.fold2 SheetBeautifyHelpers2.setOrderedPorts



////////// Algorithm 3: Circuit partition heuristic //////////

// This algorithm uses breadth first search to obtain partitions of no more than k
// elements each from the circuit. We represent the circuit as a graph where each vertex
// is a component, and each edge is a wire.
// Using breadth-first search leads to some nice properties. For example, if a connected
// component has fewer than k vertices, it is guaranteed to be returned in a single partition.

// Store graph as an adjacency list, using component ids.
type Graph = Map<ComponentId, ComponentId list>

/// Add a component to the visited set and the queue.
let enqueueAndVisit
        (visited: Set<ComponentId>)
        (queue: ComponentId list)
        (comp: ComponentId)
        : Set<ComponentId>*(ComponentId list) =
    match Set.contains comp visited with
    | true -> visited, queue
    | false -> Set.add comp visited, queue @ [comp]

/// Perform a single pass of BFS on the graph, returning the visited components (k or fewer).
let rec bfsKComponents
        (graph: Graph)
        (k: int)
        (visited: Set<ComponentId>)
        (queue: ComponentId list)
        : ComponentId list =
    match queue, Set.count visited < k with
    | [], _ | _, false ->
        Set.toList visited
    | comp::remQueue, true ->
        let visited', queue' =
            ((visited, remQueue), graph[comp])
            ||> List.fold (fun (v, q) -> enqueueAndVisit v q)
        bfsKComponents graph k visited' queue'

/// Removes a node from a graph.
let removeNode
        (graph: Graph)
        (node: ComponentId)
        : Graph =
    Map.remove node graph
    |> Map.map (fun _ lst ->
        List.filter (fun x -> x <> node) lst
    )

/// Partition a graph into subsets of no more than k components each using BFS.
/// Passes of BFS proceed from left to right in the circuit.
/// Clusters that are close together are prioritised (approximately) via DFS.
/// Could improve distance prioritisation via a modification of Dijkstra's algorithm,
/// but probably not worth it.
let rec getGraphPartitions
        (symbolMap: Map<ComponentId, SymbolT.Symbol>)
        (graph: Graph)
        (k: int)
        : ComponentId list list =
    let startNode =
        Map.toList graph
        |> List.map fst
        |> List.minBy (fun compId -> symbolMap[compId].Pos.X)
    let cluster = bfsKComponents graph k (set [startNode]) [startNode]
    let graph' =
        (graph, cluster)
        ||> List.fold removeNode
    match Map.isEmpty graph' with
    | true -> [cluster]
    | false -> getGraphPartitions symbolMap graph' k @ [cluster]

/// A record to assist with the process of building a graph.
type GraphBuilder = {CurrGraph: Graph; DiscoveredEdges: Set<ComponentId*ComponentId>}

/// Returns the updated graph and list of discovered edges after considering a wire.
let updateGraphWithWire
        (graphBuilder: GraphBuilder)
        (model: SymbolT.Model)
        (wire: BusWireT.Wire)
        : GraphBuilder =
    let {CurrGraph = graph; DiscoveredEdges = discoveredEdges} = graphBuilder
    let outputComp =
        let (OutputPortId portId) = wire.OutputPort
        ComponentId model.Ports[portId].HostId
    let inputComp =
        let (InputPortId portId) = wire.InputPort
        ComponentId model.Ports[portId].HostId
    let addEdge comp1 comp2 =
        let list1 = graph[comp1] @ [comp2]
        let list2 = graph[comp2] @ [comp1]
        graph
        |> Map.add comp1 list1
        |> Map.add comp2 list2
    if outputComp = inputComp || Set.contains (outputComp, inputComp) discoveredEdges then
        {CurrGraph = graph; DiscoveredEdges = discoveredEdges}
    else
        {CurrGraph = addEdge outputComp inputComp;
         DiscoveredEdges =
            discoveredEdges
            |> Set.add (outputComp, inputComp)
            |> Set.add (inputComp, outputComp)
        }

/// Returns partitions of circuit into clusters of no more than k components each.
/// Partitions are given as list of component ids.
let partitionCircuit
        (sheet: SheetT.Model)
        : ComponentId list list =
    let initGraph: Graph =
        sheet.Wire.Symbol.Symbols
        |> Map.toList
        |> List.filter (fun (_, sym) -> sym.Annotation = None)
        |> List.map fst
        |> List.map (fun compId -> compId, [])
        |> Map.ofList
    let wires =
        sheet.Wire.Wires
        |> Map.toList
        |> List.map snd
    let {CurrGraph = graph} =
        ({CurrGraph = initGraph; DiscoveredEdges = Set.empty}, wires)
        ||> List.fold (fun graphBuilder wire ->
                        updateGraphWithWire graphBuilder sheet.Wire.Symbol wire)
    getGraphPartitions sheet.Wire.Symbol.Symbols graph D2Params.k



////////// Helpers for top level //////////

/// Beautify a single cluster, returns the sheet with altered symbols.
let beautifyCluster
        (sheet: SheetT.Model)
        (compIds: ComponentId list)
        : SheetT.Model =
    let symbols =
        compIds
        |> List.map (fun compId -> sheet.Wire.Symbol.Symbols[compId])
    // TODO think about the impact of the order (brute force, clock face).
    let initSymsIntersected = numSymsIntersectedBySeg sheet
    let initRetraced = numRetracedSegs sheet
    let sheet' = (bruteForceOptimise compIds initSymsIntersected initRetraced symbols sheet).OptimSheet
    (sheet', symbols)
    ||> List.fold (fun currSheet sym ->
        match sym.Component.Type with
        | Custom _ ->
            customCompClockFace sheet.Wire sym
            |> replaceSymbol sheet
        | _ -> currSheet
    )
    |> fun sheet'' -> {sheet'' with Wire = reRouteWiresFrom compIds sheet''.Wire}

/// Beautify the sheet using ILS.
let rec beautifyILS
        (sheet: SheetT.Model)
        (partitions: ComponentId list list)
        (iterationsLeft: int)
        (initialScore: float)
        : SheetT.Model =
    match iterationsLeft with
    | 0 -> sheet
    | _ ->
        let sheet' =
            (sheet, partitions)
            ||> List.fold beautifyCluster
        let afterScore = sheetScore sheet'
        match abs (initialScore - afterScore) < XYPos.epsilon with
        | true -> sheet'
        | false -> beautifyILS sheet' partitions (iterationsLeft - 1) afterScore

/// Find the box enclosing all the components in the circuit.
/// Use this if we end up aligning input/output label rotation with position on schematic.
let getCircuitBox
        (sheet: SheetT.Model)
        : CircuitBoxT =
    let topLeftList =
        Map.toList sheet.Wire.Symbol.Symbols
        |> List.map (fun (_, sym) -> sym.Pos)
    let bottomRightList =
        Map.toList sheet.Wire.Symbol.Symbols
        |> List.map (fun (_, sym) ->
            let (h, w) = Symbol.getRotatedHAndW sym
            {X = sym.Pos.X + w; Y = sym.Pos.Y + h})
    let {XYPos.X = minX} = List.minBy (fun pos -> pos.X) topLeftList
    let {XYPos.X = maxX} = List.maxBy (fun pos -> pos.X) bottomRightList
    let {XYPos.Y = minY} = List.minBy (fun pos -> pos.Y) topLeftList
    let {XYPos.Y = maxY} = List.maxBy (fun pos -> pos.Y) bottomRightList
    {TopLeft = {X = minX; Y = minY}; BottomRight = {X = maxX; Y = maxY}}



////////// TOP LEVEL //////////

/// Beautify sheet by flipping components, swapping input order of MUX, and reordering ports
/// on custom components.
let sheetOrderFlip
        (sheet: SheetT.Model)
        : SheetT.Model =
    printf "Applying sheetOrderFlip"
    //printf "Clarke: %A" (SheetBeautifyHelpers.numOfIntersectSegSym sheet)
    printf "Roshan: %A" <| List.length (SheetBeautifyHelpers2.getAllRetracedSegs sheet)
    // k is the number of components in each cluster; clusters are individually optimised.
    // Since beautification has exponential time complexity (using brute force), clusters
    // must be kept fairly small.
    let partitions = partitionCircuit sheet
    totalCrossings <- 0
    totalLength <- 0
    totalAngles <- 0
    totalIts <- 0
    let initialScore = sheetScore sheet
    let sheet' = beautifyILS sheet partitions D2Params.ILSIterations initialScore
    printf "Average crossings: %A" (totalCrossings / totalIts)
    printf "Average length: %A" (totalLength / totalIts)
    printf "Average angles: %A" (totalAngles / totalIts)
    sheet'
    //let symbols = sheet.Wire.Symbol.Symbols |> Map.toList |> List.map snd
    //(sheet, symbols)
    //||> List.fold (fun currSheet symbol -> replaceSymbol currSheet (SheetBeautifyHelpers2.flipSymbol SymbolT.FlipVertical symbol))

//let sheetOrderFlip (sheet: SheetT.Model) : SheetT.Model =
//    sheet.Wire.Symbol.Symbols
//    |> Map.toList
//    |> (fun lst -> snd lst[0])
//    |> SheetBeautifyHelpers2.rotateSymbol Degree270
//    |> replaceSymbol sheet
//    |> fun sheet -> {sheet with UndoList = appendUndoList sheet.UndoList sheet}
