module SheetBeautifyD2

open CommonTypes
open DrawModelType
open Optics
open Operators
open RotateScale
open BusWireSeparate
open SheetUpdateHelpers
open System



////////// Helper functions //////////

/// Optimisation weights.
type D2WeightsT = {
    NumCrossingsNorm: float;
    VisibleLengthNorm: float;
    RightAnglesNorm: float;
    Rotate90Penalty: float;
    Rotate180Penalty: float;
}

// TODO figure out what these weights should be through testing.
let D2Weights = {
    NumCrossingsNorm = 1.0;
    VisibleLengthNorm = 1.0;
    RightAnglesNorm = 1.0;
    Rotate90Penalty = 1.0;
    Rotate180Penalty = 1.0
}

// TODO consider moving these to SheetBeautifyHelpers (if useful to others?)

/// Given a modified symbol and a sheet, find the original symbol in the sheet
/// (by component id) and replace it with the new symbol, returning the new sheet.
/// Does not update bounding boxes or wiring - symbol only!
let replaceSymbol
        (sheet: SheetT.Model)
        (symbol: SymbolT.Symbol)
        : SheetT.Model =
    sheet.Wire.Symbol.Symbols
    |> Map.add symbol.Id symbol
    |> fun symbolMap -> Optic.set SheetT.symbols_ symbolMap sheet

/// Returns a score for the given sheet, based on how well it satisfies the given
/// beautify criteria e.g. fewer wire crossings, fewer right-angles etc.
/// Lower is better.
let sheetScore
        (sheet: SheetT.Model)
        : float =

    let numCrossingsSq = float (SheetBeautifyHelpers2.numRightAngleSegCrossings sheet) ** 2.0
    let visibleLengthSq = float (SheetBeautifyHelpers2.visibleWireLength sheet) ** 2.0
    let numRightAnglesSq = float (SheetBeautifyHelpers2.numWireRightAngles sheet) ** 2.0

    // Square and add metrics to encourage optimisation algorithms to make them all moderately small,
    // as opposed to sacrificing one in order to make the others very small.
    // Same concept as L1 vs L2 regularisation in neural networks (?).
    //numCrossingsSq*numCrossingsNorm + visibleLengthSq*visibleLengthNorm + numRightAnglesSq*numRightAnglesNorm
    numCrossingsSq*D2Weights.NumCrossingsNorm

let inline (%!) a b = (a % b + b) % b



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
let generateRotations
        (symbol: SymbolT.Symbol, penalty: float)
        : (SymbolT.Symbol*float) list =
    [symbol, penalty + 0.0;
     SheetBeautifyHelpers2.rotateSymbol Degree90 symbol, penalty + D2Weights.Rotate90Penalty;
     SheetBeautifyHelpers2.rotateSymbol Degree180 symbol, penalty + D2Weights.Rotate180Penalty;
     SheetBeautifyHelpers2.rotateSymbol Degree270 symbol, penalty + D2Weights.Rotate90Penalty;
    ]

/// Takes a MUX and returns the two possible symbols from choosing whether to reverse inputs.
/// Penalty weights are zero.
let generateRevInputs
        (symbol: SymbolT.Symbol, penalty: float)
        : (SymbolT.Symbol*float) list =
    [symbol, penalty + 0.0;
     SheetBeautifyHelpers2.toggleReversedInputs symbol, penalty + 0.0]

/// Takes a sheet and a list of symbols to manipulate (flip, reverse inputs and reorder ports),
/// and returns a new sheet with the changed symbols (optimising for the beautify function),
/// and the score of this sheet.
/// compIds acts as a reference and remains constant for each function call.
/// Uses brute force - exponential time complexity. Only use for small groups of symbols.
let rec bruteForceOptimise
        (symbols: SymbolT.Symbol list)
        (compIds: ComponentId list)
        (sheet: SheetT.Model)
        : {|OptimisedSheet: SheetT.Model; Score: float|} =

    match symbols with
    | sym::remSyms ->

        let symbolChoices =
            match sym.Component.Type with
            | Not | GateN (_, _) ->
                [sym, 0.0]
                |> List.collect generateFlips
                |> List.collect generateRotations
            | Mux2 | Mux4 | Mux8 ->
                [sym, 0.0]
                |> List.collect generateFlips
                |> List.collect generateRevInputs
                |> List.collect generateRotations
            | _ -> [sym, 0.0]

        symbolChoices
        |> List.map (fun (sym, penalty) -> replaceSymbol sheet sym, penalty)
        |> List.filter (fun (sheet, _) ->
            let sheet' = updateBoundingBoxes sheet
            SheetBeautifyHelpers2.numPairsIntersectingSymbols sheet' = 0
            // Immediately reject solutions with overlapping symbols.
        )
        |> List.map (fun (sheet, penalty) -> bruteForceOptimise remSyms compIds sheet, penalty)
        |> List.minBy (fun (result, penalty) -> result.Score + penalty)
        |> fun (result, _) -> result

    | [] ->

        sheet.Wire
        |> reRouteWiresFrom compIds
        |> fun wire -> Optic.set SheetT.wire_ wire sheet
        |> fun newSheet -> {|OptimisedSheet = newSheet; Score = sheetScore newSheet|}



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
/// TODO could improve distance prioritisation via a modification of Dijkstra's algorithm.
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
        (k: int)
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
    getGraphPartitions sheet.Wire.Symbol.Symbols graph k



////////// Helpers for top level //////////

let beautifyCluster
        (sheet: SheetT.Model)
        (compIds: ComponentId list)
        : SheetT.Model =
    let symbols =
        compIds
        |> List.map (fun compId -> sheet.Wire.Symbol.Symbols[compId])
    // TODO think about the impact of the order (brute force, clock face).
    let sheet' = (bruteForceOptimise symbols compIds sheet).OptimisedSheet
    (sheet', symbols)
    ||> List.fold (fun currSheet sym ->
        match sym.Component.Type with
        | Custom _ ->
            customCompClockFace sheet.Wire sym
            |> replaceSymbol sheet
        | _ -> currSheet
    )
    //|> Optic.set DrawModelType.SheetT.wire_ (reRouteWiresFrom compIds sheet'.Wire)
    // TODO why exactly doesn't this work?
    |> fun sheet'' -> {sheet'' with Wire = reRouteWiresFrom compIds sheet''.Wire}



////////// TOP LEVEL //////////

/// Beautify sheet by flipping components, swapping input order of MUX, and reordering ports
/// on custom components.
let sheetOrderFlip
        (sheet: SheetT.Model)
        : SheetT.Model =
    printf "Applying sheetOrderFlip"
    printf "Clarke: %A" (SheetBeautifyHelpers.numOfWireRightAngleCrossings sheet)
    printf "Roshan: %A" (SheetBeautifyHelpers2.numRightAngleSegCrossings sheet)
    // k is the number of components in each cluster; clusters are individually optimised.
    // Since beautification has exponential time complexity (using brute force), clusters
    // must be kept fairly small.
    let k = 3
    let partitions = partitionCircuit k sheet
    let sheet' =
        (sheet, partitions)
        ||> List.fold beautifyCluster
    {sheet' with UndoList = appendUndoList sheet.UndoList sheet}

//let sheetOrderFlip (sheet: SheetT.Model) : SheetT.Model =
//    sheet.Wire.Symbol.Symbols
//    |> Map.toList
//    |> (fun lst -> snd lst[0])
//    |> SheetBeautifyHelpers2.rotateSymbol Degree270
//    |> replaceSymbol sheet
//    |> fun sheet -> {sheet with UndoList = appendUndoList sheet.UndoList sheet}
