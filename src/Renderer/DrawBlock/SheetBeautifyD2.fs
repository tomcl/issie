module SheetBeautifyD2

open BlockHelpers
open CommonTypes
open SymbolHelpers
open SymbolUpdate
open DrawModelType
open Optics
open Operators
open Symbol
open RotateScale
open Helpers
open BusWireSeparate
open SheetBeautifyHelpers
open SheetUpdateHelpers



////////// Helper functions //////////

/// Given a modified symbol and a sheet, find the original symbol in the sheet
/// (by component id) and replace it with the new symbol, returning the new sheet.
/// Does not update bounding boxes or wiring - symbol only!
let replaceSymbol
        (symbol: SymbolT.Symbol)
        (sheet: SheetT.Model)
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

    // Normalisation factors for each of the three components.
    let numCrossingsNorm = 1.0
    let visibleLengthNorm = 1.0
    let numRightAnglesNorm = 1.0

    let numCrossingsSq = float (numRightAngleSegCrossings sheet) ** 2.0
    let visibleLengthSq = float (visibleWireLength sheet) ** 2.0
    let numRightAnglesSq = float (numWireRightAngles sheet) ** 2.0

    // Square and add metrics to encourage optimisation algorithms to make them all moderately small, as
    // opposed to sacrificing one in order to make the others very small.
    // Same concept as L1 vs L2 regularisation in neural networks.
    numCrossingsSq*numCrossingsNorm + visibleLengthSq*visibleLengthNorm + numRightAnglesSq*numRightAnglesNorm



////////// Algorithm 1: Brute force //////////

/// Takes a model and a list of symbols to manipulate (flip, reverse inputs and reorder ports),
/// and returns a new model with the changed symbols (optimising for the beautify function),
/// and the score of this sheet.
/// compIds is a list of the component ids of all symbols under consideration.
/// Uses brute force - exponential time complexity. Only use for small groups of symbols.
let rec bruteForceOptimise
        (symbols: SymbolT.Symbol list)
        (compIds: ComponentId list)
        (sheet: SheetT.Model)
        : SheetT.Model*float =

    match symbols with
    | h::t ->
        let flippedSym =
            match h.STransform.Rotation with
            | Rotation.Degree0 | Rotation.Degree180 ->
                flipSymbolInBlock SymbolT.FlipHorizontal h.CentrePos h
            | _ -> flipSymbolInBlock SymbolT.FlipVertical h.CentrePos h

        let symbolChoices =
            match h.Component.Type with
            | Not | GateN (_, _) ->
                [h; flippedSym]
            | Mux2 | Mux4 | Mux8 ->
                [Optic.set reversedInputPorts_ (Some false) h;
                 Optic.set reversedInputPorts_ (Some true) h;
                 Optic.set reversedInputPorts_ (Some false) flippedSym;
                 Optic.set reversedInputPorts_ (Some true) flippedSym]
            | _ -> [h]

        symbolChoices
        |> List.map (fun sym -> replaceSymbol sym sheet)
        |> List.map (bruteForceOptimise t compIds)
        |> List.minBy snd

    | [] ->
        sheet.Wire
        |> reRouteWiresFrom compIds
        |> fun wire -> Optic.set SheetT.wire_ wire sheet
        |> updateBoundingBoxes
        |> fun newSheet -> newSheet, sheetScore newSheet



////////// Algorithm 2: Clock face //////////

/// Reorder the ports (given as string ids) of a custom component along a single side
/// according to the clock face algorithm.
let reorderCustomPorts
        (model: BusWireT.Model)
        (ports: string list)
        : string list =
    ports
    // TODO

/// Take a custom component symbol and reorder its ports along each edge
/// according to the clock face algorithm.
let customCompClockFace
        (model: BusWireT.Model)
        (symbol: SymbolT.Symbol)
        : SymbolT.Symbol =
    let edges = [Edge.Left; Edge.Right; Edge.Top; Edge.Bottom]
    let reorderedPorts =
        edges
        |> List.map (getOrderedPorts symbol)
        |> List.map (reorderCustomPorts model)
    (symbol, edges, reorderedPorts)
    |||> List.fold2 setOrderedPorts



////////// Algorithm 3: Circuit partition heuristic //////////

// This algorithm uses breadth first search to obtain partitions of no more than k
// elements each from the circuit. We represent the circuit as a graph where each vertex
// is a component, and each edge is a wire.

// Store graph as an adjacency list, using component ids.
type Graph = Map<string, string list>

/// Add a component to the visited set and the queue.
let enqueueAndVisit
        (visited: Set<string>)
        (queue: string list)
        (comp: string)
        : Set<string>*(string list) =
    match Set.contains comp visited with
    | true -> visited, queue
    | false -> Set.add comp visited, queue @ [comp]

/// Runs breadth first search on the circuit, getting k (or fewer) connected components.
let rec bfsCircuit
        (graph: Graph)
        (k: int)
        (visited: Set<string>)
        (queue: string list)
        : string list =
    match queue, Set.count visited < k with
    | [], _ | _, false ->
        Set.toList visited
    | comp::remQueue, true ->
        let visited', queue' =
            ((visited, remQueue), graph[comp])
            ||> List.fold (fun (v, q) -> enqueueAndVisit v q)
        bfsCircuit graph k visited' queue'

/// Partition a circuit given as a graph into clusters of no more than k components each.
/// Clusters that are close together are prioritised (approximately) via DFS.
/// TODO could improve distance prioritisation via a modification of Dijkstra's algorithm.
let rec getGraphPartitions
        (symbolMap: Map<ComponentId, SymbolT.Symbol>)
        (graph: Graph)
        (k: int)
        : string list list =
    let startNode =
        Map.toList graph
        |> List.map fst
        |> List.minBy (fun compId -> symbolMap[(ComponentId compId)].Pos.X)
    let cluster = bfsCircuit graph k Set.empty [startNode]
    let graph' =
        (graph, cluster)
        ||> List.fold (fun g node -> Map.remove node g)
    match Map.isEmpty graph' with
    | true -> [cluster]
    | false -> getGraphPartitions symbolMap graph' k @ [cluster]

/// Returns the updated graph and list of discovered edges after considering a wire.
let updateGraphWithWire
        (graph: Graph)
        (discoveredEdges: Set<string*string>)
        (model: SymbolT.Model)
        (wire: BusWireT.Wire)
        : Graph*Set<string*string> =
    let outputComp = wire.OutputPort |> function
                     | OutputPortId portId -> model.Ports[portId].HostId
    let inputComp = wire.InputPort |> function
                    | InputPortId portId -> model.Ports[portId].HostId
    let addEdge comp1 comp2 =
        let list1 = graph[comp1] @ [comp2]
        let list2 = graph[comp2] @ [comp1]
        graph
        |> Map.add comp1 list1
        |> Map.add comp2 list2
    if outputComp = inputComp || Set.contains (outputComp, inputComp) discoveredEdges then
        graph, discoveredEdges
    else
        addEdge outputComp inputComp,
        discoveredEdges
        |> Set.add (outputComp, inputComp)
        |> Set.add (inputComp, outputComp)

/// Returns partitions of circuit into clusters of no more than k components each.
/// Partitions are given as list of component ids (strings).
let partitionCircuit
        (k: int)
        (sheet: SheetT.Model)
        : string list list =
    let initGraph: Graph =
        sheet.Wire.Symbol.Symbols
        |> Map.toList
        |> List.filter (fun (_, sym) -> sym.Annotation = None)
        |> List.map fst
        |> List.map (function | ComponentId compId -> compId, [])
        |> Map.ofList
    let wires =
        sheet.Wire.Wires
        |> Map.toList
        |> List.map snd
    let graph, _ =
        ((initGraph, Set.empty), wires)
        ||> List.fold (fun (currGraph, discEdges) wire ->
                        updateGraphWithWire currGraph discEdges sheet.Wire.Symbol wire)
    getGraphPartitions sheet.Wire.Symbol.Symbols graph k
