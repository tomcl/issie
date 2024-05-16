module GraphHelpers
open EEExtensions

(*
  Helper functions & Types used to work with Directed Graphs
*)

open System
open System.Collections.Generic

/// return list of map keys
let keysL m = (Map.keys >> Seq.toList) m

/// return list of map values
let valuesL m = (Map.values >> Seq.toList) m

type Edge<'NODE when 'NODE:comparison> = {Start: 'NODE; End: 'NODE}

/// Define the graph as a map of nodes to a list of their edge ends and weights
type WeightedGraph<'NODE when 'NODE:comparison> =
    {
        /// map of start node -> list of all end nodes of edges from start
        Edges: Map<'NODE, 'NODE list>
        /// Map of edge weights
        Weights: Map<'NODE Edge, float>
    }

/// Edges held as Map with keys end edge nodes and values a list of all the
/// corresponding edge start nodes paired with edge weights
let transposeEdgesWithWeights (graph: 'a WeightedGraph) : Map<'a, ('a*float) list> =
    /// all edges in graph
    let allEdges =
        Map.toList graph.Edges
        |> List.collect (fun (v, edgeL) ->
            List.map (fun endNode -> {Start = v; End = endNode}) edgeL)
    allEdges
    |> List.groupBy (fun edge -> edge.End)
    |> List.map (fun (endNode, edgeL) -> endNode, List.map (fun edge -> edge.Start, graph.Weights[edge]) edgeL)
    |> Map.ofList

/// Apply f n times to x.
/// If n < 0 return x.
let rec power (n:int) (f: 'a->'a) (x:'a) : 'a =
    match n with
    | 0 -> x
    | i when i < 0 -> x
    | i -> power (n-1) f (f x)

/// graph:  a directed weighted graph represented as a Map of Nodes to Edges with non-negative costs.
/// source: node from which to meaure path costs.
/// Return a Map from nodes to the cost of the shortest path to reach the node from source.
/// Nodes unreachable from source are not included in the result Map.
/// 'NODE is the type of the graph nodes, which must support comparison.
let dijkstra (graph: WeightedGraph<'NODE>) (source: 'NODE) : Map<'NODE,float> =
    let rec dijkstra' (start: 'NODE) (distances: Map<'NODE, float>) =
        match Map.tryFind start distances with
        | Some distU ->
            let updatedDistances: Map<'NODE,float> =
                (distances, graph.Edges[start])
                ||> List.fold (fun dist endNode ->
                    let alt = distU + graph.Weights[{Start=start; End=endNode}]
                    if alt < dist[endNode] then
                        Map.add endNode alt dist
                    else dist) 
            let nextNode =
                graph.Edges[start]
                |> List.minBy (fun endNode -> updatedDistances[endNode])
            dijkstra' nextNode updatedDistances
        | None -> distances
    let initialDistances = Map.add source 0. Map.empty
    dijkstra' source initialDistances

/// graph is an arbitrary directed weighted graph which can have negative weights and cycles.
/// If graph has one or more cycles with negative path length reachable from source
/// return an error with one of these cycles.
/// Otherwise return a Map from graph nodes to the minimum path length from source to the node.
/// Graph nodes which cannot be reached from source are excluded from the returned Map.
/// 'NODE is the type of the graph nodes, which must support comparison.
let inline bellmanFord (graph: 'NODE WeightedGraph) (source: 'NODE): Result<Map<'NODE,float>, 'NODE list> =
    // This function must be inline to allow generic 'a.
    // Otherwise the first usage of it in a module will stop 'a from being generic
    let predecessorsWithWeights = transposeEdgesWithWeights graph
    let initialDistances = Map.ofList [ source, (source,0.)]
    let nodes = keysL graph.Edges

    let getNegativeCycle (cycleNode: 'NODE) (distances: Map<'NODE,'NODE*float>) =
        let nodesTouched = [cycleNode]
        let rec traceCycle nodesTouched (node: 'NODE) =
            if List.contains node nodesTouched then
                node :: List.takeWhile ((<>) node) nodesTouched
            else traceCycle (node :: nodesTouched) (fst distances[node])
        traceCycle nodesTouched cycleNode        
        
    let relaxNode distances (node: 'NODE) =
        predecessorsWithWeights[node]
        |> List.filter (fun (pre, _) -> Map.containsKey pre distances)
        |> (fun ewL ->
            let (predecessor, w) = List.minBy (fun (predecessor, edgeWeight) -> edgeWeight + snd distances[predecessor]) ewL
            let newDist = snd distances[predecessor] + w 
            if newDist < snd distances[node] then [node,(predecessor,newDist)] else [])
 
    let relaxNodes distanceApprox =
        List.collect (relaxNode distanceApprox) nodes
        |> (fun items -> Map.addItems items distanceApprox)

    let distances = power (nodes.Length-1) relaxNodes initialDistances
    let distances' = relaxNodes distances
    nodes
    |> List.tryFind (fun node -> distances[node] <> distances'[node])
    |> function | Some cycleNode -> Error <| getNegativeCycle cycleNode distances
                | None ->
                    distances
                    |> Map.map (fun k (pre,dist) -> dist)
                    |> Ok    
    
type GraphTestData<'a> = {End: 'a; Cost:int}

/// turn a Map representing edges and weights into a WeightedGraph
let makeGraph (gt: Map<'NODE, 'NODE GraphTestData list>) : 'NODE WeightedGraph=
    let costs =
        Map.toList gt
        |> List.collect (fun (k, vL) -> List.map (fun v -> {Start=k; End=v.End},float v.Cost) vL)
        |> Map.ofList
    let edges = Map.map (fun k vL -> List.map (fun v -> v.End) vL) gt
    { Edges = edges; Weights = costs}

let exhaustiveMinPath (graph: 'NODE WeightedGraph) (source: 'NODE): Map<'NODE,float> =
    let addPaths (pathL: 'NODE list list) =
        pathL
        |> List.collect (fun path ->
            match Map.tryFind (List.head path) graph.Edges with
            | None -> []
            | Some nextL -> List.collect  (fun next -> if List.contains next path then [] else [next :: path]) nextL)
    let lengthOf path =
        path
        |> List.pairwise
        |> List.sumBy (fun (startN,endN) -> Map.findWithDefault {Start=startN; End = endN} 0. graph.Weights )

    let zeroLengthPath = [[source]]
    [1..7]
    |> List.collect (fun numEdgesInPath -> (power numEdgesInPath addPaths zeroLengthPath))
    |> List.groupBy List.head
    |> List.map (fun (dest, pathL) -> dest, List.min (List.map  lengthOf pathL))
    |> Map.ofList

    

// Example usage
let graph =
    Map.empty
    |> Map.add "a" [{ End = "b"; Cost = 7 }; { End = "c"; Cost = 9 }; { End = "f"; Cost = 14 }]
    |> Map.add "b" [{ End = "c"; Cost = 10 }]
    |> Map.add "c" [{ End = "d"; Cost = 11 }]
    |> Map.add "d" [{ End = "e"; Cost = 6 }]
    |> Map.add "e" [{ End = "f"; Cost = 9 }]
    |> makeGraph

let sourceNode = "a"
let shortestDistances = dijkstra graph sourceNode
let x = bellmanFord (makeGraph Map.empty) sourceNode
let shortestDistancesOrCycle = bellmanFord graph
printfn "Shortest distances from node %s:" sourceNode
