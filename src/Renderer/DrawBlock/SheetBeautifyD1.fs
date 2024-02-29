module SheetBeautifyD1

open CommonTypes
open DrawModelType
open BlockHelpers
open BusWire
open SymbolT

// this is the module for team phase work D1 

// Given three visual (non-zero) segments, determine whether combine into a parallel wire
let isParallelWire (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) (seg3: BusWireT.ASegment) : bool= 
    getSegmentOrientation seg1.Start seg1.End = getSegmentOrientation seg3.Start seg3.End &&
    getSegmentOrientation seg1.Start seg1.End <> getSegmentOrientation seg2.Start seg2.End &&
    seg1.Segment.Length * seg3.Segment.Length > 0.0

// Determine whether a symbol is a Singly-connected component
let isSinglyConnectedComponent (sym: Symbol) : bool =
    let hasOnlyOnePort : string option= 
        let portsLst=
            sym.PortMaps.Order
            |> Map.toList
            |> List.collect (fun tuple -> snd tuple)
        match portsLst.Length with
        | 1 -> Some portsLst.Head
        | _ -> None
    let portHasOnlyOneWire = 
        true
    match hasOnlyOnePort with
    | Some string -> true
    | _ -> false

// determine whether a wire is Singly-constrained parallel wire
let isSinglyConstrainedParallelWire (wire: BusWireT.Wire) : bool =
    true

/// Extract parallel wires that can be straightened from wire list
let extractParalellWires (wires: BusWireT.Wire list) : BusWireT.ASegment list list = 
    wires
    |> List.map (fun wire -> getNonZeroAbsSegments wire)
    |> List.filter (fun segs -> segs.Length = 3)
    |> List.filter (fun segs -> isParallelWire segs[0] segs[1] segs[2])

