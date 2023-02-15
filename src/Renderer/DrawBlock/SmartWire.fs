module SmartWire
open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers
open SmartHelpers

open Optics
open Operators


(* HLP23

    This module will normally be used exclusively by team member doing the "smart autoroute on single
    wire creation" part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    It does not need specific "get started" test code since is called whenever a new wire is created 
    or (not usual) a symbol is moved so far that the topology of a wire chnages and it is autorouted 
    again from scratch.

*)


// helper function for finding matching symbol in model for given port ids on wire - current issue is finding the symbol with the matching input port id
let findSymbol (model: Model) (wire: Wire) : Symbol option = 
    let inputPort = string wire.InputPort
    let symbolValues =
        model.Symbol.Symbols
        |> Map.toList
        |> List.map snd
    let symbolsWithPortId =
        symbolValues
        |> List.filter (fun symbol ->
            symbol.PortMaps.Orientation.ContainsKey(inputPort))
        |> List.tryHead
    symbolsWithPortId


// returns the height needed to hug the symbol, if needed
let huggingDistance (wire: Wire) (symbol: Symbol) : float = 
    let inputPort = string wire.InputPort
    let portPos = symbol.PortMaps.Orientation |> Map.find inputPort
    let boundaryBox = symbolBox symbol
    let hugDistance = (snd boundaryBox[3]) - wire.StartPos.Y
    match portPos with
        | Left -> hugDistance
        | Right -> hugDistance
        | _ -> 0.0


/// helper function that routes a wire from input port to output port around the symbol, rather than through it
let routeAroundSymbol (model: Model) (wire: Wire) (symbol: Symbol Option) : Wire = 
    let symbolFound = symbol |> Option.get
    let hugLength = (huggingDistance wire symbolFound) + 5.0
    let newWires = 
        let newOutputSegment: Segment = {wire.Segments[2] with Length =  wire.Segments[2].Length - 7.0}
        let newInputSegment: Segment = {wire.Segments[6] with Length =  wire.Segments[6].Length + 7.0}
        let newFirstSegment: Segment = {wire.Segments[3] with Length =  wire.Segments[3].Length + hugLength}
        let newMiddleSegment: Segment = {wire.Segments[4] with Length =  wire.Segments[4].Length + 5.0}
        let newThirdSegment: Segment = {wire.Segments[5] with Length =  wire.Segments[5].Length - hugLength}
        let newWires: Wire = {wire with Segments = [wire.Segments[0]; wire.Segments[1]; newOutputSegment; newFirstSegment;  newMiddleSegment; newThirdSegment; newInputSegment; wire.Segments[7]]}
        newWires 
    match hugLength with
        | 0.0 -> wire
        | _ -> newWires
    

/// top-level function which replaces autoupdate and implements a smarter version of same
/// it is called every time a new wire is created, so is easily tested.
let smartAutoroute (model: Model) (wire: Wire): Wire =     
    let symbol = findSymbol model wire
    // printfn "Symbol found: %A" symbol
    let autoWire = autoroute model wire

    // printfn "%s" $"Wire: Initial Orientation={wire.InitialOrientation}\nSegments={autoWire.Segments}"
    // printfn "%s" $"hugging distance={huggingDistance autoWire (symbol |> Option.get)}"
    // printfn "%s" $"WIRE START POS={wire.StartPos.Y}"
    routeAroundSymbol model autoWire symbol
