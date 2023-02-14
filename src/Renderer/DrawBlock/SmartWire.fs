module SmartWire
open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers

open Optics
open Operators

// HLP23: OMAR


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

     

/// helper function that routes a wire from input port to output port around the symbol, rather than through it
/// Latest issue: WIRE HAS NO SEGMENTS
let routeAroundSymbol (model: Model) (wire: Wire) (symbol: Symbol Option) : Wire = 
    let segmentInfo =
        wire.Segments
        |> List.map (fun (seg:Segment) -> seg.Length,seg.Mode)
    printfn "%s" $"Wire: Initial Orientation={wire.InitialOrientation}\nSegments={segmentInfo}"

    let symbolFound = symbol |> Option.get
    let newWires = 
        let hugLength = symbolFound.Component.H 
        let nullSegment: Segment = { wire.Segments[0] with Length =  hugLength}
        let newFirstSegment: Segment = { new wire.Segments[2] with Length =  hugLength}
        let newThirdSegment: Segment = { wire.Segments[4] with Length =  hugLength}
        let newWires: Wire = { wire with Segments = [nullSegment; nullSegment; newFirstSegment; wire.Segments[3]; newThirdSegment; nullSegment; nullSegment]}
        newWires 

    newWires
    
    // wire

/// top-level function which replaces autoupdate and implements a smarter version of same
/// it is called every time a new wire is created, so is easily tested.
let smartAutoroute (model: Model) (wire: Wire): Wire = 
    // routeAroundSymbol model wire model
    let symbol = findSymbol model wire
    printfn "Symbol found: %A" symbol

    routeAroundSymbol model wire symbol

    // autoroute model wire
