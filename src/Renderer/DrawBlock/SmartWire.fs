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
// let findSymbol (model: Model) (wire: Wire) : Symbol option = 
//     let inputPort = string wire.InputPort
//     // let symbol = model.Symbol.Symbols |> Map.tryFind (fun k v ->
//     //     v.PortMaps.Orientation |> Map.tryFind inputPort |> Option.isSome
//     // )
//     // symbol
//     // printfn "Not implemented yet"
//     // let componentId = Map.tryFind inputPort model.Symbol.Symbols.Orientation
//     // let symbol = componentId |> Option.bind (fun id -> Map.tryFind id model.Symbol.Symbols)
//     // symbol
//     let symbol = model.Symbol.Symbols |> Map.tryFind (fun (_,sym) ->
//         sym.PortMaps.Orientation |> Map.tryFind inputPort |> Option.isSome
//     )
//     symbol
     

/// helper function that routes a wire from input port to output port around the symbol, rather than through it
let routeAroundSymbol (model: Model) (wire: Wire) (symbol: Symbol) : Wire = 

    // let segmentInfo =
    //         wire.Segments
    //         |> List.map (fun (seg:Segment) -> seg.Length,seg.Mode)
    //     printfn "%s" $"Wire: Initial Orientation={wire.InitialOrientation}\nSegments={segmentInfo}"
    
    let newWires = 
        let hugLength = symbol.Component.H + 5.0
        let newFirstSegment: Segment = { wire.Segments[2] with Length =  hugLength}
        let newThirdSegment: Segment = { wire.Segments[4] with Length =  hugLength}
        let newWires: Wire = { wire with Segments = [wire.Segments[0]; wire.Segments[1]; newFirstSegment; wire.Segments[3]; newThirdSegment; wire.Segments[5]; wire.Segments[6]]}
        newWires 

    newWires


/// top-level function which replaces autoupdate and implements a smarter version of same
/// it is called every time a new wire is created, so is easily tested.
let smartAutoroute (model: Model) (wire: Wire): Wire = 
    // routeAroundSymbol model wire model
    autoroute model wire
