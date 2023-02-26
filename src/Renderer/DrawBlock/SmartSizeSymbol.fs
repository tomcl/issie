module SmartSizeSymbol

open Elmish
open Fable.React.Props
open CommonTypes
open Fable.React
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Symbol
open Optics
open Operators

(* 
    HLP23: This module will normally be used exclusively by team member doing the "smart resize symbol" 
    part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    Normally it will update multiple wires and one symbols in the BusWire model so could use the SmartHelper 
    function for the wires.
*)

/// HLP23: To test this, it must be given two symbols interconnected by wires. It then resizes symbolToSize
/// so that the connecting wires are exactly straight
/// HLP23: It should work out the interconnecting wires (wires) from 
/// the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has adjustable size).
/// HLP23: when this function is written replace the XML comment by something suitable concisely
/// stating what it does.
let reSizeSymbol 
    (wModel: BusWireT.Model) 
    (symbolToSize: Symbol)
    (otherSymbol: Symbol) 
        : BusWireT.Model =
    // Currently works on the assumption that symbolToSize is always on the receiving end of wires
    printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"
    let sModel = wModel.Symbol
    printfn "%A" sModel.Ports
    printfn "%A" wModel.Wires
    //printfn "%A" otherSymbol.Id // could use HostID to find which way the wires are going

    let wires = wModel.Wires
    let ports = sModel.Ports
    
    let wireList = (Map.toList wires) |> List.map (fun x -> snd x)
    // picks out wires going from otherSymbol to symbolToSize
    let connectedWires = SmartHelpers.findInterconnectingWires wireList sModel symbolToSize otherSymbol 0

    // Port position testing:
    //let testRes = wireList |> List.map (fun wire -> wire.StartPos)
    //printfn "%A" testRes
    //printfn "%A" otherSymbol.Pos.Y
    //printfn "%A" otherSymbol.Component.H

    let currWire = connectedWires[0] // need to extend this to all wires using Fold
    let key = string currWire.InputPort
    let currPort = ports[key]
    let x = symbolToSize.Component.X
    let y = symbolToSize.Component.Y
    let h = symbolToSize.Component.H

    let portEdge = symbolToSize.PortMaps.Orientation[key]

    let portCount = float (symbolToSize.PortMaps.Order[portEdge] |> List.length)
    let portNum = 
        match currPort.PortNumber with
        | None -> -1.0
        | Some x -> float x

    let startPos = currWire.StartPos.X, currWire.StartPos.Y
    let endPos = x, y + (h * (portNum + 1.0))/(portCount + 1.0)

    printfn "%A" startPos
    printfn "%A" endPos
    
    // basic resizing
    let hScale = otherSymbol.Component.W / symbolToSize.Component.W
    let vScale = otherSymbol.Component.H / symbolToSize.Component.H

    let symbol' = {symbolToSize with HScale = Some hScale; VScale = Some vScale}

    // HLP23: this could be cleaned up using Optics - see SmartHelpers for examples
    // Add new wires to model & new symbols to model map
    {wModel with 
        Wires = wModel.Wires // no change for now, but probably this function should use update wires after resizing.
                             // to make that happen the test function which calls this would need to provide an updateWire
                             // function to this as a parameter (as was done in Tick3)
        Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
    }


