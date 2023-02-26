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
////the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has adjustable size).
/// HLP23: when this function is written replace teh XML comment by something suitable concisely
/// stating what it does.
let reSizeSymbol 
    (wModel: BusWireT.Model) 
    (symbolToSize: Symbol) // care about hscale and vscale and portmaps which describe the ports for each edge and vice versa & component H + W
    (otherSymbol: Symbol) 
        : BusWireT.Model =
    printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"
    let sModel = wModel.Symbol
    printfn "%A" sModel.Ports
    printfn "%A" wModel.Wires
    printfn "%A" otherSymbol.Id

    let wires = wModel.Wires // line 193 and 325
    let ports = sModel.Ports
    // sModel.Ports -> search for port ids from wires values

    //let posFinder (wires: Map<ConnectionId,Wire>) (ports: Map<string, Port>) =
        //let wireList = Map.toList wires
        //let currWire = snd wireList[0]

        //let startPos = currWire.StartPos

        //let key = string currWire.InputPort
        //let currPort = ports[key]
        //let portNum = currPort.PortNumber
        //let portHost = currPort.HostId

        //startPos
    
    let wireList = Map.toList wires
    let currWire = snd wireList[0]

    let startPos = currWire.StartPos

    let key = string currWire.InputPort
    let currPort = ports[key]
    let portNum = currPort.PortNumber
    let portHost = currPort.HostId

    //if portHost == symbolToSize.Id then
    //    let endSymbol = symbolToSize
    //else
    //    let

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


