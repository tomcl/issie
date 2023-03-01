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
open SmartHelpers

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
    // Also assumes parallel sides are vertical
    printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"

    let wireThreshold = 11.0
    let sModel = wModel.Symbol
    let wires = wModel.Wires
    let ports = sModel.Ports
    
    let wireList = (Map.toList wires) |> List.map (fun x -> snd x)
    // picks out wires going from otherSymbol to symbolToSize
    /// HLP23: AUTHOR Indraneel
    let connectedWires = findInterconnectingWires wireList sModel symbolToSize otherSymbol 0

    let wirePortsFolder lst currWire =
        let key = string currWire.InputPort
        let currPort = ports[key]
        let portOffset = getPortPos symbolToSize currPort

        let startY = currWire.StartPos.Y //assuming verttical
        let endY = symbolToSize.Component.Y + portOffset.Y

        lst @ [startY, endY]

    let wirePorts = 
        ([], connectedWires) 
        ||> List.fold wirePortsFolder 
        |> List.sortBy (fun pair -> snd pair)

    let pairDiff pair = snd pair - fst pair
    let closeWireFinder thresh pair = abs (pairDiff pair) < thresh

    let fstPorts = 
        wirePorts 
        |> List.find (closeWireFinder wireThreshold)
    let offset = pairDiff fstPorts

    let wirePorts' = wirePorts |> List.map (fun portPair -> fst portPair, snd portPair - offset)

    let sndPorts =
        wirePorts'
        |> List.findIndexBack (closeWireFinder (2.0*wireThreshold)) 
        |> List.item <| wirePorts'

    let portSep = snd sndPorts + offset - snd fstPorts
    let scale = (portSep - pairDiff sndPorts) / portSep
    let newPos = {symbolToSize.Pos with Y = symbolToSize.Pos.Y - offset}
    let symbol' = {symbolToSize with Pos = newPos; VScale = Some scale}

    // HLP23: this could be cleaned up using Optics - see SmartHelpers for examples
    // Add new wires to model & new symbols to model map
    {wModel with 
        Wires = wModel.Wires // no change for now, but probably this function should use update wires after resizing.
                             // to make that happen the test function which calls this would need to provide an updateWire
                             // function to this as a parameter (as was done in Tick3)
        Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
    }


