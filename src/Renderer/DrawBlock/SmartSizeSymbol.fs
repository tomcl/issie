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

/// HLP23: reSizeSymbol takes two symbols connected by wires and resizes symbolToSize so that any wires that
/// are nearly straight become straight
let reSizeSymbol 
    (wModel: BusWireT.Model) 
    (symbolToSize: Symbol)
    (otherSymbol: Symbol) 
        : BusWireT.Model =
    // Currently works on the assumption that symbolToSize is always on the receiving end of wires
    // Also assumes parallel sides are vertical

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

        let startY = currWire.StartPos.Y
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

    let wireScale (model: Model) (sFactor: float) =
        let mapLst = Map.toList model.Wires
        let wiresLst = mapLst |> List.map (fun pair -> snd pair)
        let segLst = ([], wiresLst) ||> List.fold (fun segLst wire -> List.append segLst [wire.Segments])
        let segLst' = 
            segLst 
            |> List.map (fun lst -> [lst[0]; lst[1]; lst[2]; {lst[3] with Length = sFactor*lst[3].Length}; lst[4]; lst[5]; lst[6]])
        let scaledWires = mapLst |> List.mapi (fun i pair -> 
            let currWire = {(snd pair) with Segments = segLst'[i]}
            fst pair, currWire
            )
        Map.ofList scaledWires


    // HLP23: this could be cleaned up using Optics - see SmartHelpers for examples
    // Add new wires to model & new symbols to model map
    {wModel with 
        Wires = wireScale wModel scale
        Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
    }


//HLP23: Shaanuka - Helper function for scaling custom component sizes

///Scales custom component size by multiplying the Symbol fields HScale and VScale by input float XScale and YScale and returns Symbol type .
let symbolSizeScale (symbol: Symbol) xScale yScale =
    let scales = symbol //{symbol with VScale = Some 1.; HScale = Some 1.} //Uncomment (replace 'symbol') to initialise scales to 1 if no initial value given

    match scales.VScale, symbol.HScale with 
    |Some vScale, Some hScale ->    let vScaleRes = vScale * yScale
                                    let hScaleRes = hScale * xScale
                                    {symbol with VScale = Some vScaleRes; HScale = Some hScaleRes}
    |_, _ -> symbol
