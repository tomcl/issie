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
    // Also assumes parallel sides are vertical
    printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"
    let sModel = wModel.Symbol

    let wires = wModel.Wires
    let ports = sModel.Ports
    
    let wireList = (Map.toList wires) |> List.map (fun x -> snd x)
    // picks out wires going from otherSymbol to symbolToSize
    let connectedWires = SmartHelpers.findInterconnectingWires wireList sModel symbolToSize otherSymbol 0

    let wireEndsFolder lst currWire =
        let key = string currWire.InputPort
        let currPort = ports[key]
        let portOffset = getPortPos symbolToSize currPort

        let startY = currWire.StartPos.Y
        let endY = symbolToSize.Component.Y + portOffset.Y

        lst @ [startY, endY]

    let endsList = 
        ([], connectedWires) 
        ||> List.fold wireEndsFolder 
        |> List.sortBy (fun pair -> snd pair)

    let pairDiff pair = snd pair - fst pair
    let closeWireFilter thresh ends = abs (pairDiff ends) < thresh

    let fstPair = 
        endsList 
        |> List.find (closeWireFilter 11.0)
    let offset = pairDiff fstPair
    // split pairs into sep lists
    let endsList' = endsList |> List.map (fun ends -> fst ends, snd ends - offset)

    let sndPair = // rename
        endsList'
        |> List.findIndexBack (closeWireFilter 21.0) 
        |> List.item <| endsList'

    let portGap = snd endsList[0] - symbolToSize.Pos.Y
    printfn "%A" portGap

    let newPos = {symbolToSize.Pos with Y = symbolToSize.Pos.Y - offset}
    let newComponent = {symbolToSize.Component with H = symbolToSize.Component.H - pairDiff sndPair}
    let scale = (snd sndPair - snd fstPair - pairDiff sndPair) / (snd sndPair - snd fstPair)

    printfn "%A" endsList'

    // now look at another set of connected port pairs and figure out the scaling factor to match them too
    // beyond that it is impossible to match 3 pairs which are all misaligned from one another


    printfn "%A" scale

    //let testSym = {symbolToSize with VScale = Some 0.8}
    //let testLst = SmartHelpers.findInterconnectingWires wireList sModel testSym otherSymbol 0
    //let y1 = getPortPos testSym ports[string testLst[0].InputPort]
    //let y2 = getPortPos testSym ports[string testLst[1].InputPort]
    //printfn "%A" y1
    //printfn "%A" y2

    //let symbol' = {symbolToSize with Pos = newPos; Component = newComponent}
    let symbol' = {symbolToSize with Pos = newPos; VScale = Some scale}

    // HLP23: this could be cleaned up using Optics - see SmartHelpers for examples
    // Add new wires to model & new symbols to model map
    {wModel with 
        Wires = wModel.Wires // no change for now, but probably this function should use update wires after resizing.
                             // to make that happen the test function which calls this would need to provide an updateWire
                             // function to this as a parameter (as was done in Tick3)
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
