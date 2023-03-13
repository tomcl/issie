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

/// HLP23: AUTHOR Ifte
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
    
    let getOrientation fstSym sndSym =
        let fstCorners = symbolBox fstSym
        let sndCorners = symbolBox sndSym
        if (((snd fstCorners[0] > snd sndCorners[2]) || (snd fstCorners[2] < snd sndCorners[0])) && (fst fstCorners[0] < fst sndCorners[1]) && (fst fstCorners[1] > fst sndCorners[0])) then
            printf "%A" "Vertical"
            Some Vertical
        else if (((fst fstCorners[0] > fst sndCorners[1]) || (fst fstCorners[1] < fst sndCorners[0])) && (snd fstCorners[0] < snd sndCorners[2]) && (snd fstCorners[2] > snd sndCorners[0])) then
            printf "%A" "Horizontal"
            Some Horizontal
        else
            None

    let getWireList wires = 
        let tupleList = Map.toList wires 
        match List.length tupleList with
        | 0 -> None
        | n -> tupleList 
               |> List.map snd 
               |> Some

    let getConnectedWires orientation wireList = 
        let connWires = findInterconnectingWires wireList sModel symbolToSize otherSymbol 1
        let orientationFilter ori wire = wire.InitialOrientation = ori
        connWires |> List.filter (orientationFilter orientation)

    let wirePortsFolder orientation lst currWire =
        let inputKey = string currWire.InputPort
        let outputKey = string currWire.OutputPort
        let inputPort = ports[inputKey]
        let outputPort = ports[outputKey]

        let getPortCoord symbol port =
            match orientation with
            | Vertical -> symbol.Component.X + (getPortPos symbol port).X
            | Horizontal -> symbol.Component.Y + (getPortPos symbol port).Y

        let portPair = 
            if outputPort.HostId = string symbolToSize.Id then
                getPortCoord symbolToSize outputPort, getPortCoord otherSymbol inputPort
            else if inputPort.HostId = string symbolToSize.Id then
                getPortCoord symbolToSize inputPort, getPortCoord otherSymbol outputPort
            else
                failwithf "no valid port IDs - check options"

        lst @ [portPair]

    let getWirePorts orientation connWires = 
        ([], connWires) 
        ||> List.fold (wirePortsFolder orientation)
        |> List.sortBy snd

    let pairDiff pair = fst pair - snd pair
    let closeWireFinder threshold pair = (abs (pairDiff pair)) < threshold

    let getFstPorts wirePorts = 
        wirePorts 
        |> List.tryFind (closeWireFinder wireThreshold)

    let subOffset offset pair = fst pair - offset, snd pair

    let getSndPorts offset wirePorts =
        wirePorts
        |> List.map (subOffset offset)
        |> List.findIndexBack (closeWireFinder (2.0*wireThreshold)) 
        |> List.item <| wirePorts

    let duplicateFilter fstPair sndPair = 
        if sndPair <> fstPair then
            Some sndPair
        else
            None

    let getPortSep offset fstPorts sndPorts = fst sndPorts - (fst fstPorts + offset)

    let getScale portSep sndPorts = (portSep - pairDiff sndPorts) / portSep

    let getNewPos orientation offset = 
        match orientation with
        | Horizontal -> {symbolToSize.Pos with Y = symbolToSize.Pos.Y - offset}
        | Vertical -> {symbolToSize.Pos with X = symbolToSize.Pos.X - offset}

    let getNewSymbol orientation newPos scale =
        match orientation with
        | Some Horizontal -> {symbolToSize with Pos = newPos; VScale = scale}
        | Some Vertical -> {symbolToSize with Pos = newPos; HScale = scale}
        | None -> symbolToSize

    //let wireScale (model: Model) (sFactor: float) =
    //    let mapLst = Map.toList model.Wires
    //    let wiresLst = mapLst |> List.map (fun pair -> snd pair)
    //    let segLst = ([], wiresLst) ||> List.fold (fun segLst wire -> List.append segLst [wire.Segments])
    //    let segLst' = 
    //        segLst 
    //        |> List.map (fun lst -> [lst[0]; lst[1]; lst[2]; {lst[3] with Length = sFactor*lst[3].Length}; lst[4]; lst[5]; lst[6]])
    //    let scaledWires = mapLst |> List.mapi (fun i pair -> 
    //        let currWire = {(snd pair) with Segments = segLst'[i]}
    //        fst pair, currWire
    //        )
    //    Map.ofList scaledWires

    let orientation = getOrientation symbolToSize otherSymbol
    let wireList = wires |> getWireList

    //let connectedWires = (orientation, wireList) ||> Option.map2 getConnectedWires

    let wirePorts = (orientation, wireList) 
                    ||> Option.map2 getConnectedWires 
                    |> Option.map2 getWirePorts orientation

    let fstPorts = wirePorts 
                   |> Option.map getFstPorts 
                   |> Option.flatten
    let offset = fstPorts 
                 |> Option.map pairDiff
    let sndPorts = (offset, wirePorts) 
                   ||> Option.map2 getSndPorts
                   |> Option.map2 duplicateFilter fstPorts
                   |> Option.flatten
    printf "%A" fstPorts
    printf "%A" sndPorts
    let portSep = (offset, fstPorts, sndPorts) 
                  |||> Option.map3 getPortSep
    printf "%A" portSep
    let scale = (portSep, sndPorts) 
                ||> Option.map2 getScale
    printf "%A" scale
    let newPos = (orientation, offset)
                 ||> Option.map2 getNewPos 
                 |> Option.defaultValue symbolToSize.Pos
    let newSymbol = getNewSymbol orientation newPos scale

    // HLP23: this could be cleaned up using Optics - see SmartHelpers for examples
    // Add new wires to model & new symbols to model map
    {wModel with 
        Symbol = {sModel with Symbols = Map.add newSymbol.Id newSymbol sModel.Symbols}
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
