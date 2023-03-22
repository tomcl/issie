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

// HLP23: AUTHOR Ifte

///HLP23: Shaanuka - Helper function for scaling custom component sizes
///Scales custom component size by multiplying the Symbol fields HScale and VScale by input float
///XScale and YScale and returns Symbol type .
let symbolSizeScale (symbol: Symbol) xScale yScale =

    match symbol.VScale, symbol.HScale with 
    |Some vScale, Some hScale ->    let vScaleRes = vScale * yScale
                                    let hScaleRes = hScale * xScale
                                    {symbol with VScale = Some vScaleRes; HScale = Some hScaleRes}
    |_, _ -> {symbol with VScale = Some yScale; HScale = Some xScale}

// Helper function compiled later
type BusWireHelpers = {
    updateSymbolWires: Model -> ComponentId -> Model
    }

/// HLP23: AUTHOR Ifte
/// reSizeSymbol takes two symbols connected by wires and resizes symbolToSize so that any wires
/// that are nearly straight become straight
let reSizeSymbol 
    (wModel: BusWireT.Model) 
    (symbolToSize: Symbol)
    (otherSymbol: Symbol) 
    (busWireHelper: BusWireHelpers)
        : BusWireT.Model =

    let wireThreshold = 11.0
    let sModel = wModel.Symbol
    let wires = wModel.Wires
    let ports = sModel.Ports
    
    /// Returns corresponding orientation if symbols have overlapping X/Y coverage and None if both
    let getOrientation fstSym sndSym =
        let fstCorners = symbolBox fstSym
        let sndCorners = symbolBox sndSym
        if (((snd fstCorners[0] > snd sndCorners[2]) || (snd fstCorners[2] < snd sndCorners[0])) 
            && (fst fstCorners[0] < fst sndCorners[1]) 
            && (fst fstCorners[1] > fst sndCorners[0])) 
        then
            printf "%A" "Vertical"
            Some Vertical
        else if (((fst fstCorners[0] > fst sndCorners[1]) || (fst fstCorners[1] < fst sndCorners[0])) 
                && (snd fstCorners[0] < snd sndCorners[2]) 
                && (snd fstCorners[2] > snd sndCorners[0])) 
        then
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

    let wirePortsMapper orientation currWire =
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
            else
                getPortCoord symbolToSize inputPort, getPortCoord otherSymbol outputPort

        portPair

    let getWirePorts orientation connWires = 
        connWires
        |> List.map (wirePortsMapper orientation)
        |> List.sortBy (fun elem -> fst elem)

    let pairDiff pair = fst pair - snd pair
    let closeWireFinder threshold pair = 
        let diff = abs (pairDiff pair)
        diff < threshold && diff <> 0.0

    let getFstIdx wirePorts = 
        wirePorts 
        |> List.tryFindIndex (closeWireFinder wireThreshold)

    let subOffset offset pair = fst pair - offset, snd pair

    let getSndIdx offset wirePorts =
        wirePorts
        |> List.map (subOffset offset)
        |> List.tryFindIndexBack (closeWireFinder (2.0*wireThreshold))

    let duplicateFilter fstIdx sndIdx = 
        if sndIdx <> fstIdx then
            Some sndIdx
        else
            None

    let getPortSep fstPorts sndPorts = fst sndPorts - fst fstPorts

    let getScale offset portSep sndPorts = (portSep - (pairDiff sndPorts - offset)) / portSep

    let getScaledSymbol orientation offset scale portPair =
        match orientation, offset, portPair with
        | Some Horizontal, Some offset', Some pair -> 
            let newY = symbolToSize.Pos.Y - offset' - (1.0 - scale)*(symbolToSize.Pos.Y - fst pair)
            let newComp = {symbolToSize.Component with Y = newY}
            let newPos = {symbolToSize.Pos with Y = newY}
            let symbolToSize' = symbolSizeScale symbolToSize 1.0 scale
            {symbolToSize' with Pos = newPos; Component = newComp}
        | Some Vertical, Some offset', Some pair -> 
            let newX = symbolToSize.Pos.X - offset' - (1.0 - scale)*(symbolToSize.Pos.X - fst pair)
            let newComp = {symbolToSize.Component with X = newX}
            let newPos = {symbolToSize.Pos with X = newX}
            let symbolToSize' = symbolSizeScale symbolToSize scale 1.0
            {symbolToSize' with Pos = newPos; Component = newComp}
        | _ -> symbolToSize

    let getNewSymbol orientation offset scale portPair =
        match scale with
        | Some sFactor -> getScaledSymbol orientation offset sFactor portPair
        | None -> getScaledSymbol orientation offset 1.0 portPair



    let orientation = getOrientation symbolToSize otherSymbol
    let wireList = wires |> getWireList

    let wirePorts = 
        (orientation, wireList) 
        ||> Option.map2 getConnectedWires 
        |> Option.map2 getWirePorts orientation

    let fstIdx = 
        wirePorts
        |> Option.map getFstIdx
        |> Option.flatten
    let fstPorts = 
        (fstIdx, wirePorts)
        ||> Option.map2 List.item 

    let offset = 
        fstPorts 
        |> Option.map pairDiff

    let sndIdx = 
        (offset, wirePorts) 
        ||> Option.map2 getSndIdx
        |> Option.flatten
        |> Option.map2 duplicateFilter fstIdx
        |> Option.flatten
    let sndPorts = 
        (sndIdx, wirePorts)
        ||> Option.map2 List.item 

    let portSep = 
        (fstPorts, sndPorts) 
        ||> Option.map2 getPortSep
    let scale = 
        (offset, portSep, sndPorts) 
        |||> Option.map3 getScale

    printf "%A" offset
    printf "%A" fstPorts
    printf "%A" sndPorts
    printf "%A" portSep
    printf "%A" scale

    let newSymbol = getNewSymbol orientation offset scale fstPorts

    // Add new symbols to model map
    let newModel = 
        {wModel with 
            Symbol = {sModel with Symbols = Map.add newSymbol.Id newSymbol sModel.Symbols}
        }
    // Add new wires to model
    let newModel' = busWireHelper.updateSymbolWires newModel symbolToSize.Id

    newModel'

/// HLP23: AUTHOR Ifte
/// Applies wire straightening across an entire sheet by scaling/translating symbols
let sheetReSizeSymbol
    (wModel: BusWireT.Model)
    (busWireHelper: BusWireHelpers)
        : BusWireT.Model =

    let wireLst =
        wModel.Wires
        |> Map.toList
        |> List.map snd

    let wireLstFolder model wire =
        let sModel = model.Symbol
        let inPortId = string wire.InputPort
        let outPortId = string wire.OutputPort
        let symbolToSize = getSymbol sModel inPortId
        let otherSymbol = getSymbol sModel outPortId

        reSizeSymbol model symbolToSize otherSymbol busWireHelper


    (wModel, wireLst)
    ||> List.fold wireLstFolder