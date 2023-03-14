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
open Optic
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

/// Record containing External Helpers required. 
/// HLP23: AUTHOR Dharmil Shah
type ExternalSmartHelpers =
    { UpdateSymbolWires: Model -> ComponentId -> Model }

type portInfo = {
    port: Port;
    sym: Symbol;
    side: Edge;
    ports: string list;
    gap: float;
    topBottomGap: float;
    portDimension: float;
    h: float;
    w: float;
    portGap: float;
}

let makePortInfo (sym: Symbol) (port: Port) =
    let side = getSymbolPortOrientation sym port
    let ports = sym.PortMaps.Order[side] //list of ports on the same side as port
    let gap = getPortPosEdgeGap sym.Component.Type 
    let topBottomGap = gap + 0.3 // extra space for clk symbol
    let portDimension = float ports.Length - 1.0
    let h,w = getRotatedHAndW sym
    let portGap = 
        match side with
        | Left | Right -> 
            float h / (portDimension + 2.0*gap)
        | Bottom | Top ->
            float w /(portDimension + 2.0*topBottomGap)
    {port=port; sym=sym; side=side; ports=ports; gap=gap; topBottomGap=topBottomGap; portDimension=portDimension; h=h; w=w; portGap=portGap}

type wireSymbols = {
    symA: Symbol;
    symB: Symbol;
    wire: Wire
}

let getPortAB wModel wireSyms =
    let ports = getPortsFrmWires wModel [wireSyms.wire]
    let portA = fiterPortBySym ports wireSyms.symA |> List.head
    let portB = fiterPortBySym ports wireSyms.symB |> List.head
    portA, portB

/// Try to get two ports that are on opposite edges
let getOppEdgePortInfo 
    (wModel: BusWireT.Model) 
    (symbolToSize: Symbol) 
    (otherSymbol: Symbol) 
        : (portInfo * portInfo) option  =
    let wires = getWiresBtwnSyms wModel symbolToSize otherSymbol

    let tryGetOppEdgePorts wireSyms =
        let portA, portB = getPortAB wModel wireSyms
        let edgeA = getSymbolPortOrientation wireSyms.symA portA
        let edgeB = getSymbolPortOrientation wireSyms.symB portB
        match edgeA, edgeB with
        | Top, Bottom | Bottom, Top | Left, Right | Right, Left -> 
            Some (makePortInfo wireSyms.symA portA, makePortInfo wireSyms.symB portB)
        | _ -> None

    wires 
    |> List.tryPick (fun w -> tryGetOppEdgePorts {symA=symbolToSize; symB=otherSymbol; wire=w})

let alignPorts (movePInfo: portInfo) (otherPInfo: portInfo) = 
    let getPortAbsPos pInfo = getPortPos pInfo.sym pInfo.port + pInfo.sym.Pos
    let movePortPos = getPortAbsPos movePInfo
    let otherPortPos = getPortAbsPos otherPInfo
    let posDiff = otherPortPos - movePortPos
    let offset = 
        match movePInfo.side with
        | Top | Bottom -> { X=posDiff.X; Y=0.0 }
        | Left | Right -> { X=0.0; Y=posDiff.Y }

    moveSymbol offset movePInfo.sym
    
        
let alignSymbols
    (wModel: BusWireT.Model) 
    (symbolToSize: Symbol) 
    (otherSymbol: Symbol)
    (smartHelpers: ExternalSmartHelpers) 
        : BusWireT.Model = 

    match getOppEdgePortInfo wModel symbolToSize otherSymbol with
    | None -> wModel
    | Some (movePortInfo, otherPortInfo) ->
        let symbol' = alignPorts movePortInfo otherPortInfo
        let model' = set (symbolOf_ symbolToSize.Id ) symbol' wModel
        smartHelpers.UpdateSymbolWires model' symbolToSize.Id

/// HLP23: To test this, it must be given two symbols interconnected by wires. It then resizes symbolToSize
/// so that the connecting wires are exactly straight
/// HLP23: It should work out the interconnecting wires (wires) from 
////the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has adjustable size).
/// HLP23: when this function is written replace teh XML comment by something suitable concisely
/// stating what it does.
let reSizeSymbol 
    (wModel: BusWireT.Model) 
    (symbolToSize: Symbol) 
    (otherSymbol: Symbol)
    (smartHelpers: ExternalSmartHelpers) 
        : BusWireT.Model =
    printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"
    let wires = getWiresBtwnSyms wModel symbolToSize otherSymbol

    // try to get two ports that are on opposite edges, if none found just use any two ports
    let resizePortInfo, otherPortInfo= 
        match getOppEdgePortInfo wModel symbolToSize otherSymbol with 
        | None ->
            let pA, pB = getPortAB wModel {symA=symbolToSize; symB=otherSymbol; wire=wires[0]}
            makePortInfo symbolToSize pA, makePortInfo symbolToSize pB
        | Some (pIA, pIB) -> (pIA, pIB)

    let h, w = 
        match resizePortInfo.side with
        | Left | Right -> otherPortInfo.portGap * (resizePortInfo.portDimension + 2.0*resizePortInfo.gap), resizePortInfo.w
        | Top | Bottom -> resizePortInfo.h, otherPortInfo.portGap * (resizePortInfo.portDimension + 2.0*resizePortInfo.topBottomGap)

    let scaledSymbol = setCustomCompHW h w symbolToSize 
    let scaledInfo = makePortInfo scaledSymbol resizePortInfo.port
    let symbol' = alignPorts scaledInfo otherPortInfo
    let model' = set (symbolOf_ symbolToSize.Id) symbol' wModel

    smartHelpers.UpdateSymbolWires model' symbolToSize.Id

/// Finds the optimal size and position for the selected symbol w.r.t. to its surrounding symbols
let optimiseSymbol 
    (wModel: BusWireT.Model) 
    (symbol: Symbol) 
    (smartHelpers: ExternalSmartHelpers) 
        : BusWireT.Model =
    
    wModel
