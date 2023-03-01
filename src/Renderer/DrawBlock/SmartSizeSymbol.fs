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


///
/// start with symbol1 and symbol2
/// 
/// want to change symbol1
/// 
/// for each wire 
/// 
/// if there are multiple, pick the pair of edges which are parallel and opposite
/// i.e. left <> right // top <> bottom
/// 
/// get the port gap for symbol1 and symbol2
/// 
/// portDimension, gap, topBottomGap are constants
/// change h and w to change gap
/// 
/// portGap = func of h, w, edge -> func of sym and port
/// 

/// get the edge from symbol1 and the edge from symbol2 which are connected by wires
/// 
/// get the wires between symbol1 and symbol2 (done)
/// 
/// check for each wire if they are connecting opposite edges, return if yes, otherwise no
/// 


/// start with symbol1 and symbol2
/// 
/// want to change symbol1
/// 
/// 

// /// For a wire between SymbolA and SymbolB, return SymbolA's port and SymbolB's port if
// let tryGetEdgePort (model: BusWireT.Model) (wire: Wire) =
//     let ports = getPortsFrmWires model [wire]
//     let portA = fiterPortBySym 

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

let setCustomCompHW h w sym = 
    let hScale = w / sym.Component.W
    let vScale = h / sym.Component.H
    {sym with HScale=Some hScale; VScale=Some vScale}

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

    let sModel = wModel.Symbol
    let wires = getConnBtwnSyms wModel symbolToSize otherSymbol
    
    let getPortAB wireSyms =
        let ports = getPortsFrmWires wModel [wireSyms.wire]
        let portA = fiterPortBySym ports wireSyms.symA |> List.head
        let portB = fiterPortBySym ports wireSyms.symB |> List.head
        portA, portB

    let tryGetOppEdge wireSyms =
        let portA, portB = getPortAB wireSyms
        let edgeA = getSymbolPortOrientation wireSyms.symA portA
        let edgeB = getSymbolPortOrientation wireSyms.symB portB
        match edgeA, edgeB with
        | Top, Bottom | Bottom, Top | Left, Right | Right, Left -> Some (portA, portB)
        | _ -> None

    // try to get two ports that are on opposite edges, if none found just use any two ports
    let twoPorts = 
        wires 
        |> List.tryPick (fun w -> tryGetOppEdge {symA=symbolToSize; symB=otherSymbol; wire=w}) 
        |> Option.defaultValue (getPortAB {symA=symbolToSize; symB=otherSymbol; wire=wires[0]})

    let resizePort, otherPort = twoPorts
    let resizePortInfo = makePortInfo symbolToSize resizePort
    let otherPortInfo = makePortInfo otherSymbol otherPort
    printfn $"GAP: {otherPortInfo.portGap}"

    let h, w = 
        match resizePortInfo.side with
        | Left | Right -> otherPortInfo.portGap * (resizePortInfo.portDimension + 2.0*resizePortInfo.gap), resizePortInfo.w
        | Top | Bottom -> resizePortInfo.h, otherPortInfo.portGap * (resizePortInfo.portDimension + 2.0*resizePortInfo.topBottomGap)

    printfn $"Und: {symbolToSize.Component.H}, {symbolToSize.Component.W}"
    printfn $"Old: {resizePortInfo.h}, {resizePortInfo.w}"
    printfn $"New: {h}, {w}"
    let symbol' = setCustomCompHW h w symbolToSize // no change at the moment

    set (symbolOf_ symbolToSize.Id ) symbol' wModel

