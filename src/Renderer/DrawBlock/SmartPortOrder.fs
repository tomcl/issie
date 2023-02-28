module SmartPortOrder
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
    HLP23: This module will normally be used exclusively by team member doing the "smart port reorder" 
    part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    Normally it will update multiple wires and symbols in the BusWire model so could use the SmartHelper 
    functions for this purpose.
*)

/// To test this, it must be given two symbols interconnected by wires. It then reorders the ports on
/// symbolToOrder so that the connecting wires do not cross.
/// It should work out the interconnecting wires (wiresToOrder) from 
/// the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has re-orderable ports).
let updatePortMapList (sym:Symbol) (index:int) (portId:string) (edge:Edge) =
    let PortMapOrder = sym.PortMaps.Order

    PortMapOrder[edge]
    |> List.updateAt index portId
  


let findMinIndex (symbol: Symbol) (portList) (edge: Edge) =

    let portList' = 
            portList
            |> List.collect (fun id -> [symbol.PortMaps.Order[edge] |> List.findIndex (fun elm -> elm = id)])

    match edge with
    | Top | Right -> 
        List.min portList'
    | Bottom | Left -> 
        List.max portList'

let findMaxIndex (symbol: Symbol) (portList) (edge: Edge) =

    let portList' = 
            portList
            |> List.collect (fun id -> [symbol.PortMaps.Order[edge] |> List.findIndex (fun elm -> elm = id)])

    match edge with
    | Top | Right -> 
        List.max portList'
    | Bottom | Left ->
        List.min portList'

// clean this by putting match inside if/elif//else
// Use Options instead of .find
let findIndexShifted (interWires: list<Wire>) (symbolToOrder: Symbol) 
    (otherSymbol: Symbol)
    (inputEdge: Edge)
    (outputEdge: Edge) = 

    let outputPortList = 
        interWires
        |> List.map(fun wire -> string wire.OutputPort)
    
    let inputPortList = 
        interWires
        |> List.map(fun wire -> string wire.InputPort)

    //need to find which edge is bigger and store that in a variable
    let outputLength = otherSymbol.PortMaps.Order[outputEdge].Length - 1
    let inputLength = symbolToOrder.PortMaps.Order[inputEdge].Length - 1

    match inputEdge, outputEdge with
    | Top, Bottom | Bottom, Top | Left, Right | Right, Left | Left, Top | Bottom, Right ->
        if (inputLength = outputLength) then
            0
        elif inputLength > outputLength then
            let inputMinIndex = findMinIndex symbolToOrder inputPortList inputEdge
            -(inputLength - inputMinIndex)
        else
            let outputMinIndex = findMinIndex otherSymbol outputPortList outputEdge
            outputMinIndex
    
    | Top, Top | Top, Left | Right, Bottom ->
        if (inputLength = outputLength) then
            0
        elif inputLength > outputLength then
            let inputMaxIndex = findMaxIndex symbolToOrder inputPortList inputEdge
            -(inputLength - inputMaxIndex)
        else
            let outputMaxIndex = findMaxIndex otherSymbol outputPortList outputEdge
            outputMaxIndex

    | Bottom, Bottom | Left, Left | Bottom, Left | Left, Bottom ->
        if (inputLength = outputLength) then
                0
            elif inputLength > outputLength then
                let inputMinIndex = findMinIndex symbolToOrder inputPortList inputEdge
                -(inputLength - inputMinIndex)
            else
                let outputMaxIndex = findMaxIndex otherSymbol outputPortList outputEdge
                outputMaxIndex

    | Right, Right | Top, Right | Right, Top ->
        if (inputLength = outputLength) then
            0
        elif inputLength > outputLength then
            let inputMaxIndex = findMaxIndex symbolToOrder inputPortList inputEdge
            -(inputLength - inputMaxIndex)
        else
            let outputMinIndex = findMinIndex otherSymbol outputPortList outputEdge
            outputMinIndex


let changePortOrder (wModel: BusWireT.Model)
    (symbolToOrder: Symbol) 
    (otherSymbol: Symbol)
    (interWires: list<Wire>) =

    let sModel = wModel.Symbol

    printfn " PortMaps.Order before: %A" symbolToOrder.PortMaps.Order[Bottom]

    let updatedSymbol = 
        (symbolToOrder, interWires)
        ||> List.fold (fun symbol wire -> 

            //need to change port maps and .Port global map when permuting the order
            // we may need to also factor in rotated symbols which change the orientation but stay the same on paper

            let outputPortId = wire.OutputPort // port id of wire exiting
            let inputPortId = wire.InputPort // port id of  wire entering
            
            let outputSymbolRot = otherSymbol.STransform.Rotation  // TODO
            let inputSymbolRot = symbolToOrder.STransform.Rotation // TODO
            
            let outputEdge = getPortOrientation sModel  (OutputId outputPortId)
            let inputEdge = getPortOrientation sModel  (InputId inputPortId)

            //printfn "Output:%A Input:%A" outputEdge inputEdge
            //printfn " PortMaps.Order: %A" symbolToOrder.PortMaps.Order[inputEdge]

            let outputPortIndex =
                otherSymbol.PortMaps.Order[outputEdge] 
                |> List.findIndex (fun elm -> elm = string outputPortId)

            // returns new symbol
            let newPortMapOrder =
                match inputEdge, outputEdge with
                | Top, Bottom | Bottom, Top | Left, Right | Right, Left | Top, Top | Bottom, Bottom | Right, Right | Left, Left
                | Top, Left | Left, Top | Top, Right | Bottom, Left | Left, Bottom | Right, Top | Right, Bottom | Bottom, Right ->

                    let remainder = findIndexShifted interWires symbolToOrder otherSymbol inputEdge outputEdge
                    let indexChange = symbolToOrder.PortMaps.Order[inputEdge].Length - (outputPortIndex) - 1 + remainder
                    printfn " Port Index: %A  %A" outputPortIndex indexChange
                    printfn " Port Index before : %A \n %A" indexChange symbolToOrder.PortMaps.Order[inputEdge].[indexChange]
                    let newPortMapList = updatePortMapList symbol indexChange (string inputPortId) inputEdge
    
                    let newOrder = 
                        symbol.PortMaps.Order
                            |> Map.add inputEdge newPortMapList

                    newOrder
            
            { symbol with PortMaps = { symbol.PortMaps with Order = newPortMapOrder } }
            )

    updatedSymbol

    // | Top, Right | Bottom, Left -> untested but done | Bottom, Right| Left, Bottom -| Right, Top | Right, Bottom 

/// Find all the InterConnecting Wires between 2 symbols given WireModel and the 2 symbols
let findInterconnectingWires (wireList:List<Wire>) (sModel)
    (symbolToOrder:Symbol) 
    (otherSymbol:Symbol) (getAllConnections:int) =
        
    wireList
    |> List.filter (fun value ->

        let inputPortHostId = string sModel.Ports[string value.InputPort].HostId
        let outputPortHostId = string  sModel.Ports[string value.OutputPort].HostId

        let symbolToOrderId = string symbolToOrder.Id
        let otherSymbolId = string otherSymbol.Id
        
        if (getAllConnections = 0) then
            (inputPortHostId = symbolToOrderId) && (outputPortHostId = otherSymbolId)
        else
            if ((inputPortHostId = symbolToOrderId) && (outputPortHostId = otherSymbolId)) then true
            elif ((inputPortHostId = otherSymbolId) && (outputPortHostId = symbolToOrderId)) then true
            else false

        )


type BusWireHelpers = {
    updateWire: Model -> Wire -> bool -> Wire
    updateSymbolWires: Model -> ComponentId -> Model
    }

let reOrderPorts 
    (wModel: BusWireT.Model) 
    (symbolToOrder: Symbol) 
    (otherSymbol: Symbol) 
    (busWireHelper: BusWireHelpers)
        : BusWireT.Model =

    printfn $"ReorderPorts: ToOrder:{symbolToOrder.Component.Label} {symbolToOrder.Id }, Other:{otherSymbol.Component.Label}"

    let sModel = wModel.Symbol

    let wireList =
        wModel.Wires
        |> Map.toList
        |> List.map snd

    let allWires = findInterconnectingWires wireList sModel symbolToOrder otherSymbol 1
    let wiresToOrder = findInterconnectingWires allWires sModel symbolToOrder otherSymbol 0 

    printfn "Wire length is %A" wiresToOrder.Length

    let symbol' = changePortOrder wModel symbolToOrder otherSymbol wiresToOrder

    let newModel = 
        {wModel with 
            Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
        }

    printfn " PortMaps.Order after: %A" symbol'.PortMaps.Order[Left]
    let wModel' = busWireHelper.updateSymbolWires newModel symbol'.Id

    wModel'

    
let sheetReOrderPorts 
    (wModel: BusWireT.Model) 
    (busWireHelper: BusWireHelpers)
        : BusWireT.Model =

    printfn $"Ordering the whole sheet"

    let sModel = wModel.Symbol

    let wireList =
        wModel.Wires
        |> Map.toList
        |> List.map snd

    let sModel' = 
        (sModel, wireList)
        ||> List.fold (fun symbol wire ->

            let outputPortId = string wire.OutputPort // port id of wire exiting
            let inputPortId = string wire.InputPort // port id of  wire entering

            let symbolToOrder = getSymbol symbol inputPortId
            let otherSymbol = getSymbol symbol outputPortId

            let symbol' = changePortOrder wModel symbolToOrder otherSymbol [wire]

            {sModel with Symbols = Map.add symbol'.Id symbol' symbol.Symbols}
        )


    //let symbol' = changePortOrder wModel symbolToOrder otherSymbol wiresToOrder

    let newModel = 
        {wModel with 
            Symbol = sModel'
        }

    //printfn " PortMaps.Order after: %A" symbol'.PortMaps.Order[Left]

    let wModel' = busWireHelper.updateSymbolWires newModel symbol'.Id

    wModel


