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

// let iterator key value sModel = 
//     printfn "PortMaps.Order Control"

//     value 
//     |> List.iter( fun x -> printfn "%A" sModel.Ports[x])

// symbolToOrder.PortMaps.Order |> Map.filter(fun key value -> key = Bottom)
//                                 |> Map.iter (fun key value -> iterator key value sModel)


let changePortOrder (wModel: BusWireT.Model)
    (symbolToOrder: Symbol) 
    (otherSymbol: Symbol)
    (interWires: list<Wire>) =

    let sModel = wModel.Symbol
    let otherSymbolPorts = otherSymbol.Component.OutputPorts
    let symbolToOrderPorts = symbolToOrder.Component.InputPorts


    //get the wire and find the edge that it's on and then find the index of that input port
    // do it for the outport port as well and check if the indexes match then they are straight if not then crossing

    let crossing_wires = 
        interWires
        |> List.collect (fun value -> 

            //need to change port maps and .Port global map 
            // when permuting the order
            // do this for top and bottom
            // we may need to also factor in rotated symbols which change the orientation but stay the same on paper

            let outputPortId = value.OutputPort // port id of wire exiting
            let inputPortId = value.InputPort // port id of  wire entering

            let outputSymbol = sModel.Ports[string outputPortId].HostId
            let inputSymbol = sModel.Ports[string inputPortId].HostId

            let outputSymbolPortMaps = sModel.Symbols[ComponentId outputSymbol].PortMaps
            let inputSymbolPortMaps = sModel.Symbols[ComponentId inputSymbol].PortMaps

            let outputEdge = getPortOrientation sModel  (OutputId outputPortId)
            let inputEdge = getPortOrientation sModel  (InputId inputPortId)
            
            printfn "%A %A" outputEdge inputEdge
            []
            )

    [1]


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



let reOrderPorts 
    (wModel: BusWireT.Model) 
    (symbolToOrder: Symbol) 
    (otherSymbol: Symbol) 
        : BusWireT.Model =


    printfn $"ReorderPorts: ToOrder:{symbolToOrder.Component.Label} {symbolToOrder.Id }, Other:{otherSymbol.Component.Label}"

    let sModel = wModel.Symbol

    let wireList =
        wModel.Wires
        |> Map.toList
        |> List.map snd

    
    let allWires = findInterconnectingWires wireList sModel symbolToOrder otherSymbol 1
    let wiresToOrder = findInterconnectingWires allWires sModel symbolToOrder otherSymbol 0 

    printfn "ALl length is %A" allWires.Length
    printfn "Wire length is %A" wiresToOrder.Length

    //let ans = changePortOrder wModel symbolToOrder otherSymbol wiresToOrder


    let symbol' = symbolToOrder // no change at the moment
    // HLP23: This could be cleaned up using Optics - see SmartHelpers for examples
    {wModel with 
        Wires = wModel.Wires // no change for now, but probably this function should use update wires after reordering.
                             // to make that happen the tyest function which calls this would need to provide an updateWire
                             // function to this as a parameter (as was done in Tick3)
        Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
    }

