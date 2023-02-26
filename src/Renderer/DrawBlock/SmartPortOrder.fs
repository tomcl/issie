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


let changePortOrder (wModel: BusWireT.Model)
    (symbolToOrder: Symbol) 
    (otherSymbol: Symbol)
    (interWires: list<Wire>) =

    let sModel = wModel.Symbol

    printfn " PortMaps.Order before: %A" symbolToOrder.PortMaps.Order[Bottom]

    let updatedSymbol = 
        (symbolToOrder, interWires)
        ||> List.fold (fun symbol wire -> 

            //need to change port maps and .Port global map 
            // when permuting the order
            // do this for top and bottom
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

            //printfn " Port Index:%A orderLength: %A" outputPortIndex symbolToOrder.PortMaps.Order[inputEdge].Length

            printfn " Port Index before : %A \n %A" outputPortIndex symbolToOrder.PortMaps.Order[inputEdge].[outputPortIndex]

            // returns new symbol
            let newPortMapOrder =
                match outputEdge, inputEdge with
                | Top, Bottom | Bottom, Top ->

                    let indexChange = symbolToOrder.PortMaps.Order[inputEdge].Length - outputPortIndex - 1
                    let newPortMapList = updatePortMapList symbol indexChange (string inputPortId) inputEdge
              
                    printfn " Port Index after: %A \n %A" outputPortIndex newPortMapList[outputPortIndex]
       
                    let newOrder = 
                        symbol.PortMaps.Order
                            |> Map.add inputEdge newPortMapList

                    //printfn " Port Index after2: %A" newOrder[inputEdge].[outputPortIndex]

                    newOrder
            
                | _, _ -> symbolToOrder.PortMaps.Order
            

            { symbol with PortMaps = { symbol.PortMaps with Order = newPortMapOrder } }
            
            )

    printfn " PortMaps.Order after: %A" updatedSymbol.PortMaps.Order[Bottom]

    updatedSymbol


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

    let wModel' = busWireHelper.updateSymbolWires newModel symbol'.Id

    wModel'

    


