module SmartPortOrder
open System
open BusWireUpdateHelpers
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
open SymbolUpdatePortHelpers

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
/// Tt should work out the interconnecting wires (wiresToOrder) from
////the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has re-orderable ports).
let reOrderPorts (wModel: BusWireT.Model) (symbolToOrder: Symbol) (otherSymbol: Symbol) : BusWireT.Model =
    printfn $"ReorderPorts: ToOrder:{symbolToOrder.Component.Label}, Other:{otherSymbol.Component.Label}"
    let sModel = wModel.Symbol
    printfn $"Input ports:{symbolToOrder.Component.InputPorts}, Output ports:{symbolToOrder.Component.OutputPorts}"
    let wiresToOrder1 = getConnectedWires wModel [ symbolToOrder.Id ] |> Set
    let wiresToOrder2 = getConnectedWires wModel [ otherSymbol.Id ] |> Set
    let wiresToOrder = Set.intersect wiresToOrder1 wiresToOrder2 |> Set.toList

    let symbolAIn = otherSymbol.Component.InputPorts
    let symbolAout = otherSymbol.Component.OutputPorts
    let symbolBIn = symbolToOrder.Component.InputPorts
    let symbolBOut = symbolToOrder.Component.OutputPorts

    let portConnections =
        List.map (fun x -> (x.OutputPort, x.InputPort)) (wiresToOrder: Wire list)

    let list1, list2 = List.unzip portConnections

    let symbolAPortMap =
        symbolAout |> List.map (fun port -> (port.Id, port.PortNumber)) |> Map.ofList

    let symbolBPortMap =
        symbolBIn |> List.map (fun port -> (port.Id, port.PortNumber)) |> Map.ofList

    let stringPortConnections =
        List.map (fun (outputId, inputId) -> (inputId.ToString(), outputId.ToString())) portConnections

    let PortNumberConnections =
        List.map
            (fun (inputId, outputId) -> 
                let inputPortNumber = symbolBPortMap.[inputId]
                let outputPortNumber = symbolAPortMap.[outputId]
                (outputPortNumber, inputPortNumber))
            stringPortConnections
    
    let PortNumberConnections2 = List.map (fun (x,y) -> (x |> Option.defaultValue 0, y |> Option.defaultValue 0)) PortNumberConnections
    
    let xyPos =
        { X = 1875.953189889666
          Y = 1523.493459541926 }
    
    (*let filterList (stringList: string list) (tupleList: (int * int) list) =
        let indexSet = Set.ofList [ for (_, second) in tupleList -> second ]
        List.filter (fun (_, index) -> Set.contains index indexSet) (List.indexed stringList)
        |> List.map fst*)
        
    let reorderStringIntList (intList: (int* int) list) (stringIntList: (string * int) list) =
        let orderSet = intList |> List.map snd |> Set.ofSeq
        List.sortBy (fun (_, second) -> Seq.tryFindIndex ((=) second) orderSet |> Option.defaultValue Int32.MaxValue) stringIntList

    let reorderPortList (idList: string List) (portConnections: (int*int) List): string List =
        let idListIndex = List.mapi (fun i x -> (x,i)) idList
        //let toBeReordered = filterList idList
        let reorderedList = reorderStringIntList  portConnections idListIndex
        //let originalPositionTable = List.mapi (fun i x -> (i,x)) idList |> Map.ofList
        List.map (fun (first, _) -> first) reorderedList
        
        
    let updatedMapOrder = Map.map (fun _ L -> List.rev L) symbolToOrder.PortMaps.Order
    let updatedPortMaps = {symbolToOrder.PortMaps with Order = updatedMapOrder}
    let updatedSymbol = {symbolToOrder with PortMaps = updatedPortMaps}
    
    //let updatePortPos (sym:Symbol) (pos:XYPos) (portId: string): Symbol
    //let getPortPosWithIndex (sym: Symbol) portsNumber side portIndex: XYPos =
    //let updatedSymbol = List.fold (fun partialUpdatedSymbol x ->  updatePortPos partialUpdatedSymbol partialUpdatedSymbol.Pos (x.ToString())) symbolToOrder list2
    //let portPositions = List.map getPortPosWithIndex symbolToOrder Left list2
    
    printfn $"Ports: {PortNumberConnections}"
    // replace this with correct wires
    printfn $"Symbol Pos:{symbolToOrder.Pos}"
    printfn $"Number of wires to reorder:{wiresToOrder.Length}"
    printfn $"H:{symbolToOrder.Component.H}, W:{symbolToOrder.Component.W}"
    printfn $"Symbol map: {symbolToOrder.PortMaps.Order}"

    let symbol' = updatedSymbol // no change at the moment
    // HLP23: This could be cleaned up using Optics - see SmartHelpers for examples
    { wModel with
        Wires = wModel.Wires // no change for now, but probably this function should use update wires after reordering.
        // to make that happen the tyest function which calls this would need to provide an updateWire
        // function to this as a parameter (as was done in Tick3)
        Symbol = { sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols } }
