module SmartPortOrder
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
    
    let wiresToOrder1 = getConnectedWires wModel [symbolToOrder.Id] |> Set
    let wiresToOrder2 = getConnectedWires wModel [otherSymbol.Id] |> Set
    let wiresToOrder = Set.intersect wiresToOrder1 wiresToOrder2 |> Set.toList
    
    let portConnections = List.map (fun x -> (x.InputPort, x.OutputPort)) (wiresToOrder : Wire list)
    let list1, list2 = List.unzip portConnections
    
    printfn $"Ports: {portConnections}"
    
    // replace this with correct wires
    printfn $"Number of connected wires:{wiresToOrder.Length}"
    let symbol' = symbolToOrder // no change at the moment
    // HLP23: This could be cleaned up using Optics - see SmartHelpers for examples
    { wModel with
        Wires = wModel.Wires // no change for now, but probably this function should use update wires after reordering.
        // to make that happen the tyest function which calls this would need to provide an updateWire
        // function to this as a parameter (as was done in Tick3)
        Symbol = { sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols } }
