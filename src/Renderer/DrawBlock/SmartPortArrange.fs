module SmartPortArrange

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

/// HLP23: AUTHOR Ifte
/// reArrangePorts rearranges ports and aligns wires across the whole sheet
let reArrangePorts
    (wModel: BusWireT.Model)
    (wireHelper: SmartHelpers.BusWireHelpers)
        : BusWireT.Model =

    (wModel, wireHelper)
    ||> SmartPortOrder.sheetReOrderPorts
    |> SmartSizeSymbol.sheetReSizeSymbol <| wireHelper