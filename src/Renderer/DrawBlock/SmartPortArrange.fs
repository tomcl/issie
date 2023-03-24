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
open SmartHelpers

// HLP23: AUTHOR Ifte

// Helper function compiled later
type BusWireHelpers = {
    updateWire: Model -> Wire -> bool -> SmartAutorouteResult
    updateSymbolWires: Model -> ComponentId -> Model
    }

/// HLP23: AUTHOR Ifte
/// reArrangePorts rearranges ports and aligns wires across the whole sheet
let reArrangePorts
    (wModel: BusWireT.Model)
    (sizeHelper: SmartSizeSymbol.BusWireHelpers)
    (portHelper: SmartPortOrder.BusWireHelpers)
        : BusWireT.Model =

    (wModel, portHelper)
    ||> SmartPortOrder.sheetReOrderPorts
    |> SmartSizeSymbol.sheetReSizeSymbol <| sizeHelper