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
/// reArrangePorts takes two symbols connected by wires and resizes symbolToSize so that any wires that
/// are nearly straight become straight
let reArrangePorts
    (wModel: BusWireT.Model)
    (sizeHelper: SmartSizeSymbol.BusWireHelpers)
    (portHelper: SmartPortOrder.BusWireHelpers)
        : BusWireT.Model =

    //let orderedModel = SmartPortOrder.reOrderPorts wModel symbolToArrange otherSymbol portHelper
    //let symMap = orderedModel.Symbol.Symbols
    //let symbolToArrange' = symMap[symbolToArrange.Id]
    //let otherSymbol' = symMap[otherSymbol.Id]
    //SmartSizeSymbol.reSizeSymbol orderedModel symbolToArrange' otherSymbol' sizeHelper

    (wModel, portHelper)
    ||> SmartPortOrder.sheetReOrderPorts
    |> SmartSizeSymbol.sheetReSizeSymbol <| sizeHelper
