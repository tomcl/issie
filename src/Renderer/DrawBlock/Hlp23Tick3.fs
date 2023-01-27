module Hlp23Tick3
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT

// open stuff possiblly needed for drawSymbol
open Fable.React
open Fable.React.Props
open Elmish
open CommonTypes
open DrawHelpers
open Symbol

/// Record containing BusWire helper functions that might be needed by updateWireHook
/// functions are fed in at the updatewireHook function call in BusWireUpdate.
/// This is needed because HLPTick3 is earlier in teh F# compile order than Buswire so
/// the functions cannot be calle directly.
/// Add functions as needed.
type Tick3BusWireHelpers = {
    AutoRoute: BusWireT.Model -> Wire -> Wire
    ReverseWire: Wire -> Wire
    }


/// return Some reactElement list to replace drawSymbol by your own code
let drawSymbolHook 
        (symbol:Symbol) 
        (theme:ThemeType) 
        : ReactElement list option=
    None

/// return Some newWire to replace updateWire by your own code defined here
let updateWireHook 
        (model: BusWireT.Model) 
        (wire: Wire) 
        (routeInputEnd:bool) 
        (tick3Helpers: Tick3BusWireHelpers)
        : Wire option=
    None
