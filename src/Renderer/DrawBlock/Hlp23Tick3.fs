module Hlp23Tick3
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT

/// return Some to replace drawSymbol by your own code
let drawSymbolHook (symbol:Symbol) (theme:ThemeType) =
    None

/// return some to replace updateWire by your own code
let updateWire (model: BusWireT.Model) (wire: Wire) (routeInputEnd:bool) =
    None
