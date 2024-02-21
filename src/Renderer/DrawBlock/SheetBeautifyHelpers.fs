module SheetBeautifyHelpers

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open CommonTypes
open BlockHelpers


//B1R
let readCustCompDim (sym: Symbol) =
    (sym.Component.H, sym.Component.W)

//B1W
//let writeCustCompDim (h: float) (w: float) (sym: Symbol) =
//    BlockHelpers.setCustomCompHW h w sym

let writeCustCompDim (h: float) (w: float) (sym: Symbol) =
    let updatedComponent = { sym.Component with H = h; W = w }
    { sym with Component = updatedComponent }

//B2W
let writeSymbolPos (symbolModel: SymbolT.Model) (x: float) (y: float) (compId: ComponentId) =
    match Map.tryFind compId symbolModel.Symbols with
    | Some symbol ->
        let updatedPos = { X = x; Y = y }
        let updatedSymbol = { symbol with Pos = updatedPos }
        { symbolModel with Symbols = symbolModel.Symbols.Add(compId, updatedSymbol) }
    | None -> symbolModel // Handle case where component ID is not found


//B3R
//B3W

//B4R

//B4W


