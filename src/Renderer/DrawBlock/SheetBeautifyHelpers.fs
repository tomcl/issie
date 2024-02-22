module SheetBeautifyHelpers

open Optics
open CommonTypes
open DrawModelType

type Dimension = {
    W: float
    H: float
}

// B1R, B1W
// returns the lens of dimensions of a custom component symbol
let getCCSymbolDimension (sym : SymbolT.Symbol) =
    { W = sym.Component.W; H = sym.Component.H }

let setCCSymbolDimension (dim : Dimension) (sym : SymbolT.Symbol) =
    { sym with Component = { sym.Component with W = dim.W; H = dim.H } }

let CCSymbolDimensions_ = Lens.create getCCSymbolDimension setCCSymbolDimension

// B2W
// The position of a symbol on the sheet
let setSymbolPos (sheetModel : SheetT.Model) (symId : ComponentId) (pos : XYPos) : SheetT.Model = 
    let updateSymPos (sym : SymbolT.Symbol) : SymbolT.Symbol = { sym with Pos = pos }
    let newSymModel: SymbolT.Model = SymbolUpdate.updateSymbol updateSymPos symId sheetModel.Wire.Symbol

    sheetModel
    |> Optic.set SheetT.symbol_ newSymModel

// B3R, B3W
// Read/write the order of ports on a specified side of a symbol