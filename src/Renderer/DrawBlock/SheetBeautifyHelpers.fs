module SheetBeautifyHelpers

open Optics

open CommonTypes
open DrawModelType

open Symbol

type Dimension = {
    W: float
    H: float
}

// B1R, B1W RW Low
// returns the lens of dimensions of a custom component symbol
let getCCSymbolDimension (sym : SymbolT.Symbol) =
    match (sym.HScale, sym.VScale) with
    | (None, None) -> { W = sym.Component.W ; H = sym.Component.H }
    | (Some hScale, Some vScale) -> { W = sym.Component.W * hScale ; H = sym.Component.H * vScale }
    | (Some hScale, None) -> { W = sym.Component.W * hScale ; H = sym.Component.H }
    | (None, Some vScale) -> { W = sym.Component.W ; H = sym.Component.H * vScale }

let setCCSymbolDimension (dim : Dimension) (sym : SymbolT.Symbol) =
    { sym with HScale = Some (dim.W/sym.Component.W); VScale = Some (dim.H/sym.Component.H) }
    // keep the W and H of the component the same, as this is calculated when creating the custom component
    // if the user performs an illegal sizing, we can revert back to this original size

let CCSymbolDimensions_ = Lens.create getCCSymbolDimension setCCSymbolDimension

// B2W Med
// The position of a symbol on the sheet
let setSymbolPosOnSheet (sheetModel : SheetT.Model) (symId : ComponentId) (pos : XYPos) : SheetT.Model = 
    let updateSymPos (sym : SymbolT.Symbol) : SymbolT.Symbol = { sym with Pos = pos }
    let newSymModel: SymbolT.Model = SymbolUpdate.updateSymbol updateSymPos symId sheetModel.Wire.Symbol

    sheetModel
    |> Optic.set SheetT.symbol_ newSymModel

// B3R, B3W RW Med
// Read/write the order of ports on a specified side of a symbol
// used Symbols.fs
let getSymPortOrder (sym : SymbolT.Symbol) (side : Edge) : string list =
    sym.PortMaps.Order[side]

let setSymPortOrder (sym : SymbolT.Symbol) (side : Edge) (orderedPorts : string list) : SymbolT.Symbol =
    let newPortOrder = sym.PortMaps.Order |> Map.add side orderedPorts

    { sym with PortMaps = { sym.PortMaps with Order = newPortOrder } }

// B4R, B4W RW Low 
// The reverses state of the inputs of a MUX2

// B5R R Low 
// The position of a port on the sheet. It cannot directly be written.

// B6R R Low 
// The Bounding box of a symbol outline (position is contained in this)

// B7R, B7W RW Low 
// The rotation state of a symbol

// B8R, B8W RW Low 
// The flip state of a symbol
