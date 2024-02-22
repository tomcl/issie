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
let getMux2IsReversed (sym : SymbolT.Symbol) : bool option =
    sym.ReversedInputPorts

let setMux2IsReversed (sym : SymbolT.Symbol) (reverseState : bool option) : SymbolT.Symbol =
    { sym with ReversedInputPorts = reverseState }

// B5R R Low 
// The position of a port on the sheet. It cannot directly be written.
let getPortPosOnSheet (sym : SymbolT.Symbol) (portId : string) : XYPos =
    let port = sym.Component.getPort(PortId portId)
    match port with
    | Some port -> getPortPos sym port
    | None -> failwithf "Port with id %s not found in symbol" portId
    
// B6R R Low 
// The Bounding box of a symbol outline (position is contained in this)
let getSymOutlineBoundingBox (sym : SymbolT.Symbol) : BoundingBox =
    getSymbolBoundingBox sym

// B7R, B7W RW Low 
// The rotation state of a symbol
let getSymRotation (sym : SymbolT.Symbol) : Rotation =
    sym.STransform.Rotation

let setSymRotation (sym : SymbolT.Symbol) (rotation : Rotation) : SymbolT.Symbol =
    { sym with STransform = { sym.STransform with Rotation = rotation } }

// B8R, B8W RW Low 
// The flip state of a symbol
let isSymFlip (sym : SymbolT.Symbol) : bool =
    sym.STransform.Flipped

let setSymFlip (sym : SymbolT.Symbol) (isFlipped : bool) : SymbolT.Symbol =
    { sym with STransform = { sym.STransform with Flipped = isFlipped } }

// T1R R Low 
// The number of pairs of symbols that intersect each other. See Tick3 for a related
// function. Count over all pairs of symbols.


// T2R R Low 
// The number of distinct wire visible segments that intersect with one or more
// symbols. See Tick3.HLPTick3.visibleSegments for a helper. Count over all visible wire
// segments.


// T3R R Low 
// The number of distinct pairs of segments that cross each other at right angles. Does
// not include 0 length segments or segments on same net intersecting at one end, or
// segments on same net on top of each other. Count over whole sheet.


// T4R R Medium 
// Sum of wiring segment length, counting only one when there are N same-net
// segments overlapping (this is the visible wire length on the sheet). Count over whole
// sheet.