module SheetBeautifyHelpers

open CommonTypes
open DrawModelType
open SymbolT
open SheetT
open Optics
open Symbol
open BlockHelpers

///B1R, dimension should be given in width * height
let getCustomSymbolDimension (symbol: SymbolT.Symbol): (float option*float option) =
    (symbol.HScale, symbol.VScale)
///B1W, dimension should be given in width * height, no option for input since no need to specifically write none
let writeCustomSymbolDimension(dimension:float option*float option) (symbol: SymbolT.Symbol): SymbolT.Symbol =
    {symbol with HScale = fst dimension; VScale = snd dimension}

///B1R B1W, lens for custom symbol dimension
let lensCustomeSymbolDimension = Lens.create getCustomSymbolDimension writeCustomSymbolDimension

/// B2W
let writeSymbolPosition (position: XYPos) (symbolID: ComponentId) (sheetModel: SheetT.Model): SheetT.Model =
    let symbol = sheetModel.Wire.Symbol.Symbols[symbolID]
    let newSymbol = {symbol with Pos = position}
    {sheetModel with Wire = {sheetModel.Wire with Symbol = {sheetModel.Wire.Symbol with Symbols = sheetModel.Wire.Symbol.Symbols.Add(symbolID, newSymbol)}}}


///B3R 
let readPortsOrder (symbol: SymbolT.Symbol) (edge: Edge): string list =
    symbol.PortMaps.Order[edge] 
///B3W
let writePortsOrder (edge: Edge) (order: string list) (symbol: SymbolT.Symbol): SymbolT.Symbol =
    {symbol with PortMaps = {symbol.PortMaps with Order = symbol.PortMaps.Order.Add(edge, order)}}
//let lensPortsOrder = Lens.create readPortsOrder writePortsOrder

/// B4R
let getReversedInputPortsMUX2 (symbol: SymbolT.Symbol): bool option =
    symbol.ReversedInputPorts
/// B4W
let writeReversedInputPortsMUX2 (reversed: bool option) (symbol: SymbolT.Symbol): SymbolT.Symbol =
    {symbol with ReversedInputPorts = reversed}
let lensReversedInputPortsMUX2 = Lens.create getReversedInputPortsMUX2 writeReversedInputPortsMUX2

/// B5R
let getPortPositionOnSheet (portId: PortId) (sheetModel: SheetT.Model): XYPos =
    getPortIdStr portId|>
    getPortLocation None sheetModel.Wire.Symbol

/// B6R
let getSymbolBoundingBoxOutline (symbol: SymbolT.Symbol): BoundingBox =
    getSymbolBoundingBox symbol

/// B7R
let getSymbolRotationState (symbol: SymbolT.Symbol): Rotation =
    symbol.STransform.Rotation
/// B7W
let writeSymbolRotationState (rotation: Rotation) (symbol: SymbolT.Symbol): SymbolT.Symbol =
    {symbol with STransform = {symbol.STransform with Rotation = rotation}}
let lensSymbolRotationState = Lens.create getSymbolRotationState writeSymbolRotationState

/// B8R
let getSymbolFlipState (symbol: SymbolT.Symbol): bool =
    symbol.STransform.Flipped
/// B8W
let writeSymbolFlipState (flipped: bool) (symbol: SymbolT.Symbol): SymbolT.Symbol =
    {symbol with STransform = {symbol.STransform with Flipped = flipped}}
let lensSymbolFlipState = Lens.create getSymbolFlipState writeSymbolFlipState


