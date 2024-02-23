module SheetBeautifyHelpers

open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open Optics
open Optic
open Operators

// B1R, B1W
// The dimensions of a custom component symbol
let getSymbolDimensions (symbol: Symbol) : (float * float) =
    (symbol.Component.W, symbol.Component.H)

let setSymbolDimensions (symbol: Symbol) (width, height) =
    { symbol with Component = { symbol.Component with H = height; W = width }}



// B2W
// The position of a symbol on the sheet
// TODO: Is this too simple? What are the other things to take into consideration?
let setSymbolPosition (symbol: Symbol) (pos : XYPos) =
    symbol |> set posOfSym_ pos



// B3R, B3W
// Read/write the order of ports on a specified side of a symbol
let getSymbolPortOrder (symbol : Symbol) (side : Edge) : string list =
    symbol.PortMaps.Order[side]

let setSymbolPortOrder (symbol : Symbol) (side : Edge) (portOrder : string list) : Symbol =
    { symbol with PortMaps = {symbol.PortMaps with Order = symbol.PortMaps.Order.Add(side, portOrder)}}


// B4R, B4W
// The reverses state of the inputs of a MUX2
let getMux2Reversed (mux2 : Symbol) : bool option =
    mux2.ReversedInputPorts

let setMux2Reversed (mux2 : Symbol) (reversed : bool option) : Symbol =
    { mux2 with ReversedInputPorts = reversed }


// B5R
// The position of a port on the sheet. It cannot directly be written
let getPortPosition (symbol: Symbol) (port: Port) : XYPos =
    Symbol.getPortPos symbol port


// B6R
// The Bounding box of a symbol outline (position is contained in this)
let getSymbolBoundingBox (symbol: Symbol) : BoundingBox =
    Symbol.getSymbolBoundingBox symbol


// B7R, B7W
// The rotation state of a symbol
let getSymbolRotation (symbol: Symbol) : Rotation =
    symbol.STransform.Rotation

let setSymbolRotation (symbol: Symbol) (rotation: Rotation) : Symbol =
    { symbol with STransform = {symbol.STransform with Rotation = rotation} }


// B8R, B8W
// The flip state of a symbol
let getSymbolFlip (symbol: Symbol) : bool =
    symbol.STransform.Flipped

let setSymbolFlip (symbol: Symbol) (flip: bool) : Symbol =
    { symbol with STransform = {symbol.STransform with Flipped = flip} }



