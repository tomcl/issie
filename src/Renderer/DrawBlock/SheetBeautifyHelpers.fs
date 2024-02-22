module SheetBeautifyHelpers

open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open Optics

// B1R, B1W
// The dimensions of a custom component symbol
let getSymbolDimensions (symbol: Symbol) : (float * float) =
    let scaledValues = symbol.getScaledDiagonal
    (scaledValues.X, scaledValues.Y)

let setSymbolDimensions (symbol: Symbol) (width, height) =
    { symbol with Component = { symbol.Component with H = height; W = width }}



// B2W
// The position of a symbol on the sheet
// TODO: Is this too simple? What are the other things to take into consideration?
let setSymbolPosition (symbol: Symbol) (pos : XYPos) =
    { symbol with Pos = pos }



// B3R, B3W
// Read/write the order of ports on a specified side of a symbol
let getSymbolPortOrder (symbol : Symbol) (side : Edge) : string list =
    match symbol.PortMaps.Order |> Map.tryFind side with
    | Some portsOnEdge -> portsOnEdge
    | None -> []


// B4R, B4W
// The reverses state of the inputs of a MUX2

// B5R
// The position of a port on the sheet. It cannot directly be written
