module SheetBeautifyHelpers

open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open SymbolUpdate
open Symbol
open Optics
open Operators
open BlockHelpers
open SymbolResizeHelpers
open EEExtensions
open DrawHelpers
open ModelType
open Sheet.SheetInterface
open Fable.React
open Fable.React.Props
open Elmish
// open HLPTick3

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team
// B3, B5, B6 T2 still trying
// lists the functions that must be written, R = Read, W = write. Where a value is RW two functions are needed one for Read and one for Write. These should be combined in a Lens (if possible).
// B3R, B3W RW Medium Read/write the order of ports on a specified side of a symbol
// B5R R Low The position of a port on the sheet. It cannot directly be written.
// B6R R Low The Bounding box of a symbol outline (position is contained in this)
// T2R R Low The number of distinct wire visible segments that intersect with one or more symbols. See Tick3.HLPTick3.visibleSegments for a helper. Count over all visible wire segments.

/// Identify a port from its component label and number.
/// Usually both an input and output port will mathc this, so
/// the port is only unique if it is known to be input or output.
/// used to specify the ends of wires, since tehee are known to be
/// connected to outputs (source) or inputs (target).
type SymbolPort = { Label: string; PortNumber: int }

/// convenience function to make SymbolPorts
let portOf (label:string) (number: int) =
    {Label=label; PortNumber = number}

// B1RW - The dimensions of a custom component symbol
let customComponentDimensionsLens : Lens<Symbol, (float * float)> =
    // Getter function: gets the width and height of a custom component symbol
    let get (sym: Symbol) = (sym.Component.W, sym.Component.H)
    // Setter function: returns a new Symbol with updated width and height
    let set (newDims: (float * float)) (sym: Symbol) =
        { sym with Component = { sym.Component with W = fst newDims; H = snd newDims } }

    (get, set)

// B2W - The position of a symbol on the sheet
let symbolPositionLens : Lens<Symbol, XYPos> =
    // Getter function: gets the position of a symbol on the sheet
    let get (sym: Symbol) = sym.Pos
    // Setter function: returns a new Symbol with updated position
    let set (newPos: XYPos) (sym: Symbol) = { sym with Pos = newPos }

    (get, set)


// B3R, B3W - Read and Write the order of ports on a specified side of a symbol
let portOrderLens (side: Edge) : Lens<PortMaps, string list option> =
    // Getter function: gets the list of port IDs for a specified side
    let get (portMaps: PortMaps) =
        Map.tryFind side portMaps.Order
    // Setter function: returns a new PortMaps with updated port order for the specified side
    let set (newOrderOpt: string list option) (portMaps: PortMaps) =
        match newOrderOpt with
        | Some newOrder ->
            let updatedOrder = Map.add side newOrder portMaps.Order
            { portMaps with Order = updatedOrder }
        | None -> portMaps  // If None, don't modify the portMaps

    (get, set)


// B4RW - The reverses state of the inputs of a MUX2
let mux2ReverseLens : Lens<Symbol, bool> =
    // Getter function: gets the reverse state of the inputs of a MUX2
    let get (sym: Symbol) = sym.Mux2.Reverse
    // Setter function: returns a new Symbol with updated reverse state
    let set (newReverse: bool) (sym: Symbol) = { sym with Mux2 = { sym.Mux2 with Reverse = newReverse } }

    (get, set)



// B5R - Read the position of a port on the sheet
let readPortPosition (sym: Symbol) (port: Port) : XYPos =
    let addXYPos (pos1: XYPos) (pos2: XYPos) : XYPos =
        { X = pos1.X + pos2.X; Y = pos1.Y - pos2.Y }
    let TopLeft = sym.Pos
    let offset = getPortPos sym port
    addXYPos TopLeft offset





// Helper function to apply scaling to the width and height if present
let applyScaling (width: float) (height: float) (hScale: float option) (vScale: float option) =
    let w = match hScale with
            | Some(scale) -> width * scale
            | None -> width
    let h = match vScale with
            | Some(scale) -> height * scale
            | None -> height
    (w, h)

// B6R - Read the Bounding box of a symbol outline
let readBoundingBox (symbol: Symbol) : BoundingBox =
    let (scaledWidth, scaledHeight) = applyScaling symbol.Component.W symbol.Component.H symbol.HScale symbol.VScale
    {
        TopLeft = symbol.Pos
        W = scaledWidth
        H = scaledHeight
    }

// B7RW - The rotation of a symbol
let symbolRotationLens : Lens<Symbol, float> =
    // Getter function: gets the rotation of a symbol
    let get (sym: Symbol) = sym.Rotation
    // Setter function: returns a new Symbol with updated rotation
    let set (newRotation: float) (sym: Symbol) = { sym with Rotation = newRotation }

    (get, set)

// B8RW - The flip state of a symbol
let symbolFlipLens : Lens<Symbol, bool> =
    // Getter function: gets the flip state of a symbol
    let get (sym: Symbol) = sym.Flip
    // Setter function: returns a new Symbol with updated flip state
    let set (newFlip: bool) (sym: Symbol) = { sym with Flip = newFlip }

    (get, set)


