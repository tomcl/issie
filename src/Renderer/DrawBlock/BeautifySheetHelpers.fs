module BeautifySheetHelpers

open DrawModelType
open CommonTypes
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Optics
open Optics.Operators
open SymbolHelpers
open BlockHelpers
open Symbol

let symbolMap_: Lens<SheetT.Model, Map<ComponentId, Symbol>> =
    Lens.create (fun m -> m.Wire.Symbol.Symbols) (fun v m ->
        { m with Wire.Symbol = { m.Wire.Symbol with Symbols = v } })

/// B1R: The dimensions of a custom component symbol
let getCustomComponentSymbolDims (sym: Symbol) : XYPos =
    // directly return the third element from the result of getCustomSymCorners
    // format: [|{X=0.0;Y=0.0}; {X=0.0;Y=yDim'}; {X=xDim';Y=yDim'}; {X=xDim';Y=0.0}|]
    (getCustomSymCorners sym).[2]

/// B1W: The dimensions of a custom component symbol
let setCustomComponentSymbolDims (sym: Symbol) (pos: XYPos) : Symbol =
    // Note: do not modify h and w as they are the default dimensions of the symbol.
    // We only scale them as needed
    let hScale = pos.X / sym.Component.H
    let vScale = pos.Y / sym.Component.W
    { sym with HScale = Some hScale; VScale = Some vScale }

/// B2W Helper
let setSymbolPos (sym: Symbol) (newPos: XYPos) : Symbol =
    let comp' = { sym.Component with X = newPos.X; Y = newPos.Y }
    { sym with
        Component = comp'
        Pos = newPos
        LabelBoundingBox =
            { sym.LabelBoundingBox with
                TopLeft = sym.LabelBoundingBox.TopLeft - sym.Pos + newPos } }

/// B2W: The position of a symbol on the sheet
let setSymbolPosOnSheet (symID: ComponentId) (pos: XYPos) (model: SheetT.Model) : SheetT.Model =
    let symbolMap = model.Wire.Symbol.Symbols
    let sym: Symbol =
        match symbolMap.TryFind(symID) with
        | Some s -> s
        | None -> failwithf "Symbol with id %A not found in model" symID

    let newSym = moveSymbol pos sym // let newSym be the symbol with the new position
    let newSymbolMap = Map.remove symID symbolMap |> Map.add symID newSym
    // return the model with the new symbolMap
    // { model with Wire.Symbol = { model.Wire.Symbol with Symbols = newSymbolMap } }
    model |> Optic.set symbolMap_ newSymbolMap

/// B3R: Read/write the order of ports on a specified side of a symbol
let getPortMapsOrder
    (sym: Symbol)
    (side: Edge)
    // get Symbol.PortMaps.Order which is Map<Edge, string list>
    // return all strings for given Edge
    =
    sym.PortMaps.Order.TryFind(side)
    |> Option.defaultValue []
/// B3W: Read/write the order of ports on a specified side of a symbol
let setPortMapsOrder (sym: Symbol) (newSide: Edge) (newOrder: string list) : Symbol =
    let portMaps = sym.PortMaps
    let newPortMaps =
        { portMaps with Order = portMaps.Order |> Map.add newSide newOrder }
    { sym with PortMaps = newPortMaps }

/// B4R: The reversed state of the inputs of a MUX2
// question for TA: should I always failwithf?
let getMUX2ReversedInput (sym: Symbol) : bool =
    match sym.Component.Type with
    | Mux2 ->
        sym.ReversedInputPorts
        |> Option.defaultValue false
    | _ -> failwithf "Symbol is not a MUX2"

/// B4W: The reversed state of the inputs of a MUX2
let setToggleMUX2ReversedInput (sym: Symbol) : Symbol =
    let toggle (state: bool option) =
        match state with
        | Some s -> Some(not s)
        | None -> None
    match sym.Component.Type with
    | Mux2 -> { sym with ReversedInputPorts = toggle sym.ReversedInputPorts }
    | _ -> failwithf "Symbol is not a MUX2"

/// B5R: The position of a port on the sheet. It cannot directly be written.
let getPortPosOnSheet (sym: Symbol) (port: Port) =
    // get the position of the symbol, and relative position of the port
    // add the two positions together and return the result

    let portPosOnSymbol = getPortPos (sym: Symbol) (port: Port)
    sym.Pos + portPosOnSymbol

/// B6R: The Bounding box of a symbol outline (position is contained in this)
let getSymbolOutlineBoundingBox (sym: Symbol) : BoundingBox =
    let h, w = getRotatedHAndW sym
    if sym.Annotation = Some ScaleButton then
        { TopLeft = sym.Pos - { X = 9.; Y = 9. }; H = 17.; W = 17. }
    else
        { TopLeft = sym.Pos; H = float (h); W = float (w) }

/// B7R: The rotation state of a symbol
let getSymbolRotation (sym: Symbol) : Rotation = sym.STransform.Rotation

// B7W: The rotation state of a symbol
let setSymbolRotation (sym: Symbol) (newRot: Rotation) : Symbol =
    let sTransform = sym.STransform
    let newSTransform = { sTransform with Rotation = newRot }
    { sym with STransform = newSTransform }

/// B8R: The flip state of a symbol
let getSymbolFlip (sym: Symbol) : bool = sym.STransform.flipped

/// B8W: The flip state of a symbol
let setSymbolFlip (sym: Symbol) (newFlip: bool) : Symbol =
    let sTransform = sym.STransform
    let newSTransform = { sTransform with flipped = newFlip }
    { sym with STransform = newSTransform }

(*

// let getEdgeOnSymbolFromPortID (sym: Symbol) (portID: string) : Edge =
//     sym.PortMaps.Orientation.TryFind(portID)
//     |> Option.defaultValue Left


// following CommonTypes.Edge, Left is the default value

// let getPosOnSheetFromPortId (model: Model) (portId : string) : XYPos =
// get symbol from portID

// calculate from symbol
// take in symbol

Prof Edstem:
For ports there are two things:

XYPos coordinates (position) - there is indeed a function that gives this to you.

Which symbol edge it is on, and where is it ordered relative to other ports on that edge (PortMaps has these things)

So by position I mean XYPos.

    type PortMaps =
        {
            /// Maps edge to list of ports on that edge, in correct order
            Order: Map<Edge, string list>
            /// Maps the port ids to which side of the component the port is on
            Orientation: Map<string, Edge>
        }
        // rotate an And gate
        // two left ports one right port
        // rotation changes the deges



    port from a sheet or a symbol?
    symbol
    buswire has wire stuff > symbol model  = symbols + positions + ref from ports to symbol
    sheet has stuff to do with bounding boxes

    work out bounding boxes from symbols


if you write a function operating on sheets
move symbol? change bounding box
fundamental: symbolMap
but.... when we are doing wire operations/moving stuff, need to calculate bounding boxes, we use two sets, one for labels and symbols

// label bounding boxes are used to work out where to place a label
// ports
// when you connect wires to ports, need to know initial direction of wire, this is done with Edge
// Each port has designated edge


*)
