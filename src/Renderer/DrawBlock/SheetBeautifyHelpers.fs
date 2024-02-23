module SheetBeautifyHelpers

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open CommonTypes
open BlockHelpers
open CommonTypes
open SymbolPortHelpers
open Symbol


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
let getPortOrderOnSide (symbol: Symbol) (edge: Edge) : string list option =
    match Map.tryFind edge symbol.PortMaps.Order with
    | Some portOrder -> Some portOrder
    | None -> None


//B3W

let setPortOrderOnSide (symbol: Symbol) (edge: Edge) (portOrder: string list) : Symbol =
    { symbol with PortMaps = { symbol.PortMaps with Order = Map.add edge portOrder symbol.PortMaps.Order } }




//B4R
let isInputPortsReversed (symbol: Symbol) : bool option =
    symbol.ReversedInputPorts

//B4W

let ReverseMuxInputs (symbol: Symbol) (symbolModel: SymbolT.Model) (compId: ComponentId) =
    let reverse = isInputPortsReversed symbol
    match Map.tryFind compId symbolModel.Symbols with
    | Some symbol ->
        match symbol.Component.Type with
        | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 ->
            match reverse with
            | Some true ->
                // Reverse the connections of the symbol's input ports
                let reversedInputPorts =
                    List.rev symbol.Component.InputPorts
                let updatedComponent =
                    { symbol.Component with
                        InputPorts = reversedInputPorts }
                Some { symbol with
                        Component = updatedComponent }
            | _ -> Some symbol // No need to reverse inputs
        | _ -> Some symbol // Component type doesn't need input reversal
    | None -> None // Symbol not found in the model

let writeReverseMuxInputs (symbol: Symbol) (reverse: bool option) : Symbol =
    match symbol.Component.Type with
        | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 -> {symbol with ReversedInputPorts = reverse}
        | _ -> symbol

            


//B5R
(*
let getPortPosition (symbol: Symbol) (portId: string) : XYPos option =
    let component = symbol.Component
    // Find the edge to which the port belongs
    let edge = symbol.PortMaps.Orientation.[portId]
    // Determine the position along the edge based on the port order
    let portOrder = symbol.PortMaps.Order |> Map.find edge
    let portIndex = List.findIndex (fun p -> p = portId) portOrder
    let portCount = List.length portOrder
    // Calculate the position based on the dimensions of the component and port order
    let portPosition =
        match edge with
        | Top -> // Calculate position along the top edge
        | Bottom -> // Calculate position along the bottom edge
        | Left -> // Calculate position along the left edge
        | Right -> // Calculate position along the right edge
    Some portPosition
    *)

let getPortPos (port: Port) (model: SymbolT.Model) : XYPos =
    let portId = port.Id
    getPortLocation None model portId

//B6
let getBoundingBox (symbol: Symbol) : BoundingBox =
    symbol.SymbolBoundingBox

//B7R
let checkRotateState (symbol: Symbol) : Rotation =
    symbol.STransform.Rotation
//B7W
let writeRotateState (symbol: Symbol) (rotate: Rotation) : Symbol =
    let updatedSTransform = {symbol.STransform with Rotation = rotate}
    let updatedSymbol = {symbol with STransform = updatedSTransform}
    updatedSymbol

let write2 (symbol: Symbol) (rotate: Rotation) : Symbol =
    match rotate with
    | Degree0 | Degree90 | Degree180 | Degree270 ->
        let updatedSTransform = {symbol.STransform with Rotation = rotate}
        let updatedSymbol = {symbol with STransform = updatedSTransform}
        updatedSymbol
    | _ -> symbol
        

//B8R
let checkFlipState (symbol: Symbol) : bool =
    symbol.STransform.Flipped

//B8W
let writeFlipState (symbol: Symbol) (flip: bool) : Symbol =
    let updatedSTransform = {symbol.STransform with Flipped = flip}
    let updatedSymbol = {symbol with STransform = updatedSTransform}
    updatedSymbol


