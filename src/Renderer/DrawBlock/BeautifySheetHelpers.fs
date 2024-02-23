module BeautifySheetHelpers

open DrawModelType
open CommonTypes
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Optics
open Optics.Operators
open SymbolHelpers
open BlockHelpers

let symbolMap_: Lens<SheetT.Model, Map<ComponentId, Symbol>> =
    Lens.create (fun m -> m.Wire.Symbol.Symbols) (fun v m ->
        { m with Wire.Symbol = { m.Wire.Symbol with Symbols = v } })

/// B1R
let getCustomComponentSymbolDims (sym: Symbol) : XYPos =
    // directly return the third element from the result of getCustomSymCorners
    // format: [|{X=0.0;Y=0.0}; {X=0.0;Y=yDim'}; {X=xDim';Y=yDim'}; {X=xDim';Y=0.0}|]
    (getCustomSymCorners sym).[2]

/// B1W
let setCustomComponentSymbolDims (sym: Symbol) (pos: XYPos) : Symbol =
    // Note: do not modify h and w as they are the default dimensions of the symbol.
    // We only scale them as needed
    let hScale = pos.X / sym.Component.H
    let vScale = pos.Y / sym.Component.W
    { sym with HScale = Some hScale; VScale = Some vScale }

/// B2W
// question to TA: should this be different from modifying Symbol.Pos?
// Edit, nevermind, Blockhelpers.moveSymbol will create a new symbol with the new position
let setSymbolPosOnSheet (symID: ComponentId) (pos: XYPos) (model: SheetT.Model) : SheetT.Model =
    let symbolMap = model.Wire.Symbol.Symbols
    let sym: Symbol =
        match symbolMap.TryFind(symID) with
        | Some s -> s
        | None -> failwithf "Symbol with id %A not found in model" symID
    // let newSym be the symbol with the new position
    let newSym = moveSymbol pos sym
    // let newSymbolMap be the symbolMap with the new symbol, newSym, and get rid of the old symbol
    let newSymbolMap = Map.remove symID symbolMap |> Map.add symID newSym
    // return the model with the new symbolMap
    // { model with Wire.Symbol = { model.Wire.Symbol with Symbols = newSymbolMap } }
    model |> Optic.set symbolMap_ newSymbolMap

/// B3R
let getPortMapsOrder
    (sym: Symbol)
    (side: Edge)
    // get Symbol.PortMaps.Order which is Map<Edge, string list>
    // return all strings for given Edge
    =
    sym.PortMaps.Order.TryFind(side)
    |> Option.defaultValue []
/// B3W
let setPortMapsOrder (sym: Symbol) (newSide: Edge) (newOrder: string list) : Symbol =
    let portMaps = sym.PortMaps
    let newPortMaps =
        { portMaps with Order = portMaps.Order |> Map.add newSide newOrder }
    { sym with PortMaps = newPortMaps }

/// B4R
// question for TA: should I always failwithf?
let getMUX2ReversedInput (sym: Symbol) : bool =
    match sym.Component.Type with
    | Mux2 ->
        sym.ReversedInputPorts
        |> Option.defaultValue false
    | _ -> failwithf "Symbol is not a MUX2"

/// B4W
let setToggleMUX2ReversedInput (sym: Symbol) : Symbol =
    let toggle (state: bool option) =
        match state with
        | Some s -> Some(not s)
        | None -> None
    match sym.Component.Type with
    | Mux2 -> { sym with ReversedInputPorts = toggle sym.ReversedInputPorts }
    | _ -> failwithf "Symbol is not a MUX2"

/// B5R
// The position of a port on the sheet. It cannot directly be written.
(*
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

*)

let visibleSegments (wId: ConnectionId) (model: SheetT.Model) : XYPos list =

    let wire = model.Wire.Wires[wId] // get wire from model

    /// helper to match even and odd integers in patterns (active pattern)
    let (|IsEven|IsOdd|) (n: int) =
        match n % 2 with
        | 0 -> IsEven
        | _ -> IsOdd

    /// Convert seg into its XY Vector (from start to end of segment).
    /// index must be the index of seg in its containing wire.
    let getSegmentVector (index: int) (seg: BusWireT.Segment) =
        // The implicit horizontal or vertical direction  of a segment is determined by
        // its index in the list of wire segments and the wire initial direction
        match index, wire.InitialOrientation with
        | IsEven, BusWireT.Vertical
        | IsOdd, BusWireT.Horizontal -> { X = 0.; Y = seg.Length }
        | IsEven, BusWireT.Horizontal
        | IsOdd, BusWireT.Vertical -> { X = seg.Length; Y = 0. }

    /// Return a list of segment vectors with 3 vectors coalesced into one visible equivalent
    /// if this is possible, otherwise return segVecs unchanged.
    /// Index must be in range 1..segVecs
    let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int) =
        if segVecs[index] =~ XYPos.zero then
            segVecs[0 .. index - 2]
            @ [ segVecs[index - 1] + segVecs[index + 1] ]
            @ segVecs[index + 2 .. segVecs.Length - 1]
        else
            segVecs

    wire.Segments
    |> List.mapi getSegmentVector
    |> (fun segVecs ->
        (segVecs, [ 1 .. segVecs.Length - 2 ])
        ||> List.fold tryCoalesceAboutIndex)
