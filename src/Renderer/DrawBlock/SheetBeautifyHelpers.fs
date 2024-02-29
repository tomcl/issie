module SheetBeautifyHelpers
// ------- Drawblocks -----------
open BlockHelpers
open BusWire
open BusWireRoute
open BusWireRoutingHelpers
open BusWireSeparate
open BusWireUpdate
open BusWireUpdateHelpers
open PopupHelpers
open RotateScale
open Sheet
open SheetDisplay
open SheetSnap
open SheetUpdateHelpers
open Symbol
open SymbolHelpers
open SymbolPortHelpers
open SymbolReplaceHelpers
open SymbolUpdate
open SymbolView
// --------- common -----------
open CommonTypes
open DrawHelpers
open EEExtensions
open Helpers
open Optics
open Optic
open Operators
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open ModelType 

(*
    This file aims to provide code of two functions that helps read and write the corresponding values in each case. 
    A Lens should also be procided if possible, in order to combine the two values. 
    See https://github.com/dyu18/hlp24-project-issie-team7/tree/indiv-az1821/README-Indiv-notes.md for more detailed documentation. 
*)

// ================================= Draw Block Build Functions ====================================

// ------------------------------------- B1 (Read & write) --------------------------------
/// The dimensions of a custom component symbol
/// input - symbol
/// output - (x y dimension)
let readCustomSymbolDim (sym : Symbol) = 
    (sym.Component.W, sym.Component.H)
let writeCustomSymbolDim ((newDim) : (float * float)) (sym : Symbol) =
    {sym with Component = { sym.Component with W = fst newDim; H = snd newDim }}
let CustomCompDim : Lens<Symbol, (float * float)> =
    Lens (readCustomSymbolDim, writeCustomSymbolDim)

// --------------------------------- B2 (write) -------------------------------------
/// The position of a symbol on the sheet
/// input - new position of symbol
/// output - new symbol at the specific position
let writeSymbolPos (newPos: XYPos) (sym: Symbol)= 
    {sym with Pos = newPos}

// --------------------------------- B3 (read & write) ------------------------------
/// Read & write the order of ports on a specified side of a symbol
/// input - the list of port ids for a specified side from a Symbol
/// output - new symbol with updated port order for the specified side
let readPortOrder (side : Edge) (sym : Symbol) =
    sym.PortMaps.Order |> Map.tryFind side 
let writePortOrder (side : Edge) (newPortOrder : list<string> option) (sym : Symbol) =
    match newPortOrder with
    | Some newPortOrder ->
        let newPorts = side |> Map.add newPortOrder sym.PortMaps.Order
        { sym with PortMaps = { sym.PortMaps with Order = newPorts } }
    | _ -> sym 
let portOrderLens (side : Edge) : Lens<Symbol, list<string> option> =
    Lens (readPortOrder side, writePortOrder side)

// --------------------------------- B4 (read & write) -------------------------------- 
/// The reverses state of the inputs of a MUX2
/// input - symbol (look up current state of reversed input ports)
/// output - (update state of reversed input ports)
let readReverseInput (sym : Symbol) = 
    sym.ReversedInputPorts
let writeReverseInput (reversedState : bool option) (sym : Symbol) = 
    let newReverseState =
        match reversedState with
        | Some state -> Some (not state)
        | _ -> Some true
    let newSym = 
        sym.Component.SymbolInfo 
        |> Option.map (fun symInfo -> 
        { symInfo with ReversedInputPorts = newReverseState })
    { sym with 
        Component = { sym.Component with SymbolInfo = newSym }
        ReversedInputPorts = newReverseState 
    }
let reversedInputsMux2_ : Lens<Symbol, bool option> = 
    Lens (readReverseInput, writeReverseInput)

// ------------------------------------ B5 (read) ------------------------------------------
/// The position of a port on the sheet. (It cannot directly be written.)
/// input - port and symbol
/// output - port position 
let readPort (port : Port) (sym : Symbol) : (XYPos) = 
    { X = sym.Pos.X + (getPortPos sym port).X; 
      Y = sym.Pos.Y - (getPortPos sym port).Y }

// TODO : check if look up for port/symbol is needed

// ---------------------------------- B6 (read) ----------------------------------------
/// The Bounding box of a symbol outline (position is contained in this)
/// input - symbol
/// output - the bouding box of the symbol
/// Note : Helper function in symbol.fs have been used to implement this
///        Consider using `getSymBoundingBox` to simplify
let readSymBoundingBox (sym: Symbol) : BoundingBox =
    let h,w = getRotatedHAndW sym
    if sym.Annotation = Some ScaleButton then 
        {TopLeft = sym.Pos - {X = 9.; Y = 9.}; H = 17. ; W = 17.}
    else 
        {TopLeft = sym.Pos; H = float(h) ; W = float(w)}

// ---------------------------------- B7 (read & wrtie) -------------------------------
/// The rotation state of a symbol
/// input - rotation
/// output - new rotated symbol
let readSymRotationState (sym: Symbol) =
    sym.Component.SymbolInfo.Rotation
let writeSymRotationState (rotate : Rotation) (sym: Symbol) = 
    {sym with STransform = {sym.STransform with Rotation = rotate}}
let symRotation : Lens<Symbol, Rotation> = 
    Lens (readSymRotationState, writeSymRotationState)

// ------------------------------------ B8 (read & write) -------------------------------
/// The flip state of a symbol
/// input - flip
/// output - new flipped symbol
let readSymFlipState (sym : Symbol) = 
    sym.Component.SymbolInfo.Flipped
let writeSymFlipState (flip : bool) (sym: Symbol) = 
    {sym with STransform = {sym.STransform with Flipped = flip}}
let symFlip : Lens<Symbol, bool> = 
    Lens (readSymFlipState, writeSymFlipState)

// ================================== Test Helper Functions =============================================

//-----------------------------------------------------------------------------------------------
// visibleSegments is included here as ahelper for info, and because it is needed in project work
//-----------------------------------------------------------------------------------------------
/// The visible segments of a wire, as a list of vectors, from source end to target end.
/// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed 
/// which if present causes the two segments on either side of it to coalesce into a single visible segment.
/// A wire can have any number of visible segments - even 1.
let visibleSegments (wId: ConnectionId) (model: SheetT.Model): XYPos list =

    let wire = model.Wire.Wires[wId] // get wire from model

    /// helper to match even and off integers in patterns (active pattern)
    let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

    /// Convert seg into its XY Vector (from start to end of segment).
    /// index must be the index of seg in its containing wire.
    let getSegmentVector (index:int) (seg: BusWireT.Segment) =
        // The implicit horizontal or vertical direction  of a segment is determined by 
        // its index in the list of wire segments and the wire initial direction
        match index, wire.InitialOrientation with
        | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> {X=0.; Y=seg.Length}
        | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> {X=seg.Length; Y=0.}

    /// Return a list of segment vectors with 3 vectors coalesced into one visible equivalent
    /// if this is possible, otherwise return segVecs unchanged.
    /// Index must be in range 1..segVecs
    let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int)  =
        if segVecs[index] =~ XYPos.zero
        then
            segVecs[0..index-2] @
            [segVecs[index-1] + segVecs[index+1]] @
            segVecs[index+2..segVecs.Length - 1]
        else
            segVecs

    wire.Segments
    |> List.mapi getSegmentVector
    |> (fun segVecs ->
            (segVecs,[1..segVecs.Length-2])
            ||> List.fold tryCoalesceAboutIndex)


// =================================== End of helper functions =======================================

