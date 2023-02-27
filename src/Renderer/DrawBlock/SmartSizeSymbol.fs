module SmartSizeSymbol

open Elmish
open Fable.React.Props
open CommonTypes
open Fable.React
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Symbol
open Optics
open Operators
open SmartHelpers

(* 
    HLP23: This module will normally be used exclusively by team member doing the "smart resize symbol" 
    part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    Normally it will update multiple wires and one symbols in the BusWire model so could use the SmartHelper 
    function for the wires.
*)

/// HLP23: To test this, it must be given two symbols interconnected by wires. It then resizes symbolToSize
/// so that the connecting wires are exactly straight
/// HLP23: It should work out the interconnecting wires (wires) from 
////the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has adjustable size).
/// HLP23: when this function is written replace teh XML comment by something suitable concisely
/// stating what it does.
/// 

// this function is used purely to return HScale or VScale of a symbol
let Scale 
    (symbolScale: float option)
        : float =
    match symbolScale with
    | Some x -> x
    | None -> 1.0


let isOverlapped
    (firstBegin: float)
    (firstEnd: float)
    (secondBegin: float)
    (secondEnd: float)
        : bool =
    if firstBegin > secondBegin && firstBegin < secondEnd
    then true
    elif firstEnd < secondEnd && firstEnd > secondBegin
    then true
    elif secondBegin > firstBegin && secondBegin < firstEnd
    then true
    elif secondEnd < firstEnd && secondEnd > firstBegin
    then true
    else false



let getPortDist 
    (symbol: Symbol) 
    (pos: Edge) 
        : float = 
    let Width = symbol.Component.W * Scale symbol.HScale
    printfn $"Width of symbol {Width}"
    let NoPorts = List.length symbol.PortMaps.Order[pos]
    Width / ((float NoPorts) + 1.0)

//this function returns all the common wires between two symbols
let commonWires
    (toSize: Symbol)
    (other: Symbol)
        : bool =
    false

//this function returns the edges of the symbols which we would like to align
//it's based purely on symbol positions on the canvas, and does not take into account 
//any connections between the two symbols. To check if there are connections, 
//function commonWires will evaluate to True if connections exist.
type WhichDimension = Width | Height

type Orientation = TopBottom | LeftRight

let relationPos 
    (toSize: Symbol)
    (other: Symbol)
        : Orientation option =
    let dimension 
        (symbol: Symbol)
        (dim: WhichDimension)
            : float = 
        match dim with
        | Width -> symbol.Pos.X + symbol.Component.W * Scale symbol.HScale
        | Height -> symbol.Pos.Y + symbol.Component.H * Scale symbol.VScale
    
    let toSizeBeginH = toSize.Pos.X
    let toSizeEndH = toSize.Pos.X + toSize.Component.W * Scale toSize.HScale
    let otherBeginH = other.Pos.X
    let otherEndH = other.Pos.X + other.Component.W * Scale other.HScale
    let toSizeBeginV = toSize.Pos.Y
    let toSizeEndV = toSize.Pos.Y + toSize.Component.H * Scale toSize.VScale
    let otherBeginV = other.Pos.Y
    let otherEndV = other.Pos.Y + other.Component.H * Scale other.VScale

    if isOverlapped toSizeBeginH toSizeEndH otherBeginH otherEndH 
    then Some TopBottom
    elif isOverlapped toSizeBeginV toSizeEndV otherBeginV otherEndV
    then Some LeftRight
    else 
    failwithf "%A" "What? Symbols don't overlap"
    None

let getDim 
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    (orient: Orientation option)
        : float * float = 
    match orient with
    | Some TopBottom ->
        if symbolToSize.Pos.Y < otherSymbol.Pos.Y
        then getPortDist symbolToSize Bottom, getPortDist otherSymbol Top
        else getPortDist symbolToSize Top, getPortDist otherSymbol Bottom
    | Some LeftRight ->
        if symbolToSize.Pos.X < otherSymbol.Pos.X
        then getPortDist symbolToSize Right, getPortDist otherSymbol Left
        else getPortDist symbolToSize Left, getPortDist otherSymbol Right
    | None -> failwithf "Whatt?"

let reSizeSymbol 
    (wModel: BusWireT.Model) 
    (symbolToSize: Symbol) 
    (otherSymbol: Symbol) 
        : BusWireT.Model =
    printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"
    let sModel = wModel.Symbol
    let Orient = relationPos symbolToSize otherSymbol
    printfn "%A" Orient
    
    
    let Dimension = getDim symbolToSize otherSymbol Orient

    
    let DimensionOfConstant, DimensionToChange = 
        match Dimension with
        | (x, y) -> y, x
        


    let ScaleFactor = DimensionOfConstant/DimensionToChange
    printfn "%A" ScaleFactor
    let wires = [] // replace this with correct wires
    
    
    let symbol' = {symbolToSize with HScale = Some (ScaleFactor * Scale symbolToSize.HScale)}
    
    let wModel' = SmartHelpers.updateModelWires wModel []
    

    // HLP23: this could be cleaned up using Optics - see SmartHelpers for examples
    {wModel' with 
        Wires = wModel.Wires // no change for now, but probably this function should use update wires after resizing.
                             // to make that happen the test function which calls this would need to provide an updateWire
                             // function to this as a parameter (as was done in Tick3)
        Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
    }


