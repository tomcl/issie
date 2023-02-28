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

open BusWireUpdateHelpers



type WhichDimension = Width | Height

type Orientation = TopBottom | LeftRight

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

//this function is used to evaluate the distance between ports of a symbol.
let getPortDist 
    (symbol: Symbol) 
    (pos: Edge) 
        : float = 
    let Width = symbol.Component.W * Scale symbol.HScale
    printfn $"Width of symbol {Width}"
    let NoPorts = List.length symbol.PortMaps.Order[pos]
    Width / ((float NoPorts) + 1.0)


//this function returns the edges of the symbols which we would like to align
//it's based purely on symbol positions on the canvas, and does not take into account 
//any connections between the two symbols. 

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
    let manageableWires = Map.toList wModel.Wires
    
    let sModel = wModel.Symbol
    let Orient = relationPos symbolToSize otherSymbol
    printfn "%A" Orient
    let SymbolIds = [(ComponentId symbolToSize.Component.Id); (ComponentId otherSymbol.Component.Id)]
    
    let Dimension = getDim symbolToSize otherSymbol Orient
    
    
    let DimensionOfConstant, DimensionToChange = 
        match Dimension with
        | (x, y) -> y, x
        
    let ScaleFactor = DimensionOfConstant/DimensionToChange

    let symbol' = 
        match Orient with
        | Some TopBottom -> {symbolToSize with HScale = Some (ScaleFactor * Scale symbolToSize.HScale)}
        | Some LeftRight -> {symbolToSize with VScale = Some (ScaleFactor * Scale symbolToSize.HScale)}
        | None -> symbolToSize

    let wModel' = {wModel with Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}}
    
    let wires' = manageableWires |> List.collect (fun (x, y) -> [x, BusWireUpdateHelpers.autoroute wModel' y]) |> Map.ofList

    // HLP23: this could be cleaned up using Optics - see SmartHelpers for examples
    {wModel' with 
        Wires = wires'      // to make that happen the test function which calls this would need to provide an updateWire                      // function to this as a parameter (as was done in Tick
    }


