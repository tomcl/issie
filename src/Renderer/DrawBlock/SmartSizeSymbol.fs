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

///HLP 23: AUTHOR Rzepala

///this record determines how far will the other components will move from a custom resized component
type ConstantsSmartSize = {
    Distance: float
}

let OffsetSmartSize = {Distance = 50.0}

let offsetPorts
    (wire: ConnectionId * Wire)
    (orderedPortsResize: (string * XYPos) list)
    (orderedPortsOther: (string * XYPos) list)
    (orient: OrientationS option)
        : XYPos = 
    let PortOneId = string (snd wire).InputPort
    let PortTwoId = string (snd wire).OutputPort
    let TmpOne = 
        orderedPortsResize 
        |> List.filter (fun (x, _) -> x = PortOneId || x = PortTwoId)
    let CoordinateOne = TmpOne.Item(0) |> snd
    let TmpTwo = 
        orderedPortsOther 
        |> List.filter (fun (x, _) -> x = PortOneId || x = PortTwoId)
    let CoordinateTwo = TmpTwo.Item(0) |> snd
    let Coordinates = CoordinateTwo - CoordinateOne
    match orient with
    | Some TopBottom ->
        {X = Coordinates.X; Y = 0.0}
    | Some LeftRight ->
        {X = 0.0; Y = Coordinates.Y}
    | None -> {X = 0.0; Y = 0.0}
// this function is used purely to return HScale or VScale of a symbol
let Scale 
    (symbolScale: float option)
        : float =
    match symbolScale with
    | Some x -> x
    | None -> 1.0


//this function is used to evaluate the distance between ports of a symbol.
let getPortDist 
    (portsOrdereds: (string * XYPos) list)
    (orient: OrientationS option)
        : float option= 
    let ItemOne = portsOrdereds.Item(1)
    let XYPosOne = snd ItemOne
    let ItemZero = portsOrdereds.Item(0)
    let XYPosZero = snd ItemZero
    let l = portsOrdereds.Length
    if l = 0 then None
    elif l = 1 then None
    else 
        match orient with
        | Some TopBottom ->
            Some (XYPosOne.X - XYPosZero.X)
        | Some LeftRight -> 
            Some (XYPosOne.Y - XYPosZero.Y)
        | None -> None
    


//this function returns the edges of the symbols which we would like to align
//it's based purely on symbol positions on the canvas, and does not take into account 
//any connections between the two symbols. 
let relationPos 
    (toSize: Symbol)
    (other: Symbol)
        : OrientationS option =
    let dimension 
        (symbol: Symbol)
        (dim: WhichDimension)
            : float = 
        match dim with
        | Widths -> symbol.Pos.X + symbol.Component.W * Scale symbol.HScale
        | Heights -> symbol.Pos.Y + symbol.Component.H * Scale symbol.VScale
    
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
    else None



let reSizeSymbol 
    (wModel: BusWireT.Model) 
    (symbolToSize: Symbol) 
    (otherSymbol: Symbol) 
        : BusWireT.Model =  
    printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"
    let manageableWires = Map.toList wModel.Wires
    let sModel = wModel.Symbol
    let Orient = relationPos symbolToSize otherSymbol
    let portsOrderedToSize, portsOrderedOther = 
        match Orient with
        | Some TopBottom -> 
            if symbolToSize.Pos.Y < otherSymbol.Pos.Y then
                getAllPortsFromEdgeOrdered wModel symbolToSize Orient Bottom,
                getAllPortsFromEdgeOrdered wModel otherSymbol Orient Top
            else
                getAllPortsFromEdgeOrdered wModel symbolToSize Orient Top,
                getAllPortsFromEdgeOrdered wModel otherSymbol Orient Bottom
        | Some LeftRight ->
            if symbolToSize.Pos.X < otherSymbol.Pos.X then
                getAllPortsFromEdgeOrdered wModel symbolToSize Orient Right,
                getAllPortsFromEdgeOrdered wModel otherSymbol Orient Left
            else 
                getAllPortsFromEdgeOrdered wModel symbolToSize Orient Left,
                getAllPortsFromEdgeOrdered wModel otherSymbol Orient Right
        | None -> [], []
    printfn "%A" portsOrderedToSize
    printfn "%A" portsOrderedOther

    let Dimension = getPortDist portsOrderedToSize Orient, getPortDist portsOrderedOther Orient
    printfn "%A" Dimension

    let checker = 
        getCommonWires wModel symbolToSize otherSymbol Orient 
        |> Map.toList
    printfn "%A" checker
    let checker' = checker.Item(0)
    let checker'' = offsetPorts checker' portsOrderedToSize portsOrderedOther Orient
    printfn "%A" checker''
    let ScaleFactor = 
        match Dimension with
        | (x, y) -> (Scale y) / (Scale x)
    
    
    let symbol' = 
        match Orient with
        | Some TopBottom -> 
            {symbolToSize with HScale = Some (ScaleFactor * Scale symbolToSize.HScale)}
        | Some LeftRight -> 
            {symbolToSize with VScale = Some (ScaleFactor * Scale symbolToSize.VScale)}
        | None -> symbolToSize
        |> moveSymbol checker''

    let wModel' = {wModel with Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}}
    
    let wires' = manageableWires |> List.collect (fun (x, y) -> [x, autoroute wModel' y]) |> Map.ofList

    {wModel' with 
        Wires = wires'
    }

