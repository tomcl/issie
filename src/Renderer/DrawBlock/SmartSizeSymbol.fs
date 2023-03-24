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

//HLP 23: AUTHOR Rzepala
//
//This is the smartSize file. When Ctrl+E (or Cmd+E) is pressed, the chosen symbol is resized, so that
//the ports on each edge are equidistant. When Ctrl+E is pressed for the second time
//it moves the symbol, so that there is a nice alignment between.
//By my own decision, the chosen custom components have to 'align' in order for this to work;
//some of their vertical or horizontal lengths have to overlap.



//this record determines how far will the other components will move from a custom resized component
type ConstantsSmartSize = {
    Distance: float
}

let OffsetSmartSize = {Distance = 50.0}

let offsetPorts
    (orderedPortsResize: (string * XYPos) list)
    (orderedPortsOther: (string * XYPos) list)
    (orient: OrientationS option)
    (wire: (ConnectionId * Wire) option)

        : XYPos = 
    match wire with
    | Some x ->
        let PortOneId = string (snd x).InputPort
        let PortTwoId = string (snd x).OutputPort
        let TmpOne = 
            orderedPortsResize 
            |> List.filter (fun (x, _) -> x = PortOneId || x = PortTwoId)
        let CoordinateOne = TmpOne[0] |> snd
        let TmpTwo = 
            orderedPortsOther 
            |> List.filter (fun (x, _) -> x = PortOneId || x = PortTwoId)
        let CoordinateTwo = TmpTwo[0] |> snd
        let Coordinates = CoordinateTwo - CoordinateOne

        match orient with
        | Some TopBottom ->
            {X = Coordinates.X; Y = 0.0}
        | Some LeftRight ->
            {X = 0.0; Y = Coordinates.Y}
        | None -> {X = 0.0; Y = 0.0}
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
    
    let l = portsOrdereds.Length
    if l = 0 then None
    elif l = 1 then None
    else 
        let ItemOne = portsOrdereds[1]
        let XYPosOne = snd ItemOne
        let ItemZero = portsOrdereds[0]
        let XYPosZero = snd ItemZero
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

let getType 
    (symbol: Symbol)
        : string =
    match symbol.Component.Type with
    | Custom (CustomComponentType) -> "Custom"
    | _ -> "Else"

let portDist 
    (wModel: BusWireT.Model) 
    (symbolToSize: Symbol) 
    (otherSymbol: Symbol) 
    (orient: OrientationS option)=
        match orient with
        | Some TopBottom -> 
            if symbolToSize.Pos.Y < otherSymbol.Pos.Y then
                getAllPortsFromEdgeOrdered wModel symbolToSize orient Bottom,
                getAllPortsFromEdgeOrdered wModel otherSymbol orient Top
            else
                getAllPortsFromEdgeOrdered wModel symbolToSize orient Top,
                getAllPortsFromEdgeOrdered wModel otherSymbol orient Bottom
        | Some LeftRight ->
            if symbolToSize.Pos.X < otherSymbol.Pos.X then
                getAllPortsFromEdgeOrdered wModel symbolToSize orient Right,
                getAllPortsFromEdgeOrdered wModel otherSymbol orient Left
            else 
                getAllPortsFromEdgeOrdered wModel symbolToSize orient Left,
                getAllPortsFromEdgeOrdered wModel otherSymbol orient Right
        | None -> [], []

let reSizeSymbol 
    (wModel: BusWireT.Model) 
    (symbolToSize: Symbol) 
    (otherSymbol: Symbol) 
        : BusWireT.Model =  
    
    printfn "%A" symbolToSize
    printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"
    let manageableWires = Map.toList wModel.Wires
    let sModel = wModel.Symbol
    let orient = relationPos symbolToSize otherSymbol
    //I tried to implement the function proposed in my individual feedback (Rzepala), 
    //But no matter what I did, it always seemed to produce the scaling factor of 1.0
    //In the end, I decided to leave the working solution.
    let portsOrderedToSize, portsOrderedOther = 
        portDist wModel symbolToSize otherSymbol orient

    let dimension = getPortDist portsOrderedToSize orient, getPortDist portsOrderedOther orient

    let scaleFactor = 
        match dimension with
        | (Some x, Some y) -> y / x
        | (None, _) -> 1.0
        | (_, _) -> 1.0 
    
    let symbol' = 
        match orient with
        | Some TopBottom -> 
            {symbolToSize with HScale = Some (scaleFactor * Scale symbolToSize.HScale)}
            |> calcLabelBoundingBox
        | Some LeftRight -> 
            {symbolToSize with VScale = Some (scaleFactor * Scale symbolToSize.VScale)}
            |> calcLabelBoundingBox
        | None -> symbolToSize


    let wModel' = {wModel with Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}}
    
    let wires' = manageableWires |> List.collect (fun (x, y) -> [x, autoroute wModel' y]) |> Map.ofList

    let wModel'' = {wModel' with  Wires = wires'}

    let portsOrderedToSize', portsOrderedOther' = 
        portDist wModel'' symbol' otherSymbol orient

    let offset = 
        getCommonWires wModel'' symbolToSize otherSymbol orient 
        |> Map.toList
        |> function 
            | [hd] -> Some hd
            | hd::tl -> Some hd
            | [] -> None
        |> offsetPorts  portsOrderedToSize' portsOrderedOther' orient

    let symbol'' = symbol' |> moveSymbol offset |> calcLabelBoundingBox

    {wModel with Symbol = {sModel with Symbols = Map.add symbol''.Id symbol'' sModel.Symbols}}

    

let reSizeSymbolDraggable 
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol) 
        : BusWireT.Model =
    
    let customComponentList = 
        wModel.Symbol.Symbols 
        |> Map.toList 
        |> List.filter (fun (_, x) -> getType x = "Custom") 
        |> List.map (fun (_, x) -> x)
        |> List.filter (fun x -> x <> symbolToSize)
    
    let closestComponent = 
        let tmp = getDistanceAlignments symbolToSize customComponentList
        match tmp with
        | (x, y) -> y

    printfn "%A" closestComponent 
    reSizeSymbol wModel symbolToSize closestComponent

