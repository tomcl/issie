module SheetBeautifyD1

open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open SheetBeautifyHelpers
open Optics

module Constants =
    ()

// D1. sheetAlignScale implementation part - Custom component scaling. Positioning of all components to reduce segments without increasing wire crossings.


// determine if two ports are on the same horizontal or vertical line
let isSameXYAxis (sym1: Symbol) (p1:Port) (sym2: Symbol) (p2:Port) =
    let xy1 = readPortPosition sym1 p1
    let xy2= readPortPosition sym2 p2
    match (xy1.X = xy2.X, xy1.Y = xy2.Y) with
    | (true, _) -> true
    | (_, true) -> true
    | _ -> false

// determine if two ports are on the opposite sides of a symbol
let isOppositeSide (sym1: Symbol) (p1:Port) (sym2: Symbol) (p2:Port) =
    let side1 = Map.tryFind p1.Id sym1.PortMaps.Orientation
    let side2 = Map.tryFind p2.Id sym2.PortMaps.Orientation
    match (side1, side2) with
    | (Some side1, Some side2) -> side1.Opposite = side2
    | _ -> false

// generate lists of all the ports that are connected to a given port(one of them is empty so only output one list)
let connectedPorts (model: SheetT.Model) (port: Port) =
    let inputPorts, outputPorts = 
        model.Wire.Wires 
        |> Map.values
        |> Seq.toList
        |> List.fold (fun (accInput, accOutput) wire ->
            match port.PortType, wire.InputPort, wire.OutputPort with
            | PortType.Input, InputPortId id, outputPort when id = port.Id -> (accInput, outputPort :: accOutput)
            | PortType.Output, inputPort, OutputPortId id when id = port.Id -> (inputPort :: accInput, accOutput)
            | _ -> (accInput, accOutput)
        ) ([], [])
    // return the list of connected ports as string list
    match port.PortType with
    | PortType.Input -> outputPorts |> List.map (fun (OutputPortId id) -> id)
    | PortType.Output -> inputPorts |> List.map (fun (InputPortId id) -> id)

// Align the scale of the symbols such that two ports are on the same axis, make sure it is opposite side already
let alignScale (model: SymbolT.Model) 
    (sym1: Symbol) (sym1Port1:Port) (sym1Port2:Port) 
    (sym2: Symbol) (sym2Port1:Port) (sym2Port2:Port) =
    let xy1= readPortPosition sym1 sym1Port1
    let xy2= readPortPosition sym2 sym1Port2
    let xy3= readPortPosition sym2 sym2Port1
    let xy4= readPortPosition sym2 sym2Port2
    // Determine alignment axis
    let axis =
        match Map.tryFind sym1Port1.Id sym1.PortMaps.Orientation, Map.tryFind sym2Port1.Id sym2.PortMaps.Orientation with
        | Some(Top), Some(Bottom) | Some(Bottom), Some(Top) -> "Horizontal"
        | Some(Left), Some(Right) | Some(Right), Some(Left) -> "Vertical"
        | _ -> failwith "Invalid or unsupported port orientation combination"
    
    // Calculate new dimensions
    let newSym1Dims, newSym2Dims =
        match axis with
        | "Horizontal" ->
            // Calculate new dimensions/positions for horizontal alignment
            let width1 = abs(xy2.X - xy1.X)
            let width2 = abs(xy4.X - xy3.X)
            match width1 < width2 with
            | true -> 
                let newWidthsym1 = (width2-width1)/width1 * sym1.Component.W + sym1.Component.W
                let newHeightsym1 = sym1.Component.H
                let newWidthsym2 = sym2.Component.W
                let newHeightsym2 = sym2.Component.H
                ((newWidthsym1,newHeightsym1), (newWidthsym2,newHeightsym2))
            | false ->
                let newWidthsym1 = sym1.Component.W
                let newHeightsym1 = sym1.Component.H
                let newWidthsym2 = (width1-width2)/width2 * sym2.Component.W + sym2.Component.W
                let newHeightsym2 = sym2.Component.H
                ((newWidthsym1,newHeightsym1), (newWidthsym2,newHeightsym2))

        | "Vertical" ->
            // Calculate new dimensions/positions for vertical alignment
            let height1 = abs(xy2.Y - xy1.Y)
            let height2 = abs(xy4.Y - xy3.Y)
            match height1 < height2 with
            | true -> 
                let newWidthsym1 = sym1.Component.W
                let newHeightsym1 = (height2-height1)/height1 * sym1.Component.H + sym1.Component.H
                let newWidthsym2 = sym2.Component.W
                let newHeightsym2 = sym2.Component.H
                ((newWidthsym1,newHeightsym1), (newWidthsym2,newHeightsym2))
            | false ->
                let newWidthsym1 = sym1.Component.W
                let newHeightsym1 = sym1.Component.H
                let newWidthsym2 = sym2.Component.W
                let newHeightsym2 = (height1-height2)/height2 * sym2.Component.H + sym2.Component.H
                ((newWidthsym1,newHeightsym1), (newWidthsym2,newHeightsym2))
        
        | _ -> failwith "Invalid or unsupported axis"

    // Apply changes using lenses
    let sym1Option = Map.tryFind sym1.Id model.Symbols
    let sym2Option = Map.tryFind sym2.Id model.Symbols

    // Update dimensions if symbols are found
    let updatedSymbols = model.Symbols
                          |> Map.change sym1.Id (Option.map (fun sym -> snd customComponentDimensionsLens newSym1Dims sym))
                          |> Map.change sym2.Id (Option.map (fun sym -> snd customComponentDimensionsLens newSym2Dims sym))

    // Return updated model
    { model with Symbols = updatedSymbols }


// determine which two sides of two symbols have the most ports connected on the same axis