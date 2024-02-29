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

// generate two lists of all the ports that are connected to a given port(one of them is empty)
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
    (inputPorts, outputPorts)
