module SheetBeautifyD2

open System
open Optics
open CommonTypes
open DrawModelType
open BlockHelpers
open DrawHelpers
open SheetUpdateHelpers
open SheetBeautifyHelpers


module Constants = ()


(* ---------------------------------------------------------------------------------------------- *)
(*                                      Clock-Face Algorithm                                      *)
(* ---------------------------------------------------------------------------------------------- *)

(* --------------------------------------- Symbol Helpers --------------------------------------- *)

/// <summary>Get all port IDs of a symbol.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>List of port ID strings.</returns>
let getSymPortIds (sym: SymbolT.Symbol): List<string> =
    sym.PortMaps.Orientation 
    |> Map.toList 
    |> List.map (fun (id, _) -> id)

/// <summary>Get the angle of a symbol's bottom right corner from positive x-axis direction.</summary>
/// <param name="sym">Target symbol.</param>
/// <param name="sheet">Model of whole sheet.</param>
/// <returns>Angle of bottom-right corner, in range [0, 0.5 * PI).</returns>
let getSymBottomRightAng (sym: SymbolT.Symbol) (sheet: SheetT.Model): float =
    sheet.BoundingBoxes
    |> Map.tryFind sym.Id
    |> Option.bind (fun bbox -> Some (System.Math.Atan (bbox.H/bbox.W)))
    |> Option.defaultValue 0.


(* ---------------------------------------- Math Helpers ---------------------------------------- *)

/// <summary>Convert vector to angle.</summary>
/// <param name="vec">Target vector.</param>
/// <returns>Angle of range [0, 2 * PI).</returns>
let vecToAng (vec: XYPos): float =
    match vec.X >= 0., System.Math.Atan (vec.Y/vec.X) with
    | true, rad when rad >= 0. -> rad
    | true, rad when rad < 0. -> 2. * System.Math.PI + rad
    | false, rad when rad >= 0. -> 1. * System.Math.PI + rad
    | false, rad when rad < 0. -> 1. * System.Math.PI + rad
    | _ -> 0. // impossible case, but give typecheck warnings if not included

/// <summary>Shift angle anti-clockwise by a degree and normalize to [0, 2 * PI).</summary>
/// <param name="ang">Base angle, in radians.</param>
/// <param name="shift">Angle to shift by, in radians.</param>
/// <returns>Shifted angle.</returns>
let shiftAng (ang: float) (shift: float) = 
    let result = ang + shift
    if result > 2. * System.Math.PI
    then result - 2. * System.Math.PI
    else result

(* --------------------------------------- Implementation --------------------------------------- *)

/// <summary>Find optimum order of ports for a symbol with the clockface algorithm.</summary>
/// <param name="sym">Target symbol.</param>
/// <param name="sheet">Model of whole sheet.</param>
/// <returns>List of port IDs of target symbol in optimum order starting from the bottom right 
/// corner in anti-clockwise direction.</returns>
let findClockfacePortIdOrder (sym: SymbolT.Symbol) (sheet: SheetT.Model): List<string> =
    let symPortIds = getSymPortIds sym
    let symCentre = sheet.BoundingBoxes[sym.Id].Centre()

    let connectedWires = // find wires connect to this symbol
        getAllWires sheet
        |> List.filter 
            (fun wire -> 
                List.contains (inputPortStr wire.InputPort) symPortIds ||
                List.contains (outputPortStr wire.OutputPort) symPortIds)

    let connectedPorts = // find the other end of the connected wire
        connectedWires
        |> List.map 
            (fun wire -> 
                wire, 
                List.contains (inputPortStr wire.InputPort) symPortIds,
                List.contains (outputPortStr wire.OutputPort) symPortIds)
        |> List.map 
            (function
                | wire, true, false -> Some (inputPortStr wire.InputPort)
                | wire, false, true -> Some (outputPortStr wire.OutputPort)
                | _ -> None) // obtain the port that is not connected to symbol
        |> List.choose id
        |> List.map (fun portId -> Map.tryFind portId sheet.Wire.Symbol.Ports)
        |> List.choose id
    
    let clockfaceConnectedPortOrder = // sort the other end of the wires
        connectedPorts
        |> List.map (fun port -> (getPortPosOnSheet port sheet, port))
        |> List.map (fun (pos, port) -> (pos-symCentre, port))
        |> List.map (fun (vec, port) -> (vecToAng vec, port))
        |> List.map 
            (fun (ang, port) -> (shiftAng ang (getSymBottomRightAng sym sheet), port)) // shift to start from bottom-right
        |> List.sortBy (fun (shiftedRad, _) -> shiftedRad)
        |> List.map snd
    
    let clockfaceSymPortIdOrder = // find the port id of the symbol end of the wires
        clockfaceConnectedPortOrder
        |> List.map 
            (fun port -> 
                List.tryFind  
                    (fun (wire: BusWireT.Wire) -> 
                        inputPortStr wire.InputPort = port.Id || outputPortStr wire.OutputPort = port.Id) 
                    connectedWires) // recover the wire which has the port in the list
        |> List.choose id
        |> List.map 
            (fun wire -> 
                if List.contains (inputPortStr wire.InputPort) symPortIds
                then inputPortStr wire.InputPort
                else outputPortStr wire.OutputPort) // recover the end that is connect to the symbol
    
    clockfaceSymPortIdOrder
