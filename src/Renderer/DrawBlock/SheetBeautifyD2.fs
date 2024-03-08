module SheetBeautifyD2

open CommonTypes
open Helpers
open Optics
open Operators
open BlockHelpers

open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT

open SheetBeautifyHelpers

/// Optic to access SymbolT.Model from SheetT.Model
let symbolModel_ = SheetT.symbol_

//--------------------------------------------------------------------------------------//
//                               Helper Functions for D2                                //
//--------------------------------------------------------------------------------------//

// Get all the XY distances from a symbol's center to opposite ports connected to the symbol.
let getSymEdgePortAmount (sym: Symbol) : (Edge * int) list =
    let portMaps = sym.PortMaps.Order
    [Top; Right; Bottom; Left]
    |> List.map (fun e -> e, portMaps[e].Length)

let getSymOppPortDistances (sym: Symbol) (wModel: BusWireT.Model) (wire: Wire) : (string * XYPos) option =
    let symS, symT = getSourceSymbol wModel wire, getTargetSymbol wModel wire // Get both symbols at each end of the wire
    let portS, portT = (getSourcePort wModel wire).Id, (getTargetPort wModel wire).Id // Get both ports at each end of the wire

    let distFromSymCentreOppPort (oppPort: string) : XYPos =
        (getPortPos oppPort wModel) - (Symbol.getRotatedSymbolCentre sym)

    match (symS.Id = sym.Id), (symT.Id = sym.Id) with
    | true, false -> Some (portS, distFromSymCentreOppPort portT) // symS="sym"
    | false, true -> Some (portT, distFromSymCentreOppPort portS) // symT="sym"
    | _ -> None // symS or symT are not "sym"

let averageXYPos (lst: (string * XYPos) list) : XYPos =
    let sum = List.fold (fun sum tuple -> sum + (snd tuple)) XYPos.zero lst
    let n = float lst.Length
    { X = sum.X/n;  Y = sum.Y/n }

(*
Bearing from North      Bearing from Top-Left
  0rad                 0rad
 _|_                    \___
| | |                   |\  |
| | |                   | \ |
|   |                   |   | 
|___|                   |___|
*)
let XYPosToRadians (xyPos: XYPos) : double =
    let Pi = System.Math.PI
    let angleFromHorizontal = atan2 (abs xyPos.Y) (abs xyPos.X)
    let angleFromVertical = (Pi/2.0 - angleFromHorizontal)

    let bearingFromNorth = match xyPos.X, xyPos.Y with
                           | x, y when x >= 0.0 && y <= 0.0 -> angleFromVertical // Quadrant 1
                           | x, y when x >= 0.0 && y >= 0.0 -> angleFromHorizontal + Pi/2.0 // Quadrant 2
                           | x, y when x <= 0.0 && y >= 0.0 -> angleFromVertical + Pi // Quadrant 3
                           | x, y when x <= 0.0 && y <= 0.0 -> angleFromHorizontal + Pi*3.0/2.0 // Quadrant 4
                           | _ -> 0.0 // Should never happen
    let bearingFromTopLeft = (bearingFromNorth + Pi/4.0) % (2.0 * Pi)
    bearingFromTopLeft

let rotationallySortCustomSymPorts (model: SheetT.Model) (sym: Symbol) : Symbol =
    
    let applyPorts ((newSym, sortedPorts): Symbol * string list) ((edge, amount): Edge * int) =
        let front, back = List.splitAt amount sortedPorts
        (setPortMapsOrder newSym edge front), back

    let sortedPorts = model 
                      |> getAllWires // Get all Wires on the sheet
                      |> List.choose (getSymOppPortDistances sym model.Wire) // (portId, XY distance to other port)
                      |> List.groupBy (fun (portId, _) -> portId) // Group wires that have same source port
                      |> List.map (fun (portId, lst) -> portId, averageXYPos lst) // Average the XY distance for grouped Wires
                      |> List.map (fun (portId, xyPos) -> portId, XYPosToRadians xyPos) // Convert XY distance to radians from north
                      |> List.sortBy (fun (_, angle) -> angle) // Sort by Angle
                      |> List.map fst // List of sorted ports from Top, Right, Bottom, Left
    let portAmount = getSymEdgePortAmount sym // Amount of ports on edges Top, Right, Bottom, Left of a symbol, in order
    let newSym, _ = List.fold applyPorts (sym, sortedPorts) portAmount // Apply the ports on all sides of the sym and return newSym
    newSym





// Get all MUX Components on sheet to for the flipping to reducing wire bends
let getAllMuxComponents (model: SheetT.Model) : Symbol list =
    let AllSymbols = model.Wire.Symbol.Symbols
                    |> Map.toList
                    |> List.map snd
    AllSymbols |> List.filter (fun sym -> match sym.Component.Type with 
                                          | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 -> true 
                                          | _ -> false ) 

// Fold Function: Test the flip of sym to see how if Visible Bends have decreased, if so return that model
let testSymbolFlip ((currBends, currCross, currModel): int * int * SheetT.Model) (sym: Symbol) : int * int * SheetT.Model = 
    let flippedSym = setSymbolFlip (not (getSymbolFlip sym)) sym // Flip the Symbol
    
    let newSymModel = SymbolUpdate.replaceSymbol currModel.Wire.Symbol flippedSym sym.Id // Replace Symbol Model/sheet with newSymbol
    let newModel = Optic.set symbolModel_ newSymModel currModel // Set the Symbol Model into SheetModel
    let newBends = countVisibleBends newModel // Test the new model/sheet by counting Visible Bends
    let newCross = countVisibleSegsPerpendicularCrossings newModel // Test the new model/sheet by counting Visible Crossings
    
    match newBends <= currBends, newCross <= currCross with
    | true, true -> (newBends, newCross, newModel)
    | _, _ -> (currBends, currCross, currModel)

//--------------------------------------------------------------------------------------//
//                                D2 Partially Completed                                //
//--------------------------------------------------------------------------------------//

// Returns you the optimal Model that has minimal Wire Bends through:
// Brute force algorithm - flipping each Mux and seeing which flips help reduce wire bends.

// TODS:
// - Apply this to permute Symbols.
// - Change the ordering of Custom component ports by designing the circular function which 
//   will perform the circular ordering of Custom Component Ports.
let sheetOrderFlip (model: SheetT.Model) : SheetT.Model =
    let muxSyms = getAllMuxComponents model
    let currBends = countVisibleBends model
    let currCross = countVisibleSegsPerpendicularCrossings model
    let (_, _, optimModel) = List.fold testSymbolFlip (currBends, currCross, model) muxSyms
    optimModel

//--------------------------------------------------------------------------------------//
//                               Helper Functions for D3                                //
//--------------------------------------------------------------------------------------//

// Returns the wire along with its length in a tuple
let getWireLength (wire: Wire) : Wire * float =
    wire, List.fold (fun sum seg -> sum + seg.Length) 0.0 wire.Segments

// Get all the source ports of the wires in the list
let getPortId (wireList: Wire list) : OutputPortId list =
    List.map (fun wire -> wire.OutputPort) wireList

//--------------------------------------------------------------------------------------//
//                                D3 Partially Completed                                //
//--------------------------------------------------------------------------------------//

// Very initial start to D3, currently filters wires on a sheet into long and short wires

// TODOS:
// - Find a way of replacing longWires with labels (e.g using AddNotConnected).
// - Figure out where to position the label.
// - What names to give each label.
let sheetWireLabelSymbol (model: SheetT.Model) : SheetT.Model =
    let wires = getAllWires model
    let wireLengths = List.map getWireLength wires
    let longWires = wireLengths 
                    |> List.filter (fun (_, length) -> length > 500.0)  // Some threshold e.g 500
                    |> List.map fst // Remove the length from snd as Wire list is now filtered
    let sourcePortIds = getPortId longWires
    model

let test = [1; 2; 3; 4; 5; 6; 7; 8]
let test2 = List.splitAt 2 test