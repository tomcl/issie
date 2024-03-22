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

// Optic to access SymbolT.Model from SheetT.Model
let symbolModel_ = SheetT.symbol_

// Optic to access WireT.Model from SheetT.Model
let wireModel_ = SheetT.wire_

//--------------------------------------------------------------------------------------//
//                               Helper Functions for D2                                //
//--------------------------------------------------------------------------------------//

// Reroutes all wires of a symbol. Used after configuring/changing the ports of symbol
let rerouteSymbolWires (model: SheetT.Model) (sym: Symbol): SheetT.Model =
    let newWireModel = BusWireSeparate.routeAndSeparateSymbolWires model.Wire sym.Id // Reroutes wires
    Optic.set wireModel_ newWireModel model // Set the Wire Model into SheetModel   

// Returns the number of ports on each edge/side of a symbol as a list
let getSymEdgePortAmount (sym: Symbol) : (Edge * int) list =
    let portMaps = sym.PortMaps.Order
    [Top; Right; Bottom; Left]
    |> List.map (fun e -> e, portMaps[e].Length)

// Get all the XY distances from a symbol's center to opposite ports connected to the symbol.
let getSymOppPortDistances (sym: Symbol) (wModel: BusWireT.Model) (wire: Wire) : (string * XYPos) option =
    let symS, symT = getSourceSymbol wModel wire, getTargetSymbol wModel wire // Get both symbols at each end of the wire
    let portS, portT = (getSourcePort wModel wire).Id, (getTargetPort wModel wire).Id // Get both ports at each end of the wire

    let distFromSymCentreToPort (oppPort: string) : XYPos =
        (getPortPos oppPort wModel) - (Symbol.getRotatedSymbolCentre sym)

    match (symS.Id = sym.Id), (symT.Id = sym.Id) with
    | true, false -> Some (portS, distFromSymCentreToPort portT) // symS="sym"
    | false, true -> Some (portT, distFromSymCentreToPort portS) // symT="sym"
    | _ -> None // symS or symT are not "sym"

// Take the average over a list of XYPos'. Used when multiple wires (and therfore multiple opposite ports coming out of the same 
// output port in a custom symbol.
let averageXYPos (lst: (string * XYPos) list) : XYPos =
    let sum = List.fold (fun sum tuple -> sum + (snd tuple)) XYPos.zero lst
    let n = float lst.Length
    { X = sum.X/n;  Y = sum.Y/n }

// Obtain the distance from a sym's centre to a port (given as portId)
let distFromSymCentreToPort (portID: string) (model: SheetT.Model) (sym: Symbol) : XYPos =
    (getPortPos portID model.Wire) - (Symbol.getRotatedSymbolCentre sym)


(* Converts a given XYPos distance to a bearing in radians, from Top-Left:

Bearing from North   
   0rad  (x, y)  
 ___||___ *         
|   ||  *|         
|   ||*  |                
|        |              
|________|                

*)
let XYPosToRadians (xyPos: XYPos) : double =
    let Pi = System.Math.PI
    let angleFromHorizontal = atan2 (abs xyPos.Y) (abs xyPos.X)
    let angleFromVertical = (Pi/2.0 - angleFromHorizontal)

    match xyPos.X, xyPos.Y with
    | x, y when x >= 0.0 && y <= 0.0 -> angleFromVertical // Quadrant 1
    | x, y when x >= 0.0 && y >= 0.0 -> angleFromHorizontal + Pi/2.0 // Quadrant 2
    | x, y when x <= 0.0 && y >= 0.0 -> angleFromVertical + Pi // Quadrant 3
    | x, y when x <= 0.0 && y <= 0.0 -> angleFromHorizontal + Pi*3.0/2.0 // Quadrant 4
    | _ -> 0.0 // Should never happen

 // Get all MUX Components on sheet to for the flipping to reducing wire bends
let getAllCusComponents (model: SheetT.Model) : Symbol list =
    let AllSymbols = model.Wire.Symbol.Symbols
                    |> Map.toList
                    |> List.map snd
    AllSymbols |> List.filter (fun sym -> match sym.Component.Type with 
                                          | Custom _ -> true 
                                          | _ -> false ) 

// Get all MUX Components on sheet to for the flipping to reducing wire bends
let getAllMuxComponents (model: SheetT.Model) : Symbol list =
    let AllSymbols = model.Wire.Symbol.Symbols
                    |> Map.toList
                    |> List.map snd
    AllSymbols |> List.filter (fun sym -> match sym.Component.Type with 
                                          | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 -> true 
                                          | _ -> false ) 

// Fold Function: Used to build the order and orientation for a symbol given you its list of the sorted ports from
// Top, Right, Bottom, Left (in that order).
let applyPorts ((order, orient, sortedPorts): Map<Edge, list<string>> * Map<string, Edge> * list<string>) ((edge, amount): Edge * int) =
    let head, tail = List.splitAt amount sortedPorts

    let newOrder = order |> Map.add edge (List.rev head)
    let newOrient = (orient, (List.rev head)) ||> List.fold (fun orient portId -> Map.add portId edge orient)
    newOrder, newOrient, tail

// Returns you all ports of a symbol as a string list
let allPorts (sym: Symbol) : string list =
   sym.PortMaps.Order
    |> Map.toList
    |> List.map snd
    |> List.fold (fun allPorts ports -> allPorts @ ports) [] 

// Flips a symbol perpendicular to where it's facing
let flipPerpindicular (sym: Symbol) : Symbol =
    match sym.STransform.Rotation with
    | Degree0 | Degree180 -> SymbolResizeHelpers.flipSymbol FlipVertical sym // Facing horizontally
    | Degree90 | Degree270 -> SymbolResizeHelpers.flipSymbol FlipHorizontal sym // Facing vertically

// Puts the first half of the top ports to the end of the list of sorted ports
let reorderForTop (topAmount: int) (lst: string list) : string list =
    let head, tail = List.splitAt (lst.Length - topAmount/2) lst
    tail @ head

//--------------------------------------------------------------------------------------//
//                                     D2 Function                                      //
//--------------------------------------------------------------------------------------//

// Test a new flipped or rotationally sorted custom symbol and compare it to the old model. Only take new model if it reduces
// wire crossings and bends.
let testNewSym (newSym: Symbol) (currSym: Symbol) (currModel: SheetT.Model) (currBends: int) (currCross: int) = 
    // Replace Symbol Model/sheet with the flipped symbol (flippedSym)
    let newSymModel = SymbolUpdate.replaceSymbol currModel.Wire.Symbol newSym currSym.Id 
    
    // Set the Symbol Model into SheetModel, then reroute all the wires of newSym
    let newModel = rerouteSymbolWires (Optic.set symbolModel_ newSymModel currModel) newSym

    let newBends = countVisibleBends newModel // Test the new model/sheet by counting Visible Bends
    let newCross = countVisibleSegsPerpendicularCrossings newModel // Test the new model/sheet by counting Visible Crossings

    match newBends <= currBends, newCross < currCross with
    | true, true -> (newBends, newCross, newModel)
    | _, _ -> (currBends, currCross, currModel)


// Rotationally sorts all the ports of a custom symbol in order of the angles made from the symbol center to a port
let rotationallySortCustomSymPorts ((currBends, currCross, currModel): int * int * SheetT.Model) (sym: Symbol) : int * int * SheetT.Model =

    let allPorts = allPorts sym

    let connPortDistances = currModel 
                            |> getAllWires // Get all Wires on the sheet
                            |> List.choose (getSymOppPortDistances sym currModel.Wire) // (sym's portId, XY distance to other port)
                            |> List.groupBy (fun (portId, _) -> portId) // Group wires that have same source port
                            |> List.map (fun (portId, lst) -> portId, averageXYPos lst) // Average the XY distance for grouped Wires

    let notConnPortDistances = 
        let connPorts = connPortDistances |> List.map fst
        allPorts
        |> List.filter (fun portId -> not (List.contains portId connPorts ) ) // If portId does not exist in the connected ports
        |> List.map (fun portId -> portId, distFromSymCentreToPort portId currModel sym) // Likewise, get distance to non-connected port
    
    let sortedPorts = (connPortDistances @ notConnPortDistances)
                      |> List.map (fun (portId, xyPos) -> portId, XYPosToRadians xyPos) // Convert XY distance to radians from north
                      |> List.sortBy (fun (_, angle) -> angle) // Sort by Angle
                      |> List.map fst // List of sorted ports from Top, Right, Bottom, Left
                      |> reorderForTop sym.PortMaps.Order[Top].Length

    let portAmount = getSymEdgePortAmount sym // Amount of ports on edges Top, Right, Bottom, Left of a symbol, in order
    let newOrder, newOrient, _ = List.fold applyPorts (Map.empty, Map.empty, sortedPorts) portAmount // Create new PortMap with new sortedPorts
    let newSym = { sym with PortMaps = { Order = newOrder; Orientation = newOrient } } // replace sym with new PortMap

    testNewSym newSym sym currModel currBends currCross



// Fold Function: Test the flip of sym to see if visible bends/crossings have decreased, if so return that model.
let testSymbolFlip ((currBends, currCross, currModel): int * int * SheetT.Model) (sym: Symbol) : int * int * SheetT.Model = 
    let flippedSym = (flipPerpindicular sym) // Flip the Symbol
    testNewSym flippedSym sym currModel currBends currCross



// Main Function Call for the D2 Deliverable. 
// - Will rotationally sort all custom symbol ports
// - Will test all different permutations of flips on Muxes
let sheetOrderFlip (model: SheetT.Model) : SheetT.Model =
    // Get all the Muxes and Custom Symbols
    let muxSyms, cusSyms = getAllMuxComponents model, getAllCusComponents model

    // First rotationally sort the ports of all the custom components
    let currBends = countVisibleBends model
    let currCross = countVisibleSegsPerpendicularCrossings model
    let (newBends, newCross, cusRotatedModel) = List.fold rotationallySortCustomSymPorts (currBends, currCross, model) cusSyms

    // Try different permutations of flips/rotations on Muxes
    let (_, _, optimModel) = List.fold testSymbolFlip (newBends, newCross, cusRotatedModel) muxSyms
    
    printfn "count crossings: %A" (countVisibleSegsPerpendicularCrossings optimModel)

    // Return the optimal model
    optimModel
