module SmartHelpers
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers

open Optics
open Operators


//-----------------------------------------------------------------------------------------------//
//---------------------------HELPERS FOR SMART DRAW BLOCK ADDITIONS------------------------------//
//-----------------------------------------------------------------------------------------------//

(*
HOW TO USE THIS MODULE.

(1) Add well-documented useful functions - see updateModelSymbols and updateModelWires
    for examples. You do not need to add performance information as in updateModelSymbols. 
    Your priority should be writing clear code. Try to avoid very inefficient implementations
    if possible (e.g. 100X slower than a similar complexity solution), but do not worry 
    about this.
(2) Note from my examples distinction between XML documentation and additional details
    in header comments.
(3) HLP23: Note comments here labelled "HLP23" which are for HLP23 class and would be deleted in
    production (Group phase) code.
(2) HLP23: Each function must have a single author specified by "HLP23: AUTHOR" in an XML comment
    as in my example: give name as Family name only (unique within teams).
(3) HLP23: Inform other members that you have written a function they could maybe use.
(4) HLP23: If two people end up with near-identical functions here team phase can rationalise if
    needed normally you are expected to share when this makes code writing faster.
(5) Note best practice here using Optics for nested record update. This is NOT curently required
    in Issie but used appropriately results in better code. Use it if you are comfortable doing so.
(5) Note on qualifying types: do this when not doing it would be ambiguous - e.g. here
    the BusWire and Symbol Model types.
(6) Note on code layout. A limit of 100 characters per line is used here. Seems about right.
*)

//----------------------------------------------------------------------------------------------//

/// Update BusWire model with given symbols. Can also be used to add new symbols.
/// This uses a fold on the Map to add symbols which makes it fast in the case that the number
/// of symbols added is very small.
//  Performance scales as O(M*log2(N)) - M = number of symbols added, N = number of existing
//  Symbols. Changing large maps is relatively expensive hence the care here.
//  This function uses best practice for nested record update with Optics. See Wiki for
//  details. Note that Optics are probably a little bit slower than F# record update (factor of 2)
//  however in this case it does not matter because the time taken is << the Map update time.
/// HLP23: AUTHOR Clarke
let updateModelSymbols 
    (model: BusWireT.Model) 
    (symbols: Symbol list)
        : BusWireT.Model =
    // HLP23: note on fold implementation. symMap is both argument and result of the
    // fold function => sequential set of updates. In thsi case much more efficient than Map.map
    // over all symbols.
    // HLP23 - see also similar updateModelWires
    let symbols' =
        (model.Symbol.Symbols,symbols)
        ||> List.fold (fun symMap symToAdd -> Map.add symToAdd.Id symToAdd symMap)
    Optic.set (symbol_ >-> symbols_) symbols' model

/// Update BusWire model with given wires. Can also be used to add new wires.
/// This uses a fold on the Map to add wires which makes it fast in the case that the number
/// of wires added is small.

//  Performance scales as O(M*log2(N)) - M = number of wires added, N = number of existing
//  wires. Changing large maps is relatively expensive hence the care here.
//  This function uses best practice for nested record update with Optics. See Wiki for
//  details. Note that Optics are probably a little bit slower than F# record update (factor of 2)
//  however in this case it does not matter because the time taken is << the Map update time.
/// HLP23: AUTHOR Clarke
let updateModelWires 
    (model: BusWireT.Model) 
    (wiresToAdd: Wire list)
        : BusWireT.Model =
    //
    // HLP23: note on fold implementation. In this (typical) example Map is
    // sequentially updated by the fold. A common and difficult to see coding mistake is to use the 
    // original wireMap (argument of Optic map function) not the updated one (wireMap argument of 
    // List.map folder) in the fold function! That is not possible here because both have the same 
    // name so the inner bound updated wireMap is always what is used in the folder function. 
    // This is good practice, and if you have ever debugged this type of mistake you will know it
    // is very necessary!

    // HLP23: note on this use of Optics.map in a pipeline. It is more "functional" than the 
    // equivalent implementation using a let definition and Optics.set. Is it clearer? Or less clear? 
    // Standard logic says we should prefer the pipeline if the name of the let definition adds 
    // nothing which is the case here. I have given you both ways of using Optics here so you can 
    // compare the two implementations and decide. NB - you are NOT required to use Optics in your 
    // own code.
    //
    // HLP23: Note how multiple updates to different parts of the model can be neatly pipelined 
    // like this using a separate Optic.map or Optic.set for each.
    //
    // HLP23: note that if the operation here was larger or part of some pipeline the
    // 2nd argument to Optic.map - which defines the model change - could be given a name and 
    // turned into a local function making the Optic.map line like:
    // |> Optic.map wires_ myNameForThisWireMapUpdate
    model
    |> Optic.map wires_ (fun wireMap  ->
        (wireMap,wiresToAdd)
        ||> List.fold (fun wireMap wireToAdd -> Map.add wireToAdd.WId wireToAdd wireMap))


/// HLP23: Author Omar
/// record type for symbol box coordinates
type SymbolBoxT = { TopLeft: XYPos; TopRight: XYPos; BottomLeft: XYPos; BottomRight: XYPos }


/// HLP23: Author Omar
/// returns a record type for a symbol's box coordinates given the symbol
let symbolBox (symbol: Symbol) : SymbolBoxT = 
    let topLeft: XYPos ={X = symbol.Pos.X; Y = symbol.Pos.Y}
    let topRight = { X = topLeft.X + symbol.Component.W; Y = topLeft.Y}
    let bottomLeft = {X = topLeft.X; Y = topLeft.Y + symbol.Component.H }
    let bottomRight = {X = topRight.X; Y = bottomLeft.Y}
    { TopLeft = topLeft; TopRight = topRight; BottomLeft = bottomLeft; BottomRight = bottomRight }


/// HLP23: Author Indraneel
/// Finds all the InterConnecting Wires between 2 symbols given WireModel.Wires and the 2 symbols
/// and a flag to get either all interconnecting wires or just wires in the direction from otherSymbol->symbolToOrder
let findInterconnectingWires (wireList:List<Wire>) (sModel)
    (symbolToOrder:Symbol) 
    (otherSymbol:Symbol) (getAllConnections:int) =
        
    wireList
    |> List.filter (fun value ->

        let inputPortHostId = string sModel.Ports[string value.InputPort].HostId
        let outputPortHostId = string  sModel.Ports[string value.OutputPort].HostId

        let symbolToOrderId = string symbolToOrder.Id
        let otherSymbolId = string otherSymbol.Id
        
        if (getAllConnections = 0) then
            (inputPortHostId = symbolToOrderId) && (outputPortHostId = otherSymbolId)
        else
            if ((inputPortHostId = symbolToOrderId) && (outputPortHostId = otherSymbolId)) then true
            elif ((inputPortHostId = otherSymbolId) && (outputPortHostId = symbolToOrderId)) then true
            else false

        )


/// HLP23: Author Indraneel
/// Returns the symbol's port index corresponding to the portId
let getSymbolIndex (symbol: Symbol) (portId: string) = 
    let edge = symbol.PortMaps.Orientation[portId]

    symbol.PortMaps.Order[edge]
    |> List.findIndex (fun elm -> elm = portId)


/// HLP23: Author Indraneel
/// Returns an option
/// in the form of an updated List otherwise None
let tryUpdateAt index value list =
    if index >= 0 && index < List.length list then
        Some (List.updateAt index value list)
    else
        None


/// HLP23: Author Indraneel
/// Returns a symbol option given a model an inputPort/outputPort
let findSymbolHelper (wModel: BusWireT.Model) (portId: string) = 
    let inputPortHostId = string wModel.Symbol.Ports[portId].HostId

    wModel.Symbol.Symbols
    |> Map.tryFind (ComponentId inputPortHostId)

    

/// HLP23: Author Omar
/// discriminated union for the modes of the findSymbol function
type findSymbolMode = Input| Output

/// HLP23: Author Omar
/// finds matching symbol in model for given port ids on wire 
let findSymbol (model: Model) (wire: Wire) (mode: findSymbolMode) : Symbol option = 
    let port = 
        match mode with
        | Input -> string wire.InputPort
        | Output -> string wire.OutputPort

    findSymbolHelper model port


/// HLP23: Author Omar
/// returns a new wire with updated segments based on the given list of segment lengths
let updateWire (wire: Wire) (lengths: float list) : Wire = 
    let newSegments = 
        lengths
        |> List.mapi (fun i length -> {wire.Segments[i] with Length = length})
    
    {wire with Segments = newSegments}


/// HLP23: Author Omar
/// discriminated union for return type of the smart autoroute function and other SmartWire functions
/// Used in the SmartWire module and BusWireUpdate module
type SmartAutorouteResult =
    | ModelT of Model
    | WireT of Wire


/// HLP23: AUTHOR Ifte
/// Returns corresponding orientation if symbols have overlapping X/Y coverage and returns None if both
let getOrientation fstSym sndSym =
    let fstCorners = symbolBox fstSym
    let sndCorners = symbolBox sndSym
    if (((fstCorners.TopLeft.Y > sndCorners.BottomLeft.Y) || (fstCorners.BottomLeft.Y < sndCorners.TopLeft.Y)) 
        && (fstCorners.TopLeft.X < sndCorners.TopRight.X)) 
        && (fstCorners.TopRight.X > sndCorners.TopLeft.X) 
    then
        Some Vertical
    else if (((fstCorners.TopLeft.X > sndCorners.TopRight.X) || (fstCorners.TopRight.X < sndCorners.TopLeft.X)) 
            && (fstCorners.TopLeft.X < sndCorners.BottomLeft.Y) 
            && (fstCorners.BottomLeft.Y > sndCorners.TopLeft.Y)) 
    then
        Some Horizontal
    else
        None


/// HLP23: AUTHOR Ifte
/// Wire update helpers which are lower in compiler order
type BusWireHelpers = {
    updateWire: Model -> Wire -> bool -> SmartAutorouteResult
    updateSymbolWires: Model -> ComponentId -> Model
    }

