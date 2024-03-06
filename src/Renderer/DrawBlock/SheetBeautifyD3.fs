module SheetBeautifyD3

open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open SheetUpdate
open SheetBeautifyHelpers
open Optics
open Optics.Operators
open BlockHelpers
open Sheet
open EEExtensions
open Symbol
open Helpers


/// Optic to access SymbolT.Model from SheetT.Model
let symbolModel_ = symbol_


/// Optic to access BusWireT.Model from SheetT.Model
let busWireModel_ = wire_

/// allowed max X or y coord of svg canvas
let maxSheetCoord = Constants.defaultCanvasSize
let middleOfSheet = {X=maxSheetCoord/2.;Y=maxSheetCoord/2.}

type SymbolPort = { Label: string; PortNumber: int }

/// Used throughout to compare labels since these are case invariant "g1" = "G1"
let caseInvariantEqual str1 str2 =
    String.toUpper str1 = String.toUpper str2




// Section A ->> User Defined Threshold
// Function to calculate user defined threshold
// Input undecided, ideas for measurement include features:
// - Feature to selected individual wires for wire label replacement (rather than all)
// - Menu option to adjust 
// - During placement
// Left input until group phase as I was thinking a possible implementation would be a general menu with tick box to "beautify sheet"
// Here the user could adjust all the metrics for this, including beautify functions from D1, D2 etc and toggle the tick box
// Since this requires a discussion with other members of the team about their implmentation and total integration, this section is left for the teamwork
// Output is a maximum wire length for now (see B)

// Will have a default threshold to begin with
let determineUserThreshold () : float = 
    failwithf "Not Implemented"


// Section B ->> Acting on threshold conditions 
// Heuristics for when to use wire labels is undecided - I am beginning with wire length
// Second Phase enhancements:
// Explore using number of wire crossings, symbol crossings, proximity to symbols or number of wire corners (complexity) as customisable replacement metrics

/// Find the symbols connected to each end of a wire
let getConnectedSymbols (wireId: ConnectionId) (model: SheetT.Model) = 
    let wire = model.Wire.Wires.[wireId]
    let sourceSymbol = getSourceSymbol model.Wire wire
    let targetSymbol = getTargetSymbol model.Wire wire
    
    sourceSymbol, targetSymbol

/// Return a list of portIds and their respective positions given a sym at either the start or end of a wire
/// Parameter isInputWL is temporary for now and allows us to chose between whether we are using the symbol at the start or end of the wire
let getSymPortNamesAndPos (sym: Symbol) (isInputWL: bool) (model: SheetT.Model) : (string * XYPos) list = 
    let ports = 
        if isInputWL then sym.Component.OutputPorts             
        else sym.Component.InputPorts
    
    let portLabelAndPos (port: Port) = 
        let portSheetPosition = portPos model port.Id
        (port.Id, portSheetPosition)

    ports
    |> List.map portLabelAndPos 


// SymLabel will be determined by a combination of the component and port names at the symbols on each end of the wire
// Position will be based off the port locations on these symbols
/// Places a wire label on the sheet 
let placeWireLabelSymbol (symLabel: string) (position: XYPos) (model: SheetT.Model) : Result<SheetT.Model, string> =
    let symLabel = String.toUpper symLabel
    let wlSymModel, wlSymId = SymbolUpdate.addSymbol [] (model.Wire.Symbol) position IOLabel symLabel
    let wlSym = wlSymModel.Symbols[wlSymId]
    match position + wlSym.getScaledDiagonal with
      {X=x;Y=y} when x > maxSheetCoord || y > maxSheetCoord ->
        Error $"symbol '{symLabel}' position {position + wlSym.getScaledDiagonal} lies outside allowed coordinates"
    | _ ->
        model
        |> Optic.set symbolModel_ wlSymModel
        |> updateBoundingBoxes 
        |> Ok
 
// To be used when we put full implementation together -> When toggling between beautifying the sheet or not (if we make this an option) or some other option
// It is important for adding features which allow the user to reverse the changes made when placing a wire symbol
/// Places a wire between two symbols
let placeWire (source: SymbolPort) (target: SymbolPort) (model: SheetT.Model) : Result<SheetT.Model,string> =
    let symbols = model.Wire.Symbol.Symbols
    let getPortId (portType:PortType) symPort =
        mapValues symbols
        |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symPort.Label)
        |> function | Some x -> Ok x | None -> Error "Can't find symbol with label '{symPort.Label}'"
        |> Result.bind (fun sym ->
            match portType with
            | PortType.Input -> List.tryItem symPort.PortNumber sym.Component.InputPorts
            | PortType.Output -> List.tryItem symPort.PortNumber sym.Component.OutputPorts
            |> function | Some port -> Ok port.Id
                        | None -> Error $"Can't find {portType} port {symPort.PortNumber} on component {symPort.Label}")
    
    match getPortId PortType.Input target, getPortId PortType.Output source with
    | Error e, _ | _, Error e -> Error e
    | Ok inPort, Ok outPort ->
        let newWire = BusWireUpdate.makeNewWire (InputPortId inPort) (OutputPortId outPort) model.Wire
        if model.Wire.Wires |> Map.exists (fun wid wire -> wire.InputPort=newWire.InputPort && wire.OutputPort = newWire.OutputPort) then
                // wire already exists
                Error "Can't create wire from {source} to {target} because a wire already exists between those ports"
        else
                model
                |> Optic.set (busWireModel_ >-> BusWireT.wireOf_ newWire.WId) newWire
                |> Ok


// Implement delete wire feature once we can visualise wire placement
let replaceWireWithLabelOnOutput (wireId: ConnectionId) (sym: Symbol) (model: SheetT.Model) = 
    let wire = model.Wire.Wires.[wireId]
    let portsList = getSymPortNamesAndPos sym true model
    
    let placeWlAtPort (model: SheetT.Model) (portID: string , portPos: XYPos) = 
        let wlPos = { X = portPos.X + 10.0; Y = portPos.Y}
        match placeWireLabelSymbol portID wlPos model with 
        | Ok updatedModel -> updatedModel
        | Error errMsg -> 
            printfn "Error: %s" errMsg
            model

    portsList 
    |> List.fold placeWlAtPort model
        
        
// ALTERNATIVE FUNCTION: If we decide that using visible segments is better in testing, this is the alternative to getWireLength
// working with the visible segment coordinate outputs
// /// Calculates the length of the visible wire
// let calcWireLength (wireId: ConnectionId) (model: SheetT.Model): float = 
//         visibleSegments wireId model
//         |> List.sumBy (fun seg -> abs seg.X + abs seg.Y)



// Options for calling this function:
// - "Beautify Sheet" toggle as mentioned in section A
// - Every time any sort of move is performed that is related to a wire e.g. moving symbol, moving wire etc
/// Replaces wires whos length exceed a threshold, with a wire label
let replaceLongWiresWithLabels (model: SheetT.Model) (lengthThreshold: float): SheetT.Model = 
    let findLongWires = 
        model.Wire.Wires
        |> Map.toList
        |> List.filter (fun (_, wire) ->
            let wireLength = getWireLength wire 
            wireLength > lengthThreshold)
        |> List.map fst


    
    // findLongWires
    // |> List.fold (fun model (wireId, wire) -> 
    //     let wireLabel = placeWireLabel wire model
        
    //     let modelWithoutWire = DeleteWiresWithPort [Some wire.InputPort; Some wire.OutputPort] model

    //     let addLabel = placeWireLabel label modelWithoutWire 
    //     )

    failwithf "Implement replacement logic"





// Section C ->> Bit legends /  Symbol rendering / Adjustments / repositioning
// To be implemented when I have a better idea of its current testing performance