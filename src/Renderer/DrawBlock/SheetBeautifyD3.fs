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
let middleOfSheet = { X = maxSheetCoord / 2.; Y = maxSheetCoord / 2. }

//type SymbolPortRec = { Label: string; PortNumber: int }
type SymbolPortRec = {
    SymbolId: ComponentId
    PortNumber: int
}

/// Used throughout to compare labels since these are case invariant "g1" = "G1"
let caseInvariantEqual str1 str2 =
    String.toUpper str1 = String.toUpper str2


// Beginning with wire length
// Second Phase enhancements:
// Explore using number of wire crossings, symbol crossings, proximity to symbols or number of wire corners (complexity) as customisable replacement metrics

/// Find the symbols connected to each end of a wire and the respective portIds that the wire is connected to
let getWireSymbolsAndPort (wireId: ConnectionId) (model: SheetT.Model) =
    let wire = model.Wire.Wires.[wireId]
    let sourceSymbol = getSourceSymbol model.Wire wire
    let targetSymbol = getTargetSymbol model.Wire wire

    let targetPortId = InputId wire.InputPort
    let sourcePortId = OutputId wire.OutputPort

    ((sourceSymbol, sourcePortId), (targetSymbol, targetPortId))

let placeWireLabelSymbol (symLabel: string) (model: SheetT.Model) (position: XYPos) :  Result<(Model * ComponentId), string> =
    let symLabel = String.toUpper symLabel
    let wlSymModel, wlSymId =
        SymbolUpdate.addSymbol [] (model.Wire.Symbol) position IOLabel symLabel
    let wlSym = wlSymModel.Symbols[wlSymId]
    match position + wlSym.getScaledDiagonal with
    | { X = x; Y = y } when x > maxSheetCoord || y > maxSheetCoord ->
        Error $"symbol '{symLabel}' position {position + wlSym.getScaledDiagonal} lies outside allowed coordinates"
    | _ ->
        let updatedModel = 
            model
            |> Optic.set symbolModel_ wlSymModel
            |> updateBoundingBoxes
        Ok (updatedModel, wlSymId)

let placeWireX (source: SymbolPortRec) (target: SymbolPortRec) (model: SheetT.Model) : Result<SheetT.Model*ConnectionId, string> =
    let getPort (symPortRec: SymbolPortRec) (portType: PortType) =
        match model.Wire.Symbol.Symbols |> Map.tryFind symPortRec.SymbolId with
        | Some symbol ->
            let ports = match portType with
                        | PortType.Input -> symbol.Component.InputPorts
                        | PortType.Output -> symbol.Component.OutputPorts
            match ports |> List.tryItem symPortRec.PortNumber with
            | Some port -> Ok port.Id
            | None -> Error $"Can't find {portType} port {symPortRec.PortNumber} on symbol with ID {symPortRec.SymbolId}"
        | None -> Error $"Can't find symbol with ID {symPortRec.SymbolId}"

    match getPort source PortType.Output, getPort target PortType.Input with
    | Error e, _
    | _, Error e -> Error e
    | Ok outPort, Ok inPort ->
        let newWire =
            BusWireUpdate.makeNewWire (InputPortId inPort) (OutputPortId outPort) model.Wire
        if model.Wire.Wires |> Map.exists (fun wid wire -> wire.InputPort = newWire.InputPort && wire.OutputPort = newWire.OutputPort) then
            Error "Can't create wire because a wire already exists between those ports"
        else
            let updatedModel = model |> Optic.set (busWireModel_ >-> BusWireT.wireOf_ newWire.WId) newWire
            Ok (updatedModel, newWire.WId)

let deleteWire (wireId: ConnectionId) (model: SheetT.Model) =
    let newWires =
        model.Wire.Wires
        |> Map.filter (fun id _ -> id <> wireId)

    { model with Wire = { model.Wire with Wires = newWires } }

let autoAdjustWLRotation (wlSymId: ComponentId) (symId: ComponentId) (portId: PortId) (model: SheetT.Model) : SheetT.Model =
    let symbolsMap = model.Wire.Symbol.Symbols
    let symLens = symbolOf_ symId
    let sym = Optic.get symLens model
    let symRotation = Optic.get symbol_rotation_ sym
    let symFlip = Optic.get symbol_flipped_ sym

    match symbolsMap |> Map.tryFind wlSymId with
        | Some wireLabel ->
            let portEdge = getPortOrientation model.Wire.Symbol portId 
            let adjustedSymbol = match portEdge with
                                    | Top | Bottom -> 
                                        let rotationCheck wireLabel = 
                                            match symRotation with 
                                            | Degree180 | Degree270 -> SymbolResizeHelpers.rotateSymbol Degree270 wireLabel
                                            | _ -> SymbolResizeHelpers.rotateSymbol Degree90 wireLabel

                                        let flipCheck wireLabel = 
                                            match symFlip with 
                                            | true -> wireLabel
                                            | false -> wireLabel

                                        wireLabel |> rotationCheck |> flipCheck
                                    | Left | Right -> 
                                        let rotationCheck wireLabel = 
                                            match symRotation with 
                                            | Degree180 | Degree90 -> SymbolResizeHelpers.rotateSymbol Degree180 wireLabel
                                            | _ -> wireLabel
                                        
                                        let flipCheck wireLabel = 
                                            match symFlip with 
                                            | true -> SymbolResizeHelpers.rotateSymbol Degree180 wireLabel
                                            | false -> wireLabel
                                        
                                        wireLabel |> rotationCheck |> flipCheck
            let updatedSymbols = Map.add wlSymId adjustedSymbol symbolsMap
            { model with Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbols } } }
        | None -> model 

let addWireLabel (wlName: string) (portId: PortId) (symId: ComponentId) (isSourceWL: bool) (model: SheetT.Model) : SheetT.Model =
    let portIdStr = getPortIdStr portId
    let portPos = SheetBeautifyHelpers.getPortPos portIdStr model.Wire.Symbol
    
    let PortIndex  =  
        match model.Wire.Symbol.Symbols |> Map.tryFind symId with
        | Some symbol ->  
            let allPorts = 
                if isSourceWL then 
                    symbol.Component.OutputPorts
                else 
                    symbol.Component.InputPorts

            allPorts 
            |> List.mapi (fun idx port -> (idx, port.Id))
            |> List.tryFind (fun (_, pid) -> pid = portIdStr)
            |> Option.map fst
            |> Option.get
        | None -> failwith "Symbol ID not found in model"

    let calcWLPos (portPos: XYPos)  = 
        let portEdge = getPortOrientation model.Wire.Symbol portId 
        
        match portEdge with
        | Right -> 
             { X = portPos.X + 40.0; Y = portPos.Y }
        | Left -> 
             { X = portPos.X - 40.0; Y = portPos.Y }
        | Bottom -> 
            { X = portPos.X + 15.0; Y = portPos.Y + 40.0 }
        | Top -> 
            { X = portPos.X + 15.0; Y = portPos.Y - 40.0 }

    let placeWlAtPort (model: SheetT.Model) (portPos: XYPos) =
        let wireLabelName = wlName + "/" + string PortIndex
       
        portPos
        |> calcWLPos 
        |> placeWireLabelSymbol wireLabelName model 
        
    match placeWlAtPort model portPos with
    | Ok (updatedModel, wlSymId) ->
        let wlRec = {SymbolId = wlSymId; PortNumber = 0}
        let symRec = {SymbolId = symId; PortNumber = PortIndex}

        let modelWithAdjustedWL = autoAdjustWLRotation wlSymId symId portId updatedModel
        
        let wireResult =
            if isSourceWL then
                placeWireX symRec wlRec modelWithAdjustedWL
            else
                placeWireX wlRec symRec modelWithAdjustedWL

        match wireResult with
        | Ok (finalModel,_) -> finalModel 
        | Error wireError -> 
            printfn "Error placing wire: %s" wireError
            updatedModel 

    | Error errMsg ->
        printfn "Error placing wire label: %s" errMsg
        model 


let replaceLongWiresWithLabels (model: SheetT.Model) (lengthThreshold: float) : SheetT.Model =
    let findLongWires =
        model.Wire.Wires
        |> Map.toList
        |> List.filter (fun (_, wire) ->
            let wireLength = getWireLength wire
            wireLength > lengthThreshold)
        |> List.map fst

    let replaceEachWire (model: SheetT.Model) (wireId: ConnectionId) =
        let (sourceSymbol, sourcePortId), (targetSymbol, targetPortId) = getWireSymbolsAndPort  wireId model
        printfn "SourceSymbol: %A" sourceSymbol.Component.Label 
        printfn "TargetSymbol: %A" targetSymbol.Component.Label
        let modelWithTargetLabels = addWireLabel sourceSymbol.Component.Label targetPortId targetSymbol.Id false model
        let modelWithSourceLabels = addWireLabel sourceSymbol.Component.Label sourcePortId sourceSymbol.Id true modelWithTargetLabels
        deleteWire wireId modelWithSourceLabels



    findLongWires |> List.fold replaceEachWire model




// findLongWires
// |> List.fold (fun model (wireId, wire) ->
//     let wireLabel = placeWireLabel wire model

//     let modelWithoutWire = DeleteWiresWithPort [Some wire.InputPort; Some wire.OutputPort] model

//     let addLabel = placeWireLabel label modelWithoutWire
//     )













// Section C ->> Bit legends /  Symbol rendering / Adjustments / repositioning
// To be implemented when I have a better idea of its current testing performance
