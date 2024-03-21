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

let placeWireLabelSymbol (symLabel: string) (position: XYPos) (model: SheetT.Model) :  Result<(Model * ComponentId), string> =
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


let addWireLabel (wlName: string) (portId: PortId) (symId: ComponentId) (isSourceWL: bool) (model: SheetT.Model) : SheetT.Model =
    let portIdStr = getPortIdStr portId
    let portPos = SheetBeautifyHelpers.getPortPos portIdStr model.Wire.Symbol
    
    printfn "%A" portPos
    printfn "%A" wlName

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

    printfn "%A" PortIndex

    let findWLPos  (portPos: XYPos) = 
        let symLens = symbolOf_ symId  
        let symRotation = Optic.get symbol_rotation_ (Optic.get symLens model)
        // let wlLens = symbolOf_ wlId  
        // let wlRotation = Optic.get symbol_rotation_ (Optic.get wlLens model)
        
        match symRotation with
        | Degree0 -> 
            if isSourceWL then { X = portPos.X + 40.0; Y = portPos.Y }
            else { X = portPos.X - 40.0; Y = portPos.Y }
        | Degree90 -> 
            if isSourceWL then { X = portPos.X; Y = portPos.Y - 40.0 }
            else { X = portPos.X; Y = portPos.Y + 40.0 }
        | Degree180 -> 
            if isSourceWL then { X = portPos.X - 40.0; Y = portPos.Y }
            else { X = portPos.X + 40.0; Y = portPos.Y }
        | Degree270 -> 
            if isSourceWL then { X = portPos.X; Y = portPos.Y + 40.0 }
            else { X = portPos.X; Y = portPos.Y - 40.0 }

    let placeWlAtPort (model: SheetT.Model) (portPos: XYPos) =
        let wlPos = findWLPos portPos

        placeWireLabelSymbol wlName wlPos model 
        
    match placeWlAtPort model portPos with
    | Ok (updatedModel, wlSymId) ->
        let wlRec = {SymbolId = wlSymId; PortNumber = 0}
        let symRec = {SymbolId = symId; PortNumber = PortIndex}

        let wireResult =
            if isSourceWL then
                placeWireX symRec wlRec updatedModel
            else
                placeWireX wlRec symRec updatedModel

        match wireResult with
        | Ok (finalModel,_) -> finalModel 
        | Error wireError -> 
            printfn "Error placing wire: %s" wireError
            updatedModel 

    | Error errMsg ->
        printfn "Error placing wire label: %s" errMsg
        model 
        

  

// ALTERNATIVE FUNCTION: If we decide that using visible segments is better in testing, this is the alternative to getWireLength
// working with the visible segment coordinate outputs
// /// Calculates the length of the visible wire
// let calcWireLength (wireId: ConnectionId) (model: SheetT.Model): float =
//         visibleSegments wireId model
//         |> List.sumBy (fun seg -> abs seg.X + abs seg.Y)


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
