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
type SymbolPortRec = { SymbolId: ComponentId; PortNumber: int }

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

let placeWireLabelSymbol
    (symLabel: string)
    (model: SheetT.Model)
    (position: XYPos)
    : Result<(Model * ComponentId), string>
    =
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
        Ok(updatedModel, wlSymId)

let placeWireX
    (source: SymbolPortRec)
    (target: SymbolPortRec)
    (model: SheetT.Model)
    : Result<SheetT.Model * ConnectionId, string>
    =
    let getPort (symPortRec: SymbolPortRec) (portType: PortType) =
        match
            model.Wire.Symbol.Symbols
            |> Map.tryFind symPortRec.SymbolId
        with
        | Some symbol ->
            let ports =
                match portType with
                | PortType.Input -> symbol.Component.InputPorts
                | PortType.Output -> symbol.Component.OutputPorts
            match ports |> List.tryItem symPortRec.PortNumber with
            | Some port -> Ok port.Id
            | None ->
                Error $"Can't find {portType} port {symPortRec.PortNumber} on symbol with ID {symPortRec.SymbolId}"
        | None -> Error $"Can't find symbol with ID {symPortRec.SymbolId}"

    match getPort source PortType.Output, getPort target PortType.Input with
    | Error e, _
    | _, Error e -> Error e
    | Ok outPort, Ok inPort ->
        let newWire =
            BusWireUpdate.makeNewWire (InputPortId inPort) (OutputPortId outPort) model.Wire
        if
            model.Wire.Wires
            |> Map.exists (fun wid wire ->
                wire.InputPort = newWire.InputPort
                && wire.OutputPort = newWire.OutputPort)
        then
            Error "Can't create wire because a wire already exists between those ports"
        else
            let updatedModel =
                model
                |> Optic.set (busWireModel_ >-> BusWireT.wireOf_ newWire.WId) newWire
            Ok(updatedModel, newWire.WId)

let deleteWire (wireId: ConnectionId) (model: SheetT.Model) =
    let newWires =
        model.Wire.Wires
        |> Map.filter (fun id _ -> id <> wireId)

    { model with Wire = { model.Wire with Wires = newWires } }

let autoAdjustWLRotation
    (wlSymId: ComponentId)
    (symId: ComponentId)
    (portId: PortId)
    (model: SheetT.Model)
    : SheetT.Model
    =
    let symbolsMap = model.Wire.Symbol.Symbols
    let symLens = symbolOf_ symId
    let sym = Optic.get symLens model
    let symRotation = Optic.get symbol_rotation_ sym
    let symFlip = Optic.get symbol_flipped_ sym

    match symbolsMap |> Map.tryFind wlSymId with
    | Some wireLabel ->
        let portEdge = getPortOrientation model.Wire.Symbol portId
        let adjustedSymbol =
            match portEdge with
            | Top
            | Bottom ->
                let rotationCheck wireLabel =
                    match symRotation with
                    | Degree180
                    | Degree270 -> SymbolResizeHelpers.rotateSymbol Degree270 wireLabel
                    | _ -> SymbolResizeHelpers.rotateSymbol Degree90 wireLabel

                let flipCheck wireLabel =
                    match symFlip with
                    | true -> wireLabel
                    | false -> wireLabel

                wireLabel |> rotationCheck |> flipCheck
            | Left
            | Right ->
                let rotationCheck wireLabel =
                    match symRotation with
                    | Degree180
                    | Degree90 -> SymbolResizeHelpers.rotateSymbol Degree180 wireLabel
                    | _ -> wireLabel

                let flipCheck wireLabel =
                    match symFlip with
                    | true -> SymbolResizeHelpers.rotateSymbol Degree180 wireLabel
                    | false -> wireLabel

                wireLabel |> rotationCheck |> flipCheck
        let updatedSymbols = Map.add wlSymId adjustedSymbol symbolsMap
        { model with
            Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbols } } }
    | None -> model

let addWireLabel
    (wlName: string)
    (portId: PortId)
    (symId: ComponentId)
    (isSourceWL: bool)
    (model: SheetT.Model)
    : (SheetT.Model * string)
    =
    let portIdStr = getPortIdStr portId
    let portPos = SheetBeautifyHelpers.getPortPos portIdStr model.Wire.Symbol

    let PortIndex =
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

    let wireLabelName =
        if isSourceWL then
            wlName + "/" + string PortIndex
        else
            wlName

    let calcWLPos (portPos: XYPos) =
        let portEdge = getPortOrientation model.Wire.Symbol portId

        match portEdge with
        | Right -> { X = portPos.X + 40.0; Y = portPos.Y }
        | Left -> { X = portPos.X - 40.0; Y = portPos.Y }
        | Bottom -> { X = portPos.X + 15.0; Y = portPos.Y + 40.0 }
        | Top -> { X = portPos.X + 15.0; Y = portPos.Y - 40.0 }

    let placeWlAtPort (model: SheetT.Model) (portPos: XYPos) =

        portPos
        |> calcWLPos
        |> placeWireLabelSymbol wireLabelName model

    match placeWlAtPort model portPos with
    | Ok(updatedModel, wlSymId) ->
        let wlRec = { SymbolId = wlSymId; PortNumber = 0 }
        let symRec = { SymbolId = symId; PortNumber = PortIndex }

        let modelWithAdjustedWL = autoAdjustWLRotation wlSymId symId portId updatedModel
        let wireLabelOutput =
            if not (isSourceWL) then
                wireLabelName
            else
                wlName

        let wireResult =
            if isSourceWL then
                placeWireX symRec wlRec modelWithAdjustedWL
            else
                placeWireX wlRec symRec modelWithAdjustedWL

        match wireResult with
        | Ok(finalModel, _) ->
            printfn "Wire label name %A" wireLabelName
            (finalModel, wireLabelName)
        | Error wireError ->
            printfn "Error placing wire: %s" wireError
            (updatedModel, wireLabelName)

    | Error errMsg ->
        printfn "Error placing wire label: %s" errMsg
        (model, wireLabelName)

let replaceLongWiresWithLabels (model: SheetT.Model) (lengthThreshold: float) : SheetT.Model =
    let findLongWires =
        model.Wire.Wires
        |> Map.toList
        |> List.filter (fun (_, wire) ->
            let wireLength = getWireLength wire
            wireLength > lengthThreshold)
        |> List.map fst

    let replaceEachWire (model: SheetT.Model) (wireId: ConnectionId) =
        let (sourceSymbol, sourcePortId), (targetSymbol, targetPortId) =
            getWireSymbolsAndPort wireId model
        printfn "SourceSymbol: %A" sourceSymbol.Component.Label
        printfn "TargetSymbol: %A" targetSymbol.Component.Label
        let (modelWithSourceLabels, newWLName) =
            addWireLabel sourceSymbol.Component.Label sourcePortId sourceSymbol.Id true model
        let (modelWithTargetLabels, _) =
            addWireLabel newWLName targetPortId targetSymbol.Id false modelWithSourceLabels

        deleteWire wireId modelWithTargetLabels

    findLongWires |> List.fold replaceEachWire model

/// Wire Label Reset
let findWiresConnectedToLabels (model: SheetT.Model) =
    let wires = model.Wire.Wires |> Map.toList

    // Check if a symbol is a label
    let isLabel (symbolId: ComponentId) =
        match model.Wire.Symbol.Symbols |> Map.tryFind symbolId with
        | Some symbol -> symbol.Component.Type = IOLabel
        | None -> false

    // Check if a wire is connected to any label
    let wireConnectedToLabel (wireId: ConnectionId, wire: BusWireT.Wire) =
        let (sourceSymbol, _), (targetSymbol, _) = getWireSymbolsAndPort wireId model
        isLabel sourceSymbol.Id || isLabel targetSymbol.Id

    wires
    |> List.choose (fun (wireId, wire) ->
        if wireConnectedToLabel (wireId, wire) then
            Some wireId
        else
            None)

let getNonLabelEndDetails (wireId: ConnectionId) (model: SheetT.Model) =
    // Assuming getWireSymbolsAndPort returns ((sourceSymbol, sourcePortId), (targetSymbol, targetPortId))
    let ((sourceSymbol, sourcePortId), (targetSymbol, targetPortId)) =
        getWireSymbolsAndPort wireId model
    let isSourceLabel = sourceSymbol.Component.Type = IOLabel
    let isTargetLabel = targetSymbol.Component.Type = IOLabel
    printf "%b,%b" isSourceLabel isTargetLabel

    let getPortIndex (portId: PortId) (symId: ComponentId) =
        let portIdStr: string = getPortIdStr portId

        let findPortIndexInList (portsList: Port list) =
            portsList
            |> List.mapi (fun idx port -> (idx, port.Id)) // Create a list of (index, portId) pairs
            |> List.tryFind (fun (_, pid) -> pid = portIdStr) // Try to find the port id
            |> Option.map fst // Map the found tuple to just the index

        match model.Wire.Symbol.Symbols |> Map.tryFind symId with
        | Some symbol ->
            let outputPortIndex = findPortIndexInList symbol.Component.OutputPorts
            match outputPortIndex with
            | Some idx -> (idx, false)
            | None ->
                match findPortIndexInList symbol.Component.InputPorts with
                | Some idx -> (idx, true)
                | None -> failwith "Port ID not found in model"
        | None -> failwith "Symbol ID not found in model"

    match isSourceLabel, isTargetLabel with
    | true, false ->
        printf "type: %A" targetSymbol.Component.Type
        let number, isInputPort = getPortIndex targetPortId targetSymbol.Id
        Some({ SymbolId = targetSymbol.Id; PortNumber = number }, sourceSymbol.Component.Label, isInputPort)
    | false, true ->
        printf "type: %A" sourceSymbol.Component.Type
        let number, isInputPort = getPortIndex sourcePortId sourceSymbol.Id
        Some({ SymbolId = sourceSymbol.Id; PortNumber = number }, targetSymbol.Component.Label, isInputPort)
    | _ -> None

let replaceLabelsWithWires (model: SheetT.Model) (lengthThreshold: float) : SheetT.Model =
    let wireEndsAndLabels =
        model.Wire.Wires
        |> Map.toList
        |> List.choose (fun (wireId, _) ->
            match getNonLabelEndDetails wireId model with
            | Some detailsAndLabel -> Some detailsAndLabel
            | None -> None)

    let rec allPairs lst =
        match lst with
        | [] -> []
        | hd :: tl -> List.map (fun x -> (hd, x)) tl @ allPairs tl

    let processPairs pairs =
        pairs
        |> List.choose (fun ((end1, label1, flag1), (end2, label2, flag2)) ->
            match flag1, flag2 with
            | false, true when label1 = label2 -> Some((end1, end2), label1)
            | true, false when label1 = label2 -> Some((end2, end1), label1)
            | _ -> None)

    let groupedByLabelAndCombinations =
        wireEndsAndLabels
        |> List.groupBy (fun (_, label, _) -> label)
        |> List.collect (fun (_, group) -> group |> allPairs |> processPairs)

    let componentsWithLabel (label: string) (model: SheetT.Model) =
        model.Wire.Symbol.Symbols
        |> Map.toList
        |> List.choose (fun (id, sym) ->
            if caseInvariantEqual sym.Component.Label label then
                Some id
            else
                None)

    // let deleteSymbols (model: SheetT.Model) compIds =
    //     let newSymbols =
    //         (model.Wire.Symbol.Symbols, compIds)
    //         ||> List.fold (fun prevModel sId -> Map.remove sId prevModel)
    //     { model with Wire.Symbol.Symbols = newSymbols }

    // Process each matched pair.
    let processPair (model: SheetT.Model) (((end1, end2), label): (SymbolPortRec * SymbolPortRec) * string) =
        printf "%A,%A" end1 end2
        match placeWireX end1 end2 model with
        | Ok(updatedModel, wireId) ->
            let wire =
                if Map.containsKey wireId updatedModel.Wire.Wires then
                    Map.find wireId updatedModel.Wire.Wires
                else
                    failwithf "Wire with ID %A not found" wireId
            let wireLength = getWireLength wire
            if wireLength <= lengthThreshold then
                // If within threshold, keep the wire and consider removing labels.
                printf "Label: %A" label
                ///deleteSymbols updatedModel (componentsWithLabel label updatedModel)
                updatedModel
            else
                // If not, delete the newly placed wire.
                let modelWithWireRemoved = deleteWire wireId updatedModel
                modelWithWireRemoved
        | Error errMsg ->
            printfn "Error placing wire: %s" errMsg
            model
    printf "IMPORTANT: %A" groupedByLabelAndCombinations
    // Fold over matched pairs to process them.
    List.fold processPair model groupedByLabelAndCombinations

let d3Function (model: SheetT.Model) (lengthThreshold: float) : SheetT.Model =
    let removedWiresModel = replaceLongWiresWithLabels model lengthThreshold
    replaceLabelsWithWires removedWiresModel lengthThreshold

// findLongWires
// |> List.fold (fun model (wireId, wire) ->
//     let wireLabel = placeWireLabel wire model

//     let modelWithoutWire = DeleteWiresWithPort [Some wire.InputPort; Some wire.OutputPort] model

//     let addLabel = placeWireLabel label modelWithoutWire
//     )

// findLongWires
// |> List.fold (fun model (wireId, wire) ->
//     let wireLabel = placeWireLabel wire model

//     let modelWithoutWire = DeleteWiresWithPort [Some wire.InputPort; Some wire.OutputPort] model

//     let addLabel = placeWireLabel label modelWithoutWire
//     )

// Section C ->> Bit legends /  Symbol rendering / Adjustments / repositioning
// To be implemented when I have a better idea of its current testing performance
