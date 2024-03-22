module SheetBeautifyD3

()

// open CommonTypes
// open DrawHelpers
// open DrawModelType
// open DrawModelType.SymbolT
// open DrawModelType.BusWireT
// open DrawModelType.SheetT
// open SheetUpdateHelpers
// open SheetUpdate
// open SheetBeautifyHelpers
// open Optics
// open Optics.Operators
// open BlockHelpers
// open Sheet
// open EEExtensions
// open Symbol
// open Helpers

// /// Optic to access SymbolT.Model from SheetT.Model
// let symbolModel_ = symbol_

// /// Optic to access BusWireT.Model from SheetT.Model
// let busWireModel_ = wire_

// /// allowed max X or y coord of svg canvas
// let maxSheetCoord = Constants.defaultCanvasSize

// /// Find the middle of the sheet
// let middleOfSheet = { X = maxSheetCoord / 2.; Y = maxSheetCoord / 2. }

// //type SymbolPortRec = { Label: string; PortNumber: int }
// type SymbolPortRec = { SymbolId: ComponentId; PortNumber: int }

// /// Used throughout to compare labels since these are case invariant "g1" = "G1"
// let caseInvariantEqual str1 str2 =
//     String.toUpper str1 = String.toUpper str2

// /// Find the symbols connected to each end of a wire and the respective portIds that the wire is connected to
// let getWireSymbolsAndPort (wireId: ConnectionId) (model: SheetT.Model) =
//     let wire = model.Wire.Wires.[wireId]
//     let sourceSymbol = getSourceSymbol model.Wire wire
//     let targetSymbol = getTargetSymbol model.Wire wire

//     let targetPortId = InputId wire.InputPort
//     let sourcePortId = OutputId wire.OutputPort

//     ((sourceSymbol, sourcePortId), (targetSymbol, targetPortId))

// /// Places a wire layer symbol given the desired name of that label and position
// let placeWireLabelSymbol
//     (symLabel: string)
//     (model: SheetT.Model)
//     (position: XYPos)
//     : Result<(Model * ComponentId), string>
//     =
//     let symLabel = String.toUpper symLabel
//     let wlSymModel, wlSymId =
//         SymbolUpdate.addSymbol [] (model.Wire.Symbol) position IOLabel symLabel
//     let wlSym = wlSymModel.Symbols[wlSymId]
//     match position + wlSym.getScaledDiagonal with
//     | { X = x; Y = y } when x > maxSheetCoord || y > maxSheetCoord ->
//         Error $"symbol '{symLabel}' position {position + wlSym.getScaledDiagonal} lies outside allowed coordinates"
//     | _ ->
//         let updatedModel =
//             model
//             |> Optic.set symbolModel_ wlSymModel
//             |> updateBoundingBoxes
//         Ok(updatedModel, wlSymId)

// /// Places a wire given source and target symbol SymbolPortRecs.
// /// SymbolPortRecs is a record containing the symbol id and port number of each symbol.
// let placeWireX
//     (source: SymbolPortRec)
//     (target: SymbolPortRec)
//     (model: SheetT.Model)
//     : Result<SheetT.Model * ConnectionId, string>
//     =
//     let getPort (symPortRec: SymbolPortRec) (portType: PortType) =
//         match
//             model.Wire.Symbol.Symbols
//             |> Map.tryFind symPortRec.SymbolId
//         with
//         | Some symbol ->
//             let ports =
//                 match portType with
//                 | PortType.Input -> symbol.Component.InputPorts
//                 | PortType.Output -> symbol.Component.OutputPorts
//             match ports |> List.tryItem symPortRec.PortNumber with
//             | Some port -> Ok port.Id
//             | None ->
//                 Error $"Can't find {portType} port {symPortRec.PortNumber} on symbol with ID {symPortRec.SymbolId}"
//         | None -> Error $"Can't find symbol with ID {symPortRec.SymbolId}"

//     match getPort source PortType.Output, getPort target PortType.Input with
//     | Error e, _
//     | _, Error e -> Error e
//     | Ok outPort, Ok inPort ->
//         let newWire =
//             BusWireUpdate.makeNewWire (InputPortId inPort) (OutputPortId outPort) model.Wire
//         if
//             model.Wire.Wires
//             |> Map.exists (fun wid wire ->
//                 wire.InputPort = newWire.InputPort
//                 && wire.OutputPort = newWire.OutputPort)
//         then
//             Error "Can't create wire because a wire already exists between those ports"
//         else
//             let updatedModel =
//                 model
//                 |> Optic.set (busWireModel_ >-> BusWireT.wireOf_ newWire.WId) newWire
//             Ok(updatedModel, newWire.WId)

// // Delete the wire with the corresponding wireId.
// let deleteWire (wireId: ConnectionId) (model: SheetT.Model) =
//     let newWires =
//         model.Wire.Wires
//         |> Map.filter (fun id _ -> id <> wireId)

//     { model with Wire = { model.Wire with Wires = newWires } }

// /// Ensures the wire label rotation is correct for all sides, orientations and flips of the connected symbol.
// /// Takes in the id of both the wire label, connected source/target symbol and the portId on the source/target symbol that the wire label is connected too.
// let autoAdjustWLRotation
//     (wlSymId: ComponentId)
//     (symId: ComponentId)
//     (portId: PortId)
//     (model: SheetT.Model)
//     : SheetT.Model
//     =
//     let symbolsMap = model.Wire.Symbol.Symbols
//     let symLens = symbolOf_ symId
//     let sym = Optic.get symLens model
//     let symRotation = Optic.get symbol_rotation_ sym
//     let symFlip = Optic.get symbol_flipped_ sym

//     match symbolsMap |> Map.tryFind wlSymId with
//     | Some wireLabel ->
//         let portEdge = getPortOrientation model.Wire.Symbol portId
//         let adjustedSymbol =
//             match portEdge with
//             | Top
//             | Bottom ->
//                 let rotationCheck wireLabel =
//                     match symRotation with
//                     | Degree180
//                     | Degree270 -> SymbolResizeHelpers.rotateSymbol Degree270 wireLabel
//                     | _ -> SymbolResizeHelpers.rotateSymbol Degree90 wireLabel

//                 let flipCheck wireLabel =
//                     match symFlip with
//                     | true -> wireLabel
//                     | false -> wireLabel

//                 wireLabel |> rotationCheck |> flipCheck
//             | Left
//             | Right ->
//                 let rotationCheck wireLabel =
//                     match symRotation with
//                     | Degree180
//                     | Degree90 -> SymbolResizeHelpers.rotateSymbol Degree180 wireLabel
//                     | _ -> wireLabel

//                 let flipCheck wireLabel =
//                     match symFlip with
//                     | true -> SymbolResizeHelpers.rotateSymbol Degree180 wireLabel
//                     | false -> wireLabel

//                 wireLabel |> rotationCheck |> flipCheck
//         let updatedSymbols = Map.add wlSymId adjustedSymbol symbolsMap
//         { model with
//             Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbols } } }
//     | None -> model

// /// Places and connects a wire label to a desired symbol.
// /// <param name="wlName">The name of the wire label being placed.</param>
// /// <param name="portId">The id of the port to connect the wire label to.</param>
// /// <param name="symId">The id of the symbol the wire label is being connected to.</param>
// /// <param name="isSourceWL">Boolean parameter to indicate whether the symbol is a source or a target symbol.</param>
// let addWireLabel
//     (wlName: string)
//     (portId: PortId)
//     (symId: ComponentId)
//     (isSourceWL: bool)
//     (model: SheetT.Model)
//     : (SheetT.Model * string)
//     =
//     let portIdStr = getPortIdStr portId
//     let portPos = SheetBeautifyHelpers.getPortPos portIdStr model.Wire.Symbol

//     let PortIndex =
//         match model.Wire.Symbol.Symbols |> Map.tryFind symId with
//         | Some symbol ->
//             let allPorts =
//                 if isSourceWL then
//                     symbol.Component.OutputPorts
//                 else
//                     symbol.Component.InputPorts

//             allPorts
//             |> List.mapi (fun idx port -> (idx, port.Id))
//             |> List.tryFind (fun (_, pid) -> pid = portIdStr)
//             |> Option.map fst
//             |> Option.get
//         | None -> failwith "Symbol ID not found in model"

//     let wireLabelName =
//         if isSourceWL then
//             wlName + "/" + string PortIndex
//         else
//             wlName

//     let calcWLPos (portPos: XYPos) =
//         let portEdge = getPortOrientation model.Wire.Symbol portId

//         match portEdge with
//         | Right -> { X = portPos.X + 40.0; Y = portPos.Y }
//         | Left -> { X = portPos.X - 40.0; Y = portPos.Y }
//         | Bottom -> { X = portPos.X + 15.0; Y = portPos.Y + 40.0 }
//         | Top -> { X = portPos.X + 15.0; Y = portPos.Y - 40.0 }

//     let placeWlAtPort (model: SheetT.Model) (portPos: XYPos) =
//         portPos
//         |> calcWLPos
//         |> placeWireLabelSymbol wireLabelName model

//     match placeWlAtPort model portPos with
//     | Ok(updatedModel, wlSymId) ->
//         let wlRec = { SymbolId = wlSymId; PortNumber = 0 }
//         let symRec = { SymbolId = symId; PortNumber = PortIndex }

//         let modelWithAdjustedWL = autoAdjustWLRotation wlSymId symId portId updatedModel

//         let wireResult =
//             if isSourceWL then
//                 placeWireX symRec wlRec modelWithAdjustedWL
//             else
//                 placeWireX wlRec symRec modelWithAdjustedWL

//         match wireResult with
//         | Ok(finalModel, _) ->
//             printfn "Wire label name %A" wireLabelName
//             (finalModel, wireLabelName)
//         | Error wireError ->
//             printfn "Error placing wire: %s" wireError
//             (updatedModel, wireLabelName)

//     | Error errMsg ->
//         printfn "Error placing wire label: %s" errMsg
//         (model, wireLabelName)

// /// High level D3 function that replaces wires that exceed the length threshold, with wire labels.
// let replaceLongWiresWithLabels (model: SheetT.Model) (lengthThreshold: float) : SheetT.Model =
//     let findLongWires =
//         model.Wire.Wires
//         |> Map.toList
//         |> List.filter (fun (_, wire) ->
//             let wireLength = getWireLength wire
//             wireLength > lengthThreshold)
//         |> List.map fst

//     let replaceEachWire (model: SheetT.Model) (wireId: ConnectionId) =
//         let (sourceSymbol, sourcePortId), (targetSymbol, targetPortId) =
//             getWireSymbolsAndPort wireId model
//         printfn "SourceSymbol: %A" sourceSymbol.Component.Label
//         printfn "TargetSymbol: %A" targetSymbol.Component.Label
//         let (modelWithSourceLabels, newWLName) =
//             addWireLabel sourceSymbol.Component.Label sourcePortId sourceSymbol.Id true model
//         let (modelWithTargetLabels, _) =
//             addWireLabel newWLName targetPortId targetSymbol.Id false modelWithSourceLabels

//         deleteWire wireId modelWithTargetLabels

//     findLongWires |> List.fold replaceEachWire model

// // Checks if a symbol is a label
// let isLabel (symbolId: ComponentId) (model: SheetT.Model) =
//     match model.Wire.Symbol.Symbols |> Map.tryFind symbolId with
//     | Some symbol -> symbol.Component.Type = IOLabel
//     | None -> false

// // Checks if a wire is connected to any label
// let wireConnectedToLabel (wireId: ConnectionId, wire: BusWireT.Wire) (model: SheetT.Model) =
//     let (sourceSymbol, _), (targetSymbol, _) = getWireSymbolsAndPort wireId model
//     isLabel sourceSymbol.Id model
//     || isLabel targetSymbol.Id model

// /// Finds wires that are connected at one end to a wire label component
// let findWiresConnectedToLabels (model: SheetT.Model) =
//     let wires = model.Wire.Wires |> Map.toList

//     wires
//     |> List.choose (fun (wireId, wire) ->
//         if wireConnectedToLabel (wireId, wire) model then
//             Some wireId
//         else
//             None)

// /// Finds port index of a specific component. Used for port record formation.
// let getPortIndex (portId: PortId) (symId: ComponentId) (model: SheetT.Model) =
//     let portIdStr: string = getPortIdStr portId

//     let findPortIndexInList (portsList: Port list) =
//         portsList
//         |> List.mapi (fun idx port -> (idx, port.Id))
//         |> List.tryFind (fun (_, pid) -> pid = portIdStr)
//         |> Option.map fst

//     match model.Wire.Symbol.Symbols |> Map.tryFind symId with
//     | Some symbol ->
//         let outputPortIndex = findPortIndexInList symbol.Component.OutputPorts
//         match outputPortIndex with
//         | Some idx -> (idx, false)
//         | None ->
//             match findPortIndexInList symbol.Component.InputPorts with
//             | Some idx -> (idx, true)
//             | None -> failwith "Port ID not found in model"
//     | None -> failwith "Symbol ID not found in model"

// /// Used to get port record for the non wire label component, wire label name and port type (input or output port)
// let getNonLabelEndDetails (wireId: ConnectionId) (model: SheetT.Model) =
//     let ((sourceSymbol, sourcePortId), (targetSymbol, targetPortId)) =
//         getWireSymbolsAndPort wireId model
//     let isSourceLabel = sourceSymbol.Component.Type = IOLabel
//     let isTargetLabel = targetSymbol.Component.Type = IOLabel
//     printf "%b,%b" isSourceLabel isTargetLabel

//     match isSourceLabel, isTargetLabel with
//     | true, false ->
//         printf "type: %A" targetSymbol.Component.Type
//         let number, isInputPort = getPortIndex targetPortId targetSymbol.Id model
//         Some({ SymbolId = targetSymbol.Id; PortNumber = number }, sourceSymbol.Component.Label, isInputPort)
//     | false, true ->
//         printf "type: %A" sourceSymbol.Component.Type
//         let number, isInputPort = getPortIndex sourcePortId sourceSymbol.Id model
//         Some({ SymbolId = sourceSymbol.Id; PortNumber = number }, targetSymbol.Component.Label, isInputPort)
//     | _ -> None

// /// Helper function to find all possible pairs in a list
// let rec allPairs lst =
//     match lst with
//     | [] -> []
//     | hd :: tl -> List.map (fun x -> (hd, x)) tl @ allPairs tl

// /// Helper function for pair processing that checks port type label flags and ensures wire labels are the same
// let processPairs pairs =
//     pairs
//     |> List.choose (fun ((end1, label1, flag1), (end2, label2, flag2)) ->
//         match flag1, flag2 with
//         | false, true when label1 = label2 -> Some((end1, end2), label1)
//         | true, false when label1 = label2 -> Some((end2, end1), label1)
//         | _ -> None)

// /// Replaces Wire labels with wires if potential wire between wire labels is below threshold
// let replaceLabelsWithWires (model: SheetT.Model) (lengthThreshold: float) : SheetT.Model =
//     let wireEndsAndLabels =
//         model.Wire.Wires
//         |> Map.toList
//         |> List.choose (fun (wireId, _) ->
//             match getNonLabelEndDetails wireId model with
//             | Some detailsAndLabel -> Some detailsAndLabel
//             | None -> None)

//     let groupedByLabelAndCombinations =
//         wireEndsAndLabels
//         |> List.groupBy (fun (_, label, _) -> label)
//         |> List.collect (fun (_, group) -> group |> allPairs |> processPairs)

//     // let componentsWithLabel (label: string) (model: SheetT.Model) =
//     //     model.Wire.Symbol.Symbols
//     //     |> Map.toList
//     //     |> List.choose (fun (id, sym) ->
//     //         if caseInvariantEqual sym.Component.Label label then
//     //             Some id
//     //         else
//     //             None)

//     // let deleteSymbols (model: SheetT.Model) compIds =
//     //     let newSymbols =
//     //         (model.Wire.Symbol.Symbols, compIds)
//     //         ||> List.fold (fun prevModel sId -> Map.remove sId prevModel)
//     //     { model with Wire.Symbol.Symbols = newSymbols }

//     /// Checks each matched pair of ports that are connected by wire label and replaces if wire below threshold.
//     let processPair (model: SheetT.Model) (((end1, end2), label): (SymbolPortRec * SymbolPortRec) * string) =
//         printf "%A,%A" end1 end2
//         match placeWireX end1 end2 model with
//         | Ok(updatedModel, wireId) ->
//             let wire =
//                 if Map.containsKey wireId updatedModel.Wire.Wires then
//                     Map.find wireId updatedModel.Wire.Wires
//                 else
//                     failwithf "Wire with ID %A not found" wireId
//             let wireLength = getWireLength wire
//             if wireLength <= lengthThreshold then
//                 printf "Label: %A" label
//                 //deleteSymbols updatedModel (componentsWithLabel label updatedModel)
//                 updatedModel
//             else
//                 let modelWithWireRemoved = deleteWire wireId updatedModel
//                 modelWithWireRemoved
//         | Error errMsg ->
//             printfn "Error placing wire: %s" errMsg
//             model

//     List.fold processPair model groupedByLabelAndCombinations

// /// Overall function that implements both replacing long wires with labels and replacing wire labels if a wire between them is below threshould
// let d3Function (model: SheetT.Model) (lengthThreshold: float) : SheetT.Model =
//     let removedWiresModel = replaceLongWiresWithLabels model lengthThreshold
//     replaceLabelsWithWires removedWiresModel lengthThreshold
