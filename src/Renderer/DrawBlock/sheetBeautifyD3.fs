module SheetBeautifyD3

open DrawModelType
open CommonTypes

let sheetWireLabelSymbol (sheet: SheetT.Model) = 
    // Note for confused readers: in Issie, a 'wire label' is a flying wire 'virtually' connecting two points on the sheet

    let getAllConnectedPorts (pId:string) =
        sheet.Wire.Wires
        |> Map.toArray 
        |> Array.collect (fun (_,wire) -> 
            if wire.OutputPort = OutputPortId pId then 
                match wire.InputPort with | InputPortId pId' -> [|sheet.Wire.Symbol.Ports[pId']|]
            elif wire.InputPort = InputPortId pId then
                match wire.OutputPort with | OutputPortId pId' -> [|sheet.Wire.Symbol.Ports[pId']|]
            else
                [||])

    /// <summary>Merge two maps, prioritising the second map's entries</summary>
    let mergeMaps map1 map2 =
        Map.fold (fun acc k v -> Map.add k v acc) map1 map2

    let shouldWireBeLabel (wire:BusWireT.Wire) =
        (wire.Segments
        |> List.map (fun s -> s.Length)
        |> List.sum) > 150 // Only if total wire length > 10 should it become label-ified

    let symbolGetInputConnected (symbol: SymbolT.Symbol) =
        Array.tryFind (fun (wire: BusWireT.Wire) -> wire.InputPort = (InputPortId symbol.Component.InputPorts.Head.Id)) (Helpers.mapValues sheet.Wire.Wires)

    let symbolGetOutputConnected (symbol: SymbolT.Symbol) =
        Array.tryFind (fun (wire: BusWireT.Wire) -> wire.OutputPort = (OutputPortId symbol.Component.OutputPorts.Head.Id)) (Helpers.mapValues sheet.Wire.Wires)

    // todo: this properly
    let greatestDistanceBetweenLabelInOut (symbol_array:SymbolT.Symbol array) =
        let inLabels, outLabels =
            symbol_array
            |> Array.partition  (fun sym -> Option.isSome(symbolGetInputConnected sym))

        let inPort = Array.exactlyOne inLabels

        let inPortPosition = inPort.Pos

        outLabels
            |> Array.map (fun p -> p.Pos)
            |> Array.map (fun thisOutPos -> euclideanDistance thisOutPos inPortPosition)
            |> Array.max
        

    let wiresToLabelify =
        sheet.Wire.Wires
        |> Map.filter (fun _ w -> shouldWireBeLabel w)
        |> Helpers.mapKeys


    let labelSymbolsToWireify =
        sheet.Wire.Symbol.Symbols
        |> Helpers.mapValues
        |> Array.filter (fun s -> s.Component.Type = ComponentType.IOLabel)
        |> Array.groupBy (fun s -> s.Component.Label)
        |> Array.filter (
            // Turn to long wire if furthest in and out pair are less than 5 units apart
            fun (label, symbols) -> (greatestDistanceBetweenLabelInOut symbols < 100)
        )

    let labelsToWireify =
        labelSymbolsToWireify
        |> Array.map fst


    let getAllSymbolDrivenPortIds (symbols: SymbolT.Symbol array) =
        symbols
        |> Array.choose (fun s ->
            symbolGetOutputConnected s)
        |> Array.map (fun w -> (string w.OutputPort))
        |> Array.collect (getAllConnectedPorts)
        |> Array.map (fun (p:Port) -> (InputPortId p.Id))

    // There should (in theory!) be exactly one 'driving' out-port
    // for each label, but this constraint may only exist at run time
    // so todo: see if this causes errors
    let getDrivingPortIdOfLabelSymbols (symbols: SymbolT.Symbol array) =
        let inputPort = string (symbols
            |> Array.choose (fun symbol -> symbolGetInputConnected symbol)
            |> Array.exactlyOne).InputPort
        
        inputPort
            |> getAllConnectedPorts
            |> Array.map (fun (p:Port) -> (OutputPortId p.Id))
            |> Array.exactlyOne



    // Create wires representing the connections which were previously made by labels
    let newWiresReplacingLabels =
        labelSymbolsToWireify
        |> Array.map (
            fun (label, symbols) -> 
            (symbols |> getAllSymbolDrivenPortIds, symbols)
        ) |> Array.collect (fun (portIds, symbols) ->
            portIds
            |> Array.map (fun portId -> (getDrivingPortIdOfLabelSymbols symbols, portId))
        )
        |> Array.map (fun (drivingPort,receivingPort) -> BusWireUpdate.makeNewWire receivingPort drivingPort sheet.Wire)
        |> Array.map (fun wire -> (wire.WId, wire))
        |> Map.ofArray

        

    // Create symbols + wires for new labels
    let makeInOutSymbolsOfConnection (conn: ConnectionId) (sheetModel: SheetT.Model) =
        
        let model = sheetModel.Wire.Symbol

        // We can use the 'old' sheet parameter here because we're dealing with already-existent connections
        let oldConnectionEndingPortId = sheet.Wire.Wires[conn].OutputPort
        let oldConnectionStartingPortId = sheet.Wire.Wires[conn].InputPort

        let oldConnectionDrivingPort = sheet.Wire.Symbol.Ports[string sheet.Wire.Wires[conn].OutputPort]
        let oldConnectionDrivingComponent = sheet.Wire.Symbol.Symbols[ComponentId oldConnectionDrivingPort.HostId]

        // todo: Think through how you get port labels?? assuming that components can have multiple output ports which I think they can

       

        // Label should come from the 'driving' port
        let label = oldConnectionDrivingComponent.Component.Label

        let startPos = Symbol.getPortLocation None sheetModel.Wire.Symbol (string oldConnectionStartingPortId)
        let startPosOffset = {startPos with X = startPos.X - 40.0}


        let endPos = Symbol.getPortLocation None sheetModel.Wire.Symbol (string oldConnectionEndingPortId)
        let endPosOffset = {endPos with X = endPos.X + 40.0}

        // Make the label for the start and end
        let (postStartSymbolModel, startLabelCId) = SymbolUpdate.addSymbol [] model endPosOffset ComponentType.IOLabel label
        let startLabelInputPortId = postStartSymbolModel.Symbols[startLabelCId].Component.InputPorts.Head.Id

        let (postLabelSymbolModel, endLabelCId) = SymbolUpdate.addSymbol [] postStartSymbolModel startPosOffset ComponentType.IOLabel label
        let endLabelOutputPortId = postLabelSymbolModel.Symbols[endLabelCId].Component.OutputPorts.Head.Id

        
        
        let destPos = Symbol.getPortLocation None postStartSymbolModel (string startLabelInputPortId)

        let newSheetWire = {sheet.Wire with Symbol = postLabelSymbolModel}

        // Make wire going into label
        let firstWire = BusWireUpdate.makeNewWire (InputPortId startLabelInputPortId) oldConnectionEndingPortId newSheetWire

        // Make wire out of label
        let secondWire = BusWireUpdate.makeNewWire oldConnectionStartingPortId (OutputPortId endLabelOutputPortId) newSheetWire



        let newWires =
            sheetModel.Wire.Wires
            |> Map.add firstWire.WId firstWire
            |> Map.add secondWire.WId secondWire


        {sheetModel with Wire = {sheetModel.Wire with Symbol = postLabelSymbolModel; Wires = newWires }}

    let newSymbolsModel =
        wiresToLabelify
        |> Array.fold (fun sheetState cid ->
            makeInOutSymbolsOfConnection cid sheetState) sheet


    // Keep only symbols which were not wire labels being removed
    let remainingSymbols =
        newSymbolsModel.Wire.Symbol.Symbols
        |> Map.filter (
           fun cid s -> s.Component.Type <> CommonTypes.ComponentType.IOLabel || not (Array.contains s.Component.Label labelsToWireify)
        )

    let isToRemovedLabel (portId: string) =
        let connectionComponent = newSymbolsModel.Wire.Symbol.Symbols[ComponentId newSymbolsModel.Wire.Symbol.Ports[portId].HostId].Component

        (connectionComponent.Type = CommonTypes.ComponentType.IOLabel) && (Array.contains connectionComponent.Label labelsToWireify)

    
    
    // Keep only wires which were not turned into labels OR were wires to/from labels
    let updatedWires =
        sheet.Wire.Wires
        |> mergeMaps newWiresReplacingLabels
        |> mergeMaps newSymbolsModel.Wire.Wires
        |> Map.filter (fun cid wire -> not (Array.contains cid wiresToLabelify))
        |> Map.filter (fun cid wire -> (not (isToRemovedLabel (string wire.InputPort)) && not (isToRemovedLabel (string wire.OutputPort))))

        //|> mergeMaps (newSymbolsModel.Wire.Wires )))


    // todo use lenses
    {newSymbolsModel with Wire = {sheet.Wire with Wires = updatedWires; Symbol = {newSymbolsModel.Wire.Symbol with Symbols = remainingSymbols}}}
