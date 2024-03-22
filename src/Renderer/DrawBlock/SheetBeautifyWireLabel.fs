module SheetBeautifyWireLabel

open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open SheetBeautifyHelpers
open Optics
open Optics
open BlockHelpers
open System
open EEExtensions
open Symbol
open SymbolUpdate
open DrawModelType
open Helpers
open SheetT
open RotateScale
open DrawHelpers

///constants used in DEBUG mode
module DEBUG_CONSTANTS =
    // set this to true to only select wires that are greater than DEBUG_MAX_LENGTH
    let DEBUG:bool = false
    let DEBUG_MAX_LENGTH:float = 1.

/// constants used by SheetBeautifyWireLabel
module Constants =

    (*
    A few metrics are designed to define possible characteristics of wires that should be replaced by 
    wire labels, and characteristic of wire Labels that should be changed back to wires. The specific 
    values are temporary and should be determined later. 

    Median is used to avoid the influence of outliers. 
    
    TODO:identify suitable values through tests or create logic for the values to be user assigned during runtime
    TODO: confirm which order the metrics should be satified if they conflict, currently designed as the greater 
        the line number, the more important the metric is.
    *)
    /// The maximum length of a wire, TODO: find a suitable value
    let maxWireLength = 750.

    /// If the length of a wire is greater than this ratio of the median length of all wires,
    /// then the wire is considered too long.
    let maxLengthRatio = 2.

    /// If the intersection of a wire with all other wires is greater than this ratio of the median
    /// intersection of all wires, then the wire is considered too intersecting.
    let maxIntersectionRatio = 4.0

    ///If the distance between two wire label of the same wire connection is less than this ratio of the median
    /// length of all wires, then the wire labels are considered too close and unecesarry.
    let minLabelDistanceRatio = 1

    /// If the number of wire bends is greater than this ratio of the median number of bends of all wires,
    /// then the wire is considered too bendy.
    let maxBendRatio = 4.0


/// Some Helper functions copied from Tick3 testdrawblock and edited
module sheetBeutifyWireLabelHelpers =
    let busWireModel_:Lens<SheetT.Model,BusWireT.Model> = wire_
    let maxSheetCoord = Sheet.Constants.defaultCanvasSize
    let symbolModel_ = SheetT.symbol_
    type SymbolPort = { Label: string; PortNumber: int }
    //some helper functions, also defined in tick3 testdrawblock
    let getOkOrFail (res: Result<'a,string>) =
        match res with
        | Ok x -> x
        | Error mess ->
            failwithf "%s" mess
    let caseInvariantEqual str1 str2 =
        String.toUpper str1 = String.toUpper str2

    let extractIntOption (data: int option) =
        match data with
        | Some x -> 
            x
        | None ->
            0 // return default int value

    let labelHasRoom (sheet: SheetT.Model) labelSym=
        not (noSymbolOverlap (boxesIntersect) sheet.BoundingBoxes labelSym)

    /// Place a new symbol with label symLabel onto the Sheet with given position.
    /// Return error if symLabel is not unique on sheet, or if position is outside allowed sheet coordinates (0 - maxSheetCoord).
    /// To be safe place components close to (maxSheetCoord/2.0, maxSheetCoord/2.0).
    /// symLabel - the component label, will be uppercased to make a standard label name
    /// compType - the type of the component
    /// position - the top-left corner of the symbol outline.
    /// model - the Sheet model into which the new symbol is added.
    /// function from tick3 testdrawblock and modified, added extra symbol output because we need it in our function
    /// added rotation
    /// might need to add position detection and same net detection
    let placeSymbol (symLabel: string) (compType: ComponentType) (position: XYPos) (rotation) (model: SheetT.Model)  =
        let symLabel = String.toUpper symLabel // make label into its standard casing
        let symModel, symId = SymbolUpdate.addSymbol [] (model.Wire.Symbol) position compType symLabel
        let sym:Symbol = symModel.Symbols[symId]
        let rotatedSym = SymbolResizeHelpers.rotateAntiClockByAng rotation sym
        let symModel = replaceSymbol symModel rotatedSym symId
        let doesLabelHaveRoom= labelHasRoom model rotatedSym
        let res=
            match position + rotatedSym.getScaledDiagonal with
            | {X=x;Y=y} when x > maxSheetCoord || y > maxSheetCoord || (doesLabelHaveRoom)->
                Error $"symbol '{symLabel}' position {position + rotatedSym.getScaledDiagonal} lies outside allowed coordinates'{maxSheetCoord}' or there is no room for it"
            | _ ->
                model
                |> Optic.set symbolModel_ symModel
                |> updateBoundingBoxes // could optimise this by only updating symId bounding boxes
                |> Ok

        sym,res

    ///Place Wire function for replacing IO Labels, 
    /// takes in the port of the original wire, the symbol of the ioLabel, 
    /// and a bool to indicate if the original port is input or output
    let placeWire (originalPort:Port)(ioLabelSymbol: Symbol)  (model: SheetT.Model) : SheetT.Model =
        let originalPortId = originalPort.Id
        let ioLabelPortId = 
            match originalPort.PortType with
            | PortType.Input -> ioLabelSymbol.Component.OutputPorts.Head
            | PortType.Output -> ioLabelSymbol.Component.InputPorts.Head
            |> fun port -> port.Id 
        let newWire = 
            match originalPort.PortType with
            | PortType.Input -> BusWireUpdate.makeNewWire (InputPortId originalPortId) (OutputPortId ioLabelPortId) model.Wire
            | PortType.Output -> BusWireUpdate.makeNewWire (InputPortId ioLabelPortId) (OutputPortId originalPortId) model.Wire
        
        {model with Wire={model.Wire with Wires= Map.add newWire.WId newWire model.Wire.Wires}}// optic did not work, so changed to this equivalent
            
    let findIOLabelConnectedToPort (port:Port) (sheet:SheetT.Model )= ()
    let replaceWireLableWithWire porta portb sheeet  = ()


/// Helper functions to find illegal wires
module findIllegalWiresHelpers =

    /// get the length of a wire
    let getWireLength (wire: BusWireT.Wire) =
        wire
        |> getAbsSegments
        |> List.map (fun segment -> segment.Segment.Length)
        |> List.map(fun x -> abs x)
        |> List.sum

    /// get the number of intersections of a wire with all other wires on sheet
    let getWireIntersection (sheetModel: SheetT.Model) (wire: BusWireT.Wire) : int =
        let nets = SegmentHelpers.allWireNets sheetModel
        let distinctSegs =
            nets
            |> List.collect (fun (_, net) -> SegmentHelpers.getVisualSegsFromNetWires true sheetModel net)
        let wireSegs = 
            wire
            |> List.singleton
            |> SegmentHelpers.getVisualSegsFromNetWires true sheetModel
        List.allPairs wireSegs distinctSegs
        |> List.filter (fun (seg1,seg2) -> seg1 > seg2 && SegmentHelpers.isProperCrossing seg1 seg2)
        |> List.length
        |> fun x -> x / 2

    /// get the number of bends of a wire
    let getWireBends (wire: BusWireT.Wire): int =
        wire
        |> getNonZeroAbsSegments
        |> List.length
        |> fun x -> x - 1

    /// Get the number of wire labels on the sheet
    let getNumberWireLabels(sheetModel: SheetT.Model): int =
        sheetModel.Wire.Symbol.Symbols
        |> Map.toList
        |> List.map (fun (Id, symbol) -> symbol)
        |> List.filter (fun symbol -> symbol.Component.Type = ComponentType.IOLabel)
        |> List.length


    /// get median of a List
    let median (list) =
        let sortedList = List.sort list
        let length = List.length sortedList
        if length % 2 = 0 then
            (sortedList.[length / 2 - 1] + sortedList.[length / 2]) / 2.0
        else
            sortedList.[length / 2]

    /// Get the median length of all wires
    let getMedianLength(sheetModel: SheetT.Model): float =
        let IOLabelsPortIDs = 
            sheetModel.Wire.Symbol.Symbols
            |> Map.toList
            |> List.map snd
            |> List.filter (fun symbol -> symbol.Component.Type = ComponentType.IOLabel)
            |> List.map (fun symbol -> symbol.Component.InputPorts.Head.Id, symbol.Component.OutputPorts.Head.Id)
            |> List.collect (fun (inputPortId, outputPortId) -> [inputPortId; outputPortId])
        sheetModel.Wire.Wires
        |> Map.toList
        |> List.map snd
        |> List.filter (fun wire -> not (List.contains (string wire.InputPort) IOLabelsPortIDs) && not (List.contains (string wire.OutputPort) IOLabelsPortIDs))
        |> List.map (fun wire -> getWireLength wire)
        |> List.filter(fun x -> x > 5.) // Artifacts: to not consider wires generated in previous optimation
        |> median
    
    /// Get the median number of intersections of all wires
    let getMedianIntersection(sheetModel: SheetT.Model): float =
        sheetModel.Wire.Wires
        |> Map.toList
        |> List.map (fun (id, wire) -> getWireIntersection sheetModel wire)
        |> List.map(fun intersections -> float intersections)
        |> median

    /// Get the median number of bends across wires
    let getMedianBend(sheetModel: SheetT.Model): float =
        sheetModel.Wire.Wires
        |> Map.toList
        |> List.map (fun (id, wire) -> getWireBends wire)
        |> List.map(fun bends -> float bends)
        |> median

    /// get wires that are too long
    let getWiresTooLong(sheetModel: SheetT.Model): Wire Set =
        
        sheetModel.Wire.Wires
        |> Map.filter (fun id wire -> (getWireLength wire > Constants.maxLengthRatio * getMedianLength sheetModel) || (getWireLength wire > Constants.maxWireLength))
        |> Map.toList
        |> List.map snd
        |> Set.ofList
    
    /// get wires that are too intersecting
    let getWiresTooIntersecting(sheetModel: SheetT.Model) : Wire Set =
        sheetModel.Wire.Wires
        |> Map.filter (fun id wire -> float(getWireIntersection sheetModel wire) > Constants.maxIntersectionRatio * getMedianIntersection sheetModel)
        |> Map.toList
        |> List.map snd
        |> Set.ofList

    /// get pairs of wire labels that are too close
    let getWireLablesTooClose(sheetModel: SheetT.Model): list<Symbol * Symbol>  =
        List.empty //TODO: wire labels are not implemented yet

    /// get wires that are too bendy
    let getWiresTooBendy(sheetModel: SheetT.Model): Wire Set  =
        sheetModel.Wire.Wires
        |> Map.filter (fun id wire -> float(getWireBends wire) > Constants.maxBendRatio * getMedianBend sheetModel)
        |> Map.toList
        |> List.map snd
        |> Set.ofList

    /// get bit legends overlapping symbols or wires
    let getOverlappingBitLegends(sheetModel: SheetT.Model): int =
        //TODO: bit legends are not implemented yet
        0

/// functions to find illegal wires and replace them with wire labels
/// top level function takes in a sheet model and outputs a new sheet model with the illegal wires replaced
module illegalWireToWireLabels =
    open findIllegalWiresHelpers
    open sheetBeutifyWireLabelHelpers

    /// the output type of the test
    type illegalWireMetrics = {
        //basic metrics
        /// the total number of wires on the sheet
        numberWires: int
        /// the total number of wire labels on the sheet
        numberWireLabels: int
        
        //informational metrics that can be useful to know for the result of the implementation
        /// the median length of all wires
        medianLength: float
        /// the median number of intersections of all wires
        medianIntersection: float
        /// the median number of bends of all wires
        medianBend: float
        /// the list of wires that are too long, set is used to make union easier
        wiresTooLong:  Wire Set 
        /// the list of wires that are too intersecting, set is used to make union easier
        wiresTooIntersecting:  Wire Set 
        /// the list of pairs of wire labels that are too close, TODO: wire labels are not implemented yet
        wireLablesTooClose: list<Symbol * Symbol> 
        /// the list of wires that are too bendy, set is used to make union easier
        wiresTooBendy:  Wire Set 
        /// list of bit legends overlapping symbols or wires
        wireOverlappingBitLegends:  int //TODO: bit legends are not implemented yet
    }

    /// Calculate the metrics of the sheet
    let getIllegalWireMetrics (sheetModel: SheetT.Model): illegalWireMetrics =
        
        {
            numberWires = sheetModel.Wire.Wires |> Map.count
            numberWireLabels = getNumberWireLabels sheetModel

            medianLength = getMedianLength sheetModel
            medianIntersection = getMedianIntersection sheetModel
            medianBend = getMedianBend sheetModel

            wiresTooLong = getWiresTooLong sheetModel
            wiresTooIntersecting = getWiresTooIntersecting sheetModel
            wireLablesTooClose = getWireLablesTooClose sheetModel
            wiresTooBendy = getWiresTooBendy sheetModel

            wireOverlappingBitLegends = getOverlappingBitLegends sheetModel
        }

    /// return wire list of all illegal wires defined by the metrics
    let getIllegalWires (sheetModel: SheetT.Model): Wire list =
        sheetModel
        |> getIllegalWireMetrics
        |> fun metrics -> 
            // printf "number of wires too long: %i\n" metrics.wiresTooLong.Count
            // printf "number of wires too intersecting: %i\n" metrics.wiresTooIntersecting.Count
            // printf "number of wires too bendy: %i\n" metrics.wiresTooBendy.Count
            metrics.wiresTooLong
            |> Set.union metrics.wiresTooIntersecting
            |> Set.union metrics.wiresTooBendy
            |> Set.toList

    /// debug function, only select wires that are greater than max length
    let getLongWires (wires: Wire List)=
        printf "number of wires: %i\n" wires.Length
        let filteredWires =
            wires
            |> List.map (fun wire -> (getWireLength wire),wire) //calculates the length for each wire
            |> List.filter( fun (wireLen,wire) -> wireLen > DEBUG_CONSTANTS.DEBUG_MAX_LENGTH) // filters out the short wires
            |> List.map ( fun tuple -> snd tuple)// returns a list of long wires
        printf "number of long wires: %i\n" filteredWires.Length
        filteredWires

    /// Main sheet beautify function, replaces all illegal wires with wire labels
    /// takes in a wire and a sheet model, replace the wire with wire labels
    let replaceWireWithWireLabels (wire:Wire) (initialSheet:SheetT.Model)=
        //find the target and source ports of the original wire
        let (targetPort:Port)= getTargetPort initialSheet.Wire wire
        let (sourcePort:Port)= getSourcePort initialSheet.Wire wire
        ///this function checks if there is already a wire label for the given source port
        ///returns the list of already attached wirelabels, empty list if none
        let commonSourceWiresLabels (port: Port) (sheet:SheetT.Model)=
            sheet.Wire.Wires
            |> Map.toList 
            |> List.map snd
            |> List.filter (fun wire -> getSourcePort sheet.Wire wire = port) //find all wires that have the same source port
            |> List.map (fun wire -> getTargetPort sheet.Wire wire)
            |> List.map (fun port -> port.HostId) //get the component id of the target port
            |> List.map (fun id -> ComponentId id)
            |> List.choose (fun id -> Map.tryFind id sheet.Wire.Symbol.Symbols) 
            |> List.filter (fun sym -> sym.Component.Type = IOLabel)
        let newIOLabelName (port: Port) (sheet:SheetT.Model)=
            let hostComponentName =
                sheet.Wire.Symbol.Symbols
                |> Map.tryFind (ComponentId port.HostId)
                |> function
                    | Some sym -> sym.Component.Label + "_I"
                    | None -> failwithf "The given component should be in the list of symbols"
            let generateLabel (model: SymbolT.Model) (name:string) : string =
                let listSymbols = List.map snd (Map.toList model.Symbols)
                let newCompBaseName, newCompNo = extractIOPrefix name []
                //printfn "s %s , n%i" newCompBaseName newCompNo
                let existingNumbers =
                    listSymbols
                    |> List.collect (fun sym ->
                        let baseName,no = extractIOPrefix sym.Component.Label []
                        if baseName = newCompBaseName then
                            [no]
                        else []
                    )
                match existingNumbers with
                |[] -> name
                |[-1] ->
                    if newCompNo = -1 then
                        name+"1"
                    else name
                |lst -> 
                    let max = List.max existingNumbers
                    if List.exists (fun x -> x=newCompNo) lst then
                        newCompBaseName + (string (max+1))
                    else 
                        name
            generateLabel sheet.Wire.Symbol hostComponentName
        let sourceLabel: string option= 
            match commonSourceWiresLabels sourcePort initialSheet with
            | [] -> Some (newIOLabelName sourcePort initialSheet)//(sourcePort.Id ) //TODO: get the name of the port
            | _ -> None //if there is already a wire label, then we do not need to create a new one
        let targetLabel: string=
            match sourceLabel with
            | Some label -> label
            | None -> (commonSourceWiresLabels sourcePort initialSheet).Head.Component.Label //if there is already a wire label, then we do not need to create a new one
        let readPortPos(sheet:SheetT.Model) (port : Port)=
            let sym =
                sheet.Wire.Symbol.Symbols
                |> Map.toList
                |> List.tryFind (fun (_, sym) -> sym.Component.Id = port.HostId)
                |> function
                    | Some (_, sym) -> sym
                    | None -> failwithf "The given component should be in the list of symbols"
            let portOffset = getPortPos sym port
            sym.Pos + portOffset
        let targetPortPos= readPortPos initialSheet targetPort
        let sourcePortPos= readPortPos initialSheet sourcePort
        // if isInput=true, then the label is acting as an input, else output
        //calculates the position of the newly added io labels
        let calcLabelPositionAndRotation (port:Port) (sheet:SheetT.Model) (portPos:XYPos)  isInput =
            let a,b,height,width=getComponentProperties IOLabel "1"
            let orientation:Edge=getPortOrientationFrmPortIdStr  sheet.Wire.Symbol port.Id 
            //printf "orientation %A\n" orientation
            // let portPos= port.
            let labelPos=
                match orientation with
                | Top -> {X= portPos.X+15.; Y=portPos.Y-1.5*height-25.} 
                | Bottom-> {X= portPos.X+15. ; Y=portPos.Y+1.5*height}
                | Left-> {X= portPos.X-1.*width ; Y=portPos.Y}
                | Right ->{X= portPos.X+1.*width ; Y=portPos.Y}
            let labelRotation=
                match orientation,isInput with
                | Top, true-> Degree90
                | Top, false-> Degree270
                | Bottom, true-> Degree270
                | Bottom, false-> Degree90
                | Left, true-> Degree0 
                | Right,true -> Degree180
                | Left,false-> Degree180
                | Right,false -> Degree0

            labelPos, labelRotation
        let targetIOLabelPos, targetIORotation= calcLabelPositionAndRotation targetPort initialSheet targetPortPos true
        let sourceIOLabelPos, sourceIORotation=calcLabelPositionAndRotation sourcePort initialSheet sourcePortPos false
        let targetLabelSymbolSheet = 
            initialSheet
            |> fun sheet -> {sheet with Wire={sheet.Wire with Wires = sheet.Wire.Wires.Remove(wire.WId)}} //removing original wire
            |> placeSymbol targetLabel IOLabel targetIOLabelPos targetIORotation
            |> function
                |(sym, Ok sheet) -> Ok (placeWire targetPort sym sheet)
                |(sym, Error mess) -> Error "fail to place target symbol" 
        //only place sourceLabelSymbol if targetLabelSymbol was placed successfully
        //and if there is not already a source label Symbol for the source port
        let sourceLabelSymbolSheet = 
            match sourceLabel with
            | Some label -> 
                targetLabelSymbolSheet
                |> function
                    | Ok sheet -> 
                        sheet
                        |> placeSymbol label IOLabel sourceIOLabelPos sourceIORotation
                        |> fun (sym, sheet) -> 
                            match sheet with
                            | Ok sheet -> Ok (placeWire sourcePort sym sheet)
                            | Error mess -> Error mess
                    | Error mess -> 
                        Error mess
            | None -> 
                targetLabelSymbolSheet

        sourceLabelSymbolSheet
        |> function
            | Ok sheet -> 
            // printf "Ok"
                sheet
            | Error mess -> 
                //printf "Error placing IOlabel symbol: %s\n" mess
                initialSheet //do nothing if there is some error in placing the target label symbol
    
    /// Top Level functon to replace all illegal wires with wire labels
    let optimizeWires (sheet:SheetT.Model)=
        let wiresToOptimize = 
            if DEBUG_CONSTANTS.DEBUG then
                sheet.Wire.Wires
                |> Map.toList
                |> List.map (fun (id, wire) -> wire)
                |> getLongWires
            else
                getIllegalWires sheet
        (sheet, wiresToOptimize)
        ||> List.fold (fun sheet wire -> replaceWireWithWireLabels wire sheet)


/// functions to find illegal wire labels and replace them with wires
/// Buggy, need to fix
module illgealWireLabelToWires =
    // change wire labels back to wires
    let findConnectedLabels (model:SheetT.Model) (label:string) =
        let symbolsMap = model.Wire.Symbol.Symbols
        let labels :Symbol array = 
            mapValues symbolsMap
            |> Array.filter (fun sym -> caseInvariantEqual sym.Component.Label label)
            //|>(fun symArray -> if (Array.isEmpty symArray) then failwithf "can't find iolabel with name" else symArray    )
        labels

    let findNewWirePorts(model:SheetT.Model)(labels: Symbol array) =
        let wires=model.Wire.Wires |> Map.toList |> List.map snd
        let connectedInputPorts=model.Wire.Symbol.InputPortsConnected |> Set.map (fun portid -> getInputPortIdStr portid)
        let connectedOutputPorts=
            model.Wire.Symbol.OutputPortsConnected 
            |>Map.toList 
            |> List.filter (fun (pId,num) -> num>0 )
            |> List.map fst
            |> List.map (fun portid -> getOutputPortIdStr portid)
        // let sourcelabel=
        //     labels
        //     |> Array.filter(fun label -> Set.contains(label.Component.InputPorts[0]. ) connectedInputPorts)

        let sourceLabelPortId= // get the port ids of the input label, which is the output port of a wire that connects to it
            labels
            |> Array.filter(fun label -> Set.contains(label.Component.InputPorts[0].Id ) connectedInputPorts)
            |>(fun symArray -> if (Array.isEmpty symArray) then failwithf "can't find labels act as input" else symArray    )
            |> Array.map( fun label ->label.Component.InputPorts[0].Id) // there should be only 1 source label
            |> Array.toList
        let outputLabelPortIds= // get the port ids of the input label, which is the in port of a wire that connects to it
            labels
            |> Array.filter(fun label -> List.contains(label.Component.OutputPorts[0].Id ) connectedOutputPorts)
            |>(fun symArray -> if (Array.isEmpty symArray) then failwithf "can't find labels act as output" else symArray    )
            |> Array.map( fun label ->label.Component.OutputPorts[0].Id)
            |> Array.toList// there can be many output labels

        let sourceWire =
            wires
            |> List.tryFind(fun wire -> List.contains(getOutputPortIdStr wire.OutputPort) sourceLabelPortId)
            |> function
                    | Some wire -> wire
                    | None -> failwithf "can't find source wire"
        let outputWires=
            wires
            |> List.filter(fun wire -> List.contains(getInputPortIdStr wire.InputPort) outputLabelPortIds)
            |>(fun wires -> if (List.isEmpty wires) then failwithf "can't find labels act as input" else wires    )

        let sourceport=
            sourceWire.InputPort
        let outputports=
            outputWires |> List.map(fun wire ->wire.OutputPort)


        sourceport, outputports



    let makeWire inputport outputport (sheet:SheetT.Model) =
        let newWire =  BusWireUpdate.makeNewWire inputport outputport sheet.Wire
        
        {sheet with Wire={sheet.Wire with Wires= Map.add newWire.WId newWire sheet.Wire.Wires}}
    
    //let findShortWiresToReplace() :string=()
        
    let replaceWireLablesWithWire (labelName:string) (sheet:SheetT.Model)  =
        let connectedLabels:Symbol array= findConnectedLabels sheet labelName
        //printf "connectedLabels!!!!!!!!!!!!! %d" connectedLabels.Length

        let newWireports=findNewWirePorts sheet connectedLabels
        //printf "inputPort" 
        let inputPort=fst newWireports
        let outputPorts=snd newWireports

        
        let sheetwithRemovedLabels=
            connectedLabels
            |> Array.toList
            |> List.map(fun label -> label.Id)
            |> List.fold( fun (sheet:SheetT.Model) labelId-> 
                {sheet with Wire= {sheet.Wire with Symbol = {sheet.Wire.Symbol with Symbols=sheet.Wire.Symbol.Symbols.Remove(labelId)}}}) sheet

        let targetLabelSymbolSheet = 
            outputPorts
            |>List.fold( fun (sheet:SheetT.Model) outputport -> 
                (makeWire inputPort outputport sheet)) sheetwithRemovedLabels
        targetLabelSymbolSheet

    ///top level function to replace all illegal wire labels with wires
    let optimizeLabels(sheet:SheetT.Model)=
        sheet
        //|> replaceWireLablesWithWire "MUX1_I"

/// functions to reconnect wires that are connected to the same source port
/// if the source port is connected to an IO label
/// then reconnect all the wires to the output port of the IO label
module reconnectCommonSourceWires = 

    /// get the Connected Wire given a target Port on IO Label
    let getWireofTargetPort (port:Port) (sheet:SheetT.Model): Wire=
        match port.PortType with
        | PortType.Input -> 
            sheet.Wire.Wires
            |> Map.toList 
            |> List.map snd
            |> List.find (fun wire -> getTargetPort sheet.Wire wire = port) //find all wires that have the same source port, shoul only be one
        | PortType.Output -> failwithf "expecting input port, got output port, when trying to optimize"
        
    /// get all other wires from the same source Port given a wire
    let commonSourceWires(targetWire: Wire) (sheet:SheetT.Model)=
        let port = getSourcePort sheet.Wire targetWire
        sheet.Wire.Wires
        |> Map.toList 
        |> List.map snd
        |> List.filter (fun wire -> getSourcePort sheet.Wire wire = port) //find all wires that have the same source port
        |> List.filter (fun wire -> targetWire.WId <> wire.WId) //remove the original wire
        //|> (fun a -> failwithf "commonSourceWires %d" a.Length)

    /// given sheet, output a list of all wireId which can be reconnecteed to the source Port of a IO label
    /// combine it with the new PortId to reconnect to
    let wireToReconnect (sheet: SheetT.Model)=
        let isSourcePortBinded (port:Port) (sheet: SheetT.Model) =
            //printf "isSourcePortBinded %d" sheet.Wire.Symbol.InputPortsConnected.Count
            sheet.Wire.Wires
            |> Map.toList
            |> List.exists (fun (id, wire) -> getTargetPort sheet.Wire wire = port)
        sheet.Wire.Symbol.Symbols
        |> Map.toList
        |> List.map (fun (id, symbol) -> symbol)
        |> List.filter (fun symbol -> symbol.Component.Type = ComponentType.IOLabel)
        |> List.map (fun symbol -> (OutputPortId symbol.Component.OutputPorts.Head.Id, symbol.Component.InputPorts.Head))
        |> List.filter (fun (reconnectPortId, port) -> isSourcePortBinded port sheet)
        |> List.map (fun (reconnectPortId, port) -> (reconnectPortId, getWireofTargetPort port sheet))
        |> List.map (fun (reconnectPortId, targetWire) -> (reconnectPortId, commonSourceWires targetWire sheet))
        |> List.collect (fun (reconnectPortId, wires) -> 
            wires
            |> List.map (fun wire -> (reconnectPortId, wire))
        )
        |> List.map (fun (reconnectPortId, wire) -> (wire.WId, reconnectPortId))


    ///top level function to reconnect all the common source wires
    let optimizeReconnect(sheet:SheetT.Model)=
        let newWireModel = 
            (sheet.Wire, wireToReconnect sheet)
            ||> List.fold (fun sheet reconnectWire -> 
                    let wireId, sourcePortId = reconnectWire
                    let wire = sheet.Wires.[wireId]
                    let newWire = {wire with OutputPort = sourcePortId}
                    {sheet with Wires = sheet.Wires.Add(wireId, newWire)}
                    )   
        {sheet with Wire=newWireModel}

        
///Top level function to beautify the sheet, D3 deliverable
let wireLabelBeautify(sheet:SheetT.Model)=
    /// Reroute all the wires associating with IO Labels in the sheet
    /// making sure that no wire intersect symbols
    let rerouteIOLabelWires (sheet:SheetT.Model)=
        let IOLabelIds =
            sheet.Wire.Symbol.Symbols
            |> Map.toList
            |> List.filter (fun (id, symbol) -> symbol.Component.Type = ComponentType.IOLabel)
            |> List.map (fun (id, wire) -> id)
        let newWireModel= BusWireRoute.updateWires sheet.Wire IOLabelIds {X = 0; Y = 0}
        {sheet with Wire=newWireModel}

    sheet
    |> illegalWireToWireLabels.optimizeWires
    |> illgealWireLabelToWires.optimizeLabels
    |> reconnectCommonSourceWires.optimizeReconnect
    |> rerouteIOLabelWires