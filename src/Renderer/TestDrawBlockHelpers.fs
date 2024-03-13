module TestDrawBlockHelpers

module SimpleSymbol =
    open EEExtensions
    open Optics
    open Optics.Operators
    open Helpers
    open CommonTypes
    open ModelType
    open DrawModelType
    open Sheet.SheetInterface
    open SheetBeautifyHelpers

    /// create an initial empty Sheet Model
    let initSheetModel = DiagramMainView.init().Sheet

    /// Optic to access BusWireT.Model from SheetT.Model
    let busWireModel_ = SheetT.wire_

    /// Optic to access SymbolT.Model from SheetT.Model
    let symbolModel_ = SheetT.symbol_

    /// allowed max X or y coord of svg canvas
    let maxSheetCoord = Sheet.Constants.defaultCanvasSize

    let middleOfSheet = {X=maxSheetCoord/2.;Y=maxSheetCoord/2.}

    /// Used throughout to compare labels since these are case invariant "g1" = "G1"
    let caseInvariantEqual str1 str2 =
        String.toUpper str1 = String.toUpper str2

    /// convenience unsafe function to extract Ok part of Result or fail if value is Error
    let getOkOrFail (res: Result<'a,string>) =
        match res with
        | Ok x -> x
        | Error mess ->
            failwithf "%s" mess

    let flipPortMaps (sym: SymbolT.Symbol) : SymbolT.PortMaps =
        let portOrientation =
            sym.PortMaps.Orientation |> Map.map (fun id side -> SymbolResizeHelpers.flipSideHorizontal side)

        let flipPortList currPortOrder side =
            currPortOrder |> Map.add (SymbolResizeHelpers.flipSideHorizontal side ) sym.PortMaps.Order[side]

        let portOrder =
            (Map.empty, [Top; Left; Bottom; Right]) ||> List.fold flipPortList
            |> Map.map (fun edge order -> List.rev order)

        {Order=portOrder;Orientation=portOrientation}


    /// Identify a port from its component label and number.
    /// Usually both an input and output port will mathc this, so
    /// the port is only unique if it is known to be input or output.
    /// used to specify the ends of wires, since tehee are known to be
    /// connected to outputs (source) or inputs (target).
    type SymbolPort = { Label: string; PortNumber: int }

    type SimpleSymbol = {
        SymLabel : string
        CompType : ComponentType
        Position: XYPos
        STransform: STransform
    }

    type SimpleConnection = {
        Source: SymbolPort
        Target: SymbolPort
    }

    type TestModel = {
        SimpleSymbols : SimpleSymbol List
        Connections : SimpleConnection List
    }

    let connections_ = Lens.create (fun m -> m.Connections) (fun v m -> {m with Connections = v})

    let createSimpleSymbol (label: string) (compType: ComponentType) (position: XYPos) (sTransform: STransform) =
        { SymLabel = label;
        CompType = compType;
        Position = position;
        STransform = sTransform }

    let createTestModel (simpleSymbols: SimpleSymbol List) (connections: SimpleConnection List) =
        { SimpleSymbols = simpleSymbols
          Connections = connections }

    let getSimSymbolMap (model: SheetT.Model) : Map<ComponentId, SimpleSymbol> =
        let extractValues (label: string) (symbol: SymbolT.Symbol) : SimpleSymbol=
            { SymLabel = label
              CompType = symbol.Component.Type
              Position = { X = symbol.Pos.X + float symbol.Component.W / 2.0; Y = symbol.Pos.Y + float symbol.Component.H / 2.0 }
              STransform = symbol.STransform }

        Optic.get SheetT.symbols_ model
        |> Map.toList
        |> List.mapi (fun index symbolMap -> // have access to index here so can add to symLabel if desired
                let symbol = snd symbolMap
                (fst symbolMap, extractValues (symbol.Component.Label) symbol))
        |> Map.ofList


    let getSimpleConnections (model: SheetT.Model) (symbolMap: Map<ComponentId, SimpleSymbol>) =
        let getSymLabel (hostId: ComponentId) =
            symbolMap
            |> Map.find hostId
            |> fun sym -> sym.SymLabel

        let getPortIndex (port: Port) (portList: List<Port>) =
            portList
            |> List.findIndex (fun elm -> port.Id = elm.Id)

        let getSymbolPort (portType: PortType) (port: Port) =
            let compId = ComponentId port.HostId
            let Symbol = Optic.get (SheetT.symbolOf_ compId) model

            let portNum =
                match portType with
                    | PortType.Input -> getPortIndex port Symbol.Component.InputPorts
                    | PortType.Output -> getPortIndex port Symbol.Component.OutputPorts

            { Label = getSymLabel compId
              PortNumber = portNum }

        BusWire.extractConnections model.Wire
        |> List.map (fun conn ->
             { Source = getSymbolPort PortType.Output conn.Source
               Target = getSymbolPort PortType.Input conn.Target })


    let getTestModel (model: SheetT.Model) =
        let simpleSymbolMap = getSimSymbolMap model

        { SimpleSymbols = Map.values simpleSymbolMap |> Array.toList
          Connections = getSimpleConnections model simpleSymbolMap  }

//------------------------------------------------------------------------------------------------------------------------//
//------------------------------functions to build issue schematics programmatically--------------------------------------//
//------------------------------------------------------------------------------------------------------------------------//
    module Builder =

        /// <summary> Output a sheet model with a SimpleSymbol added to it. </summary>
        /// <param name="model">The Sheet model into which the new symbol is added.</param>
        /// <param name="simSymbol">The SimpleSymbol to be added to the model.</param>
        let placeSimpleSymbol (simSymbol: SimpleSymbol) (model: SheetT.Model) : Result<SheetT.Model, string> =
            let symLabel = String.toUpper simSymbol.SymLabel // make label into its standard casing
            let symModel, symId = SymbolUpdate.addSymbol [] (model.Wire.Symbol) simSymbol.Position simSymbol.CompType symLabel
            let sym = symModel.Symbols[symId]

            let portMaps' =
                if simSymbol.STransform.Flipped
                then
                    flipPortMaps sym
                    |> SymbolResizeHelpers.rotatePortInfo simSymbol.STransform.Rotation
                else
                    SymbolResizeHelpers.rotatePortInfo simSymbol.STransform.Rotation sym.PortMaps

            let sym' = sym
                      |> Optic.set symbol_flipped_ simSymbol.STransform.Flipped
                      |> Optic.set symbol_rotation_ simSymbol.STransform.Rotation
                      |> Optic.set SymbolT.portMaps_ portMaps'

            let symModel' = Optic.set (SymbolT.symbolOf_ symId) sym' symModel

            match simSymbol.Position with
            | {X=x;Y=y} when x > maxSheetCoord || y > maxSheetCoord ->
                Error $"symbol '{symLabel}' position {simSymbol.Position + sym.getScaledDiagonal} lies outside allowed coordinates"
            | _ ->
                model
                |> Optic.set symbolModel_ symModel'
                |> SheetUpdateHelpers.updateBoundingBoxes
                |> Ok


        /// Add a (newly routed) wire, source specifies the Output port, target the Input port.
        /// Return an error if either of the two ports specified is invalid, or if the wire duplicates and existing one.
        /// The wire created will be smart routed but not separated from other wires: for a nice schematic
        /// separateAllWires should be run after  all wires are added.
        /// source, target: respectively the output port and input port to which the wire connects.
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
                     |> Optic.map busWireModel_ (BusWireSeparate.updateWireSegmentJumpsAndSeparations [newWire.WId])
                     |> Ok


        /// <summary> Fold over a list of simple symbols and output a sheet model with all of them added. </summary>
        /// <param name="model">The Sheet model into which the new symbols are added.</param>
        /// <param name="simSymbolList">The list of SimpleSymbols to be added to the model.</param>
        let placeSimSymbolList (simSymbolList: List<SimpleSymbol>) (model: SheetT.Model) =
            (Ok model,simSymbolList)
            ||> List.fold (fun curModel curSimSymbol ->
                Result.bind (placeSimpleSymbol curSimSymbol) curModel)


        let placeConnections (conns: List<SimpleConnection>) (model: SheetT.Model) =
            (Ok model,conns)
            ||> List.fold (fun curModel curConn ->
                Result.bind (placeWire curConn.Source curConn.Target) curModel)


        /// Run the global wire separation algorithm (should be after all wires have been placed and routed)
        let separateAllWires (model: SheetT.Model) : SheetT.Model =
            model
            |> Optic.map busWireModel_ (BusWireSeparate.updateWireSegmentJumpsAndSeparations (model.Wire.Wires.Keys |> Seq.toList))

        let placeTestModel (testModel: TestModel) =
            initSheetModel
            |> placeSimSymbolList testModel.SimpleSymbols
            |> Result.bind (placeConnections testModel.Connections)
            |> Result.map separateAllWires
            |> getOkOrFail


        /// Place a new symbol with label symLabel onto the Sheet with given position.
        /// Return error if symLabel is not unique on sheet, or if position is outside allowed sheet coordinates (0 - maxSheetCoord).
        /// To be safe place components close to (maxSheetCoord/2.0, maxSheetCoord/2.0).
        /// symLabel - the component label, will be uppercased to make a standard label name
        /// compType - the type of the component
        /// position - the top-left corner of the symbol outline.
        /// model - the Sheet model into which the new symbol is added.
        let placeSymbol (symLabel: string) (compType: ComponentType) (position: XYPos) (model: SheetT.Model) : Result<SheetT.Model, string> =
            let symLabel = String.toUpper symLabel // make label into its standard casing
            let symModel, symId = SymbolUpdate.addSymbol [] (model.Wire.Symbol) position compType symLabel
            let sym = symModel.Symbols[symId]
            match position + sym.getScaledDiagonal with
            | {X=x;Y=y} when x > maxSheetCoord || y > maxSheetCoord ->
                Error $"symbol '{symLabel}' position {position + sym.getScaledDiagonal} lies outside allowed coordinates"
            | _ ->
                model
                |> Optic.set symbolModel_ symModel
                |> SheetUpdateHelpers.updateBoundingBoxes // could optimise this by only updating symId bounding boxes
                |> Ok


        /// Place a new symbol onto the Sheet with given position and scaling (use default scale if this is not specified).
        /// The ports on the new symbol will be determined by the input and output components on some existing sheet in project.
        /// Return error if symLabel is not unique on sheet, or ccSheetName is not the name of some other sheet in project.
        let placeCustomSymbol
                (symLabel: string)
                (ccSheetName: string)
                (project: Project)
                (scale: XYPos)
                (position: XYPos)
                (model: SheetT.Model)
                    : Result<SheetT.Model, string> =
           let symbolMap = model.Wire.Symbol.Symbols
           if caseInvariantEqual ccSheetName project.OpenFileName then
                Error "Can't create custom component with name same as current opened sheet"
            elif not <| List.exists (fun (ldc: LoadedComponent) -> caseInvariantEqual ldc.Name ccSheetName) project.LoadedComponents then
                Error "Can't create custom component unless a sheet already exists with smae name as ccSheetName"
            elif symbolMap |> Map.exists (fun _ sym ->  caseInvariantEqual sym.Component.Label symLabel) then
                Error "Can't create custom component with duplicate Label"
            else
                let canvas = model.GetCanvasState()
                let ccType: CustomComponentType =
                    {
                        Name = ccSheetName
                        InputLabels = Extractor.getOrderedCompLabels (Input1 (0, None)) canvas
                        OutputLabels = Extractor.getOrderedCompLabels (Output 0) canvas
                        Form = None
                        Description = None
                    }
                placeSymbol symLabel (Custom ccType) position model
