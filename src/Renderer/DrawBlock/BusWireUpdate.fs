module BusWireUpdate

open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers
open BlockHelpers
open BusWireRoute
open Optics
open Optics.Operators
open Operators
open BlockHelpers

//---------------------------------------------------------------------------------//
//------------------------------BusWire Init & Update functions--------------------//
//---------------------------------------------------------------------------------//

/// Initialises an empty BusWire Model
let init () = 
    let symbols,_ = SymbolView.init()
    {   
        Wires = Map.empty;
        Symbol = symbols; 
        CopiedWires = Map.empty; 
        SelectedSegment = []; 
        LastMousePos = {X = 0.0; Y = 0.0};
        ErrorWires = []
        Notifications = None
        Type = Constants.initialWireType
        ArrowDisplay = Constants.initialArrowDisplay
        SnapToNet = true
    } , Cmd.none

let dragSegment wire index (mMsg: MouseT) model =
    match List.tryItem index wire.Segments with
    | None -> 
        printfn "Bad segment in Dragsegment... ignoring drag"
        model
    | Some seg when index < 1 || index > wire.Segments.Length-2 ->
        printfn "Bad index - can't move that segment"
        model
    | Some seg ->        
        let (startPos,endPos) = getAbsoluteSegmentPos wire index
        if seg.Draggable then
            let distanceToMove = 
                match getSegmentOrientation startPos endPos with
                | Horizontal -> mMsg.Pos.Y - startPos.Y
                | Vertical -> mMsg.Pos.X - startPos.X

            let newWire = moveSegment model seg distanceToMove 
            let newWires = Map.add seg.WireId newWire model.Wires

            { model with Wires = newWires }
        else
            printfn "Can't move undraggable"
            model

let newWire inputId outputId model =
    let wireId = ConnectionId(JSHelpers.uuid())
    let nWire =
        {
            WId = wireId
            InputPort = inputId
            OutputPort = outputId
            Color = HighLightColor.DarkSlateGrey
            Width = 1
            Segments = []
            StartPos = { X = 0; Y = 0 }
            InitialOrientation = Horizontal
        }
        |> smartAutoroute model

    if Map.exists (fun wid wire -> wire.InputPort=nWire.InputPort && wire.OutputPort = nWire.OutputPort) model.Wires then
            // wire already exists
            model, None
        else
            printfn "Separating new wire"
            let newModel = 
                model
                |> Optic.set (wireOf_ nWire.WId) nWire
                |> BusWireSeparate.updateWireSegmentJumpsAndSeparations [nWire.WId]
            newModel, Some BusWidths

let calculateBusWidths model =
        //printfn "BusWidths Message"
        // (1) Call Issie bus inference
        // (2) Add widths to maps on symbols on wires
        let processConWidths (connWidths: ConnectionsWidth) =
            let addWireWidthFolder (wireMap: Map<ConnectionId, Wire>) _ wire  =
                let width =
                    match connWidths[wire.WId] with
                    | Some a -> a
                    | None -> wire.Width
                let newColor = 
                    if wire.Color = Purple || wire.Color = Brown then Purple else DarkSlateGrey
                wireMap.Add ( wire.WId, { wire with Width = width ; Color = newColor} )

            let addSymbolWidthFolder (m: Map<ComponentId,Symbol>) (_: ConnectionId) (wire: Wire) =
                    let inPort = model.Symbol.Ports[match wire.InputPort with InputPortId ip -> ip]
                    let symId = ComponentId inPort.HostId
                    let symbol = m[symId]

                    match symbol.Component.Type with
                    | SplitWire n ->
                        match inPort.PortNumber with
                        | Some 0 -> {symbol with InWidth0 = Some wire.Width}
                        | x -> failwithf $"What? wire found with input port {x} other than 0 connecting to SplitWire"
                        |> (fun sym -> Map.add symId sym m)
                    | SplitN _ -> 
                        match inPort.PortNumber with
                        | Some 0 -> {symbol with InWidth0 = Some wire.Width}
                        | x -> failwithf $"What? wire found with input port {x} other than 0 connecting to SplitN"
                        |> (fun sym -> Map.add symId sym m)
                    | MergeWires ->
                        match inPort.PortNumber with
                        | Some 0 ->
                            Map.add symId  {symbol with InWidth0 = Some wire.Width} m
                        | Some 1 ->
                            Map.add symId {symbol with InWidth1 = Some wire.Width} m
                        | x -> failwithf $"What? wire found with input port {x} other than 0 or 1 connecting to MergeWires"
                    | MergeN nInps -> 
                        match inPort.PortNumber with
                        | Some n when (n < nInps && n >= 0) -> 
                            let newInWidths: int option list = 
                                match symbol.InWidths with
                                | None -> List.init nInps (fun i -> 
                                    if i = n then Some wire.Width else None)
                                | Some list -> 
                                    match list.Length with 
                                    | len when len = nInps ->
                                        List.mapi (fun i x -> 
                                            if i = n then Some wire.Width else x) list
                                    | _ -> List.init nInps (fun i -> 
                                        if i = n then Some wire.Width else None)
                            Map.add symId {symbol with InWidths = Some newInWidths} m
                        | x -> failwithf $"What? wire found with input port {x} other than [0..{nInps-1}] connecting to MergeN"
                    | _ -> m

            let newWires = ((Map.empty, model.Wires) ||> Map.fold addWireWidthFolder)

            let symbolsWithWidths =
                (model.Symbol.Symbols, newWires) ||> Map.fold addSymbolWidthFolder

 
            { model with
                Wires = newWires; 
                Notifications = None;
                ErrorWires=[];
                Symbol = {model.Symbol with Symbols = symbolsWithWidths}
            }
                    
        let canvasState = (SymbolUpdate.extractComponents model.Symbol, extractConnections model)

        match BusWidthInferer.inferConnectionsWidth canvasState with
        | Ok connWidths ->
            processConWidths connWidths, None
        | Error e ->
            { model with Notifications = Some e.Msg }, Some (ErrorWires e.ConnectionsAffected)


/// Handles messages
let update (msg : Msg) (issieModel : ModelType.Model) : ModelType.Model*Cmd<ModelType.Msg> =
    let model = issieModel.Sheet.Wire

    let toIssieModel wireModel = {issieModel with Sheet={issieModel.Sheet with Wire=wireModel}}



    let withNoMsg (model: ModelType.Model) = model, Cmd.none

    /// Command to issie level
    let withIssieMsg (msg: ModelType.Msg) (model: ModelType.Model) = model, Cmd.ofMsg msg
    
    /// Command to Sheet level
    let withSheetMsg (msg: DrawModelType.SheetT.Msg) (model: ModelType.Model) = model, Cmd.ofMsg (ModelType.Msg.Sheet msg)
    
    /// Command to BusWire level
    let withMsg (msg: DrawModelType.BusWireT.Msg) (model: ModelType.Model) = model, Cmd.ofMsg (ModelType.Msg.Sheet (DrawModelType.SheetT.Msg.Wire msg))
    
    /// Command to Symbol level
    let withSymbolMsg (msg: DrawModelType.SymbolT.Msg) (model: ModelType.Model) = model, Cmd.ofMsg (ModelType.Msg.Sheet (DrawModelType.SheetT.Msg.Wire (DrawModelType.BusWireT.Symbol msg)))

    let withOptMsg (msgOpt: Msg option) = (if msgOpt.IsSome then withMsg (Option.get msgOpt) else withNoMsg)

    ///
    let withMsgs (msgs: Msg list) (model : Model) =
        let wireMsg msg = Cmd.ofMsg (ModelType.Msg.Sheet (DrawModelType.SheetT.Msg.Wire msg))
        model, Cmd.batch (List.map wireMsg msgs)

    match msg with
    | Symbol sMsg ->
        // update Symbol model with a Symbol message
        let sm,sCmd = SymbolUpdate.update sMsg model.Symbol
        {issieModel with Sheet={issieModel.Sheet with Wire={model with Symbol=sm}}}, (Cmd.map (fun msg -> ModelType.Msg.Sheet (DrawModelType.SheetT.Msg.Wire msg)) sCmd)


    | UpdateWires (componentIdList, diff) ->
        // update wires after moving components in componentIdList by diff
        // wires between components are translated not routed as optimisation
        {issieModel with Sheet={issieModel.Sheet with Wire=updateWires model componentIdList diff}} |> withNoMsg

    | UpdateSymbolWires compId ->
        // update all the wires coming from a single symbol
        // useful if the symbol has been flipped or ports have been moved
        // partial routing will be done if this makes sense
        {issieModel with Sheet={issieModel.Sheet with Wire=BusWireSeparate.routeAndSeparateSymbolWires model compId}} |> withNoMsg

    | AddWire ( (inputId, outputId) : (InputPortId * OutputPortId) ) ->
        // add a newly created wire to the model
        // then send BusWidths message which will re-infer bus widths
        // the new wires (extarcted as connections) are not added back into Issie model. 
        // This happens on save or when starting a simulation (I think)
        let newModel, msgOpt = newWire inputId outputId model
        {issieModel with Sheet={issieModel.Sheet with Wire=newModel}} |> (if msgOpt.IsSome then withMsg (Option.get msgOpt) else withNoMsg)
    
    | BusWidths ->
        let newModel, msgOpt = calculateBusWidths model
        toIssieModel newModel
        |> withOptMsg msgOpt

    | CopyWires (connIds : list<ConnectionId>) ->
        // add given wires to Copiedwires state (NB, this contains wires at time of copy)
        let copiedWires = Map.filter (fun connId _ -> List.contains connId connIds) model.Wires
        {issieModel with Sheet={issieModel.Sheet with Wire={ model with CopiedWires = copiedWires }}} |> withNoMsg

    | ErrorWires (connectionIds : list<ConnectionId>) ->
        // record these wires in model.ErrorWires and highlight them as red.
        // reset the wires that were remobed from model.ErrorWires dark grey 
        // (what if they are supposed to be something else?? Colors carry too muhc state!)
        let newWires =
            model.Wires
            |> Map.map
                (fun id wire ->
                    if List.contains id connectionIds then
                        {wire with Color = HighLightColor.Red}
                    else if List.contains id model.ErrorWires then
                        {wire with Color = HighLightColor.DarkSlateGrey}
                    else wire
                )

        {issieModel with Sheet={ issieModel.Sheet with Wire={ model with Wires = newWires ; ErrorWires = connectionIds }}} |> withNoMsg

    | SelectWires (connectionIds : list<ConnectionId>) -> 
        // selects all wires in connectionIds, and also deselects all other wires
        let newWires =
            model.Wires
            |> Map.map
                (fun id wire ->
                    if List.contains id model.ErrorWires then
                        if List.contains id connectionIds then
                            {wire with Color = HighLightColor.Brown}
                        else
                            {wire with Color = HighLightColor.Red}
                    else if List.contains id connectionIds then
                        {wire with Color = HighLightColor.Purple}
                    else
                        {wire with Color = HighLightColor.DarkSlateGrey}
                )

        {issieModel with Sheet={ issieModel.Sheet with Wire={ model with Wires = newWires ; ErrorWires = []}}} |> withNoMsg


    | DeleteWires (connectionIds : list<ConnectionId>) ->
        // deletes wires from model, then runs bus inference
        // Issie model is not affected but will extract connections from wires
        // at some time.
        let newWires =
             model.Wires
             |> Map.filter (fun id wire -> not (List.contains id connectionIds))
        let model =
            {model with Wires = newWires}
        {issieModel with Sheet={ issieModel.Sheet with Wire={ model with Wires = newWires ; ErrorWires = connectionIds }}} |> withMsg BusWidths

    | DeleteWiresWithPort (delPorts:(Port option) list) ->
        issieModel
        |> Optic.set (ModelType.sheet_ >-> DrawModelType.SheetT.wire_) (deleteWiresWithPort delPorts model)
        |> withNoMsg


    | DragSegment (segIdL : SegmentId list, mMsg: MouseT) ->
        let checkSegmentOK segId =
            let index, connId = segId
            let wire = model.Wires[connId]
            0 <= index && index < wire.Segments.Length
        segIdL
        |> List.filter checkSegmentOK
        |> function
            | [] -> issieModel |> withNoMsg
            | segIdL ->
                match mMsg.Op with
                | Down ->
                    {issieModel with Sheet={ issieModel.Sheet with Wire={ model with SelectedSegment = segIdL}}} |> withMsg (ResetJumps [])
                | Drag ->
                    (model, segIdL)
                    ||> List.fold (fun model segId ->
                            let index, connId = segId
                            let wire = model.Wires[connId]
                            dragSegment wire index mMsg model)
                    |> fun model -> {issieModel with Sheet={ issieModel.Sheet with Wire=model}}
                    |> withNoMsg
                | _ -> issieModel |> withNoMsg

    | CoalesceWire wId ->
        coalesceInWire wId model
        |> fun model -> {issieModel with Sheet={ issieModel.Sheet with Wire=model}}
        |> withNoMsg

    | ColorWires (connIds, color) -> 
        // Just Changes the colour of the wires, Sheet calls pasteWires before this
        let newWires =
            (List.fold (fun prevWires cId ->
                let oldWireOpt = Map.tryFind cId model.Wires
                match oldWireOpt with
                | None -> 
                    prevWires
                | Some oldWire ->
                    Map.add cId { oldWire with Color = color } prevWires) model.Wires connIds)
        {issieModel with Sheet={ issieModel.Sheet with Wire={ model with Wires = newWires }}} |> withNoMsg

    | ResetJumps connIds ->
        // removes wire 'jumps' at start of drag operation for neater component movement 
        // without jump recalculation
        // makejumps at end of a drag operation restores new jumps
        let newModel = resetWireSegmentJumps connIds model
        {issieModel with Sheet={ issieModel.Sheet with Wire=newModel}} |> withNoMsg

    | MakeJumps (separate, connIds) ->
        // recalculates (slowly) wire jumps after a drag operation
        let newModel =
            if separate then
                BusWireSeparate.updateWireSegmentJumpsAndSeparations connIds model
            else
                updateWireSegmentJumps connIds model
        {issieModel with Sheet={ issieModel.Sheet with Wire=newModel}} |> withNoMsg

    | ResetModel -> 
        // How we start with nothing loaded
        {issieModel with Sheet={ issieModel.Sheet with Wire={ model with Wires = Map.empty; ErrorWires = []; Notifications = None }}} |> withNoMsg

    | LoadConnections conns -> 
        // we assume components (and hence ports) are loaded before connections
        // Issie connections are loaded as wires
        // vertices on Issie connections contains routing info so wires can be 
        // reconstructed precisely

        /// check whether a laoded wires position matches a symbol vertex
        /// If the vertices lits is empty the evrtex will be None, and not match
        let posMatchesVertex (pos:XYPos) (vertexOpt: (float*float) option) =
            match vertexOpt with
            | None -> 
                false
            | Some vertex ->
                let epsilon = Constants.vertexLoadMatchTolerance
                abs (pos.X - (fst vertex)) < epsilon &&
                abs (pos.Y - (snd vertex)) < epsilon
        
        // get the newly loaded wires
        let newWires =
            conns
            |> List.map ( fun conn ->
                let inputId = InputPortId conn.Target.Id
                let outputId = OutputPortId conn.Source.Id
                let connId = ConnectionId conn.Id
                let getVertex (x,y,_) = (x,y)
                let segments = issieVerticesToSegments connId conn.Vertices
                let makeWirePosMatchSymbol inOut (wire:Wire) =
                    match inOut with
                    | true -> 
                        posMatchesVertex
                                (Symbol.getInputPortLocation None model.Symbol inputId)
                                (List.tryLast conn.Vertices |> Option.map getVertex)
                    | false ->
                        posMatchesVertex
                            (Symbol.getOutputPortLocation None model.Symbol outputId)
                            (List.tryHead conn.Vertices |> Option.map getVertex)
                    |> (fun b ->
                        if b then
                            wire
                        else
                            updateWire model wire inOut)
                connId,
                { 
                    WId = ConnectionId conn.Id
                    InputPort = inputId
                    OutputPort = outputId
                    Color = HighLightColor.DarkSlateGrey
                    Width = 1
                    Segments = segments
                    StartPos = Symbol.getOutputPortLocation None model.Symbol outputId
                    InitialOrientation = 
                        getOutputPortOrientation model.Symbol outputId 
                        |> getOrientationOfEdge
                }
                |> makeWirePosMatchSymbol false
                |> makeWirePosMatchSymbol true
                |> (fun wire -> {wire with Segments = makeEndsDraggable wire.Segments})
            )
            |> Map.ofList

        let connIds =
            conns
            |> List.map (fun conn -> ConnectionId conn.Id)

        {issieModel with Sheet={ issieModel.Sheet with Wire={ model with Wires = newWires }}}
        |> withMsg (MakeJumps (false,connIds))

    | UpdateWireDisplayType (style: WireType) ->
        printfn "Updating wire display type (=> reseparation of wires)"
        {model with Type = style }
        |> BusWireSeparate.updateWireSegmentJumpsAndSeparations []
        |> fun model -> {issieModel with Sheet={ issieModel.Sheet with Wire=model}}
        |> withNoMsg

    | ToggleArrowDisplay  ->
        {issieModel with Sheet={ issieModel.Sheet with Wire={ model with ArrowDisplay = not model.ArrowDisplay}}} |> withNoMsg

    | UpdateConnectedWires (componentIds: ComponentId list) ->
        // partial or full autoroutes all ends of wires conencted to given symbols
        // typically used after rotating or flipping symbols
        printfn "Updating connected wires"
        let updatePortIdMessages: seq<Cmd<ModelType.Msg>> = 
            componentIds
            |> Symbol.getPortLocations model.Symbol
            |> (fun (m1,m2) -> 
                let inputPorts = Seq.map (fun (InputPortId portId) -> portId) m1.Keys |> Seq.toList
                let outputPorts = Seq.map (fun (OutputPortId portId) -> portId) m2.Keys |> Seq.toList
                inputPorts @ outputPorts
                |> List.map (Msg.RerouteWire >> Cmd.ofMsg)
                |> Seq.ofList)
            |> Seq.map (Cmd.map (fun cmd -> ModelType.Msg.Sheet (DrawModelType.SheetT.Msg.Wire cmd)))
        issieModel, Cmd.batch updatePortIdMessages

    | RerouteWire (portId: string) ->
        // parially or fully autoroutes wires connected to port
        // typically used after port has moved
        // NB if direction of port has changed wire must be autorouted.
        let portOpt = Map.tryFind portId model.Symbol.Ports 

        let rerouteInputEnd (wire:Wire) = 
            wire.InputPort = InputPortId portId
        
        let wiresToReroute = 
            model.Wires
            |> Map.filter (fun _id wire -> 
                wire.InputPort = InputPortId portId  || wire.OutputPort = OutputPortId portId)
            |> Map.toList

        let newWires =
            (model.Wires, wiresToReroute)
            ||> List.fold (fun wires (wid, wire) ->
                let wire' = updateWire model wire (rerouteInputEnd wire)
                Map.add wid wire' wires)

        {issieModel with Sheet={ issieModel.Sheet with Wire={model with Wires = newWires}}} |> withNoMsg
    | ToggleSnapToNet ->
        {issieModel with Sheet={ issieModel.Sheet with Wire={model with SnapToNet = not model.SnapToNet}}} |> withNoMsg
    

//---------------------------------------------------------------------------------//        
//---------------------------Other interface functions-----------------------------//
//---------------------------------------------------------------------------------//        


/// Checks if a wire intersects a bounding box by checking if any of its segments intersect
/// returns some of distance to wire, if wire does intersect
let wireIntersectsBoundingBox (wire : Wire) (box : BoundingBox) =
    let segmentIntersectsBox segStart segEnd state seg =
        match state with
        | Some x -> Some x
        | None -> segmentIntersectsBoundingBox box segStart segEnd
    
    foldOverSegs segmentIntersectsBox None wire

/// Returns a list of wire IDs in the model that intersect the given selectBox
/// the wires are sorted by closeness to the centre of the box.
/// optimised for speed and low heap use
let getIntersectingWires (wModel : Model) (selectBox : BoundingBox) : list<ConnectionId*float> =
    let mutable wires : (ConnectionId * float) list= []
    wModel.Wires
    |> Map.iter (fun id wire  ->
        match wireIntersectsBoundingBox wire selectBox with
        | Some dist -> wires <- (id,dist) :: wires
        | None -> ())
    wires
    |> List.sortBy snd

/// Searches if the position of the cursor is on a wire in a model,
/// where n is 5 pixels adjusted for top level zoom.
/// If there are multiple hits retrn the closest.
let getClickedWire (wModel : Model) (pos : XYPos) (n : float) : ConnectionId Option =
    let boundingBox = {BoundingBox.TopLeft = {X = pos.X - n; Y = pos.Y - n}; H = n*2.; W = n*2.}
    let intersectingWires = getIntersectingWires (wModel : Model) boundingBox
    List.tryHead intersectingWires
    |> Option.map fst

/// Updates the model to have new wires between pasted components
let pasteWires (wModel : Model) (newCompIds : list<ComponentId>) : (Model * list<ConnectionId>) =
    let oldCompIds = getCopiedSymbols wModel.Symbol
    let pastedWires =
        let createNewWire (oldWire : Wire) : list<Wire> =
            let newId = ConnectionId(JSHelpers.uuid())
            let oldPorts = (oldWire.InputPort, oldWire.OutputPort)
            match SymbolUpdate.getEquivalentCopiedPorts wModel.Symbol oldCompIds newCompIds  oldPorts with
            | Some (newInputPort, newOutputPort) ->

                let portOnePos, portTwoPos = 
                    Symbol.getTwoPortLocations wModel.Symbol (InputPortId newInputPort) (OutputPortId newOutputPort)
                let outputPortOrientation = getOutputPortOrientation wModel.Symbol (OutputPortId newOutputPort)
                let segmentList = makeInitialSegmentsList newId portOnePos portTwoPos outputPortOrientation
                [
                    {
                        oldWire with
                            WId = newId;
                            InputPort = InputPortId newInputPort;
                            OutputPort = OutputPortId newOutputPort;
                            Segments = segmentList;
                            StartPos = portOnePos;
                    }
                    |> smartAutoroute wModel
                ]
            | None -> []

        wModel.CopiedWires
        |> Map.toList
        |> List.map snd
        |> List.collect createNewWire
        |> List.map (fun wire -> wire.WId, wire)
        |> Map.ofList

    let newWireMap = Map.fold ( fun acc newKey newVal -> Map.add newKey newVal acc ) pastedWires wModel.Wires
    let pastedConnIds =
        pastedWires
        |> Map.toList
        |> List.map fst

    { wModel with Wires = newWireMap }, pastedConnIds


