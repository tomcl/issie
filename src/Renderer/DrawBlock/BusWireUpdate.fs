module BusWireUpdate

open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers
open SmartWire
open Optics
open Operators
open SmartWire

//---------------------------------------------------------------------------------//
//----------------------Helper functions that need SmartRoute etc------------------//
//---------------------------------------------------------------------------------//
/// Returns a re-routed wire from the given model.
/// First attempts partial autorouting, and defaults to full autorouting if this is not possible.
/// Reverse indicates if the wire should be processed in reverse, 
/// used when an input port (end of wire) is moved.
let updateWire (model : Model) (wire : Wire) (reverse : bool) : Wire =
    let newPort = 
        match reverse with
        | true -> Symbol.getInputPortLocation None model.Symbol wire.InputPort
        | false -> Symbol.getOutputPortLocation None model.Symbol wire.OutputPort
    
    let partial =
        if reverse then
            partialAutoroute model (reverseWire wire) newPort true
            |> Option.map reverseWire
        else 
            partialAutoroute model wire newPort false
    

    let smartRoute = smartAutoroute model wire
    let updateWire' = 
        match smartRoute with
        | WireT wire -> partial |> Option.defaultValue wire
        | ModelT _ -> printfn "updateWire: smartRoute returned ModelT" ; wire
    updateWire'

/// Re-routes the wires in the model based on a list of components that have been altered.
/// If the wire input and output ports are both in the list of moved components, 
/// it does not re-route wire but instead translates it.
/// Keeps manual wires manual (up to a point).
/// Otherwise it will auto-route wires connected to components that have moved
let updateWires (model : Model) (compIdList : ComponentId list) (diff : XYPos) =

    let wires = filterWiresByCompMoved model compIdList

    let newWires =
        model.Wires
        |> Map.toList
        |> List.map (fun (cId, wire) -> 
            if List.contains cId wires.Both //Translate wires that are connected to moving components on both sides
            then (cId, moveWire wire diff)
            elif List.contains cId wires.Inputs //Only route wires connected to ports that moved for efficiency
            then (cId, updateWire model wire true)
            elif List.contains cId wires.Outputs
            then (cId, updateWire model wire false)
            else (cId, wire))
        |> Map.ofList

    { model with Wires = newWires }

let updateSymbolWires (model: Model) (compId: ComponentId) =
    let wires = filterWiresByCompMoved model [compId]
    
    let newWires =
        model.Wires
        |> Map.toList
        |> List.map (fun (cId, wire) ->
            if List.contains cId wires.Both then // Update wires that are connected on both sides
                cId, (
                    updateWire model wire true 
                    |> fun wire -> updateWire model wire false)
            elif List.contains cId wires.Inputs then 
                cId, updateWire model wire true
            elif List.contains cId wires.Outputs then
                cId, updateWire model wire false
            else cId, wire)
        |> Map.ofList
    { model with Wires = newWires }

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
        SelectedSegment = None; 
        LastMousePos = {X = 0.0; Y = 0.0};
        ErrorWires = []
        Notifications = None
        Type = Constants.initialWireType
        ArrowDisplay = Constants.initialArrowDisplay
    } , Cmd.none


            
/// Handles messages
let update (msg : Msg) (model : Model) : Model*Cmd<Msg> =

    match msg with
    | Symbol sMsg ->
        // update Symbol model with a Symbol message
        let sm,sCmd = SymbolUpdate.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd


    | UpdateWires (componentIdList, diff) ->
        // update wires after moving components in componentIdList by diff
        // wires between components are translated not routed as optimisation
        updateWires model componentIdList diff, Cmd.none

    | UpdateSymbolWires compId ->
        // update all the wires coming from a single symbol
        // useful if the symbol has been flipped or ports have been moved
        // partial routing will be done if this makes sense
        updateSymbolWires model compId, Cmd.none

    | AddWire ( (inputId, outputId) : (InputPortId * OutputPortId) ) ->
        // add a newly created wire to the model
        // then send BusWidths message which will re-infer bus widths
        // the new wires (extarcted as connections) are not added back into Issie model. 
        // This happens on save or when starting a simulation (I think)
        let wireId = ConnectionId(JSHelpers.uuid())
        let newWire = 
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
        
        match newWire with
        | WireT wire -> 
            let newModel = updateWireSegmentJumps [wireId] (Optic.set (wireOf_ wire.WId) wire model)
            newModel, Cmd.ofMsg BusWidths
        | ModelT model -> 
            model, Cmd.ofMsg BusWidths
    
    | BusWidths ->
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
                    | MergeWires ->
                        match inPort.PortNumber with
                        | Some 0 ->
                            Map.add symId  {symbol with InWidth0 = Some wire.Width} m
                        | Some 1 ->
                            Map.add symId {symbol with InWidth1 = Some wire.Width} m
                        | x -> failwithf $"What? wire found with input port {x} other than 0 or 1 connecting to MergeWires"
                    | _ -> m

            let newWires = ((Map.empty, model.Wires) ||> Map.fold addWireWidthFolder)

            let symbolsWithWidths =
                (model.Symbol.Symbols, newWires) ||> Map.fold addSymbolWidthFolder

            { model with
                Wires = newWires; 
                Notifications = None;
                ErrorWires=[];
                Symbol = {model.Symbol with Symbols = symbolsWithWidths}
            }, Cmd.none

        let canvasState = (SymbolUpdate.extractComponents model.Symbol, extractConnections model)

        match BusWidthInferer.inferConnectionsWidth canvasState with
        | Ok connWidths ->
            processConWidths connWidths
        | Error e ->
                { model with Notifications = Some e.Msg }, Cmd.ofMsg (ErrorWires e.ConnectionsAffected)

    | CopyWires (connIds : list<ConnectionId>) ->
        // add given wires to Copiedwires state (NB, this contains wires at time of copy)
        let copiedWires = Map.filter (fun connId _ -> List.contains connId connIds) model.Wires
        { model with CopiedWires = copiedWires }, Cmd.none

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

        { model with Wires = newWires ; ErrorWires = connectionIds }, Cmd.none

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

        { model with Wires = newWires }, Cmd.none

    | DeleteWires (connectionIds : list<ConnectionId>) ->
        // deletes wires from model, then runs bus inference
        // Issie model is not affected but will extract connections from wires
        // at some time.
        let newWires =
             model.Wires
             |> Map.filter (fun id wire -> not (List.contains id connectionIds))
        let model =
            {model with Wires = newWires}
        { model with Wires = newWires }, Cmd.ofMsg BusWidths

    | DeleteWiresOnPort (delPorts:(Port option) list) ->
        match delPorts with
        |[] ->
            model, Cmd.none
        |_ -> 
            let wires = model.Wires |> Map.toList
            let connIds = 
                ([],delPorts)
                ||> List.fold (fun conns p ->
                    match p with
                    |Some port ->
                        let localConns = 
                            wires
                            |> List.filter (fun (connId,wire) -> ((wire.InputPort.ToString() = port.Id) || (wire.OutputPort.ToString() = port.Id)))
                            |> List.map fst
                        conns@localConns
                    |None -> conns                    
                )
            model, Cmd.ofMsg (DeleteWires connIds)

    | DragSegment (segId : SegmentId, mMsg: MouseT) ->
        let index, connId = segId
        let wire = model.Wires[connId]
        match mMsg.Op with
        | Down ->
            match List.tryItem index wire.Segments with
            | None -> 
                printfn "Bad segment in DragSegment DOWN... ignoring drag"
                model,Cmd.none
            | Some seg ->
                {model with SelectedSegment = Some segId}, Cmd.ofMsg (ResetJumps [])
        | Drag ->
            match List.tryItem index wire.Segments with
            | None -> 
                printfn "Bad segment in Dragsegment... ignoring drag"
                model,Cmd.none
            | Some seg when index < 1 || index > wire.Segments.Length-2 ->
                printfn "Bad index - can't move that segment"
                model,Cmd.none
            | Some seg ->               
                let (startPos,endPos) = getAbsoluteSegmentPos wire index
                if seg.Draggable then
                    let distanceToMove = 
                        match getSegmentOrientation startPos endPos with
                        | Horizontal -> mMsg.Pos.Y - startPos.Y
                        | Vertical -> mMsg.Pos.X - startPos.X

                    let newWire = moveSegment model seg distanceToMove 
                    let newWires = Map.add seg.WireId newWire model.Wires

                    { model with Wires = newWires }, Cmd.none
                else
                    printfn "Can't move undraggable"
                    model, Cmd.none

        | _ -> model, Cmd.none

    | CoalesceWire wId ->
        coalesceInWire wId model, Cmd.none

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
        { model with Wires = newWires }, Cmd.none

    | ResetJumps connIds ->
        // removes wire 'jumps' at start of drag operation for neater component movement 
        // without jump recalculation
        // makejumps at end of a drag operation restores new jumps
        printfn $"Resetting jumps with {connIds.Length} connections"
        let newModel = resetWireSegmentJumps connIds model
        newModel, Cmd.none

    | MakeJumps connIds ->
        // recalculates (slowly) wire jumps after a drag operation
        printfn $"Making jumps with {connIds.Length} connections"
        let newModel = updateWireSegmentJumps connIds model
        newModel, Cmd.none

    | ResetModel -> 
        // How we start with nothing loaded
        { model with Wires = Map.empty; ErrorWires = []; Notifications = None }, Cmd.none

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
                let epsilon = 0.00001
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
                        Symbol.getOutputPortOrientation model.Symbol outputId 
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

        { model with Wires = newWires }, Cmd.ofMsg (MakeJumps connIds)

    | UpdateWireDisplayType (style: WireType) ->
        {model with Type = style }
        |> updateWireSegmentJumps []
        |> (fun model -> model,Cmd.none)

    | ToggleArrowDisplay  ->
        {model with ArrowDisplay = not model.ArrowDisplay}, Cmd.none

    | UpdateConnectedWires (componentIds: ComponentId list) ->
        // partial or full autoroutes all ends of wires conencted to given symbols
        // typically used after rotating or flipping symbols
        let updatePortIdMessages = 
            componentIds
            |> Symbol.getPortLocations model.Symbol
            |> (fun (m1,m2) -> 
                let inputPorts = Seq.map (fun (InputPortId portId) -> portId) m1.Keys |> Seq.toList
                let outputPorts = Seq.map (fun (OutputPortId portId) -> portId) m2.Keys |> Seq.toList
                inputPorts @ outputPorts
                |> List.map (Msg.RerouteWire >> Cmd.ofMsg))
        model, Cmd.batch updatePortIdMessages

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

        {model with Wires = newWires}, Cmd.none


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
let getIntersectingWires (wModel : Model) (selectBox : BoundingBox) : list<ConnectionId*float> =
    wModel.Wires
    |> Map.map (fun _id wire -> wireIntersectsBoundingBox wire selectBox)
    |> Map.filter (fun _id optDist -> optDist <> None)
    |> Map.toList
    |> List.collect (function | (id, Some dist) -> [(id,dist)] | _,None -> [])
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
    let oldCompIds = Symbol.getCopiedSymbols wModel.Symbol

    let pastedWires =
        let createNewWire (oldWire : Wire) : list<Wire> =
            let newId = ConnectionId(JSHelpers.uuid())
            let oldPorts = (oldWire.InputPort, oldWire.OutputPort)
            match SymbolUpdate.getEquivalentCopiedPorts wModel.Symbol oldCompIds newCompIds  oldPorts with
            | Some (newInputPort, newOutputPort) ->

                let portOnePos, portTwoPos = 
                    Symbol.getTwoPortLocations wModel.Symbol (InputPortId newInputPort) (OutputPortId newOutputPort)
                let outputPortOrientation = Symbol.getOutputPortOrientation wModel.Symbol (OutputPortId newOutputPort)
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
                    |>  smartAutoroute wModel
                        |> function
                            | WireT wire -> wire
                            | ModelT _ -> failwith "Expected a WireT value."

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


