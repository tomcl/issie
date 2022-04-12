module SheetUpdate

open CommonTypes
open Browser
open Elmish
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open Sheet

/// Update function to move symbols in model.SelectedComponents
let moveSymbols (model: Model) (mMsg: MouseT) =
    let nextAction, isDragAndDrop =
        match model.Action with
        | DragAndDrop -> DragAndDrop, true
        | _ -> MovingSymbols, false

    match model.SelectedComponents with
    | [symId] -> // Attempt Snap-to-Grid if there is only one moving component
        let symbol = 
            match Map.tryFind symId model.Wire.Symbol.Symbols with
            | Some symbol -> symbol
            | None ->
                failwithf "What? can't move a symbol which does not exist in the model!"

        let compId = model.SelectedComponents.Head
        let bBox = model.BoundingBoxes[compId]
        let snapXY, moveDelta = snap2DSymbol model.AutomaticScrolling mMsg.Pos symbol model

        let errorComponents  =
            if notIntersectingComponents model bBox compId then [] else [compId]

        {model with
             Action = nextAction
             SnapSymbols = snapXY
             LastMousePos = mMsg.Pos
             ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.Movement}
             ErrorComponents = errorComponents},
        Cmd.batch [ symbolCmd (MoveSymbols (model.SelectedComponents, moveDelta))
                    Cmd.ofMsg (UpdateSingleBoundingBox model.SelectedComponents.Head)
                    Cmd.ofMsg (UpdateSingleLabelBoundingBox model.SelectedComponents.Head)
                    symbolCmd (ErrorSymbols (errorComponents,model.SelectedComponents,isDragAndDrop))
                    Cmd.ofMsg CheckAutomaticScrolling
                    wireCmd (BusWireT.UpdateWires (model.SelectedComponents, moveDelta))]
    | _ -> // Moving multiple symbols -> don't do snap-to-grid
        let errorComponents =
            model.SelectedComponents
            |> List.filter (fun sId -> not (notIntersectingComponents model model.BoundingBoxes[sId] sId))
        {model with Action = nextAction ; 
                    LastMousePos = mMsg.Pos; 
                    ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.Movement}; 
                    ErrorComponents = errorComponents },
        Cmd.batch [ symbolCmd (SymbolT.MoveSymbols (model.SelectedComponents, mMsg.Pos - model.LastMousePos))
                    symbolCmd (SymbolT.ErrorSymbols (errorComponents,model.SelectedComponents,isDragAndDrop))
                    Cmd.ofMsg UpdateBoundingBoxes
                    Cmd.ofMsg UpdateLabelBoundingBoxes
                    Cmd.ofMsg CheckAutomaticScrolling
                    wireCmd (BusWireT.UpdateWires (model.SelectedComponents, mMsg.Pos - model.LastMousePos))]

/// Inputs are segment ID being dragged and new mouse position.
/// Performs the Segment Drag operation implementing snaps.
/// This function must be in update and creates additional commands
/// to implement the drag oeporation.
let snapWire 
        (model: Model) 
        (mMsg: MouseT) 
        (segId: SegmentId)
            : Model * Cmd<Msg> = 
    
    let nextAction, isMovingWire =
        match model.Action with
        | MovingWire segId -> MovingWire segId, true
        | _ -> Idle, false

    let index,connId = segId
    let aSegment = BusWire.getASegmentFromId  model.Wire segId
    let snapXY, delta = snap2DSegment model.AutomaticScrolling mMsg.Pos aSegment model
    let newPos = aSegment.Start + delta
    let newmMsg = {mMsg with Pos = newPos} 
                                
    { model with
        Action = nextAction;
        LastMousePos = mMsg.Pos;
        ScrollingLastMousePos = {Pos = mMsg.Pos; Move = mMsg.Movement};
        ErrorComponents = [];
        SnapSegments = snapXY
    },
    Cmd.batch [ wireCmd (BusWireT.DragSegment (segId, newmMsg));
                Cmd.ofMsg CheckAutomaticScrolling] 


// ----------------------------------------- Mouse Update Helper Functions ----------------------------------------- //
// (Kept in separate functions since Update function got too long otherwise)

let appendUndoList (undoList: Model List) (model_in: Model): Model List =
    let rec removeLast lst =
        match lst with
        | _ :: lst when List.isEmpty lst -> []
        | hd :: lst -> hd :: (removeLast lst)
        | [] -> []

    match List.length undoList with
    | n when n < 500 -> model_in :: undoList
    | _ -> model_in :: (removeLast undoList)


/// Mouse Down Update, Can have clicked on: Label, InputPort / OutputPort / Component / Wire / Canvas. Do correct action for each.
let mDownUpdate 
        (model: Model) 
        (mMsg: MouseT) 
            : Model * Cmd<Msg> =
    let newModel =
        match model.TmpModel with
        | None -> model
        | Some newModel -> newModel

    match model.Action with
    | DragAndDrop ->
        let errorComponents =
            model.SelectedComponents
            |> List.filter (fun sId -> not (notIntersectingComponents model model.BoundingBoxes[sId] sId))

        match List.isEmpty errorComponents with
        | false -> model, Cmd.none
        | true ->
            {model with
                BoundingBoxes = Symbol.getBoundingBoxes model.Wire.Symbol // TODO: Improve here in group stage when we are concerned with efficiency
                Action = Idle
                SnapSymbols = emptySnap
                SnapSegments = emptySnap
                UndoList = appendUndoList model.UndoList newModel
                RedoList = []
                AutomaticScrolling = false
            },
            Cmd.batch [ symbolCmd (SymbolT.SelectSymbols model.SelectedComponents)
                        wireCmd (BusWireT.SelectWires model.SelectedWires)
                        wireCmd (BusWireT.ResetJumps [])]
    | _ ->
        match (mouseOn model mMsg.Pos) with
        | Label compId ->
            {model with Action = InitialiseMovingLabel compId},
            Cmd.ofMsg (SheetT.Msg.Wire (BusWireT.Msg.Symbol (SelectSymbols [compId])))

        | InputPort (portId, portLoc) ->
            if not model.Toggle then
                {model with Action = ConnectingInput portId; ConnectPortsLine = portLoc, mMsg.Pos; TmpModel=Some model},
                symbolCmd SymbolT.ShowAllOutputPorts
            else
                let  portIdstr = match portId with | InputPortId x -> x
                {model with Action = MovingPort portIdstr}
                , symbolCmd (SymbolT.MovePort (portIdstr, mMsg.Pos))

        | OutputPort (portId, portLoc) ->
            if not model.Toggle then
                {model with Action = ConnectingOutput portId; ConnectPortsLine = portLoc, mMsg.Pos; TmpModel=Some model},
                symbolCmd SymbolT.ShowAllInputPorts
            else
                let  portIdstr = match portId with | OutputPortId x -> x
                {model with Action = MovingPort portIdstr}
                , symbolCmd (SymbolT.MovePort (portIdstr, mMsg.Pos))

        | Component compId ->

            let msg, action =
                if model.IsWaveSim then
                    ToggleNet ([SymbolUpdate.extractComponent model.Wire.Symbol compId], []), Idle
                else DoNothing, InitialiseMoving compId

            if model.Toggle || mMsg.ShiftKeyDown
            then
                printfn "Toggling component"
                let newComponents =
                    if List.contains compId model.SelectedComponents
                    then List.filter (fun cId -> cId <> compId) model.SelectedComponents // If component selected was already in the list, remove it
                    else compId :: model.SelectedComponents // If user clicked on a new component add it to the selected list
                {model with 
                    SelectedComponents = newComponents; 
                    SnapSymbols = emptySnap
                    LastValidPos = mMsg.Pos ; 
                    LastValidBoundingBoxes=model.BoundingBoxes ; 
                    Action = action; LastMousePos = mMsg.Pos; 
                    TmpModel = Some model; 
                    PrevWireSelection = model.SelectedWires},
                Cmd.batch [symbolCmd (SymbolT.SelectSymbols newComponents); Cmd.ofMsg msg]
            else
                let newComponents, newWires =
                    if List.contains compId model.SelectedComponents
                    then model.SelectedComponents, model.SelectedWires // Keep selection for symbol movement
                    else [compId], [] // If user clicked on a new component, select that one instead
                let snapXY =
                    match newComponents with
                    | [compId] -> 
                        getNewSymbolSnapInfo model model.Wire.Symbol.Symbols[compId]
                    | _ -> emptySnap

                {model with 
                    SelectedComponents = newComponents; 
                    SnapSymbols = snapXY
                    LastValidPos = mMsg.Pos ; 
                    LastValidBoundingBoxes=model.BoundingBoxes ; 
                    SelectedWires = newWires; Action = action; 
                    LastMousePos = mMsg.Pos; TmpModel = Some model},
                Cmd.batch [ symbolCmd (SymbolT.SelectSymbols newComponents)
                            wireCmd (BusWireT.SelectWires newWires)
                            Cmd.ofMsg msg]

        | Connection connId ->
            let aSeg = BusWireUpdate.getClickedSegment model.Wire connId mMsg.Pos
            let (i,wId) = aSeg.Segment.getId()
            let segments = model.Wire.Wires[wId].Segments
            if i > segments.Length - 1 then
                failwithf "What? Error in getClcikedSegment: "
            let msg =
                if model.IsWaveSim then
                    ToggleNet ([], [BusWire.extractConnection model.Wire connId])
                else DoNothing

            if model.Toggle
            then
                let newWires =
                    if List.contains connId model.SelectedWires
                    then List.filter (fun cId -> cId <> connId) model.SelectedWires // If component selected was already in the list, remove it
                    else connId :: model.SelectedWires // If user clicked on a new component add it to the selected list

                { model with SelectedWires = newWires; Action = Idle; TmpModel = Some model; PrevWireSelection = model.SelectedWires},
                Cmd.batch [wireCmd (BusWireT.SelectWires newWires); Cmd.ofMsg msg]
            else
                let snapXY = getNewSegmentSnapInfo model aSeg
                { model with 
                    SelectedComponents = []; 
                    SelectedWires = [ connId ]; 
                    SnapSegments = snapXY
                    Action = MovingWire (aSeg.Segment.getId()); 
                    TmpModel = Some model},
                Cmd.batch [ symbolCmd (SymbolT.SelectSymbols [])
                            wireCmd (BusWireT.SelectWires [ connId ])
                            wireCmd (BusWireT.DragSegment (aSeg.Segment.getId(), mMsg))
                            wireCmd (BusWireT.ResetJumps [ connId ] )
                            Cmd.ofMsg msg]
        | Canvas ->
            let newComponents, newWires =
                if model.Toggle
                then model.SelectedComponents, model.SelectedWires //do not deselect if in toggle mode
                else [], []
            // Start Creating Selection Box and Reset Selected Components
            let initialiseSelection = 
                {model.DragToSelectBox with TopLeft= {X=mMsg.Pos.X; Y=mMsg.Pos.Y}}
            {model with DragToSelectBox = initialiseSelection; Action = Selecting; SelectedComponents = newComponents; SelectedWires = newWires },
            Cmd.batch [ symbolCmd (SymbolT.SelectSymbols newComponents)
                        wireCmd (BusWireT.SelectWires newWires) ]

/// Mouse Drag Update, can be: drag-to-selecting, moving symbols, connecting wire between ports.
let mDragUpdate 
        (model: Model) 
        (mMsg: MouseT) 
            : Model * Cmd<Msg> =
    let setDragCursor (model:Model, cmd: Cmd<Msg>) : Model*Cmd<Msg> =
        let dragCursor = 
            match model.Action with
            | MovingLabel _ -> Grabbing
            | MovingSymbols _ -> Grabbing
            | _ -> model.CursorType
        {model with CursorType = dragCursor}, cmd
    match model.Action with
    | MovingWire segId -> 
        snapWire model mMsg segId 
    | Selecting ->
        let initialX = model.DragToSelectBox.TopLeft.X
        let initialY = model.DragToSelectBox.TopLeft.Y
        let newDragToSelectBox = {model.DragToSelectBox with W = (mMsg.Pos.X - initialX); H = (mMsg.Pos.Y - initialY)}
        {model with DragToSelectBox = newDragToSelectBox
                    ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.Movement}
                    LastMousePos = mMsg.Pos
         }, Cmd.ofMsg CheckAutomaticScrolling
    | InitialiseMovingLabel compId->
        {model with
                Action = MovingLabel
                LastMousePos = mMsg.Pos
                ScrollingLastMousePos = {Pos = mMsg.Pos; Move = mMsg.Movement}
                SelectedLabel = Some compId
            }, Cmd.ofMsg DoNothing
    | InitialiseMoving _ ->
        let movingWires = BusWireUpdate.getConnectedWireIds model.Wire model.SelectedComponents
        let newModel, cmd = moveSymbols model mMsg
        newModel, Cmd.batch [ cmd ]
    | MovingSymbols | DragAndDrop ->
        moveSymbols model mMsg
    | MovingLabel ->
        let movingCompId =
            match model.SelectedLabel with
            | Some compid -> compid
            | None -> 
                failwithf "What? no component found for moving label operation"
        {model with
            Action = MovingLabel
            LastMousePos = mMsg.Pos
            ScrollingLastMousePos = {Pos = mMsg.Pos; Move = mMsg.Movement}
        },
        Cmd.batch [ symbolCmd (SymbolT.MoveLabel (movingCompId, mMsg.Pos - model.LastMousePos))
                    Cmd.ofMsg UpdateLabelBoundingBoxes ]

    | ConnectingInput _ ->
        let nearbyComponents = findNearbyComponents model mMsg.Pos 50 
        let _, nearbyOutputPorts = findNearbyPorts model

        let targetPort, drawLineTarget =
            match mouseOnPort nearbyOutputPorts mMsg.Pos 12.5 with
            | Some (OutputPortId portId, portLoc) -> (portId, portLoc) // If found target, snap target of the line to the port
            | None -> ("", mMsg.Pos)

        { model with
                    NearbyComponents = nearbyComponents
                    ConnectPortsLine = (fst model.ConnectPortsLine, drawLineTarget)
                    TargetPortId = targetPort
                    LastMousePos = mMsg.Pos
                    ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.Movement}}
        , Cmd.ofMsg CheckAutomaticScrolling
    | ConnectingOutput _ ->
        let nearbyComponents = findNearbyComponents model mMsg.Pos 50
        let nearbyInputPorts, _ = findNearbyPorts model

        let targetPort, drawLineTarget =
            match mouseOnPort nearbyInputPorts mMsg.Pos 12.5 with
            | Some (InputPortId portId, portLoc) -> (portId, portLoc) // If found target, snap target of the line to the port
            | None -> ("", mMsg.Pos)

        { model with
                    NearbyComponents = nearbyComponents
                    ConnectPortsLine = (fst model.ConnectPortsLine, drawLineTarget)
                    TargetPortId = targetPort
                    LastMousePos = mMsg.Pos
                    ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.Movement} }
        , Cmd.ofMsg CheckAutomaticScrolling

    | MovingPort portId->
        model, symbolCmd (SymbolT.MovePort (portId, mMsg.Pos))
    | _ -> model, Cmd.none
    |> setDragCursor
/// Mouse Up Update, can have: finished drag-to-select, pressed on a component, finished symbol movement, connected a wire between ports
let mUpUpdate (model: Model) (mMsg: MouseT) : Model * Cmd<Msg> = // mMsg is currently un-used, but kept for future possibilities
    let newModel =
        match model.TmpModel with
        | None -> model
        | Some newModel -> {newModel with SelectedComponents = model.SelectedComponents}
    match model.Action with
    | MovingWire segId ->
        let _, connId = segId
        { model with Action = Idle ; UndoList = appendUndoList model.UndoList newModel; RedoList = [] },
        Cmd.batch [ wireCmd (BusWireT.DragSegment (segId, mMsg))
                    wireCmd (BusWireT.CoalesceWire connId)
                    wireCmd (BusWireT.MakeJumps [ connId ] ) ]
    | Selecting ->
        let newComponents = findIntersectingComponents model model.DragToSelectBox
        let newWires = 
            BusWireUpdate.getIntersectingWires model.Wire model.DragToSelectBox
            |> List.map fst
        let resetDragToSelectBox = {model.DragToSelectBox with H = 0.0; W = 0.0}
        let selectComps, selectWires =
            if mMsg.ShiftKeyDown then
                model.SelectedComponents, model.SelectedWires
            elif model.Toggle then
                symDiff newComponents model.SelectedComponents, symDiff newWires model.SelectedWires
            else 
                newComponents, newWires
        { model with 
            DragToSelectBox = resetDragToSelectBox; 
            Action = Idle; SelectedComponents = selectComps; 
            SelectedWires = selectWires; 
            AutomaticScrolling = false },
        Cmd.batch [ symbolCmd (SymbolT.SelectSymbols selectComps)
                    wireCmd (BusWireT.SelectWires selectWires) ]

    | InitialiseMoving compId -> 
            // not sure there is any point to running this from mouse UP this now we are not altering selection?
            // legacy case due for removal?
            { model with Action = Idle}, wireCmd (BusWireT.SelectWires [])

    | InitialiseMovingLabel compId ->
        { model with Action = Idle; SelectedLabel = Some compId },
        Cmd.ofMsg DoNothing

    | MovingLabel ->
        {model with Action = Idle}, Cmd.ofMsg DoNothing

    | MovingSymbols ->
        // Reset Movement State in Model
        match model.ErrorComponents with
        | [] ->
            let movingWires = BusWireUpdate.getConnectedWireIds model.Wire model.SelectedComponents
            {model with
                // BoundingBoxes = Symbol.getBoundingBoxes model.Wire.Symbol
                Action = Idle
                SnapSymbols = emptySnap
                SnapSegments = emptySnap
                UndoList = appendUndoList model.UndoList newModel
                RedoList = []
                AutomaticScrolling = false },
            wireCmd (BusWireT.MakeJumps movingWires)
        | _ ->
            let movingWires = BusWireUpdate.getConnectedWireIds model.Wire model.SelectedComponents
            {model with
                BoundingBoxes = model.LastValidBoundingBoxes
                Action = Idle
                SnapSymbols = emptySnap
                SnapSegments = emptySnap
                AutomaticScrolling = false },
            Cmd.batch [ symbolCmd (SymbolT.MoveSymbols (model.SelectedComponents, (model.LastValidPos - mMsg.Pos)))
                        Cmd.ofMsg UpdateBoundingBoxes
                        Cmd.ofMsg UpdateLabelBoundingBoxes
                        symbolCmd (SymbolT.SelectSymbols (model.SelectedComponents))
                        wireCmd (BusWireT.UpdateWires (model.SelectedComponents, model.LastValidPos - mMsg.Pos))
                        wireCmd (BusWireT.MakeJumps movingWires) ]
    | ConnectingInput inputPortId ->
        let cmd, undoList ,redoList =
            if model.TargetPortId <> "" // If a target has been found, connect a wire
            then wireCmd (BusWireT.AddWire (inputPortId, (OutputPortId model.TargetPortId))),
                           appendUndoList model.UndoList newModel, []
            else Cmd.none , model.UndoList , model.RedoList
        {model with Action = Idle; TargetPortId = ""; UndoList = undoList ; RedoList = redoList ; AutomaticScrolling = false }, cmd
    | ConnectingOutput outputPortId ->
        let cmd , undoList , redoList =
            if model.TargetPortId <> "" // If a target has been found, connect a wire
            then  wireCmd (BusWireT.AddWire (InputPortId model.TargetPortId, outputPortId)),
                           appendUndoList model.UndoList newModel , []
            else Cmd.none , model.UndoList , model.RedoList
        { model with Action = Idle; TargetPortId = ""; UndoList = undoList ; RedoList = redoList ; AutomaticScrolling = false  }, cmd
    | MovingPort portId ->
        let symbol = Symbol.getCompId model.Wire.Symbol portId
        {model with Action = Idle},
        Cmd.batch [
            symbolCmd (SymbolT.MovePortDone (portId, mMsg.Pos))
            wireCmd (BusWireT.UpdateSymbolWires symbol);
            wireCmd (BusWireT.RerouteWire portId)]
    | _ -> model, Cmd.none

/// Mouse Move Update, looks for nearby components and looks if mouse is on a port
let mMoveUpdate 
        (model: Model) 
        (mMsg: MouseT) 
            : Model * Cmd<Msg> =
    match model.Action with
    | DragAndDrop -> moveSymbols model mMsg
    | InitialisedCreateComponent (ldcs, compType, lbl) ->
        let labelTest = if lbl = "" then SymbolUpdate.generateLabel model.Wire.Symbol compType else lbl
        let newSymbolModel, newCompId = SymbolUpdate.addSymbol ldcs model.Wire.Symbol mMsg.Pos compType labelTest

        { model with Wire = { model.Wire with Symbol = newSymbolModel }
                     Action = DragAndDrop
                     SelectedComponents = [ newCompId ]
                     SelectedWires = []
                     LastMousePos = mMsg.Pos
                     ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.Movement} },
        Cmd.batch [ Cmd.ofMsg UpdateBoundingBoxes
                    Cmd.ofMsg UpdateLabelBoundingBoxes
                    symbolCmd (SymbolT.SelectSymbols [])
                    symbolCmd (SymbolT.PasteSymbols [ newCompId ]) ]
    | _ ->
        let nearbyComponents = findNearbyComponents model mMsg.Pos 50 // TODO Group Stage: Make this more efficient, update less often etc, make a counter?

        let newCursor =
            match model.CursorType, model.Action with
            | Spinner,_ -> Spinner
            | _ ->
                match mouseOn { model with NearbyComponents = nearbyComponents } mMsg.Pos with // model.NearbyComponents can be outdated e.g. if symbols have been deleted -> send with updated nearbyComponents.
                | InputPort _ | OutputPort _ -> ClickablePort // Change cursor if on port
                | Label _ -> GrabLabel
                | Connection _ -> GrabWire
                | Component _ -> GrabSymbol
                | _ -> Default

        { model with 
            NearbyComponents = nearbyComponents; 
            CursorType = newCursor; 
            LastMousePos = mMsg.Pos; 
            ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.Movement} },
        symbolCmd (SymbolT.ShowPorts nearbyComponents) // Show Ports of nearbyComponents

let getScreenCentre (model : Model) : XYPos =
    let canvas = document.getElementById "Canvas"
    {
        X = (canvas.scrollLeft + canvas.clientWidth / 2.0) / model.Zoom
        Y = (canvas.scrollTop + canvas.clientHeight / 2.0) / model.Zoom
    }

/// Update Function
let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Wire (BusWireT.Symbol SymbolT.Msg.UpdateBoundingBoxes) -> 
        // Symbol cannot directly send a message to Sheet box Sheet message type is out of scape. This
        // is used so that a symbol message can be intercepted by sheet and used there.
        printfn "UpdateBoundingBoxes from Symbol!!!"
        model, Cmd.batch [
                Cmd.ofMsg UpdateBoundingBoxes; 
                Cmd.ofMsg UpdateLabelBoundingBoxes
                ]
    | Wire wMsg ->
        let wModel, wCmd = BusWireUpdate.update wMsg model.Wire
        { model with Wire = wModel }, Cmd.map Wire wCmd
    | ToggleGrid ->
        {model with ShowGrid = not model.ShowGrid}, Cmd.none
    | KeyPress DEL ->
        let wiresConnectedToComponents = BusWireUpdate.getConnectedWireIds model.Wire model.SelectedComponents
        // Ensure there are no duplicate deletions by using a Set
        let wireUnion =
            Set.union (Set.ofList wiresConnectedToComponents) (Set.ofList model.SelectedWires)
            |> Set.toList

        // let inputPorts, outputPorts = BusWire.getPortIdsOfWires model.Wire wireUnion
        { model with SelectedComponents = []; SelectedWires = []; UndoList = appendUndoList model.UndoList model ; RedoList = [] },
        Cmd.batch [ wireCmd (BusWireT.DeleteWires wireUnion) // Delete Wires before components so nothing bad happens
                    symbolCmd (SymbolT.DeleteSymbols model.SelectedComponents)
                    Cmd.ofMsg UpdateBoundingBoxes
                    Cmd.ofMsg UpdateLabelBoundingBoxes]
    | KeyPress CtrlS -> // For Demo, Add a new square in upper left corner
        printfn "saving symbols"
        { model with BoundingBoxes = Symbol.getBoundingBoxes model.Wire.Symbol; UndoList = appendUndoList model.UndoList model ; RedoList = []},
        Cmd.batch [ Cmd.ofMsg UpdateBoundingBoxes; symbolCmd SymbolT.SaveSymbols ] // Need to update bounding boxes after adding a symbol.
    | KeyPress AltShiftZ ->
        TimeHelpers.printStats()
        model, Cmd.none
    | KeyPress CtrlC ->
        model,
        Cmd.batch [
            symbolCmd (SymbolT.CopySymbols model.SelectedComponents) // Better to have Symbol keep track of clipboard as symbols can get deleted before pasting.
            wireCmd (BusWireT.CopyWires model.SelectedWires)
        ]
    | KeyPress CtrlV ->
        let newSymbolModel, pastedCompIds = SymbolUpdate.pasteSymbols model.Wire.Symbol model.LastMousePos // Symbol has Copied Symbols stored
        let newBusWireModel, pastedConnIds = BusWireUpdate.pasteWires { model.Wire with Symbol = newSymbolModel } pastedCompIds

        { model with Wire = newBusWireModel
                     SelectedComponents = pastedCompIds
                     SelectedWires = pastedConnIds
                     TmpModel = Some model
                     Action = DragAndDrop },
        Cmd.batch [ Cmd.ofMsg UpdateBoundingBoxes
                    Cmd.ofMsg UpdateLabelBoundingBoxes
                    symbolCmd (SymbolT.SelectSymbols []) // Select to unhighlight all other symbols
                    symbolCmd (SymbolT.PasteSymbols pastedCompIds)
                    wireCmd (BusWireT.SelectWires [])
                    wireCmd (BusWireT.ColorWires (pastedConnIds, HighLightColor.Thistle)) ]
    | KeyPress ESC -> // Cancel Pasting Symbols, and other possible actions in the future
        match model.Action with
        | DragAndDrop ->
            { model with SelectedComponents = []
                         SelectedWires = []
                         SnapSymbols = emptySnap
                         SnapSegments = emptySnap
                         Action = Idle },
            Cmd.batch [ symbolCmd (SymbolT.DeleteSymbols model.SelectedComponents)
                        wireCmd (BusWireT.DeleteWires model.SelectedWires)
                        Cmd.ofMsg UpdateBoundingBoxes
                        Cmd.ofMsg UpdateLabelBoundingBoxes]
        | _ -> model, Cmd.none
    | KeyPress CtrlZ ->
        match model.UndoList with
        | [] -> model , Cmd.none
        | prevModel :: lst ->
            let symModel = { prevModel.Wire.Symbol with CopiedSymbols = model.Wire.Symbol.CopiedSymbols }
            let wireModel = { prevModel.Wire with CopiedWires = model.Wire.CopiedWires ; Symbol = symModel}
            { prevModel with Wire = wireModel ; UndoList = lst ; RedoList = model :: model.RedoList ; CurrentKeyPresses = Set.empty } , Cmd.none
    | KeyPress CtrlY ->
        match model.RedoList with
        | [] -> model , Cmd.none
        | newModel :: lst -> { newModel with UndoList = model :: model.UndoList ; RedoList = lst} , Cmd.none
    | KeyPress CtrlA ->
        let symbols = model.Wire.Symbol.Symbols |> Map.toList |> List.map fst
        let wires = model.Wire.Wires |> Map.toList |> List.map fst
        { model with
            SelectedComponents = symbols
            SelectedWires = wires
        } , Cmd.batch [ symbolCmd (SymbolT.SelectSymbols symbols)
                        wireCmd (BusWireT.SelectWires wires) ]
    | KeyPress CtrlW ->
        match canvasDiv with
        | None -> model, Cmd.none
        | Some el ->
            let paras = fitCircuitToWindowParas model
            el.scrollTop <- paras.ScrollY
            el.scrollLeft <- paras.ScrollX
            { model with Zoom = paras.MagToUse}, Cmd.ofMsg (UpdateScrollPos (el.scrollLeft, el.scrollTop))
    | ToggleSelectionOpen ->
        //if List.isEmpty model.SelectedComponents && List.isEmpty model.SelectedWires then
        //    model, Cmd.none
        //else
            {model with Toggle = true}, Cmd.none
    | ToggleSelectionClose ->
        {model with Toggle = false}, Cmd.none

    | MouseMsg mMsg -> // Mouse Update Functions can be found above, update function got very messy otherwise
        match mMsg.Op with
        | Down -> mDownUpdate model mMsg
        | Drag -> mDragUpdate model mMsg
        | Up -> mUpUpdate model mMsg
        | Move -> mMoveUpdate model mMsg
    | UpdateBoundingBoxes -> 
        { model with BoundingBoxes = Symbol.getBoundingBoxes model.Wire.Symbol }, Cmd.none
    | UpdateSingleBoundingBox compId ->
        match Map.containsKey compId model.BoundingBoxes with
        | true -> {model with BoundingBoxes = model.BoundingBoxes.Add (compId, (Symbol.getBoundingBox model.Wire.Symbol compId))}, Cmd.none
        | false -> model, Cmd.none
    | UpdateLabelBoundingBoxes -> 
        { model with LabelBoundingBoxes = Symbol.getLabelBoundingBoxes model.Wire.Symbol }, Cmd.none
    | UpdateSingleLabelBoundingBox compId ->
        match Map.containsKey compId model.LabelBoundingBoxes with
        | true -> {model with LabelBoundingBoxes = model.LabelBoundingBoxes.Add (compId, (Symbol.getLabelBoundingBox model.Wire.Symbol compId))}, Cmd.none
        | false -> model, Cmd.none
 
    | UpdateScrollPos (scrollX, scrollY) ->
        let scrollDif = { X = scrollX; Y = scrollY } - model.ScrollPos
        let newLastScrollingPos =
            {
             Pos =
                {
                    X = model.ScrollingLastMousePos.Pos.X + scrollDif.X / model.Zoom
                    Y = model.ScrollingLastMousePos.Pos.Y + scrollDif.Y / model.Zoom
                }
             Move = model.ScrollingLastMousePos.Move
            }
        let cmd =
            if model.AutomaticScrolling then
                Cmd.ofMsg CheckAutomaticScrolling // Also check if there is automatic scrolling to continue
            else
                Cmd.none
        { model with ScrollPos = { X = scrollX; Y = scrollY }; ScrollingLastMousePos = newLastScrollingPos }, cmd
    | KeyPress ZoomIn ->
        { model with Zoom = model.Zoom + 0.05 }, Cmd.ofMsg (KeepZoomCentered model.LastMousePos)
    | KeyPress ZoomOut ->
        let leftScreenEdge, rightScreenEdge,_,_ = getScreenEdgeCoords()
        //Check if the new zoom will exceed the canvas width
        let newZoom =
            if rightScreenEdge - leftScreenEdge < (DrawHelpers.canvasUnscaledSize * (model.Zoom - 0.05)) then model.Zoom - 0.05
            else model.Zoom

        { model with Zoom = newZoom }, Cmd.ofMsg (KeepZoomCentered model.LastMousePos)
    | KeepZoomCentered oldScreenCentre ->
        let canvas = document.getElementById "Canvas"
        let newScreenCentre = getScreenCentre model
        let requiredOffset = oldScreenCentre - newScreenCentre

        // Update screen so that the zoom is centred around the middle of the screen.
        canvas.scrollLeft <- canvas.scrollLeft + requiredOffset.X * model.Zoom
        canvas.scrollTop <- canvas.scrollTop + requiredOffset.Y * model.Zoom

        model, Cmd.none
    | ManualKeyDown key -> // Needed for e.g. Ctrl + C and Ctrl + V as they are not picked up by Electron
        let newPressedKeys = model.CurrentKeyPresses.Add (key.ToUpper()) // Make it fully upper case to remove CAPS dependency
        let newCmd =
            match Set.contains "CONTROL" newPressedKeys || Set.contains "META" newPressedKeys with
            | true ->
                if Set.contains "C" newPressedKeys then
                    Cmd.ofMsg (KeyPress CtrlC)
                elif Set.contains "V" newPressedKeys then
                    Cmd.ofMsg (KeyPress CtrlV)
                elif Set.contains "A" newPressedKeys then
                    Cmd.ofMsg (KeyPress CtrlA)
                elif Set.contains "W" newPressedKeys then
                    Cmd.ofMsg (KeyPress CtrlW)
                else
                    Cmd.none
            | false -> Cmd.none

        { model with CurrentKeyPresses = newPressedKeys }, newCmd
    | ManualKeyUp key -> { model with CurrentKeyPresses = model.CurrentKeyPresses.Remove (key.ToUpper()) }, Cmd.none
    | CheckAutomaticScrolling ->
        let canvas = document.getElementById "Canvas"
        let wholeApp = document.getElementById "WholeApp"
        let rightSelection = document.getElementById "RightSelection"

        let leftScreenEdge = canvas.scrollLeft
        let rightScreenEdge = leftScreenEdge + wholeApp.clientWidth - rightSelection.clientWidth
        let upperScreenEdge = canvas.scrollTop
        let lowerScreenEdge = upperScreenEdge + canvas.clientHeight
        let mPosX = model.ScrollingLastMousePos.Pos.X * model.Zoom // Un-compensate for zoom as we want raw distance from mouse to edge screen
        let mPosY = model.ScrollingLastMousePos.Pos.Y * model.Zoom // Un-compensate for zoom as we want raw distance from mouse to edge screen
        let mMovX = model.ScrollingLastMousePos.Move.X
        let mMovY = model.ScrollingLastMousePos.Move.Y
        /// no scrolling if too far from edge, or if moving away from edge
        let checkForAutomaticScrolling1D (edge: float) (mPos: float) (mMov: float) =
            let scrollMargin = 100.0
            let scrollSpeed = 10.0
            let edgeDistance = abs (edge - mPos)

            if edgeDistance < scrollMargin && mMov >= -0.0000001 // just in case there are FP rounding errors
            then scrollSpeed * (scrollMargin - edgeDistance) / scrollMargin // Speed should be faster the closer the mouse is to the screen edge
            else 0.0

        canvas.scrollLeft <- canvas.scrollLeft - (checkForAutomaticScrolling1D leftScreenEdge mPosX -mMovX) // Check left-screen edge
        canvas.scrollLeft <- canvas.scrollLeft + (checkForAutomaticScrolling1D rightScreenEdge mPosX mMovX) // Check right-screen edge
        canvas.scrollTop <- canvas.scrollTop - (checkForAutomaticScrolling1D upperScreenEdge mPosY -mMovY) // Check upper-screen edge
        canvas.scrollTop <- canvas.scrollTop + (checkForAutomaticScrolling1D lowerScreenEdge mPosY mMovY) // Check lower-screen edge
        let xDiff = canvas.scrollLeft - leftScreenEdge
        let yDiff = canvas.scrollTop - upperScreenEdge

        if xDiff <> 0.0 || yDiff <> 0.0 then // Did any automatic scrolling happen?
            let newMPos = { X = model.LastMousePos.X + xDiff / model.Zoom ; Y = model.LastMousePos.Y + yDiff / model.Zoom }
            // Need to update mouse movement as well since the scrolling moves the mouse relative to the canvas, but no actual mouse movement will be detected.
            // E.g. a moving symbol should stick to the mouse as the automatic scrolling happens and not lag behind.
            let outputModel, outputCmd =
                match model.Action with
                | DragAndDrop ->
                    mMoveUpdate { model with AutomaticScrolling = true } 
                                { Pos = newMPos; Op = Move;  Movement = {X=0.;Y=0.}; ShiftKeyDown=false}
                | MovingSymbols | ConnectingInput _ | ConnectingOutput _ | Selecting ->
                    mDragUpdate { model with AutomaticScrolling = true } 
                                { Pos = newMPos; Op = Drag; Movement = {X=0.;Y=0.}; ShiftKeyDown = false}
                | _ ->
                    { model with AutomaticScrolling = true }, Cmd.none
            let notAutomaticScrolling msg = match msg with | CheckAutomaticScrolling -> false | _ -> true
            // Don't want to go into an infinite loop (program would crash), don't check for automatic scrolling immediately (let it be handled by OnScroll listener).
            let filteredOutputCmd = Cmd.map (fun msg -> if notAutomaticScrolling msg then msg else DoNothing) outputCmd
            // keep model ScrollPos uptodate with real scrolling position
            let outputModel = {outputModel with ScrollPos = {X = canvas.scrollLeft; Y = canvas.scrollTop}}

            outputModel, filteredOutputCmd
        else
            { model with AutomaticScrolling = false}, Cmd.none

    | Rotate rotation ->
        model,
        Cmd.batch [
            symbolCmd (SymbolT.RotateLeft(model.SelectedComponents, rotation)) // Better to have Symbol keep track of clipboard as symbols can get deleted before pasting.
            wireCmd (BusWireT.UpdateConnectedWires model.SelectedComponents)
            Cmd.ofMsg SheetT.UpdateBoundingBoxes
            Cmd.ofMsg SheetT.UpdateLabelBoundingBoxes
        ]

    | Flip orientation ->
        model,
        Cmd.batch [
            symbolCmd (SymbolT.Flip(model.SelectedComponents,orientation)) // Better to have Symbol keep track of clipboard as symbols can get deleted before pasting.
            wireCmd (BusWireT.UpdateConnectedWires model.SelectedComponents)
            Cmd.ofMsg SheetT.UpdateBoundingBoxes
        ]
    | SaveSymbols ->
        model, symbolCmd SymbolT.SaveSymbols
    | WireType Jump ->
        let wires = model.Wire.Wires |> Map.toList |> List.map fst
        model,
        Cmd.batch [
            wireCmd (BusWireT.UpdateWireDisplayType BusWireT.Jump)
            wireCmd (BusWireT.MakeJumps wires)
        ]

    | WireType Radiussed ->
        let wires = model.Wire.Wires |> Map.toList |> List.map fst
        model,
        Cmd.batch [
            wireCmd (BusWireT.UpdateWireDisplayType BusWireT.Radial)
            wireCmd (BusWireT.MakeJumps wires)
        ]
       
    | WireType Modern ->
        let wires = model.Wire.Wires |> Map.toList |> List.map fst
        model,
        Cmd.batch [
            wireCmd (BusWireT.UpdateWireDisplayType BusWireT.Modern)
            wireCmd (BusWireT.MakeJumps wires)
        ]
                
    // ---------------------------- Issie Messages ---------------------------- //

    | InitialiseCreateComponent (ldcs, compType, lbl) ->
        { model with Action = (InitialisedCreateComponent (ldcs, compType, lbl)) ; TmpModel = Some model}, Cmd.none
    | FlushCommandStack -> { model with UndoList = []; RedoList = []; TmpModel = None }, Cmd.none
    | ResetModel ->
        { model with
            BoundingBoxes = Map.empty
            LastValidBoundingBoxes = Map.empty
            SelectedComponents = []
            SelectedWires = []
            NearbyComponents = []
            ErrorComponents = []
            DragToSelectBox = {TopLeft={X=0.0; Y=0.0}; H=0.0; W=0.0}
            ConnectPortsLine = {X=0.0; Y=0.0}, {X=0.0; Y=0.0}
            TargetPortId = ""
            Action = Idle
            LastMousePos = { X = 0.0; Y = 0.0 }
            SnapSymbols = emptySnap
            SnapSegments = emptySnap
            //ScrollPos = { X = 0.0; Y = 0.0 } Fix for scroll bug on changing sheets
            LastValidPos = { X = 0.0; Y = 0.0 }
            CurrentKeyPresses = Set.empty
            UndoList = []
            RedoList = []
            TmpModel = None
            Zoom = 1.0
            AutomaticScrolling = false
            ScrollingLastMousePos = {Pos={ X = 0.0; Y = 0.0 };Move={X=0.0; Y=0.0}}
            MouseCounter = 0
            LastMousePosForSnap = { X = 0.0; Y = 0.0 }
        }, Cmd.none
    | UpdateSelectedWires (connIds, on) ->
        let oldWires = model.SelectedWires
        let newWires =
            if on then oldWires @ connIds
            else List.filter (fun conId -> List.contains conId connIds |> not) oldWires
        {model with SelectedWires = newWires}, wireCmd (BusWireT.SelectWires newWires)
    | ColourSelection (compIds, connIds, colour) ->
        {model with SelectedComponents = compIds; SelectedWires = connIds},
        Cmd.batch [
            symbolCmd (SymbolT.ColorSymbols (compIds, colour)) // Better to have Symbol keep track of clipboard as symbols can get deleted before pasting.
            wireCmd (BusWireT.ColorWires (connIds, colour))
        ]
    | ResetSelection ->
        {model with SelectedComponents = []; SelectedWires = []},
        Cmd.batch [
            symbolCmd (SymbolT.SelectSymbols []) // Better to have Symbol keep track of clipboard as symbols can get deleted before pasting.
            wireCmd (BusWireT.SelectWires [])
        ]
    | SetWaveSimMode mode ->
        {model with IsWaveSim = mode}, Cmd.none
    | SelectWires cIds ->
        //If any of the cIds of the netgroup given are inside the previous selection (not current as this will always be true)
        //then deselect (remove from the selected list) any wires from the selected list that are in that cId list
        //otherwise add the cId list to the selectedwires model
        let newWires =
            if List.exists (fun cId -> List.contains cId model.PrevWireSelection) cIds then
                List.filter (fun cId -> List.contains cId cIds |> not) model.PrevWireSelection
            else
                List.append cIds model.PrevWireSelection
        {model with SelectedWires = newWires}, 
        Cmd.batch [
            Cmd.ofMsg (ColourSelection([], newWires, HighLightColor.Blue)); 
            wireCmd (BusWireT.SelectWires newWires)]
    | SetSpinner isOn ->
        if isOn then {model with CursorType = Spinner}, Cmd.none
        else {model with CursorType = Default}, Cmd.none

    | ToggleNet _ | DoNothing | _ -> model, Cmd.none

/// Init function
let init () =
    let wireModel, cmds = (BusWireUpdate.init ())
    let boundingBoxes = Symbol.getBoundingBoxes wireModel.Symbol
    let labelBoundingBoxes = Symbol.getLabelBoundingBoxes wireModel.Symbol

    {
        Wire = wireModel
        PopupViewFunc = None
        PopupDialogData = {Text=None; Int=None; Int2=None}
        BoundingBoxes = boundingBoxes
        LabelBoundingBoxes = labelBoundingBoxes
        LastValidBoundingBoxes = boundingBoxes
        SelectedComponents = []
        SelectedLabel = None
        SelectedWires = []
        NearbyComponents = []
        ErrorComponents = []
        DragToSelectBox = {TopLeft = {X=0.0; Y=0.0}; H=0.0; W=0.0}
        ConnectPortsLine = {X=0.0; Y=0.0}, {X=0.0; Y=0.0}
        TargetPortId = ""
        Action = Idle
        ShowGrid = true
        LastMousePos = { X = 0.0; Y = 0.0 }
        SnapSymbols=emptySnap
        SnapSegments = emptySnap
        CursorType = Default
        ScrollPos = { X = 0.0; Y = 0.0 }
        LastValidPos = { X = 0.0; Y = 0.0 }
        CurrentKeyPresses = Set.empty
        UndoList = []
        RedoList = []
        TmpModel = None
        Zoom = 1.0
        AutomaticScrolling = false
        ScrollingLastMousePos = {Pos={ X = 0.0; Y = 0.0 }; Move={X = 0.0; Y  =0.0}}
        MouseCounter = 0
        LastMousePosForSnap = { X = 0.0; Y = 0.0 }
        Toggle = false
        IsWaveSim = false
        PrevWireSelection = []
    }, Cmd.none



