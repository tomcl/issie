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
open Optics

let rotateLabel (sym:Symbol) =
    let currentRot = Option.defaultValue Degree0 sym.LabelRotation
    {sym with 
        LabelRotation  = Some <| Symbol.combineRotation Degree270 currentRot
        LabelHasDefaultPos = true}

let rotateSelectedLabelsClockwise (model:Model) =
    let symMap = model.Wire.Symbol.Symbols
    let syms = 
        model.SelectedComponents
        |> List.map(fun compId -> symMap[compId])
        
    (symMap, syms)
    ||> List.fold (fun sMap sym -> 
        Map.add sym.Id ((rotateLabel >> Symbol.calcLabelBoundingBox) sym) sMap)
    |> (fun sMap ->
        Optic.set symbols_ sMap model, Cmd.none)

let bbOrientation (bb: BoundingBox) =
    let ratio = Constants.boxAspectRatio
    match abs bb.W > ratio*abs bb.H, abs bb.H > ratio*abs bb.W with
    | true, false when abs bb.W > 10. -> Some (Horizontal, abs bb.W / (abs bb.H+0.1))
    | false, true when abs bb.H > 10. -> Some (Vertical, abs bb.H / (abs bb.W + 0.1))
    | _ -> None

let workOutArrangement (arrange: Arrange) (syms: Symbol list) =
    syms
    |> List.groupBy symbolMatch
    |> List.map (fun (sTyp,syms) ->  syms, (symbolBBUnion true >> Option.bind bbOrientation) syms)
    |> List.filter (fun (_,x) -> x <> None)
    |> List.sortByDescending (fun (syms, bbData) -> syms.Length, (bbData |> Option.get |> snd))
    |> List.tryHead
    |> Option.map (fun (syms,bbData) ->
        match syms, bbData, arrange with
        | [], _, _-> 
            [], Error "No alignable symbols found"
        | syms, Some(orient,_), DistributeSymbols _ when syms.Length < 3 ->
            syms, Error "3 or more symbols of the same type are needed to distribute"
        | syms, Some(orient,_), _ ->
            syms, Ok orient
        | syms, _, _ ->
            syms, Error "alignment failed")
    |> Option.defaultValue ([], Error "No alignable symnbols found")



  

let projectXY isX (pos: XYPos) =
    match isX with | true -> pos.X | false -> pos.Y

let injectXY isX f (pos:XYPos) =
    match isX with | true -> {pos with X = f} | false -> {pos with Y = f}

let alignPosition (symbols: Symbol list) (isX: bool) =
    symbols
    |> List.map (Symbol.getRotatedSymbolCentre >> projectXY isX)
    |> (fun lst -> 
        let av = List.sum lst / float lst.Length
        List.zip lst symbols 
        |> List.collect (fun (c,sym) -> 
            let offset = av - c
            [
                Symbol <| MoveSymbols([sym.Id], injectXY isX offset {X=0;Y=0})
                BusWireT.UpdateSymbolWires sym.Id
            ]))

let distributePosition (symbols: Symbol list) (isX: bool) =
    symbols
    |> List.map (Symbol.getRotatedSymbolCentre >> projectXY isX)
    |> (fun lst ->
            let maxF, minF = List.max lst, List. min lst
            let incr = (maxF - minF) / ((float lst.Length) - 1.)
            List.zip lst symbols
            |> List.sortBy fst
            |> List.mapi (fun i (f,sym) -> 
                let offset = injectXY isX (minF + (float i)*incr - f ) {X=0;Y=0}
                [
                    Symbol <| MoveSymbols ([sym.Id], offset)
                    BusWireT.UpdateSymbolWires sym.Id
                ])
            |> List.concat)

let arrangeSymbols (arrange: Arrange) (model:Model) : Model * Cmd<Msg> =
    let syms, result =
        model.SelectedComponents
        |> List.map (fun sId -> model.Wire.Symbol.Symbols[sId])
        |> workOutArrangement arrange
    let newSelected = 
        syms |> List.map (fun sym -> ComponentId sym.Component.Id)
    match result with
    | Error _mess -> 
        {model with SelectedComponents = newSelected}, Cmd.none
    | Ok orientation ->
        let postludeCmds = [ 
            Cmd.ofMsg UpdateBoundingBoxes; 
            ]
        let cmds =
            match arrange with
            | AlignSymbols -> 
                alignPosition syms (orientation = Vertical)
            | DistributeSymbols -> 
                distributePosition syms (orientation = Horizontal) 
            |> List.map (Wire >> Cmd.ofMsg)
        Optic.set selectedComponents_ newSelected model, (Cmd.batch (cmds @ postludeCmds))

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
             ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.ScreenMovement}
             ErrorComponents = errorComponents},
        Cmd.batch [ symbolCmd (MoveSymbols (model.SelectedComponents, moveDelta))
                    Cmd.ofMsg (UpdateSingleBoundingBox model.SelectedComponents.Head)
                    symbolCmd (ErrorSymbols (errorComponents,model.SelectedComponents,isDragAndDrop))
                    Cmd.ofMsg CheckAutomaticScrolling
                    wireCmd (BusWireT.UpdateWires (model.SelectedComponents, moveDelta))]
    | _ -> // Moving multiple symbols -> don't do snap-to-grid
        let errorComponents =
            model.SelectedComponents
            |> List.filter (fun sId -> not (notIntersectingComponents model model.BoundingBoxes[sId] sId))
        {model with Action = nextAction ; 
                    LastMousePos = mMsg.Pos; 
                    ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.ScreenMovement}; 
                    ErrorComponents = errorComponents },
        Cmd.batch [ symbolCmd (SymbolT.MoveSymbols (model.SelectedComponents, mMsg.Pos - model.LastMousePos))
                    symbolCmd (SymbolT.ErrorSymbols (errorComponents,model.SelectedComponents,isDragAndDrop))
                    Cmd.ofMsg UpdateBoundingBoxes
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
        ScrollingLastMousePos = {Pos = mMsg.Pos; Move = mMsg.ScreenMovement};
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
        | Canvas when mMsg.ShiftKeyDown ->
            // Start Panning with drag, setting up offset to calculate scroll poistion during drag.
            // When panning ScreenScrollPos muts move in opposite direction to ScreenPage.
            {model with Action = Panning ( model.ScreenScrollPos + mMsg.ScreenPage)}, Cmd.none
        | Label compId ->
            {model with Action = InitialiseMovingLabel compId},
            Cmd.ofMsg (SheetT.Msg.Wire (BusWireT.Msg.Symbol (SelectSymbols [compId])))

        | InputPort (portId, portLoc) ->
            if not model.CtrlKeyDown then
                {model with Action = ConnectingInput portId; ConnectPortsLine = portLoc, mMsg.Pos; TmpModel=Some model},
                symbolCmd SymbolT.ShowAllOutputPorts
            else
                let  portIdstr = match portId with | InputPortId x -> x
                {model with Action = MovingPort portIdstr}
                , symbolCmd (SymbolT.MovePort (portIdstr, mMsg.Pos))

        | OutputPort (portId, portLoc) ->
            if not model.CtrlKeyDown then
                {model with Action = ConnectingOutput portId; ConnectPortsLine = portLoc, mMsg.Pos; TmpModel=Some model},
                symbolCmd SymbolT.ShowAllInputPorts
            else
                let  portIdstr = match portId with | OutputPortId x -> x
                {model with Action = MovingPort portIdstr}
                , symbolCmd (SymbolT.MovePort (portIdstr, mMsg.Pos))

        | Component compId ->

            let msg, action = DoNothing, InitialiseMoving compId

            if model.CtrlKeyDown || mMsg.ShiftKeyDown
            then
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
            let (i,wId) = aSeg.Segment.GetId()
            let segments = model.Wire.Wires[wId].Segments
            if i > segments.Length - 1 then
                failwithf "What? Error in getClcikedSegment: "
            let msg = DoNothing

            if model.CtrlKeyDown
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
                    Action = MovingWire (aSeg.Segment.GetId()); 
                    TmpModel = Some model},
                Cmd.batch [ symbolCmd (SymbolT.SelectSymbols [])
                            wireCmd (BusWireT.SelectWires [ connId ])
                            wireCmd (BusWireT.DragSegment (aSeg.Segment.GetId(), mMsg))
                            wireCmd (BusWireT.ResetJumps [ connId ] )
                            Cmd.ofMsg msg]
        | Canvas ->
            let newComponents, newWires =
                if model.CtrlKeyDown
                then model.SelectedComponents, model.SelectedWires //do not deselect if in CtrlKeyDown mode
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
            | MovingSymbols _ -> CursorType.ClickablePort
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
                    ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.ScreenMovement}
                    LastMousePos = mMsg.Pos
         }, Cmd.ofMsg CheckAutomaticScrolling
    | InitialiseMovingLabel compId->
        {model with
                Action = MovingLabel
                LastMousePos = mMsg.Pos
                ScrollingLastMousePos = {Pos = mMsg.Pos; Move = mMsg.ScreenMovement}
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
            CursorType = Grabbing
            LastMousePos = mMsg.Pos
            ScrollingLastMousePos = {Pos = mMsg.Pos; Move = mMsg.ScreenMovement}
        },
        symbolCmd (SymbolT.MoveLabel (movingCompId, mMsg.Pos - model.LastMousePos))

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
                    ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.ScreenMovement}}
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
                    ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.ScreenMovement} }
        , Cmd.ofMsg CheckAutomaticScrolling

    | MovingPort portId->
        model, symbolCmd (SymbolT.MovePort (portId, mMsg.Pos))
    | Panning initPos->
        let sPos = initPos - mMsg.ScreenPage
        model, Cmd.ofMsg (Msg.UpdateScrollPos sPos)
    | Idle 
    | InitialisedCreateComponent _ 
    | Scrolling -> model, Cmd.none
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
            elif model.CtrlKeyDown then
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
                     ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.ScreenMovement} },
        Cmd.batch [ Cmd.ofMsg UpdateBoundingBoxes
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
        let newModel = { model with NearbyComponents = nearbyComponents; CursorType = newCursor; LastMousePos = mMsg.Pos; ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.ScreenMovement} } 
        
        if Set.contains "CONTROL" model.CurrentKeyPresses then
            newModel , symbolCmd (SymbolT.ShowCustomOnlyPorts nearbyComponents)
        else 
            newModel, symbolCmd (SymbolT.ShowPorts nearbyComponents) // Show Ports of nearbyComponents

let getVisibleScreenCentre (model : Model) : XYPos =
    let canvas = document.getElementById "Canvas"
    {
        X = (canvas.scrollLeft + canvas.clientWidth / 2.0) / model.Zoom
        Y = (canvas.scrollTop + canvas.clientHeight / 2.0) / model.Zoom
    }




/// Update Function
let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    /// check things that might not have been correctly completed in the last update and if so do them
    /// Mostly thsi is a hack to deal with the fact that dependent state is held separately rather than
    /// being derived fucntionally from the state it depends on, so it muts be explicitly updated.
    /// TODO: add something to check whether wires need updating

    let postUpdateChecks (model: Model) =
        // Executed every update so performance is important.
        // Since normally state will be correct it is only necessary to make the checking
        // fast.
        let sModel = Optic.get symbol_ model
        sModel.Symbols
        |> (fun sMap ->
                (model,sMap)
                ||> Map.fold (fun model sId sym -> 
                        if Map.containsKey sId model.BoundingBoxes 
                           && sym.Pos = model.BoundingBoxes[sId].TopLeft then
                            model 
                        else
                            Optic.set boundingBoxes_ (Symbol.getBoundingBoxes sModel) model))
        |> ensureCanvasExtendsBeyondScreen
                                
    match msg with
    | Wire (BusWireT.Symbol SymbolT.Msg.UpdateBoundingBoxes) -> 
        // Symbol cannot directly send a message to Sheet box Sheet message type is out of scape. This
        // is used so that a symbol message can be intercepted by sheet and used there.
        model, Cmd.batch [
                Cmd.ofMsg UpdateBoundingBoxes; 
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
                  ]
    | KeyPress CtrlS -> // For Demo, Add a new square in upper left corner
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
                      ]
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
            let model', paras = fitCircuitToWindowParas model
            writeCanvasScroll paras.Scroll
            model', 
            Cmd.batch 
                [
                    Cmd.ofMsg (UpdateScrollPos paras.Scroll)
                    Cmd.ofMsg UpdateBoundingBoxes
                ]
    

    | PortMovementStart ->
        match model.Action with
        | Idle -> {model with CtrlKeyDown = true}, symbolCmd (SymbolT.ShowCustomOnlyPorts model.NearbyComponents) 
        | _ -> model, Cmd.none

    | PortMovementEnd ->
        match model.Action with
        | Idle -> {model with CtrlKeyDown = false}, symbolCmd (SymbolT.ShowPorts model.NearbyComponents)
        | _ -> {model with CtrlKeyDown = false}, Cmd.none

    | MouseMsg mMsg -> // Mouse Update Functions can be found above, update function got very messy otherwise
        let mouseAlreadyDown = match model.Action with | MovingPort _ | ConnectingInput _ | ConnectingOutput _ -> true |_ -> false
        match mMsg.Op with
        | Down when mouseAlreadyDown = true -> model, Cmd.none
        | Down -> mDownUpdate model mMsg
        | Drag -> mDragUpdate model mMsg
        | Up -> mUpUpdate model mMsg
        | Move -> mMoveUpdate model mMsg

    | UpdateBoundingBoxes -> 
        let model =
            model
            |> Optic.set boundingBoxes_ (Symbol.getBoundingBoxes model.Wire.Symbol)
            |> Optic.map symbols_ (Map.map (fun sId sym -> Symbol.calcLabelBoundingBox sym))

        model, Cmd.none

    | UpdateSingleBoundingBox compId ->
        match Map.containsKey compId model.BoundingBoxes with
        | true -> 
            {model with 
                BoundingBoxes = model.BoundingBoxes.Add (compId, (Symbol.getBoundingBox model.Wire.Symbol compId))}
            |> Optic.map symbols_ (Map.change compId (Option.map Symbol.calcLabelBoundingBox))               
                , Cmd.none
        | false -> model, Cmd.none

    | UpdateScrollPosFromCanvas dispatch ->
        let model =
            match canvasDiv with
            | None -> model
            | Some el -> 
                let canvas = document.getElementById "Canvas"
                // UpdateScrollPos here is needed to make CheckAutomaticScrolling work properly
                // Possibly UpdateScrollPos must be after view to trigger the next checkAutomaticScrolling
                // When checkAutomaticScrolling is sone in a better way, this could be removed
                dispatch <| UpdateScrollPos {X=canvas.scrollLeft; Y=canvas.scrollTop} 
                {model with ScreenScrollPos = {X= el.scrollLeft; Y = el.scrollTop}}
        model, Cmd.none

 
    | UpdateScrollPos scrollPos ->
        if model.ScrollUpdateIsOutstanding then 
            model, Cmd.none
        else
            let scrollDif = scrollPos - model.ScreenScrollPos
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
            //Sheet.writeCanvasScroll scrollPos            
            { model with 
                ScreenScrollPos = scrollPos
                ScrollUpdateIsOutstanding = false
                ScrollingLastMousePos = newLastScrollingPos }, 
                cmd

    // Zooming in increases model.Zoom. The centre of the screen will stay centred (if possible)
    | KeyPress ZoomIn ->
        let oldScreenCentre = getVisibleScreenCentre model
        { model with Zoom = min Constants.maxMagnification (model.Zoom*Constants.zoomIncrement) }, 
        Cmd.ofMsg (KeepZoomCentered oldScreenCentre)

    // Zooming out decreases the model.Zoom. The centre of the screen will stay centred (if possible)
    | KeyPress ZoomOut ->
        // get current screen edge coords
        let edge = getScreenEdgeCoords model
        //Check if the new zoom will exceed the canvas width or height
        let newZoom =
            let minXZoom = (edge.Right - edge.Left) / model.CanvasSize
            let minYZoom = (edge.Top - edge.Bottom) / model.CanvasSize
            List.max [model.Zoom / Constants.zoomIncrement; minXZoom; minYZoom]
        let oldScreenCentre = getVisibleScreenCentre model

        { model with Zoom = newZoom }, 
        Cmd.ofMsg (KeepZoomCentered oldScreenCentre)

    | KeepZoomCentered oldScreenCentre ->
        let canvas = document.getElementById "Canvas"
        let newScreenCentre = getVisibleScreenCentre model
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

    | ManualKeyUp key -> 
        { model with CurrentKeyPresses = model.CurrentKeyPresses.Remove (key.ToUpper()) }, Cmd.none

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
            let zero = {X=0.;Y=0.}
            let defaultMsg = { Pos = newMPos; Op = Move;  ScreenMovement = zero; ScreenPage=zero; ShiftKeyDown=false}
            let outputModel, outputCmd =
                match model.Action with
                | DragAndDrop ->
                    mMoveUpdate { model with AutomaticScrolling = true } {defaultMsg with Op = Move}                             
                | MovingSymbols | ConnectingInput _ | ConnectingOutput _ | Selecting ->
                    mDragUpdate { model with AutomaticScrolling = true } {defaultMsg with Op = Drag}                               
                | _ ->
                    { model with AutomaticScrolling = true }, Cmd.none
            let notAutomaticScrolling msg = match msg with | CheckAutomaticScrolling -> false | _ -> true
            // Don't want to go into an infinite loop (program would crash), don't check for automatic scrolling immediately (let it be handled by OnScroll listener).
            let filteredOutputCmd = Cmd.map (fun msg -> if notAutomaticScrolling msg then msg else DoNothing) outputCmd
            // keep model ScrollPos uptodate with real scrolling position
            let outputModel = {outputModel with ScreenScrollPos = {X = canvas.scrollLeft; Y = canvas.scrollTop}}

            outputModel, filteredOutputCmd
        else
            { model with AutomaticScrolling = false}, Cmd.none

    | Arrangement arrange ->
        arrangeSymbols arrange model

    | RotateLabels ->
        rotateSelectedLabelsClockwise model
    
    | Rotate rotation ->
        model,
        Cmd.batch [
            symbolCmd (SymbolT.RotateLeft(model.SelectedComponents, rotation)) // Better to have Symbol keep track of clipboard as symbols can get deleted before pasting.
            wireCmd (BusWireT.UpdateConnectedWires model.SelectedComponents)
            Cmd.ofMsg SheetT.UpdateBoundingBoxes
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
    |> Optic.map fst_ postUpdateChecks


/// Init function
let init () =
    let wireModel, cmds = (BusWireUpdate.init ())
    let boundingBoxes = Symbol.getBoundingBoxes wireModel.Symbol

    {
        Wire = wireModel
        PopupViewFunc = None
        PopupDialogData = {Text=None; Int=None; Int2=None}
        BoundingBoxes = boundingBoxes
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
        ScreenScrollPos = { X = 0.0; Y = 0.0 }
        LastValidPos = { X = 0.0; Y = 0.0 }
        CurrentKeyPresses = Set.empty
        UndoList = []
        RedoList = []
        TmpModel = None
        Zoom = 1.0
        CanvasSize = Constants.defaultCanvasSize
        AutomaticScrolling = false
        ScrollingLastMousePos = {Pos={ X = 0.0; Y = 0.0 }; Move={X = 0.0; Y  =0.0}}
        MouseCounter = 0
        LastMousePosForSnap = { X = 0.0; Y = 0.0 }
        CtrlKeyDown = false
        ScrollUpdateIsOutstanding = false
        PrevWireSelection = []
    }, Cmd.none



