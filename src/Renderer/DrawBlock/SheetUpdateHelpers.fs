module SheetUpdateHelpers

open CommonTypes
open Elmish
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open Optics
open Sheet
open DrawHelpers
open Browser



let rotateLabel (sym:Symbol) =
    let newRot =
        Option.defaultValue Degree0 sym.LabelRotation
        |> Symbol.combineRotation Degree270
        |> Some
    
    {sym with 
        LabelRotation  = newRot
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



let hextoInt (s:string) =
    let s0 = s[0].ToString()
    let s1 = s[1].ToString()
    let i0 = 
        match s0 with
        |"a" -> 10 |"b" ->11 |"c" ->12 |"d" ->13 |"e" ->14 |"f" ->15
        |_ -> int <| s0
    let i1 = 
        match s1 with
        |"a" -> 10 |"b" ->11 |"c" ->12 |"d" ->13 |"e" ->14 |"f" ->15
        |_ -> int <| s1
    (i0*16+i1)
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
            let aSeg = BusWireUpdateHelpers.getClickedSegment model.Wire connId mMsg.Pos
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
        let movingWires = BusWireUpdateHelpers.getConnectedWireIds model.Wire model.SelectedComponents
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
            let tick3Helpers: Hlp23Tick3.Tick3BusWireHelpers = 
                {
                    AutoRoute = BusWireUpdateHelpers.autoroute
                    ReverseWire = BusWireUpdateHelpers.reverseWire
                    MoveSegment = BusWireUpdateHelpers.moveSegment
                }
            let movingWires = BusWireUpdateHelpers.getConnectedWireIds model.Wire model.SelectedComponents
            model
            |> Hlp23Tick3.smartAutoRouteWires movingWires tick3Helpers
            |> (fun model -> {
                model with
                    // BoundingBoxes = Symbol.getBoundingBoxes model.Wire.Symbol
                    Action = Idle
                    SnapSymbols = emptySnap
                    SnapSegments = emptySnap
                    UndoList = appendUndoList model.UndoList newModel
                    RedoList = []
                    AutomaticScrolling = false },
                                wireCmd (BusWireT.MakeJumps movingWires))
        | _ ->
            let movingWires = BusWireUpdateHelpers.getConnectedWireIds model.Wire model.SelectedComponents
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
        let labelTest = 
            match compType with
            |Input _ | Input1 (_,_) |Output _ |Viewer _ |IOLabel ->
                SymbolUpdate.generateIOLabel model.Wire.Symbol compType lbl
            | _ ->
                if lbl = "" then SymbolUpdate.generateLabel model.Wire.Symbol compType else lbl
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

