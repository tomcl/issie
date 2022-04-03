module SheetUpdate

open CommonTypes
open Browser
open Elmish
open DrawHelpers

open Sheet

/// Update function to move symbols in model.SelectedComponents
let moveSymbols (model: Model) (mMsg: MouseT) =
    let nextAction, isDragAndDrop =
        match model.Action with
        | DragAndDrop -> DragAndDrop, true
        | _ -> MovingSymbols, false

    match model.SelectedComponents.Length with
    | 1 -> // Attempt Snap-to-Grid if there is only one moving component

        /// Checks for snap-to-grid in one dimension (x-coordinates or y-coordinates)
        /// Input / output is an anonymous record to deal with too many arguments otherwise

        let checkForSnap1D (input: {| SnapInfo: LastSnap Option; Indicator: float Option; CurrMPos: float; LastMPos: float; Side1: float; Side2: float; PosDirection: float; SymbolMargins: list<float*float> |}) =
 
            match input.SnapInfo with
            | Some { Pos = oldPos; SnapLength = previousSnap } -> // Already snapped, see if mouse is far enough to un-snap
                if abs (input.CurrMPos - oldPos) > unSnapMargin
                then {| DeltaPos = (input.CurrMPos - oldPos) - previousSnap; SnapInfo = None; Indicator = None |} // Un-snap
                else {| DeltaPos = 0.0; SnapInfo = input.SnapInfo; Indicator = input.Indicator |} // Don't un-snap
            | None -> // No snapping has occurred yet, check which side is closer to a grid and see if it should snap, also save which side it is
                let margins = [ (input.Side1 % gridSize), input.Side1
                                ((input.Side1 % gridSize) - gridSize), input.Side1
                                (input.Side2 % gridSize), input.Side2
                                ((input.Side2 % gridSize) - gridSize), input.Side2 ] @ input.SymbolMargins

                let getMarginWithDirection (sortedMargins: (float*float)list) (dir: float) =
                    if abs(fst(sortedMargins[0]) - fst(sortedMargins[1])) < 0.05 then
                        //printfn "HERE"
                        //printfn "%A" dir
                        if dir > 0. then
                        //    printf "HERE1"
                            sortedMargins[0]
                        else sortedMargins[1]
                    else
                        sortedMargins[0]
                let absMargins = List.map (fun (margin,x) -> (abs margin,x) ) margins
                let sortedMargins = List.rev (List.sortByDescending (fun (margin, _) -> margin) absMargins)

                //printfn "%A" sortedMargins

                match getMarginWithDirection sortedMargins input.PosDirection with // abs since there negative margins as well (e.g. snap left)
                | margin, side when margin < 0.4 && not model.AutomaticScrolling -> // disable snap if autoscrolling
                    // Snap to grid and save info for future un-snapping
                    {| DeltaPos = -margin
                       SnapInfo = Some {Pos = input.CurrMPos; SnapLength = -margin - (input.CurrMPos - input.LastMPos)} // Offset with (CurrMPos - LastMPos), so that the symbol stays aligned with the mouse after un-snapping
                       Indicator = Some (side - margin) |}
                | _ -> // Don't do any snap-to-grid
                    {| DeltaPos = (input.CurrMPos - input.LastMPos)
                       SnapInfo = None
                       Indicator = None |}

        let compId = model.SelectedComponents.Head
        let boundingBox = model.BoundingBoxes[compId]
        let x1, x2, y1, y2 = boundingBox.TopLeft.X, boundingBox.TopLeft.X + boundingBox.W, boundingBox.TopLeft.Y, boundingBox.TopLeft.Y + boundingBox.H
        
        // printfn "%A" mMsg.Pos.X
        // printfn "%A" model.LastMousePos.X

        /// Finds margins for moving shaped against other symbol sides 
        let xMargins, yMargins =
            let symbols = 
                Map.toList model.Wire.Symbol.Symbols
                |> List.map (fun x -> snd x) 
                |> List.filter (fun (x:Symbol.Symbol) -> (not x.Moving) && (not (x.Id = compId)))
            let extractEdges (prevList) (sym:Symbol.Symbol) = 
                (fst prevList @ [sym.Pos.X] @ [sym.Pos.X + float sym.Component.W],snd prevList @ [sym.Pos.Y] @ [sym.Pos.Y + float sym.Component.H])
            let x,y = List.fold extractEdges ([],[]) symbols
            let xMarg = List.map (fun margin -> margin-x1, x1) x @ List.map (fun margin -> margin-x2, x2) x
            let yMarg = List.map (fun margin -> margin-y1, y1) y @ List.map (fun margin -> margin-y2, y2) y
            xMarg, yMarg



        let snapX = checkForSnap1D {| 
            SnapInfo = model.Snap.XSnap
            Indicator = model.SnapIndicator.XLine
            CurrMPos = mMsg.Pos.X
            LastMPos = model.LastMousePos.X
            Side1 = x1
            Side2 = x2
            PosDirection = (mMsg.Pos.X - model.LastMousePosForSnap.X)  
            SymbolMargins = xMargins|}

        let snapY = checkForSnap1D {| 
            SnapInfo = model.Snap.YSnap
            Indicator = model.SnapIndicator.YLine
            CurrMPos = mMsg.Pos.Y
            LastMPos = model.LastMousePos.Y
            Side1 = y1
            Side2 = y2
            PosDirection = (mMsg.Pos.Y - model.LastMousePosForSnap.Y) 
            SymbolMargins = yMargins|}

        let errorComponents  =
            if notIntersectingComponents model boundingBox compId then [] else [compId]

        let updateLastMousePosForSnap , updateMouseCounter =
                                if model.MouseCounter > 5 then
                                    mMsg.Pos , 0
                                 else
                                    model.LastMousePos , model.MouseCounter + 1
        {model with
             Action = nextAction
             LastMousePos = mMsg.Pos
             ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.Movement}
             ErrorComponents = errorComponents
             Snap = {XSnap = snapX.SnapInfo; YSnap = snapY.SnapInfo}
             SnapIndicator = {XLine = snapX.Indicator; YLine = snapY.Indicator}
             MouseCounter = updateMouseCounter
             LastMousePosForSnap = updateLastMousePosForSnap},
        Cmd.batch [ symbolCmd (Symbol.MoveSymbols (model.SelectedComponents, {X = snapX.DeltaPos; Y = snapY.DeltaPos}))
                    Cmd.ofMsg (UpdateSingleBoundingBox model.SelectedComponents.Head)
                    symbolCmd (Symbol.ErrorSymbols (errorComponents,model.SelectedComponents,isDragAndDrop))
                    Cmd.ofMsg CheckAutomaticScrolling
                    wireCmd (BusWire.UpdateWires (model.SelectedComponents, posDiff mMsg.Pos model.LastMousePos))]
    | _ -> // Moving multiple symbols -> don't do snap-to-grid
        let errorComponents =
            model.SelectedComponents
            |> List.filter (fun sId -> not (notIntersectingComponents model model.BoundingBoxes[sId] sId))
        {model with Action = nextAction ; LastMousePos = mMsg.Pos; ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.Movement}; ErrorComponents = errorComponents },
        Cmd.batch [ symbolCmd (Symbol.MoveSymbols (model.SelectedComponents, posDiff mMsg.Pos model.LastMousePos))
                    symbolCmd (Symbol.ErrorSymbols (errorComponents,model.SelectedComponents,isDragAndDrop))
                    Cmd.ofMsg UpdateBoundingBoxes
                    Cmd.ofMsg CheckAutomaticScrolling
                    wireCmd (BusWire.UpdateWires (model.SelectedComponents, posDiff mMsg.Pos model.LastMousePos))]


let snapWire (model: Model) (mMsg: MouseT) (connId: ConnectionId): Model * Cmd<Msg> = 
    
    let nextAction, isMovingWire =
        match model.Action with
        | MovingWire connId -> MovingWire connId, true
        | _ -> Idle, false

    let checkForSnap 
        (input: 
            {| 
                SnapInfo: LastSnap Option; 
                Indicator: float Option; 
                CurrMPos: XYPos; 
                LastMPos: XYPos; 
                orientation: BusWire.Orientation |}) =
        
        let clickedWireId = model.SelectedWires.Head
        let clickedWire = model.Wire.Wires |> Map.find clickedWireId

        let clickedSegId = BusWireUpdate.getClickedSegment model.Wire clickedWireId input.CurrMPos

        let segPositions (index,wId) =
            (None, model.Wire.Wires[wId])
            ||> BusWire.foldOverSegs (fun startPos endPos state seg ->
                                            if seg.Index = index then Some (startPos, endPos)
                                            else state)

        let segStart, segEnd = Option.get (segPositions clickedSegId)

        let segOrientation: BusWire.Orientation = BusWire.getSegmentOrientation segStart segEnd

        let distanceFromPrevGridline: float =
            match segOrientation with
            | BusWire.Orientation.Vertical -> segStart.X % gridSize
            | BusWire.Orientation.Horizontal -> segStart.Y % gridSize

        let margin: float =
            if distanceFromPrevGridline > (gridSize / 2.0) then
                gridSize - distanceFromPrevGridline
            else distanceFromPrevGridline
        
        match input.SnapInfo with
        | Some {Pos = oldPos; SnapLength = previousSnap} -> //already snapped, see if far enough to unsnap
            match segOrientation, input.orientation with
            | BusWire.Orientation.Vertical , BusWire.Orientation.Vertical->
                if abs (input.CurrMPos.X - oldPos) - previousSnap > unSnapMargin then
                    {| DeltaPos = (input.CurrMPos.X - oldPos) - previousSnap; SnapInfo = None; Indicator = None |} //unSnap
                else {| DeltaPos = input.CurrMPos.X-input.LastMPos.X; SnapInfo = input.SnapInfo; Indicator = input.Indicator |} // no unSnap
            | BusWire.Orientation.Horizontal, BusWire.Orientation.Horizontal->
                if abs (input.CurrMPos.Y - oldPos) - previousSnap > unSnapMargin then
                    {| DeltaPos = (input.CurrMPos.Y - oldPos) - previousSnap; SnapInfo = None; Indicator = None |}
                else {| DeltaPos = 0.; SnapInfo = input.SnapInfo; Indicator = input.Indicator |}
            | _ -> {| DeltaPos = input.CurrMPos.Y-input.LastMPos.Y; SnapInfo = None; Indicator = None |}
        | None ->
            match segOrientation, input.orientation with
            | BusWire.Orientation.Vertical, BusWire.Orientation.Vertical ->
                match (margin < snapMargin), (not model.AutomaticScrolling), (distanceFromPrevGridline - margin < 1.) with
                | true, true, true -> 
                    {| DeltaPos = margin;
                       SnapInfo = Some {Pos = input.CurrMPos.X; SnapLength = -margin};
                       Indicator = Some (segStart.X - margin) |}
                | true, true, false ->
                    {| DeltaPos = -margin;
                       SnapInfo = Some {Pos = input.CurrMPos.X; SnapLength = margin};
                       Indicator = Some (segStart.X + margin) |}
                | _ ->
                    {| DeltaPos = (input.CurrMPos.X - input.LastMPos.X);
                       SnapInfo = None;
                       Indicator = None |}
            | BusWire.Orientation.Horizontal, BusWire.Orientation.Horizontal ->
                match (margin < snapMargin), (not model.AutomaticScrolling), (distanceFromPrevGridline - margin < 1.) with
                | true, true, true -> 
                    {| DeltaPos = margin;
                       SnapInfo = Some {Pos = input.CurrMPos.Y; SnapLength = -margin};
                       Indicator = Some (segStart.Y - margin) |}
                | true, true, false -> 
                    {| DeltaPos = -margin;
                       SnapInfo = Some {Pos = input.CurrMPos.Y; SnapLength = margin};
                       Indicator = Some (segStart.Y + margin) |}
                | _ ->
                    {| DeltaPos = (input.CurrMPos.Y - input.LastMPos.Y);
                       SnapInfo = None;
                       Indicator = None |}
            | _ ->  {| DeltaPos = 0.; SnapInfo = None; Indicator = None |}
    
    let snapX = checkForSnap {| 
        SnapInfo = model.Snap.XSnap;
        Indicator = model.SnapIndicator.XLine;
        CurrMPos = mMsg.Pos;
        LastMPos = model.LastMousePos;
        orientation = BusWire.Vertical    |}

    let snapY = checkForSnap {| 
        SnapInfo = model.Snap.YSnap;
        Indicator = model.SnapIndicator.YLine;
        CurrMPos = mMsg.Pos;   
        LastMPos = model.LastMousePos;
        orientation = BusWire.Horizontal    |}

    let updateLastMousePosForSnap, updateMouseCounter =
        if model.MouseCounter > 5 then
            mMsg.Pos, 0
        else model.LastMousePos, model.MouseCounter + 1
    let wirePos = 
        match snapX.SnapInfo, snapY.SnapInfo with
        | Some sInfo , None -> {X=sInfo.Pos; Y=mMsg.Pos.Y}
        | None, Some sInfo -> {X=mMsg.Pos.X; Y=sInfo.Pos}
        | _ -> mMsg.Pos

    let mMsg' = 
        {mMsg with Pos = wirePos} 
                                
    { model with
        Action = nextAction;
        LastMousePos = mMsg.Pos;
        ScrollingLastMousePos = {Pos = mMsg.Pos; Move = mMsg.Movement};
        ErrorComponents = [];
        Snap = {XSnap = snapX.SnapInfo; YSnap = snapY.SnapInfo};
        SnapIndicator = {XLine = snapX.Indicator; YLine = snapY.Indicator};
        MouseCounter = updateMouseCounter;
        LastMousePosForSnap = updateLastMousePosForSnap },
    Cmd.batch [ wireCmd (BusWire.DragWire (model.SelectedWires.Head, mMsg'));
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


/// Mouse Down Update, Can have clicked on: InputPort / OutputPort / Component / Wire / Canvas. Do correct action for each.
let mDownUpdate (model: Model) (mMsg: MouseT) : Model * Cmd<Msg> =
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
                Snap = {XSnap = None; YSnap = None}
                SnapIndicator = {XLine = None; YLine = None}
                UndoList = appendUndoList model.UndoList newModel
                RedoList = []
                AutomaticScrolling = false
            },
            Cmd.batch [ symbolCmd (Symbol.SelectSymbols model.SelectedComponents)
                        wireCmd (BusWire.SelectWires model.SelectedWires)
                        wireCmd (BusWire.ResetJumps [])]
    | _ ->
        match (mouseOn model mMsg.Pos) with
        | InputPort (portId, portLoc) ->
            if not model.Toggle then
                {model with Action = ConnectingInput portId; ConnectPortsLine = portLoc, mMsg.Pos; TmpModel=Some model},
                symbolCmd Symbol.ShowAllOutputPorts
            else
                let  portIdstr = match portId with | InputPortId x -> x
                {model with Action = MovingPort portIdstr}
                , symbolCmd (Symbol.MovePort (portIdstr, mMsg.Pos))

        | OutputPort (portId, portLoc) ->
            if not model.Toggle then
                {model with Action = ConnectingOutput portId; ConnectPortsLine = portLoc, mMsg.Pos; TmpModel=Some model},
                symbolCmd Symbol.ShowAllInputPorts
            else
                let  portIdstr = match portId with | OutputPortId x -> x
                {model with Action = MovingPort portIdstr}
                , symbolCmd (Symbol.MovePort (portIdstr, mMsg.Pos))

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
                    LastValidPos = mMsg.Pos ; 
                    LastValidBoundingBoxes=model.BoundingBoxes ; 
                    Action = action; LastMousePos = mMsg.Pos; 
                    TmpModel = Some model; 
                    PrevWireSelection = model.SelectedWires},
                Cmd.batch [symbolCmd (Symbol.SelectSymbols newComponents); Cmd.ofMsg msg]
            else
                let newComponents, newWires =
                    if List.contains compId model.SelectedComponents
                    then model.SelectedComponents, model.SelectedWires // Keep selection for symbol movement
                    else [compId], [] // If user clicked on a new component, select that one instead
                {model with SelectedComponents = newComponents; LastValidPos = mMsg.Pos ; LastValidBoundingBoxes=model.BoundingBoxes ; SelectedWires = newWires; Action = action; LastMousePos = mMsg.Pos; TmpModel = Some model},
                Cmd.batch [ symbolCmd (Symbol.SelectSymbols newComponents)
                            wireCmd (BusWire.SelectWires newWires)
                            Cmd.ofMsg msg]

        | Connection connId ->
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
                Cmd.batch [wireCmd (BusWire.SelectWires newWires); Cmd.ofMsg msg]
            else
                { model with SelectedComponents = []; SelectedWires = [ connId ]; Action = MovingWire connId; TmpModel=Some model},
                Cmd.batch [ symbolCmd (Symbol.SelectSymbols [])
                            wireCmd (BusWire.SelectWires [ connId ])
                            wireCmd (BusWire.DragWire (connId, mMsg))
                            wireCmd (BusWire.ResetJumps [ connId ] )
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
            Cmd.batch [ symbolCmd (Symbol.SelectSymbols newComponents)
                        wireCmd (BusWire.SelectWires newWires) ]

/// Mouse Drag Update, can be: drag-to-selecting, moving symbols, connecting wire between ports.
let mDragUpdate (model: Model) (mMsg: MouseT) : Model * Cmd<Msg> =
    match model.Action with
    | MovingWire connId -> snapWire model mMsg connId 
    | Selecting ->
        let initialX = model.DragToSelectBox.TopLeft.X
        let initialY = model.DragToSelectBox.TopLeft.Y
        let newDragToSelectBox = {model.DragToSelectBox with W = (mMsg.Pos.X - initialX); H = (mMsg.Pos.Y - initialY)}
        {model with DragToSelectBox = newDragToSelectBox
                    ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.Movement}
                    LastMousePos = mMsg.Pos
         }, Cmd.ofMsg CheckAutomaticScrolling
    | InitialiseMoving _ ->
        let movingWires = BusWireUpdate.getConnectedWireIds model.Wire model.SelectedComponents
        let newModel, cmd = moveSymbols model mMsg
        newModel, Cmd.batch [ cmd ]
    | MovingSymbols | DragAndDrop ->
        moveSymbols model mMsg
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
        model, symbolCmd (Symbol.MovePort (portId, mMsg.Pos))
    | _ -> model, Cmd.none
/// Mouse Up Update, can have: finished drag-to-select, pressed on a component, finished symbol movement, connected a wire between ports
let mUpUpdate (model: Model) (mMsg: MouseT) : Model * Cmd<Msg> = // mMsg is currently un-used, but kept for future possibilities
    let newModel =
        match model.TmpModel with
        | None -> model
        | Some newModel -> {newModel with SelectedComponents = model.SelectedComponents}
    match model.Action with
    | MovingWire connId ->
        { model with Action = Idle ; UndoList = appendUndoList model.UndoList newModel; RedoList = [] },
        Cmd.batch [ wireCmd (BusWire.DragWire (connId, mMsg))
                    wireCmd (BusWire.MakeJumps [ connId ] ) ]
    | Selecting ->
        let newComponents = findIntersectingComponents model model.DragToSelectBox
        let newWires = BusWireUpdate.getIntersectingWires model.Wire model.DragToSelectBox
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
        Cmd.batch [ symbolCmd (Symbol.SelectSymbols selectComps)
                    wireCmd (BusWire.SelectWires selectWires) ]

    | InitialiseMoving compId -> 
            // not sure there is any point to running this from mouse UP this now we are not altering selection?
            // legacy case due for removal?
            { model with Action = Idle}, wireCmd (BusWire.SelectWires [])

    | MovingSymbols ->
        // Reset Movement State in Model
        match model.ErrorComponents with
        | [] ->
            let movingWires = BusWireUpdate.getConnectedWireIds model.Wire model.SelectedComponents
            {model with
                // BoundingBoxes = Symbol.getBoundingBoxes model.Wire.Symbol
                Action = Idle
                Snap = {XSnap = None; YSnap = None}
                SnapIndicator = {XLine = None; YLine = None }
                UndoList = appendUndoList model.UndoList newModel
                RedoList = []
                AutomaticScrolling = false },
            wireCmd (BusWire.MakeJumps movingWires)
        | _ ->
            let movingWires = BusWireUpdate.getConnectedWireIds model.Wire model.SelectedComponents
            {model with
                BoundingBoxes = model.LastValidBoundingBoxes
                Action = Idle
                Snap = {XSnap = None; YSnap = None}
                SnapIndicator = {XLine = None; YLine = None }
                AutomaticScrolling = false },
            Cmd.batch [ symbolCmd (Symbol.MoveSymbols (model.SelectedComponents, (posDiff model.LastValidPos mMsg.Pos)))
                        symbolCmd (Symbol.SelectSymbols (model.SelectedComponents))
                        wireCmd (BusWire.UpdateWires (model.SelectedComponents, posDiff model.LastValidPos mMsg.Pos))
                        wireCmd (BusWire.MakeJumps movingWires) ]
    | ConnectingInput inputPortId ->
        let cmd, undoList ,redoList =
            if model.TargetPortId <> "" // If a target has been found, connect a wire
            then wireCmd (BusWire.AddWire (inputPortId, (OutputPortId model.TargetPortId))),
                           appendUndoList model.UndoList newModel, []
            else Cmd.none , model.UndoList , model.RedoList
        {model with Action = Idle; TargetPortId = ""; UndoList = undoList ; RedoList = redoList ; AutomaticScrolling = false }, cmd
    | ConnectingOutput outputPortId ->
        let cmd , undoList , redoList =
            if model.TargetPortId <> "" // If a target has been found, connect a wire
            then  wireCmd (BusWire.AddWire (InputPortId model.TargetPortId, outputPortId)),
                           appendUndoList model.UndoList newModel , []
            else Cmd.none , model.UndoList , model.RedoList
        { model with Action = Idle; TargetPortId = ""; UndoList = undoList ; RedoList = redoList ; AutomaticScrolling = false  }, cmd
    | MovingPort portId ->
        let symbol = Symbol.getCompId model.Wire.Symbol portId
        {model with Action = Idle},
        Cmd.batch [
            symbolCmd (Symbol.MovePortDone (portId, mMsg.Pos))
            wireCmd (BusWire.UpdateSymbolWires symbol);
            wireCmd (BusWire.RerouteWire portId)]
    | _ -> model, Cmd.none

/// Mouse Move Update, looks for nearby components and looks if mouse is on a port
let mMoveUpdate (model: Model) (mMsg: MouseT) : Model * Cmd<Msg> =
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
                    symbolCmd (Symbol.SelectSymbols [])
                    symbolCmd (Symbol.PasteSymbols [ newCompId ]) ]
    | _ ->
        let nearbyComponents = findNearbyComponents model mMsg.Pos 50 // TODO Group Stage: Make this more efficient, update less often etc, make a counter?

        let newCursor =
            match model.CursorType with
            | Spinner -> Spinner
            | _ ->
                match mouseOn { model with NearbyComponents = nearbyComponents } mMsg.Pos with // model.NearbyComponents can be outdated e.g. if symbols have been deleted -> send with updated nearbyComponents.
                | InputPort _ | OutputPort _ -> ClickablePort // Change cursor if on port
                | _ -> Default

        { model with NearbyComponents = nearbyComponents; CursorType = newCursor; LastMousePos = mMsg.Pos; ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.Movement} },
        symbolCmd (Symbol.ShowPorts nearbyComponents) // Show Ports of nearbyComponents

let getScreenCentre (model : Model) : XYPos =
    let canvas = document.getElementById "Canvas"
    {
        X = (canvas.scrollLeft + canvas.clientWidth / 2.0) / model.Zoom
        Y = (canvas.scrollTop + canvas.clientHeight / 2.0) / model.Zoom
    }

/// Update Function
let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
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
        Cmd.batch [ wireCmd (BusWire.DeleteWires wireUnion) // Delete Wires before components so nothing bad happens
                    symbolCmd (Symbol.DeleteSymbols model.SelectedComponents)
                    Cmd.ofMsg UpdateBoundingBoxes ]
    | KeyPress CtrlS -> // For Demo, Add a new square in upper left corner
        printfn "saving symbols"
        { model with BoundingBoxes = Symbol.getBoundingBoxes model.Wire.Symbol; UndoList = appendUndoList model.UndoList model ; RedoList = []},
        Cmd.batch [ Cmd.ofMsg UpdateBoundingBoxes; symbolCmd Symbol.SaveSymbols ] // Need to update bounding boxes after adding a symbol.
    | KeyPress AltShiftZ ->
        TimeHelpers.printStats()
        model, Cmd.none
    | KeyPress CtrlC ->
        model,
        Cmd.batch [
            symbolCmd (Symbol.CopySymbols model.SelectedComponents) // Better to have Symbol keep track of clipboard as symbols can get deleted before pasting.
            wireCmd (BusWire.CopyWires model.SelectedWires)
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
                    symbolCmd (Symbol.SelectSymbols []) // Select to unhighlight all other symbols
                    symbolCmd (Symbol.PasteSymbols pastedCompIds)
                    wireCmd (BusWire.SelectWires [])
                    wireCmd (BusWire.ColorWires (pastedConnIds, HighLightColor.Thistle)) ]
    | KeyPress ESC -> // Cancel Pasting Symbols, and other possible actions in the future
        match model.Action with
        | DragAndDrop ->
            { model with SelectedComponents = []
                         SelectedWires = []
                         Snap = {XSnap = None; YSnap = None}
                         SnapIndicator = {XLine = None; YLine = None }
                         Action = Idle },
            Cmd.batch [ symbolCmd (Symbol.DeleteSymbols model.SelectedComponents)
                        wireCmd (BusWire.DeleteWires model.SelectedWires)
                        Cmd.ofMsg UpdateBoundingBoxes ]
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
        } , Cmd.batch [ symbolCmd (Symbol.SelectSymbols symbols)
                        wireCmd (BusWire.SelectWires wires) ]
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
        //printf "%A" mMsg
        match mMsg.Op with
        | Down -> mDownUpdate model mMsg
        | Drag -> mDragUpdate model mMsg
        | Up -> mUpUpdate model mMsg
        | Move -> mMoveUpdate model mMsg
    | UpdateBoundingBoxes -> { model with BoundingBoxes = Symbol.getBoundingBoxes model.Wire.Symbol }, Cmd.none
    | UpdateSingleBoundingBox compId ->
        match Map.containsKey compId model.BoundingBoxes with
        | true -> {model with BoundingBoxes = model.BoundingBoxes.Add (compId, (Symbol.getBoundingBox model.Wire.Symbol compId))}, Cmd.none
        | false -> model, Cmd.none
    | UpdateScrollPos (scrollX, scrollY) ->
        let scrollDif = posDiff { X = scrollX; Y = scrollY } model.ScrollPos
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
        let requiredOffset = posDiff oldScreenCentre newScreenCentre

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

            // Don't want to go into an infinite loop (program would crash), don't check for automatic scrolling immediately (let it be handled by OnScroll listener).
            let filteredOutputCmd = Cmd.map (fun msg -> if msg <> CheckAutomaticScrolling then msg else DoNothing) outputCmd
            // keep model ScrollPos uptodate with real scrolling position
            let outputModel = {outputModel with ScrollPos = {X = canvas.scrollLeft; Y = canvas.scrollTop}}

            outputModel, filteredOutputCmd
        else
            { model with AutomaticScrolling = false}, Cmd.none

    | Rotate rotation ->
        model,
        Cmd.batch [
            symbolCmd (Symbol.RotateLeft(model.SelectedComponents, rotation)) // Better to have Symbol keep track of clipboard as symbols can get deleted before pasting.
            wireCmd (BusWire.UpdateConnectedWires model.SelectedComponents)
        ]

    | Flip orientation ->
        model,
        Cmd.batch [
            symbolCmd (Symbol.Flip(model.SelectedComponents,orientation)) // Better to have Symbol keep track of clipboard as symbols can get deleted before pasting.
            wireCmd (BusWire.UpdateConnectedWires model.SelectedComponents)
        ]
    | SaveSymbols ->
        model, symbolCmd Symbol.SaveSymbols
    | WireType Jump ->
        let wires = model.Wire.Wires |> Map.toList |> List.map fst
        model,
        Cmd.batch [
            wireCmd (BusWire.UpdateWireDisplayType BusWire.Jump)
            wireCmd (BusWire.MakeJumps wires)
        ]

    | WireType Radiussed ->
        let wires = model.Wire.Wires |> Map.toList |> List.map fst
        model,
        Cmd.batch [
            wireCmd (BusWire.UpdateWireDisplayType BusWire.Radial)
            wireCmd (BusWire.MakeJumps wires)
        ]
       
    | WireType Modern ->
        let wires = model.Wire.Wires |> Map.toList |> List.map fst
        model,
        Cmd.batch [
            wireCmd (BusWire.UpdateWireDisplayType BusWire.Modern)
            wireCmd (BusWire.MakeJumps wires)
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
            Snap = { XSnap = None; YSnap = None}
            SnapIndicator = { XLine = None; YLine = None }
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
        {model with SelectedWires = newWires}, wireCmd (BusWire.SelectWires newWires)
    | ColourSelection (compIds, connIds, colour) ->
        {model with SelectedComponents = compIds; SelectedWires = connIds},
        Cmd.batch [
            symbolCmd (Symbol.ColorSymbols (compIds, colour)) // Better to have Symbol keep track of clipboard as symbols can get deleted before pasting.
            wireCmd (BusWire.ColorWires (connIds, colour))
        ]
    | ResetSelection ->
        {model with SelectedComponents = []; SelectedWires = []},
        Cmd.batch [
            symbolCmd (Symbol.SelectSymbols []) // Better to have Symbol keep track of clipboard as symbols can get deleted before pasting.
            wireCmd (BusWire.SelectWires [])
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
            wireCmd (BusWire.SelectWires newWires)]
    | SetSpinner isOn ->
        if isOn then {model with CursorType = Spinner}, Cmd.none
        else {model with CursorType = Default}, Cmd.none

    | ToggleNet _ | DoNothing | _ -> model, Cmd.none



/// Init function
let init () =
    let wireModel, cmds = (BusWireUpdate.init ())
    let boundingBoxes = Symbol.getBoundingBoxes wireModel.Symbol

    {
        Wire = wireModel
        BoundingBoxes = boundingBoxes
        LastValidBoundingBoxes = boundingBoxes
        SelectedComponents = []
        SelectedWires = []
        NearbyComponents = []
        ErrorComponents = []
        DragToSelectBox = {TopLeft = {X=0.0; Y=0.0}; H=0.0; W=0.0}
        ConnectPortsLine = {X=0.0; Y=0.0}, {X=0.0; Y=0.0}
        TargetPortId = ""
        Action = Idle
        ShowGrid = true
        LastMousePos = { X = 0.0; Y = 0.0 }
        Snap = { XSnap = None; YSnap = None}
        SnapIndicator = { XLine = None; YLine = None }
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


