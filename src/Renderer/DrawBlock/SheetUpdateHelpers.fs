module SheetUpdateHelpers

open CommonTypes
open Elmish
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open Optics
open Sheet
open SheetSnap
open DrawHelpers
open BusWireRoutingHelpers
open BlockHelpers
open Browser
open Optics
open Operators

let fitCircuitToScreenUpdate (model: Model) =
    let model', parasOpt = fitCircuitToWindowParas model
    match parasOpt with
    | Some paras ->
        model', 
        Cmd.batch 
            [
                sheetCmd (SheetT.Msg.UpdateScrollPos paras.Scroll)
                sheetCmd SheetT.Msg.UpdateBoundingBoxes
                if abs (model.Zoom - model'.Zoom) > 0.001 then
                    ModelType.Msg.RunAfterRender (false, (fun dispatch model -> (dispatch <| ModelType.Msg.Sheet (SheetT.Msg.KeyPress CtrlW)); model))
                    |> Cmd.ofMsg
            ]
    | None -> model, Cmd.none

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
        | syms, Some(orient,_), DistributeSymbols when syms.Length < 3 ->
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

let arrangeSymbols (arrange: Arrange) (model:Model) : Model * Cmd<ModelType.Msg> =
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
            sheetCmd UpdateBoundingBoxes; 
            ]
        let cmds =
            match arrange with
            | AlignSymbols -> 
                alignPosition syms (orientation = Vertical)
            | DistributeSymbols -> 
                distributePosition syms (orientation = Horizontal) 
            |> List.map (Wire >> sheetCmd)
        Optic.set selectedComponents_ newSelected model, (Cmd.batch (cmds @ postludeCmds))

/// Update function to move symbols in model.SelectedComponents
let moveSymbols (model: Model) (mMsg: MouseT) =
    let nextAction, isDragAndDrop =
        match model.Action with
        | DragAndDrop -> DragAndDrop, true
        | _ -> MovingSymbols, false // DragAndDrop, false

    match model.SelectedComponents with
    | [] -> model, Cmd.none
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
                    sheetCmd (UpdateSingleBoundingBox model.SelectedComponents.Head)
                    symbolCmd (ErrorSymbols (errorComponents,model.SelectedComponents,isDragAndDrop))
                    sheetCmd CheckAutomaticScrolling
                    wireCmd (BusWireT.UpdateWires (model.SelectedComponents, moveDelta))]
    | _ -> // Moving multiple symbols -> don't do snap-to-grid
        let errorComponents =
            model.SelectedComponents
            |> List.filter (fun sId -> not (notIntersectingComponents model model.BoundingBoxes[sId] sId)) 
        {model with Action = nextAction;
                    LastMousePos = mMsg.Pos; 
                    ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.ScreenMovement}; 
                    ErrorComponents = errorComponents},
        Cmd.batch [ symbolCmd (SymbolT.MoveSymbols (model.SelectedComponents, mMsg.Pos - model.LastMousePos))
                    symbolCmd (SymbolT.ErrorSymbols (errorComponents,model.SelectedComponents,isDragAndDrop))
                    sheetCmd UpdateBoundingBoxes
                    sheetCmd CheckAutomaticScrolling
                    wireCmd (BusWireT.UpdateWires (model.SelectedComponents, mMsg.Pos - model.LastMousePos))]

/// Inputs are segment ID being dragged and new mouse position.
/// Performs the Segment Drag operation implementing snaps.
/// This function must be in update and creates additional commands
/// to implement the drag oeporation.
let snapWire 
        (model: Model) 
        (mMsg: MouseT) 
        (segIdL: SegmentId list)
            : Model * Cmd<ModelType.Msg> = 
    
    let nextAction, isMovingWire =
        match model.Action with
        | MovingWire segId -> MovingWire segId, true
        | _ -> Idle, false
    match segIdL with
    | [] ->
        { model with
            Action = nextAction;
            LastMousePos = mMsg.Pos;
            ScrollingLastMousePos = {Pos = mMsg.Pos; Move = mMsg.ScreenMovement};
            ErrorComponents = [];
            SnapSegments = emptySnap
        }, Cmd.none
    | segId :: _ ->
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
        Cmd.batch [ wireCmd (BusWireT.DragSegment (segIdL, newmMsg));
                    sheetCmd CheckAutomaticScrolling] 



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
    let rec removeLast inputLst =
        inputLst
        |> List.truncate (max 0 (inputLst.Length - 1))

    match List.length undoList with
    | n when n < Constants.maxUndoListSize -> 
        model_in :: undoList
    | _ -> 
        model_in :: (removeLast undoList)


/// Mouse Down Update, Can have clicked on: Label, InputPort / OutputPort / Component / Wire / Canvas. Do correct action for each.
let mDownUpdate 
        (model: Model) 
        (mMsg: MouseT) 
            : Model * Cmd<ModelType.Msg> =
    let newModel =
        match model.TmpModel with
        | None -> model
        | Some newModel -> newModel
    // printfn "running mDownUpdate"
    //printfn "mDownUpdate Action: %A" model.Action
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
                AutomaticScrolling = false
            }
            |> Optic.map wire_ (BusWireSeparate.updateWireSegmentJumpsAndSeparations model.SelectedWires)
            |> (fun model ->
                        model ,
                        Cmd.batch [ symbolCmd (SymbolT.SelectSymbols model.SelectedComponents)
                                    wireCmd (BusWireT.SelectWires model.SelectedWires)
                                    wireCmd (BusWireT.ResetJumps [])])
    | _ ->
        match (mouseOn model mMsg.Pos) with
        | Canvas when mMsg.ShiftKeyDown ->
            // Start Panning with drag, setting up offset to calculate scroll poistion during drag.
            // When panning ScreenScrollPos muts move in opposite direction to ScreenPage.
            {model with Action = Panning ( model.ScreenScrollPos + mMsg.ScreenPage)}, Cmd.none
        | Label compId ->
            {model with Action = InitialiseMovingLabel compId; TmpModel = Some model},
                sheetCmd (SheetT.Msg.Wire (BusWireT.Msg.Symbol (SelectSymbols [compId])))
                
        | InputPort (portId, portLoc) ->
            if not model.CtrlKeyDown then
                {model with Action = ConnectingInput portId; ConnectPortsLine = portLoc, mMsg.Pos; TmpModel=Some model},
                symbolCmd SymbolT.ShowAllOutputPorts
            else
                let  portIdstr = match portId with | InputPortId x -> x
                {model with Action = MovingPort portIdstr}, 
                symbolCmd (SymbolT.MovePort (portIdstr, mMsg.Pos))

        | OutputPort (portId, portLoc) ->
            if not model.CtrlKeyDown then
                {model with Action = ConnectingOutput portId; ConnectPortsLine = portLoc, mMsg.Pos; TmpModel=Some model},
                symbolCmd SymbolT.ShowAllInputPorts
            else
                let portIdstr = match portId with | OutputPortId x -> x
                {model with Action = MovingPort portIdstr}
                , symbolCmd (SymbolT.MovePort (portIdstr, mMsg.Pos))
        // HLP23 AUTHOR: BRYAN TAN
        | ComponentCorner (compId, fixedCornerLoc, _) ->
            if not model.CtrlKeyDown then
                model, Cmd.none
            else
                let symbolMap = Optic.get symbols_ model
                let symbol = symbolMap[compId]
                {model with Action = ResizingSymbol (compId, fixedCornerLoc); LastValidSymbol = Some symbol}, 
                symbolCmd (SymbolT.ResizeSymbol (compId, fixedCornerLoc, mMsg.Pos))
        // HLP 23: AUTHOR Khoury & Ismagilov
        // Modified and added parts to deal with the scaling box functions
        | Component compId ->
            match model.Wire.Symbol.Symbols[compId].Annotation with
                | Some ScaleButton ->   
                    let scalingBoxCentre:XYPos = model.ScalingBox.Value.ScalingBoxBound.Centre()
                    // printfn "startCentre:%A" scalingBoxCentre
                    // printfn "startMousePos:%A" mMsg.Pos
                    {model with
                        ScalingBoxCentrePos = scalingBoxCentre
                        ScalingTmpModel = None
                        Action = Scaling;
                        LastMousePos = mMsg.Pos;
                        TmpModel = Some model}, Cmd.none
                
                | Some (RotateButton rotation) ->
                    {model with TmpModel = Some model; Action = Idle}, 
                        Cmd.batch [ sheetCmd (Rotate rotation); 
                                    wireCmd (BusWireT.UpdateConnectedWires model.SelectedComponents)]

                |_ ->  
                    let msg, action = DoNothing, InitialiseMoving compId
                    if model.CtrlKeyDown || mMsg.ShiftKeyDown
                    then
                        let newComponents =
                            if List.contains compId model.SelectedComponents
                            then List.filter (fun cId -> cId <> compId) model.SelectedComponents // If component selected was already in the list, remove it
                            else compId :: model.SelectedComponents // If user clicked on a new component add it to the selected list

                        {model with SelectedComponents = newComponents; 
                                    SnapSymbols = emptySnap;
                                    LastValidPos = mMsg.Pos; 
                                    LastValidBoundingBoxes=model.BoundingBoxes; 
                                    Action = action; LastMousePos = mMsg.Pos; 
                                    //TmpModel = Some model; 
                                    PrevWireSelection = model.SelectedWires},
                            Cmd.batch [symbolCmd (SymbolT.SelectSymbols newComponents); sheetCmd msg]
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
                                SnapSymbols = snapXY;
                                LastValidPos = mMsg.Pos; 
                                LastValidBoundingBoxes=model.BoundingBoxes; 
                                SelectedWires = newWires; Action = action; 
                                LastMousePos = mMsg.Pos}, // TmpModel = Some model},
                            Cmd.batch [ symbolCmd (SymbolT.SelectSymbols newComponents)
                                        wireCmd (BusWireT.SelectWires newWires)
                                        sheetCmd msg]
                                        
        | Connection connId ->
            let aSegL = BusWireUpdateHelpers.getClickedSegment model.Wire connId mMsg.Pos
            let segIdL = aSegL |> List.map (fun aSeg -> aSeg.Segment.GetId)
            let connIdL = segIdL |> List.map snd
            let msg = DoNothing

            if model.CtrlKeyDown
            then
                let newWires =
                    if List.contains connId model.SelectedWires
                    then List.filter (fun cId -> cId <> connId) model.SelectedWires // If component selected was already in the list, remove it
                    else connId :: model.SelectedWires // If user clicked on a new component add it to the selected list

                match model.ErrorComponents with
                | [] -> 
                    { model with
                        SelectedWires = newWires;
                        Action = Idle;
                        TmpModel = Some model;
                        PrevWireSelection = model.SelectedWires},
                    Cmd.batch [wireCmd (BusWireT.SelectWires newWires); sheetCmd msg]
                | _ -> 
                        //printfn "Error components (Right)"
                        {model with Action = DragAndDrop}, 
                        Cmd.batch [sheetCmd DoNothing]
            else
                let snapXY = getNewSegmentSnapInfo model aSegL
                {model with 
                        SelectedComponents = []; 
                        SelectedWires = [ connId ]; 
                        SnapSegments = snapXY
                        Action = MovingWire segIdL; 
                        TmpModel = Some model},
                    Cmd.batch [ symbolCmd (SymbolT.SelectSymbols [])
                                wireCmd (BusWireT.SelectWires [ connId ])
                                wireCmd (BusWireT.DragSegment (segIdL, mMsg))
                                wireCmd (BusWireT.ResetJumps connIdL )
                                sheetCmd msg]
        | Canvas ->
            let newComponents, newWires =
                if model.CtrlKeyDown
                then model.SelectedComponents, model.SelectedWires //do not deselect if in CtrlKeyDown mode
                else [], []
            // Start Creating Selection Box and Reset Selected Components
            let initialiseSelection = 
                {model.DragToSelectBox with TopLeft= {X=mMsg.Pos.X; Y=mMsg.Pos.Y}}
            match model.CtrlKeyDown with
            | true ->
                match model.ErrorComponents with
                | [] -> 
                    {model with DragToSelectBox = initialiseSelection; Action = Selecting; SelectedComponents = newComponents; SelectedWires = newWires },
                    Cmd.batch [ symbolCmd (SymbolT.SelectSymbols newComponents)
                                wireCmd (BusWireT.SelectWires newWires) ]
                | _ -> 
                        //printfn "Error components (Right)"
                        {model with Action = DragAndDrop}, 
                        Cmd.none

            | false ->
                match model.ScalingBox with
                | None ->
                    {model with DragToSelectBox = initialiseSelection; Action = Selecting; SelectedComponents = newComponents; SelectedWires = newWires},
                    Cmd.batch [ symbolCmd (SymbolT.SelectSymbols newComponents)
                                wireCmd (BusWireT.SelectWires newWires) ]
                | _ ->
                    match model.ErrorComponents with
                    | [] -> 
                        //printfn "No error components (Wrong)"
                        {model with DragToSelectBox = initialiseSelection; Action = Selecting; SelectedComponents = newComponents; SelectedWires = newWires},
                        Cmd.batch [ symbolCmd (SymbolT.SelectSymbols newComponents)
                                    wireCmd (BusWireT.SelectWires newWires)]
                    | _ -> 
                        //printfn "Error components (Right)"
                        {model with Action = DragAndDrop}, 
                        Cmd.batch [sheetCmd DoNothing]

/// Mouse Drag Update, can be: drag-to-selecting, moving symbols, connecting wire between ports.
let mDragUpdate 
        (model: Model) 
        (mMsg: MouseT) 
            : Model * Cmd<ModelType.Msg> =
    let setDragCursor (model:Model, cmd: Cmd<ModelType.Msg>) : Model*Cmd<ModelType.Msg> =
        let dragCursor = 
            match model.Action with
            | MovingLabel -> Grabbing
            | MovingSymbols -> ClickablePort
            | _ -> model.CursorType
        {model with CursorType = dragCursor}, cmd
    match model.Action with
    | MovingWire segIdL -> 
        snapWire model mMsg segIdL
    // HLP 23: AUTHOR Khoury & Ismagilov
    // New Action, when we click on scaling button and drag the components and box should scale with mouse
    | Scaling ->
        let modelBeforeUpdate = model
        let scalingBoxCentre:XYPos = model.ScalingBoxCentrePos
        let newScalingBoxOppositeMouse = 
            {X = scalingBoxCentre.X - (mMsg.Pos.X - scalingBoxCentre.X);
             Y = scalingBoxCentre.Y - (mMsg.Pos.Y - scalingBoxCentre.Y)}
        
        // printfn " mousePos:%A" mMsg.Pos
        // printfn " newScalingBoxOppositeMouse:%A" newScalingBoxOppositeMouse

        let newBBMin = 
            {X = min (newScalingBoxOppositeMouse.X) (mMsg.Pos.X)  + 50.;
             Y = min (newScalingBoxOppositeMouse.Y) (mMsg.Pos.Y)  + 50.}
        let newBBMax = 
            {X = max (newScalingBoxOppositeMouse.X) (mMsg.Pos.X)  - 50.;
             Y = max (newScalingBoxOppositeMouse.Y) (mMsg.Pos.Y)  - 50.}
        
        let selectedSymbols = RotateScale.findSelectedSymbols (modelBeforeUpdate.SelectedComponents) (modelBeforeUpdate.Wire.Symbol)

        let xYSC = RotateScale.getScalingFactorAndOffsetCentreGroup newBBMin newBBMax selectedSymbols
        let scaleSymFunc = RotateScale.scaleSymbol xYSC
        let newSymModel = RotateScale.groupNewSelectedSymsModel (modelBeforeUpdate.SelectedComponents) (modelBeforeUpdate.Wire.Symbol) selectedSymbols scaleSymFunc
        let newModel = {{model with Wire = {model.Wire with Symbol = newSymModel}} with BoundingBoxes = Symbol.getBoundingBoxes {model with Wire = {model.Wire with Symbol = newSymModel}}.Wire.Symbol}

        let newSelectedSymbols = RotateScale.findSelectedSymbols (newModel.SelectedComponents) (newModel.Wire.Symbol)

        let oneCompBoundsBothEdges = RotateScale.oneCompBoundsBothEdges newSelectedSymbols

        let errorComponents =
            modelBeforeUpdate.SelectedComponents
            |> List.filter (fun sId -> not (notIntersectingComponents newModel newModel.BoundingBoxes[sId] sId))
        let errorSelectedComponents =
            modelBeforeUpdate.SelectedComponents
            |> List.filter (fun sId -> not (notIntersectingSelectedComponents newModel newModel.BoundingBoxes[sId] sId))

        let staySameModel = 
            if errorSelectedComponents<>[] then (Some modelBeforeUpdate)
            elif (oneCompBoundsBothEdges && model.ScalingTmpModel.IsSome) then (modelBeforeUpdate.ScalingTmpModel)
            else None
        
        let scalingTmpModel = 
            match oneCompBoundsBothEdges, modelBeforeUpdate.ScalingTmpModel.IsNone, errorSelectedComponents<>[] with 
            | true, true, _ -> None
            | false, _, false -> Some newModel
            | _ -> modelBeforeUpdate.ScalingTmpModel
        

        if (staySameModel.IsSome) then
            //printfn "scaling stay same"
            {staySameModel.Value with
                        ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.ScreenMovement}
                        ScalingTmpModel = scalingTmpModel;
                        }, 
                    Cmd.batch [
                        sheetCmd CheckAutomaticScrolling
                        wireCmd (BusWireT.UpdateConnectedWires staySameModel.Value.SelectedComponents)
                        sheetCmd UpdateBoundingBoxes
                    ]
        else
        //printfn "scaling not same"
        {newModel with
                    ScalingTmpModel = scalingTmpModel
                    ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.ScreenMovement}
                    ErrorComponents = errorComponents},
        Cmd.batch [ 
            symbolCmd (SymbolT.ErrorSymbols (errorComponents,newModel.SelectedComponents,false))
            sheetCmd CheckAutomaticScrolling
            wireCmd (BusWireT.UpdateConnectedWires model.SelectedComponents)
            sheetCmd UpdateBoundingBoxes
        ]
    | Selecting ->
        let initialX = model.DragToSelectBox.TopLeft.X
        let initialY = model.DragToSelectBox.TopLeft.Y
        let newDragToSelectBox = {model.DragToSelectBox with W = (mMsg.Pos.X - initialX); H = (mMsg.Pos.Y - initialY)}
        {model with DragToSelectBox = newDragToSelectBox
                    ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.ScreenMovement}
                    LastMousePos = mMsg.Pos
         }, sheetCmd CheckAutomaticScrolling
    | InitialiseMovingLabel compId->
        {model with
                Action = MovingLabel
                LastMousePos = mMsg.Pos
                ScrollingLastMousePos = {Pos = mMsg.Pos; Move = mMsg.ScreenMovement}
                SelectedLabel = Some compId
                TmpModel = Some model
            }, sheetCmd DoNothing
    | InitialiseMoving _ ->
        let newModel, cmd = moveSymbols model mMsg
        {newModel with TmpModel = Some newModel}, Cmd.batch [cmd]


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
        , sheetCmd CheckAutomaticScrolling
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
        , sheetCmd CheckAutomaticScrolling

    | MovingPort portId->
        model, symbolCmd (SymbolT.MovePort (portId, mMsg.Pos))
    // HLP23 AUTHOR: BRYAN TAN
    | ResizingSymbol (compId, fixedCornerLoc) ->
        let bBox = model.BoundingBoxes[compId]
        let errorComponents  =
            if notIntersectingComponents model bBox compId then [] else [compId]
        {model with ErrorComponents = errorComponents},
        Cmd.batch [
            symbolCmd (SymbolT.ResizeSymbol (compId, fixedCornerLoc, mMsg.Pos))
            sheetCmd (UpdateSingleBoundingBox compId)
            symbolCmd (ErrorSymbols (errorComponents,[compId],false))
            wireCmd (BusWireT.UpdateSymbolWires compId);]
        
    | Panning initPos->
        let sPos = initPos - mMsg.ScreenPage
        model, sheetCmd (Msg.UpdateScrollPos sPos)
    | Idle 
    | InitialisedCreateComponent _ 
    | Scrolling -> model, Cmd.none
    |> setDragCursor


/// Mouse Up Update, can have: finished drag-to-select, pressed on a component, finished symbol movement, connected a wire between ports
let mUpUpdate (model: Model) (mMsg: MouseT) : Model * Cmd<ModelType.Msg> = // mMsg is currently un-used, but kept for future possibilities
    let newModel =
        match model.TmpModel with
        | None -> model
        | Some newModel -> {newModel with SelectedComponents = model.SelectedComponents}
    //printfn "mUpUpdate with action: %A" model.Action
    match model.Action with
    | MovingWire segIdL ->
        let connIdL = segIdL |> List.map snd
        let coalesceCmds = connIdL |> List.map (fun conn -> wireCmd (BusWireT.CoalesceWire conn))
        { model with Action = Idle; UndoList = appendUndoList model.UndoList newModel}, 
        Cmd.batch ([ wireCmd (BusWireT.DragSegment (segIdL, mMsg))                    
                     wireCmd (BusWireT.MakeJumps (true,connIdL )) ] @ coalesceCmds)
    | Selecting ->
        //let box = model.DragToSelectBox
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
        // HLP 23: AUTHOR Khoury & Ismagilov
        { model with 
            DragToSelectBox = resetDragToSelectBox; 
            Action = Idle; SelectedComponents = selectComps; 
            SelectedWires = selectWires; 
            AutomaticScrolling = false },
        Cmd.batch [ symbolCmd (SymbolT.SelectSymbols selectComps)
                    sheetCmd DoNothing
                    wireCmd (BusWireT.SelectWires selectWires)]

    | InitialiseMoving compId -> 
            // not sure there is any point to running this from mouse UP this now we are not altering selection?
            // legacy case due for removal?
            { model with Action = Idle}, wireCmd (BusWireT.SelectWires [])

    | InitialiseMovingLabel compId ->
        { model with Action = Idle; SelectedLabel = Some compId},
        sheetCmd DoNothing

    | MovingLabel ->
        {model with Action = Idle; UndoList = appendUndoList model.UndoList newModel}, sheetCmd DoNothing

    | Scaling -> 
        let outputModel = 
            match model.ErrorComponents with
            |[] -> model
            | _ -> newModel 
        {outputModel with Action = Idle; UndoList = appendUndoList model.UndoList newModel}, sheetCmd DoNothing

    | MovingSymbols ->
        // Reset Movement State in Model
        match model.ErrorComponents with
        | [] ->
            let movingWires = BusWireUpdateHelpers.getConnectedWireIds model.Wire model.SelectedComponents
            match model.SelectedComponents.Length with
                | s when s < 2 -> 
                    {model with
                        BoundingBoxes = model.LastValidBoundingBoxes
                        Action = Idle
                        UndoList = appendUndoList model.UndoList newModel
                        SnapSymbols = emptySnap
                        SnapSegments = emptySnap
                        AutomaticScrolling = false },
                            wireCmd (BusWireT.MakeJumps (true,movingWires))
                | _ -> 
                    {model with
                        ErrorComponents = [];
                        BoundingBoxes = model.LastValidBoundingBoxes;
                        Action = Idle;
                        UndoList = appendUndoList model.UndoList newModel;
                        SnapSymbols = emptySnap;
                        SnapSegments = emptySnap;
                        AutomaticScrolling = false },
                    Cmd.batch [ //symbolCmd (SymbolT.MoveSymbols (model.SelectedComponents, (model.LastValidPos - mMsg.Pos)))
                                symbolCmd (SymbolT.SelectSymbols (model.SelectedComponents))
                                //wireCmd (BusWireT.UpdateWires (model.SelectedComponents, model.LastValidPos - mMsg.Pos))
                                wireCmd (BusWireT.MakeJumps (true,movingWires))
                                sheetCmd DoNothing]
            

        | _ ->
            let movingWires = BusWireUpdateHelpers.getConnectedWireIds model.Wire model.SelectedComponents
            {model with
                BoundingBoxes = model.LastValidBoundingBoxes;
                Action = Idle;
                SnapSymbols = emptySnap;
                SnapSegments = emptySnap;
                AutomaticScrolling = false },
            Cmd.batch [ symbolCmd (SymbolT.MoveSymbols (model.SelectedComponents, (model.LastValidPos - mMsg.Pos)))
                        sheetCmd UpdateBoundingBoxes
                        symbolCmd (SymbolT.SelectSymbols (model.SelectedComponents))
                        wireCmd (BusWireT.UpdateWires (model.SelectedComponents, model.LastValidPos - mMsg.Pos))
                        wireCmd (BusWireT.MakeJumps (true,movingWires)) ]

    | ConnectingInput inputPortId ->
        let cmd, undoList ,redoList =
            if model.TargetPortId <> "" // If a target has been found, connect a wire\
            then wireCmd (BusWireT.AddWire (inputPortId, (OutputPortId model.TargetPortId))),
                            appendUndoList model.UndoList newModel, newModel.RedoList
            else Cmd.none , newModel.UndoList, newModel.RedoList
        {model with Action = Idle; TargetPortId = ""; UndoList = undoList ; RedoList = redoList ; AutomaticScrolling = false }, cmd

    | ConnectingOutput outputPortId ->
        let cmd , undoList , redoList =
            if model.TargetPortId <> "" // If a target has been found, connect a wire
            then  wireCmd (BusWireT.AddWire (InputPortId model.TargetPortId, outputPortId)),
                            appendUndoList model.UndoList newModel, newModel.RedoList
            else Cmd.none , newModel.UndoList, newModel.RedoList
        { model with Action = Idle; TargetPortId = ""; UndoList = undoList ; RedoList = redoList ; AutomaticScrolling = false  }, cmd

    | MovingPort portId ->
        let symbol = getCompId model.Wire.Symbol portId
        {model with Action = Idle},
        Cmd.batch [
            symbolCmd (SymbolT.MovePortDone (portId, mMsg.Pos))
            wireCmd (BusWireT.UpdateSymbolWires symbol);
            wireCmd (BusWireT.RerouteWire portId)]

    // HLP23 AUTHOR: BRYAN TAN
    | ResizingSymbol (compId, fixedCornerLoc) -> 
        match model.ErrorComponents with 
        | [] ->
            {model with Action = Idle; LastValidSymbol = None; UndoList = appendUndoList model.UndoList newModel},
            Cmd.batch [
                symbolCmd (SymbolT.ResizeSymbolDone (compId, None, fixedCornerLoc, mMsg.Pos))
                sheetCmd UpdateBoundingBoxes
                wireCmd (BusWireT.UpdateSymbolWires compId);]
        | _ ->
            {model with
                BoundingBoxes = model.LastValidBoundingBoxes
                Action = Idle
                SnapSymbols = emptySnap
                SnapSegments = emptySnap
                AutomaticScrolling = false 
                LastValidSymbol = None
            },
            Cmd.batch [ 
                symbolCmd (SymbolT.ResizeSymbolDone (compId, model.LastValidSymbol, fixedCornerLoc, mMsg.Pos))
                sheetCmd UpdateBoundingBoxes
                symbolCmd (SymbolT.SelectSymbols (model.SelectedComponents))
                wireCmd (BusWireT.UpdateSymbolWires compId);
            ]
    | _ -> model, Cmd.batch [sheetCmd DoNothing]

/// Mouse Move Update, looks for nearby components and looks if mouse is on a port
let mMoveUpdate 
        (model: Model) 
        (mMsg: MouseT) 
            : Model * Cmd<ModelType.Msg> =
    match model.Action with
    | DragAndDrop ->
        moveSymbols model mMsg

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
        Cmd.batch [ sheetCmd UpdateBoundingBoxes
                    symbolCmd (SymbolT.SelectSymbols [])
                    symbolCmd (SymbolT.PasteSymbols [ newCompId ]) ]
    | _ ->
        let nearbyComponents = findNearbyComponents model mMsg.Pos 50 // TODO Group Stage: Make this more efficient, update less often etc, make a counter?
        
        // HLP23 AUTHOR: BRYAN TAN
        let ctrlPressed = List.exists (fun (k,_) -> k = "CONTROL") (SheetDisplay.getActivePressedKeys model)
        let newCursor =
            match model.CursorType, model.Action with
            | Spinner,_ -> Spinner
            | _ ->
                match mouseOn { model with NearbyComponents = nearbyComponents } mMsg.Pos with // model.NearbyComponents can be outdated e.g. if symbols have been deleted -> send with updated nearbyComponents.
                | InputPort (_, p) | OutputPort (_, p) -> ClickablePort // Change cursor if on port
                // | InputPort _ | OutputPort _ -> ClickablePort // Change cursor if on port
                | Label _ -> GrabLabel
                | Connection _ -> GrabWire
                | Component compId -> 
                    match model.Wire.Symbol.Symbols[compId].Annotation with 
                    | Some ScaleButton -> ResizeNESW
                    | _ -> GrabSymbol
                | ComponentCorner (_,_,idx) when ctrlPressed -> 
                    match (idx % 2) with
                    | 0 -> ResizeNWSE
                    | _ -> ResizeNESW
                | _ -> Default
        let newModel = {
            model with
                NearbyComponents = nearbyComponents;
                CursorType = newCursor;
                LastMousePos = mMsg.Pos;
                ScrollingLastMousePos = {Pos=mMsg.Pos; Move=mMsg.ScreenMovement} } 
        if ctrlPressed then
            newModel , Cmd.batch [symbolCmd (SymbolT.ShowCustomOnlyPorts nearbyComponents); symbolCmd (SymbolT.ShowCustomCorners nearbyComponents)]
        else 
            newModel, symbolCmd (SymbolT.ShowPorts nearbyComponents) // Show Ports of nearbyComponents

let getVisibleScreenCentre (model : Model) : XYPos =
    let canvas = document.getElementById "Canvas"
    {
        X = (canvas.scrollLeft + canvas.clientWidth / 2.0) / model.Zoom
        Y = (canvas.scrollTop + canvas.clientHeight / 2.0) / model.Zoom
    }

let validateTwoSelectedSymbols (model:Model) =
        match model.SelectedComponents with
        | [s1; s2] as syms -> 
            let symbols = model.Wire.Symbol.Symbols
            let getSym sId = 
                Map.tryFind sId symbols
            match getSym s1, getSym s2 with
            | Some s1, Some s2 -> 
                printfn $"Testing with\ns1= {s1.Component.Type}\n s2={s2.Component.Type}"
                Some(s1,s2)
            | _ -> 
                printfn "Error: can't validate the two symbols selected to reorder ports"
                None
        | syms -> 
            printfn $"Can't test because number of selected symbols ({syms.Length}) is not 2"
            None

/// Geometric helper used for testing. Probably needs a better name, and to be collected with other
/// This should perhaps be generalised for all orientations and made a helper function.
/// However different testing may be needed, so who knows?
/// Return the vertical channel between two bounding boxes, if they do not intersect and
/// their vertical coordinates overlap.
let rec getChannel (bb1:BoundingBox) (bb2:BoundingBox) : (BoundingBox * Orientation) option =
    if bb1.TopLeft.X > bb2.TopLeft.X then
        getChannel bb2 bb1
    else
        if overlap2DBox bb1 bb2 then
            None // boxes intersect, invalid channel
        elif (bb1.TopLeft.Y > bb2.TopLeft.Y + bb2.H || bb1.TopLeft.Y + bb1.H < bb2.TopLeft.Y)
             && bb2.TopLeft.X > bb1.TopLeft.X + bb1.W then
            None // symbols are not aligned vertically
        // Vertical Channel
        elif bb2.TopLeft.X > bb1.TopLeft.X + bb1.W then
            let x1, x2 = bb1.TopLeft.X + bb1.W, bb2.TopLeft.X 
            let union = boxUnion bb1 bb2
            let topLeft = { Y=union.TopLeft.Y; X=x1 }
            Some ( { TopLeft = topLeft; H = union.H; W = x2 - x1 }, Vertical )
        // Horizontal Channel
        else
            let union = boxUnion bb1 bb2
            if bb1.TopLeft.Y + bb1.H < bb2.TopLeft.Y then // bb2 below bb1
                let y1, y2 = bb1.TopLeft.Y + bb1.H, bb2.TopLeft.Y
                let topLeft = { X = bb1.TopLeft.X; Y = bb1.TopLeft.Y + bb1.H }
                Some ( { TopLeft = topLeft; H = y2 - y1; W = union.W }, Horizontal )
            else // bb2 above bb1
                let y1, y2 = bb1.TopLeft.Y, bb2.TopLeft.Y + bb2.H
                let topLeft = { X = union.TopLeft.X; Y = bb2.TopLeft.Y + bb2.H }
                Some ( { TopLeft = topLeft; H = y1 - y2; W = union.W }, Horizontal )
