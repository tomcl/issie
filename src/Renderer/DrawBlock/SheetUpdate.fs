module SheetUpdate

open CommonTypes
open Browser
open Elmish
open Fable.React
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open Sheet
open SheetSnap
open SheetDisplay
open Optics
open FilesIO
open FSharp.Core
open Fable.Core.JsInterop
open BuildUartHelpers
open Node

module node = Node.Api

importReadUart

/// Update Function
let update (msg : Msg) (issieModel : ModelType.Model): ModelType.Model*Cmd<ModelType.Msg> =
    /// In this module model = Sheet model
    let model = issieModel.Sheet
    let model =
        match model.LastCursorType <> model.CursorType with
        | false -> {model with LastCursorType = model.CursorType} // always update this
        | true -> model
        |> (fun model ->
            let scrollPos = getScrollProps()
            match scrollPos with
            | Some scroll when scroll <> model.ScreenScrollPos ->
                {model with ScreenScrollPos = scroll}
            | _ -> model)
                
    

    /// check things that might not have been correctly completed in the last update and if so do them
    /// Mostly this is a hack to deal with the fact that dependent state is held separately rather than
    /// being derived fucntionally from the state it depends on, so it must be explicitly updated.
    /// TODO: add something to check whether wires need updating
    let postUpdateChecks (model:Model, cmd:Cmd<ModelType.Msg> ) = 
        // Executed every update so performance is important.
        // Since normally state will be correct it is only necessary to make the checking
        // fast.
        let start = TimeHelpers.getTimeMs()
        let sModel = Optic.get symbol_ model
        let inputModel =
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
        (inputModel, cmd)
        |> RotateScale.postUpdateScalingBox
        // |> (fun currentmodel -> {currentmodel with TmpModel = Some currentmodel})

    match msg with
    | Wire (BusWireT.Symbol SymbolT.Msg.UpdateBoundingBoxes) -> 
        // Symbol cannot directly send a message to Sheet box Sheet message type is out of scape. This
        // is used so that a symbol message can be intercepted by sheet and used there.
        model, Cmd.batch [
                sheetCmd UpdateBoundingBoxes; 
                ]
    | Wire wMsg ->
        let wModel, (wCmd) = BusWireUpdate.update wMsg issieModel
        { model with Wire = wModel.Sheet.Wire }, wCmd
    | ToggleGrid ->
        {model with ShowGrid = not model.ShowGrid}, Cmd.none
    | KeyPress DEL ->
        let wiresConnectedToComponents = BusWireUpdateHelpers.getConnectedWireIds model.Wire model.SelectedComponents
        // Ensure there are no duplicate deletions by using a Set
        let wireUnion =
            Set.union (Set.ofList wiresConnectedToComponents) (Set.ofList model.SelectedWires)
            |> Set.toList

        // let inputPorts, outputPorts = BusWire.getPortIdsOfWires model.Wire wireUnion
        { model with SelectedComponents = []; SelectedWires = []; UndoList = appendUndoList model.UndoList model; RedoList = [] },
        Cmd.batch [ wireCmd (BusWireT.DeleteWires wireUnion) // Delete Wires before components so nothing bad happens
                    symbolCmd (SymbolT.DeleteSymbols model.SelectedComponents)
                    sheetCmd UpdateBoundingBoxes
                  ]
    | KeyPress CtrlS -> // For Demo, Add a new square in upper left corner
        { model with BoundingBoxes = Symbol.getBoundingBoxes model.Wire.Symbol; UndoList = appendUndoList model.UndoList model ; RedoList = []},
        Cmd.batch [ sheetCmd UpdateBoundingBoxes; symbolCmd SymbolT.SaveSymbols ] // Need to update bounding boxes after adding a symbol.
    // HLP 23: AUTHOR Khoury & Ismagilov
    // Gets bounding box dimentions and creates the necessary symbol buttons

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
        let newSymbolModel, pastedCompIds = SymbolUpdate.pasteSymbols model.Wire.Symbol model.Wire.Wires model.LastMousePos // Symbol has Copied Symbols stored
        let newBusWireModel, pastedConnIds = BusWireUpdate.pasteWires { model.Wire with Symbol = newSymbolModel } pastedCompIds

        { model with Wire = newBusWireModel
                     SelectedComponents = pastedCompIds
                     SelectedWires = pastedConnIds
                     TmpModel = Some model
                     Action = DragAndDrop },
        Cmd.batch [ sheetCmd UpdateBoundingBoxes
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
            Cmd.batch [ symbolCmd (SymbolT.DeleteSymbols (model.SelectedComponents))
                        wireCmd (BusWireT.DeleteWires model.SelectedWires)
                        sheetCmd UpdateBoundingBoxes
                      ]
        | _ -> model, Cmd.none

    | KeyPress CtrlZ ->
        match model.UndoList with
        | [] -> model , Cmd.none
        | prevModel :: lst ->
            let appendRedoList (redoList: Model List) (model_in: Model): Model List =
                let rec removeLast inputLst =
                    inputLst
                    |> List.truncate (max 0 (inputLst.Length - 1))
    
                match List.length redoList with
                |n when n < 500 -> model_in :: redoList
                | _ -> model_in :: (removeLast redoList)

            {prevModel with RedoList = appendRedoList model.RedoList model; UndoList = lst; CurrentKeyPresses = []}, Cmd.batch [sheetCmd DoNothing]

    | KeyPress CtrlY ->
        match model.RedoList with
        | [] -> model , Cmd.none
        | newModel :: lst -> { newModel with UndoList = model :: model.UndoList; RedoList = lst; CurrentKeyPresses = []} , Cmd.batch [sheetCmd DoNothing]

    | KeyPress CtrlA ->
        let symbols = model.Wire.Symbol.Symbols |> Map.toList |> List.map fst
        let wires = model.Wire.Wires |> Map.toList |> List.map fst
        { model with
            SelectedComponents = symbols
            SelectedWires = wires
        } , Cmd.batch [ symbolCmd (SymbolT.SelectSymbols symbols)
                        wireCmd (BusWireT.SelectWires wires) ]

    | KeyPress CtrlW ->
        fitCircuitToScreenUpdate model
    
    | PortMovementStart ->
        match model.Action with
        | Idle -> 
            {model with CtrlKeyDown = true}, 
            Cmd.batch 
                [
                    symbolCmd (SymbolT.ShowCustomOnlyPorts model.NearbyComponents)
                    symbolCmd (SymbolT.ShowCustomCorners model.NearbyComponents)
                ]
        | _ -> model, Cmd.none

    | PortMovementEnd ->
        match model.Action with
        | Idle -> 
            {model with CtrlKeyDown = false}, 
            Cmd.batch 
                [
                    symbolCmd (SymbolT.ShowPorts model.NearbyComponents)
                    symbolCmd (SymbolT.HideCustomCorners model.NearbyComponents)
                ]
        | _ -> {model with CtrlKeyDown = false}, Cmd.none

    | MouseMsgOrig(mEv, mouseOp, headerHeight) ->
        
        let mMsg:MouseT = {
                Op = mouseOp ;
                ShiftKeyDown = mEv.shiftKey
                ScreenMovement = {X= mEv.movementX;Y=mEv.movementY}
                ScreenPage = {X=mEv.pageX; Y=mEv.pageY}
                Pos = getDrawBlockPos mEv headerHeight model
                }
        // Mouse Update Functions can be found above, update function got very messy otherwise
        let mouseAlreadyDown = match model.Action with | MovingPort _ | ConnectingInput _ | ConnectingOutput _ -> true |_ -> false
        match mMsg.Op with
        | Down when mouseAlreadyDown = true -> model, Cmd.none
        | Down -> mDownUpdate model mMsg
        | Drag -> 
            mDragUpdate model mMsg
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

    | UpdateScrollPosFromCanvas(pos) ->
        {model with ScreenScrollPos = pos}, Cmd.none

 
    | UpdateScrollPos scrollPos ->
        let scrollDif = scrollPos - model.ScreenScrollPos * (1. / model.Zoom)
        let newLastScrollingPos =
            {
                Pos = model.ScrollingLastMousePos.Pos + scrollDif
                Move = model.ScrollingLastMousePos.Move
            }
        let cmd =
            if model.AutomaticScrolling then
                sheetCmd CheckAutomaticScrolling // Also check if there is automatic scrolling to continue
            else
                Cmd.none
        // keep last 4 updates to filter corresponding OnScroll events
        recentProgrammaticScrollPos <- scrollPos :: List.truncate 4 recentProgrammaticScrollPos
        scrollSequence <- scrollSequence + 1 // increment sequence counter
        viewIsAfterUpdateScroll <- true
        writeCanvasScroll scrollPos |> ignore
        { model with 
            ScreenScrollPos = scrollPos
            ScrollingLastMousePos = newLastScrollingPos }, 
            cmd

    | AddNotConnected (ldcs, port, pos, rotation) ->
        let (newSymModel, ncID) = SymbolUpdate.addSymbol ldcs model.Wire.Symbol pos NotConnected ""
        let ncPortId = newSymModel.Symbols[ncID].Component.InputPorts[0].Id
        // add a newly created wire to the model
        // then send BusWidths message which will re-infer bus widths
        // the new wires (extarcted as connections) are not added back into Issie model. 
        // This happens on save or when starting a simulation (I think)
        let newWireModel, msgOpt = BusWireUpdate.newWire (InputPortId ncPortId) (OutputPortId port.Id) {model.Wire with Symbol = newSymModel}
        let wCmd = if msgOpt.IsSome then wireCmd (Option.get msgOpt) else Cmd.none
            
        {model with Wire = newWireModel}, Cmd.batch [wCmd;
                                    symbolCmd (RotateAntiClockAng ([ncID], rotation));
                                    wireCmd (UpdateConnectedWires [ncID]);
                                    sheetCmd (UpdateBoundingBoxes)]

    // Zooming in increases model.Zoom. The centre of the screen will stay centred (if possible)
    | KeyPress ZoomIn ->
        let oldScreenCentre = getVisibleScreenCentre model
        { model with Zoom = min Constants.maxMagnification (model.Zoom*Constants.zoomIncrement) }, 
        sheetCmd (KeepZoomCentered oldScreenCentre)

    // Zooming out decreases the model.Zoom. The centre of the screen will stay centred (if possible)
    | KeyPress ZoomOut ->
        // get current screen edge coords
        match getScreenEdgeCoords model with
        | Some edge ->
            //Check if the new zoom will exceed the canvas width or height
            let newZoom =
                let minXZoom = (edge.Right - edge.Left) / model.CanvasSize
                let minYZoom = (edge.Top - edge.Bottom) / model.CanvasSize
                List.max [model.Zoom / Constants.zoomIncrement; minXZoom; minYZoom]
            let oldScreenCentre = getVisibleScreenCentre model

            { model with Zoom = newZoom }, 
            sheetCmd (KeepZoomCentered oldScreenCentre)
        | None-> model, Cmd.none

    | KeepZoomCentered oldScreenCentre ->
        let canvas = document.getElementById "Canvas"
        let newScreenCentre = getVisibleScreenCentre model
        let requiredOffset = oldScreenCentre - newScreenCentre

        // Update screen so that the zoom is centred around the middle of the screen.
        printf "KeepZoomCentred"
        canvas.scrollLeft <- canvas.scrollLeft + requiredOffset.X * model.Zoom
        canvas.scrollTop <- canvas.scrollTop + requiredOffset.Y * model.Zoom
        model, Cmd.none

    | ManualKeyDown key -> // Needed for e.g. Ctrl + C and Ctrl + V as they are not picked up by Electron
        let containsKey key  = List.exists (fun (key',time) -> key'=key)
        let newPressedKeys = (key.ToUpper(), TimeHelpers.getTimeMs()) :: getActivePressedKeys model  // Make it fully upper case to remove CAPS dependency
        let newCmd =
            match containsKey "CONTROL" newPressedKeys || containsKey "META" newPressedKeys with
            | true ->
                if containsKey "C" newPressedKeys then
                    sheetCmd (KeyPress CtrlC)
                elif containsKey "V" newPressedKeys then
                    sheetCmd (KeyPress CtrlV)
                elif containsKey "A" newPressedKeys then
                    sheetCmd (KeyPress CtrlA)
                elif containsKey "W" newPressedKeys then
                    sheetCmd (KeyPress CtrlW)
                else
                    Cmd.none
            | false -> Cmd.none


        { model with CurrentKeyPresses = newPressedKeys }, newCmd

    | ManualKeyUp key ->
        /// remove all (key,timestamp) elements matching key
        let removeAllKeys key =
            List.filter (fun (k,t) -> k <> key)
        { model with CurrentKeyPresses = removeAllKeys (key.ToUpper()) model.CurrentKeyPresses}, Cmd.none

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
            then
                //printf "automaticScrolling adjustment..."
                scrollSpeed * (scrollMargin - edgeDistance) / scrollMargin // Speed should be faster the closer the mouse is to the screen edge
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
            let defaultMsg = {Pos = newMPos; Op = Move;  ScreenMovement = zero; ScreenPage=zero; ShiftKeyDown=false}
            let outputModel, outputCmd =
                match model.Action with
                | DragAndDrop ->
                    mMoveUpdate { model with AutomaticScrolling = true } {defaultMsg with Op = Move}                             
                | MovingSymbols | ConnectingInput _ | ConnectingOutput _ | Selecting ->
                    mDragUpdate { model with AutomaticScrolling = true } {defaultMsg with Op = Drag}                               
                | _ ->
                    { model with AutomaticScrolling = true }, Cmd.none
            let notAutomaticScrolling msg = match msg with | ModelType.Sheet CheckAutomaticScrolling -> false | _ -> true
            // Don't want to go into an infinite loop (program would crash), don't check for automatic scrolling immediately (let it be handled by OnScroll listener).
            let filteredOutputCmd = Cmd.map (fun msg ->
                if notAutomaticScrolling msg then msg else ModelType.Sheet DoNothing) outputCmd
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
        //Replaced normal rotation, so individual and block rotation is correct
        //HLP23: Author Ismagilov
        // printfn "Running Rotate %A" rotation
        let rotmodel = 
            {model with Wire = {model.Wire with Symbol = (RotateScale.rotateBlock model.SelectedComponents model.Wire.Symbol rotation)}
                        TmpModel = Some model
                        UndoList = appendUndoList model.UndoList model}

        let newModel = {rotmodel with BoundingBoxes = Symbol.getBoundingBoxes rotmodel.Wire.Symbol}
         
        let errorComponents =
            newModel.SelectedComponents
            |> List.filter (fun sId -> not (notIntersectingComponents newModel newModel.BoundingBoxes[sId] sId))
       
        match errorComponents with
            | [] -> 
                {newModel with ErrorComponents = errorComponents; Action = Idle},
                        Cmd.batch [
                            symbolCmd (SymbolT.ErrorSymbols (errorComponents,newModel.SelectedComponents,false))
                            wireCmd (BusWireT.UpdateConnectedWires newModel.SelectedComponents)
                            sheetCmd SheetT.UpdateBoundingBoxes]
            | _ ->
                {newModel with ErrorComponents = errorComponents; Action = DragAndDrop},
                    Cmd.batch [
                        symbolCmd (SymbolT.ErrorSymbols (errorComponents,newModel.SelectedComponents,false))
                        wireCmd (BusWireT.UpdateConnectedWires newModel.SelectedComponents)
                        sheetCmd SheetT.UpdateBoundingBoxes]

    

    //HLP23: Author Ismagilov
    | Flip orientation ->
        let flipmodel = 
            {model with Wire = {model.Wire with Symbol = (RotateScale.flipBlock model.SelectedComponents model.Wire.Symbol orientation)}
                        TmpModel = Some model
                        UndoList = appendUndoList model.UndoList model}

        let newModel = {flipmodel with BoundingBoxes = Symbol.getBoundingBoxes flipmodel.Wire.Symbol}

        let errorComponents =
            newModel.SelectedComponents
            |> List.filter (fun sId -> not (notIntersectingComponents newModel newModel.BoundingBoxes[sId] sId))

        let nextAction = 
            match errorComponents with
                | [] -> 
                    newModel.Action
                | _ ->
                    DragAndDrop

        {newModel with ErrorComponents = errorComponents; Action = nextAction},
        Cmd.batch [
            symbolCmd (SymbolT.ErrorSymbols (errorComponents,newModel.SelectedComponents,false))
            wireCmd (BusWireT.UpdateConnectedWires newModel.SelectedComponents)
            sheetCmd SheetT.UpdateBoundingBoxes
        ]

    | SaveSymbols ->
        model, symbolCmd SymbolT.SaveSymbols

    | WireType Jump ->
        let wires = model.Wire.Wires |> Map.toList |> List.map fst
        model,
        Cmd.batch [
            wireCmd (BusWireT.UpdateWireDisplayType BusWireT.Jump)
            wireCmd (BusWireT.MakeJumps (false,wires))
        ]

    | WireType Radiussed ->
        let wires = model.Wire.Wires |> Map.toList |> List.map fst
        model,
        Cmd.batch [
            wireCmd (BusWireT.UpdateWireDisplayType BusWireT.Radial)
            wireCmd (BusWireT.MakeJumps (false,wires))
        ]
       
    | WireType Modern ->
        let wires = model.Wire.Wires |> Map.toList |> List.map fst
        model,
        Cmd.batch [
            wireCmd (BusWireT.UpdateWireDisplayType BusWireT.Modern)
            wireCmd (BusWireT.MakeJumps (false,wires))
        ]
                
    // ---------------------------- Issie Messages ---------------------------- //

    | InitialiseCreateComponent (ldcs, compType, lbl) ->
        match compType with
        | IsGate -> 
            { model with
                Action = (InitialisedCreateComponent (ldcs, compType, lbl));
                UndoList = appendUndoList model.UndoList model;
                TmpModel = Some model;
                Wire.Symbol.HintPane = Some [
                    "You can change the number of inputs"                                                                       
                    "in the component properties menu"
                    ]
            }, Cmd.none
        | NoGate ->
            { model with Action = (InitialisedCreateComponent (ldcs, compType, lbl)) ; TmpModel = Some model; UndoList = appendUndoList model.UndoList model}, Cmd.none
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
            CurrentKeyPresses = []
            UndoList = []
            RedoList = []
            TmpModel = None
            ScalingTmpModel = None
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
            sheetCmd (ColourSelection([], newWires, HighLightColor.SkyBlue)); 
            wireCmd (BusWireT.SelectWires newWires)]
    | SetSpinner isOn ->
        if isOn then {model with CursorType = Spinner}, Cmd.none
        else {model with CursorType = Default}, Cmd.none

    | StartCompiling (path, name, profile) ->
        printfn "starting compiling %s :: %s" path name
        {model with
            Compiling = true
            CompilationStatus = {
                Synthesis = InProgress 0
                PlaceAndRoute = Queued
                Generate = Queued
                Upload = Queued
            }
            DebugIsConnected = false
            DebugState = match profile with | Verilog.Debug -> Paused | Verilog.Release -> NotDebugging
        }, sheetCmd (StartCompilationStage (Synthesis, path, name, profile))
    | StartCompilationStage (stage, path, name, profile) ->
        printfn "are we compiling? %A" model.Compiling
        printfn "do we have process? %A" (model.CompilationProcess |> Option.map (fun c -> c.pid))
        if not model.Compiling then
            model, Cmd.none
        else 
            let cwd = getCWD ()
            let include_path = 
                match JSHelpers.debugLevel <> 0 with
                |true -> cwd+"/static/hdl"
                |false -> cwd+"/resources/static/hdl" 
            
            printfn "include_path: %s" include_path

            let pcf,deviceType,devicePackage,USBdevice =
                match model.DebugDevice, profile with
                |Some "IceStick",Verilog.Release -> 
                    $"{include_path}/icestick.pcf", "--hx1k", "tq144", "i:0x0403:0x6010"
                
                |Some "IceStick",Verilog.Debug -> 
                    $"{include_path}/icestick_debug.pcf", "--hx1k", "tq144", "i:0x0403:0x6010"
                
                |Some "IssieStick-v0.1", Verilog.Release -> 
                    $"{include_path}/issiestick-0.1.pcf", "--hx4k", "tq144", "i:0x0403:0xed1c"
                
                |Some "IssieStick-v0.1", Verilog.Debug -> 
                    $"{include_path}/issiestick-0.1_debug.pcf", "--hx4k", "tq144", "i:0x0403:0xed1c"
                
                |Some "IssieStick-v1.0", Verilog.Release -> 
                    $"{include_path}/issiestick-1.0.pcf", "--hx8k", "bg121", "i:0x0403:0xed1c"
                
                |Some "IssieStick-v1.0", Verilog.Debug -> 
                    $"{include_path}/issiestick-1.0_debug.pcf", "--hx8k", "bg121", "i:0x0403:0xed1c"
                
                |_,_ -> failwithf "Undefined device used in compilation!"
            
            let (prog, args) = 
                // make build dir
                match stage with
                | Synthesis     -> "yosys", ["-p"; $"read_verilog -I{include_path} {path}/{name}.v; synth_ice40 -flatten -json {path}/build/{name}.json"]//"sh", ["-c"; "sleep 4 && echo 'finished synthesis'"]
                | PlaceAndRoute -> "nextpnr-ice40", ["--package"; $"{devicePackage}"; $"{deviceType}"; "--pcf"; $"{pcf}"; "--json"; $"{path}/build/{name}.json"; "--asc"; $"{path}/build/{name}.asc"]//"sh", ["-c"; "sleep 5 && echo 'finisheded pnr'"]
                | Generate      -> "icepack", [$"{path}/build/{name}.asc"; $"{path}/build/{name}.bin"]//"sh", ["-c"; "sleep 3 && echo 'generated stuff'"]
                | Upload        -> "iceprog", ["-d"; $"{USBdevice}"; $"{path}/build/{name}.bin"]//"sh", ["-c"; "sleep 2 && echo 'it is alive'"]

            let options = {| shell = false |} |> toPlainJsObj
            let child = node.childProcess.spawn (prog, args |> ResizeArray, options);
            //printfn "child pid: %A" child.pid

            let startComp dispatch =
                let dispatchS msg = dispatch (ModelType.Sheet msg)
                printf "starting stage %A" stage
                Async.StartImmediate(async {
                let exit_code = ref 0
                try
                    let keepGoing = ref true

                    // TODO: record data and display it in special tab
                    child.stdout.on ("data", fun _ -> ()) |> ignore
                    child.stderr.on ("data", fun e -> printfn "Error: %s" e) |> ignore
                    child.on("exit", fun code ->
                        keepGoing.Value <- false
                        exit_code.Value <- code
                    ) |> ignore

                    while keepGoing.Value do
                        do! Async.Sleep 1000
                        printf "state of child: %A" keepGoing.Value
                        dispatchS <| TickCompilation child.pid
                finally
                    printf "Child finished with exit code: %i" exit_code.Value
                    if exit_code.Value = 0 then
                        dispatchS <| FinishedCompilationStage
                        match stage with
                        | Synthesis -> dispatchS <| StartCompilationStage (PlaceAndRoute, path, name, profile)
                        | PlaceAndRoute -> dispatchS <| StartCompilationStage (Generate, path, name, profile)
                        | Generate -> dispatchS <| StartCompilationStage (Upload, path, name, profile)
                        | Upload when profile = Verilog.Debug-> dispatchS <| DebugConnect
                        | _ -> ()
                    else
                        dispatchS <| StopCompilation
                })

            {model with CompilationProcess = Some child}, Cmd.ofSub <| startComp
    | StopCompilation ->
        //printfn "stopping compilation"
        match model.CompilationProcess with
        | Some child -> child.kill()
        | _ -> ()

        let failIfInProgress stage =
            match stage with
            | InProgress t -> Failed
            | s -> s

        { model with
            Compiling = false
            CompilationStatus = {
                Synthesis = failIfInProgress model.CompilationStatus.Synthesis
                PlaceAndRoute = failIfInProgress model.CompilationStatus.PlaceAndRoute
                Generate = failIfInProgress model.CompilationStatus.Generate
                Upload = failIfInProgress model.CompilationStatus.Upload
            }
            CompilationProcess = None
        }, Cmd.none
    | TickCompilation pid ->
        //printfn "ticking %A while process is %A" pid (model.CompilationProcess |> Option.map (fun c -> c.pid))
        let correctPid =
            model.CompilationProcess
            |> Option.map (fun child -> child.pid = pid) 
            |> Option.defaultValue false

        let tick stage =
            match stage with
                | InProgress t when correctPid -> InProgress (t + 1)
                | s -> s

        {model with
            CompilationStatus = {
                Synthesis = tick model.CompilationStatus.Synthesis
                PlaceAndRoute = tick model.CompilationStatus.PlaceAndRoute
                Generate = tick model.CompilationStatus.Generate
                Upload = tick model.CompilationStatus.Upload
            }
        }, Cmd.none
    | FinishedCompilationStage ->
        let model =
            if not model.Compiling then
                model
            else
                let finishOrStart cur prev =
                    match (prev, cur) with
                        | (_, InProgress t) -> Completed t
                        | (Some (InProgress _), _) -> InProgress 0
                        | (_, cur) -> cur
                let synthesis = model.CompilationStatus.Synthesis
                let placeAndRoute = model.CompilationStatus.PlaceAndRoute
                let generate = model.CompilationStatus.Generate
                let upload = model.CompilationStatus.Upload
                let isCompiling =
                    match upload with
                    | InProgress _ -> false
                    | _ -> model.Compiling

                {model with
                    Compiling = isCompiling
                    CompilationStatus = {
                        Synthesis = finishOrStart synthesis None
                        PlaceAndRoute = finishOrStart placeAndRoute <| Some synthesis
                        Generate = finishOrStart generate <| Some placeAndRoute
                        Upload = finishOrStart upload <| Some generate
                    }
                }

        model, Cmd.none
    | DebugSingleStep ->
        //printfn "mappings: %A" model.DebugMappings
        let remainder = (Array.length model.DebugMappings) % 8
        let viewerNo = 
            match remainder with
            |0 -> (Array.length model.DebugMappings) / 8 
            |_ ->  (Array.length model.DebugMappings) / 8 + 1
        
        model, sheetCmd (DebugStepAndRead viewerNo)
    | DebugStepAndRead n ->
        //printfn "reading"
        
        let readSingleStep viewers dispatch =
            
            Async.StartImmediate(async {
            let exit_code = ref 0
            try
                let keepGoing = ref true

                let r = stepAndReadAllViewers(viewers)
                r.``then``(fun v -> 
                    v
                    |> Array.iteri (fun i reading -> 
                        //printfn "got : %s" (reading[0].ToString() + reading[1].ToString())
                        dispatch <| ModelType.Sheet (OnDebugRead (hextoInt (reading[0].ToString() + reading[1].ToString()),i))
                    ) 
                ) |> ignore
                    
                keepGoing.Value <- false
            finally
                ()
            })
        
        model, Cmd.ofSub (readSingleStep n)
    | DebugRead n ->
        //printfn "reading"
        
        let readSingleStep viewers dispatch =
            
            Async.StartImmediate(async {
            let exit_code = ref 0
            try
                let keepGoing = ref true

                let r = readAllViewers(viewers)
                r.``then``(fun v -> 
                    v
                    |> Array.iteri (fun i reading -> 
                        //printfn "got : %s" (reading[0].ToString() + reading[1].ToString())
                        dispatch <| ModelType.Sheet (OnDebugRead (hextoInt (reading[0].ToString() + reading[1].ToString()),i))
                    ) 
                ) |> ignore
                    
                keepGoing.Value <- false
            finally
                ()
            })
        
        model, Cmd.ofSub (readSingleStep n)
    | DebugConnect ->      
        let remainder = (Array.length model.DebugMappings) % 8
        let viewerNo = 
            match remainder with
            |0 -> (Array.length model.DebugMappings) / 8 
            |_ ->  (Array.length model.DebugMappings) / 8 + 1
        
        let connectAndRead viewers dispatch =
            Async.StartImmediate(async {
            let exit_code = ref 0
            try
                let keepGoing = ref true

                let c = simpleConnect()
                c.``then``(fun v -> 
                    dispatch <| ModelType.Sheet (DebugRead viewers)
                )|> ignore
                    
                keepGoing.Value <- false
            finally
                ()
            })

        { model with
            DebugIsConnected = true
        }, Cmd.ofSub (connectAndRead viewerNo)
    | DebugDisconnect ->
        printfn "Closing device"
        disconnect()

        { model with DebugIsConnected = false}, Cmd.none
    | OnDebugRead (data,whichViewer) ->
        let part = whichViewer
        let bits =
            [0..7]
            |> List.rev
            |> List.map (fun i -> Some <| (data / (pown 2 i)) % 2)
            |> List.map (fun b -> b.ToString())
            |> String.concat ","
        //printfn $"read {data} from {part} ([{bits}])"
        { model with
            DebugData = List.insertAt part data (List.removeAt part model.DebugData)
        }, Cmd.none
    | DebugUpdateMapping mappings ->
        {model with DebugMappings = mappings }, Cmd.none
    | DebugContinue ->
        //fs.writeFileSync ("/dev/ttyUSB1", "C")
        continuedOp ()
        printfn "Continued execution"
        

        {model with DebugState = Running}, Cmd.none
    | DebugPause ->
        //fs.writeFileSync ("/dev/ttyUSB1", "P")
        pause()
        printfn "Continued execution Stopped"
        let remainder = (Array.length model.DebugMappings) % 8
        let viewerNo = 
            match remainder with
            | 0 -> (Array.length model.DebugMappings) / 8 
            | _ ->  (Array.length model.DebugMappings) / 8 + 1
        
        
        {model with DebugState = Paused}, sheetCmd (DebugStepAndRead viewerNo)
    | SetDebugDevice device ->
        {model with DebugDevice = Some device}, Cmd.none
    
    | ToggleSnapToNet ->
        model, (wireCmd BusWireT.ToggleSnapToNet)

    | SheetBatch sheetMsgs -> //  execute a set of sheet messages immediately without view function
        model, Cmd.batch (List.map (ModelType.Msg.Sheet >> Cmd.ofMsg) sheetMsgs)

    | SetWireModel wModel ->
        {model with Wire = wModel}, Cmd.none
       
    | ToggleNet _ | DoNothing | _ -> model, Cmd.none
    |> postUpdateChecks
    // |> Optic.map fst_ postUpdateChecks
    // |> RotateScale.postUpdateScalingBox
    |> (fun (model, (cmd: Cmd<ModelType.Msg>)) -> {issieModel with Sheet = model}, cmd)


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
        ShowGrid = false
        LastMousePos = { X = 0.0; Y = 0.0 }
        ScalingBoxCentrePos = { X = 0.0; Y = 0.0 }
        InitMouseToScalingBoxCentre = { X = 0.0; Y = 0.0 }
        SnapSymbols=emptySnap
        SnapSegments = emptySnap
        CursorType = Default
        LastCursorType = Default
        ScreenScrollPos = { X = 0.0; Y = 0.0 }
        LastValidPos = { X = 0.0; Y = 0.0 }
        LastValidSymbol = None
        CurrentKeyPresses = []
        UndoList = []
        RedoList = []
        TmpModel = None
        ScalingTmpModel = None
        Zoom = 1.0
        CanvasSize = Constants.defaultCanvasSize
        AutomaticScrolling = false
        ScrollingLastMousePos = {Pos={ X = 0.0; Y = 0.0 }; Move={X = 0.0; Y  =0.0}}
        MouseCounter = 0
        LastMousePosForSnap = { X = 0.0; Y = 0.0 }
        CtrlKeyDown = false
        PrevWireSelection = []
        Compiling = false
        CompilationStatus = {Synthesis = Queued; PlaceAndRoute = Queued; Generate = Queued; Upload = Queued}
        CompilationProcess = None
        DebugState = NotDebugging
        DebugData = [1..256] |> List.map (fun i -> 0b00111011)
        DebugIsConnected = false
        DebugMappings = [||]
        DebugDevice = None
        ScalingBox = None
    }, (Cmd.none: Cmd<ModelType.Msg>)



