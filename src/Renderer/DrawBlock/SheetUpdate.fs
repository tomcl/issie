module SheetUpdate

open CommonTypes
open Browser
open Elmish
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
open Fable.Core
open Fable.Core.JsInterop
open BuildUartHelpers
open Node.ChildProcess
open Node
open SmartHelpers

module node = Node.Api

importReadUart 

open Symbol

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
        let wiresConnectedToComponents = BusWireUpdateHelpers.getConnectedWireIds model.Wire model.SelectedComponents
        // Ensure there are no duplicate deletions by using a Set
        let wireUnion =
            Set.union (Set.ofList wiresConnectedToComponents) (Set.ofList model.SelectedWires)
            |> Set.toList

        // let inputPorts, outputPorts = BusWire.getPortIdsOfWires model.Wire wireUnion
        match model.ButtonList with
        | [] -> 
            { model with SelectedComponents = []; SelectedWires = []; UndoList = appendUndoList model.UndoList model ; RedoList = [] },
            Cmd.batch [ wireCmd (BusWireT.DeleteWires wireUnion) // Delete Wires before components so nothing bad happens
                        symbolCmd (SymbolT.DeleteSymbols model.SelectedComponents)
                        Cmd.ofMsg UpdateBoundingBoxes
                    ]
        | _ -> 
            { model with SelectedComponents = []; SelectedWires = []; UndoList = appendUndoList model.UndoList model ; RedoList = []; ButtonList = []; Box = {model.Box with ShowBox = false} },
            Cmd.batch [ wireCmd (BusWireT.DeleteWires wireUnion) // Delete Wires before components so nothing bad happens
                        symbolCmd (SymbolT.DeleteSymbols model.SelectedComponents)
                        symbolCmd (SymbolT.DeleteSymbols model.ButtonList)
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
        let newSymbolModel, pastedCompIds = SymbolUpdate.pasteSymbols model.Wire.Symbol model.Wire.Wires model.LastMousePos // Symbol has Copied Symbols stored
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
                
    // HLP 23: AUTHOR Khoury & Ismagilov
    // Gets bounding box dimentions and creates the necessary symbol buttons
    | DrawBox ->
        let box: BoundingBox = 
                    match model.SelectedComponents.Length with
                     | s when s<2 -> model.Box.BoxBound
                     | _  -> 
                                let myList =  model.SelectedComponents
                                                            |> List.map (fun id -> Map.find id model.Wire.Symbol.Symbols)
                                myList |> getBlock
        if (model.SelectedComponents.Length < 2) then 
            (model), Cmd.none
        else

            let topleftX =  box.TopLeft.X 
            let topleftY =  box.TopLeft.Y
            let width = box.W
            let height= box.H
            let createdrotateACWSym = {(createNewSymbolButton {X=topleftX- 69.5; Y=topleftY+((height/2. )- 12.5)} RotateButton  "RotateACW" Distinctive) with SymbolT.STransform = {Rotation=Degree90 ; flipped=false}}
            let rotateACWSym = {createdrotateACWSym with Component = {createdrotateACWSym.Component with H = 25.; W=25.}}
            let createdrotateCWSym = createNewSymbolButton {X=topleftX+width+ 57.; Y=topleftY+((height/2.)-12.5)} RotateButton  "RotateCW" Distinctive
            let rotateCWSym = {createdrotateCWSym with Component = {createdrotateCWSym.Component with H = 25.; W= 25.}}
            let buttonSym = createNewSymbolButton {X=topleftX+width+ 46.5; Y=topleftY-53.5} ScaleButton "ScaleButton"  Distinctive
            let newSymbolMap = model.Wire.Symbol.Symbols |> Map.add buttonSym.Id buttonSym |> Map.add rotateACWSym.Id rotateACWSym |> Map.add rotateCWSym.Id rotateCWSym
            ({model with Box = {model.Box with BoxBound = box; ShowBox = true; ScaleButton = Some buttonSym; RotateCWButton= Some rotateCWSym; RotateACWButton= Some rotateACWSym}; Wire= {model.Wire with Symbol = {model.Wire.Symbol with Symbols = newSymbolMap}}; ButtonList = [buttonSym.Id; rotateACWSym.Id; rotateCWSym.Id]}), Cmd.none
            //Center and Radius might be unused here ^

            
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
        //printfn $"Keys={newPressedKeys}, Key={key}"
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
        // model,
        // Cmd.batch [
        //     symbolCmd (SymbolT.RotateLeft(model.SelectedComponents, rotation)) // Better to have Symbol keep track of clipboard as symbols can get deleted before pasting.
        //     wireCmd (BusWireT.UpdateConnectedWires model.SelectedComponents)
        //     Cmd.ofMsg SheetT.UpdateBoundingBoxes
        // ]

        //Replaced normal rotation, so individual and block rotation is correct
        //HLP23: Author Ismagilov
        let rotmodel = 
            {model with Wire = {model.Wire with Symbol = (SmartRotate.rotateBlock model.SelectedComponents model.Wire.Symbol rotation)}}

        let newModel = 
         match (model.SelectedComponents.Length) with
         | s when s < 2 -> {rotmodel with BoundingBoxes = getBoundingBoxes rotmodel.Wire.Symbol; }
         | _ -> 
            let block = SmartHelpers.getBlock (List.map (fun sId -> rotmodel.Wire.Symbol.Symbols.[sId]) model.SelectedComponents)
            let newBoxPos = adjustPosForBlockRotation rotation rotmodel.Box.BoxBound.H rotmodel.Box.BoxBound.W (rotatePointAboutBlockCentre rotmodel.Box.BoxBound.TopLeft (block.Centre()) rotation)
            let newBox = {rotmodel.Box with BoxBound = {rotmodel.Box.BoxBound with TopLeft = newBoxPos; H = rotmodel.Box.BoxBound.W; W = rotmodel.Box.BoxBound.H}}
            match model.ButtonList with
            | [] ->  {rotmodel with BoundingBoxes = getBoundingBoxes rotmodel.Wire.Symbol; Box = newBox}
            | _ ->
                let symButton =  rotmodel.Box.ScaleButton.Value
                let rotateACWButton =  rotmodel.Box.RotateACWButton.Value
                let rotateCWButton =  rotmodel.Box.RotateCWButton.Value

                let newRotACWPos = {X = newBox.BoxBound.TopLeft.X - 76.5  ; Y = newBox.BoxBound.TopLeft.Y + ((newBox.BoxBound.H/2.)- 12.5) }
                let rotACWNewButton = {rotateACWButton with Pos = newRotACWPos; Component = {rotateACWButton.Component with X = newRotACWPos.X; Y = newRotACWPos.Y}}

                let newSymPos = {X = newBox.BoxBound.TopLeft.X + newBox.BoxBound.W + 39.5 ; Y = newBox.BoxBound.TopLeft.Y - 60.5 }
                let symNewButton = {symButton with Pos = newSymPos; Component = {symButton.Component with X = newSymPos.X; Y = newSymPos.Y}}

                let newRotCWPos = {X = newBox.BoxBound.TopLeft.X + newBox.BoxBound.W + 50.  ; Y = newBox.BoxBound.TopLeft.Y + ((newBox.BoxBound.H/2.) - 12.5) }
                let rotCWNewButton = {rotateCWButton with Pos = newRotCWPos; Component = {rotateCWButton.Component with X = newRotCWPos.X; Y = newRotCWPos.Y}}

                let newSymModel = {rotmodel.Wire.Symbol with Symbols = (rotmodel.Wire.Symbol.Symbols |> Map.add symButton.Id symNewButton |> Map.add rotateACWButton.Id rotACWNewButton |> Map.add rotateCWButton.Id rotCWNewButton)}

                let tmpmodel = 
                    {rotmodel with Box = {newBox with ScaleButton = Some symNewButton; RotateCWButton= Some rotCWNewButton; RotateACWButton= Some rotACWNewButton}; Wire = {rotmodel.Wire with Symbol = newSymModel}}
                {tmpmodel with BoundingBoxes = getBoundingBoxes tmpmodel.Wire.Symbol; }
        printfn "ButtonIds: %A" model.ButtonList
        let errorComponents =
            newModel.SelectedComponents
            |> List.filter (fun sId -> not (notIntersectingComponents newModel newModel.BoundingBoxes[sId] sId))

        printfn $"ErrorComponents={errorComponents}"

        
        match errorComponents with
            | [] -> 
                 match model.ButtonList with
                 | [] ->
                    {newModel with ErrorComponents = errorComponents; Action = Idle;},
                        Cmd.batch [
                            symbolCmd (SymbolT.ErrorSymbols (errorComponents,newModel.SelectedComponents,false))
                            wireCmd (BusWireT.UpdateConnectedWires newModel.SelectedComponents)
                            Cmd.ofMsg DrawBox
                            Cmd.ofMsg SheetT.UpdateBoundingBoxes
                        ]
                 | _ -> 
                    {newModel with ErrorComponents = errorComponents; Action = Idle;},
                        Cmd.batch [
                            symbolCmd (SymbolT.ErrorSymbols (errorComponents,newModel.SelectedComponents,false))
                            wireCmd (BusWireT.UpdateConnectedWires newModel.SelectedComponents)
                            Cmd.ofMsg SheetT.UpdateBoundingBoxes
                        ]
            | _ ->
                {newModel with ErrorComponents = errorComponents; Action = DragAndDrop; ButtonList = []; Box = {model.Box with ShowBox = false}},
                    Cmd.batch [
                        symbolCmd (SymbolT.DeleteSymbols model.ButtonList)
                        symbolCmd (SymbolT.ErrorSymbols (errorComponents,newModel.SelectedComponents,false))
                        wireCmd (BusWireT.UpdateConnectedWires newModel.SelectedComponents)
                        Cmd.ofMsg SheetT.UpdateBoundingBoxes
                    ]

    

    | Flip orientation ->
        // model,
        // Cmd.batch [
        //     symbolCmd (SymbolT.Flip(model.SelectedComponents,orientation)) // Better to have Symbol keep track of clipboard as symbols can get deleted before pasting.
        //     wireCmd (BusWireT.UpdateConnectedWires model.SelectedComponents)
        //     Cmd.ofMsg SheetT.UpdateBoundingBoxes
        // ]

        //Replaced normal flipping, so individual and block flipping is correct
        //HLP23: Author Ismagilov
        let flipmodel = 
            {model with Wire = {model.Wire with Symbol = (SmartRotate.flipBlock model.SelectedComponents model.Wire.Symbol orientation)}}

        let newModel = {flipmodel with BoundingBoxes = getBoundingBoxes flipmodel.Wire.Symbol}

        let errorComponents =
            newModel.SelectedComponents
            |> List.filter (fun sId -> not (notIntersectingComponents newModel newModel.BoundingBoxes[sId] sId))

        printfn $"ErrorComponents={errorComponents}"

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
            Cmd.ofMsg (ColourSelection([], newWires, HighLightColor.SkyBlue)); 
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
        }, Cmd.ofMsg (StartCompilationStage (Synthesis, path, name, profile))
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

            let pcf,deviceType,devicePackage =
                match model.DebugDevice, profile with
                |Some "IceStick",Verilog.Release -> 
                    $"{include_path}/icestick.pcf", "--hx1k", "tq144"
                
                |Some "IceStick",Verilog.Debug -> 
                    $"{include_path}/icestick_debug.pcf", "--hx1k", "tq144"
                
                |Some "IssieStick-v0.1", Verilog.Release -> 
                    $"{include_path}/issiestick-0.1.pcf", "--hx4k", "tq144"
                
                |Some "IssieStick-v0.1", Verilog.Debug -> 
                    $"{include_path}/issiestick-0.1_debug.pcf", "--hx4k", "tq144"
                
                |Some "IssieStick-v1.0", Verilog.Release -> 
                    $"{include_path}/issiestick-1.0.pcf", "--hx8k", "bg121"
                
                |Some "IssieStick-v1.0", Verilog.Debug -> 
                    $"{include_path}/issiestick-1.0_debug.pcf", "--hx8k", "bg121"
                
                |_,_ -> failwithf "Undefined device used in compilation!"
            
            let (prog, args) = 
                // make build dir
                match stage with
                | Synthesis     -> "yosys", ["-p"; $"read_verilog -I{include_path} {path}/{name}.v; synth_ice40 -flatten -json {path}/build/{name}.json"]//"sh", ["-c"; "sleep 4 && echo 'finished synthesis'"]
                | PlaceAndRoute -> "nextpnr-ice40", ["--package"; $"{devicePackage}"; $"{deviceType}"; "--pcf"; $"{pcf}"; "--json"; $"{path}/build/{name}.json"; "--asc"; $"{path}/build/{name}.asc"]//"sh", ["-c"; "sleep 5 && echo 'finisheded pnr'"]
                | Generate      -> "icepack", [$"{path}/build/{name}.asc"; $"{path}/build/{name}.bin"]//"sh", ["-c"; "sleep 3 && echo 'generated stuff'"]
                | Upload        -> "iceprog", [$"{path}/build/{name}.bin"]//"sh", ["-c"; "sleep 2 && echo 'it is alive'"]

            let options = {| shell = false |} |> toPlainJsObj
            let child = node.childProcess.spawn (prog, args |> ResizeArray, options);
            //printfn "child pid: %A" child.pid

            let startComp dispatch =
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
                        dispatch <| TickCompilation child.pid
                finally
                    printf "Child finished with exit code: %i" exit_code.Value
                    if exit_code.Value = 0 then
                        dispatch <| FinishedCompilationStage
                        match stage with
                        | Synthesis -> dispatch <| StartCompilationStage (PlaceAndRoute, path, name, profile)
                        | PlaceAndRoute -> dispatch <| StartCompilationStage (Generate, path, name, profile)
                        | Generate -> dispatch <| StartCompilationStage (Upload, path, name, profile)
                        | Upload when profile = Verilog.Debug-> dispatch <| DebugConnect
                        | _ -> ()
                    else
                        dispatch <| StopCompilation
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
        
        model, Cmd.ofMsg (DebugStepAndRead viewerNo)
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
                        dispatch <| (OnDebugRead (hextoInt (reading[0].ToString() + reading[1].ToString()),i))
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
                        dispatch <| (OnDebugRead (hextoInt (reading[0].ToString() + reading[1].ToString()),i))
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
                    dispatch <| (DebugRead viewers)
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
            |0 -> (Array.length model.DebugMappings) / 8 
            |_ ->  (Array.length model.DebugMappings) / 8 + 1
        
        
        {model with DebugState = Paused}, Cmd.ofMsg (DebugStepAndRead viewerNo)
    | SetDebugDevice device ->
        {model with DebugDevice = Some device}, Cmd.none
    | KeyPress CtrlR ->
        // Test code called from Edit menu item
        // Validate the lits of selected symbols: it muts have just 2 for
        // the test to work.
         validateTwoSelectedSymbols model
         |> function
            | Some (s1,s2) ->
                {model with Wire = SmartPortOrder.reOrderPorts model.Wire s1 s2}, Cmd.none
            | None -> 
                printfn "Error: can't validate the two symbols selected to reorder ports"
                model, Cmd.none

    ///HLP23 AUTHORS: Ismagilov & Rzepala
    /// updated the following function to restrict resized symbol movement, so that
    /// there is no overlap with other symbols
    | TestPortPosition ->
        // Test code called from Edit menu item
        // Validate the lits of selected symbols: it muts have just 2 for
        // the test to work.
         validateTwoSelectedSymbols model
         |> function
            | Some (s1,s2) ->
                
                let scalemodel = 
                    if s1<> s2 then 
                        {model with Wire = SmartSizeSymbol.reSizeSymbol model.Wire s1 s2}
                    else {model with Wire = SmartSizeSymbol.reSizeSymbolDraggable model.Wire s1}
                    
                let newModel = {scalemodel with BoundingBoxes = getBoundingBoxes scalemodel.Wire.Symbol}
                let errorComponents =
                    newModel.SelectedComponents
                    |> List.filter (fun sId -> not (notIntersectingComponents newModel newModel.BoundingBoxes[sId] sId))

                let errorSelectedComponents =
                    newModel.SelectedComponents
                    |> List.filter (fun sId -> not (notIntersectingSelectedComponents newModel newModel.BoundingBoxes[sId] sId))

                printfn $"ErrorComponents={errorComponents}"

                let nextAction = 
                    match errorComponents with
                        | [] -> 
                            Idle
                        | _ ->
                            DragAndDrop

                match errorSelectedComponents with
                    | [] ->
                        {newModel with ErrorComponents = errorComponents; Action = nextAction}, 
                        Cmd.batch [
                                symbolCmd (SymbolT.ErrorSymbols (errorComponents,newModel.SelectedComponents,false))
                                wireCmd (BusWireT.UpdateConnectedWires newModel.SelectedComponents)
                                Cmd.ofMsg SheetT.UpdateBoundingBoxes
                        ]
                    | _ -> model,Cmd.none
            | None -> 
                printfn "Error: can't validate the two symbols selected to reorder ports"
                model, Cmd.none

    | KeyPress CtrlT ->
        // Added test code to test multiple components
        // Must select 2 or more componenets for it to work
        //HLP23 AUTHOR: Khoury
         validateMultipleSelectedSymbols model
         |> function
            | Some s -> 
                {model with Wire = SmartPortOrder.multipleReorderPorts model.Wire s}, Cmd.none
            | None -> 
                printfn "Error: can't validate two or more symbols selected to reorder ports"
                model, Cmd.none
    | KeyPress CtrlL ->
        //Creates a replace wire with label popup
        //HLP23 AUTHOR: Klapper
        let filteredWireList = model.SelectedWires 
                            |> List.filter (fun element -> SmartHelpers.isWireConnectedToLabel model.Wire model.Wire.Wires[element] |> not)
        match filteredWireList with
        | hd::[] -> {model with Wire = (SmartHelpers.wireReplacePopUp model.Wire hd); UndoList = model.UndoList @ [model]}, Cmd.none 
        | hd::tl -> {model with Wire = (SmartHelpers.wireReplaceAllPopup model.Wire filteredWireList); UndoList = model.UndoList @ [model]}, Cmd.none
        | _ -> 
            printfn "No valid wires have been selected"
            model, Cmd.none
    | KeyPress CtrlShiftL -> 
        //Replaces all wires with labels without popup
        //HLP23 AUTHOR: Klapper
        let filteredWireList = model.SelectedWires 
                            |> List.map (fun element -> model.Wire.Wires[element])
                            |> List.filter (fun element -> SmartHelpers.isWireConnectedToLabel model.Wire element |> not)
        match filteredWireList with
        | hd::[] -> {model with Wire = SmartHelpers.replaceWireWithLabel model.Wire hd; UndoList = model.UndoList @ [model]}, Cmd.none
        | hd::tl -> 
            let newWireModel = (model.Wire, filteredWireList)
                                ||> List.fold (fun state element -> SmartHelpers.replaceWireWithLabel state element)
            {model with Wire = newWireModel; UndoList = model.UndoList @ [model]}, Cmd.none
        | _ -> 
            printfn "No valid wires have been selected"
            model, Cmd.none
    | FormSmartChannel (orient:Orientation)->
        //Creates a smart channel in the orientation specified
        //HLP23 AUTHOR: Klapper
        let filteredComps =
             model.SelectedComponents 
            |> List.filter (fun id -> model.Wire.Symbol.Symbols[id].Component.Type <> ComponentType.IOLabel)
        match filteredComps with
        | lst when lst.Length >= 2 ->
            findChannel model -1 -1 orient lst 
            |> function
                | Some l, Some r -> 
                    let lhd::ltl = l |> List.map (fun id -> model.BoundingBoxes[id])
                    let lBB = (lhd, ltl) ||> List.fold boxUnion
                    let rhd::rtl = r |> List.map (fun id -> model.BoundingBoxes[id])
                    let rBB = (rhd, rtl) ||> List.fold boxUnion
                    getChannel lBB rBB orient
                    |> function
                            | None ->
                                printfn "Symbols are not oriented for a vertical channel"
                                model, Cmd.none
                            | Some channel ->
                                    let newModel =  SmartChannel.smartChannelRoute orient channel {model with SelectedWires = []}
                                    if newModel.SelectedWires.Length = 0 then
                                        {newModel with UndoList = model.UndoList @ [model]}, Cmd.ofMsg (SheetT.ColourSelection <| (l @ r , [], HighLightColor.Yellow))
                                    else
                                        {newModel with UndoList = model.UndoList @ [model]}, Cmd.batch [Cmd.ofMsg (KeyPress CtrlL);Cmd.ofMsg (SheetT.ColourSelection <| (l @ r , [], HighLightColor.Yellow))]
                | _, _ -> 
                    printfn "Could not autoform channel from selected components"
                    model, Cmd.none
        | _ ->
            printfn "Please select two or more symbols"
            model, Cmd.none
    


    // HLP23 AUTHOR Ismagilov
    // Caught message for scaling up selected components
    | KeyPress CtrlU -> 
        printfn "Scaling up"
        match model.SelectedComponents.Length with
        | n when n<2 -> model, Cmd.none
        | _ ->  
            let scalemodel = 
                {model with Wire = {model.Wire with Symbol = (SmartRotate.scaleBlock model.SelectedComponents model.Wire.Symbol ScaleUp)}}

            let newModel = {scalemodel with BoundingBoxes = getBoundingBoxes scalemodel.Wire.Symbol}

            let errorComponents =
                newModel.SelectedComponents
                |> List.filter (fun sId -> not (notIntersectingComponents newModel newModel.BoundingBoxes[sId] sId))

            printfn $"ErrorComponents={errorComponents}"

            let nextAction = 
                match errorComponents with
                    | [] -> 
                        Idle
                        
                    | _ ->
                        DragAndDrop
            {newModel with ErrorComponents = errorComponents; Action = nextAction}, 
            Cmd.batch [
                    symbolCmd (SymbolT.ErrorSymbols (errorComponents,newModel.SelectedComponents,false))
                    wireCmd (BusWireT.UpdateConnectedWires newModel.SelectedComponents)
                    Cmd.ofMsg SheetT.UpdateBoundingBoxes
            ]


    //HLP23 AUTHOR Ismagilov
    //Caught message for scaling down selected components
    | KeyPress CtrlI -> 
        printfn "Scaling down"
        match model.SelectedComponents.Length with
        | n when n<2 -> model, Cmd.none
        | _ ->  
            let scalemodel = 
                {model with Wire = {model.Wire with Symbol = (SmartRotate.scaleBlock model.SelectedComponents model.Wire.Symbol ScaleDown)}}

            let newModel = {scalemodel with BoundingBoxes = getBoundingBoxes scalemodel.Wire.Symbol}

            let errorComponents =
                newModel.SelectedComponents
                |> List.filter (fun sId -> not (notIntersectingComponents newModel newModel.BoundingBoxes[sId] sId))

            let errorSelectedComponents =
                newModel.SelectedComponents
                |> List.filter (fun sId -> not (notIntersectingSelectedComponents newModel newModel.BoundingBoxes[sId] sId))

            printfn $"ErrorComponents={errorComponents}"

            let nextAction = 
                match errorComponents with
                    | [] -> 
                        Idle
                    | _ ->
                        DragAndDrop

            match errorSelectedComponents with
                | [] ->
                    {newModel with ErrorComponents = errorComponents; Action = nextAction}, 
                    Cmd.batch [
                            symbolCmd (SymbolT.ErrorSymbols (errorComponents,newModel.SelectedComponents,false))
                            wireCmd (BusWireT.UpdateConnectedWires newModel.SelectedComponents)
                            Cmd.ofMsg SheetT.UpdateBoundingBoxes
                    ]
                | _ -> model,Cmd.none

    //HLP23 AUTHOR Ismagilov
    //Sets the chosen style for the selected components
    | SetStyle (style:StyleType) ->
        let syms = model.Wire.Symbol.Symbols
        let compList = 
            syms
            |> Map.toList
            |> List.map (fun (key, _) -> key)

        let resetSymbols = 
            syms
            |> Map.map 
                (fun _ sym ->  
                    {sym with Appearance = {sym.Appearance with Style=style}}
                )
        {model with Wire = {model.Wire with Symbol = {model.Wire.Symbol with Style = style; Symbols = resetSymbols}}; },
        Cmd.batch  [ 
                wireCmd (BusWireT.UpdateConnectedWires compList)
                Cmd.ofMsg UpdateBoundingBoxes
        ]
    | TestAllTogether orientation ->
        //HLP23 AUTHOR: Klapper
        //Executes port reorderin and channel formation right after each other
        validateTwoSelectedSymbols model
         |> function 
            | Some (s1, s2) -> 
                model, Cmd.batch [ 
                                        (Cmd.ofMsg (KeyPress CtrlR)) 
                                        (Cmd.ofMsg (FormSmartChannel orientation))]
            | None -> 
                model, Cmd.batch [ 
                                        (Cmd.ofMsg (KeyPress CtrlT)) 
                                        (Cmd.ofMsg (FormSmartChannel orientation))]
        

    

    | ToggleNet _ | DoNothing | _ -> model, Cmd.none
    |> Optic.map fst_ postUpdateChecks


/// Init function
let init () =
    let wireModel, cmds = (BusWireUpdate.init ())
    let boundingBoxes = Symbol.getBoundingBoxes wireModel.Symbol
    {
        Wire = wireModel
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
        Compiling = false
        CompilationStatus = {Synthesis = Queued; PlaceAndRoute = Queued; Generate = Queued; Upload = Queued}
        CompilationProcess = None
        DebugState = NotDebugging
        DebugData = [1..256] |> List.map (fun i -> 0b00111011)
        DebugIsConnected = false
        DebugMappings = [||]
        DebugDevice = None
        Box = {MovingPos= [];
                WidthStart=0.;
                StartingMouse = {X=0;Y=0};
                HeightStart=0.;
                TopLeftStart= {X=0;Y=0};
                StartingPos = {X=0;Y=0};
                ShowBox = false ;
                BoxBound = {TopLeft = {X=0.0; Y=0.0}; H=0.0; W=0.0};
                ScaleButton = None; 
                RotateCWButton = None;
                RotateACWButton = None;}
        ButtonList =[]
    }, Cmd.none


