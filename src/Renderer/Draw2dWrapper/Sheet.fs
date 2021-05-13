module Sheet
open CommonTypes
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open EEEHelpers
open Helpers

///Static variables
let canvasSize = 3500.0

/// Used to keep mouse movement (AKA velocity) info as well as position
type XYPosMov = {
    Pos: XYPos
    Move: XYPos
    }

/// Used to keep track of what the mouse is on
type MouseOn =
    | InputPort of CommonTypes.InputPortId * XYPos
    | OutputPort of CommonTypes.OutputPortId * XYPos
    | Component of CommonTypes.ComponentId
    | Connection of CommonTypes.ConnectionId
    | Canvas
    
/// Keeps track of the current action that the user is doing
type CurrentAction =
    | Selecting
    | InitialiseMoving of CommonTypes.ComponentId // In case user clicks on a component and never drags the mouse then we'll have saved the component that the user clicked on to reset any multi-selection to that component only.
    | MovingSymbols
    | DragAndDrop
    | MovingWire of CommonTypes.ConnectionId // Sends mouse messages on to BusWire
    | ConnectingInput of CommonTypes.InputPortId // When trying to connect a wire from an input
    | ConnectingOutput of CommonTypes.OutputPortId // When trying to connect a wire from an output
    | Scrolling // For Automatic Scrolling by moving mouse to edge to screen
    | Idle
    // ------------------------------ Issie Actions ---------------------------- //
    | InitialisedCreateComponent of ComponentType * string

type UndoAction =
    | MoveBackSymbol of CommonTypes.ComponentId List * XYPos
    | UndoPaste of CommonTypes.ComponentId list
    
/// Used for Snap-to-Grid, keeps track of mouse coordinates when the snapping started, and the amount to un-snap in the future.
type LastSnap =
    {
        Pos: float
        SnapLength: float
    }
    
/// Used for Snap-to-Grid, keeps track of the last snap for each coordinate. None if no snapping has occurred.
type MouseSnapInfo = 
    {
        XSnap: LastSnap Option 
        YSnap: LastSnap Option
    }

/// Keeps track of what cursor to show
type CursorType =
    | Default
    | ClickablePort
    | NoCursor
with
    member this.Text() = 
        match this with
        | Default -> "default"
        | ClickablePort -> "move"
        | NoCursor -> "none"

/// Keeps track of coordinates of visual snap-to-grid indicators.
type SnapIndicator =
    {
        XLine: float Option
        YLine: float Option
    }

/// For Keyboard messages
type KeyboardMsg =
    | CtrlS | CtrlC | CtrlV | CtrlZ | CtrlY | CtrlA | CtrlW | AltC | AltV | AltZ | AltShiftZ | ZoomIn | ZoomOut | DEL | ESC

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | KeepZoomCentered of XYPos
    | MouseMsg of MouseT
    | UpdateBoundingBoxes
    | UpdateSingleBoundingBox of ComponentId
    | UpdateScrollPos of X: float * Y: float
    | ManualKeyUp of string // For manual key-press checking, e.g. CtrlC
    | ManualKeyDown of string // For manual key-press checking, e.g. CtrlC
    | CheckAutomaticScrolling
    | DoNothing
    // ------------------- Issie Interface Messages ----------------------
    | InitialiseCreateComponent of ComponentType * string // Need to initialise for drag-and-drop
    | FlushCommandStack
    | ResetModel
    | UpdateSelectedWires of ConnectionId list * bool
    | ColourSelection of compIds : ComponentId list * connIds : ConnectionId list * colour : HighLightColor
    | ToggleSelectionOpen
    | ToggleSelectionClose
    | ResetSelection
    | SetWaveSimMode of bool
    | ToggleNet of CanvasState //This message does nothing in sheet, but will be picked up by the update function
    | SelectWires of ConnectionId list


// ------------------ Helper Functions that need to be before the Model type --------------------------- //

/// Creates a command to Symbol    
let symbolCmd (msg: Symbol.Msg) = Cmd.ofMsg (Wire (BusWire.Symbol msg))

/// Creates a command to BusWire
let wireCmd (msg: BusWire.Msg) = Cmd.ofMsg (Wire msg)


type Model = {
    Wire: BusWire.Model
    BoundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>
    LastValidBoundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>
    SelectedComponents: CommonTypes.ComponentId List
    SelectedWires: CommonTypes.ConnectionId list
    NearbyComponents: CommonTypes.ComponentId list
    ErrorComponents: CommonTypes.ComponentId list
    DragToSelectBox: BoundingBox
    ConnectPortsLine: XYPos * XYPos // Visual indicator for connecting ports, defines two vertices to draw a line in-between.
    TargetPortId: string // Keeps track of if a target port has been found for connecting two wires in-between.
    Action: CurrentAction 
    ShowGrid: bool // Always true at the moment, kept in-case we want an optional grid
    LastMousePos: XYPos // For Symbol Movement
    Snap: MouseSnapInfo // For Snap-to-Grid
    SnapIndicator: SnapIndicator // For Snap-to-Grid
    CursorType: CursorType
    ScrollPos: XYPos
    LastValidPos: XYPos
    CurrentKeyPresses: Set<string> // For manual key-press checking, e.g. CtrlC
    Zoom: float
    TmpModel: Model Option
    UndoList: Model List
    RedoList: Model List
    AutomaticScrolling: bool // True if mouse is near the edge of the screen and is currently scrolling. This improved performance for manual scrolling with mouse wheel (don't check for automatic scrolling if there is no reason to)
    ScrollingLastMousePos: XYPosMov // For keeping track of mouse movement when scrolling. Can't use LastMousePos as it's used for moving symbols (won't be able to move and scroll symbols at same time)
    // Scrolling: bool // True if mouse is currently near edge and is automatically scrolling. Can't have in CurrentAction as we need both CurrentAction and Scrolling
    MouseCounter: int
    LastMousePosForSnap: XYPos
    Toggle : bool
    IsWaveSim : bool
    PrevWireSelection : ConnectionId list
    } with
    
    // ---------------------------------- Issie Interfacing functions ----------------------------- //
    
    /// Given a compType, return a label
    member this.GenerateLabel (compType: ComponentType) : string =
        // printfn "%A" this.Wire.Symbol.Symbols.Count
        Symbol.generateLabel this.Wire.Symbol compType
    
    /// Given a compId, return the corresponding component
    member this.GetComponentById (compId: ComponentId) =
        Symbol.extractComponent this.Wire.Symbol compId
        
    /// Change the label of Component specified by compId to lbl
    member this.ChangeLabel (dispatch: Dispatch<Msg>) (compId: ComponentId) (lbl: string) =
        dispatch <| (Wire (BusWire.Symbol (Symbol.ChangeLabel (compId, lbl) ) ) )
        
    /// Run Bus Width Inference check
    member this.DoBusWidthInference dispatch =
        dispatch <| (Wire (BusWire.BusWidths))
        
    /// Given a compId and a width, update the width of the Component specified by compId
    member this.ChangeWidth (dispatch: Dispatch<Msg>) (compId: ComponentId) (width: int) =
        dispatch <| (Wire (BusWire.Symbol (Symbol.ChangeNumberOfBits (compId, width) ) ) )
        this.DoBusWidthInference dispatch
        
    /// Given a compId and a LSB, update the LSB of the Component specified by compId
    member this.ChangeLSB (dispatch: Dispatch<Msg>) (compId: ComponentId) (lsb: int64) =
        dispatch <| (Wire (BusWire.Symbol (Symbol.ChangeLsb (compId, lsb) ) ) )
        
    /// Return Some string if Sheet / BusWire / Symbol has a notification, if there is none then return None
    member this.GetNotifications =
        // Currently only BusWire has notifications
        this.Wire.Notifications
        
    /// Get the current canvas state in the form of (Component list * Connection list)
    member this.GetCanvasState () =
        let compList = Symbol.extractComponents this.Wire.Symbol
        let connList = BusWire.extractConnections this.Wire
        
        compList, connList
        
    /// Clears the Undo and Redo stack of Sheet
    member this.FlushCommandStack dispatch =
        dispatch <| FlushCommandStack
        
    /// Clears the canvas, removes all components and connections
    member this.ClearCanvas dispatch =
        dispatch <| ResetModel
        dispatch <| (Wire BusWire.ResetModel)
        dispatch <| (Wire (BusWire.Symbol (Symbol.ResetModel ) ) )
        
    /// Loads components into Symbol model
    member this.LoadComponents dispatch components = 
        dispatch <| (Wire (BusWire.Symbol (Symbol.LoadComponents components ) ) )
        dispatch <| UpdateBoundingBoxes
        
    /// Loads components into BusWire model
    member this.LoadConnections dispatch connections =
        dispatch <| (Wire (BusWire.LoadConnections connections))        
        
    /// Returns a list of selected components
    member this.GetSelectedComponents =
        this.SelectedComponents
        |> List.map (Symbol.extractComponent this.Wire.Symbol)
        
    /// Returns a list of selected connections
    member this.GetSelectedConnections =
        this.SelectedWires
        |> List.map (BusWire.extractConnection this.Wire)
        
    /// Returns a list of selected components and connections in the form of (Component list * Connection list)
    member this.GetSelectedCanvasState =
        this.GetSelectedComponents, this.GetSelectedConnections
        
    /// Given a list of connIds, select those connections
    member this.SelectConnections dispatch on connIds =
        dispatch <| UpdateSelectedWires (connIds, on)
        
    /// Update the memory of component specified by connId at location addr with data value
    member this.WriteMemoryLine dispatch connId addr value =
        dispatch <| (Wire (BusWire.Symbol (Symbol.WriteMemoryLine (connId, addr, value))))

// ---------------------------- CONSTANTS ----------------------------- //
let gridSize = 30.0 // Size of each square grid
let snapMargin = gridSize / 25.0 // How strongly snap-to-grid snaps to the grid, small value so that there is not excessive snapping when moving symbols
let unSnapMargin = gridSize / 5.0 // How much movement there needs to be for un-snapping 


// ------------------------------------------- Helper Functions ------------------------------------------- //

//Calculates the symmetric difference of two lists, returning a list of the given type
let symDiff lst1 lst2 =
    let a = Set.ofList lst1
    let b = Set.ofList lst2
    (a - b) + (b - a)
    |> Set.toList

/// Calculates the change in coordinates of two XYPos
let posDiff (a: XYPos) (b: XYPos) = {X=a.X-b.X; Y=a.Y-b.Y}

let getScreenEdgeCoords () = 
    let canvas = document.getElementById "Canvas"
    let wholeApp = document.getElementById "WholeApp"
    let rightSelection = document.getElementById "RightSelection"
    let leftScreenEdge = canvas.scrollLeft
    let rightScreenEdge = leftScreenEdge + wholeApp.clientWidth - rightSelection.clientWidth
    (leftScreenEdge, rightScreenEdge)
    
/// Checks if pos is inside any of the bounding boxes of the components in boundingBoxes
let insideBox (boundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>) (pos: XYPos) : CommonTypes.ComponentId Option =
    let insideOneBox _ boundingBox =
        let {BoundingBox.X=xBox; Y=yBox; H=hBox; W=wBox} = boundingBox
        pos.X >= xBox && pos.X <= xBox + wBox && pos.Y >= yBox && pos.Y <= yBox + hBox
    
    boundingBoxes
    |> Map.tryFindKey insideOneBox // If there are multiple components overlapping (should not happen), return first one found
    
/// Calculates if two bounding boxes intersect by comparing corner coordinates of each box
let boxesIntersect (box1: BoundingBox) (box2: BoundingBox) =
    // Requires min and max since H & W can be negative, i.e. we don't know which corner is which automatically
    // Boxes intersect if there is overlap in both x and y coordinates 
    min box1.X (box1.X + box1.W) < max box2.X (box2.X + box2.W)
    && min box2.X (box2.X + box2.W) < max box1.X (box1.X + box1.W)
    && min box1.Y (box1.Y + box1.H) < max box2.Y (box2.Y + box2.H)
    && min box2.Y (box2.Y + box2.H) < max box1.Y (box1.Y + box1.H)
    
/// Finds all components that touch a bounding box (which is usually the drag-to-select box)
let findIntersectingComponents (model: Model) (box1: BoundingBox) =
    model.BoundingBoxes
    |> Map.filter (fun _ boundingBox -> boxesIntersect boundingBox box1)
    |> Map.toList 
    |> List.map fst

let posAdd (pos : XYPos) (a : float, b : float) : XYPos =
    {X = pos.X + a; Y = pos.Y + b}
    
/// Finds all components (that are stored in the Sheet model) near pos
let findNearbyComponents (model: Model) (pos: XYPos) =
    List.allPairs [-50.0 .. 10.0 .. 50.0] [-50.0 .. 10.0 .. 50.0] // Larger Increments -> More Efficient. But can miss small components then.
    |> List.map ((fun x -> posAdd pos x) >> insideBox model.BoundingBoxes)
    |> List.collect ((function | Some x -> [x] | _ -> []))
    
/// Checks if pos is inside any of the ports in portList    
let mouseOnPort portList (pos: XYPos) (margin: float) =
    let radius = 5.0

    let insidePortCircle (pos: XYPos) (portLocation: XYPos): bool =        
        let distance = ((pos.X - portLocation.X) ** 2.0 + (pos.Y - portLocation.Y) ** 2.0) ** 0.5
        distance <= radius + margin

        
    match List.tryFind (fun (_, portLocation) -> insidePortCircle pos portLocation) portList with // + 2.5 margin to make it a bit easier to click on, maybe it's due to the stroke width?
    | Some (portId, portLocation) -> Some (portId, portLocation)
    | None -> None

/// Returns the ports of all model.NearbyComponents
let findNearbyPorts (model: Model) =
    let inputPortsMap, outputPortsMap = Symbol.getPortLocations model.Wire.Symbol model.NearbyComponents
    
    (inputPortsMap, outputPortsMap) ||> (fun x y -> (Map.toList x), (Map.toList y))

/// Returns what is located at pos
/// Priority Order: InputPort -> OutputPort -> Component -> Wire -> Canvas
let mouseOn (model: Model) (pos: XYPos) : MouseOn =
    let inputPorts, outputPorts = findNearbyPorts model

    //TODO FIX THIS - QUICK FIX TO MAKE WORK, NOT IDEAL
    //The ports/wires are being loaded in the correct place but the detection is not working 
    //Something is wrong with the mouse coordinates somewhere, might be caused by zoom? not sure
    //let pos = {X = posIn.X - 2.; Y = posIn.Y - 4.} 

    match mouseOnPort inputPorts pos 2.5 with
    | Some (portId, portLoc) -> InputPort (portId, portLoc)
    | None ->
        match mouseOnPort outputPorts pos 2.5 with
        | Some (portId, portLoc) -> OutputPort (portId, portLoc)
        | None ->
            match insideBox model.BoundingBoxes pos with
            | Some compId -> Component compId
            | None -> 
                match BusWire.getWireIfClicked model.Wire pos (5./model.Zoom) with
                | Some connId -> Connection connId
                | None -> Canvas

let notIntersectingComponents (model: Model) (box1: BoundingBox) (inputId: CommonTypes.ComponentId) =
   model.BoundingBoxes
   |> Map.filter (fun sId boundingBox -> boxesIntersect boundingBox box1 && inputId <> sId)
   |> Map.isEmpty 

/// Update function to move symbols in model.SelectedComponents
let moveSymbols (model: Model) (mMsg: MouseT) =
    let nextAction, isDragAndDrop =
        match model.Action with
        | DragAndDrop -> DragAndDrop, true
        | _ -> MovingSymbols, false
    
    // printfn "%A" nextAction

    match model.SelectedComponents.Length with
    | 1 -> // Attempt Snap-to-Grid if there is only one moving component
        
        /// Checks for snap-to-grid in one dimension (x-coordinates or y-coordinates)
        /// Input / output is an anonymous record to deal with too many arguments otherwise
        let checkForSnap1D (input: {| SnapInfo: LastSnap Option; Indicator: float Option; CurrMPos: float; LastMPos: float; Side1: float; Side2: float; PosDirection: float |}) =
            
            match input.SnapInfo with
            | Some { Pos = oldPos; SnapLength = previousSnap } -> // Already snapped, see if mouse is far enough to un-snap
                if abs (input.CurrMPos - oldPos) > unSnapMargin
                then {| DeltaPos = (input.CurrMPos - oldPos) - previousSnap; SnapInfo = None; Indicator = None |} // Un-snap
                else {| DeltaPos = 0.0; SnapInfo = input.SnapInfo; Indicator = input.Indicator |} // Don't un-snap
            | None -> // No snapping has occurred yet, check which side is closer to a grid and see if it should snap, also save which side it is
                let margins = [ (input.Side1 % gridSize), input.Side1
                                ((input.Side1 % gridSize) - gridSize), input.Side1
                                (input.Side2 % gridSize), input.Side2
                                ((input.Side2 % gridSize) - gridSize), input.Side2 ]
                
                let getMarginWithDirection (sortedMargins: (float*float)list) (dir: float) = 
                    if abs(fst(sortedMargins.[0]) - fst(sortedMargins.[1])) < 0.1 then
                        //printfn "HERE"
                        //printfn "%A" dir 
                        if dir > 0. then 
                        //    printf "HERE1"
                            sortedMargins.[0] 
                        else sortedMargins.[1]
                    else 
                        sortedMargins.[0]

                let sortedMargins = List.rev (List.sortByDescending (fun (margin, _) -> abs margin) margins)

                //printfn "%A" sortedMargins

                match getMarginWithDirection sortedMargins input.PosDirection with // abs since there negative margins as well (e.g. snap left)
                | margin, side when abs margin < snapMargin -> // Snap to grid and save info for future un-snapping
                    {| DeltaPos = -margin
                       SnapInfo = Some {Pos = input.CurrMPos; SnapLength = -margin - (input.CurrMPos - input.LastMPos)} // Offset with (CurrMPos - LastMPos), so that the symbol stays aligned with the mouse after un-snapping
                       Indicator = Some (side - margin) |}
                | _ -> // Don't do any snap-to-grid
                    {| DeltaPos = (input.CurrMPos - input.LastMPos)
                       SnapInfo = None
                       Indicator = None |} 
        
        let compId = model.SelectedComponents.Head
        let boundingBox = model.BoundingBoxes.[compId]
        let x1, x2, y1, y2 = boundingBox.X, boundingBox.X + boundingBox.W, boundingBox.Y, boundingBox.Y + boundingBox.H
        
        // printfn "%A" mMsg.Pos.X
        // printfn "%A" model.LastMousePos.X

        let snapX = checkForSnap1D {| SnapInfo = model.Snap.XSnap
                                      Indicator = model.SnapIndicator.XLine
                                      CurrMPos = mMsg.Pos.X
                                      LastMPos = model.LastMousePos.X
                                      Side1 = x1
                                      Side2 = x2
                                      PosDirection = (mMsg.Pos.X - model.LastMousePosForSnap.X)  |}
                              
        let snapY = checkForSnap1D {| SnapInfo = model.Snap.YSnap
                                      Indicator = model.SnapIndicator.YLine
                                      CurrMPos = mMsg.Pos.Y
                                      LastMPos = model.LastMousePos.Y
                                      Side1 = y1
                                      Side2 = y2
                                      PosDirection = (mMsg.Pos.Y - model.LastMousePosForSnap.Y) |}
        
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
                    wireCmd (BusWire.UpdateWires model.SelectedComponents)]
    | _ -> // Moving multiple symbols -> don't do snap-to-grid
        let errorComponents = 
            model.SelectedComponents
            |> List.filter (fun sId -> not (notIntersectingComponents model model.BoundingBoxes.[sId] sId))

        {model with Action = nextAction ; LastMousePos = mMsg.Pos; ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.Movement}; ErrorComponents = errorComponents },
        Cmd.batch [ symbolCmd (Symbol.MoveSymbols (model.SelectedComponents, posDiff mMsg.Pos model.LastMousePos))
                    symbolCmd (Symbol.ErrorSymbols (errorComponents,model.SelectedComponents,isDragAndDrop))
                    Cmd.ofMsg UpdateBoundingBoxes
                    Cmd.ofMsg CheckAutomaticScrolling 
                    wireCmd (BusWire.UpdateWires model.SelectedComponents)]
        
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
            |> List.filter (fun sId -> not (notIntersectingComponents model model.BoundingBoxes.[sId] sId))

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
                        wireCmd (BusWire.SelectWires model.SelectedWires) ]
    | _ ->
        match (mouseOn model mMsg.Pos) with
        | InputPort (portId, portLoc) -> 
            {model with Action = ConnectingInput portId; ConnectPortsLine = portLoc, mMsg.Pos; TmpModel=Some model},
            symbolCmd Symbol.ShowAllOutputPorts
        | OutputPort (portId, portLoc) -> 
            {model with Action = ConnectingOutput portId; ConnectPortsLine = portLoc, mMsg.Pos; TmpModel=Some model},
            symbolCmd Symbol.ShowAllInputPorts
        | Component compId ->
            let msg, action = 
                if model.IsWaveSim then 
                    ToggleNet ([Symbol.extractComponent model.Wire.Symbol compId], []), Idle
                else DoNothing, InitialiseMoving compId

            if model.Toggle 
            then 
                let newComponents =
                    if List.contains compId model.SelectedComponents
                    then List.filter (fun cId -> cId <> compId) model.SelectedComponents // If component selected was already in the list, remove it
                    else compId :: model.SelectedComponents // If user clicked on a new component add it to the selected list

                {model with SelectedComponents = newComponents; LastValidPos = mMsg.Pos ; LastValidBoundingBoxes=model.BoundingBoxes ; Action = action; LastMousePos = mMsg.Pos; TmpModel = Some model; PrevWireSelection = model.SelectedWires},
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
            let initialiseSelection = {model.DragToSelectBox with X=mMsg.Pos.X; Y=mMsg.Pos.Y}
            {model with DragToSelectBox = initialiseSelection; Action = Selecting; SelectedComponents = newComponents; SelectedWires = newWires },
            Cmd.batch [ symbolCmd (Symbol.SelectSymbols newComponents)
                        wireCmd (BusWire.SelectWires newWires) ]
        
/// Mouse Drag Update, can be: drag-to-selecting, moving symbols, connecting wire between ports.
let mDragUpdate (model: Model) (mMsg: MouseT) : Model * Cmd<Msg> = 
    match model.Action with
    | MovingWire connId -> model, wireCmd (BusWire.DragWire (connId, mMsg))
    | Selecting ->
        let initialX = model.DragToSelectBox.X
        let initialY = model.DragToSelectBox.Y
        let newDragToSelectBox = {model.DragToSelectBox with W = (mMsg.Pos.X - initialX); H = (mMsg.Pos.Y - initialY)}
        {model with DragToSelectBox = newDragToSelectBox
                    ScrollingLastMousePos = {Pos=mMsg.Pos;Move=mMsg.Movement}
                    LastMousePos = mMsg.Pos
         }, Cmd.ofMsg CheckAutomaticScrolling
    | InitialiseMoving _ ->
        let movingWires = BusWire.getConnectedWires model.Wire model.SelectedComponents
        let newModel, cmd = moveSymbols model mMsg
        newModel, Cmd.batch [ cmd; wireCmd (BusWire.ResetJumps movingWires) ]
    | MovingSymbols | DragAndDrop ->
        moveSymbols model mMsg
    | ConnectingInput _ -> 
        let nearbyComponents = findNearbyComponents model mMsg.Pos
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
        let nearbyComponents = findNearbyComponents model mMsg.Pos
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
    | _ -> model, Cmd.none
    
/// Mouse Up Update, can have: finished drag-to-select, pressed on a component, finished symbol movement, connected a wire between ports
let mUpUpdate (model: Model) (mMsg: MouseT) : Model * Cmd<Msg> = // mMsg is currently un-used, but kept for future possibilities 
    let newModel = 
        match model.TmpModel with 
        | None -> model
        | Some newModel -> newModel

    match model.Action with
    | MovingWire connId ->
        { model with Action = Idle ; UndoList = appendUndoList model.UndoList newModel; RedoList = [] },
        Cmd.batch [ wireCmd (BusWire.DragWire (connId, mMsg))
                    wireCmd (BusWire.MakeJumps [ connId ] ) ]
    | Selecting ->
        let newComponents = findIntersectingComponents model model.DragToSelectBox
        let newWires = BusWire.getIntersectingWires model.Wire model.DragToSelectBox
        let resetDragToSelectBox = {model.DragToSelectBox with H = 0.0; W = 0.0}
        let selectComps, selectWires = 
            if model.Toggle 
            then 
                symDiff newComponents model.SelectedComponents, symDiff newWires model.SelectedWires
            else newComponents, newWires

        { model with DragToSelectBox = resetDragToSelectBox; Action = Idle; SelectedComponents = selectComps; SelectedWires = selectWires; AutomaticScrolling = false },
        Cmd.batch [ symbolCmd (Symbol.SelectSymbols selectComps)
                    wireCmd (BusWire.SelectWires selectWires) ]

    | InitialiseMoving compId -> // If user clicked on a component and never moved it, then select that component instead. (resets multi-selection as well)
            { model with Action = Idle; SelectedComponents = [ compId ]; SelectedWires = [] },
            Cmd.batch [ symbolCmd (Symbol.SelectSymbols [ compId ])
                        wireCmd (BusWire.SelectWires []) ]
    | MovingSymbols ->
        // Reset Movement State in Model
        match model.ErrorComponents with 
        | [] ->
            let movingWires = BusWire.getConnectedWires model.Wire model.SelectedComponents
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
            let movingWires = BusWire.getConnectedWires model.Wire model.SelectedComponents
            {model with
                BoundingBoxes = model.LastValidBoundingBoxes 
                Action = Idle
                Snap = {XSnap = None; YSnap = None}
                SnapIndicator = {XLine = None; YLine = None }
                AutomaticScrolling = false }, 
            Cmd.batch [ symbolCmd (Symbol.MoveSymbols (model.SelectedComponents, (posDiff model.LastValidPos mMsg.Pos)))
                        symbolCmd (Symbol.SelectSymbols (model.SelectedComponents))
                        wireCmd (BusWire.UpdateWires model.SelectedComponents)
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
    | _ -> model, Cmd.none
    
/// Mouse Move Update, looks for nearby components and looks if mouse is on a port
let mMoveUpdate (model: Model) (mMsg: MouseT) : Model * Cmd<Msg> =
    match model.Action with
    | DragAndDrop -> moveSymbols model mMsg
    | InitialisedCreateComponent (compType, lbl) ->
        let labelTest = if lbl = "" then Symbol.generateLabel model.Wire.Symbol compType else lbl
        let newSymbolModel, newCompId = Symbol.addSymbol model.Wire.Symbol mMsg.Pos compType labelTest

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
        let nearbyComponents = findNearbyComponents model mMsg.Pos // TODO Group Stage: Make this more efficient, update less often etc, make a counter?
        
        let newCursor =
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
        let wModel, wCmd = BusWire.update wMsg model.Wire
        { model with Wire = wModel }, Cmd.map Wire wCmd
    | KeyPress DEL ->
        let wiresConnectedToComponents = BusWire.getConnectedWires model.Wire model.SelectedComponents
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
        { model with BoundingBoxes = Symbol.getBoundingBoxes model.Wire.Symbol; UndoList = appendUndoList model.UndoList model ; RedoList = []},
        Cmd.batch [ symbolCmd (Symbol.AddSymbol ({X = 50.0; Y = 50.0}, And, "test 1")); Cmd.ofMsg UpdateBoundingBoxes ] // Need to update bounding boxes after adding a symbol.
    | KeyPress AltShiftZ ->
        printStats() 
        model, Cmd.none
    | KeyPress CtrlC ->
        model,
        Cmd.batch [
            symbolCmd (Symbol.CopySymbols model.SelectedComponents) // Better to have Symbol keep track of clipboard as symbols can get deleted before pasting.
            wireCmd (BusWire.CopyWires model.SelectedWires)
        ]
    | KeyPress CtrlV ->
        let newSymbolModel, pastedCompIds = Symbol.pasteSymbols model.Wire.Symbol model.LastMousePos // Symbol has Copied Symbols stored
        let newBusWireModel, pastedConnIds = BusWire.pasteWires { model.Wire with Symbol = newSymbolModel } pastedCompIds
        
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
            let wireModel = { prevModel.Wire with CopiedWX = model.Wire.CopiedWX ; Symbol = symModel}
            { prevModel with Wire = wireModel ; UndoList = lst ; RedoList = model :: model.RedoList ; CurrentKeyPresses = Set.empty } , Cmd.none
    | KeyPress CtrlY -> 
        match model.RedoList with 
        | [] -> model , Cmd.none
        | newModel :: lst -> { newModel with UndoList = model :: model.UndoList ; RedoList = lst} , Cmd.none
    | KeyPress CtrlA -> 
        let symbols = model.Wire.Symbol.Symbols |> Map.toList |> List.map fst
        let wires = model.Wire.WX |> Map.toList |> List.map fst
        { model with 
            SelectedComponents = symbols
            SelectedWires = wires
        } , Cmd.batch [ symbolCmd (Symbol.SelectSymbols symbols)
                        wireCmd (BusWire.SelectWires wires) ]
    | KeyPress CtrlW ->
        let leftScreenEdge, rightScreenEdge = getScreenEdgeCoords()
        let newZoom = (rightScreenEdge - leftScreenEdge) / canvasSize

        { model with Zoom = newZoom }, Cmd.none
    | ToggleSelectionOpen ->
        //if List.isEmpty model.SelectedComponents && List.isEmpty model.SelectedWires then  
        //    model, Cmd.none
        //else
            {model with Toggle = true}, Cmd.none
    | ToggleSelectionClose -> 
        {model with Toggle = false}, Cmd.none

    | MouseMsg mMsg -> // Mouse Update Functions can be found above, update function got very messy otherwise
        // printf "%A" mMsg
        match mMsg.Op with
        | Down -> mDownUpdate model mMsg
        | Drag -> mDragUpdate model mMsg
        | Up -> mUpUpdate model mMsg
        | Move -> mMoveUpdate model mMsg
    | UpdateBoundingBoxes -> { model with BoundingBoxes = Symbol.getBoundingBoxes model.Wire.Symbol }, Cmd.none
    | UpdateSingleBoundingBox compId ->
        match Map.containsKey compId model.BoundingBoxes with
        | true -> {model with BoundingBoxes = model.BoundingBoxes.Add (compId, (Symbol.getOneBoundingBox model.Wire.Symbol compId))}, Cmd.none
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
        let leftScreenEdge, rightScreenEdge = getScreenEdgeCoords()
        //Check if the new zoom will exceed the canvas width
        let newZoom = 
            if rightScreenEdge - leftScreenEdge < (canvasSize * (model.Zoom - 0.05)) then model.Zoom - 0.05
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
            
            if edgeDistance < scrollMargin && mMov >= 0.
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
                    mMoveUpdate { model with AutomaticScrolling = true } { Pos = newMPos; Op = Move;  Movement = {X=0.;Y=0.}}
                | MovingSymbols | ConnectingInput _ | ConnectingOutput _ | Selecting ->
                    mDragUpdate { model with AutomaticScrolling = true } { Pos = newMPos; Op = Drag; Movement = {X=0.;Y=0.}}
                | _ -> 
                    { model with AutomaticScrolling = true }, Cmd.none
            
            // Don't want to go into an infinite loop (program would crash), don't check for automatic scrolling immediately (let it be handled by OnScroll listener).
            let filteredOutputCmd = Cmd.map (fun msg -> if msg <> CheckAutomaticScrolling then msg else DoNothing) outputCmd
            
            outputModel, filteredOutputCmd
        else
            { model with AutomaticScrolling = false }, Cmd.none
                
    // ---------------------------- Issie Messages ---------------------------- //

    | InitialiseCreateComponent (compType, lbl) ->
        { model with Action = (InitialisedCreateComponent (compType, lbl)) ; TmpModel = Some model}, Cmd.none
    | FlushCommandStack -> { model with UndoList = []; RedoList = []; TmpModel = None }, Cmd.none
    | ResetModel ->
        { model with
            BoundingBoxes = Map.empty
            LastValidBoundingBoxes = Map.empty
            SelectedComponents = []
            SelectedWires = []
            NearbyComponents = []
            ErrorComponents = []
            DragToSelectBox = {X=0.0; Y=0.0; H=0.0; W=0.0}
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
        {model with SelectedWires = newWires}, Cmd.batch[Cmd.ofMsg (ColourSelection([], newWires, HighLightColor.Blue)); wireCmd (BusWire.SelectWires newWires)]
    | ToggleNet _ | DoNothing | _ -> model, Cmd.none

/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. 
let displaySvgWithZoom (model: Model) (headerHeight: float) (style: CSSProp list) (svgReact: ReactElement List) (dispatch: Dispatch<Msg>)=
    // Hacky way to get keypresses such as Ctrl+C to work since Electron does not pick them up.
    document.onkeydown <- (fun key ->
        if key.which = 32.0 then// Check for spacebar
            key.preventDefault() // Disable scrolling with spacebar
            dispatch <| (ManualKeyDown key.key)
        else
            dispatch <| (ManualKeyDown key.key) )
    document.onkeyup <- (fun key -> dispatch <| (ManualKeyUp key.key))

    let sizeInPixels = sprintf "%.2fpx" ((canvasSize * model.Zoom))

    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = ev.buttons <> 0.

    /// Dispatch a MouseMsg (compensated for zoom)
    let mouseOp op (ev:Types.MouseEvent) =
        //printfn "%s" $"Op:{ev.movementX},{ev.movementY}"
        dispatch <| MouseMsg {
            Op = op ; 
            Movement = {X= ev.movementX;Y=ev.movementY}
            Pos = { 
                X = (ev.pageX + model.ScrollPos.X) / model.Zoom  ; 
                Y = (ev.pageY - headerHeight + model.ScrollPos.Y) / model.Zoom}
            }
        // dispatch <| MouseMsg {Op = op ; Pos = { X = (ev.pageX + model.ScrollPos.X) / model.Zoom  ; Y = (ev.pageY - topMenuBarHeight + model.ScrollPos.Y) / model.Zoom }}
    let scrollUpdate () =
        let canvas = document.getElementById "Canvas"
        dispatch <| UpdateScrollPos (canvas.scrollLeft, canvas.scrollTop) // Add the new scroll offset to the model
    let wheelUpdate (ev: Types.WheelEvent) =
        if Set.contains "CONTROL" model.CurrentKeyPresses then
            // ev.preventDefault()
            if ev.deltaY > 0.0 then // Wheel Down
                dispatch <| KeyPress ZoomOut
            else
                dispatch <| KeyPress ZoomIn
        else () // Scroll normally if Ctrl is not held down

    div [ HTMLAttr.Id "Canvas"
          Style (CSSProp.Cursor (model.CursorType.Text()) :: style)
          OnMouseDown (fun ev -> (mouseOp Down ev)) 
          OnMouseUp (fun ev -> (mouseOp Up ev)) 
          OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move) ev)
          OnScroll (fun _ -> scrollUpdate ())
          OnWheel wheelUpdate
        ]
        [ 
          svg
            [ Style 
                [
                    Height sizeInPixels
                    Width sizeInPixels           
                ]
            ]
            [ g // group list of elements with list of attributes
                [ Style [Transform (sprintf "scale(%f)" model.Zoom)]] // top-level transform style attribute for zoom  
                svgReact // the application code
            ]
        ]

/// View function, displays symbols / wires and possibly also a grid / drag-to-select box / connecting ports line / snap-to-grid visualisation
let view (model:Model) (headerHeight: float) (style) (dispatch : Msg -> unit) =
    let start = Helpers.getTimeMs()
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    
    let wholeCanvas = $"{max 100.0 (100.0 / model.Zoom)}" + "%" 
    let grid =
        svg [ SVGAttr.Width wholeCanvas; SVGAttr.Height wholeCanvas; SVGAttr.XmlSpace "http://www.w3.org/2000/svg" ] [
            defs [] [
                pattern [
                    Id "Grid"
                    SVGAttr.Width $"{gridSize}"
                    SVGAttr.Height $"{gridSize}"
                    SVGAttr.PatternUnits "userSpaceOnUse"
                ] [
                    path [
                        SVGAttr.D $"M {gridSize} 0 L 0 0 0 {gridSize}"
                        SVGAttr.Fill "None"
                        SVGAttr.Stroke "Gray"
                        SVGAttr.StrokeWidth "0.5"
                        ] []
                ]
            ] 
            rect [SVGAttr.Width wholeCanvas; SVGAttr.Height wholeCanvas; SVGAttr.Fill "url(#Grid)"] []
        ]
        
    let dragToSelectBox =
        let {BoundingBox.X=fX; Y=fY; H=fH; W=fW} = model.DragToSelectBox
        let polygonPoints = $"{fX},{fY} {fX+fW},{fY} {fX+fW},{fY+fH} {fX},{fY+fH}"
        let selectionBox = { defaultPolygon with Stroke = "Black"; StrokeWidth = "0.1px"; Fill = "Blue"; FillOpacity = 0.05 }
        
        makePolygon polygonPoints selectionBox
    
    let snapIndicatorLine = { defaultLine with Stroke = "Red"; StrokeWidth = "0.5px"; StrokeDashArray = "5, 5" }
        
    let connectingPortsWire =
        let connectPortsLine = { defaultLine with Stroke = "Green"; StrokeWidth = "2.0px"; StrokeDashArray = "5, 5" }
        let {XYPos.X = x1; Y = y1}, {XYPos.X = x2; Y = y2} = model.ConnectPortsLine
        [ makeLine x1 y1 x2 y2 connectPortsLine
          makeCircle x2 y2 { portCircle with Fill = "Green" }
        ]
        
    let snapIndicatorLineX =
        match model.SnapIndicator.XLine with
        | Some xPos ->
            [ makeLine xPos 0.0 xPos wholeCanvas snapIndicatorLine ]
        | None ->
            []
    
    let snapIndicatorLineY =
        match model.SnapIndicator.YLine with
        | Some yPos ->
            [ makeLine 0.0 yPos wholeCanvas yPos snapIndicatorLine ]
        | None ->
            []
    
    let displayElements =
        if model.ShowGrid
        then [ grid; wireSvg ]
        else [ wireSvg ]
        
    match model.Action with // Display differently depending on what state Sheet is in
    | Selecting ->
        displaySvgWithZoom model headerHeight style ( displayElements @ [ dragToSelectBox ] ) dispatch
    | ConnectingInput _ | ConnectingOutput _ ->
        displaySvgWithZoom model headerHeight style ( displayElements @ connectingPortsWire ) dispatch
    | MovingSymbols | DragAndDrop ->
        displaySvgWithZoom model headerHeight style ( displayElements @ snapIndicatorLineX @ snapIndicatorLineY ) dispatch
    | _ ->
        displaySvgWithZoom model headerHeight style displayElements dispatch
    |> Helpers.instrumentInterval "SheetView" start

/// Init function
let init () = 
    let wireModel, cmds = (BusWire.init ())
    let boundingBoxes = Symbol.getBoundingBoxes wireModel.Symbol
    
    {
        Wire = wireModel
        BoundingBoxes = boundingBoxes
        LastValidBoundingBoxes = boundingBoxes
        SelectedComponents = []
        SelectedWires = []
        NearbyComponents = []
        ErrorComponents = []
        DragToSelectBox = {X=0.0; Y=0.0; H=0.0; W=0.0}
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
