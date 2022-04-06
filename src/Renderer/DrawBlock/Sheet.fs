(*
  This module coordinates the draw block user interface managing component selection, moving, auto-scrolling etc
*)

module Sheet
open CommonTypes
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open DrawHelpers
open DrawModelType
open DrawModelType.SheetT



let mutable canvasDiv:Types.Element option = None

// ------------------ Helper Functions that need to be before the Model type extensions ---------- //

/// Creates a command to Symbol
let symbolCmd (msg: SymbolT.Msg) = Cmd.ofMsg (Wire (BusWireT.Symbol msg))

/// Creates a command to BusWire
let wireCmd (msg: BusWireT.Msg) = Cmd.ofMsg (Wire msg)


    // ---------------------------------- Issie Interfacing functions ----------------------------- //

module SheetInterface =
    type Model with
        /// Given a compType, return a label
        member this.GenerateLabel (compType: ComponentType) : string =
            SymbolUpdate.generateLabel this.Wire.Symbol compType

        /// Given a compId, return the corresponding component
        member this.GetComponentById (compId: ComponentId) =
            SymbolUpdate.extractComponent this.Wire.Symbol compId

        /// Change the label of Component specified by compId to lbl
        member this.ChangeLabel (dispatch: Dispatch<Msg>) (compId: ComponentId) (lbl: string) =
            dispatch <| (Wire (BusWireT.Symbol (SymbolT.ChangeLabel (compId, lbl) ) ) )

        /// Run Bus Width Inference check
        member this.DoBusWidthInference dispatch =
            dispatch <| (Wire (BusWireT.BusWidths))

        /// Given a compId and a width, update the width of the Component specified by compId
        member this.ChangeWidth (dispatch: Dispatch<Msg>) (compId: ComponentId) (width: int) =
            dispatch <| (Wire (BusWireT.Symbol (SymbolT.ChangeNumberOfBits (compId, width) ) ) )
            this.DoBusWidthInference dispatch

        /// Given a compId and a LSB, update the LSB of the Component specified by compId
        member this.ChangeLSB (dispatch: Dispatch<Msg>) (compId: ComponentId) (lsb: int64) =
            dispatch <| (Wire (BusWireT.Symbol (SymbolT.ChangeLsb (compId, lsb) ) ) )

        /// Return Some string if Sheet / BusWire / Symbol has a notification, if there is none then return None
        member this.GetNotifications =
            // Currently only BusWire has notifications
            this.Wire.Notifications

        /// Get the current canvas state in the form of (Component list * Connection list)
        member this.GetCanvasState () =
            let compList = SymbolUpdate.extractComponents this.Wire.Symbol
            let connList = BusWire.extractConnections this.Wire

            compList, connList

        /// Clears the Undo and Redo stack of Sheet
        member this.FlushCommandStack dispatch =
            dispatch <| FlushCommandStack

        /// Clears the canvas, removes all components and connections
        member this.ClearCanvas dispatch =
            dispatch <| ResetModel
            dispatch <| (Wire BusWireT.ResetModel)
            dispatch <| (Wire (BusWireT.Symbol (SymbolT.ResetModel ) ) )

        /// Returns a list of selected components
        member this.GetSelectedComponents =
            this.SelectedComponents
            |> List.collect ( fun compId ->
                if Map.containsKey compId this.Wire.Symbol.Symbols then
                    [SymbolUpdate.extractComponent this.Wire.Symbol compId]
                else
                    [])
        
        /// Returns a list of selected connections
        member this.GetSelectedConnections =
            this.SelectedWires
            |> List.collect (fun connId ->
                if Map.containsKey connId this.Wire.Wires then
                    [BusWire.extractConnection this.Wire connId]
                else 
                    [])
        
        /// Returns a list of selected components and connections in the form of (Component list * Connection list)
        member this.GetSelectedCanvasState =
            this.GetSelectedComponents, this.GetSelectedConnections

        /// Given a list of connIds, select those connections
        member this.SelectConnections dispatch on connIds =
            dispatch <| UpdateSelectedWires (connIds, on)

        /// Update the memory of component specified by connId at location addr with data value
        member this.WriteMemoryLine dispatch connId addr value =
            dispatch <| (Wire (BusWireT.Symbol (SymbolT.WriteMemoryLine (connId, addr, value))))

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


let getScreenEdgeCoords () =
    let canvas = document.getElementById "Canvas"
    let wholeApp = document.getElementById "WholeApp"
    let rightSelection = document.getElementById "RightSelection"
    let topMenu = document.getElementById "TopMenu"
    let leftScreenEdge = canvas.scrollLeft
    let rightScreenEdge = leftScreenEdge + wholeApp.clientWidth - rightSelection.offsetWidth
    let topScreenEdge = canvas.scrollTop
    let bottomScreenEdge = topScreenEdge + rightSelection.offsetHeight - topMenu.clientHeight
    (leftScreenEdge, rightScreenEdge,topScreenEdge,bottomScreenEdge)

/// Checks if pos is inside any of the bounding boxes of the components in boundingBoxes
let inline insideBox (boundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>) (pos: XYPos) : CommonTypes.ComponentId Option =
    let insideOneBox _ boundingBox =
        let {BoundingBox.TopLeft={X = xBox; Y=yBox}; H=hBox; W=wBox} = boundingBox
        pos.X >= xBox && pos.X <= xBox + wBox && pos.Y >= yBox && pos.Y <= yBox + hBox

    boundingBoxes
    |> Map.tryFindKey insideOneBox // If there are multiple components overlapping (should not happen), return first one found

/// return a BB equivalent to input but with (X,Y) = LH Top coord, (X+W,Y+H) = RH bottom coord
/// note that LH Top is lower end of the two screen coordinates
let standardiseBox (box:BoundingBox) =
    let x = min box.TopLeft.X (box.TopLeft.X+box.W)
    let y = min box.TopLeft.Y (box.TopLeft.Y+box.H)
    let w = abs box.W
    let h = abs box.H
    { TopLeft = {X=x; Y=y} ; W=w;H=h}


let transformScreenToPos (screenPos:XYPos) (scrollPos:XYPos) mag =
    {X=(screenPos.X + scrollPos.X)/mag;
     Y=(screenPos.Y + scrollPos.Y)/mag}


/// calculates the smallest bounding box that contains two BBs, in form with W,H > 0
let boxUnion (box:BoundingBox) (box':BoundingBox) =
    let maxX = max (box.TopLeft.X+box.W) (box'.TopLeft.X + box'.W)
    let maxY = max (box.TopLeft.Y + box.H) (box'.TopLeft.Y + box'.H)
    let minX = min box.TopLeft.X box'.TopLeft.X
    let minY = min box.TopLeft.Y box'.TopLeft.Y
    {
        TopLeft = {X = minX; Y = minY}
        W = maxX - minX
        H = maxY - minY
    }

let symbolToBB (symbol:SymbolT.Symbol) =
    let co = symbol.Component
    {TopLeft = symbol.Pos; W=co.W; H=co.H}
    

/// Inputs must also have W,H > 0.
/// Maybe this should include wires as well?
let symbolBBUnion (model:Model) =
    let symbols =
        model.Wire.Symbol.Symbols
        |> Map.toList
    match symbols with
    | [] -> None
    | (_,sym) :: rest ->
        (symbolToBB sym, rest)
        ||> List.fold (fun (box:BoundingBox) (_,sym) ->
                boxUnion box (symbolToBB sym))
        |> Some

let fitCircuitToWindowParas (model:Model) =
    let maxMagnification = 2.
    let boxOpt = symbolBBUnion model
    let sBox =
        match boxOpt with
        | None -> {TopLeft = {X=100.; Y=100.}; W=100.; H=100.} // default if sheet is empty
        | Some box -> 
            {
                    TopLeft = box.TopLeft
                    W = box.W
                    H = box.H
            }
    let boxEdge = max 30. ((max sBox.W sBox.H) * 0.05)
    let lh,rh,top,bottom = getScreenEdgeCoords()
    let wantedMag = min ((rh - lh)/(sBox.W+2.*boxEdge)) ((bottom-top)/(sBox.H+2.*boxEdge))
    let magToUse = min wantedMag maxMagnification
    let xMiddle = (sBox.TopLeft.X + sBox.W/2.)*magToUse
    let xScroll = xMiddle - (rh-lh)/2.
    let yMiddle = (sBox.TopLeft.Y + (sBox.H)/2.)*magToUse
    let yScroll = yMiddle - (bottom-top)/2.

    {|ScrollX=xScroll; ScrollY=yScroll; MagToUse=magToUse|}


let isBBoxAllVisible (bb: BoundingBox) =
    let lh,rh,top,bottom = getScreenEdgeCoords()
    let bbs = standardiseBox bb
    lh < bb.TopLeft.Y && 
    top < bb.TopLeft.X && 
    bb.TopLeft.Y+bb.H < bottom && 
    bb.TopLeft.X+bb.W < rh

/// could be made more efficient, since segments contain redundant info
let getWireBBox (wire: BusWireT.Wire) (model: Model) =
    let updateBoundingBox segStart (segEnd: XYPos) state seg =
        let newTop = min state.TopLeft.Y segEnd.Y
        let newBottom = max (state.TopLeft.Y+state.H) segEnd.Y
        let newRight = max (state.TopLeft.X+state.W) segEnd.X
        let newLeft = min state.TopLeft.X segEnd.X
        {TopLeft={X=newTop; Y=newLeft}; W=newRight-newLeft; H=newBottom-newTop }
    BusWire.foldOverSegs updateBoundingBox {TopLeft = wire.StartPos; W=0; H=0;} wire
    

let isAllVisible (model: Model)(conns: ConnectionId list) (comps: ComponentId list) =
    let wVisible =
        conns
        |> List.map (fun cid -> Map.tryFind cid model.Wire.Wires)
        |> List.map (Option.map (fun wire -> getWireBBox wire model))
        |> List.map (Option.map isBBoxAllVisible)
        |> List.map (Option.defaultValue true)
        |> List.fold (&&) true
    let cVisible =
        comps
        |> List.collect (fun comp ->
            if Map.containsKey comp model.Wire.Symbol.Symbols then 
                [Symbol.getBoundingBox model.Wire.Symbol comp]
            else
                [])
        |> List.map isBBoxAllVisible
        |> List.fold (&&) true
    wVisible && cVisible





/// Calculates if two bounding boxes intersect by comparing corner coordinates of each box
let boxesIntersect (box1: BoundingBox) (box2: BoundingBox) =
    // Requires min and max since H & W can be negative, i.e. we don't know which corner is which automatically
    // Boxes intersect if there is overlap in both x and y coordinates 
    min box1.TopLeft.X (box1.TopLeft.X + box1.W) < max box2.TopLeft.X (box2.TopLeft.X + box2.W)
    && min box2.TopLeft.X (box2.TopLeft.X + box2.W) < max box1.TopLeft.X (box1.TopLeft.X + box1.W)
    && min box1.TopLeft.Y (box1.TopLeft.Y + box1.H) < max box2.TopLeft.Y (box2.TopLeft.Y + box2.H)
    && min box2.TopLeft.Y (box2.TopLeft.Y + box2.H) < max box1.TopLeft.Y (box1.TopLeft.Y + box1.H)
    
/// Finds all components that touch a bounding box (which is usually the drag-to-select box)
let findIntersectingComponents (model: Model) (box1: BoundingBox) =
    model.BoundingBoxes
    |> Map.filter (fun _ boundingBox -> boxesIntersect boundingBox box1)
    |> Map.toList
    |> List.map fst

let posAdd (pos : XYPos) (a : float, b : float) : XYPos =
    {X = pos.X + a; Y = pos.Y + b}

/// Finds all components (that are stored in the Sheet model) near pos
let findNearbyComponents (model: Model) (pos: XYPos) (range: float)  =

    List.allPairs [-range .. 10.0 .. range] [-range .. 10.0 .. range] // Larger Increments -> More Efficient. But can miss small components then.
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
/// Priority Order: InputPort -> OutputPort -> Component -> Label -> Wire -> Component -> Canvas
let mouseOn (model: Model) (pos: XYPos) : MouseOn =

    let inputPorts, outputPorts = findNearbyPorts model
    match mouseOnPort inputPorts pos 2.5 with
    | Some (portId, portLoc) -> InputPort (portId, portLoc)
    | None ->
        match mouseOnPort outputPorts pos 2.5 with
        | Some (portId, portLoc) -> OutputPort (portId, portLoc)
        | None ->
            match insideBox model.LabelBoundingBoxes pos with
            | Some compId -> 
                Label compId
            | None ->
                match BusWireUpdate.getClickedWire model.Wire pos (5./model.Zoom) with
                | Some connId -> Connection connId
                | None ->
                    match insideBox model.BoundingBoxes pos with
                    | Some compId -> Component compId
                    | None -> Canvas


let notIntersectingComponents (model: Model) (box1: BoundingBox) (inputId: CommonTypes.ComponentId) =
   model.BoundingBoxes
   |> Map.filter (fun sId boundingBox -> boxesIntersect boundingBox box1 && inputId <> sId)
   |> Map.isEmpty


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

    let sizeInPixels = sprintf "%.2fpx" ((DrawHelpers.canvasUnscaledSize * model.Zoom))

    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = ev.buttons <> 0.

    /// Dispatch a MouseMsg (compensated for zoom)
    let mouseOp op (ev:Types.MouseEvent) =
        dispatch <| MouseMsg {
            Op = op ;
            ShiftKeyDown = ev.shiftKey
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
    let cursorText = model.CursorType.Text()

    div [ HTMLAttr.Id "Canvas"
          Key cursorText // force cursor change to be rendered
          Style (CSSProp.Cursor cursorText :: style)
          OnMouseDown (fun ev -> (mouseOp Down ev))
          OnMouseUp (fun ev -> (mouseOp Up ev))
          OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move) ev)
          OnScroll (fun _ -> scrollUpdate ())
          Ref (fun el ->
            canvasDiv <- Some el
            if not (isNull el) then
                // in case this element is newly created, set scroll position from model
                el.scrollLeft <- model.ScrollPos.X
                el.scrollTop <- model.ScrollPos.Y)
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
                (makeCircle 
                    model.LastMousePos.X 
                    model.LastMousePos.Y
                    {defaultCircle with R= 1.0} :: svgReact) // the application code
            ]
        ]

/// View function, displays symbols / wires and possibly also a grid / drag-to-select box / connecting ports line / snap-to-grid visualisation
let view (model:Model) (headerHeight: float) (style) (dispatch : Msg -> unit) =
    let start = TimeHelpers.getTimeMs()
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
        let {BoundingBox.TopLeft = {X=fX; Y=fY}; H=fH; W=fW} = model.DragToSelectBox
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
    | MovingWire connId -> 
        displaySvgWithZoom model headerHeight style (displayElements @ snapIndicatorLineX @ snapIndicatorLineY) dispatch
    | _ ->
        displaySvgWithZoom model headerHeight style displayElements dispatch
    |> TimeHelpers.instrumentInterval "SheetView" start

