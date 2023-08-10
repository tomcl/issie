﻿module SheetDisplay
open CommonTypes
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open DrawHelpers
open DrawModelType
open DrawModelType.SheetT
open Optics
open Operators  
open Sheet
open SheetSnap

/// This actually writes to the DOM a new scroll position.
/// In the special case that DOM has not yel been created it does nothing.
let writeCanvasScroll (scrollPos:XYPos) =
    canvasDiv
    |> Option.iter (fun el -> el.scrollLeft <- scrollPos.X; el.scrollTop <- scrollPos.Y)

let getDrawBlockPos (ev: Types.MouseEvent) (headerHeight: float) (sheetModel:Model) =
    {
        X = (ev.pageX + sheetModel.ScreenScrollPos.X) / sheetModel.Zoom  ;
        Y = (ev.pageY - headerHeight + sheetModel.ScreenScrollPos.Y) / sheetModel.Zoom
    }

/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner.
let displaySvgWithZoom 
        (model: Model) 
        (headerHeight: float) 
        (style: CSSProp list) 
        (svgReact: ReactElement List) 
        (dispatch: Dispatch<Msg>) 
            : ReactElement=

    let zoom = model.Zoom
    // Hacky way to get keypresses such as Ctrl+C to work since Electron does not pick them up.
    document.onkeydown <- (fun key ->
        if key.which = 32.0 then// Check for spacebar
            // key.preventDefault() // Disable scrolling with spacebar
            dispatch <| (ManualKeyDown key.key)
        else
            dispatch <| (ManualKeyDown key.key) )
    document.onkeyup <- (fun key -> dispatch <| (ManualKeyUp key.key))

    let sizeInPixels = sprintf "%.2fpx" ((model.CanvasSize * model.Zoom))

    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = ev.buttons <> 0.
    

    /// Dispatch a MouseMsg (compensated for zoom)
    let mouseOp op (ev:Types.MouseEvent) =
        dispatch <| MouseMsg {
            Op = op ;
            ShiftKeyDown = ev.shiftKey
            ScreenMovement = {X= ev.movementX;Y=ev.movementY}
            ScreenPage = {X=ev.pageX; Y=ev.pageY}
            Pos = getDrawBlockPos ev headerHeight model
            }

    let wheelUpdate (ev: Types.WheelEvent) =
        if Set.contains "CONTROL" model.CurrentKeyPresses then
            // ev.preventDefault()
            if ev.deltaY > 0.0 then // Wheel Down
                dispatch <| KeyPress ZoomOut
            else
                dispatch <| KeyPress ZoomIn
        else () // Scroll normally if Ctrl is not held down
    let cursorText = model.CursorType.Text()

    div [ 
          HTMLAttr.Id "Canvas"
          Key cursorText // force cursor change to be rendered
          Style (CSSProp.Cursor cursorText :: style)
          OnMouseDown (fun ev -> (mouseOp Down ev))
          OnMouseUp (fun ev -> (mouseOp Up ev))
          OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move) ev)
          OnScroll (fun _ -> dispatch <| (UpdateScrollPosFromCanvas dispatch))
          Ref (fun el ->
            canvasDiv <- Some el
            writeCanvasScroll model.ScreenScrollPos
            )

          OnWheel wheelUpdate
        ]
        [
          svg
            [ Style
                [
                    Height sizeInPixels
                    Width sizeInPixels
                ]
              Id "DrawBlockSVGTop"
            ]
            [ g // group list of elements with list of attributes
                [ Style [Transform (sprintf "scale(%f)" zoom)]] // top-level transform style attribute for zoom
                    svgReact // the application code
            ]
        ]

/// View function, displays symbols / wires and possibly also a grid / drag-to-select box / connecting ports line / snap-to-grid visualisation
let view 
        (model:Model) 
        (headerHeight: float) 
        (style: CSSProp list) 
        (dispatch : Msg -> unit) 
            : ReactElement =
    let start = TimeHelpers.getTimeMs()
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch

    let wholeCanvas = $"{max 100.0 (100.0 / model.Zoom)}" + "%"
    let snapIndicatorLineX = snapIndicatorLineX model wholeCanvas
    let snapIndicatorLineY = snapIndicatorLineY model wholeCanvas
    /// show all the snap lines (used primarily for debugging snap)
    let snapDisplay (model:Model) =
        let snapLineY (ypt:SnapData) = snapLineHorizontal wholeCanvas ypt.Snap
        let snapLineX (xpt:SnapData) = snapLineVertical wholeCanvas xpt.Snap
        Array.append
            (model.SnapSymbols.SnapX.SnapData |> Array.map snapLineX)
            (model.SnapSymbols.SnapY.SnapData |> Array.map snapLineY)
        |> Array.toList


    let gridSize = Constants.gridSize
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

    let scalingBox = 
        let {BoundingBox.TopLeft = {X=fX; Y=fY}; H=fH; W=fW} = model.Box.BoxBound
        match model.SelectedComponents.Length with
        | s when s<2 -> [makeAnyPath {X=0;Y=0} (makeLineAttr 0.0 0.0) defaultPath] @ [makeCircle 0.0 0.0 {defaultCircle with R=0.0}]
        | _ -> [makeAnyPath {X=fX+50.0+fW;Y=(fY-46.5)} ((makeLineAttr 0.0 (fH+96.5))+(makeLineAttr -(fW+100.0) 0)+(makeLineAttr 0.0 (-(fH)-100.0))+(makeLineAttr (fW+96.5) 0.0)) {defaultPath with StrokeDashArray="4,4"}]

    let connectingPortsWire =
        let connectPortsLine = { defaultLine with Stroke = "Green"; StrokeWidth = "2.0px"; StrokeDashArray = "5, 5" }
        let {XYPos.X = x1; Y = y1}, {XYPos.X = x2; Y = y2} = model.ConnectPortsLine
        [ makeLine x1 y1 x2 y2 connectPortsLine
          makeCircle x2 y2 { portCircle with Fill = "Green" }
        ]

    let displayElements =
        if model.ShowGrid
        then [ grid; wireSvg ]
        else [ wireSvg ]

    // uncomment the display model react for visbility of all snaps
    let snaps = snapIndicatorLineX @ snapIndicatorLineY // snapDisplay model

    match model.Action, model.Box.ShowBox with // Display differently depending on what state Sheet is in
    | Selecting,_ ->
        displaySvgWithZoom model headerHeight style ( displayElements @ [ dragToSelectBox ] ) dispatch
    | ConnectingInput _, false | ConnectingOutput _, false ->
        displaySvgWithZoom model headerHeight style ( displayElements @ connectingPortsWire ) dispatch
    | ConnectingInput _, true | ConnectingOutput _, true ->
        displaySvgWithZoom model headerHeight style ( displayElements @ scalingBox @ connectingPortsWire ) dispatch
    | DragAndDrop, false ->
        displaySvgWithZoom model headerHeight style ( displayElements @ snaps) dispatch
    | DragAndDrop, true->
        displaySvgWithZoom model headerHeight style ( displayElements @ snaps @ scalingBox) dispatch
    | (MovingSymbols),_  ->
        displaySvgWithZoom model headerHeight style ( displayElements @ snaps @ scalingBox) dispatch
    | MovingWire _,_ -> 
        displaySvgWithZoom model headerHeight style (displayElements @ snaps) dispatch
    | Scaling , _ -> 
        displaySvgWithZoom model headerHeight style ( displayElements @  scalingBox ) dispatch
    | _ , true-> 
        displaySvgWithZoom model headerHeight style ( displayElements @  scalingBox ) dispatch

    | _ ->
        displaySvgWithZoom model headerHeight style displayElements dispatch
    |> TimeHelpers.instrumentInterval "SheetView" start

