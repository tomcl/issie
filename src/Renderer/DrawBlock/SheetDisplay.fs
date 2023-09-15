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
/// In the special case that DOM has not yet been created it does nothing.
let writeCanvasScroll (scrollPos:XYPos) =
    //printf "%s" $"***writing canvas scroll: {scrollPos.X},{scrollPos.Y}"
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
        // right button oprations are only used for context menus
        if int ev.button = 0 then // button = 0 => left, button = 2 => right
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
    let firstView = viewIsAfterUpdateScroll
    viewIsAfterUpdateScroll <- false
    div [ 
          HTMLAttr.Id "Canvas"
          Key cursorText // force cursor change to be rendered
          Style (CSSProp.Cursor cursorText :: style)
          OnMouseDown (fun ev -> (mouseOp Down ev))
          OnMouseUp (fun ev -> (mouseOp Up ev))
          OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move) ev)
          OnScroll (fun _ ->
            match canvasDiv with
            | None -> ()
            |Some el ->
                if not firstView then
                    dispatch <| UpdateScrollPosFromCanvas(scrollSequence,{X= el.scrollLeft; Y=el.scrollTop}, dispatch))
          Ref (fun el ->
            canvasDiv <- Some el
            //printfn "%s" $"Writing from Ref {scrollSequence}: {model.ScreenScrollPos.X},{model.ScreenScrollPos.Y}"
            writeCanvasScroll model.ScreenScrollPos)
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
    

    // rotating the default horizontal scaleButton icon to match the diagonal of the scalingBox    
    let rotateScaleButtonPoint boxW  boxH  point =
        let diagonal =  sqrt(boxW**2.0+boxH**2.0)
        let cosTheta = - (boxW / diagonal)
        let sinTheta = boxH / diagonal 
        let {XYPos.X = x; XYPos.Y = y} = point
        {X = x*cosTheta - y*sinTheta; Y = (y*cosTheta + x*sinTheta)}
        
    /// Draws an annotation on the SVG canvas - equivalent of drawSymbol but used for visual objects
    /// with no underlying electrical component.
    /// annotations have an Annotation field and a dummy Component used to provide expected H,W
    let drawAnnotation (symbol:SymbolT.Symbol) boxH boxW=
        let transform = symbol.STransform
        let outlineColour, strokeWidth = "black", "1.0"
        let H,W = symbol.Component.H, symbol.Component.W
        let createAnyPath (startingPoint: XYPos) (pathAttr: string) colour strokeWidth outlineColour = 
            [makeAnyPath startingPoint pathAttr {defaultPath with Fill = colour; StrokeWidth = strokeWidth; Stroke = outlineColour}]
        match symbol.Annotation with
        | None -> 
            failwithf "Should not be getting Annotation = None for drawing scalingBox buttons "
        | Some a ->
            match a with
            | SymbolT.ScaleButton ->
                let shapePointsPre = 
                    [   (4.5, -2.); 
                        (4.5, -5.); (10.5, 0.); (4.5, 5.); (4.5, 2.);
                        (-4.5, 2.); 
                        (-4.5, 5.); (-10.5, 0.); (-4.5, -5.); (-4.5, -2.);
                        (4.5, -2.)
                    ]
                    |> List.map (fun (x,y) -> rotateScaleButtonPoint boxW boxH {X=x;Y=y})


                let shapePoints =  
                    [1..10]
                    |> List.fold (fun lst x -> (shapePointsPre[x] - shapePointsPre[x-1])::lst) [shapePointsPre[0]]
                    |> List.rev

                let arrowHeadTopRight = ((makeLineAttr (shapePoints[1].X) shapePoints[1].Y)) + ((makeLineAttr (shapePoints[2].X) shapePoints[2].Y)) + ((makeLineAttr (shapePoints[3].X) shapePoints[3].Y)) + ((makeLineAttr (shapePoints[4].X) shapePoints[4].Y))+ ((makeLineAttr (shapePoints[5].X) shapePoints[5].Y))
                let arrowHeadBottomLeft = ((makeLineAttr (shapePoints[6].X) shapePoints[6].Y)) + ((makeLineAttr (shapePoints[7].X) shapePoints[7].Y)) + ((makeLineAttr (shapePoints[8].X) shapePoints[8].Y)) + ((makeLineAttr (shapePoints[9].X) shapePoints[9].Y))+ ((makeLineAttr (shapePoints[10].X) shapePoints[10].Y))
                (createAnyPath (symbol.Pos+shapePoints[0])(arrowHeadTopRight+arrowHeadBottomLeft) "grey" strokeWidth outlineColour)
            
            | SymbolT.RotateButton _ ->
            
                //chooses the shape of curvy components so flip and rotations are correct
                //HLP23: Author Ismagilov
                let adjustCurvyPoints (points:XYPos[] List) = 
                    match transform.Rotation,transform.flipped with 
                        | Degree0, false -> points[0]
                        | Degree0, true -> points[2]
                        | Degree90, _-> points[1]
                        | Degree180, true -> points[0]
                        | Degree180, false -> points[2]
                        | Degree270,_ -> points[3]

                let curvyShape =
                    [   [| (W/3., 7.*H/9.); (0.,(-H/9.)); (-W/4.,(H/6.));(W/4.,H/6.);(0, -H/9.);(0., -W/2.);
                            (0, W/2.);(-W/4., 0);(0, H/9.);(W/4., 0);(0.001, 7.*W/18.);(0.001, -7.*W/18.)
                        |]
                        [|  (2.*W/3., 7.*H/9.); (0.,(-H/9.)); (W/4.,(H/6.));(-W/4.,H/6.);(0, -H/9.);(0.001, -W/2.);
                            (0.001, W/2.);(W/4., 0);(0, H/9.);(-W/4., 0);(0, 7.*W/18.);(0, -7.*W/18.)
                        |]
                    ]                                   
                    |> List.map (Array.map (fun (x,y) -> {X=x;Y=y}))
                    |> adjustCurvyPoints

                let arrowHead = ((makeLineAttr (curvyShape[1].X) curvyShape[1].Y)) + ((makeLineAttr (curvyShape[2].X) curvyShape[2].Y)) + ((makeLineAttr (curvyShape[3].X) curvyShape[3].Y)) + ((makeLineAttr (curvyShape[4].X) curvyShape[4].Y))
                let arcAttr1  = makePartArcAttr (W/2.)(curvyShape[5].Y) (curvyShape[5].X) (curvyShape[6].Y) (curvyShape[6].X)
                let touchUp = ((makeLineAttr (curvyShape[7].X) curvyShape[7].Y)) + ((makeLineAttr (curvyShape[8].X) curvyShape[8].Y)) + ((makeLineAttr (curvyShape[9].X) curvyShape[9].Y)) 
                let arcAttr2  = makePartArcAttr (7.*W/18.)(curvyShape[10].Y) (curvyShape[10].X) (curvyShape[11].Y) (curvyShape[11].X)

                (createAnyPath (symbol.Pos + curvyShape[0]) (arrowHead+arcAttr1+touchUp+arcAttr2) "grey" strokeWidth outlineColour) 


    let scalingBox = 
        match model.ScalingBox with
        | None -> [makeAnyPath {X=0;Y=0} (makeLineAttr 0.0 0.0) defaultPath] @ [makeCircle 0.0 0.0 {defaultCircle with R=0.0}]
        | _ -> 
            let {BoundingBox.TopLeft = {X=fX; Y=fY}; H=fH; W=fW} = model.ScalingBox.Value.ScalingBoxBound
            [makeAnyPath {X=fX+50.0+fW;Y=(fY-46.5)} ((makeLineAttr 0.0 (fH+96.5))+(makeLineAttr -(fW+100.0) 0)+(makeLineAttr 0.0 (-(fH)-100.0))+(makeLineAttr (fW+96.5) 0.0)) {defaultPath with StrokeDashArray="4,4"}] 
            @ drawAnnotation model.ScalingBox.Value.RotateDeg270Button (fH+100.) (fW+100.)
            @ drawAnnotation model.ScalingBox.Value.RotateDeg90Button (fH+100.) (fW+100.)
            @ drawAnnotation model.ScalingBox.Value.ScaleButton (fH+100.) (fW+100.)


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

    match model.Action, model.ScalingBox with // Display differently depending on what state Sheet is in
    | Selecting, _ ->
        // printfn "displaying selectingBox"
        displaySvgWithZoom model headerHeight style ( displayElements @ [ dragToSelectBox ] ) dispatch
    | ConnectingInput _, None | ConnectingOutput _, None ->
        displaySvgWithZoom model headerHeight style ( displayElements @ connectingPortsWire ) dispatch
    | ConnectingInput _, Some _  | ConnectingOutput _, Some _->
        displaySvgWithZoom model headerHeight style ( displayElements @ scalingBox @ connectingPortsWire ) dispatch
    | DragAndDrop, None ->
        displaySvgWithZoom model headerHeight style ( displayElements @ snaps) dispatch
    | DragAndDrop, Some _ ->
        displaySvgWithZoom model headerHeight style ( displayElements @ snaps @ scalingBox) dispatch
    | (MovingSymbols),_  ->
        displaySvgWithZoom model headerHeight style ( displayElements @ snaps @ scalingBox) dispatch
    | MovingWire _,_ -> 
        displaySvgWithZoom model headerHeight style (displayElements @ snaps) dispatch
    | Scaling, _ -> 
        // printfn "displaying scalingBox when action = scaling"
        displaySvgWithZoom model headerHeight style ( displayElements @  scalingBox ) dispatch
    | _ , Some _ -> 
        // printfn "displaying scalingBox when action is not scaling"
        displaySvgWithZoom model headerHeight style ( displayElements @  scalingBox ) dispatch

    | _ ->
        displaySvgWithZoom model headerHeight style displayElements dispatch
    //|> TimeHelpers.instrumentInterval "SheetView" start

