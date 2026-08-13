module SheetDisplay
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

// Chromium reports a trackpad pinch as a wheel event with ctrlKey/metaKey set, even though no
// physical modifier key is held. This single flag is shared with KeyBindings so the two gestures
// can be handled differently without keeping a second copy of the same state.
let mutable private physicalModifierHeld = false

let setPhysicalModifierHeld value =
    physicalModifierHeld <- value

let isPhysicalModifierHeld () = physicalModifierHeld

/// This actually writes to the DOM a new scroll position.
/// In the special case that DOM has not yet been created it does nothing.
let writeCanvasScroll (scrollPos:XYPos) =
    putScrollProps scrollPos

/// The canvas div currently mounted in the DOM, used to detect when React has created a fresh
/// one. Under React 18 a hot reload remounts the whole tree, so the canvas loses its DOM scroll
/// position; the model - which hot reload preserves - still holds the real position, and it is
/// restored once per fresh mount. Normal renders reuse the same element and write nothing.
/// Cleared on unmount so this reference never keeps a detached canvas (and its whole SVG
/// subtree) alive - holding DOM refs past unmount was a documented React 17 leak, and there is
/// no reason to rely on React 18 having fixed it when the reference can simply be dropped.
let mutable private mountedCanvas: Browser.Types.Element option = None

/// The scroll position held in the model at the last render, for canvasRef to restore from.
/// Passed this way rather than by closing over the model so that canvasRef keeps the same
/// function identity across renders: React then invokes it only on real mount and unmount,
/// not on every render (a changed ref identity makes React call oldRef(null) + newRef(el)
/// each render, which would defeat the fresh-mount detection).
let mutable private modelScrollPos: XYPos = {X=0.; Y=0.}

/// Canvas div ref: on a fresh mount restore the model's scroll position; on unmount drop the
/// element reference.
/// NB the ref passed to React must be the SAME function object on every render: React
/// re-invokes a ref whose identity changed (null, then the element) on each render, which would
/// make the mount detection fire continually and yank the scroll to the model's lagging
/// position (seen as the canvas oscillating between two nearby offsets). Fable eta-expands a
/// module-level function - named or let-bound lambda alike - into a fresh arrow at every use
/// site, so the one closure is pinned inside `lazy`: it is created once, and .Value returns
/// that same object each render.
let private canvasRef : Lazy<Browser.Types.Element -> unit> =
    lazy
        (fun el ->
            match el with
            | null -> mountedCanvas <- None
            | el ->
                let isNewMount =
                    match mountedCanvas with
                    | Some prev -> not (System.Object.ReferenceEquals(prev, el))
                    | None -> true
                if isNewMount then
                    mountedCanvas <- Some el
                    let canvas = el :?> Browser.Types.HTMLElement
                    canvas.scrollLeft <- modelScrollPos.X
                    canvas.scrollTop <- modelScrollPos.Y)

let getDrawBlockPos (ev: Types.MouseEvent) (headerHeight: float) (sheetModel:Model) =
    {
        X = (ev.pageX + sheetModel.ScreenScrollPos.X) / sheetModel.Zoom  ;
        Y = (ev.pageY - headerHeight + sheetModel.ScreenScrollPos.Y) / sheetModel.Zoom
    }

let wheelUpdate (ev: Types.WheelEvent) _model dispatch =
    let isZoomGesture = ev.ctrlKey || ev.metaKey
    match Sheet.wheelZoom ev.deltaMode ev.deltaY isZoomGesture (isPhysicalModifierHeld ()) with
    | None -> ()
    | Some (PinchZoom zoomFactor)
    | Some (PhysicalWheelZoom zoomFactor) ->
        if abs (zoomFactor - 1.0) > 0.0001 then
            dispatch <| PreciseZoom zoomFactor

/// Is the mouse button currently down?
let mDown (ev:Types.MouseEvent) = ev.buttons <> 0.
    

/// Dispatch a MouseMsg (compensated for zoom)
let mouseOp op (ev:Types.MouseEvent) dispatch headerHeight=
    // right button oprations are only used for context menus
    if int ev.button = 0 then // button = 0 => left, button = 2 => right
        dispatch <| MouseMsgOrig (ev, op, headerHeight)
 


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
    // Keys used to be picked up here, by reassigning document.onkeydown on every render. They are
    // now handled once, in KeyBindings.

    let sizeInPixels = sprintf "%.2fpx" ((model.CanvasSize * model.Zoom))

    let currentCanvas = document.getElementById("Canvas")
    let cursorText = model.CursorType.Text()
    let firstView = viewIsAfterUpdateScroll
    viewIsAfterUpdateScroll <- false
    let scrollOpt = getScrollProps()
    let scrollAttrL: IHTMLProp list =
        match scrollOpt, firstView with
        | Some scroll, false ->
            [
                HTMLAttr.Custom("scrollleft", scroll.X); HTMLAttr.Custom("scrolltop", scroll.Y)
            ]
            
        | _ -> []
    modelScrollPos <- model.ScreenScrollPos
    let attrs : IHTMLProp list =
        [
              HTMLAttr.Id "Canvas"
              // Focusable, so that "which keys go where" is visible rather than implied: clicking
              // the canvas takes focus off any input box, Tab reaches it as an ordinary stop, and
              // the focus ring says where the keyboard is pointing.
              HTMLAttr.TabIndex 0
              Ref canvasRef.Value
              //Key cursorText // force cursor change to be rendered
              Style ( CSSProp.Cursor cursorText :: style)
              OnMouseDown (fun ev -> (mouseOp Down ev dispatch headerHeight))
              OnMouseUp (fun ev -> (mouseOp Up ev dispatch headerHeight))
              OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move) ev dispatch headerHeight)
              OnScroll (fun _ ->
                match scrollOpt with
                | None -> ()
                | Some scrollPos ->
                    if not firstView then
                        dispatch <| UpdateScrollPosFromCanvas scrollPos)
              let sPos = model.ScreenScrollPos
              match not firstView, scrollOpt with
                | true, Some scroll ->putScrollProps scroll |> ignore
                | _ -> ()
              OnWheel (fun ev -> wheelUpdate ev model dispatch)
        ]
    div (scrollAttrL @  attrs)
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
/// `overlay` is drawn on top of the wires, in draw block coordinates, so it pans and zooms with
/// the schematic. It exists for things the draw block cannot work out for itself - at present the
/// value on the wire under the cursor, which only the UI layer knows, because only it knows a
/// simulation is running. Empty the rest of the time.
let view
        (model:Model)
        (headerHeight: float)
        (style: CSSProp list)
        (overlay: ReactElement list)
        (dispatch : Msg -> unit)
            : ReactElement =
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
        let selectionBox = {Stroke = "Black"; StrokeWidth = "0.1px"; Fill = "Blue"; FillOpacity = 0.05 }

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
        // A near-black outline on mid-grey read as a rough drawing rather than as a control. These
        // are the only things on the canvas that are pressed rather than wired up, so they are
        // drawn as toolbar icons are: one dark shape, no outline of its own, on a pale disc that
        // gives them an edge and lifts them off whatever they happen to be over.
        let iconColour = "#5a6169"
        let outlineColour, strokeWidth = iconColour, "0.6"
        let H,W = symbol.Component.H, symbol.Component.W
        let createAnyPath (startingPoint: XYPos) (pathAttr: string) colour strokeWidth outlineColour =
            [makeAnyPath startingPoint pathAttr
                {defaultPath with
                    Fill = colour; StrokeWidth = strokeWidth; Stroke = outlineColour
                    // the arrowheads are sharp corners meeting curves: rounding the ends stops
                    // them reading as chipped at the sizes these are drawn at
                    StrokeLinecap = "round" }]
        /// Shrink `elements` about `centre`, leaving that point where it is.
        ///
        /// The rotate curl is drawn from its own geometry, which comes out reaching 16.8 units
        /// from the centre of a disc of 15 - so it crossed the edge, and sat a little off centre
        /// besides. Scaling brings the whole of it inside with the margin the scale arrow has
        /// (11.2 inside 13), and takes the off-centring down with it, without touching the shape
        /// or the machinery that turns it for each of the two rotate buttons.
        let scaledAbout (centre: XYPos) (factor: float) elements =
            [ g [ Style [ Transform
                            $"translate({centre.X}px, {centre.Y}px) scale({factor}) \
                              translate({-centre.X}px, {-centre.Y}px)" ] ]
                  elements ]
        /// The disc an icon sits on. The centre is passed in because the two annotations are drawn
        /// from different origins: the scale button's shape is built around symbol.Pos, and the
        /// rotate button's from it as a top-left corner.
        let iconDisc (centre: XYPos) radius =
            [ makeCircle centre.X centre.Y
                { defaultCircle with
                    R = radius; Fill = "#f7f8fa"; FillOpacity = 0.95
                    Stroke = "#b9c0c7"; StrokeWidth = "1.0" } ]
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
                // 13, because the double arrow is 21 across and has to sit inside its disc
                iconDisc symbol.Pos 13.
                @ (createAnyPath (symbol.Pos+shapePoints[0])(arrowHeadTopRight+arrowHeadBottomLeft)
                       iconColour strokeWidth outlineColour)
            
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

                let centre = symbol.Pos + { X = W / 2.; Y = H / 2. }
                iconDisc centre 15.
                @ scaledAbout centre 0.78
                      (createAnyPath (symbol.Pos + curvyShape[0]) (arrowHead+arcAttr1+touchUp+arcAttr2)
                           iconColour strokeWidth outlineColour)


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
        let connectPortsLine = {Stroke = "Green"; StrokeWidth = "2.0px"; StrokeDashArray = "5, 5" }
        let {XYPos.X = x1; Y = y1}, {XYPos.X = x2; Y = y2} = model.ConnectPortsLine
        [ makeLine x1 y1 x2 y2 connectPortsLine
          makeCircle x2 y2 { portCircle with Fill = "Green" }
        ]

    let displayElements =
        // overlay last, so it is drawn over the wires it is describing
        if model.ShowGrid
        then [ grid; wireSvg ] @ overlay
        else [ wireSvg ] @ overlay

    // uncomment the display model react for visbility of all snaps
    let snaps = snapIndicatorLineX @ snapIndicatorLineY // snapDisplay model

    match model.Action, model.ScalingBox with // Display differently depending on what state Sheet is in
    | Selecting, _ ->
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
        displaySvgWithZoom model headerHeight style ( displayElements @  scalingBox ) dispatch
    | _ , Some _ -> 
        displaySvgWithZoom model headerHeight style ( displayElements @  scalingBox ) dispatch

    | _ ->
        displaySvgWithZoom model headerHeight style displayElements dispatch
    //|> TimeHelpers.instrumentInterval "SheetView" start

