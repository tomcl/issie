(*
    f# wrapper for draw_lib library.
*)

module Draw2dWrapper

open DiagramTypes
open JSHelpers
open DiagramStyle

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

// Interface with JS library.

[<Emit("
(function () {
    let canvas = new draw2d.Canvas($0, $1, $2);
    canvas.setScrollArea('#'+$0);
    // Make sure the canvas does not overflow the parent div.
    let style = document.getElementById($0).style;
    style.height = 'auto'; style.width = 'auto';
    return canvas;
})()
")>]
let private createCanvas (id : string) (width : int) (height : int) : JSCanvas = jsNative

[<Emit("
// Show canvas grid.
$0.installEditPolicy(new draw2d.policy.canvas.ShowGridEditPolicy());
// Fadeout all decorations like ports, resize handles if the user didn't move
// the mouse.
$0.installEditPolicy(new draw2d.policy.canvas.FadeoutDecorationPolicy());
// Install a Connection create policy which matches to a 'circuit like'
// connections.
$0.installEditPolicy( new draw2d.policy.connection.ComposedConnectionCreatePolicy(
    [
        // Create a connection via Drag&Drop of ports.
        new draw2d.policy.connection.DragConnectionCreatePolicy({
            createConnection:createConnection
        }),
        // Or via click and point.
        new draw2d.policy.connection.OrthogonalConnectionCreatePolicy({
            createConnection:createConnection
        })
    ])
);
// Install policy to allow to zoom with: SHIFT + mouse wheel.
$0.installEditPolicy(new draw2d.policy.canvas.WheelZoomPolicy());
")>]
let private initialiseCanvas (canvas : JSCanvas) : unit = jsNative

let private createAndInitialiseCanvas (id : string) : JSCanvas =
    let canvas = createCanvas id 3000 2000
    initialiseCanvas canvas
    canvas

[<Emit("$0.add($1);")>]
let private addFigureToCanvas (canvas : JSCanvas) (figure : JSComponent) : unit = jsNative

[<Emit("$0.createPort($1, $2)")>]
let private addPort' (figure : JSComponent) (portName : string) (locator : obj) : unit = jsNative

// InputPortLocator works better than LeftLocator, since it moves the port to
// make space for new ones (instead of overlapping them).
[<Emit("new draw2d.layout.locator.InputPortLocator()")>]
let private leftLocator () = jsNative

// Similarly to InputPortLocator, OutputPortLocator is better than RightLocator.
[<Emit("new draw2d.layout.locator.OutputPortLocator()")>]
let private rightLocator () = jsNative

// Using this multiple times in the same figure will produce an overlap.
[<Emit("new draw2d.layout.locator.BottomLocator()")>]
let private bottomLocator () = jsNative

// Using this multiple times in the same figure will produce an overlap.
[<Emit("new draw2d.layout.locator.TopLocator()")>]
let private topLocator () = jsNative

let private addPort (figure : JSComponent) (pType : PortType) (pLocation : PortLocation) : unit =
    let locator = match pLocation with
                  | DiagramTypes.Bottom -> bottomLocator ()
                  | DiagramTypes.Left -> leftLocator ()
                  | DiagramTypes.Right -> rightLocator ()
                  | DiagramTypes.Top -> topLocator ()
    match pType with
    | PortType.Input -> addPort' figure "input" locator
    | PortType.Output -> addPort' figure "output" locator

[<Emit("$0.add(new draw2d.shape.basic.Label({text:$1, stroke:0}), new draw2d.layout.locator.TopLocator());")>]
let private addLabel (figure : JSComponent) (label : string) : unit = jsNative

[<Emit("
$0.installEditPolicy(new draw2d.policy.figure.AntSelectionFeedbackPolicy({
    onSelect: function(canvas, figure, isPrimarySelection) {
        figure.setBackgroundColor('#ff675c');
        $1(figure);
    },
    onUnselect: function(canvas, figure) {
        figure.setBackgroundColor('lightgray');
        $2(figure);
    }
}));
")>]
let private installSelectionPolicy (figure : JSComponent) (onSelect : JSComponent -> unit) (onUnselect : JSComponent -> unit) : unit = jsNative

[<Emit("new draw2d.shape.digital.Input({x:$0,y:$1,resizeable:false});")>]
let private createDigitalInput (x : int) (y : int) : JSComponent = jsNative

[<Emit("new draw2d.shape.digital.Output({x:$0,y:$1,resizeable:false});")>]
let private createDigitalOutput (x : int) (y : int) : JSComponent = jsNative

[<Emit("new draw2d.shape.digital.Not({x:$0,y:$1,resizeable:false});")>]
let private createDigitalNot (x : int) (y : int) : JSComponent = jsNative

[<Emit("new draw2d.shape.digital.And({x:$0,y:$1,resizeable:false});")>]
let private createDigitalAnd (x : int) (y : int) : JSComponent = jsNative

[<Emit("new draw2d.shape.digital.Or({x:$0,y:$1,resizeable:false});")>]
let private createDigitalOr (x : int) (y : int) : JSComponent = jsNative

[<Emit("new draw2d.shape.digital.Xor({x:$0,y:$1,resizeable:false});")>]
let private createDigitalXor (x : int) (y : int) : JSComponent = jsNative

[<Emit("new draw2d.shape.digital.Nand({x:$0,y:$1,resizeable:false});")>]
let private createDigitalNand (x : int) (y : int) : JSComponent = jsNative

[<Emit("new draw2d.shape.digital.Nor({x:$0,y:$1,resizeable:false});")>]
let private createDigitalNor (x : int) (y : int) : JSComponent = jsNative

[<Emit("new draw2d.shape.digital.Xnor({x:$0,y:$1,resizeable:false});")>]
let private createDigitalXnor (x : int) (y : int) : JSComponent = jsNative

[<Emit("new draw2d.shape.digital.Mux2({x:$0,y:$1,resizeable:false});")>]
let private createDigitalMux2 (x : int) (y : int) : JSComponent = jsNative

let private initialiseComponent (comp : JSComponent) (dispatch : JSDiagramMsg -> unit) (label : string) : unit =
    // Install the behaviour on selection.
    installSelectionPolicy comp
        (SelectComponent >> dispatch)
        (UnselectComponent >> dispatch)
    // Every component is assumed to have a label (may be empty string).
    addLabel comp label

let private createComponent
        (canvas : JSCanvas)
        (componentType : ComponentType)
        (defaultLabel : string)
        (x : int)
        (y : int)
        (dispatch : JSDiagramMsg -> unit)
        : unit =
    let comp = match componentType with
               | Input  -> createDigitalInput x y
               | Output -> createDigitalOutput x y
               | Not  -> createDigitalNot x y
               | And  -> createDigitalAnd x y
               | Or   -> createDigitalOr x y
               | Xor  -> createDigitalXor x y
               | Nand -> createDigitalNand x y
               | Nor  -> createDigitalNor x y
               | Xnor -> createDigitalXnor x y
               | Mux2 -> createDigitalMux2 x y
    initialiseComponent comp dispatch defaultLabel
    addFigureToCanvas canvas comp

[<Emit("
$0.getFigures().find(function(figure){
    return figure.id === $1;
})
")>]
let private getComponentById (canvas : JSCanvas) (id : string) : JSComponent = jsNative

let private getAllComponents (canvas : JSCanvas) : JSComponent list =
    let figures = canvas?getFigures()?data // JS list of components.
    [0..figures?length - 1] |> List.map (fun i -> figures?(i))

// TODO: for now only supports labels.
let private editComponent (canvas : JSCanvas) (id : string) (newLabel : string) : unit =
    let jsComponent = getComponentById canvas id
    if isNull jsComponent
    then failwithf "what? could not find diagram component with Id: %s" id
    else jsComponent?children?data?(0)?figure?setText(newLabel) // TODO: this only works for labels and it is very hacky.

// TODO this can be probably made more efficient by only returning the
// attributes we care about.
// .getPersistentAttributes removes stuff we need (e.g. labels) and include
// stuff we dont need for runtime processing.
// Maybe writing a custom function is the right thing to do.
// When saving the state of a diagram to a file, you want to get the persitent
// attributes, of both figures and lines.
[<Emit("
(function () {
    let components = [];
    $0.getFigures().each(function (i, figure) {
        components.push(figure);
    });
    let connections = [];
    $0.getLines().each(function (i, line) {
        connections.push(line);
    });
    return {components: components, connections: connections};
})();
")>]
let private getCanvasState (canvas : JSCanvas) : JSCanvasState = jsNative

[<Emit("draw2dStateIO.getCanvasStateAsString($0)")>]
let private getCanvasStateAsString (canvas : JSCanvas) : string = jsNative

// TODO: remove.
let private testDiagram = "WwogIHsKICAgICJ0eXBlIjogImRyYXcyZC5zaGFwZS5kaWdpdGFsLkFuZCIsCiAgICAiaWQiOiAiNGFiNjA4ZGEtZjUzNS0xY2I3LTI1YzktNzlhODc2ZWEyOTU1IiwKICAgICJ4IjogMTAwLAogICAgInkiOiAxMDAsCiAgICAid2lkdGgiOiA0MCwKICAgICJoZWlnaHQiOiA0MCwKICAgICJhbHBoYSI6IDEsCiAgICAic2VsZWN0YWJsZSI6IHRydWUsCiAgICAiZHJhZ2dhYmxlIjogdHJ1ZSwKICAgICJhbmdsZSI6IDAsCiAgICAidXNlckRhdGEiOiB7CiAgICAgICJjb21wb25lbnRUeXBlIjogIkFuZCIsCiAgICAgICJzdG9yZWQiOiB7fQogICAgfSwKICAgICJjc3NDbGFzcyI6ICJkcmF3MmRfc2hhcGVfZGlnaXRhbF9BbmQiLAogICAgInBvcnRzIjogWwogICAgICB7CiAgICAgICAgInR5cGUiOiAiZHJhdzJkLklucHV0UG9ydCIsCiAgICAgICAgImlkIjogIjc1ZmFmNjNlLTExMDYtMGMwYS02ODBjLTk4NjgzNTc0OTJmYyIsCiAgICAgICAgIndpZHRoIjogMTAsCiAgICAgICAgImhlaWdodCI6IDEwLAogICAgICAgICJhbHBoYSI6IDEsCiAgICAgICAgInNlbGVjdGFibGUiOiBmYWxzZSwKICAgICAgICAiZHJhZ2dhYmxlIjogdHJ1ZSwKICAgICAgICAiYW5nbGUiOiAwLAogICAgICAgICJ1c2VyRGF0YSI6IHt9LAogICAgICAgICJjc3NDbGFzcyI6ICJkcmF3MmRfSW5wdXRQb3J0IiwKICAgICAgICAiYmdDb2xvciI6ICJyZ2JhKDc5LDEwNCwxMTIsMSkiLAogICAgICAgICJjb2xvciI6ICJyZ2JhKDI3LDI3LDI3LDEpIiwKICAgICAgICAic3Ryb2tlIjogMSwKICAgICAgICAiZGFzaGFycmF5IjogbnVsbCwKICAgICAgICAibWF4RmFuT3V0IjogOTAwNzE5OTI1NDc0MDk5MSwKICAgICAgICAibmFtZSI6ICJpbnB1dDAiLAogICAgICAgICJzZW1hbnRpY0dyb3VwIjogImdsb2JhbCIsCiAgICAgICAgInBvcnQiOiAiZHJhdzJkLklucHV0UG9ydCIsCiAgICAgICAgImxvY2F0b3IiOiAiZHJhdzJkLmxheW91dC5sb2NhdG9yLklucHV0UG9ydExvY2F0b3IiLAogICAgICAgICJsb2NhdG9yQXR0ciI6IHt9CiAgICAgIH0sCiAgICAgIHsKICAgICAgICAidHlwZSI6ICJkcmF3MmQuSW5wdXRQb3J0IiwKICAgICAgICAiaWQiOiAiZGI1NTNhYjUtNjhlZC1hYzYxLWQyZDMtZGZkNjlmNTVkNzQ1IiwKICAgICAgICAid2lkdGgiOiAxMCwKICAgICAgICAiaGVpZ2h0IjogMTAsCiAgICAgICAgImFscGhhIjogMSwKICAgICAgICAic2VsZWN0YWJsZSI6IGZhbHNlLAogICAgICAgICJkcmFnZ2FibGUiOiB0cnVlLAogICAgICAgICJhbmdsZSI6IDAsCiAgICAgICAgInVzZXJEYXRhIjoge30sCiAgICAgICAgImNzc0NsYXNzIjogImRyYXcyZF9JbnB1dFBvcnQiLAogICAgICAgICJiZ0NvbG9yIjogInJnYmEoNzksMTA0LDExMiwxKSIsCiAgICAgICAgImNvbG9yIjogInJnYmEoMjcsMjcsMjcsMSkiLAogICAgICAgICJzdHJva2UiOiAxLAogICAgICAgICJkYXNoYXJyYXkiOiBudWxsLAogICAgICAgICJtYXhGYW5PdXQiOiA5MDA3MTk5MjU0NzQwOTkxLAogICAgICAgICJuYW1lIjogImlucHV0MSIsCiAgICAgICAgInNlbWFudGljR3JvdXAiOiAiZ2xvYmFsIiwKICAgICAgICAicG9ydCI6ICJkcmF3MmQuSW5wdXRQb3J0IiwKICAgICAgICAibG9jYXRvciI6ICJkcmF3MmQubGF5b3V0LmxvY2F0b3IuSW5wdXRQb3J0TG9jYXRvciIsCiAgICAgICAgImxvY2F0b3JBdHRyIjoge30KICAgICAgfSwKICAgICAgewogICAgICAgICJ0eXBlIjogImRyYXcyZC5PdXRwdXRQb3J0IiwKICAgICAgICAiaWQiOiAiMzcxMDllODgtYzNkYi1iNDg5LTU5ZTYtYmU0NjQ2YjVmNjA5IiwKICAgICAgICAid2lkdGgiOiAxMCwKICAgICAgICAiaGVpZ2h0IjogMTAsCiAgICAgICAgImFscGhhIjogMSwKICAgICAgICAic2VsZWN0YWJsZSI6IGZhbHNlLAogICAgICAgICJkcmFnZ2FibGUiOiB0cnVlLAogICAgICAgICJhbmdsZSI6IDAsCiAgICAgICAgInVzZXJEYXRhIjoge30sCiAgICAgICAgImNzc0NsYXNzIjogImRyYXcyZF9PdXRwdXRQb3J0IiwKICAgICAgICAiYmdDb2xvciI6ICJyZ2JhKDc5LDEwNCwxMTIsMSkiLAogICAgICAgICJjb2xvciI6ICJyZ2JhKDI3LDI3LDI3LDEpIiwKICAgICAgICAic3Ryb2tlIjogMSwKICAgICAgICAiZGFzaGFycmF5IjogbnVsbCwKICAgICAgICAibWF4RmFuT3V0IjogOTAwNzE5OTI1NDc0MDk5MSwKICAgICAgICAibmFtZSI6ICJvdXRwdXQwIiwKICAgICAgICAic2VtYW50aWNHcm91cCI6ICJnbG9iYWwiLAogICAgICAgICJwb3J0IjogImRyYXcyZC5PdXRwdXRQb3J0IiwKICAgICAgICAibG9jYXRvciI6ICJkcmF3MmQubGF5b3V0LmxvY2F0b3IuT3V0cHV0UG9ydExvY2F0b3IiLAogICAgICAgICJsb2NhdG9yQXR0ciI6IHt9CiAgICAgIH0KICAgIF0sCiAgICAiYmdDb2xvciI6ICJyZ2JhKDIxMSwyMTEsMjExLDEpIiwKICAgICJjb2xvciI6ICJyZ2JhKDI3LDI3LDI3LDEpIiwKICAgICJzdHJva2UiOiAwLAogICAgInJhZGl1cyI6IDAsCiAgICAiZGFzaGFycmF5IjogbnVsbAogIH0KXQ=="

[<Emit("draw2dStateIO.loadCanvasStateFromString($0, $1)")>]
let private loadCanvasStateFromString (canvas : JSCanvas) (state : string) : unit = jsNative

// React wrapper.

type DisplayModeType = Hidden | Visible

type Draw2dReactProps = {
    Dispatch : JSDiagramMsg -> unit
    DisplayMode : DisplayModeType
}

type Draw2dReact(initialProps) =
    inherit PureStatelessComponent<Draw2dReactProps>(initialProps)

    let divId = "Draw2dCanvas"

    override this.componentDidMount() =
        log "Mounting Draw2dReact component"
        createAndInitialiseCanvas divId |> InitCanvas |> this.props.Dispatch

    override this.render() =
        let style = match this.props.DisplayMode with
                    | Hidden -> canvasHiddenStyle
                    | Visible -> canvasVisibleStyle
        div [ Id divId; style ] []

let inline private createDraw2dReact props = ofType<Draw2dReact,_,_> props []

type Draw2dWrapper() =
    let mutable canvas : JSCanvas option = None
    let mutable dispatch : (JSDiagramMsg -> unit) option = None

    /// Returns a react element containing the canvas.
    /// The dispatch function has to be: JSDiagramMsg >> dispatch
    member this.CanvasReactElement jsDiagramMsgDispatch displayMode =
        // Initialise dispatch if needed.
        match dispatch with
        | None -> dispatch <- Some jsDiagramMsgDispatch
        | Some _ -> ()
        // Return react element with relevant props.
        createDraw2dReact {
            Dispatch = jsDiagramMsgDispatch
            DisplayMode = displayMode
        }

    member this.InitCanvas newCanvas =
        match canvas with
        | None -> canvas <- Some newCanvas
        | Some _ -> failwithf "what? InitCanvas should never be called when canvas is already created" 

    member this.CreateComponent componentType defaultLabel =
        match canvas, dispatch with
        | None, _ | _, None -> log "Warning: Draw2dWrapper.CreateComponent called when canvas or dispatch is None"
        | Some c, Some d -> createComponent c componentType defaultLabel 100 100 d

    // For now only changes the label. TODO
    member this.EditComponent componentId newLabel = 
        match canvas with
        | None -> log "Warning: Draw2dWrapper.EditComponent called when canvas is None"
        | Some c -> editComponent c componentId newLabel

    member this.GetCanvasState () =
        match canvas with
        | None ->
            log "Warning: Draw2dWrapper.GetCanvasState called when canvas is None"
            None
        | Some c ->
            Some <| getCanvasState c

    member this.GetCanvasStateAsString () =
        match canvas with
        | None ->
            log "Warning: Draw2dWrapper.GetCanvasStateAsString called when canvas is None"
            None
        | Some c ->
            Some <| getCanvasStateAsString c

    member this.LoadCanvasStateFromString state =
        match canvas, dispatch with
        | None, _ | _, None -> log "Warning: Draw2dWrapper.LoadCanvasStateFromString called when canvas or dispatch is None"
        | Some c, Some d ->
            // TODO: clearDiagram
            loadCanvasStateFromString c testDiagram
            let getLabelFromUserData (comp : JSComponent) : string =
                getFailIfNull comp ["userData"; "stored"; "label"]
            getAllComponents c
            |> List.map (fun comp -> initialiseComponent comp d (getLabelFromUserData comp))
            |> ignore
