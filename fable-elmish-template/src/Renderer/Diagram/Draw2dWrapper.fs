(*
    f# wrapper for draw_lib library.
*)

module Draw2dWrapper

open DiagramTypes
open JSHelpers
open DiagramStyle
open Shapes

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
    | Input -> addPort' figure "input" locator
    | Output -> addPort' figure "output" locator

[<Emit("$0.add(new draw2d.shape.basic.Label({text:$1, stroke:0}), new draw2d.layout.locator.TopLocator());")>]
let private addLabel (figure : JSComponent) (label : string) : unit = jsNative

[<Emit("
$0.installEditPolicy(new draw2d.policy.figure.AntSelectionFeedbackPolicy({
    onSelect: function(canvas, figure, isPrimarySelection) {
        figure.setBackgroundColor('#ff675c');
        $1(figure);
    },
    onUnselect: function(canvas, figure) {
        figure.setBackgroundColor('gray');
        $2(figure);
    }
}));
")>]
let private installSelectionPolicy (figure : JSComponent) (onSelect : JSComponent -> unit) (onUnselect : JSComponent -> unit) : unit = jsNative

[<Emit("new draw2d.geo.Point($0, $1);")>]
let private createPoint (x : int) (y : int) : JSComponent = jsNative

[<Emit("new draw2d.shape.basic.Polygon({x:$0,y:$1,resizeable:false});")>]
let private createPolygon (x : int) (y : int) : JSComponent = jsNative

[<Emit("$0.addVertex($1);")>]
let private addVertex (polygon : JSComponent) (point : JSComponent) : JSComponent = jsNative

[<Emit("$0.removeVertexAt($1);")>]
let private removeVertexAt (polygon : JSComponent) (index : int) : JSComponent = jsNative

let private createShape (canvas : JSCanvas) (shape : Shape) (x : int) (y : int) (dispatch : JSDiagramMsg -> unit): JSComponent =
    let pol = createPolygon x y
    // Add vertices.
    shape.Vertices
    |> List.map (fun (vx, vy) -> createPoint (x + vx) (y + vy) |> addVertex pol)
    |> ignore
    // Remove first three default vertices.
    [0..2] |> List.map (fun _ -> removeVertexAt pol 0) |> ignore
    
    // Add ports.
    shape.Ports
    |> List.map (fun (pType, pLocator) -> addPort pol pType pLocator)
    |> ignore

    addLabel pol shape.Label
    installSelectionPolicy pol
        (SelectComponent >> dispatch)
        (UnselectComponent >> dispatch)
    addFigureToCanvas canvas pol
    pol

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
        connections.push(line.getPersistentAttributes());
    });
    return {components: components, connections: connections};
})();
")>]
let private getCanvasState (canvas : JSCanvas) : JSCanvasState = jsNative

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

    member this.CreateMux2 () =
        match canvas, dispatch with
        | None, _ | _, None -> log "Warning: Draw2dWrapper.CreateMux2 called when canvas or dispatch is None"
        | Some c, Some d -> createShape c mux2 100 100 d |> ignore
    
    member this.CreateBox () =
        match canvas, dispatch with
        | None, _ | _, None -> log "Warning: Draw2dWrapper.CreateBox called when canvas or dispatch is None"
        | Some c, Some d -> createShape c box 100 100 d |> ignore

    member this.GetCanvasState () =
        match canvas with
        | None ->
            log "Warning: Draw2dWrapper.GetCanvasState called when canvas is None"
            None
        | Some c ->
            Some <| getCanvasState c
