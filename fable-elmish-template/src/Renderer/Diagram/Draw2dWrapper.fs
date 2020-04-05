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

// Messages that will be sent from JS code.
type JSEditorMsg =
    | InitCanvas of Canvas
    | SelectFigure of Figure
    | UnselectFigure of Figure

// Interface with JS library.

[<Emit("new draw2d.Canvas($0);")>]
let private createCanvas (id : string) : Canvas = jsNative

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
// 
")>]
let private initialiseCanvas (canvas : Canvas) : unit = jsNative

[<Emit("
const dim = new draw2d.geo.Rectangle({x:0,y:0,width:$1,height:$2});
$0.setDimension(dim);
")>]
let private resizeCanvas (canvas : Canvas) (width : int) (height : int) : unit = jsNative

let private createAndInitialiseCanvas (id : string) : Canvas =
    let canvas = createCanvas id
    initialiseCanvas canvas
    //resizeCanvas canvas 2000 1000
    canvas

[<Emit("$0.add($1);")>]
let private addFigureToCanvas (canvas : Canvas) (figure : Figure) : unit = jsNative

[<Emit("$0.createPort($1)")>]
let private addPort (figure : Figure) (portName : string) : unit = jsNative
// Only input or output?

[<Emit("$0.add(new draw2d.shape.basic.Label({text:$1, stroke:0}), new draw2d.layout.locator.TopLocator());")>]
let private addLabel (figure : Figure) (label : string) : unit = jsNative

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
let private installSelectionPolicy (figure : Figure) (onSelect : Figure -> unit) (onUnselect : Figure -> unit) : unit = jsNative

[<Emit("new draw2d.shape.basic.Rectangle({x:$0,y:$1,width:$2,height:$3,resizeable:false});")>]
let private createBox' (x : int) (y : int) (width : int) (height : int) : Figure = jsNative

let private createBox (canvas : Canvas) (x : int) (y : int) (width : int) (height : int) (dispatch : JSEditorMsg -> unit): Figure =
    let box = createBox' x y width height
    addPort box "input"
    addPort box "output"
    addLabel box "box"
    installSelectionPolicy box (SelectFigure >> dispatch) (UnselectFigure >> dispatch)
    addFigureToCanvas canvas box
    box

// TODO this can be probably made more efficient by only returning the
// attributes we care about.
[<Emit("
(function () {
    let components = [];
    $0.getFigures().each(function (i, figure) {
        components.push(figure.getPersistentAttributes());
    });
    let connections = [];
    $0.getLines().each(function (i, element) {
        connections.push(element.getPersistentAttributes());
    });
    return {components: components, connections: connections};
})();
")>]
let private getCanvasState (canvas : Canvas) : CanvasState = jsNative

[<Emit("$0.setZoom($1, true);")>]
let private setZoom (canvas : Canvas) (zoom : float) : unit = jsNative

// React wrapper.

type DisplayModeType = Hidden | Visible

type Draw2dReactProps = {
    Dispatch : JSEditorMsg -> unit
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
    let mutable canvas : Canvas option = None
    let mutable dispatch : (JSEditorMsg -> unit) option = None

    /// Returns a react element containing the canvas.
    /// The dispatch function has to be: JSEditorMsg >> dispatch
    member this.CanvasReactElement jsEditorMsgDispatch displayMode =
        // Initialise dispatch if needed.
        match dispatch with
        | None -> dispatch <- Some jsEditorMsgDispatch
        | Some _ -> ()
        // Return react element with relevant props.
        createDraw2dReact {
            Dispatch = jsEditorMsgDispatch
            DisplayMode = displayMode
        }

    member this.InitCanvas newCanvas =
        match canvas with
        | None -> canvas <- Some newCanvas
        | Some _ -> failwithf "what? InitCanvas should never be called when canvas is already created" 
    
    member this.CreateBox () =
        match canvas, dispatch with
        | None, _ | _, None -> log "Warning: Draw2dWrapper.CreateBox called when canvas or dispatch is None"
        | Some c, Some d -> createBox c 100 100 50 50 d |> ignore
    
    member this.ResizeCanvas width height =
        match canvas with
        | None -> log "Warning: Draw2dWrapper.ResizeCanvas called when canvas is None"
        | Some c -> resizeCanvas c width height

    member this.SetZoom zoom =
        match canvas with
        | None -> log "Warning: Draw2dWrapper.SetZoom called when canvas is None"
        | Some c -> setZoom c zoom 

    member this.GetCanvasState () =
        match canvas with
        | None ->
            log "Warning: Draw2dWrapper.GetCanvasState called when canvas is None"
            None
        | Some c ->
            Some <| getCanvasState c
