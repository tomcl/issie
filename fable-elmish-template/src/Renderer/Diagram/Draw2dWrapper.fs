(*
    f# wrapper for draw_lib library.
*)

module Draw2dWrapper

open DiagramTypes
open JSHelpers

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

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

let private createAndInitialiseCanvas (id : string) : Canvas =
    let canvas = createCanvas id
    initialiseCanvas canvas
    canvas

[<Emit("$0.add($1);")>]
let private addFigureToCanvas (canvas : Canvas) figure : unit = jsNative

[<Emit("$0.createPort($1)")>]
let private addPort (box : Box) (portName : string) : unit = jsNative
// Only input or output?

[<Emit("new draw2d.shape.basic.Rectangle({x:$0,y:$1,width:$2,height:$3});")>]
let private createBox' (x : int) (y : int) (width : int) (height : int) : Box = jsNative

let private createBox (canvas : Canvas) (x : int) (y : int) (width : int) (height : int) : Box =
    let box = createBox' x y width height
    addPort box "input"
    addPort box "output"
    addFigureToCanvas canvas box
    box

[<Emit("
var writer = new draw2d.io.json.Writer();
writer.marshal($0,function(json){
    console.log(JSON.stringify(json, null, 2));
});
")>]
let private logCanvas (canvas : Canvas) : unit = jsNative

[<Emit("
const dim = new draw2d.geo.Rectangle({x:0,y:0,width:$1,height:$2});
$0.setDimension(dim);
")>]
let private resizeCanvas (canvas : Canvas) (width : int) (height : int) : unit = jsNative

[<Emit("$0.setZoom($1, true);")>]
let private setZoom (canvas : Canvas) (zoom : float) : unit = jsNative

// React wrapper.

type DisplayModeType = Hidden | Visible

type Draw2dReactProps = {
    DispatchFunc : Canvas -> unit
    DisplayMode : DisplayModeType
}

type Draw2dReact(initialProps) =
    inherit PureStatelessComponent<Draw2dReactProps>(initialProps)

    let divId = "Draw2dCanvas"
    let hiddenStyle = Style [ Height "0px"; Width "0px" ]
    let visibleStyle = Style [ Height "500px"; Width "1000px" ]

    override this.componentDidMount() =
        log "Mounting Draw2dReact component"
        this.props.DispatchFunc <| createAndInitialiseCanvas divId

    override this.render() =
        let style = match this.props.DisplayMode with
                    | Hidden -> hiddenStyle
                    | Visible -> visibleStyle
        div [ Id divId; style ] []

let inline private createDraw2dReact props = ofType<Draw2dReact,_,_> props []

type Draw2dWrapper() =
    let mutable canvas : Canvas option = None

    /// Returns a react element containing the canvas.
    /// The dispatch function has to be: InitCanvas >> dispatch
    member this.CanvasReactElement dispatch displayMode =
        createDraw2dReact { DispatchFunc = dispatch; DisplayMode = displayMode } // Already created.

    member this.InitCanvas newCanvas =
        match canvas with
        | None -> canvas <- Some newCanvas
        | Some _ -> failwithf "what? InitCanvas should never be called when canvas is already created" 

    member this.GetDiagram () = // TODO
        match canvas with
        | None ->
            log "Warning: Draw2dWrapper.GetDiagram called when canvas is None"
        | Some c -> logCanvas c
    
    member this.CreateBox () =
        match canvas with
        | None -> log "Warning: Draw2dWrapper.CreateBox called when canvas is None"
        | Some c -> createBox c 100 100 50 50 |> ignore
    
    member this.ResizeCanvas width height =
        match canvas with
        | None -> log "Warning: Draw2dWrapper.ResizeCanvas called when canvas is None"
        | Some c -> resizeCanvas c width height

    member this.SetZoom zoom =
        match canvas with
        | None -> log "Warning: Draw2dWrapper.SetZoom called when canvas is None"
        | Some c -> setZoom c zoom 
