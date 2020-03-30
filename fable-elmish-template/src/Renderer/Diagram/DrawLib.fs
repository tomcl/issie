(*
    f# wrapper for draw_lib library.
*)

module DrawLib

open DiagramTypes

open Fable.Core
open Fable.Core.JsInterop

//[<ImportAll("draw2d")>]
//let draw2d: obj = jsNative
//
//let createCanvas (id : string) : Canvas =
//    draw2d.Canvas(id);

[<Emit("new draw2d.Canvas($0);")>]
let createCanvas (id : string) : Canvas = jsNative

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
let initialiseCanvas (canvas : Canvas) : unit = jsNative

[<Emit("$0.add($1);")>]
let addFigureToCanvas (canvas : Canvas) figure : unit = jsNative

[<Emit("$0.createPort($1)")>]
let addPort (box : Box) (portName : string) : unit = jsNative
// Only input or output?

[<Emit("new draw2d.shape.basic.Rectangle({x:$0,y:$1,width:$2,height:$3});")>]
let createBox' (x : int) (y : int) (width : int) (height : int) : Box = jsNative

let createBox (canvas : Canvas) (x : int) (y : int) (width : int) (height : int) : Box =
    let box = createBox' x y width height
    addFigureToCanvas canvas box
    addPort box "input"
    addPort box "output"
    box

let createAndInitialiseCanvas (id : string) : Canvas =
    let canvas = createCanvas id
    initialiseCanvas canvas
    canvas

