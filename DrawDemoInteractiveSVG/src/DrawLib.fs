(*
    f# wrapper for draw_lib library.
*)

module DrawLib

open Types

open Fable.Core
open Fable.Core.JsInterop

//type IAlert =
//    abstract triggerAlert : message:string -> unit
//    abstract someString: string

//[<ImportAll("./draw_lib/alert.js")>]
//let mylib : IAlert = jsNative

[<Emit("InteractiveSVG.create($0, $1, $2);")>]
let createSVG (id : string) (width : int) (height : int) : SVG = jsNative

[<Emit("$0.addPoint({ x: $1, y: $2 });")>]
let addPoint (svg : SVG) (x : int) (y : int) : Point = jsNative

[<Emit("$0.addStaticPoint({ x: $1, y: $2, class: 'generated-point' });")>]
let addStaticPoint (svg : SVG) (x : int) (y : int) : Point = jsNative

[<Emit("$0.addLine({ p1: $1, p2: $2 });")>]
let addLine (svg : SVG) (p1 : Point) (p2 : Point) : Line = jsNative

let addSquare (svg : SVG) (x : int) (y : int) (width : int) (height : int) : Square =
    let topLeft     = addPoint svg x y
    let bottomLeft  = addStaticPoint svg x (y + height)
    let topRight    = addStaticPoint svg (x + width) y
    let bottomRight = addStaticPoint svg (x + width) (y + height)
    addLine svg topLeft topRight       |> ignore
    addLine svg topLeft bottomLeft     |> ignore
    addLine svg bottomLeft bottomRight |> ignore
    addLine svg topRight bottomRight   |> ignore
    Square topLeft

(*
[<Emit("$0.addStaticPoint({ x: $1.x + $2, y: $1.y + $3, class: 'generated-point' }).addDependency([$1],function($1) {return { cx: $1.x + $2, cy: $1.y + $3};});")>]
let addStaticPoint (svg : SVG) (p: Point) (xdis : int) (ydis : int) : Point = jsNative

let addSquare (svg : SVG) (x : int) (y : int) (width : int) (height : int) : Square =
    let topLeft     = addPoint svg x y
    let bottomLeft  = addStaticPoint svg topLeft 0 height
    let topRight    = addStaticPoint svg topLeft width 0
    let bottomRight = addStaticPoint svg topLeft width height
    addLine svg topLeft topRight       |> ignore
    addLine svg topLeft bottomLeft     |> ignore
    addLine svg bottomLeft bottomRight |> ignore
    addLine svg topRight bottomRight   |> ignore
    Square topLeft
*)
// let createPoint (x : int) (y : int) : obj = jsNative

//[<ImportAll("./draw_lib/interactiveSVG.js")>]
//let drawLib : IDrawLib = jsNative
