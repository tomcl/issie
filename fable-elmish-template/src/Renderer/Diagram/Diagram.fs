module Diagram

open Fulma
open Elmish
open Elmish.HMR
open Elmish.React
open Elmish.Debug
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core
open Fable.Core.JsInterop

open DiagramTypes
open MyStyle
open Draw2dWrapper
open JSHelpers

type Model = {
    Canvas : Draw2dWrapper // JS Canvas object.
    Zoom : float
}

type Messages =
    | InitCanvas of Canvas
    //| ZoomIn
    //| ZoomOut

// -- Init Model

let init() = { Canvas = new Draw2dWrapper(); Zoom = 1.0 }

// -- Create View

let hideView model dispatch =
    model.Canvas.ResizeCanvas 1 1
    div [] [
        model.Canvas.CanvasReactElement (InitCanvas >> dispatch) Hidden
    ]

let displayView model dispatch =
    model.Canvas.ResizeCanvas 1000 500
    div [] [
        model.Canvas.CanvasReactElement (InitCanvas >> dispatch) Visible
        Button.button [ Button.Props [ OnClick (fun _ -> model.Canvas.CreateBox()) ] ] [ str "Add box" ]
        Button.button [ Button.Props [ OnClick (fun _ -> model.Canvas.GetDiagram())] ] [ str "Get state" ]
        //Button.button [ Button.Props [ OnClick (fun _ -> dispatch ZoomIn)] ] [ str "Zoom in" ]
        //Button.button [ Button.Props [ OnClick (fun _ -> dispatch ZoomOut)] ] [ str "Zoom out" ]
    ]

// -- Update Model

let update msg model =
    match msg with
    | InitCanvas canvas ->
        model.Canvas.InitCanvas canvas // Side effect of changing the wrapper state.
        model
    //| ZoomIn ->
    //    model.Canvas.SetZoom <| min (model.Zoom + 0.5) 10.0
    //    { model with Zoom = model.Zoom + 0.5 }
    //| ZoomOut ->
    //    model.Canvas.SetZoom <| max (model.Zoom - 0.5) 0.5
    //    { model with Zoom = model.Zoom - 0.5 }
