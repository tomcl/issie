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
}

type Messages =
    | InitCanvas of Canvas

// -- Init Model

let init() = { Canvas = new Draw2dWrapper() }

// -- Create View

let hideView model dispatch =
    div [ Style [Height "0px"; Width "0px"] ] [
        model.Canvas.CanvasReactElement (InitCanvas >> dispatch) Hidden
    ]

let displayView model dispatch =
    div [] [
        model.Canvas.CanvasReactElement (InitCanvas >> dispatch) Visible
        Button.button [ Button.Props [ OnClick (fun _ -> model.Canvas.CreateBox()) ] ] [ str "Add box" ]
        Button.button [ Button.Props [ OnClick (fun _ -> model.Canvas.GetDiagram())] ] [ str "Get state" ]
    ]

// -- Update Model

let update msg model =
    match msg with
    | InitCanvas canvas ->
        model.Canvas.InitCanvas canvas // Side effect of changing the wrapper state.
        model