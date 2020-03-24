module App.View

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core
open Fable.Core.JsInterop

open Fable.React
open Fable.React.Props


open Elmish.React
open Elmish.Debug
open Elmish.HMR

open MyStyle
open DrawLib
open Types

[<Emit("typeof $0")>]
let jsType (x: obj) : unit = jsNative

[<Emit("console.log($0)")>]
let log msg : unit = jsNative

[<Emit("alert($0)")>]
let alert msg : unit = jsNative

type Model = {
    Svg : Option<SVG>; // JS InteractiveSVG object.
    Components : DiagramComponent list
}

type Messages =
    | NewSvg of SVG
    | NewPoint of Point
    | NewLine of Line
    | NewSquare of Square

// -- Init Model

let init() = { Svg = None; Components = []}

// -- Update Model

let update msg model =
    match msg with
    | NewSvg svg -> { model with Svg = Some svg }
    | NewPoint point -> { model with Components = Pt point :: model.Components }
    | NewLine line -> { model with Components = Ln line :: model.Components }
    | NewSquare square -> { model with Components = Sq square :: model.Components }

// -- Create View

let text (content: string) : ReactElement = unbox content

let createSVGAction model dispatch =
    match model.Svg with
    | Some _ -> alert "Svg element already created"
    | None -> createSVG "svg" 240 400 |> NewSvg |> dispatch

let addPointAction model dispatch =
    match model.Svg with
    | None -> alert "No Svg element is present"
    | Some svg -> addPoint svg 10 100 |> NewPoint |> dispatch

let addLineAction model dispatch =
    match model.Svg with
    | None -> alert "No Svg element is present"
    | Some svg ->
        let p1 : Point = addPoint svg 50 100
        let p2 : Point = addPoint svg 70 130
        addLine svg p1 p2 |> NewLine |> dispatch

let addSquareAction model dispatch =
    match model.Svg with
    | None -> alert "No Svg element is present"
    | Some svg -> addSquare svg 10 100 50 50 |> NewSquare |> dispatch

let pageHeader model =
    match model.Svg with
    | None -> text "Drawing demo"
    | Some _ -> text <| sprintf "Drawing demo %A" model.Components

let view model dispatch =
    div
        [ bodyStyle ]
        [
            h1 [ titleStyle ] [ pageHeader model ]
            div [ ClassName "svg-wrapper"; Id "svg" ] []
            button [ OnClick (fun _ -> createSVGAction model dispatch)] [ text "Create svg" ]
            button [ OnClick (fun _ -> addPointAction model dispatch)] [ text "Add point" ]
            button [ OnClick (fun _ -> addLineAction model dispatch)] [ text "Add line" ]
            button [ OnClick (fun _ -> addSquareAction model dispatch)] [ text "Add square" ]
        ]

// App
Program.mkSimple init update view
|> Program.withReactBatched "elmish-app"
|> Program.run
