module Renderer

open Fulma
open Elmish
open Elmish.HMR
open Elmish.React
open Elmish.Debug
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core
open Fable.Core.JsInterop

open Types
open MyStyle
open DrawLib

importSideEffects "./../../app/scss/main.scss" 

// TODO move all of these JS functions and interface them properly.

[<Emit("typeof $0")>]
let jsType (x: obj) : unit = jsNative

[<Emit("console.log($0)")>]
let log msg : unit = jsNative

[<Emit("alert($0)")>]
let alert msg : unit = jsNative

[<Emit("
var writer = new draw2d.io.json.Writer();
writer.marshal($0,function(json){
    console.log(JSON.stringify(json, null, 2));
});
")>]
let logCanvas (canvas : Canvas) : unit = jsNative

type Model = {
    Canvas : Option<Canvas>; // JS Canvas object.
    Text : string;
}
    
type Messages =
    | NewCanvas of Canvas
    | NewBox of Box
    | GetState

// -- Init Model

let init() = { Canvas = None; Text = ""}

// -- Create View

let createCanvasAction model dispatch =
    match model.Canvas with
    | Some _ -> alert "Canvas already created"
    | None -> createAndInitialiseCanvas "canvas" |> NewCanvas |> dispatch

let createBoxAction model dispatch =
    match model.Canvas with
    | None -> alert "Canvas not present"
    | Some canvas -> createBox canvas 100 100 50 50 |> NewBox |> dispatch

let getStateAction model dispatch =
    match model.Canvas with
    | None -> alert "Canvas not present"
    | Some _ -> dispatch GetState

let extractState canvas =
    canvas |> Option.map logCanvas |> ignore 
    canvas

let pageHeader model =
    match model.Canvas with
    | None -> str "Drawing demo"
    | Some _ -> str "Drawing demo, Canvas created"

let view model dispatch =
    Hero.hero [] [
        Hero.body [] [
            div [ Id "canvas"; canvasStyle ] []
            Button.button [ Button.Props [ OnClick (fun _ -> createCanvasAction model dispatch) ] ] [ str "Create canvas" ]
            Button.button [ Button.Props [ OnClick (fun _ -> createBoxAction model dispatch) ] ] [ str "Add box" ]
            Button.button [ Button.Props [ OnClick (fun _ -> getStateAction model dispatch)] ] [ str "Get state" ]
            div [ ClassName "block" ] [ Box.box' [] [ str model.Text ] ]
        ]
    ]

// -- Update Model

let update msg model =
    match msg with
    | NewCanvas canvas -> { model with Canvas = Some canvas }
    | NewBox box -> model // Do nothing
    | GetState -> { model with Text = sprintf "%A" <| extractState model.Canvas }


Program.mkSimple init update view
|> Program.withReact "electron-app"
|> Program.run
