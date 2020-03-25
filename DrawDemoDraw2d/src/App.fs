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

let text (content: string) : ReactElement = unbox content

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
    | None -> text "Drawing demo"
    | Some _ -> text "Drawing demo, Canvas created"

let view model dispatch =
    div
        [ bodyStyle ]
        [
            //h1 [ titleStyle ] [ pageHeader model ]
            div [ Id "canvas"; canvasStyle ] []
            button [ OnClick (fun _ -> createCanvasAction model dispatch)] [ text "Create canvas" ]
            button [ OnClick (fun _ -> createBoxAction model dispatch)] [ text "Add box" ]
            button [ OnClick (fun _ -> getStateAction model dispatch)] [ text "Get state" ]
            br []
            br []
            textarea
                [ Id "state"; ReadOnly true; Style [ Height "200px"; Width "400px" ]; Value model.Text ]
                []
        ]

// -- Update Model

let update msg model =
    match msg with
    | NewCanvas canvas -> { model with Canvas = Some canvas }
    | NewBox box -> model // Do nothing
    | GetState -> { model with Text = sprintf "%A" <| extractState model.Canvas }

// App
Program.mkSimple init update view
|> Program.withReactBatched "elmish-app"
|> Program.run
