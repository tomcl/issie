module Renderer

open Fulma
open Elmish
open Elmish.HMR
open Elmish.React
open Elmish.Debug
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core.JsInterop

importSideEffects "./../../app/scss/main.scss" 

type Model =
    { 
        Value: int 
    }
    
type Msg =
    | Increment 
    | Decrement

let init () = { Value = 0 }, Cmd.none

let update msg model =
    match msg with
    | Increment -> { model with Value = model.Value + 1 }, Cmd.none
    | Decrement -> { model with Value = model.Value - 1 }, Cmd.none
    
let view model dispatch =
  Hero.hero [] [
      Hero.body [] [
          div [ ClassName "block" ] [
              Box.box' [] [
                  str <| model.Value.ToString()
              ]
          ]
          Button.button [ Button.Props [ OnClick (fun _ -> Increment |> dispatch ) ] ] [
              str "+1"
          ]
          Button.button [ Button.Props [ OnClick (fun _ -> Decrement |> dispatch ) ] ] [
              str "-1"
          ]
      ]
  ]
 
Program.mkProgram init update view
|> Program.withReact "electron-app"
|> Program.run
