module Renderer

open Elmish
open Elmish.HMR
open Elmish.React
open Elmish.Debug
open Fable.Core.JsInterop

importSideEffects "./../../app/scss/main.scss" 

// This setup is useful to add other pages, in case they are needed.

type Model = DiagramModelType.Model

type Messages = DiagramMessageType.Msg

// -- Init Model

let init() = DiagramMainView.init()

// -- Create View

let view model dispatch = DiagramMainView.displayView model dispatch

// -- Update Model

let update msg model = DiagramMainView.update msg model

Program.mkSimple init update view
|> Program.withReact "electron-app"
|> Program.run
