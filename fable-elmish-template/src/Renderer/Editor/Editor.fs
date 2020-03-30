module Editor

open Fulma
open Elmish
open Elmish.HMR
open Elmish.React
open Elmish.Debug
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core
open Fable.Core.JsInterop

open EditorTypes
open CodeMirrorWrap

// TODO move all of these JS functions and interface them properly.

[<Emit("typeof $0")>]
let jsType (x: obj) : unit = jsNative

[<Emit("console.log($0)")>]
let log msg : unit = jsNative

[<Emit("alert($0)")>]
let alert msg : unit = jsNative

type Model = {
    Editor: Editor option;
    Code: string;
}

type Messages =
    | CreateEditor
    | ChangeCode of string

// -- Init Model

let init() = { Code = ""; Editor = None }

// -- Create View

let view model dispatch =
    div [] [
        div [ Id "editor"; Style [ Width "100%"; Height "50%" ]] []
        Button.button [ Button.OnClick (fun _ -> CreateEditor |> dispatch )] [ str "Create Editor" ]
    ]

// -- Update Model

let update msg model =
    match msg with
    | ChangeCode code -> { model with Code = code }
    | CreateEditor -> { model with Editor = Some <| createEditor "editor" }
