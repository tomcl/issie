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
open CodeMirrorWrapper
open JSHelpers

type Model = {
    Editor: Editor option;
    Code: string;
}

type Messages =
    | CreateEditor
    | ChangeCode of string
    | GetCode

// -- Init Model

let init() = { Code = ""; Editor = None }

// -- Create View

let view model dispatch =
    div [] [
        div [ Id "editor"; Style [ Width "100%"; Height "50%" ]] []
        Button.button [ Button.OnClick (fun _ -> CreateEditor |> dispatch )] [ str "Create Editor" ]
        Button.button [ Button.OnClick (fun _ -> GetCode |> dispatch )] [ str "Get code" ]
        div [] [ str model.Code ]
    ]

// -- Update Model

let update msg model =
    match msg with
    | CreateEditor -> { model with Editor = Some <|createEditor "editor" }
    | ChangeCode code -> { model with Code = code }
    | GetCode -> { model with Code = getCode <| Option.get model.Editor }
