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
    Editor: CodeMirrorWrapper;
    Code: string;
}

type Messages =
    | InitEditor of Editor // This message has to be dispatched only once.
    | GetCode

// -- Init Model

let init() = { Code = ""; Editor = new CodeMirrorWrapper() }

// -- Create View

/// View when the page is selected.
let displayView model dispatch =
    div [] [
        model.Editor.EditorReactElement (InitEditor >> dispatch) Visible
        Button.button [ Button.OnClick (fun _ -> GetCode |> dispatch )] [ str "Get code" ]
        div [] [ str model.Code ]
    ]

/// View when the page is hidden. We still display a 0px by 0px editor element
/// because the editor is attached to it.
let hideView model dispatch =
    div [] [
        model.Editor.EditorReactElement (InitEditor >> dispatch) Hidden
    ]

// -- Update Model

let update msg model =
    match msg with
    | InitEditor editor ->
        model.Editor.InitEditor editor
        model
    | GetCode -> { model with Code = model.Editor.GetCode() }
