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

open JSHelpers
open CommonStyle

importSideEffects "./../../app/scss/main.scss" 

type Page =
    | DiagramPage
    | EditorPage

type Model = {
    Page : Page
    Diagram : DiagramModelType.Model
    Editor: Editor.Model
}

type Messages =
    | PageMsg of Page
    | DiagramMsg of DiagramMessageType.Msg
    | EditorMsg of EditorTypes.Msg

// -- Init Model

let init() = {
    Page = DiagramPage
    Diagram = DiagramMainView.init()
    Editor = Editor.init()
}

// -- Create View

//let pageView model dispatch =
//    match model.Page with
//    | DiagramPage -> Diagram.view model.Diagram (DiagramMsg >> dispatch)
//    | EditorPage -> Editor.view model.Editor (EditorMsg >> dispatch)

let editorView model dispacth =
    if model.Page = EditorPage
    then Editor.displayView
    else Editor.hideView
    <|| (model.Editor, (EditorMsg >> dispacth))

let diagramView model dispatch =
    if model.Page = DiagramPage
    then DiagramMainView.displayView
    else DiagramMainView.hideView
    <|| (model.Diagram, (DiagramMsg >> dispatch))

let view model dispatch =
    div [] [
        div [ navbarStyle ] [
            Tabs.tabs [ Tabs.IsBoxed; Tabs.Props [ ] ] [
            Tabs.tab
                [ Tabs.Tab.IsActive (model.Page = DiagramPage) ]
                [ a [ OnClick (fun _ -> PageMsg DiagramPage |> dispatch ) ] [ str "Diagram" ] ]
            Tabs.tab
                [ Tabs.Tab.IsActive (model.Page = EditorPage) ]
                [ a [ OnClick (fun _ -> PageMsg EditorPage |> dispatch ) ] [ str "Editor" ] ]
            ]
        ]
        diagramView model dispatch
        editorView model dispatch
    ]

// -- Update Model

let update msg model =
    match msg with
    | PageMsg page -> { model with Page = page } 
    | DiagramMsg msg' -> { model with Diagram = DiagramMainView.update msg' model.Diagram }
    | EditorMsg msg' -> {model with Editor = Editor.update msg' model.Editor }

Program.mkSimple init update view
|> Program.withReact "electron-app"
|> Program.run
