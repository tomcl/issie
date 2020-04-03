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
    Diagram : Diagram.Model
    Editor: Editor.Model
}

type Messages =
    | PageMsg of Page
    | DiagramMsg of Diagram.Messages
    | EditorMsg of Editor.Messages

// -- Init Model

let init() = {
    Page = DiagramPage
    Diagram = Diagram.init()
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
    then Diagram.displayView
    else Diagram.hideView
    <|| (model.Diagram, (DiagramMsg >> dispatch))

let view model dispatch =
    div [] [
        //Navbar.navbar [] [
        //    Navbar.Item.div [ Navbar.Item.IsHoverable ] [
        //        Navbar.Link.div [ Navbar.Link.IsArrowless; Navbar.Link.Option.Props.OnClick (fun _ -> PageMsg DiagramPage |> dispatch) ] [ str "Diagram" ]
        //    ]
        //]
        div [ navbarStyle ] [
            Button.button [Button.OnClick (fun _ -> PageMsg DiagramPage |> dispatch )] [str "Diagram"]
            Button.button [Button.OnClick (fun _ -> PageMsg EditorPage |> dispatch )] [str "Editor"]
        ]
        diagramView model dispatch
        editorView model dispatch
    ]

// -- Update Model

let update msg model =
    match msg with
    | PageMsg page -> { model with Page = page } 
    | DiagramMsg msg' -> { model with Diagram = Diagram.update msg' model.Diagram }
    | EditorMsg msg' -> {model with Editor = Editor.update msg' model.Editor }

Program.mkSimple init update view
|> Program.withReact "electron-app"
|> Program.run
