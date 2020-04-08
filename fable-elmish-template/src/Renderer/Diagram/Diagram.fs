module Diagram

open Fulma
open Elmish
open Elmish.HMR
open Elmish.React
open Elmish.Debug
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.Browser
open Fable.Core
open Fable.Core.JsInterop

open DiagramTypes
open Draw2dWrapper
open JSHelpers
open DiagramStyle
open Extractor

type Model = {
    Diagram : Draw2dWrapper
    Zoom : float
    State : Component list * Connection list
    SelectedComponent : (Component * JSComponent) option // Keep a ref to the original jsObject to edit it.
}

// -- Init Model

let init() = {
    Diagram = new Draw2dWrapper()
    Zoom = 1.0
    State = [], []
    SelectedComponent = None
}

// -- Create View

let prettyPrintState (components, connections) =
    [ str "Components:"; br [] ] @
    List.collect (fun c -> [ str <| sprintf "%A" c; br [] ]) components @
    [ str "Connections:"; br [] ] @
    List.collect (fun c -> [ str <| sprintf "%A" c; br [] ]) connections

let getStateAction model dispatch =
    match model.Diagram.GetCanvasState () with
    | None -> ()
    | Some state -> extractState state |> UpdateState |> dispatch

let viewSelectedComponent model =
    match model.SelectedComponent with
    | None -> div [] [ str "No component selected" ]
    | Some (comp, jsComp) ->
        let formId = "component-properties-form-" + comp.Id
        let readOnlyFormField name value =
            Field.div [] [
                Label.label [] [ str name ]
                Control.div [] [ Input.text [ Input.Props [ ReadOnly true; Name name ]; Input.IsStatic true; Input.Value value ] ] ]
        let formField name defaultValue =
            // Use comp.Id as key to refresh. DefaultValue is only updated when
            // the form is created and not anymore. The Key force the re-rendering
            // of the element every time the Key value changes.
            Field.div [] [
                Label.label [] [ str name ]
                Control.div [] [ Input.text [ Input.Props [ Name name; Key comp.Id ]; Input.DefaultValue defaultValue ] ] ]
        let readOnlyIfNone name value =
            match value with
            | None -> readOnlyFormField name "none"
            | Some v -> formField name v
        form [ Id formId ] [
            readOnlyFormField "Id" comp.Id
            readOnlyFormField "Type" <| sprintf "%A" comp.Type
            readOnlyIfNone "Label" comp.Label
            readOnlyFormField "Input ports" <| sprintf "%d" comp.InputPorts.Length
            readOnlyFormField "Output ports" <| sprintf "%d" comp.OutputPorts.Length
            // Submit.
            Field.div [ Field.IsGrouped ] [
                Control.div [] [
                    Button.button [
                        Button.Color IsPrimary
                        Button.OnClick (fun e ->
                            e.preventDefault()
                            // TODO: dont think this is the right way to do it.
                            let form = document.getElementById <| formId
                            let label : string = getFailIfNull form ["elements"; "Label"; "value"]
                            // TODO: again, this very hacky.
                            jsComp?children?data?(0)?figure?setText(label)
                        )
                    ] [ str "Update" ]
                ]
            ]
        ]

let hideView model dispatch =
    div [] [
        model.Diagram.CanvasReactElement (JSDiagramMsg >> dispatch) Hidden
    ]

let displayView model dispatch =
    div [] [
        model.Diagram.CanvasReactElement (JSDiagramMsg >> dispatch) Visible
        div [ rightSectionStyle ] [
            div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
                Heading.h4 [] [ str "Component properties" ]
                viewSelectedComponent model
            ]
        ]
        div [ bottomSectionStyle ] [
            Button.button [ Button.Props [ OnClick (fun _ -> model.Diagram.CreateComponent Mux2 "mux2") ] ] [ str "Add mux2" ]
            Button.button [ Button.Props [ OnClick (fun _ -> model.Diagram.CreateComponent Not "") ] ] [ str "Add not" ]
            Button.button [ Button.Props [ OnClick (fun _ -> model.Diagram.CreateComponent And "") ] ] [ str "Add and" ]
            Button.button [ Button.Props [ OnClick (fun _ -> getStateAction model dispatch) ] ] [ str "Get state" ]
            div [] (prettyPrintState model.State)
        ]
        //Button.button [ Button.Props [ OnClick (fun _ -> dispatch ZoomIn)] ] [ str "Zoom in" ]
        //Button.button [ Button.Props [ OnClick (fun _ -> dispatch ZoomOut)] ] [ str "Zoom out" ]
    ]

// -- Update Model

let handleJSDiagramMsg msg model =
    match msg with
    | InitCanvas canvas -> // Should be triggered only once.
        model.Diagram.InitCanvas canvas
        model
    | SelectComponent jsComponent ->
        { model with SelectedComponent = Some (extractComponent jsComponent, jsComponent) }
    | UnselectComponent jsComponent ->
         { model with SelectedComponent = None }

let update msg model =
    match msg with
    | JSDiagramMsg msg' -> handleJSDiagramMsg msg' model
    | UpdateState (com, con) -> {model with State = (com, con)}
    //| ZoomIn ->
    //    model.Canvas.SetZoom <| min (model.Zoom + 0.5) 10.0
    //    { model with Zoom = model.Zoom + 0.5 }
    //| ZoomOut ->
    //    model.Canvas.SetZoom <| max (model.Zoom - 0.5) 0.5
    //    { model with Zoom = model.Zoom - 0.5 }
