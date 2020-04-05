module Diagram

open Fulma
open Elmish
open Elmish.HMR
open Elmish.React
open Elmish.Debug
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core
open Fable.Core.JsInterop

open DiagramTypes
open Draw2dWrapper
open JSHelpers
open DiagramStyle

type Model = {
    Diagram : Draw2dWrapper
    Zoom : float
    State : Component list * Connection list
}

// -- Init Model

let init() = { Diagram = new Draw2dWrapper(); Zoom = 1.0; State = [], []}

// -- Create View

let prettyPrintState (components, connections) =
    [ str "Components:"; br [] ] @
    List.collect (fun c -> [ str <| sprintf "%A" c; br [] ]) components @
    [ str "Connections:"; br [] ] @
    List.collect (fun c -> [ str <| sprintf "%A" c; br [] ]) connections

let extractComponents (jsComponents : JSComponents) : Component list =
    let extractPorts ports portType =
        let portsLen = ports?length
        ([], [0..portsLen - 1]) ||> List.fold (
            fun state idx ->
                if ports?(idx)?port = portType // Append only if the portType is what required.
                then ports?(idx)?name :: state
                else state
        )
    let extract jsComponent : Component = {
        Id = jsComponent?id
        InputPorts = extractPorts jsComponent?ports "draw2d.InputPort"
        OutputPorts = extractPorts jsComponent?ports "draw2d.OutputPort" 
    }
    let componentsLen : int = jsComponents?length
    List.map (fun (i : int) -> extract jsComponents?(i)) [0..componentsLen - 1]

let extractConnections (jsConnections : JSConnections) : Connection list =
    let extractPort jsPort : ConnectionPort = {
        ComponentId = jsPort?node
        PortId = jsPort?port
    }
    let extract jsConnection : Connection = {
        Id = jsConnection?id
        Source = extractPort jsConnection?source
        Target = extractPort jsConnection?target
    }
    let connectionsLen : int = jsConnections?length
    List.map (fun (i : int) -> extract jsConnections?(i)) [0..connectionsLen - 1]

let extractState (state : JSCanvasState) =
    log state
    let components : JSComponents = state?components
    let connections : JSConnections = state?connections
    extractComponents components, extractConnections connections

let getStateAction model dispatch =
    match model.Diagram.GetCanvasState () with
    | None -> ()
    | Some state -> extractState state |> UpdateState |> dispatch

let hideView model dispatch =
    div [] [
        model.Diagram.CanvasReactElement (JSDiagramMsg >> dispatch) Hidden
    ]

let displayView model dispatch =
    div [] [
        model.Diagram.CanvasReactElement (JSDiagramMsg >> dispatch) Visible
        div [ rightSectionStyle ] [ str "hello" ]
        div [ bottomSectionStyle ] [
            Button.button [ Button.Props [ OnClick (fun _ -> model.Diagram.CreateBox()) ] ] [ str "Add box" ]
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
    | SelectFigure figure ->
        log "selected"
        log figure
        model
    | UnselectFigure figure ->
        log "unselected"
        log figure
        model

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
