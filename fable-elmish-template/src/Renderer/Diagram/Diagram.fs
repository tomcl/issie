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

type Model = {
    Canvas : Draw2dWrapper // JS Canvas object.
    Zoom : float
}

type Messages =
    | InitCanvas of Canvas
    //| ZoomIn
    //| ZoomOut

// -- Init Model

let init() = { Canvas = new Draw2dWrapper(); Zoom = 1.0 }

// -- Create View

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

let extractState (state : CanvasState) =
    log state
    let components : JSComponents = state?components
    let connections : JSConnections = state?connections
    log <| extractComponents components
    log <| extractConnections connections

let getStateAction model =
    match model.Canvas.GetCanvasState () with
    | None -> ()
    | Some state -> extractState state |> ignore

let hideView model dispatch =
    div [] [
        model.Canvas.CanvasReactElement (InitCanvas >> dispatch) Hidden
    ]

let displayView model dispatch =
    div [] [
        model.Canvas.CanvasReactElement (InitCanvas >> dispatch) Visible
        Button.button [ Button.Props [ OnClick (fun _ -> model.Canvas.CreateBox()) ] ] [ str "Add box" ]
        Button.button [ Button.Props [ OnClick (fun _ -> getStateAction model) ] ] [ str "Get state" ]
        //Button.button [ Button.Props [ OnClick (fun _ -> dispatch ZoomIn)] ] [ str "Zoom in" ]
        //Button.button [ Button.Props [ OnClick (fun _ -> dispatch ZoomOut)] ] [ str "Zoom out" ]
    ]

// -- Update Model

let update msg model =
    match msg with
    | InitCanvas canvas ->
        model.Canvas.InitCanvas canvas // Side effect of changing the wrapper state.
        model
    //| ZoomIn ->
    //    model.Canvas.SetZoom <| min (model.Zoom + 0.5) 10.0
    //    { model with Zoom = model.Zoom + 0.5 }
    //| ZoomOut ->
    //    model.Canvas.SetZoom <| max (model.Zoom - 0.5) 0.5
    //    { model with Zoom = model.Zoom - 0.5 }
