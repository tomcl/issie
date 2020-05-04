module DiagramMainView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

open DiagramTypes
open DiagramModelType
open CommonStyle
open Draw2dWrapper
open Extractor
open StateIO
open OnDiagramButtonsView
open CatalogueView
open SelectedComponentView
open SimulationView

// -- Init Model

let init() = {
    Diagram = new Draw2dWrapper()
    State = [], []
    SelectedComponent = None
    Simulation = None
    RightTab = Catalogue
    OpenPath = None
    Hilighted = [], []
    Clipboard = [], []
    LoadedComponents = []
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

let saveStateAction model dispatch =
    match model.Diagram.GetCanvasState () with
    | None -> ()
    | Some state -> extractState state
                    |> saveStateToFile model.OpenPath
                    |> SetOpenPath
                    |> dispatch 

let loadStateAction model dispatch =
    match loadStateFromFile () with
    | None -> ()
    | Some (path, canvasState) ->
        dispatch <| SetHighlighted ([],[]) // Remove current highlights.
        model.Diagram.FlushCommandStack () // Discard all undo/redo.
        model.Diagram.ClearCanvas()
        Some path |> SetOpenPath |> dispatch // Set the new filepath.
        // Finally load the new state in the canvas.
        let components, connections = canvasState
        List.map model.Diagram.LoadComponent components |> ignore
        List.map (model.Diagram.LoadConnection true) connections |> ignore

// TODO replace this with an openProject logic.
let loadComponentsFromFolder dispatch =
    (parseAllDiagramsInFolder "/home/marco/Documents/Imperial/FYP/diagrams/")
    |> SetLoadedComponents
    |> dispatch

// Views

let viewRightTab model dispatch =
    match model.RightTab with
    | Catalogue ->
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Catalogue" ]
            div [ Style [ MarginBottom "15px" ] ] [ str "Click component to add it to the diagram" ]
            viewCatalogue model
        ]
    | Properties ->
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Component properties" ]
            viewSelectedComponent model
        ]
    | Simulation ->
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Simulation" ]
            viewSimulation model dispatch
        ]

let hideView model dispatch =
    div [] [
        model.Diagram.CanvasReactElement (JSDiagramMsg >> dispatch) Hidden
    ]

let displayView model dispatch =
    div [] [
        model.Diagram.CanvasReactElement (JSDiagramMsg >> dispatch) Visible
        viewOnDiagramButtons model dispatch
        div [ rightSectionStyle ] [
            Tabs.tabs [ Tabs.IsFullWidth; Tabs.IsBoxed; Tabs.Props [ ] ] [
                Tabs.tab
                    [ Tabs.Tab.IsActive (model.RightTab = Catalogue) ]
                    [ a [ OnClick (fun _ -> ChangeRightTab Catalogue |> dispatch ) ] [ str "Catalogue" ] ]
                Tabs.tab
                    [ Tabs.Tab.IsActive (model.RightTab = Properties) ]
                    [ a [ OnClick (fun _ -> ChangeRightTab Properties |> dispatch ) ] [ str "Properties" ] ]
                Tabs.tab
                    [ Tabs.Tab.IsActive (model.RightTab = Simulation) ]
                    [ a [ OnClick (fun _ -> ChangeRightTab Simulation |> dispatch ) ] [ str "Simulation" ] ]
            ]
            viewRightTab model dispatch
        ]
        div [ bottomSectionStyle ] [
            Button.button [ Button.Props [ OnClick (fun _ -> getStateAction model dispatch) ] ] [ str "Get state" ]
            Button.button [ Button.Props [ OnClick (fun _ -> saveStateAction model dispatch ) ] ] [ str "Save diagram" ]
            Button.button [ Button.Props [ OnClick (fun _ -> loadStateAction model dispatch) ] ] [ str "Load diagram" ]
            Button.button [ Button.Props [ OnClick (fun _ -> loadComponentsFromFolder dispatch) ] ] [ str "Load components" ]
            div [] (prettyPrintState model.State)
        ]
    ]

// -- Update Model

let handleJSDiagramMsg msg model =
    match msg with
    | InitCanvas canvas -> // Should be triggered only once.
        model.Diagram.InitCanvas canvas
        model
    | SelectComponent jsComponent ->
        { model with SelectedComponent = Some <| extractComponent jsComponent }
    | UnselectComponent jsComponent ->
         { model with SelectedComponent = None }

let update msg model =
    match msg with
    | JSDiagramMsg msg' -> handleJSDiagramMsg msg' model
    | UpdateState (com, con) -> { model with State = (com, con) }
    | StartSimulation simData -> { model with Simulation = Some simData }
    | SetSimulationGraph graph ->
        match model.Simulation with
        | None -> failwithf "what? Simulation graph set when no simulation running"
        | Some sim ->
            match sim with
            | Error _ -> failwithf "what? Simulation graph set when simulation is error"
            | Ok simData -> { model with Simulation = { simData with Graph = graph } |> Ok |> Some }
    | EndSimulation -> { model with Simulation = None }
    | ChangeRightTab newTab -> { model with RightTab = newTab }
    | SetOpenPath openPath -> { model with OpenPath = openPath }
    | SetHighlighted (componentIds, connectionIds) ->
        let oldComponentIds, oldConnectionIds = model.Hilighted
        oldComponentIds
        |> List.map (fun (ComponentId c) -> model.Diagram.UnHighlightComponent c)
        |> ignore
        componentIds
        |> List.map (fun (ComponentId c) -> model.Diagram.HighlightComponent c)
        |> ignore
        oldConnectionIds
        |> List.map (fun (ConnectionId c) -> model.Diagram.UnHighlightConnection c)
        |> ignore
        connectionIds
        |> List.map (fun (ConnectionId c) -> model.Diagram.HighlightConnection c)
        |> ignore
        { model with Hilighted = (componentIds, connectionIds) }
    | SetClipboard components -> { model with Clipboard = components }
    | SetLoadedComponents lc -> { model with LoadedComponents = lc }
