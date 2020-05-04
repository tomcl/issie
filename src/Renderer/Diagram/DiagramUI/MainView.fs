module DiagramMainView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

open JSHelpers
open DiagramTypes
open DiagramMessageType
open DiagramModelType
open CommonStyle
open Draw2dWrapper
open Extractor
open StateIO
open OnDiagramButtonsView
open CatalogueView
open SelectedComponentView
open SimulationView
open PopupView

// -- Init Model

let init() = {
    Diagram = new Draw2dWrapper()
    State = [], []
    SelectedComponent = None
    Simulation = None
    RightTab = Catalogue
    CurrProject = None
    Hilighted = [], []
    Clipboard = [], []
    Popup = None
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
    | Some state -> () //extractState state TODO
                    //|> saveStateToFile model.OpenPath
                    //|> SetOpenPath
                    //|> dispatch 

let loadStateAction model dispatch = ()
//    match loadStateFromFile () with
//    | None -> ()
//    | Some (path, canvasState) ->
//        dispatch <| SetHighlighted ([],[]) // Remove current highlights.
//        model.Diagram.FlushCommandStack () // Discard all undo/redo.
//        model.Diagram.ClearCanvas()
//        //Some path |> SetOpenPath |> dispatch // Set the new filepath. TODO
//        // Finally load the new state in the canvas.
//        let components, connections = canvasState
//        List.map model.Diagram.LoadComponent components |> ignore
//        List.map (model.Diagram.LoadConnection true) connections |> ignore

// TODO replace this with an openProject logic.
let loadComponentsFromFolder dispatch = ()
    //(parseAllDiagramsInFolder "/home/marco/Documents/Imperial/FYP/diagrams/")
    //|> SetLoadedComponents TODO
    //|> dispatch

////////

let loadStateIntoCanvas state model dispatch =
    dispatch <| SetHighlighted ([],[]) // Remove current highlights.
    model.Diagram.FlushCommandStack () // Discard all undo/redo.
    model.Diagram.ClearCanvas()        // Clear the canvas.
    // Finally load the new state in the canvas.
    let components, connections = state
    List.map model.Diagram.LoadComponent components |> ignore
    List.map (model.Diagram.LoadConnection true) connections |> ignore

let newProjectAction model dispatch _ =
    ()

let openProjectAction model dispatch _ =
    match askForExistingProjectPath () with
    | None -> () // User gave no path.
    | Some path ->
        match tryLoadComponentsFromPath path with
        | Error err -> log err // TODO: popup?
        | Ok components ->
            let openFileName, openFileState =
                match components with
                | [] -> failwith "what? EMPTY PROJECT NOT IMPLEMENTED YET" // TODO: create file, and load it?
                | comp :: _ -> comp.Name, comp.CanvasState
            loadStateIntoCanvas openFileState model dispatch
            {
                ProjectPath = path
                OpenFileName = openFileName
                LoadedComponents = components
            }
            |> SetProject |> dispatch

// Views

/// Display the content of the right tab.
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

/// Display the initial Open/Create Project menu at the beginning if no project
/// is open.
let viewNoProjectMenu model dispatch =
    let menuItem label action =
        Menu.Item.li [
            Menu.Item.IsActive false
            Menu.Item.OnClick action
        ] [ str label ]
    let initialMenu =
        Menu.menu [] [
            Menu.list [] [
                menuItem "New project" (newProjectAction model dispatch)
                menuItem "Open project" (openProjectAction model dispatch)
            ]
        ]
    match model.CurrProject with
    | Some _ -> div [] []
    | None ->
        stablePopup initialMenu

let hideView model dispatch =
    div [] [
        model.Diagram.CanvasReactElement (JSDiagramMsg >> dispatch) Hidden
    ]

let displayView model dispatch =
    div [] [
        model.Diagram.CanvasReactElement (JSDiagramMsg >> dispatch) Visible
        viewNoProjectMenu model dispatch
        viewPopup model
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
    | SetProject project -> { model with CurrProject = Some project }
    | ShowPopup popup -> { model with Popup = Some popup }
    | ClosePopup -> { model with Popup = None }
