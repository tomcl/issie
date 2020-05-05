module DiagramMainView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

open JSHelpers
open DiagramStyle
open DiagramTypes
open DiagramMessageType
open DiagramModelType
open Draw2dWrapper
open Extractor
open FilesIO
open OnDiagramButtonsView
open CatalogueView
open SelectedComponentView
open SimulationView
open PopupView

// -- Init Model

let init() = {
    Diagram = new Draw2dWrapper()
    SelectedComponent = None
    Simulation = None
    RightTab = Catalogue
    CurrProject = None
    Hilighted = [], []
    Clipboard = [], []
    Popup = None
    PopupDialogText = None
}

// -- Create View

let loadStateIntoCanvas state model dispatch =
    dispatch <| SetHighlighted ([],[]) // Remove current highlights.
    model.Diagram.FlushCommandStack () // Discard all undo/redo.
    model.Diagram.ClearCanvas()        // Clear the canvas.
    // Finally load the new state in the canvas.
    let components, connections = state
    List.map model.Diagram.LoadComponent components |> ignore
    List.map (model.Diagram.LoadConnection true) connections |> ignore

let reloadProjectComponents project =
    match tryLoadComponentsFromPath project.ProjectPath with
    | Error err -> failwith "what? reloading project components" // TODO: this should probably not crash the program.
    | Ok components -> { project with LoadedComponents = components }

let saveOpenFileAction model =
    match model.Diagram.GetCanvasState (), model.CurrProject with
    | None, _ | _, None -> ()
    | Some state, Some project ->
        extractState state
        |> saveStateToFile project.ProjectPath project.OpenFileName
        |> ignore

let getFileInProject name project =
    project.LoadedComponents
    |> List.tryFind (fun comp -> comp.Name = name)

let isFileInProject name project =
    getFileInProject name project
    |> function | None -> false | Some _ -> true

/// Open the specified file.
let openFileInProject name model dispatch =
    match model.CurrProject with
    | None -> log "Warning: openFileInProject called with no project"
    | Some project ->
        saveOpenFileAction model // Save current file.
        match getFileInProject name project with
        | None -> log <| sprintf "Warning: openFileInProject could not find the component %s in the project" name
        | Some loadedComponent ->
            loadStateIntoCanvas loadedComponent.CanvasState model dispatch
            // Reload components so the project we just closed is up to date in
            // our CurrProj.
            { project with OpenFileName = name }
            |> reloadProjectComponents |> SetProject |> dispatch

/// Create a new file in this project. Do not open it automatically.
let addFileToProject model dispatch =
    match model.CurrProject with
    | None -> () // TODO log warning?
    | Some project ->
        // Prepare dialog popup.
        let title = "Add file to project"
        let before =
            fun popupDialogText ->
                let maybeWarning =
                    if isFileInProject popupDialogText project
                    then div [ Style [Color "red"] ] [ str "This file already exists." ]
                    else div [] []
                div [] [
                    str "A new file will be created at:"
                    br[]
                    str <| pathJoin [|project.ProjectPath; popupDialogText + ".dgm"|]
                    maybeWarning
                ]
        let placeholder = "Insert module name"
        let buttonText = "Add"
        let buttonAction =
            fun popupDialogText ->
                let name = popupDialogText
                // Create empty file.
                createEmptyDgmFile project.ProjectPath name
                // Add the file to the project.
                let newComponent = {
                    Name = name
                    FilePath = pathJoin [|project.ProjectPath; name + ".dgm"|]
                    CanvasState = [],[]
                    InputLabels = []
                    OutputLabels = []
                }
                { project with LoadedComponents = newComponent :: project.LoadedComponents }
                |> SetProject |> dispatch
                // Close the popup.
                dispatch ClosePopup
        let isDisabled =
            fun popupDialogText -> (isFileInProject popupDialogText project) || (popupDialogText = "")
        dialogPopup title before placeholder buttonText buttonAction isDisabled dispatch

// TODO remove file
// TODO change open file
// TODO nice menu
// TODO error popup

/// Create a new project.
let newProject model dispatch _ =
    match askForNewProjectPath () with
    | None -> () // User gave no path.
    | Some path ->
        match tryCreateFolder path with
        | Error err -> log err // TODO
        | Ok _ ->
            createEmptyDgmFile path "main"
            loadStateIntoCanvas ([],[]) model dispatch
            {
                ProjectPath = path
                OpenFileName = "main"
                LoadedComponents = []
            }
            |> SetProject |> dispatch

/// Open a project.
let openProject model dispatch _ =
    match askForExistingProjectPath () with
    | None -> () // User gave no path.
    | Some path ->
        match tryLoadComponentsFromPath path with
        | Error err -> log err // TODO: popup?
        | Ok components ->
            let openFileName, openFileState =
                match components with
                | [] -> // No files in the project. Create one and open it.
                    createEmptyDgmFile path "main"
                    "main", ([],[])
                | comp :: _ -> // Pick one file at random to open initally.
                    comp.Name, comp.CanvasState
            loadStateIntoCanvas openFileState model dispatch
            {
                ProjectPath = path
                OpenFileName =  openFileName
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
                menuItem "New project" (newProject model dispatch)
                menuItem "Open project" (openProject model dispatch)
            ]
        ]
    match model.CurrProject with
    | Some _ -> div [] []
    | None ->
        stablePopup initialMenu

let viewTopMenu model dispatch =
    let projectPath, fileName =
        match model.CurrProject with
        | None -> "no open project", "no open file"
        | Some project -> project.ProjectPath, project.OpenFileName
    log fileName
    let makeFileLine name =
        Navbar.Item.div [ Navbar.Item.Props [ Style [ Width "100%"] ] ] [
            Level.level [ Level.Level.Props [ Style [ Width "100%"] ] ] [
                Level.left [] [
                    Level.item [] [ str name ]
                ]
                Level.right [ Props [ Style [MarginLeft "20px"] ] ] [
                    Level.item [] [
                        Button.button [
                            Button.Size IsSmall
                            Button.Color IsPrimary
                            Button.OnClick (fun _ ->
                                log name
                                openFileInProject name model dispatch)
                        ] [ str "open" ]
                    ]
                    Level.item [] [
                        Button.button [
                            Button.Size IsSmall
                            Button.Color IsInfo
                        ] [ str "rename" ]
                    ]
                    Level.item [] [
                        Button.button [
                            Button.Size IsSmall
                            Button.Color IsDanger
                        ] [ str "delete" ]
                    ]
                ]
            ]
        ]
    let fileTab =
        match model.CurrProject with
        | None -> Navbar.Item.div [] []
        | Some project ->
            let projectFiles = project.LoadedComponents
                               |> List.map (fun comp -> makeFileLine comp.Name)
            Navbar.Item.div [ Navbar.Item.HasDropdown; Navbar.Item.IsHoverable ] [
                Navbar.Link.a [] [ str "Files" ]
                Navbar.Dropdown.div [] (
                    [
                        Navbar.Item.a [
                            Navbar.Item.Props [
                                OnClick (fun _ -> addFileToProject model dispatch) ] ]
                            [ str "New file" ]
                        Navbar.divider [] [] 
                    ]
                    @ projectFiles
                )
            ]
    div [ navbarStyle ] [
        Navbar.navbar [ Navbar.Props [Style [Height "100%"; Width "100%"]] ] [
            Navbar.Brand.div [ Props [Style [Height "100%"; Width "100%"]] ] [
                Navbar.Item.div [ Navbar.Item.HasDropdown; Navbar.Item.IsHoverable ] [
                    Navbar.Link.a [] [ str "Project" ]
                    Navbar.Dropdown.div [] [
                        Navbar.Item.a [] [ str "New project" ]
                        Navbar.Item.a [] [ str "Open project" ] 
                    ]
                ]
                fileTab
                Navbar.Item.div [] [
                    Navbar.Item.div [] [
                        Breadcrumb.breadcrumb [ Breadcrumb.HasArrowSeparator ] [
                            Breadcrumb.item [] [ str projectPath ]
                            Breadcrumb.item [] [ str fileName ]
                        ]
                    ]
                ]
                Navbar.Item.div [] [
                    Navbar.Item.div [] [
                        Button.button [ Button.Props [
                            OnClick (fun _ -> saveOpenFileAction model )
                        ] ] [ str "Save" ]
                    ]
                ]
            ]
        ]
    ]

let hideView model dispatch =
    div [] [
        model.Diagram.CanvasReactElement (JSDiagramMsg >> dispatch) Hidden
    ]

let displayView model dispatch =
    div [] [
        viewTopMenu model dispatch
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
    | ClosePopup -> { model with Popup = None; PopupDialogText = None }
    | SetPopupDialogText text -> { model with PopupDialogText = text }
