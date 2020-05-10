(*
    FileMenuView.fs

    View for the top menu, and related functionalities.
*)

module FileMenuView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

open Helpers
open JSHelpers
open DiagramStyle
open DiagramMessageType
open DiagramModelType
open DiagramTypes
open FilesIO
open Extractor
open PopupView

// TODO close project -> so one can reopen a project even when they were using another one.
// TODO remove file
// TODO error popup

let private loadStateIntoCanvas state model dispatch =
    dispatch <| SetHighlighted ([],[]) // Remove current highlights.
    model.Diagram.ClearCanvas()        // Clear the canvas.
    // Finally load the new state in the canvas.
    let components, connections = state
    List.map model.Diagram.LoadComponent components |> ignore
    List.map (model.Diagram.LoadConnection true) connections |> ignore
    model.Diagram.FlushCommandStack () // Discard all undo/redo.

let private reloadProjectComponents project =
    match tryLoadComponentsFromPath project.ProjectPath with
    | Error err -> failwith "what? reloading project components" // TODO: this should probably not crash the program.
    | Ok components -> { project with LoadedComponents = components }

let private saveOpenFileAction model =
    match model.Diagram.GetCanvasState (), model.CurrProject with
    | None, _ | _, None -> ()
    | Some state, Some project ->
        extractState state
        |> saveStateToFile project.ProjectPath project.OpenFileName
        |> ignore

let private getFileInProject name project =
    project.LoadedComponents
    |> List.tryFind (fun comp -> comp.Name = name)

let private isFileInProject name project =
    getFileInProject name project
    |> function | None -> false | Some _ -> true

/// Create a new empty .dgm file and return corresponding loaded component.
let private createEmptyDiagramFile projectPath name =
    createEmptyDgmFile projectPath name
    {   
        Name = name
        FilePath = pathJoin [| projectPath; name + ".dgm" |]
        CanvasState = [],[]
        InputLabels = []
        OutputLabels = []
    }

/// Open the specified file.
let private openFileInProject name project model dispatch =
    match getFileInProject name project with
    | None -> log <| sprintf "Warning: openFileInProject could not find the component %s in the project" name
    | Some loadedComponent ->
        loadStateIntoCanvas loadedComponent.CanvasState model dispatch
        // Reload components so the project we just closed is up to date in
        // our CurrProj.
        { project with OpenFileName = name }
        |> reloadProjectComponents |> SetProject |> dispatch

/// Remove file.
let private removeFileInProject name project model dispatch =
    removeFile project.ProjectPath name
    // Remove the file from the dependencies and update project.
    let newComponents =
        List.filter (fun lc -> lc.Name <> name) project.LoadedComponents
    // Make sure there is at least one file in the project.
    let newComponents =
        match List.isEmpty newComponents with
        | false -> newComponents
        | true -> [(createEmptyDiagramFile project.ProjectPath "main")]
    let project = { project with LoadedComponents = newComponents }
    project |> SetProject |> dispatch
    // If the file was displayed, open and display another one instead.
    // It is safe to access position 0 as we are guaranteed that there is at
    // least one element in newComponents.
    assertThat (not <| List.isEmpty project.LoadedComponents) "removeFileInProject"
    match name = project.OpenFileName with
    | false -> ()
    | true -> openFileInProject project.LoadedComponents.[0].Name project model dispatch

/// Create a new file in this project. Do not open it automatically.
let private addFileToProject model dispatch =
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


/// Close current project, if any.
let private closeProject model dispatch _ =
    dispatch CloseProject
    model.Diagram.ClearCanvas()

/// Create a new project.
let private newProject model dispatch _ =
    match askForNewProjectPath () with
    | None -> () // User gave no path.
    | Some path ->
        match tryCreateFolder path with
        | Error err -> log err // TODO
        | Ok _ ->
            let initialDiagram = createEmptyDiagramFile path "main"
            // Load the diagram.
            loadStateIntoCanvas initialDiagram.CanvasState model dispatch
            // Add the file to the project.
            {
                ProjectPath = path
                OpenFileName = "main"
                LoadedComponents = [initialDiagram]
            }
            |> SetProject |> dispatch

/// Open a project.
let private openProject model dispatch _ =
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
    | None -> stablePopup initialMenu

/// Display top menu.
let viewTopMenu model dispatch =
    let projectPath, fileName =
        match model.CurrProject with
        | None -> "no open project", "no open file"
        | Some project -> project.ProjectPath, project.OpenFileName
    let makeFileLine name project =
        Navbar.Item.div [ Navbar.Item.Props [ Style [ Width "100%"] ] ] [
            Level.level [ Level.Level.Props [ Style [ Width "100%"] ] ] [
                Level.left [] [
                    Level.item [] [ str name ]
                ]
                Level.right [ Props [ Style [MarginLeft "20px"] ] ] [
                    Level.item [] [
                        Button.button [
                            Button.Size IsSmall
                            Button.IsOutlined
                            Button.Color IsPrimary
                            Button.OnClick (fun _ ->
                                saveOpenFileAction model // Save current file.
                                openFileInProject name project model dispatch
                            )
                        ] [ str "open" ]
                    ]
                    // Add option to rename?
                    //Level.item [] [
                    //    Button.button [
                    //        Button.Size IsSmall
                    //        Button.IsOutlined
                    //        Button.Color IsInfo
                    //    ] [ str "rename" ]
                    //]
                    Level.item [] [
                        Button.button [
                            Button.Size IsSmall
                            Button.IsOutlined
                            Button.Color IsDanger
                            Button.OnClick (fun _ ->
                                let title = "Delete file"
                                let body = div [] [
                                    str "Are you sure you want to delete the follwing file?"
                                    br []
                                    str <| pathJoin [| project.ProjectPath; name + ".dgm" |]
                                    br []
                                    str <| "This action is irreversible."
                                ]
                                let buttonText = "Delete"
                                let buttonAction =
                                    fun _ ->
                                        removeFileInProject name project model dispatch
                                        dispatch ClosePopup
                                confirmationPopup title body buttonText buttonAction dispatch
                            )
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
                               |> List.map (fun comp -> makeFileLine comp.Name project)
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
                        Navbar.Item.a [
                            Navbar.Item.Props [ OnClick <| newProject model dispatch ]
                        ] [ str "New project" ]
                        Navbar.Item.a [
                            Navbar.Item.Props [ OnClick <| openProject model dispatch ]
                        ] [ str "Open project" ]
                        Navbar.Item.a [
                            Navbar.Item.Props [ OnClick <| closeProject model dispatch ]
                        ] [ str "Close project" ]
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
