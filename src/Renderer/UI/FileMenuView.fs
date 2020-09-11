(*
    FileMenuView.fs

    View for the top menu, and related functionalities.
*)

module FileMenuView

open Fulma
open Fable.React
open Fable.React.Props

open Helpers
open JSHelpers
open DiagramStyle
open DiagramMessageType
open DiagramModelType
open CommonTypes
open FilesIO
open Extractor
open PopupView
open System

/// returns a string option representig the current file name if file is loaded, otherwise None
let getCurrFile (model: Model) =
    match model.CurrProject with
    | Some proj -> Some proj.OpenFileName
    | None -> None

/// returns a WaveSimModel option if a file is loaded, otherwise None
let currWS (model: Model) =
    match getCurrFile model with
    | Some fileName when Map.containsKey fileName (fst model.WaveSim) -> Some (fst model.WaveSim).[fileName]
    | _ -> None

   
let private displayFileErrorNotification err dispatch =
    errorNotification err CloseFilesNotification
    |> SetFilesNotification
    |> dispatch

let private loadStateIntoCanvas state model dispatch =
    dispatch <| SetHighlighted([], []) // Remove current highlights.
    model.Diagram.ClearCanvas() // Clear the canvas.
    // Finally load the new state in the canvas.
    let components, connections = state
    List.map model.Diagram.LoadComponent components |> ignore
    List.map (model.Diagram.LoadConnection true) connections |> ignore
    model.Diagram.FlushCommandStack() // Discard all undo/redo.
    // Run the a connection widhts inference.
    InferWidths()
    |> JSDiagramMsg
    |> dispatch
    // Set no unsaved changes.
    SetHasUnsavedChanges false
    |> JSDiagramMsg
    |> dispatch
    // set autosave status clean

/// extract SavedwaveInfo from model to be saved
let getSavedWave (model:Model) wsModel2SavedWaveInfoFunc : SavedWaveInfo option = 
    match currWS model with
    | Some wSMod -> wsModel2SavedWaveInfoFunc wSMod |> Some
    | None -> None


/// add waveInfo to model
let setSavedWave compIds savedWaveInfo2wsModelFunc (wave: SavedWaveInfo option) model : Model =
    match wave, getCurrFile model with
    | None, _ -> model
    | Some waveInfo, Some fileName -> 
        { model with WaveSim = Map.add fileName (savedWaveInfo2wsModelFunc compIds model waveInfo) 
                                                (fst model.WaveSim), 
                               snd model.WaveSim }
    | Some waveInfo, _ -> model
            

/// Save the file currently open.
let saveOpenFileAction isAuto model wsModel2SavedWaveInfoFunc =
    match model.Diagram.GetCanvasState (), model.CurrProject with
    | None, _ | _, None -> ()
    | Some jsState, Some project ->
        extractState jsState
        |> (fun state -> 
                let savedState = state, getSavedWave model wsModel2SavedWaveInfoFunc
                if isAuto then
                    saveAutoStateToFile project.ProjectPath project.OpenFileName savedState
                else 
                    saveStateToFile project.ProjectPath project.OpenFileName savedState
                    removeFileWithExtn ".dgmauto" project.ProjectPath project.OpenFileName)


let private getFileInProject name project = project.LoadedComponents |> List.tryFind (fun comp -> comp.Name = name)

let private isFileInProject name project =
    getFileInProject name project
    |> function
    | None -> false
    | Some _ -> true

/// Create a new empty .dgm file and return corresponding loaded component.
let private createEmptyDiagramFile projectPath name =
    createEmptyDgmFile projectPath name

    {   
        Name = name
        TimeStamp = System.DateTime.Now
        WaveInfo = None
        FilePath = pathJoin [| projectPath; name + ".dgm" |]
        CanvasState = [],[]
        InputLabels = []
        OutputLabels = []
    }

let updateLoadedComponents name (setFun: LoadedComponent -> LoadedComponent) (lcLst: LoadedComponent list) =
    let n = List.tryFindIndex (fun (lc: LoadedComponent) -> lc.Name = name) lcLst
    match n with
    | None -> failwithf "Can't find name='%s' in components:%A" name lcLst
    | Some n ->
        List.mapi (fun i x -> if i = n then setFun x else x) lcLst

let setupProject (pPath:string) (ldComps: LoadedComponent list) (model: Model) (dispatch: Msg->Unit)=
    let openFileName, openFileState =
        match ldComps with
        | [] -> // No files in the project. Create one and open it.
            createEmptyDgmFile pPath "main"
            "main", ([],[])
        | comps ->
            // load the most recently saved file
            let comp = comps |> List.maxBy (fun comp -> comp.TimeStamp)
            comp.Name, comp.CanvasState
    dispatch EndSimulation // End any running simulation.
    //
    loadStateIntoCanvas openFileState model dispatch
    {
        ProjectPath = pPath
        OpenFileName =  openFileName
        LoadedComponents = ldComps
    }
    |> SetProject |> dispatch

/// Open the specified file.
let private openFileInProject wsModel2SavedWaveInfoFunc name project model dispatch =
    match getFileInProject name project with
    | None -> log <| sprintf "Warning: openFileInProject could not find the component %s in the project" name
    | Some loadedComponent ->
        saveOpenFileAction false model wsModel2SavedWaveInfoFunc
        let oldState = 
            model.Diagram.GetCanvasState()
            |> Option.map extractState
            |> Option. defaultValue <| failwithf "What? Current file %A has no canvas to save" project.OpenFileName
        saveOpenFileAction false model wsModel2SavedWaveInfoFunc
        // make sure correct file gets opened.
        let lcs = 
            project.LoadedComponents 
            //|> updateLoadedComponents project.OpenFileName (fun _ -> oldState) // update old file
            |> updateLoadedComponents name (fun lc -> {lc with TimeStamp = DateTime.Now}) // make sure desired file gets opened
        setupProject project.ProjectPath lcs model dispatch
        dispatch EndSimulation // End any running simulation.

/// Remove file.
let private removeFileInProject wsModel2SavedWaveInfoFunc name project model dispatch =
    removeFile project.ProjectPath name
    removeFile project.ProjectPath (name + "auto")
    // Remove the file from the dependencies and update project.
    let newComponents = List.filter (fun (lc: LoadedComponent) -> lc.Name <> name) project.LoadedComponents
    // Make sure there is at least one file in the project.
    let newComponents =
        match List.isEmpty newComponents with
        | false -> newComponents
        | true -> [ (createEmptyDiagramFile project.ProjectPath "main") ]

    let project = { project with LoadedComponents = newComponents }
    project
    |> SetProject
    |> dispatch
    // If the file was displayed, open and display another one instead.
    // It is safe to access position 0 as we are guaranteed that there is at
    // least one element in newComponents.
    assertThat (not <| List.isEmpty project.LoadedComponents) "removeFileInProject"
    match name = project.OpenFileName with
    | false -> ()
    | true -> openFileInProject wsModel2SavedWaveInfoFunc project.LoadedComponents.[0].Name project model dispatch

/// Create a new file in this project and open it automatically.
let addFileToProject wsModel2SavedWaveInfoFunc model dispatch =
    match model.CurrProject with
    | None -> log "Warning: addFileToProject called when no project is currently open"
    | Some project ->
        // Prepare dialog popup.
        let title = "Add sheet to project"

        let before =
            fun (dialogData: PopupDialogData) ->
                let dialogText = getText dialogData

                let maybeWarning =
                    if isFileInProject dialogText project
                    then div [ Style [ Color "red" ] ] [ str "This sheet already exists." ]
                    else div [] []
                div []
                    [ str "A new sheet will be created at:"
                      br []
                      str <| pathJoin
                                 [| project.ProjectPath
                                    dialogText + ".dgm" |]
                      maybeWarning ]

        let placeholder = "Insert design sheet name"
        let body = dialogPopupBodyOnlyText before placeholder dispatch
        let buttonText = "Add"

        let buttonAction =
            fun (dialogData: PopupDialogData) ->
                // Save current file.
                saveOpenFileAction false model wsModel2SavedWaveInfoFunc
                // Create empty file.
                let name = getText dialogData
                createEmptyDgmFile project.ProjectPath name
                // Add the file to the project.
                let newComponent = {
                    Name = name
                    TimeStamp = System.DateTime.Now
                    WaveInfo = None
                    FilePath = pathJoin [|project.ProjectPath; name + ".dgm"|]
                    CanvasState = [],[]
                    InputLabels = []
                    OutputLabels = []
                }
                let updatedProject =
                    { project with
                          LoadedComponents = newComponent :: project.LoadedComponents
                          OpenFileName = name }
                // Update the project.
                updatedProject
                |> SetProject
                |> dispatch
                // Open the file.
                openFileInProject wsModel2SavedWaveInfoFunc name updatedProject model dispatch
                // Close the popup.
                dispatch ClosePopup
                dispatch EndSimulation // End any running simulation.

        let isDisabled =
            fun (dialogData: PopupDialogData) ->
                let dialogText = getText dialogData
                (isFileInProject dialogText project) || (dialogText = "")

        dialogPopup title body buttonText buttonAction isDisabled dispatch

/// Close current project, if any.
let private closeProject model dispatch _ =
    dispatch EndSimulation // End any running simulation.
    dispatch CloseProject
    model.Diagram.ClearCanvas()

/// Create a new project.
let private newProject model dispatch _ =
    match askForNewProjectPath() with
    | None -> () // User gave no path.
    | Some path ->
        match tryCreateFolder path with
        | Error err ->
            log err
            let errMsg = "Could not create a folder for the project."
            displayFileErrorNotification errMsg dispatch
        | Ok _ ->
            dispatch EndSimulation // End any running simulation.
            // Create empty placeholder projectFile.
            let projectFile = baseName path + ".dprj"
            writeFile (pathJoin [| path; projectFile |]) ""
            // Create empty initial diagram file.
            let initialDiagram = createEmptyDiagramFile path "main"
            // Load the diagram.
            loadStateIntoCanvas initialDiagram.CanvasState model dispatch
            // Add the file to the project.
            { ProjectPath = path
              OpenFileName = "main"
              LoadedComponents = [ initialDiagram ] }
            |> SetProject
            |> dispatch







/// work out what to do opening a file
let rec resolveComponentOpenPopup 
        (pPath:string)
        (components: LoadedComponent list)  
        (resolves: LoadStatus list) 
        (model: Model)
        (dispatch: Msg -> Unit) =
    dispatch ClosePopup
    match resolves with
    | [] -> setupProject pPath components model dispatch
    | Resolve (ldComp,autoComp) :: rLst ->
        // ldComp, autocomp are from attemps to load saved file and its autosave version.
        let buttonAction autoSave _ =
            let comp = if autoSave then autoComp else ldComp
            let data =  stateToJsonString (comp.CanvasState,comp.WaveInfo)
            writeFile comp.FilePath data
            resolveComponentOpenPopup pPath (comp :: components) rLst  model dispatch   
        // special case when autosave data is most recent
        let title = "Warning!"
        let body = str <|  sprintf "Warning: changes were made to sheet '%s' after your last Save. \
                                There is an automatically saved version which is \
                                more uptodate. Do you want to keep the newer AutoSaved version or \
                                the older saved version?"  ldComp.Name  
        choicePopup title body "Newer AutoSaved file" "Older Saved file" buttonAction dispatch
    | OkAuto autoComp :: rLst ->
         let errMsg = "Could not load saved project file '%s' - using autosave file instead"
         displayFileErrorNotification errMsg dispatch
         resolveComponentOpenPopup pPath (autoComp::components) rLst model dispatch
    | OkComp comp ::rLst -> 
        resolveComponentOpenPopup pPath (comp::components) rLst model dispatch
 

/// open an existing project
let private openProject model dispatch _ =
    match askForExistingProjectPath () with
    | None -> () // User gave no path.
    | Some path ->
        match loadAllComponentFiles path with
        | Error err ->
            log err
            let errMsg = "Could not load diagrams files in the project. The files may be malformed."
            displayFileErrorNotification errMsg dispatch
        | Ok componentsToResolve ->
            resolveComponentOpenPopup path [] componentsToResolve model dispatch


/// Display the initial Open/Create Project menu at the beginning if no project
/// is open.
let viewNoProjectMenu model dispatch =
    let menuItem label action =
        Menu.Item.li
            [ Menu.Item.IsActive false
              Menu.Item.OnClick action ] [ str label ]

    let initialMenu =
        Menu.menu []
            [ Menu.list []
                  [ menuItem "New project" (newProject model dispatch)
                    menuItem "Open project" (openProject model dispatch) ] ]

    match model.CurrProject with
    | Some _ -> div [] []
    | None -> unclosablePopup None initialMenu None []


/// Display top menu.
let viewTopMenu model wsModel2WaveInfoFunc messagesFunc simulateButtonFunc dispatch =
    let compIds = getComponentIds model
    
    messagesFunc model dispatch

    //printfn "FileView"
    let style = Style [ Width "100%" ] //leftSectionWidth model

    let projectPath, fileName =
        match model.CurrProject with
        | None -> "no open project", "no open sheet"
        | Some project -> project.ProjectPath, project.OpenFileName

    let makeFileLine name project =
        Navbar.Item.div [ Navbar.Item.Props [ style ] ]
            [ Level.level [ Level.Level.Props [ style ] ]
                  [ Level.left [] [ Level.item [] [ str name ] ]
                    Level.right [ Props [ Style [ MarginLeft "20px" ] ] ]
                        [ Level.item []
                              [ Button.button
                                  [ Button.Size IsSmall
                                    Button.IsOutlined
                                    Button.Color IsPrimary
                                    Button.Disabled(name = project.OpenFileName)
                                    Button.OnClick(fun _ ->
                                        saveOpenFileAction false model wsModel2WaveInfoFunc // Save current file.
                                        openFileInProject wsModel2WaveInfoFunc name project model dispatch) ] [ str "open" ] ]
                          // Add option to rename?
                          //Level.item [] [
                          //    Button.button [
                          //        Button.Size IsSmall
                          //        Button.IsOutlined
                          //        Button.Color IsInfo
                          //    ] [ str "rename" ]
                          //]
                          Level.item []
                              [ Button.button
                                  [ Button.Size IsSmall
                                    Button.IsOutlined
                                    Button.Color IsDanger
                                    Button.OnClick(fun _ ->
                                        let title = "Delete sheet"

                                        let body =
                                            div []
                                                [ str "Are you sure you want to delete the following design sheet?"
                                                  br []
                                                  str <| pathJoin
                                                             [| project.ProjectPath
                                                                name + ".dgm" |]
                                                  br []
                                                  str <| "This action is irreversible." ]

                                        let buttonText = "Delete"

                                        let buttonAction =
                                            fun _ ->
                                                removeFileInProject wsModel2WaveInfoFunc name project model dispatch
                                                dispatch ClosePopup
                                        confirmationPopup title body buttonText buttonAction dispatch) ]
                                    [ str "delete" ] ] ] ] ]

    let fileTab =
        match model.CurrProject with
        | None -> Navbar.Item.div [] []
        | Some project ->
            let projectFiles = project.LoadedComponents |> List.map (fun comp -> makeFileLine comp.Name project)
            Navbar.Item.div
                [ Navbar.Item.HasDropdown
                  Navbar.Item.Props
                      [ OnClick(fun _ ->
                          if model.TopMenu = Files then Closed else Files
                          |> SetTopMenu
                          |> dispatch) ] ]
                [ Navbar.Link.a [] [ str "Sheets" ]
                  Navbar.Dropdown.div
                      [ Navbar.Dropdown.Props
                          [ Style
                              [ Display
                                  (if (let b = model.TopMenu = Files
                                       b) then
                                      DisplayOptions.Block
                                   else
                                       DisplayOptions.None) ] ] ]
                      ([ Navbar.Item.a [ Navbar.Item.Props [ OnClick(fun _ -> addFileToProject wsModel2WaveInfoFunc model dispatch) ] ]
                             [ str "New Sheet" ]
                         Navbar.divider [] [] ]
                       @ projectFiles) ]

    div [ leftSectionWidth model ]
        [ Navbar.navbar
            [ Navbar.Props
                [ Style
                    [ Height "100%"
                      Width "100%" ] ] ]
              [ Navbar.Brand.div
                  [ Props
                      [ Style
                          [ Height "100%"
                            Width "100%" ] ] ]
                    [ Navbar.Item.div
                        [ Navbar.Item.HasDropdown
                          Navbar.Item.Props
                              [ OnClick(fun _ ->
                                  if model.TopMenu = Project then Closed else Project
                                  |> SetTopMenu
                                  |> dispatch) ] ]
                          [ Navbar.Link.a [] [ str "Project" ]
                            Navbar.Dropdown.div
                                [ Navbar.Dropdown.Props
                                    [ Style
                                        [ Display
                                            (if model.TopMenu = Project then
                                                DisplayOptions.Block
                                             else
                                                 DisplayOptions.None) ] ] ]
                                [ Navbar.Item.a [ Navbar.Item.Props [ OnClick <| newProject model dispatch ] ]
                                      [ str "New project" ]
                                  Navbar.Item.a [ Navbar.Item.Props [ OnClick <| openProject model dispatch ] ]
                                      [ str "Open project" ]
                                  Navbar.Item.a [ Navbar.Item.Props [ OnClick <| closeProject model dispatch ] ]
                                      [ str "Close project" ] ] ]
                      fileTab
                      Navbar.Item.div []
                          [ Navbar.Item.div []
                                [ Breadcrumb.breadcrumb [ Breadcrumb.HasArrowSeparator ]
                                      [ Breadcrumb.item [] [ str <| cropToLength 30 false projectPath ]
                                        Breadcrumb.item [] [ span [ Style [ FontWeight "bold" ] ] [ str fileName ] ] ] ] ]
                      Navbar.Item.div []
                          [ Navbar.Item.div []
                                [ Button.button
                                    [ Button.Color(if model.HasUnsavedChanges then IsSuccess else IsWhite)
                                      Button.OnClick(fun _ ->
                                          saveOpenFileAction false model wsModel2WaveInfoFunc
                                          SetHasUnsavedChanges false
                                          |> JSDiagramMsg
                                          |> dispatch) ] [ str "Save" ] ] ]
                      Navbar.End.div []
                          [ 
                            Navbar.Item.div [] 
                                [ simulateButtonFunc compIds model dispatch ] ]
                      Navbar.End.div []
                          [ Navbar.Item.div []
                                [ Button.button [ Button.OnClick(fun _ -> PopupView.viewInfoPopup dispatch) ] [ str "Info" ] ] ] ] ] ]

