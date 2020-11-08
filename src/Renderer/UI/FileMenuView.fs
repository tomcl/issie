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
open MessageType
open ModelType
open CommonTypes
open FilesIO
open Extractor
open PopupView
open System

    




/// returns a WaveSimModel option if a file is loaded, otherwise None
let currWaveSimModel (model: Model) =
    match getCurrFile model with
    | Some fileName when Map.containsKey fileName (fst model.WaveSim) -> Some (fst model.WaveSim).[fileName]
    | _ -> None

   
let private displayFileErrorNotification err dispatch =
    let note = errorFilesNotification err
    dispatch <| SetFilesNotification note

/// Send messages to change Diagram Canvas and specified sheet waveSim in model
let private loadStateIntoModel (compToSetup:LoadedComponent) waveSim ldComps model dispatch =
    let JSdispatch mess = 
        mess
        |> JSDiagramMsg
        |> dispatch
    let name = compToSetup.Name
    printfn "Loading..."
    dispatch <| SetHighlighted([], []) // Remove current highlights.
    model.Diagram.ClearCanvas() // Clear the canvas.
    // Finally load the new state in the canvas.
    dispatch <| SetIsLoading true
    printfn "Check 1..."
    let components, connections = compToSetup.CanvasState
    List.map model.Diagram.LoadComponent components |> ignore
    printfn "Check 2..."
    List.map (model.Diagram.LoadConnection true) connections |> ignore
    printfn "Check 3..."
    let errs = model.Diagram.GetAndClearLoadConnectionErrors()
    if errs <> [] then
        let errMsg =  
            (sprintf "Issie failed to load %d connections: which were incorrectly 
                saved due to a bug in the draw library.\n 
                Please recreate these connections, altering slightly component positions,\n 
                or drawing connections in the opposite direction,
                to work around this bug. \n 
                If the error repeats please make a bug report" (List.length errs))
        let error = errorFilesNotification errMsg
        dispatch <| SetFilesNotification error
    let errs = model.Diagram.GetAndClearLoadComponentErrors()
    if errs <> [] then
        let errMsg =  
            (sprintf "Issie failed to load %d component: which were incorrectly 
                saved due to a bug in the draw library.\n 
                Please recreate these connections, altering slightly component positions,\n 
                or drawing connections in the opposite direction,
                to work around this bug. \n 
                If the error repeats please make a bug report. Details: %s\n\n" (List.length errs)) (String.concat "\n\n" errs + "\n\n")
        let error = errorFilesNotification errMsg
        dispatch <| SetFilesNotification error
    model.Diagram.FlushCommandStack() // Discard all undo/redo.
    // Run the a connection widths inference.
    printfn "Check 4..."
    JSdispatch <| InferWidths()
    printfn "Check 5..."
    // Set no unsaved changes.
    JSdispatch <| SetHasUnsavedChanges false
    // set waveSim data
    dispatch <| SetWaveSimModel(name, waveSim)
    dispatch <| (
        {
            ProjectPath = dirName compToSetup.FilePath
            OpenFileName =  compToSetup.Name
            LoadedComponents = ldComps
        }
        |> SetProject) // this message actually changes the project in model
    dispatch <| SetWaveSimIsStale true
    dispatch <| SetIsLoading false 
    printfn "Check 6..."
    

let updateLoadedComponents name (setFun: LoadedComponent -> LoadedComponent) (lcLst: LoadedComponent list) =
    let n = List.tryFindIndex (fun (lc: LoadedComponent) -> lc.Name = name) lcLst
    match n with
    | None -> failwithf "Can't find name='%s' in components:%A" name lcLst
    | Some n ->
        List.mapi (fun i x -> if i = n then setFun x else x) lcLst


let updateProjectFromCanvas (model:Model) =
    match model.Diagram.GetCanvasState() with
    | None -> model.CurrentProj
    | Some state ->  
        extractState state
        |> fun canvas ->
            let inputs, outputs = parseDiagramSignature canvas
            let setLc lc =
                { lc with
                    CanvasState = canvas
                    InputLabels = inputs
                    OutputLabels = outputs
                }
            model.CurrentProj
            |> Option.map (fun p -> 
                {   
                    p with LoadedComponents = updateLoadedComponents p.OpenFileName setLc p.LoadedComponents
                })


/// extract SavedwaveInfo from model to be saved
let getSavedWave (model:Model) : SavedWaveInfo option = 
    match currWaveSimModel model with
    | Some wSModel -> waveSimModel2SavedWaveInfo wSModel |> Some
    | None -> None


/// add waveInfo to model
let setSavedWave compIds (wave: SavedWaveInfo option) model : Model =
    match wave, getCurrFile model with
    | None, _ -> model
    | Some waveInfo, Some fileName -> 
        { model with WaveSim = Map.add fileName (savedWaveInfo2WaveSimModel waveInfo) 
                                                (fst model.WaveSim), 
                               snd model.WaveSim }
    | Some waveInfo, _ -> model
            

/// Save the sheet currently open, return  the new sheet's Loadedcomponent if this has changed
let saveOpenFileAction isAuto model =
    match model.Diagram.GetCanvasState (), model.CurrentProj with
    | None, _ | _, None -> None
    | Some jsState, Some project ->
        let reducedState = extractReducedState jsState
        extractState jsState
        |> (fun state -> 
                let savedState = state, getSavedWave model
                if isAuto then
                    saveAutoStateToFile project.ProjectPath project.OpenFileName savedState
                    None
                else 
                    saveStateToFile project.ProjectPath project.OpenFileName savedState
                    removeFileWithExtn ".dgmauto" project.ProjectPath project.OpenFileName
                    let origLdComp =
                        project.LoadedComponents
                        |> List.find (fun lc -> lc.Name = project.OpenFileName)
                    let savedWaveSim =
                        Map.tryFind project.OpenFileName (fst model.WaveSim)
                        |> Option.map waveSimModel2SavedWaveInfo
                    Some (makeLoadedComponentFromCanvasData state origLdComp.FilePath DateTime.Now savedWaveSim, reducedState))


// save current open file, updating model etc, and returning the loaded component and the saved (unreduced) canvas state
let saveOpenFileActionWithModelUpdate (model: Model) (dispatch: Msg -> Unit) =
    let opt = saveOpenFileAction false model
    let ldcOpt = Option.map fst opt
    let reducedState = Option.map snd opt |> Option.defaultValue ([],[])
    match model.CurrentProj with
    | None -> failwithf "What? Should never be able to save sheet when project=None"
    | Some p -> 
      // update loaded components for saved file
      updateLdCompsWithCompOpt ldcOpt p.LoadedComponents
      |> (fun lc -> {p with LoadedComponents=lc})
      |> SetProject
      |> dispatch
      // update Autosave info
      SetLastSavedCanvas (p.OpenFileName,reducedState)
      |> dispatch
    SetHasUnsavedChanges false
    |> JSDiagramMsg
    |> dispatch
    opt

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


let createEmptyComponentAndFile (pPath:string)  (sheetName: string) : LoadedComponent =
    createEmptyDgmFile pPath sheetName
    {
        Name=sheetName
        WaveInfo = None
        TimeStamp = DateTime.Now
        FilePath= pathJoin [|pPath; sprintf "%s.dgm" sheetName|]
        CanvasState=([],[])
        InputLabels = []
        OutputLabels = []
    }

/// Load a new project as defined by parameters.
/// Ends any existing simulation
/// Closes WaveSim if this is being used
let setupProjectFromComponents (sheetName: string) (ldComps: LoadedComponent list) (model: Model) (dispatch: Msg->Unit)=
    let compToSetup =
        match ldComps with
        | [] -> failwithf "setupProjectComponents must be called with at least one LoadedComponent"
        | comps ->
            // load sheetName
            match comps |> List.tryFind (fun comp -> comp.Name = sheetName) with
            | None -> failwithf "What? can't find sheet %s in loaded sheets %A" sheetName (comps |> List.map (fun c -> c.Name))
            | Some comp -> comp
    match model.CurrentProj with
    | None -> ()
    | Some p ->
        dispatch EndSimulation // Message ends any running simulation.
        // TODO: make each sheet wavesim remember the list of waveforms.
    let waveSim = 
        compToSetup.WaveInfo
        |> Option.map savedWaveInfo2WaveSimModel 
        |> Option.defaultValue (MessageType.initWS [||] Map.empty)


    loadStateIntoModel compToSetup waveSim ldComps model dispatch
    {
        ProjectPath = dirName compToSetup.FilePath
        OpenFileName =  compToSetup.Name
        LoadedComponents = ldComps
    }
    |> SetProject // this message actually changes the project in model
    |> dispatch

/// Open the specified file, saving the current file if needed.
/// Creates messages sufficient to do all necessary model and diagram change
/// Terminates a simulation if one is running
/// Closes waveadder if it is open
let private openFileInProject name project model dispatch =
    match getFileInProject name project with
    | None -> log <| sprintf "Warning: openFileInProject could not find the component %s in the project" name
    | Some lc ->
        match updateProjectFromCanvas model with
        | None -> failwithf "What? current project cannot be None at this point in openFileInProject"
        | Some p ->
            let opt = saveOpenFileAction false model
            let ldcOpt = Option.map fst opt
            let ldComps = updateLdCompsWithCompOpt ldcOpt project.LoadedComponents
            let reducedState = Option.map snd opt |> Option.defaultValue ([],[])
            match model.CurrentProj with
            | None -> failwithf "What? Should never be able to save sheet when project=None"
            | Some p -> 
                // update Autosave info
                SetLastSavedCanvas (p.OpenFileName,reducedState)
                |> dispatch
            SetHasUnsavedChanges false
            |> JSDiagramMsg
            |> dispatch

            setupProjectFromComponents name ldComps model dispatch

/// Remove file.
let private removeFileInProject name project model dispatch =
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
    | true -> openFileInProject project.LoadedComponents.[0].Name project model dispatch

/// Create a new file in this project and open it automatically.
let addFileToProject model dispatch =
    match model.CurrentProj with
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
                // Create empty file.
                let name = (getText dialogData).ToLower()
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
 
                // Open the file, updating the project, saving current file
                openFileInProject name updatedProject model dispatch
                // Close the popup.
                dispatch ClosePopup

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
            let initialComponent = createEmptyComponentAndFile path "main"
            setupProjectFromComponents "main" [initialComponent] model dispatch








/// work out what to do opening a file
let rec resolveComponentOpenPopup 
        (pPath:string)
        (components: LoadedComponent list)  
        (resolves: LoadStatus list) 
        (model: Model)
        (dispatch: Msg -> Unit) =
    let chooseWhichToOpen comps =
        (List.maxBy (fun comp -> comp.TimeStamp) comps).Name
    dispatch ClosePopup
    match resolves with
    | [] -> setupProjectFromComponents (chooseWhichToOpen components) components model dispatch
    | Resolve (ldComp,autoComp) :: rLst ->
        // ldComp, autocomp are from attemps to load saved file and its autosave version.
        let buttonAction autoSave _ =
            let comp = {(if autoSave then autoComp else ldComp) with TimeStamp = DateTime.Now}
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

    match model.CurrentProj with
    | Some _ -> div [] []
    | None -> unclosablePopup None initialMenu None []


/// Display top menu.
let viewTopMenu model messagesFunc simulateButtonFunc dispatch =
    let compIds = getComponentIds model
    
    messagesFunc model dispatch

    //printfn "FileView"
    let style = Style [ Width "100%" ] //leftSectionWidth model

    let projectPath, fileName =
        match model.CurrentProj with
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
                                        openFileInProject name project model dispatch) ] [ str "open" ] ]
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
                                                removeFileInProject name project model dispatch
                                                dispatch ClosePopup
                                        confirmationPopup title body buttonText buttonAction dispatch) ]
                                    [ str "delete" ] ] ] ] ]

    let fileTab =
        match model.CurrentProj with
        | None -> Navbar.Item.div [] []
        | Some project ->
            let projectFiles = project.LoadedComponents |> List.map (fun comp -> makeFileLine comp.Name project)
            Navbar.Item.div
                [ Navbar.Item.HasDropdown
                  Navbar.Item.Props
                      [ OnClick(fun _ ->
                          if model.TopMenuOpenState = Files then Closed else Files
                          |> SetTopMenu
                          |> dispatch) ] ]
                [ Navbar.Link.a [] [ str "Sheets" ]
                  Navbar.Dropdown.div
                      [ Navbar.Dropdown.Props
                          [ Style
                              [ Display
                                  (if (let b = model.TopMenuOpenState = Files
                                       b) then
                                      DisplayOptions.Block
                                   else
                                       DisplayOptions.None) ] ] ]
                      ([ Navbar.Item.a [ Navbar.Item.Props [ OnClick(fun _ -> addFileToProject model dispatch) ] ]
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
                                  if model.TopMenuOpenState = Project then Closed else Project
                                  |> SetTopMenu
                                  |> dispatch) ] ]
                          [ Navbar.Link.a [] [ str "Project" ]
                            Navbar.Dropdown.div
                                [ Navbar.Dropdown.Props
                                    [ Style
                                        [ Display
                                            (if model.TopMenuOpenState = Project then
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
                                    [ Button.Color(if model.SavedSheetIsOutOfDate then IsSuccess else IsWhite)
                                      Button.OnClick(fun _ -> saveOpenFileActionWithModelUpdate model dispatch |> ignore) ] [ str "Save" ] ] ]
                      Navbar.End.div []
                          [ 
                            Navbar.Item.div [] 
                                [ simulateButtonFunc compIds model dispatch ] ]
                      Navbar.End.div []
                          [ Navbar.Item.div []
                                [ Button.button [ Button.OnClick(fun _ -> PopupView.viewInfoPopup dispatch) ] [ str "Info" ] ] ] ] ] ]

