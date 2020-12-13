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
open ModelType
open CommonTypes
open FilesIO
open Extractor
open PopupView
open System

/// Interlock to ensure we do not do two file-related operations at the same time 
/// should really be handled by making all state-chnage actions go through messages and model update
/// this is a hack.
let mutable fileProcessingBusy: string list = []

/// returns true if file activity lock is gained
let requestFileActivity (a:string) dispatch =
    let busy = fileProcessingBusy <> []
    if busy then
        let notification = 
            warningNotification $"Please wait till previous file operation {fileProcessingBusy} has finished before starting another!" CloseFilesNotification
        dispatch <| SetFilesNotification notification
        false
    else
        fileProcessingBusy <- a :: fileProcessingBusy
        true

let releaseFileActivity (a:string) (dispatch)=
    dispatch <| ModelType.ReleaseFileActivity a

let releaseFileActivityImplementation a =
    match fileProcessingBusy with
    | a' :: rest when a' = a -> 
        fileProcessingBusy <- rest
        if fileProcessingBusy <> [] then
            failwithf "What? A blocking file operation has terminated leaving busy interlock: %A" fileProcessingBusy
    | _ ->
        failwithf "What? releaseFileActivity '%A' called when activities = %A" a fileProcessingBusy



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
    //printfn "Loading..."
    dispatch <| SetHighlighted([], []) // Remove current highlights.
    model.Diagram.ClearCanvas() // Clear the canvas.
    // Finally load the new state in the canvas.
    dispatch <| SetIsLoading true
    //printfn "Check 1..."
    let components, connections = compToSetup.CanvasState
    List.map model.Diagram.LoadComponent components |> ignore
    //printfn "Check 2..."
    List.map (model.Diagram.LoadConnection true) connections |> ignore
    //printfn "Check 3..."
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
    //printfn "Check 4..."
    JSdispatch <| InferWidths()
    //printfn "Check 5..."
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
    dispatch <| SetWaveSimIsOutOfDate true
    dispatch <| SetIsLoading false 
    //printfn "Check 6..."
    

let updateLoadedComponents name (setFun: LoadedComponent -> LoadedComponent) (lcLst: LoadedComponent list) =
    let n = List.tryFindIndex (fun (lc: LoadedComponent) -> lc.Name = name) lcLst
    match n with
    | None -> 
        printf "In updateLoadedcomponents can't find name='%s' in components:%A" name lcLst
        lcLst
    | Some n ->
        List.mapi (fun i x -> if i = n then setFun x else x) lcLst

/// return current project with current sheet updated from canvas if needed
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
        |> Option.defaultValue (ModelType.initWS [||] Map.empty)


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
let private openFileInProject' saveCurrent name project (model:Model) dispatch =
    let newModel = {model with CurrentProj = Some project}
    match getFileInProject name project with
    | None -> 
        log <| sprintf "Warning: openFileInProject could not find the component %s in the project" name
    | Some lc ->
        match updateProjectFromCanvas model with
        | None -> failwithf "What? current project cannot be None at this point in openFileInProject"
        | Some p ->
            if saveCurrent then 
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
            setupProjectFromComponents name project.LoadedComponents newModel dispatch

let openFileInProject name project (model:Model) dispatch =
    if requestFileActivity "openFileInProject" dispatch then 
        openFileInProject' true name project (model:Model) dispatch
        dispatch <| ReleaseFileActivity "openFileInProject"


/// return a react warning message if name if not valid for a sheet Add or Rename, or else None
let maybeWarning dialogText project =
    let redText txt = Some <| div [ Style [ Color "red" ] ] [ str txt ]
    if isFileInProject dialogText project then
        redText "This sheet already exists." 
    elif dialogText.StartsWith " " || dialogText.EndsWith " " then
        redText "The name cannot start or end with a space."
    elif String.exists ((=) '.') dialogText then
        redText "The name cannot contain a file suffix."
    elif not <| String.forall (fun c -> Char.IsLetterOrDigit c || c = ' ') dialogText then
        redText "The name must be alphanumeric."
    elif ((dialogText |> Seq.tryItem 0) |> Option.map Char.IsDigit) = Some true then
        redText "The name must not start with a digit"
    else None


/// rename a sheet
let renameSheet oldName newName (model:Model) dispatch =
    let saveAllFilesFromProject (proj: Project) =
        proj.LoadedComponents
        |> List.iter (fun ldc ->
            let name = ldc.Name
            let state = ldc.CanvasState
            let waveInfo = ldc.WaveInfo
            saveStateToFile proj.ProjectPath name (state,waveInfo)
            removeFileWithExtn ".dgmauto" proj.ProjectPath name)

    let renameComps oldName newName (comps:Component list) : Component list = 
        comps
        |> List.map (fun comp -> 
            match comp with 
            | {Type= Custom ({Name = compName} as customType)} when compName = oldName-> 
                {comp with Type = Custom {customType with Name = newName} }
            | c -> c)

    let renameCustomComponents newName (ldComp:LoadedComponent) =
        let state = ldComp.CanvasState
        {ldComp with CanvasState = renameComps oldName newName (fst state), snd state}

    let renameSheetsInProject oldName newName proj =
        {proj with
            OpenFileName = if proj.OpenFileName = oldName then newName else proj.OpenFileName
            LoadedComponents =
                proj.LoadedComponents
                |> List.map (fun ldComp -> 
                    match ldComp with
                    | {Name = lcName} when lcName = oldName -> 
                        {ldComp with Name=newName}
                    | _ ->
                        renameCustomComponents newName ldComp )
        }
    if requestFileActivity "renameSheet" dispatch then
        match updateProjectFromCanvas model with
        | None -> 
            releaseFileActivity "renameSheet" dispatch
            failwithf "What? current project cannot be None at this point in renamesheet"
        | Some p ->
            let opt = saveOpenFileAction false model
            let ldcOpt = Option.map fst opt
            let ldComps = updateLdCompsWithCompOpt ldcOpt p.LoadedComponents
            let reducedState = Option.map snd opt |> Option.defaultValue ([],[])
            // update Autosave info
            SetLastSavedCanvas (p.OpenFileName,reducedState)
            |> dispatch
            SetHasUnsavedChanges false
            |> JSDiagramMsg
            |> dispatch
            let proj' = renameSheetsInProject oldName newName p
            setupProjectFromComponents proj'.OpenFileName proj'.LoadedComponents model dispatch
            [".dgm";".dgmauto"] |> List.iter (fun extn -> renameFile extn proj'.ProjectPath oldName newName)
            /// save all the other files
            saveAllFilesFromProject proj'
            dispatch <| ReleaseFileActivity "renameSheet"

        
    


/// rename file
let renameFileInProject name project model dispatch =
    match model.CurrentProj, getCurrentWSMod model with
    | None,_ -> log "Warning: renameFileInProject called when no project is currently open"
    | Some project, Some ws when ws.WSViewState<>WSClosed ->
        displayFileErrorNotification "Sorry, you must close the wave simulator before renaming design sheets!" dispatch
        switchToWaveEditor model dispatch
    | Some project, _ ->
        // Prepare dialog popup.
        let title = "Rename sheet in project"

        let before =
            fun (dialogData: PopupDialogData) ->
                let dialogText = getText dialogData

                div []
                    [ 
                      str <| "Warning: the current sheet will be saved during this operation."
                      br []
                      str <| "Names of existing components in other sheets that use the renamed sheet will still reflect the old sheet name.";
                      str <| " You may change names manually if you wish, operation does not depend on the name."
                      br []; br []
                      str <| sprintf "Sheet %s will be renamed as %s:" name dialogText
                      br []; br []
                      //str <| dialogText + ".dgm"
                      Option.defaultValue (div [] []) (maybeWarning dialogText project)]

        let placeholder = "New name for design sheet"
        let body = dialogPopupBodyOnlyText before placeholder dispatch
        let buttonText = "Rename"

        let buttonAction =
            fun (dialogData: PopupDialogData) ->
                // Create empty file.
                let newName = (getText dialogData).ToLower()
                // rename the file in the project.
                renameSheet name newName model dispatch
                dispatch ClosePopup

        let isDisabled =
            fun (dialogData: PopupDialogData) ->
                let dialogText = getText dialogData
                (isFileInProject dialogText project) || (dialogText = "")

        dialogPopup title body buttonText buttonAction isDisabled dispatch



/// Remove file.
let private removeFileInProject name project model dispatch =
    if requestFileActivity "removefileinproject" dispatch
    then 
        match getCurrentWSMod model with
        | Some ws when ws.WSViewState<>WSClosed ->
            displayFileErrorNotification "Sorry, you must close the wave simulator before removing design sheets!" dispatch
            switchToWaveEditor model dispatch
        | _ ->
        
            removeFile project.ProjectPath name
            removeFile project.ProjectPath (name + "auto")
            // Remove the file from the dependencies and update project.
            let newComponents = List.filter (fun (lc: LoadedComponent) -> lc.Name.ToLower() <> name.ToLower()) project.LoadedComponents
            // Make sure there is at least one file in the project.
            let project' = {project with LoadedComponents = newComponents}
            match newComponents, name = project.OpenFileName with
            | [],true -> 
                let newComponents = [ (createEmptyDiagramFile project.ProjectPath "main") ]
                openFileInProject' false project.LoadedComponents.[0].Name project' model dispatch
            | [], false -> 
                failwithf "What? - this cannot happen"
            | nc, true ->
                openFileInProject' false project'.LoadedComponents.[0].Name project' model dispatch
            | nc, false ->
                // nothing chnages except LoadedComponents
                dispatch <| SetProject project'
        dispatch <| ReleaseFileActivity "removefileinproject"

                

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
                let warn = maybeWarning dialogText project
                div []
                    [ str "A new sheet will be created at:"
                      br []
                      str <| pathJoin
                                 [| project.ProjectPath
                                    dialogText + ".dgm" |]
                      Option.defaultValue (div [] []) warn ]

        let placeholder = "Insert design sheet name"
        let body = dialogPopupBodyOnlyText before placeholder dispatch
        let buttonText = "Add"
        let buttonAction =
            fun (dialogData: PopupDialogData) ->
                if requestFileActivity "add file" dispatch then 
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
                    openFileInProject' true name updatedProject model dispatch
                    // Close the popup.
                    dispatch ClosePopup
                    dispatch <| ReleaseFileActivity "add file"

        let isDisabled =
            fun (dialogData: PopupDialogData) ->
                let dialogText = getText dialogData
                (isFileInProject dialogText project) || (dialogText = "") || (maybeWarning dialogText project <> None)

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




let quantifyChanges (ldc1:LoadedComponent) (ldc2:LoadedComponent) =
    let comps1,conns1 = ldc1.CanvasState
    let comps2,conns2 = ldc2.CanvasState
    let reduceComp comp1 =
        {comp1 with X=0;Y=0}
    let reduceConn conn1 =
        {conn1 with Vertices = []}
    /// Counts the number of unequal items in the two lists.
    /// Determine equality from whether reduced applied to each item is equal
    let unmatched reduce lst1 lst2 =
        let rL1 = Set <| List.map reduce lst1
        let rL2 = Set <| List.map reduce lst2
        Set.union (Set.difference rL1 rL2) (Set.difference rL2 rL1)
        |> Set.count
    unmatched reduceComp comps1 comps2, unmatched reduceConn conns1 conns2






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
        let compChanges, connChanges = quantifyChanges ldComp autoComp
        let writeComponentToFile comp =
            let data =  stateToJsonString (comp.CanvasState,comp.WaveInfo)
            writeFile comp.FilePath data
        let buttonAction autoSave _ =
            let comp = {(if autoSave then autoComp else ldComp) with TimeStamp = DateTime.Now}
            writeComponentToFile comp
            if compChanges + connChanges >= 3 then
                let backupComp = 
                    let comp = if autoSave then ldComp else autoComp
                    let path = pathWithoutExtension comp.FilePath +  ".dgm"
                    ensureDirectory <| pathJoin [| dirName path ; "backup" |]
                    let backupPath = pathJoin [| dirName path ; "backup" ; baseName path |]
                    printfn "Writing backup file to %s" backupPath
                    {comp with 
                        TimeStamp = DateTime.Now
                        FilePath = backupPath}
                writeComponentToFile backupComp
            resolveComponentOpenPopup pPath (comp :: components) rLst  model dispatch   
        // special case when autosave data is most recent
        let title = "Warning!"
        let message =
            match compChanges + connChanges with
            | 0 -> 
                sprintf "There were layout changes made sheet %s after your last save.\
                         There is an automatically saved version which is \
                         more uptodate. Do you want to keep the newer AutoSaved version or \
                         the older saved version?"  ldComp.Name  
            | n when n < 3 ->   
                sprintf "Warning: %d component and %d connection changes were made to sheet '%s' after your last Save. \
                         There is an automatically saved version which is \
                         more uptodate. Do you want to keep the newer AutoSaved version or \
                         the older saved version?"  compChanges connChanges ldComp.Name 
            | n -> 
                sprintf "Warning: %d component and %d connection changes were made to sheet '%s' after your last Save. \
                         There is an automatically saved version which is \
                         more uptodate. Do you want to keep the newer AutoSaved version or \
                         the older saved version? This is a large change so the option you do not choose\
                         will be saved as file '%s.dgmbackup'"  compChanges connChanges ldComp.Name ldComp.Name
        let body = str message
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
        traceIf "project" (fun () -> "loading files")
        match loadAllComponentFiles path with
        | Error err ->
            log err
            displayFileErrorNotification err dispatch
        | Ok componentsToResolve ->
            traceIf "project " (fun () -> "resolving popups...")
            resolveComponentOpenPopup path [] componentsToResolve model dispatch
            traceIf "project" (fun () ->  "project successfully opened.")


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
                                        openFileInProject name project model dispatch) ] [ str "open" ] 
                          ]
                          // Add option to rename?
                          Level.item [] [
                              Button.button [
                                  Button.Size IsSmall
                                  Button.IsOutlined
                                  Button.Color IsInfo
                                  Button.OnClick(fun _ ->
                                      renameFileInProject name project model dispatch) ] [ str "rename" ]
                          ]
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

