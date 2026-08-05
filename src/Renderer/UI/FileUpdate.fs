module FileUpdate
open Elmish
open Fulma
open Fable.React
open Fable.React.Props
open Helpers
open ModelType
open ElectronAPI
open FilesIO
open SimGraphTypes
open ModelHelpers
open PopupHelpers
open CommonTypes
open CatalogueView
open TopMenuView
open Sheet.SheetInterface
open DrawModelType
open MenuHelpers
open Optics
open Optics.Optic
open Optics.Operators
open JSHelpers

/// force either save of current file before action, or abort (closeProject is special case of this)
/// In addition, if not aborting, save current lockstate of all files.
let doActionWithSaveFileDialog (name: string) (nextAction: Msg)  model dispatch _ =
    let closeDialogButtons keepOpen _ =
        if keepOpen then
            dispatch ClosePopup
        else
            dispatch nextAction

    // sheets are flagged out of date by a lock state change, or by an id correction on project load
    let sheetsNeedingSave =
        match model.CurrentProj with
        | None -> ""
        | Some p ->
            p.LoadedComponents
            |> List.filter (fun c -> c.LoadedComponentIsOutOfDate)
            |> List.map (fun c -> c.Name)
            |> String.concat ","


    if model.SavedSheetIsOutOfDate then 
        choicePopup 
            $"{name}?" 
            (div [] [ str "The current sheet has unsaved changes."])
            "Go back to sheet" 
            $"{name} without saving changes"  
            closeDialogButtons 
            dispatch
    elif sheetsNeedingSave <> "" then
        choicePopup
            $"Do you want to close without saving?"
            (div [] [ str $"""The sheets {sheetsNeedingSave} have unsaved changes."""])
            "Go back to sheet" 
            $"{name} without saving changes"  
            closeDialogButtons 
            dispatch
    else
        dispatch nextAction

/// Create the project directory, its marker and its first sheet, and open it.
let private createProjectAt (path: string) model dispatch =
    match tryCreateFolder path with
    | Error err ->
        JSHelpers.log err
        displayFileErrorNotification err dispatch
    | Ok _ ->
        dispatch EndSimulation // End any running simulation.
        dispatch <| TruthTableMsg CloseTruthTable // Close any open Truth Table.
        dispatch EndWaveSim
        // Create empty placeholder projectFile.
        writeFile (projectMarkerPath path) ""
        |> Notifications.displayAlertOnError dispatch
        // Create empty initial diagram file.
        let initialComponent = createEmptyComponentAndFile path "main"
        dispatch <| SetUserData {model.UserData with LastUsedDirectory = Some path}
        setupProjectFromComponents false "main" [initialComponent] model dispatch

/// Where a new project of this name, in this folder, would go - or why it would not go anywhere.
///
/// Every objection the creation used to raise after the event is asked here instead, of what the
/// user has typed so far, so that the Create button can say whether it would work rather than the
/// user finding out by pressing it.
let private newProjectPath (parent: string) (name: string) : Result<string, string> =
    match projectNameError name with
    | Some err -> Error err
    | None when parent = "" -> Error "Choose the folder to create the project in."
    | None when not (isDirectory parent) -> Error $"'{parent}' is not a folder."
    | None when inspectProjectDirectory parent = IsProject ->
        Error $"'{baseName parent}' is itself an Issie project, and a project cannot contain another."
    | None ->
        let path = pathJoin [| parent; name |]
        if exists path then Error $"'{name}' already exists in that folder." else Ok path

/// Create a new project, asking for its name and where to put it.
///
/// This was a native SAVE dialog: it asked the user to save a file that was really a directory,
/// offered to overwrite a folder it would not overwrite, and kept the naming rules to itself until
/// one was broken - at which point a native error box appeared and the dialog reopened empty. The
/// form asks the two things that are actually needed, judges them as they are typed, and says what
/// it is about to create before creating it.
let private newProject model dispatch =
    warnAppWidth dispatch (fun _ ->
    // The last used directory is the last project opened or created, so the folder holding it is
    // where this user keeps their projects - a better guess than the directory itself, which would
    // propose a project inside the last one.
    let defaultParent =
        model.UserData.LastUsedDirectory
        |> Option.map dirName
        |> Option.defaultValue (electronRemote.app.getPath ElectronAPI.Electron.AppGetPath.Documents)
    dispatch <| SetPopupDialogText (Some "")
    dispatch <| SetPopupDialogText2 (Some defaultParent)

    let body =
        fun (model': Model) ->
            let name = getText model'.PopupDialogData
            let parent = getText2 model'.PopupDialogData
            let outcome = newProjectPath parent name
            div [] [
                str "What should the project be called?"
                Input.text [
                    Input.Props [AutoFocus true; SpellCheck false]
                    Input.Placeholder "Project name"
                    Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
                ]
                br []
                str "Where should it go?"
                div [Style [Display DisplayOptions.Flex; AlignItems AlignItemsOptions.Center]] [
                    // Typed as well as browsed: a path can be pasted, which the dialog alone never
                    // allowed. Controlled by the dialog text so that Browse writes into it.
                    div [Style [Flex "1"; MarginRight "8px"]] [
                        Input.text [
                            Input.Value parent
                            Input.Props [SpellCheck false; Style [FontFamily "monospace"]]
                            Input.Placeholder "Folder to create the project in"
                            Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText2 >> dispatch)
                        ]
                    ]
                    Button.button [
                        Button.Size IsSmall
                        Button.OnClick (fun _ ->
                            askForFolder "Where should the project folder go?" "Use this folder"
                                (Some parent)
                            |> Option.iter (Some >> SetPopupDialogText2 >> dispatch))
                    ] [str "Browse..."]
                ]
                br []
                // Said before it happens, because a project is a directory of files rather than
                // the one file the old save dialog implied, and nothing else tells the user that.
                match outcome with
                | Ok path ->
                    div [Style [Color "green"]] [
                        str $"Will create the folder {path}, holding {name}.dprj and the first \
                              sheet, main.dgm."
                    ]
                | Error err ->
                    // "enter a name" is what an untouched form always says: it is a prompt rather
                    // than a complaint, so it is not dressed as one.
                    let untouched = name = "" && parent <> ""
                    div [Style [Color (if untouched then "grey" else "red")]] [str err]
            ]

    let buttonAction =
        fun (model': Model) ->
            match newProjectPath (getText2 model'.PopupDialogData) (getText model'.PopupDialogData) with
            | Ok path ->
                dispatch ClosePopup
                createProjectAt path model dispatch
            | Error _ -> () // unreachable: the button is disabled until this is Ok

    let isDisabled =
        fun (model': Model) ->
            newProjectPath (getText2 model'.PopupDialogData) (getText model'.PopupDialogData)
            |> Result.isError

    dialogPopup "New project" body "Create" buttonAction isDisabled [] dispatch)

    

/// Open the folder the user chose, which may not be a project.
///
/// A folder picker draws every folder alike, so what comes back has to be judged rather than
/// assumed: it may be a project, a project whose marker has been lost, the folder the user's
/// projects live IN, or nothing to do with Issie. Only the last is a dead end, and even that is
/// said out loud. Recursive because choosing from the projects found inside a folder arrives back
/// here, and one of those may itself be missing its marker.
let rec private openChosenFolder (path: string) model dispatch =
    /// Turning the spinner back off: it is put on in the hope of showing during a load, and every
    /// way out of here that does not load something would otherwise leave it spinning over an
    /// unchanged app.
    let giveUp () = dispatch (Sheet (SheetT.SetSpinner false))
    match inspectProjectDirectory path with
    | IsProject -> openProjectFromPath path model dispatch

    | SheetsButNoMarker ->
        // Loadable, and worth loading: the sheets are the project. The marker is what says so to
        // everything that has only the folder to go on, so offer to put it back rather than either
        // refusing the folder or writing to it uninvited.
        giveUp ()
        choicePopup
            "Add the missing project file?"
            (div [] [
                str $"'{baseName path}' holds Issie sheets but no .dprj project file, which is \
                      what marks a folder as an Issie project."
                br []; br []
                str $"Issie can open it either way. Adding {baseName path}.dprj lets it be \
                      recognised as a project in future." ])
            "Add it and open"
            "Open without it"
            (fun addMarker _ ->
                dispatch ClosePopup
                if addMarker then
                    writeFile (projectMarkerPath path) ""
                    |> Notifications.displayAlertOnError dispatch
                openProjectFromPath path model dispatch)
            dispatch

    | NotAProject ->
        giveUp ()
        match projectsWithin path with
        | [] ->
            closablePopup
                "Not an Issie project"
                (div [] [
                    str $"'{path}' is not an Issie project, and holds none."
                    br []; br []
                    str "An Issie project is a folder of .dgm sheet files, marked by a .dprj file \
                         of the same name as the folder. Choose such a folder, or the folder your \
                         projects are kept in." ])
                (div [] [])
                []
                dispatch
        | found ->
            // The folder the projects live in, which is at least as likely a thing to browse to as
            // a project itself - and which the picker gives no way to tell apart from one. Offering
            // what is inside turns the near miss into the thing that was wanted.
            closablePopup
                "Projects in this folder"
                (div [] [
                    str $"'{path}' is not itself an Issie project, but it contains these. \
                          Choose one to open it."
                    br []; br []
                    Menu.menu [] [
                        Menu.list []
                            (found |> List.map (fun (projectPath, kind) ->
                                Menu.Item.li
                                    [ Menu.Item.IsActive false
                                      Menu.Item.OnClick (fun _ ->
                                        dispatch ClosePopup
                                        dispatch (Sheet (SheetT.SetSpinner true))
                                        openChosenFolder projectPath model dispatch) ]
                                    [ div [] [
                                        str (baseName projectPath)
                                        match kind with
                                        | SheetsButNoMarker ->
                                            span [Style [Color "grey"; MarginLeft "8px"]]
                                                 [str "(sheets, but no project file)"]
                                        | _ -> null ] ]))
                    ] ])
                (div [] [])
                []
                dispatch

/// open an existing project
let private openProject model dispatch =
    //trying to force the spinner to load earlier
    //doesn't really work right now
    warnAppWidth dispatch (fun _ ->
    dispatch (Sheet (SheetT.SetSpinner true))
    let dirName =
        match Option.map readFilesFromDirectory model.UserData.LastUsedDirectory with
        | Some [] | None -> None
        | _ -> model.UserData.LastUsedDirectory
    match askForExistingProjectPath dirName with
    | None -> dispatch (Sheet (SheetT.SetSpinner false)) // User gave no path.
    | Some path -> openChosenFolder path model dispatch)

/// Close current project, if any.
let forceCloseProject (model:Model) dispatch =
    dispatch (StartUICmd CloseProject)
    let sheetDispatch sMsg = dispatch (Sheet sMsg) 
    dispatch EndSimulation // End any running simulation.
    dispatch <| TruthTableMsg CloseTruthTable // Close any open Truth Table.
    // End any running simulation.
    dispatch EndSimulation
    dispatch EndWaveSim
    model.Sheet.ClearCanvas sheetDispatch
    dispatch <| UpdateModel (
        fun model ->
            { model with
                RightPaneTabVisible = Properties
                Pending = []}
                )
    dispatch FinishUICmd

/// Implement a command involving file operations from Update, with access to dispatch
/// Invoked by message: `FileCommand(fc,dispatch)`.
/// TODO - refactor to remove dispatch dependence
let fileCommand (fc: FileCommandType) (dispatch: (Msg->Unit)) (model: Model) =
    match fc with
    | FileAddFile ->
        addFileToProject model dispatch
        model, Cmd.none        
    | FileImportSheet ->

        MiscMenuView.importSheet model dispatch
        model, Cmd.none

    | FileNewProject withSave ->
        if withSave then
            doActionWithSaveFileDialog "New project" (ExecFuncInMessage(newProject,dispatch)) model dispatch ()
        else
                newProject model dispatch
        model, Cmd.none

    | FileOpenProject  withSave ->
        if withSave then
            doActionWithSaveFileDialog "Open project" (ExecFuncInMessage(openProject,dispatch)) model dispatch ()
        else
            openProject model dispatch
        model, Cmd.none

    | FileCloseProject  ->
        doActionWithSaveFileDialog "Close project" (ExecFuncInMessage(forceCloseProject,dispatch)) model dispatch ()
        model, Cmd.none

    | FileSaveOpenFile ->
        saveOpenFileActionWithModelUpdate model dispatch |> ignore
        // A library sheet whose last instance has gone is dropped here rather than when the
        // instance was deleted, so that deleting an instance stays undoable.
        dispatch <| UpdateModel MenuHelpers.sweepUnusedLibrarySheets
        model, Cmd.none

    | FileShowDemos demoOpts ->
        showDemoProjects model dispatch demoOpts
        model, Cmd.none
        
    
