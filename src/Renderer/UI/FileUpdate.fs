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

    

/// The folder the browser is looking at, and the counter that makes it look at the disk again.
/// Empty when the browser is not open, which the popup never renders in.
let private browserState (model: Model) =
    model.ProjectBrowser |> Option.defaultValue { Folder = ""; Generation = 0 }

type private BrowserListProps = {
    Folder: string
    Generation: int
    Open: string -> unit
    Navigate: string -> unit
}

/// The folders inside the one being browsed, each saying what Issie makes of it.
///
/// Memoised on the folder and the generation. A popup body runs on every message, so without this
/// every keystroke in the path field would list whatever prefix had been typed so far; with it,
/// the disk is read when the folder changes and once a second thereafter. Props holding functions
/// are exactly what equalsButFunctions is for.
let private browserList =
    FunctionComponent.Of(
        (fun (props: BrowserListProps) ->
            if not (isDirectory props.Folder) then
                div [Style [Color "red"; Padding "8px 0"]] [str "That folder does not exist."]
            else
                match listFolderForOpening props.Folder with
                | [] ->
                    div [Style [Color "grey"; Padding "8px 0"]]
                        [str "Nothing in this folder. Go up, or type a path above."]
                | entries ->
                    Menu.menu [] [
                        Menu.list []
                            (entries |> List.map (fun entry ->
                                let name = baseName entry.Path
                                let sheets =
                                    if entry.SheetCount = 1 then "1 sheet" else $"{entry.SheetCount} sheets"
                                let label, note, act =
                                    match entry.Kind with
                                    | IsProject ->
                                        b [] [str name], span [Style [Color "grey"]] [str sheets], props.Open
                                    | SheetsButNoMarker ->
                                        b [] [str name],
                                        span [Style [Color "darkorange"]] [str $"{sheets}, no project file"],
                                        props.Open
                                    // an ordinary folder is not a dead end: it is where the next
                                    // level of browsing goes
                                    | NotAProject ->
                                        span [Style [Color "dimgrey"]] [str name],
                                        span [Style [Color "grey"]] [str "›"],
                                        props.Navigate
                                Menu.Item.li
                                    [ Menu.Item.IsActive false
                                      Menu.Item.OnClick (fun _ -> act entry.Path) ]
                                    [ div [Style [Display DisplayOptions.Flex; JustifyContent "space-between"]]
                                          [ label; note ] ]))
                    ]),
        "ProjectBrowserList",
        Fable.React.Helpers.equalsButFunctions)

/// Open the folder the user chose, which may not be a project.
///
/// What is chosen has to be judged rather than assumed: it may be a project, a project whose
/// marker has been lost, or a folder that merely contains projects. The last is not a refusal -
/// it is where browsing continues. Mutually recursive with the browser, since the browser opens
/// folders and an unopenable folder puts the browser back.
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

    // Not something to open, so it is something to look inside: browsing continues there. This is
    // the case a native folder picker cannot help with, since it draws the folder holding your
    // projects exactly like the projects themselves.
    | NotAProject ->
        giveUp ()
        viewProjectBrowser path model dispatch

/// Choose a project to open, from a list Issie draws itself.
///
/// A native folder picker cannot show which folders are projects - it has no idea - so browsing in
/// one is guesswork until something is chosen. Here every folder says what it is before it is
/// picked, and an ordinary folder is a way further in rather than a mistake. The system dialog
/// remains, as Browse, for the places this list will not reach: another drive, a share, anywhere
/// worth typing rather than walking to.
and private viewProjectBrowser (startFolder: string) model dispatch =
    dispatch <| SetProjectBrowserFolder startFolder

    let body =
        fun (model': Model) ->
            let browser = browserState model'
            let folder = browser.Folder
            let recents = model'.UserData.RecentProjects |> Option.defaultValue []
            let goTo (path: string) = dispatch <| SetProjectBrowserFolder path
            let openIt (path: string) =
                dispatch ClosePopup
                dispatch (Sheet (SheetT.SetSpinner true))
                // model' rather than the model this popup was opened with: opening reads the
                // recent list to add to it, and the popup outlives whatever else has happened
                openChosenFolder path model' dispatch
            div [] [
                if not (List.isEmpty recents) then
                    div [Style [MarginBottom "10px"]] [
                        b [] [str "Recent"]
                        Menu.menu [] [
                            Menu.list []
                                (recents |> List.map (fun path ->
                                    Menu.Item.li
                                        [ Menu.Item.IsActive false
                                          Menu.Item.OnClick (fun _ -> openIt path) ]
                                        [ div [HTMLAttr.Title path]
                                              [str (cropToLength Constants.maxDisplayedPathLengthInRecentProjects false path)] ]))
                        ]
                    ]
                    hr []

                b [] [str "Folder"]
                div [Style [Display DisplayOptions.Flex; AlignItems AlignItemsOptions.Center; MarginBottom "8px"]] [
                    // typed as well as walked to, which is how another drive or a share is reached
                    // without this becoming a file manager
                    div [Style [Flex "1"; MarginRight "8px"]] [
                        Input.text [
                            Input.Value folder
                            Input.Props [SpellCheck false; Style [FontFamily "monospace"]]
                            Input.OnChange (getTextEventValue >> SetProjectBrowserFolder >> dispatch)
                        ]
                    ]
                    Button.button [
                        Button.Size IsSmall
                        Button.Disabled (isFilesystemRoot folder)
                        Button.OnClick (fun _ -> goTo (dirName folder))
                    ] [str "Up"]
                    // No Refresh button: the folder is re-read once a second while this is open,
                    // so a folder made or renamed outside Issie simply appears.
                    Button.button [
                        Button.Size IsSmall
                        Button.Props [Style [MarginLeft "4px"]]
                        Button.OnClick (fun _ ->
                            askForFolder "Choose a project folder, or a folder of projects" "Use this folder"
                                (Some folder)
                            |> Option.iter (fun chosen ->
                                // whatever was chosen, honour the intent: open it if it can be
                                // opened, otherwise carry on browsing from there
                                match inspectProjectDirectory chosen with
                                | NotAProject -> goTo chosen
                                | _ -> openIt chosen))
                    ] [str "Browse..."]
                ]

                browserList {
                    Folder = folder
                    Generation = browser.Generation
                    Open = openIt
                    Navigate = goTo
                }
            ]

    dynamicClosablePopup "Open project" body (fun _ -> div [] []) [Width "640px"] dispatch

/// open an existing project
let private openProject model dispatch =
    warnAppWidth dispatch (fun _ ->
    // Where this user keeps their projects: the folder holding the last one they had open, which
    // is where the next one almost always is. The project itself would show only its sheets.
    let start =
        model.UserData.LastUsedDirectory
        |> Option.filter isDirectory
        |> Option.map dirName
        |> Option.defaultValue (electronRemote.app.getPath ElectronAPI.Electron.AppGetPath.Documents)
    viewProjectBrowser start model dispatch)

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
        
    
