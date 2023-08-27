(*
    FileMenuView.fs

    View for the top menu, and related functionalities: renamimg, loadimg, saving, deleting sheets
*)

module FileMenuView
open EEExtensions
open Fulma
open Fable.React
open Fable.React.Props
open Fulma.Extensions.Wikiki

open Helpers
open JSHelpers
open DiagramStyle
open ModelType
open ModelHelpers
open CommonTypes
open FilesIO
open Extractor
open Notifications
open PopupHelpers
open DrawModelType
open Sheet.SheetInterface
open Optics
open FileMenuHelpers

open System

module Constants =
    let numberOfRecentProjects: int  = 5
    let maxDisplayedPathLengthInRecentProjects: int  = 60

let private displayFileErrorNotification err dispatch =
    let note = errorFilesNotification err
    dispatch <| SetFilesNotification note




    



//////////////////

/// Save the Verilog file currently open, return the new sheet's Loadedcomponent if this has changed.
/// Do not change model.
let updateVerilogFileAction newCS name model (dispatch: Msg -> Unit)=
    match model.CurrentProj with
    | None -> failwithf "No project"
    | Some project ->
        // "DEBUG: Saving Sheet"
        // printfn "DEBUG: %A" project.ProjectPath
        // printfn "DEBUG: %A" project.OpenFileName
        let sheetInfo = {Form=Some (Verilog name);Description=None} //only user defined sheets are editable and thus saveable
        let savedState = newCS, getSavedWave model,(Some sheetInfo)
        saveStateToFile project.ProjectPath name savedState
        |> displayAlertOnError dispatch
        removeFileWithExtn ".dgmauto" project.ProjectPath name
        let origLdComp =
            project.LoadedComponents
            |> List.find (fun lc -> lc.Name = name)
        let savedWaveSim =
            Map.tryFind name model.WaveSim
            |> Option.map getSavedWaveInfo
        let (SheetInfo:SheetInfo option) = match origLdComp.Form with |None -> None |Some form -> Some {Form=Some form;Description=origLdComp.Description}
        let (newLdc, ramCheck) = makeLoadedComponentFromCanvasData newCS origLdComp.FilePath DateTime.Now savedWaveSim SheetInfo
        let newState =
            newCS
            |> (fun (comps, conns) -> 
                    comps
                    |> List.map (fun comp -> 
                        match List.tryFind (fun (c:Component) -> c.Id=comp.Id) ramCheck with
                        | Some newRam -> 
                            // TODO: create consistent helpers for messages
                            dispatch <| Sheet (SheetT.Wire (BusWireT.Symbol (SymbolT.WriteMemoryType (ComponentId comp.Id, newRam.Type))))
                            newRam
                        | _ -> comp), conns)
        writeComponentToBackupFile 4 1. newLdc dispatch
        Some (newLdc,newState)
        
/// save current open Verilog file, updating model etc, and returning the loaded component and the saved (unreduced) canvas state
let updateVerilogFileActionWithModelUpdate (newCS:CanvasState) name (model: Model) (dispatch: Msg -> Unit) =
    let p' =
        match model.CurrentProj with
        | None -> failwithf "What? Should never be able to save sheet when project=None"
        | Some p -> {p with WorkingFileName = Some name}
    let model' = {model with CurrentProj = Some p'}

    let opt = updateVerilogFileAction newCS name model' dispatch
    let ldcOpt = Option.map fst opt
    let state = Option.map snd opt |> Option.defaultValue ([],[])
    match model'.CurrentProj with
    | None -> failwithf "What? Should never be able to save sheet when project=None"
    | Some p -> 
        // update loaded components for saved file
        updateLdCompsWithCompOpt ldcOpt p.LoadedComponents
        |> (fun lc -> {p with LoadedComponents=lc})
        |> SetProject
        |> dispatch

    let p'' =
        match model'.CurrentProj with
        | None -> failwithf "What? Should never be able to save sheet when project=None"
        | Some p -> 
            // update loaded components for saved file
            updateLdCompsWithCompOpt ldcOpt p.LoadedComponents
            |> (fun lc -> {p with LoadedComponents=lc})

    SetHasUnsavedChanges false
    |> JSDiagramMsg
    |> dispatch
    dispatch FinishUICmd     
    p''

//////////////////









let createEmptyComponentAndFile (pPath:string)  (sheetName: string): LoadedComponent =
    createEmptyDgmFile pPath sheetName |> ignore
    {
        Name=sheetName
        WaveInfo = None
        TimeStamp = DateTime.Now
        FilePath= pathJoin [|pPath; sprintf "%s.dgm" sheetName|]
        CanvasState=([],[])
        InputLabels = []
        OutputLabels = []
        Form = Some User
        Description = None
    }
    

    




/// return a react warning message if name if not valid for a sheet Add or Rename, or else None
let maybeWarning dialogText project =
    let redText txt = Some <| div [ Style [ Color "red" ] ] [ str txt ]
    if isFileInProject dialogText project then
        redText "This sheet already exists." 
    elif dialogText.StartsWith " " || dialogText.EndsWith " " then
        redText "The sheet name cannot start or end with a space."
    elif String.exists ((=) '.') dialogText then
        redText "The sheet name cannot contain a file suffix."
    elif not <| String.forall (fun c -> Char.IsLetterOrDigitOrUnderscore c || c = ' ') dialogText then
        redText "The sheet name must contain only letters, digits, spaces or underscores"
    elif ((dialogText |> Seq.tryItem 0) |> Option.map Char.IsDigit) = Some true then
        redText "The name must not start with a digit"
    else None


/// rename a sheet
let renameSheet oldName newName (model:Model) dispatch =

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
            WorkingFileName = if proj.OpenFileName = oldName then Some newName else proj.WorkingFileName
            LoadedComponents =
                proj.LoadedComponents
                |> List.map (fun ldComp -> 
                    match ldComp with
                    | {Name = lcName} when lcName = oldName -> 
                        {ldComp with Name=newName; FilePath = pathJoin [|(dirName ldComp.FilePath);newName + ".dgm"|] }
                    | _ ->
                        renameCustomComponents newName ldComp )
        }
    match updateProjectFromCanvas model dispatch with
    | None -> 
        failwithf "What? current project cannot be None at this point in renamesheet"
    | Some p ->
        let updatedModel = {model with CurrentProj = Some p}
        let opt = saveOpenFileAction false updatedModel dispatch
        let ldcOpt = Option.map fst opt
        let ldComps = updateLdCompsWithCompOpt ldcOpt p.LoadedComponents
        let reducedState = Option.map snd opt |> Option.defaultValue ([],[])
        //SetHasUnsavedChanges false
        //|> JSDiagramMsg
        //|> dispatch
        [".dgm"] |> List.iter (fun extn -> 
            renameFile extn p.ProjectPath oldName newName
            |> displayAlertOnError dispatch)
        let proj' = renameSheetsInProject oldName newName p
        setupProjectFromComponents false proj'.OpenFileName proj'.LoadedComponents model dispatch
        //printfn "???Sheets after rename"
        //printSheetNames {model with CurrentProj = Some proj'}
        // save all the other files
        saveAllProjectFilesFromLoadedComponentsToDisk proj'
        dispatch FinishUICmd


/// rename file
let renameFileInProject name project model dispatch =
    match model.CurrentProj with
    | None -> log "Warning: renameFileInProject called when no project is currently open"
    | Some project ->
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
            fun (model: Model) ->
                // Create empty file.
                let newName = (getText model.PopupDialogData).ToLower()
                // rename the file in the project.
                dispatch(ExecFuncInMessage(renameSheet name newName, dispatch))
                dispatch ClosePopup

        let isDisabled =
            fun (model: Model) ->
                let dialogData = model.PopupDialogData
                let dialogText = getText dialogData
                (isFileInProject dialogText project) || (dialogText = "")

        dialogPopup title body buttonText buttonAction isDisabled [] dispatch

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
            fun (model': Model) ->
                    let dialogData = model'.PopupDialogData
                    // Create empty file.
                    let name = (getText dialogData).ToLower()
                    createEmptyDgmFile project.ProjectPath name
                    |> displayAlertOnError dispatch
                    // Add the file to the project.
                    let newComponent = {
                        Name = name
                        TimeStamp = System.DateTime.Now
                        WaveInfo = None
                        FilePath = pathJoin [|project.ProjectPath; name + ".dgm"|]
                        CanvasState = [],[]
                        InputLabels = []
                        OutputLabels = []
                        Form = Some User
                        Description = None
                    }
                    let updatedProject =
                        { project with
                              LoadedComponents = newComponent :: project.LoadedComponents
                              OpenFileName = name
                              WorkingFileName = Some name }
 
                    // Open the file, updating the project, saving current file
                    openFileInProject' true name updatedProject model dispatch
                    // Close the popup.
                    dispatch ClosePopup
                    dispatch FinishUICmd

        let isDisabled =
            fun (model': Model) ->
                let dialogData = model'.PopupDialogData
                let dialogText = getText dialogData
                (isFileInProject dialogText project) || (dialogText = "") || (maybeWarning dialogText project <> None)

        dialogPopup title body buttonText buttonAction isDisabled [] dispatch

/// Close current project, if any.
let forceCloseProject model dispatch =
    dispatch (StartUICmd CloseProject)
    let sheetDispatch sMsg = dispatch (Sheet sMsg) 
    dispatch EndSimulation // End any running simulation.
    dispatch <| TruthTableMsg CloseTruthTable // Close any open Truth Table.
    // End any running simulation.
    dispatch EndSimulation
    dispatch EndWaveSim
    model.Sheet.ClearCanvas sheetDispatch
    dispatch FinishUICmd

/// force either save of current file before action, or abort (closeProject is special case of this)
let doActionWithSaveFileDialog (name: string) (nextAction: Msg)  model dispatch _ =
    let closeDialogButtons keepOpen _ =
        if keepOpen then
            dispatch ClosePopup
        else
            dispatch nextAction

    if model.SavedSheetIsOutOfDate then 
        choicePopup 
                $"{name}?" 
                (div [] [ str "The current sheet has unsaved changes."])
                "Go back to sheet" 
                $"{name} without saving changes"  
                closeDialogButtons 
                dispatch
    else
        dispatch nextAction

/// Create a new project.
let private newProject model dispatch  =
    match askForNewProjectPath model.UserData.LastUsedDirectory with
    | None -> () // User gave no path.
    | Some path ->
        match tryCreateFolder path with
        | Error err ->
            log err
            displayFileErrorNotification err dispatch
        | Ok _ ->
            dispatch EndSimulation // End any running simulation.
            dispatch <| TruthTableMsg CloseTruthTable // Close any open Truth Table.
            dispatch EndWaveSim
            // Create empty placeholder projectFile.
            let projectFile = baseName path + ".dprj"
            writeFile (pathJoin [| path; projectFile |]) ""
            |> displayAlertOnError dispatch
            // Create empty initial diagram file.
            let initialComponent = createEmptyComponentAndFile path "main"
            dispatch <| SetUserData {model.UserData with LastUsedDirectory = Some path}
            setupProjectFromComponents false "main" [initialComponent] model dispatch

/// work out what to do opening a file
let rec resolveComponentOpenPopup 
        (pPath:string)
        (components: LoadedComponent list)  
        (resolves: LoadStatus list) 
        (model: Model)
        (dispatch: Msg -> Unit) =
    let chooseWhichToOpen comps =
        let onlyUserCreated = List.filter (fun comp -> match comp.Form with |Some User |None -> true |_ ->false) comps
        (List.maxBy (fun comp -> comp.TimeStamp) onlyUserCreated).Name
    dispatch ClosePopup
    match resolves with
    | [] -> setupProjectFromComponents false (chooseWhichToOpen components) components model dispatch
    | Resolve (ldComp,autoComp) :: rLst ->
        // ldComp, autocomp are from attemps to load saved file and its autosave version.
        let compChanges, connChanges = quantifyChanges ldComp autoComp
        let buttonAction autoSave _ =
            let comp = {(if autoSave then autoComp else ldComp) with TimeStamp = DateTime.Now}
            writeComponentToFile comp
            |> displayAlertOnError dispatch
            if compChanges + connChanges > 0 then
                writeComponentToBackupFile 0 1. comp dispatch
            resolveComponentOpenPopup pPath (comp :: components) rLst  model dispatch   
        // special case when autosave data is most recent
        let title = "Warning!"
        let message, color =
            match compChanges + connChanges with
            | 0 -> 
                sprintf "There were layout but no circuit changes made in sheet %s after your last save. \
                         There is an automatically saved version which is \
                         more uptodate. Do you want to keep the newer AutoSaved version or \
                         the older Saved version?"  ldComp.Name, "green"  
            | n when n < 3 ->   
                sprintf "Warning: %d component and %d connection changes were made to sheet '%s' after your last Save. \
                         There is an automatically saved version which is \
                         more uptodate. Do you want to keep the newer AutoSaved version or \
                         the older saved version?"  compChanges connChanges ldComp.Name, "orange"
            | n -> 
                sprintf "Warning: %d component and %d connection changes were made to sheet '%s' after your last Save. \
                         There is an automatically saved version which is \
                         more uptodate. Do you want to keep the newer AutoSaved version or \
                         the older saved version? This is a large change so the option you do not choose \
                         will be saved as file 'backup/%s.dgm'"  compChanges connChanges ldComp.Name ldComp.Name, "red"
        let body = 
            div [Style [Color color]] [str message] 
        choicePopup title body "Newer AutoSaved file" "Older Saved file" buttonAction dispatch
    | OkAuto autoComp :: rLst ->
         let errMsg = "Could not load saved project file '%s' - using autosave file instead"
         displayFileErrorNotification errMsg dispatch
         resolveComponentOpenPopup pPath (autoComp::components) rLst model dispatch
    | OkComp comp ::rLst -> 
        resolveComponentOpenPopup pPath (comp::components) rLst model dispatch

let addToRecents path recents =
    recents
    |> Option.defaultValue []
    |> List.filter ((<>) path)
    |> List.truncate Constants.numberOfRecentProjects
    |> List.insertAt 0 path
    |> Some

/// open an rxisting porject from its path
let openProjectFromPath (path:string) model dispatch =
    dispatch (ExecFuncAsynch <| fun () ->
        traceIf "project" (fun () -> "loading files")
        match loadAllComponentFiles path with
        | Error err ->
            log err
            displayFileErrorNotification err dispatch
        | Ok (componentsToResolve: LoadStatus list) ->
            traceIf "project" (fun () -> "resolving popups...")
            
            resolveComponentOpenPopup path [] componentsToResolve model dispatch
            traceIf "project" (fun () ->  "project successfully opened.")
            dispatch <| SetUserData {
                model.UserData with 
                    LastUsedDirectory = Some path; 
                    RecentProjects = addToRecents path model.UserData.RecentProjects
                    }
        Elmish.Cmd.none)
    

/// open an existing project
let private openProject model dispatch =
    //trying to force the spinner to load earlier
    //doesn't really work right now
    dispatch (Sheet (SheetT.SetSpinner true))
    let dirName =
        match Option.map readFilesFromDirectory model.UserData.LastUsedDirectory with
        | Some [] | None -> None
        | _ -> model.UserData.LastUsedDirectory
    match askForExistingProjectPath dirName with
    | None -> () // User gave no path.
    | Some path -> openProjectFromPath path model dispatch

/// Display the initial Open/Create Project menu at the beginning if no project
/// is open.
let viewNoProjectMenu model dispatch =
    let menuItem label action =
        Menu.Item.li
            [ Menu.Item.IsActive false
              Menu.Item.OnClick action ] [ str label ]
    
    let recentsList = 
        model.UserData
        |> (fun ud -> ud.RecentProjects)
        |> Option.defaultValue []
        |> List.map (fun path -> 
                        menuItem 
                            (cropToLength  Constants.maxDisplayedPathLengthInRecentProjects false path) 
                            (fun _ -> openProjectFromPath path model dispatch))

    let initialMenu =
        Menu.menu []
            [ Menu.list []
                  ([ menuItem "New project" (fun _ -> newProject model dispatch)
                     menuItem "Open project" (fun _ -> openProject model dispatch)]
                  @ (if recentsList <> [] then [hr []] else [])
                  @ recentsList)
            ]

    match model.CurrentProj with
    | Some _ -> div [] []
    | None -> 
        unclosablePopup None initialMenu None [] dispatch


//These two functions deal with the fact that there is a type error otherwise..
let goBackToProject model dispatch _ =
    dispatch (SetExitDialog false)

let closeApp model dispatch _ =
    dispatch CloseApp



/// Find all sheets that depend on the given sheet in the current project, return the sheet's signature as well.
/// If given sheet name doesn't exist in the project, return signature of working file
let getDependentsFromSheet (model:Model) (sheetName : string) =
    let getCorrectFileName (project:Project) = 
        match project.WorkingFileName with
        |Some name -> name
        |None -> project.OpenFileName

    mapOverProject None model <| fun p ->
         
         let newSig =
             p.LoadedComponents
             |> List.tryFind (fun ldc -> ldc.Name = sheetName)
             |> (fun ldcOption ->
                    match ldcOption with
                    | Some ldc -> parseDiagramSignature ldc.CanvasState
                    | None ->
                        p.LoadedComponents
                        |> List.find (fun ldc -> ldc.Name = getCorrectFileName p)
                        |> (fun ldc -> parseDiagramSignature ldc.CanvasState)
                )
                          
         let instances =
             p.LoadedComponents
             |> List.filter (fun ldc -> ldc.Name <> sheetName)
             |> List.collect (fun ldc -> 
                 fst ldc.CanvasState
                 |> List.collect (
                     function 
                         | {Type = Custom { Name=name; InputLabels=ins; OutputLabels=outs}
                            Id = cid} when name = sheetName-> [ldc.Name, cid,  (ins,outs)]
                         | _ -> []))

         Some(newSig, instances)
   
// get relevant info about a sheet for display on popup
let getSheetInfo (model : Model) (oldSheetPath : string) (newSheetPath : string) =

    // get sheets in current project that would depend on an existent sheet, same as one that's being imported
    
    if newSheetPath |> exists then
        let sheetName = baseNameWithoutExtension oldSheetPath // could use newSheetPath as well here            

        match getDependentsFromSheet model sheetName with
        | None -> Some ""
        | Some (newSig, instances) ->
            instances
            |> List.map (fun (sheet,_,sg) -> sheet)
            |> List.distinct
            |> String.concat ","
            |> Some

    else None

// import sheet from directory, ask user to sort out dependency issues
let private importSheet model dispatch =
    match model.CurrentProj with
    | None -> log "Current project must be open for sheet to be imported to it"
    | Some project -> 
        let projectDir = project.ProjectPath

        dispatch <| (Sheet (SheetT.SetSpinner false))

        let importDecisions model = getImportDecisions model.PopupDialogData

        let updateDecisions (sheetPath: string) (decisionOption: ImportDecision option) (model' : Model) =
            let updatedDecisions = Map.add sheetPath decisionOption (importDecisions model')

            dispatch <| UpdateImportDecisions updatedDecisions
        
        /// Return only sheets that exist / don't exist in the destination directory based on boolean. True -> return existent. False -> return non-existent.
        let filterSheets (allSheets : string list) (existing : bool) =
            allSheets
            |> List.filter (fun sheetPath ->
                   
                let newSheetPath = pathJoin [|projectDir; baseName sheetPath|]

                if not <| fileNameIsBad (baseNameWithoutExtension <| baseName sheetPath) then
                    if existing then exists <| newSheetPath else not (exists <| newSheetPath)
                else
                    false
                   
            )

        // Function to check if all decisions are made
        let allDecisionsMade allSheets =
            fun (model : Model) ->
                match filterSheets allSheets true with
                | [] -> true
                | sheets ->
                    sheets
                    |> List.forall (fun sheetPath -> Map.containsKey sheetPath (importDecisions model))
               
        let copySheet (sourcePath: string) (newPath: string) model dispatch =
            match readFile sourcePath |> writeFile newPath with
            | Ok _ -> ()
            | Error msg -> displayFileErrorNotification msg dispatch

        let createSheetInfo (model : Model) ((sheetPath, dependencies): string * Set<string>) =
            let fileName = baseName sheetPath

            let newSheetPath = pathJoin [|projectDir; fileName|]

            let hasDependencies = Set.count dependencies <> 0

            let sheetExists, depSheets =
                match getSheetInfo model sheetPath newSheetPath with
                | Some depSheets -> true, depSheets
                | None -> false, ""

            let decisionMadeMatches (sheetPath : string) (decision : ImportDecision option) =
                fun (model : Model) ->
                    let valueOption = Map.tryFind sheetPath (importDecisions model)

                    match valueOption with
                    | Some decision' -> decision' = decision
                    | None -> false

            if projectDir = dirName sheetPath then
                displayFileErrorNotification "Cannot import sheet from curent directory" dispatch

                div [] [
                p [] [ 
                    str "Cannot import "
                    strong [] [ str fileName ]
                    str " because it is from current directory. Import has been disabled. "
                    ]
                ]

            
            else
                let Button (sheetPath : string) (buttonDecision : ImportDecision option) (name : string) (isDisabled : bool) =
                    [ Button.button
                        [ 
                            Button.Size IsSmall
                            Button.IsOutlined
                            Button.Color IsPrimary
                            Button.Disabled isDisabled
                            Button.IsFocused (decisionMadeMatches sheetPath buttonDecision model)
                            Button.OnClick(fun _ ->
                                updateDecisions sheetPath buttonDecision model
                            )] [ str name ]             
                    ]

                let dependencyReactElement dependency =                            
          
                    match dependency with
                    | dependencyPath ->
                        match hasExtn ".dgm" dependencyPath with
                        | true ->
                        Navbar.Item.div [ Navbar.Item.Props [] ]
                            [ Level.level [ Level.Level.Props []]
                                  [ Level.left [Props [Style [FontWeight "bold"]]] [ Level.item [] [ str <| baseNameWithoutExtension dependencyPath ] ]
                                    Level.right [ Props [ Style [ MarginLeft "5px" ] ] ]
                                        [
                                          Level.item []
                                            [
                                            
                                            match Map.containsKey dependencyPath (importDecisions model) with
                                            | true ->
                                                str "will be imported."
                                            | false ->
                                                str "will be ignored."
                                 
                                            ]

                                          Level.item []
                                            (Button dependencyPath (Some Overwrite) "Import" false)

                                        ] ] ]
                        | false ->
          
                            p [Style [Color "red"; FontWeight "Bold"]]
                            [
                                str "Warning: "
                                strong [] [str dependencyPath]
                                str " doesn't exist in source and destination directories."
                            ]

          
                match sheetExists with
                | true ->

                    match tryLoadComponentFromPath sheetPath with
                    | Error err ->
                        log err
                        div [] [str err]
                    | Ok ldcSource ->

                        let sourceSig = parseDiagramSignature ldcSource.CanvasState

                        let destSig =
                            tryGetLoadedComponents model
                            |> List.find (fun ldc -> ldc.Name = baseNameWithoutExtension sheetPath)
                            |> (fun ldc -> parseDiagramSignature ldc.CanvasState)
                                        

                        div [] [
                        p [] [
                        str "Sheet "
                        strong [] [str fileName]
                        str <| sprintf " already exists in project. "
                        str <| (match depSheets with
                                | "" -> ""
                                | _ -> sprintf "Dependents: %s " depSheets
                                )
                        ]

                        match (sourceSig <> destSig) with
                        | true ->
                            if (depSheets <> "") then
                                p [Style [Color "red"]] [
                                    str "Overwrite disabled because sheets contain different hardware. Danger of conflicts in dependents."
                                ]
                            else
                                p [Style [Color "green"]] [
                                    str "Sheets contain different hardware, but overwrite allowed as there are no dependents."
                                ]

                        | false ->
                            str ""

                        Navbar.Item.div [ Navbar.Item.Props [] ]
                            [ Level.level [ Level.Level.Props []]
                                  [ Level.left [Props [Style [FontWeight "bold"]]] [ Level.item [] [ str "Decision: " ] ]
                                    Level.right [ Props [ Style [ MarginLeft "20px" ] ] ]
                                        [        
                                          Level.item []
                                            (Button sheetPath (Some Overwrite) "Overwrite" ((sourceSig <> destSig) && (depSheets <> "")))
                               
                                          Level.item []
                                            (Button sheetPath (Some Rename) "Rename" false)

                                        ] ] ]

                        match hasDependencies with
                        | true ->
                            p [Style [Color "blue"; FontWeight "bold"]] [
                                str "Import dependencies: "
                            ]

                            let content = 
                                dependencies
                                |> Set.toArray
                                |> Array.map (fun dependency ->
                   
                                    dependencyReactElement dependency
                                )

                            (div [] content)

                            
                        | false -> str ""
                        ]
                                
                | false ->
                    if fileNameIsBad (pathWithoutExtension fileName)
                    then

                        div [] [
                        p [] [ 
                            str "Can't load file name "
                            strong [] [ str fileName ]
                            str <| sprintf" from project %s" sheetPath
                            str " because it contains incorrect characters.\nFile names used as sheets must contain only alphanumeric and space characters before the '.dgm' extension"  
                        ]]

                    else
                       
                        div [] [
                        p [] [
                            str "Sheet "
                            strong [] [ str fileName ]
                            str " will be imported without conflicts."]

                        match hasDependencies with
                        | true ->
                            p [Style [Color "blue"; FontWeight "bold"]] [
                                str "Import dependencies: "
                            ]

                            let content = 
                                dependencies
                                |> Set.toArray
                                |> Array.map (fun dependency ->
                                    dependencyReactElement dependency
                                )

                            (div [] content)   
                        
                        | false -> str ""
                        ]

        match askForExistingSheetPaths model.UserData.LastUsedDirectory with
        | None -> () // User gave no path.
        | Some paths ->
            let sourceProjectPath = dirName paths[0]

            let pathsWithDependencies =
                paths
                |> List.map (fun path ->
                    
                    let rec parse2 (path : string) (deps : string list) =
                        
                        match tryLoadComponentFromPath path with
                        | Error err ->
                            log <| err
                            deps  // the dependency doesn't exist in source directory
                            
                        | Ok ldc ->
                            let comps, _ = ldc.CanvasState


                            let customCompsPaths =
                                comps
                                |> List.filter (fun comp ->
                                    match comp.Type with
                                    | Custom _ -> true
                                    | _ -> false
                                )
                                |> List.map (fun comp ->
                                    match comp.Type with
                                    | Custom ct ->
                                        let dependencyPath = pathJoin [|sourceProjectPath; ct.Name + ".dgm"|]

                                        match exists dependencyPath with
                                        | true -> dependencyPath
                                        | false -> ""

                                    | _ -> ""
                                )
                                |> List.distinct
                                |> List.filter (fun s -> s <> "")
                            
                                
                            match List.length customCompsPaths with
                            | 0 ->  deps
                            | _ ->
                                customCompsPaths
                                |> List.collect (fun dependencyPath ->
                                    let dependencyName = baseName dependencyPath

                                    match exists dependencyPath with
                                    | true ->
                                        match (exists <| pathJoin [|projectDir; dependencyName|]) || (List.contains dependencyPath paths) with
                                        | true -> parse2 dependencyPath deps
                                        | false -> parse2 dependencyPath (dependencyPath :: deps)

                                    | false ->
                                        match (exists <| pathJoin [|projectDir; baseName dependencyPath|]) || (List.contains dependencyPath paths) with
                                        | true -> parse2 dependencyPath deps
                                        | false -> parse2 dependencyPath (dependencyName :: deps)  // dependency doesn't exist in either directory
                                )

                    let dependencies =
                        parse2 path []
                        |> Set.ofList

                    (path, dependencies)
                )

        
            let popupBody =
                fun (model' : Model) ->
                    let content =
                        pathsWithDependencies
                        |> List.map (createSheetInfo model')
                        |> List.toArray
                    (div [] content)

            let buttonAction =
                fun (model' : Model) ->
                    // based on the decision, make new sheet path and copy sheet over

                    let newSheetPaths = 
                        (importDecisions model')
                        |> Map.toList
                        |> List.map (fun (sheetPath, decision) ->
                            match decision with
                            | Some Overwrite  ->
                                sheetPath, pathJoin [|projectDir; baseName sheetPath|]

                            | Some Rename ->
                                sheetPath, pathJoin [|projectDir; baseNameWithoutExtension sheetPath + "_Copy" + ".dgm"|]

                            | None -> sheetPath, ""

                        )


                    filterSheets paths false
                    |> List.iter (fun oldSheetPath ->
                        let newSheetPath = pathJoin [|projectDir; baseName oldSheetPath|] 

                        copySheet oldSheetPath newSheetPath model' dispatch
                       
                    )

                    newSheetPaths |> List.iter (fun (oldSheetPath, newSheetPath) ->
                                match newSheetPath with
                                | "" -> ()
                                | path -> copySheet oldSheetPath path model' dispatch)
                               
                    openProjectFromPath projectDir model' dispatch

                    dispatch ClosePopup
                    dispatch FinishUICmd

            let isDisabled =
                fun (model': Model) ->
                    not <| allDecisionsMade paths model'

            dialogPopup "Resolve import conflicts" popupBody "OK" buttonAction isDisabled [] dispatch

/// Display top menu.
let getInfoButton (name:string) (project:Project) : ReactElement =
    let comp =
        project.LoadedComponents
        |> List.find (fun ldc -> ldc.Name = name)

    match comp.Description with
    |Some discr ->
        div 
            [
                HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline} {Tooltip.IsInfo} {Tooltip.IsTooltipRight}"
                Tooltip.dataTooltip discr
                Style [FontSize "20px"; MarginTop "0px"; MarginRight "10px"; Float FloatOptions.Left]] 
            [str "\U0001F6C8"]
    | None ->
        null

type LockState = Locked | Unlocked

let invertSheetLockState = function | Locked -> Unlocked | Unlocked -> Locked

let sheetIsLocked sheet model =
    let project = Option.get  model.CurrentProj
    let ldc = List.find (fun ldc -> ldc.Name = sheet) project.LoadedComponents 
    match ldc.Form with
    | Some ProtectedTopLevel |Some ProtectedSubSheet -> true
    | _ -> false

    

let changeLockState (isSubSheet: bool) (name: string) ((updateLock: LockState -> LockState))  (model: Model) (dispatch : Msg -> unit) =
    let project = Option.get  model.CurrentProj
    let ldc = List.find (fun ldc -> ldc.Name = name) project.LoadedComponents 
    let lockState =
        match ldc.Form with
        | Some ProtectedTopLevel |Some ProtectedSubSheet -> Some Locked
        | Some User -> Some Unlocked
        | _ -> None
    let lockUnlock updateLock form =
        match Option.map updateLock lockState with
        | Some Locked ->
            if isSubSheet then Some ProtectedSubSheet else Some ProtectedTopLevel
        | Some Unlocked-> Some User
        | None -> form
    let ldc' = {ldc with Form = (lockUnlock updateLock ldc.Form)}
    let updatedLdcs = updateLdCompsWithCompOpt (Some ldc') project.LoadedComponents
    let p' = {project with LoadedComponents = updatedLdcs}
    let cs = ldc'.CanvasState
    let sheetInfo = {Form = ldc'.Form; Description = ldc'.Description} 
    let savedState = cs, getSavedWave model,(Some sheetInfo)
    saveStateToFile project.ProjectPath name savedState
    |> displayAlertOnError dispatch
    printf "Changing lock state of %s (was '%A') to '%A'" name lockState (Option.map updateLock lockState)
    dispatch <| SetProject p'

let getLockButton (name:string) (isSubSheet:bool) (project:Project) (model:Model) dispatch : ReactElement =
    match JSHelpers.debugLevel <> 0 with
    |true -> 
        let ldc = List.find (fun ldc -> ldc.Name = name) project.LoadedComponents 
        let buttonText =
            match ldc.Form with
            |Some ProtectedTopLevel |Some ProtectedSubSheet -> "Unlock"
            |_ -> "Lock"
        Level.item []
            [ Button.button
                [ 
                    Button.Size IsSmall
                    Button.IsOutlined
                    Button.Color IsPrimary
                    Button.OnClick(fun _ -> changeLockState
                                                isSubSheet
                                                name
                                                invertSheetLockState
                                                model
                                                dispatch

                        )] [ str buttonText ] 
            ]
    |false -> null 

let addVerticalScrollBars r =
    // dealwith case where Canvas does not exist
    let (el :Browser.Types.HTMLElement option) = unbox (Browser.Dom.document.getElementById "Canvas")
    match el with
    | None -> r
    | Some el -> 
        let height =el.offsetHeight - 100.0
        [div 
            [Style 
                [
                    MaxHeight height; 
                    OverflowY OverflowOptions.Auto
                    OverflowX OverflowOptions.Clip
                ]
            ] 
            r]


let viewTopMenu model dispatch =
    let compIds = getComponentIds model

    //printfn "FileView"
    let style = Style [ Width "100%" ; BorderBottom "2px solid lightgray"] //leftSectionWidth model
    let styleNoBorder = Style [Width "100%"]
    let projectPath, fileName =
        match model.CurrentProj with
        | None -> "no open project", "no open sheet"
        | Some project -> project.ProjectPath, project.OpenFileName

    let makeFileLine isSubSheet name project model =
        let nameProps = 
            if isSubSheet then 
                [] else  
                [Props [Style [FontWeight "bold"]]]
        Navbar.Item.div [ Navbar.Item.Props [ styleNoBorder  ] ]
            [ Level.level [ Level.Level.Props [ styleNoBorder ]]
                  [ Level.left nameProps [ Level.item [] [ str name] ]
                    Level.right [ Props [ Style [ MarginLeft "20px" ] ] ]
                        [ 
                          (getInfoButton name project)
                          Level.item []
                              [ Button.button
                                  [ Button.Size IsSmall
                                    Button.IsOutlined
                                    Button.Color IsPrimary
                                    Button.Disabled(name = project.OpenFileName)
                                    Button.OnClick(fun _ ->
                                        dispatch (StartUICmd ChangeSheet)
                                        printfn "Starting UI Cmd"
                                        dispatch <| ExecFuncInMessage(
                                            (fun model dispatch -> 
                                                let p = Option.get model.CurrentProj
                                                openFileInProject name p model dispatch), dispatch)) ] [ str "open" ] 
                          ]
                          // Add option to rename?
                          Level.item [] [
                              Button.button [
                                  Button.Size IsSmall
                                  Button.IsOutlined
                                  Button.Color IsInfo
                                  Button.OnClick(fun _ ->
                                      dispatch (StartUICmd RenameSheet)
                                      renameFileInProject name project model dispatch) ] [ str "rename" ]
                          ]
                          Level.item []
                              [ Button.button
                                  [ Button.Size IsSmall
                                    Button.IsOutlined
                                    Button.Color IsDanger
                                    Button.OnClick (fun _ -> deleteFileConfirmationPopup name model dispatch) ]
                                    [ str "delete" ] ] 
                          (getLockButton name isSubSheet project model dispatch)] ] ]

    let fileTab model =
        match model.CurrentProj with
        | None -> Navbar.Item.div [] []
        | Some project ->

            let sTrees = getSheetTrees project

            let allRoots = allRootSheets sTrees
            let isSubSheet sh = not <| Set.contains sh allRoots
            let openSheetAction  (sheet:SheetTree) dispatch =
                printfn "Trying to open %s with %A" sheet.SheetName sheet.SheetNamePath
                dispatch (StartUICmd ChangeSheet)
                printfn "Starting UI Cmd"
                dispatch <| ExecFuncInMessage(
                    (fun model dispatch -> 
                        let p = Option.get model.CurrentProj
                        openFileInProject (sheet.SheetName) p model dispatch), dispatch)

            let sheetColor (sheet:SheetTree) =
                match sheet.SheetName = project.OpenFileName, sheetIsLocked sheet.SheetName model with
                | true, true -> IColor.IsCustomColor "pink"
                | true, false -> IColor.IsCustomColor "lightslategrey"
                | false, true -> IColor.IsDanger
                | false, false -> IColor.IsCustomColor "darkslategrey"

            let breadcrumbConfig =  {
                Breadcrumbs.Constants.defaultConfig with
                    ClickAction = openSheetAction
                    ColorFun = sheetColor
                    BreadcrumbIdPrefix = "SheetMenuBreadcrumb"
                }

            let breadcrumbs = [
                    div [Style [TextAlign TextAlignOptions.Center; FontSize "15px"]] [str "Sheets with Design Hierarchy"]
                    Breadcrumbs.allRootHierarchiesFromProjectBreadcrumbs breadcrumbConfig dispatch model
                    ]

            let projectFiles = 
                project.LoadedComponents 
                |> List.filter (fun comp -> 
                    match JSHelpers.debugLevel <> 0 with
                    |true -> (comp.Form = Some User || comp.Form = Some ProtectedTopLevel || comp.Form = Some ProtectedSubSheet)
                    |false -> (comp.Form = Some User)
                )
                |> List.map (fun comp -> 
                    let tree = sTrees[comp.Name]
                    makeFileLine (isSubSheet tree.SheetName) comp.Name project model , tree)
                |> List.sortBy (fun (line,tree) -> isSubSheet tree.SheetName, tree.SheetName, -tree.Size, tree.SheetName.ToLower())
                |> List.map fst
                |> addVerticalScrollBars
            Navbar.Item.div
                [ Navbar.Item.HasDropdown
                  Navbar.Item.Props
                      [ OnClick(fun _ ->
                          //printSheetNames model
                          printfn "OnClick - inverting TopMenuOpenState when current state is: %A" model.TopMenuOpenState
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
                                      DisplayOptions.None) 
                                ] ]
                      ]
                          ([ Navbar.Item.a [ Navbar.Item.Props 
                                [ OnClick(fun _ -> 
                                    dispatch (StartUICmd AddSheet)
                                    addFileToProject model dispatch) ] ]
                                     [ str "New Sheet" ]
                             Navbar.divider [] []
                             Navbar.Item.a [ Navbar.Item.Props 
                                [ OnClick(fun _ -> 
                                    dispatch (StartUICmd ImportSheet)
                                    importSheet model dispatch) ] ]
                                     [ str "Import Sheet" ]
                             Navbar.divider [] []
                             ]
                           @ breadcrumbs)]
                       

    div [   HTMLAttr.Id "TopMenu"
            leftSectionWidth model
            Style [ Position PositionOptions.Absolute
                    UserSelect UserSelectOptions.None

                    ]
        ]
        [ Navbar.navbar
            [ Navbar.Props
                [  Style
                    [ Height "100%"
                      Width "100%" 
                      BorderBottom "2px solid lightgray"] ] ]
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
                                [ Navbar.Item.a [ Navbar.Item.Props [ OnClick <| doActionWithSaveFileDialog "New project" (ExecFuncInMessage(newProject,dispatch)) model dispatch ] ]
                                      [ str "New project" ]
                                  Navbar.Item.a [ Navbar.Item.Props [ OnClick <| doActionWithSaveFileDialog "Open project" (ExecFuncInMessage(openProject,dispatch)) model dispatch ] ]
                                      [ str "Open project" ]
                                  Navbar.Item.a [ Navbar.Item.Props [ OnClick <| doActionWithSaveFileDialog "Close project" (ExecFuncInMessage(forceCloseProject,dispatch)) model dispatch ] ]
                                      [ str "Close project" ] ] ]

                      fileTab model
                      Navbar.Item.div []
                          [ Navbar.Item.div []
                                [ Breadcrumb.breadcrumb [ Breadcrumb.HasArrowSeparator ]
                                      [ Breadcrumb.item [] [ str <| cropToLength 30 false projectPath ]
                                        Breadcrumb.item [] [ span [ Style [ FontWeight "bold" ] ] [ str fileName ] ] ] ] ]
                      Navbar.Item.div []
                          [ Navbar.Item.div []
                                [ Button.button
                                    ((if model.SavedSheetIsOutOfDate  then 
                                        []
                                       else
                                        [ Button.Color IsLight ]) @
                                    [
                                      Button.Color IsSuccess  
                                      
                                      Button.OnClick(fun _ -> 
                                        dispatch (StartUICmd SaveSheet)
                                        saveOpenFileActionWithModelUpdate model dispatch |> ignore
                                        dispatch <| Sheet(SheetT.DoNothing) //To update the savedsheetisoutofdate send a sheet message
                                        ) ]) [ str "Save" ] ] ]
                      Navbar.Item.div []
                          [ Navbar.Item.div []
                                [ Button.button 
                                    [ Button.OnClick(fun _ -> PopupHelpers.viewInfoPopup dispatch) 
                                      Button.Color IsInfo
                                    ] 
                                    [ str "Info" ] 
                                  
                                ]
                            ]
                      Navbar.Item.div []
                          (if model.UISheetTrail = [] then
                                []
                          else
                                [ Navbar.Item.div []
                                    [ Button.button 
                                        [   Button.OnClick(fun _ -> dispatch <| SheetBackAction dispatch) 
                                            Button.Color IsSuccess
                                        ] 
                                        [ str "Back" ] 
                                  
                                    ]
                                ])
                      Navbar.End.div [] [                               
                            div [                                    
                                    Style [
                                        Color "blue"
                                        Display DisplayOptions.Flex;
                                        Width "200px";                                    
                                        AlignItems AlignItemsOptions.Center;
                                        TextAlign TextAlignOptions.Center]]
                                [getHintPaneElement model]
                                        
                            // add space padding on RH of navbar to improve top bar formatting
                            // this is a bit of a hack - but much easier than matching styles                                 
                            Text.div [Props [Style [PaddingRight "7000px"]]] [str ""]
                        ] ] ]]
