(*
    FileMenuView.fs

    View for the top menu, and related functionalities: renamimg, loadimg, saving, deleting sheets
*)

module TopMenuView
open System
open EEExtensions

open Fable.React
open Fable.React.Props

open Helpers
open JSHelpers
open DiagramStyle
open ModelType
open ModelHelpers
open CommonTypes
open FilesIO
open CanvasExtractor
open Notifications
open PopupHelpers
open DrawModelType
open Sheet.SheetInterface
open Optics
open Optics.Operators
open MenuHelpers
open MiscMenuView

open Fulma.Extensions.Wikiki

open Fulma
open Fulma.Color



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


let createEmptyComponentAndFile (pPath:string)  (sheetName: string): LoadedComponent =
    createEmptyDgmFile pPath sheetName |> ignore
    {
        Name=sheetName
        LoadedComponentIsOutOfDate = false
        WaveInfo = None
        TimeStamp = DateTime.Now
        FilePath= pathJoin [|pPath; sprintf "%s.dgm" sheetName|]
        CanvasState=([],[])
        InputLabels = []
        OutputLabels = []
        Form = Some User
        Description = None
    }
    
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
                        LoadedComponentIsOutOfDate = false
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





/// load demo project into Issie executables
let loadDemoProject model dispatch basename =
    warnAppWidth dispatch (fun _ ->
        let isMac = Node.Api.``process``.platform = Node.Base.Darwin
        let homeDir = if isMac then pathJoin [|FilesIO.staticDir(); ".."; ".."|] else "."

        let newDir = homeDir + "/demos/" + basename
        let sourceDir = FilesIO.staticDir() + "/demos/" + basename
        printf "%s" $"loading demo {sourceDir} into {newDir}"

        ensureDirectory (homeDir + "/demos/")
        ensureDirectory newDir

        readFilesFromDirectory newDir
        |> List.iter (fun path -> unlink <| pathJoin[|newDir; path|])

        dispatch EndSimulation // End any running simulation.
        dispatch <| TruthTableMsg CloseTruthTable // Close any open Truth Table.
        dispatch EndWaveSim

        //let projectFile = baseName newDir + ".dprj"
        //writeFile (pathJoin [| newDir; projectFile |]) ""
        //|> displayAlertOnError dispatch

        let files = readFilesFromDirectory sourceDir

        let isNotDir path =
            hasExtn ".dgm" path || hasExtn ".txt" path || hasExtn ".ram" path

        // copy over files from source path to new path
        files
        |> List.filter isNotDir
        |> List.iter (fun basename ->
            let newPath = pathJoin [|newDir; basename|]
            copyFile (pathJoin [|sourceDir; basename|]) newPath)

        openDemoProjectFromPath newDir model dispatch
        
    )



/// show menu for choosing demo project
let showDemoProjects model dispatch (demosInfo : (string * int * int) list) =
    match model.CurrentProj with
    | Some proj -> ()
    | None ->
    
        let menuItem demoInfo action =
            let basename =
                match demoInfo with
                | (basename, _, _) ->
                    let basenameL = String.length basename
                    basename[1..basenameL-1]
       
            Menu.Item.li
                [ Menu.Item.IsActive false
                  Menu.Item.OnClick action ] [
                  div [] [
                        p [Style [FontWeight "bold"]] [str basename]
                        br []
                        match basename with
                        | "cpu" ->
                            div [] [
                                str "The EEP1 CPU architecture designed in Year 1 labs"
                            ]
                        | "fulladder" ->
                            div [] [
                                str "Full adder circuit built from 2 half adders"
                            ]
                        | "registerFile" ->
                            div [] [
                                str "regx16x8 file from EEP1 demo using wire labels to simplify wiring"
                            ]
                        | "adder (4-bit)" ->
                            div [] [
                                str "Cascading 1-bit full adders to create 4-bit adder"
                            ]
                        | "eratosthenes" ->
                            div [] [
                                str "The EEP1 CPU running a program to calculate prime numbers"
                            ]
                        | _ -> str "Information about other design"

                        //br []
                      
                        //div [] [
                        //    str "Components: "
                        //    str (string <| componentsCount)
                        //    str " Sheets: "
                        //    str (string <| sheetsCount)
                        //]
                  ]

               ]
    
        let demosContent =
            fun (model' : Model) ->
                if List.isEmpty demosInfo then
                    [
                    div [] [
                        str "The directory that is supposed to contain the demos doesn't exist!"
                    ]
                    ]
                else
                    demosInfo
                    |> List.map(fun (path, componentsCount, sheetsCount) ->
                                menuItem (path, componentsCount, sheetsCount)
                                    (fun _ -> loadDemoProject model' dispatch path))
        
        
        let demosList =
            fun (model' : Model) ->
                div [] [
                p [Style [FontWeight "bold"]] [str "Note: Reloading the demo deletes all changes made to it."]
                br []
                Menu.menu []
                    [ Menu.list []
                          (demosContent model)
                    ]
            ]

        let foot =
            fun (model' : Model) ->
                div [] []

        dynamicClosablePopup "Choose Demo Project" demosList foot [] dispatch

/// Display the initial Open/Create Project menu at the beginning if no project
/// is open.
let viewNoProjectMenu model dispatch =
    let menuItem label action =
        Menu.Item.li
            [ Menu.Item.IsActive false
              Menu.Item.OnClick action ] [ str label ]

    let demos = FilesIO.staticDir()
    let demos = demos + "/demos"

    let demoProjects =
        readFilesFromDirectory demos

    let demosInfo =
        demoProjects
        |> List.map(fun basename ->
            (basename, 0, 0))

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
                  ([ menuItem "New project" (fun _ -> dispatch <| FileCommand(FileNewProject false, dispatch))
                     menuItem "Open project" (fun _ -> dispatch <| FileCommand(FileOpenProject false, dispatch))
                     menuItem "Open demo project" (fun _ -> dispatch <| FileCommand(FileShowDemos demosInfo, dispatch))]
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
    let ldcOp = List.tryFind (fun ldc -> ldc.Name = sheet) project.LoadedComponents

    let ldc = 
        ldcOp
        |>
        Option.defaultValue (List.find (fun ldc -> ldc.Name = project.OpenFileName) project.LoadedComponents)

    match ldc.Form with
    | Some ProtectedTopLevel |Some ProtectedSubSheet -> true
    | _ -> false

    
/// Change model to alter lock of sheet as determined by updateLock.
/// Unlockable sheets are kept the same.
/// isSubSheet must be true only if sheet is a root of the design hierarchy.
let changeLockState (isSubSheet: bool) (sheet: SheetTree) (updateLock: LockState -> LockState) =
    let lockState = function
        | ProtectedTopLevel | ProtectedSubSheet ->
            Some Locked
        | User -> Some Unlocked
        | _ -> None
    let formUpdate form =
        match Option.map updateLock (lockState form) with
        | Some Unlocked -> User
        | Some Locked when isSubSheet -> ProtectedSubSheet
        | Some Locked -> ProtectedTopLevel
        | None -> form
    Optic.map (projectOpt_ >?> loadedComponentOf_ sheet.SheetName >?> formOpt_) (Option.map formUpdate)
    >> Optic.set (projectOpt_ >?> loadedComponentOf_ sheet.SheetName >?> loadedComponentIsOutOfDate_) true
    >> Optic.set savedSheetIsOutOfDate_ true
    

/// Change model to alter lock of tree with root sheet as determined by updateLock.
/// Unlockable sheets are kept the same.
/// isSubSheet must be true only if sheet is a root of the design hierarchy.
let changeSubtreeLockState (isSubSheet: bool) (sheet: SheetTree) (updateLock: LockState -> LockState) =
    foldOverTree isSubSheet (fun b sheet -> changeLockState b sheet updateLock) sheet



let addVerticalScrollBars (el: Browser.Types.HTMLElement option) r =
    // dealwith case where Canvas does not exist
    match el with
    | None -> r
    | Some el ->
        let height = el.offsetHeight - 50.0
        let width = el.offsetWidth - 50.0
        //printf "%s" $"Height={height}, width={width}"

        [div 
            [Style 
                [
                    MaxHeight height;
                    MaxWidth width;
                    OverflowY OverflowOptions.Auto
                    OverflowX OverflowOptions.Auto
                ]
            ] 
            r]


let viewTopMenu model dispatch =
    let compIds = getComponentIds model
    // Used for geometry to keep app reasonably responsive
    let (el:Browser.Types.HTMLElement option) = unbox (Browser.Dom.document.getElementById "Canvas")
    let numPathChars =
        match el with
        | None -> Constants.maxNumPathChars
        | Some el ->
            if el.offsetWidth > Constants.largeScreenCanvasWidth then
                Constants.maxNumPathChars
            else
                Constants.minNumPathChars

    //printfn "FileView"
    let style = Style [ Width "100%" ; BorderBottom "2px solid lightgray"] //leftSectionWidth model
    let styleNoBorder = Style [Width "100%"]
    let projectPath, fileName =
        match model.CurrentProj with
        | None -> "no open project", "no open sheet"
        | Some project -> project.ProjectPath, project.OpenFileName



    let fileTab model =
        match model.CurrentProj with
        | None -> Navbar.Item.div [] []
        | Some project ->
            let updatedProject = getUpdatedLoadedComponents project model
            let updatedModel = {model with CurrentProj = Some updatedProject}

            let sTrees = getSheetTrees false updatedProject

            let allRoots = allRootSheets sTrees
            let isSubSheet sh = not <| Set.contains sh allRoots
            let openSheetAction  (sheet:SheetTree) dispatch =
                //printfn "Trying to open %s with %A" sheet.SheetName sheet.SheetNamePath
                dispatch (StartUICmd ChangeSheet)
                //printfn "Starting UI Cmd"
                dispatch <| ExecFuncInMessage(
                    (fun model dispatch -> 
                        let p = Option.get model.CurrentProj
                        openFileInProject (sheet.SheetName) p model dispatch), dispatch)

            let sheetColor (sheet:SheetTree) =
                match sheet.SheetName = project.OpenFileName, sheetIsLocked sheet.SheetName updatedModel with
                | true, true -> IColor.IsCustomColor "pink"
                | true, false -> IColor.IsInfo
                | false, true -> IColor.IsDanger
                | false, false -> IColor.IsCustomColor "darkslategrey"

            let breadcrumbConfig =  {
                MiscMenuView.Constants.defaultConfig with
                    ClickAction = openSheetAction
                    ColorFun = sheetColor
                    BreadcrumbIdPrefix = "SheetMenuBreadcrumb"
                }

            let breadcrumbs = [
                    div [Style [TextAlign TextAlignOptions.Center; FontSize "15px"]] [str "Sheets with Design Hierarchy"]
                    MiscMenuView.allRootHierarchiesFromProjectBreadcrumbs breadcrumbConfig dispatch updatedModel
                    ]
            let topMenuOpenState = model.TopMenuOpenState
            Navbar.Item.div
                [ Navbar.Item.HasDropdown
                  Navbar.Item.Props
                      [ OnClick(fun _ ->
                          //printSheetNames model
                          //printfn "OnClick - inverting TopMenuOpenState when current state is: %A" model.TopMenuOpenState
                          if topMenuOpenState = Files then Closed else Files
                          |> SetTopMenu
                          |> dispatch) ] ]
                [ Navbar.Link.a [] [ str "Sheets" ]
                  Navbar.Dropdown.div
                      [ Navbar.Dropdown.Props
                          [ Style
                              [ Display
                                  (if (let b = topMenuOpenState = Files
                                       b) then
                                      DisplayOptions.Block
                                   else
                                      DisplayOptions.None)
                                ] ]
                      ]
                          ([ Navbar.Item.a [ Navbar.Item.Props 
                                [ OnClick(fun _ -> 
                                    dispatch (StartUICmd AddSheet)
                                    dispatch <| FileCommand(FileAddFile,dispatch)) ]]
                                    [ str "New Sheet" ]
                             Navbar.divider [] []
                             Navbar.Item.a [ Navbar.Item.Props 
                                [ OnClick(fun _ -> 
                                    dispatch (StartUICmd ImportSheet)
                                    dispatch <| FileCommand(FileImportSheet,dispatch)) ] ]
                                     [ str "Import Sheet" ]
                             Navbar.divider [] []
                             ]
                           @ breadcrumbs
                           |> addVerticalScrollBars el)]
                       

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
                      BorderBottom "2px solid lightgray"]
                   ] ]
            [ Navbar.Brand.div
                  [ Props
                      [ Style
                          [ Height "100%"
                            Width "100%" ] ] ]
                    [
                      // Sheets menu
                      fileTab model
                      let topMenuOpenState = model.TopMenuOpenState
                      // Projects menu
                      Navbar.Item.div
                        [ Navbar.Item.HasDropdown
                          Navbar.Item.Props
                              [ OnClick(fun _ ->
                                  if topMenuOpenState = Project then Closed else Project
                                  |> SetTopMenu
                                  |> dispatch) ] ]
                          [ Navbar.Link.a [] [ str "Project" ]
                            Navbar.Dropdown.div
                                [ Navbar.Dropdown.Props
                                    [ Style
                                        [ Display
                                            (if topMenuOpenState = Project then
                                                DisplayOptions.Block
                                             else
                                                 DisplayOptions.None) ] ] ]
                                [ Navbar.Item.a [ Navbar.Item.Props [ OnClick <| fun _ -> dispatch <| FileCommand (FileNewProject true, dispatch)] ]
                                      [ str "New project" ]
                                  Navbar.Item.a [ Navbar.Item.Props [ OnClick <| fun _ -> dispatch <| FileCommand (FileOpenProject true, dispatch)] ]
                                      [ str "Open project" ]
                                  Navbar.Item.a [ Navbar.Item.Props [ OnClick <| fun _ -> dispatch <| FileCommand (FileCloseProject, dispatch) ] ]
                                      [ str "Close project" ] ] ]

                      // make the path in the navbar responsive
                      let hidePath = numPathChars < Constants.numCharsHidePath
                      let pathItem = Breadcrumb.item [] [ str <| if hidePath then "" else cropToLength numPathChars false projectPath]
                      let nameItem = Breadcrumb.item [] [ span [ Style [ FontWeight "bold" ] ] [ str fileName ] ]
                      let tip = $"{projectPath}:{fileName}"
                      Navbar.Item.div []
                          [ Navbar.Item.div []
                             [ div [
                                    HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline} {Tooltip.IsTooltipBottom}"
                                    Tooltip.dataTooltip tip
                                ]
                             [ Breadcrumb.breadcrumb
                                   [ Breadcrumb.HasArrowSeparator ]
                                   (if hidePath then [nameItem] else [pathItem ; nameItem])]]]                                     
                                        
                      Navbar.Item.div []
                          [ Navbar.Item.div []
                                [ Fulma.Button.button
                                    ((if model.SavedSheetIsOutOfDate  then 
                                        []
                                      else
                                        [ Fulma.Button.Color IsLight ]) @
                                    [
                                      Fulma.Button.Color IsSuccess  
                                      
                                      Fulma.Button.OnClick(fun _ -> 
                                        dispatch (StartUICmd SaveSheet)
                                        dispatch <| FileCommand(FileSaveOpenFile,dispatch)
                                        dispatch <| Sheet(SheetT.DoNothing) //To update the savedsheetisoutofdate send a sheet message
                                        ) ]) [ str "Save" ] ] ]
                      Navbar.Item.div []
                          [ Navbar.Item.div []
                                [ Fulma.Button.button 
                                    [ Fulma.Button.OnClick(fun _ -> UIPopups.viewInfoPopup dispatch) 
                                      Fulma.Button.Color IsInfo
                                    ] 
                                    [ str "Info" ] 
                                  
                                ]
                            ]
                      Navbar.Item.div []
                          (if model.UISheetTrail = [] then
                                []
                          else
                                [ Navbar.Item.div []
                                    [ Fulma.Button.button 
                                        [   Fulma.Button.OnClick(fun _ -> dispatch <| SheetBackAction dispatch) 
                                            Fulma.Button.Color IsSuccess
                                        ] 
                                        [ str "Back" ] 
                                  
                                    ]
                                ])
                      Navbar.End.div [] [                               
                            div [                                    
                                    Style [
                                        Color "#5282d1"
                                        Display DisplayOptions.Flex;
                                        Width "200px";                                    
                                        AlignItems AlignItemsOptions.Center;
                                        TextAlign TextAlignOptions.Center]]
                                [div [Style [FontSize "90%"]] (getHintPaneElement model)]
                                        
                            // add space padding on RH of navbar to improve top bar formatting
                            // this is a bit of a hack - but much easier than matching styles                                 
                            Text.div [Props [Style [PaddingRight "7000px"]]] [str ""]
                        ] ]]]
