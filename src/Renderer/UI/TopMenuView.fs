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
        // regenerating the file must not clear a previously chosen top-sheet flag
        let wasTop =
            project.LoadedComponents
            |> List.tryFind (fun lc -> lc.Name = name)
            |> Option.map (fun lc -> lc.IsTopSheet)
        let sheetInfo: SheetInfo = {Form=Some (Verilog name);Description=None; ParameterDefinitions=None; IsTopSheet = wasTop} //only user defined sheets are editable and thus saveable
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
        let (SheetInfo:SheetInfo option) =
            match origLdComp.Form with
            |None -> None
            |Some form -> Some {Form=Some form;Description=origLdComp.Description; ParameterDefinitions=origLdComp.LCParameterSlots; IsTopSheet = Some origLdComp.IsTopSheet}
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
        LCParameterSlots = None
        IsTopSheet = false
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
                        LCParameterSlots = None
                        IsTopSheet = false
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
        // The working copy goes in the user's own directory. It used to go beside the
        // installation - "." on Windows and Linux, next to the bundle on macOS - which is not
        // reliably writable: see FilesIO.userDataDirectory.
        let sourceDir = pathJoin [| FilesIO.staticDir (); "demos"; basename |]
        match FilesIO.tryUserDemosDirectory () |> Result.bind (fun d -> tryEnsureDirectory (pathJoin [| d; basename |])) with
        | Error msg ->
            displayFileErrorNotification $"The demo could not be opened: {msg}" dispatch
        | Ok newDir ->
        printf "%s" $"loading demo {sourceDir} into {newDir}"

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


//-------------------------------------------------------------------------------------------------//
//-------------------------------------EDIT AND VIEW MENUS-----------------------------------------//
//-------------------------------------------------------------------------------------------------//

// These hold what used to be on the Electron Edit and View menus. Everything reachable only from
// those menus has to have a home here before they can be deleted, since an Electron menu is the
// only thing registering their keyboard accelerators and both go at once.
//
// Every item shows its own shortcut, read from KeyTypes rather than written out here, so the menu
// documents the keys and cannot drift from what actually fires.

/// global. because an opened module here shadows the Node namespace
let private isMacPlatform = global.Node.Api.``process``.platform = global.Node.Base.Darwin

/// A dropdown item labelled with the keyboard shortcut that also invokes it.
let private itemWithKey (label: string) (id: KeyTypes.ShortcutId) (action: unit -> unit) =
    Navbar.Item.a
        [ Navbar.Item.Props
            [ OnClick(fun _ -> action ())
              Style [ Display DisplayOptions.Flex; JustifyContent "space-between" ] ] ]
        [ str label
          span
              [ Style [ MarginLeft "2.5em"; Opacity "0.5"; FontSize "90%"; WhiteSpace WhiteSpaceOptions.Nowrap ] ]
              [ str (KeyTypes.idLabel isMacPlatform id) ] ]

/// A non-clickable heading grouping the items under it. Bulma's navbar has no nested dropdown, so
/// what were submenus on the Electron menus are flat groups here.
let private itemGroupHeading (label: string) =
    Navbar.Item.div
        [ Navbar.Item.Props
            [ Style [ FontSize "85%"; Opacity "0.55"; PaddingBottom "0"; Cursor "default" ] ] ]
        [ str label ]

/// The dropdown, shown only when this is the open one.
let private dropdown (isOpen: bool) (items: ReactElement list) =
    Navbar.Dropdown.div
        [ Navbar.Dropdown.Props
            [ Style [ Display(if isOpen then DisplayOptions.Block else DisplayOptions.None) ] ] ]
        items

/// A top-level entry on the renderer menu bar which toggles its own dropdown.
let private topMenuEntry (name: string) (thisMenu: TopMenu) (openState: TopMenu) dispatch items =
    Navbar.Item.div
        [ Navbar.Item.HasDropdown
          Navbar.Item.Props
              [ OnClick(fun _ ->
                  (if openState = thisMenu then Closed else thisMenu) |> SetTopMenu |> dispatch) ] ]
        [ Navbar.Link.a [] [ str name ]
          dropdown (openState = thisMenu) items ]

let private editMenuItems dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let keyDispatch = SheetT.KeyPress >> sheetDispatch
    let wireOf (f: ComponentId list -> BusWireT.Model -> BusWireT.Model) =
        dispatch
        <| UpdateModel(fun m -> m |> Optic.map (sheet_ >-> SheetT.wire_) (f m.Sheet.SelectedComponents))

    [ itemWithKey "Undo" KeyTypes.ScUndo (fun () -> keyDispatch SheetT.KeyboardMsg.CtrlZ)
      itemWithKey "Redo" KeyTypes.ScRedo (fun () -> keyDispatch SheetT.KeyboardMsg.CtrlY)
      Navbar.divider [] []
      itemWithKey "Select all" KeyTypes.ScSelectAll (fun () -> keyDispatch SheetT.KeyboardMsg.CtrlA)
      itemWithKey "Copy" KeyTypes.ScCopy (fun () -> keyDispatch SheetT.KeyboardMsg.CtrlC)
      itemWithKey "Paste" KeyTypes.ScPaste (fun () -> keyDispatch SheetT.KeyboardMsg.CtrlV)
      itemWithKey "Delete" KeyTypes.ScDelete (fun () -> keyDispatch SheetT.KeyboardMsg.DEL)
      Navbar.divider [] []
      itemWithKey "Rotate clockwise" KeyTypes.ScRotateClockwise (fun () ->
          sheetDispatch (SheetT.Rotate CommonTypes.Degree90))
      itemWithKey "Rotate anticlockwise" KeyTypes.ScRotateAnticlockwise (fun () ->
          sheetDispatch (SheetT.Rotate CommonTypes.Degree270))
      itemWithKey "Flip vertically" KeyTypes.ScFlipVertical (fun () ->
          sheetDispatch (SheetT.Flip SymbolT.FlipVertical))
      itemWithKey "Flip horizontally" KeyTypes.ScFlipHorizontal (fun () ->
          sheetDispatch (SheetT.Flip SymbolT.FlipHorizontal))
      itemWithKey "Align" KeyTypes.ScAlign (fun () ->
          sheetDispatch (SheetT.Arrangement SheetT.AlignSymbols))
      itemWithKey "Distribute" KeyTypes.ScDistribute (fun () ->
          sheetDispatch (SheetT.Arrangement SheetT.DistributeSymbols))
      itemWithKey "Rotate label" KeyTypes.ScRotateLabel (fun () -> sheetDispatch SheetT.RotateLabels)
      Navbar.divider [] []
      itemWithKey "Separate wires from selection" KeyTypes.ScSeparateWires (fun () ->
          wireOf BusWireSeparate.reSeparateWiresFrom)
      itemWithKey "Reroute wires from selection" KeyTypes.ScRerouteWires (fun () ->
          wireOf BusWireSeparate.reRouteWiresFrom)
      Navbar.divider [] []
      itemWithKey "How to move component ports" KeyTypes.ScMovePortsHelp (fun () ->
          dispatch
          <| ShowStaticInfoPopup(
              "How to move component ports",
              SymbolPortHelpers.moveCustomPortsPopup (),
              dispatch)) ]

let private viewMenuItems dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let keyDispatch = SheetT.KeyPress >> sheetDispatch
    let busWireDispatch (bMsg: BusWireT.Msg) = sheetDispatch (SheetT.Msg.Wire bMsg)
    let symbolDispatch msg = busWireDispatch (BusWireT.Msg.Symbol msg)
    let setTheme theme =
        dispatch <| SetThemeUserData theme
        symbolDispatch (SymbolT.Msg.SetTheme theme)
    // The Electron roles these replace acted on the *zoom level*, stepping by 0.5, so match that
    // rather than scaling a factor - otherwise the steps feel different from what users had.
    let stepAppZoom delta =
        let wc = JSHelpers.electronRemote.getCurrentWebContents ()
        wc.setZoomLevel (max -9.0 (min 9.0 (wc.getZoomLevel () + delta)))

    [ itemWithKey "Enter/exit fullscreen" KeyTypes.ScFullScreen (fun () ->
          let w = JSHelpers.electronRemote.getCurrentWindow ()
          w.setFullScreen (not (w.isFullScreen ())))
      Navbar.divider [] []
      itemWithKey "Zoom application in" KeyTypes.ScAppZoomIn (fun () -> stepAppZoom 0.5)
      itemWithKey "Zoom application out" KeyTypes.ScAppZoomOut (fun () -> stepAppZoom -0.5)
      itemWithKey "Zoom application reset" KeyTypes.ScAppZoomReset (fun () ->
          JSHelpers.electronRemote.getCurrentWebContents().setZoomLevel 0.0)
      Navbar.divider [] []
      itemWithKey "Zoom diagram in" KeyTypes.ScDiagramZoomIn (fun () ->
          keyDispatch SheetT.KeyboardMsg.ZoomIn)
      itemWithKey "Zoom diagram out" KeyTypes.ScDiagramZoomOut (fun () ->
          keyDispatch SheetT.KeyboardMsg.ZoomOut)
      itemWithKey "Zoom diagram to fit" KeyTypes.ScDiagramZoomToFit (fun () ->
          keyDispatch SheetT.KeyboardMsg.CtrlW)
      Navbar.divider [] []
      itemWithKey "Show/hide grid" KeyTypes.ScToggleGrid (fun () -> sheetDispatch SheetT.Msg.ToggleGrid)
      itemWithKey "Show/hide wire arrows" KeyTypes.ScToggleWireArrows (fun () ->
          busWireDispatch BusWireT.Msg.ToggleArrowDisplay)
      Navbar.divider [] []
      itemGroupHeading "Wire style"
      itemWithKey "Jump" KeyTypes.ScWireTypeJump (fun () ->
          sheetDispatch (SheetT.WireType SheetT.WireTypeMsg.Jump))
      itemWithKey "Radiussed" KeyTypes.ScWireTypeRadiussed (fun () ->
          sheetDispatch (SheetT.WireType SheetT.WireTypeMsg.Radiussed))
      itemWithKey "Modern" KeyTypes.ScWireTypeModern (fun () ->
          sheetDispatch (SheetT.WireType SheetT.WireTypeMsg.Modern))
      Navbar.divider [] []
      itemGroupHeading "Theme"
      itemWithKey "Colourful" KeyTypes.ScThemeDefault (fun () -> setTheme SymbolT.ThemeType.Colourful)
      itemWithKey "Light" KeyTypes.ScThemeLight (fun () -> setTheme SymbolT.ThemeType.Light)
      itemWithKey "Grayscale" KeyTypes.ScThemeGrayscale (fun () -> setTheme SymbolT.ThemeType.White)
      Navbar.divider [] []
      itemWithKey "Show/hide build tab" KeyTypes.ScToggleBuildTab (fun () ->
          dispatch ChangeBuildTabVisibility)
      itemWithKey "Show/hide app memory display" KeyTypes.ScToggleMemoryDisplay (fun () ->
          JSHelpers.loggingMemory <- not JSHelpers.loggingMemory
          printfn $"""Memory display is now {if JSHelpers.loggingMemory then "on" else "off"}.""") ]


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

            // Library sheets are not part of the design the user navigates: they are the innards
            // of a catalogue component, added and removed with its instances, and cannot be
            // opened. Everything else - parameter analysis, width inference, simulation - still
            // sees them, because they are ordinary sheets. A library author does need to see them,
            // so the developer menu can turn them back on.
            let sTrees = getSheetTreesFiltered model.ShowLibrarySheets false updatedProject

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

            // The top sheet exists to settle which values a sheet is drawn at when its instances
            // disagree, so it is surfaced only when some sheet actually does. Testing instead for
            // the mere presence of a parameter turned the whole apparatus on for a design with one
            // instance of one parameterised sheet, where there is nothing to settle - and turned it
            // on for a library component too, whose sheet is never displayed at all.
            let topSheet =
                match ParameterAnalysis.projectHasAmbiguousDisplay updatedProject.LoadedComponents with
                | true -> ParameterAnalysis.effectiveTopSheet updatedProject.LoadedComponents
                | false -> None
            /// sheets in the instance tree under the top: computed from the sheet trees already
            /// built for this menu, not by a second walk
            let sheetsUnderTop =
                let rec namesIn (tree: SheetTree) =
                    tree.SheetName :: List.collect namesIn tree.SubSheets
                topSheet
                |> Option.bind (fun top -> Map.tryFind top sTrees)
                |> Option.map (namesIn >> Set.ofList)
                |> Option.defaultValue Set.empty

            let sheetColor (sheet:SheetTree) =
                let isTop = topSheet = Some sheet.SheetName
                let outsideTop = topSheet.IsSome && not (Set.contains sheet.SheetName sheetsUnderTop)
                match sheet.SheetName = project.OpenFileName, sheetIsLocked sheet.SheetName updatedModel with
                | true, true -> IColor.IsCustomColor "pink"
                | true, false when isTop -> IColor.IsSuccess
                | true, false -> IColor.IsInfo
                | false, true -> IColor.IsDanger
                | false, false when isTop -> IColor.IsCustomColor "darkgreen"
                // outside the top's tree: necessarily displayed at default parameter values
                | false, false when outsideTop -> IColor.IsCustomColor "grey"
                | false, false -> IColor.IsCustomColor "darkslategrey"

            let breadcrumbConfig =  {
                MiscMenuView.Constants.defaultConfig with
                    ClickAction = openSheetAction
                    ColorFun = sheetColor
                    ShowLibrarySheets = model.ShowLibrarySheets
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
                          |> dispatch)
                        // Bulma positions .navbar-dropdown against this item, which would start
                        // the sheet tree wherever the "Sheets" label happens to sit. Taking the
                        // item out of the positioning chain hands that job to the nearest
                        // positioned ancestor - #TopMenu, whose left edge is the app's left edge.
                        Style [ Position PositionOptions.Static ] ] ]
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
                                // left of #TopMenu rather than of the "Sheets" item. Bulma also
                                // sets min-width to 100% of the same box, which is now the whole
                                // bar rather than one item - far wider than the tree needs, so it
                                // is dropped and the width left to the content and to the
                                // addVerticalScrollBars cap below.
                                CSSProp.Left "0"      // CSSProp. because Edge.Left is also in scope
                                CSSProp.MinWidth "0"
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
                    // no BorderBottom: what is directly below - the canvas, and the right pane's
                    // tab body - already draws a 2px BorderTop, and with the bar and the canvas
                    // now flush the two abutted into one 4px line
                    [ Height "100%"
                      Width "100%" ]
                   ] ]
            [ Navbar.Brand.div
                  [ Props
                      [ Style
                          [ Height "100%"
                            Width "100%" ] ] ]
                    [
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
                                      [ str "Close project" ]
                                  // whole-project operations, homeless once the Electron Sheet
                                  // menu goes. They act on the project, not the open sheet, so
                                  // they belong here rather than on Sheets - whose dropdown has
                                  // the sheet tree below it and no room to spare.
                                  Navbar.divider [] []
                                  // Writing Verilog is per-sheet, so it lives on the sheet's own
                                  // pill in the Sheets menu rather than here, where it could only
                                  // ever have meant whichever sheet happened to be open.
                                  itemWithKey "Save project in new format" KeyTypes.ScSaveProjectNewFormat
                                      (fun () -> dispatch <| MenuAction(MenuSaveProjectInNewFormat, dispatch)) ] ]

                      // Sheets menu. Second on the bar, but its dropdown is still pinned to the
                      // left edge of the app - see fileTab, which holds the sheet tree and wants
                      // every pixel of width it can get.
                      fileTab model

                      topMenuEntry "Edit" Edit topMenuOpenState dispatch (editMenuItems dispatch)
                      topMenuEntry "View" View topMenuOpenState dispatch (viewMenuItems dispatch)

                      // make the path in the navbar responsive
                      let hidePath = numPathChars < Constants.numCharsHidePath
                      // the id is what the right-click handler matches on to offer the path menu:
                      // the text here is cropped to fit the bar, so the menu reads the real path
                      // from the model rather than from what is displayed.
                      let pathItem =
                          Breadcrumb.item
                              [ Breadcrumb.Item.Props [ Id "ProjectPath" ] ]
                              [ str <| if hidePath then "" else cropToLength numPathChars false projectPath]
                      let nameItem = Breadcrumb.item [] [ span [ Style [ FontWeight "bold" ] ] [ str fileName ] ]
                      let tip = $"{projectPath}:{fileName}"
                      // one Navbar.Item.div each, not two: a nested pair renders two navbar-items,
                      // and each one adds 0.75rem of padding a side. The inner ones cost 24px
                      // apiece and did nothing else.
                      Navbar.Item.div []
                             [ div [
                                    HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline} {Tooltip.IsTooltipBottom}"
                                    Tooltip.dataTooltip tip
                                ]
                             [ Breadcrumb.breadcrumb
                                   [ Breadcrumb.HasArrowSeparator ]
                                   (if hidePath then [nameItem] else [pathItem ; nameItem])]]

                      Navbar.Item.div []
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
                                        ) ]) [ str "Save" ] ]
                      Navbar.Item.div []
                                [ Fulma.Button.button
                                    [ Fulma.Button.OnClick(fun _ -> UIPopups.viewInfoPopup dispatch)
                                      Fulma.Button.Color IsInfo
                                    ]
                                    [ str "Info" ]
                                ]
                      Navbar.Item.div []
                          (if model.UISheetTrail = [] then
                                []
                          else
                                [ Fulma.Button.button
                                    [   Fulma.Button.OnClick(fun _ -> dispatch <| SheetBackAction dispatch)
                                        Fulma.Button.Color IsSuccess
                                    ]
                                    [ str "Back" ]
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
