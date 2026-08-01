module MiscMenuView
open Fable.React
open Fable.React.Props
open Fulma
open Fable.React
open Fable.React.Props
open System
open EEExtensions
open CommonTypes
open ModelType
open ModelHelpers
open MenuHelpers
open CanvasExtractor
open FilesIO
open PopupHelpers
open DrawModelType
open Optics



(****************************************************************************************************
*****************************************************************************************************
                   Miscellaneous Code to perform Top Menu based UI functions.
                   **********************************************************
*****************************************************************************************************
*****************************************************************************************************)

type BreadcrumbConfig = {
    AllowDuplicateSheets: bool
    BreadcrumbIdPrefix: string
    BreadcrumbText: (SheetTree -> string) option
    ColorFun: SheetTree -> IColor
    ClickAction: SheetTree -> (Msg -> unit) -> unit
    ElementProps: IHTMLProp list
    ElementStyleProps: CSSProp list
    /// button options (other than OnClick and Color)
    ButtonOptions: Button.Option list 
    NoWaves: SheetTree -> int
    }

module Constants =
    let gridBoxSeparation = "5px"

    let colArrayStyle = Style [
                BorderColor "white";
                BorderWidth "10px";
                BorderStyle "solid";
                Padding "50px"]

    let defaultConfig: BreadcrumbConfig = {
        AllowDuplicateSheets = false
        BreadcrumbIdPrefix = "BreadcrumbDefault"
        BreadcrumbText = None
        ColorFun = fun _ -> IColor.IsGreyDark
        ClickAction = fun _ _ -> ()
        ElementProps = [ ]
        ElementStyleProps = [           
            Border "2px"            
            BorderColor "LightGrey"
            BorderRightColor "DarkGrey"
            BorderStyle "Solid"
            Background "LightGrey"
            Padding "5px"]
        ButtonOptions = [
                Button.Size IsSmall
                Button.IsOutlined
                Button.IsExpanded
                Button.IsFocused true
                Button.Disabled false
                ]
        NoWaves = (fun _ -> 0)
    }

//--------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------//
//-----------------------------BREADCRUMB DISPLAY OF SHEET HIERARCHY--------------------------//
//--------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------//

let gridBox (gap:string) s =
    div [Style [Display DisplayOptions.InlineGrid; GridGap gap; JustifyContent "Start"]] s


let rec gridArea (gridPos: CSSGridPos): string =
    match gridPos with
    | PosElement(a,b) ->
        PosAreaSpan(a,b,1,1) |> gridArea
    | PosAreaAbsolute(xStart,yStart,xEnd,yEnd) ->
        PosAreaSpan(xStart,yStart, xEnd - xStart + 1, yEnd - yStart + 1) |> gridArea
    | PosAreaSpan(startX,startY,spanX,spanY) ->
        $"{startY} / {startX} / span {spanY} / span {spanX}"


/// a Grid item centre justified and occupying given area
let gridElement reactElementId props styleProps (pos: CSSGridPos) (x: ReactElement) =
    div ( props @ [
        Id reactElementId
        Style (styleProps @ [
            Display DisplayOptions.Flex
            FlexDirection "column"
            TextAlign TextAlignOptions.Center
            JustifySelf JustifySelfOptions.Center;
            //AlignSelf AlignSelfOptions.Center;
            JustifyContent "center"
            Width "100%"
            Height "100%"
            GridArea (gridArea pos) ])]) [x]

let positionDesignHierarchyInGrid (rootSheet: string) (trees: Map<string,SheetTree>) =
    let tree = trees[rootSheet]
    let maxDepth = trees[rootSheet].Depth

    let rec getTreeHeight (tree: SheetTree) =
        match tree.SubSheets with
        | [] -> 1
        | subs ->
            subs
            |> List.sumBy getTreeHeight

    let rec getSheetPositions (startX: int) (startY: int) (tree: SheetTree) =
        let height = getTreeHeight tree
        match tree.SubSheets with
        | [] -> [PosAreaSpan(startX, startY, 1, height), tree]
        | others ->
            ((0,[]), others)
            ||> List.fold (fun (offset,posL') subSheet ->
                let offset' = offset + getTreeHeight subSheet
                let posL'' =
                    posL' @ getSheetPositions (startX+1) (startY+offset) subSheet
                offset', posL'')
            |> snd
            |> List.append [PosAreaSpan(startX, startY, 1, height), tree]
    getSheetPositions 1 1 tree

let positionRootAndFocusChildrenInGrid (root: string) (pathToFocus:string list) (trees: Map<string,SheetTree>) =
    let tree = trees[root] // tree from root
    let sheetsInPath =
        [0..pathToFocus.Length-1]
        |> List.map (fun i -> pathToFocus[0..i])
        |> List.map (fun path -> Option.get <| tree.lookupPath path)
    let children =
        (List.last sheetsInPath).SubSheets
        |> List.sortBy (fun sheet -> sheet.BreadcrumbName)
    children
    |> List.mapi (fun i sheet -> PosAreaSpan(sheetsInPath.Length + 1, 1, 1, children.Length),sheet)
      
let makeGridFromSheetsWithPositions
        (cfg: BreadcrumbConfig)
        (dispatch: Msg -> unit)
        (posL: (CSSGridPos*SheetTree) list)
            : ReactElement =
    posL
    |> List.map (fun (pos, sheet) ->
            let crumbId = cfg.BreadcrumbIdPrefix + ":" + sheet.SheetName + ":" + String.concat ":" sheet.LabelPath
            let extraStyle = match sheet.SubSheets with | [] -> [BackgroundColor "white"; BorderWidth "0px"] | _ -> cfg.ElementStyleProps
            let number = cfg.NoWaves sheet
            gridElement
                crumbId
                cfg.ElementProps
                (extraStyle)
                pos
                (Button.button [
                    Button.Props [Id crumbId]
                    Button.Color (cfg.ColorFun sheet)
                    Button.Modifiers [Modifier.TextColor IColor.IsLight]
                    Button.OnClick(fun ev -> cfg.ClickAction sheet dispatch)
                    ] [
                        let name =
                            match cfg.BreadcrumbText, cfg.AllowDuplicateSheets, sheet.LabelPath with
                            | Some f,_,_ -> f sheet
                            | _, true, [] -> sheet.SheetName
                            | _, true, path -> path[path.Length - 1]
                            | _, false, _ -> sheet.SheetName
                        str $"{name}"
                        if number > 0 then
                            div [
                                Style [
                                    Position PositionOptions.Absolute
                                    CSSProp.Bottom "0px"
                                    CSSProp.Right "0px"
                                    BackgroundColor "transparent"  // Remove white background
                                    Width "20px"  // Increased size to fit text
                                    Height "20px"
                                    Display DisplayOptions.Flex
                                    AlignItems AlignItemsOptions.Center
                                    JustifyContent "center"
                                    FontSize "10px"  // Increased font size
                                    Color "white"  // Ensure text is visible
                                    CSSProp.Overflow OverflowOptions.Visible // Prevents text from being clipped
                                ]
                            ] [str $"{number}"]
                        else
                            null
                    ]))             

    |> gridBox Constants.gridBoxSeparation 
    
/// display as a ReactElement the breadcrumbs.
/// clickAction - what happens when a given breadcrumb is clicked.
/// project - the model project.
let makeBreadcrumbsFromPositions
        (sheetTreeMap: Map<string,SheetTree>)
        (cfg: BreadcrumbConfig)
        (positionSheetsInGrid: Map<string,SheetTree> -> (CSSGridPos*SheetTree) list)
        (dispatch: Msg -> unit)
             : ReactElement =
        sheetTreeMap
        |> positionSheetsInGrid
        |> makeGridFromSheetsWithPositions cfg dispatch

/// Breadcrumbs of entire simulated design hierarchy.
/// Display as a ReactElement the breadcrumbs.
/// ClickAction - what happens when a given breadcrumb (labelled by its path to root) is clicked.
let hierarchyBreadcrumbs
        (cfg: BreadcrumbConfig)
        (dispatch: Msg -> unit)
        (model: Model) =
    mapOverProject (div [] []) model (fun p ->
        let root = Option.defaultValue p.OpenFileName model.WaveSimSheet
        let sheetTreeMap = getSheetTrees cfg.AllowDuplicateSheets p
        makeBreadcrumbsFromPositions sheetTreeMap cfg (positionDesignHierarchyInGrid root) dispatch)



/// Breadcrumbs of entire design hierarchy from given sheet
/// Display as a ReactElement the breadcrumbs.
/// rootSheet - root of hierrarchy displayed
/// clickAction - what happens when a given breadcrumb (labelled by its path to root) is clicked.
let hierarchyFromSheetBreadcrumbs
        (rootSheet: string)
        (cfg: BreadcrumbConfig)
        (dispatch: Msg -> unit)
        (model: Model) =
    mapOverProject (div [] []) model (fun p ->
        let sheetTreeMap = getSheetTrees cfg.AllowDuplicateSheets p
        makeBreadcrumbsFromPositions sheetTreeMap cfg (positionDesignHierarchyInGrid rootSheet) dispatch)

/// Breadcrumbs of entire design hierarchy of every root sheet in project
/// Display as a ReactElement the breadcrumbs.
/// rootSheet - root of hierrarchy displayed
/// clickAction - what happens when a given breadcrumb (labelled by its path to root) is clicked.
let allRootHierarchiesFromProjectBreadcrumbs
        (cfg: BreadcrumbConfig)
        (dispatch: Msg -> unit)
        (model: Model) =
    mapOverProject ([div [] []]) model (fun p ->
        let sheetTreeMap = getSheetTrees cfg.AllowDuplicateSheets p
        allRootSheets sheetTreeMap
        |> Set.toList
        |> List.map (fun root ->
            makeBreadcrumbsFromPositions sheetTreeMap cfg (positionDesignHierarchyInGrid root) dispatch))
        |> List.mapi (fun i el ->
            tr [ Constants.colArrayStyle
                ] [ td [CellSpacing "50px"] [el]])
        |> fun rows -> table [] [tbody [] rows]

/// is there a duplicate sheet name anywhere in hierarchy?
let hierarchiesHaveDuplicates (model: Model) =
    mapOverProject false model (fun p ->
        getSheetTrees true p
        |> Map.toList
        |> List.map (fun (_,sheet) ->
            let sheetNames =
                sheet.SubSheets
                |> List.map (fun sheet -> sheet.SheetName)
            sheetNames.Length = (List.distinct sheetNames).Length)
        |> List.exists id)


/// Breadcrumbs of the focus sheet, with sheets on its path to root, and its children.
/// Provides navigation while occupying small vertical area. Untested.
/// *** TODO: hookthis function to simulation and test ***
/// Should be called extracting inputs from waveform simulation.
/// Display as a ReactElement the breadcrumbs.
/// clickAction - what happens when a given breadcrumb (labelled by its path to root) is clicked.
/// project - the model project
let smallSimulationBreadcrumbs
        (rootName: string)
        (pathToFocus: string list)
        (cfg: BreadcrumbConfig)
        (dispatch: Msg -> unit)
        (model: Model)
             : ReactElement =
    mapOverProject (div [] []) model (fun p ->       
        makeBreadcrumbsFromPositions (getSheetTrees cfg.AllowDuplicateSheets p) cfg (positionRootAndFocusChildrenInGrid rootName pathToFocus) dispatch)

//--------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------//
//----------------------------------SHEET IMPORT & UTILITY FUNCTIONS--------------------------//
//--------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------//


/// return a react warning message if name if not valid for a sheet Add or Rename, or else None
let maybeWarning dialogText project =
    let redText txt = Some <| div [ Style [ Color "red" ] ] [ str txt ]
    if isFileInProject dialogText project then
        redText "This sheet already exists."
    elif (ComponentLibraries.reservedPrefixOf project.LoadedComponents dialogText).IsSome then
        // the prefix belongs to a library this project uses: a component of that library added
        // later is named into it, and would otherwise have nowhere to go
        let prefix = (ComponentLibraries.reservedPrefixOf project.LoadedComponents dialogText).Value
        redText $"Sheet names starting {prefix} belong to a component library used by this project."
    elif dialogText.StartsWith " " || dialogText.EndsWith " " then
        redText "The sheet name cannot start or end with a space."
    elif String.exists ((=) '.') dialogText then
        redText "The sheet name cannot contain a file suffix."
    elif not <| String.forall (fun c -> Char.IsLetterOrDigitOrUnderscore c || c = ' ') dialogText then
        redText "The sheet name must contain only letters, digits, spaces or underscores"
    elif ((dialogText |> Seq.tryItem 0) |> Option.map Char.IsDigit) = Some true then
        redText "The name must not start with a digit"
    else None


/// Find all sheets that depend on the given sheet in the current project
let getDependentsFromSheet (model:Model) (sheetName : string) =

    mapOverProject None model <| fun p ->
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

         Some instances

/// given paths from source directory and destination directory of a sheet that needs importing, if it exists in destination directory,
/// return its dependents wrapped in 'Some'. Otherwise return 'None'
let getDependents(model : Model) (oldSheetPath : string) (newSheetPath : string) =

    // get sheets in current project that would depend on an existent sheet, same as one that's being imported
    
    if newSheetPath |> exists then
        let sheetName = baseNameWithoutExtension oldSheetPath // could use newSheetPath as well here            

        match getDependentsFromSheet model sheetName with
        | None -> Some ""
        | Some instances ->
            instances
            |> List.map (fun (sheet,_,_) -> sheet)
            |> List.distinct
            |> String.concat ","
            |> Some

    else None

let Button sheetPath (buttonDecision : ImportDecision option) label isDisabled model dispatch =
    // a button has been pressed if the decision made about this sheet matched the decisions assosiated with this button
    let hasBeenPressed =
        fun (model : Model) ->
            let valueOption = Map.tryFind sheetPath (getImportDecisions model.PopupDialogData)

            match valueOption with
            | Some decision' -> decision' = buttonDecision
            | None -> false

    let updateDecisions (sheetPath: string) (decisionOption: ImportDecision option) (model' : Model) _ =
        let updatedDecisions = Map.add sheetPath decisionOption (getImportDecisions model'.PopupDialogData)

        dispatch <| UpdateImportDecisions updatedDecisions

    [ Button.button
        [ 
            Button.Size IsSmall
            Button.IsOutlined
            Button.Color IsPrimary
            Button.Disabled isDisabled
            Button.IsFocused <| hasBeenPressed model
            Button.OnClick(fun _ -> dispatch <| ExecFuncInMessage(updateDecisions sheetPath buttonDecision,dispatch))
            ] [ str label ]             
    ]

/// return text that should display in 'Action' column of import sheets popup
let getDecisionText path model sheetIsDependency decisionNeeded =

    let getDecision =
        fun (model : Model) ->
            Map.tryFind path (getImportDecisions model.PopupDialogData)

    match getDecision model with
    | Some decision ->
        match decision with
        | Some Overwrite -> if sheetIsDependency then p [] [str "Import"] else p [Style [Constants.blueColor]] [str "Overwrite"]
        | Some Rename -> p [Style [Constants.blueColor]] [str "Rename"]
        | None -> p [Style [Constants.redColor]] [str "Ignore"]
    | None ->
        if (sheetIsDependency || decisionNeeded) then p [Style [Constants.redColor]] [str "Ignore"] else p [] [str "Import"]

/// return react element for 2 buttons that are side by side
let twoButtons leftButton rightButton =
    Level.level []
            [        
                Level.item []
                    leftButton
                                  
                Level.item []
                    rightButton

            ]

/// prepare information that should display on a row for a dependency of a sheet
let dependencyRow sheetPath model dispatch dependencyPath =                           

    match hasExtn ".dgm" dependencyPath with
    | true ->
                            
        tr [] [
            td [Style [Constants.boldStyle]] [str <| baseNameWithoutExtension dependencyPath]
            td [] [ str "Dependency of "
                    strong [] [str <| baseNameWithoutExtension sheetPath]
                    str "."
            ]
            td [] [
                    (Button dependencyPath None "Ignore" false model dispatch)
                    |> twoButtons (Button dependencyPath (Some Overwrite) "Import" false model dispatch) 
            ]
            td [] [
                getDecisionText dependencyPath model true false
            ]
        ]

    | false ->

        tr [] [
            td [Style [Constants.boldStyle]] [str <| baseNameWithoutExtension dependencyPath]
            td [] [ str "Dependency of "
                    strong [] [str <| baseNameWithoutExtension sheetPath]
                    str "."
                    p [Style [Constants.redColor]] [str "Doesn't exist in source and destination directories."]
            ]
            td [] [str "N/A"]
            td [] [p [Style [Constants.redColor]] [str "Ignore"]]
        ]

/// for each imported sheet, create layout for its row in the popup table, as well as rows for all its dependencies.
let createSheetInfo model projectDir dispatch ((sheetPath, dependencies): string * Set<string>) : ReactElement array =

    let newSheetPath = pathJoin [|projectDir; baseName sheetPath|]

    let hasDependencies = Set.count dependencies <> 0

    let sheetExists, depSheets =
        match getDependents model sheetPath newSheetPath with
        | Some depSheets -> true, depSheets
        | None -> false, ""

    if projectDir = dirName sheetPath then

        [|
        tr [] [
            td [Style [Constants.boldStyle]] [str <| baseNameWithoutExtension sheetPath]
            td [] [str "Cannot be imported as it is from the current directory"]
            td [] [str "N/A"]
            td [] [str "N/A"]
        ]
        |]
            
    else

        match sheetExists with
        | true ->

            match tryLoadComponentFromPath sheetPath with
            | Error err ->

                [|
                tr [] [
                    td [Style [Constants.boldStyle]] [str <| baseNameWithoutExtension sheetPath]
                    td [] [str err]
                    td [] []
                    td [] [str "Ignore"]
                ]
                |]

            | Ok ldcSource ->

                let sourceSig = parseDiagramSignature ldcSource.CanvasState

                let destSig =
                    tryGetLoadedComponents model
                    |> List.find (fun ldc -> ldc.Name = baseNameWithoutExtension sheetPath)
                    |> (fun ldc -> parseDiagramSignature ldc.CanvasState)

                let overwriteDisabled = (sourceSig <> destSig) && (depSheets <> "")

                let sheetRow = 
                    [|
                    tr [] [
                        td [Style [Constants.boldStyle]] [str <| baseNameWithoutExtension sheetPath]
                        td [] [
                            str "Sheet already exists in destination directory. "
                            br []
                            match hasDependencies with
                            | true ->
                                str "Sheet has dependencies."

                            | false -> str ""
                            br []
                            match (sourceSig <> destSig) with
                            | true ->
                                if (depSheets <> "") then
                                    p [Style [Constants.redColor]] [
                                        str "Overwrite disabled because sheets contain different hardware. Danger of conflicts in dependents."
                                    ]
                                else
                                    p [Style [Constants.greenColor]] [
                                        str "Sheets contain different hardware, but overwrite allowed as there are no dependents."
                                    ]

                            | false ->
                                str ""
                        ]
                        td [] [

                            (Button sheetPath (Some Rename) "Rename" false model dispatch)
                            |> twoButtons (Button sheetPath (Some Overwrite) "Overwrite" overwriteDisabled model dispatch)
                        ]
                        td [] [

                            getDecisionText sheetPath model false true
                        ]
                    ]
                    |]

                let dependencyRows =
                    dependencies
                    |> Set.toArray
                    |> Array.map (fun dependency ->
                        dependencyRow sheetPath model dispatch dependency 
                    )

                Array.append sheetRow dependencyRows
                            
               
        | false ->
            if fileNameIsBad ((pathWithoutExtension << baseName) sheetPath)
            then

                [|
                tr [] [
                    td [Style [Constants.boldStyle]] [str <| baseNameWithoutExtension sheetPath]
                    td [] [str "Cannot be imported because it contains incorrect characters. "]
                    td [] []
                    td [] [str "Ignore"]
                ]
                |]
            else

                let sheetRow =

                    [|
                    tr [] [
                        td [Style [Constants.boldStyle]] [str <| baseNameWithoutExtension sheetPath]
                        td [] [
                                
                            match hasDependencies with
                            | true ->
                                str "Sheet will be imported, but has dependencies."
      
                            | false ->
                                str "Sheet will be imported without conflicts"
                        ]
                        td [] []
                        td [] [
                            getDecisionText sheetPath model false false
                        ]
                                
                    ]
                    |]

                let dependencyRows =
                    dependencies
                    |> Set.toArray
                    |> Array.map (fun dependency ->
                        dependencyRow sheetPath model dispatch dependency 
                    )

                Array.append sheetRow dependencyRows

/// Returns with a list of each path with its dependencies. This includes dependencies of dependencies
let pathsWithDependencies destProjectDir paths sourceProjectDir =
    paths
    |> List.map (fun path ->

        /// for each path, search for its dependencies, as well as dependencies of dependencies.
        let rec search (path : string) (deps : string list) =
                        
            match tryLoadComponentFromPath path with
            | Error err ->
                match (exists <| pathJoin [|destProjectDir; baseName path|]) with
                | true -> deps
                | false ->
                    (baseNameWithoutExtension path :: deps)  // dependency doesn't exist in either directory
  
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
                            let dependencyPath = pathJoin [|sourceProjectDir; ct.Name + ".dgm"|]

                            dependencyPath

                        | _ -> ""
                    )
                    |> List.distinct
                    |> List.filter (fun s -> s <> "")
                                
                match List.length customCompsPaths with
                | 0 -> deps
                | _ ->
                    customCompsPaths
                    |> List.collect (fun dependencyPath ->
                        match (exists <| pathJoin [|destProjectDir; baseName dependencyPath|]) || (List.contains dependencyPath paths) with
                        | true -> search dependencyPath deps
                        | false ->
                            if exists dependencyPath then search dependencyPath (dependencyPath :: deps)
                            else search dependencyPath (baseNameWithoutExtension dependencyPath :: deps) // dependency doesn't exist in source and dest directories
                    )

        let dependencies =
            search path []
            |> Set.ofList

        (path, dependencies)
    )

/// Copy the sheet file at oldPath into the current project under a name typed by the user.
/// nameOf turns the dialog text into the new sheet name: Import prefixes it, Duplicate uses it whole.
/// notes are extra lines displayed above the preview of the new name.
/// afterCopy is given the new sheet name and the current model, and decides how the project is reloaded.
/// Example: nameOf = (fun s -> s + "_adder"), text "old" -> sheet "old_adder" in file old_adder.dgm.
let copySheetIntoProjectPopup title placeholder buttonText notes nameOf oldPath afterCopy model dispatch =
    match model.CurrentProj with
    | None -> JSHelpers.log "Warning: copySheetIntoProjectPopup called when no project is currently open"
    | Some project ->
        // sheet names are always lower case, so validation and the preview use the name that will be created
        let newNameOf (dialogData: PopupDialogData) = nameOf ((getText dialogData).ToLower())

        let before =
            fun (dialogData: PopupDialogData) ->
                let newName = newNameOf dialogData
                div []
                    (notes @
                        [ str <| sprintf "New name: %s" newName
                          Option.defaultValue (div [] []) (maybeWarning newName project) ])

        let body = dialogPopupBodyOnlyText before placeholder dispatch

        let buttonAction =
            fun (model': Model) ->
                let newName = newNameOf model'.PopupDialogData
                let newPath = pathJoin [|project.ProjectPath; newName + ".dgm"|]
                // fresh ids: a copied sheet must not share ids with the sheet it came from
                copySheetWithNewIds oldPath newPath
                afterCopy newName model'
                dispatch ClosePopup

        let isDisabled =
            fun (model': Model) ->
                let newName = newNameOf model'.PopupDialogData
                (getText model'.PopupDialogData = "") || fileNameIsBad newName || maybeWarning newName project <> None

        dialogPopup title body buttonText buttonAction isDisabled [] dispatch

/// When sheet selected for import is from current directory,
/// show popup asking user to rename the file
let renameSheetBeforeImportPopup oldPath model dispatch =
    let notes =
        [ str <| sprintf "Warning: Sheet %s is from current directory." (baseName oldPath)
          br []
          br [] ]
    let nameOf dialogText = dialogText + "_" + baseNameWithoutExtension oldPath
    let afterCopy _ model' = openProjectFromPath (dirName oldPath) model' dispatch
    copySheetIntoProjectPopup "Duplicate sheet " "Prefix for design sheet" "Rename" notes nameOf oldPath afterCopy model dispatch

/// Duplicate the named sheet of the current project under a new name typed by the user.
/// If that sheet is the open one it is saved to disk first when dirty, since the copy is made
/// from the file rather than from the canvas.
let duplicateSheet (sheetName: string) model dispatch =
    match model.CurrentProj with
    | None -> JSHelpers.log "Current project must be open for a sheet to be duplicated"
    | Some project ->
        // the new sheet is loaded from the copied file and added to the project, leaving other sheets alone
        let afterCopy newName (model': Model) =
            let newPath = pathJoin [|project.ProjectPath; newName + ".dgm"|]
            match model'.CurrentProj, tryLoadComponentFromPath newPath with
            | Some proj, Ok ldc ->
                let proj' = {proj with LoadedComponents = ldc :: proj.LoadedComponents}
                openFileInProject' false newName proj' model' dispatch
            | _, Error err -> displayFileErrorNotification err dispatch
            | _ -> ()

        let advice =
            div []
                [ str "Duplicating a sheet is only necessary if you intend to implement similar but \
                       different versions of the sheet. If you want copies of sheet hardware you can add \
                       the sheet multiple times as a component from this Project in the Catalog." ]

        match getFileInProject sheetName project with
        | Some ({Form = Some User} as sheet) ->
            let notes = [ str <| sprintf "Sheet '%s' will be duplicated as:" sheet.Name; br [] ]
            let nameDialog () =
                copySheetIntoProjectPopup "Duplicate sheet" "New name for duplicated sheet" "Duplicate"
                                          notes id sheet.FilePath afterCopy model dispatch
            // saving here rather than in the dialog action keeps the copy in step with what is on screen
            if sheetName = project.OpenFileName && model.SavedSheetIsOutOfDate then
                match saveOpenFileToModel model with
                | Some {CurrentProj = Some p} -> dispatch <| SetProject p
                | _ -> ()
            choicePopup "Duplicate sheet" advice "Continue" "Cancel"
                (fun isContinue _ ->
                    dispatch ClosePopup
                    match isContinue with
                    | true -> nameDialog ()
                    | false -> ())
                dispatch
        | _ ->
            displayFileErrorNotification $"Sheet '{sheetName}' cannot be duplicated." dispatch

let importSheetPopup destProjectDir paths sourceProjectDir dispatch =

    /// Return only sheets that exist / don't exist in the destination directory based on boolean. True -> return existent. False -> return non-existent.
    let filterSheets (existing : bool) =
        paths
        |> List.filter (fun sheetPath ->
                   
            let newSheetPath = pathJoin [|destProjectDir; baseName sheetPath|]

            if not <| fileNameIsBad (baseNameWithoutExtension <| baseName sheetPath) then
                if existing then exists <| newSheetPath else not (exists <| newSheetPath)
            else
                false
                   
        )

    /// Check if a decision has been made about all sheets on import popup that require decision to be made
    let allDecisionsMade allSheets =
        fun (model : Model) ->
            match filterSheets true with
            | [] -> true
            | sheets ->
                sheets
                |> List.forall (fun sheetPath -> Map.containsKey sheetPath (getImportDecisions model.PopupDialogData))

    let headCell heading =  th [ ] [ str heading ]

    let popupBody =
        fun (model' : Model) ->
            let content =
                pathsWithDependencies destProjectDir paths sourceProjectDir
                |> List.map (createSheetInfo model' destProjectDir dispatch)
                |> List.toArray

            div [] [
                Table.table [] [
                        thead [] [ tr [] (List.map headCell ["Sheet" ;"Information"; "Decision"; "Action"]) ]
                        tbody [] ( Array.concat content )
                       
                ]
            ]

    /// look through decisions made about each sheet. Overwrite or Rename sheet based on decision made
    let buttonAction =
        fun (model' : Model) ->
                   
            let newSheetPaths = 
                getImportDecisions model'.PopupDialogData
                |> Map.toList
                |> List.map (fun (sheetPath, decision) ->
                    match decision with
                    | Some Overwrite  ->
                        sheetPath, pathJoin [|destProjectDir; baseName sheetPath|]

                    | Some Rename ->
                        sheetPath, pathJoin [|destProjectDir; baseNameWithoutExtension sheetPath + "_Copy" + ".dgm"|]

                    | None -> sheetPath, ""

                )

            filterSheets false
            |> List.iter (fun oldSheetPath ->
                let newSheetPath = pathJoin [|destProjectDir; baseName oldSheetPath|]

                copySheetWithNewIds oldSheetPath newSheetPath

            )

            newSheetPaths
            |> List.iter (fun (oldSheetPath, newSheetPath) ->
                        match newSheetPath with
                        | "" -> ()
                        | path -> copySheetWithNewIds oldSheetPath path )

            JSHelpers.log destProjectDir

            if dirName destProjectDir = "demos" then
                openDemoProjectFromPath destProjectDir model' dispatch
            else
                openProjectFromPath destProjectDir model' dispatch

            dispatch ClosePopup
            dispatch FinishUICmd

    let isDisabled =
        fun (model': Model) ->
            not <| allDecisionsMade paths model'

    dialogPopup "Resolve import conflicts" popupBody "OK" buttonAction isDisabled [] dispatch

/// Import sheet from directory, ask user to sort out dependency issues
let importSheet model dispatch =
    match model.CurrentProj with
    | None -> JSHelpers.log "Current project must be open for sheet to be imported to it"
    | Some project -> 
        let projectDir = project.ProjectPath

        dispatch <| (Sheet (SheetT.SetSpinner false))

        match askForExistingSheetPaths model.UserData.LastUsedDirectory with
        | None -> () // User gave no path.
        | Some paths ->
            let sourceProjectDir = dirName paths[0]

            // handle if sheets from current directory
            paths
            |> List.iter (fun path ->
                match projectDir = sourceProjectDir with
                | true ->
                    renameSheetBeforeImportPopup path model dispatch
                | false -> 
                    match saveOpenFileToModel model with
                      | Some {CurrentProj = Some p} ->
                        dispatch <| SetProject p
                      | _ -> ()
                    importSheetPopup projectDir paths sourceProjectDir dispatch
        )

