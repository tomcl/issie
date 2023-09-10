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
open Extractor
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
    ColorFun: SheetTree -> IColor
    ClickAction: SheetTree -> (Msg -> unit) -> unit
    ElementProps: IHTMLProp list
    ElementStyleProps: CSSProp list
    /// button options (other than OnClick and Color)
    ButtonOptions: Button.Option list 
    }

module Constants =
    let gridBoxSeparation = "5px"

    let colArrayStyle = Style [
                BorderColor "white";
                BorderWidth "10px";
                BorderStyle "solid";
                Padding "50px"]

    let defaultConfig = {
        AllowDuplicateSheets = false
        BreadcrumbIdPrefix = "BreadcrumbDefault"
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
                    ] [str $"{sheet.SheetName}" ]))             

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
    elif dialogText.StartsWith " " || dialogText.EndsWith " " then
        redText "The sheet name cannot start or end with a space."
    elif String.exists ((=) '.') dialogText then
        redText "The sheet name cannot contain a file suffix."
    elif not <| String.forall (fun c -> Char.IsLetterOrDigitOrUnderscore c || c = ' ') dialogText then
        redText "The sheet name must contain only letters, digits, spaces or underscores"
    elif ((dialogText |> Seq.tryItem 0) |> Option.map Char.IsDigit) = Some true then
        redText "The name must not start with a digit"
    else None




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
let importSheet model dispatch =
    match model.CurrentProj with
    | None -> JSHelpers.log "Current project must be open for sheet to be imported to it"
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
               
        /// rename file
        let renameSheetBeforeImport oldPath project model dispatch =
            match model.CurrentProj with
            | None -> JSHelpers.log "Warning: renameSheetBeforeImport called when no project is currently open"
            | Some project ->
                // Prepare dialog popup.
                let title = "Duplicate sheet "

                let sheetName = baseName oldPath
                let before =
                    fun (dialogData: PopupDialogData) ->
                        let dialogText = getText dialogData

                        div []
                            [ 
                              str <| sprintf "Warning: Sheet %s is from current directory." sheetName
                              br []
                              br []
                              str <| sprintf "New name: %s" (dialogText + "_" + baseNameWithoutExtension oldPath)
                              Option.defaultValue (div [] []) (maybeWarning (dialogText + "_" + baseNameWithoutExtension oldPath) project)]

                let placeholder = "Prefix for design sheet"
                let body = dialogPopupBodyOnlyText before placeholder dispatch
                let buttonText = "Rename"

                let buttonAction =
                    fun (model': Model) ->
                        // Create empty file.
                        let newName = (getText model'.PopupDialogData).ToLower() + "_" + sheetName
                        let newPath = pathJoin [|dirName oldPath; newName|]
                        // copy the file over with its new name

                        copyFile oldPath newPath
                        openProjectFromPath projectDir model' dispatch
                        dispatch ClosePopup

                let isDisabled =
                    fun (model': Model) ->
                        let dialogData = model'.PopupDialogData
                        let dialogText = getText dialogData
                        (isFileInProject (dialogText + "_" + baseNameWithoutExtension oldPath) project) || (dialogText = "") ||
                        fileNameIsBad (dialogText + "_" + baseNameWithoutExtension oldPath)

                dialogPopup title body buttonText buttonAction isDisabled [] dispatch

        let createSheetInfo (model : Model) ((sheetPath, dependencies): string * Set<string>) : ReactElement array =
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

            let getDecision (sheetPath : string) =
                fun (model : Model) ->
                     Map.tryFind sheetPath (importDecisions model)

            if projectDir = dirName sheetPath then
                // displayFileErrorNotification "Cannot import sheet from curent directory" dispatch

                [|tr [] [
                    td [Style [FontWeight "bold"]] [str <| baseNameWithoutExtension sheetPath]
                    td [] [str "Cannot be imported as it is from the current directory"]
                    td [] [str "N/A"]
                    td [] [str "N/A"]
                ] |]
            
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

                let getDecisionText path model sheetIsDependency decisionNeeded =
                    match getDecision path model with
                    | Some decision ->
                        match decision with
                        | Some Overwrite -> if sheetIsDependency then p [] [str "Import"] else p [Style [Color "blue"]] [str "Overwrite"]
                        | Some Rename -> p [Style [Color "blue"]] [str "Rename"]
                        | None -> p [Style [Color "red"]] [str "Ignore"]
                    | None ->
                        if (sheetIsDependency || decisionNeeded) then p [Style [Color "red"]] [str "Ignore"] else p [] [str "Import"]


                let dependencyReactElement dependencyPath =                           

                    match hasExtn ".dgm" dependencyPath with
                    | true ->
                            
                        tr [] [
                            td [Style [FontWeight "bold"]] [str <| baseNameWithoutExtension dependencyPath]
                            td [] [str "Dependency of "
                                   strong [] [str <| baseNameWithoutExtension sheetPath]
                                   str "."
                            ]
                            td [] [
                                
                                    Level.level []
                                            [        
                                                Level.item []
                                                    (Button dependencyPath (Some Overwrite) "Import" false)
                               
                                                Level.item []
                                                    (Button dependencyPath None "Ignore" false)

                                            ]
                            ]
                            td [] [
                                getDecisionText dependencyPath model true false
                            ]
                        ]
                        


                    | false ->

                        tr [] [
                            td [Style [FontWeight "bold"]] [str <| baseNameWithoutExtension dependencyPath]
                            td [] [str "Dependency of "
                                   strong [] [str <| baseNameWithoutExtension sheetPath]
                                   str "."
                                   p [Style [Color "red"]] [str "Doesn't exist in source and destination directories."]
                            ]
                            td [] [str "N/A"]
                            td [] [p [Style [Color "red"]] [str "Ignore"]]
                        ]

                match sheetExists with
                | true ->

                    match tryLoadComponentFromPath sheetPath with
                    | Error err ->

                        [|tr [] [
                            td [Style [FontWeight "bold"]] [str <| baseNameWithoutExtension sheetPath]
                            td [] [str err]
                            td [] []
                            td [] [str "Ignore"]
                        ]|]
                    | Ok ldcSource ->

                        let sourceSig = parseDiagramSignature ldcSource.CanvasState

                        let destSig =
                            tryGetLoadedComponents model
                            |> List.find (fun ldc -> ldc.Name = baseNameWithoutExtension sheetPath)
                            |> (fun ldc -> parseDiagramSignature ldc.CanvasState)

                        let hardwareDoesNotMatch = (sourceSig <> destSig)

                        let sheetRow = 
                            [|tr [] [
                                td [Style [FontWeight "bold"]] [str <| baseNameWithoutExtension sheetPath]
                                td [] [
                                    str "Sheet already exists in destination directory. "
                                    br []
                                    match hasDependencies with
                                    | true ->
                                        str "Sheet has dependencies."

                                    | false -> str ""
                                    br []
                                    match hardwareDoesNotMatch with
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
                                ]
                                td [] [

                                    Level.level []
                                            [        
                                                    Level.item []
                                                        (Button sheetPath (Some Overwrite) "Overwrite" ((sourceSig <> destSig) && (depSheets <> "")))

                                                    Level.item []
                                                        (Button sheetPath (Some Rename) "Rename" false)

                                            ]
                                ]
                                td [] [

                                    getDecisionText sheetPath model false true
                                ]
                            ]|]

                        let dependencyRows =
                            dependencies
                            |> Set.toArray
                            |> Array.map (fun dependency ->
                                dependencyReactElement dependency 
                            )

                        Array.append sheetRow dependencyRows
                            
               
                | false ->
                    if fileNameIsBad (pathWithoutExtension fileName)
                    then

                        [|tr [] [
                            td [Style [FontWeight "bold"]] [str <| baseNameWithoutExtension sheetPath]
                            td [] [str "Cannot be imported because it contains incorrect characters. "]
                            td [] []
                            td [] [str "Ignore"]
                        ]|]
                    else

                        let sheetRow =

                            [|tr [] [
                                td [Style [FontWeight "bold"]] [str <| baseNameWithoutExtension sheetPath]
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
                                
                            ]|]

                        let dependencyRows =
                            dependencies
                            |> Set.toArray
                            |> Array.map (fun dependency ->
                                dependencyReactElement dependency 
                            )

                        Array.append sheetRow dependencyRows

        match askForExistingSheetPaths model.UserData.LastUsedDirectory with
        | None -> () // User gave no path.
        | Some paths ->
            let sourceProjectPath = dirName paths[0]

            
            // handle if sheets from current directory
            paths
            |> List.iter (fun path ->
                match projectDir = sourceProjectPath with
                | true ->
                    renameSheetBeforeImport path project model dispatch

                | false -> 
                
                    let pathsWithDependencies =
                        paths
                        |> List.map (fun path ->

                            /// Returns with a list of all paths of sheets that are dependencies of the sheet path 'path'. This includes dependencies of dependencies
                            let rec parse (path : string) (deps : string list) =
                        
                                match tryLoadComponentFromPath path with
                                | Error err ->
                                    match (exists <| pathJoin [|projectDir; baseName path|]) with
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
                                                let dependencyPath = pathJoin [|sourceProjectPath; ct.Name + ".dgm"|]

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
                                            let dependencyName = baseName dependencyPath

                                            match exists dependencyPath with
                                            | true ->
                                                match (exists <| pathJoin [|projectDir; dependencyName|]) || (List.contains dependencyPath paths) with
                                                | true -> parse dependencyPath deps
                                                | false -> parse dependencyPath (dependencyPath :: deps)

                                            | false ->
                                                match (exists <| pathJoin [|projectDir; baseName dependencyPath|]) || (List.contains dependencyPath paths) with
                                                | true -> parse dependencyPath deps
                                                | false -> parse dependencyPath (baseNameWithoutExtension dependencyPath :: deps)  // dependency doesn't exist in either directory
                                        )

                            let dependencies =
                                parse path []
                                |> Set.ofList

                            (path, dependencies)
                        )

                    let headCell heading =  th [ ] [ str heading ]

                    let popupBody =
                        fun (model' : Model) ->
                            let content =
                                pathsWithDependencies
                                |> List.map (createSheetInfo model')
                                |> List.toArray

                            div [] [
                                Table.table [] [
                                        thead [] [ tr [] (List.map headCell ["Sheet" ;"Information"; "Decision"; "Action"]) ]
                                        tbody [] ( Array.concat content )
                       
                                ]
                            ]

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

                                copyFile oldSheetPath newSheetPath
                       
                            )

                            newSheetPaths |> List.iter (fun (oldSheetPath, newSheetPath) ->
                                        match newSheetPath with
                                        | "" -> ()
                                        | path -> copyFile oldSheetPath path )
                               
                            openProjectFromPath projectDir model' dispatch

                            dispatch ClosePopup
                            dispatch FinishUICmd

                    let isDisabled =
                        fun (model': Model) ->
                            not <| allDecisionsMade paths model'

                    dialogPopup "Resolve import conflicts" popupBody "OK" buttonAction isDisabled [] dispatch
        )

