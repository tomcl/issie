module MenuHelpers
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
open CanvasExtractor
open Notifications
open PopupHelpers
open DrawModelType
open Sheet.SheetInterface
open Notifications
open Optics
open Optics.Operators
open System

module Constants =
    let minGoodAppWidth = 1250.
    let minAppWidth = 1060.
    let typicalAppWidth = 1600.

    let numberOfRecentProjects: int  = 5
    let maxDisplayedPathLengthInRecentProjects: int  = 60
    /// canvas width < this => use fewer chars in path
    let largeScreenCanvasWidth = 1000
    /// max number of chars in path before cropping
    let maxNumPathChars = 25
    /// min number of chars in path before cropping
    let minNumPathChars = 7
    // NB if numCharsHidePath > minNumPathChars than path is either full-size or hidden
    let numCharsHidePath = 10
    let boldStyle = FontWeight "bold"
    let redColor = Color "red"
    let blueColor = Color "blue"
    let greenColor = Color "green"
   
let displayFileErrorNotification err dispatch =
    let note = errorFilesNotification err
    dispatch <| SetFilesNotification note

let warnAppWidth (dispatch: Msg -> unit) (afterFun: _ -> unit ) =
    let appWidth = Browser.Dom.self.innerWidth
    let styledSpan styles txt = span [Style styles] [str <| txt]
    let bSpan txt = styledSpan [FontWeight "bold"] txt
    let tSpan txt = span [] [str txt]

    if appWidth < Constants.minGoodAppWidth then
        (Some afterFun, dispatch)
        ||> PopupHelpers.dynamicConfirmationPopup "Issie Window Size Warning" "Continue" (fun model ->
            let appWidth = Browser.Dom.self.innerWidth
            let keyOf3 s1 s2 s3 = span [] [bSpan s1; tSpan " + "; bSpan s2 ; tSpan " + "; bSpan s3]
            div [] ([
                div [] [str $"The issie app window is currently "; bSpan $"{appWidth} pixels"; str " in width."]
                div [] [str "Issie works best with a width of > 1250 pixels, and typically 1600 pixels."]
                div [] [str "Issie UI will be "; bSpan "slightly degraded" ; str " when width < 1150 pixels."]
                div [] [str "Issie UI will be "; bSpan "severely degraded" ; str " when width < 1050 pixels."]
                div [] [
                    str "Web Zoom Out ("
                    (keyOf3 "Ctrl" "Shift" "-")
                    str ") or In ("
                    (keyOf3 "Ctrl" "Shift" "+")
                    str ") will increase or decrease window width"]
                (if appWidth < 1250 then bSpan "You are advised to Zoom Out now." else str "")
                ] |> List.collect (fun s -> [s; br []])))
    else
        afterFun()
        




let extractLabelBase (text:string) : string =
    text.ToUpper()
    |> Seq.takeWhile (fun ch -> ch <> '(')
    |> Seq.filter Char.IsLetterOrDigitOrUnderscore
    |> Seq.map (fun ch -> ch.ToString())
    |> String.concat ""

let formatLabelAsBus (width:int) (text:string) =
    let text' = extractLabelBase text
    match width with
    | 1 -> text'
    | _ -> sprintf "%s(%d:%d)" (text'.ToUpper()) (width-1) 0
   

let formatLabelFromType compType (text:string) =
    let text' = extractLabelBase text
    match compType with
    | Input1 (1, _) | Output 1 -> text'
    | _ -> text'


let formatLabel (comp:Component) (text:string) =
    formatLabelFromType comp.Type (text:string)

// TODO: removed formatLabel for now
let setComponentLabel model (sheetDispatch) (comp:Component) (text:string) =
    // let label = formatLabel comp text
    let label = text.ToUpper() // TODO
    model.Sheet.ChangeLabel sheetDispatch (ComponentId comp.Id) label
    match comp.Type with
    | IOLabel ->
        // need to redo bus width inference after IoLabel component change because this cabn alter circuit correctness
        let busWireDispatch bMsg = sheetDispatch (DrawModelType.SheetT.Msg.Wire bMsg)
        busWireDispatch DrawModelType.BusWireT.Msg.BusWidths
    | _ -> ()

let updateSymbolRAMs (ramCheck: Component list) (sModel: SymbolT.Model) =
    (sModel, ramCheck)
    ||> List.fold (fun sModel comp ->
            let cId = (ComponentId comp.Id)
            if Map.containsKey cId sModel.Symbols then 
                SymbolUpdate.writeMemoryType sModel cId comp.Type
            else
               sModel)


let loadComponentWithRAMChanges newCS savedWaveSim ldc model =
        let sheetInfo = {Form = ldc.Form; Description = ldc.Description} //only user defined sheets are editable and thus saveable
        let filePath = ldc.FilePath
        let (newLdc, ramCheck) = makeLoadedComponentFromCanvasData newCS filePath DateTime.Now savedWaveSim (Some sheetInfo)
        model
        |> Optic.map (sheet_ >-> SheetT.symbol_) (updateSymbolRAMs ramCheck)

/// temporary shim for compatibility while dispatch is still being used.
let raiseFileNotification  (dispatch : Msg -> unit) (msg: string option) =
    match msg with
    | Some err -> dispatch <| SetFilesNotification (errorFilesNotification err)
    | None -> ()



/// maybe no longer needed...
let fileEntryBox files fName dialog dispatch =
    let inputValidate text =
        (text = "" || 
        List.exists ((=) text) files || 
        not <| Seq.forall Char.IsLetterOrDigitOrUnderscore text || 
        not <| String.startsWithLetter text)
        |> not
    let n1,n2, _,_ = getMemorySetup dialog 1

    Input.text [
        Input.Props [Style [MarginLeft "2em"]]
        Input.DefaultValue fName
        Input.Placeholder "Enter file name"
        Input.Color (if inputValidate fName then IsSuccess else IsDanger)
        Input.OnChange 
            (getTextEventValue 
            >> (fun newName -> 
                    let newKey = if inputValidate newName then ToFile newName else ToFileBadName newName
                    dispatch <| ModelType.SetPopupDialogMemorySetup (Some(n1,n2, newKey,None) ) ) )
        ]
/// Make a poup with menu to view and select a memory data source
let makeSourceMenu 
        (model: Model)
        (updateMem: ComponentId -> (Memory1 -> Memory1) -> Unit)
        (cid: ComponentId)
        (dispatch: Msg -> Unit)
        (modelCurrent: Model) =
    let dialog = modelCurrent.PopupDialogData
    let projOpt = model.CurrentProj
    match dialog.MemorySetup with
    | None ->
        printfn "Error: can't find memory setup in dialog data"
        div [] []
    | Some (n1, n2, mem, nameOpt) ->

        let popupKey mSetup =
            match mSetup with
            | Some(_,_, key,_) -> 
                key
            | None -> 
                FromData



        let onSelect key  =
            let n1,n2, mem,_ = getMemorySetup dialog 1 // current values
            //dispatch <| ModelType.SetPopupDialogMemorySetup (Some(n1,n2,key,None))
            dispatch <| SetPopupDialogMemorySetup (Some (n1,n2,key, match key with | FromFile name -> Some name | _ -> None))
        
            match key, projOpt with
            | FromFile s, Some p ->
                let mem1 = {Init = FromFile s; AddressWidth = n1; WordWidth = n2; Data=Map.empty}
                let sheetDispatch sMsg = dispatch (Sheet sMsg)
                let mem = FilesIO.initialiseMem mem1 p.ProjectPath
                match mem with
                | Ok mem' -> updateMem cid (fun _ -> mem')
                | Error msg -> 
                    dispatch <| SetFilesNotification
                                    (Notifications.errorFilesNotification msg) 
            | _ ->
                updateMem cid (fun mem -> {mem with Init = FromData})
                

        let files =
            FilesIO.readFilesFromDirectoryWithExtn dialog.ProjectPath ".ram"
            |> List.map (FilesIO.removeExtn ".ram" >> Option.get)
       
        let existingFiles =
            List.map FromFile files

        /// Create one item in the drop-down RAM source menu
        let printSource inList key =

            match key with
            | FromData -> [str "Unlink and use data from memory viewer/editor"]
            | FromFile s -> [str $"Link memory to file {s}.ram"]
            | _ -> []

        let menuItem (key) =
            let react = printSource true key
            Menu.Item.li
                [ Menu.Item.IsActive (key = popupKey dialog.MemorySetup)
                  Menu.Item.OnClick (fun _ -> onSelect key) ]
                react 

        let noFileItem =
            Menu.Item.li
                [ Menu.Item.IsActive (mem = FromData)
                  Menu.Item.OnClick (fun _ -> onSelect FromData) ] (printSource true FromData)

        let modalMessageWithRamFiles =
                "Use this menu to change how the memory initial data is sourced. \
                You can link data to the contents of an external file in your project folder, or unlink it. \
                Unlinked data can be edited from the properties panel."

        let modalMessageNoRamFiles =
                "You cannot now link this file because your project directory has no .ram files. \
                Add a .ram file (with data in the format you can see if you write a memory) to your \
                project directory, then return to this menu to link it."

        let modalMessageBadFileLink s =
                "You have linked this component to file '{s}' which does not exist or is badly formatted. \
                Please either correct the file or remove the link."
        

        let msg, menu =
            match mem with
            | _ when existingFiles.Length > 0 ->
                modalMessageWithRamFiles, noFileItem :: List.map menuItem existingFiles
            | FromFile s -> 
                modalMessageBadFileLink s, [noFileItem]
            | _ ->
                modalMessageNoRamFiles, [noFileItem]


        div [] [
            Label.label [] [str msg]
            br []; br []
            Menu.menu []
                [ Menu.list [] menu ]
            Level.level [ Level.Level.Props [ Style [ Width "100%"; PaddingTop "20px"] ] ] [
                Level.left [] []
                Level.right [] [
                    Level.item [] [
                        Button.button [
                            Button.Color IsSuccess
                            Button.OnClick (fun _ -> 
                                dispatch ClosePopup)
                        ] [ str "Change Source" ]
                    ]
                ]
            ]
        
        ]

/// Node in the sheet tree, child nodes correspond to custom components in sheet.
type SheetTree = {
    /// path of custom component labels to node or [] if node is top level
    LabelPath: string list 
    /// name of sheet
    SheetName: string
    /// path of sheet names to current sheet name - NB this is not unique
    SheetNamePath: string list
    /// unique name to display on breadcrumbs
    BreadcrumbName: string
    /// size of tree including this node (1 for leaves)
    Size: int
    /// depth of tree beneth this need: 0 for leaves
    Depth: int
    /// children
    SubSheets: SheetTree list
    /// Use only to display tree on a grid
    GridArea: CSSGridPos option
    }

with member this.lookupPath path =
        let rec lookup sheet =
            match sheet.LabelPath = path with
            | true -> Some sheet
            | false -> List.tryPick lookup sheet.SubSheets
        lookup this

let subSheets_ = Optics.Lens.create (fun a -> a.SubSheets) (fun s a -> {a with SubSheets = s})
let breadcrumbName_ = Optics.Lens.create (fun a -> a.BreadcrumbName) (fun s a -> {a with BreadcrumbName = s})

/// Throughout the tree of sheets adjust breadcrumbName so it is unique within the children of each sheet
let rec makeBreadcrumbNamesUnique (tree: SheetTree) =
    tree.SubSheets
    |> List.map (fun subsheet ->
        let nameNotUnique =
            tree.SubSheets
            |> List.exists (fun subs' ->
                                subsheet.SheetName = subs'.SheetName &&
                                subsheet.LabelPath <> subs'.LabelPath)
        subsheet
        |> match nameNotUnique with
           | true -> Optic.set breadcrumbName_ $"{subsheet.SheetName}:{List.last subsheet.LabelPath}"
           | false -> id
        |> makeBreadcrumbNamesUnique)
    |> fun subsheets -> {tree with SubSheets = List.sortBy (fun subs -> subs.BreadcrumbName) subsheets}

            
let rec foldOverTree (isSubSheet: bool) (folder: bool -> SheetTree -> Model -> Model) (tree: SheetTree) (model: Model)=
    printf "traversing %A" tree.SheetName
    model
    |> folder isSubSheet tree
    |> fun model -> List.fold (fun model tree -> foldOverTree false folder tree model) model tree.SubSheets
    

/// Get the subsheet tree for all sheets in the current project.
/// Returns a map from sheet name to tree of SheetTree nodes
let getSheetTrees (allowAllInstances: bool) (p:Project): Map<string,SheetTree> =
    let ldcMap = 
        p.LoadedComponents
        |> List.map (fun ldc -> ldc.Name,ldc)
        |> Map.ofList

    let rec subSheets (path: string list) (sheet: string) (labelPath: string list) (sheetPath: string list): SheetTree=
        let ldc = Map.tryFind sheet ldcMap
        match ldc with
        | None -> {
            SheetName=sheet
            LabelPath = []
            SheetNamePath = []
            Size = 1;
            Depth = 0;
            SubSheets = [];
            GridArea = None
            BreadcrumbName = ""
            }
        | Some ldc ->
            let comps,_ = ldc.CanvasState
            comps
            |> List.collect (fun comp -> 
                    match comp.Type with 
                    | Custom ct when not <| List.contains ct.Name path -> 
                        [subSheets (ct.Name :: path) ct.Name (labelPath @ [comp.Label]) (sheetPath @ [sheet])] 
                    | _ -> 
                        [])
            |> (fun subs -> {
                    SheetName = sheet;
                    BreadcrumbName = sheet
                    LabelPath = labelPath
                    SheetNamePath = sheetPath
                    Depth =
                        subs
                        |> List.map (fun s -> s.Depth)
                        |> fun l -> 0 :: l
                        |> List.max
                    Size = List.sumBy (fun sub -> sub.Size) subs + 1; 
                    SubSheets = if allowAllInstances then subs else (subs |> List.distinctBy (fun sh -> sh.SheetName))
                    GridArea = None
                })
        |> makeBreadcrumbNamesUnique

    p.LoadedComponents
    |> List.map (fun ldc ->ldc.Name, subSheets [] ldc.Name [] [])
    |> Map.ofList



let allRootSheets (sTrees:Map<string,SheetTree>) =
    let rec subSheetsOf path sh =
        match Map.tryFind sh sTrees with
        | Some tree -> tree.SubSheets
        | None -> []
        |> List.collect (fun ssh -> 
            match List.contains ssh.SheetName path with
            | true -> []
            | false -> ssh.SheetName :: subSheetsOf (ssh.SheetName :: path) ssh.SheetName)
        |> List.distinct
    mapKeys sTrees
    |> Seq.collect (subSheetsOf [])
    |> Set
    |> Set.difference (set <| mapKeys sTrees)


//--------------------------------------------------------------------------------------------//
//---------------------Code for CanvasState comparison and FILE BACKUP------------------------//
//--------------------------------------------------------------------------------------------//

/// Works out number of components and connections changed between two LoadedComponent circuits
/// a new ID => a change even if the circuit topology is identical. Layout differences do not
/// mean changes, as is implemented in the reduce functions which remove layout.
let quantifyChanges (ldc1:LoadedComponent) (ldc2:LoadedComponent) =
    let comps1,conns1 = ldc1.CanvasState
    let comps2,conns2 = ldc2.CanvasState
    let reduceComp comp1:Component =
        {comp1 with X=0;Y=0}
    let reduceConn conn1 =
        {conn1 with Vertices = []}
    /// Counts the number of unequal items in the two lists.
    /// Determine equality from whether reduce applied to each item is equal
    let unmatched reduce lst1 lst2 =
        let mapToSet = List.map reduce >> Set
        let rL1, rL2 = mapToSet lst1, mapToSet lst2
        Set.union (Set.difference rL1 rL2) (Set.difference rL2 rL1)
        |> Set.count
    unmatched reduceComp comps1 comps2, unmatched reduceConn conns1 conns2



let writeComponentToFile comp =
    let data =  stateToJsonString (comp.CanvasState,comp.WaveInfo,Some {Form=comp.Form;Description=comp.Description})
    writeFile comp.FilePath data

/// return an option containing sequence data and file name and directory of the latest
/// backup file for given component, if it exists.
let readLastBackup comp =
    let path = pathWithoutExtension comp.FilePath 
    let baseN = baseName path
    let backupDir = pathJoin [| dirName path ; "backup" |]
    latestBackupFileData backupDir baseN
    |> Option.map (fun (seq, fName) -> seq, fName, backupDir)
  
/// Write Loadedcomponent comp to a backup file if there has been any change.
/// Overwrite the existing backup file only if it is a small, and recent, change.
/// Parameters determine thresholds of smallness and recency
/// return () - display an error if the write goes wrong.
let writeComponentToBackupFile (numCircuitChanges: int) (numHours:float) comp (dispatch: Msg -> Unit)= 
    let nSeq, backupFileName, backFilePath =
        match readLastBackup comp with
        | Some( n, fp, path) -> n+1,fp, path
        | None -> 0, "", pathJoin [|comp.FilePath; "backup"|]
    //printfn "seq=%d,name=%s,path=%s" nSeq backupFileName backFilePath
    let wantToWrite, oldFile =
        if backupFileName = "" then
            true, None
        else
            let oldBackupFile = pathJoin [|backFilePath ; backupFileName|]
            match tryLoadComponentFromPath (oldBackupFile) with
            | Ok comp' ->
                if not (compareIOs comp comp') then
                    true, None // need to save, to a new backup file
                elif compareCanvas 10000. comp.CanvasState comp'.CanvasState then
                    false, None // no need for a new backup
                else
                    let nComps,nConns = quantifyChanges comp' comp
                    let interval = comp.TimeStamp - comp'.TimeStamp
                    if interval.TotalHours > numHours || nComps + nConns  > numCircuitChanges then
                        true, None
                    else
                        true, Some oldBackupFile
                        
            | err -> 
                printfn "Error: writeComponentToBackup\n%A" err
                true, None
    if wantToWrite then
        let timestamp = System.DateTime.Now
        let backupPath =
                // work out new path to write based on time.
                let path = pathWithoutExtension comp.FilePath
                let baseN = baseName path
                let ds = EEExtensions.String.replaceChar '/' '-' (timestamp.ToShortDateString())
                let suffix = EEExtensions.String.replaceChar ' ' '-' (sprintf "%s-%02dh-%02dm" ds timestamp.Hour timestamp.Minute)
                let backupDir = pathJoin [| dirName path ; "backup" |]
                ensureDirectory <| pathJoin [| dirName path ; "backup" |]
                pathJoin [| dirName path ; "backup" ; sprintf "%s-%03d-%s.dgm" baseN nSeq suffix |]
        // write the new backup file
        {comp with 
            TimeStamp = timestamp
            FilePath = backupPath}
        |> writeComponentToFile
        |> displayAlertOnError dispatch
        // if necessary delete the old backup file
        match oldFile with
        | Some oldPath when oldPath <> backupPath ->
            if Node.Api.fs.existsSync (Fable.Core.U2.Case1 oldPath) then
                Node.Api.fs.unlink (Fable.Core.U2.Case1 oldPath, ignore) // Asynchronous.
            else
                ()
        | _ -> ()

/// Write Loadedcomponent comp to a backup file if there has been any change.
/// Overwrite the existing backup file only if it is a small, and recent, change.
/// Parameters determine thresholds of smallness and recency
/// return () - ignore errors
let writeComponentToBackupFileNow (numCircuitChanges: int) (numHours:float) comp = 
    let nSeq, backupFileName, backFilePath =
        match readLastBackup comp with
        | Some( n, fp, path) -> n+1,fp, path
        | None -> 0, "", pathJoin [|comp.FilePath; "backup"|]
    //printfn "seq=%d,name=%s,path=%s" nSeq backupFileName backFilePath
    let wantToWrite, oldFile =
        if backupFileName = "" then
            true, None
        else
            let oldBackupFile = pathJoin [|backFilePath ; backupFileName|]
            match tryLoadComponentFromPath (oldBackupFile) with
            | Ok comp' ->
                if not (compareIOs comp comp') then
                    true, None // need to save, to a new backup file
                elif compareCanvas 10000. comp.CanvasState comp'.CanvasState then
                    false, None // no need for a new backup
                else
                    let nComps,nConns = quantifyChanges comp' comp
                    let interval = comp.TimeStamp - comp'.TimeStamp
                    if interval.TotalHours > numHours || nComps + nConns  > numCircuitChanges then
                        true, None
                    else
                        true, Some oldBackupFile
                        
            | err -> 
                printfn "Error: writeComponentToBackup\n%A" err
                true, None
    if wantToWrite then
        let timestamp = System.DateTime.Now
        let backupPath =
                // work out new path to write based on time.
                let path = pathWithoutExtension comp.FilePath
                let baseN = baseName path
                let ds = EEExtensions.String.replaceChar '/' '-' (timestamp.ToShortDateString())
                let suffix = EEExtensions.String.replaceChar ' ' '-' (sprintf "%s-%02dh-%02dm" ds timestamp.Hour timestamp.Minute)
                let backupDir = pathJoin [| dirName path ; "backup" |]
                ensureDirectory <| pathJoin [| dirName path ; "backup" |]
                pathJoin [| dirName path ; "backup" ; sprintf "%s-%03d-%s.dgm" baseN nSeq suffix |]
        // write the new backup file
        {comp with 
            TimeStamp = timestamp
            FilePath = backupPath}
        |> writeComponentToFile
        |> ignore
        // if necessary delete the old backup file
        match oldFile with
        | Some oldPath when oldPath <> backupPath ->
            if Node.Api.fs.existsSync (Fable.Core.U2.Case1 oldPath) then
                Node.Api.fs.unlink (Fable.Core.U2.Case1 oldPath, ignore) // Asynchronous.
            else
                ()
        | _ -> ()

//-------------------------------------------------------------------------------------------------//
//-----------------------------------------FILE MENU HELPERS---------------------------------------//
//-------------------------------------------------------------------------------------------------//

let getFileInProject name project = project.LoadedComponents |> List.tryFind (fun comp -> comp.Name = name)

let isFileInProject name project =
    getFileInProject name project
    |> function
    | None -> false
    | Some _ -> true

/// Send messages to change Diagram Canvas and specified sheet waveSim in model
let private loadStateIntoModel (finishUI:bool) (compToSetup:LoadedComponent) waveSim ldComps (model:Model) dispatch =
    // it seems still need this, however code has been deleted!
    //Sheet.checkForTopMenu () // A bit hacky, but need to call this once after everything has loaded to compensate mouse coordinates.
    let ldcs = tryGetLoadedComponents model
    let name = compToSetup.Name
    let components, connections = compToSetup.CanvasState
    //printfn "Loading..."
    let msgs = 
        [
            SetHighlighted([], []) // Remove current highlights.
    
            // Clear the canvas.
            Sheet SheetT.ResetModel
            Sheet (SheetT.Wire BusWireT.ResetModel)
            Sheet (SheetT.Wire (BusWireT.Symbol (SymbolT.ResetModel ) ) )
    
            // Finally load the new state in the canvas.
            SetIsLoading true
            //printfn "Check 1..."
    
            //Load components
            Sheet (SheetT.Wire (BusWireT.Symbol (SymbolT.LoadComponents (ldcs,components ))))
    
            Sheet (SheetT.Wire (BusWireT.LoadConnections connections))

            Sheet SheetT.FlushCommandStack // Discard all undo/redo.
            // Run the a connection widths inference.
            //printfn "Check 4..."
    
            Sheet (SheetT.Wire (BusWireT.BusWidths))
            // JSdispatch <| InferWidths()
            //printfn "Check 5..."
            // Set no unsaved changes.

            Sheet SheetT.UpdateBoundingBoxes

            Sheet (SheetT.Wire (BusWireT.MakeJumps (true, connections |> List.map (fun conn -> ConnectionId conn.Id ))))

            // set waveSim data
            AddWSModel (name, waveSim)

            // this message actually changes the project in model
            SetProject {
                ProjectPath = dirName compToSetup.FilePath
                OpenFileName =  compToSetup.Name
                WorkingFileName = Some compToSetup.Name
                LoadedComponents = ldComps
            }

            Sheet (SheetT.KeyPress  SheetT.KeyboardMsg.CtrlW)
            SynchroniseCanvas
            SetIsLoading false 
            if finishUI then FinishUICmd else DoNothing

            //printfn "Check 6..."
        ]
    //INFO - Currently the spinner will ALWAYS load after 'SetTopMenu x', probably it is the last command in a chain
    //Ideally it should happen before this, but it is not currently doing this despite the async call
    //This will set a spinner for both Open project and Change sheet which are the two most lengthly processes
    dispatch <| (Sheet (SheetT.SetSpinner true))
    dispatch <| SendSeqMsgAsynch msgs
    // msgs is bundled together and as a result a scroll from the ctrl-W scroll change is inserted in the event queue
    // after the ctrl-w. We need anotehr ctrl-w to make sure this scroll event does not reset scroll
    // the order in which messages get processed is problematic here - and the solution ad hoc - a better
    // solution would be to understand exactly what determines event order in the event queue
    dispatch <| Sheet (SheetT.KeyPress  SheetT.KeyboardMsg.CtrlW)
    dispatch SynchroniseCanvas
    //dispatch <| Sheet (SheetT.KeyPress  SheetT.KeyboardMsg.CtrlW)
    //dispatch SynchroniseCanvas


/// Load a new project as defined by parameters.
/// Ends any existing simulation
/// Closes WaveSim if this is being used
let setupProjectFromComponents (finishUI:bool) (sheetName: string) (ldComps: LoadedComponent list) (model: Model) (dispatch: Msg->Unit)=
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
        dispatch <|TruthTableMsg CloseTruthTable // Message closes any open Truth Table.
        //dispatch EndWaveSim
        // TODO: make each sheet wavesim remember the list of waveforms.

    let savedWaveSim =
        compToSetup.WaveInfo
        |> Option.map loadWSModelFromSavedWaveInfo 
        |> Option.defaultValue initWSModel

    let waveSim =
        model.WaveSimSheet
        |> Option.map (fun sheet -> (Map.tryFind sheet  model.WaveSim))
        |> Option.defaultValue None
        |> Option.defaultValue savedWaveSim
        


    loadStateIntoModel finishUI compToSetup waveSim ldComps model dispatch
    {
        ProjectPath = dirName compToSetup.FilePath
        OpenFileName =  compToSetup.Name
        WorkingFileName = Some compToSetup.Name
        LoadedComponents = ldComps
    }
    |> SetProject // this message actually changes the project in model
    |> dispatch
    dispatch SynchroniseCanvas


/// Create a new empty .dgm file and return corresponding loaded component.
let private createEmptyDiagramFile projectPath name =
    createEmptyDgmFile projectPath name |> ignore

    {   
        Name = name
        LoadedComponentIsOutOfDate = false
        TimeStamp = System.DateTime.Now
        WaveInfo = None
        FilePath = pathJoin [| projectPath; name + ".dgm" |]
        CanvasState = [],[]
        InputLabels = []
        OutputLabels = []
        Form = Some User
        Description = None
    }


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

/// open an existing demo project from its path
let openDemoProjectFromPath (path:string) model dispatch =

    warnAppWidth dispatch (fun _ ->

        traceIf "project" (fun () -> "loading files")
        match loadAllComponentFiles path with
        | Error err ->
            log err
            displayFileErrorNotification err dispatch

        | Ok (componentsToResolve: LoadStatus list) ->
            traceIf "project" (fun () -> "resolving popups...")
            
            resolveComponentOpenPopup path [] componentsToResolve model dispatch
            traceIf "project" (fun () ->  "project successfully opened.")

    )

/// open an existing project from its path
let openProjectFromPath (path:string) model dispatch =
    warnAppWidth dispatch (fun _ ->
    dispatch (ExecFuncAsynch <| fun () ->
        traceIf "project" (fun () -> "loading files")
        match loadAllComponentFiles path with
        | Error err ->
            log err
            displayFileErrorNotification err dispatch
            model.UserData.RecentProjects
            |> Option.map (List.filter ((<>) path)) 
        | Ok (componentsToResolve: LoadStatus list) ->
            traceIf "project" (fun () -> "resolving popups...")
            
            resolveComponentOpenPopup path [] componentsToResolve model dispatch
            traceIf "project" (fun () ->  "project successfully opened.")
            addToRecents path model.UserData.RecentProjects
        |> fun recents ->
                dispatch <| SetUserData {
                    model.UserData with 
                        LastUsedDirectory = Some path; 
                        RecentProjects = recents
                        }
        Elmish.Cmd.none))



/// returns a WaveSimModel option if a file is loaded, otherwise None
let currWaveSimModel (model: Model) =
    match getCurrFile model with
    | Some fileName -> Map.tryFind fileName model.WaveSim
    | _ -> None


/// Return LoadedComponents with sheet name updated according to setFun.
/// Do not update model. 
let updateLoadedComponents name (setFun: LoadedComponent -> LoadedComponent) (lcLst: LoadedComponent list) (dispatch: (Msg -> Unit))=
    let n = List.tryFindIndex (fun (lc: LoadedComponent) -> lc.Name = name) lcLst
    match n with
    | None -> 
        printf "In updateLoadedcomponents can't find name='%s' in components:%A" name lcLst
        lcLst
    | Some n ->
        let oldLc = lcLst[n]
        let newLc = setFun oldLc
        writeComponentToBackupFile 0 1. oldLc dispatch
        List.mapi (fun i x -> if i = n then newLc else x) lcLst

/// return current project with current sheet updated from canvas if needed.
/// Do not update model.
let updateProjectFromCanvas (model:Model) (dispatch:Msg -> Unit) =
    match model.Sheet.GetCanvasState() with
    | ([], []) -> model.CurrentProj
    | canvasState ->  
        canvasState
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
                    p with LoadedComponents = updateLoadedComponents p.OpenFileName setLc p.LoadedComponents dispatch
                })


/// extract SavedWaveInfo from model to be saved
let getSavedWave (model: Model) : SavedWaveInfo option = 
    match currWaveSimModel model with
    | Some wsModel -> Some (getSavedWaveInfo wsModel)
    | None -> None

/// Save the sheet currently open, return  the new sheet's Loadedcomponent if this has changed.
/// Do not change model.
/// update Symbol model with new RAM contents.
let saveOpenFileAction isAuto model (dispatch: Msg -> Unit)=
    match model.Sheet.GetCanvasState (), model.CurrentProj with
    | _, None -> None
    | canvasState, Some project ->
        // "DEBUG: Saving Sheet"
        // printfn "DEBUG: %A" project.ProjectPath
        // printfn "DEBUG: %A" project.OpenFileName
        let ldc = project.LoadedComponents |> List.find (fun lc -> lc.Name = project.OpenFileName)
        let sheetInfo = {Form = ldc.Form; Description = ldc.Description} //only user defined sheets are editable and thus saveable
        let savedState = canvasState, getSavedWave model,(Some sheetInfo)
        if isAuto then
            failwithf "Auto saving is no longer used"
            None
        else 
            saveStateToFile project.ProjectPath project.OpenFileName savedState
            |> displayAlertOnError dispatch
            removeFileWithExtn ".dgmauto" project.ProjectPath project.OpenFileName
            let origLdComp =
                project.LoadedComponents
                |> List.find (fun lc -> lc.Name = project.OpenFileName)
                |> Optic.set loadedComponentIsOutOfDate_ false
            let savedWaveSim =
                Map.tryFind project.OpenFileName model.WaveSim
                |> Option.map getSavedWaveInfo
            let (SheetInfo:SheetInfo option) = match origLdComp.Form with |None -> None |Some form -> Some {Form=Some form;Description=origLdComp.Description}
            let (newLdc, ramCheck) = makeLoadedComponentFromCanvasData canvasState origLdComp.FilePath DateTime.Now savedWaveSim SheetInfo
            let newState =
                canvasState
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

/// Save the sheet currently open, return updated model
/// dispatch not needed.
/// currently errors in saving are not processed: because
/// without dispatch we cannot add an alert.
/// this could be changed by using the Notification field in the returned model
let saveOpenFileToModel model =
    match model.Sheet.GetCanvasState (), model.CurrentProj with
    | _, None -> None
    | canvasState, Some project ->
        // "DEBUG: Saving Sheet"
        // printfn "DEBUG: %A" project.ProjectPath
        // printfn "DEBUG: %A" project.OpenFileName
        let ldc = project.LoadedComponents |> List.find (fun lc -> lc.Name = project.OpenFileName)
        let sheetInfo = {Form = ldc.Form; Description = ldc.Description} //only user defined sheets are editable and thus saveable
        let savedState = canvasState, getSavedWave model,(Some sheetInfo)
        saveStateToFile project.ProjectPath project.OpenFileName savedState |> ignore
        removeFileWithExtn ".dgmauto" project.ProjectPath project.OpenFileName
        let origLdComp =
            project.LoadedComponents
            |> List.find (fun lc -> lc.Name = project.OpenFileName)
        let savedWaveSim =
            Map.tryFind project.OpenFileName model.WaveSim
            |> Option.map getSavedWaveInfo
        let (SheetInfo:SheetInfo option) = match origLdComp.Form with |None -> None |Some form -> Some {Form=Some form;Description=origLdComp.Description}
        let (newLdc, ramCheck) = makeLoadedComponentFromCanvasData canvasState origLdComp.FilePath DateTime.Now savedWaveSim SheetInfo
        let sModel, newState =
            canvasState
            |> (fun (comps, conns) ->
                let sModel, comps = 
                    ((model.Sheet.Wire.Symbol,[]), comps)
                    ||> List.fold (fun (sModel, newComps) comp -> 
                        match List.tryFind (fun (c:Component) -> c.Id=comp.Id) ramCheck with
                        | Some newRam -> 
                            // TODO: create consistent helpers for messages
                            SymbolUpdate.writeMemoryType sModel (ComponentId comp.Id) (newRam.Type), (newRam :: newComps)                            
                        | _ -> sModel, comp :: newComps)
                sModel, (comps,conns))
        writeComponentToBackupFileNow 4 1. newLdc
        let newLdc' = {newLdc with CanvasState=newState}
        let project' =
            project
            |> Optic.set (loadedComponentOf_ project.OpenFileName) newLdc'
        model
        |> Optic.set (sheet_ >-> SheetT.symbol_) sModel
        |> Optic.set currentProj_ (Some project')
        |> Some
        

let saveOpenProjectInNewFormat (model: Model) =
    match model.CurrentProj with
    | None -> failwith "No opened project"
    | Some project ->
        project.LoadedComponents
        |> List.map (fun comp ->
            let sheetInfo = {Form=comp.Form;Description=comp.Description}
            let savedState = comp.CanvasState, None, Some sheetInfo
            match saveStateToFileExperimental project.ProjectPath comp.Name savedState with
            | Ok _ -> printfn "Successfully saved %s" comp.Name
            | Error errr -> printfn "Error on saving %s: %s" comp.Name errr)
        |> fun _ -> printfn "Done"

/// save current open file, updating model etc, and returning the loaded component and the saved (unreduced) canvas state
let saveOpenFileActionWithModelUpdate (model: Model) (dispatch: Msg -> Unit) =
    let opt = saveOpenFileAction false model dispatch
    let ldcOpt = Option.map fst opt
    let state = Option.map snd opt |> Option.defaultValue ([],[])
    match model.CurrentProj with
    | None -> failwithf "What? Should never be able to save sheet when project=None"
    | Some p -> 
        // update loaded components for saved file
        updateLdCompsWithCompOpt ldcOpt p.LoadedComponents
        |> (fun lc -> {p with LoadedComponents=lc})
        |> SetProject
        |> dispatch

    SetHasUnsavedChanges false
    |> JSDiagramMsg
    |> dispatch
    dispatch FinishUICmd
    opt


/// Open the specified file, saving the current file if needed.
/// Creates messages sufficient to do all necessary model and diagram change
/// Terminates a simulation if one is running
/// Closes waveadder if it is open
let openFileInProject' saveCurrent name project (model:Model) dispatch =
    let newModel = {model with CurrentProj = Some project}
    match getFileInProject name project with
    | None ->
        printf "%s" $"Anomalous project: sheet {name}.dgm not found"
        SetFilesNotification <| errorFilesNotification 
           $"Warning: Issie could not find the file '{name}.dgm' in the project. Did you delete a file manually?"
        |> dispatch
        dispatch FinishUICmd
    | Some {Form=Some (ProtectedTopLevel | ProtectedSubSheet)} when debugLevel = 0 ->
        SetFilesNotification <| errorFilesNotification 
            $"Warning: The sheet '{name}' is protected and cannot be opened."
        |> dispatch
        dispatch FinishUICmd
    | Some lc ->
        match updateProjectFromCanvas model dispatch with
        | None -> failwithf "What? current project cannot be None at this point in openFileInProject"
        | Some p ->
            let updatedModel = {newModel with CurrentProj = Some p}
            //printSheetNames updatedModel
            let ldcs =
                if saveCurrent then 
                    let opt = saveOpenFileAction false updatedModel dispatch
                    let ldcOpt = Option.map fst opt
                    let ldComps = updateLdCompsWithCompOpt ldcOpt project.LoadedComponents
                    ldComps
                else
                    project.LoadedComponents
            //printSheetNames {newModel with CurrentProj = Some {Option.get newModel.CurrentProj with LoadedComponents = ldcs }}
            setupProjectFromComponents true name ldcs updatedModel dispatch

let openFileInProject name project (model:Model) dispatch =
    openFileInProject' model.SavedSheetIsOutOfDate name project (model:Model) dispatch



let removeAllCustomComps (name:string) project =
    let ldcs = project.LoadedComponents
    ldcs
    |> List.map (fun lc -> 
        let comps,conns = lc.CanvasState
        let idsToBeDeleted = 
            comps |> List.filter (fun comp -> 
                match comp.Type with
                |Custom c when c.Name = name -> true
                |_ -> false
            )
            |> List.map (fun comp -> comp.Id)
        let newComps = 
            comps |> List.filter (fun comp -> 
                match comp.Type with
                |Custom c when c.Name = name -> 
                    false
                |_ -> true
            )
        let newConns =
            conns |> List.filter (fun conn ->
                match conn.Source.HostId,conn.Target.HostId with
                |hostId,_ when (List.exists (fun id -> id = hostId) idsToBeDeleted) -> false
                |_,targetId when (List.exists (fun id -> id = targetId) idsToBeDeleted) -> false
                |_,_ -> true
            )
        {lc with CanvasState=(newComps,newConns)})


/// Remove file.
let removeFileInProject name project model dispatch =
    removeFile project.ProjectPath name
    // Remove the file from the dependencies and update project.
    let newComponents = List.filter (fun (lc: LoadedComponent) -> lc.Name.ToLower() <> name.ToLower()) project.LoadedComponents
    // Make sure there is at least one file in the project.
    let project' = {project with LoadedComponents = newComponents}

    //delete all custom components from that sheet
    let newComponents' = removeAllCustomComps name project' 
    let project' = {project' with LoadedComponents = newComponents'}

    match newComponents, name = project.OpenFileName with
    | [],true -> 
        // reate a new empty file with default name main as sole file in project
        let newComponents = [ (createEmptyDiagramFile project.ProjectPath "main") ]
        let project' = {project' with LoadedComponents = newComponents; OpenFileName="main"; WorkingFileName=Some "main"}
        openFileInProject' false newComponents[0].Name project' model dispatch
    | [], false -> 
        failwithf "What? - this cannot happen"
    | nc, true ->
        // open one of the undeleted loadedcomponents
        //printfn $"remove sheet '{name}'"
        //printSheetNames {model with CurrentProj = Some project'}
        openFileInProject' false project'.LoadedComponents[0].Name project' model dispatch
    | nc, false ->
        // nothing chnages except LoadedComponents
        //printfn $"remove sheet '{name}'"
        //printSheetNames {model with CurrentProj = Some project'}
        openFileInProject' false project'.OpenFileName project' model dispatch
    dispatch FinishUICmd
       
let deleteFileConfirmationPopup (sheetName: string) (model: Model) (dispatch: Msg -> unit) =
    let title = "Delete sheet"
    let project = Option.get model.CurrentProj
    let body =
        div []
            [ str "Are you sure you want to delete the following design sheet?"
              br []
              str <| pathJoin
                        [| project.ProjectPath
                           sheetName + ".dgm" |]
              br []
              str <| "This action is irreversible." ]

    let buttonText = "Delete"

    let buttonAction =
        fun _ ->
            dispatch (StartUICmd DeleteSheet)
            dispatch <| ExecFuncInMessage(removeFileInProject sheetName project,dispatch)
            dispatch ClosePopup
    confirmationPopup title buttonText body buttonAction dispatch

//--------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------//
//---------------------Code for CanvasState comparison and FILE BACKUP------------------------//
//--------------------------------------------------------------------------------------------//






let getHintPaneElement (model:Model) =
    match model.Sheet.Wire.Symbol.HintPane, model.TopMenuOpenState with
    | _, Files-> [str "Click -> Open Sheet"; br []; str "Left-click -> Rename or Delete"]
    | Some hintStrL, _ ->
        hintStrL
        |> List.map (fun x -> [str x; br []])
        |> List.concat
        |> fun lst -> if lst.Length = 0 then [] else lst[0..lst.Length-2]
    | _ -> [str ""]




