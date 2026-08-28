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

    /// A shortlist, not a history: past the first few, a name in it is no faster to pick out than
    /// the project browser next to it would find the folder. Entries beyond this are dropped on
    /// display as well as on write, so lowering it takes effect on a list already saved to disk.
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
    model.Sheet.ChangeLabel sheetDispatch (comp.Id) label
    match comp.Type with
    | IOLabel ->
        // need to redo bus width inference after IoLabel component change because this cabn alter circuit correctness
        let busWireDispatch bMsg = sheetDispatch (DrawModelType.SheetT.Msg.Wire bMsg)
        busWireDispatch DrawModelType.BusWireT.Msg.BusWidths
    | _ -> ()

let updateSymbolRAMs (ramCheck: Component list) (sModel: SymbolT.Model) =
    (sModel, ramCheck)
    ||> List.fold (fun sModel comp ->
            let cId = (comp.Id)
            if Map.containsKey cId sModel.Symbols then 
                SymbolUpdate.writeMemoryType sModel cId comp.Type
            else
               sModel)


let loadComponentWithRAMChanges newCS savedWaveSim ldc model =
        let sheetInfo:SheetInfo = {Form = ldc.Form; Description = ldc.Description; ParameterDefinitions=ldc.LCParameterSlots; IsTopSheet = Some ldc.IsTopSheet} //only user defined sheets are editable and thus saveable
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
        Log.warn "no memory setup in the dialog data"
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
                let mem1 = {Init = FromFile s; AddressWidth = n1; WordWidth = n2; Data=Map.empty; Comments=None}
                let sheetDispatch sMsg = dispatch (Sheet sMsg)
                let mem = FilesIO.initialiseMem mem1 p.ProjectPath
                match mem with
                // The file is read at the widths the memory is drawn at, which need not be the
                // widths it has everywhere: a parameterised memory is several sizes at once, and
                // the file is the contents of all of them. Linking it is refused where it does not
                // fit one, rather than leaving a design that cannot be simulated.
                | Ok mem' ->
                    match MemoryData.dataProblemAtWidths
                            (ModelHelpers.memoryWidthsInDesign model cid mem') mem'.Data with
                    | None -> updateMem cid (fun _ -> mem')
                    | Some problem ->
                        dispatch <| SetFilesNotification
                                        (Notifications.errorFilesNotification
                                            $"'{s}.ram' does not fit this memory: {problem}.")
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
    /// design-time name of sheet
    SheetName: string
    /// design-time sheet names from the root of the tree down to and including this node.
    /// This is what identifies a node once several instances of one sheet inside one parent are
    /// collapsed into a single node: unlike LabelPath it does not depend on which of them was
    /// kept, and unlike SheetName it tells apart the places one sheet is reached from.
    SheetPath: string list
    /// the custom component instances between this node and the root of the tree, INNERMOST
    /// FIRST - an InstancePath, and the same order the simulator's AccessPath is in. Unlike the
    /// two paths above this one is ids, so it is unaffected by relabelling.
    SheetAccessPath: ComponentId list
    /// unique name to display on breadcrumbs
    /// this is usually the design-time name of sheet
    /// with instance name added if this is not unique
    BreadcrumbName: string
    /// size of tree including this node (1 for leaves)
    Size: int
    /// depth of tree beneth this need: 0 for leaves
    Depth: int
    /// children
    SubSheets: SheetTree list
    /// Use only to display tree on a grid
    GridArea: CSSGridPos option
    } with

     /// Keyed by LabelPath, while the wave selector keys its nodes by SheetPath: a node stands for
     /// every instance of its sheet at that point in the tree, so it has no one label path.
     member this.lookupPath path =
        let rec lookup sheet =
            match sheet.LabelPath = path with
            | true -> Some sheet
            | false -> List.tryPick lookup sheet.SubSheets
        lookup this

let subSheets_ = Optics.Lens.create (fun a -> a.SubSheets) (fun s a -> {a with SubSheets = s})
let breadcrumbName_ = Optics.Lens.create (fun a -> a.BreadcrumbName) (fun s a -> {a with BreadcrumbName = s})

/// One custom component instance on a sheet's canvas. The field names are prefixed because F#
/// resolves an unannotated record field to the LAST type declaring it: `Label`, `Id` and `Sheet`
/// would each capture uses meant for Component and for the Elmish Model, in every file compiled
/// after this one.
type SheetInstance = {
    /// the label the instance carries on the canvas it sits on
    InstLabel: string
    /// the id of the custom component on that canvas
    InstId: ComponentId
    /// the design-time name of the sheet it instantiates
    InstSheet: string
    }

/// What is directly inside each sheet of a project, by sheet NAME - so one entry however many
/// times a sheet is instantiated, with the sheets inside it NAMED rather than held. That costs one
/// pass over each canvas and not the design's expansion, and it makes a sheet that contains itself
/// representable without anything lazy.
///
/// It deliberately holds nothing that depends on where a sheet sits: label path, access path,
/// breadcrumb name, size and depth are all properties of an OCCURRENCE, and a design several
/// routes reach one sheet by has many occurrences of one entry here. Those are filled in by
/// materialiseTree, for the occurrences that are going to be looked at.
type SheetShapes = Map<string, SheetInstance list>

/// Make each child's breadcrumb name unique among its siblings, and sort them by it - which is the
/// order the breadcrumbs are drawn in. Applied to one node's children as that node is built, so
/// each node is visited once; it used to be a recursion applied again at every level above, which
/// walked a subtree once per ancestor it had.
let private nameChildrenUniquely (subSheets: SheetTree list) =
    subSheets
    |> List.map (fun subsheet ->
        let nameNotUnique =
            subSheets
            |> List.exists (fun subs' ->
                                subsheet.SheetName = subs'.SheetName &&
                                subsheet.LabelPath <> subs'.LabelPath)
        subsheet
        |> match nameNotUnique with
           | true -> Optic.set breadcrumbName_ $"{subsheet.SheetName}:{List.last subsheet.LabelPath}"
           | false -> id)
    |> List.sortBy (fun subs -> subs.BreadcrumbName)

            
let rec foldOverTree (isSubSheet: bool) (folder: bool -> SheetTree -> Model -> Model) (tree: SheetTree) (model: Model)=
    model
    |> folder isSubSheet tree
    |> fun model -> List.fold (fun model tree -> foldOverTree false folder tree model) model tree.SubSheets
    

/// What is directly inside each sheet of the project: one pass over each canvas, and so a cost
/// that is the design's own size whatever that design expands to.
///
/// showLibrarySheet: a library sheet it answers false for is left out, and so are the instances
/// that would put it inside someone else - a library component is one thing, not a sheet with
/// innards. It has to be decided here rather than by filtering the project first: the shapes are
/// read off each sheet's canvas, so a sheet removed from LoadedComponents still leaves its
/// instance naming it, with nothing to find under that name.
///
/// A predicate rather than one flag for the lot, because a library component the user has asked
/// to look inside appears in the Sheets menu while the rest of the library stays hidden.
///
/// The sheets are given as a list rather than as a project, because not every hierarchy is drawn
/// from the project: the wave selector draws the design that was SIMULATED, which the simulation
/// carries as a list of exactly the sheets it needed.
let getSheetShapes (showLibrarySheet: string -> bool) (ldcs: LoadedComponent list): SheetShapes =
    let ldcMap =
        ldcs
        |> List.map (fun ldc -> ldc.Name, ldc)
        |> Map.ofList

    let hidden (sheet: string) =
        not (showLibrarySheet sheet)
        && (Map.tryFind sheet ldcMap |> Option.map ComponentLibraries.isLibrarySheet |> Option.defaultValue false)

    ldcs
    |> List.filter (fun ldc -> not (hidden ldc.Name))
    |> List.map (fun ldc ->
        let comps, _ = ldc.CanvasState
        ldc.Name,
        comps
        |> List.choose (fun comp ->
            match comp.Type with
            | Custom ct when not (hidden ct.Name) ->
                Some { InstLabel = comp.Label; InstId = comp.Id; InstSheet = ct.Name }
            | _ -> None))
    |> Map.ofList

/// Build the part of a hierarchy that is going to be looked at.
///
/// `expand` is asked of each node's SheetPath: a node it says false to comes back as a leaf and
/// nothing below it is built at all. That is what lets a design many routes reach one sheet in be
/// drawn without being walked, since the drawn tree is then the only tree there is.
/// `fun _ -> true` builds the whole thing, which is what the Sheets menu asks for.
///
/// allInstances = false collapses several instances of one sheet inside one parent into one node,
/// dropping the others BEFORE their subtrees are built rather than after. Instances of a sheet
/// expand identically - same children, same depth - so this is the same tree; what it is not is
/// the same amount of work.
///
/// A sheet reached from inside itself is not descended into. The guard is on the ancestor path, so
/// the shapes may name each other in a cycle and this still terminates.
let materialiseTree
        (expand: string list -> bool)
        (allInstances: bool)
        (shapes: SheetShapes)
        (root: string)
        : SheetTree =
    let rec node
            (ancestors: string list)
            (sheet: string)
            (sheetPath: string list)
            (labelPath: string list)
            (accessPath: ComponentId list)
            : SheetTree =
        let leaf subs = {
            SheetName = sheet
            BreadcrumbName = sheet
            LabelPath = labelPath
            SheetPath = sheetPath
            SheetAccessPath = accessPath
            // A leaf is 0 deep and anything else is one deeper than its deepest child.
            // The `+ 1` used to be missing, which made every node in every tree 0 deep.
            Depth =
                match subs with
                | [] -> 0
                | subs -> 1 + (subs |> List.map (fun s -> s.Depth) |> List.max)
            Size = List.sumBy (fun sub -> sub.Size) subs + 1
            SubSheets = subs
            GridArea = None
            }
        // A sheet the project does not hold, and one whose contents are not being shown, are both
        // leaves. Size and Depth describe what was built, so a suppressed node reads as 1 and 0.
        match Map.tryFind sheet shapes with
        | None -> leaf []
        | Some _ when not (expand sheetPath) -> leaf []
        | Some instances ->
            instances
            |> List.filter (fun inst -> not (List.contains inst.InstSheet ancestors))
            |> (fun instances ->
                    if allInstances then instances
                    else instances |> List.distinctBy (fun inst -> inst.InstSheet))
            |> List.map (fun inst ->
                    node
                        (inst.InstSheet :: ancestors)
                        inst.InstSheet
                        (sheetPath @ [inst.InstSheet])
                        (labelPath @ [inst.InstLabel])
                        // ids innermost first, names root first: see InstancePath in Ids.fs
                        (inst.InstId :: accessPath))
            |> nameChildrenUniquely
            |> leaf

    node [] root [root] [] []

/// The sheets that more than one route from the root reaches, and which therefore appear more than
/// once in its hierarchy. Worked out from the design graph - each sheet's parents are counted, not
/// its occurrences - so a design whose expansion is astronomical still costs its own size.
///
/// Everything below such a sheet is one too: a sheet inside a sheet that appears twice appears
/// twice itself, however singular its own parent is.
let multiPathSheets (shapes: SheetShapes) (root: string): Set<string> =
    let childrenOf sheet =
        match Map.tryFind sheet shapes with
        | None -> []
        | Some instances -> instances |> List.map (fun inst -> inst.InstSheet) |> List.distinct

    let rec reach seen sheet =
        if Set.contains sheet seen then seen
        else childrenOf sheet |> List.fold reach (Set.add sheet seen)
    let reachable = reach Set.empty root

    /// Reached from two different sheets, or from itself, is reached more than once.
    let seed =
        reachable
        |> Set.toList
        |> List.collect (fun parent -> childrenOf parent |> List.map (fun child -> child, parent))
        |> List.filter (fun (child, _) -> Set.contains child reachable)
        |> List.groupBy fst
        |> List.filter (fun (child, parents) ->
            let parents = parents |> List.map snd |> Set.ofList
            Set.count parents > 1 || Set.contains child parents)
        |> List.map fst
        |> Set.ofList

    let rec spread found frontier =
        match frontier with
        | [] -> found
        | sheet :: rest ->
            let fresh =
                childrenOf sheet
                |> List.filter (fun child -> Set.contains child reachable && not (Set.contains child found))
            spread (List.fold (fun found child -> Set.add child found) found fresh) (fresh @ rest)
    spread seed (Set.toList seed)

/// Get the subsheet tree for all sheets in the current project.
/// Returns a map from sheet name to tree of SheetTree nodes.
/// Every sheet is a root here and every node is built: callers that draw one hierarchy, and that
/// can leave part of it unopened, should use getSheetShapes and materialiseTree directly.
let getSheetTreesFiltered (showLibrarySheet: string -> bool) (allowAllInstances: bool) (p:Project): Map<string,SheetTree> =
    let shapes = getSheetShapes showLibrarySheet p.LoadedComponents
    shapes |> Map.map (fun sheet _ -> materialiseTree (fun _ -> true) allowAllInstances shapes sheet)

/// Get the subsheet tree for all sheets in the current project, library sheets included.
let getSheetTrees (allowAllInstances: bool) (p:Project): Map<string,SheetTree> =
    getSheetTreesFiltered (fun _ -> true) allowAllInstances p

/// Which library sheets the Sheets menu and the design hierarchy show: none, unless the developer
/// toggle is on, or the user has asked to look inside that particular component.
let librarySheetsShown (model: Model) (sheet: string) =
    model.ShowLibrarySheets || Set.contains sheet model.OpenedLibrarySheets



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
    stateToJsonString (comp.CanvasState,comp.WaveInfo,Some {
        Form=comp.Form;
        Description=comp.Description
        ParameterDefinitions = comp.LCParameterSlots
        IsTopSheet = Some comp.IsTopSheet})
    |> Result.bind (writeFile comp.FilePath)

/// Drop library sheets that nothing instantiates any more, deleting their files.
/// Run when the project is saved rather than when an instance is deleted: undo restores model
/// snapshots, so deleting the sheet at deletion time would leave undo unable to bring it back.
let sweepUnusedLibrarySheets (model: Model) : Model =
    match model.CurrentProj with
    | None -> model
    | Some project ->
        match ComponentLibraries.unusedLibrarySheets project.LoadedComponents with
        | [] -> model
        | unused ->
            let unusedNames = unused |> List.map (fun ldc -> ldc.Name) |> Set.ofList
            // never remove the sheet the user is looking at, whatever its form - nor one they
            // have opened to look inside, which would otherwise be taken away mid-session by
            // deleting the last instance of it. It is swept on the next save after they put it
            // away, or in the next session, since nothing stays viewed across a project reopen.
            let toRemove =
                unusedNames
                |> Set.remove project.OpenFileName
                |> Set.filter (fun name -> not (Set.contains name model.OpenedLibrarySheets))
            toRemove
            |> Set.iter (fun name ->
                match project.LoadedComponents |> List.tryFind (fun ldc -> ldc.Name = name) with
                | Some ldc -> removeFileWithExtn ".dgm" project.ProjectPath ldc.Name
                | None -> ())
            {model with
                CurrentProj =
                    Some {project with
                            LoadedComponents =
                                project.LoadedComponents
                                |> List.filter (fun ldc -> not (Set.contains ldc.Name toRemove))}}

/// Write every sheet that differs from its file, except the open one, and clear its flag.
///
/// ONLY THE OPEN SHEET MAY BE UNSAVED. A change to one sheet routinely reaches others - binding a
/// parameter writes it on every instance, reconciling ports rewrites the sheets that hold them,
/// setting the top sheet rewrites the flag on all of them - and a closed sheet left waiting for a
/// save is a sheet the user cannot see, did not knowingly edit, and has no reason to save. Several
/// places used to mark such sheets and leave them, so the state existed and had to be handled at
/// project close.
///
/// A library component's sheet belongs to its library and is never written back, whatever asks; it
/// is left alone here and not called unsaved either.
let saveDirtyClosedSheets (openSheet: string) (ldcs: LoadedComponent list) : LoadedComponent list =
    ldcs
    |> List.map (fun ldc ->
        match ldc.LoadedComponentIsOutOfDate
              && ldc.Name <> openSheet
              && not (ComponentLibraries.isLibrarySheet ldc) with
        | false -> ldc
        | true ->
            writeComponentToFile ldc |> ignore
            {ldc with LoadedComponentIsOutOfDate = false; TimeStamp = System.DateTime.Now})

/// Make the named sheet the current top sheet governing parameter display, clearing the flag
/// from every other sheet. The flag is per-sheet view state persisted in the .dgm file.
/// Every sheet but the open one is written at once, so the choice survives without a manual save.
let setTopSheetState (sheetName: string) (model: Model) : Model =
    match model.CurrentProj with
    | None -> model
    | Some project ->
        let updateLdc (ldc: LoadedComponent) =
            let flag = ldc.Name = sheetName
            match flag = ldc.IsTopSheet with
            | true -> ldc
            | false -> {ldc with IsTopSheet = flag; LoadedComponentIsOutOfDate = true}
        let ldcs =
            project.LoadedComponents
            |> List.map updateLdc
            |> saveDirtyClosedSheets project.OpenFileName
        {model with CurrentProj = Some {project with LoadedComponents = ldcs}}
        |> (fun m ->
            match List.exists (fun (ldc: LoadedComponent) -> ldc.LoadedComponentIsOutOfDate) ldcs with
            | true -> Optic.set savedSheetIsOutOfDate_ true m
            | false -> m)

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
                Log.error $"writing a component backup: {err}"
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
            // through FilesIO rather than node directly: these were the only two filesystem calls
            // in the app that bypassed the wrappers, and after contextIsolation there is no node
            // here to bypass them with
            if FilesIO.exists oldPath then
                FilesIO.unlink oldPath
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
                Log.error $"writing a component backup: {err}"
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
            // through FilesIO rather than node directly: these were the only two filesystem calls
            // in the app that bypassed the wrappers, and after contextIsolation there is no node
            // here to bypass them with
            if FilesIO.exists oldPath then
                FilesIO.unlink oldPath
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
    let msgs =
        [
            // First of all, before the canvas is touched: stop holding the sheet being left at
            // what it loaded with, if it was a library component being viewed. Everything below
            // changes the canvas, and a pin still armed would put the old sheet's symbols back as
            // fast as the new sheet's were loaded. PinReadOnlyCanvas at the end of this list arms
            // it again if the sheet now being opened is one too.
            UpdateModel (Optic.set readOnlyBaseline_ None)

            SetHighlighted([], []) // Remove current highlights.

            // Clear the canvas.
            Sheet SheetT.ResetModel
            Sheet (SheetT.Wire BusWireT.ResetModel)
            Sheet (SheetT.Wire (BusWireT.Symbol (SymbolT.ResetModel ) ) )
    
            // Finally load the new state in the canvas.
            SetIsLoading true
    
            //Load components
            Sheet (SheetT.Wire (BusWireT.Symbol (SymbolT.LoadComponents (ldcs,components ))))
    
            Sheet (SheetT.Wire (BusWireT.LoadConnections connections))

            Sheet SheetT.FlushCommandStack // Discard all undo/redo.
            // Run the a connection widths inference.
    
            Sheet (SheetT.Wire (BusWireT.BusWidths))
            // JSdispatch <| InferWidths()
            // Set no unsaved changes.

            Sheet SheetT.UpdateBoundingBoxes

            Sheet (SheetT.Wire (BusWireT.MakeJumps (true, connections |> List.map (fun conn -> conn.Id ))))

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

            // after everything is loaded: ask which top-level sheet governs parameter display,
            // in the rare case where several exist and they disagree about this sheet.
            // FinishUICmd overwrites any popup, so this must come after it.
            CheckTopSheetChoice

            // draw the sheet at the parameter values it takes under that top sheet rather than at
            // its declared defaults. Must follow the choice above, which can change the top.
            PropagateParameters

            // Centre once more when the loaded sheet has actually been PAINTED. Replacing the
            // canvas's content can move its DOM scroll, and that arrives afterwards as an
            // ordinary scroll event which overwrites the fit the ctrl-W above computed - so the
            // last word has to come after the paint, not merely after the messages above. Two
            // animation frames from here is that paint (the fence runWhenPainted documents);
            // NOT dispatched directly after the asynchronous batch, which put it through the
            // queue AHEAD of the list and mid-load (how switching sheets could quietly empty a
            // loaded component), and NOT the RunAfterRender slot, which a competing ask
            // replaces silently. SynchroniseCanvas after the fit, as ever, so the recentring
            // does not count as an edit.
            ExecFuncInMessage(
                (fun _ dispatch ->
                    Browser.Dom.window.requestAnimationFrame (fun _ ->
                        Browser.Dom.window.requestAnimationFrame (fun _ ->
                            dispatch (Sheet (SheetT.KeyPress SheetT.KeyboardMsg.CtrlW))
                            dispatch SynchroniseCanvas)
                        |> ignore)
                    |> ignore),
                dispatch)

            // last of all: if this sheet is a library sheet being viewed, hold it at what it has
            // just become. Everything above changes the canvas as part of loading it, so nothing
            // may be pinned until they have all run.
            PinReadOnlyCanvas

        ]
    //INFO - Currently the spinner will ALWAYS load after 'SetTopMenu x', probably it is the last command in a chain
    //Ideally it should happen before this, but it is not currently doing this despite the async call
    //This will set a spinner for both Open project and Change sheet which are the two most lengthly processes
    dispatch <| (Sheet (SheetT.SetSpinner true))
    dispatch <| SendSeqMsgAsynch msgs


/// Load a new project as defined by parameters.
/// Ends any existing simulation
/// Closes WaveSim if this is being used
let setupProjectFromComponents (finishUI:bool) (sheetName: string) (ldComps: LoadedComponent list) (model: Model) (dispatch: Msg->Unit)=
    // Every instance binds every parameter its sheet declares. A project saved before that was
    // required arrives without it, so it is repaired here - the single funnel every load and every
    // sheet change passes through - rather than guarded against at each reader. Idempotent: it
    // does nothing at all to a project that already holds the invariant, which is all of them
    // after the first load. Repaired sheets are flagged, so PropagateParameters writes them.
    let ldComps = ParameterAnalysis.bindMissingInstanceParams ldComps
    let compToSetup =
        match ldComps with
        | [] -> failwithf "setupProjectComponents must be called with at least one LoadedComponent"
        | comps ->
            // load sheetName
            match comps |> List.tryFind (fun comp -> comp.Name = sheetName) with
            | None -> failwithf "What? can't find sheet %s in loaded sheets %A" sheetName (comps |> List.map (fun c -> c.Name))
            | Some comp -> comp
    // This function is the one funnel for opening a project AND for changing sheet within one, so
    // whether the project is changing has to be asked rather than assumed. It is the project the
    // sheet being opened belongs to that decides, which is where the new Project record below gets
    // its ProjectPath from.
    let leavingProject =
        match model.CurrentProj with
        | Some p -> p.ProjectPath <> dirName compToSetup.FilePath
        | None -> true

    match model.CurrentProj with
    | None -> ()
    | Some p ->
        dispatch EndSimulation // Message ends any running simulation.
        dispatch <|TruthTableMsg CloseTruthTable // Message closes any open Truth Table.
        // NOT unconditional, which is why the plain `dispatch EndWaveSim` that stood here was
        // commented out: a waveform simulation is meant to survive a sheet change, since
        // WaveSimSheet names the sheet being simulated and that need not be the one on screen.
        // It must not survive a change of project. EndWaveSim is the only thing that clears
        // WaveSimSheet, so without this the name of a sheet of the project being left reached
        // the new project's sheet list on the next render of the waveform viewer - which used to
        // raise, killing the UI until the app was reloaded, and now reports a simulation error
        // about a sheet the user cannot see.
        if leavingProject && model.WaveSimSheet <> None && model.WaveSimSheet <> Some "" then
            dispatch EndWaveSim
        // TODO: make each sheet wavesim remember the list of waveforms.

    let savedWaveSim =
        compToSetup.WaveInfo
        |> Option.map (loadWSModelFromSavedWaveInfo ldComps compToSetup.Name)
        |> Option.defaultValue initWSModel

    // Within a project the running simulation's model carries over, so that leaving a sheet and
    // coming back finds the same waves selected. Across projects it cannot: its selected waves
    // name components of a design that is no longer open, so the sheet's own saved WaveInfo - or
    // nothing - is what the new project starts from.
    let waveSim =
        if leavingProject then
            savedWaveSim
        else
            model.WaveSimSheet
            |> Option.bind (fun sheet -> Map.tryFind sheet model.WaveSim)
            |> Option.defaultValue savedWaveSim


    // The load list loadStateIntoModel schedules is the WHOLE of the load: SetProject and
    // SynchroniseCanvas used to be dispatched again here, directly - which sent them through the
    // queue AHEAD of that list. The project was renamed before its canvas existed, and the
    // synchronise then wrote an empty canvas into the newly named sheet. The list already
    // carries both, in order, so the load is the one place that sequence lives.
    loadStateIntoModel finishUI compToSetup waveSim ldComps model dispatch


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
        LCParameterSlots = None
        IsTopSheet = false
    }


/// Write one sheet back exactly as it was read, keeping the time it says it was last saved.
///
/// The ordinary save stamps the moment it is written, which is right for an edit and wrong for a
/// rewrite the user did not ask for: the stamp is what says which sheet they were last working on,
/// and stamping every sheet of a project at once would lose that (see chooseWhichToOpen).
let private writeComponentKeepingTimeStamp (comp: LoadedComponent) =
    let sheetInfo: SheetInfo =
        { Form = comp.Form
          Description = comp.Description
          ParameterDefinitions = comp.LCParameterSlots
          IsTopSheet = Some comp.IsTopSheet }

    stateToJsonStringAt comp.TimeStamp (comp.CanvasState, comp.WaveInfo, Some sheetInfo)
    |> Result.bind (writeFile comp.FilePath)

/// Put a project's files into the current id form when they were read in the old one.
///
/// Ids became integers; a .dgm written before that holds uuids, and every load allocates integers
/// for them afresh. The mapping is deterministic, so the design works either way - but nothing on
/// disk ever changes, so the ids in the file match nothing anyone can see in the running app, and
/// every open pays the conversion again. Opening the project is where it is settled, once.
///
/// The WHOLE project or none of it. Component ids are design-unique, and admitting the sheets
/// re-mints any that collide - so a sheet whose own ids were already integers can still have come
/// out of the load different from its file, and writing only the uuid ones would leave the design
/// half-converted.
///
/// Not for a project Issie may only write nothing to - the shipped demos read from the read-only
/// static directory, a folder on a read-only share. There is no asking in advance whether a
/// directory can be written, so the first refusal is the answer: the write is attempted, and a
/// project that refuses it simply keeps its old ids. Said in the log and nowhere else, because
/// nothing about the design is wrong and there is nothing for the user to do.
///
/// A library sheet belongs to its library and is never written back, whatever asks.
let convertProjectIdsOnDisk (ldcs: LoadedComponent list) : LoadedComponent list =
    if not (ldcs |> List.exists (fun ldc -> ldc.LoadedComponentIsOutOfDate)) then
        ldcs
    else

    let writable, refused =
        ldcs
        |> List.filter (fun ldc -> not (ComponentLibraries.isLibrarySheet ldc))
        |> List.fold
            (fun (written, refused) ldc ->
                match refused with
                // one refusal answers for the project: they are all in the same directory
                | Some _ -> written, refused
                | None ->
                    match writeComponentKeepingTimeStamp ldc with
                    | Ok() -> ldc.Name :: written, None
                    | Error message -> written, Some message)
            ([], None)

    match refused with
    | Some message -> Log.dbg Log.Files $"project ids left in their old form - it cannot be written: {message}"
    | None -> Log.dbg Log.Files ("project ids written in their new form on: " + String.concat ", " (List.rev writable))

    // Settled either way. A project that could not be written keeps its old ids, and saying so at
    // every close - "these sheets have unsaved changes" - would be telling the user about something
    // they did not do and cannot act on; the sheets themselves are exactly as they were read.
    ldcs |> List.map (fun ldc -> { ldc with LoadedComponentIsOutOfDate = false })

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
    | [] ->
        // components is accumulated in reverse of the order files were read: scan in read order so
        // that the same sheet keeps its ids each time the project is opened.
        // admitDesign seeds the id allocators from the loaded sheets and re-mints anything that
        // breaks an invariant - a component id used twice across the design, or any id that is
        // duplicated within its sheet. An in-memory change only: files pick the corrected ids
        // up as the user saves them normally.
        // Renumbering is ROUTINE here, not a defect to report: every sheet of a project saved
        // before ids were integers converts to per-sheet 1..n and is renumbered into the
        // design's namespace on its way in, silently - the old duplicate-uuid popup would have
        // greeted every legacy project. Files pick the new ids up as they are saved.
        let ldcs, corrected = RegenerateIds.admitDesign (List.rev components)

        match corrected with
        | [] -> ()
        | names -> Log.dbg Log.Files ("project open renumbered ids on: " + String.concat ", " names)

        // and where the files still hold the ids of before that move, they are written as they now
        // are - so the conversion happens once rather than on every open
        let ldcs = convertProjectIdsOnDisk ldcs

        setupProjectFromComponents false (chooseWhichToOpen ldcs) ldcs model dispatch
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

/// Put a recent list in the form it is held and compared in: every path written this platform's
/// way, each project once, and no more of them than the limit. Applied on every read as well as on
/// every write, since a list saved by an earlier version is still in IssieSettings.json and nothing
/// else would ever tidy it.
///
/// The normalising is what makes the entries comparable at all. The same folder arrives written
/// both ways - a dialog and the demo copier give backslashes on Windows, a path built by pathJoin
/// or read back out of the settings file gives forward slashes - and a list that compared the
/// strings therefore held one entry per SPELLING: the same project twice, taking two of the five
/// places, and a cross that removed only the spelling that was clicked.
let private trimRecents (recents: string list) =
    recents
    |> List.map normalisePath
    // newest first, so the first of two spellings of one project is the one to keep
    |> List.distinct
    |> List.truncate Constants.numberOfRecentProjects

/// A recent list as it came out of the settings file, in the form the rest of the code holds it in.
/// Settings written before paths were normalised have the same project under two spellings, and one
/// written by an older version can be longer than the limit; tidying on the way IN means the next
/// write of the settings file heals it, rather than leaving that to the next project opened.
let tidyRecents (recents: string list option) : string list option = recents |> Option.map trimRecents

/// Take one project off a recent list, however its path is written.
let private withoutRecent (path: string) (recents: string list option) =
    // trimmed BEFORE the filter, not after: the filter is what has to see normalised paths, or a
    // click on the list removes nothing and the entry comes back normalised
    recents |> Option.map (trimRecents >> List.filter ((<>) (normalisePath path)))

let addToRecents path recents =
    let path = normalisePath path

    recents
    |> Option.defaultValue []
    |> trimRecents
    |> List.filter ((<>) path)
    |> List.insertAt 0 path
    // trimmed after the insert, not before it: truncating first left room for the new entry to
    // make the list one longer than the limit, every time
    |> trimRecents
    |> Some

/// The recent projects to offer, newest first. Read the list through this rather than from
/// UserData directly.
let recentProjects (model: Model) : string list =
    model.UserData.RecentProjects
    |> Option.defaultValue []
    |> trimRecents

/// Take one project off the recent list. The project itself is untouched.
let forgetRecentProject (path: string) (model: Model) dispatch =
    dispatch <| SetUserData {
        model.UserData with
            RecentProjects =
                model.UserData.RecentProjects |> withoutRecent path
        }

/// One row of a recent-projects list: the project, which opens on a click, and a cross that takes
/// it off the list.
///
/// Only opening a project ever put one here, and only opening four others ever took it away - so a
/// project opened once by mistake, or one whose folder has since been moved or deleted, sat at the
/// top of the list of five for as long as it took to open five more. The cross is worded and
/// coloured as removal from a list rather than deletion, because that is all it is: nothing on
/// disk changes.
let recentProjectItem (path: string) (label: ReactElement) (openIt: string -> unit) model dispatch =
    Menu.Item.li
        [ Menu.Item.IsActive false
          Menu.Item.OnClick (fun _ -> openIt path) ]
        [ div [Class "recentItem"] [
            // both overflow axes hidden: hiding only one makes CSS force the other to auto, which
            // puts a scrollbar on every row
            div [ HTMLAttr.Title path
                  Style [ Flex "1"; MinWidth "0"
                          OverflowX OverflowOptions.Hidden
                          OverflowY OverflowOptions.Hidden
                          TextOverflow "ellipsis"
                          WhiteSpace WhiteSpaceOptions.Nowrap ] ]
                [ label ]
            span [ Class "recentForget"
                   HTMLAttr.Title $"Remove '{baseName path}' from this list. The project itself is \
                                    not deleted."
                   OnClick (fun ev ->
                        // the row underneath this opens the project: forgetting one must not also
                        // be the last time it is opened
                        ev.stopPropagation()
                        forgetRecentProject path model dispatch) ]
                [ str "✕" ]
          ] ]

/// open an existing demo project from its path
let openDemoProjectFromPath (path:string) model dispatch =

    warnAppWidth dispatch (fun _ ->

        Log.dbg Log.Files $"loading demo project {path}"
        match loadAllComponentFiles path with
        | Error err ->
            Log.error err
            displayFileErrorNotification err dispatch

        | Ok (componentsToResolve: LoadStatus list) ->
            resolveComponentOpenPopup path [] componentsToResolve model dispatch
            Log.dbg Log.Files $"opened project {path}"

    )

/// open an existing project from its path
let openProjectFromPath (path:string) model dispatch =
    warnAppWidth dispatch (fun _ ->
    dispatch (ExecFuncAsynch <| fun () ->
        Log.dbg Log.Files $"loading project {path}"
        match loadAllComponentFiles path with
        | Error err ->
            Log.error err
            displayFileErrorNotification err dispatch
            model.UserData.RecentProjects |> withoutRecent path
        | Ok (componentsToResolve: LoadStatus list) ->
            resolveComponentOpenPopup path [] componentsToResolve model dispatch
            Log.dbg Log.Files $"opened project {path}"
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
        Log.warn $"updateLoadedComponents cannot find a sheet named '{name}'"
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
    // A library component being looked at is not the user's to change and is held at what it
    // loaded with, so there is nothing here to take back - and this writes a backup .dgm every
    // time it is called, which is on every sheet switch.
    | _ when openSheetIsReadOnly model -> model.CurrentProj
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
                    // components deleted from the canvas must not leave parameter slots behind,
                    // and a renamed one must not leave its slot describing the old name
                    LCParameterSlots = CanvasExtractor.tidyParamSlots canvas lc.LCParameterSlots
                }
            model.CurrentProj
            |> Option.map (fun p -> 
                {
                    p with LoadedComponents = updateLoadedComponents p.OpenFileName setLc p.LoadedComponents dispatch
                })


/// extract SavedWaveInfo from model to be saved.
///
/// Takes the design to walk rather than reading it off the project, because the selection is saved
/// as label paths and the project's copy of the sheet being saved is the version before the save -
/// see designWithSheet, which is what every caller passes.
let getSavedWave (ldcs: LoadedComponent list) (model: Model) : SavedWaveInfo option =
    match currWaveSimModel model, getCurrFile model with
    | Some wsModel, Some sheet -> Some(getSavedWaveInfo ldcs sheet wsModel)
    | _ -> None

/// Save the sheet currently open, return  the new sheet's Loadedcomponent if this has changed.
/// Do not change model.
/// update Symbol model with new RAM contents.
let saveOpenFileAction isAuto model (dispatch: Msg -> Unit)=
    match model.Sheet.GetCanvasState (), model.CurrentProj with
    // A library component's sheet is never written back, whatever asks: it belongs to the
    // library, not to this project, and it cannot have changed anyway.
    | _ when openSheetIsReadOnly model -> None
    | _, None -> None
    | canvasState, Some project ->
        // "DEBUG: Saving Sheet"
        let ldc = project.LoadedComponents |> List.find (fun lc -> lc.Name = project.OpenFileName)
        // slots of components deleted from the canvas must not be saved
        let sheetInfo: SheetInfo = {Form = ldc.Form; Description = ldc.Description ; ParameterDefinitions= CanvasExtractor.tidyParamSlots canvasState ldc.LCParameterSlots; IsTopSheet = Some ldc.IsTopSheet} //only user defined sheets are editable and thus saveable
        let design = designWithSheet project project.OpenFileName canvasState
        let savedState = canvasState, getSavedWave design model,(Some sheetInfo)
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
                |> Option.map (getSavedWaveInfo design project.OpenFileName)
            let (SheetInfo:SheetInfo option) =
                match origLdComp.Form with
                |None -> None
                |Some form -> Some {Form=Some form;Description=origLdComp.Description; ParameterDefinitions=origLdComp.LCParameterSlots; IsTopSheet = Some origLdComp.IsTopSheet}
            let (newLdc, ramCheck) = makeLoadedComponentFromCanvasData canvasState origLdComp.FilePath DateTime.Now savedWaveSim SheetInfo
            let newState =
                canvasState
                |> (fun (comps, conns) -> 
                        comps
                        |> List.map (fun comp -> 
                            match List.tryFind (fun (c:Component) -> c.Id=comp.Id) ramCheck with
                            | Some newRam -> 
                                // TODO: create consistent helpers for messages
                                dispatch <| Sheet (SheetT.Wire (BusWireT.Symbol (SymbolT.WriteMemoryType (comp.Id, newRam.Type))))
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
    // as saveOpenFileAction: a library component's sheet is never written back
    | _ when openSheetIsReadOnly model -> None
    | _, None -> None
    | canvasState, Some project ->
        // "DEBUG: Saving Sheet"
        let ldc = project.LoadedComponents |> List.find (fun lc -> lc.Name = project.OpenFileName)
        // slots of components deleted from the canvas must not be saved
        let sheetInfo: SheetInfo = {Form = ldc.Form; Description = ldc.Description; ParameterDefinitions= CanvasExtractor.tidyParamSlots canvasState ldc.LCParameterSlots; IsTopSheet = Some ldc.IsTopSheet} //only user defined sheets are editable and thus saveable
        let design = designWithSheet project project.OpenFileName canvasState
        let savedState = canvasState, getSavedWave design model,(Some sheetInfo)
        saveStateToFile project.ProjectPath project.OpenFileName savedState |> ignore
        removeFileWithExtn ".dgmauto" project.ProjectPath project.OpenFileName
        let origLdComp =
            project.LoadedComponents
            |> List.find (fun lc -> lc.Name = project.OpenFileName)
        let savedWaveSim =
            Map.tryFind project.OpenFileName model.WaveSim
            |> Option.map (getSavedWaveInfo design project.OpenFileName)
        let (SheetInfo:SheetInfo option) =
            match origLdComp.Form with
            |None -> None
            |Some form -> Some {Form=Some form;Description=origLdComp.Description; ParameterDefinitions=origLdComp.LCParameterSlots; IsTopSheet = Some origLdComp.IsTopSheet}
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
                            SymbolUpdate.writeMemoryType sModel (comp.Id) (newRam.Type), (newRam :: newComps)                            
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
            let sheetInfo = {Form=comp.Form;Description=comp.Description; ParameterDefinitions= comp.LCParameterSlots; IsTopSheet = Some comp.IsTopSheet}
            let savedState = comp.CanvasState, None, Some sheetInfo
            match saveStateToFileExperimental project.ProjectPath comp.Name savedState with
            | Ok _ -> Log.dbg Log.Files $"saved {comp.Name}"
            | Error errr -> Log.error $"saving {comp.Name}: {errr}")

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
        Log.warn $"sheet {name}.dgm is not in the project"
        SetFilesNotification <| errorFilesNotification 
           $"Warning: Issie could not find the file '{name}.dgm' in the project. Did you delete a file manually?"
        |> dispatch
        dispatch FinishUICmd
    | Some {Form=Some (ProtectedTopLevel | ProtectedSubSheet)} when debugLevel = 0 ->
        SetFilesNotification <| errorFilesNotification
            $"Warning: The sheet '{name}' is protected and cannot be opened."
        |> dispatch
        dispatch FinishUICmd
    // A library sheet is kept out of sight everywhere else - the Sheets menu, the design
    // hierarchy, the waveform simulator - so it can be opened only by asking to view it, which
    // puts it in OpenedLibrarySheets and makes it read-only for as long as the project is open.
    // The guard belongs here rather than at the callers: this is the single funnel every way of
    // opening a sheet passes through. The developer toggle still lets it through, as it does
    // everywhere else.
    | Some lc when
        ComponentLibraries.isLibrarySheet lc
        && not model.ShowLibrarySheets
        && not (Set.contains name model.OpenedLibrarySheets) ->
        SetFilesNotification <| errorFilesNotification
            $"'{name}' is part of a component library, so it cannot be opened."
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
        //printSheetNames {model with CurrentProj = Some project'}
        openFileInProject' false project'.LoadedComponents[0].Name project' model dispatch
    | nc, false ->
        // nothing chnages except LoadedComponents
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




