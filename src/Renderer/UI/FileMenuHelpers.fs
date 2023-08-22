module FileMenuHelpers
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
open Optics.Operators
open System



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
            printfn $"Select {key}"
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
                  Menu.Item.OnClick (fun _ -> onSelect key) ] react 

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

            
        
    

/// Get the subsheet tree for all sheets in the current project.
/// Returns a map from sheet name to tree of SheetTree nodes
let getSheetTrees (p:Project): Map<string,SheetTree> =
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
                    SubSheets = subs
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
       
