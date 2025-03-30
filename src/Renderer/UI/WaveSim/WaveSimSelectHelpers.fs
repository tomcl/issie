module WaveSimSelectHelpers

//---------------------------------------------------------------------------------------//
//-------------Waveform Selection Popup and RAM Selection Helpers------------------------//
//---------------------------------------------------------------------------------------//

// Functions to make modal popups that allows waveforms and RAMs
// to be selected or deselected for display in the waveform simulator.

open Fable.React
open Fable.React.Props
open Fulma
open Fulma.Extensions.Wikiki
open JSHelpers
open NumberHelpers
open ModelType
open CommonTypes
open MemoryEditorView
open PopupHelpers
open UIPopups
open Notifications
open Sheet.SheetInterface
open DrawModelType
open FilesIO
open CatalogueView
open TopMenuView
open MenuHelpers
open MiscMenuView
open Constants
open Browser.Types
open WaveSimStyle
open WaveSimHelpers
open SimGraphTypes
open SimTypes
open DiagramStyle

// -----------------------------------------
// Helper Functions & Filtering Logic
// -----------------------------------------

module Constants =
    let numPortColumns = 4

type TableRow = TableRow of ReactElement

let waveRowIProps props rowItems : TableRow =
    tr props (rowItems |> List.map (fun cell -> td [Style [BorderStyle "none"]] [ cell ]))
    |> TableRow

let waveRow props rowItems = waveRowIProps [Style props] rowItems
    

let wavePropsTable (rows: TableRow list) =
    table [ Style [ Background "#f0f0f0"] ] [
        tbody
            [Style []]
            (rows |> List.map (function | TableRow row -> row))
        ]

    

/// Ensures that only valid waves (and selected waves) are returned.
let ensureWaveConsistency (ws: WaveSimModel) =
    let fs = Simulator.getFastSim()
    let okWaves =
        Map.values ws.AllWaves
        |> Seq.toList
        |> List.filter (fun wave -> Map.containsKey wave.WaveId.Id fs.WaveComps)
    if okWaves.Length <> ws.AllWaves.Count then
        printfn "EnsureWaveConsistency: waves,Length=%d, ws.AllWaves.Count=%d" okWaves.Length ws.AllWaves.Count
    let okSelectedWaves =
        ws.SelectedWaves |> List.filter (fun selW -> Map.containsKey selW ws.AllWaves)
    if okSelectedWaves.Length <> ws.SelectedWaves.Length then
        printfn "ok selected waves length = %d <> selectedwaves length = %d" okSelectedWaves.Length ws.SelectedWaves.Length
    okWaves, okSelectedWaves

let getComponentName (wave: Wave) =
    let fs = Simulator.getFastSim()
    let comp = Simulator.getFastSim().WaveComps.[wave.WaveId.Id]
    match comp.FType with
    | Not -> "NOT"
    | Mux2 -> "MUX2"
    | Demux2 -> "DEMUX2"
    | DFF -> "DFF"
    | DFFE -> "DFFE"
    | Register _ -> "REGISTER"
    | RegisterE _ -> "REGISTER-E"
    | ROM1 _ -> "ROM"
    | RAM1 _ -> "RAM"
    | AsyncROM1 _ -> "ASYNC-ROM"
    | AsyncRAM1 _ -> "ASYNC-RAM"
    | Custom _ -> "CUSTOM"
    | Input1 _ -> "INPUT"
    | Output _ -> "OUTPUT"
    | Constant1 _ -> "CONSTANT"
    | BusSelection _ -> "BUS-SELECT"
    | IOLabel -> "IO-LABEL"
    | _ -> comp.FType.ToString().ToUpperInvariant()

let isSubSheetOf (subSheetId: string) (sheets: string list) =
    let fs = Simulator.getFastSim()
    let rec isSubSheetOf' subSheetId =
        List.contains subSheetId sheets ||
        match fs.SimSheetStructure[subSheetId] with
        | None -> false
        | Some parent -> isSubSheetOf' parent.SimSheetName
    isSubSheetOf' subSheetId

let updateSheetString (newSheetName: string) (ws: WaveSimModel) =
    let s = ws.SheetSearchString.Trim().ToUpperInvariant()
    if s.EndsWith "*" then
        newSheetName + "*"
    else
        newSheetName

/// Filtering function that applies an AND operation across four search criteria.
/// OfSheet is used to return the waves that match the sheet box
/// All returns all filtered waves without any sheet filtering.
let filterWaves (wsModel: WaveSimModel) =
    let fs = Simulator.getFastSim()
    let waves, okSelectedWaves = ensureWaveConsistency wsModel
    let matchWithBox (searchString: string) (matcher:string) =
        let s = searchString.Trim().ToUpperInvariant()
        s = "" || s = "*" || matcher.ToUpperInvariant().Contains s

    let searchFilteredWaves =
        waves
        |> List.filter (fun wave -> 
            matchWithBox wsModel.ComponentSearchString wave.CompLabel
            && matchWithBox wsModel.PortSearchString wave.PortLabel
            && matchWithBox wsModel.WaveSearchString wave.ViewerDisplayName
            && matchWithBox wsModel.ComponentTypeSearchString (getComponentName wave)
        )
    let sheetBox = wsModel.SheetSearchString.Trim().ToUpperInvariant()
    let sheet = sheetBox.TrimEnd '*'
    let allSubSheets = sheetBox.EndsWith "*"
    let allSheets =
        fs.SimSheetStructure.Keys
        |> Seq.toList

    let filteredSheets =
        allSheets
        |> List.tryPick (fun sheet' -> if sheet' = sheet then Some [sheet] else None)
        |> Option.defaultValue (List.filter (fun (sheetId:string) -> sheetId.Contains sheet) allSheets)

    let searchSheets =
        allSheets
        |> List.filter (fun sheet -> List.contains sheet filteredSheets ||
                                     (allSubSheets && isSubSheetOf sheet filteredSheets))

    let sheetFilteredWaves =
        searchFilteredWaves
        |> List.filter (fun wave -> List.contains wave.SheetId searchSheets)

    {| All = searchFilteredWaves; Sheets = searchSheets; OfSheet = sheetFilteredWaves|}



// -----------------------------------------
// Search Box UI Components
// -----------------------------------------

/// A style to add some margin between search boxes.
let searchBoxContainerStyle = Style [ MarginRight "10px" ]

/// Search box for wave names.
let waveSearchBox (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    div [ searchBoxContainerStyle ] [
        Input.text [
            Input.Option.Props [ Style [ MarginBottom "1rem"; Width "100%" ] ]
            Input.Option.Placeholder "Search wave names..."
            Input.Option.OnChange (fun value -> 
                dispatch (UpdateWSModel (fun wsm ->
                    { wsm with
                        WaveSearchString = value.Value.ToUpper()
                        ComponentSearchString = "" // Clear component search when wave search changes.
                        PortSearchString = ""        // Clear port search when wave search changes.
                    }
                ))
            )
        ]
    ]



/// Search box for sheet names.
let sheetSearchBox (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    div [ searchBoxContainerStyle ] [
        Input.text [
            Input.Option.Value wsModel.SheetSearchString  // Bind current value
            Input.Option.Props [ Style [ MarginBottom "1rem"; Width "100%" ] ]
            Input.Option.Placeholder "Search sheet names..."
            Input.Option.OnChange (fun value ->
                dispatch (UpdateWSModel (fun wsm -> 
                    { wsm with SheetSearchString = value.Value.ToUpper() }
                ))
            )
        ]
    ]

/// Checkbox to select all subsheets.
let selectAllSubsheetsBox (ws:WaveSimModel) dispatch =
    let s = ws.SheetSearchString
    div [ Style [ MarginLeft "15px"; Display DisplayOptions.Flex; AlignItems AlignItemsOptions.Center; MarginBottom "20px" ] ] [
        Checkbox.checkbox [] [
            Checkbox.input [
                Props [
                    Checked (s.EndsWith "*")
                    OnChange (fun _ ->
                        let newSearch =
                            if s.EndsWith "*" then s.TrimEnd('*')
                            else s + "*"
                        dispatch (UpdateWSModel (fun ws -> { ws with SheetSearchString = newSearch }))
                    )
                ]
            ]
            str "All Subsheets"
        ]
    ]


/// Search box for component names.
let componentSearchBox (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    div [ searchBoxContainerStyle ] [
        Input.text [
            Input.Option.Value wsModel.ComponentSearchString
            Input.Option.Props [ Style [ MarginBottom "1rem"; Width "100%" ] ]
            Input.Option.Placeholder "Search component names..."
            Input.Option.OnChange (fun value ->
                dispatch (UpdateWSModel (fun wsm -> { wsm with ComponentSearchString = value.Value.ToUpper() }))
            )
        ]
    ]

/// Search box for port names.
let portSearchBox (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    div [ searchBoxContainerStyle ] [
        Input.text [
            Input.Option.Value wsModel.PortSearchString
            Input.Option.Props [ Style [ MarginBottom "1rem"; Width "100%" ] ]
            Input.Option.Placeholder "Search port names..."
            Input.Option.OnChange (fun value ->
                dispatch (UpdateWSModel (fun wsm -> { wsm with PortSearchString = value.Value.ToUpper() }))
            )
        ]
    ]



// -----------------------------------------
// Breadcrumb Display
// -----------------------------------------

/// Displays a breadcrumb of sheets based on the current search and wave matches.
let waveSelectBreadcrumbs
        (wsModel: WaveSimModel)
        (filteredWaves: {| All: Wave list; Sheets: string list; OfSheet: Wave list|})
        (dispatch: Msg -> unit)
        (model: Model) : ReactElement =
    match model.CurrentProj with
    | None -> div [] [ str "No project open" ]
    | Some project ->
        let fs = Simulator.getFastSim()
        let updatedProject = ModelHelpers.getUpdatedLoadedComponents project model
        let updatedModel = { model with CurrentProj = Some updatedProject }
        // Extract sheet names from wave names.
        let sheetCounts =
            filteredWaves.All |> List.countBy (fun wave -> wave.SheetLabel)
        let sheetColor (sheet: SheetTree) =
            let sheetName = sheet.SimName fs
            let sheetSearch = wsModel.SheetSearchString.Trim().ToUpperInvariant()
            // Collect the other search strings.
            if List.contains sheetName filteredWaves.Sheets then
                IColor.IsCustomColor "pink"
            else
                IColor.IsCustomColor "darkslategrey"
        let sheetMatches (sheet: SheetTree) =
            match List.tryFind (fun (name, _) -> name = sheet.SimName fs) sheetCounts with
            | Some (_, count) -> count
            | None -> 0
        let updateSearchStringHelper (sheet: SheetTree) : (Msg -> unit) -> unit =
            fun dispatch ->
                dispatch (UpdateWSModel (fun ws -> { ws with SheetSearchString = updateSheetString (sheet.SimName fs) ws}))
        let sheetName (node: SheetTree) =
            node.SimName fs
        let breadcrumbConfig = { 
            MiscMenuView.Constants.defaultConfig with
                ClickAction = updateSearchStringHelper
                ColorFun = sheetColor
                NoWaves = sheetMatches
                AllowDuplicateSheets = true
                BreadcrumbText = Some sheetName
        }
        let breadcrumbs = [
            div [ Style [ TextAlign TextAlignOptions.Center; FontSize "20px" ] ] [ str "Design Hierarchy: click to filter" ]
            MiscMenuView.hierarchyBreadcrumbs breadcrumbConfig dispatch updatedModel
        ]
        div [] breadcrumbs

// -----------------------------------------
// Info Button (for the modal header)
// -----------------------------------------

let infoButton : ReactElement =
    div [
        HTMLAttr.ClassName (sprintf "%s %s %s %s" Tooltip.ClassName Tooltip.IsMultiline Tooltip.IsInfo Tooltip.IsTooltipRight)
        Tooltip.dataTooltip "Find ports by any part of their name. '.' = show all. '*' = show selected. '-' = collapse all"
        Style [ FontSize "25px"; MarginTop "0px"; MarginLeft "10px"; Float FloatOptions.Left ]
    ] [ str Constants.infoSignUnicode ]

// -----------------------------------------
// Wave Selection UI (Left Column)
// -----------------------------------------

// The following functions (toggleSelectAll, toggleWaveSelection, etc.) handle the UI for selecting/deselecting waves.
// (Note: helper functions such as summaryProps, subSheetsToNameReact, isWaveSelected, checkboxInputProps,
//  wavesToIds, details/summary helpers, getCompGroup, GroupItem, summaryName, SheetItem are assumed to exist.)

let toggleSelectAll (selected: bool) (wsModel: WaveSimModel) (dispatch: Msg -> unit) : unit =
    let start = TimeHelpers.getTimeMs ()
    let selectedWaves = if selected then Map.keys wsModel.AllWaves |> Seq.toList else []
    dispatch (GenerateWaveforms { wsModel with SelectedWaves = selectedWaves })
    |> TimeHelpers.instrumentInterval "toggleSelectAll" start

let selectAll (wsModel: WaveSimModel) (dispatch: Msg -> unit) =
    let allWavesSelected = Map.forall (fun index _ -> isWaveSelected index wsModel) wsModel.AllWaves
    tr (summaryProps false (SheetItem []) wsModel dispatch) [
        th [] [
            Checkbox.checkbox [] [
                Checkbox.input [
                    Props (checkboxInputProps @ [
                        Checked allWavesSelected
                        OnChange (fun _ -> toggleSelectAll (not allWavesSelected) wsModel dispatch)
                    ])
                ]
            ]
        ]
        th [] [ str "Select All" ]
    ]

let toggleWaveSelection (index: WaveIndexT) (wsModel: WaveSimModel) (dispatch: Msg -> unit) =
    let selectedWaves =
        if List.contains index wsModel.SelectedWaves then
            List.except [index] wsModel.SelectedWaves
        else
            index :: wsModel.SelectedWaves
    let wsModel' = { wsModel with SelectedWaves = selectedWaves }
    dispatch (GenerateWaveforms wsModel')

let toggleSelectSubGroup (wsModel: WaveSimModel) (dispatch: Msg -> unit) (selected: bool) (waves: WaveIndexT list) =
    let comps = (Simulator.getFastSim()).WaveComps
    let selectedWaves =
        if selected then
            let wavesWithMinDepth =
                if waves = [] then [] else
                    waves
                    |> List.groupBy (fun wave -> comps.[wave.Id].AccessPath.Length)
                    |> List.sort
                    |> List.head
                    |> snd
            List.append wsModel.SelectedWaves wavesWithMinDepth
        else
            List.except waves wsModel.SelectedWaves
    dispatch (GenerateWaveforms { wsModel with SelectedWaves = selectedWaves })

let checkboxRow (wsModel: WaveSimModel) (dispatch: Msg -> unit) (index: WaveIndexT) : TableRow =
    let fontStyle = if isWaveSelected index wsModel then boldFontStyle else normalFontStyle
    let wave = wsModel.AllWaves.[index]
    waveRow fontStyle [
            Checkbox.checkbox [] [
                Checkbox.input [
                    Props (checkboxInputProps @ [
                        OnChange (fun _ -> toggleWaveSelection index wsModel dispatch)
                        Checked (isWaveSelected index wsModel)
                    ])
                ]
            ]
            str wave.DisplayName 
        ]

let checkBoxItem wsModel isChecked waveIds dispatch =
    Checkbox.checkbox [] [
        Checkbox.input [
            Props [
                Checked isChecked
                OnChange (fun _ -> toggleSelectSubGroup wsModel dispatch (not isChecked) waveIds)
            ]
        ]
    ]

let waveCheckBoxItem (wsModel: WaveSimModel) (waveIds: WaveIndexT list) dispatch =
    let comps = (Simulator.getFastSim()).WaveComps
    let minDepthSelectedWaves =
        if waveIds = [] then [] else
            waveIds
            |> List.groupBy (fun waveId -> comps.[waveId.Id].AccessPath.Length)
            |> List.sort
            |> List.head
            |> snd
    let checkBoxState = List.exists (fun w -> List.contains w wsModel.SelectedWaves) minDepthSelectedWaves
    Checkbox.checkbox  [] [
        Checkbox.input [
            Props [
                Checked checkBoxState
                OnChange (fun _ -> toggleSelectSubGroup wsModel dispatch (not checkBoxState) waveIds)
                Style [MarginRight "10px"]
            ]
        ]
    ]

                        

let makePortRow (ws: WaveSimModel) (dispatch: Msg -> unit) (waves: Wave list) =
    let wave =
        match waves with
        | [wave] -> wave
        | _ -> failwithf "Expected a single wave in port row; got %d" waves.Length
    waveRow [] [
        waveCheckBoxItem ws [wave.WaveId] dispatch 
        str wave.PortLabel 
        str (match wave.WaveId.PortType with | PortType.Output -> "Output" | PortType.Input -> "Input") 
    ]

let makeSelectionGroup
        showDetails
        (ws: WaveSimModel)
        (dispatch: Msg -> unit)
        (summaryItem: ReactElement)
        (rows: TableRow list)
        (cBox: CheckBoxStyle)
        (waves: Wave list) =
    let wi = wavesToIds waves
    waveRowIProps (summaryProps false cBox ws dispatch) [
        waveCheckBoxItem ws wi dispatch 
            
        details (detailsProps showDetails cBox ws dispatch) [
                    summary (summaryProps true cBox ws dispatch) [ summaryItem ]
                    wavePropsTable rows]
                
                
        ]
            
        
    

/// Type to output the selected waves and a flag for whether to show detailed view.
type WaveSelectionOutput = {
    WaveList: Wave list
    ShowDetails: bool
}

/// Top-level function to select and filter waves for display.
/// Uses the filtering logic from `filterWaves`.
let selectWaves (ws: WaveSimModel) (waves: Wave list) (dispatch: Msg -> unit) : WaveSelectionOutput =
    let waves = List.sortBy (fun wave -> wave.ViewerDisplayName) waves
    let showDetails = ((List.length waves < 20) || (ws.WaveSearchString.Length > 0))
                        && (ws.WaveSearchString <> "-")
    { WaveList = waves; ShowDetails = showDetails }

let makeFlatGroupRow
        (portsPerRow: int)
        showDetails (ws: WaveSimModel)
        (dispatch: Msg -> unit)
        (subSheet: string list)
        (grp: ComponentGroup)
        (wavesInGroup: Wave list) =
    let cBox = GroupItem (grp, subSheet)
    let summaryReact = summaryName ws cBox subSheet wavesInGroup
    let rowItems =
        wavesInGroup
        |> List.groupBy (fun (wave:Wave) -> wave.CompLabel)
        |> List.collect (fun (comp, waves) ->
            waves
            |> List.mapi (fun i wave -> {|W=wave; Column = i|})
            |> List.groupBy (fun waves -> waves.Column / portsPerRow)
            |> List.map (fun (portCol, waves) ->
                let isFirst = waves[0].Column = 0
                let portCells (wave:{|W:Wave;Column:int|}) =
                    let isSelected = isWaveSelected wave.W.WaveId ws
                    let fontStyle = if isSelected then boldFontStyle else normalFontStyle

                    [
                        div [Style [Display DisplayOptions.Flex; AlignItems AlignItemsOptions.Center ]] [
                            input [
                                Type "Checkbox"
                                OnChange (fun _ -> toggleWaveSelection wave.W.WaveId ws dispatch)
                                Checked isSelected
                                Style (MarginLeft "10px" :: MarginRight "5px" :: fontStyle)
                            ]
                            p [Style (MarginRight "10px" :: fontStyle)] [str $"{wave.W.PortLabel}"]
                        ]
                    ]
                let compNameCell =
                    p
                        [Style ([MarginRight "10px"; MarginLeft "10px"; Color "blue"] @ boldFontStyle)]
                        [str <| if isFirst then $"{waves[0].W.CompLabel}" else ""]

                let portNameCells =
                    [0..portsPerRow-1]
                    |> List.collect (fun i ->
                        List.tryItem i waves
                        |> function | None -> [str ""] | Some wave -> portCells wave)
                waveRow [] (compNameCell :: portNameCells)))

                    
            
    makeSelectionGroup showDetails ws dispatch summaryReact rowItems cBox wavesInGroup

let makeFlatList
        (ws: WaveSimModel)
        (dispatch: Msg -> unit)
        (subSheet: string list)
        (waves: Wave list)
        (showDetails: bool) =
    let fs = Simulator.getFastSim()
    let groupedBySubSheet =
        waves
        |> List.groupBy (fun w -> w.SheetId)

    let subSheetRows =
        groupedBySubSheet
        |> List.map (fun (subSheetName, wavesInSubSheet) ->
            let componentGroups =
                wavesInSubSheet |> List.groupBy (fun wave -> getCompGroup fs wave)
            let groupRows =
                componentGroups
                |> List.map (fun (grp, groupWaves) ->
                    makeFlatGroupRow Constants.numPortColumns showDetails ws dispatch [] grp groupWaves
                )
            makeSelectionGroup showDetails ws dispatch (str subSheetName) groupRows (SheetItem [subSheetName]) wavesInSubSheet
        )

    wavePropsTable subSheetRows
    

let renderwaves (ws: WaveSimModel) (dispatch: Msg -> unit) (waveselect: WaveSelectionOutput) : ReactElement =
    let showDetails = waveselect.ShowDetails
    let wavelist = waveselect.WaveList
    // Use the new flat approach
    makeFlatList ws dispatch [] wavelist showDetails

// -----------------------------------------
// Modal Display for Wave Selection
// -----------------------------------------

/// Displays the modal for wave selection. The top row shows the serach boxes.
/// Below a two‑column grid shows the wave selection (left) and breadcrumbs (right).
let selectWavesModal (wsModel: WaveSimModel) (dispatch: Msg -> unit) (model: Model) : ReactElement =
    // Helper to close the modal and reset search string.
    let resetSearchFilters (ws: WaveSimModel) =
        { ws with 
             WaveSearchString = ""
             SheetSearchString = ""
             ComponentSearchString = ""
             PortSearchString = ""
             ComponentTypeSearchString = ""
             HighlightedSheets = Set.empty
        }

    let closeModal () =
        dispatch (UpdateWSModel (fun ws -> 
            resetSearchFilters { ws with WaveModalActive = false }
        ))

    // Handler for closing the modal (with confirmation if >50 waves are selected).
    let handleModalClose _ =
        let numWaves = List.length wsModel.SelectedWaves
        if numWaves > 50 then
            UIPopups.viewWaveSelectConfirmationPopup
                50
                numWaves
                (fun finish _ ->
                    dispatch ClosePopup
                    if finish then closeModal ())
                dispatch
        else
            closeModal ()
        // Always reset the search string.
        dispatch (UpdateWSModel (fun ws -> { ws with SearchString = "" }))

    if not wsModel.WaveModalActive then div [] []
    else
        let filteredWaves = filterWaves wsModel
        Modal.modal [
            Modal.IsActive wsModel.WaveModalActive
            Modal.Props [ Style [ ZIndex 20000 ] ]
        ] [
            // Modal background to allow closing on click.
            Modal.background [
                Props [ OnClick (fun _ -> dispatch (UpdateWSModel (fun ws -> { ws with WaveModalActive = false }))) ]
            ] []
            // Main modal card.
            Modal.Card.card [ Props [ Style [ MinWidth "90%" ] ] ] [
                // Header with title and delete button.
                Modal.Card.head [] [
                    Modal.Card.title [] [
                        Level.level [] [
                            Level.left [] [ str "Select Waves" ]
                            Level.right [] [
                                Delete.delete [
                                    Delete.Option.Size IsMedium
                                    Delete.Option.OnClick handleModalClose
                                ] []
                            ]
                        ]
                    ]
                ]
                // Body with info row, search boxes row, then two columns for selection and breadcrumbs.
                Modal.Card.head [
                    Props [
                        Style [
                            BackgroundColor "white"
                            Border "none"
                            Margin "0"
                            Padding "0"
                            Height "auto"
                            BorderTopLeftRadius "0"
                            BorderTopRightRadius "0"
                        ]
                    ]
                ] [
                    div [
                        Style [
                            GridColumn "1 / span 2"
                            MarginBottom "15px"
                            MarginTop "15px"
                            Display DisplayOptions.Flex
                            FlexDirection "row"
                            FlexWrap "wrap"
                            MarginLeft "10px"
                        ]
                    ] [
                        // Info button and wave count.
                        div [ Style [ Display DisplayOptions.Flex; AlignItems AlignItemsOptions.Center; MarginBottom "20px" ] ] [
                            infoButton
                        ]
                        // search boxes
                        waveSearchBox wsModel dispatch
                        sheetSearchBox wsModel dispatch
                        componentSearchBox wsModel dispatch
                        portSearchBox wsModel dispatch
                        // Select All Subsheets checkbox
                        selectAllSubsheetsBox wsModel dispatch
                    ]

                ]

                // Body with info row, search boxes row, then two columns for selection and breadcrumbs.
                Modal.Card.body [
                    Props [
                        Style [
                            Height "70vh"
                            OverflowY OverflowOptions.Visible
                            Display DisplayOptions.Grid
                            GridTemplateColumns "1fr 1fr"
                            GridGap "10px"
                            Width "100%"
                        ]
                    ]
                ] [
                
                    // Left column: breadcrumbs with its own scrollbar.
                    div [
                        Style [
                            Height "100%"
                            OverflowY OverflowOptions.Auto
                        ]
                    ] [ 
                        waveSelectBreadcrumbs wsModel filteredWaves dispatch model 
                    ]

                    // Right column: wave selection with its own scrollbar.
                    div [
                        Style [
                            Height "100%"
                            OverflowY OverflowOptions.Auto
                        ]
                    ] [
                        let waveselect = selectWaves wsModel filteredWaves.OfSheet dispatch
                        renderwaves wsModel dispatch waveselect
                    ]
                ]
                // Footer with Done button.
                Modal.Card.foot [ Props [ Style [ Display DisplayOptions.InlineBlock; Float FloatOptions.Right ] ] ] [
                    Fulma.Button.button [
                        Fulma.Button.OnClick (fun _ -> closeModal ())
                        Fulma.Button.Color IsSuccess
                        Fulma.Button.Props [ Style [ Display DisplayOptions.InlineBlock; Float FloatOptions.Right ] ]
                    ] [ str "Done" ]
                ]
            ]
        ]


//----------------------------------------------------------------------------------------//
//-------------Waveform Selection Tree Display and Helpers--------------------------------//
//-------------THIS CODE IS NOT USED IN THE CURRENT IMPLEMENTATION------------------------//
//----------------------------------------------------------------------------------------//

(*
type WTNode =  
    | SheetNode of string list 
    | ComponentNode of FComponentId 
    | GroupNode of ComponentGroup * string list 
    | PortNode of Wave


type WaveTreeNode = {
    WTNode: WTNode
    HiddenNodes: WaveTreeNode list
    ShowDetails: bool 
}

type WaveDisplayTree = WaveTreeNode list

// -----------------------------------------
let makeWaveDisplayTree (wsModel: WaveSimModel) (showDetails: bool) (wavesToDisplay : Wave list): WaveDisplayTree =

    // Functions that give maximum controllability 
    // on the structure of the tree (within reason)
    let z = true

    // true: Wave -> () ... false: Component -> Wave -> () tree
    let shouldFlattenComponent (x: Wave list) =  z
        // List.length x < 5

    // false: Group -> Component ... true : Component
    let shouldFlattenGroup (x : (ComponentGroup * Wave list) list ) : bool = z
        // x
        // |> List.collect snd
        // |>  ( fun lst -> lst.Length < 5 )
        // /// Determines if all component groups should be flattened
    
    let shouldFlattenAllGroups (groupedWaves: (ComponentGroup * Wave list) list) : bool =
        shouldFlattenGroup groupedWaves

    /// Determines if a specific component group should be flattened
    let shouldFlattenAgroup (compWaves: ComponentGroup * Wave list) : bool = z 
            // compWaves
            // |>  snd
            // |> List.length
            // |> (>) 5

    let shouldFlattenSheet (x : (string list * Wave list) list ) : bool = z
        // x
        // |> List.collect snd
        // |>  ( fun lst -> lst.Length < 5 )



    let makePortNode (ws:WaveSimModel) (wave:Wave) : WaveTreeNode = 
        { WTNode = PortNode wave
          HiddenNodes = []
          ShowDetails = showDetails }

    // makes a single componentNode OR returns a list of WavesNodes
    let makeComponentNode (ws: WaveSimModel) (waves: Wave list) : WaveTreeNode list=
        let getComponentFromWave (wave: Wave) = wave.WaveId.Id
        // no need for this function
        // this function checks if all the waves in a Component are of that component
        // does this by matching all Wave.SubSheet fields
        let comp = 
            let x =
                waves
                |> List.map (fun wave -> wave.SubSheet)
                |> List.groupBy id
                |> List.length
            match x with
                | 1 -> getComponentFromWave waves[0] 
                | _ -> getComponentFromWave waves[0] //failwithf "makeComponentNode broken: Waves should be from the same component" 


        let rows = 
            waves
            |> List.map (makePortNode ws)

        let compNode : WaveTreeNode = 
            {
                WTNode = ComponentNode comp;
                HiddenNodes = rows;
                ShowDetails = showDetails
            }
            
        match shouldFlattenComponent waves with
        | true  ->  rows
        | false -> [compNode]



    let makeGroupNode (showDetails: bool) (fs: FastSimulation) (lst: (string list * Wave list) list) : WaveTreeNode list =
        /// Extracts unique subSheet names from the input
        let subSheet = 
            lst |> List.map fst |> List.concat |> List.distinct 

        /// Groups waves by their component group
        let groupedWaves =
            lst
            |> List.collect snd
            |> List.groupBy (fun wave -> getCompGroup fs wave)


        /// If flattening is required, convert all grouped waves into component nodes
        let flattenedNodes = 
            groupedWaves 
            |> List.collect (fun (_, waves) -> makeComponentNode wsModel waves)





        if shouldFlattenAllGroups groupedWaves then 
            flattenedNodes
        else 
            let groupNodes = 
                groupedWaves 
                |> List.map (fun (cGroup, groupWaves) -> 
                    /// Groups waves by component ID
                    let compWaves = groupWaves |> List.groupBy (fun w -> w.WaveId.Id)

                    /// Generates hidden nodes by converting grouped waves into component nodes
                    let hiddenNodes = 
                        compWaves
                        |> List.collect (fun (_, waves) -> makeComponentNode wsModel waves)

                    if shouldFlattenAgroup (cGroup, groupWaves) then 
                        hiddenNodes
                    else
                        let gNode: WaveTreeNode = 
                            {   WTNode = GroupNode (cGroup, subSheet)
                                HiddenNodes = hiddenNodes
                                ShowDetails = showDetails 
                            }
                        [gNode]
                )
            
            groupNodes |> List.concat





    // makes a single sheetNode or returns a list of GroupNodes
    let rec makeSheetNode (showDetails: bool) (subSheet: string list) (waves: Wave list) : WaveTreeNode list =
        let fs = Simulator.getFastSim()

        let wavesBySheetOrComponent = 
            waves
            |> List.groupBy (fun w -> List.truncate (subSheet.Length + 1) w.SubSheet)
        
        let wavesOfSubSheets = 
            wavesBySheetOrComponent
            |> List.filter (fun (g,_) -> g <> subSheet)
        
        let wavesOfComponents =
            wavesBySheetOrComponent
            |> List.filter (fun (g, wLst) -> g = subSheet)

        
        let subSheetNodes =
            match shouldFlattenSheet wavesOfSubSheets with
            | true  -> makeGroupNode showDetails fs wavesOfSubSheets
            | false -> 
                wavesOfSubSheets
                |> List.collect (fun (subSheet', waves') -> makeSheetNode showDetails subSheet' waves')


        // i need the string list to path through
        let groupNodes = makeGroupNode showDetails fs wavesOfComponents

        let hiddenNodes = subSheetNodes @ groupNodes

        let finalTree = 
            match shouldFlattenSheet wavesBySheetOrComponent with
            | true  -> hiddenNodes
            | false -> [{   
                    WTNode = SheetNode subSheet 
                    HiddenNodes = hiddenNodes 
                    ShowDetails = showDetails 
                }]
        finalTree

    makeSheetNode showDetails [] wavesToDisplay

let getWaveDisplayName (wave: Wave) : string =
    wave.SubSheet @ [wave.PortLabel] 
    |> List.reduce (fun acc s ->acc + "." + s)


let implementWaveSelector (wsModel: WaveSimModel) (dispatch: Msg -> unit) (wTree: WaveDisplayTree): ReactElement =
    // This function implements the display of the waveform selection table
    // as a set of rows that can be hidden or displayed.
    // The structure and order of the rows is determined by the tree structure.
    // Details to display in each row are determined by the node content.
    // Additional details can be looked up from wsModel if necessary.
    
    /// Get the waves associated with a node for checkbox operations
    let getWavesFromNode (node: WaveTreeNode) : Wave list =
        let rec collectWaves (n: WaveTreeNode) : Wave list =
            match n.WTNode with
            | PortNode wave -> [wave]
            | _ -> List.collect collectWaves n.HiddenNodes
        collectWaves node
        |> List.sortBy (fun wave -> getWaveDisplayName wave)
    
    /// Create a row for a port node (leaf node in tree)
    let makePortNodeRow (wave: Wave) : TableRow =
        let subSheet =
            match wave.SubSheet with
            | [] -> str (Simulator.getFastSim().SimulatedTopSheet)
            | _  -> subSheetsToNameReact wave.SubSheet

        waveRow [] [
            waveCheckBoxItem wsModel [wave.WaveId] dispatch
            str wave.PortLabel
            (match wave.WaveId.PortType with 
            | PortType.Output -> "Output" 
            | PortType.Input -> "Input"
            |> str)
            
        ]
    
    /// Create a row for a component node with its port details
    let rec makeComponentNodeRow (node: WaveTreeNode) : TableRow =
        match node.WTNode with
        | ComponentNode compId ->
            let fc = Simulator.getFastSim().WaveComps[compId]
            let waves = getWavesFromNode node
            let cBox = ComponentItem fc
            let summaryReact = summaryName wsModel cBox fc.SubSheet waves
            let portRows = 
                node.HiddenNodes
                |> List.map (fun pNode -> 
                    match pNode.WTNode with
                    | PortNode wave -> makePortNodeRow wave
                    | _ -> failwithf "Expected PortNode but got something else")

            makeSelectionGroup node.ShowDetails wsModel dispatch summaryReact portRows cBox waves
        | _ -> failwithf "Expected ComponentNode but got something else"
    
    /// Create a row for a group node with its component details
    let rec makeGroupNodeRow (node: WaveTreeNode) : TableRow =
        match node.WTNode with
        | GroupNode (cGroup, subSheet) ->
            let waves = getWavesFromNode node
            let cBox = GroupItem (cGroup, subSheet)
            let summaryReact = summaryName wsModel cBox subSheet waves
            
            let componentRows =
                node.HiddenNodes
                |> List.map (fun cNode -> 
                    match cNode.WTNode with
                    | ComponentNode _ -> makeComponentNodeRow cNode
                    | PortNode  wave -> makePortNodeRow wave 
                    | _ -> failwithf "Shouldn't happen since only a Component or a Wave can be under a Group "
                    // failwithf "Expected ComponentNode but got something else: WaveNode  "
                )
            makeSelectionGroup node.ShowDetails wsModel dispatch summaryReact componentRows cBox waves
        | _ -> failwithf "Expected GroupNode but got something else"
    
    /// Create a row for a sheet node with its group and subsheet details
    let rec makeSheetNodeRow (node: WaveTreeNode) : TableRow =
        match node.WTNode with
        | SheetNode subSheet ->
            // If this is the top sheet, we render it differently (as a table)
            if subSheet = [] || subSheet = [wsModel.TopSheet] then
                let rows = 
                    node.HiddenNodes
                    |> List.map (fun childNode ->
                        match childNode.WTNode with
                        | GroupNode _ -> makeGroupNodeRow childNode
                        | SheetNode _ -> makeSheetNodeRow childNode
                        | ComponentNode _ -> makeComponentNodeRow childNode
                        | PortNode wave -> makePortNodeRow wave
                        )
                
                waveRow [] [wavePropsTable rows]
            else
                // For non-top sheets
                let waves = getWavesFromNode node
                let cBox = SheetItem subSheet
                let summaryReact = summaryName wsModel cBox subSheet waves
                
                // Process child nodes based on their type
                let childRows = 
                    node.HiddenNodes
                    |> List.map (fun childNode ->
                        match childNode.WTNode with
                        | GroupNode _ -> makeGroupNodeRow childNode
                        | SheetNode _ -> makeSheetNodeRow childNode
                        | ComponentNode _ -> makeComponentNodeRow childNode
                        | PortNode wave -> makePortNodeRow wave
                        )
                
                makeSelectionGroup node.ShowDetails wsModel dispatch summaryReact childRows cBox waves
        | _ -> failwithf "Expected SheetNode but got something else"
    
    // Process each node in the wave display tree based on its type
    let elements =
        wTree
        |> List.map (fun node ->
            match node.WTNode with
            | SheetNode _ -> makeSheetNodeRow node
            | GroupNode _ -> makeGroupNodeRow node
            | ComponentNode _ -> makeComponentNodeRow node
            | PortNode wave -> makePortNodeRow wave)
    
    wavePropsTable elements
    *)
