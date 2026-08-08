module WaveSimSelectHelpers

//---------------------------------------------------------------------------------------//
//-------------Waveform Selection Popup and RAM Selection Helpers------------------------//
//---------------------------------------------------------------------------------------//

// Functions to make modal popups that allows waveforms and RAMs
// to be selected or deselected for display in the waveform simulator.
open EEExtensions
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
open Optics

// -----------------------------------------
// Helper Functions & Filtering Logic
// -----------------------------------------

module Constants =
    let numPortColumns = 4
    let maxWarningViewerWaves = 25
    let maxRecommendedViewerWaves = 50

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
        Map.valuesL ws.AllWaves
        |> List.filter (fun wave -> Map.containsKey wave.WaveId.Id fs.WaveComps)
    if okWaves.Length <> ws.AllWaves.Count then
        Log.dbg Log.Wave $"wave consistency: {okWaves.Length} valid waves of {ws.AllWaves.Count}"
    let okSelectedWaves =
        ws.SelectedWaves |> List.filter (fun selW -> Map.containsKey selW ws.AllWaves)
    if okSelectedWaves.Length <> ws.SelectedWaves.Length then
        Log.dbg Log.Wave $"wave consistency: {okSelectedWaves.Length} valid selected waves of {ws.SelectedWaves.Length}"
    okWaves, okSelectedWaves

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
            && (not wsModel.ShowOnlySelected || List.contains wave.WaveId okSelectedWaves)
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
            Input.Option.Placeholder "Filter sheet names..."
            Input.Option.OnChange (fun value ->
                dispatch (UpdateWSModel (fun wsm -> 
                    { wsm with SheetSearchString = value.Value.ToUpper() }
                ))
            )
        ]
    ]

/// Checkbox to select all subsheets.
let waveCheckBox
        (state_: Lens<WaveSimModel,'STATE>)
        (isChecked: 'STATE -> bool)
        (action: bool -> 'STATE -> 'STATE)
        (name: string)
        (ws:WaveSimModel)
        dispatch =
    let state = Optic.get state_ ws
    let ticked = isChecked state
    div [ Style [ MarginLeft "15px"; Display DisplayOptions.Flex; AlignItems AlignItemsOptions.Center; MarginBottom "20px" ] ] [
        Checkbox.checkbox [] [
            Checkbox.input [
                Props [
                    Checked (isChecked state)
                    OnChange (fun _ ->
                        dispatch (UpdateWSModel <| Optic.map state_ (action ticked) )
                    )
                ]
            ]
            str name
        ]
    ]

let selectAllSubsheetsBox (ws:WaveSimModel) dispatch =
    waveCheckBox
        sheetSearchString_
        (fun (s:string) -> s.EndsWith "*")
        (fun _ (s:string) -> if s.EndsWith "*" then s.TrimEnd('*') else s + "*")
        "All Subsheets"
        ws
        dispatch


let showOnlySelectedBox (ws:WaveSimModel) dispatch =
    waveCheckBox
        showOnlySelected_
        id
        (fun _ ticked -> not ticked)
        "Show Only Selected"
        ws
        dispatch

/// Search box for component names.
let componentSearchBox (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    div [ searchBoxContainerStyle ] [
        Input.text [
            Input.Option.Value wsModel.ComponentSearchString
            Input.Option.Props [ Style [ MarginBottom "1rem"; Width "100%" ] ]
            Input.Option.Placeholder "Filter component labels..."
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
            Input.Option.Placeholder "Filter port names..."
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
                // A library component is opaque here whatever the Sheets menu is set to show: none
                // of its innards are offered as waves, so it must not appear in the hierarchy that
                // selects them either.
                ShowLibrarySheets = false
                BreadcrumbText = Some sheetName
        }
        let hierarchyText =
            let allSheets = fs.SimSheetStructure |> Map.keysL
            let sheetFilter = wsModel.SheetSearchString.ToUpperInvariant().Trim()
            let withSubsheets = sheetFilter.EndsWith "*"
            let sheetFilter = sheetFilter.TrimEnd '*'
            match sheetFilter, withSubsheets with
            | "" , _  ->
                "Design hierarchy: click to filter by sheet"
            | sheet , false when List.contains sheet allSheets ->
                $"Design hierarchy: filtered by {sheet} without subsheets"
            | sheet , true when List.contains sheet allSheets ->
                $"Design hierarchy: filtered by {sheet} with subsheets"
            | sheet , true ->
                $"Design hierarchy: filtered by {sheet} with subsheets"
            | sheet , false ->
                $"Design hierarchy: filtered by {sheet} without subsheets"

        let breadcrumbs = [
            // Heading and tree share the panel, as in the Sheet menu - the same tree should not
            // sit on two different backgrounds depending on where it is shown.
            div [ HTMLAttr.ClassName "treePanel"
                  Style [Display DisplayOptions.Flex; FlexDirection "column"; AlignItems AlignItemsOptions.Center]] [
                div [ Style [ TextAlign TextAlignOptions.Center; FontSize "20px" ; FontWeight 600; PaddingBottom "10px"] ] [
                    str hierarchyText
                    ]
                MiscMenuView.hierarchyBreadcrumbs breadcrumbConfig dispatch updatedModel
                ]
        ]
        div [] breadcrumbs

// -----------------------------------------
// Wave Selection UI (Left Column)
// -----------------------------------------

// The following functions (toggleWaveSelection, toggleSelectSubGroup, etc.) handle the UI for selecting/deselecting waves.
// (Note: helper functions such as summaryProps, subSheetsToNameReact, isWaveSelected, checkboxInputProps,
//  wavesToIds, details/summary helpers, getCompGroup, GroupItem, summaryName, SheetItem are assumed to exist.)

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

                        

/// Makes a summary row which is one row of a table and
/// can be expanded to show more details presented as a sub-table.
/// The details are passed as:
/// rows - a list of TableRow elements each representing a component with some ports or a group of components.
/// waves: the corresponding list of waveforms to display.
/// In general waves.Length > rows.Length since one row will typically have multiple ports and therefore multiple waves.
/// The summary item is the ReactElement to display in the summary row: it is given Style etc from summaryProps.
let makeSummaryItem
        showDetails
        (ws: WaveSimModel)
        (summaryItem: ReactElement)
        (rows: TableRow list)
        (cBox: CheckBoxStyle)
        (waves: Wave list)
        (dispatch: Msg -> unit) =
    let wi = wavesToIds waves
    waveRowIProps
        (summaryProps false cBox ws dispatch)
        [
            waveCheckBoxItem ws wi dispatch             
            details
                (detailsProps showDetails cBox ws dispatch)
                [
                    summary (summaryProps true cBox ws dispatch) [ summaryItem ]
                    wavePropsTable rows
                ]
        ]
            
        
    

let makeFlatGroupRow
        showDetails
        (ws: WaveSimModel)
        (grp: ComponentGroup)
        (wavesInGroup: Wave list)
        (dispatch: Msg -> unit) =
    let portsPerRow = Constants.numPortColumns
    let cBox = GroupItem (grp, [])
    let summaryReact = summaryName ws cBox [] wavesInGroup
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

                    
            
    makeSummaryItem showDetails ws summaryReact rowItems cBox wavesInGroup dispatch

let makeSelectionTable
        (ws: WaveSimModel)
        (waves: Wave list)
        (dispatch: Msg -> unit)
        =
    let fs = Simulator.getFastSim()
    let waves = List.sortBy (fun wave -> wave.ViewerDisplayName) waves
    let sheetNum = waves |> List.distinctBy (fun w -> w.SheetId) |> List.length
    let showDetails =
        ((List.length waves < 50) ||
        (ws.WaveSearchString.Length > 0)) ||
         ws.ShowOnlySelected ||
         sheetNum < 2
    let subSheetRows =
        waves
        |> List.groupBy (fun w -> w.SheetId)
        |> List.map (fun (subSheetName, wavesInSubSheet) ->
            let componentGroups =
                wavesInSubSheet
                |> List.groupBy (fun wave -> getCompGroup fs wave)
            let groupRows =
                componentGroups
                |> List.map (fun (grp, groupWaves) ->
                    makeFlatGroupRow showDetails ws grp groupWaves dispatch
                )
            makeSummaryItem showDetails ws (str subSheetName) groupRows (SheetItem [subSheetName]) wavesInSubSheet dispatch
        )
    let messageColour =
        match ws.SelectedWaves.Length with
        | n when n > Constants.maxRecommendedViewerWaves -> "red"
        | n when n > Constants.maxWarningViewerWaves -> "orange"
        | _ -> "green"
    div [Style [Display DisplayOptions.Flex; FlexDirection "column"; AlignItems AlignItemsOptions.Center]]
        [
            p [Style [FontSize "20px"; FontWeight "600"; Color messageColour; PaddingBottom "15px"]] [str $"{ws.SelectedWaves.Length} waveforms selected"]       
            wavePropsTable subSheetRows
        ]
    



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
             ShowOnlySelected = false
        }

    let closeModal () =
        dispatch (UpdateWSModel (fun ws -> 
            resetSearchFilters { ws with WaveModalActive = false }
        ))

    // Handler for closing the modal (with confirmation if >50 waves are selected).
    let handleModalClose _ =
        let numWaves = List.length wsModel.SelectedWaves
        if numWaves > Constants.maxRecommendedViewerWaves then
            UIPopups.viewWaveSelectConfirmationPopup
                Constants.maxRecommendedViewerWaves
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
            Modal.Card.card [ Props [ Style [ MinWidth "95%" ] ] ] [
                // Header with title and delete button.
                Modal.Card.head [] [
                    Modal.Card.title [] [
                        Level.level [] [
                            Level.left
                                [Props [Style [FontSize "20px"; FontWeight "600"]]]
                                [ str "Select Waveforms using selection pane checkboxes. Filter using top boxes or clicking hierarchy" ]
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
                        // search boxes
                        // waveSearchBox wsModel dispatch // Not needed currently
                        componentSearchBox wsModel dispatch
                        portSearchBox wsModel dispatch
                        sheetSearchBox wsModel dispatch
                        // Select All Subsheets checkbox
                        selectAllSubsheetsBox wsModel dispatch
                        showOnlySelectedBox wsModel dispatch
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
                        makeSelectionTable wsModel filteredWaves.OfSheet dispatch
                    ]
                ]
                // Footer with Done button.
                Modal.Card.foot [ Props [ Style [ Display DisplayOptions.InlineBlock; Float FloatOptions.Right ] ] ] [
                    Fulma.Button.button [
                        Fulma.Button.OnClick (fun _ -> handleModalClose ())
                        Fulma.Button.Color IsSuccess
                        Fulma.Button.Props [ Style [ Display DisplayOptions.InlineBlock; Float FloatOptions.Right ] ]
                    ] [ str "Done" ]
                ]
            ]
        ]
