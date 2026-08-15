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
    /// Above this many selected waveforms Done refuses rather than warns: the modal stays open and
    /// the user has to deselect. Twice the recommended number - far enough above the warning that
    /// reaching it takes deliberate work, and low enough that the viewer that opens still scrolls.
    let maxAllowedViewerWaves = 100
    /// Below this many waves on offer the selector opens every row, on the grounds that a list this
    /// short is quicker to read than to click through.
    let maxAutoExpandWaves = 50

/// What leaving the wave selection dialog with a given number of waveforms selected does.
type SelectionVerdict =
    /// the dialog closes and the viewer shows them
    | SelectionOk
    /// the user is warned, and may go on
    | SelectionWarn
    /// the dialog stays open until some are deselected
    | SelectionRefuse

let selectionVerdict (numWaves: int) =
    if numWaves > Constants.maxAllowedViewerWaves then SelectionRefuse
    elif numWaves > Constants.maxRecommendedViewerWaves then SelectionWarn
    else SelectionOk

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
///
/// One pill per NODE of the collapsed hierarchy, not per instance: a sheet instantiated four times
/// inside one parent is one pill, and which of the four it stands for is the combo box in the
/// other pane. Both panes are drawn from the same hierarchy, so they cannot disagree about it.
let waveSelectBreadcrumbs
        (wsModel: WaveSimModel)
        (hierarchy: WaveSimHierarchy.SelectorHierarchy)
        (filteredWaves: {| All: Wave list; Sheets: string list; OfSheet: Wave list|})
        (dispatch: Msg -> unit)
        (model: Model) : ReactElement =
    match model.CurrentProj with
    | None -> div [] [ str "No project open" ]
    | Some _ ->
        let fs = Simulator.getFastSim()
        // Extract sheet names from wave names.
        let sheetCounts =
            filteredWaves.All |> List.countBy (fun wave -> wave.SheetLabel)
        /// The instance a pill stands for. None where the simulation has nothing there - an empty
        /// sheet, or a simulation of an earlier version of the design - and such a pill is inert.
        let instanceOf (sheet: SheetTree) =
            WaveSimHierarchy.nodeOf hierarchy sheet |> Option.bind (fun node -> node.NodeInstance)
        let sheetColor (sheet: SheetTree) =
            match instanceOf sheet with
            | Some instance when List.contains instance filteredWaves.Sheets -> IColor.IsCustomColor "pink"
            | _ -> IColor.IsCustomColor "darkslategrey"
        let sheetMatches (sheet: SheetTree) =
            match instanceOf sheet with
            | None -> 0
            | Some instance ->
                match List.tryFind (fun (name, _) -> name = instance) sheetCounts with
                | Some (_, count) -> count
                | None -> 0
        /// Clicking a pill filters the signals by that sheet, and opens the node so that what it
        /// filtered to is on screen rather than behind a closed row.
        let updateSearchStringHelper (sheet: SheetTree) : (Msg -> unit) -> unit =
            fun dispatch ->
                match instanceOf sheet with
                | None -> ()
                | Some instance ->
                    dispatch (UpdateWSModel (fun ws ->
                        { ws with SheetSearchString = updateSheetString instance ws }
                        |> fun ws -> setWaveSheetSelectionOpen ws [sheet.SheetPath] true))
        /// The design-time sheet name, with the instance after it where there is a choice of them.
        let sheetName (node: SheetTree) =
            match WaveSimHierarchy.nodeOf hierarchy node with
            | Some sel when sel.NodeInstances.Length > 1 ->
                match sel.NodeInstance with
                | Some instance -> $"{node.SheetName} ({instance})"
                | None -> node.SheetName
            | _ -> node.SheetName
        let breadcrumbConfig = {
            MiscMenuView.Constants.defaultConfig with
                ClickAction = updateSearchStringHelper
                ColorFun = sheetColor
                NoWaves = sheetMatches
                BreadcrumbText = Some sheetName
                IsCollapsible = fun sheet ->
                    WaveSimHierarchy.nodeOf hierarchy sheet
                    |> Option.map (fun node -> node.NodeCollapsible)
                    |> Option.defaultValue false
                ExpandAction = fun sheet dispatch ->
                    let show = not (Set.contains sheet.SheetPath wsModel.ShowSheetDetail)
                    dispatch (UpdateWSModel (fun ws -> setWaveSheetSelectionOpen ws [sheet.SheetPath] show))
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
                MiscMenuView.breadcrumbsOfTree breadcrumbConfig hierarchy.HierTree dispatch
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
            
        
    

/// A summary row like makeSummaryItem, with one thing shown in the body above the rows. For a
/// sheet that is the combo box choosing which of its instances the rows below belong to, which
/// belongs there rather than in the summary: a click on the summary opens and closes the row.
let makeSheetItem
        showDetails
        (ws: WaveSimModel)
        (summaryItem: ReactElement)
        (aboveRows: ReactElement)
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
                    aboveRows
                    wavePropsTable rows
                ]
        ]

let makeFlatGroupRow
        showDetails
        (ws: WaveSimModel)
        (nodeKey: string list)
        (grp: ComponentGroup)
        (wavesInGroup: Wave list)
        (dispatch: Msg -> unit) =
    let portsPerRow = Constants.numPortColumns
    // Keyed by the node the group sits in, so that opening the gates of one sheet does not open
    // the gates of every other sheet in the list.
    let cBox = GroupItem (grp, nodeKey)
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

/// The combo box choosing which instance of a sheet the signals below it belong to. Shown only
/// where there is a choice to make - most sheets in most designs are instantiated once.
let private instanceSelector (node: WaveSimHierarchy.SelectorNode) (dispatch: Msg -> unit) =
    match node.NodeInstances, node.NodeInstance with
    | _ :: _ :: _, Some shown ->
        div [Style [Display DisplayOptions.Flex; AlignItems AlignItemsOptions.Center; MarginLeft "10px"]] [
            p [Style (MarginRight "8px" :: normalFontStyle)] [str "Instance"]
            Select.select [Select.Size IsSmall] [
                select [
                    // Bulma gives the control a height and 5px of vertical padding, and something
                    // in the app's CSS leaves select at content-box - so the box it is drawn in is
                    // 10px taller than the box it is laid out in, and it covers the first group of
                    // signals under it. Said here rather than in the CSS: this is the only select
                    // inside a table row, which is what makes the overspill land on something.
                    Style [BoxSizing BoxSizingOptions.BorderBox]
                    Value shown
                    OnChange (fun ev ->
                        let chosen = ev.Value
                        dispatch <| UpdateWSModel (fun ws ->
                            { ws with SelectedSheetInstance = Map.add node.NodeKey chosen ws.SelectedSheetInstance }))
                ] (node.NodeInstances |> List.map (fun instance -> option [Value instance] [str instance]))
            ]
        ]
    | _ -> null

/// One row per sheet instance, which is what the pane showed before the hierarchy was collapsed.
/// Kept for Show Only Selected: a wave already chosen inside an instance that no combo box is
/// currently showing has to stay reachable, or it could never be deselected.
let private makeInstanceRows showDetails (ws: WaveSimModel) (fs: FastSimulation) waves dispatch =
    waves
    |> List.groupBy (fun (w: Wave) -> w.SheetId)
    |> List.map (fun (instance, wavesOfInstance) ->
        let groupRows =
            wavesOfInstance
            |> List.groupBy (fun wave -> getCompGroup fs wave)
            |> List.map (fun (grp, groupWaves) ->
                makeFlatGroupRow showDetails ws [instance] grp groupWaves dispatch)
        makeSummaryItem showDetails ws (str instance) groupRows (SheetItem [instance]) wavesOfInstance dispatch)

/// One row per node of the collapsed hierarchy, holding the signals of the instance that node is
/// showing. A node whose instance has no waves left after filtering is left out, which is how the
/// search boxes and the sheet filter narrow the list.
let private makeNodeRows
        showDetails
        (ws: WaveSimModel)
        (fs: FastSimulation)
        (hierarchy: WaveSimHierarchy.SelectorHierarchy)
        (waves: Wave list)
        dispatch =
    let wavesByInstance = waves |> List.groupBy (fun w -> w.SheetId) |> Map.ofList
    hierarchy.HierOrder
    |> List.choose (fun node ->
        node.NodeInstance
        |> Option.bind (fun instance ->
            Map.tryFind instance wavesByInstance
            |> Option.map (fun wavesOfNode -> node, instance, wavesOfNode)))
    |> List.map (fun (node, instance, wavesOfNode) ->
        let groupRows =
            wavesOfNode
            |> List.groupBy (fun wave -> getCompGroup fs wave)
            |> List.map (fun (grp, groupWaves) ->
                makeFlatGroupRow showDetails ws node.NodeKey grp groupWaves dispatch)
        // A node the user opens and closes is never opened by the auto-expand: only one node of a
        // sheet is meant to be showing at a time, and forcing them all open would say otherwise.
        let autoOpen = showDetails && not node.NodeCollapsible
        let title =
            let path = String.concat "." node.NodeKey
            if node.NodeInstances.Length > 1 then $"{path} ({instance})" else path
        makeSheetItem
            autoOpen ws (str title) (instanceSelector node dispatch)
            groupRows (SheetItem node.NodeKey) wavesOfNode dispatch)

let makeSelectionTable
        (ws: WaveSimModel)
        (hierarchy: WaveSimHierarchy.SelectorHierarchy)
        (waves: Wave list)
        (dispatch: Msg -> unit)
        =
    let fs = Simulator.getFastSim()
    let waves = List.sortBy (fun wave -> wave.ViewerDisplayName) waves
    let sheetNum = waves |> List.distinctBy (fun w -> w.SheetId) |> List.length
    let showDetails =
        ((List.length waves < Constants.maxAutoExpandWaves) ||
        (ws.WaveSearchString.Length > 0)) ||
         ws.ShowOnlySelected ||
         sheetNum < 2
    let subSheetRows =
        if ws.ShowOnlySelected then
            makeInstanceRows showDetails ws fs waves dispatch
        else
            makeNodeRows showDetails ws fs hierarchy waves dispatch
    let messageColour =
        match ws.SelectedWaves.Length with
        | n when n > Constants.maxAllowedViewerWaves -> "darkred"
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
             SearchString = ""
        }

    let closeModal () =
        dispatch (UpdateWSModel (fun ws ->
            resetSearchFilters { ws with WaveModalActive = false }
        ))

    /// Leaving the dialog - Done, the X in the header, or a click on the background. Above the
    /// recommended number of waveforms this warns; above what the viewer can show at all it
    /// refuses and the dialog stays open. The filters are not reset on that path: they are what
    /// finds the waveforms to deselect, which is what the refusal asks the user to do.
    let handleModalClose _ =
        let numWaves = List.length wsModel.SelectedWaves
        match selectionVerdict numWaves with
        | SelectionRefuse ->
            UIPopups.viewWaveSelectRefusalPopup Constants.maxAllowedViewerWaves numWaves dispatch
        | SelectionWarn ->
            UIPopups.viewWaveSelectConfirmationPopup
                Constants.maxRecommendedViewerWaves
                numWaves
                (fun finish _ ->
                    dispatch ClosePopup
                    if finish then closeModal ())
                dispatch
        | SelectionOk ->
            closeModal ()

    if not wsModel.WaveModalActive then div [] []
    else
        let filteredWaves = filterWaves wsModel
        // Both panes are drawn from one hierarchy, worked out here: which nodes there are and
        // which instance each of them is showing has to be one answer, not two.
        let hierarchy =
            match model.CurrentProj with
            | None -> WaveSimHierarchy.emptyHierarchy
            | Some project ->
                WaveSimHierarchy.getSelectorHierarchy
                    (Simulator.getFastSim())
                    (ModelHelpers.getUpdatedLoadedComponents project model)
                    wsModel
        Modal.modal [
            Modal.IsActive wsModel.WaveModalActive
            Modal.Props [ Style [ ZIndex 20000 ] ]
        ] [
            // Modal background to allow closing on click. Through the same handler as Done: this
            // used to close the dialog directly, which dodged the warning about how many
            // waveforms were selected, and would now dodge the refusal as well.
            Modal.background [
                Props [ OnClick handleModalClose ]
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
                        waveSelectBreadcrumbs wsModel hierarchy filteredWaves dispatch model
                    ]

                    // Right column: wave selection with its own scrollbar.
                    div [
                        Style [
                            Height "100%"
                            OverflowY OverflowOptions.Auto
                        ]
                    ] [
                        makeSelectionTable wsModel hierarchy filteredWaves.OfSheet dispatch
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
