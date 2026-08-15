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

    

/// The waves of each sheet instance, worked out once per AllWaves map rather than once per render.
///
/// The selector wants the waves of the handful of instances it is drawing and had no way to ask for
/// them: it read every wave in the design and threw nearly all of them away, on every keystroke in
/// a search box and every click on a pill. main6 of largeTest carries 208,896 waves.
///
/// AllWaves is replaced whenever the waves are rebuilt, so a new map is exactly the signal that an
/// index built from the old one is stale - and nothing has to remember to clear it.
let wavesByInstance: Map<WaveIndexT, Wave> -> Map<string, Wave list> =
    Helpers.memoizeByIdentity (fun allWaves ->
        Map.valuesL allWaves
        |> List.groupBy (fun (wave: Wave) -> wave.SheetId)
        |> Map.ofList)

/// Ensures that only valid waves (and selected waves) are returned. A design edited under a running
/// simulation can leave a wave naming a component the simulation no longer has.
let ensureWaveConsistency (ws: WaveSimModel) (candidates: Wave list) =
    let fs = Simulator.getFastSim()
    let okWaves =
        candidates
        |> List.filter (fun wave -> Map.containsKey wave.WaveId.Id fs.WaveComps)
    if okWaves.Length <> candidates.Length then
        Log.dbg Log.Wave $"wave consistency: {okWaves.Length} valid waves of {candidates.Length}"
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
///
/// `shown` is the sheet instances the two panes are going to draw - the handful the collapsed
/// hierarchy resolved to, one per node. A wave of any other instance cannot appear in either pane,
/// so it is dropped before anything else looks at it. That is the difference between work
/// proportional to the design somebody wrote and work proportional to what it expands to: main6 of
/// largeTest is seven sheets and tens of thousands of instances, carrying 208,896 waves between
/// them, and the dialog draws seven rows.
///
/// None means no restriction, which is what Show Only Selected needs: a wave already chosen inside
/// an instance no combo box is currently showing must stay reachable, or it could never be
/// deselected.
///
/// Every membership test here is against a Set. They were lists as long as the expansion, tested
/// once per wave, which is a product of two numbers that both grow with the design - the reason
/// opening this dialog on a large design took minutes. The worst case was the DEFAULT one: an empty
/// sheet box matches every instance, since every string contains the empty string.
let filterWaves (shown: Set<string> option) (wsModel: WaveSimModel) =
    let fs = Simulator.getFastSim()
    // Only the instances on show are read out of the index. This is the step that makes the cost
    // the design's rather than its expansion's: everything below works on what it returns.
    let candidates =
        match shown with
        | Some instances ->
            let index = wavesByInstance wsModel.AllWaves
            instances
            |> Set.toList
            |> List.collect (fun instance -> Map.tryFind instance index |> Option.defaultValue [])
        | None -> Map.valuesL wsModel.AllWaves
    let waves, okSelectedWaves = ensureWaveConsistency wsModel candidates
    let selectedIds = Set.ofList okSelectedWaves
    let matchWithBox (searchString: string) (matcher:string) =
        let s = searchString.Trim().ToUpperInvariant()
        s = "" || s = "*" || matcher.ToUpperInvariant().Contains s

    let searchFilteredWaves =
        waves
        |> List.filter (fun wave ->
            matchWithBox wsModel.ComponentSearchString wave.CompLabel
            && matchWithBox wsModel.PortSearchString wave.PortLabel
            && (not wsModel.ShowOnlySelected || Set.contains wave.WaveId selectedIds)
        )
    let sheetBox = wsModel.SheetSearchString.Trim().ToUpperInvariant()
    let sheet = sheetBox.TrimEnd '*'
    let allSubSheets = sheetBox.EndsWith "*"
    // The sheet box is matched against the instances on show, for the same reason: what it decides
    // is which pills are highlighted and which rows are drawn, and both are drawn from those.
    let allSheets =
        match shown with
        | Some instances -> Set.toList instances
        | None -> fs.SimSheetStructure.Keys |> Seq.toList

    let filteredSheets =
        allSheets
        |> List.tryPick (fun sheet' -> if sheet' = sheet then Some [sheet] else None)
        |> Option.defaultValue (List.filter (fun (sheetId:string) -> sheetId.Contains sheet) allSheets)

    let searchSheets =
        allSheets
        |> List.filter (fun sheet -> List.contains sheet filteredSheets ||
                                     (allSubSheets && isSubSheetOf sheet filteredSheets))
        |> Set.ofList

    let sheetFilteredWaves =
        searchFilteredWaves
        |> List.filter (fun wave -> Set.contains wave.SheetId searchSheets)

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
        (filteredWaves: {| All: Wave list; Sheets: Set<string>; OfSheet: Wave list|})
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
            | Some instance when Set.contains instance filteredWaves.Sheets -> IColor.IsCustomColor "pink"
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
        let breadcrumbConfig = {
            MiscMenuView.Constants.defaultConfig with
                ClickAction = updateSearchStringHelper
                ColorFun = sheetColor
                NoWaves = sheetMatches
                // The design-time sheet name and nothing else. A pill's place in the tree is what
                // says which occurrence it is, and the combo box in the other pane says which
                // instance - so naming the instance here repeated one and pre-empted the other.
                BreadcrumbText = Some (fun sheet -> sheet.SheetName)
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

/// One row per sheet of the collapsed hierarchy, holding the signals of the instance that row is
/// showing. A row whose instance has no waves left after filtering is left out, which is how the
/// search boxes and the sheet filter narrow the list.
///
/// The list is flat for a sheet only one route reaches: wherever it sits in the design it appears
/// at top level, once, and a combo box says which of the instances inside its one parent it is
/// showing. A sheet SEVERAL routes reach cannot be there - at top level there is nothing to say
/// which route it stands for - so its row sits inside each parent that instantiates it, and the
/// combo boxes down that chain settle the occurrence between them. That is what keeps the rows
/// drawn proportional to the sheets somebody wrote rather than to what the design expands to:
/// largeTest is seven sheets and 49,152 instances of the innermost one.
let private makeNodeRows
        showDetails
        (ws: WaveSimModel)
        (fs: FastSimulation)
        (hierarchy: WaveSimHierarchy.SelectorHierarchy)
        (waves: Wave list)
        dispatch =
    let wavesByInstance = waves |> List.groupBy (fun w -> w.SheetId) |> Map.ofList

    /// The row for this sheet, and the rows of the sheets below it that belong at top level. The
    /// second is in the order the nodes are walked, parents first, which is the order the flat
    /// list had when every node was in it.
    let rec rowsOf (sheet: SheetTree) =
        match WaveSimHierarchy.nodeOf hierarchy sheet with
        | None -> None, []
        | Some node ->
            // A sheet reached more than one way is drawn here, inside its parent. One reached only
            // this way is drawn at top level, and so are the ones it in turn sends there.
            let nested, flat =
                sheet.SubSheets
                |> List.map (fun sub ->
                    let own, flat = rowsOf sub
                    match WaveSimHierarchy.nodeOf hierarchy sub with
                    | Some child when child.NodeMultiRoute -> Option.toList own, flat
                    | _ -> [], Option.toList own @ flat)
                |> List.unzip
                |> fun (nested, flat) -> List.concat nested, List.concat flat

            let wavesOfNode =
                node.NodeInstance
                |> Option.bind (fun instance -> Map.tryFind instance wavesByInstance)
                |> Option.defaultValue []
            let groupRows =
                wavesOfNode
                |> List.groupBy (fun wave -> getCompGroup fs wave)
                |> List.map (fun (grp, groupWaves) ->
                    makeFlatGroupRow showDetails ws node.NodeKey grp groupWaves dispatch)

            // Nothing of its own and nothing underneath it means nothing to draw. Signals of its
            // own are not required: a sheet that only passes wires through still has to be drawn
            // when something inside it matched, since its row is the only way to reach that.
            if List.isEmpty groupRows && List.isEmpty nested then
                None, flat
            else
                // A node the user opens and closes is never opened by the auto-expand: only one
                // node of a sheet is meant to be showing at a time, and forcing them all open
                // would say otherwise.
                let autoOpen = showDetails && not node.NodeCollapsible
                // The sheet's own name, not its path and not its instance. Where the row sits says
                // which occurrence it is and the combo box beside it says which instance, so the
                // path was only ever length: main6.main5.main4.main3 to say main3.
                let row =
                    makeSheetItem
                        autoOpen ws (str sheet.SheetName) (instanceSelector node dispatch)
                        (groupRows @ nested) (SheetItem node.NodeKey) wavesOfNode dispatch
                Some row, flat

    let top, flat = rowsOf hierarchy.HierTree
    Option.toList top @ flat

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
        // Both panes are drawn from one hierarchy, worked out here: which nodes there are and
        // which instance each of them is showing has to be one answer, not two.
        //
        // Before the waves are filtered, not after: what it resolves to is the handful of instances
        // the panes will draw, and that is what stops the filter reading every wave in the design.
        let hierarchy =
            match model.CurrentProj with
            | None -> WaveSimHierarchy.emptyHierarchy
            | Some project ->
                WaveSimHierarchy.getSelectorHierarchy
                    (Simulator.getFastSim())
                    (ModelHelpers.getUpdatedLoadedComponents project model)
                    wsModel
        // Show Only Selected lists the instances a wave was chosen in rather than the ones on show,
        // so it is the one mode that has to look wider than the hierarchy.
        let shownInstances =
            if wsModel.ShowOnlySelected then
                None
            else
                hierarchy.HierOrder
                |> List.choose (fun node -> node.NodeInstance)
                |> Set.ofList
                |> Some
        let filteredWaves = filterWaves shownInstances wsModel
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
