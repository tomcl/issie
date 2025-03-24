module HLP25CodeBsn722

open Hlp25Types
open Fable.React
open Fable.React.Props

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
//////Added by me//////
open Fulma
open Fulma.Extensions.Wikiki
open MiscMenuView
open Constants
open Browser.Types
open HLP25CodeBdw722
open HLP25CodeBsc3321

//------------------------------------- Part B ---------------------------------------------------//
//----------------------------- Sample Code for HLP25 --------------------------------------------//
//----------------------------- use these to get started -----------------------------------------//
//-------------------- Modify the signatures as you see fit for your implementation---------------//
//------------------------------------------------------------------------------------------------//


/// Displays a breadcrumb display of the simulation design sheet hierarchy with
/// coloured sheets indicating where the search string is found. Possibly the number of
/// matches in each sheet is displayed.
/// 
/// 
/// 



/////////////////////////////// Helper Functions //////////////////////////////////////

let ensureWaveConsistency (ws:WaveSimModel) =
        let fs = Simulator.getFastSim()
        let okWaves =
            Map.values ws.AllWaves
            |> Seq.toList
            |> List.filter (fun wave -> Map.containsKey wave.WaveId.Id fs.WaveComps )
        if okWaves.Length <> ws.AllWaves.Count then
            printfn $"EnsureWaveConsistency: waves,Length={okWaves.Length}, ws.Allwaves.Count={ws.AllWaves.Count}"
        let okSelectedWaves =
            ws.SelectedWaves
            |> List.filter (fun selW -> Map.containsKey selW ws.AllWaves)
        if okSelectedWaves.Length <> ws.SelectedWaves.Length then
            printfn $"ok selected waves length = {okSelectedWaves.Length} <> selectedwaves length = {ws.SelectedWaves.Length}"
        okWaves, okSelectedWaves 



////////////////////////////////////////////////////////////////////////////////////////
let waveSelectBreadcrumbs (wsModel: WaveSimModel) (dispatch: Msg -> unit) (model: Model): ReactElement =
    // See MiscMenuView for Breadcrumb generation functions
    // see WaveSelectView for the existing Waveform Selector search box
    // Use the existing Waveform Selector search box as a template for the new search boxes.
    match model.CurrentProj with
    | None -> 
        div [] [ str "No project open" ]
    | Some project ->
        // Update project components for current model
        let updatedProject = ModelHelpers.getUpdatedLoadedComponents project model
        let updatedModel = { model with CurrentProj = Some updatedProject }

        // Ensure consistency and extract valid waves
        let okWaves, okSelectedWaves = ensureWaveConsistency wsModel

        // Helper: Filter waves based on search string
        let filteredWaves =
            match wsModel.WaveSearchString with
            | "" | "-" -> filterWaves wsModel okWaves dispatch
            | "*" -> okSelectedWaves |> List.map (fun waveId -> wsModel.AllWaves.[waveId])
                                     |> fun waves -> filterWaves wsModel waves dispatch
            | _ -> filterWaves wsModel okWaves dispatch

        let filteredWaveNames = filteredWaves |> List.map (fun wave -> wave.ViewerDisplayName)

        // Extract sheet names from wave names by splitting on '.'
        let sheetNames =
            filteredWaveNames
            |> List.collect (fun name ->
                name.Split('.')
                |> Array.map (fun s -> s.Trim().ToLowerInvariant())
                |> Array.toList)

        // Count occurrences for each sheet name using countBy. This is used to determine the number of matching waves for each sheet.
        let sheetCounts = sheetNames |> List.countBy id

        // Determine the color based on whether the sheet is in the current search results
        let sheetColor (sheet: SheetTree) =
            if List.contains (sheet.SheetName.ToLowerInvariant()) sheetNames then 
                IColor.IsCustomColor "pink"
            else 
                IColor.IsCustomColor "darkslategrey"

        // Return the number of matching waves for a given sheet
        let sheetMatches (sheet: SheetTree) =
            match List.tryFind (fun (name, _) -> name = sheet.SheetName.ToLowerInvariant()) sheetCounts with
            | Some (_, count) -> count
            | None -> 0

        // When a breadcrumb is clicked, update the search string to the sheet name. This is used to filter waves.
        let updateSearchStringHelper (sheet: SheetTree) : (Msg -> unit) -> unit =
            fun dispatch ->
                dispatch (UpdateWSModel (fun ws -> { ws with SheetSearchString = sheet.SheetName.ToUpperInvariant() }))

        // Build the breadcrumb configuration using our helper functions
        let breadcrumbConfig = {
            MiscMenuView.Constants.defaultConfig with
                ClickAction = updateSearchStringHelper
                ColorFun = sheetColor
                NoWaves = sheetMatches
        }

        // Compose the breadcrumb display
        let breadcrumbs = [
            div [ Style [ TextAlign TextAlignOptions.Center; FontSize "15px" ] ] [ str "Sheets with Design Hierarchy" ]
            MiscMenuView.hierarchyBreadcrumbs breadcrumbConfig dispatch updatedModel
        ]
        div [] breadcrumbs




////////////////////// HELPER FUNCTIONS  //////////////////////

let infoButton  : ReactElement =
    div 
        [
            HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline} {Tooltip.IsInfo} {Tooltip.IsTooltipRight}"
            Tooltip.dataTooltip "Find ports by any part of their name. '.' = show all. '*' = show selected. '-' = collapse all"
            Style [FontSize "25px"; MarginTop "0px"; MarginLeft "10px"; Float FloatOptions.Left]] 
        [str Constants.infoSignUnicode]

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


let selectWavesModalHlp25 (wsModel: WaveSimModel) (dispatch: Msg -> unit) (model: Model): ReactElement =
    // See WaveSimSelect.selectWavesModal for the existing Waveform Selector top level view
    // This contains a search box to filter waves, and a wave selection box to select/deselect
    // the (filtered) waves for display.
    // Both will be changed in part B
    //
    // The wave selection box will be replaced by a breadcrumb display,
    // and a new wave selection box, side-by-side
    //
    // The current wave selection rows are displayed in a table by the recursive function makeSheetRow
    // This makes, hierarchically, rows for sheets and its components and ports
    // Subsheets are displayed as a single row with a button to open the subsheet
    //
    // The new wave selection rows should not use this sheet hierarchy (the breadcrums deal with that).
    // For an MVP they could be displayed as a flat list of components names, which open when clicked to show ports.
    // This is a simplification of the current display, and implemented in the function makeComponentRow.
    //
    // For a full implementation:
    // The display should depend on the number of (filtered) waves, fewer waves should show more detail.
    // Each row should contain the component sheet name.
    // Whether ports are hidden or not should depend on the number of waves.
    // Each row should (maybe) include component group, with (maybe) the list ordered by class and then component name
    //        See makeComponentGroup for how components are grouped in current display.
    // The display should be adjusted so that the user can quickly select any number of waves.
    // It should also be possible to select all waves in a sheet.
    // Note that each signal can be selected in multiple places, from its driving port, and its receiving port(s).
    // Although these are separate waves only one wave from each signal will be allowed in the waveform viewer.
    // Duplicates are filtered out: 

    // Helper to close the modal and reset search string
    let closeModal () =
        dispatch (UpdateWSModel (fun ws -> { ws with WaveModalActive = false; SearchString = "" }))

    // Handler for closing the modal via the delete button,
    // showing a confirmation popup if more than 50 waves are selected.
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
        // Always reset the search string
        dispatch (UpdateWSModel (fun ws -> { ws with SearchString = "" }))

    Modal.modal [
        Modal.IsActive wsModel.WaveModalActive
        Modal.Props [ Style [ ZIndex 20000 ] ]
    ] [
        // Modal background to allow closing on click
        Modal.background [
            Props [
                OnClick (fun _ -> dispatch (UpdateWSModel (fun ws -> { ws with WaveModalActive = false })))
            ]
        ] []
        
        // Main modal card
        Modal.Card.card [ Props [ Style [ MinWidth "900px" ] ] ] [
            // Modal header with title and delete button
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
            // Modal body: top row for info and search bar, then two columns for selector and breadcrumbs
            Modal.Card.body [
                Props [
                    Style [
                        OverflowY OverflowOptions.Visible
                        Display DisplayOptions.Grid
                        GridTemplateColumns "1fr 1fr"
                        GridGap "10px"
                        Width "100%"
                    ]
                ]
            ] [
                // Top row: info button and waves count
                div [
                    Style [
                        GridColumn "1 / span 2"
                        MarginBottom "15px"
                        Display DisplayOptions.Flex
                        JustifyContent "space-between"
                        AlignItems AlignItemsOptions.Center
                    ]
                ] [
                    div [] [ infoButton ]
                    div [] [ str (sprintf "%d waves selected" (List.length wsModel.SelectedWaves)) ]
                ]
                // Search bar spanning both columns
                div [
                    Style [
                        GridColumn "1 / span 2"
                        MarginBottom "15px"
                        Display DisplayOptions.Flex
                        FlexDirection "column"
                    ]
                ] [
                    div [] [waveSearchBox wsModel dispatch]
                    div [] [ sheetSearchBox wsModel dispatch ]
                    div [] [ componentSearchBox wsModel dispatch ]
                    div [] [ portSearchBox wsModel dispatch ]
                    div [] [ componentTypeSearchBox wsModel dispatch ]
                ]
                // Left column: placeholder for wave selection component
                
                div [] [
                            let wavestooutput = selectWavesHlp25 wsModel dispatch
                            wavestooutput |> renderwaves wsModel dispatch 
                        ]
                // Right column: Breadcrumb display
                div [] [ waveSelectBreadcrumbs wsModel dispatch model ]
            ]
            // Modal footer with the Done button
            Modal.Card.foot [ Props [ Style [ Display DisplayOptions.InlineBlock; Float FloatOptions.Right ] ] ] [
                Fulma.Button.button [
                    Fulma.Button.OnClick (fun _ -> closeModal ())
                    Fulma.Button.Color IsSuccess
                    Fulma.Button.Props [ Style [ Display DisplayOptions.InlineBlock; Float FloatOptions.Right ] ]
                ] [ str "Done" ]
            ]
        ]
    ]


//////////////////////////////////// TEST FUNCTIONS //////////////////////////////////////////
let defaultWaveSimModel: WaveSimModel = {
    DefaultCursor = CursorType.Default
    WSConfig = {LastClock = 0; FirstClock = 0; FontSize = 12; FontWeight = 10}
    WSConfigDialog = None
    State = WaveSimState.Empty
    TopSheet = ""
    Sheets = Map.empty
    AllWaves = Map.empty
    SelectedWaves = []
    Hlp25State = None
    StartCycle = 0
    ShownCycles = 0
    SamplingZoom = 1
    CursorDisplayCycle = 0
    CursorExactClkCycle = 0
    ClkCycleBoxIsEmpty = false
    Radix = NumberBase.Dec 
    WaveformColumnWidth = 0.0
    WaveModalActive = false
    RamModalActive = false
    RamComps = []
    SelectedRams = Map.empty
    RamStartLocation = Map.empty
    SearchString = "ALU"
    ShowSheetDetail = Set.empty
    ShowComponentDetail = Set.empty
    ShowGroupDetail = Set.empty
    HoveredLabel = None
    DraggedIndex = None
    PrevSelectedWaves = None
    ScrollbarTbWidth = 0.0
    ScrollbarTbPos = 0.0
    ScrollbarTbOffset = None
    ScrollbarBkgWidth = 0.0
    ScrollbarBkgRepCycs = 0
    ScrollbarQueueIsEmpty = true
    WaveSearchString = ""
    SheetSearchString = ""
    ComponentSearchString = ""
    PortSearchString = ""
    ComponentTypeSearchString = ""
    HighlightedSheets = Set.empty
}







// let testWaveSelectBreadcrumbs model dispatch =
//     let action _ _ = ()
//     PopupHelpers.closablePopup
//         "Design Hierarchy of current sheet"
//         (waveSelectBreadcrumbs defaultWaveSimModel dispatch model)
//         (div [] []) []
//         dispatch

// let testWaveSelectModal model dispatch =
//     let action _ _ = ()
//     PopupHelpers.closablePopup
//         "Select Waves Modal"
//         (selectWavesModalHlp25 defaultWaveSimModel dispatch model)
//         (div [] []) []
//         dispatch


//////////////////////////////////// CHANGES MADE TO THE ORIGINAL CODE //////////////////////////////////////////

// type BreadcrumbConfig = {
//     AllowDuplicateSheets: bool
//     BreadcrumbIdPrefix: string
//     ColorFun: SheetTree -> IColor
//     ClickAction: SheetTree -> (Msg -> unit) -> unit
//     ElementProps: IHTMLProp list
//     ElementStyleProps: CSSProp list
//     /// button options (other than OnClick and Color)
//     ButtonOptions: Button.Option list 
//     NoWaves: SheetTree -> int    <----- ADDED THIS FIELD TO HELP DISPLAY NUMBER OF MATCHES. LATER USED IN MiscMenuView.makeGridFromSheetsWithPositions
//     }



// let defaultConfig = {
//         AllowDuplicateSheets = false
//         BreadcrumbIdPrefix = "BreadcrumbDefault"
//         ColorFun = fun _ -> IColor.IsGreyDark
//         ClickAction = fun _ _ -> ()
//         ElementProps = [ ]
//         ElementStyleProps = [           
//             Border "2px"            
//             BorderColor "LightGrey"
//             BorderRightColor "DarkGrey"
//             BorderStyle "Solid"
//             Background "LightGrey"
//             Padding "5px"]
//         ButtonOptions = [
//                 Button.Size IsSmall
//                 Button.IsOutlined
//                 Button.IsExpanded
//                 Button.IsFocused true
//                 Button.Disabled false
//                 ]
//         NoWaves = fun _ -> 0     <----- INITIALISED TO 0
//     }


// Modified this function to display the number of matches on each breadcrumb sheet
// let makeGridFromSheetsWithPositions
//         (cfg: BreadcrumbConfig)
//         (dispatch: Msg -> unit)
//         (posL: (CSSGridPos*SheetTree) list)
//             : ReactElement =
//     posL
//     |> List.map (fun (pos, sheet) ->
//             let crumbId = cfg.BreadcrumbIdPrefix + ":" + sheet.SheetName + ":" + String.concat ":" sheet.LabelPath
//             let extraStyle = match sheet.SubSheets with | [] -> [BackgroundColor "white"; BorderWidth "0px"] | _ -> cfg.ElementStyleProps
//             let number = cfg.NoWaves sheet    <----- ADDED THIS LINE TO GET NUMBER OF MATCHES
//             gridElement
//                 crumbId
//                 cfg.ElementProps
//                 (extraStyle)
//                 pos
//                 (Button.button [
//                     Button.Props [Id crumbId]
//                     Button.Color (cfg.ColorFun sheet)
//                     Button.Modifiers [Modifier.TextColor IColor.IsLight]
//                     Button.OnClick(fun ev -> cfg.ClickAction sheet dispatch)
//                     ] [
//                         str $"{sheet.SheetName}"
//                         if number > 0 then
//                             div [
//                                 Style [     
//                                     Position PositionOptions.Absolute
//                                     CSSProp.Bottom "0px"
//                                     CSSProp.Right "0px"
//                                     BackgroundColor "transparent"
//                                     Width "20px" 
//                                     Height "20px"
//                                     Display DisplayOptions.Flex
//                                     AlignItems AlignItemsOptions.Center
//                                     JustifyContent "center"
//                                     FontSize "10px" 
//                                     Color "white"  
//                                     CSSProp.Overflow OverflowOptions.Visible
//                                 ]  <------ STYLING USED TO DISPLAY NUMBER OF MATCHES AT BOTTOM RIGHT CORNER
//                             ] [str $"{number}"]   <----- ADDED THIS TO DISPLAY NUMBER OF MATCHES
//                         else
//                             null
//                     ]))             

//     |> gridBox Constants.gridBoxSeparation 

