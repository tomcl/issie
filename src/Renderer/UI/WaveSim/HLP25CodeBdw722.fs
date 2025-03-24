module HLP25CodeBdw722

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
//added libraries///////////////////
open Fulma


// NOTES:
// 1. Added new fields to 'WaveSimModel' for filtering:
//  - 'WaveSearchString'
//  - 'SheetSearchString'
//  - 'ComponentSearchString'
//  - 'PortSearchString'
//  - 'ComponentTypeSearchString'
//  - 'HighlightedSheets' (tracks sheets containing matches from search)
// 2. Filtering functions apply logical AND across all search criteria
// 3. When the wave search box changes, component and port searches are cleared

//------------------------------------- Part B ---------------------------------------------------//
//----------------------------- Sample Code for HLP25 --------------------------------------------//
//----------------------------- use these to get started -----------------------------------------//
//-------------------- Modify the signatures as you see fit for your implementation---------------//
//------------------------------------------------------------------------------------------------//

/// 1. An input box for a string that can be used to seach wave names in the Waveform Selector.
/// Any substring of a wave name of form 'sheet.CompName.PortName' should be matched.
/// 2. A search box for parts of sheet names
/// 3. A search box for component names.
/// 4. A search box for port names
/// 5. (optional) a search box for component type
/// Overall search is AND of all five searches.
/// The search boxes should be able to filter a list of wave names in the Waveform Selector.
/// In addition the search boxes must change a breadcrumb display so that the user can see coloured the
/// sheets in which matches are found.
/// The Sheet search box 2.  has additional functionality to allow the user to navigate to the design visually.
/// When a breadcrumb is clicked the corresponding sheet name is displayed in the sheet box and displayed
/// ports and components are restricted to those in the sheet.
/// Box 1 has additional functionality: when it is changed the component and port boxes are emptied. (good?)

//------------------------------------UI ELEMENTS FOR SEARCH BOXES----------------------------------//

// Default styling for each search box
let searchBoxStyle = Style [
    MarginBottom "1rem"
    Width "50%"
]

// Search box wave names
let waveSearchBox (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    div [] [
        Input.text [
            Input.Option.Props [searchBoxStyle]
            Input.Option.Placeholder "Search wave names..."
            Input.Option.OnChange (fun value -> 
                dispatch <| UpdateWSModel (fun wsm -> 
                {wsModel with
                    WaveSearchString = value.Value.ToUpper()
                    ComponentSearchString = "" // Reset component search when wave search changes
                    PortSearchString = "" // Reset port search when wave search changes
                })
            )
        ]
    ]

// Search box for sheet names
let sheetSearchBox (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    div [] [
        Input.text [
            Input.Option.Props [searchBoxStyle]
            Input.Option.Placeholder "Search sheet names..."
            Input.Option.OnChange (fun value ->
                dispatch <| UpdateWSModel (fun wsm -> {wsModel with SheetSearchString = value.Value.ToUpper()})
            )
        ]
    ]

// Search box for component names
let componentSearchBox (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    div [] [
        Input.text [
            Input.Option.Props [searchBoxStyle]
            Input.Option.Placeholder "Search component names..."
            Input.Option.OnChange (fun value ->
                dispatch <| UpdateWSModel (fun wsm -> {wsModel with ComponentSearchString = value.Value.ToUpper()})
            )
        ]
    ]

// Search box for port names
let portSearchBox (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    div [] [
        Input.text [
            Input.Option.Props [searchBoxStyle]
            Input.Option.Placeholder "Search port names..."
            Input.Option.OnChange (fun value ->
                dispatch <| UpdateWSModel (fun wsm -> {wsModel with PortSearchString = value.Value.ToUpper()})
            )
        ]
    ]

// Search box for component type
let componentTypeSearchBox (wsModel: WaveSimModel) (dispatch: Msg -> unit) :  ReactElement =
    div [] [
        Input.text [
            Input.Option.Props [searchBoxStyle]
            Input.Option.Placeholder "Search component types..."
            Input.Option.OnChange (fun value -> 
                dispatch <| UpdateWSModel (fun wsm -> {wsModel with ComponentTypeSearchString = value.Value.ToUpper()})
            )
        ]
    ]
//----------------------------------FILTERING FUNCTION(S) ANDED FOR EACH SEARCH BOX-----------------------------------//
// Applies filtering logic to the list of 'waves' based on active search criteria
// Filtering is an AND operation across wave, sheet, componnet, and port searches
let filterWaves (wsModel: WaveSimModel) (waves: Wave list) (dispatch: Msg -> unit) =
    // Process waves and track sheets with matches
    let filteredWaves,  matchingSheets =
        waves
        |> List.fold( fun (filtered, sheetSet) wave -> 
            let addMatchingSheet sheet = Set.add sheet sheetSet
            
            // Check if the wave matches the sheet search criteria
            let matchesSheet, updatedSheets1 =
                if wsModel.SheetSearchString = "" then true, sheetSet
                else
                    match wave.SubSheet with
                    | [] -> 
                        let topSheet = Simulator.getFastSim().SimulatedTopSheet
                        let matches = topSheet.ToUpper().Contains(wsModel.SheetSearchString)
                        if matches then true, addMatchingSheet []
                        else false, sheetSet
                    | sheets -> 
                        let matches = sheets |> List.exists (fun s -> s.ToUpper().Contains(wsModel.SheetSearchString))
                        if matches then true, addMatchingSheet sheets
                        else false, sheetSet

            // Check if the wave matches the component search criteria
            let matchesComponent, updatedSheets2 =
                if wsModel.ComponentSearchString = "" then true, updatedSheets1
                else 
                    let matches = wave.CompLabel.ToUpper().Contains(wsModel.ComponentSearchString)
                    if matches then true, addMatchingSheet wave.SubSheet
                    else false, updatedSheets1

            // Check if the wave matches the port search criteria
            let matchesPort, updatedSheets3 =
                if wsModel.PortSearchString = "" then true, updatedSheets2
                else 
                    let matches = wave.PortLabel.ToUpper().Contains(wsModel.PortSearchString)
                    if matches then true, addMatchingSheet wave.SubSheet
                    else false, updatedSheets2

            // Check if the wave matches wave name search criteria
            let matchesWave, updatedSheets4 =
                if wsModel.WaveSearchString = "" then true, updatedSheets3
                else 
                    let matches = wave.ViewerDisplayName.ToUpper().Contains(wsModel.WaveSearchString)
                    if matches then true, addMatchingSheet wave.SubSheet
                    else false, updatedSheets3
            
            // Add component type matching
            let matchesComponentType, updatedSheets5 =
                if wsModel.ComponentTypeSearchString = "" then true, updatedSheets4
                else
                    let fs = Simulator.getFastSim()
                    let comp = fs.WaveComps[wave.WaveId.Id]
                    let typeStr =
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
                        | Custom c -> "CUSTOM"
                        | Input1 _ -> "INPUT"
                        | Output _ -> "OUTPUT"
                        | Constant1 _ -> "CONSTANT"
                        | BusSelection _ -> "BUS-SELECT"
                        | IOLabel -> "IO-LABEL"
                        | _ -> comp.FType.ToString().ToUpper()

                    let matches = typeStr.Contains(wsModel.ComponentTypeSearchString)
                    if matches then true, addMatchingSheet wave.SubSheet
                    else false, updatedSheets4

            // Keep only waves that match ALL non-empty search criteria
            if matchesSheet && matchesComponent && matchesPort && matchesWave && matchesComponentType then
                (wave :: filtered, updatedSheets5)
            else
                (filtered, updatedSheets5)
        ) ([], Set.empty) // Initialize accumulator: empty filtered list and empty set

    // Update the model with highlighted sheets
    dispatch <| UpdateWSModel (fun wsm -> {wsModel with HighlightedSheets = matchingSheets})

    // Return filtered list of waves
    filteredWaves

//palceholder function to avoid errors in newSelectwaves
let ensureWaveConsistency (ws:WaveSimModel) : (list<Wave> * list<WaveIndexT>) =
    failwithf "already implemented in main issie code"

//palceholder function to avoid errors in newSelectwaves
let rec makeSheetRow (showDetails: bool) (ws: WaveSimModel) (dispatch: Msg -> unit) (subSheet: string list) (waves: Wave list) : ReactElement =
    failwithf "already implemented in main issie code"
//new fucntion to display wave selection rows using new filtering function for search boxes
let newSelectWaves (ws: WaveSimModel) (subSheet: string list) (dispatch: Msg -> unit) : ReactElement =
    if not ws.WaveModalActive then div [] []
    else
        let okWaves, okSelectedWaves = ensureWaveConsistency ws
        let wavesToDisplay = 
            match ws.WaveSearchString with
            | "-" when ws.ShowSheetDetail.Count <> 0 || ws.ShowComponentDetail.Count <> 0 || ws.ShowGroupDetail.Count <> 0 ->
                dispatch <| SetWaveSheetSelectionOpen (ws.ShowSheetDetail |> Set.toList, false)
                dispatch <| SetWaveGroupSelectionOpen (ws.ShowGroupDetail |> Set.toList, false)
                dispatch <| SetWaveComponentSelectionOpen (ws.ShowComponentDetail |> Set.toList, false)
                []
            | "" | "-" -> filterWaves ws okWaves dispatch
            | "*" -> 
                okSelectedWaves
                |> List.map (fun wi -> ws.AllWaves[wi])
                |> fun waves -> filterWaves ws waves dispatch
            | _ -> filterWaves ws okWaves dispatch
        let showDetails = ((wavesToDisplay.Length < 10)) 
        wavesToDisplay
        |> makeSheetRow showDetails ws dispatch []
//-------------------------------------------------------------------------------------------------------------------------------////

//-----------------------------------------------CHANGES MADE TO ORIGINAL CODE-----------------------------------------//
// type WaveSimModel = {
//    ...
//    WaveSearchString: string
//    SheetSearchString: string
//    ComponentSearchString: string
//    PortSearchString: string
//    ComponentTypeSearchString: string
//    HighlightedSheets: Set<string list>
//    ...
//}
//
//
//let initWSModel : WaveSimModel = {
//    ...
//    WaveSearchString = ""
//    SheetSearchString = ""
//    ComponentSearchString = ""
//    PortSearchString = ""
//    ComponentTypeSearchString: ""
//    HighlightedSheets = Set.empty
//...
//}

// Added to Playground.fs to test functionality and rendering of search boxes
// let testSearchBoxes (model: ModelType.Model) (dispatch: Msg -> unit) =
//     //create dummy WaveSimModel to use for testing
//     let (wsModel: WaveSimModel) = {
//         WaveSearchString = ""
//         SheetSearchString = ""
//         ComponentSearchString = ""
//         PortSearchString = ""
//         ComponentTypeSearchString = ""
//         HighlightedSheets = Set.empty
//         ... all othe required fields for wsModel
//     }
//     PopupHelpers.closablePopup
//         "Wave Search Interface"
//         (div [] [
//             div [] [
//                 waveSearchBox wsModel dispatch
//                 sheetSearchBox wsModel dispatch
//                 componentSearchBox wsModel dispatch
//                 portSearchBox wsModel dispatch
//                 componentTypeSearchBox wsModel dispatch
//             ]
//             div [Style [
//                 MarginTop "20px"
//                 PaddingTop "20px"
//                 BorderTop "1px solid #ccc"
//             ]] [
//                 str (sprintf "Wave Search: %s" wsModel.WaveSearchString)
//                 br []
//                 str (sprintf "Sheet Search: %s" wsModel.SheetSearchString)
//                 br []
//                 str (sprintf "Component Search: %s" wsModel.ComponentSearchString)
//                 br []
//                 str (sprintf "Port Search: %s" wsModel.PortSearchString)
//                 br []
//                 str (sprintf "Component Type Search")
//             ]
//         ])
//         (div [] [])
//         []
//         dispatch

// Added to Renderer.fs
// makeDebugItem "Search Boxes Test" None
//     (fun _ -> dispatch <| Msg.ExecFuncInMessage(Playground.Breadcrumbs.testSearchBoxes, dispatch) )