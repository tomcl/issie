module HLP25CodeBsc3321

open Hlp25Types
open Fable.React
open Fable.React.Props
open Fulma
open Fulma.Extensions.Wikiki
open WaveSimStyle
open WaveSimHelpers
open SimGraphTypes
open SimTypes
open DiagramStyle
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

open HLP25CodeBdw722

//------------------------------------- Part B ---------------------------------------------------//
//----------------------------- Sample Code for HLP25 --------------------------------------------//
//----------------------------- use these to get started -----------------------------------------//
//-------------------- Modify the signatures as you see fit for your implementation---------------//
//------------------------------------------------------------------------------------------------//



//--------------------------------------- Helper functions ----------------------------------//

let toggleSelectAll (selected: bool) (wsModel: WaveSimModel) dispatch : unit =
    let start = TimeHelpers.getTimeMs ()
    let selectedWaves = if selected then Map.keys wsModel.AllWaves |> Seq.toList else []
    //printf "length: %A" (List.length selectedWaves)
    dispatch <| GenerateWaveforms {wsModel with SelectedWaves = selectedWaves}
    |> TimeHelpers.instrumentInterval "toggleSelectAll" start

/// Row in wave selection table that selects all values in wsModel.AllWaves
let selectAll (wsModel: WaveSimModel) dispatch =
    let allWavesSelected = Map.forall (fun index _ -> isWaveSelected index wsModel) wsModel.AllWaves

    tr (summaryProps false (SheetItem []) wsModel dispatch) [
        th [] [
            Checkbox.checkbox []
                [ Checkbox.input [
                    Props 
                        (checkboxInputProps @ [
                            Checked allWavesSelected
                            OnChange(fun _ -> toggleSelectAll (not allWavesSelected) wsModel dispatch )
                    ])
                ] ]
            ]
        th [] [str "Select All"]
    ]

/// Toggle selection for a single wave.
let toggleWaveSelection (index: WaveIndexT) (wsModel: WaveSimModel) (dispatch: Msg -> unit) =
    //printfn $"toggling {index}"
    let selectedWaves =
        if List.contains index wsModel.SelectedWaves then
            List.except [index] wsModel.SelectedWaves
        else [index] @ wsModel.SelectedWaves
    let wsModel = {wsModel with SelectedWaves = selectedWaves}
    dispatch <| GenerateWaveforms wsModel
   

/// Toggle selection of a list of waves.
let toggleSelectSubGroup (wsModel: WaveSimModel) dispatch (selected: bool) (waves: WaveIndexT list) =
    let comps = (Simulator.getFastSim()).WaveComps
    let selectedWaves =
        if selected then
            let wavesWithMinDepth =
                if waves = [] then [] else
                    waves
                    |> List.groupBy (fun waves -> comps[waves.Id].AccessPath.Length)
                    |> List.sort
                    |> List.head
                    |> snd

            List.append wsModel.SelectedWaves wavesWithMinDepth

        else
            List.except waves wsModel.SelectedWaves
    dispatch <| GenerateWaveforms {wsModel with SelectedWaves = selectedWaves}


/// Table row of a checkbox and name of a wave.
let checkboxRow (wsModel: WaveSimModel) dispatch (index: WaveIndexT) =
    let fontStyle = if isWaveSelected index wsModel then boldFontStyle else normalFontStyle
    let wave = wsModel.AllWaves[index]
    tr  [ fontStyle ]
        [
            td  [ noBorderStyle ]
                [ Checkbox.checkbox []
                    [ Checkbox.input [
                        Props (checkboxInputProps @ [
                            OnChange(fun _ -> toggleWaveSelection index wsModel dispatch )
                            Checked <| isWaveSelected index wsModel 
                        ])
                    ] ]
                ]
            td  [ noBorderStyle ]
                [ str wave.DisplayName ]
        ]



/// Implemements a checkbox, with toggle state stored in WaveSimModel under ShowDetailMap
/// using  waveIds as key.
let checkBoxItem  wsModel isChecked waveIds  dispatch =
    Checkbox.checkbox [] [
        Checkbox.input [
            Props [
                Checked isChecked
                OnChange (fun _ -> toggleSelectSubGroup 
                                        wsModel 
                                        dispatch 
                                        (not isChecked)
                                        waveIds)
            ]
        ]
    ]

/// Implemements a checkbox, with toggle state determined by SelectedWaves.
let waveCheckBoxItem  (wsModel:WaveSimModel) (waveIds:WaveIndexT list)  dispatch =
    let comps = Simulator.getFastSim().WaveComps
    let minDepthSelectedWaves =
        if waveIds = [] then [] else
            waveIds
            |> List.groupBy (fun waveId -> comps[waveId.Id].AccessPath.Length)
            |> List.sort
            |> List.head
            |> snd
    let checkBoxState =
        List.exists (fun w -> List.contains w wsModel.SelectedWaves) minDepthSelectedWaves
    Checkbox.checkbox [] [
        Checkbox.input [
            Props [
                Checked checkBoxState
                OnChange (fun _ -> toggleSelectSubGroup                                         
                                        wsModel 
                                        dispatch
                                        (not checkBoxState)
                                        waveIds)                                         
            ]
        ]
    ]

/// Implements one row (with one port) that can be selected or deselected
let makePortRow (ws: WaveSimModel) (dispatch: Msg -> Unit) (waves: Wave list)  =
    let wave = 
        match waves with 
        | [waves] -> waves 
        | _ -> failwithf "What? {waves.Length} waves passed to portRow"
    let subSheet =
        match wave.SubSheet with
        | [] -> str (Simulator.getFastSim().SimulatedTopSheet)
        | _  -> subSheetsToNameReact wave.SubSheet

    tr [] [
        td [] [waveCheckBoxItem ws  [wave.WaveId] dispatch]
        td [] [str wave.PortLabel]
        //td [] [str <| match (fst wave.WaveId.Id) with ComponentId s -> s[0..5] ]
        td [] [str <| match wave.WaveId.PortType with | PortType.Output -> "Output" | PortType.Input -> "Input"]
        ]

/// Returns a tr react element representing a thing with a checkbox with summary name and details beneath
/// Implements a group of waves, components or sheets that can be hidden or shown, with a checkbox to select all
/// and a summary item that can be clicked to show details.
let makeSelectionGroup 
        (showDetails:bool)
        (ws: WaveSimModel) 
        (dispatch: Msg -> Unit)
        (summaryItem: ReactElement) 
        (rowItems: ReactElement list) 
        (cBox: CheckBoxStyle) 
        (waves: Wave list)  =
    let wi = wavesToIds waves
    tr
        (summaryProps false cBox ws dispatch) [
            th [] [
                waveCheckBoxItem ws  wi  dispatch
            ]
            th [] [
                details
                    (detailsProps showDetails cBox ws dispatch)
                    [   
                        summary
                            (summaryProps true cBox ws dispatch)
                            [ summaryItem ]
                    
                        Table.table [] [  tbody [] rowItems                             
                    ]
                ]
            ]
        ]





//------------------------------------- My attempt at implementing the makeWaveDisplayTree function---------------------------------------------------//



/// Differentiator between the different node types. 
// type WTType =
//     | SheetNode
//     | ComponentNode
//     | LeafNode

// /// Node "variable" which lets us keep track of name, type and any waves associated with the current level of hierarchy
// type WTNode = {
//     Title: string
//     Kind: WTNodeKind
//     Waves: Wave list
// }

// /// Internal nodes on the tree with reference to child nodes
// type WaveTreeNode = {
//     WTNode: WTNode
//     Children: WaveTreeNode list
// }


// /// "Master" return tree which is returned and passed as parameter to the eventual ImplementWaveDisplayTree
// type WaveDisplayTree = WaveTreeNode list


// /// Filter the waves based on the entered search string(s)
// let gatherFilteredWaves (wsModel: WaveSimModel) =
//     let okWaves, okSelectedWaves = ensureWaveConsistency wsModel
//     let st = wsModel.SearchString.ToUpper()
//     applyFiltering wsModel st okWaves okSelectedWaves

// /// Group the waves by subsheet
// let groupBySubSheet (waves: Wave list) =
//     waves
//     |> List.groupBy (fun w ->
//         match w.SubSheet with
//         | [] -> "Top-Level"
//         | path -> String.concat "." path
//     )

// /// Constructor for making a leaf node which is the lowest level of the tree
// let makeLeafNode (w: Wave) : WaveTreeNode =
//     {
//         WTNode = {
//             Title = w.ViewerDisplayName
//             Kind = LeafNode
//             Waves = [w]
//         }
//         Children = []
//     }

// /// Constructor for a component node which is an intermediate node of the tree with leaf nodes as children.
// let makeComponentNode (compName: ComponentGroup) (wavesInComp: Wave list) : WaveTreeNode =
//     let leafNodes =
//         wavesInComp
//         |> List.map makeLeafNode

//     {
//         WTNode = {
//             Title = string compName
//             Kind = ComponentNode
//             Waves = []
//         }
//         Children = leafNodes
//     }

// /// Constructor for sheet nodes which are the master node(s) of the tree which has component nodes as its children.
// let makeSheetNode (sheetName: string) (waves: Wave list) : WaveTreeNode =
//     let fs = Simulator.getFastSim()

//     let groupedByComp =
//         waves
//         |> List.groupBy (fun w -> getCompGroup fs w)

//     let componentNodes =
//         groupedByComp
//         |> List.map (fun (grp, wavesInComp) ->
//             makeComponentNode grp wavesInComp
//         )

//     {
//         WTNode = {
//             Title = sheetName
//             Kind = SheetNode
//             Waves = []
//         }
//         Children = componentNodes
//     }

// /// Top level function to make the Tree by first grouping by subsheet and then calling the subsheet constructor for each grouping->calls the component grouping
// /// constuctors->calls the leaf nodes constructors.
// let makeWaveDisplayTree (wsModel: WaveSimModel) : WaveDisplayTree =
//     let waves = gatherFilteredWaves wsModel
//     let grouped = groupBySubSheet waves

//     grouped
//     |> List.map (fun (sheetName, wavesInSheet) ->
//         makeSheetNode sheetName wavesInSheet
//     )



//------------------------------------- My attempt at implementing the selectWavesHlp25 function---------------------------------------------------//


/// The functions below all work together to implement the selectWavesHlp25 function.
/// Displays a react element that allows the user to select waves for display in the waveform viewer.
/// Implements just simple flat sheet with grouping by component, giving a clean user interface.
/// I have had to import some modules from WaveSimSelect as there is no forward referencing.

///This is a type which seeks to provide a helpful output selection from the filtering that can be used to displau waves.
type WaveSelectionOutput = {
    WaveList : list<Wave>
    ShowDetails : bool
}

/// Filters the waves based on the entry in the search text. "*" should display all possible selected waves and otherwise it should match based on the "searchText"
let filterSelectedWaves 
    (ws: WaveSimModel)
    (searchText: string)
    (okWaves: list<Wave>)
    (okSelectedWaves: list<WaveIndexT>)
    : list<Wave> =
    
    match searchText with
    | "*" ->
        okSelectedWaves
        |> List.map (fun wi -> ws.AllWaves.[wi])
    | _ ->
        okWaves
        |> List.filter (fun x -> 
            x.ViewerDisplayName.ToUpper().Contains(searchText)
        )
         
 /// function copied from the initial waveSimSelect file.
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


/// Top level function that controls filtering of which waves to select and passing it onto the rendering stage. Abstracts the filtering and rendering!
let selectWavesHlp25 (ws: WaveSimModel) (dispatch: Msg -> unit) : WaveSelectionOutput =
    if not ws.WaveModalActive then 
        { WaveList = []; ShowDetails = false }
    else
        let okWaves, okSelectedWaves = ensureWaveConsistency ws
        let searchText = ws.SearchString.ToUpper()
        let wavesToDisplay = filterSelectedWaves ws searchText okWaves okSelectedWaves
        
        let showDetails = 
            ((wavesToDisplay.Length < 10) || searchText.Length > 0)
            && searchText <> "-"
        { WaveList = wavesToDisplay
          ShowDetails = showDetails }

/// Function that handles creation of a single row which contains waves that have been grouped by component per subsheet. This is a UI function that handles UI rendering. 
let makeFlatGroupRow
    (showDetails: bool)
    (ws: WaveSimModel)
    (dispatch: Msg -> Unit)
    (subSheet: string list)
    (grp: ComponentGroup)
    (wavesInGroup: Wave list)
    : ReactElement =
    
    let cBox = GroupItem (grp, subSheet)
    let summaryReact = summaryName ws cBox subSheet wavesInGroup
    let rowItems =
        wavesInGroup
        |> List.map (fun wave ->
            tr [] [
                td [] [ str wave.ViewerDisplayName ]
                td [] [
                    input [
                            Type "Checkbox"
                            OnChange(fun _ -> toggleWaveSelection wave.WaveId ws dispatch )
                            Checked <| isWaveSelected wave.WaveId ws 
                    ] 
                ]
            ]
        )
    makeSelectionGroup showDetails ws dispatch summaryReact rowItems cBox wavesInGroup

/// UI function that is responsible for grouping waves by subsheet->then grouping waves based on component in each subsheet. Calls subfunction to handle rendering of the table rows themselves.
let makeFlatList 
    (ws: WaveSimModel)
    (dispatch: Msg -> Unit)
    (subSheet: string list)
    (waves: Wave list)
    (showDetails: bool)
    : ReactElement =

    let fs = Simulator.getFastSim()

    // 1) Group waves first by their subSheet (Datapath, ControlPath, etc.)
    let groupedBySubSheet =
        waves
        |> List.groupBy (fun w -> 
            match w.SubSheet with
            | [] -> "Top-Level" // Default if no subsheet
            | sheetPath -> String.concat "." sheetPath // Create readable name
        )

    let subSheetRows =
        groupedBySubSheet
        |> List.map (fun (subSheetName, wavesInSubSheet) ->
            let componentGroups =
                wavesInSubSheet
                |> List.groupBy (fun wave -> getCompGroup fs wave)

            let groupRows =
                componentGroups
                |> List.map (fun (grp, groupWaves) ->
                    makeFlatGroupRow showDetails ws dispatch subSheet grp groupWaves
                )

            // Wrap all component groups for this subSheet in a collapsible section
            makeSelectionGroup showDetails ws dispatch (str subSheetName) groupRows (SheetItem [subSheetName]) wavesInSubSheet
        )

    // 3) Wrap everything in a table
    Table.table [
        Table.IsBordered
        Table.IsFullWidth
        Table.Props [ Style [ BorderWidth 0 ] ]
    ] [
        tbody [] subSheetRows
    ]


///Top level UI function that calls the makeFlatList function. The output of HlpSelectWaves25 (target function for assignment) is what is passed as parameter into this function to handle all the rendering. 
let renderwaves (ws: WaveSimModel) (dispatch: Msg -> unit) (waveselect: WaveSelectionOutput) : ReactElement =
    let showDetails = waveselect.ShowDetails
    let wavelist = waveselect.WaveList
    // Use the new flat approach
    makeFlatList ws dispatch [] wavelist showDetails





