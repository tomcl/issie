module HLP25CodeB_aam522

// open Hlp25Types    // LINKING PROBLEMS WHEN DEFINING WTNode IN Hlp25Types.fs
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




//------------------------------------- Part B ---------------------------------------------------//
//----------------------------- Sample Code for HLP25 --------------------------------------------//
//----------------------------- use these to get started -----------------------------------------//
//-------------------- Modify the signatures as you see fit for your implementation---------------//
//------------------------------------------------------------------------------------------------//

// Additional modules to Open
open SimTypes
open WaveSimHelpers


open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props

// Types are defined such that they uniquely identify each type
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


// ------------------------


let makeWaveDisplayTree (wsModel: WaveSimModel) (showDetails: bool) (wavesToDisplay : Wave list): WaveDisplayTree =
    // Not required for MVP; but useful for a full implementation.
    // This function could be written as individual HLP25 code.
    //
    // The wave selector display is arranged as a tree of sheets, components and ports.
    // Nodes in the tree correspond to sets of items that are hidden and optionally displayed
    // The actual display is currently implemented by the recursive function makeSheetTree
    // this both doe sthe implementation and determines the recursive tree structure.
    // 
    // This function abstracts out the tree structure from the implementation.
    // It could be used to implement an optimal tree structure for a given set of
    // filtered waves.


    // Functions that give maximum controllability on the structure of the tree (within reason)
    // Can be made arbitrary
    let z = false

    // true: Wave -> () ... fasle: Component -> Wave -> () tree
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




// ----------------- Validate Tree

let rec validateSheet (node: WaveTreeNode) : bool =
    match node with
    | { WTNode = SheetNode _; HiddenNodes = nodes } when not (List.isEmpty nodes) -> true
    | _ -> false

and validateGroup (node: WaveTreeNode) : bool =
    match node with
    | { WTNode = GroupNode _; HiddenNodes = children } -> 
        List.forall validateComponent children
    | _ -> false

and validateComponent (node: WaveTreeNode) : bool =
    match node with
    | { WTNode = ComponentNode _; HiddenNodes = children } -> 
        List.forall validatePortNode children
    | _ -> false

and validatePortNode (node: WaveTreeNode) : bool =
    match node with
    | { WTNode = PortNode _; HiddenNodes = [] } -> true
    | _ -> false


// /// Converts a tree of wave display nodes into a react element that can be displayed in the Waveform Selector.
// /// The output display uses check boxes and clickables to display or hide nodes, and select/deselect waves.
// let getWaveDisplayName (wave: Wave) : string =
//     wave.SubSheet @ [wave.PortLabel] 
//     |> List.reduce (fun acc s ->acc + "." + s)
// let implementWaveSelector (wsModel: WaveSimModel) (dispatch: Msg -> unit) (wTree: WaveDisplayTree): ReactElement =
//     // This function implements the display of the waveform selection table
//     // as a set of rows that can be hidden or displayed.
//     // The structure and order of the rows is determined by the tree structure.
//     // Details to display in each row are determined by the node content.
//     // Additional details can be looked up from wsModel if necessary.
    
//     /// Get the waves associated with a node for checkbox operations
//     let getWavesFromNode (node: WaveTreeNode) : Wave list =
//         let rec collectWaves (n: WaveTreeNode) : Wave list =
//             match n.WTNode with
//             | PortNode wave -> [wave]
//             | _ -> List.collect collectWaves n.HiddenNodes
//         collectWaves node
    
//     /// Create a row for a port node (leaf node in tree)
//     let makePortNodeRow (wave: Wave) : ReactElement =
//         let subSheet =
//             match wave.SubSheet with
//             | [] -> str (Simulator.getFastSim().SimulatedTopSheet)
//             | _  -> subSheetsToNameReact wave.SubSheet

//         tr [] [
//             td [] [waveCheckBoxItem wsModel [wave.WaveId] dispatch]
//             td [] [str wave.PortLabel]
//             td [] [
//                 str <| 
//                 match wave.WaveId.PortType with 
//                 | PortType.Output -> "Output" 
//                 | PortType.Input -> "Input"
//             ]
//         ]
    
//     /// Create a row for a component node with its port details
//     let rec makeComponentNodeRow (node: WaveTreeNode) : ReactElement =
//         match node.WTNode with
//         | ComponentNode compId ->
//             let fc = Simulator.getFastSim().WaveComps[compId]
//             let waves = getWavesFromNode node
//             let cBox = ComponentItem fc
//             let summaryReact = summaryName wsModel cBox fc.SubSheet waves
//             let portRows = 
//                 node.HiddenNodes
//                 |> List.map (fun pNode -> 
//                     match pNode.WTNode with
//                     | PortNode wave -> makePortNodeRow wave
//                     | _ -> failwithf "Expected PortNode but got something else")

//             makeSelectionGroup node.ShowDetails wsModel dispatch summaryReact portRows cBox waves
//         | _ -> failwithf "Expected ComponentNode but got something else"
    
//     /// Create a row for a group node with its component details
//     let rec makeGroupNodeRow (node: WaveTreeNode) : ReactElement =
//         match node.WTNode with
//         | GroupNode (cGroup, subSheet) ->
//             let waves = getWavesFromNode node
//             let cBox = GroupItem (cGroup, subSheet)
//             let summaryReact = summaryName wsModel cBox subSheet waves
            
//             let componentRows =
//                 node.HiddenNodes
//                 |> List.map (fun cNode -> 
//                     match cNode.WTNode with
//                     | ComponentNode _ -> makeComponentNodeRow cNode
//                     | PortNode  wave -> makePortNodeRow wave 
//                     | _ -> failwithf "Shouldn't happen since only a Component or a Wave can be under a Group "
//                     // failwithf "Expected ComponentNode but got something else: WaveNode  "
//                 )
//             makeSelectionGroup node.ShowDetails wsModel dispatch summaryReact componentRows cBox waves
//         | _ -> failwithf "Expected GroupNode but got something else"
    
//     /// Create a row for a sheet node with its group and subsheet details
//     let rec makeSheetNodeRow (node: WaveTreeNode) : ReactElement =
//         match node.WTNode with
//         | SheetNode subSheet ->
//             // If this is the top sheet, we render it differently (as a table)
//             if subSheet = [] || subSheet = [wsModel.TopSheet] then
//                 let rows = 
//                     node.HiddenNodes
//                     |> List.map (fun childNode ->
//                         match childNode.WTNode with
//                         | GroupNode _ -> makeGroupNodeRow childNode
//                         | SheetNode _ -> makeSheetNodeRow childNode
//                         | ComponentNode _ -> makeComponentNodeRow childNode
//                         | PortNode wave -> makePortNodeRow wave
//                         )
                
//                 Table.table [
//                     Table.IsBordered
//                     Table.IsFullWidth
//                     Table.Props [
//                         Style [BorderWidth 0]
//                     ]] [tbody [] rows]
//             else
//                 // For non-top sheets
//                 let waves = getWavesFromNode node
//                 let cBox = SheetItem subSheet
//                 let summaryReact = summaryName wsModel cBox subSheet waves
                
//                 // Process child nodes based on their type
//                 let childRows = 
//                     node.HiddenNodes
//                     |> List.map (fun childNode ->
//                         match childNode.WTNode with
//                         | GroupNode _ -> makeGroupNodeRow childNode
//                         | SheetNode _ -> makeSheetNodeRow childNode
//                         | ComponentNode _ -> makeComponentNodeRow childNode
//                         | PortNode wave -> makePortNodeRow wave
//                         )
                
//                 makeSelectionGroup node.ShowDetails wsModel dispatch summaryReact childRows cBox waves
//         | _ -> failwithf "Expected SheetNode but got something else"
    
//     // Process each node in the wave display tree based on its type
//     let elements =
//         wTree
//         |> List.map (fun node ->
//             match node.WTNode with
//             | SheetNode _ -> makeSheetNodeRow node
//             | GroupNode _ -> makeGroupNodeRow node
//             | ComponentNode _ -> makeComponentNodeRow node
//             | PortNode wave -> makePortNodeRow wave)
    
//     // If there's only one element and it's a table, return it directly
//     match elements with
//     | [single] -> single
//     | _ -> 
//         // Otherwise wrap all elements in a table
//         Table.table [
//             Table.IsBordered
//             Table.IsFullWidth
//             Table.Props [
//                 Style [BorderWidth 0]
//             ]] [tbody [] elements]

// let doesThisWork (string: string) (strings: string list) : string =
//     strings @ [string]
//     |> List.reduce (fun acc s ->acc + "." + s)

// doesThisWork "abc" ["Z", "X", "Y"]