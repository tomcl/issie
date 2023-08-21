module Breadcrumbs
open Fable.React
open Fable.React.Props
open CommonTypes
open ModelType
open ModelHelpers
open FileMenuHelpers
open Optics
open Optics.Optic

(*
    This code uses getSheetTrees to obtain a tree of subsheets and displays this nicely
    as a partial tree of breadcrumbs.

    The tree of sheets can (worst case) be very large, so not all of the tree is displayed
*)


    
(*
// better to use CSS Grid - so commented out for now.
// might be useful somewhere?


/// creates a ReactElement table
/// crumbA must be a rectangular array
/// crumbA[y][x] is will be column x in row y of the table (indexed from 0)

let arrayToTable
        (tableProps: Props.IHTMLProp list)
        (colProps: Props.IHTMLProp list)
        (rowProps: Props.IHTMLProp list)
        (itemProps: Props.IHTMLProp list)
        (crumbA: ReactElement array array)
            : ReactElement =

    let getTableRow (rowEls: ReactElement array) =
        rowEls
        |> Array.map (fun el -> td itemProps [el])
        |> tr rowProps
    
    crumbA
    |> Array.map getTableRow
    |> Array.map (fun el -> tr colProps [el])
    |> table tableProps
*)   

let gridBox (gap:string) s =
    div [Style [Display DisplayOptions.InlineGrid; GridGap gap; JustifyContent "Start"]] s


let rec gridArea (gridPos: CSSGridPos): string =
    match gridPos with
    | PosElement(a,b) ->
        PosAreaSpan(a,b,1,1) |> gridArea
    | PosAreaAbsolute(xStart,yStart,xEnd,yEnd) ->
        PosAreaSpan(xStart,yStart, xEnd - xStart + 1, yEnd - yStart + 1) |> gridArea
    | PosAreaSpan(startX,startY,spanX,spanY) ->
        $"{startY} / {startX} / span {spanY} / span {spanX}"


/// a Grid item centre justified and occupying given area
let gridElement cssProps styleProps (pos: CSSGridPos) (x: ReactElement) =
    div (cssProps @ [
        Style (styleProps @ [
            Display DisplayOptions.Flex
            FlexDirection "column"
            TextAlign TextAlignOptions.Center
            JustifySelf JustifySelfOptions.Center;
            //AlignSelf AlignSelfOptions.Center;
            JustifyContent "center"
            Border "2px"
            BorderColor "Black"
            BorderStyle "Solid"
            Background "LightGrey"
            Padding "5px"
            Width "100%"
            Height "100%"
            GridArea (gridArea pos) ])]) [x]

let positionDesignHierarchyInGrid (rootSheet: string) (trees: Map<string,SheetTree>) =
    let tree = trees[rootSheet]
    let maxDepth = trees[rootSheet].Depth

    let rec getTreeHeight (tree: SheetTree) =
        match tree.SubSheets with
        | [] -> 1
        | subs ->
            subs
            |> List.sumBy getTreeHeight

    let rec getSheetPositions (startX: int) (startY: int) (tree: SheetTree) =
        let height = getTreeHeight tree
        match tree.SubSheets with
        | [] -> [PosAreaSpan(startX, startY, 1, height), tree]
        | others ->
            ((0,[]), others)
            ||> List.fold (fun (offset,posL') subSheet ->
                let offset' = offset + getTreeHeight subSheet
                let posL'' =
                    posL' @ getSheetPositions (startX+1) (startY+offset) subSheet
                offset', posL'')
            |> snd
            |> List.append [PosAreaSpan(startX, startY, 1, height), tree]

 
    getSheetPositions 1 1 tree

let positionRootAndFocusChildrenInGrid (root: string) (pathToFocus:string list) (trees: Map<string,SheetTree>) =
    let tree = trees[root] // tree from root
    let sheetsInPath =
        [0..pathToFocus.Length-1]
        |> List.map (fun i -> pathToFocus[0..i])
        |> List.map (fun path -> Option.get <| tree.lookupPath path)
    let children =
        (List.last sheetsInPath).SubSheets
        |> List.sortBy (fun sheet -> sheet.BreadcrumbName)
    children
    |> List.mapi (fun i sheet -> PosAreaSpan(sheetsInPath.Length + 1, 1, 1, children.Length),sheet)
      




let makeGridFromSheetsWithPositions
        (clickAction:string list -> (Msg -> unit) -> unit)
        (dispatch: Msg -> unit)
        (posL: (CSSGridPos*SheetTree) list)
            : ReactElement =
    posL
    |> List.map (fun (pos, sheet) ->
            gridElement
                [OnClick (fun ev -> clickAction sheet.LabelPath dispatch)]
                []
                pos
                (str $"{sheet.SheetName}"))
    |> gridBox "5pt" 
    

/// display as a ReactElement the breadcrumbs.
/// clickAction - what happens when a given breadcrumb is clicked.
/// project - the model project.
let makeBreadcrumbsFromPositions
        (sheetTreeMap: Map<string,SheetTree>)
        (positionSheetsInGrid: Map<string,SheetTree> -> (CSSGridPos*SheetTree) list)
        (clickAction: string list -> (Msg -> unit) -> unit)
        (dispatch: Msg -> unit)
             : ReactElement =
        sheetTreeMap
        |> positionSheetsInGrid
        |> makeGridFromSheetsWithPositions clickAction dispatch

/// Breadcrumbs of entire simulated design hierarchy.
/// Display as a ReactElement the breadcrumbs.
/// ClickAction - what happens when a given breadcrumb (labelled by its path to root) is clicked.
let hierarchyBreadcrumbs
        (clickAction: string list -> (Msg -> unit) -> unit)
        (dispatch: Msg -> unit)
        (model: Model) =
    mapOverProject (div [] []) model (fun p ->
        let root = Option.defaultValue p.OpenFileName model.WaveSimSheet
        let sheetTreeMap = getSheetTrees p
        makeBreadcrumbsFromPositions sheetTreeMap (positionDesignHierarchyInGrid root) clickAction dispatch)



/// Breadcrumbs of entire design hierarchy from given sheet
/// Display as a ReactElement the breadcrumbs.
/// rootSheet - root of hierrarchy displayed
/// clickAction - what happens when a given breadcrumb (labelled by its path to root) is clicked.
let hierarchyFromSheetBreadcrumbs
        (rootSheet: string)
        (clickAction: string list -> (Msg -> unit) -> unit)
        (dispatch: Msg -> unit)
        (model: Model) =
    mapOverProject (div [] []) model (fun p ->
        let sheetTreeMap = getSheetTrees p
        makeBreadcrumbsFromPositions sheetTreeMap (positionDesignHierarchyInGrid rootSheet) clickAction dispatch)

/// Breadcrumbs of entire design hierarchy of every root sheet in project
/// Display as a ReactElement the breadcrumbs.
/// rootSheet - root of hierrarchy displayed
/// clickAction - what happens when a given breadcrumb (labelled by its path to root) is clicked.
let allRootHierarchiesFromProjectBreadcrumbs
        (clickAction: string list -> (Msg -> unit) -> unit)
        (dispatch: Msg -> unit)
        (model: Model) =
    mapOverProject ([div [] []]) model (fun p ->
        let sheetTreeMap = getSheetTrees p
        allRootSheets sheetTreeMap
        |> Set.toList
        |> List.map (fun root ->
            makeBreadcrumbsFromPositions sheetTreeMap (positionDesignHierarchyInGrid root) clickAction dispatch))
        |> List.mapi (fun i el -> tr [Style [ BorderColor "white"; BorderWidth "10px"; BorderStyle "solid" ;Padding "50px"]] [td [CellSpacing "50px"] [el]])
        |> table []


/// Breadcrumbs of the focus sheet, with sheets on its path to root, and its children.
/// Provides navigation while occupying small vertical area. Untested.
/// *** TODO: hookthis function to simulation and test ***
/// Should be called extracting inputs from waveform simulation.
/// Display as a ReactElement the breadcrumbs.
/// clickAction - what happens when a given breadcrumb (labelled by its path to root) is clicked.
/// project - the model project
let smallSimulationBreadcrumbs
        (rootName: string)
        (pathToFocus: string list)
        (clickAction: string list -> (Msg -> unit) -> unit)
        (dispatch: Msg -> unit)
        (model: Model)
             : ReactElement =
    mapOverProject (div [] []) model (fun p ->       
        makeBreadcrumbsFromPositions (getSheetTrees p) (positionRootAndFocusChildrenInGrid rootName pathToFocus) clickAction dispatch)


