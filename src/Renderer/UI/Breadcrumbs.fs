module Breadcrumbs
open Fable.React
open Fable.React.Props
open Fulma
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

type BreadcrumbConfig = {
    BreadcrumbIdPrefix: string
    ColorFun: SheetTree -> IColor
    ClickAction: SheetTree -> (Msg -> unit) -> unit
    ElementProps: IHTMLProp list
    ElementStyleProps: CSSProp list
    /// button options (other than OnClick and Color)
    ButtonOptions: Button.Option list 
    }

module Constants =
    let gridBoxSeparation = "5px"

    let colArrayStyle = Style [
                BorderColor "white";
                BorderWidth "10px";
                BorderStyle "solid";
                Padding "50px"]

    let defaultConfig = {
        BreadcrumbIdPrefix = "BreadcrumbDefault"
        ColorFun = fun _ -> IColor.IsGreyDark
        ClickAction = fun _ _ -> ()
        ElementProps = [ ]
        ElementStyleProps = [           
            Border "2px"            
            BorderColor "LightGrey"
            BorderRightColor "DarkGrey"
            BorderStyle "Solid"
            Background "LightGrey"
            Padding "5px"]
        ButtonOptions = [
                Button.Size IsSmall
                Button.IsOutlined
                Button.IsExpanded
                Button.IsFocused true
                Button.Disabled false
                ]
    }
    

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
let gridElement reactElementId props styleProps (pos: CSSGridPos) (x: ReactElement) =
    div ( props @ [
        Id reactElementId
        Style (styleProps @ [
            Display DisplayOptions.Flex
            FlexDirection "column"
            TextAlign TextAlignOptions.Center
            JustifySelf JustifySelfOptions.Center;
            //AlignSelf AlignSelfOptions.Center;
            JustifyContent "center"
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
        (cfg: BreadcrumbConfig)
        (dispatch: Msg -> unit)
        (posL: (CSSGridPos*SheetTree) list)
            : ReactElement =
    posL
    |> List.map (fun (pos, sheet) ->
            let crumbId = cfg.BreadcrumbIdPrefix + ":" + sheet.SheetName + ":" + String.concat ":" sheet.LabelPath
            let extraStyle = match sheet.SubSheets with | [] -> [BackgroundColor "white"; BorderWidth "0px"] | _ -> cfg.ElementStyleProps
            gridElement
                crumbId
                cfg.ElementProps
                (extraStyle)
                pos
                (Button.button [
                    Button.Props [Id crumbId]
                    Button.Color (cfg.ColorFun sheet)
                    Button.Modifiers [Modifier.TextColor IColor.IsLight]
                    Button.OnClick(fun ev -> cfg.ClickAction sheet dispatch)
                    ] [str $"{sheet.SheetName}" ]))             

    |> gridBox Constants.gridBoxSeparation 
    

/// display as a ReactElement the breadcrumbs.
/// clickAction - what happens when a given breadcrumb is clicked.
/// project - the model project.
let makeBreadcrumbsFromPositions
        (sheetTreeMap: Map<string,SheetTree>)
        (cfg: BreadcrumbConfig)
        (positionSheetsInGrid: Map<string,SheetTree> -> (CSSGridPos*SheetTree) list)
        (dispatch: Msg -> unit)
             : ReactElement =
        sheetTreeMap
        |> positionSheetsInGrid
        |> makeGridFromSheetsWithPositions cfg dispatch

/// Breadcrumbs of entire simulated design hierarchy.
/// Display as a ReactElement the breadcrumbs.
/// ClickAction - what happens when a given breadcrumb (labelled by its path to root) is clicked.
let hierarchyBreadcrumbs
        (cfg: BreadcrumbConfig)
        (dispatch: Msg -> unit)
        (model: Model) =
    mapOverProject (div [] []) model (fun p ->
        let root = Option.defaultValue p.OpenFileName model.WaveSimSheet
        let sheetTreeMap = getSheetTrees p
        makeBreadcrumbsFromPositions sheetTreeMap cfg (positionDesignHierarchyInGrid root) dispatch)



/// Breadcrumbs of entire design hierarchy from given sheet
/// Display as a ReactElement the breadcrumbs.
/// rootSheet - root of hierrarchy displayed
/// clickAction - what happens when a given breadcrumb (labelled by its path to root) is clicked.
let hierarchyFromSheetBreadcrumbs
        (rootSheet: string)
        (cfg: BreadcrumbConfig)
        (dispatch: Msg -> unit)
        (model: Model) =
    mapOverProject (div [] []) model (fun p ->
        let sheetTreeMap = getSheetTrees p
        makeBreadcrumbsFromPositions sheetTreeMap cfg (positionDesignHierarchyInGrid rootSheet) dispatch)

/// Breadcrumbs of entire design hierarchy of every root sheet in project
/// Display as a ReactElement the breadcrumbs.
/// rootSheet - root of hierrarchy displayed
/// clickAction - what happens when a given breadcrumb (labelled by its path to root) is clicked.
let allRootHierarchiesFromProjectBreadcrumbs
        (cfg: BreadcrumbConfig)
        (dispatch: Msg -> unit)
        (model: Model) =
    mapOverProject ([div [] []]) model (fun p ->
        let sheetTreeMap = getSheetTrees p
        allRootSheets sheetTreeMap
        |> Set.toList
        |> List.map (fun root ->
            makeBreadcrumbsFromPositions sheetTreeMap cfg (positionDesignHierarchyInGrid root) dispatch))
        |> List.mapi (fun i el ->
            tr [ Constants.colArrayStyle
                ] [ td [CellSpacing "50px"] [el]])
        |> fun rows -> table [] [tbody [] rows]


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
        (cfg: BreadcrumbConfig)
        (dispatch: Msg -> unit)
        (model: Model)
             : ReactElement =
    mapOverProject (div [] []) model (fun p ->       
        makeBreadcrumbsFromPositions (getSheetTrees p) cfg (positionRootAndFocusChildrenInGrid rootName pathToFocus) dispatch)


