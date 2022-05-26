module DiagramStyle

open CommonTypes
open ModelType
open Fulma
open Fable.React
open Fable.React.Props

let headerHeight = "72px"
let private headerHeightWithBorderOffset = "74px"
let private rightSectionWidthS = "400px" // Small right section.
let private rightSectionWidthL = "650px" // Large right section.
let minViewerWidth = 400
let minEditorWidth = 400

let rightSectionWidthViewerDefault = 650

// Get 
let getHeaderHeight =
    headerHeight
    |> String.filter (fun c -> (int(c) <= 57 && int(c) >= 48))
    |> float
    
let rightSectionWidth (model:Model) =
    match model.RightPaneTabVisible with
    | RightTab.Properties | RightTab.Catalogue -> rightSectionWidthS
    | RightTab.Simulation -> 
        match model.SimSubTabVisible with
        | SimSubTab.StepSim -> rightSectionWidthL
        | SimSubTab.WaveSim | SimSubTab.TruthTable -> sprintf "%dpx" model.WaveSimViewerWidth

let leftSectionWidth model = Style [
    Width (sprintf "calc(100%s - %s - 10px)" "%" (rightSectionWidth model))
]

let navbarStyle model = Style [
    Width "100%"
    Height headerHeight
]

/// For making Sheet contained inside left section (don't want sheet behind right section tabs) NOT USED
let leftSectionStyle model =
    let leftSectionWidth = leftSectionWidth model
    Style [
        Position PositionOptions.Fixed
        Left "0px"
        Top "0px"
        Height  "100%" //(sprintf "calc(100%s - %s)" "%" headerHeight) // WindowSize - headerHeight
        Width leftSectionWidth
        OverflowX OverflowOptions.Hidden
        OverflowY OverflowOptions.Hidden
        BorderTop "2px solid lightgray"
        UserSelect UserSelectOptions.None
        ZIndex 31
        BackgroundColor "white"
        //UserSelect UserSelectOptions.None
]

let rightSectionStyle model = 
    let widthRightSec = rightSectionWidth model
    Style [
        Position PositionOptions.Fixed
        Right "0px"
        Top "0px"
        Height  "100%" //(sprintf "calc(100%s - %s)" "%" headerHeight) // WindowSize - headerHeight
        Width widthRightSec
        OverflowX OverflowOptions.Hidden
        OverflowY OverflowOptions.Scroll
        BorderTop "2px solid lightgray"
        UserSelect UserSelectOptions.None
        ZIndex 31
        BackgroundColor "white"
        //UserSelect UserSelectOptions.None
]

let canvasVisibleStyle model = 
    let widthRightSec = rightSectionWidth model
    Style [
        Display DisplayOptions.Block
        Position PositionOptions.Absolute // Required to work.
        OverflowX OverflowOptions.Scroll
        OverflowY OverflowOptions.Scroll
        Top headerHeight // Placed just under the header.
        Left "0px"
        Bottom "0px"
        Right widthRightSec
        BorderTop "2px solid lightgray"
    ]
    
// Used by Sheet
let canvasVisibleStyleList model = 
    let widthRightSec = rightSectionWidth model
    [
        Display DisplayOptions.Block
        Position PositionOptions.Absolute // Required to work.
        OverflowX OverflowOptions.Scroll
        OverflowY OverflowOptions.Scroll
        Top headerHeight // Placed under header with offset for the border. // headerHeight // Placed just under the header.
        Left "0px"
        Bottom "0px"
        Right widthRightSec
        BorderTop "2px solid lightgray"
    ]

let canvasSmallMenuStyle = Style [
    Display DisplayOptions.Block
    Position PositionOptions.Absolute // Required to work.
    OverflowX OverflowOptions.Hidden
    OverflowY OverflowOptions.Hidden
    Left "10px"
    Bottom "25px"
    Right (sprintf "calc(100%s - 300px)" "%")
    WhiteSpace WhiteSpaceOptions.Nowrap
]

let canvasSmallButtonStyle = Style [
    MarginRight "5px"
    BackgroundColor "white"
    BorderRadius "4px"
    BorderStyle "solid"
    Outline "none"
    Padding "4px"
    Opacity 0.7
]

let notificationStyle = Style [
    ZIndex 100 // In front of everything.
    Position PositionOptions.Absolute
    UserSelect UserSelectOptions.None
    Right "20px"
    Bottom "20px"
]

let simulationNumberStyle = Style [
    Width "320px"
    Height "30px"
]

let simulationBitStyle = Style [
    Width "100px"
    Height "30px"
    PaddingTop "3px"
]

let menuLabelStyle = Style [
    Outline "none"
    MarginTop "10px"
    MarginBottom "10px"
    Color "#7a7a7a"
    FontSize "0.80em"
    LetterSpacing "0.1em"
    TextTransform "uppercase"
]
