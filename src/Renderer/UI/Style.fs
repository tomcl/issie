module DiagramStyle

open ModelType
open Fable.React
open Fable.React.Props
open Browser.Dom

module Constants =
    let dividerBarWidth = 10

/// Where the canvas starts, which must equal where the top menu bar ends.
///
/// The bar's height is driven by its content: a 40px button inside a navbar-item with 8px of
/// padding above and below is 56px, and the bar starts 2px down from the window's top border.
/// Keep this in step with that. It was 72px from when each button sat inside two nested
/// navbar-items rather than one, and the 14px left over showed as an empty white strip between
/// the line under the menu bar and the line above the canvas.
let headerHeight = "58px"
/// Small right section.
let private rightSectionWidthS = "400px"
/// Large right section.
let private rightSectionWidthL = "650px"
let minViewerWidth = 400
let minEditorWidth() = int ((document.getElementById "WholeApp").offsetWidth * 0.25)

let rightSectionWidthViewerDefault = 650

let getHeaderHeight =
    headerHeight
    |> String.filter (fun c -> (int(c) <= 57 && int(c) >= 48))
    |> float
    
let rightSectionWidth (model:Model) =
    match model.RightPaneTabVisible with
    | RightTab.Properties | RightTab.Catalogue | RightTab.Transition -> rightSectionWidthS
    | RightTab.Build -> rightSectionWidthL
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
        OverflowX OverflowOptions.Visible
        OverflowY OverflowOptions.Visible
        //OverflowY OverflowOptions.Auto
        BorderTop "2px solid lightgray"
        UserSelect UserSelectOptions.None
        ZIndex 31
        BackgroundColor "white"
        //UserSelect UserSelectOptions.None
]

/// Fill what is left of the height below a header of the given size, scrolling if the content does
/// not fit. The overflow is a parameter rather than always Auto because the caller needs to switch
/// it off for a moment, and saying so with a second Style prop does not work: React keeps the last
/// style it is given and drops the rest, so the height quietly went with it - which left the right
/// tab body with no height at all, and nothing in it able to scroll.
let belowHeaderStyle headerSize overflow =
    Style [
        OverflowY overflow
        Height $"calc(100%% - {headerSize})"
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
    let background =
        match model.Sheet.Wire.Symbol.Theme with
        |DrawModelType.SymbolT.ThemeType.White -> BackgroundColor "white"
        |DrawModelType.SymbolT.ThemeType.Light -> BackgroundColor "rgba(255,255,0,0.1)"  //light yellow
        |DrawModelType.SymbolT.ThemeType.Colourful -> BackgroundColor "rgba(0,0,0,0.05)" //light gray
    
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
        background
    ]

/// The strip across the top of the canvas saying that the sheet below is a library component being
/// looked at and cannot be changed.
///
/// Placed over the canvas rather than above it so that nothing moves when it appears, and it takes
/// no pointer events: it sits on top of the schematic, and a band the mouse could not draw through
/// would be worse than no banner.
let canvasReadOnlyBannerStyle model = Style [
    Position PositionOptions.Absolute
    Top headerHeight
    Left "0px"
    Right (rightSectionWidth model)
    CSSProp.PointerEvents "none"
    ZIndex 10
    Padding "4px 10px"
    FontSize "12px"
    TextAlign TextAlignOptions.Center
    Color "#4b2e83"
    BackgroundColor "rgba(147,112,219,0.16)"
    BorderBottom "1px solid rgba(102,51,153,0.45)"
    WhiteSpace WhiteSpaceOptions.Nowrap
    OverflowX OverflowOptions.Hidden
    TextOverflow "ellipsis"
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

let constraintNumberStyle = Style [
    Width "200px"
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

let sortArrowStyle = Style [
    Margin "0"
    Display DisplayOptions.Block
    Width "100%"
    Height "50%"
    Padding "0 0 0 0"
    Top "0"
    FontSize "50%"
    Position PositionOptions.Relative
    BorderColor "white"
]

let colMoveArrowStyle = Style [
    Margin "0"
    Display DisplayOptions.Block
    Width "100%"
    Height "50%"
    Padding "0 0 0 0"
    Top "0"
    FontSize "80%"
    Position PositionOptions.Relative
    BorderColor "white"
]

let ttGridColumnProps index = [
    Border "1px solid gray"
    Padding "7px"
    FontSize "18px"
    TextAlign TextAlignOptions.Left
    GridColumnStart <| string (index+1)
    GridColumnEnd <| string (index+2)
    Transition "all 0.2s ease-in-out"
    //OverflowX OverflowOptions.Auto
    //OverflowWrap "break-word"
]

let ttGridHiddenColumnProps gridWidth= [
    GridColumnStart (string <| gridWidth + 1)
    GridColumnEnd (string <| gridWidth + 2)
    Width 0
    OverflowX OverflowOptions.Hidden
    Visibility "hidden"
]

let ttGridContainerStyle model = 
    // Compute number of visible columns (total IOs minus hidden columns)
    let totalCols = model.TTConfig.IOOrder.Length
    let hiddenCols = model.TTConfig.HiddenColumns.Length
    // Ensure at least 1 to avoid invalid CSS when table is empty
    let visibleCols = max 1 (totalCols - hiddenCols)
    Style [
        Display DisplayOptions.Grid
        GridAutoFlow "column"
        // Make grid span the container width and distribute columns evenly
        Width "100%"
        GridTemplateColumns (sprintf "repeat(%d, minmax(0, 1fr))" visibleCols)
    ]


/// display react of refresh button with color (e.g. white) at given height (e.g. 10px)
let refreshSvg (color:string) (height:string)=
    svg [
            ViewBox "0 0 512 512"
            SVGAttr.Height height
        ] [
            path [
                D "M496 48V192c0 17.69-14.31 32-32 32H320c-17.69 0-32-14.31-32-32s14.31-32
                32-32h63.39c-29.97-39.7-77.25-63.78-127.6-63.78C167.7 96.22 96 167.9 96 256s71.69
                159.8 159.8 159.8c34.88 0 68.03-11.03 95.88-31.94c14.22-10.53 34.22-7.75 44.81
                6.375c10.59 14.16 7.75 34.22-6.375 44.81c-39.03 29.28-85.36 44.86-134.2 44.86C132.5
                479.9 32 379.4 32 256s100.5-223.9 223.9-223.9c69.15 0 134 32.47 176.1 86.12V48c0-17.69
                14.31-32 32-32S496 30.31 496 48z"
                Style [
                    Fill color
                    Stroke color
                    StrokeWidth "5px"
                ]
            ] []
        ]

/// A pushpin, upright when it is holding something and tilted when it is not - the same shape in
/// both states, so the icon is recognised once and then read by its angle rather than by comparing
/// two drawings. Colour reinforces it: grey while the menu is free to close, dark once pinned.
let pinSvg (pinned: bool) =
    svg [
            ViewBox "0 0 24 24"
            SVGAttr.Height "16px"
            SVGAttr.Width "16px"
            Style [
                Display DisplayOptions.Block
                Transform (if pinned then "none" else "rotate(-45deg)")
            ]
        ] [
            path [
                D "M16 9V4h1c.55 0 1-.45 1-1s-.45-1-1-1H7c-.55 0-1 .45-1 1s.45 1 1 1h1v5c0
                   1.66-1.34 3-3 3v2h5.97v7l1 1 1-1v-7H19v-2c-1.66 0-3-1.34-3-3z"
                Style [ Fill (if pinned then "#363636" else "#8a8a8a") ]
            ] []
        ]

let emptyRefreshSVG =
    svg [
        SVGAttr.Height "20"
        SVGAttr.Width "20"
    ] []
let zoomOutSVG =
    svg [
            ViewBox "0 0 192.904 192.904"
            SVGAttr.Height "20px"
        ] [ path [
                D "M190.707,180.101l-47.079-47.077c11.702-14.072,18.752-32.142,18.752-51.831C162.381,36.423,125.959,0,81.191,0
                C36.422,0,0,36.423,0,81.193c0,44.767,36.422,81.187,81.191,81.187c19.689,0,37.759-7.049,51.831-18.75l47.079,47.077
                c1.464,1.465,3.384,2.197,5.303,2.197c1.919,0,3.839-0.732,5.303-2.197C193.637,187.778,193.637,183.03,190.707,180.101z
                M15,81.193C15,44.694,44.693,15,81.191,15c36.497,0,66.189,29.694,66.189,66.193c0,36.496-29.692,66.187-66.189,66.187
                C44.693,147.38,15,117.689,15,81.193z"
            ] []
            path [
                D "M118.035,73.689H44.346c-4.142,0-7.5,3.358-7.5,7.5c0,4.142,3.358,7.5,7.5,7.5h73.689c4.142,0,7.5-3.358,7.5-7.5
                    C125.535,77.047,122.177,73.689,118.035,73.689z"
            ] []
        ]

let zoomInSVG =
    svg [
            ViewBox "0 0 192.904 192.904"
            SVGAttr.Height "20px"
        ] [ path [
                D "M190.707,180.101l-47.079-47.077c11.702-14.072,18.752-32.142,18.752-51.831C162.381,36.423,125.959,0,81.191,0
                C36.422,0,0,36.423,0,81.193c0,44.767,36.422,81.187,81.191,81.187c19.689,0,37.759-7.049,51.831-18.75l47.079,47.077
                c1.464,1.465,3.384,2.197,5.303,2.197c1.919,0,3.839-0.732,5.303-2.197C193.637,187.778,193.637,183.03,190.707,180.101z
                M15,81.193C15,44.694,44.693,15,81.191,15c36.497,0,66.189,29.694,66.189,66.193c0,36.496-29.692,66.187-66.189,66.187
                C44.693,147.38,15,117.689,15,81.193z"
            ] []
            path [
                D "M118.035,73.689H88.69V44.345c0-4.142-3.357-7.5-7.5-7.5s-7.5,3.358-7.5,7.5v29.345H44.346c-4.143,0-7.5,3.358-7.5,7.5
                c0,4.142,3.357,7.5,7.5,7.5H73.69v29.346c0,4.142,3.357,7.5,7.5,7.5s7.5-3.358,7.5-7.5V88.689h29.345c4.143,0,7.5-3.358,7.5-7.5
                C125.535,77.047,122.178,73.689,118.035,73.689z"
            ] []
        ]

let colorSpan color text = span [Style [Color color]] [str text]
