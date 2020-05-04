module DiagramStyle

open CommonStyle

open Fable.Helpers.React.Props

let canvasHiddenStyle = Style [
    Display "none"
]

let canvasVisibleStyle = Style [
    Display "block"
    Position "absolute" // Required to work.
    Overflow "scroll"
    Top headerHeight // Placed just under the header.
    Left "0px"
    Bottom bottomSectionHeight // Leave some space at the bottom.
    Right rightSectionWidth
    Border "2px solid gray"
]

let canvasSmallMenuStyle = Style [
    Display "block"
    Position "absolute" // Required to work.
    Overflow "hidden"
    Left "10px"
    Bottom (sprintf "calc(%s + 25px)" bottomSectionHeight)
    Right (sprintf "calc(100%s - 300px)" "%")
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
