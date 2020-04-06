module DiagramStyle

open CommonStyle

open Fable.Helpers.React.Props

let rightSectionWidth = "400px"
let bottomSectionHeight = "300px"

let rightSectionStyle = Style [
    Position "fixed"
    Right "0px"
    Height (sprintf "calc(100%s - %s)" "%" headerHeight) // WindowSize - headerHeight
    Width rightSectionWidth
    Overflow "scroll"
    Border "2px solid gray"
    //BackgroundColor "red"
]

let bottomSectionStyle =  Style [
    Position "fixed"
    Bottom "0px"
    Height bottomSectionHeight
    Width (sprintf "calc(100%s - %s)" "%" rightSectionWidth) // WindowSize - rightSectionWidth
    Overflow "scroll"
    Border "2px solid gray"
    //BackgroundColor "blue"
]

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
