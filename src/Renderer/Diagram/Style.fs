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
