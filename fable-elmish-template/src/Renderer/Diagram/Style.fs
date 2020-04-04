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
    // Important is necessary to 
    Top (important headerHeight) // Placed just under the header.
    Left (important "0px")
    Bottom (important "300px") // Leave some space at the bottom. TODO: make this relative to window size?
    Right (important "400px")
    Border "2px solid gray"
]

