module EditorStyle

open CommonStyle

open Fable.Helpers.React.Props

let editorHiddenStyle = Style [
    Height "0px"
    Width "0px"
]

let editorVisibleStyle = Style [
    Position "absolute"
    Overflow "hidden"
    Top headerHeight // Placed just under the header.
    Left "0px"
    Bottom bottomSectionHeight // Leave some space at the bottom.
    Right rightSectionWidth
    Border "2px solid gray"
]

