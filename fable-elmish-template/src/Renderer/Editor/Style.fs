module EditorStyle

open CommonStyle

open Fable.Helpers.React.Props

let editorHiddenStyle = Style [
    Height "0px"
    Width "0px"
]

let editorVisibleStyle = Style [
    Height "auto"
    Width "auto"
    Border "2px solid gray"
]

