module CommonStyle

open Fable.Helpers.React.Props

let important s = s //+ " !important" 

let headerHeight = "50px"

let navbarStyle = Style [
    Width "100%"
    Height headerHeight
]

