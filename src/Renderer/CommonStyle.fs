module CommonStyle

open Fable.Helpers.React.Props

let headerHeight = "50px"
let bottomSectionHeight = "300px"
let rightSectionWidth = "400px"

let navbarStyle = Style [
    Width "100%"
    Height headerHeight
]

let rightSectionStyle = Style [
    Position "fixed"
    Right "0px"
    Height (sprintf "calc(100%s - %s)" "%" headerHeight) // WindowSize - headerHeight
    Width rightSectionWidth
    OverflowX "hidden"
    OverflowY "scroll"
    Border "2px solid gray"
]

let bottomSectionStyle =  Style [
    Position "fixed"
    Bottom "0px"
    Height bottomSectionHeight
    Width (sprintf "calc(100%s - %s)" "%" rightSectionWidth) // WindowSize - rightSectionWidth
    OverflowX "hidden"
    OverflowY "scroll"
    Border "2px solid gray"
]
