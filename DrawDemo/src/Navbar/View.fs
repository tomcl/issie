module Navbar.View

open Fable.React
open Fable.React.Props

let navButton classy href faClass txt =
    p
        [ ClassName "control" ]
        [ a
            [ ClassName (sprintf "button %s" classy)
              Href href ]
            [ span
                [ ClassName "icon" ]
                [ i
                    [ ClassName (sprintf "fab %s" faClass) ]
                    [ ] ]
              span
                [ ]
                [ str txt ] ] ]

let navButtons =
    span
        [ ClassName "navbar-item" ]
        [ div
            [ ClassName "field is-grouped" ]
            [ navButton "twitter" "https://twitter.com/FableCompiler" "fa-twitter" "Twitter"
              navButton "github" "https://github.com/elmish/elmish" "fa-github" "Fork me"
              navButton "github" "https://gitter.im/fable-compiler/Fable" "fa-gitter" "Gitter" ] ]

let root =
    nav
        [ ClassName "navbar is-dark" ]
        [ div
            [ ClassName "container" ]
            [ div
                [ ClassName "navbar-brand" ]
                [ h1
                    [ ClassName "navbar-item title is-4" ]
                    [ str "Elmish" ] ]
              div
                [ ClassName "navbar-end" ]
                [ navButtons ] ] ]
