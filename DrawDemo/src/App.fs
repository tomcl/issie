module App.View

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Types
open App.State
open Global

importAll "../sass/main.sass"

open Fable.React
open Fable.React.Props

let menuItem label page currentPage =
    li
      [ ]
      [ a
          [ classList [ "is-active", page = currentPage ]
            Href (toHash page) ]
          [ str label ] ]

let menu currentPage =
  aside
    [ ClassName "menu" ]
    [ p
        [ ClassName "menu-label" ]
        [ str "General" ]
      ul
        [ ClassName "menu-list" ]
        [ menuItem "Home" Home currentPage
          menuItem "Counter sample" Counter currentPage
          menuItem "About" Page.About currentPage
          menuItem "Draw" Page.Draw currentPage ] ]

let root model dispatch =

  let pageHtml page =
    match page with
    | Page.About -> Info.View.root
    | Counter -> Counter.View.root model.Counter (CounterMsg >> dispatch)
    | Home -> Home.View.root model.Home (HomeMsg >> dispatch)
    | Draw -> Draw.View.root model.Draw (DrawMsg >> dispatch)

  div
    []
    [ Navbar.View.root
      div
        [ ClassName "section" ]
        [ div
            [ ClassName "container" ]
            [ div
                [ ClassName "columns" ]
                [ div
                    [ ClassName "column is-3" ]
                    [ menu model.CurrentPage ]
                  div
                    [ ClassName "column" ]
                    [ pageHtml model.CurrentPage ] ] ] ] ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update root
|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactBatched "elmish-app"
|> Program.run
