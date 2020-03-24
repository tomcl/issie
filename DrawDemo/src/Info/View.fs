module Info.View

open Fable.React
open Fable.React.Props

let root =
  div
    [ ClassName "content" ]
    [ h1
        [ ]
        [ str "About page" ]
      p
        [ ]
        [ str "This template is a simple application build with Fable + Elmish + React." ] ]
