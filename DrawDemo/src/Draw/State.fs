module Draw.State

open Elmish
open Types

let init () : Model * Cmd<Msg> =
  "", []

let update msg model : Model * Cmd<Msg> =
    match msg with
    | DrawAction str ->
        str, []
