(*
    Helpers.fs

    Some fsharp only (no JS) utility functions.
*)

module Helpers

let assertThat cond msg =
    if not cond
    then failwithf "what? assert failed: %s" msg
