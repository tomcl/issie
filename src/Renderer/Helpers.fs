(*
    Helpers.fs

    Some fsharp only (no JS) utility functions.
*)

module Helpers

let assertThat cond msg =
    if not cond
    then failwithf "what? assert failed: %s" msg

/// Return the first error found in a list of results, or the list of Oks if
/// there are none.
let tryFindError (lst : Result<'a,'b> list) : Result<'a list, 'b> =
    let isError el = match el with | Error _ -> true | Ok _ -> false
    let extractOk el = match el with | Ok ok -> ok | Error _ -> failwith "what? Impossible case in tryFindError"
    match List.tryFind isError lst with
    | Some (Error err) -> Error err
    | None -> List.map extractOk lst |> Ok
    | _ -> failwith "what? Impossible case in tryFindError"

/// Return 2^exponent.
let pow2 (exponent : int) : int =
    int (2. ** float(exponent)) // TODO use bit-shift.

/// Return 2^exponent, packed into an int64.
let pow2int64 (exponent : int) : int64 =
    int64 (2. ** float(exponent))

/// Try to convert a string to an int, or return an error message if that was
/// not possible.
let strToInt (str : string) : Result<int64, string> =
    try
        Ok <| int64 str
    with
        | e -> Error <| e.ToString ()
