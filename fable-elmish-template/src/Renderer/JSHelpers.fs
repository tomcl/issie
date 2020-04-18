module JSHelpers

open Fable.Core
open Fable.Core.JsInterop

// TODO interface those functions properly.

[<Emit("typeof $0")>]
let jsType (var: obj) : unit = jsNative

[<Emit("console.log($0)")>]
let log msg : unit = jsNative

[<Emit("alert($0)")>]
let alert msg : unit = jsNative

[<Emit("($0 == null || $0 === 'undefined')")>]
let isNull (var : obj) : bool = jsNative

/// Access nested fields of a js object, failing if at any point of the chain
/// the requested field is null.
/// Should be used when the fields are guaranteed to exist.
/// For example ["a"; "b"; "c"] is equivalent to the jsCode `obj.a.b.c`, but
/// with checks against null at every layer.
let rec getFailIfNull jsObj (fields : string list) =
    match fields with
    | [lastField] ->
        if isNull jsObj?(lastField)
        then failwithf "what? %s is null or undefined" lastField
        else jsObj?(lastField)
    | nextField :: fields' ->
        if isNull jsObj?(nextField)
        then failwithf "what? %s is null or undefined" nextField
        else getFailIfNull jsObj?(nextField) fields'
    | [] -> failwithf "what? getFailIfNull called with no fields to get"
