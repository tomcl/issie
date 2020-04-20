module JSHelpers

open Fable.Core
open Fable.Core.JsInterop

[<Emit("typeof $0")>]
let jsType (var: obj) : unit = jsNative

[<Emit("console.log($0)")>]
let log msg : unit = jsNative

[<Emit("alert($0)")>]
let alert msg : unit = jsNative

[<Emit("($0 == null || $0 === 'undefined')")>]
let isNull (obj : obj) : bool = jsNative

/// Assert js object is not null, and return it.
let assertNotNull obj msg =
    if isNull obj
    then failwithf "what? assertNotNull failed: %s" msg
    else obj

/// Access nested fields of a js object, failing if at any point of the chain
/// the requested field is null.
/// Should be used when the fields are guaranteed to exist.
/// For example ["a"; "b"; "c"] is equivalent to the jsCode `obj.a.b.c`, but
/// with checks against null at every layer.
let rec getFailIfNull jsObj (fields : string list) =
    assertNotNull jsObj "jsObj is null in getFailIfNull" |> ignore
    match fields with
    | [lastField] ->
        assertNotNull jsObj?(lastField) <| sprintf "jsObj.%s is null in getFailIfNull" lastField
    | nextField :: fields' ->
        let jsObj' = assertNotNull jsObj?(nextField) <| sprintf "jsObj.%s is null in getFailIfNull" nextField
        getFailIfNull jsObj' fields'
    | [] -> failwithf "what? getFailIfNull called with no fields to get"

/// Transforms a js list of jsType into an f# list of jsType.
/// If jsList is not a js list, fail.
let jsListToFSharpList jsList =
    let len = getFailIfNull jsList ["length"]
    [0..len - 1] |> List.map (fun i -> jsList?(i))
