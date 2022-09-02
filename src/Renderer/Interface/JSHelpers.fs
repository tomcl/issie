(*
    JSHelpers.fs
    Some utility functions that rely on Node/JS functionality
*)

module JSHelpers

open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open ElectronAPI
open Fable.React

/// Fix to access the deprecated @electron.remote module.
/// This must be enabled from main.fs
/// NB the interface used here is not precisely correct, because it
/// exposes the original electron-remote API. The @electron.remote API is
/// a bit reduced, but with some extra code to control access.
/// electronRemote replaces electron.remote and renderer.remote in old interface
[<ImportAll("@electron/remote")>]
let electronRemote : Electron.Remote = jsNative


[<Emit("typeof $0")>]
let jsType (var: obj) : unit = jsNative

[<Emit("console.log($0)")>]
let log msg : unit = jsNative

let logString msg : unit =
    log <| sprintf "%A" msg

let logChain msg =
    logString msg
    msg

[<Emit("alert($0)")>]
let alert msg : unit = jsNative

[<Emit("($0 == null || $0 === 'undefined')")>]
let isNull (obj : obj) : bool = jsNative

[<Emit("console.time($0)")>]
let startTimer (label : string) : unit = jsNative

[<Emit("console.timeEnd($0)")>]
let stopAndLogTimer (label : string) : unit = jsNative

/// Assert js object is not null, and return it.
let assertNotNull obj msg =
#if ASSERTS
    Helpers.assertThat (not <| isNull obj) ("(assertNotNull) " + msg)
#endif
    obj

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

[<Emit("[]")>]
let emptyJsList () = jsNative

let fshaprListToJsList (list : 'a list) =
    let jsList = emptyJsList ()
    list |> List.map (fun el -> jsList?push(el)) |> ignore
    jsList
    
/// Get the value for a change event in an input textbox.
let getTextEventValue (event: Browser.Types.Event) =
    getFailIfNull event.currentTarget ["value"] |> unbox<string>

// Due to the way FABLE embeds integers in floats, with type erasure at runtime,
// values that need to be int in F# code must be explicitly converted
// to int as here. Otherwise obscure bugs can happen where a JS apparent integer
// turns into an F# integer value that is not precisely equal to
// the real F# integer.

/// Get the value for a change event in an input number box, 
/// making sure it is an F# integer (JS integer values may not be precise)
let getIntEventValue (event: Browser.Types.Event) =
    getFailIfNull event.currentTarget ["value"] |> unbox<float> |> int

let getFloatEventValue (event: Browser.Types.Event) =
    getFailIfNull event.currentTarget ["value"] |> unbox<float>

let getInt64EventValue( event: Browser.Types.Event) =
    let boxText = getFailIfNull event ["target";"value"] |> unbox<string>
    let (ok,n) = System.Int64.TryParse boxText
    if not ok then 0L else n


/// Get the value for a blur event in an input textbox.
let getTextFocusEventValue (event: FocusEvent) =
    getFailIfNull event ["target";"value"] |> unbox<string>

#if (ASSERTS || DEBUG)
let mutable debugLevel = 1
#else
let mutable debugLevel = 0
#endif

/// trace UI execution: "view" - mark view function. "update" print update messages.
let mutable debugTraceUI: string Set = Set []

/// Call debugAction() and print its result if debugTraceUI mutable contains string traceCode
let traceIf traceCode debugAction =
    if Set.contains traceCode debugTraceUI then printfn <| debugAction()

/// Hack to provide a constant global variable
/// set from command line arguments of main process.
/// 0 => production. 1 => dev. 2 => debug.
let setDebugLevel() =
    let hasSwitch swName = electronRemote.app.commandLine.hasSwitch swName
    if hasSwitch "debug" || hasSwitch "-d" then
        debugLevel <- 2
    elif hasSwitch "w" then
        debugLevel <- 1

/// return a v4 (random) universally unique identifier (UUID)
let uuid():string = import "v4" "uuid"

