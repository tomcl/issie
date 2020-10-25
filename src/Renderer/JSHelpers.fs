(*
    JSHelpers.fs

    Some JS related utility functions.
*)

module JSHelpers

open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open Electron
open Fable.React
open JSTypes

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
    Helpers.assertThat (not <| isNull obj) ("(assertNotNull) " + msg)
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
let getTextEventValue (event: Event) =
    getFailIfNull event.currentTarget ["value"] |> unbox<string>

/// Get the value for a change event in an input number box.
let getIntEventValue (event: Event) =
    getFailIfNull event.currentTarget ["value"] |> unbox<int>

/// Get the value for a blur event in an input textbox.
let getTextFocusEventValue (event: FocusEvent) =
    getFailIfNull event ["target";"value"] |> unbox<string>

#if DEBUG
let mutable debugLevel = 1
#else
let mutable debugLevel = 0
#endif

/// Hack to provide a constant global variable
/// set from command line arguments of main process.
/// 0 => production. 1 => dev. 2 => debug.
let setDebugLevel() =
    let argV =
        electron.remote.``process``.argv
        |> Seq.toList
        |> (function | [] -> [] | _ :: args' -> args')
        |> List.map (fun s -> s.ToLower())
    let isArg s = List.contains s argV

    if isArg "--debug" || isArg "-d" then
        debugLevel <- 2
    elif isArg "-w" then
        debugLevel <- 1

/// deliver string suitable for HTML color from a HighlightColor type value
let getColorString (col: CommonTypes.HighLightColor) =
    (sprintf "%A" col).ToLower()


/// Properties for react-tippy
type TooltipsOpts =
    | Content of string
    | Animation of string
    | Arrow of bool
    | Theme of string
    | Offset of int * int
    | HideOnClick of bool
    | Placement of string
    | Delay of int * int
    | ZIndex of int
    | Interactive of bool
    | Boundary of string
    | PopperOptions of obj
    | MaxWidth of int
    | AppendTo of HTMLElement


let tippyOptsWithContent p c =
    [
        Delay (700,100)
        Placement p
        Animation "fade"
        MaxWidth 250
        Arrow true
        Theme "material"
        Interactive true
        AppendTo Browser.Dom.document.body
        HideOnClick true
        Content c
    ] 
    |> keyValueList CaseRules.LowerFirst


let tippyOpts p =
    [
        Delay (700,100)
        Animation "fade"
        Placement p
        MaxWidth 250
        Arrow true
        Theme "material"
        Interactive true
        AppendTo Browser.Dom.document.body
        HideOnClick true
    ] 
    |> keyValueList CaseRules.LowerFirst

type TippyInstance =
    abstract setProps: obj -> unit
    abstract destroy: unit -> unit

type TippySingleton =
    abstract setProps: obj -> unit
    abstract destroy: unit -> unit
    
/// top-level function from tippy.js to make tooltips
/// #id will make tooltip on element id
///
let tippy' (rClass : string, tippyOpts : obj): TippyInstance array  = importDefault<TippyInstance array> "tippy.js"

let tippyDom (els : Browser.Types.Element array, tippyOpts : obj): TippyInstance array  = importDefault<TippyInstance array> "tippy.js"


let createSingleton (tippys: TippyInstance array, props: obj): TippySingleton = import "createSingleton" "tippy.js"

import "*"  "tippy.js/themes/material.css"
let tippy (tippyOpts: obj) (rClass:string) = tippy'(rClass, tippyOpts)
let tippy1 rId pos mess = tippy (tippyOptsWithContent pos mess) ("#"+rId) 

let mutable tippyRecord: Map<string,TippyInstance list> = Map.ofList []

let recordTippyInstance (prefix: string) (tip: TippyInstance array) =
    () (*
    printfn "Recording new instance of Tippy: %s" prefix
    Map.tryFind prefix tippyRecord
    |> Option.defaultValue []
    |> (fun lst -> tippyRecord <- Map.add prefix (tip :: lst) tippyRecord) *)
    

let tipRef (prefix:string) (pos:string) (text:string) (element: ReactElement) (tip: string) =
    let ids = prefix + (
                text 
                |> Seq.toArray 
                |> Array.filter System.Char.IsLetterOrDigit
                |> Array.map string
                |> String.concat "")               
    div [Props.Id ids; Props.Ref (fun element -> 
        // Ref is trigger with null once for stateless element so we need to wait for the second trigger
        if not (isNull element) && not (element.hasAttribute "data-tippy-content") then 
            let tippyInst = tippy1 ids pos tip
            recordTippyInstance prefix tippyInst)           
        ] [element]

let tipStr (pos:string) (text:string) (tip: string) = tipRef "Str_" pos text (str text) tip



let testCanvas = Browser.Dom.document.createElement("canvas") :?> HTMLCanvasElement
let canvasWidthContext = testCanvas.getContext_2d()

let getTextWidthInPixels(txt:string, font:string) =
   canvasWidthContext.font <- font; // e.g. "16px times new roman";
   canvasWidthContext.measureText(txt).width;

