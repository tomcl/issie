module WorkerInterface


open Browser.Types
open Browser.Blob
open Browser.Dom
open Fable.Core
open Fable.Core.JsInterop

[<Global>]
let URL : obj = jsNative

[<Emit("$0.toString()")>]
let funToString f : string = jsNative

[<Emit("self.postMessage($0)")>]
let postMessage a : unit = jsNative

[<Emit("importScripts($0)")>]
let importScripts s = jsNative

[<Emit("onmessage = ($0)")>]
let defineWorkerOnMsg onMsgFn = jsNative


let inline depsParser (deps: string []) =
    match deps with
    | [||] -> None
    | _ ->
        deps
        |> String.concat ", "
        |> sprintf "importScripts('%s'); "
        |> Some

/// Creates the worker blob url via stringifying the parameters.
let inline createWorkerBlobUrl depArr onMsgFun =
    printfn "origin: %A" document.location.origin
    let onMessage = 
        sprintf "onmessage=(%s)" 
            (funToString onMsgFun)

    let blobOptions =
        jsOptions<BlobPropertyBag>(fun o ->
            o.``type`` <- "text/javascript")
    match depsParser depArr with
    | Some deps ->
        Blob.Create([| deps :> obj; onMessage :> obj |], blobOptions)
    | None ->
        Blob.Create([| onMessage :> obj |], blobOptions)
    |> URL?createObjectURL

[<Emit("$1.postMessage($0)")>]
let sendWorkerMsg msg worker = jsNative

[<Emit("$0.terminate()")>]
let terminateWorker worker = jsNative

[<Emit("close()")>]
let closeWorker() = jsNative

[<Emit("new Worker($0)")>]
let inline newWorker blobUrl = jsNative

[<Emit("new Worker(new URL($0, import.meta.url), {type: \"module\"})")>]
// [<Emit("new Worker($0)")>]
let inline newWorkerUrl url = jsNative

[<Emit("$1.onmessage = ($0)")>]
let inline setWorkerOnMsg onMsgFun worker = jsNative


let createWorker depArr onMsgFun =
    newWorker (createWorkerBlobUrl depArr onMsgFun)