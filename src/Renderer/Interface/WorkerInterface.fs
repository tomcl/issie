module WorkerInterface


open Browser.Types
open Browser.Blob
open Browser.Dom
open Fable.Core
open Fable.Core.JsInterop

[<Global>]
let URL : obj = jsNative

[<Emit("self.postMessage($0)")>]
let postMessage a : unit = jsNative


[<Emit("onmessage = ($0)")>]
let defineWorkerOnMsg onMsgFn = jsNative


[<Emit("$1.postMessage($0)")>]
let sendWorkerMsg msg worker = jsNative

[<Emit("$0.terminate()")>]
let terminateWorker worker = jsNative

[<Emit("close()")>]
let closeWorker() = jsNative

[<Emit("new Worker(new URL($0, import.meta.url), {type: \"module\"})")>]
// [<Emit("new Worker($0)")>]
let inline newWorkerUrl url = jsNative

[<Emit("$1.onmessage = ($0)")>]
let inline setWorkerOnMsg onMsgFun worker = jsNative
