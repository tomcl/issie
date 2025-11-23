Note that an buggy example implementation of web workers with JSON encoding of parameters is in the `dev-kc` branch of issie.

## Web workers setup

Put your worker in one module and define at least an `onmessage` function in it using this helper function (maybe put this in a `WorkerInterface.fs` file with common helpers for workers):

```fs
[<Emit("onmessage = ($0)")>]
let defineWorkerOnMsg onMsgFn = jsNative
```

(if you are not familiar with the `Emit` syntax look up how to interoperate with JavaScript from Fable F#)

### Worker.fs

Then a minimal `Worker.fs` might look like this:

```fs
module TestWorker

open WorkerInterface

let testFn (msg: {|data: string|}) =
    postMessage "Worker reply from worker with data: %s" msg.data
    
defineWorkerOnMsg testFn
```

using (from WorkerInterface.fs)

```fs
[<Emit("self.postMessage($0)")>]
let postMessage a : unit = jsNative


[<Emit("onmessage = ($0)")>]
let defineWorkerOnMsg onMsgFn = jsNative
```

### Starting the worker from the main thread

Using WorkerInterface.fs

```fs
[<Emit("$1.postMessage($0)")>]
let sendWorkerMsg msg worker = jsNative

[<Emit("new Worker(new URL($0, import.meta.url), {type: \"module\"})")>]
let inline newWorkerUrl url = jsNative

[<Emit("$1.onmessage = ($0)")>]
let inline setWorkerOnMsg onMsgFun worker = jsNative
```

(`{type: \"module\"}` is not that relevant)

we can start a new worker and deal with replies from it like this:

```fs
let worker = newWorkerUrl("./Worker.fs.js");
setWorkerOnMsg (fun (msg: {|data: string|}) -> printfn "reply from worker: %s" msg.data) worker
sendWorkerMsg "test message" worker
```

### Integrating it with the Elmish workflow

Make worker as part of model and do `newWorkerUrl` in Elmish `init` (in `Renderer.fs`)

Add a new `Msg` in `ModelType.fs` for starting a worker and in its match case in `Update.fs` to send a message to the worker.

Then get a reply for the worker using a subscription like this (in `Renderer.fs`) which you can use to trigger a Msg in Update ('HandleWorkerReply'):

```fs
let waveGenWorkerSub (model: Model) =
    let subWaveGenWorker dispatch =
        let onMsgFn (msg: {|data: string|}) =
            dispatch <| HandleWorkerReply msg.data
        setWorkerOnMsg onMsgFn model.worker // assuming worker in model
    Cmd.ofSub subWaveGenWorker

Program.mkProgram init update view'
|> Program.withReactBatched "app"
|> Program.withSubscription attachMenusAndKeyShortcuts
|> Program.withSubscription keyPressListener
|> Program.withSubscription waveGenWorkerSub // add subscription here
|> Program.run
```

### Encoding data

All data passed over postMessage is cloned using the "Structured cloning algorithm".
This means that for certain objects data will be lost when just using postMessage without any encoding.

The solution to this is to encode and decode it using [`Thoth.Json`](https://thoth-org.github.io/Thoth.Json/).

You will probably have to write your own encoders and decoders for certain objects like `ReactElement` or `IProps` (properties of react elements).

For examples see the `dev-kc` branch of issie.

## How to decide whether web workers are useful for a specific problem

This will require a lot of testing and depends a lot on how much input and output the worker will give.
The bigger the input and output which needs to be both en- and decoded the bigger the overhead which makes the worker less worth to use and may even make the performance worse.
Hence, you need to investigate the performance on a case by case basis.