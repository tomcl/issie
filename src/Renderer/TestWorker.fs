module TestWorker

open WorkerInterface

let testFn (msg: {|data: string|}) =
    let start = TimeHelpers.getTimeMs()
    if msg.data = "long" then
        for i in [1..10000000] do
            i/2 |> ignore
    else
        null |> ignore // do nothing
    
    postMessage <| ((TimeHelpers.getInterval start)/1000.)
    closeWorker()
    

defineWorkerOnMsg testFn
// let onmessage (msg: {|data: string|}) =
//     postMessage <| sprintf "Reply from worker for request %s" msg.data