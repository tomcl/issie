module WebWorker

open WorkerInterface

let longTestFn (msg: {|data: string|}) =
    let start = TimeHelpers.getTimeMs()
    for i in [1..10000000] do
        for j in [1..10] do
            j/2 |> ignore
    postMessage <| sprintf "Reply from worker for request: '%s' in %f seconds" msg.data ((TimeHelpers.getInterval start)/1000.)
    closeWorker()
    

defineWorkerOnMsg longTestFn
// let onmessage (msg: {|data: string|}) =
//     postMessage <| sprintf "Reply from worker for request %s" msg.data