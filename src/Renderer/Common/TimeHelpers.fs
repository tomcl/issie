(*
Functions to measure elapsed time and instrument the operation of update and view functions, displaying
results in various forms.
*)

module TimeHelpers
open CommonTypes
open Helpers

let checkPerformance m n startTimer stopTimer =
    printfn "Checking performance with size = %d, iterations = %d" m n
    let arrayBuffer() = 
        let buff =
            [|0..m-1|]
            |> Array.map (fun i -> (i+1) % m)
        let mutable index = 0
        let mutable el = 0
        startTimer "Array"
        while index < n do
             index <- index + 1
             el <- buff[el]    
        el |> ignore
        stopTimer "Array"   

    let arrayBufferLookup() = 
        let buff =
            [|0..m-1|]
            |> Array.map (fun i -> (i+1) % m)
        let mutable index = 0
        let mutable el = 0
        startTimer "ArrayBufferLookup"
        while index < n / 2 do
            index <- index + 1
            for i = 0 to (m-1)/2 do
                el <- buff[el] + index
        el |> ignore
        stopTimer "ArrayBufferLookup"   

    let mutableArrayBuffer() = 
         let buff =
             [|0..m-1|]
             |> Array.map (fun i -> (i+1) % m)
         let mutable index = 0
         let mutable el = 0
         startTimer "Mutable Array"
         while index < n do
              index <- index + 1
              el <- if el+1 < m then el+1 else 0
              buff[el]  <- index    
         buff |> ignore
         stopTimer "Mutable Array"   


    let updateArrayBuffer() = 
         let buff =
             [|0..m-1|]
             |> Array.map (fun i -> (i+1) % m)
         let mutable index = 0
         let mutable el = 0
         let mutable arr = buff
         startTimer "Copy-update Array"
         let z = (buff,[0..n]) ||> List.fold (fun buff i -> 
            let r = (Array.copy buff)
            r[i % m] <- i
            r)
         z[0] |> ignore          
         stopTimer "Copy-update Array"   

    let listBuffer() = 
        let buff =
            [0..m-1]
            |> List.map (fun i -> (i+1) % m)
        let mutable index = 0
        let mutable el = 0
        startTimer "List"
        while index < n do
             index <- index + 1
             el <- buff[el]
        el |> ignore
        stopTimer "List"   

    let dictBuffer() =
        let dict = System.Collections.Generic.Dictionary()
        [|0..m-1|]
        |> Array.iter (fun i -> dict[i] <- (i+1) % m)
        let mutable index = 0
        let mutable el = 0
        startTimer "Dict"
        while index < n do
            index <- index + 1
            el <- dict[el]
        index |> ignore
        stopTimer "Dict"   

    let mapBuffer() = 
        let buff =
            [|0..m-1|]
            |> Array.map (fun i -> i,(i+1) % m)
            |> Map.ofArray
        let mutable index = 0
        let mutable el = 0
        startTimer "Map"
        while index < n do
            index <- index + 1
            el <- buff[el]
        index |> ignore
        stopTimer "Map"   


        
    let updateMapBuffer() = 
        let buff =
            [|0..m-1|]
            |> Array.map (fun i -> i,(i+1) % m)
            |> Map.ofArray
        startTimer "UpdateMap"
        let buf = (buff, [|0..n-1|]) ||> Array.fold (fun buff i -> Map.add (i % m) i buff)
        stopTimer "UpdateMap" 
        buf.Count |> ignore

    let updateDictBuffer() = 
        let dict = System.Collections.Generic.Dictionary()
        [|0..m-1|]
        |> Array.iter (fun i -> dict[i] <- (i+1) % m)
        startTimer "UpdateDict"
        let dict = (dict, [|0..n-1|]) ||> Array.fold (fun dict i -> 
            let d = System.Collections.Generic.Dictionary(dict)
            d)
        stopTimer "UpdateDict" 


    arrayBuffer()
    arrayBuffer()
    arrayBufferLookup()
    arrayBufferLookup()
    mutableArrayBuffer()
    mutableArrayBuffer()
    updateArrayBuffer()
    updateArrayBuffer()
    listBuffer()
    listBuffer()
    mapBuffer()
    mapBuffer()
    dictBuffer()
    dictBuffer()
    updateMapBuffer()
    updateMapBuffer()
    updateDictBuffer()

//-----------------Code to record and print execution time statistics-------//

open Fable.Core

[<Emit("performance.now()")>]
let performanceNow() : float = jsNative

let timeNowInMicroS() = 
    performanceNow()
    |> (fun t -> t * 1000.)

let timeNowInMS() = 
    performanceNow()

type Stats = {
    Min: float
    Max: float
    Av: float
    Num: float
    }

/// add time t to st
let addTimeToStats (t:float) (st:Stats) =
    {
        Min = min st.Min t
        Max = max st.Max t
        Av = (st.Av*st.Num + t)/(st.Num+1.)
        Num = st.Num + 1.
    }

/// execution time stats indexed by name in recordExecutionStats
let mutable executionStats = Map<string,Stats> []

/// Run (f arg) recording its time in executionStats under name.
/// NB - this will run f multiple times if needed to estimate average speed more accurately.
/// If an execution time of 5ms for this function is too long reduce timeLimit.
/// The multiple time execution will not work, and will give lower than real results, if
/// f is memoised. In that case set timeLimit to 0. for only one execution.
let recordExecutionTimeStats (name: string) (f: 'a -> 'b) (arg: 'a) : 'b =
    let timeLimit = 0. // time in ms to execute f for.
    let t1 = timeNowInMicroS()
    let execTime() = float (timeNowInMicroS() - t1) / 1000.
    let res = f arg // do f
    let mutable iterations = 1
    while execTime() < timeLimit do // do f multiple times if it is fast to get more accurate speed statistics
        iterations <- iterations + 1
        f arg |> ignore // do f again
    let t = execTime() / float iterations
    executionStats <-
        Map.tryFind name executionStats
        |> Option.map (addTimeToStats t)
        |> Option.defaultValue {Min=t;Max=t;Av=t;Num=1.}  
        |> (fun st -> Map.add name st executionStats)
    res

/// print
let printStats() =
    executionStats
    |> Map.toList
    |> List.iter (fun (name,st) -> 
        printfn "%s time: min=%.3fms max=%.3fms av=%.3fms samples:%d" name st.Min st.Max st.Av (int st.Num))
    executionStats <- Map [] // reset stats


/// returns absolute time in ms, works under both .Net and Fable
let getTimeMs() = 
#if FABLE_COMPILER
    Fable.Core.JS.Constructors.Date.now()
#else
    let start = System.DateTime.UtcNow;           
    float start.Ticks / float 10000
#endif

let getInterval (startTime:float) =
    getTimeMs() - startTime

/// Return time taken by thunk()
/// Run thunk() as many times as is needed
/// for total elapsed time in ms to be  > limitMs.
/// Return average time of all runs.
/// To minimise cache effects run thunk() once before
/// starting to time.
let getTimeOfInMs (limitMs: float) (thunk: Unit -> Unit) =
    thunk()
    let startT = getTimeMs()
    let mutable i = 0
    while getInterval startT < limitMs do
        i <- i+1
        thunk()
    getInterval startT / float i

type AggregatedData = {
    PrintInterval: float
    LastPrintTime: float
    Counts: Map<string,int>
    Times: Map<string,float>
    MinVals: Map<string,float>
    MaxVals: Map<string,float>
}

/// controls how time intervals are collected and displayed
type InstrumentationControl =
    | ImmediatePrint of Threshold: float * UpdateThreshold: float
    | Aggregate of AggregatedData
    | Off

/// initialise instrumentation parameter for immediate time printing
let immediate threshold updateThreshold =
    ImmediatePrint(threshold,updateThreshold)

/// initialse instrumentation parameter for aggregate time printing
let aggregate(printInterval: float ) =
    Aggregate {
        PrintInterval = printInterval
        LastPrintTime = Fable.Core.JS.Constructors.Date.now()
        Times = Map.empty
        MinVals = Map.empty
        MaxVals = Map.empty
        Counts = Map.empty
    }

/// Parameter that controls how recorded times are processed.                     
let mutable instrumentation: InstrumentationControl = 
    //aggregate 10000.  // for aggregate printing every 10s
    immediate 10. 10. // for immediate printing
    // Off // for no printing

/// print out the current aggregate of recorded times if this is requried. 
/// Return initialised aggregate totals after print
let printAgg (agg: AggregatedData) =
    let now = Fable.Core.JS.Constructors.Date.now()
    let getData name =
        let num = mapFindWithDef  0 name agg.Counts
        let numF = float num
        if num = 0 then 
            0.,"" 
        else
            let tot = (mapFindWithDef 0. name agg.Times)
            tot,
            $"%8.2f{tot/numF}%8.1f{mapFindWithDef 0. name agg.MaxVals}\
            %8.1f{mapFindWithDef 0. name agg.MinVals}%8.1f{tot}  %s{name}"
            
    let intv = now - agg.LastPrintTime
    if intv < agg.PrintInterval then
        agg // do nothing
    else
        let head = sprintf "Interval times in ms after %.1fs\n      Av     Max     Min  Total    Name\n" (intv / 1000.)
        let timeLines = 
            (mapKeys agg.Counts)
            |> Seq.map getData
            |> Seq.filter (fun (tot,_) -> tot > 10.)
            |> Seq.sortBy fst
            |> Seq.map snd
            |> String.concat "\n"
        printfn "%s" (head + timeLines)
        { agg with 
            LastPrintTime = now
            Counts = Map.empty
            MaxVals = Map.empty
            MinVals = Map.empty
            Times=Map.empty}

/// process a new time interval updating the aggregated data for future printout
let updateAgg (name:string) (time: float) (agg: AggregatedData) =
    { agg with
        Counts = mapUpdateWithDef 0 ((+) 1) name agg.Counts
        Times = mapUpdateWithDef 0. ((+) time) name agg.Times
        MaxVals = mapUpdateWithDef 0. (max time) name agg.MaxVals
        MinVals = mapUpdateWithDef 1.0E10 (min time) name agg.MinVals
    }


/// According to current settings, process and/or print a named time interval.
/// the interval is between intervalStartTime passed as arg 2, and the time at
/// which this function is called (all times obtained using getTimeMs).
let instrumentTime (intervalName: string) (intervalStartTime: float) =
    match instrumentation with
    | Off -> ()
    | ImmediatePrint(threshold,updateThreshold) ->
        let interval = getInterval intervalStartTime
        let threshold = 
            if intervalName.StartsWith "update" 
            then updateThreshold 
            else threshold 
        if interval > threshold then
            printfn "%s" $"{intervalName}: %.1f{interval}ms"
    | Aggregate agg ->
        let interval = getTimeMs() - intervalStartTime
        let agg = updateAgg intervalName interval agg
        let agg = printAgg agg
        instrumentation <- Aggregate agg

                



/// print out a time interval
let private printInterval name startTime =
    instrumentTime name startTime

/// This function with its first two args should be put in a pipe after the code to be timed.
/// it will return its piped input, with the side effect of recording the time delay in the
/// function. Parameter start must be defined at the start of the code to be timed using getTimeMs.
let instrumentInterval name startTime output =
    printInterval name startTime
    output

/// Record the elapsed time taken in execution of (func arg).
/// Return the result from (func arg).
let instrumentFunctionCall name func arg =
    let startTime = getTimeMs()
    func arg
    |> instrumentInterval name
   

