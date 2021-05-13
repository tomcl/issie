(*
    Helpers.fs

    Some fsharp only (no JS) utility functions.
*)

module Helpers
open CommonTypes

    [<AutoOpen>]
    module JsonHelpers =
        open Fable.SimpleJson


        type SavedInfo =
            | CanvasOnly of CanvasState
            | CanvasWithFileWaveInfo of CanvasState * SavedWaveInfo option * System.DateTime
            | CanvasWithFileWaveInfoAndNewConns of CanvasState * SavedWaveInfo option * System.DateTime

            member self.getCanvas = 
                match self with
                | CanvasOnly c -> c 
                | CanvasWithFileWaveInfo (c,_,_) -> c
                | CanvasWithFileWaveInfoAndNewConns (c,_,_) -> c

            member self.getTimeStamp = 
                match self with
                | CanvasOnly _ -> System.DateTime.MinValue 
                | CanvasWithFileWaveInfo (_,_,ts) -> ts
                | CanvasWithFileWaveInfoAndNewConns (_,_,ts) -> ts

            member self.getWaveInfo =
                match self with
                | CanvasOnly _ -> None 
                | CanvasWithFileWaveInfo (_,waveInfo,_) -> waveInfo
                | CanvasWithFileWaveInfoAndNewConns (_,waveInfo,_) -> waveInfo




        let stateToJsonString (cState: CanvasState, waveInfo: SavedWaveInfo option) : string =
            let time = System.DateTime.Now
            //printfn "%A" cState
            try (*
                 printfn "\n--------cState----------\n%A\n" cState
                 printfn "\n-----savedWaveInfo--------\n%A\n------------\n" waveInfo

                 SimpleJson.stringify ([||]) |> ignore
                 printfn "testWI:"
                 SimpleJson.stringify (testWI) |> ignore
                 printfn "\ntrying to stringify waveinfo"
                 SimpleJson.stringify (waveInfo) |> ignore
                 printfn "\n trying to stringify cState"
                 SimpleJson.stringify (cState) |> ignore
                 printfn "\n trying to stringify time"
                 SimpleJson.stringify (time) |> ignore
                 printfn "\n\nTrying to stringify all" *)
             
                 Json.serialize<SavedInfo> (CanvasWithFileWaveInfoAndNewConns (cState, waveInfo, time))
            with
            | e -> 
                printfn "HELP: exception in SimpleJson.stringify %A" e
                "Error in stringify"
        

        let jsonStringToState (jsonString : string) =
             Json.tryParseAs<CanvasState> jsonString
             |> (function
                    | Ok state -> Ok (CanvasOnly state)
                    | Error _ ->
                        match Json.tryParseAs<SavedInfo> jsonString with
                        | Ok state -> Ok state
                        | Error str -> 
                            printfn "Error in Json parse of %s : %s" jsonString str
                            Error str)



(*-----------------------------------General helpers-----------------------------------------*)

/// Return a memoized version of funcToMemoize where.
/// Repeated calls with equivalent inputs return a stored result.
/// Inputs a, a' are deemed equivalent if keyFunc a = keyFunc a'.
/// Use this as well as LazyView etc, it has a different usage since it need not
/// have React output and comparison is via a key function.
let memoizeBy (keyFunc: 'a -> 'k) (funcToMemoize: 'a -> 'c) : 'a -> 'c =
    let mutable lastKey: 'k option = None
    let mutable lastValue: 'c option = None
    fun (a: 'a) ->
        let newKey = Some (keyFunc a)
        if newKey = lastKey 
        then Option.get lastValue
        else 
            lastKey <-newKey
            let v = funcToMemoize a
            lastValue <- Some v
            v


            
/// Array of map keys
let mapKeys (map:Map<'a,'b>) = map |> Map.toSeq |> Seq.map fst |> Array.ofSeq

/// Array of map values
let mapValues (map:Map<'a,'b>) = map |> Map.toSeq |> Seq.map snd |> Array.ofSeq

/// Array of map key,value items
let mapItems (map:Map<'a,'b>) = map |> Map.toSeq |> Array.ofSeq

/// Look up key in map, return defVal if key is not found
let mapFindWithDef (defVal: 'b) (key: 'a) (map:Map<'a,'b>) = 
    Option.defaultValue defVal (Map.tryFind key map)

/// If key exists in map: (key:v) -> (key:update v), otherwise create new item
/// (key : update v)
let mapUpdateWithDef (defVal: 'b) (update: 'b -> 'b) (key: 'a) (map:Map<'a,'b>)  =
    let v = Option.defaultValue defVal (Map.tryFind key map)
    Map.add key (update v) map

/// Union of maps, common keys take m1 value
let mapUnion m1 m2 =
    (m2, m1)
    ||> Map.fold (fun m key value -> Map.add key value m )

/// create inverse map
let mapInverse (m:Map<'A,'B>) =
    m
    |> Map.toArray
    |> Array.map (fun (a,b) -> b,a)
    |> Map.ofArray

let shortPComp (comp:Component) =
    match comp.Type with
    | Custom sc -> sprintf "%s:Custom.%s.%A->%A" comp.Label sc.Name sc.InputLabels sc.OutputLabels
    | _ -> sprintf "%s:%A" comp.Label comp.Type

/// return initial n characters of a string
let sprintInitial n (s:string) = 
    s
    |> Seq.truncate n
    |> Seq.map string
    |> String.concat ""

let assertThat cond msg =
    if not cond
    then failwithf "what? assert failed: %s" msg

/// Return the first error found in a list of results, or the list of Oks if
/// there are none.
let tryFindError (lst : Result<'a,'b> list) : Result<'a list, 'b> =
    let isError el = match el with | Error _ -> true | Ok _ -> false
    let extractOk el = match el with | Ok ok -> ok | Error _ -> failwith "what? Impossible case in tryFindError"
    match List.tryFind isError lst with
    | Some (Error err) -> Error err
    | None -> List.map extractOk lst |> Ok
    | _ -> failwith "what? Impossible case in tryFindError"

/// Return 2^exponent.
let pow2 (exponent : int) : int =
    1 <<< exponent // TODO use bit-shift.

/// Return 2^exponent, packed into an int64.
let pow2int64 (exponent : int) : int64 =
    1L <<< exponent

/// Set an element of the list at the specified position.
/// This function is slow: O(n). Do not use unless necessary.
let listSet (lst : 'a list) (item : 'a) (idx : int) : 'a list =
    assertThat (idx >= 0 && idx < lst.Length)
    <| sprintf "Index out of range in listSet. Idx: %d, list length: %d" idx lst.Length
    let p1, p2 = List.splitAt idx lst
    // p2 has always at least one element as idx < lst.Length.
    // Remove the first element of p2.
    let _, p2 = List.splitAt 1 p2
    p1 @ [item] @ p2

/// Crop a string to the specified length.
/// fromStart indicates whether you want the first <len> characters or the last
/// <len> characters.
let cropToLength (len : int) (fromStart : bool) (str : string) =
    match str.Length <= len with
    | true -> str
    | false when fromStart -> str.[..len-1] + "..." // From start.
    | false -> "..." + str.[str.Length - len..]     // From end.


let getMemData (address: int64) (memData: Memory1) =
    assertThat (memData.AddressWidth > 63 || (1UL <<< memData.AddressWidth) > (uint64 address)) (
        sprintf "Inconsistent memory access: address %A, memData %A" address memData)
    Map.tryFind address memData.Data
    |> Option.defaultValue 0L

//--------------------Helper Functions-------------------------------//
//-------------------------------------------------------------------//

let getNetList ((comps,conns) : CanvasState) =
    let id2X f =
        comps
        |> List.map f
        |> Map.ofList
    let id2Outs = id2X (fun (c:Component) -> ComponentId c.Id,c.OutputPorts)
    let id2Ins = id2X (fun (c:Component) -> ComponentId c.Id,c.InputPorts)
    let id2Comp = id2X (fun (c:Component) -> ComponentId c.Id,c)

    let getPortInts sel initV ports = 
        ports
        |> List.map (fun port -> 
            match port.PortNumber with
            | Some pn -> sel pn , initV
            | _ -> failwithf "Missing port in list %A" ports)
        |> Map.ofList

    let initNets =
        comps
        |> List.map ( fun comp ->
            {
                Id = ComponentId comp.Id
                Type = comp.Type
                Label = comp.Label
                Inputs =  getPortInts InputPortNumber None comp.InputPorts 
                Outputs = getPortInts OutputPortNumber [] comp.OutputPorts
            })
        |> List.map (fun comp -> comp.Id,comp)
        |> Map.ofList

    let getOutputPortNumber (p:Port) = 
        id2Outs.[ComponentId p.HostId]
        |> List.find (fun p1 -> p1.Id = p.Id)
        |> (fun p -> match p.PortNumber with Some n -> n | None -> failwithf "Missing input port number on %A" p.HostId)
        |> OutputPortNumber
       
   
    let getInputPortNumber (p:Port) = 
        id2Ins.[ComponentId p.HostId]
        |> List.find (fun p1 -> p1.Id = p.Id)
        |> (fun p -> match p.PortNumber with Some n -> n | None -> failwithf "Missing input port number on %A" p.HostId)
        |> InputPortNumber
    
    let updateNComp compId updateFn (nets:NetList) =
        Map.add compId (updateFn nets.[compId]) nets

    let updateInputPorts pNum src (comp:NetListComponent) =
        { comp with Inputs = Map.add pNum (Some src) comp.Inputs}

    let updateInputsComp compId pNum src nets =
        let uFn = updateInputPorts pNum src
        updateNComp compId uFn nets

    let updateOutputPorts pNum tgt (comp:NetListComponent) =
        {comp with Outputs = Map.add pNum (tgt :: comp.Outputs.[pNum]) comp.Outputs}

    let updateOutputsComp compId pNum tgt nets =
        let uFn = updateOutputPorts pNum tgt
        updateNComp compId uFn nets
        
    let target (conn:Connection) =
        {
            TargetCompId = ComponentId conn.Target.HostId
            InputPort = getInputPortNumber conn.Target
            TargetConnId = ConnectionId conn.Id
        }
    let source (conn:Connection) =
        {
            SourceCompId = ComponentId conn.Source.HostId
            OutputPort = getOutputPortNumber conn.Source
            SourceConnId = ConnectionId conn.Id
        }

    let addConnectionsToNets (nets:Map<ComponentId,NetListComponent>) (conn:Connection) =
        let tgt = target conn
        let src = source conn
        let tComp = id2Comp.[tgt.TargetCompId]
        let sComp = id2Comp.[src.SourceCompId]
        nets
        |> updateOutputsComp (ComponentId sComp.Id) src.OutputPort tgt
        |> updateInputsComp (ComponentId tComp.Id)tgt.InputPort src

    (initNets, conns) ||> List.fold addConnectionsToNets



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
             el <- buff.[el]    
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
             for i = 0 to m-1 do
                index <- index + buff.[i]
                el <- el + m
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
              buff.[el]  <- index    
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
            r.[i % m] <- i
            r)
         z.[0] |> ignore          
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
             el <- buff.[el]
        el |> ignore
        stopTimer "List"   

    let dictBuffer() =
        let dict = System.Collections.Generic.Dictionary()
        [|0..m-1|]
        |> Array.iter (fun i -> dict.[i] <- (i+1) % m)
        let mutable index = 0
        let mutable el = 0
        startTimer "Dict"
        while index < n do
            index <- index + 1
            el <- dict.[el]
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
            el <- buff.[el]
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
        |> Array.iter (fun i -> dict.[i] <- (i+1) % m)
        startTimer "UpdateDict"
        let dict = (dict, [|0..n-1|]) ||> Array.fold (fun dict i -> 
            let d = System.Collections.Generic.Dictionary(dict)
            d)
        stopTimer "UpdateDict" 


    arrayBuffer()
    arrayBuffer()
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

let getTimeMs() = Fable.Core.JS.Constructors.Date.now()


let getInterval (startTime:float) =
    getTimeMs() - startTime


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
    Off // for aggregate printing
    // immediate 2. 2. // for immediate printing
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
            |> Array.map getData
            |> Array.filter (fun (tot,_) -> tot > 10.)
            |> Array.sortBy fst
            |> Array.map snd
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
   


    
