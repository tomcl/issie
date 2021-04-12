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

            member self.getCanvas = 
                match self with
                | CanvasOnly c -> c 
                | CanvasWithFileWaveInfo (c,_,_) -> c

            member self.getTimeStamp = 
                match self with
                | CanvasOnly _ -> System.DateTime.MinValue 
                | CanvasWithFileWaveInfo (_,_,ts) -> ts
            member self.getWaveInfo =
                match self with
                | CanvasOnly _ -> None 
                | CanvasWithFileWaveInfo (_,waveInfo,_) -> waveInfo



        let stateToJsonString (cState: CanvasState, waveInfo: SavedWaveInfo option) : string =
            let time = System.DateTime.Now
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
             
                 Json.serialize<SavedInfo> (CanvasWithFileWaveInfo (cState, waveInfo, time))
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

let mapKeys (map:Map<'a,'b>) = map |> Map.toSeq |> Seq.map fst |> Array.ofSeq
let mapValues (map:Map<'a,'b>) = map |> Map.toSeq |> Seq.map snd |> Array.ofSeq
let mapItems (map:Map<'a,'b>) = map |> Map.toSeq |> Array.ofSeq

let mapUnion m1 m2 =
    (m2, m1)
    ||> Map.fold (fun m key value -> Map.add key value m )

let mapInverse (m:Map<'A,'B>) =
    m
    |> Map.toArray
    |> Array.map (fun (a,b) -> b,a)
    |> Map.ofArray

let shortPComp (comp:Component) =
    match comp.Type with
    | Custom sc -> sprintf "%s:Custom.%s.%A->%A" comp.Label sc.Name sc.InputLabels sc.OutputLabels
    | _ -> sprintf "%s:%A" comp.Label comp.Type

/// print initial n characters of a string
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


let getMemData (address: int64) (memData: Memory) =
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


    arrayBuffer()
    arrayBuffer()
    mutableArrayBuffer()
    mutableArrayBuffer()
    updateArrayBuffer()
    updateArrayBuffer()
    listBuffer()
    listBuffer()
    mapBuffer()
    mapBuffer()
    updateMapBuffer()
    updateMapBuffer()

// true for printout of times for updates and parts of view function
let mutable instrumentation = true
let mutable threshold = 0.
let mutable updateThreshold = 5.

let getTimeMs() = Fable.Core.JS.Constructors.Date.now()

let printInterval name startTime =
    if instrumentation then 
        printfn $"%s{name}: %.1f{getTimeMs() - startTime}ms"

let instrumentInterval name startTime output =
    let interval = getTimeMs() - startTime
    if interval > threshold then
        printInterval name startTime
    output



    
