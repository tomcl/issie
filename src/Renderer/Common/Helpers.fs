(*
    Helpers.fs

    Some miscellaneous fsharp only (no JS) utility functions.
*)

module Helpers
open CommonTypes


    [<AutoOpen>]
    module JsonHelpers =
        open Fable.SimpleJson
        open LegacyCanvas

        type SavedCanvasUnknownWaveInfo<'T> = | NewCanvasWithFileWaveSheetInfoAndNewConns of CanvasState * 'T option * SheetInfo option * System.DateTime

        type SavedInfo =
            | CanvasOnly of LegacyCanvasState
            | CanvasWithFileWaveInfo of LegacyCanvasState * SavedWaveInfo option * System.DateTime
            | CanvasWithFileWaveInfoAndNewConns of LegacyCanvasState * SavedWaveInfo option * System.DateTime
            | NewCanvasWithFileWaveInfoAndNewConns of CanvasState * SavedWaveInfo option * System.DateTime
            | NewCanvasWithFileWaveSheetInfoAndNewConns of CanvasState * SavedWaveInfo option * SheetInfo option * System.DateTime
            
            member self.getCanvas = 
                match self with
                | CanvasOnly c -> legacyTypesConvert c 
                | CanvasWithFileWaveInfo (c,_,_) -> legacyTypesConvert c
                | CanvasWithFileWaveInfoAndNewConns (c,_,_) -> legacyTypesConvert c
                | NewCanvasWithFileWaveInfoAndNewConns(c,_,_) -> c
                | NewCanvasWithFileWaveSheetInfoAndNewConns (c,_,_,_) -> c

            member self.getTimeStamp = 
                match self with
                | CanvasOnly _ -> System.DateTime.MinValue 
                | CanvasWithFileWaveInfo (_,_,ts) -> ts
                | CanvasWithFileWaveInfoAndNewConns (_,_,ts) -> ts
                | NewCanvasWithFileWaveInfoAndNewConns (_,_,ts) -> ts
                | NewCanvasWithFileWaveSheetInfoAndNewConns (_,_,_,ts) -> ts

            member self.getWaveInfo =
                match self with
                | CanvasOnly _ -> None 
                | CanvasWithFileWaveInfo (_,waveInfo,_) -> waveInfo
                | CanvasWithFileWaveInfoAndNewConns (_,waveInfo,_) -> waveInfo
                | NewCanvasWithFileWaveInfoAndNewConns (_,waveInfo,_) -> waveInfo
                | NewCanvasWithFileWaveSheetInfoAndNewConns (_,waveInfo,_,_) -> waveInfo

            member self.getSheetInfo =
                match self with
                | CanvasOnly _ -> None 
                | CanvasWithFileWaveInfo (_,waveInfo,_) -> None
                | CanvasWithFileWaveInfoAndNewConns (_,waveInfo,_) -> None
                | NewCanvasWithFileWaveInfoAndNewConns (_,_,ts) -> None
                | NewCanvasWithFileWaveSheetInfoAndNewConns (_,_,sheetInfo,_) -> sheetInfo

        let stateToJsonString (cState: CanvasState, waveInfo: SavedWaveInfo option, sheetInfo: SheetInfo option) : string =
            let time = System.DateTime.Now
            //printfn "%A" cState
            try            
                 Json.serialize<SavedInfo> (NewCanvasWithFileWaveSheetInfoAndNewConns (cState, waveInfo, sheetInfo, time))
            with
            | e -> 
                printfn "HELP: exception in SimpleJson.stringify %A" e
                "Error in stringify"
        
        let jsonStringToState (jsonString : string) =
             Json.tryParseAs<LegacyCanvasState> jsonString
             |> (function
                    | Ok state -> Ok (CanvasOnly state)
                    | Error _ ->
                        match Json.tryParseAs<SavedInfo> jsonString with
                        | Ok state -> Ok state
                        | Error str -> 
                            match Json.tryParseAs<SavedCanvasUnknownWaveInfo<obj>> jsonString with
                            | Ok (SavedCanvasUnknownWaveInfo.NewCanvasWithFileWaveSheetInfoAndNewConns(cState,_,sheetInfo,time)) ->
                                Ok <| NewCanvasWithFileWaveSheetInfoAndNewConns(cState,None,sheetInfo,time)                               
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

/// replace new lines in a string by ';' for easier debug printing of records using %A
let nocr (s:string) = 
    s.Replace("\n",";")



// access to JS reference equality operation (===)



// NB mapKeys and mapValues should probably be changed to use F# 6 Map.kets, Map.values

/// Array of map keys
let inline mapKeys (map:Map<'a,'b>) = map |> Map.toArray |> Array.map fst

/// Array of map values
let inline mapValues (map:Map<'a,'b>) = map |> Map.toArray |> Array.map snd 

/// Map a function over a pair of elements.
/// mapPair f (x,y) = f x, f y.
let inline mapPair (f: 'S -> 'T) ((p1,p2): 'S * 'S) =
    f p1, f p2

/// Look up key in map, return defVal if key is not found
let inline mapFindWithDef (defVal: 'b) (key: 'a) (map:Map<'a,'b>) = 
    Option.defaultValue defVal (Map.tryFind key map)

/// If key exists in map: (key:v) -> (key:update v), otherwise create new item
/// (key : update v)
let inline mapUpdateWithDef (defVal: 'b) (update: 'b -> 'b) (key: 'a) (map:Map<'a,'b>)  =
    let v = Option.defaultValue defVal (Map.tryFind key map)
    Map.add key (update v) map

/// Union of maps, common keys take m1 value
let inline mapUnion m1 m2 =
    (m2, m1)
    ||> Map.fold (fun m key value -> Map.add key value m )

/// create inverse map
let inline mapInverse (m:Map<'A,'B>) =
    m
    |> Map.toSeq
    |> Seq.map (fun (a,b) -> b,a)
    |> Map.ofSeq

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
#if ASSERTS
    assertThat (idx >= 0 && idx < lst.Length)
    <| sprintf "Index out of range in listSet. Idx: %d, list length: %d" idx lst.Length
#endif
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
    | false when fromStart -> str[..len-1] + "..." // From start.
    | false -> "..." + str[str.Length - len..]     // From end.


let getMemData (address: int64) (memData: Memory1) =
#if ASSERTS
    assertThat (memData.AddressWidth > 63 || (1UL <<< memData.AddressWidth) > (uint64 address)) (
        sprintf "Inconsistent memory access: address %A, memData %A" address memData)
#endif
    Map.tryFind address memData.Data
    |> Option.defaultValue 0L

/// Returns a new array with the elements at index i1 and index i2 swapped
let swapArrayEls i1 i2 (arr: 'a[]) =
    arr
    |> Array.mapi (fun i x ->
        if i = i1 then arr[i2]
        else if i = i2 then arr[i1]
        else x)

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

    let getPortInts sel initV (ports: Port list) = 
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
        id2Outs[ComponentId p.HostId]
        |> List.find (fun p1 -> p1.Id = p.Id)
        |> (fun p -> match p.PortNumber with Some n -> n | None -> failwithf "Missing input port number on %A" p.HostId)
        |> OutputPortNumber
       
   
    let getInputPortNumber (p:Port) = 
        id2Ins[ComponentId p.HostId]
        |> List.find (fun p1 -> p1.Id = p.Id)
        |> (fun p -> match p.PortNumber with Some n -> n | None -> failwithf "Missing input port number on %A" p.HostId)
        |> InputPortNumber
    
    let updateNComp compId updateFn (nets:NetList) =
        Map.add compId (updateFn nets[compId]) nets

    let updateInputPorts pNum src (comp:NetListComponent) =
        { comp with Inputs = Map.add pNum (Some src) comp.Inputs}

    let updateInputsComp compId pNum src nets =
        let uFn = updateInputPorts pNum src
        updateNComp compId uFn nets

    let updateOutputPorts pNum tgt (comp:NetListComponent) =
        {comp with Outputs = Map.add pNum (tgt :: comp.Outputs[pNum]) comp.Outputs}

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
        let tComp = id2Comp[tgt.TargetCompId]
        let sComp = id2Comp[src.SourceCompId]
        nets
        |> updateOutputsComp (ComponentId sComp.Id) src.OutputPort tgt
        |> updateInputsComp (ComponentId tComp.Id)tgt.InputPort src

    (initNets, conns) ||> List.fold addConnectionsToNets

let testMatch (diffX:float) (diffY:float)  normRot=
    let s:float = 1.0
    let lengthList() : float list = 
        match normRot with
        // Same orientation
        | 0 when (diffX >= 0) -> [s; 0; diffX; diffY; 0; 0; -s]                                                    
        | 0 when (diffX < 0) -> [s; 0; 0; diffY; diffX; 0; -s]                                             
        // Opposite orientation
        | 180 when (diffX >= 0) -> [s; 0; (diffX - 2.0 * s)/2.0; diffY; (diffX - 2.0 * s)/2.0; 0; s]           
        | 180 when (diffX < 0) -> [s; diffY/2.0; (diffX - 2.0 * s); diffY/2.0; 0; 0; s]            
        // Perpendicular orientation: if startPort points to the right, endPort points down
        | 90 when ((diffX >= 0) && (diffY >= 0)) -> [s; 0; (diffX - s)/2.0; (diffY + s); (diffX - s)/2.0; 0; 0; -s] 
        | 90 when ((diffX >= 0) && (diffY < 0)) -> [s; 0; (diffX - s); (diffY + s); 0; 0; 0; -s]                
        | 90 when ((diffX < 0) && (diffY >= 0)) -> [s; 0; 0; (diffY + s); (diffX - s); 0; 0; -s]               
        | 90 when ((diffX < 0) && (diffY < 0)) -> [s; 0; 0; (diffY+s)/2.0; (diffX-s); (diffY+s)/2.0; 0; -s]    
        // Perpendicular orientation: if startPort points to the right, endPort points up
        | 270 when ((diffX >= 0) && (diffY >= 0)) -> [s; 0; (diffX - s); (diffY - s); 0; 0; 0; s]         
        | 270 when ((diffX >= 0) && (diffY < 0)) -> [s; 0; (diffX - s)/2.0; (diffY - s); (diffX - s)/2.0; 0; 0; s] 
        | 270 when ((diffX < 0) && (diffY >= 0)) -> [s; 0; 0; (diffY - s)/2.0; (diffX - s); (diffY - s)/2.0; 0; s]   
        | 270 when ((diffX < 0) && (diffY < 0)) -> [s; 0; 0; (diffY - s); (diffX - s); 0; 0; s]  
        // Edge case that should never happen
        | _ -> [s; 0; 0; 0; 0; 0; s]
    lengthList()
