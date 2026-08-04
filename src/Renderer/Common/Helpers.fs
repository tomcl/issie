(*
    Helpers.fs

    Some miscellaneous fsharp only (no JS) utility functions.
*)

module Helpers
open EEExtensions
open CommonTypes
open System.Text.RegularExpressions
    [<AutoOpen>]
    module JsonHelpers =
        open Fable.SimpleJson
        open LegacyCanvas
        #if FABLE_COMPILER
        open Thoth.Json
        #else
        open Thoth.Json.Net
        #endif

        type JSONCanvasState = JSONComponent.Component list * Connection list

        type SavedCanvasUnknownWaveInfo<'T> = | NewCanvasWithFileWaveSheetInfoAndNewConns of JSONCanvasState * 'T option * SheetInfo option * System.DateTime

        type SavedInfo =
            | CanvasOnly of LegacyCanvasState
            | CanvasWithFileWaveInfo of LegacyCanvasState * SavedWaveInfo option * System.DateTime
            | CanvasWithFileWaveInfoAndNewConns of LegacyCanvasState * SavedWaveInfo option * System.DateTime
            | NewCanvasWithFileWaveInfoAndNewConns of JSONCanvasState * SavedWaveInfo option * System.DateTime
            | NewCanvasWithFileWaveSheetInfoAndNewConns of JSONCanvasState * SavedWaveInfo option * SheetInfo option * System.DateTime
            
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

        let extraCoder =
            Extra.empty
            |> Extra.withInt64
            |> Extra.withUInt64
            |> Extra.withBigInt
            |> Extra.withCustom CommonTypes.componentIdEncoder CommonTypes.componentIdDecoder

        /// converts Component to JSONComponent.Component for saving as JSON.
        /// this conversion does not affect the JSON generated.
        let convStateToJC ( compL, connL) = (List.map convertToJSONComponent compL, connL)

        /// Code to convert a CanvasState to a JSON string, does not work for bigints (I think).
        /// A serialisation failure is an Error: it must never be written to the sheet's
        /// .dgm file, which would overwrite the sheet with garbage.
        let stateToJsonString (cState: CanvasState, waveInfo: SavedWaveInfo option, sheetInfo: SheetInfo option) : Result<string,string> =
            let time = System.DateTime.Now
            //printfn "%A" cState
            try
                 let savedInfo = NewCanvasWithFileWaveSheetInfoAndNewConns (convStateToJC cState, waveInfo, sheetInfo, time)
                 #if FABLE_COMPILER
                 Json.serialize<SavedInfo> savedInfo
                 #else
                 // SimpleJson serialisation works only under Fable: on .NET (tests) use Thoth
                 Encode.Auto.toString(space = 0, value = savedInfo, extra = extraCoder)
                 #endif
                 |> (fun json -> Regex.Replace(json, """(\d+\.\d\d)\d+""", "$1")) // reduce json size by truncating floats to 2 d.p.
                 |> Ok
            with
            | e ->
                Error $"JSON serialisation of the sheet failed, so it was not saved: {e.Message}"
        /// Code to convert a CanvasState to a JSON string, allowing bigints
        let stateToJsonStringExperimental (cState: CanvasState, waveInfo: SavedWaveInfo option, sheetInfo: SheetInfo option) : Result<string,string> =
            let time = System.DateTime.Now
            try
                Encode.Auto.toString(space = 0, value = (NewCanvasWithFileWaveSheetInfoAndNewConns (convStateToJC cState, waveInfo, sheetInfo, time)), extra = extraCoder)
                |> Ok
            with
            | e ->
                Error $"JSON serialisation of the sheet failed, so it was not saved: {e.Message}"

        /// NB tryParseNativeAs, not tryParseAs. Both are SimpleJson and both end in the same
        /// AST-to-value conversion; they differ only in how the AST is built. tryParseAs uses
        /// Fable.Parsimmon parser combinators over the whole file and measured ~8x slower than the
        /// native JSON.parse variant - 59ms against 7ms on a 108KB sheet - and this function tries
        /// up to three types in turn, so it paid that cost more than once per sheet loaded.
        /// The two were checked to agree on every demo sheet, for each of the three types below.
        let jsonStringToState (jsonString : string) =
            #if FABLE_COMPILER
            Json.tryParseNativeAs<LegacyCanvasState> jsonString
            |> (function
                | Ok state -> Ok (CanvasOnly state)
                | Error _ ->
                    match Json.tryParseNativeAs<SavedInfo> jsonString with
                    | Ok state -> Ok state
                    | Error str -> 
                        match Json.tryParseNativeAs<SavedCanvasUnknownWaveInfo<obj>> jsonString with
                        | Ok (SavedCanvasUnknownWaveInfo.NewCanvasWithFileWaveSheetInfoAndNewConns(cState,_,sheetInfo,time)) ->
                            Ok <| NewCanvasWithFileWaveSheetInfoAndNewConns(cState,None,sheetInfo,time)                               
                        | Error str -> 
                            printfn "Error in Json parse of %s : %s" jsonString str
                            Error str)
            #else
            match Decode.Auto.fromString<LegacyCanvasState>(jsonString, extra = extraCoder) with
            | Ok state -> Ok (CanvasOnly state)
            | Error _ ->
                match Decode.Auto.fromString<SavedInfo>(jsonString, extra = extraCoder) with
                | Ok state -> Ok state
                | Error str ->
                    match Decode.Auto.fromString<SavedCanvasUnknownWaveInfo<obj>>(jsonString, extra = extraCoder) with
                    | Ok (SavedCanvasUnknownWaveInfo.NewCanvasWithFileWaveSheetInfoAndNewConns(cState,_,sheetInfo,time)) ->
                        Ok <| NewCanvasWithFileWaveSheetInfoAndNewConns(cState,None,sheetInfo,time)
                    | Error str ->
                        printfn "Error in Json parse of %s : %s" jsonString str
                        Error str
            #endif


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
let inline mapKeys (map:Map<'a,'b>) = map |> Map.keysA

/// Array of map values
let inline mapValues (map:Map<'a,'b>) = map |> Map.valuesA

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

/// Running a list of operations that can fail, stopping at the first Error.
///
/// Written out by hand this is a fold over Result whose accumulator is built with `got @ [x]`,
/// which is quadratic and was repeated in half a dozen places. These are linear and are the only
/// version.
module ResultList =

    /// Thread a state through `f` for each item in turn. The first Error stops the walk and is
    /// returned; later items are not visited.
    let fold (f: 's -> 'a -> Result<'s, 'e>) (state: 's) (items: 'a list) : Result<'s, 'e> =
        let rec walk state items =
            match items with
            | [] -> Ok state
            | item :: rest ->
                match f state item with
                | Error e -> Error e
                | Ok state -> walk state rest
        walk state items

    /// Apply `f` to every item, collecting the results in order.
    let traverse (f: 'a -> Result<'b, 'e>) (items: 'a list) : Result<'b list, 'e> =
        fold (fun got item -> f item |> Result.map (fun value -> value :: got)) [] items
        |> Result.map List.rev

    /// Apply `f` to every item for its effect only.
    let iter (f: 'a -> Result<unit, 'e>) (items: 'a list) : Result<unit, 'e> =
        fold (fun () item -> f item) () items

    /// The list of Oks, or the first Error.
    let sequence (results: Result<'a, 'e> list) : Result<'a list, 'e> = traverse id results

/// Return 2^exponent.
let pow2 (exponent : int) : int =
    1 <<< exponent // TODO use bit-shift.

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


let getMemData (address: bigint) (memData: Memory1) =
#if ASSERTS
    assertThat (memData.AddressWidth > 63 || (1I <<< memData.AddressWidth) > address) (
        sprintf "Inconsistent memory access: address %A, memData %A" address memData)
#endif
    Map.tryFind address memData.Data
    |> Option.defaultValue 0I

/// Returns a new array with the elements at index i1 and index i2 swapped
let swapArrayEls i1 i2 (arr: 'a[]) =
    arr
    |> Array.mapi (fun i x ->
        if i = i1 then arr[i2]
        else if i = i2 then arr[i1]
        else x)

//--------------------Helper Functions-------------------------------//
//-------------------------------------------------------------------//

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

open Fable.Core

/// Functions to print human-readable version of CanvasState
module PrintSimple =

    /// Print any object as Javascript for low-level debug
    [<Emit("console.log($0)")>]
    let JSPrint msg : unit = jsNative

    /// Crop a string to first n chars
    let crop (s:string) =
        sprintInitial 3 s

    /// print a component simply
    let pComponent (comp: Component) =
        let inPorts =
            comp.InputPorts
            |> List.map (fun p -> crop p.Id)
        let outPorts =
            comp.OutputPorts
            |> List.map (fun p -> crop p.Id)
        $"|{comp.Label}:{comp.Type} PIN={inPorts} POut={outPorts}|"

    /// Print a connection simply
    let pConnection (conn: Connection) =
        $"{crop conn.Source.Id}->{crop conn.Target.Id}"

    /// human-readable print of CanvasState.
    let pState ((comps, conns): CanvasState) =
        "\n-----COMPS-----\n" +
        (comps
        |> List.map pComponent
        |> String.concat "\n") +
        "\n\n-----CONNS----\n" +
        (conns
        |> List.map pConnection
        |> String.concat "\n") +
        "\n"

/// Take a project and compress all of the IDs.
module ReduceKeys =
    open Optics
    /// make ComponentID, PortID, ConnectionID keys all short:
    /// ComponentID -> Cxxx
    /// PortID -> Pxxx
    /// ConnectionId -> Wxxx
    /// xxx = base 36 alphanumeric number
    let a36ToD (ch: char) = int ch - int 'a'
    let dToA36 (d: int) = char d

    let toA36 (n:int) =
        let rec toA36' (alphas: char list) = function
            | 0 when alphas = [] -> [dToA36 0]
            | 0 -> alphas
            | n  -> toA36' (dToA36 (n % 36) :: alphas) (n / 36)
        toA36' [] n
        |> string
        

    let toD alphas =
        let rec toD' res = function
            | [] -> res
            | x :: chs -> toD' (a36ToD x + res*36) chs
        alphas
        |> Seq.toList
        |> toD' 0

    let getIndexFromReduced (s: string) =
        match s.Length with
        | 0 | 1 -> failwithf "Can't convert A36 string ids of less than 2 chars to decimal (first char muts be C|W|P)"
        | n ->
            match s[0] with
            | 'C' | 'P' | 'W' -> toD s[1..n-1]
            | x -> failwithf $"{s} does not start with C|P|W and so is not an A36 ID for Component, Wire, or Port"

    let getReducedFromIndex (typ:string) (index:int) =
        match typ with
        | "W" | "C" | "P" -> typ + toA36 index
        | s -> failwithf $"Can't recognise {s} as W or P or C."
        
    type Reducer =
        {
        mutable NextID: int
        mutable KeyMap: Map<string, int>
        }

    with
        static member Init() = {NextID = 0; KeyMap = Map.empty}

        member this.Scan (id:string) =
                if id.Length < 10 then
                    this.NextID <- max this.NextID (getIndexFromReduced id + 1)

        member this.ScanPort (p: Port) = this.Scan p.Id
                   
        member this.ScanComp (comp:Component) =
            this.Scan comp.Id
            List.iter this.ScanPort comp.InputPorts
            List.iter this.ScanPort comp.OutputPorts

        member this.ScanConn (conn:Connection) =
            this.Scan conn.Id

        member this.ScanCanvas ((compL,connL):CanvasState) =
            List.iter this.ScanComp compL
            List.iter this.ScanConn connL

        member this.ScanProject (p: Project) =
            p.LoadedComponents
            |> List.iter (fun ldc -> this.ScanCanvas ldc.CanvasState)

        member this.ReduceID (typ: string) (longId:string) =
            match typ with
            | "C" | "W" | "P" when longId.Length > 10 -> 
                match Map.tryFind longId this.KeyMap with
                | Some index -> index
                | None ->
                    let index = this.NextID
                    this.NextID <- index + 1
                    this.KeyMap <- Map.add longId index this.KeyMap
                    index
                |> getReducedFromIndex typ
                |> Some
            | "C" | "W" | "P" when longId[0] = typ[0] ->
                None                
            | s -> failwithf $"{s} is not a valid key type: 'C','W','P' are required for Component, Wire, or Port"

        member this.Reduce (typ: string) (longId:string) =
            this.ReduceID typ longId
            |> Option.defaultValue longId
            

        member this.ReduceSymInfo (si: SymbolInfo) =
            si
            |> Optic.map portOrder_ (Map.map (fun _ lis -> List.map (this.Reduce "P") lis))
            |> Optic.map portOrientation_ (Map.toList >> List.map (fun (s,e) -> this.Reduce "P" s,e) >> Map.ofList)

                

        member this.ReduceComp (comp:Component) =
            let rId = this.ReduceID "C" comp.Id
            let iPOK, iPortL = this.ReducePortL comp.InputPorts
            let oPOK, oPortL = this.ReducePortL comp.OutputPorts
            // if symInfo reduction causes change then either input or output port list will also do this,
            // so we do not need to add symInfo to the match
            let symInfo = Option.map this.ReduceSymInfo comp.SymbolInfo 
            match rId,iPOK,oPOK with
            | None, true, true -> comp
            | _ ->
                { comp with
                    Id = Option.defaultValue comp.Id rId
                    InputPorts = iPortL
                    OutputPorts = oPortL
                    SymbolInfo = symInfo }

        member this.ReducePortOpt (port:Port) =
            let pId = this.ReduceID "P" port.Id
            let hId = this.ReduceID "C" port.HostId
            match pId, hId with
            | None, None -> None
            | None, Some h -> Some {port with HostId = h}
            | Some p, None -> Some {port with Id = p}
            | Some p, Some h -> Some {port with Id=p; HostId = h}

        member this.ReducePort(port:Port) = Option.defaultValue port (this.ReducePortOpt port)

        member this.ReducePortL (portL:Port list) =
                ((true,[]), portL)
                ||> List.fold (fun (noChange, rPortL) port ->
                    match this.ReducePortOpt port with
                    | None -> noChange, (port :: portL)
                    | Some port -> false, (port :: portL))
 
        member this.ReduceConn (conn:Connection) =
            let wId = this.ReduceID "W" conn.Id
            let sPort = this.ReducePortOpt conn.Source
            let tPort = this.ReducePortOpt conn.Target
            match wId, sPort, tPort with
            | None, None, None -> conn
            | _ ->
                let wId' = Option.defaultValue conn.Id wId
                let sPort' = Option.defaultValue conn.Source sPort
                let tPort' = Option.defaultValue conn.Target tPort
                { conn with
                    Id = wId'
                    Source = sPort'
                    Target = tPort'}

        member this.ReduceCanvasState ((comps,conns): CanvasState) =
            List.map this.ReduceComp comps,
            List.map this.ReduceConn conns

        member this.ReduceLDC (ldc:LoadedComponent) = {ldc with CanvasState = this.ReduceCanvasState ldc.CanvasState}

    let compressLDC (name: string) (p:Project) =
        let r = Reducer.Init()
        let updateLdc (ldcs: LoadedComponent list) =
            let n = List.findIndex (fun ldc -> ldc.Name = name) ldcs
            List.updateAt n (r.ReduceLDC ldcs[n]) ldcs
        r.ScanProject p
        Optic.map loadedComponents_ updateLdc p

/// Give every component, port and connection on one sheet a fresh uuid, rewriting every
/// reference to the old ones. Used when a sheet is copied (Import / Duplicate Sheet), and to
/// repair projects in which two sheets were given the same ids by an earlier copy.
/// Ids are unique within a sheet only, but a few places (waveform sheet labels, wire
/// highlighting) assume they are unique across the whole project.
module RegenerateIds =
    open Optics
    open ParameterTypes

    /// old id -> fresh uuid, for every component id, port id and connection id on the sheet.
    /// One map serves all three: uuids are globally unique so the kinds cannot collide.
    let makeIdMap ((comps, conns): CanvasState) : Map<string,string> =
        let compIds = comps |> List.map (fun comp -> comp.Id)
        let portIds = comps |> List.collect (fun comp -> comp.InputPorts @ comp.OutputPorts) |> List.map (fun port -> port.Id)
        let connIds = conns |> List.map (fun conn -> conn.Id)
        compIds @ portIds @ connIds
        |> List.distinct
        |> List.map (fun id -> id, DrawHelpers.uuid ())
        |> Map.ofList

    /// Ids not in the map are left alone: saved sheets can reference ports that no longer exist,
    /// and waveform access paths reference components on other sheets.
    let private sub (idMap: Map<string,string>) (id: string) =
        Map.tryFind id idMap
        |> Option.defaultValue id

    let private remapPort idMap (port: Port) =
        {port with Id = sub idMap port.Id; HostId = sub idMap port.HostId}

    /// PortOrientation is keyed by port id, PortOrder holds port ids in its values.
    let private remapSymbolInfo idMap (info: SymbolInfo) =
        info
        |> Optic.map portOrientation_ (Map.toList >> List.map (fun (id, edge) -> sub idMap id, edge) >> Map.ofList)
        |> Optic.map portOrder_ (Map.map (fun _ ids -> List.map (sub idMap) ids))

    /// ComponentSlotExpr is keyed by ParamSlot, which holds the component id as a string.
    let private remapSlots idMap (slots: ComponentSlotExpr) =
        slots
        |> Map.toList
        |> List.map (fun (slot, expr) -> Optic.map compId_ (sub idMap) slot, expr)
        |> Map.ofList

    let private remapComp idMap (comp: Component) =
        {comp with
            Id = sub idMap comp.Id
            InputPorts = List.map (remapPort idMap) comp.InputPorts
            OutputPorts = List.map (remapPort idMap) comp.OutputPorts
            SymbolInfo = Option.map (remapSymbolInfo idMap) comp.SymbolInfo
            SlotInfo = Option.map (remapSlots idMap) comp.SlotInfo}

    let private remapConn idMap (conn: Connection) =
        {conn with
            Id = sub idMap conn.Id
            Source = remapPort idMap conn.Source
            Target = remapPort idMap conn.Target}

    /// Apply an id map to a canvas: component ids, port ids and host ids, symbol layout maps,
    /// parameter slots, connection ids and endpoints. Geometry and labels are untouched.
    let remapCanvasState (idMap: Map<string,string>) ((comps, conns): CanvasState) : CanvasState =
        List.map (remapComp idMap) comps, List.map (remapConn idMap) conns

    /// An FComponentId is a component id plus the access path of custom components containing it.
    /// Path entries belong to other sheets, so they fall through sub unchanged.
    let private remapFCompId idMap ((ComponentId cid, ap): FComponentId) : FComponentId =
        ComponentId(sub idMap cid), List.map (fun (ComponentId id) -> ComponentId(sub idMap id)) ap

    let private remapWaveInfo idMap (wi: SavedWaveInfo) =
        let remapKeys map = map |> Map.toList |> List.map (fun (k, v) -> remapFCompId idMap k, v) |> Map.ofList
        {wi with
            SelectedWaves = wi.SelectedWaves |> Option.map (List.map (fun wave -> {wave with Id = remapFCompId idMap wave.Id}))
            SelectedFRams = wi.SelectedFRams |> Option.map remapKeys
            SelectedRams =
                wi.SelectedRams
                |> Option.map (Map.toList >> List.map (fun (ComponentId cid, v) -> ComponentId(sub idMap cid), v) >> Map.ofList)
            DisplayedPortIds = wi.DisplayedPortIds |> Option.map (Array.map (sub idMap))}

    /// Regenerate every id on a sheet, including the parameter slots and saved waveform selection
    /// which live on the LoadedComponent rather than in its CanvasState.
    let regenerateSheetIds (ldc: LoadedComponent) : LoadedComponent =
        let idMap = makeIdMap ldc.CanvasState
        {ldc with
            CanvasState = remapCanvasState idMap ldc.CanvasState
            LCParameterSlots = ldc.LCParameterSlots |> Option.map (Optic.map paramSlots_ (remapSlots idMap))
            WaveInfo = ldc.WaveInfo |> Option.map (remapWaveInfo idMap)}

    /// Scan sheets in order. Any sheet reusing a component or connection id already seen on an
    /// earlier sheet has all of its ids regenerated and is marked as needing saving.
    /// Returns the sheets, and the names of those that were changed.
    let correctDuplicateIds (ldcs: LoadedComponent list) : LoadedComponent list * string list =
        let idsOf (ldc: LoadedComponent) =
            let comps, conns = ldc.CanvasState
            List.map (fun (comp: Component) -> comp.Id) comps, List.map (fun (conn: Connection) -> conn.Id) conns
        let step (seenComps, seenConns, sheets, changed) (ldc: LoadedComponent) =
            let compIds, connIds = idsOf ldc
            let clashes =
                List.exists (fun id -> Set.contains id seenComps) compIds ||
                List.exists (fun id -> Set.contains id seenConns) connIds
            let ldc' =
                if clashes then regenerateSheetIds ldc |> Optic.set loadedComponentIsOutOfDate_ true
                else ldc
            let compIds', connIds' = idsOf ldc'
            let changed' = if clashes then ldc'.Name :: changed else changed
            Set.union seenComps (Set.ofList compIds'), Set.union seenConns (Set.ofList connIds'), ldc' :: sheets, changed'
        let _, _, sheets, changed = List.fold step (Set.empty, Set.empty, [], []) ldcs
        List.rev sheets, List.rev changed

//------------------------------------------------------------------------------------//
//---------------------------Low Level Component Helpers------------------------------//
let isInput =
    function
    | Input1 _ -> true
    | _ -> false

let isOutput =
    function
    | Output _ -> true
    | _ -> false

let isViewer =
    function
    | Viewer _ -> true
    | _ -> false

let isCustom =
    function
    | Custom _ -> true
    | _ -> false

let isIOLabel =
    function
    | IOLabel -> true
    | _ -> false

let getCustomName =
    function
    | Custom custom -> custom.Name
    | _ -> failwithf "what? getCustomName should only be called with custom components"

let getCustomComponentType =
    function
    | Custom custom -> custom
    | _ -> failwithf "what? getCustomComponentType should only be called with custom components"
                    
