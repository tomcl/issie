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

        type JSONCanvasState = JSONComponent.Component list * JSONComponent.Connection list

        /// A saved sheet with its wave info left UNREAD: 'T is instantiated at obj, which parses
        /// anything. This is the last resort of jsonStringToState, and it is what lets the saved
        /// selection change shape without a legacy reader - a selection this version cannot
        /// understand costs the selection, never the sheet.
        ///
        /// Every variant that can carry wave info is mirrored, because any of them can be the one
        /// that fails. Only the newest was, which meant an older save holding an unreadable
        /// selection failed to load at all rather than losing it.
        type SavedCanvasUnknownWaveInfo<'T> =
            | CanvasWithFileWaveInfo of LegacyCanvasState * 'T option * System.DateTime
            | CanvasWithFileWaveInfoAndNewConns of LegacyCanvasState * 'T option * System.DateTime
            | NewCanvasWithFileWaveInfoAndNewConns of JSONCanvasState * 'T option * System.DateTime
            | NewCanvasWithFileWaveSheetInfoAndNewConns of JSONCanvasState * 'T option * JSONWave.SheetInfo option * System.DateTime

        // Every type below is the FILE form: ids are strings there (uuids in old files, integers
        // written as strings in new ones), and become the in-memory integer ids only through
        // sheetOfJson at the end of this file - which is where a uuid gets its integer.
        type SavedInfo =
            | CanvasOnly of LegacyCanvasState
            | CanvasWithFileWaveInfo of LegacyCanvasState * JSONWave.SavedWaveInfo option * System.DateTime
            | CanvasWithFileWaveInfoAndNewConns of LegacyCanvasState * JSONWave.SavedWaveInfo option * System.DateTime
            | NewCanvasWithFileWaveInfoAndNewConns of JSONCanvasState * JSONWave.SavedWaveInfo option * System.DateTime
            | NewCanvasWithFileWaveSheetInfoAndNewConns of JSONCanvasState * JSONWave.SavedWaveInfo option * JSONWave.SheetInfo option * System.DateTime
            
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

        /// The same save with its wave info discarded. The cases are qualified because SavedInfo
        /// names its own the same way, and being able to write them side by side is the point.
        let private withWaveInfoDropped (saved: SavedCanvasUnknownWaveInfo<obj>) : SavedInfo =
            match saved with
            | SavedCanvasUnknownWaveInfo.CanvasWithFileWaveInfo(cState, _, time) ->
                CanvasWithFileWaveInfo(cState, None, time)
            | SavedCanvasUnknownWaveInfo.CanvasWithFileWaveInfoAndNewConns(cState, _, time) ->
                CanvasWithFileWaveInfoAndNewConns(cState, None, time)
            | SavedCanvasUnknownWaveInfo.NewCanvasWithFileWaveInfoAndNewConns(cState, _, time) ->
                NewCanvasWithFileWaveInfoAndNewConns(cState, None, time)
            | SavedCanvasUnknownWaveInfo.NewCanvasWithFileWaveSheetInfoAndNewConns(cState, _, sheetInfo, time) ->
                NewCanvasWithFileWaveSheetInfoAndNewConns(cState, None, sheetInfo, time)

        let extraCoder =
            Extra.empty
            |> Extra.withInt64
            |> Extra.withUInt64
            |> Extra.withBigInt
            |> Extra.withCustom CommonTypes.componentIdEncoder CommonTypes.componentIdDecoder

        /// converts a live CanvasState to its file form: legacy-compatible component types, and
        /// every integer id written as a decimal string.
        let convStateToJC (compL, connL) : JSONCanvasState =
            List.map convertToJSONComponent compL, List.map convertToJSONConnection connL

        /// Code to convert a CanvasState to a JSON string, does not work for bigints (I think).
        /// A serialisation failure is an Error: it must never be written to the sheet's
        /// .dgm file, which would overwrite the sheet with garbage.
        let stateToJsonStringAt (time: System.DateTime) (cState: CanvasState, waveInfo: SavedWaveInfo option, sheetInfo: SheetInfo option) : Result<string,string> =
            try
                 let savedInfo =
                     NewCanvasWithFileWaveSheetInfoAndNewConns (
                         convStateToJC cState,
                         Option.map waveInfoToJson waveInfo,
                         Option.map sheetInfoToJson sheetInfo,
                         time)
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
        /// The ordinary save: stamped with the time it is written, which is what says which sheet
        /// the user was last working on - see MenuHelpers.chooseWhichToOpen. A rewrite that is not
        /// an edit of the user's, such as the id conversion a project open does, keeps the stamp it
        /// read and goes through stateToJsonStringAt instead.
        let stateToJsonString (state: CanvasState * SavedWaveInfo option * SheetInfo option) : Result<string,string> =
            stateToJsonStringAt System.DateTime.Now state

        /// Code to convert a CanvasState to a JSON string, allowing bigints
        let stateToJsonStringExperimental (cState: CanvasState, waveInfo: SavedWaveInfo option, sheetInfo: SheetInfo option) : Result<string,string> =
            let time = System.DateTime.Now
            try
                Encode.Auto.toString(space = 0, value = (NewCanvasWithFileWaveSheetInfoAndNewConns (convStateToJC cState, Option.map waveInfoToJson waveInfo, Option.map sheetInfoToJson sheetInfo, time)), extra = extraCoder)
                |> Ok
            with
            | e ->
                Error $"JSON serialisation of the sheet failed, so it was not saved: {e.Message}"

        #if !FABLE_COMPILER
        /// Read one of the sheet formats under .NET, where SimpleJson - which is what the app uses,
        /// and so what wrote almost every .dgm in existence - cannot run. SimpleJsonDotNet reads
        /// those; Thoth is kept as a second attempt because stateToJsonString writes with Thoth
        /// when it is itself running under .NET, so a sheet written by the tests is in that format.
        let inline private decodeSaved< ^T> (jsonString: string) : Result< ^T, string> =
            match SimpleJsonDotNet.tryDeserialise< ^T> jsonString with
            | Ok state -> Ok state
            | Error simpleJsonMsg ->
                match Decode.Auto.fromString< ^T>(jsonString, extra = extraCoder) with
                | Ok state -> Ok state
                | Error thothMsg -> Error $"not SimpleJson ({simpleJsonMsg}); not Thoth ({thothMsg})"
        #endif

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
                        | Ok saved -> Ok(withWaveInfoDropped saved)
                        | Error str -> 
                            Log.error $"could not parse saved JSON ({jsonString.Length} chars): {str}"
                            Error str)
            #else
            match decodeSaved<LegacyCanvasState> jsonString with
            | Ok state -> Ok (CanvasOnly state)
            | Error _ ->
                match decodeSaved<SavedInfo> jsonString with
                | Ok state -> Ok state
                | Error str ->
                    match decodeSaved<SavedCanvasUnknownWaveInfo<obj>> jsonString with
                    | Ok saved -> Ok(withWaveInfoDropped saved)
                    | Error str ->
                        Log.error $"could not parse saved JSON ({jsonString.Length} chars): {str}"
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

/// Return a memoized version of funcToMemoize whose stored result is reused for as long as it is
/// called with the SAME OBJECT, by reference, rather than an equal one.
///
/// memoizeBy compares keys with =, which is what you want for a small key and quite wrong for a
/// large one: on a Map, a FastSimulation or anything else with a deep structure the comparison
/// costs more than the function being memoised, and on a value holding a closure it does not
/// terminate sensibly at all. Identity is the right question for anything that is REBUILT rather
/// than mutated - a simulation, or a map replaced wholesale when its contents change - because
/// then a new object is exactly the signal that the answer is stale. It also means nothing has to
/// remember to invalidate the memo.
///
/// One slot, like memoizeBy: these are used where the argument changes rarely and is asked about
/// often, so alternating between two arguments would defeat it.
///
/// A filled slot retains its KEY as well as its value, and every one of these is keyed on a whole
/// simulation or on a map of every waveform in one. That is fine while simulations replace one
/// another - a new key drops the old - but not when one is ENDED and nothing replaces it, which is
/// the retention removeAllSimulationsFromModel exists to prevent. So each memo registers a way to
/// empty itself: see clearIdentityMemos. Registration happens where memoizeByIdentity is APPLIED,
/// so apply it once, in a top-level binding, and not inside a function that is called repeatedly.
let private identityMemos: ResizeArray<unit -> unit> = ResizeArray()

let memoizeByIdentity (funcToMemoize: 'a -> 'b) : 'a -> 'b =
    let mutable last: ('a * 'b) option = None
    identityMemos.Add(fun () -> last <- None)
    fun (a: 'a) ->
        match last with
        | Some(key, value) when System.Object.ReferenceEquals(key, a) -> value
        | _ ->
            let value = funcToMemoize a
            last <- Some(a, value)
            value

/// Empty every memoizeByIdentity memo, releasing whatever their keys and values hold on to.
///
/// Call where something large is discarded rather than replaced - a simulation being ended. Doing
/// so is always safe: a memo is only ever an answer that can be worked out again, so the cost of
/// emptying one that was still wanted is that it is filled again on the next call.
let clearIdentityMemos () = identityMemos |> Seq.iter (fun clear -> clear ())

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

    /// Crop a string to first n chars
    let crop (s:string) =
        sprintInitial 3 s

    /// print a component simply
    let pComponent (comp: Component) =
        let inPorts =
            comp.InputPorts
            |> List.map (fun p -> string p.Id)
        let outPorts =
            comp.OutputPorts
            |> List.map (fun p -> string p.Id)
        $"|{comp.Label}:{comp.Type} PIN={inPorts} POut={outPorts}|"

    /// Print a connection simply
    let pConnection (conn: Connection) =
        $"{conn.Source.Id}->{conn.Target.Id}"

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

// ---------------------------------------------------------------------------------------------
// Integer canvas ids: allocation, admission, remapping, and the file boundary.
//
// Component ids are unique across the whole DESIGN, allocated densely from 1 - the one id
// namespace with a global invariant, and the density is what lets a design's components index
// arrays directly. Port and connection ids need only be unique WITHIN their sheet: nothing
// resolves either outside the sheet it belongs to. They are minted from design-lifetime
// allocators all the same - over-uniqueness is harmless and saves threading sheet context into
// the draw block - but admission enforces only the per-sheet invariant for them.
// Ids 0 and below are sentinels and are never allocated.
// ---------------------------------------------------------------------------------------------

/// The uuid generator's replacement: integer id allocation, lowest unallocated first.
module IdAllocator =

    /// Which integers are taken, one flag per value: assignment is a first-zero scan from a
    /// cursor that only advances, so ids come out dense and seeding is one pass.
    type Allocator = { mutable Used: uint32[]; mutable Cursor: int }

    /// Allocators are cheap: sheet-scoped ones are made, seeded and dropped per operation.
    let makeAllocator (initialSize: int) : Allocator =
        { Used = Array.zeroCreate (max 16 initialSize); Cursor = 1 }

    let private ensureSize (alloc: Allocator) (index: int) =
        if index >= alloc.Used.Length then
            let bigger = Array.zeroCreate (max (2 * alloc.Used.Length) (index + 1))
            Array.blit alloc.Used 0 bigger 0 alloc.Used.Length
            alloc.Used <- bigger

    /// Mark an id as taken. Sentinels (0 and below) are ignored.
    let reserve (alloc: Allocator) (id: int) =
        if id > 0 then
            ensureSize alloc id
            alloc.Used[id] <- 1u

    let isUsed (alloc: Allocator) (id: int) =
        id > 0 && id < alloc.Used.Length && alloc.Used[id] <> 0u

    /// The smallest unused positive integer, marked taken as it is handed out.
    let next (alloc: Allocator) : int =
        let mutable i = max 1 alloc.Cursor

        while i < alloc.Used.Length && alloc.Used[i] <> 0u do
            i <- i + 1

        ensureSize alloc i
        alloc.Used[i] <- 1u
        alloc.Cursor <- i + 1
        i

    let reset (alloc: Allocator) =
        Array.fill alloc.Used 0 alloc.Used.Length 0u
        alloc.Cursor <- 1

    // The three design-lifetime allocators, one per id kind. Global mutable state encapsulated
    // behind the functions below (docs/mutableState.md): RegenerateIds.admitDesign resets and
    // re-seeds them when a project opens, so nothing leaks from one project to the next.
    let private components = makeAllocator 10_000
    let private ports = makeAllocator 40_000
    let private connections = makeAllocator 20_000

    let resetAll () =
        reset components
        reset ports
        reset connections

    /// A fresh component id, unique across the open design.
    let newComponentId () = ComponentId(next components)

    /// A fresh port id. Unique across the design in practice (one allocator serves every
    /// sheet), though only uniqueness within a sheet is required of it.
    let newPortId () = PortId(next ports)

    /// A fresh connection id; as newPortId, over-unique by construction.
    let newConnectionId () = ConnectionId(next connections)

    let reserveComponentId (ComponentId id) = reserve components id
    let reservePortId (PortId id) = reserve ports id
    let reserveConnectionId (ConnectionId id) = reserve connections id
    let componentIdUsed (ComponentId id) = isUsed components id

/// Rewriting the ids a sheet holds - when a sheet is copied into a project, and when a loaded
/// sheet breaks an id invariant.
module RegenerateIds =
    open Optics
    open ParameterTypes

    /// Ids not in a map are left alone: saved sheets can reference ports that no longer exist,
    /// and waveform access paths reference components on other sheets.
    let inline private sub (idMap: Map<'id, 'id>) (id: 'id) =
        Map.tryFind id idMap |> Option.defaultValue id

    let private remapPort compMap portMap (port: Port) =
        { port with Id = sub portMap port.Id; HostId = sub compMap port.HostId }

    /// PortOrientation is keyed by port id, PortOrder holds port ids in its values.
    let private remapSymbolInfo portMap (info: SymbolInfo) =
        info
        |> Optic.map portOrientation_ (Map.toList >> List.map (fun (id, edge) -> sub portMap id, edge) >> Map.ofList)
        |> Optic.map portOrder_ (Map.map (fun _ ids -> List.map (sub portMap) ids))

    /// ComponentSlotExpr is keyed by ParamSlot, which holds the component id.
    let private remapSlots (compMap: Map<ComponentId, ComponentId>) (slots: ComponentSlotExpr) =
        slots
        |> Map.toList
        |> List.map (fun (slot, expr) -> Optic.map compId_ (sub compMap) slot, expr)
        |> Map.ofList

    let private remapComp compMap portMap (comp: Component) =
        { comp with
            Id = sub compMap comp.Id
            InputPorts = List.map (remapPort compMap portMap) comp.InputPorts
            OutputPorts = List.map (remapPort compMap portMap) comp.OutputPorts
            SymbolInfo = Option.map (remapSymbolInfo portMap) comp.SymbolInfo
            SlotInfo = Option.map (remapSlots compMap) comp.SlotInfo }

    let private remapConn compMap portMap connMap (conn: Connection) =
        { conn with
            Id = sub connMap conn.Id
            Source = remapPort compMap portMap conn.Source
            Target = remapPort compMap portMap conn.Target }

    /// Apply id maps - component, port and connection ids are separate namespaces, so separate
    /// maps - to a canvas: ids, host ids, symbol layout maps, parameter slots, endpoints.
    let remapCanvasState compMap portMap connMap ((comps, conns): CanvasState) : CanvasState =
        List.map (remapComp compMap portMap) comps, List.map (remapConn compMap portMap connMap) conns

    /// The saved selection holds no component ids - it is label paths (see WavePath.fs) - so
    /// renumbering a sheet leaves it alone. Only the legacy SelectedRams field, which nothing
    /// writes and nothing reads, still names this sheet's components by id.
    ///
    /// This used to remap the selection, and had to be careful about it: only the FIRST access
    /// path entry lives on the sheet being remapped, so an indiscriminate map would corrupt the
    /// deeper entries, whose per-sheet integer ids can coincide with this sheet's. That care is
    /// what a label path makes unnecessary rather than merely correct.
    let private remapWaveInfo compMap (wi: SavedWaveInfo) =
        { wi with
            SelectedRams =
                wi.SelectedRams
                |> Option.map (Map.toList >> List.map (fun (cid, v) -> sub compMap cid, v) >> Map.ofList) }

    /// Apply id maps to everything a LoadedComponent holds: the canvas, the parameter slots and
    /// the saved waveform selection, which live on the LoadedComponent rather than in its
    /// CanvasState.
    let remapLoadedComponent compMap portMap connMap (ldc: LoadedComponent) : LoadedComponent =
        { ldc with
            CanvasState = remapCanvasState compMap portMap connMap ldc.CanvasState
            LCParameterSlots = ldc.LCParameterSlots |> Option.map (Optic.map paramSlots_ (remapSlots compMap))
            WaveInfo = ldc.WaveInfo |> Option.map (remapWaveInfo compMap) }

    /// Give every component, port and connection on a sheet fresh ids from the design's
    /// allocators, rewriting every reference. Used when a sheet is copied into a project:
    /// Import / Duplicate Sheet, and library component materialisation.
    let regenerateSheetIds (ldc: LoadedComponent) : LoadedComponent =
        let comps, conns = ldc.CanvasState

        let freshFor mint ids =
            ids |> List.distinct |> List.map (fun id -> id, mint ()) |> Map.ofList

        let compMap = comps |> List.map (fun comp -> comp.Id) |> freshFor IdAllocator.newComponentId

        let portMap =
            comps
            |> List.collect (fun comp -> comp.InputPorts @ comp.OutputPorts)
            |> List.map (fun port -> port.Id)
            |> freshFor IdAllocator.newPortId

        let connMap = conns |> List.map (fun conn -> conn.Id) |> freshFor IdAllocator.newConnectionId
        remapLoadedComponent compMap portMap connMap ldc

    /// Admit a sheet into the open design, re-minting what breaks an invariant: a component id
    /// already used elsewhere in the design (component ids are design-unique), or any id that
    /// is a sentinel or duplicated within the sheet (per-sheet uniqueness is every kind's
    /// floor). Clean ids are reserved so later minting cannot collide with them. Returns the
    /// sheet and whether anything had to change.
    let admitSheet (ldc: LoadedComponent) : LoadedComponent * bool =
        let comps, conns = ldc.CanvasState
        let compIds = comps |> List.map (fun comp -> comp.Id)

        let portIds =
            comps
            |> List.collect (fun comp -> comp.InputPorts @ comp.OutputPorts)
            |> List.map (fun port -> port.Id)

        let connIds = conns |> List.map (fun conn -> conn.Id)

        /// A sentinel or an internal duplicate. `value` reads the integer inside whichever
        /// kind of id this is: the sentinel check is about the number, the duplicate check is
        /// not.
        let inline broken value ids =
            List.exists (fun id -> value id <= 0) ids
            || List.length (List.distinct ids) <> List.length ids

        if broken cToInt compIds
           || broken pToInt portIds
           || broken (fun (ConnectionId n) -> n) connIds then
            // a sentinel or an internal duplicate: a malformed sheet - renumber it wholesale
            regenerateSheetIds ldc, true
        else
            portIds |> List.iter IdAllocator.reservePortId
            connIds |> List.iter IdAllocator.reserveConnectionId

            let compMap =
                compIds
                |> List.choose (fun id ->
                    if IdAllocator.componentIdUsed id then
                        Some(id, IdAllocator.newComponentId ())
                    else
                        IdAllocator.reserveComponentId id
                        None)
                |> Map.ofList

            if Map.isEmpty compMap then
                ldc, false
            else
                remapLoadedComponent compMap Map.empty Map.empty ldc, true

    /// Open a design: reset the allocators and admit every sheet in read order, so the same
    /// design gets the same ids each time it is opened. Returns the sheets and the names of any
    /// whose ids had to change.
    let admitDesign (ldcs: LoadedComponent list) : LoadedComponent list * string list =
        IdAllocator.resetAll ()
        let admitted = ldcs |> List.map admitSheet

        admitted |> List.map fst,
        admitted |> List.choose (fun (ldc, changed) -> if changed then Some ldc.Name else None)

// ---------------------------------------------------------------------------------------------
// The file boundary: string ids in saved .dgm JSON become the integer ids everything in memory
// uses. An id already written as an integer keeps its value; anything else - a uuid, in files
// from before ids were integers - is allocated the first free integer in its namespace, per
// kind, per sheet. Design-wide component uniqueness is not settled here: admitDesign does that
// when the sheets join a project.
// ---------------------------------------------------------------------------------------------

/// The integer a saved id string stands for, if it is one our saves write: a positive decimal
/// of at most 9 digits with no leading zero ("042" would alias "42" once parsed, so it does
/// not count as an integer id and is re-allocated instead).
let private tryIdInt (s: string) : int option =
    if s.Length >= 1 && s.Length <= 9 && s[0] <> '0' && String.forall System.Char.IsDigit s then
        Some(int s)
    else
        None

/// Whether a saved sheet's ids are in the OLD form: uuids, written before ids were integers.
///
/// Loading such a sheet allocates integers for them, so what is then in memory is not what is on
/// disk - deterministically, so nothing is wrong with it, but the file keeps its uuids until
/// something writes it. Opening a project that can be written is where that is settled: see
/// MenuHelpers.convertProjectIdsOnDisk. One old id anywhere in the sheet is enough, since the
/// whole sheet is rewritten either way.
let jsonCanvasHasOldIds ((comps, conns): JSONCanvasState) =
    let isOld (id: string) = (tryIdInt id).IsNone

    comps
    |> List.exists (fun comp ->
        isOld comp.Id
        || (comp.InputPorts @ comp.OutputPorts |> List.exists (fun port -> isOld port.Id)))
    || conns |> List.exists (fun conn -> isOld conn.Id)

/// Convert one parsed sheet to in-memory form. The wave info needs no id mapping of its own: a
/// saved selection is label paths (see WavePath.fs). The mapping passed to waveInfoOfJson is for
/// the legacy SelectedRams field alone.
let sheetOfJson
    (canvas: JSONCanvasState)
    (waveInfo: JSONWave.SavedWaveInfo option)
    (sheetInfo: JSONWave.SheetInfo option)
    : CanvasState * SavedWaveInfo option * SheetInfo option =
    let jsonComps, jsonConns = canvas

    // one namespace's mapping: parseable ids keep their values and are reserved; the rest get
    // the first free integers; a string not on the sheet at all maps to the 0 sentinel
    let makeMapping (ids: string list) : string -> int =
        let ids = List.distinct ids
        let alloc = IdAllocator.makeAllocator (List.length ids + 1)
        ids |> List.iter (fun id -> tryIdInt id |> Option.iter (IdAllocator.reserve alloc))

        let assigned =
            ids
            |> List.choose (fun id ->
                match tryIdInt id with
                | Some _ -> None
                | None -> Some(id, IdAllocator.next alloc))
            |> Map.ofList

        fun id ->
            match tryIdInt id with
            | Some n -> n
            | None -> Map.tryFind id assigned |> Option.defaultValue 0

    let mapCompIdInt = jsonComps |> List.map (fun comp -> comp.Id) |> makeMapping
    let mapCompId = mapCompIdInt >> ComponentId

    let mapPortIdInt =
        jsonComps
        |> List.collect (fun comp -> comp.InputPorts @ comp.OutputPorts)
        |> List.map (fun port -> port.Id)
        |> makeMapping

    let mapPortId = mapPortIdInt >> PortId

    let mapConnId = (jsonConns |> List.map (fun conn -> conn.Id) |> makeMapping) >> ConnectionId

    let comps = jsonComps |> List.map (convertFromJSONComponent mapCompId mapPortId)
    let conns = jsonConns |> List.map (convertFromJSONConnection mapConnId mapCompId mapPortId)

    (comps, conns), waveInfo |> Option.map (waveInfoOfJson mapCompId), sheetInfo |> Option.map (sheetInfoOfJson mapCompId)

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
                    
