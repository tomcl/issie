/// Loads Issie project fixtures from disk for tests running under plain dotnet.
/// FilesIO cannot be used here: its module initialisation requires Electron. The
/// few file primitives needed are reimplemented over System.IO instead.
module TestFixtures

open System.IO
open EEExtensions
open CommonTypes
open Helpers.JsonHelpers

/// Directory holding the test fixture projects, located relative to this source file
let fixturesDir =
    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures"))

/// Deserialises the JSON that Fable.SimpleJson writes (the app's .dgm format), which
/// Thoth.Json.Net cannot read: SimpleJson encodes a union case as a string ("User") or
/// a single-property object ({"Input1": [1, 0]}), options as the value or null, and
/// record-keyed maps as arrays of pairs. Driven by reflection so the whole SavedInfo
/// schema, including every ComponentType case, is covered without enumeration.
module SimpleJsonFormat =
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq
    open Microsoft.FSharp.Reflection

    /// Parse without Newtonsoft's automatic date detection, which would corrupt
    /// date-like strings in labels or descriptions
    let parseJson (s: string) : JToken =
        use stringReader = new StringReader(s)
        use jsonReader = new JsonTextReader(stringReader, DateParseHandling = DateParseHandling.None)
        JToken.Load jsonReader

    let rec des (t: System.Type) (tok: JToken) : obj =
        let fail () =
            failwith $"Cannot deserialise JSON {tok.Type} as {t.Name}: {string tok}"
        let genericDef = if t.IsGenericType then Some(t.GetGenericTypeDefinition()) else None
        if t = typeof<string> then
            if tok.Type = JTokenType.Null then null else box (tok.Value<string>())
        elif t = typeof<int> then box (tok.Value<int>())
        elif t = typeof<uint32> then box (tok.Value<uint32>())
        elif t = typeof<float> then box (tok.Value<float>())
        elif t = typeof<bool> then box (tok.Value<bool>())
        elif t = typeof<bigint> then
            box (bigint.Parse(string tok, System.Globalization.CultureInfo.InvariantCulture))
        elif t = typeof<System.DateTime> then
            box (System.DateTime.Parse(string tok, System.Globalization.CultureInfo.InvariantCulture,
                                       System.Globalization.DateTimeStyles.RoundtripKind))
        elif genericDef = Some typedefof<option<_>> then
            let cases = FSharpType.GetUnionCases(t, true)
            let noneCase = cases |> Array.find (fun c -> c.Name = "None")
            let someCase = cases |> Array.find (fun c -> c.Name = "Some")
            if tok.Type = JTokenType.Null then
                FSharpValue.MakeUnion(noneCase, [||], true)
            else
                FSharpValue.MakeUnion(someCase, [| des (t.GetGenericArguments()[0]) tok |], true)
        elif t.IsArray then
            let elemType = t.GetElementType()
            let ja = tok :?> JArray
            let arr = System.Array.CreateInstance(elemType, ja.Count)
            ja |> Seq.iteri (fun i el -> arr.SetValue(des elemType el, i))
            box arr
        elif genericDef = Some typedefof<list<_>> then
            let elemType = t.GetGenericArguments()[0]
            let cases = FSharpType.GetUnionCases(t, true)
            let emptyCase = cases |> Array.find (fun c -> c.Name = "Empty")
            let consCase = cases |> Array.find (fun c -> c.Name = "Cons")
            let elements = (tok :?> JArray) |> Seq.map (des elemType) |> Seq.toList
            (FSharpValue.MakeUnion(emptyCase, [||], true), List.rev elements)
            ||> List.fold (fun tail head -> FSharpValue.MakeUnion(consCase, [| head; tail |], true))
        elif genericDef = Some typedefof<Map<_, _>> then
            let keyType, valType = t.GetGenericArguments()[0], t.GetGenericArguments()[1]
            let tupleType = FSharpType.MakeTupleType [| keyType; valType |]
            let pairs =
                match tok with
                | :? JObject as jo ->
                    // string-representable keys: the key is the property name
                    jo.Properties()
                    |> Seq.map (fun p ->
                        let key =
                            if keyType = typeof<string> then box p.Name
                            else des keyType (new JValue(p.Name))
                        FSharpValue.MakeTuple([| key; des valType p.Value |], tupleType))
                | :? JArray as ja ->
                    // structural keys: an array of [key, value] pairs
                    ja
                    |> Seq.map (fun pair ->
                        let pair = pair :?> JArray
                        FSharpValue.MakeTuple([| des keyType pair[0]; des valType pair[1] |], tupleType))
                | _ -> fail ()
            let typedPairs = System.Array.CreateInstance(tupleType, Seq.length pairs)
            pairs |> Seq.iteri (fun i pair -> typedPairs.SetValue(pair, i))
            System.Activator.CreateInstance(t, typedPairs)
        elif FSharpType.IsTuple t then
            let elemTypes = FSharpType.GetTupleElements t
            let ja = tok :?> JArray
            FSharpValue.MakeTuple(elemTypes |> Array.mapi (fun i et -> des et ja[i]), t)
        elif FSharpType.IsUnion(t, true) then
            let cases = FSharpType.GetUnionCases(t, true)
            // Fable erases a single-case single-field union to its payload, and SimpleJson
            // writes the erased form (e.g. ComponentId as a bare string)
            let erasedCase =
                match cases with
                | [| single |] when single.GetFields().Length = 1 ->
                    let isTaggedObject =
                        tok.Type = JTokenType.Object
                        && (tok :?> JObject).Properties() |> Seq.exists (fun p -> p.Name = single.Name)
                    if isTaggedObject then None else Some single
                | _ -> None
            match erasedCase with
            | Some single ->
                FSharpValue.MakeUnion(single, [| des (single.GetFields()[0]).PropertyType tok |], true)
            | None ->
                let findCase (name: string) =
                    // case-insensitive: some old files hold lowercase case names (e.g. gate
                    // "or"), and F# forbids two cases differing only in capitalisation
                    match cases |> Array.tryFind (fun c -> System.String.Equals(c.Name, name, System.StringComparison.OrdinalIgnoreCase)) with
                    | Some case -> case
                    | None -> failwith $"Union {t.Name} has no case '{name}'"
                match tok.Type with
                | JTokenType.String ->
                    FSharpValue.MakeUnion(findCase (tok.Value<string>()), [||], true)
                | JTokenType.Object ->
                    let prop = (tok :?> JObject).Properties() |> Seq.exactlyOne
                    let case = findCase prop.Name
                    let fields = case.GetFields()
                    let args =
                        match fields with
                        | [| field |] -> [| des field.PropertyType prop.Value |]
                        | fields ->
                            let ja = prop.Value :?> JArray
                            fields |> Array.mapi (fun i f -> des f.PropertyType ja[i])
                    FSharpValue.MakeUnion(case, args, true)
                | _ -> fail ()
        elif FSharpType.IsRecord(t, true) then
            let jo = tok :?> JObject
            let values =
                FSharpType.GetRecordFields(t, true)
                |> Array.map (fun field ->
                    match jo.TryGetValue field.Name with
                    | true, v -> des field.PropertyType v
                    | false, _ when field.PropertyType.IsGenericType
                                    && field.PropertyType.GetGenericTypeDefinition() = typedefof<option<_>> ->
                        // field added since this file was saved: default an option to None
                        des field.PropertyType (JValue.CreateNull())
                    | false, _ -> failwith $"Record {t.Name} is missing field {field.Name}")
            FSharpValue.MakeRecord(t, values, true)
        else
            fail ()

    let deserialise<'T> (json: string) : 'T =
        des typeof<'T> (parseJson json) :?> 'T

// --- .ram parsing ---
// FilesIO.readMemLines is the production parser and is pure, so it is used directly rather
// than mirrored here. Only the file read has to be done separately: FilesIO does that
// through node, which is not available under .NET.

let private readMemLines (addressWidth: int) (wordWidth: int) (lines: string array) =
    FilesIO.readMemLines addressWidth wordWidth lines

/// Reload a memory component's contents from its .ram file, as the app does on project load
let private initialiseMemory (projectPath: string) (comp: Component) =
    match comp.Type with
    | RAM1 mem
    | ROM1 mem
    | AsyncROM1 mem
    | AsyncRAM1 mem ->
        match mem.Init with
        | FromFile name ->
            let fPath = Path.Combine(projectPath, name + ".ram")
            match readMemLines mem.AddressWidth mem.WordWidth (File.ReadAllLines fPath) with
            | Ok defns ->
                let data = defns |> Array.map (fun (addr, dat, _) -> addr, dat) |> Map.ofArray
                let comments =
                    defns
                    |> Array.choose (fun (addr, _, c) -> c |> Option.map (fun c -> addr, c))
                    |> Map.ofArray
                let mem =
                    { mem with
                        Data = data
                        Comments = (if Map.isEmpty comments then None else Some comments) }
                { comp with Type = getMemType comp.Type mem }
            | Error msg -> failwith $"Cannot parse {fPath}: {msg}"
        | _ -> comp
    | _ -> comp

/// Upgrade legacy component types, mirroring FilesIO.getLatestComp
let private getLatestComp (comp: Component) =
    let updateMem (mem: Memory) : Memory1 =
        { Init = FromData
          Data = mem.Data
          AddressWidth = mem.AddressWidth
          WordWidth = mem.WordWidth
          Comments = None }
    match comp.Type with
    | RAM mem -> { comp with Type = RAM1(updateMem mem) }
    | ROM mem -> { comp with Type = ROM1(updateMem mem) }
    | AsyncROM mem -> { comp with Type = AsyncROM1(updateMem mem) }
    | Constant(width, cVal) -> { comp with Type = Constant1(width, cVal, $"%A{cVal}") }
    | Input width -> { comp with Type = Input1(width, None) }
    | _ -> comp

/// Load one .dgm file as a LoadedComponent, mirroring the app's loader
let loadLoadedComponent (filePath: string) : LoadedComponent =
    let savedInfo =
        try
            SimpleJsonFormat.deserialise<SavedInfo> (File.ReadAllText filePath)
        with e ->
            failwith $"Cannot parse {filePath}: {e.Message}"
    let jsonComps, conns = savedInfo.getCanvas
    let comps = jsonComps |> List.map (convertFromJSONComponent >> getLatestComp)
    let canvas = comps |> List.map (initialiseMemory (Path.GetDirectoryName filePath)), conns
    let inputs, outputs = CanvasExtractor.parseDiagramSignature canvas
    let sheetInfo = savedInfo.getSheetInfo
    { Name = Path.GetFileNameWithoutExtension filePath
      TimeStamp = savedInfo.getTimeStamp
      FilePath = filePath
      WaveInfo = savedInfo.getWaveInfo
      CanvasState = canvas
      InputLabels = inputs
      OutputLabels = outputs
      LCParameterSlots = sheetInfo |> Option.bind (fun si -> si.ParameterDefinitions)
      Form = (match sheetInfo with Some si -> si.Form | None -> Some User)
      LoadedComponentIsOutOfDate = false
      IsTopSheet = sheetInfo |> Option.bind (fun si -> si.IsTopSheet) |> Option.defaultValue false
      Description = sheetInfo |> Option.bind (fun si -> si.Description) }

/// Load every sheet of a fixture project directory
let loadProject (projectName: string) : LoadedComponent list =
    Directory.GetFiles(Path.Combine(fixturesDir, projectName), "*.dgm")
    |> Array.toList
    |> List.map loadLoadedComponent
