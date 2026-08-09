(*
    SimpleJsonDotNet.fs

    Reads, outside Fable, the JSON that Fable.SimpleJson writes.
*)

/// Deserialises the JSON encoding used by Fable.SimpleJson, which is what the app writes into
/// every .dgm file. Without this, nothing running under .NET can open a sheet, because
/// Thoth.Json.Net - what `Helpers.jsonStringToState` otherwise uses there - cannot read what the
/// app wrote. The two encodings disagree about unions (Thoth writes an array; SimpleJson writes a
/// bare string for a nullary case and a single-property object otherwise), about options (Thoth
/// wraps the value; SimpleJson writes the value itself, or null) and about maps with structural
/// keys (SimpleJson writes an array of pairs).
///
/// Rather than enumerate the `SavedInfo` schema - which would mean every `ComponentType` case, and
/// a new one every time a component is added - this walks the target type by reflection, so it
/// covers whatever the types say.
///
/// Fable never sees any of this: under Fable the app calls SimpleJson itself, and Newtonsoft is not
/// available. Under .NET it reaches this project transitively through Thoth.Json.Net.
module SimpleJsonDotNet

#if !FABLE_COMPILER

open System.IO
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Microsoft.FSharp.Reflection

/// Parse without Newtonsoft's automatic date detection, which would otherwise rewrite date-like
/// strings appearing in labels or descriptions
let private parseJson (s: string) : JToken =
    use stringReader = new StringReader(s)
    use jsonReader = new JsonTextReader(stringReader, DateParseHandling = DateParseHandling.None)
    JToken.Load jsonReader

let rec private des (t: System.Type) (tok: JToken) : obj =
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
    elif t = typeof<obj> then
        // asked to read a value whose type is not known - SavedCanvasUnknownWaveInfo, the last
        // resort for a file holding wave information in a shape this version cannot name. The
        // caller only wants the rest of the file, so hand back the token unread.
        box tok
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
        // Fable erases a single-case single-field union to its payload, and SimpleJson writes the
        // erased form (e.g. ComponentId as a bare string)
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
                // case-insensitive: some old files hold lowercase case names (e.g. gate "or"), and
                // F# forbids two cases differing only in capitalisation
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

/// Read json as a 'T, or say why it is not one. Failure is an ordinary outcome here, not a bug:
/// jsonStringToState works out which of three formats a sheet file is in by trying each in turn.
let tryDeserialise<'T> (json: string) : Result<'T, string> =
    try
        Ok(des typeof<'T> (parseJson json) :?> 'T)
    with e ->
        Error e.Message

#endif
