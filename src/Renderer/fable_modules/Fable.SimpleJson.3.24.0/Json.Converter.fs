namespace Fable.SimpleJson

open System
open Fable.Core
open FSharp.Reflection
open System.Numerics
open System.Collections
open System.Collections.Generic
open Fable.Core.JsInterop

module Node =

    /// Converts Base64 string into a byte array in Node environment
    [<Emit("Array.prototype.slice.call(Buffer.from($0, 'base64'))")>]
    let bytesFromBase64 (value: string) : byte array = jsNative

module Convert =
    let internal isBrowser () : bool = importDefault "./isBrowser.js"

    let insideBrowser = isBrowser()

    [<Emit "($0 === undefined)">]
    let private isUndefined (value: obj) : bool = jsNative
    let private isDefined (value: obj) : bool = not (isUndefined value)

    /// <summary>Uses internal representation of F# maps to determine whether we are using Fable 3 or not</summary>
    let usingFable3() =
        #if FABLE_COMPILER_3
        true
        #else
        let map = JS.JSON.parse(JS.JSON.stringify (Map.ofList [ 1, 1; 2, 2 ]))
        let tree = get "tree" map
        isDefined tree && isDefined (get "k" tree) && isDefined (get "v" tree) && isDefined (get "h" tree)
        #endif

    let isUsingFable3 = usingFable3()

    [<Emit("typeof WorkerGlobalScope !== 'undefined' && self instanceof WorkerGlobalScope")>]
    let internal insideWorker :  bool = jsNative

    [<Emit("$0[$1] = $2")>]
    let internal setProp o k v = jsNative

    type InternalMap =
        | MapEmpty
        | MapOne of string * Json
        | MapNode of string * Json * InternalMap * InternalMap

    let rec flattenMap = function
        | MapEmpty -> [ ]
        | MapOne (key, value) -> [ key, value ]
        | MapNode (key, value, left, right) ->
            [ yield! flattenMap left
              yield! flattenMap right
              yield  (key, value) ]

    let (|KeyValue|_|) key (map: Map<string, Json>) =
        map
        |> Map.tryFind key
        |> Option.map (fun value -> key, value, Map.remove key map)

    let (|NonArray|_|) = function
        | JArray _ -> None
        | json -> Some json

    let (|MapEmpty|_|) json =
        match json with
        | JString "MapEmpty" -> Some json
        | _ -> None

    let (|MapKey|_|) = function
        | JNumber number -> Some (string number)
        | JString key -> Some key
        | _ -> None

    let (|MapOne|_|) = function
        | JArray [ JString "MapOne"; MapKey key; value ] -> Some (key, value)
        | _ -> None

    let (|MapNode|_|) = function
        | JArray [ JString "MapNode"; MapKey key; value; left; right; JNumber _  ] ->
            Some (key, value, left, right)
        | _ -> None

    let rec generateMap json =
        match json with
        | MapEmpty _ -> Some InternalMap.MapEmpty
        | MapOne (key, value) -> Some (InternalMap.MapOne (key, value))
        | MapNode (key, value, left, right) ->
            match generateMap left, generateMap right with
            | Some leftMap, Some rightMap ->
                Some (InternalMap.MapNode(key, value, leftMap, rightMap))
            | _ -> None
        | _ -> None

    let rec flatteFable3Map (tree: Map<string, Json>) =
        [
            match Map.tryFind "k" tree, Map.tryFind "v" tree with
            | Some (JString key), Some value -> (key, value)
            | _ -> ()

            match Map.tryFind "left" tree with
            | Some (JObject left) -> yield! flatteFable3Map left
            | _ -> ()

            match Map.tryFind "right" tree with
            | Some (JObject right) -> yield! flatteFable3Map right
            | _ -> ()
        ]

    let rec flattenFable3Lists (linkedList: Map<string, Json>) =
        [
            match Map.tryFind "head" linkedList with
            | Some value -> value
            | None -> ()

            match Map.tryFind "tail" linkedList with
            | Some (JObject tail) -> yield! flattenFable3Lists tail
            | _ -> ()
        ]

    /// <summary>Returns whether the type information resembles a type of a sequence of elements (including tuples)</summary>
    let arrayLike = function
        | TypeInfo.Array _ -> true
        | TypeInfo.List _ -> true
        | TypeInfo.Seq _ -> true
        | TypeInfo.Tuple _ -> true
        | TypeInfo.Set _ -> true
        | TypeInfo.ResizeArray _ -> true
        | TypeInfo.HashSet _ -> true
        | _ -> false

    let isRecord = function
        | TypeInfo.Record recordType -> true
        | _ -> false

    let unionOfRecords = function
        | TypeInfo.Union getCases ->
            let (unionCases, unionType) = getCases()
            unionCases
            |> Seq.forall (fun case -> case.CaseTypes.Length = 1 && isRecord case.CaseTypes.[0])
        | _ ->
            false

    let optional = function
        | TypeInfo.Option _ -> true
        | _ -> false

    let isQuoted (input: string) =
        input.StartsWith "\"" && input.EndsWith "\""

    let betweenQuotes (input: string) = "\"" + input + "\""

    let removeQuotes (input: string) =
        input.Substring(1, input.Length - 2)

    let rec fromJsonAs (input: Json) (typeInfo: Fable.SimpleJson.TypeInfo) : obj =
        match input, typeInfo with
        | JNumber value, TypeInfo.Float -> unbox value
        | JString value, TypeInfo.Float when value.ToLower() = "nan" -> unbox (Double.NaN)
        | JString value, TypeInfo.Float -> unbox (float value)
        | JNumber value, TypeInfo.Float32 -> unbox (float32 value)
        | JString value, TypeInfo.Float32 when value.ToLower() = "nan" -> unbox (Double.NaN)
        | JString value, TypeInfo.Float32 -> unbox (float32 value)
        // reading number as int -> floor it
        | JNumber value, TypeInfo.Int32 -> unbox (JS.Math.floor(value))
        | JBool value, TypeInfo.Bool -> unbox value
        // reading int from string -> parse it
        | JString value, TypeInfo.Int32 -> unbox (int value)
        | JString value, TypeInfo.Char -> unbox (char value)
        | JNumber value, TypeInfo.Char -> unbox (char (unbox<int> value))
        // reading into strings
        | JString value, TypeInfo.String -> unbox value
        | JNumber value, TypeInfo.String -> unbox (string value)
        // uri
        | JString value, TypeInfo.Uri -> unbox(Uri(value))
        // decimals
        | JString value, TypeInfo.Decimal -> unbox (decimal value)
        | JNumber value, TypeInfo.Decimal -> unbox (decimal value)
        | JString value, TypeInfo.Short -> unbox (int16 value)
        | JNumber value, TypeInfo.Short -> unbox (int16 value)
        // Unsigned integers
        | JNumber value, TypeInfo.UInt16 -> unbox (uint16 value)
        | JString value, TypeInfo.UInt16 -> unbox (uint16 value)
        | JNumber value, TypeInfo.UInt32 -> unbox (uint32 value)
        | JString value, TypeInfo.UInt32 -> unbox (uint32 value)
        | JNumber value, TypeInfo.UInt64 -> unbox (uint64 value)
        | JString value, TypeInfo.UInt64 -> unbox (uint64 value)
        | JNumber value, TypeInfo.TimeSpan -> unbox (JS.Math.floor value)
        | JString value, TypeInfo.Enum getlElemType ->
            let (underlyingType, originalType) = getlElemType()
            match underlyingType with
            | TypeInfo.Int32 ->
                match Int32.TryParse(value) with
                | true, parsedNumber ->
                    unbox parsedNumber
                | false, _ ->
                    failwithf "The value '%s' is not valid for enum of type '%s'" value originalType.Name
            | TypeInfo.Long ->
                match Int64.TryParse(value) with
                | true, parsedNumber ->
                    unbox parsedNumber
                | false, _ ->
                    failwithf "The value '%s' is not valid for enum of type '%s'" value originalType.Name
            | other ->
                failwithf "The value '%s' cannot be converted to enum of type '%s'" value originalType.Name
        | JNumber value, TypeInfo.Enum getElemType ->
            let (_, originalType) = getElemType()
            unbox value
        // byte[] coming from the server is serialized as base64 string
        // convert it back to the actual byte array
        | JString value, TypeInfo.Array getElemType ->
            let elemType = getElemType()
            match elemType with
            | TypeInfo.Byte ->
                if insideWorker || insideBrowser
                then unbox (Convert.FromBase64String value)
                else unbox (Node.bytesFromBase64 value)
            | otherType -> failwithf "Cannot convert arbitrary string '%s' to %A" value otherType

        // null values for strings are just the null string
        | JNull, TypeInfo.String -> unbox null
        | JNull, TypeInfo.Unit -> unbox ()
        | genericJson, TypeInfo.Object -> unbox (SimpleJson.toPlainObject genericJson)
        // int64 as string -> parse it
        | JString value, TypeInfo.Long -> unbox (int64 value)
        | JString value, TypeInfo.Byte -> unbox (byte value)
        | JNumber value, TypeInfo.Byte -> unbox (byte value)
        | JNumber value, TypeInfo.SByte -> unbox (sbyte value)
        | JString value, TypeInfo.SByte -> unbox (sbyte value)
        // BigInt as string -> parse it
        | JString value, TypeInfo.BigInt -> unbox (BigInteger.Parse value)
        | JNumber value, TypeInfo.BigInt -> unbox (bigint (JS.Math.floor(value)))
        // parse formatted date time
        | JString value, TypeInfo.DateTime -> unbox (DateTime.Parse(value))
        // parse formatted date time offset
        | JString value, TypeInfo.DateTimeOffset -> unbox (DateTimeOffset.Parse(value))
        | JNumber value, TypeInfo.DateTimeOffset ->
            let seconds = int64 (JS.Math.floor(value))
            unbox (DateTimeOffset.FromUnixTimeSeconds seconds)
#if NET6_0_OR_GREATER
        // TimeOnly, DateOnly
        | JNumber value, TypeInfo.DateOnly -> unbox (DateOnly.FromDayNumber (int value))
        | JString value, TypeInfo.DateOnly -> unbox (DateOnly.FromDayNumber (int value))
        | JString value, TypeInfo.TimeOnly -> unbox (TimeOnly (int64 value))
#endif

        // deserialize union from objects
        // { "One": 20 } or {"One": [20]} -> One of int
        | JObject values, TypeInfo.Union (getTypes) ->
            let (cases, unionType) = getTypes()
            match Map.toList values with
            | [ caseName, JArray values ] ->
                cases
                |> Array.tryFind (fun case -> case.CaseName = caseName)
                |> function
                    | None ->
                        let caseNames = Array.map (fun case -> sprintf " '%s' " case.CaseName) cases
                        let expectedCases = String.concat ", " caseNames
                        failwithf "Case %s was not valid for type '%s', expected one of the cases [%s]" caseName unionType.Name expectedCases
                    | Some foundCase when Array.length foundCase.CaseTypes = 1 && arrayLike foundCase.CaseTypes.[0] ->
                        let deserialized = fromJsonAs (JArray values) foundCase.CaseTypes.[0]
                        FSharpValue.MakeUnion(foundCase.Info, [| deserialized |])
                        |> unbox
                    | Some foundCase when Array.length foundCase.CaseTypes = 1 && optional foundCase.CaseTypes.[0] ->
                        let parsedOptional = unbox (fromJsonAs (JArray values) foundCase.CaseTypes.[0])
                        FSharpValue.MakeUnion(foundCase.Info, [| parsedOptional |])
                        |> unbox
                    | Some foundCase ->
                        if Array.length foundCase.CaseTypes = 1
                            && not (arrayLike foundCase.CaseTypes.[0])
                            && Array.length foundCase.CaseTypes <> List.length values
                        then failwithf "Expected case '%s' to have %d argument types but the JSON data only contained %d values" foundCase.CaseName (Array.length foundCase.CaseTypes) (List.length values)
                        let parsedValues =
                            Array.ofList values
                            |> Array.zip foundCase.CaseTypes
                            |> Array.map (fun (valueType, value) -> fromJsonAs value valueType)
                        FSharpValue.MakeUnion(foundCase.Info, parsedValues)
                        |> unbox
            | [ caseName, NonArray json ] ->
                cases
                |> Array.tryFind (fun case -> case.CaseName = caseName)
                |> function
                    | Some ({ CaseName = caseName; Info = caseInfo; CaseTypes = [| caseType |] }) ->
                        FSharpValue.MakeUnion(caseInfo, [| unbox fromJsonAs json caseType |])
                        |> unbox
                    | _ ->
                        let caseNames = Array.map (fun case -> sprintf " '%s' " case.CaseName) cases
                        let expectedCases = String.concat ", " caseNames
                        failwithf "Case %s was not valid for type '%s', expected one of the cases [%s]" caseName unionType.Name expectedCases

            // Specific for Fable 3
            | otherwise when Map.containsKey "tag" values && Map.containsKey "fields" values && Map.count values = 2 ->
                match Map.tryFind "tag" values, Map.tryFind "fields" values with
                | Some (JNumber caseIndex), Some (JArray fieldValues) ->
                    let foundCase = cases.[int caseIndex]
                    let values =
                        fieldValues
                        |> Array.ofList
                        |> Array.mapi (fun index value -> fromJsonAs value (foundCase.CaseTypes.[index]))

                    FSharpValue.MakeUnion(foundCase.Info, values)
                | _ ->
                    failwithf "Could not deserialize JSON(%s) into type %s" (SimpleJson.toString (JObject values)) unionType.FullName

            | otherwise when unionOfRecords typeInfo ->
                let discriminators = ["__typename"; "$typename"; "$type" ]
                let foundDiscriminatorKey =
                    discriminators
                    |> List.tryFind (fun keyword -> Map.containsKey keyword values)

                match foundDiscriminatorKey with
                | None ->
                    failwithf "Could not serialize the JSON object into the union of records of type %s because the JSON did not contain a known discriminator. Expected '__typename', '$typeName' or '$type'" unionType.Name
                | Some discriminatorKey ->
                    let discriminatorValueJson = Map.find discriminatorKey values
                    match discriminatorValueJson with
                    | JString discriminatorValue ->
                        let foundUnionCase =
                            cases
                            |> Seq.tryFind (fun case -> case.CaseName.ToUpperInvariant() = discriminatorValue.ToUpperInvariant())

                        match foundUnionCase with
                        | None ->
                            failwithf "Union of records of type '%s' does not have a matching case '%s'" unionType.Name discriminatorValue
                        | Some case ->
                            // Assuming the case types is [recordType]
                            // one element of types and the first element is a record
                            // as satisfied by the unionOfRecords function
                            let record = unbox (fromJsonAs (JObject values) (case.CaseTypes.[0]))
                            FSharpValue.MakeUnion(case.Info, [| record |])
                    | otherwise ->
                        failwithf "Union of records of type '%s' cannot be deserialized with the value of the discriminator key is not a string to match against a specific union case" unionType.Name
            | otherwise ->
                // TODO!!! Better error messages here
                let unexpectedJson = JS.JSON.stringify otherwise
                let expectedType = JS.JSON.stringify cases
                failwithf "Expected JSON:\n%s\nto match the type\n%s" unexpectedJson expectedType
        | JNull, TypeInfo.Option _ -> unbox None
        | jsonValue, TypeInfo.Option optionalTypeDelayed when jsonValue <> JNull ->
            let optionalType = optionalTypeDelayed()
            let parsedOptional = unbox (fromJsonAs jsonValue optionalType)
            unbox Some parsedOptional
        | JString value, TypeInfo.Guid _ -> unbox (System.Guid.Parse(value))
        // int64 as a number, convert it to int then to in64
        | JNumber value , TypeInfo.Long _ -> unbox int64 (int value)
        // int64 as the internal representation from Long.js
        // then reconstruct it from the high/low (two integers) components
        | JObject dict, TypeInfo.Long _ ->
            let get key = Map.tryFind key dict
            [ get "low"; get "high"; get "unsigned" ]
            |> List.choose id
            |> function
                | [ JNumber low; JNumber high; JBool _ ] ->
                    let lowBytes = BitConverter.GetBytes(int low)
                    let highBytes = BitConverter.GetBytes(int high)
                    let combinedBytes = Array.concat [ lowBytes; highBytes ]
                    BitConverter.ToInt64(combinedBytes, 0)
                    |> unbox
                | _ -> failwithf "Unable to construct int64 from object literal { low: int, high: int, unsigned: bool }"
        // convert a single case string to union
        // "One" -> One, here is a special case where the case in quoted inside the string
        | JString caseName, TypeInfo.Union getTypes when isQuoted caseName ->
            let (caseTypes, unionType) = getTypes()
            caseTypes
            |> Array.tryFind (fun case -> case.CaseName = removeQuotes caseName)
            |> function
                | Some ({ Info = caseInfo }) -> unbox (FSharpValue.MakeUnion(caseInfo, [||]))
                | None ->
                    let caseNames = Array.map (fun case -> sprintf " '%s' " case.CaseName) caseTypes
                    let expectedCases = String.concat ", " caseNames
                    failwithf "Case %s was not valid for type '%s', expected one of the cases [%s]" caseName unionType.Name expectedCases
        // convert a single case string to union
        // "One" -> One
        | JString caseName, TypeInfo.Union getTypes ->
            let (caseTypes, unionType) = getTypes()
            caseTypes
            |> Array.tryFind (fun case -> case.CaseName = caseName)
            |> function
                | Some ({ Info = caseInfo }) -> unbox (FSharpValue.MakeUnion(caseInfo, [||]))
                | None ->
                    let caseNames = Array.map (fun case -> sprintf " '%s' " case.CaseName) caseTypes
                    let expectedCases = String.concat ", " caseNames
                    failwithf "Case %s was not valid for type '%s', expected one of the cases [%s]" caseName unionType.Name expectedCases
        | JString serializedRecord, TypeInfo.Record getFields ->
            fromJsonAs (SimpleJson.parse serializedRecord) typeInfo
        // convert unions from arrays
        // ["One", 20] -> One of int
        | JArray caseValue, TypeInfo.Union getTypes ->
            let (cases, unionType) = getTypes()
            match caseValue with
            // Union case without values
            | [ JString caseName ] ->
                cases
                |> Array.tryFind (fun case -> case.CaseName = caseName)
                |> function
                    | Some ({ CaseName = caseName; Info = caseInfo; CaseTypes = caseInfoTypes }) ->
                        // single case without values
                        unbox (FSharpValue.MakeUnion(caseInfo, [||]))
                    | None ->
                        let caseNames = Array.map (fun case -> sprintf " '%s' " case.CaseName) cases
                        let expectedCases = String.concat ", " caseNames
                        failwithf "Case '%s' was not valid for type '%s', expected one of the cases [%s]" caseName unionType.Name expectedCases
            | JString caseName :: values ->
                cases
                |> Array.tryFind (fun case -> case.CaseName = caseName)
                |> function
                    | None ->
                        let caseNames = Array.map (fun ({ CaseName = name }) -> name) cases
                        let expectedCases = String.concat ", " caseNames
                        failwithf "Case %s was not valid, expected one of [%s]" caseName expectedCases
                    | Some ({ CaseName = foundCaseName; Info = caseInfo; CaseTypes = types }) ->
                        if Array.length types <> List.length values
                        then failwithf "The number of union case parameters for '%s' is different" foundCaseName
                        let parsedValues =
                            Array.ofList values
                            |> Array.zip types
                            |> Array.map (fun (valueType, value) -> fromJsonAs value valueType)
                        FSharpValue.MakeUnion(caseInfo, parsedValues)
                        |> unbox
            | otherwise ->
                let unexpectedJson = JS.JSON.stringify otherwise
                let expectedType = JS.JSON.stringify cases
                failwithf "Expected JSON:\n%s\nto match the type\n%s" unexpectedJson expectedType
        // Arrays
        | JArray values, TypeInfo.Array elementTypeDelayed ->
            let elementType = elementTypeDelayed()
            values
            |> List.map (fun value -> unbox (fromJsonAs value elementType))
            |> Array.ofList
            |> unbox
        // Lists
        | JArray values, TypeInfo.List elementTypeDelayed ->
            let elementType = elementTypeDelayed()
            values
            |> List.map (fun value -> unbox (fromJsonAs value elementType))
            |> unbox

        // Specific for Fable 3
        | JObject linkedList, TypeInfo.List elementTypeDelayed ->
            let elementType = elementTypeDelayed()
            let flattenedList = flattenFable3Lists linkedList
            flattenedList
            |> List.map (fun value -> unbox (fromJsonAs value elementType))
            |> unbox

        | JArray values, TypeInfo.Set elementTypeDelayed ->
            let elementType = elementTypeDelayed()
            values
            |> List.map (fun value -> unbox (fromJsonAs value elementType))
            |> Set.ofList
            |> unbox

        | JArray values, TypeInfo.Seq elementTypeDelayed ->
            let elementType = elementTypeDelayed()
            let converted = List.map (fun value -> unbox (fromJsonAs value elementType)) values
            unbox converted
        // Tuples, become just arrays
        | JArray array, TypeInfo.Tuple tupleTypesDelayed ->
            let tupleTypes = tupleTypesDelayed()
            array
            |> Array.ofList
            |> Array.zip tupleTypes
            |> Array.map (fun (jsonType, jsonData) -> fromJsonAs jsonData jsonType)
            |> unbox
        // Records
        | JObject dict, TypeInfo.Record getTypes ->
            let fields, recordType = getTypes()
            // Match the JSON object literal keys with their types
            let recordValues =
                let values = Map.toList dict
                fields
                |> Array.map (fun ({ FieldName = fieldName; FieldType = fieldType }) ->
                    values
                    |> List.tryFind (fun (key, value) -> fieldName = key)
                    |> function
                        | Some (key, value) -> unbox (fromJsonAs value fieldType)
                        | None ->
                            match fieldType with
                            // field type is an option of something, just return None
                            | TypeInfo.Option _ -> unbox None
                            | _ ->
                              // field type is required and it doens't exist in the JSON
                              // then generate a nice error message
                              let dictKeys =
                                  Map.toList dict
                                  |> List.map (fst >> sprintf "'%s'")
                                  |> String.concat ", "
                                  |> sprintf "[ %s ]"
                              let recordFields =
                                  fields
                                  |> Array.map (fun ({ FieldName = name; FieldType = innerFieldType }) ->
                                      match innerFieldType with
                                      | TypeInfo.Option _ -> sprintf "optional('%s')" name
                                      | _ -> sprintf "required('%s')" name)
                                  |> String.concat ", "
                                  |> sprintf "[ %s ]"
                              failwithf "Could not find the required key '%s' in the JSON object literal with keys %s to match with record type '%s' that has fields %s" fieldName dictKeys recordType.Name recordFields)
            unbox (FSharpValue.MakeRecord(recordType, recordValues))

        | JArray tuples, TypeInfo.Map getTypes ->
            let (keyType, valueType) = getTypes()
            let pairs =
                [ for keyValuePair in tuples do
                    let tuple = fromJsonAs keyValuePair (TypeInfo.Tuple (let a = [| keyType; valueType |] in fun () -> a))
                    yield tuple ]
            match keyType with
            | TypeInfo.Int32
            | TypeInfo.String
            | TypeInfo.Bool ->
                pairs
                |> unbox<(string * obj) list>
                |> Map.ofList
                |> unbox
            | _ ->
                pairs
                |> unbox<(IStructuralComparable * obj) list>
                |> Map.ofList
                |> unbox

        | JArray tuples, TypeInfo.Dictionary getTypes ->
            let (keyType, valueType, originalType) = getTypes()
            let pairs =
                [ for keyValuePair in tuples do
                    let tuple = fromJsonAs keyValuePair (TypeInfo.Tuple (fun () -> [| keyType; valueType |]))
                    yield tuple ]

            let output =
                match keyType with
                | TypeInfo.Union _ ->  Dictionary<Result<_, _>, _>()
                | TypeInfo.Record _ -> Dictionary<{| dummy: int |}, _>() |> unbox
                | _ -> Dictionary<IStructuralComparable, _>() |> unbox

            for (key, value) in (unbox<(IStructuralComparable * obj) list> pairs) do output.Add(unbox key, value)
            unbox output

        | JObject dict, TypeInfo.Dictionary getTypes ->
            let (keyType, valueType, originalType) = getTypes()
            dict
            |> Map.toList
            |> List.map (fun (key, value) -> fromJsonAs (JString key) keyType, fromJsonAs value valueType )
            |> fun pairs ->
                let output =
                    match keyType with
                    | TypeInfo.Union _ -> Dictionary<Result<_, _>, _>()
                    | TypeInfo.Record _ -> Dictionary<{| dummy: int |}, _>() |> unbox
                    | _ -> Dictionary<IStructuralComparable, _>() |> unbox

                for (key, value) in pairs do output.Add(unbox key, value)

                unbox output

        | JArray items, TypeInfo.HashSet getType ->
            let elemType = getType()
            let hashset =
                match elemType with
                | TypeInfo.Union _ -> HashSet<Result<_, _>>()
                | TypeInfo.Record _ -> HashSet<{| dummy: int |}>() |> unbox
                | _ -> HashSet<IStructuralComparable>() |> unbox

            for item in items do
                let deserialized = fromJsonAs item elemType
                hashset.Add(unbox deserialized) |> ignore

            unbox hashset

        | JObject map, TypeInfo.Map getTypes ->
            let (keyType, valueType) = getTypes()
            // check whether the map is serialized to it's internal representation
            // and convert that to back to a normal map from the data
            match Map.tryFind "comparer" map, Map.tryFind "tree" map with
            | Some (JObject comparer), Some (JArray tree) when Map.isEmpty comparer ->
                match generateMap (JArray tree) with
                | Some internalMap ->
                    let pairs =
                        flattenMap internalMap
                        |> List.map (fun (key, value) ->
                            let nextKey =
                                if not (isQuoted key)
                                then unbox (fromJsonAs (JString key) keyType)
                                else unbox (fromJsonAs (SimpleJson.parseNative key) keyType)
                            let nextValue = unbox (fromJsonAs value valueType)
                            unbox<obj> nextKey, nextValue)
                    match keyType with
                    | TypeInfo.Int32
                    | TypeInfo.String
                    | TypeInfo.Bool ->
                        pairs
                        |> unbox<(string * obj) list>
                        |> Map.ofList
                        |> unbox
                    | _ ->
                        pairs
                        |> unbox<(IStructuralComparable * obj) list>
                        |> Map.ofList
                        |> unbox

                | None ->
                    let inputJson = SimpleJson.toString (JArray tree)
                    failwithf "Could not generate map from JSON\n %s" inputJson

            // Specific for Fable 3
            | Some (JObject comparer), Some (JObject tree) when Map.isEmpty comparer ->
                let flattenedMap = Map.ofList (flatteFable3Map tree)
                fromJsonAs (JObject flattenedMap) typeInfo
            | _ ->
                // if comparer and tree are not present,
                // assume we are parsing Fable 1 object literal
                // and converting that to map
                let pairs =
                    map
                    |> Map.toList
                    |> List.map (fun (key, value) ->
                        let nextKey =
                            if not (isQuoted key)
                            then
                                if Converter.isPrimitive keyType || Converter.enumUnion keyType
                                then
                                    // for primitive type, just read them as string and parse
                                    unbox (fromJsonAs (JString key) keyType)
                                else
                                    // server-side JSON can still be complex (for complex types)
                                    // but doesn't have to be quoted, parse again here
                                    unbox (fromJsonAs (SimpleJson.parseNative key) keyType)
                            else
                                unbox (fromJsonAs (SimpleJson.parseNative key) keyType)
                        let nextValue = unbox (fromJsonAs value valueType)
                        unbox<string> nextKey, nextValue)

                match keyType with
                | TypeInfo.Int32
                | TypeInfo.String
                | TypeInfo.Bool ->
                    pairs
                    |> unbox<(string * obj) list>
                    |> Map.ofList
                    |> unbox
                | _ ->
                    pairs
                    |> unbox<(IStructuralComparable * obj) list>
                    |> Map.ofList
                    |> unbox
        | _, TypeInfo.Any getType ->
            let unknownType = getType()
            failwithf "Cannot convert %s to %s" (SimpleJson.toString input) unknownType.FullName
        | _ ->
            failwithf "Cannot convert %s to %s" (SimpleJson.toString input) (JS.JSON.stringify typeInfo)

    let fromJson<'t> json typeInfo =
        unbox<'t> (fromJsonAs json typeInfo)

    let quoteText (inputText: string) : string = importDefault "./quote.js"

    let rec serialize value (typeInfo: TypeInfo) =
        match typeInfo with
        | TypeInfo.String ->
            let content = unbox<string> value
            if isNull content
            then "null"
            else quoteText content
        | TypeInfo.Unit -> "null"
        | TypeInfo.Float
        | TypeInfo.Float32 ->
            if Double.IsNaN(unbox value)
            then quoteText "NaN"
            else string (unbox<double> value)
        | TypeInfo.Char -> quoteText (string (unbox<char> value))
        | TypeInfo.Byte
        | TypeInfo.SByte
        | TypeInfo.UInt16
        | TypeInfo.UInt32
        | TypeInfo.Short
        | TypeInfo.Enum _
        | TypeInfo.TimeSpan
        | TypeInfo.Int32 -> string (unbox<int> value)
        | TypeInfo.UInt64
        | TypeInfo.Long -> betweenQuotes (string (unbox<int64> value))
        | TypeInfo.BigInt -> betweenQuotes (string (unbox<bigint> value))
        | TypeInfo.Decimal -> betweenQuotes (string (unbox<decimal> value))
        | TypeInfo.Bool -> if unbox<bool> value then "true" else "false"
        | TypeInfo.Guid -> betweenQuotes ((unbox<Guid> value).ToString())
        | TypeInfo.Uri -> betweenQuotes ((unbox<Uri> value).ToString())
        | TypeInfo.DateTime -> betweenQuotes ((unbox<DateTime> value).ToString("O"))
        | TypeInfo.DateTimeOffset -> betweenQuotes ((unbox<DateTimeOffset> value).ToString("O"))
#if NET6_0_OR_GREATER
        | TypeInfo.DateOnly -> string (unbox<DateOnly> value).DayNumber
        | TypeInfo.TimeOnly -> betweenQuotes (string (unbox<TimeOnly> value).Ticks)
#endif
        | TypeInfo.Record getFields ->
            let (fieldTypes, recordType) = getFields()
            let serializedFields =
                fieldTypes
                |> Array.map (fun field ->
                    let fieldValue = FSharpValue.GetRecordField(value, field.PropertyInfo)
                    sprintf "\"%s\": %s" field.FieldName (serialize fieldValue field.FieldType)
                )

            "{" + String.concat ", " serializedFields + "}"

        | TypeInfo.ResizeArray getElementType ->
            let elementType = getElementType()
            let values =
                value
                |> unbox<ResizeArray<obj>>
                |> Seq.map (fun element -> serialize element elementType)
                |> String.concat ", "

            "[" + values + "]"

        | TypeInfo.HashSet getElementType ->
            let elementType = getElementType()
            let values =
                value
                |> unbox<HashSet<obj>>
                |> Seq.map (fun element -> serialize element elementType)
                |> String.concat ", "

            "[" + values + "]"

        | TypeInfo.Set getElementType ->
            let elementType = getElementType()
            let values =
                value
                |> unbox<Set<IComparable>>
                |> Seq.map (fun element -> serialize element elementType)
                |> String.concat ", "

            "[" + values + "]"

        | TypeInfo.Array getElementType ->
            let elementType = getElementType()
            let values =
                value
                |> unbox<obj []>
                |> Array.map (fun element -> serialize element elementType)
                |> String.concat ", "

            "[" + values + "]"

        | TypeInfo.List getElementType ->
            let elementType = getElementType()
            let values =
                value
                |> unbox<obj list>
                |> List.map (fun element -> serialize element elementType)
                |> String.concat ", "

            "[" + values + "]"

        | TypeInfo.Seq getElementType ->
            let elementType = getElementType()
            let values =
                value
                |> unbox<obj seq>
                |> Seq.toArray
                |> Array.map (fun element -> serialize element elementType)
                |> String.concat ", "

            "[" + values + "]"

        | TypeInfo.Option getElementType ->
            match unbox<obj option> value with
            | None -> "null"
            | Some existingValue -> serialize existingValue (getElementType())

        | TypeInfo.Union getCases ->
            let (unionCases, unionType) = getCases()
            let (usedCase, fields) = FSharpValue.GetUnionFields(value, unionType)
            let caseTypes =
                unionCases
                |> Array.find (fun case -> case.CaseName = usedCase.Name)
                |> fun case -> case.CaseTypes

            if enumUnion typeInfo || Array.isEmpty caseTypes then
                betweenQuotes usedCase.Name
            elif caseTypes.Length = 1 then
                "{" + betweenQuotes usedCase.Name + ": " + serialize fields.[0] caseTypes.[0] + "}"
            else
                let serializedFields =
                    caseTypes
                    |> Array.mapi (fun index caseType -> serialize fields.[index] caseType)
                    |> String.concat ", "

                "{" + betweenQuotes usedCase.Name + ": " + "[" + serializedFields + "] }"

        | TypeInfo.Map getPairTypes ->
            let (keyType, valueType) = getPairTypes()

            let serializedValues =
                value
                |> unbox<Map<IComparable, obj>>
                |> Map.toArray
                |> Array.map (fun (key, value) ->
                    let serializedKey = serialize key keyType
                    let serializedValue = serialize value valueType

                    if isPrimitive keyType || enumUnion keyType then
                        if not (isQuoted serializedKey)
                        then (quoteText serializedKey) + ": " + serializedValue
                        else serializedKey + ": " + serializedValue
                    else
                        "[" + serializedKey + ", " + serializedValue + "]"
                )
                |> String.concat ", "

            if isPrimitive keyType || enumUnion keyType
            then "{" + serializedValues + "}"
            else "[" + serializedValues + "]"

        | TypeInfo.Dictionary getPairTypes ->
            let (keyType, valueType, originalType) = getPairTypes()
            let serializedValues =
                value
                |> unbox<Dictionary<IComparable, obj>>
                |> Seq.map (fun pair ->
                    let (key, value) = pair.Key, pair.Value
                    let serializedKey = serialize key keyType
                    let serializedValue = serialize value valueType

                    if isPrimitive keyType || enumUnion keyType then
                        if not (isQuoted serializedKey)
                        then (betweenQuotes serializedKey) + ": " + serializedValue
                        else serializedKey + ": " + serializedValue
                    else
                        "[" + serializedKey + ", " + serializedValue + "]"
                )
                |> String.concat ", "

            if isPrimitive keyType || enumUnion keyType
            then "{" + serializedValues + "}"
            else "[" + serializedValues + "]"

        | TypeInfo.Tuple getTupleTypes ->
            let tupleTypes = getTupleTypes()

            if tupleTypes.Length = 1 then
                "[" + serialize value tupleTypes.[0] + "]"
            else
                let serializedValues =
                    value
                    |> unbox<obj array>
                    |> Array.mapi (fun index element -> serialize element tupleTypes.[index])
                    |> String.concat ", "

                "[" + serializedValues + "]"

        | TypeInfo.Object ->
            SimpleJson.stringify value

        | TypeInfo.Any getType ->
            // fallback to low-level serialization
            SimpleJson.stringify value

        | _ ->
            "null"

[<AutoOpenAttribute>]
module ConverterExtensions =
    type Json with

        /// <summary>
        /// Serialized the input value object into JSON, uses built-in JSON.stringify and should be used with Fable 2.x or earlier
        /// </summary>
        static member stringify (value: obj) : string =
            if Convert.isUsingFable3
            then JS.console.warn("It looks like you using the function Json.stringify from Fable.SimpleJson while also using Fable 3 (nagareyama). Please use Json.serialize instead which supports both Fable 3 and Fable 2.x")
            SimpleJson.stringify value

        /// <summary>
        /// Serialized the input value into JSON using Reflection. Compatible with Fable 2.x and Fable 3 (codename: nagareyama)
        /// </summary>
        static member inline serialize<'t> (value: 't) : string =
            let typeInfo = TypeInfo.createFrom<'t>()
            Convert.serialize value typeInfo

        /// Parses the input string as JSON and tries to convert it as the given type argument
        static member inline parseAs<'t> (input: string) : 't =
            match SimpleJson.tryParse input with
            | None -> failwith "Couldn't parse the input JSON string because it seems to be invalid"
            | Some inputJson ->
                let typeInfo = TypeInfo.createFrom<'t> ()
                Convert.fromJson<'t> inputJson typeInfo

        /// Parses the input string as JSON using native parsing and tries to convert it as the given type argument
        static member inline parseNativeAs<'t> (input: string) : 't =
            let inputJson = SimpleJson.parseNative input
            let typeInfo = TypeInfo.createFrom<'t> ()
            Convert.fromJson<'t> inputJson typeInfo

        /// Tries to parse the input string as JSON and tries to convert it as the given type argument, returing a (hopefully) useful error message when it fails
        static member inline tryParseAs<'t> (input: string) : Result<'t, string> =
            try Ok (Json.parseAs<'t> input)
            with | ex -> Error ex.Message

        /// Tries to parse the input string as JSON using native parsing and tries to convert it as the given type argument
        static member inline tryParseNativeAs<'t> (input: string) : Result<'t, string> =
            try Ok (Json.parseNativeAs<'t> input)
            with | ex -> Error ex.Message

        /// Tries to convert parsed JSON object as the given type parameter argument, this method is used when you want to apply transformations to the JSON object before parsing
        static member inline convertFromJsonAs<'t> (input: Json) : 't =
            let typeInfo = TypeInfo.createFrom<'t> ()
            Convert.fromJson<'t> input typeInfo

        /// Tries to convert parsed JSON object as the given type parameter argument, this method is used when you want to apply transformations to the JSON object before parsing
        static member inline tryConvertFromJsonAs<'t> (input: Json) : Result<'t, string> =
            try Ok (Json.convertFromJsonAs<'t> input)
            with | ex -> Error ex.Message
