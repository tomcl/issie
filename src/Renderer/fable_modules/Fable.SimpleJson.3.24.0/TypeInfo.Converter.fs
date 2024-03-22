namespace Fable.SimpleJson

open System
open FSharp.Reflection
open Fable.Core
open System.Reflection
open System.Collections.Generic

[<AutoOpen>]
module Converter =
    let (|PrimitiveType|_|) (primType: Type) =
        match primType.FullName with
        | "System.String" -> Some TypeInfo.String
        | "System.Char" -> Some TypeInfo.Char
        | "System.Int16" -> Some TypeInfo.Short
        | "System.Int32" -> Some TypeInfo.Int32
        | "Microsoft.FSharp.Core.int64`1"
        | "System.Int64" -> Some TypeInfo.Long
        | "System.UInt16" -> Some TypeInfo.UInt16
        | "System.UInt32" -> Some TypeInfo.UInt32
        | "System.UInt64" -> Some TypeInfo.UInt64
        | "System.DateTime" -> Some TypeInfo.DateTime
        | "System.DateOnly" -> Some TypeInfo.DateOnly
        | "System.TimeOnly" -> Some TypeInfo.TimeOnly
        | "System.TimeSpan" -> Some TypeInfo.TimeSpan
        | "System.DateTimeOffset" -> Some TypeInfo.DateTimeOffset
        | "System.Boolean" -> Some  TypeInfo.Bool
        | "System.Single" -> Some TypeInfo.Float32
        | "System.Double" -> Some TypeInfo.Float
        | "Microsoft.FSharp.Core.decimal`1"
        | "System.Decimal" -> Some TypeInfo.Decimal
        | "System.Numerics.BigInteger" -> Some TypeInfo.BigInt
        | "Microsoft.FSharp.Core.Unit" -> Some TypeInfo.Unit
        | "System.Guid" -> Some TypeInfo.Guid
        | "System.Byte" -> Some TypeInfo.Byte
        | "System.SByte" -> Some TypeInfo.SByte
        | "System.Object" -> Some TypeInfo.Object
        | "System.Uri" -> Some TypeInfo.Uri
        | _ -> None

    let (|RecordType|_|) (t: Type) =
        if FSharpType.IsRecord t
        then
            FSharpType.GetRecordFields t
            |> Array.map (fun field -> field, field.Name, field.PropertyType)
            |> Some
        else None

    let (|SetType|_|) (t: Type) =
        if t.FullName.StartsWith "Microsoft.FSharp.Collections.FSharpSet`1"
        then t.GetGenericArguments().[0] |> Some
        else None

    let (|Nullable|_|) (t: Type) =
        if t.FullName.StartsWith "System.Nullable`1"
        then t.GetGenericArguments().[0] |> Some
        else None

    let (|UnionType|_|) (t: Type) =
        if FSharpType.IsUnion t
        then
            FSharpType.GetUnionCases t
            |> Array.map (fun info ->
                let caseName = info.Name
                let caseTypes = info.GetFields() |> Array.map (fun prop -> prop.PropertyType)
                caseName, info, caseTypes)
            |> Some
        else None

    let (|MapType|_|) (t: Type) =
        if (t.FullName.StartsWith "Microsoft.FSharp.Collections.FSharpMap`2")
        then
            let genArgs = t.GetGenericArguments()
            Some (genArgs.[0], genArgs.[1])
        else None

    let (|ListType|_|) (t: Type) =
        if (t.FullName.StartsWith "Microsoft.FSharp.Collections.FSharpList`1")
        then t.GetGenericArguments().[0] |> Some
        else None

    let rec flattenFuncTypes (typeDef: Type) =
        [| if FSharpType.IsFunction typeDef
           then let (domain, range) = FSharpType.GetFunctionElements typeDef
                yield! flattenFuncTypes domain
                yield! flattenFuncTypes range
           else yield typeDef |]

    let (|FuncType|_|) (t: Type) =
        if FSharpType.IsFunction t
        then flattenFuncTypes t |> Some
        else None

    let (|ArrayType|_|) (t:Type) =
        if t.IsArray
        then t.GetElementType() |> Some
        else None

    let (|OptionType|_|) (t:Type) =
        if (t.FullName.StartsWith "Microsoft.FSharp.Core.FSharpOption`1")
        then t.GetGenericArguments().[0] |> Some
        else None

    let (|TupleType|_|) (t: Type) =
        if FSharpType.IsTuple t
        then FSharpType.GetTupleElements(t) |> Some
        else None

    let (|SeqType|_|) (t: Type) =
        if t.FullName.StartsWith "System.Collections.Generic.IEnumerable`1"
        then  t.GetGenericArguments().[0] |> Some
        else None

    let (|DictionaryType|_|) (t: Type) =
        if t.FullName.StartsWith "System.Collections.Generic.Dictionary"
        then
          let genArgs = t.GetGenericArguments()
          Some (genArgs.[0], genArgs.[1])
        else
          None

    let (|ResizeArrayType|_|) (t: Type) =
        if t.FullName.StartsWith "System.Collections.Generic.List"
        then t.GetGenericArguments().[0] |> Some
        else None

    let (|HashSetType|_|) (t: Type) =
        if t.FullName.StartsWith "System.Collections.Generic.HashSet"
        then t.GetGenericArguments().[0] |> Some
        else None

    let (|AsyncType|_|) (t:Type) =
        if t.FullName.StartsWith "Microsoft.FSharp.Control.FSharpAsync`1"
        then  t.GetGenericArguments().[0] |> Some
        else None

    let (|PromiseType|_|) (t:Type) =
        if t.FullName.StartsWith "Fable.Core.JS.Promise`1"
        then t.GetGenericArguments().[0] |> Some
        else None

    let private lazyToDelayed (l:Lazy<_>) = fun () -> l.Value

    let (|EnumType|_|) (t: Type) =
        if t.IsEnum
        then Enum.GetUnderlyingType(t) |> Some
        else None

    let rec private _createTypeInfo (resolvedType: Type) : Fable.SimpleJson.TypeInfo =
        match resolvedType with
        | PrimitiveType typeInfo -> typeInfo
        | FuncType (types) -> TypeInfo.Func <| lazyToDelayed (lazy (Array.map createTypeInfo types))
        | RecordType fields ->
            let l = lazy (
                let fields =
                    [| for (field, fieldName, fieldType) in fields ->
                        { PropertyInfo = field
                          FieldName = fieldName;
                          FieldType = createTypeInfo fieldType } |]
                fields, resolvedType)
            TypeInfo.Record (lazyToDelayed l)

        | UnionType cases ->
            let l = lazy (
                [| for (caseName, caseInfo, caseTypes) in cases ->
                    { CaseName = caseName;
                      Info = caseInfo;
                      CaseTypes = Array.map createTypeInfo caseTypes } |], resolvedType)
            TypeInfo.Union (lazyToDelayed l)

        | EnumType elemType -> TypeInfo.Enum (lazyToDelayed <| lazy (createTypeInfo elemType, resolvedType))
        | ListType elemType -> TypeInfo.List (lazyToDelayed <| lazy (createTypeInfo elemType))
        | ResizeArrayType elemType -> TypeInfo.ResizeArray (lazyToDelayed <| lazy (createTypeInfo elemType))
        | HashSetType elemType -> TypeInfo.HashSet (lazyToDelayed <| lazy (createTypeInfo elemType))
        | ArrayType elemType -> TypeInfo.Array (lazyToDelayed <| lazy (createTypeInfo elemType))
        // Checking for tuples has to happen after checking for arrays
        | TupleType types -> TypeInfo.Tuple (lazyToDelayed <| lazy (Array.map createTypeInfo types))
        | OptionType elemType -> TypeInfo.Option (lazyToDelayed <| lazy (createTypeInfo elemType))
        | Nullable elemType -> TypeInfo.Option (lazyToDelayed <| lazy (createTypeInfo elemType))
        | SetType elemType -> TypeInfo.Set (lazyToDelayed <| lazy (createTypeInfo elemType))
        | MapType (keyType, valueType) -> TypeInfo.Map (lazyToDelayed <| lazy (createTypeInfo keyType, createTypeInfo valueType))
        | DictionaryType (keyType, valueType) -> TypeInfo.Dictionary (lazyToDelayed <| lazy (createTypeInfo keyType, createTypeInfo valueType, valueType))
        | SeqType elemType -> TypeInfo.Seq (lazyToDelayed <| lazy (createTypeInfo elemType))
        | AsyncType elemType -> TypeInfo.Async (lazyToDelayed <| lazy (createTypeInfo elemType))
        | PromiseType elemType -> TypeInfo.Promise (lazyToDelayed <| lazy (createTypeInfo elemType))
        | _ -> TypeInfo.Any (lazyToDelayed <| lazy (resolvedType))

    and private typeInfoCache = Dictionary<Type,Fable.SimpleJson.TypeInfo>()

    and createTypeInfo (resolvedType: Type) : Fable.SimpleJson.TypeInfo =
        match typeInfoCache.TryGetValue resolvedType with
        | true, ti -> ti
        | false, _ ->
            let ti = _createTypeInfo resolvedType
            // see https://github.com/fable-compiler/Fable/issues/1871
            // Type equality doesn't work for anonymous records - all anon records are considered equal.
            // For anonymous records, the name is the empty string.
            let notAnonymousRecord =
                not (String.IsNullOrEmpty resolvedType.FullName)
                && not (resolvedType.FullName.EndsWith("`1[]"))
                && not (resolvedType.FullName.EndsWith("`2[]"))

            if notAnonymousRecord then
                typeInfoCache.[resolvedType] <- ti
                ti
            else
                ti

    type Fable.SimpleJson.TypeInfo with
        static member inline createFrom<'t> () : Fable.SimpleJson.TypeInfo =
            createTypeInfo typeof<'t>

    /// returns whether a type is primitive
    let isPrimitive = function
        | TypeInfo.Unit
        | TypeInfo.String
        | TypeInfo.UInt16
        | TypeInfo.UInt32
        | TypeInfo.UInt64
        | TypeInfo.Int32
        | TypeInfo.Bool
        | TypeInfo.Float32
        | TypeInfo.Float
        | TypeInfo.Decimal
        | TypeInfo.Short
        | TypeInfo.Long
        | TypeInfo.Byte
        | TypeInfo.DateTime
        | TypeInfo.DateTimeOffset
        | TypeInfo.DateOnly
        | TypeInfo.TimeOnly
        | TypeInfo.BigInt
        | TypeInfo.Guid
        | TypeInfo.Option _ -> true
        | otherwise -> false

    /// returns whether the discrimiated union type is like a enum
    let enumUnion = function
        | TypeInfo.Union getCases ->
            getCases()
            |> fst
            |> Array.forall (fun case -> Array.isEmpty case.CaseTypes)
        | otherwise -> false
