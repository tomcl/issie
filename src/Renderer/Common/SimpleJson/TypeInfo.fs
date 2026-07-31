namespace rec Fable.SimpleJson

open System
open FSharp.Reflection
open System.Reflection

type RecordField = {
    FieldName: string
    FieldType: TypeInfo
    PropertyInfo : PropertyInfo
}

type UnionCase = {
    CaseName: string
    CaseTypes: TypeInfo [ ]
    Info: UnionCaseInfo
}

/// A type that encodes type information which is easily serializable
[<RequireQualifiedAccess>]
type TypeInfo =
    | Unit
    | Char
    | String
    | UInt16
    | UInt32
    | UInt64
    | Int32
    | Bool
    | Float32
    | Float
    | Decimal
    | Short
    | Long
    | Byte
    | SByte
    | DateTime
    | DateTimeOffset
    | DateOnly
    | TimeOnly
    | BigInt
    | TimeSpan
    | Guid
    | Uri
    | Object
    | Any of (unit -> Type)
    | Async of (unit -> TypeInfo)
    | Promise of (unit -> TypeInfo)
    | Option of (unit -> TypeInfo)
    | List of (unit -> TypeInfo)
    | Set of (unit -> TypeInfo)
    | Array of (unit -> TypeInfo)
    | Seq of (unit -> TypeInfo)
    | Tuple of (unit -> TypeInfo [ ])
    | Map of (unit -> TypeInfo * TypeInfo)
    | Dictionary of (unit -> TypeInfo * TypeInfo * Type)
    | ResizeArray of (unit -> TypeInfo)
    | HashSet of (unit -> TypeInfo)
    | Func of (unit -> TypeInfo [ ])
    | Enum of (unit -> TypeInfo * Type)
    | Record of (unit -> RecordField [ ] * Type)
    | Union of (unit -> UnionCase [ ] * Type)