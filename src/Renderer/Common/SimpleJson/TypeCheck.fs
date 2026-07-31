namespace Fable.SimpleJson

open Fable.Core
open Fable.Core.JsInterop

module TypeCheck =

    [<Emit("typeof ($0) === 'string'")>]
    let typeofString (x: obj) : bool = jsNative

    [<Emit("typeof ($0) === 'boolean'")>]
    let typeofBool (x: obj) : bool = jsNative

    [<Emit("typeof ($0) === 'number'")>]
    let typeofNumber (x: obj) : bool = jsNative
    [<Emit("typeof ($0) === 'object'")>]
    let typeofObject (x: obj) : bool = jsNative

    let (|NativeString|_|) (x: obj) =
        if typeofString x
        then Some (unbox<string> x)
        else None

    let (|NativeBool|_|) (x: obj) =
        if typeofBool x
        then Some (unbox<bool> x)
        else None

    let (|NativeNumber|_|) (x: obj) =
        if typeofNumber x
        then Some (unbox<float> x)
        else None

    let (|NativeObject|_|) (x: obj) =
        if typeofObject x
        then Some x
        else None

    let (|Null|_|) (x: obj) =
        if isNull x
        then Some x
        else None

    let (|NativeArray|_|) (x: obj) =
        if (JS.Constructors.Array.isArray x)
        then Some (unbox<obj[]> x)
        else None