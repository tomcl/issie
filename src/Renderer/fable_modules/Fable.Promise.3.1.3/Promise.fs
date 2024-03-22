
[<RequireQualifiedAccess>]
module Promise

#nowarn "1182" // Unused values

open System
open Fable.Core
open Fable.Core.JsInterop

[<Emit("new Promise($0)")>]
/// <summary>
/// Create a promise from a function
/// <example>
/// <code lang="fsharp">
/// let write (path: string) (content: string) =
///     Promise.create (fun resolve reject ->
///         Node.Api.fs.writeFile(path, content, (fun res ->
///             match res with
///             | Some res -> reject (res :?> System.Exception)
///             | None -> resolve ()
///         ))
///     )
/// </code>
/// </example>
/// </summary>
/// <param name="f">
/// Function used to create the promise, it receives two other function parameters:
///
/// - <c>success</c> : called when the promise is resolved
/// - <c>fail</c> : called when the promise is rejected
/// </param>
/// <typeparam name="'T">Return type of the promise</typeparam>
/// <returns>
/// The promise created by the function
/// </returns>
let create (f: ('T -> unit) -> (exn -> unit) -> unit): JS.Promise<'T> = jsNative

[<Emit("new Promise(resolve => setTimeout(resolve, $0))")>]

/// <summary>
/// Create a promise which wait <c>X</c> ms before resolving.
/// <example>
/// <code lang="fsharp">
/// // Do something
/// doSomething ()
/// // Sleep for 1 second
/// |> Promise.sleep 1000
/// // Do another thing
/// |> Promise.map (fun _ ->
///     doAnotherThing ()
/// )
/// |> Promise.map ...
/// </code>
/// </example>
/// </summary>
/// <param name="ms">Number of milliseconds to wait</param>
/// <returns>A delayed promise</returns>
let sleep (ms: int): JS.Promise<unit> = jsNative

[<Emit("Promise.resolve($0)")>]
/// <summary>
/// Create a promise (in resolved state) with supplied value.
/// <example>
/// <code lang="fsharp">
/// Promise.lift {| Firstname = "John" |}
/// |> Promise.map (fun user ->
///     console.log $"Hello, %s{user.Firstname}"
///     // Expected output: "Hello, John"
/// )
/// |> Promise.map ...
/// </code>
/// </example>
/// </summary>
/// <param name="a">Value to return</param>
/// <typeparam name="'T"></typeparam>
/// <returns>Returns a promise returning the supplied value</returns>
let lift<'T> (a: 'T): JS.Promise<'T> = jsNative

[<Emit("Promise.reject($0)")>]
/// <summary>
/// Creates promise (in rejected state) with supplied reason.
/// <example>
/// <code lang="fsharp">
/// Promise.reject (Exception "User not found")
/// |> Promise.map (fun _ ->
///     // This promise is skipped
/// )
/// |> Promise.catch (fun error ->
///     console.error $"An error ocurred: %s{error.Message}"
///     // Expected output: "An error ocurred: User not found"
/// )
/// |> Promise.map ...
/// </code>
/// </example>
/// </summary>
/// <param name="reason">Reason to return</param>
/// <typeparam name="'T"></typeparam>
/// <returns>Return a promise in a rejected state</returns>
let reject<'T> (reason: exn) : JS.Promise<'T> = jsNative

[<Emit("$1.then($0)")>]
/// <summary>
/// Bind a value into a promise of a new type.
/// <example>
/// <code lang="fsharp">
/// Promise.lift {| Firstname = "John" |}
/// |> Promise.bind (fun user ->
///     // Do something with user and returns a promise
///     Promise.create (fun resolve reject ->
///         resolve $"Hello, %s{user.Firstname}"
///     )
/// )
/// |> Promise.map (fun message ->
///     console.log message
///     // Expected output: "Hello, John"
/// )
/// |> Promise.map ...
/// </code>
/// </example>
/// </summary>
/// <param name="a">
/// A function that takes the value of type <c>T1</c> and transforms it into a promise of type <c>T2</c>.
/// </param>
/// <param name="pr">The input promise</param>
/// <typeparam name="'T1"></typeparam>
/// <typeparam name="'T2"></typeparam>
/// <returns>A promise of the output type of the binder.</returns>
let bind (a: 'T1 -> JS.Promise<'T2>) (pr: JS.Promise<'T1>): JS.Promise<'T2> = jsNative

[<Emit("$1.then($0)")>]
/// <summary>
/// Map a value into another type, the result will be wrapped in a promise for you.
/// <example>
/// <code lang="fsharp">
/// Promise.lift {| Firstname = "John" |}
/// |> Promise.map (fun user ->
///     user.Firstname
/// ) // Returns a Promise&lt;string&gt; with the value "John"
/// |> Promise.map ...
/// </code>
/// </example>
/// </summary>
/// <param name="a">A function to apply to the result of the input promise.</param>
/// <param name="pr">The input promise</param>
/// <typeparam name="'T1"></typeparam>
/// <typeparam name="'T2"></typeparam>
/// <returns>A promise after applying the mapping function</returns>
let map (a: 'T1 -> 'T2) (pr: JS.Promise<'T1>): JS.Promise<'T2> = jsNative

[<Emit("void ($1.then($0))")>]
/// <summary>
/// Call a function with the result of a promise and stop the promise chain.
///
/// This is equivalent to <c>Promise.map ... |> ignore</c>
/// <example>
/// <code lang="fsharp">
/// fetchUser ()
/// |> Promise.iter (fun user ->
///     console.log "User firstname is user.Firstname"
/// ) // unit
/// </code>
/// </example>
/// </summary>
/// <param name="a">A function to apply to the result of the input promise</param>
/// <param name="pr">The input promise</param>
/// <typeparam name="'T"></typeparam>
let iter (a: 'T -> unit) (pr: JS.Promise<'T>): unit = jsNative


/// <summary>
/// This is an identity function, it calls the given function and return the promise value untouched.
/// <example>
/// <code lang="fsharp">
/// fetchUser ()
/// |> Promise.tap (fun user ->
///     // Do something
///     console.log "The user has been received"
/// )
/// |> Promise.map (fun user ->
///     // user value is available here untouched
/// )
/// </code>
/// </example>
/// </summary>
/// <param name="fn">A function to call after receiving the receiving of the input promise</param>
/// <param name="a">The input promise</param>
/// <typeparam name="'T"></typeparam>
/// <returns>A promise of the same type as the input promise</returns>
let tap (fn: 'T -> unit) (a: JS.Promise<'T>): JS.Promise<'T> =
    a |> map (fun x -> fn x; x)

[<Emit("$1.catch($0)")>]
/// <summary>
/// Handle an errored promise allowing you pass a return value.
///
/// This version of <c>catch</c> expects a function returning just <c>'T</c>, as opposed to <c>Promise&lt;'T&gt;</c>. If you need to return <c>Promise&lt;'T&gt;</c>, use <c>catchBind</c>.
/// <example>
/// <code lang="fsharp">
/// Promise.create (fun resolve reject ->
///     reject (System.Exception "User not found")
/// )
/// |> Promise.catch (fun error ->
///     // Log the error
///     console.error error
///     // Do something to recover from the error
///     Error error.Message
/// )
/// |> Promise.map ...
/// </code>
/// </example>
/// </summary>
/// <param name="fail">Function to call if the input promise fail</param>
/// <param name="pr">The input promise</param>
/// <typeparam name="'T"></typeparam>
/// <returns>A promise which result of the call of fail</returns>
let catch (fail: exn -> 'T) (pr: JS.Promise<'T>): JS.Promise<'T> = jsNative

[<Emit("$1.catch($0)")>]
/// <summary>
/// Handle an errored promise allowing to call a promise.
///
/// This version of <c>catch</c> expects a function returning <c>Promise&lt;'T&gt;</c> as opposed to just <c>'T</c>. If you need to return just 'T, use <c>catch</c>.
/// <example>
/// <code lang="fsharp">
/// Promise.create (fun resolve reject ->
///     reject (System.Exception "User not found")
/// )
/// |> Promise.catchBind (fun error ->
///     // Recover from the error, here we call another promise and returns it's result
///     logErrorToTheServer error
/// )
/// |> Promise.map ...
/// </code>
/// </example>
/// </summary>
/// <param name="fail">Function to call if the input promise fail</param>
/// <param name="pr">The input promise</param>
/// <typeparam name="'T"></typeparam>
/// <returns>A promise which is the result of the fail function</returns>
let catchBind (fail: exn -> JS.Promise<'T>) (pr: JS.Promise<'T>): JS.Promise<'T> = jsNative

[<Emit("void ($1.catch($0))")>]
/// <summary>
/// Used to catch errors at the end of a promise chain.
/// <example>
/// <code lang="fsharp">
/// Promise.create (fun resolve reject ->
///     reject (System.Exception "User not found")
/// )
/// |> Promise.map (fun _ ->
///     // ...
/// )
/// |> Promise.map (fun _ ->
///     // ...
/// )
/// |> Promise.catchEnd (fun error ->
///     // ...
/// ) // Returns unit
/// </code>
/// </example>
/// </summary>
/// <param name="fail">Fuction to call if the input promise fail</param>
/// <param name="pr">The input promise</param>
/// <typeparam name="'T"></typeparam>
let catchEnd (fail: exn -> unit) (pr: JS.Promise<'T>): unit = jsNative

[<Emit("$2.then($0).catch($1)")>]
/// <summary>
/// A combination of <c>map</c> and <c>catch</c>, this function applies the <c>success</c> continuation when the input promise resolves successfully, or <c>fail</c> continuation when the input promise fails.
/// <example>
/// <code lang="fsharp">
/// somePromise
/// |> Promise.either
///     (fun x -> string x)
///     (fun err -> Promise.lift err.Message)
/// |> Promise.map ...
/// </code>
/// </example>
/// </summary>
/// <param name="success">Function to call if the input promise succeeds</param>
/// <param name="fail">Function to call if the input promise fail</param>
/// <param name="pr">The input promise</param>
/// <typeparam name="'T1"></typeparam>
/// <typeparam name="'T2"></typeparam>
/// <returns>A new promise which is the result of calling <c>success</c> if <c>pr</c> succeedes, or of <c>fail</c> if <c>pr</c> failed</returns>
let either (success: 'T1 -> 'T2) (fail: exn -> 'T2) (pr: JS.Promise<'T1>): JS.Promise<'T2> = jsNative

[<Emit("$2.then($0).catch($1)")>]
/// <summary>
/// A combination of <c>bind</c> and <c>catchBind</c>, this function applies the <c>success</c> continuation when the input promise resolves successfully, or <c>fail</c> continuation when the input promise fails.
/// <example>
/// <code lang="fsharp">
/// somePromise
/// |> Promise.eitherBind
///     (fun x -> string x |> Promise.lift)
///     (fun err -> Promise.lift err.Message)
/// |> Promise.map ...
/// </code>
/// </example>
/// </summary>
/// <param name="success">Binder function to call if the input promise succeeds</param>
/// <param name="fail">Binder function to call if the input promise fail</param>
/// <param name="pr">The input promise</param>
/// <typeparam name="'T1"></typeparam>
/// <typeparam name="'T2"></typeparam>
/// <returns>A new promise which is the result of calling <c>success</c> if <c>pr</c> succeedes, or of <c>fail</c> if <c>pr</c> failed</returns>
let eitherBind (success: 'T1 -> JS.Promise<'T2>) (fail: exn -> JS.Promise<'T2>) (pr: JS.Promise<'T1>): JS.Promise<'T2> = jsNative

[<Emit("void ($2.then($0).catch($1))")>]
/// <summary>
/// Same as <c>Promise.either</c> but stopping the promise execution.
/// <example>
/// <code lang="fsharp">
/// somePromise
/// |> Promise.eitherEnd
///     (fun x -> string x)
///     (fun err -> Promise.lift err.Message)
/// </code>
/// </example>
/// </summary>
/// <param name="success">Binder function to call if the input promise succeeds</param>
/// <param name="fail">Binder function to call if the input promise fail</param>
/// <param name="pr">The input promise</param>
/// <typeparam name="'T"></typeparam>
let eitherEnd (success: 'T -> unit) (fail: exn -> unit) (pr: JS.Promise<'T>): unit = jsNative

[<Emit("void $0")>]
/// <summary>
/// Start a promise.
///
/// Fake starting a promise. It is faking it because promise are hot meaning they execute directly after their creation.
///
/// <c>Promise.start</c> is equivalent to <c>promise |> ignore</c>
///
/// <example>
/// <code lang="fsharp">
/// myPromise
/// |> Promise.start
/// </code>
/// </example>
/// </summary>
/// <param name="pr">The input promise</param>
/// <typeparam name="'T"></typeparam>
let start (pr: JS.Promise<'T>): unit = jsNative

[<Emit("$1.catch($0)")>]
/// <summary>
/// Same as <c>Promise.start</c> but forcing you to handle the rejected state.
/// <example>
/// <code lang="fsharp">
/// myPromise
/// |> Promise.tryStart
///     (fun error ->
///         // Do something on error
///     )
/// </code>
/// </example>
/// </summary>
/// <param name="fail">Function to apply if the promise fail</param>
/// <param name="pr">The input promise</param>
/// <typeparam name="'T"></typeparam>
let tryStart (fail: exn -> unit) (pr: JS.Promise<'T>): unit = jsNative

[<Emit("Promise.all($0)")>]
/// <summary>
/// Takes a sequence of promises as an input, and returns a single <c>Promise</c> that resolves to an array of the results of the input promises.
/// <example>
/// <code lang="fsharp">
/// let p1 =
///     promise {
///         do! Promise.sleep 100
///         return 1
///     }
/// let p2 =
///     promise {
///         do! Promise.sleep 200
///         return 2
///     }
/// let p3 =
///     promise {
///         do! Promise.sleep 300
///         return 3
///     }
///
/// Promise.Parallel [p1; p2; p3]
/// |> Promise.map (fun res ->
///     // res = [|1; 2; 3 |]
/// )
/// |> Promise.map ...
/// </code>
/// </example>
/// </summary>
/// <param name="pr">A list of promise to wait for</param>
/// <typeparam name="'T"></typeparam>
/// <returns>Return a new promise returning an array containing all the promise result</returns>
let Parallel (pr: seq<JS.Promise<'T>>): JS.Promise<'T[]> = jsNative

[<Emit("Promise.all($0)")>]
/// <summary>
/// Takes a sequence of promises as an input, and returns a single <c>Promise</c> that resolves to an array of the results of the input promises.
/// <example>
/// <code lang="fsharp">
/// let p1 =
///     promise {
///         do! Promise.sleep 100
///         return 1
///     }
/// let p2 =
///     promise {
///         do! Promise.sleep 200
///         return 2
///     }
/// let p3 =
///     promise {
///         do! Promise.sleep 300
///         return 3
///     }
///
/// Promise.all [p1; p2; p3]
/// |> Promise.map (fun res ->
///     // res = [|1; 2; 3 |]
/// )
/// |> Promise.map ...
/// </code>
/// </example>
///
/// Note: If you need to return mixed types you can use boxing and unboxing
///
/// <example>
/// <code lang="fsharp">
/// let users =
///     promise {
///         let! users = fetchUsers ()
///         return box users
///     }
/// let posts =
///     promise {
///         let! posts = fetchPosts ()
///         return box posts
///     }
///
/// Promise.all [p1; p2]
/// |> Promise.map (fun res ->
///     let users = unbox&lt;User list&gt; res.[0]
///     let posts = unbox&lt;Post list&gt; res.[1]
///     // ...
/// )
/// </code>
/// </example>
/// </summary>
/// <param name="pr">A list of promise to wait for</param>
/// <typeparam name="'T"></typeparam>
/// <returns>Return a new promise returning an array containing all the promise result</returns>
let all (pr: seq<JS.Promise<'T>>): JS.Promise<'T[]> = jsNative


/// <summary>
/// Map the <c>Promise</c> result into a <c>Result</c> type.
/// <example>
/// <code lang="fsharp">
// Success example
/// Promise.lift 42
/// |> Promise.result
/// |> Promise.map (fun value ->
///     // value = Ok 42
/// )
///
/// // Fail example
/// Promise.reject "Invalid value"
/// |> Promise.result
/// |> Promise.map (fun value ->
///     // value = Error "Invalid value"
/// )
/// </code>
/// </example>
/// </summary>
/// <param name="a">The input promise</param>
/// <typeparam name="'T"></typeparam>
/// <returns>A promise returning <c>Ok</c> if the input promise succeed, <c>Error</c> if the input promise failed</returns>
let result (a: JS.Promise<'T>): JS.Promise<Result<'T, exn>> =
    either Ok Error a

/// <summary>
/// Evaluates to `myPromise |> Promise.map (Result.map fn)`
/// <example>
/// <code lang="fsharp">
/// Promise.lift 42
/// |> Promise.result
/// |> Promise.mapResult (fun value ->
///     value + 10
/// )
/// |> Promise.map (fun value ->
///     // value = Ok 52
/// )
/// </code>
/// </example>
/// </summary>
/// <param name="fn">The mapping function</param>
/// <param name="a">The input promise</param>
/// <typeparam name="'T1"></typeparam>
/// <typeparam name="'T2"></typeparam>
/// <typeparam name="'E"></typeparam>
/// <returns>A promise returning the result of applying the mapping function to the input promise result</returns>
let mapResult (fn: 'T1 -> 'T2) (a: JS.Promise<Result<'T1, 'E>>): JS.Promise<Result<'T2, 'E>> =
    a |> map (Result.map fn)


/// <summary>
/// Transform the success part of a result promise into another promise.
/// <example>
/// <code lang="fsharp">
/// let multiplyBy2 (value : int) =
///     Promise.create (fun resolve reject ->
///         resolve (value * 2)
///     )
///
/// Promise.lift 42
/// |> Promise.result
/// |> Promise.bindResult (fun value ->
///     multiplyBy2 value
/// )
/// |> Promise.map (fun value ->
///     // value = Ok 84
/// )
/// </code>
/// </example>
/// </summary>
/// <param name="fn">The binder function</param>
/// <param name="a">The input promise</param>
/// <typeparam name="'T1"></typeparam>
/// <typeparam name="'T2"></typeparam>
/// <typeparam name="'E"></typeparam>
/// <returns>Returns a new promise applying to the binder function to it if the input promise succeed</returns>
let bindResult (fn: 'T1 -> JS.Promise<'T2>) (a: JS.Promise<Result<'T1, 'E>>): JS.Promise<Result<'T2, 'E>> =
    a |> bind (fun a ->
        match a with
        | Ok a ->
            fn a |> map Ok
        | Error e ->
            lift (Error e))

/// <summary>
/// Evaluates to <c>myPromise |> Promise.map (Result.map fn)</c>
/// <example>
/// <code lang="fsharp">
/// Promise.reject -1
/// |> Promise.result
/// |> Promise.mapResultError (fun value ->
///     $"%s{value} is not a valid value"
/// )
/// |> Promise.map (fun value ->
///     // value = Error "-1 is not a valid value"
/// )
/// </code>
/// </example>
/// </summary>
/// <param name="fn">A mapper function</param>
/// <param name="a">The input promise</param>
/// <typeparam name="'E1"></typeparam>
/// <typeparam name="'E2"></typeparam>
/// <typeparam name="'T"></typeparam>
/// <returns>A promise returning the result of applying the mapper function to the input promise in case of error, otherwise the result of the input promise as it is</returns>
let mapResultError (fn: 'E1 -> 'E2) (a: JS.Promise<Result<'T, 'E1>>): JS.Promise<Result<'T, 'E2>> =
    a |> map (Result.mapError fn)

/// <summary>
/// A builder to provide promise Computation Expression support.
///
/// The CE is available via <c>promise { ... }</c>
/// <example>
/// <code lang="fsharp">
/// let double (value : int) =
///     promise {
///         return value * 2
///     }
/// </code>
/// </example>
/// </summary>
type PromiseBuilder() =
    [<Emit("$1.then($2)")>]
    member _.Bind(p: JS.Promise<'T1>, f: 'T1 -> JS.Promise<'T2>): JS.Promise<'T2> = jsNative

    [<Emit("$1.then(() => $2)")>]
    member _.Combine(p1: JS.Promise<unit>, p2: JS.Promise<'T>): JS.Promise<'T> = jsNative

    member _.For(seq: seq<'T>, body: 'T -> JS.Promise<unit>): JS.Promise<unit> =
        // (lift (), seq)
        // ||> Seq.fold (fun p a ->
        //     bind (fun () -> body a) p)
        let mutable p = lift ()
        for a in seq do
            p <- p |> bind (fun () -> body a)
        p

    [<Emit("$1.then($2)")>]
    member _.For(p: JS.Promise<'T1>, f: 'T1 -> JS.Promise<'T2>): JS.Promise<'T2> = jsNative

    member this.While(guard: unit -> bool, p: JS.Promise<unit>): JS.Promise<unit> =
        if guard()
        then bind (fun () -> this.While(guard, p)) p
        else lift()

    [<Emit("Promise.resolve($1)")>]
    member _.Return(a: 'T): JS.Promise<'T> = jsNative

    [<Emit("$1")>]
    member _.ReturnFrom(p: JS.Promise<'T>): JS.Promise<'T> = jsNative

    [<Emit("Promise.resolve()")>]
    member _.Zero(): JS.Promise<unit> = jsNative

    member _.TryFinally(p: JS.Promise<'T>, compensation: unit -> unit): JS.Promise<'T> =
        either (fun (x: 'T) -> compensation(); x) (fun er -> compensation(); raise er) p

    [<Emit("$1.catch($2)")>]
    member _.TryWith(p: JS.Promise<'T>, catchHandler: exn -> JS.Promise<'T>): JS.Promise<'T> = jsNative

    // Delay must generate a cold promise-like object that re-runs every time it's called,
    // so we cannot use the JS Promise constructor which is stateful
    member _.Delay(generator: unit -> JS.Promise<'T>): JS.Promise<'T> =
        !!createObj[
            "then" ==> fun onSuccess onError ->
                try generator().``then``(onSuccess, onError)
                with er ->
                    if isNull(box onError) then reject er
                    else
                        try onError er |> lift
                        with er -> reject er
            "catch" ==> fun onError ->
                try generator().catch(onError)
                with er ->
                    try onError er |> lift
                    with er -> reject er
        ]

    // Make sure we call `then` because this may be used with "cold" fake promises generated by Delay
    member _.Run(p:JS.Promise<'T>): JS.Promise<'T> = p.``then``(id)

    member this.Using<'T1, 'T2 when 'T1 :> IDisposable>(resource: 'T1, binder: 'T1 -> JS.Promise<'T2>): JS.Promise<'T2> =
        this.TryFinally(binder(resource), fun () -> resource.Dispose())

    [<Emit("Promise.all([$1, $2])")>]
    member _.MergeSources(a: JS.Promise<'T1>, b: JS.Promise<'T2>): JS.Promise<'T1 * 'T2> = jsNative

    [<Emit("Promise.all([$1, $2, $3])")>]
    member _.MergeSources3(a: JS.Promise<'T1>, b: JS.Promise<'T2>, c: JS.Promise<'T3>): JS.Promise<'T1 * 'T2 * 'T3> = jsNative

    [<Emit("Promise.all([$1, $2, $3, $4])")>]
    member _.MergeSources4(a: JS.Promise<'T1>, b: JS.Promise<'T2>, c: JS.Promise<'T3>, d: JS.Promise<'T4>): JS.Promise<'T1 * 'T2 * 'T3 * 'T4> = jsNative

    [<Emit("Promise.all([$1, $2, $3, $4, $5])")>]
    member _.MergeSources5(a: JS.Promise<'T1>, b: JS.Promise<'T2>, c: JS.Promise<'T3>, d: JS.Promise<'T4>, e: JS.Promise<'T5>): JS.Promise<'T1 * 'T2 * 'T3 * 'T4 * 'T5> = jsNative

    [<Emit("Promise.all([$1, $2, $3, $4, $5, $6])")>]
    member _.MergeSources6(a: JS.Promise<'T1>, b: JS.Promise<'T2>, c: JS.Promise<'T3>, d: JS.Promise<'T4>, e: JS.Promise<'T5>, f: JS.Promise<'T6>): JS.Promise<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6> = jsNative

//    member _.BindReturn(y: JS.Promise<'T1>, f) = map f y

    [<Emit("Promise.all([$1,$2]).then(([a,b]) => $3(a,b))")>]
    [<CustomOperation("andFor", IsLikeZip=true)>]
    member _.Merge(a: JS.Promise<'T1>, b: JS.Promise<'T2>, [<ProjectionParameter>] resultSelector : 'T1 -> 'T2 -> 'R): JS.Promise<'R> = jsNative
