module HMR.Vite

open Fable.Core

[<StringEnum>]
type Status =
    /// The process is waiting for a call to check (see below)
    | [<CompiledName("idle")>] Idle
    /// The process is checking for updates
    | [<CompiledName("check")>] Check
    /// The process is getting ready for the update (e.g. downloading the updated module)
    | [<CompiledName("prepare")>] Prepare
    /// The update is prepared and available
    | [<CompiledName("ready")>] Ready
    /// The process is calling the dispose handlers on the modules that will be replaced
    | [<CompiledName("dispose")>] Dispose
    /// The process is calling the accept handlers and re-executing self-accepted modules
    | [<CompiledName("apply")>] Apply
    /// An update was aborted, but the system is still in it's previous state
    | [<CompiledName("abort")>] Abort
    /// An update has thrown an exception and the system's state has been compromised
    | [<CompiledName("fail")>] Fail

type ApplyOptions =
    /// Ignore changes made to unaccepted modules.
    abstract ignoreUnaccepted : bool option with get, set
    /// Ignore changes made to declined modules.
    abstract ignoreDeclined : bool option with get, set
    /// Ignore errors throw in accept handlers, error handlers and while reevaluating module.
    abstract ignoreErrored : bool option with get, set
    /// Notifier for declined modules
    abstract onDeclined : (obj -> unit) option with get, set
    /// Notifier for unaccepted modules
    abstract onUnaccepted : (obj -> unit) option with get, set
    /// Notifier for accepted modules
    abstract onAccepted : (obj -> unit) option with get, set
    /// Notifier for disposed modules
    abstract onDisposed : (obj -> unit) option with get, set
    /// Notifier for errors
    abstract onErrored : (obj -> unit) option with get, set


// Vite seems to be very strict on how import.meta.hot is invoked
// so make sure Fable doesn't surround it with parens
[<AllowNullLiteral>]
type IHot =

    [<Emit("import.meta.hot")>]
    abstract active: bool

    [<Emit("import.meta.hot.data")>]
    abstract data: obj option

    [<Emit("import.meta.hot.data[$1]")>]
    abstract getData: key: string -> obj

    [<Emit("import.meta.hot.data[$1] = $2")>]
    abstract setData: key: string * obj -> unit

    /// **Description**
    /// Accept updates for itself.
    /// **Parameters**
    ///   * `errorHandler` - parameter of type `(obj -> unit) option` - Function to fire when the dependencies are updated
    ///
    /// **Output Type**
    ///   * `unit`
    ///
    /// **Exceptions**
    ///
    [<Emit("import.meta.hot.accept($1...)")>]
    abstract accept: ?handler: (obj -> unit) -> unit

    /// Add a handler which is executed when the current module code is replaced.
    /// This should be used to remove any persistent resource you have claimed or created.
    /// If you want to transfer state to the updated module, add it to given `data` parameter.
    /// This object will be available at `module.hot.data` after the update.
    [<Emit("import.meta.hot.dispose($1...)")>]
    abstract dispose: (obj -> unit) -> unit

    /// Add a handler which is executed when the current module code is replaced.
    /// This should be used to remove any persistent resource you have claimed or created.
    /// If you want to transfer state to the updated module, add it to given `data` parameter.
    /// This object will be available at `module.hot.data` after the update.
    [<Emit("import.meta.hot.dispose($1...)")>]
    abstract dispose: obj -> unit

    [<Emit("import.meta.hot.invalidate()")>]
    abstract invalidate: unit -> unit

[<Global>]
let hot : IHot = jsNative

[<Emit("(import.meta.hot /* If error see https://github.com/elmish/hmr/issues/35 */)")>]
let active : bool = jsNative
