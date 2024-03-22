module HMR.Parcel

open Fable.Core

// I wasn't able to find Parcel HMR API definition
// So this bindings is minimal and not complete

[<AllowNullLiteral>]
type IHot =

    /// Optional data coming from disposed module
    abstract data : obj option

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
    abstract accept: ?errorHandler: (obj -> unit) -> unit

    /// **Description**
    /// Add a handler which is executed when the current module code is replaced.
    /// This should be used to remove any persistent resource you have claimed or created.
    /// If you want to transfer state to the updated module, add it to given `data` parameter.
    /// This object will be available at `module.hot.data` after the update.
    /// **Parameters**
    ///   * `data` - parameter of type `obj`
    ///
    /// **Output Type**
    ///   * `unit`
    ///
    /// **Exceptions**
    ///
    abstract dispose: data: obj -> unit

[<Emit("module.hot")>]
let hot : IHot = jsNative

[<Emit("module.hot")>]
let active : bool = jsNative
