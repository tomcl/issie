/// The `electron` module itself, for the main process only.
///
/// These bindings used to live in ElectronAPI.fs, which both projects compile - the renderer needs
/// its several hundred type definitions. But a binding is an import, and `import * from "electron"`
/// runs when the file loads, so the renderer was requiring electron simply by using its types. A
/// renderer without node integration cannot do that, and it is not a lazy failure: it throws as the
/// bundle loads, before any of Issie runs.
///
/// So the types stayed shared and the module came here, where requiring it is exactly right. If the
/// renderer ever appears to need one of these values again, that is the signal that something wants
/// a bridge operation instead.
[<AutoOpen>]
module ElectronModule

open Fable.Core
open ElectronAPI

let [<ImportAll("electron")>] common: Common.IExports = jsNative
let electron = common

let [<ImportAll("electron")>] main: Electron.IExports = jsNative
let mainProcess = main

[<ImportAll("electron")>]
let renderer: Renderer.IExports = jsNative
