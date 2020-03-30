module CodeMirrorWrapper

open EditorTypes

open Fable.Core
open Fable.Core.JsInterop

[<Emit("
CodeMirror(document.getElementById($0), {
  lineNumbers: true,
  lineWrapping: true,
  theme: 'mbo',
  mode: 'javascript'
});")>]
let createEditor (id : string) : Editor = jsNative
