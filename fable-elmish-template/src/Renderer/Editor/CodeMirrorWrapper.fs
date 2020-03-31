module CodeMirrorWrapper

open EditorTypes

open Fable.Core

[<Emit("
CodeMirror(document.getElementById($0), {
  lineNumbers: true,
  lineWrapping: true,
  matchBrackets: true,
  theme: \"mbo\", // Not working?
  mode: {name: \"verilog\"}
});")>]
let createEditor (id : string) : Editor = jsNative

[<Emit("$0.getValue()")>]
let getCode (editor : Editor) : string = jsNative
