module NearleyBindings

open Fable.Core
open Fable.Core.JsInterop


[<Import("default", from="nearley")>]

[<Emit("import * as verilogGrammar from '../VerilogComponent/VerilogGrammar.js' ")>]
let importGrammar : unit = jsNative

[<Emit("import {fix} from '../VerilogComponent/parser.js' ")>]
let importFix : unit = jsNative

[<Emit("import {parseFromFile} from '../VerilogComponent/parser.js' ")>]
let importParser : unit = jsNative

[<Emit("fix($0)")>]
let fix (data:string): string = jsNative

[<Emit("parseFromFile($0)")>]
let parseFromFile (source:string): string = jsNative