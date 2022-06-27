module NearleyBindings

open Fable.Core
open Fable.Core.JsInterop


[<Import("default", from="nearley")>]

[<Emit("import * as verilogGrammar from './verilog.js' ")>]
let importGrammar : unit = jsNative

[<Emit("import {fix} from './parser.js' ")>]
let importFix : unit = jsNative

[<Emit("import {parseFromFile} from './parser.js' ")>]
let importParser : unit = jsNative

// [<Emit("new nearley.Parser(nearley.Grammar.fromCompiled(verilogGrammar))")>]
// let newParser : obj = jsNative

// [<Emit("parser.feed($0)")>]
// let feedParser (data:string): unit = jsNative

[<Emit("fix($0)")>]
let fix (data:string): string = jsNative

// [<Emit("JSON.stringify(parser.results[0])")>]
// let parse : string = jsNative

[<Emit("parseFromFile($0)")>]
let parseFromFile (source:string): string = jsNative