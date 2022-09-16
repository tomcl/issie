module BuildUartHelpers

open Fable.Core

[<Emit("import {pauseOp,continuedOp,connectAndRead,simpleConnect,disconnect,step,readAllViewers,stepAndReadAllViewers} from '../UartFiles/IS-uart.js' ")>]
let importReadUart : unit = jsNative

[<Emit("pauseOp()")>]
let pause (): unit = jsNative

[<Emit("continuedOp()")>]
let continuedOp (): unit = jsNative


[<Emit("connectAndRead($0)")>]
let connect (numberOfViewers:int): JS.Promise<string array> = jsNative

[<Emit("simpleConnect($0)")>]
let simpleConnect (): JS.Promise<unit> = jsNative

[<Emit("disconnect()")>]
let disconnect (): unit = jsNative

[<Emit("step()")>]
let step (): unit = jsNative

[<Emit("readAllViewers($0)")>]
let readAllViewers (numberOfViewers:int): JS.Promise<string array> = jsNative

[<Emit("stepAndReadAllViewers($0)")>]
let stepAndReadAllViewers (numberOfViewers:int): JS.Promise<string array> = jsNative