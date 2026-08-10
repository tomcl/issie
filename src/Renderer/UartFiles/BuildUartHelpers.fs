/// The debug UART, as the draw block sees it.
///
/// These were direct calls into IS-uart.js, imported into the renderer bundle. That file requires
/// the native `usb` module, which a renderer without node integration cannot load at all, so it now
/// lives in the main process and these are messages to it - see src/Main/Bridge.fs.
///
/// The signatures are unchanged, which is the whole reason this port was cheap: the operations were
/// already asynchronous, already took nothing but a viewer count, and already shared no objects with
/// their caller. Nothing in SheetUpdate.fs had to change.
module BuildUartHelpers

open Fable.Core

let pause () : unit = Bridge.uartPause ()

let continuedOp () : unit = Bridge.uartContinue ()

let connect (numberOfViewers: int) : JS.Promise<string array> =
    Bridge.uartConnectAndRead numberOfViewers

let simpleConnect () : JS.Promise<unit> = Bridge.uartSimpleConnect ()

let disconnect () : unit = Bridge.uartDisconnect ()

let step () : unit = Bridge.uartStep ()

let readAllViewers (numberOfViewers: int) : JS.Promise<string array> =
    Bridge.uartReadAllViewers numberOfViewers

let stepAndReadAllViewers (numberOfViewers: int) : JS.Promise<string array> =
    Bridge.uartStepAndReadAllViewers numberOfViewers
