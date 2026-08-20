/// The wire protocol between Issie's renderer and this sidecar, in full.
///
/// Every WebSocket message is binary, and one message is one request or one response:
///
///     byte 0        command; a response carries the request's command with ResponseFlag set
///     bytes 1..4    correlation id, uint32 little-endian - chosen by the renderer, echoed
///                   back unchanged, and how the renderer matches a response to its request
///     bytes 5..7    padding, always zero - the header is 8 bytes so that a BINARY response
///                   payload starts 8-aligned and the renderer can overlay Uint32Array or
///                   Float64Array views on the received buffer with no copy
///     bytes 8..     payload
///
/// There is no length field: a WebSocket message is self-delimiting. The renderer's half of
/// this contract is src/Renderer/Interface/SidecarClient.fs - the two files change together.
///
/// The three commands exist to measure the channel, one direction at a time:
///   Echo      payload comes back verbatim              - symmetric round trip
///   Upload    payload discarded, header-only response  - renderer-to-sidecar
///   Download  payload is a 4-byte LE byte count N,
///             response carries N bytes                 - sidecar-to-renderer
///
/// SendDesign carries real cargo rather than measuring. Its payload is length-prefixed UTF-8
/// strings (uint32 LE byte count, then the bytes, repeated): first the top sheet name, then one
/// CommonTypes.SimpleSheet JSON per sheet - per-sheet rather than one design JSON so the
/// receiver can cache decoded sheets and skip unchanged ones (DesignCache.fs). The renderer
/// encodes with the vendored SimpleJson serializer, this side decodes with SimpleJsonDotNet.
/// The response payload is a small JSON object with what was received, how many sheets needed
/// decoding, and how long that took.
module Issie.Sidecar.Protocol

[<Literal>]
let Echo = 0x01uy

[<Literal>]
let Upload = 0x02uy

[<Literal>]
let Download = 0x03uy

[<Literal>]
let SendDesign = 0x04uy

/// Build a simulation of the last-sent design's top sheet. Payload: uint32 LE maxArraySize.
/// Reply: JSON build report or error.
[<Literal>]
let SimBuild = 0x05uy

/// Run the built simulation towards a target cycle within a time budget. Payload: uint32 LE
/// target cycle, uint32 LE timeout ms (0 = unbounded). Reply: JSON {clockTick, done, ms}.
/// The caller chunks: repeat until done, cancel by not sending the next chunk - the same
/// contract the renderer's own progress loop uses.
[<Literal>]
let SimRun = 0x06uy

/// The deterministic-stimulus digest text of the last-sent design. Payload: uint32 LE ticks.
/// Reply: the raw render text (an error reply starts with '{').
[<Literal>]
let SimDigest = 0x07uy

/// Drop the simulation session. Empty payload. Reply: JSON.
[<Literal>]
let SimEnd = 0x08uy

/// The SimLog ring - one record per simulation build and per run invocation - as a JSON array,
/// the same shape the renderer's DevHarness simLog command returns for its own runtime.
/// Empty payload.
[<Literal>]
let SimLog = 0x09uy

/// Set top-level input values on the built simulation at a cycle. Payload: uint32 LE cycle,
/// uint32 LE count, then per input uint32 LE component id, uint32 LE value low word, uint32 LE
/// value high word. Reply: JSON.
[<Literal>]
let SimSetInputs = 0x0Auy

/// Read a window of output step data from the built simulation, as binary. Payload: uint32 LE
/// start cycle, uint32 LE cycle count, uint32 LE item count, then per item uint32 LE component
/// id, uint32 LE output port number, uint32 LE access-path length, then that many uint32 LE
/// path component ids (root first). Reply payload on success: uint32 LE item count, uint32 LE
/// cycle count, then item-major uint32 LE values - so values start at byte 16 of the frame,
/// 8-aligned for a zero-copy Uint32Array view. Signals wider than 32 bits are refused for now.
/// An error reply is JSON text (starts with '{').
[<Literal>]
let SimRead = 0x0Buy

[<Literal>]
let ResponseFlag = 0x80uy

/// Command byte, four bytes of correlation id, three bytes of padding: 8, so that binary
/// response payloads start 8-aligned for zero-copy typed-array views on the renderer side.
[<Literal>]
let HeaderSize = 8

/// 64MB. Nothing the latency test sends is near it; anything larger is a protocol error.
[<Literal>]
let MaxMessage = 67108864
