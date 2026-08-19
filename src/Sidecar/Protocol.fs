/// The wire protocol between Issie's renderer and this sidecar, in full.
///
/// Every WebSocket message is binary, and one message is one request or one response:
///
///     byte 0        command; a response carries the request's command with ResponseFlag set
///     bytes 1..4    correlation id, uint32 little-endian - chosen by the renderer, echoed
///                   back unchanged, and how the renderer matches a response to its request
///     bytes 5..     payload
///
/// There is no length field: a WebSocket message is self-delimiting. The renderer's half of
/// this contract is src/Renderer/Interface/SidecarClient.fs - the two files change together.
///
/// The three commands exist to measure the channel, one direction at a time:
///   Echo      payload comes back verbatim              - symmetric round trip
///   Upload    payload discarded, header-only response  - renderer-to-sidecar
///   Download  payload is a 4-byte LE byte count N,
///             response carries N bytes                 - sidecar-to-renderer
module Issie.Sidecar.Protocol

[<Literal>]
let Echo = 0x01uy

[<Literal>]
let Upload = 0x02uy

[<Literal>]
let Download = 0x03uy

[<Literal>]
let ResponseFlag = 0x80uy

/// Command byte plus the four bytes of correlation id.
[<Literal>]
let HeaderSize = 5

/// 64MB. Nothing the latency test sends is near it; anything larger is a protocol error.
[<Literal>]
let MaxMessage = 67108864
