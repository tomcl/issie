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
/// SendDesign carries real cargo rather than measuring, and carries it ONE SHEET PER MESSAGE.
/// Payload: uint32 LE sheet index, uint32 LE sheet count, then length-prefixed UTF-8 strings
/// (uint32 LE byte count, then the bytes) - the top sheet's name, then that one sheet's
/// CommonTypes.SimpleSheet JSON.
///
/// One sheet because decoding is the cost and this side handles one message at a time: the whole
/// 18-sheet 3cpu design decodes in ~300ms against ~25ms for its largest single sheet, and a
/// handler holds the serve loop for as long as it runs. Per-sheet framing also lets the receiver
/// cache decoded sheets and skip unchanged ones (DesignCache.fs).
///
/// Index 0 begins an upload, discarding any abandoned one AND the current simulation session: a
/// design is only ever sent with every simulation closed, so nothing is taken from a caller using
/// it, and afterwards a command left over from before the design changed names an epoch that no
/// longer exists. The sheets become a design when the last of `count` has arrived, which the reply
/// reports as "complete".
///
/// The renderer encodes with the vendored SimpleJson serializer, this side decodes with
/// SimpleJsonDotNet.
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
/// Reply: JSON build report including the session `epoch` this build issued, or an error.
///
/// **Every command below that depends on a session carries that epoch**, and the sidecar refuses
/// one naming any other session. The renderer cannot see inside this process, so without it every
/// belief it holds about the session - that one exists, that it is of the design last sent, how far
/// its clock has run - is unverifiable, and a reply from a superseded simulation is indistinguish-
/// able from a reply from the current one. See docs/dev/sidecarInvariants.md, section C.
[<Literal>]
let SimBuild = 0x05uy

/// Run the built simulation towards a target cycle within a time budget. Payload: uint32 LE
/// epoch, uint32 LE target cycle, uint32 LE timeout ms (0 = unbounded). Reply: JSON
/// {epoch, clockTick, firstValidCycle, done, ms}. The caller chunks: repeat until done, cancel by
/// not sending the next chunk - the same contract the renderer's own progress loop uses.
///
/// firstValidCycle is the earliest cycle whose data is still correct. The step arrays are a
/// circular buffer, so a simulation run past its array length has overwritten its own beginning;
/// without this number "overwritten" and "not yet reached" are the same silence to the caller.
[<Literal>]
let SimRun = 0x06uy

/// The deterministic-stimulus digest text of the last-sent design. Payload: uint32 LE ticks.
/// Reply: the raw render text (an error reply starts with '{').
///
/// **Declared long**, like SimBuild: it builds and runs a simulation of its own. Unlike SimBuild
/// that is not a limitation to be lifted - this is a development and test command, used by
/// simCompare and by the golden-model tests to compare the two runtimes byte for byte, and
/// bounding it would refuse exactly the large designs a divergence hunt most wants to check. It
/// touches no session, so a long one cannot disturb a simulation - only occupy the serve loop.
[<Literal>]
let SimDigest = 0x07uy

/// Drop the simulation session. Payload: uint32 LE epoch. Reply: JSON.
[<Literal>]
let SimEnd = 0x08uy

/// The SimLog ring - one record per simulation build and per run invocation - as a JSON array,
/// the same shape the renderer's DevHarness simLog command returns for its own runtime.
/// Empty payload.
[<Literal>]
let SimLog = 0x09uy

/// Set top-level input values on the built simulation at a cycle. Payload: uint32 LE epoch,
/// uint32 LE cycle, uint32 LE count, then per input uint32 LE component id, uint32 LE value low
/// word, uint32 LE value high word. Reply: JSON.
[<Literal>]
let SimSetInputs = 0x0Auy

/// Read sampled output data from the built simulation, as binary: for each signal, `samples`
/// values taken every `rep` cycles from `start` - the same (StartCycle, SamplingZoom,
/// ShownCycles) parameters the waveform viewer's own generation uses, so a zoomed-out view is
/// one request with rep > 1, and a tooltip is the degenerate one-signal one-sample request.
/// Payload: uint32 LE epoch, uint32 LE start cycle, uint32 LE rep (cycles between samples, >= 1),
/// uint32 LE sample count, uint32 LE signal count, then per signal uint32 LE component id, uint32 LE
/// output port number, uint32 LE access-path length, then that many uint32 LE path component
/// ids (root first). Reply payload on success: uint32 LE signal count, uint32 LE sample count,
/// uint32 LE words per sample, four bytes of padding, then signal-major values, each `words`
/// uint32 LE words least significant first - so values start at byte 16 of the frame, 8-aligned
/// for a zero-copy Uint32Array view. Any width: the words per sample are read from the simulation
/// and stated in the reply, so a caller whose idea of a width is stale still reads what was sent.
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
