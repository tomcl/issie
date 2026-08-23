/// The SendDesign payload and the per-sheet decode cache behind it.
///
/// A design arrives ONE SHEET PER MESSAGE, each framed as length-prefixed UTF-8 strings - the top
/// sheet name, then that sheet's CommonTypes.SimpleSheet JSON - preceded by which sheet of how
/// many it is. One sheet rather than one design because decoding is the cost and a handler must be
/// bounded: the whole 18-sheet 3cpu design decodes in ~300ms, its largest single sheet in ~25ms,
/// and a handler occupies the sidecar's serial loop for as long as it runs.
///
/// The cache is keyed by the exact JSON string: content-addressed with string equality as the
/// check, so there is no hash to agree on and no collision to reason about. An edit touches one
/// sheet, so in the steady state a design costs one sheet's decode and seventeen string
/// comparisons.
///
/// The renderer's framing half is SidecarClient.packStrings; the two change together.
module Issie.Sidecar.DesignCache

open System
open CommonTypes

/// Split the string part of a SendDesign payload: for each, a uint32 little-endian byte length
/// then that many bytes of UTF-8. The first string is the top sheet name, the rest are sheets -
/// exactly one of them, now that sheets arrive one per message.
let parsePayload (payload: byte array) : Result<string * string list, string> =
    let rec strings offset acc =
        if offset = payload.Length then
            Ok(List.rev acc)
        elif offset + 4 > payload.Length then
            Error $"sidecar: truncated design frame at byte {offset}"
        else
            let len = BitConverter.ToInt32(payload, offset)

            if len < 0 || offset + 4 + len > payload.Length then
                Error $"sidecar: bad string length {len} at byte {offset} of the design frame"
            else
                strings (offset + 4 + len) (Text.Encoding.UTF8.GetString(payload, offset + 4, len) :: acc)

    match strings 0 [] with
    | Ok(topSheet :: sheetJsons) -> Ok(topSheet, sheetJsons)
    | Ok [] -> Error "sidecar: empty design frame"
    | Error e -> Error e

/// Decode one sheet through the cache: a sheet whose JSON is identical to one already decoded is
/// reused as it is. Returns the sheet, whether it had to be decoded, and the cache with it in.
///
/// The cache GROWS across an upload and is pruned when the upload completes - see
/// `keepOnly`. It cannot be pruned per sheet, because the sheets not yet sent are exactly the ones
/// whose cached copies are about to be wanted.
let decodeSheet
    (cache: Map<string, SimpleSheet>)
    (json: string)
    : Result<SimpleSheet * bool * Map<string, SimpleSheet>, string> =
    match Map.tryFind json cache with
    | Some sheet -> Ok(sheet, false, cache)
    | None ->
        match SimpleJsonDotNet.tryDeserialise<SimpleSheet> json with
        | Ok sheet -> Ok(sheet, true, Map.add json sheet cache)
        | Error e -> Error $"sheet did not decode: {e}"

/// The cache holding exactly the given sheets, so memory stays bounded by one design and a sheet
/// that vanishes from the design vanishes from the cache. Applied when an upload completes.
let keepOnly (jsons: string list) (cache: Map<string, SimpleSheet>) : Map<string, SimpleSheet> =
    let wanted = Set.ofList jsons
    cache |> Map.filter (fun json _ -> Set.contains json wanted)
