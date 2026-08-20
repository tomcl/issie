/// The SendDesign payload and the per-sheet decode cache behind it.
///
/// The renderer frames a design as length-prefixed UTF-8 strings - the top sheet name first,
/// then one CommonTypes.SimpleSheet JSON per sheet - rather than one SimpleDesign JSON, so that
/// this side can skip sheets it has already decoded. The cache is keyed by the exact JSON
/// string: content-addressed with string equality as the check, so there is no hash to agree on
/// and no collision to reason about. An edit touches one sheet, so in the steady state a design
/// costs one sheet's decode and seventeen string comparisons - measured at ~300ms for a full
/// 18-sheet decode against ~25ms for the largest single sheet.
///
/// The renderer's framing half is SidecarClient.packStrings; the two change together.
module Issie.Sidecar.DesignCache

open System
open CommonTypes

/// Split a SendDesign payload into its strings: for each, a uint32 little-endian byte length
/// then that many bytes of UTF-8. The first string is the top sheet name, the rest are sheets.
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

/// Decode sheets through the cache: a sheet whose JSON is identical to one already decoded is
/// reused as it is. Returns the sheets in order, how many actually needed decoding, and the new
/// cache - which holds exactly this design's sheets, so memory stays bounded by one design and
/// sheets that vanish from the design vanish from the cache.
let decodeSheets
    (cache: Map<string, SimpleSheet>)
    (sheetJsons: string list)
    : Result<SimpleSheet list * int * Map<string, SimpleSheet>, string> =
    (Ok([], 0, Map.empty), sheetJsons)
    ||> List.fold (fun state json ->
        state
        |> Result.bind (fun (sheets, decoded, newCache) ->
            match Map.tryFind json cache with
            | Some sheet -> Ok(sheet :: sheets, decoded, Map.add json sheet newCache)
            | None ->
                match SimpleJsonDotNet.tryDeserialise<SimpleSheet> json with
                | Ok sheet -> Ok(sheet :: sheets, decoded + 1, Map.add json sheet newCache)
                | Error e -> Error $"sheet did not decode: {e}"))
    |> Result.map (fun (sheets, decoded, newCache) -> List.rev sheets, decoded, newCache)
