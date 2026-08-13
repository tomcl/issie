module MemoryData

(*
    MemoryData.fs

    What a memory's contents must satisfy to be the contents OF that memory: every address inside
    the 2^AddressWidth locations it has, and every word inside its WordWidth bits.

    This is asked in three places, and they are three because a memory's widths can be parameters.

      - when the contents are EDITED, or a .ram file is linked, against every width the memory has
        anywhere in the design. A memory on a parameterised sheet has one width per set of bindings
        the sheet is used at, so data that fits the sheet as drawn may not fit an instance of it,
        and the edit is refused rather than quietly making one instance wrong.
      - when a WIDTH is edited, nothing is checked and nothing is thrown away. The data is what the
        user typed; the width is what they are typing now, and a memory passing through a size that
        does not hold its data is an ordinary step on the way to one that does.
      - when the design is SIMULATED, against the widths that instance resolved to. That is the
        point at which a memory has one definite shape, so it is the point at which contents that
        do not fit it are an error rather than a state being passed through.
*)

open CommonTypes

/// One location, and what is wrong with holding it in a memory of these widths.
///
/// Addresses and words are unsigned: a memory location holds a bit pattern, and the sign of the
/// number Issie displays for it is a matter of how the pattern is read, not of what is stored.
let locationProblem (addressWidth: int) (wordWidth: int) (address: bigint) (value: bigint) : string option =
    let locations = 1I <<< addressWidth
    let words = 1I <<< wordWidth
    match address, value with
    | a, _ when a.Sign < 0 -> Some $"address {a} is negative"
    | a, _ when a >= locations ->
        Some $"address {a} is outside the {locations} locations of a {addressWidth}-bit address"
    | _, v when v.Sign < 0 -> Some $"the value {v} at address {address} is negative"
    | _, v when v >= words ->
        Some $"the value {v} at address {address} does not fit in {wordWidth}-bit words"
    | _ -> None

/// What is wrong with holding `data` in a memory of these widths, said as one sentence.
///
/// The lowest offending address is the one named, so that the same data reported twice reports the
/// same location, and the count says how much else is wrong - naming every one of them would be a
/// list as long as the memory.
let dataProblem (addressWidth: int) (wordWidth: int) (data: Map<bigint, bigint>) : string option =
    let problems =
        data
        |> Map.toList
        |> List.sortBy fst
        |> List.choose (fun (address, value) ->
            locationProblem addressWidth wordWidth address value)
    match problems with
    | [] -> None
    | [ only ] -> Some only
    | first :: rest -> Some $"{first} ({List.length rest} other location(s) do not fit either)"

/// What is wrong with the contents a memory is carrying, at its own widths.
let memoryProblem (mem: Memory1) : string option =
    dataProblem mem.AddressWidth mem.WordWidth mem.Data

/// What is wrong with holding `data` in a memory that is EVERY one of these shapes.
///
/// One memory component has several shapes when its widths are parameters and the sheet it is on
/// is used at several sets of values. Data has to fit all of them - it is one map, copied into
/// every instance - so the shape that fails is the one reported, named so that the message points
/// at a size the user can see in the properties pane rather than at "some instance".
let dataProblemAtWidths (widths: (int * int) list) (data: Map<bigint, bigint>) : string option =
    widths
    |> List.distinct
    // the narrowest first, so that the tightest shape is the one whose message is shown: data too
    // wide for the smallest is usually too wide for it by the most
    |> List.sortBy (fun (addressWidth, wordWidth) -> addressWidth + wordWidth)
    |> List.tryPick (fun (addressWidth, wordWidth) ->
        dataProblem addressWidth wordWidth data
        |> Option.map (fun problem ->
            match List.length (List.distinct widths) with
            | 1 -> problem
            | _ ->
                $"{problem}. This memory is {addressWidth} address bits by {wordWidth} data bits in \
                  at least one instance of this sheet"))

/// What is wrong with adding this one location to a memory that is every one of these shapes.
/// Written separately from dataProblemAtWidths so that editing one location reports that location
/// rather than the lowest bad address of the whole memory.
let locationProblemAtWidths (widths: (int * int) list) (address: bigint) (value: bigint) : string option =
    dataProblemAtWidths widths (Map [ address, value ])
