(*
    NumberHelpers.fs

    A collection of functions that allow to covert numbers into various
    representations.
*)

module NumberHelpers

open Helpers
open SimulatorTypes

// -- padding

/// Pad a string of bits to the given width.
let padBitsToWidth width (bitsStr : string) : string =
    (String.replicate (width - bitsStr.Length) "0") + bitsStr

/// Pad wireData with Zeros as the Most Significant Bits (e.g. at position 0).
let padToWidth (bits : WireData) width : WireData =
    List.replicate (width - bits.Length) Zero @ bits

// -- conversions

/// Convert an int into a bit list with the provided width. The Most Significant
/// Bits are the one with low index (e.g. MSB is at position 0, LSB is at
/// position N).
/// Return an error if the integer represents a value with too many bits.
let rec convertIntToWireData (num : int64) width : Result<WireData, string> =
    let toBit = function | 0 -> Zero | 1 -> One | _ -> failwith "toBit only accepts 0 or 1"
    let rec intToBinary (i : int64) =
        match int i with
        | 0 | 1 -> [toBit <| int i]
        | _ -> let bit = toBit <| int (i % (int64 2))
               bit :: (intToBinary (i / (int64 2)))
    let bits = List.rev <| intToBinary num
    match bits.Length <= width with
    | true -> Ok <| padToWidth bits width
    | false -> Error <| sprintf "Too many bits. Expected up to %d but got %d." width bits.Length

/// Convert a bit to string.
let bitToString (bit : Bit) : string =
    match bit with Zero -> "0" | One -> "1"

/// Convert a list of bits to a string.
let rec bitsToString (bits : WireData) : string =
    match bits with
    | [] -> ""
    | bit :: bits' -> (bitToString bit) + (bitsToString bits')

/// Try to convert a string of bits into a Bit list. Return None if such
/// conversion is not possible.
let stringToBits (bitsStr : string) : WireData option =
    let rec convert (bitChars : char list) : WireData option =
        match bitChars with
        | [] -> Some []
        | bitChar :: bitChars' when bitChar = '0' ->
            Option.map (fun bits -> Zero :: bits) (convert bitChars')
        | bitChar :: bitChars' when bitChar = '1' ->
            Option.map (fun bits -> One :: bits) (convert bitChars')
        | _ -> None
    convert <| Seq.toList bitsStr

/// Try to convert a string to an int, or return an error message if that was
/// not possible.
let strToInt (str : string) : Result<int64, string> =
    try
        Ok <| int64 str
    with
        | e -> Error <| e.ToString ()

/// Convert a list of Bits into an int. The Most Significant Bits are the one
/// with low index (e.g. MSB is at position 0, LSB is at position N).
let rec convertWireDataToInt (bits : WireData) : int64 =
    let mag = (bits.Length - 1)
    match bits with
    | [] -> int64 0
    | Zero :: bits' -> convertWireDataToInt bits'
    | One :: bits' -> pow2int64(mag) + convertWireDataToInt bits'
