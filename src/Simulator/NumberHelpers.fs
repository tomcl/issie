(*
    NumberHelpers.fs

    A collection of functions that allow to covert numbers into various
    representations.
*)

module NumberHelpers

open Helpers
open SimulatorTypes

/// Convert an hex string into a binary string.
let private hexToBin (hStr : string) : string =
    let rec convert h =
        match h with
        | [] -> ""
        | c :: h' ->
            let digit =
                match c with
                | '0' -> "0000" | '1' -> "0001" | '2' -> "0010" | '3' -> "0011"
                | '4' -> "0100" | '5' -> "0101" | '6' -> "0110" | '7' -> "0111"
                | '8' -> "1000" | '9' -> "1001" | 'a' -> "1010" | 'b' -> "1011"
                | 'c' -> "1100" | 'd' -> "1101" | 'e' -> "1110" | 'f' -> "1111"
                | c -> failwithf "Invalid char %c while converting hex %s to binary" c hStr
            digit + (convert h')
    let chars = hStr.ToLower() |> Seq.toList
    match chars with
    | [] -> ""
    | c :: chars' ->
        let firstDigit = // Avoid leading zeros.
            match c with
            | '0' -> "0" | '1' -> "1" | '2' -> "10" | '3' -> "11"
            | '4' -> "100" | '5' -> "101" | '6' -> "110" | '7' -> "111"
            | '8' -> "1000" | '9' -> "1001" | 'a' -> "1010" | 'b' -> "1011"
            | 'c' -> "1100" | 'd' -> "1101" | 'e' -> "1110" | 'f' -> "1111"
            | c -> failwithf "Invalid char %c while converting hex %s to binary" c hStr
        firstDigit + (convert chars')

let hex64 (num : int64) = "0x" + num.ToString("X")
let bin64 (num : int64) = "0b" + (hexToBin <| num.ToString("X"))
let dec64 (num : int64) = num.ToString()
let hex (num : int) = hex64 <| int64 num
let bin (num : int) = bin64 <| int64 num
let dec (num : int) = dec64 <| int64 num

/// Convert a bit to string.
let bitToString (bit : Bit) : string =
    match bit with Zero -> "0" | One -> "1"

/// Pad wireData with Zeros as the Most Significant Bits (e.g. at position N).
let private padToWidth width (bits : WireData) : WireData =
    bits @ List.replicate (width - bits.Length) Zero

/// Convert an int into a Bit list with the provided width. The Least
/// Significant Bits are the one with low index (e.g. LSB is at position 0, MSB
/// is at position N). Little Endian.
/// If the number has more bits than width, then more bits will be returned.
let convertIntToWireData (width : int) (num : int64) : WireData =
    let toBit = function | 0 -> Zero | 1 -> One | _ -> failwith "toBit only accepts 0 or 1"
    let rec intToBinary (i : int64) =
        match int i with
        | 0 | 1 -> [toBit <| int i]
        | _ -> let bit = toBit <| int (i % (int64 2))
               bit :: (intToBinary (i / (int64 2)))
    padToWidth width (intToBinary num)

/// Convert a list of Bits into an int. The Least Significant Bits are the one
/// with low index (e.g. LSB is at position 0, MSB is at position N).
/// Little Endian.
let convertWireDataToInt (bits : WireData) : int64 =
    let rec convert bits idx =
        match bits with
        | [] -> int64 0
        | Zero :: bits' -> convert bits' (idx + 1)
        | One :: bits' -> pow2int64(idx) + convert bits' (idx + 1)
    convert bits 0

/// Try to convert a string to an int, or return an error message if that was
/// not possible.
let strToInt (str : string) : Result<int64, string> =
    try
        Ok <| int64 str
    with
        | _ -> Error <| "Invalid number."

let private countBits (num : int64) : int =
    (String.length <| bin64 num) - 2

/// Check a number is formed by at most <width> bits.
let private checkWidth (width : int) (num : int64) : string option =
    let bitsCount = countBits num
    match bitsCount <= width with
    | true -> None
    | false -> Some <| sprintf "Too many bits. Expected up to %d but got %d." width bitsCount

/// Convert a string to a number making sure that it has no more bits than
/// specified in width.
let strToIntCheckWidth (str : string) (width : int) : Result<int64, string> =
    strToInt str
    |> Result.bind (fun num ->
        match checkWidth width num with
        | None -> Ok num
        | Some err -> Error err
    )
