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

let addZeros64 (width:int) (pFun:int64 -> string) (n: int64) =
    let s = pFun n
    let bits = 
        match s.[1] with
        | 'x' -> 4
        | 'b' -> 1
        | _ -> failwithf "Wrong use of addZeros64: s = %s" s
    let extra = (width - (s.Length - 2)*bits) / bits
    s.[0..1] + String.replicate extra "0" + s.[2..]

let addZeros (width:int) (pFun:int -> string) (n: int) =
    let s = pFun n
    let bits = 
        match s.[1] with
        | 'x' -> 4
        | 'b' -> 1
        | _ -> failwithf "Wrong use of addZeros: s = %s" s
    let extra = ((width - (s.Length - 2))*bits + (2<<<bits - 1)) / bits
    s.[0..1] + String.replicate extra "0" + s.[2..]

let hex64 (num : int64) = "0x" + num.ToString("X")
let fillHex64 width = addZeros64 width hex64

let bin64 (num : int64) = "0b" + (hexToBin <| num.ToString("X"))
let sDec64 (num : int64) = num.ToString()
let dec64 (num: int64) = (uint64 num).ToString()

let hex (num : int) = hex64 <| int64 num
let fillHex width = addZeros width hex

let bin (num : int) = bin64 <| int64 num
let dec (num : int) = dec64 <| int64 num

let fillBin64 width = addZeros64 width bin64
let fillBin width = addZeros width bin

/// Convert a bit to string.
let bitToString (bit : Bit) : string =
    match bit with Zero -> "0" | One -> "1"

/// Pad wireData with Zeros as the Most Significant Bits (e.g. at position N).
let private padToWidth width (bits : WireData) : WireData =
    if bits.Length > width then List.truncate width bits
    else bits @ List.replicate (width - bits.Length) Zero

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
    if num >= 0L then
        padToWidth width (intToBinary num)
    else
        padToWidth width (intToBinary (num &&& (1L <<< width) - 1L))

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


let convertInt64ToFastData (width:int) (n:int64) =
    let n' = uint64 n
    let dat = 
        if width > 32 then 
            let mask = bigIntMask width
            BigWord (bigint n' &&& mask) 
        else 
            let mask = (1u <<< width) - 1u
            Word (uint32 n' &&& mask)
    {Dat=dat; Width = width}

let convertIntToFastData (width:int) (n:uint32) =
    if width <= 32 then 
        {Dat = Word n; Width = width}
    else
        {Dat = BigWord (bigint n); Width = width} 

let convertBigintToFastData (width:int) (b:bigint) =
    if width <= 32 then
        {Dat = BigWord b; Width = width}
    else
        failwithf "Converting a small (<= 32 bit) bigint should be to a word not a bigint fastdata"

let convertFastDataToInt64 (d:FastData) =
    match d.Dat with
    | Word n -> uint64 n
    | BigWord n -> 
        if d.Width > 64 then
            failwithf $"Can't convert a {d.Width} width bigint to int64"
        uint64 n

let convertFastDataToBigint (d:FastData) =
    match d.Dat with
    | Word n -> bigint n
    | BigWord n -> n

let convertFastDataToInt (d:FastData) =
    match d.Dat with
    | Word n -> n
    | BigWord _ -> failwithf $"Can't convert {d.Dat} to integer" 

let convertFastDataToWireData bits =
    bits |> convertFastDataToInt64 |> int64 |> convertIntToWireData bits.Width

let emptyFastData = {Width=0; Dat=Word 0u}




/// Try to convert a string to an int, or return an error message if that was
/// not possible.
let strToInt (str : string) : Result<int64, string> =
    try
        Ok <| int64 str
    with
        | _ -> Error <| "Invalid number."

        (*

let toInt = EEExtensions.Char.toInt

/// convert a digit character: binary, decimal, or hexadecimal, to its numeric value
let cDigitInt (ch:char) =
    match toInt ch with
    | d when d >= int32 '0' && d <= int32 '9' -> Some(d - int32 '0')
    | d when d >= toInt 'A' && d <= toInt 'Z' -> Some(d - toInt 'A' + 10)
    | _ -> None

let convertUInt64 (stringToConvert: string) =
    let rec pow64 n = 
    let getRadixNum (radix:int) (ns: int option list) =
        if Seq.forall (function | Some n -> n < radix && n >= 0 | None -> false) ns
        then Some (List.sumBy (fun n -> uint64 n + ))

    let aInt = toInt 'A'
    let s = EEExtensions.String.trim (EEExtensions.String.toUpper stringToConvert)
    if EEExtensions.String.startsWith "0X" s then
        let hexDigits = s.[2..s.Length-1]
        let convDigits = hexDigits |> List.map cDigitInt 
        if checkRadix 16

*)

let private countBits (num : int64) : int =
    (String.length <| bin64 num) - 2

/// Check a number is formed by at most <width> bits.
let rec private checkWidth (width : int) (num : int64) : string option =
    if num < 0L then
        checkWidth width <| (-num) - 1L
    else    
        let bitsCount = countBits num
        match bitsCount <= width with
        | true -> None
        | false -> Some <| sprintf "Expected %d or less bits." width

/// Convert a string to a number making sure that it has no more bits than
/// specified in width.
let strToIntCheckWidth (width : int) (str : string)  : Result<int64, string> =
    match str.Trim() with
    | "" -> Ok 0L // special case
    | str ->
        strToInt str
        |> Result.bind (fun num ->
            match checkWidth width num with
            | None -> Ok num
            | Some err -> Error err
        )
