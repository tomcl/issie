(*
    NumberHelpers.fs

    A collection of functions that allow to covert numbers into various
    representations.
*)

module NumberHelpers

open CommonTypes
open Helpers
open SimulatorTypes

module Constants =
    /// width at which numbers that display as very small signe ddecimal numbers are so displayed
    let maxHexOnlyDisplayWidth = 16
    /// width at which binary numbers are displayed as (more compact) hex numbers.
    let maxBinaryDisplayWidth = 32
    /// max width of an Issie Constant component
    let maxConstantWidth = 2048
    /// max no of chars displayed before display functions truncate result
    let maxNumericCharsBeforeTruncation = 80


/// Convert an hex string into a binary string.
let private hexToBin (hStr: string) : string =
    let rec convert h =
        match h with
        | [] -> ""
        | c :: h' ->
            let digit =
                match c with
                | '0' -> "0000"
                | '1' -> "0001"
                | '2' -> "0010"
                | '3' -> "0011"
                | '4' -> "0100"
                | '5' -> "0101"
                | '6' -> "0110"
                | '7' -> "0111"
                | '8' -> "1000"
                | '9' -> "1001"
                | 'a' -> "1010"
                | 'b' -> "1011"
                | 'c' -> "1100"
                | 'd' -> "1101"
                | 'e' -> "1110"
                | 'f' -> "1111"
                | c -> failwithf "Invalid char %c while converting hex %s to binary" c hStr

            digit + (convert h')

    let chars = hStr.ToLower() |> Seq.toList

    match chars with
    | [] -> ""
    | c :: chars' ->
        let firstDigit = // Avoid leading zeros.
            match c with
            | '0' -> "0"
            | '1' -> "1"
            | '2' -> "10"
            | '3' -> "11"
            | '4' -> "100"
            | '5' -> "101"
            | '6' -> "110"
            | '7' -> "111"
            | '8' -> "1000"
            | '9' -> "1001"
            | 'a' -> "1010"
            | 'b' -> "1011"
            | 'c' -> "1100"
            | 'd' -> "1101"
            | 'e' -> "1110"
            | 'f' -> "1111"
            | c -> failwithf "Invalid char %c while converting hex %s to binary" c hStr

        firstDigit + (convert chars')

/// Format a hex or bin printed string adding commas every 4 digits.
/// If width > 0 pad with 0s up to width bits.
/// if width = 0 work ok with no padding
let addCommasAndZeros (width: int) (printedChars: string) =
    let divCeiling n divisor = (n - 1 + divisor) / divisor
    let nonZeroDigits = printedChars[1 .. printedChars.Length - 1]
    let numDigits = nonZeroDigits.Length

    let bitsPerDigit =
        match printedChars[0] with
        | 'x' -> 4
        | 'b' -> 1
        | r -> failwithf "Wrong use of addZeros: radix Char = %c" r

    let extraZerosNum =
        (width - numDigits * bitsPerDigit)
        |> max 0
        |> fun n -> divCeiling n bitsPerDigit
        |> min 64

    let digits = String.replicate extraZerosNum "0" + nonZeroDigits
    let num4Chunks = divCeiling digits.Length 4

    let commaSeparatedDigits =
        match num4Chunks with
        | 1 -> digits // no ',' added
        | _ ->
            digits
            |> Seq.rev
            |> Seq.chunkBySize 4
            |> Seq.rev
            |> Seq.map (Seq.rev >> System.String.Concat)
            |> String.concat ","

    printedChars[0..0] + commaSeparatedDigits


let big8 = 256I
let big16 = 65536I
let big32 = 1I <<< 32
let big64 = 1I <<< 64

/// Add commas to a string every 3 digits, and leading zeros to fill to given width
let addZerosBignum (width: int) (pFun: bigint -> string) (n: bigint) = pFun n |> addCommasAndZeros width


/// string representation of the hex digits of num, without leading zero or prefix.
/// Needed because "%x" does not work on bignums.
let rec hexDigitsBignum (num: bigint) =
    let hex32 (d: uint32) = $"%x{d}"
    let hex32Filled (d: uint32) = $"%08x{d}"
    let q,r = bigint.DivRem(num, 1I <<< 32)
    if q = 0I then
        hex32 (uint32 r) 
    else
        hexDigitsBignum q + hex32Filled (uint32 r)
    

/// print a bignum as hex
let hexBignum (num: bigint) =
    "x" + hexDigitsBignum num

/// print a bignum as binary
let binBignum (num: bigint) = "b" + (hexToBin <| hexDigitsBignum num)

/// NB without width bignum must be signed, but mostly Issie bignums are non-negative and use two's complement
/// So this function may be of limited use.
let sDecBignum (num: bigint) = num.ToString()

/// This is the same as sDecBignum. It is here for symmetry.
let decBignum (num: bigint) = num.ToString() //?

/// Fill a hex printed bignum with zeros to a given width.
let fillHexBignum width (n:bigint) =
    addZerosBignum width hexBignum n

/// Fill a binary printed bignum with zeros to a given width.
let fillBinBignum width = addZerosBignum width binBignum


/// Convert a bit to string.
let bitToString (bit: Bit) : string =
    match bit with
    | Zero -> "0"
    | One -> "1"



/// print a bignum according to a radix.
/// if
let rec bigValToPaddedString (width: int) (radix: NumberBase) (x: System.Numerics.BigInteger) =
    if width = 0 then
        "0" // width = 0 is possible, and must not break this
    elif x < 0I then
        $"Bignum {x} is negative"
    elif width < 1 then
        $"Error: {width} is not a valid bignum width"
    elif x >= (1I <<< width) then
        bigValToPaddedString width radix (x % (1I <<< width))
    else
        match radix with
        | SDec ->
            if x >= (1I <<< width - 1) then
                "-" + ((1I <<< width) - x).ToString()
            else
                x.ToString()
        | Dec -> x.ToString()
        | Bin
        | Hex ->
            if width <= 64 then
                (match radix with
                 | Bin -> fillBinBignum
                 | Hex -> fillHexBignum
                 | _ -> failwithf "Can't happen")
                    width
                    x
            else
                bigValToPaddedString (width - 64) radix (x / (1I <<< 64))
                + ","
                + (match radix with
                   | Hex -> (fillHexBignum 64 (x % (1I <<< 64)))[2..65]
                   | _ -> (fillBinBignum 64 (x % (1I <<< 64)))[2..65])

let rec bigValToString (radix: NumberBase) (x: System.Numerics.BigInteger) =
    if x < 0I then
        $"Bignum {x} is negative"
    else
        match radix with
        | Dec
        | SDec -> // can't know if sign is negative in this case
            x.ToString()
        | Bin
        | Hex ->
            if x <= (1I <<< 64) then
                (match radix with
                 | Bin -> binBignum >> addCommasAndZeros 0
                 | Hex -> hexBignum >> addCommasAndZeros 0
                 | _ -> failwithf "Can't happen") (
                    x
                )
            elif radix = Bin then
                "Can't display binary format > 64 bits"
            else
                bigValToString radix (x / (1I <<< 64))
                



/// Convert bigint to string according to radix.
/// binary and hex numbers are zero padded to width
/// binary is displayed as hex if width > 8
let valToPaddedString (width: int) (radix: NumberBase) (value: bigint) : string =
    match radix with
    | Dec -> decBignum value
    | Bin -> fillBinBignum width value
    | Hex when width <= 4 ->
            match value with
            | x when x < 10I -> string (char (int '0' + int x))
            | x -> string (char (int 'A' + int x - 10))
    | Hex -> fillHexBignum width value
    | SDec -> sDecBignum value

/// Pad wireData with Zeros as the Most Significant Bits (e.g. at position N).
let private padToWidth width (bits: WireData) : WireData =
    if bits.Length > width then
        List.truncate width bits
    else
        bits @ List.replicate (width - bits.Length) Zero

let fastDataToPaddedString maxChars radix (fd: FastData) =
    let getPrefixAndDigits (s: string) =
        match s.Length, s.[0] with
        | n, '-' -> "-", s[1 .. n - 1]
        | n, 'b'
        | n, 'x' -> s.[0..0], s[1 .. n - 1]
        | _ -> "", s

    let stripLeadingZeros s =
        let rec strip index (s: string) =
            if
                index < s.Length - 1
                && (s[index] = '0' || s[index] = ',')
            then
                strip (index + 1) s
            else
                s[index .. s.Length - 1]

        let pre, digits = getPrefixAndDigits s
        pre, strip 0 digits

    let displayRadix =
        match radix with
        | Bin when fd.Width > Constants.maxBinaryDisplayWidth -> Hex
        | r -> r

    match fd.Dat with
    | Word w ->
        let signBit = w &&& (1u <<< (fd.Width - 1))

        let signExtendedW =
            match displayRadix, signBit <> 0u with
            | SDec, true -> int64 (int32 w) - (1L <<< fd.Width)
            | _ -> int64 (uint64 w)

        valToPaddedString fd.Width displayRadix (bigint signExtendedW)

    | BigWord big ->
        let displayRadix =
            match displayRadix with
            | Hex when fd.Width > Constants.maxHexOnlyDisplayWidth && (decBignum big).Length < 3 -> Hex
            | r -> r
        bigValToPaddedString fd.Width displayRadix big
    |> (fun s ->
        match s.Length < maxChars with
        | true -> s // no change needed
        | false ->
            let pre, digits = stripLeadingZeros s
            let n = digits.Length

            let pre' =
                (match displayRadix with
                 | Dec
                 | SDec -> ""
                 | _ -> $"{fd.Width}'")
                + pre

            if pre'.Length + digits.Length <= maxChars then
                // stripping leading zeros makes length ok
                pre' + digits
            else
                // truncate MS digits replacing by '..'
                pre'
                + ".."
                + digits[n + 2 + pre'.Length - maxChars .. n - 1])

let UInt32ToPaddedString maxChars radix (width: int) (n: uint32) =
    let getPrefixAndDigits (s: string) =
        match s.Length, s.[0], s.[1] with
        | n, '-', _ -> "-", s[1 .. n - 1]
        | n, '0', 'b'
        | n, '0', 'x' -> s.[1..1], s[2 .. n - 1]
        | _ -> "", s

    let stripLeadingZeros s =
        let rec strip index (s: string) =
            if
                index < s.Length - 1
                && (s[index] = '0' || s[index] = ',')
            then
                strip (index + 1) s
            else
                s[index .. s.Length - 1]

        let pre, digits = getPrefixAndDigits s
        pre, strip 0 digits

    let displayRadix =
        match radix with
        | Bin when width > Constants.maxBinaryDisplayWidth -> Hex
        | r -> r

    let signBit = n &&& (1u <<< (width - 1))

    let signExtendedW =
        match displayRadix, signBit <> 0u with
        | SDec, true -> int64 (int32 n) - (1L <<< width)
        | _ -> int64 (uint64 n)

    let s = valToPaddedString width displayRadix (bigint signExtendedW)

    match s.Length < maxChars with
    | true -> s // no change needed
    | false ->
        let pre, digits = stripLeadingZeros s
        let n = digits.Length

        if pre.Length + digits.Length <= maxChars then
            // stripping leading zeros makes length ok
            pre + digits
        else
            // truncate MS digits replacing by '..'
            pre
            + ".."
            + digits[n + 2 + pre.Length - maxChars .. n - 1]

let BigIntToPaddedString maxChars radix (width: int) (fd: bigint) =
    let getPrefixAndDigits (s: string) =
        match s.Length, s.[0], s.[1] with
        | n, '-', _ -> "-", s[1 .. n - 1]
        | n, '0', 'b'
        | n, '0', 'x' -> s.[1..1], s[2 .. n - 1]
        | _ -> "", s

    let stripLeadingZeros s =
        let rec strip index (s: string) =
            if
                index < s.Length - 1
                && (s[index] = '0' || s[index] = ',')
            then
                strip (index + 1) s
            else
                s[index .. s.Length - 1]

        let pre, digits = getPrefixAndDigits s
        pre, strip 0 digits

    let displayRadix =
        match radix with
        | Bin when width > Constants.maxBinaryDisplayWidth -> Hex
        | r -> r

    let s = bigValToPaddedString width displayRadix fd
    match s.Length < maxChars with
    | true -> s // no change needed
    | false ->
        let pre, digits = stripLeadingZeros s
        let n = digits.Length

    

        if pre.Length + digits.Length <= maxChars then
            // stripping leading zeros makes length ok
            pre + digits
        else
            // truncate MS digits replacing by '..'
            pre
            + ".."
            + digits[n + 2 + pre.Length - maxChars .. n - 1]


/// make negative bigints into the equivalent two's complement positive value.
/// Must be given a width.
let twosComp (width: int) (n: bigint) : bigint=
    if n < 0I then
        (1I <<< width) + n
    else
        n

/// Convert an int into a Bit list with the provided width. The Least
/// Significant Bits are the one with low index (e.g. LSB is at position 0, MSB
/// is at position N). Little Endian.
/// If the number has more bits than width, then more bits will be returned.
let convertIntToWireData (width: int) (num: bigint) : WireData =
    let toBit =
        function
        | 0 -> Zero
        | 1 -> One
        | _ -> failwith "toBit only accepts 0 or 1"

    let rec intToBinary (i: bigint) =
        if i < 2I then 
            [ toBit <| int i ]
        else
            let bit = toBit <| int (i % 2I)
            bit :: (intToBinary (i / (2I)))

    if num >= 0I then
        padToWidth width (intToBinary num)
    else
        padToWidth width (intToBinary ((num &&& (1I <<< width)) - 1I))

/// Convert a list of Bits into an int. The Least Significant Bits are the one
/// with low index (e.g. LSB is at position 0, MSB is at position N).
/// Little Endian.
let convertWireDataToInt (bits: WireData) : bigint =
    let rec convert bits idx =
        match bits with
        | [] -> 0I
        | Zero :: bits' -> convert bits' (idx + 1)
        | One :: bits' -> (1I <<< idx) + convert bits' (idx + 1)

    convert bits 0

let convertIntToFastData (width: int) (n: uint32) =
    if width <= 32 then
        { Dat = Word n; Width = width }
    else
        { Dat = BigWord(bigint n); Width = width }

let convertBigintToFastData (width: int) (b: bigint) =
    let b = twosComp width b
    if width > 32 then
        { Dat = BigWord b; Width = width }
    else
        { Dat = Word (uint32 b); Width = width }

/// convert to 64 bits - if too large take LS 64 bits
let convertFastDataToInt64 (d: FastData) =
    match d.Dat with
    | Word n -> uint64 n
    | BigWord b ->
        if d.Width > 64 then
            b % (1I <<< 64)
        else
            b
        |> uint64



let convertBigintToUInt64 (w: int) (b: bigint) =
    if w > 64 then b % (1I <<< 64) else b
    |> uint64

let convertBigintToUInt32 (w: int) (b: bigint) =
    if w > 32 then b % (1I <<< 32) else b
    |> uint32

/// convert to a bigint - always works. Bits < width will be correct.
let convertFastDataToBigint (d: FastData) =
    match d.Dat with
    | Word n -> bigint n
    | BigWord n -> n

/// convert to int with an exception if data is too large
let convertFastDataToInt (d: FastData) =
    match d.Dat with
    | Word n -> n
    | BigWord _ -> failwithf $"Can't convert {d.Dat} to integer"

/// Lossy conversion of bigint to int32 without exceptions
/// TODO: change this - and all dependencies with 32 bit int - to bigint?
let convertFastDataToInt32 (d: FastData) =
    match d.Dat with
    | Word n -> int32 n
    | BigWord n -> int32 (n &&& bigint 0xffffffff)

let convertBigintToInt32 (b: bigint) = int32 (b &&& bigint 0xffffffff)


let rec convertFastDataToWireData (fastDat: FastData) =
    let bigToWire width big =
        big
        |> convertIntToWireData width

    let rec bigToWire width b =
        if b < 0I then
            printfn $"Warning - invalid BigWord FastData case {b} < 0"
            []
        else
            bigToWire width b

    match fastDat.Dat with
    | Word w -> convertIntToWireData fastDat.Width (bigint w)
    | BigWord b -> bigToWire fastDat.Width b

let convertWireDataToFastData (wd: WireData) =
    if wd.Length <= 32 then
        { Dat = Word(uint32 (uint64 (convertWireDataToInt wd))); Width = wd.Length }
    else
        List.indexed wd
        |> List.map (fun (i, bit) ->
            match bit with
            | Zero -> 0I
            | One -> (1I <<< i))
        |> List.sum
        |> (fun big -> { Dat = BigWord big; Width = wd.Length })

let emptyFastData = { Width = 0; Dat = Word 0u }



/// Try to convert a string to a bigint, or return an error message if that was
/// not possible.
let strToBigint (str: string) : Result<bigint, string> =
    let removeCommas (str: string) =
        str.Replace(",", "")
    let str = str.ToLower()
    let str = if str.Length > 1 && str[0] = 'x' || str[0]='b' then "0" + str else str
    let success, n =
        str
        |> removeCommas
        |> bigint.TryParse 
    match success with
    | false -> Error "Invalid number."
    | true -> Ok n
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
        let hexDigits = s[2..s.Length-1]
        let convDigits = hexDigits |> List.map cDigitInt
        if checkRadix 16

*)

let private countBits (num: bigint) : int =

    let rec log2Int (n:bigint) =
        if n = 0I then 0
        elif n < 0I then log2Int ( -n - 1I)
        else log2Int (n >>> 1) + 1
    let n = log2Int num        
    if n = 0 then 1 else n
        

/// Check a number is formed by at most <width> bits.
let rec checkWidth (width: int) (num: bigint) : string option =
    if num < 0I then
        checkWidth width <| (-num) - 1I
    else
        let bitsCount = countBits num
        match bitsCount <= width with
        | true -> None
        | false -> Some <| sprintf "Expected %d or less bits." width

/// Convert a string to a number making sure that it has no more bits than
/// specified in width.
let strToIntCheckWidth (width: int) (str: string) : Result<bigint, string> =
    match str.Trim() with
    | "" -> Ok 0I // special case
    | str ->
        strToBigint str
        |> Result.bind (fun num ->
            match checkWidth width num with
            | None -> Ok num
            | Some err -> Error err)

let convertBinToDec (bits: string) : bigint =
    let rec convert bits idx =
        match bits with
        | [] -> 0I
        | '0' :: bits' -> convert bits' (idx - 1)
        | '1' :: bits' -> (1I <<< idx) + convert bits' (idx - 1)
        | _ -> failwithf "Not binary input, should not happen!"
    convert ((bits.ToCharArray()) |> Array.toList) (bits.Length-1)
    

/// Converts a binary, hex or decimal number to decimal
let toDecimal (num: string) numBase (width:string) =
    let width = width |> int
    match numBase with
    | "'d" -> num |> bigint.Parse
    | "'b" -> num |> convertBinToDec
    | "'h" -> 
        num.ToLower()  |> hexToBin |> convertBinToDec
    | _ -> failwithf "Wrong base, should not happen!"
