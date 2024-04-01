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
    let maxBinaryDisplayWidth = 32
    /// size of font for text on non-binary waves
    let fontSizeValueOnWave = "10px"
    /// Text used to display vlaues on non-binary waves
    let valueOnWaveText = { DrawHelpers.defaultText with FontSize = fontSizeValueOnWave }


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
/// If pad with 0s up to width bits.
/// if width = 0 work ok with no padding
let addCommasAndZeros (width: int) (printedChars: string) =
    let divCeiling n divisor = (n - 1 + divisor) / divisor
    let nonZeroDigits = printedChars[2 .. printedChars.Length - 1]
    let numDigits = nonZeroDigits.Length

    let bitsPerDigit =
        match printedChars[1] with
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

    printedChars[0..1] + commaSeparatedDigits

let addZeros64 (width: int) (pFun: int64 -> string) (n: int64) = pFun n |> addCommasAndZeros width

let addZeros (width: int) (pFun: int -> string) (n: int) = pFun n |> addCommasAndZeros width

let hex64 (num: int64) = "0x" + num.ToString("X")

let fillHex64 width = addZeros64 width hex64

let bin64 (num: int64) = "0b" + (hexToBin <| num.ToString("X"))
let sDec64 (num: int64) = num.ToString()
let dec64 (num: int64) = (uint64 num).ToString()

let hex (num: int) = hex64 <| int64 num
let fillHex width = addZeros width hex

let bin (num: int) = bin64 <| int64 num
let dec (num: int) = dec64 <| int64 num

let fillBin64 width = addZeros64 width bin64
let fillBin width = addZeros width bin

/// Convert a bit to string.
let bitToString (bit: Bit) : string =
    match bit with
    | Zero -> "0"
    | One -> "1"

let big8 = 256I
let big16 = 65536I
let big32 = 1I <<< 32
let big64 = 1I <<< 64

/// print a bignum according to a radix.
/// if
let rec bigValToPaddedString (width: int) (radix: NumberBase) (x: System.Numerics.BigInteger) =
    if width = 0 then
        "0" // width = 0 is possible, and must not break this
    elif x < 0I then
        $"Bignm {x} is negative"
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
                 | Bin -> fillBin64
                 | Hex -> fillHex64
                 | _ -> failwithf "Can't happen")
                    width
                    (int64 (uint64 x))
            else
                bigValToPaddedString (width - 64) radix (x / (1I <<< 64))
                + ","
                + (match radix with
                   | Hex -> (fillHex64 64 (int64 (uint64 (x % (1I <<< 64)))))[2..65]
                   | _ -> (fillBin64 64 (int64 (uint64 (x % (1I <<< 64)))))[2..65])

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
                 | Bin -> bin64 >> addCommasAndZeros 0
                 | Hex -> hex64 >> addCommasAndZeros 0
                 | _ -> failwithf "Can't happen") (
                    int64 (uint64 x)
                )
            elif radix = Bin then
                "Can't display binary format > 64 bits"
            else
                bigValToString radix (x / (1I <<< 64))
                + ","
                + $"%16X{uint64 (x % 1I <<< 64)}"

/// Convert int64 to string according to provided radix
let valToString (radix: NumberBase) (value: int64) : string =
    match radix with
    | Dec -> dec64 value
    | Bin -> bin64 value |> addCommasAndZeros 0
    | Hex -> hex64 value |> addCommasAndZeros 0
    | SDec -> sDec64 value

/// Convert int64 to string according to radix.
/// binary and hex numbers are zero padded to width
/// binary is displayed as hex if width > 8
let valToPaddedString (width: int) (radix: NumberBase) (value: int64) : string =
    match radix with
    | Dec -> dec64 value
    | Bin -> fillBin64 width value
    | Hex
    | Bin -> fillHex64 width value
    | SDec -> sDec64 value

/// Pad wireData with Zeros as the Most Significant Bits (e.g. at position N).
let private padToWidth width (bits: WireData) : WireData =
    if bits.Length > width then
        List.truncate width bits
    else
        bits @ List.replicate (width - bits.Length) Zero

let fastDataToPaddedString maxChars radix (fd: FastData) =
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
        | Bin when fd.Width > Constants.maxBinaryDisplayWidth -> Hex
        | r -> r

    match fd.Dat with
    | Word w ->
        let signBit = w &&& (1u <<< (fd.Width - 1))

        let signExtendedW =
            match displayRadix, signBit <> 0u with
            | SDec, true -> int64 (int32 w) - (1L <<< fd.Width)
            | _ -> int64 (uint64 w)

        valToPaddedString fd.Width displayRadix signExtendedW

    | BigWord big -> bigValToPaddedString fd.Width displayRadix big
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

let UInt32ToPaddedString maxChars radix (width: int) (fd: uint32) =
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

    let signBit = fd &&& (1u <<< (width - 1))

    let signExtendedW =
        match displayRadix, signBit <> 0u with
        | SDec, true -> int64 (int32 fd) - (1L <<< width)
        | _ -> int64 (uint64 fd)

    let s = valToPaddedString width displayRadix signExtendedW

    match s.Length < maxChars with
    | true -> s // no change needed
    | false ->
        let pre, digits = stripLeadingZeros s
        let n = digits.Length

        let pre' =
            (match displayRadix with
             | Dec
             | SDec -> ""
             | _ -> $"{width}'")
            + pre

        if pre'.Length + digits.Length <= maxChars then
            // stripping leading zeros makes length ok
            pre' + digits
        else
            // truncate MS digits replacing by '..'
            pre'
            + ".."
            + digits[n + 2 + pre'.Length - maxChars .. n - 1]
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

        let pre' =
            (match displayRadix with
             | Dec
             | SDec -> ""
             | _ -> $"{width}'")
            + pre

        if pre'.Length + digits.Length <= maxChars then
            // stripping leading zeros makes length ok
            pre' + digits
        else
            // truncate MS digits replacing by '..'
            pre'
            + ".."
            + digits[n + 2 + pre'.Length - maxChars .. n - 1]

/// Convert an int into a Bit list with the provided width. The Least
/// Significant Bits are the one with low index (e.g. LSB is at position 0, MSB
/// is at position N). Little Endian.
/// If the number has more bits than width, then more bits will be returned.
let convertIntToWireData (width: int) (num: int64) : WireData =
    let toBit =
        function
        | 0 -> Zero
        | 1 -> One
        | _ -> failwith "toBit only accepts 0 or 1"

    let rec intToBinary (i: int64) =
        match int i with
        | 0
        | 1 -> [ toBit <| int i ]
        | _ ->
            let bit = toBit <| int (i % (int64 2))
            bit :: (intToBinary (i / (int64 2)))

    if num >= 0L then
        padToWidth width (intToBinary num)
    else
        padToWidth width (intToBinary (num &&& (1L <<< width) - 1L))

/// Convert a list of Bits into an int. The Least Significant Bits are the one
/// with low index (e.g. LSB is at position 0, MSB is at position N).
/// Little Endian.
let convertWireDataToInt (bits: WireData) : int64 =
    let rec convert bits idx =
        match bits with
        | [] -> int64 0
        | Zero :: bits' -> convert bits' (idx + 1)
        | One :: bits' -> pow2int64 (idx) + convert bits' (idx + 1)

    convert bits 0

let convertInt64ToFastData (width: int) (n: int64) =
    let n' = uint64 n

    let dat =
        if width > 32 then
            BigWord(bigint n' % (1I <<< width))
        else
            let mask =
                if width = 32 then
                    0xFFFFFFFFu
                else
                    (1u <<< width) - 1u
            Word(uint32 n' &&& mask)

    { Dat = dat; Width = width }

let convertIntToFastData (width: int) (n: uint32) =
    if width <= 32 then
        { Dat = Word n; Width = width }
    else
        { Dat = BigWord(bigint n); Width = width }

let convertBigintToFastData (width: int) (b: bigint) = { Dat = BigWord b; Width = width }

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

let convertBigIntToUInt64 (w: int) (b: bigint) =
    if w > 64 then b % (1I <<< 64) else b
    |> uint64

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
/// TODO: chnage this - and all dependencies with 32 bit int - to int64
let convertFastDataToInt32 (d: FastData) =
    match d.Dat with
    | Word n -> int32 n
    | BigWord n -> int32 (n &&& bigint 0xffffffff)

let convertBigIntToInt32 (b: bigint) = int32 (b &&& bigint 0xffffffff)

let convertInt64ToUInt32 (width: int) (n: int64) =
    let n' = uint64 n
    let mask =
        if width = 32 then
            0xFFFFFFFFu
        else
            (1u <<< width) - 1u
    uint32 n' &&& mask

let convertInt64ToBigInt (width: int) (n: int64) =
    let n' = uint64 n
    bigint n' % (1I <<< width)

let rec convertFastDataToWireData (fastDat: FastData) =
    let big64ToWire width big =
        big
        |> uint64
        |> int64
        |> convertIntToWireData width

    let rec bigToWire width b =
        if b < 0I then
            printfn $"Warning - invalid BigWord FastData case {b} < 0"
            []
        elif width <= 64 then
            big64ToWire width b
        else
            let lsBits = b % (1I <<< 64)
            big64ToWire 64 lsBits
            @ bigToWire (width - 64) (b / (1I <<< 64))

    match fastDat.Dat with
    | Word w -> convertIntToWireData fastDat.Width (int64 w)
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

/// Try to convert a string to an int, or return an error message if that was
/// not possible.
let strToInt (str: string) : Result<int64, string> =
    try
        Ok <| int64 str
    with _ ->
        Error <| "Invalid number."

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

let private countBits (num: int64) : int = (String.length <| bin64 num) - 2

/// Check a number is formed by at most <width> bits.
let rec checkWidth (width: int) (num: int64) : string option =
    if num < 0L then
        checkWidth width <| (-num) - 1L
    else
        let bitsCount = countBits num

        match bitsCount <= width with
        | true -> None
        | false -> Some <| sprintf "Expected %d or less bits." width

/// Convert a string to a number making sure that it has no more bits than
/// specified in width.
let strToIntCheckWidth (width: int) (str: string) : Result<int64, string> =
    match str.Trim() with
    | "" -> Ok 0L // special case
    | str ->
        strToInt str
        |> Result.bind (fun num ->
            match checkWidth width num with
            | None -> Ok num
            | Some err -> Error err)

let convertBinToDec (bits: string) : int64 =
    let rec convert bits idx =
        match bits with
        | [] -> int64 0
        | '0' :: bits' -> convert bits' (idx - 1)
        | '1' :: bits' -> pow2int64(idx) + convert bits' (idx - 1)
        | _ -> failwithf "Not binary input, should not happen!"
    convert ((bits.ToCharArray()) |> Array.toList) (bits.Length-1)
    

/// Converts a binary, hex or decimal number to decimal
let toDecimal (num: string) numBase (width:string) =
    let width = width |> int
    match numBase with
    | "'d" -> num |> int64
    | "'b" -> num |> convertBinToDec
    | "'h" -> 
        num.ToLower()  |> hexToBin |> convertBinToDec
    | _ -> failwithf "Wrong base, should not happen!"


//----------------------------------------------------------------------------------------------//
//----------------------------------------------------------------------------------------------//
//------------------------------New numeric display Helpers-------------------------------------//
//----------------------------------------------------------------------------------------------//

/// If the first two arguments are constants this function will return be a very fast
/// upper bound approximation to maximum text width of a number of given bit-width
/// displayed in a given radix.
let getUpperBoundNumericWidth (font: DrawHelpers.Text) (radix: NumberBase) : (uint32 -> float) =
    let digit (n:int) = sprintf $"{n}"
    /// Return all possible digits for given radix except for ones known to be not widest.
    let rec worstDigits radix =
        match radix with
        | Bin -> [0] |> List.map digit
        | Dec | SDec -> [0;2;3;4;5;6;7;8;9] |> List.map digit
        | Hex -> ["A";"B";"C";"D";"E";"F"] @ worstDigits Dec
    /// For given radix work out the worst possible width in pixels
    let worstWidth (quads:int)  =
        let valPart (d:string) : string =
            match radix with
            | Dec | SDec -> String.replicate (4*quads)  d
            | _ -> String.replicate quads (String.replicate 4 d + ",")
        let prefix =
            match radix with
            | Bin -> "0b"
            | Dec -> ""
            | SDec -> "-"
            | Hex -> "0x"
        worstDigits radix
        |> List.map (fun d -> prefix + valPart d)
        |> List.map (fun s -> s, DrawHelpers.getTextWidthInPixels font s)
        |> List.maxBy snd
        |> snd
    /// maximum width of 40 digits
    let width40 = worstWidth 10
    /// maximum width of 4 digits (including a terminating ',')
    let width4 = worstWidth 1
    /// using linear approximation this is the slope of the width / number of bits graph
    let slope = (width40 - width4)/36.
    /// calculated width of 0 digits (from prefix + ',')
    let width0 = width4 - slope*4.
    /// the function returned here is fast
    fun (numBits: uint32) ->
  
        let maxDigits =
            match radix with
            | Bin -> numBits
            | Hex -> ((int numBits - 1) / 4) + 1 |> uint32
            | Dec -> System.Math.Ceiling (log10 2.0 * float numBits) |> uint32
            | SDec -> System.Math.Ceiling (log10 2.0 * float (numBits - 1u) + 0.000000001) |> uint32
        maxDigits
        |> (fun x -> width0 + slope * float x)

/// This function should not be used directly
/// for speed reasons it should be called via the functions below that have specific radixes
let waveNumberWidthP = getUpperBoundNumericWidth Constants.valueOnWaveText
        
let getWaveNumberWidthHex =  waveNumberWidthP Hex
let getWaveNumberWidthDec =  waveNumberWidthP Dec
let getWaveNumberWidthSDec =  waveNumberWidthP SDec
let getWaveNumberWidthBin =  waveNumberWidthP Bin

/// Return the width in pixels of a text string printed on waveform in a given radix 
/// for a given widthInBits 
let waveNumberWidth (radix: NumberBase) (widthInBits: int) =
    let w = uint32 widthInBits
    // make sure that the radix-dependent evalutaion is done just once
    match radix with
    | Hex -> getWaveNumberWidthHex w
    | Dec -> getWaveNumberWidthDec w
    | SDec -> getWaveNumberWidthSDec w
    | Bin -> getWaveNumberWidthBin w

/// Efficiently returns a string equivalent to b in base radix
/// If radix = Hex or bin this is as a fixed length set of width digits in given radix.
/// with ',' markers added every 4 digits.
/// If radix = Dec this is as a binary unsigned number.
/// If radix = SDec the number is interpreted a two's complement signed in width numBits
let rec displayBigint (radix: NumberBase) (numBits: int) (b: bigint) =
    /// Deals with SDec special case
    let displayBigintSDec (numBits: int) (b: bigint) =
        match b with
        | _ when b = 0I ->
            "0"
        | _ when b < (1I <<< numBits - 1) ->
            displayBigint Dec numBits b
        | _ ->
            let twosComplementNegatedB = ((1I <<< numBits) - b)
            "-" + displayBigint Dec numBits twosComplementNegatedB

    match radix with
    | SDec -> displayBigintSDec (numBits: int) (b: bigint)
    | _ ->
        /// converts byte array of digits in range 0 - 15 into hex string
        let toHexString (bytes: byte array) (start:int) (count: int) =
            String.init count (fun n ->
                let n = bytes[start+n]
                (if n < 10uy then n + 0x30uy else n + byte 'A')
                |> char
                |> string)    
        let baseN = match radix with | Hex -> 16I | Dec | SDec -> 10I | Bin -> 2I
        let width =
            match radix with
            | Hex -> (numBits - 1) / 4 + 1
            | Bin -> numBits
            | Dec | SDec -> 0 // not used
        let rec getDigits (w: int) (b: bigint) digitL =
            if w = 0 then  digitL
            else
                let digit = byte (b % baseN)
                getDigits (w - 1) (b / baseN) (digit :: digitL)
        let digits = getDigits width b [] |> List.toArray
        match radix with
        | Hex | Bin ->
            let numOtherQuads = (width - 1) / 4
            let msQuad = toHexString digits 0 (width - numOtherQuads*4)
            [numOtherQuads-1 .. -1 .. 0]
            |> List.map (fun q ->
                    let start = width - q*4 - 4
                    toHexString digits start 4)
            |> (fun l -> msQuad :: l)
            |> String.concat ","
            |> (+)  (match radix with | Hex -> "0x" | Bin -> "0b" | _ -> "")
        | Dec | SDec ->
            sprintf $"{b}"




/// Efficiently returns a string equivalent to b in base radix
/// If radix = Hex or bin this is as a fixed length set of width digits in given radix.
/// with ',' markers added every 4 digits.
/// If radix = Dec this is as a binary unsigned number.
/// If radix = SDec the number is interpreted a two's complement signed in width numBits
let rec displayUint32 (radix: NumberBase) (numBits: int) (b: uint32) =
    /// Deals with SDec special case
    let displayUint32SDec (numBits: int) (b: uint32) =
        match b with
        | _ when b = 0u ->
            "0"
        | _ when b < (1u <<< numBits - 1) ->
            displayUint32 Dec numBits b
        | _ ->
            let twosComplementNegatedB = (1u <<< numBits) - b
            "-" + displayUint32 Dec numBits twosComplementNegatedB

    match radix with
    | SDec -> displayUint32SDec numBits b
    | _ ->
        /// converts byte array of digits in range 0 - 15 into hex string
        let toHexString (bytes: byte array) (start:int) (count: int) =
            String.init count (fun n ->
                let n = bytes[start+n]
                (if n < 10uy then n + 0x30uy else n + byte 'A' - 10uy)
                |> char
                |> string)               
        let baseN = match radix with | Hex -> 16u | Dec | SDec -> 10u | Bin -> 2u
        let width =
            match radix with
            | Hex -> (numBits - 1) / 4 + 1
            | Bin -> numBits
            | Dec | SDec -> 0 // not used
        let rec getDigits (w: int) (b: uint32) digitL =
            if w = 0 then  digitL
            else
                let digit = byte (b % baseN)
                getDigits (w - 1) (b / baseN) (digit :: digitL)
        let digits = getDigits width b [] |> List.toArray
        match radix with
        | Hex | Bin ->
            let numOtherQuads = (width - 1) / 4
            let msQuad = toHexString digits 0 (width - numOtherQuads*4)
            [numOtherQuads-1 .. -1 .. 0]
            |> List.map (fun q ->
                    let start = width - q*4 - 4
                    toHexString digits start 4)
            |> (fun l -> msQuad :: l)
            |> String.concat ","
            |> (+)  (match radix with | Hex -> "0x" | Bin -> "0b" | _ -> "")
        | Dec | SDec ->
            sprintf $"{b}"



let displayFastData (radix: NumberBase) (fd: FastData) =
    match fd.Dat with
    | Word u -> displayUint32 radix fd.Width u
    | BigWord b -> displayBigint radix fd.Width b

/// Returns a display string for the data in specified radix.
/// If radix display is too large for given width fall back from binary to hexadecimal.
/// If width is larger than maxBinaryDisplayWidth fall back from binary to hexadecimal.
/// return correct string to display as Error is it still is too large for width, otherwise Ok.
let displayFastDataInWidth (widthInPixels: float) (radix: NumberBase) (fd: FastData) =
    let isTooWide = widthInPixels < waveNumberWidth radix fd.Width
    let fallBackExpected = radix = Bin && (isTooWide || fd.Width > Constants.maxBinaryDisplayWidth)
    let actualRadix = if fallBackExpected then Bin else radix
    displayFastData actualRadix fd
    |> match widthInPixels > waveNumberWidth actualRadix fd.Width with
       | true -> Ok
       | false -> Error


let displayFastDataInWidthOrBlank (widthInPixels: float) (radix: NumberBase) (fd: FastData) =
    displayFastDataInWidth widthInPixels radix fd
    |> function | Ok x -> x | Error x -> ""


let checkDisplayFuncs (width: int) (radix: NumberBase) (numb: uint32) =
    let w1 = waveNumberWidth radix width
    let s = displayUint32 radix width numb
    let w2 = DrawHelpers.getTextWidthInPixels Constants.valueOnWaveText s
    printf $"%.2f{w2/w1} base={radix} width={width} 0x%x{numb}, %d{numb} {s}"

let widthTests() =
    [
        Hex,12, 0x123u
        Hex,12,0xFFFu
        Hex,28, 0xAF5677u
        Hex,3, 0x5u
        Bin, 22, 0x0fu
        Dec, 15, 29999u
        Dec, 10, 900u
        SDec, 8, 0xF0u
    ]
    |> List.iter (fun (r,w,n) -> checkDisplayFuncs w r n)
