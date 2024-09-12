(*
    NumberHelpers.fs

    A collection of functions that allow to covert numbers into various
    representations.
*)

module NumberHelpers

open CommonTypes
open Helpers
open SimGraphTypes
open SimTypes

module Constants =
    /// width at which numbers that display as very small signe ddecimal numbers are so displayed
    let maxHexOnlyDisplayWidth = 16
    /// width at which binary numbers are displayed as (more compact) hex numbers.
    let maxBinaryDisplayWidth = 32
    /// Max width of an Issie bus. There is no real need for any restriction.
    /// since all code is bigint based, but this is a reasonable limit.
    /// There are performance & UI issues for very large busses.
    let maxIssieBusWidth = 16384
    /// max no of chars displayed before display functions truncate result
    let maxNumericCharsBeforeTruncation = 80

/// Make negative bigints into the equivalent two's complement positive value.
/// Must be given a width.
let twosComp (width: int) (n: bigint) : bigint=
    if n < 0I then
        (1I <<< width) + n
    else
        n

/// Make twos comp bigint n into the equivalent signed bignum value.
/// Must be given a width.
let twosCompValue (width: int) (n: bigint) : bigint=
    if n >= (1I <<< (width - 1)) then
        n - (1I <<< width)
    else
        n

//--------------------------------------------------------------------------------------------------------------//
//-------------------------------------Printing of digital busses in given radixes------------------------------//
//--------------------------------------------------------------------------------------------------------------//

/// Convert a hex string into a binary string without unnecessary leading zeros.
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
                | 'a' | 'A' -> "1010"
                | 'b' | 'B'  -> "1011"
                | 'c' | 'C' -> "1100"
                | 'd' | 'D' -> "1101"
                | 'e' | 'E' -> "1110"
                | 'f' | 'F' -> "1111"
                | c ->
                    printf "Invalid char %c while converting hex %s to binary" c hStr
                    failwithf "Invalid char %c while converting hex %s to binary" c hStr

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
            | 'a' | 'A' -> "1010"
            | 'b' | 'B'  -> "1011"
            | 'c' | 'C' -> "1100"
            | 'd' | 'D' -> "1101"
            | 'e' | 'E' -> "1110"
            | 'f' | 'F' -> "1111"
            | c ->
                printf "Invalid char %c while converting hex %s to binary" c hStr
                failwithf "Invalid char %c while converting hex %s to binary" c hStr

        firstDigit + (convert chars')



    

/// Display a bignum as hex, always using 'x' prefix.
/// NB - printf %x does bnot work for bignums
let hexBignum (num: bigint) =
    /// string representation of the hex digits of num, without leading zero or prefix.
    /// Needed because "%x" does not work on bignums.
    let rec hexDigitsBignum (num: bigint) =
        let hex32 (d: uint32) = $"%X{d}"
        let hex32Filled (d: uint32) = $"%08X{d}"
        let q,r = bigint.DivRem(num, 1I <<< 32)
        if q = 0I then
            hex32 (uint32 r) 
        else
            hexDigitsBignum q + hex32Filled (uint32 r)

    "x" + hexDigitsBignum num

/// Display a bignum as binary, always using 'b' prefix.
let binBignum (num: bigint) =
    let hex =  hexBignum num
    "b" + hexToBin hex[1..]


/// Display bigint as signed twos complement value if width > 0.
/// Otherwise display bigint in signed decimal as is.
let sDecBignum (width: int) (num: bigint) =
    match width with
    | 0 -> num.ToString()
    | _ -> (twosCompValue width num).ToString()

/// The same as sDecBignum with width of 0.
/// Display the bignum in signed decimal as is.
let decBignum (num: bigint) = num.ToString()


/// Format a hex or bin printed string adding commas every 4 digits.
/// PrintedChars: hex or bin string prefixed 'x' or 'b'.
/// If width > 0 pad with 0s up to width bits.
/// if width = 0 add commas but no leading zeros.
/// Output: zero-filled string prexixed by 'x' or 'b'. up tp width bits.
let private addCommasAndZeros (width: int) (printedChars: string) =
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

/// Add commas to a string every 4 digits, and leading zeros to fill to given width in bits (not digits).
/// If width = 0 do not add leading zeros.
/// The string must be prefixed x or b to indicate hex or binary digits.
/// pFun: function to generate print string.
/// n: bigint to print with zeros and comms.
let private addZerosBignum (width: int) (pFun: bigint -> string) (n: bigint) = pFun n |> addCommasAndZeros width

/// Fill a hex printed bigint with zeros to a given width in bits.
/// Add commas every 4 digits for readability.
let fillHexBignum width (n:bigint) =
    addZerosBignum width hexBignum n

/// Fill a binary printed bignum with zeros to a given width.
/// Add commas every 4 bits fro readability.
let fillBinBignum width =
    addZerosBignum width binBignum


/// Display a bigint according to a radix and width in bits.
/// if the radix is hex or bin zero-fill to the width, and separate with commas.
let rec private printWithFill (width: int) (radix: NumberBase) (x: bigint) =
    if width = 0 then
        "0" // width = 0 is possible, and must not break this
    elif x < 0I then
        $"Bignum {x} is negative"
    elif width < 1 then
        $"Error: {width} is not a valid bignum width"
    elif x >= (1I <<< width) then
        // if bignum is negative or has extra bits - force it to correct bit range
        printWithFill width radix (x % (1I <<< width))
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
                 (match radix with
                 | Bin -> fillBinBignum
                 | Hex -> fillHexBignum
                 | _ -> failwithf "Can't happen")
                    width
                    x
               

/// Convert bigint to string according to radix.
/// Binary and hex numbers are zero padded to width
/// Binary is displayed as hex if width > 16
/// Single-bit binary or hex numbers do not have x or b prefix.
/// Multi-bit hex or binary numbers do.
let valToPaddedString (width: int) (radix: NumberBase) (value: bigint) : string =
    match radix with
    | Dec -> decBignum value
    | Bin when width < 2 -> (binBignum value)[1..1]
    | Bin when width > 16 -> fillHexBignum width value
    | Bin -> fillBinBignum width value
    | Hex when width <= 4 -> (hexBignum value)[1..1]
    | Hex -> fillHexBignum width value
    | SDec -> sDecBignum width value

/// Compress a string to a given estimated width (maxChars) in characters.
/// TODO: maybe this should be exact using TextMetrics? Need to check performance.
let private compress width maxChars radix s =
    let maxChars = float maxChars
    let getPrefixAndDigits (s: string) =
        match s.Length, s.[0] with
        | n, '-' -> "-", s[1 .. n - 1]
        | n, 'b'
        | n, 'x' -> s.[0..0], s[1 .. n - 1]
        | _ -> "", s

    let pre, digits = getPrefixAndDigits s
    let n = digits.Length
    let estimateCharLength c =
        match c with
        | ',' | '1' -> 0.75
        | _ -> 1.0
    let dotsLength = 3.0 * estimateCharLength '.'
    let estimateLength digits =
                digits
                |> Seq.sumBy estimateCharLength

    let findBestLength maxLength (digits: string) =
        let lastDigitIndex = digits.Length - 1
        let rec find sum num  =
            if num = 0 then num
            else
                let sum = sum + estimateCharLength digits[num]
                if sum < maxLength then
                    find sum (num - 1)
                else
                    num
        digits[(find 0. lastDigitIndex)..lastDigitIndex]
             

    let preLength = if radix = Dec then 0. else 0.75


    if preLength + estimateLength digits <= maxChars then // length is OK
        pre + digits
    else
        let digitSpace = maxChars - preLength - dotsLength            
        // truncate MS digits replacing by '...'
        let truncatedDigits = findBestLength digitSpace digits
        (pre
        + "..."
        + truncatedDigits)

/// Print a FastData as a string with a given radix and width.
/// If the string is too long, truncate it.
/// MaxChars: the max printedlength of the string in char widths (emm).
/// Radix: the radix to use for the string.
/// Fd: the FastData to print.
let fastDataToPaddedString maxChars radix (fd: FastData) =
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

        valToPaddedString fd.Width displayRadix (bigint w)

    | BigWord big ->
        let displayRadix =
            match displayRadix with
            | Hex when fd.Width > Constants.maxHexOnlyDisplayWidth && (decBignum big).Length < 3 -> SDec
            | r -> r
        printWithFill fd.Width displayRadix big
    |> (fun s ->
        match s.Length < maxChars with
        | true -> s // no change needed
        | false ->
            compress fd.Width maxChars displayRadix s)

/// Print a uint32 (n) as a string with a given radix and width.
/// If the string is too long, truncate it.
/// MaxChars: the max printedlength of the string in char widths (emm).
/// Radix: the radix to use for the string.
/// width: the bus width within which n should be printed.
let UInt32ToPaddedString maxChars radix (width: int) (n: uint32) =
    fastDataToPaddedString maxChars radix {Dat = Word n; Width = width}

/// Print a bigint (n) as a string with a given radix and width.
/// If the string is too long, truncate it.
/// MaxChars: the max printedlength of the string in char widths (emm).
/// Radix: the radix to use for the string.
/// width: the bus width within which n should be printed.
let BigIntToPaddedString maxChars radix (width: int) (n: bigint) =
    fastDataToPaddedString maxChars radix {Dat = BigWord n; Width = width}
 

//-------------------------------------------------------------------------------------------//
//---------------------------------WireData Conversion---------------------------------------//
//-------------------------------------------------------------------------------------------//

// this should be rationalised using otehr functions.

/// Convert a bit to string.
let bitToString (bit: Bit) : string =
    match bit with
    | Zero -> "0"
    | One -> "1"

/// Convert an int into a Bit list with the provided width. The Least
/// Significant Bits are the one with low index (e.g. LSB is at position 0, MSB
/// is at position N). Little Endian.
/// If the number has more bits than width, then more bits will be returned.
let convertIntToWireData (width: int) (num: bigint) : WireData =
    /// Pad wireData with Zeros as the Most Significant Bits (e.g. at position N).
    let padToWidth width (bits: WireData) : WireData =
        if bits.Length > width then
            List.truncate width bits
        else
            bits @ List.replicate (width - bits.Length) Zero
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

//----------------------------------------------------------------------------------------------------------//
//----------------------------------------------FastData Conversion-----------------------------------------//
//----------------------------------------------------------------------------------------------------------//

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


//--------------------------------------------------------------------------------------------------------------//
//------------------------------------Numeric Parsing of Radixed Strings----------------------------------------//
//--------------------------------------------------------------------------------------------------------------//

/// Try to convert a string to a bigint, or return an error message if that was
/// not possible.
let strToBigint (str: string) : Result<bigint, string> =
    let removeCommas (str: string) =
        str.Replace(",", "")
    let str = str.ToLower().Trim()
    let str = if str.Length > 1 && str[0] = 'x' || str[0]='b' || str.Length = 0 then "0" + str else str
    let success, n =
        str
        |> removeCommas
        |> bigint.TryParse 
    match success with
    | false -> Error "Invalid number."
    | true -> Ok n

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

/// Converts a binary, hex or decimal number to decimal
/// Non-standard - used by Verilog code. Should rationalise?
let toDecimal (num: string) numBase (width:string) =
    /// Convert a hex string into a binary string without unnecessary leading zeros.
    let convertBinToDec (bits: string) : bigint =
        let rec convert bits idx =
            match bits with
            | [] -> 0I
            | '0' :: bits' -> convert bits' (idx - 1)
            | '1' :: bits' -> (1I <<< idx) + convert bits' (idx - 1)
            | _ -> failwithf "Not binary input, should not happen!"
        convert ((bits.ToCharArray()) |> Array.toList) (bits.Length-1)

    let width = width |> int
    match numBase with
    | "'d" -> num |> bigint.Parse
    | "'b" -> num |> convertBinToDec
    | "'h" -> 
        num.ToLower()  |> hexToBin |> convertBinToDec
    | _ -> failwithf "Wrong base, should not happen!"
