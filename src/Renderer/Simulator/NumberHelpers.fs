(*
    NumberHelpers.fs

    A collection of functions that allow to covert numbers into various
    representations.
*)

module OldNumberHelpers
open CommonTypes
open Helpers
open SimulatorTypes

module Constants =
    let maxBinaryDisplayWidth = 32

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

/// Format a hex or bin printed string adding commas every 4 digits.
/// If pad with 0s up to width bits.
/// if width = 0 work ok with no padding
let addCommasAndZeros (width: int) (printedChars:string) =
    let divCeiling n divisor = (n - 1 + divisor) / divisor
    let nonZeroDigits = printedChars[2..printedChars.Length-1]
    let numDigits = nonZeroDigits.Length
    let bitsPerDigit = 
        match printedChars[1] with
        | 'x' -> 4
        | 'b' -> 1
        | r -> failwithf "Wrong use of addZeros: radix Char = %c" r
    let extraZerosNum = 
        (width - numDigits*bitsPerDigit)
        |> max 0
        |> fun n -> divCeiling n bitsPerDigit
        |> min 64
    let digits = String.replicate extraZerosNum "0" + nonZeroDigits
    let num4Chunks = divCeiling digits.Length 4
    let commaSeparatedDigits =
        match num4Chunks with
        | 1 -> 
            digits // no ',' added
        | _ ->
            digits
            |> Seq.rev
            |> Seq.chunkBySize 4
            |> Seq.rev
            |> Seq.map (Seq.rev >> System.String.Concat)
            |> String.concat ","
    printedChars[0..1] + commaSeparatedDigits

let addZeros64 (width:int) (pFun:int64 -> string) (n: int64) =
    pFun n
    |> addCommasAndZeros width


let addZeros (width:int) (pFun:int -> string) (n: int) =
    pFun n
    |> addCommasAndZeros width

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

let big8 = 256I
let big16 = 65536I
let big32 = 1I <<< 32
let big64 = 1I <<< 64


/// print a bignum according to a radix.
/// if 
let rec bigValToPaddedString (width: int) (radix: NumberBase)  (x: System.Numerics.BigInteger) =
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
            if x >= (1I <<< width - 1)
            then
                "-" + ((1I <<< width) - x).ToString()
            else
                x.ToString()
        | Dec -> 
            x.ToString()
        | Bin
        | Hex ->
            if width <= 64 then
                (match radix with 
                 | Bin -> fillBin64 
                 | Hex -> fillHex64 
                 | _ -> failwithf "Can't happen") width (int64 (uint64 x))
            else
                bigValToPaddedString  (width - 64) radix (x / (1I <<< 64)) + "," +
                (match radix with
                 | Hex -> (fillHex64 64 (int64 (uint64 (x % (1I <<< 64)))))[2..65]
                 | _   -> (fillBin64 64 (int64 (uint64 (x % (1I <<< 64)))))[2..65])
            
let rec bigValToString (radix: NumberBase) (x: System.Numerics.BigInteger) =
    if x < 0I then
        $"Bignum {x} is negative"
    else
        match radix with
        | Dec 
        | SDec -> /// can't know if sign is negative in this case
            x.ToString()
        | Bin 
        | Hex ->
            if x <= (1I <<< 64) then
                (match radix with 
                        | Bin -> bin64 >> addCommasAndZeros 0
                        | Hex -> hex64 >> addCommasAndZeros 0
                        | _ -> failwithf "Can't happen") (int64 (uint64 x))
            elif radix = Bin then
                "Can't display binary format > 64 bits"
            else
                bigValToString radix (x / (1I <<< 64)) + "," +
                $"%16X{uint64(x % 1I <<< 64)}"
            

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
    | Hex | Bin -> fillHex64 width value
    | SDec -> sDec64 value

/// Pad wireData with Zeros as the Most Significant Bits (e.g. at position N).
let private padToWidth width (bits : WireData) : WireData =
    if bits.Length > width then List.truncate width bits
    else bits @ List.replicate (width - bits.Length) Zero

let fastDataToPaddedString maxChars radix  (fd: FastData) =
    let getPrefixAndDigits (s:string) =
        match s.Length, s.[0], s.[1] with
        | n, '-', _ ->
            "-", s[1..n-1]
        | n, '0', 'b' 
        | n, '0', 'x' ->
            s.[1..1], s[2..n-1]
        | _ -> 
            "",s


    let stripLeadingZeros s =
        let rec strip index (s:string) =
            if index < s.Length - 1 && (s[index] = '0' || s[index] = ',') then
                strip (index+1) s
            else
                s[index..s.Length-1]
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
            let pre' = (match displayRadix with | Dec | SDec -> "" | _ -> $"{fd.Width}'") + pre
            if pre'.Length + digits.Length <= maxChars then
                // stripping leading zeros makes length ok
                pre' + digits
            else
                // truncate MS digits replacing by '..'
                pre' + ".." + digits[n + 2 + pre'.Length - maxChars..n - 1])



            

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
            BigWord (bigint n' % (1I <<< width))
        else 
            let mask = if width = 32 then 0xFFFFFFFFu else (1u <<< width) - 1u
            Word (uint32 n' &&& mask)
    {Dat=dat; Width = width}

let convertIntToFastData (width:int) (n:uint32) =
    if width <= 32 then 
        {Dat = Word n; Width = width}
    else
        {Dat = BigWord (bigint n); Width = width} 

let convertBigintToFastData (width:int) (b:bigint) =
        {Dat = BigWord b; Width = width}

/// convert to 64 bits - if too large take LS 64 bits
let convertFastDataToInt64 (d:FastData) =
    match d.Dat with
    | Word n -> uint64 n
    | BigWord b -> 
        if d.Width > 64 then
            b % (1I <<< 64)
        else b
        |> uint64
/// convert to a bigint - always works. Bits < width will be correct.
let convertFastDataToBigint (d:FastData) =
    match d.Dat with
    | Word n -> bigint n
    | BigWord n -> n

/// convert to int with an exception if data is too large
let convertFastDataToInt (d:FastData) =
    match d.Dat with
    | Word n -> n
    | BigWord _ -> failwithf $"Can't convert {d.Dat} to integer" 

/// Lossy conversion of bigint to int32 without exceptions
/// TODO: chnage this - and all dependencies with 32 bit int - to int64
let convertFastDataToInt32 (d:FastData) =
    match d.Dat with
    | Word n -> int32 n
    | BigWord n -> int32 (n &&& bigint 0xffffffff)

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
            big64ToWire 64 lsBits @ bigToWire (width - 64) (b / (1I <<< 64))
    match fastDat.Dat with
    | Word w ->
        convertIntToWireData fastDat.Width (int64 w)       
    | BigWord b ->
        bigToWire fastDat.Width b

let convertWireDataToFastData (wd: WireData) =
    if wd.Length <= 32 then
        {Dat = Word (uint32 (uint64 (convertWireDataToInt wd))) ; Width = wd.Length}
    else
        List.indexed wd
        |> List.map (fun (i, bit) -> match bit with | Zero ->  0I  | One -> (1I <<< i))
        |> List.sum
        |> (fun big -> {Dat = BigWord big; Width = wd.Length})

            
            
        
        

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
        let hexDigits = s[2..s.Length-1]
        let convDigits = hexDigits |> List.map cDigitInt 
        if checkRadix 16

*)

let private countBits (num : int64) : int =
    (String.length <| bin64 num) - 2

/// Check a number is formed by at most <width> bits.
let rec checkWidth (width : int) (num : int64) : string option =
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
