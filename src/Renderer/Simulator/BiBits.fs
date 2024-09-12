module BiBits

//------------------------------------------------------------------------------//
//-------------------EXPERIMENTAL - new data structure to replace WireData------//
//------------------------------------------------------------------------------//


// Currently this code is not used, because it is experimental and not yet implemented in the project.


type BitInt = uint32 array

let getBIBit (bits: BitInt) (pos: int) : uint32 = bits[pos / 32] >>> (pos % 32)

/// get a field of bits width 'width' offset by 'offset', return the field with 0 offset
let inline getUpperField (x: uint32) (width: int) (offset: int) : uint32 =
    (x >>> offset) &&& ((1u <<< width) - 1u)

/// get the lower 'width' bits, return then offset by 'offset' bits
let inline getLowerField (x: uint32) (width: int) (offset: int) : uint32 =
    (x &&& ((1u <<< width) - 1u)) <<< offset

let getBIBitsInt (bits: BitInt) (msb: int) (lsb: int) : uint32 =
    let width = msb - lsb + 1

    if width < 32 then
        let lowerWord = bits[lsb / 32]
        let offset = lsb % 32
        let lowerChunk = (lowerWord >>> offset)

        if offset + width <= 32 then
            // output from only one word
            lowerChunk
        else
            // one word output from two words of source
            let upperChunk = getLowerField bits[lsb / 32 + 1] (width - offset - 32) offset
            lowerChunk ||| upperChunk
    else
        failwithf "Cannot extract bits {msb}..{lsb} as a single 32 bit word"

let getBIBits (bits: BitInt) (msb: int) (lsb: int) : BitInt =
    let lsw = lsb / 32
    let outWidth = msb - lsb + 1
    let msw = msb / 32
    let offset = lsb % 32

    if offset = 0 then
        bits[msw..lsw]
    else
        let outWords = outWidth / 32 + 1

        Array.init outWords (fun n ->
            match n with
            | n when n + lsw = msw -> getLowerField bits[msw] (outWidth - offset % 32) offset
            | n ->
                getLowerField bits[n + lsw + 1] (32 - offset) offset
                ||| getUpperField bits[n + lsw] offset offset)

let floatCarryBit = Seq.init 32 (fun _ -> 2.) |> Seq.reduce (*)

let addBIBits (bits1: BitInt) (bits2: BitInt) (cin: uint32) : BitInt * uint32 =
    let mutable tempCarry = if cin = 1u then floatCarryBit else 0.

    let outs =
        Array.init bits1.Length (fun n ->
            tempCarry <-
                float bits1[n]
                + float bits2[n]
                + (if tempCarry >= floatCarryBit then
                       1.
                   else
                       0.)

            uint32 tempCarry)

    outs,
    (if tempCarry >= floatCarryBit then
         1u
     else
         0u)

let binopBIBits (op: uint32 -> uint32 -> uint32) (bits1: BitInt) (bits2: BitInt) : BitInt =
    Array.init bits1.Length (fun n -> op bits1[n] bits2[n])

/// invert bits1: assuming that width is the bit width of bits1
/// MS bits not used by bits1 are not inverted.
let invertBIBits (bits: BitInt) (width: int) =
    let msw = width / 32

    Array.init bits.Length (fun n ->
        let x = bits[n]

        if n = msw then
            x &&& ((1u <<< width % 32) - 1u)
        else
            x ^^^ 0xFFFFFFFFu)

/// append bits2 on MSB side of bits1
let appendBIBits ((bits1: BitInt, width1: int)) ((bits2: BitInt, width2: int)) =
    let outWidth = width1 + width2
    let outMSW = outWidth / 32
    let offset = width1 % 32
    let msw1 = width1 / 32

    if offset = 0 then
        // we can do straight array append
        Array.append bits1 bits2
    elif outMSW = width1 / 32 then
        // the added bits can be put in the existing MSW of width1
        let out = Array.copy bits1
        out[outMSW] <-
            out[outMSW]
            ||| getLowerField bits2[0] width2 offset
        out
    else
        Array.init (outMSW + 1) (fun n ->
            match n with
            | _ when n = outMSW ->
                getLowerField bits2[n - msw1] (32 - offset) offset
                ||| getUpperField bits2[n - msw1 + 1] (offset + outWidth - 32) offset

            | _ when n = width1 / 32 ->
                getLowerField bits1[n - width1 % 32] (32 - offset) offset
                ||| getUpperField bits2[n - width1 / 32 + 1] offset offset
            | _ when n >= width1 / 32 ->
                getLowerField bits2[n - width1 / 32] (32 - offset) offset
                ||| getUpperField bits2[n - width1 / 32 + 1] offset offset
            | _ -> bits1[n])

