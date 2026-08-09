module EvalKernel

open CommonTypes
open SimGraphTypes
open SimTypes
open NumberHelpers
open System
open Helpers

//------------------------------------------------------------------------------//
//------------The primitives every component evaluator is built from------------//
//------------------------------------------------------------------------------//
//
// Bit and gate operations, bus masks, and the memory accessors. Shared by all three
// evaluators - EvalReference, EvalCompiled and EvalAlgebraic - which is why they are here
// rather than in any one of them: each used to reach into EvalReference for these and take
// its whole dispatcher as a dependency along with them.
let inline assertThat cond msg =
    if not cond then
        failwithf "what? assert failed: %s" msg

/// Assert that the FData only contain a single bit, and return such bit.
let inline extractBit (fd: FastData) (busWidth: int) : uint32 =
#if ASSERTS
    assertThat (fd.Width = 1 || fd.Width = 2 || fd.Width = 3)
    <| sprintf "extractBit called with wireData: %A" fd
#endif
    match fd.Dat with
    | Word n -> n
    | BigWord _ -> failwithf $"Can't extract %d{busWidth} bit from BigWord data {fd.Dat} of width {fd.Width}"

let inline extractBitFData (fd_: FData) (busWidth: int) : uint32 =
    match fd_ with
    | Alg _ -> failwithf "Can't extract data from Algebra"
    | Data fd ->
#if ASSERTS
        assertThat (fd.Width = 1 || fd.Width = 2 || fd.Width = 3)
        <| sprintf "extractBit called with wireData: %A" fd
#endif
        match fd.Dat with
        | Word n -> n
        | BigWord _ -> failwithf $"Can't extract %d{busWidth} bit from BigWord data {fd.Dat} of width {fd.Width}"

let inline packBit (bit: uint32) : FastData =
    if bit = 0u then
        { Dat = Word 0u; Width = 1 }
    else
        { Dat = Word 1u; Width = 1 }

let inline packBitFData (bit: uint32) : FData =
    if bit = 0u then
        Data { Dat = Word 0u; Width = 1 }
    else
        Data { Dat = Word 1u; Width = 1 }

/// Read the content of the memory at the specified address.
let readMemoryAddrUInt32DataUInt32 (mem: Memory1) (address: uint32) : uint32 =
    let outDataInt = Helpers.getMemData (bigint address) mem
    convertBigintToUInt32 mem.WordWidth outDataInt

let readMemoryAddrUInt32DataBigInt (mem: Memory1) (address: uint32) : bigint =
    Helpers.getMemData (bigint address) mem

let readMemoryAddrBigIntDataUInt32 (mem: Memory1) (address: bigint) : uint32 =
    let outDataInt = Helpers.getMemData address mem
    convertBigintToUInt32 mem.WordWidth outDataInt

let readMemoryAddrBigIntDataBigInt (mem: Memory1) (address: bigint) : bigint =
    Helpers.getMemData address mem

let readMemoryFData (mem: Memory1) (address: FData) : FData =
    match address with
    | Alg _ -> failwithf "Can't read memory from Algebra"
    | Data addr ->
        let addr = convertFastDataToBigint addr
        Helpers.getMemData addr mem
        // the value read is a data word, so it is WordWidth wide. This used to say
        // AddressWidth, which mislabelled every read and, when WordWidth > 32 >= AddressWidth,
        // sent it down the uint32 branch of convertBigintToFastData and truncated it
        |> convertBigintToFastData mem.WordWidth
        |> Data

/// Write the content of the memory at the specified address.
let writeMemory (mem: Memory1) (address: FastData) (data: FastData) : Memory1 =
    let intAddr = convertFastDataToBigint address
    let intData = convertFastDataToBigint data

    { mem with Data = Map.add intAddr intData mem.Data }

let writeMemoryAddrUInt32DataUInt32 (mem: Memory1) (address: uint32) (data: uint32) : Memory1 =
    let intAddr = twosComp mem.AddressWidth (bigint address)
    let intData = twosComp mem.WordWidth (bigint data)

    { mem with Data = Map.add intAddr intData mem.Data }

let writeMemoryAddrUInt32DataBigInt (mem: Memory1) (address: uint32) (data: bigint) : Memory1 =
    let intAddr = twosComp mem.AddressWidth  (bigint address)
    let intData = twosComp mem.WordWidth data

    { mem with Data = Map.add intAddr intData mem.Data }

let writeMemoryAddrBigIntDataUInt32 (mem: Memory1) (address: bigint) (data: uint32) : Memory1 =
    let intAddr = twosComp mem.AddressWidth address
    let intData = twosComp mem.WordWidth (bigint data)

    { mem with Data = Map.add intAddr intData mem.Data }

let writeMemoryAddrBigIntDataBigInt (mem: Memory1) (address: bigint) (data: bigint) : Memory1 =
    let intAddr = twosComp mem.AddressWidth address
    let intData = twosComp mem.WordWidth data

    { mem with Data = Map.add intAddr intData mem.Data }

let getRamStateMemory numSteps step (state: StepArray<SimulationComponentState> option) memory : Memory1 =
    match state, numSteps with
    | _, 1 -> memory
    | Some arr, _ ->
        match arr.Step[step] with
        | RamState memory -> memory
        | _ -> failwithf "What? getRamStateMemory called with invalid state"
    | _ -> failwithf "what? getRamStateMemory called with an invalid state: %A" state

let getRomStateMemory comp =
    match comp.FType with
    | ROM memory
    | AsyncROM memory -> memory
    | _ -> failwithf "What? getRomStateMemory called with invalid state"



let inline bitNot bit = bit ^^^ 1u

let inline bitNotB width bit = (1I <<< width) - 1I - bit

/// Increment, wrapping to 0 at 2^width. Used by the counters on the uint32 path, which
/// counted in bigint and so allocated several heap bigints per component per step on what
/// is meant to be the fast path. At width 32 uint32 addition wraps to 0 of its own accord,
/// which is the wanted result, and the wrap test cannot be written there in any case since
/// 1u <<< 32 is 1u.
let inline incrementWithinWidth (width: int) (lastOut: uint32) =
    let next = lastOut + 1u

    if width < 32 && next = (1u <<< width) then
        0u
    else
        next

let inline bitAnd bit0 bit1 = bit0 &&& bit1

let inline bitOr bit0 bit1 = bit0 ||| bit1

let inline bitXor bit0 bit1 = bit0 ^^^ bit1

let inline bitNand bit0 bit1 = bitAnd bit0 bit1 |> bitNot

let inline bitNor bit0 bit1 = bitOr bit0 bit1 |> bitNot

let inline bitXnor bit0 bit1 = bitXor bit0 bit1 |> bitNot

let inline bitGate gateType =
    match gateType with
    | And | Nand -> bitAnd
    | Or | Nor -> bitOr
    | Xor | Xnor -> bitXor


let inline  getBinaryOp gateType =
    match gateType with
    | And -> bitAnd
    | Or -> bitOr
    | Xor -> bitXor
    | Nand -> bitNand
    | Nor -> bitNor
    | Xnor -> bitXnor

let inline algNot exp = UnaryExp(NotOp, exp)

let inline  algAnd exp1 exp2 = BinaryExp(exp1, BitAndOp, exp2)

let inline  algOr exp1 exp2 = BinaryExp(exp1, BitOrOp, exp2)

let inline  algXor exp1 exp2 = BinaryExp(exp1, BitXorOp, exp2)

let inline  algNand exp1 exp2 = algAnd exp1 exp2 |> algNot

let inline  algNor exp1 exp2 = algOr exp1 exp2 |> algNot

let inline  algXnor exp1 exp2 = algXor exp1 exp2 |> algNot

let inline  algGate gateType =
    match gateType with
    | And | Nand-> algAnd
    | Or | Nor -> algOr
    | Xor | Xnor -> algXor
