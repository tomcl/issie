module FastReduce

open CommonTypes
open SimGraphTypes
open SimTypes
open NumberHelpers
open System
open Helpers

//------------------------------------------------------------------------------//
//-----------------------------Fast Reduction of Components---------------------//
//------------------------------------------------------------------------------//

//------------------------------------------------------------------------------//
// ----------Subfunctions used by fastReduce to evaluate a component-----------//
//------------------------------------------------------------------------------//

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
let private readMemoryAddrUInt32DataUInt32 (mem: Memory1) (address: uint32) : uint32 =
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
        |> convertBigintToFastData mem.AddressWidth
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

//---------------------------------------------------------------------------------------//
// ------------------------------MAIN COMPONENT SIMULATION FUNCTION----------------------//
//--------------------------------------fastReduce---------------------------------------//
//---------------------------------------------------------------------------------------//

/// Given a component, compute its outputs from its inputs, which must already be evaluated.
/// Outputs and inputs are both contained as time sequences in arrays. This function will calculate
/// simStep outputs from (previously calculated) simStep outputs and clocked (simStep-1) outputs.
/// Memory has state separate from simStep-1 output, for this the state is recalculated.
/// Inputs and outputs come from either UInt32Step or BigIntStep arrays in IOArray record.
let fastReduce (maxArraySize: int) (numStep: int) (isClockedReduction: bool) (comp: FastComponent) : Unit =
    let componentType = comp.FType

    // printfn "Reducing %-30A │ ID=%A │ Name=%20A │ Step=%6d │ clocked=%A"  comp.FType comp.ShortId comp.FullName numStep isClockedReduction
    // printfn "Reducing %A...%A %A (step %d) clocked=%A"  comp.FType comp.ShortId comp.FullName numStep isClockedReduction
    let n = comp.InputLinks.Length

    let simStep = numStep % maxArraySize
    let simStepOld =
        if simStep = 0 then
            maxArraySize - 1
        else
            simStep - 1
#if ASSERTS
    printfn "Warning: simulation is running with ASSERTS on for debugging -this will be very slow!"
#endif

    ///  get data feom input i of component
    let inline checkInputPortNumber i =
#if ASSERTS
        let n = comp.InputLinks.Length
        assertThat
            (i < n)
            (sprintf
                "What? Invalid input port (%d:step%d) used by %s:%s (%A) reducer with %d Ins"
                i
                simStep
                comp.FullName
                comp.ShortId
                componentType
                n)
#else
        ()
#endif

    let inline checkOutputWidth () =
#if ASSERTS
        if comp.OutputWidth 0 = 0 then
            failwithf "Can't reduce %A (%A) because outputwidth is not known" comp.FullName comp.FType
#else
        ()
#endif

    ///  get data feom input i of component
    let inline insUInt32 i =
        checkInputPortNumber i
        comp.InputLinks[i].UInt32Step[simStep]

    let inline insBigInt i =
        checkInputPortNumber i
        comp.InputLinks[i].BigIntStep[simStep]

    /// get last cycle data from output i (for clocked components)
    let inline getLastCycleOutUInt32 n =
        checkOutputWidth ()
        match numStep with
        | 0 -> 0u
        | _ -> comp.Outputs[n].UInt32Step[simStepOld]

    let inline getLastCycleOutBigInt n =
        checkOutputWidth ()
        match numStep with
        | 0 -> 0I
        | _ -> comp.Outputs[n].BigIntStep[simStepOld]

    /// get last cycle data from output i for component
    let inline insOldUInt32 i =
        checkInputPortNumber i
        match numStep with
        | 0 -> 0u
        | _ -> comp.GetInputUInt32 (simStepOld) (InputPortNumber i)

    let inline insOldBigInt i =
        checkInputPortNumber i
        match numStep with
        | 0 -> 0I
        | _ -> comp.GetInputBigInt (simStepOld) (InputPortNumber i)

    /// Write current step output data for output port pn
    let inline putUInt32 pn fd = comp.PutOutputUInt32 (simStep) (OutputPortNumber pn) fd
    let inline putBigInt pn fd = comp.PutOutputBigInt (simStep) (OutputPortNumber pn) fd

    /// Write current State (used only for RAMs, DFFs and registers use previous cycle output as state)
    let inline putState state =
        match comp.State with
        | None -> failwithf "Attempt to put state into component %s without state array" comp.FullName
        | Some stateArr -> stateArr.Step[simStep] <- state
    
    let inline getBinaryGateReducer (bitOp: uint32 -> uint32 -> uint32) : Unit =
        let bit0, bit1 = insUInt32 0, insUInt32 1
        putUInt32 0 <| bitOp bit1 bit0

    /// implement a binary combinational operation for n inputs
    let inline getNInpBinaryGateReducer (nIns: int) (gateType: GateComponentType) : Unit =
        let mutable gateResult = insUInt32 0
        for gateInputNum = 1 to nIns-1 do
            gateResult <- bitGate gateType gateResult (insUInt32 gateInputNum)
        if (isNegated gateType) then
            putUInt32 0 (bitNot gateResult)
        else
            putUInt32 0 gateResult

    /// Error checking (not required in production code) check widths are consistent
    let inline checkWidth width w =
#if ASSERTS
        assertThat
            (w = width)
            (sprintf
                "Input node reducer for (%A:%A - STEP %d) received wrong number of bits: expected %d but got %d"
                comp.FullName
                comp.FType
                simStep
                width
                w)
#else
        ()
#endif

    // Reduce the component in this match.
    // Each case case calculated the output for its type of component
    // NB CustomComponent is not needed since these are removed in the FastComponent
    // generation stage - being replaced by the relavant sheet logic.

    match componentType, comp.UseBigInt with
    | ROM _, _
    | RAM _, _
    | AsyncROM _, _ -> failwithf "What? Legacy RAM component types should never occur"
    | Input _, _ -> failwithf "Legacy Input component types should never occur"
    | Input1(width, _), false ->
        if comp.Active then //REVIEW - is this neccessary?
            let bits = insUInt32 0
            //printfn "Got input 0 = %A Links=<%A> len=%d" bits comp.InputLinks comp.InputLinks.Length
            checkWidth width (comp.InputWidth 0)
            //printfn "output array = %A" comp.Outputs
            putUInt32 0 bits
    | Input1(width, _), true ->
        if comp.Active then
            let bits = insBigInt 0
            checkWidth width (comp.OutputWidth 0)
            putBigInt 0 bits
    | Constant1(width, cVal, _), false
    | Constant(width, cVal), false -> putUInt32 0 <| uint32 (convertBigintToInt32 cVal)
    | Constant1(width, cVal, _), true
    | Constant(width, cVal), true -> putBigInt 0 <| cVal
    | Output width, false ->
        let bits = insUInt32 0
        // printfn "In output bits=%A, ins = %A" bits comp.InputLinks
        checkWidth width (comp.InputWidth 0)
        putUInt32 0 bits
    | Output width, true ->
        let bits = insBigInt 0
        checkWidth width (comp.InputWidth 0)
        putBigInt 0 bits
    | Viewer width,false ->
        let bits = insUInt32 0
        //printfn "In output bits=%A, ins = %A" bits comp.InputLinks
        checkWidth width (comp.InputWidth 0)
        putUInt32 0 bits
    | Viewer width,true ->
        let bits = insBigInt 0
        checkWidth width (comp.InputWidth 0)
        putBigInt 0 bits

    | IOLabel,false ->
        let bits = insUInt32 0
        //let bits = comp.InputLinks[0][simStep]
        //printfn "Reducing IOLabel %A" comp.SimComponent.Label
        putUInt32 0 bits
    | IOLabel,true ->
        let bits = insBigInt 0
        putBigInt 0 bits
    | NotConnected, _ -> ()
    | Not, false ->
        let bit = insUInt32 0
        putUInt32 0 <| bitNot bit
    | BusSelection(width, lsb), false ->
#if ASSERTS
        let w = comp.InputWidth 0
        assertThat
            (w >= width + lsb)
            (sprintf "Bus Selection received too few bits: expected at least %d but got %d" (width + lsb) w)
#endif
        let bits = insUInt32 0
        let outBits = getBitsFromUInt32 (lsb + width - 1) lsb bits
        putUInt32 0 outBits
    | BusSelection(width, lsb), true ->
#if ASSERTS
        let w = comp.InputWidth 0
        assertThat
            (w >= width + lsb)
            (sprintf "Bus Selection received too few bits: expected at least %d but got %d" (width + lsb) w)
#endif
        let bits = insBigInt 0
        match comp.BigIntState with
        | None -> failwith "This should never happen, BusSelection with UseBigInt = true must have Some(BigIntState), but get None"
        | Some { InputIsBigInt = _; OutputIsBigInt = outs } ->
            if outs[0] then
                let outBits = getBitsFromBigInt (lsb + width - 1) lsb bits
                putBigInt 0 outBits
            else
                let outBits = getBitsFromBigIntToUInt32 (lsb + width - 1) lsb bits
                putUInt32 0 outBits
    | BusCompare(width, compareVal), false
    | BusCompare1(width, compareVal, _), false ->
        //printfn "Reducing compare %A" comp.SimComponent.Label
#if ASSERTS
        let w = comp.InputWidth 0
        assertThat
            (w = width)
            ($"Bus Compare {comp.FullName} received wrong number of bits: expecting  {width} but got {w}")
#endif
        let bits = insUInt32 0
        let inputNum = bigint bits
        let outNum =
            if inputNum = compareVal then
                1u
            else
                0u

        putUInt32 0 outNum
    | BusCompare(width, compareVal), true
    | BusCompare1(width, compareVal, _),true ->
#if ASSERTS
        let w = comp.InputWidth 0
        assertThat
            (w = width)
            ($"Bus Compare {comp.FullName} received wrong number of bits: expecting  {width} but got {w}")
#endif
        let bits = insBigInt 0
        let inputNum = bits
        let outNum =
            if inputNum = compareVal then
                1u
            else
                0u

        putUInt32 0 outNum

    | GateN (gateType, n),false ->
        if n = 2 then
            getBinaryGateReducer (getBinaryOp gateType)
        else
            getNInpBinaryGateReducer n gateType

    | Not, true
    | GateN _, true -> failwith "This should never happen, 1-bit component should not use BigInt"

    | Mux2,false ->
#if ASSERT
        let w0, w1 = (comp.InputWidth 0), (comp.InputWidth 1)
        assertThat (w0 = w1) <| sprintf "Mux2 %s received two inputs with different widths: (%A) <> (%A)" comp.FullName w0 w1
#endif
        let bits0, bits1, bitSelect = (insUInt32 0), (insUInt32 1), (insUInt32 2)
        let out =
            if bitSelect = 0u then
                bits0
            else
                bits1

        putUInt32 0 out
    | Mux2,true->
#if ASSERT
        let w0, w1 = (comp.InputWidth 0), (comp.InputWidth 1)
        assertThat (w0 = w1) <| sprintf "Mux2 %s received two inputs with different widths: (%A) <> (%A)" comp.FullName w0 w1
#endif
        let bits0, bits1, bitSelect = (insBigInt 0), (insBigInt 1), (insUInt32 2)
        let out =
            if bitSelect = 0u then
                bits0
            else
                bits1

        putBigInt 0 out
    | Mux4,false ->
#if ASSERT
        let w0, w1, w2, w3 = (comp.InputWidth 0), (comp.InputWidth 1), (comp.InputWidth 2), (comp.InputWidth 3)
        assertThat (
            w0 = w1
            && w0 = w2
            && w0 = w3
        )
        <| sprintf
            "Mux4 %s received two inputs with different widths: (%A) <> (%A) <> (%A) <> (%A)"
            comp.FullName
            w0
            w1
            w2
            w3
#endif
        let bits0, bits1, bits2, bits3, bitSelect = (insUInt32 0), (insUInt32 1), (insUInt32 2), (insUInt32 3), (insUInt32 4)
        let out =
            match bitSelect with
            | 0u -> bits0
            | 1u -> bits1
            | 2u -> bits2
            | 3u -> bits3
            | _ -> failwithf "Cannot happen"

        putUInt32 0 out
    | Mux4,true ->
#if ASSERT
        let w0, w1, w2, w3 = (comp.InputWidth 0), (comp.InputWidth 1), (comp.InputWidth 2), (comp.InputWidth 3)
        assertThat (
            w0 = w1
            && w0 = w2
            && w0 = w3
        )
        <| sprintf
            "Mux4 %s received two inputs with different widths: (%A) <> (%A) <> (%A) <> (%A)"
            comp.FullName
            w0
            w1
            w2
            w3
#endif
        let bits0, bits1, bits2, bits3, bitSelect = (insBigInt 0), (insBigInt 1), (insBigInt 2), (insBigInt 3), (insUInt32 4)
        let out =
            match bitSelect with
            | 0u -> bits0
            | 1u -> bits1
            | 2u -> bits2
            | 3u -> bits3
            | _ -> failwithf "Cannot happen"

        putBigInt 0 out
    | Mux8,false ->
#if ASSERT
        let w0, w1, w2, w3, w4, w5, w6, w7 = (comp.InputWidth 0), (comp.InputWidth 1), (comp.InputWidth 2), (comp.InputWidth 3), (comp.InputWidth 4), (comp.InputWidth 5), (comp.InputWidth 6), (comp.InputWidth 7)
        assertThat (
            (w0) = (w1)
            && (w0) = (w2)
            && (w0) = (w3)
            && (w0) = (w4)
            && (w0) = (w5)
            && (w0) = (w6)
            && (w0) = (w7)
        )
        <| sprintf
            "Mux8 %s received two inputs with different widths: (%A) <> (%A) <> (%A) <> (%A) <> (%A) <> (%A) <> (%A) <> (%A)"
            comp.FullName
            w0
            w1
            w2
            w3
            w4
            w5
            w6
            w7
#endif
        let bits0, bits1, bits2, bits3, bits4, bits5, bits6, bits7, bitSelect = insUInt32 0, insUInt32 1, insUInt32 2, insUInt32 3, insUInt32 4, insUInt32 5, insUInt32 6, insUInt32 7, insUInt32 8
        let out =
            match bitSelect with
            | 0u -> bits0
            | 1u -> bits1
            | 2u -> bits2
            | 3u -> bits3
            | 4u -> bits4
            | 5u -> bits5
            | 6u -> bits6
            | 7u -> bits7
            | _ -> failwithf "Cannot happen"

        putUInt32 0 out
    | Mux8,true ->
#if ASSERT
        let w0, w1, w2, w3, w4, w5, w6, w7 = (comp.InputWidth 0), (comp.InputWidth 1), (comp.InputWidth 2), (comp.InputWidth 3), (comp.InputWidth 4), (comp.InputWidth 5), (comp.InputWidth 6), (comp.InputWidth 7)
        assertThat (
            (w0) = (w1)
            && (w0) = (w2)
            && (w0) = (w3)
            && (w0) = (w4)
            && (w0) = (w5)
            && (w0) = (w6)
            && (w0) = (w7)
        )
        <| sprintf
            "Mux8 %s received two inputs with different widths: (%A) <> (%A) <> (%A) <> (%A) <> (%A) <> (%A) <> (%A) <> (%A)"
            comp.FullName
            w0
            w1
            w2
            w3
            w4
            w5
            w6
            w7
#endif
        let bits0, bits1, bits2, bits3, bits4, bits5, bits6, bits7, bitSelect = insBigInt 0, insBigInt 1, insBigInt 2, insBigInt 3, insBigInt 4, insBigInt 5, insBigInt 6, insBigInt 7, insUInt32 8
        let out =
            match bitSelect with
            | 0u -> bits0
            | 1u -> bits1
            | 2u -> bits2
            | 3u -> bits3
            | 4u -> bits4
            | 5u -> bits5
            | 6u -> bits6
            | 7u -> bits7
            | _ -> failwithf "Cannot happen"

        putBigInt 0 out
    | Demux2, false ->
        let fdIn, bitSelect = insUInt32 0, insUInt32 1

        let out0, out1 =
            if bitSelect = 0u then
                fdIn, 0u
            else
                0u, fdIn

        putUInt32 0 out0
        putUInt32 1 out1
    | Demux2, true ->
        let fdIn, bitSelect = insBigInt 0, insUInt32 1

        let out0, out1 =
            if bitSelect = 0u then
                fdIn, 0I
            else
                0I, fdIn

        putBigInt 0 out0
        putBigInt 1 out1
    | Demux4, false ->
        let fdIn, bitSelect = insUInt32 0, insUInt32 1

        let out0, out1, out2, out3 =
            match bitSelect with
            | 0u -> fdIn, 0u, 0u, 0u
            | 1u -> 0u, fdIn, 0u, 0u
            | 2u -> 0u, 0u, fdIn, 0u
            | 3u -> 0u, 0u, 0u, fdIn
            | _ -> failwithf "Cannot happen"

        putUInt32 0 out0
        putUInt32 1 out1
        putUInt32 2 out2
        putUInt32 3 out3
    | Demux4, true ->
        let fdIn, bitSelect = insBigInt 0, insUInt32 1

        let out0, out1, out2, out3 =
            match bitSelect with
            | 0u -> fdIn, 0I, 0I, 0I
            | 1u -> 0I, fdIn, 0I, 0I
            | 2u -> 0I, 0I, fdIn, 0I
            | 3u -> 0I, 0I, 0I, fdIn
            | _ -> failwithf "Cannot happen"

        putBigInt 0 out0
        putBigInt 1 out1
        putBigInt 2 out2
        putBigInt 3 out3
    | Demux8,false ->
        let fdIn, bitSelect = insUInt32 0, insUInt32 1

        let out0, out1, out2, out3, out4, out5, out6, out7 =
            match bitSelect with
            | 0u -> fdIn, 0u, 0u, 0u, 0u, 0u, 0u, 0u
            | 1u -> 0u, fdIn, 0u, 0u, 0u, 0u, 0u, 0u
            | 2u -> 0u, 0u, fdIn, 0u, 0u, 0u, 0u, 0u
            | 3u -> 0u, 0u, 0u, fdIn, 0u, 0u, 0u, 0u
            | 4u -> 0u, 0u, 0u, 0u, fdIn, 0u, 0u, 0u
            | 5u -> 0u, 0u, 0u, 0u, 0u, fdIn, 0u, 0u
            | 6u -> 0u, 0u, 0u, 0u, 0u, 0u, fdIn, 0u
            | 7u -> 0u, 0u, 0u, 0u, 0u, 0u, 0u, fdIn
            | _ -> failwithf "Cannot happen"

        putUInt32 0 out0
        putUInt32 1 out1
        putUInt32 2 out2
        putUInt32 3 out3
        putUInt32 4 out4
        putUInt32 5 out5
        putUInt32 6 out6
        putUInt32 7 out7
    | Demux8, true ->
        let fdIn, bitSelect = insBigInt 0, insUInt32 1

        let out0, out1, out2, out3, out4, out5, out6, out7 =
            match bitSelect with
            | 0u -> fdIn, 0I, 0I, 0I, 0I, 0I, 0I, 0I
            | 1u -> 0I, fdIn, 0I, 0I, 0I, 0I, 0I, 0I
            | 2u -> 0I, 0I, fdIn, 0I, 0I, 0I, 0I, 0I
            | 3u -> 0I, 0I, 0I, fdIn, 0I, 0I, 0I, 0I
            | 4u -> 0I, 0I, 0I, 0I, fdIn, 0I, 0I, 0I
            | 5u -> 0I, 0I, 0I, 0I, 0I, fdIn, 0I, 0I
            | 6u -> 0I, 0I, 0I, 0I, 0I, 0I, fdIn, 0I
            | 7u -> 0I, 0I, 0I, 0I, 0I, 0I, 0I, fdIn
            | _ -> failwithf "Cannot happen"

        putBigInt 0 out0
        putBigInt 1 out1
        putBigInt 2 out2
        putBigInt 3 out3
        putBigInt 4 out4
        putBigInt 5 out5
        putBigInt 6 out6
        putBigInt 7 out7
    | NbitsAdder numberOfBits, false
    | NbitsAdderNoCout numberOfBits, false ->
        let cin, a, b = insUInt32 0, insUInt32 1, insUInt32 2
        let sum, cout =
            let w = comp.InputWidth 1

            let mask = (1ul <<< w) - 1ul

            if w = 32 then
                // mask is not needed, but 64 bit adition is needed!
                let sumInt = uint64 a + uint64 b + uint64 (cin &&& 1u)
                let cout = uint32 (sumInt >>> w) &&& 1u
                let sum = uint32 sumInt
                sum, cout
            else
                let sumInt = (a &&& mask) + (b &&& mask) + (cin &&& 1u)
                let cout = (sumInt >>> w) &&& 1u
                let sum = sumInt &&& mask
                sum, cout

        match componentType with
        | NbitsAdder _ ->
            putUInt32 0 sum
            putUInt32 1 cout
        | _ -> putUInt32 0 sum
    | NbitsAdder numberOfBits, true
    | NbitsAdderNoCout numberOfBits, true ->
        let cin, a, b = insUInt32 0, insBigInt 1, insBigInt 2
        let sum, cout =
            let w = comp.InputWidth 1

            let mask = bigIntMask w
            let a = a &&& mask
            let b = b &&& mask
            let sumInt =
                if cin = 0u then
                    a + b
                else
                    a + b + 1I

            let sum = sumInt &&& bigIntMask w

            let cout =
                if (sumInt >>> w) = 0I then
                    0u
                else
                    1u
            sum, cout

        match componentType with
        | NbitsAdder _ ->
            putBigInt 0 sum
            putUInt32 1 cout
        | _ -> putBigInt 0   sum

    | NbitsAdderNoCin numberOfBits, false
    | NbitsAdderNoCinCout numberOfBits, false ->
        let a, b = insUInt32 0, insUInt32 1
        let sum, cout =
            let cin = 0u
            let w = comp.InputWidth 1

            let mask = (1ul <<< w) - 1ul

            if w = 32 then
                // mask is not needed, but 64 bit adition is needed!
                let sumInt = uint64 a + uint64 b + uint64 (cin &&& 1u)
                let cout = uint32 (sumInt >>> w) &&& 1u
                let sum = uint32 sumInt
                sum, cout
            else
                let sumInt = (a &&& mask) + (b &&& mask) + (cin &&& 1u)
                let cout = (sumInt >>> w) &&& 1u
                let sum = sumInt &&& mask
                sum, cout

        match componentType with
        | NbitsAdderNoCin _ ->
            putUInt32 0  sum
            putUInt32 1 cout
        | _ -> putUInt32 0  sum
    | NbitsAdderNoCin numberOfBits, true
    | NbitsAdderNoCinCout numberOfBits, true ->
        let a, b = insBigInt 0, insBigInt 1
        let sum, cout =
            let cin = 0u
            let w = comp.InputWidth 1

            let mask = bigIntMask w
            let a = a &&& mask
            let b = b &&& mask
            let sumInt =
                if cin = 0u then
                    a + b
                else
                    a + b + 1I

            let sum = sumInt &&& bigIntMask w

            let cout =
                if (sumInt >>> w) = 0I then
                    0u
                else
                    1u
            sum,  cout

        match componentType with
        | NbitsAdderNoCin _ ->
            putBigInt 0 sum
            putUInt32 1 cout
        | _ -> putBigInt 0 sum

    | NbitsXor(numberOfBits, op),false ->
        let a,b = insUInt32 0, insUInt32 1
        let res =
                    match op with
                    | None -> a ^^^ b
                    | Some Multiply -> (a * b) &&& ((1u <<< comp.InputWidth 0) - 1u)

        putUInt32 0 res
    | NbitsXor(numberOfBits, op),true ->
        let a,b = insBigInt 0, insBigInt 1
        let res =
                    match op with
                    | None -> a ^^^ b
                    | Some Multiply -> (a * b) &&& ((1I <<< comp.InputWidth 0) - 1I)

        putBigInt 0 res
    | NbitsOr numberOfBits, false ->
        let a,b = insUInt32 0, insUInt32 1
        let res = a ||| b
        putUInt32 0 res
    | NbitsOr numberOfBits, true ->
        let a,b = insBigInt 0, insBigInt 1
        let res = a ||| b
        putBigInt 0 res
    | NbitsAnd numberOfBits, false ->
        let a, b = insUInt32 0, insUInt32 1
        let res = a &&& b
        putUInt32 0 res
    | NbitsAnd numberOfBits, true ->
        let a, b = insBigInt 0, insBigInt 1
        let res = a &&& b
        putBigInt 0 res
    | NbitsNot numberOfBits, false ->
        let a = insUInt32 0
        let w = comp.InputWidth 0
        let minusOne = (1u <<< w) - 1u
        let res = 
            match w with 
            | 32 -> ~~~a
            | _ -> minusOne &&& (~~~a)
        putUInt32 0 res
    | NbitsNot numberOfBits, true ->
        let a = insBigInt 0
        // failwithf $"TODO: fable does not support op_OnesComplement function"
        // BigWord (System.Numerics.BigInteger.op_OnesComplement a)  FIX: 2^n-1-a
        let w = comp.InputWidth 0
        // (bigint^w)
        let mask = (1I <<< w) - 1I
        let res = mask - (a &&& mask)
        putBigInt 0 res

    | NbitSpreader numberOfBits, false ->
        let a = insUInt32 0
        let res =
            match a with
            | 0u -> 0u
            | 1u -> (1u <<< numberOfBits) - 1u
            | _ -> failwithf $"Can't happen"

        putUInt32 0 res
    | NbitSpreader numberOfBits, true ->
        let a = insUInt32 0
        let res =
            match a with
            | 0u -> 0I
            | 1u -> (1I <<< numberOfBits) - 1I
            | _ -> failwithf $"Can't happen"

        putBigInt 0 res

    | Custom c, _ ->
        // Custom components are removed
        failwithf "what? Custom components are removed before the fast simulation: %A" c
    | MergeWires, false ->
        let bits0, bits1 = insUInt32 0, insUInt32 1
        // Little endian, bits coming from the top wire are the least significant.
        let res = (bits1 <<< comp.InputWidth 0) ||| bits0
        putUInt32 0 res
    | MergeWires, true ->
        match comp.BigIntState with
        | None -> failwith "MergeWires with BigIntState"
        | Some { InputIsBigInt = ins; OutputIsBigInt = outs } ->
            // Little endian, bits coming from the top wire are the least significant.
            match ins[0], ins[1] with
            | false, false ->
                let bits0, bits1 = insUInt32 0, insUInt32 1
                let res = ((bigint bits1) <<< comp.InputWidth 0) ||| (bigint bits0)
                putBigInt 0 res
            | false, true ->
                let bits0, bits1 = insUInt32 0, insBigInt 1
                let res = (bits1 <<< comp.InputWidth 0) ||| (bigint bits0)
                putBigInt 0 res
            | true, false ->
                let bits0, bits1 = insBigInt 0, insUInt32 1
                let res = ((bigint bits1) <<< comp.InputWidth 0) ||| bits0
                putBigInt 0 res
            | true, true ->
                let bits0, bits1 = insBigInt 0, insBigInt 1
                let res = (bits1 <<< comp.InputWidth 0) ||| bits0
                putBigInt 0 res
    | MergeN n , false -> 
        let mergeTwoValues (width: int) (value1: uint32) (value2: uint32) =
            (value1 <<< width) ||| value2
        let res = List.fold2 (fun acc width input ->
            if input < 0 then
                failwith "Input values must be non-negative"
            mergeTwoValues width acc (insUInt32 input)) 0u (List.rev (List.map (fun x -> comp.InputWidth x) [0..n-1])) [for x in n-1..-1..0 -> x]
        putUInt32 0 res
    | MergeN n , true ->
        match comp.BigIntState with
        | None -> failwith "MergeN with BigIntState"
        | Some { InputIsBigInt = ins; OutputIsBigInt = outs } -> 
            failwith "TODO: MergeN with BigIntState"
    | SplitWire topWireWidth, false ->
        let bits = insUInt32 0
#if ASSERTS
        let w = comp.InputWidth 0
        assertThat (w >= topWireWidth + 1)
        <| sprintf "SplitWire received too few bits: expected at least %d but got %d" (topWireWidth + 1) w
#endif
        let bits0, bits1 =
            let bits1 = getBitsFromUInt32 ((comp.InputWidth 0) - 1) topWireWidth bits
            let bits0 = getBitsFromUInt32 (topWireWidth - 1) 0 bits
            bits0, bits1
        // Little endian, bits leaving from the top wire are the least significant.
        putUInt32 0 bits0
        putUInt32 1 bits1
    | SplitN (n, outputWidths, lsBits), false -> 
        let msBits = List.map2(fun width lsb -> width+lsb-1) outputWidths lsBits
        let bits = insUInt32 0
#if ASSERTS
        let maxMsb = List.max msBits
        let w = comp.InputWidth 0
        assertThat (w >= maxMsb+1)
        <| sprintf "SplitWire received too few bits: expected at least %d but got %d" (maxMsb + 1) w
#endif
        (lsBits, msBits)
        ||> List.iteri2 (
            fun index lsb msb -> 
                let outBits = getBitsFromUInt32 msb lsb bits
                putUInt32 index outBits)
    | SplitN _, true -> 
        match comp.BigIntState with
        | None -> failwith "SplitN with BigIntState"
        | Some { InputIsBigInt = ins; OutputIsBigInt = outs } -> 
            failwith "TODO: SplitN with BigIntState"
    | SplitWire topWireWidth, true ->
        let bits = insBigInt 0
#if ASSERTS
        let w = comp.InputWidth 0
        assertThat (w >= topWireWidth + 1)
        <| sprintf "SplitWire received too few bits: expected at least %d but got %d" (topWireWidth + 1) w
#endif
        match comp.BigIntState with
        | None -> failwith "SplitWire with BigIntState"
        | Some { InputIsBigInt = ins; OutputIsBigInt = outs } ->
            // Little endian, bits leaving from the top wire are the least significant.
            match ins[0], outs[0] with
            | false, false ->
                let bits0 = getBitsFromBigIntToUInt32 (topWireWidth - 1) 0 bits
                let bits1 = getBitsFromBigIntToUInt32 ((comp.InputWidth 0) - 1) topWireWidth bits
                putUInt32 0 bits0
                putUInt32 1 bits1
            | false, true ->
                let bits0 = getBitsFromBigIntToUInt32 (topWireWidth - 1) 0 bits
                let bits1 = getBitsFromBigInt ((comp.InputWidth 0) - 1) topWireWidth bits
                putUInt32 0 bits0
                putBigInt 1 bits1
            | true, false ->
                let bits0 = getBitsFromBigInt (topWireWidth - 1) 0 bits
                let bits1 = getBitsFromBigIntToUInt32 ((comp.InputWidth 0) - 1) topWireWidth bits
                putBigInt 0 bits0
                putUInt32 1 bits1
            | true, true ->
                let bits0 = getBitsFromBigInt (topWireWidth - 1) 0 bits
                let bits1 = getBitsFromBigInt ((comp.InputWidth 0) - 1) topWireWidth bits
                putBigInt 0 bits0
                putBigInt 1 bits1
    | DFF, false ->
        let d = insOldUInt32 0
        putUInt32 0 d
    | DFFE, false ->
        let d, en = insOldUInt32 0, insOldUInt32 1
        match en with
        | 0u -> putUInt32 0 (getLastCycleOutUInt32 0)
        | 1u -> putUInt32 0 d
        | _ -> failwith "Can't happen"
    | DFF, true | DFFE, true -> failwithf "DFF/DFFE with BigIntState"

    | Register width, false ->
#if ASSERTS
        let w = comp.InputWidth 0
        assertThat (w = width) <| sprintf "Register received data with wrong width: expected %d but got %A" width w
#endif
        let bits = insOldUInt32 0
        putUInt32 0 bits
    | Register width, true ->
#if ASSERTS
        let w = comp.InputWidth 0
        assertThat (w = width) <| sprintf "Register received data with wrong width: expected %d but got %A" width w
#endif
        let bits = insOldBigInt 0
        putBigInt 0 bits

    | RegisterE width, false ->
#if ASSERTS
        let w = comp.InputWidth 0
        assertThat (w = width) <| sprintf "RegisterE received data with wrong width: expected %d but got %A" width w
#endif
        let bits, enable = insOldUInt32 0, insOldUInt32 1
        match enable with
        | 0u -> putUInt32 0 (getLastCycleOutUInt32 0)
        | 1u -> putUInt32 0 bits
        | _ -> failwithf "RegisterE received invalid enable value: %A" enable
    | RegisterE width, true ->
#if ASSERTS
        let w = comp.InputWidth 0
        assertThat (w = width) <| sprintf "RegisterE received data with wrong width: expected %d but got %A" width w
#endif
        let bits, enable = insOldBigInt 0, insOldUInt32 1
        match enable with
        | 0u -> putBigInt 0 (getLastCycleOutBigInt 0)
        | 1u -> putBigInt 0 bits
        | _ -> failwithf "RegisterE received invalid enable value: %A" enable
    | Counter width, false ->
#if ASSERTS
        let w = comp.InputWidth 0
        assertThat (w = width) <| sprintf "Counter received data with wrong width: expected %d but got %A" width w
#endif
        let bits, load, enable = insOldUInt32 0, insOldUInt32 1, insOldUInt32 2
        let res =
            match enable, load with
            | 1u, 0u ->
                let lastOut = getLastCycleOutUInt32 0
                let n = (bigint lastOut) + 1I
                if n = (1I <<< width) then
                    0u
                else
                    uint32 n
            | 1u, 1u ->
                bits
            | _ ->
                getLastCycleOutUInt32 0
        putUInt32 0 res
    | Counter width, true ->
#if ASSERTS
        let w = comp.InputWidth 0
        assertThat (w = width) <| sprintf "Counter received data with wrong width: expected %d but got %A" width w
#endif
        let bits, load, enable = insOldBigInt 0, insOldUInt32 1, insOldUInt32 2
        let res =
            match enable, load with
            | 1u, 0u ->
                let lastOut = getLastCycleOutBigInt 0
                let n = lastOut + 1I
                if n = (1I <<< width) then
                    0I
                else
                    n
            | 1u, 1u ->
                bits
            | _ ->
                getLastCycleOutBigInt 0
        putBigInt 0 res
    | CounterNoEnable width, false ->
#if ASSERTS
        let w = comp.InputWidth 0
        assertThat (w = width) <| sprintf "Counter received data with wrong width: expected %d but got %A" width w
#endif
        let bits, load = insOldUInt32 0, insOldUInt32 1
        let res =
            match load with
            | 0u ->
                let lastOut = getLastCycleOutUInt32 0
                let n = (bigint lastOut) + 1I
                if n = (1I <<< width) then
                    0u
                else
                    uint32 n
            | 1u ->
                bits
            | _ -> failwithf "CounterNoEnable received invalid load value: %A" load
        putUInt32 0 res
    | CounterNoEnable width, true ->
#if ASSERTS
        let w = comp.InputWidth 0
        assertThat (w = width) <| sprintf "Counter received data with wrong width: expected %d but got %A" width w
#endif
        let bits, load = insOldBigInt 0, insOldUInt32 1
        let res =
            match load with
            | 0u ->
                let lastOut = getLastCycleOutBigInt 0
                let n = lastOut + 1I
                if n = (1I <<< width) then
                    0I
                else
                    n
            | 1u ->
                bits
            | _ -> failwithf "CounterNoEnable received invalid load value: %A" load
        putBigInt 0 res
    | CounterNoLoad width, false ->
        let enable = insOldUInt32 0
        let res =
            match enable with
            | 1u ->
                let lastOut = getLastCycleOutUInt32 0
                let n = (bigint lastOut) + 1I
                if n = (1I <<< width) then
                    0u
                else
                    uint32 n
            | 0u ->
                getLastCycleOutUInt32 0
            | _ -> failwithf "CounterNoLoad received invalid enable value: %A" enable
        putUInt32 0 res
    | CounterNoLoad width, true ->
        let enable = insOldUInt32 0
        let res =
            match enable with
            | 1u ->
                let lastOut = getLastCycleOutBigInt 0
                let n = lastOut + 1I
                if n = (1I <<< width) then
                    0I
                else
                    n
            | 0u ->
                getLastCycleOutBigInt 0
            | _ -> failwithf "CounterNoLoad received invalid enable value: %A" enable
        putBigInt 0 res
    | CounterNoEnableLoad width, false ->
        let lastOut = getLastCycleOutUInt32 0
        let n = (bigint lastOut) + 1I
        let res =
            if n = (1I <<< width) then
                0u
            else
                uint32 n
        putUInt32 0 res
    | CounterNoEnableLoad width, true ->
        let lastOut = getLastCycleOutBigInt 0
        let n = lastOut + 1I
        let res =
            if n = (1I <<< width) then
                0I
            else
                n
        putBigInt 0 res
    | AsyncROM1 mem, false -> // Asynchronous ROM.
#if ASSERTS
        let w = comp.InputWidth 0
        assertThat (w = mem.AddressWidth) <| sprintf "ROM received address with wrong width: expected %d but got %A" mem.AddressWidth w
#endif
        let addr = insUInt32 0
        let outData = readMemoryAddrUInt32DataUInt32 mem addr
        putUInt32 0 outData
    | AsyncROM1 mem, true -> // Asynchronous ROM.
#if ASSERTS
        let w = comp.InputWidth 0
        assertThat (w = mem.AddressWidth) <| sprintf "ROM received address with wrong width: expected %d but got %A" mem.AddressWidth w
#endif
        match comp.BigIntState with
        | None -> failwithf "ROM received data with wrong width: expected %d but got %A" mem.WordWidth (comp.InputWidth 0)
        | Some { InputIsBigInt = ins; OutputIsBigInt = outs } ->
            match ins[0], outs[0] with
            | true, true ->
                let addr = insBigInt 0
                let outData = readMemoryAddrBigIntDataBigInt mem addr
                putBigInt 0 outData
            | false, true ->
                let addr = insUInt32 0
                let outData = readMemoryAddrUInt32DataBigInt mem addr
                putBigInt 0 outData
            | true, false ->
                let addr = insBigInt 0
                let outData = readMemoryAddrBigIntDataUInt32 mem addr
                putUInt32 0 outData
            | false, false -> failwithf "ROM received data with wrong width: expected %d but got %A" mem.WordWidth (comp.InputWidth 0)
    | ROM1 mem, false -> // Synchronous ROM.
#if ASSERTS
        let w = comp.InputWidth 0
        assertThat (w = mem.AddressWidth) <| sprintf "ROM received address with wrong width: expected %d but got %A" mem.AddressWidth w
#endif
        let addr = insOldUInt32 0
        let outData = readMemoryAddrUInt32DataUInt32 mem addr
        putUInt32 0 outData
    | ROM1 mem, true -> // Synchronous ROM.
#if ASSERTS
        let w = comp.InputWidth 0
        assertThat (w = mem.AddressWidth) <| sprintf "ROM received address with wrong width: expected %d but got %A" mem.AddressWidth w
#endif
        match comp.BigIntState with
        | None -> failwithf "ROM received data with wrong width: expected %d but got %A" mem.WordWidth (comp.InputWidth 0)
        | Some { InputIsBigInt = ins; OutputIsBigInt = outs } ->
            match ins[0], outs[0] with
            | true, true ->
                let addr = insBigInt 0
                let outData = readMemoryAddrBigIntDataBigInt mem addr
                putBigInt 0 outData
            | false, true ->
                let addr = insUInt32 0
                let outData = readMemoryAddrUInt32DataBigInt mem addr
                putBigInt 0 outData
            | true, false ->
                let addr = insBigInt 0
                let outData = readMemoryAddrBigIntDataUInt32 mem addr
                putUInt32 0 outData
            | false, false -> failwithf "ROM received data with wrong width: expected %d but got %A" mem.WordWidth (comp.InputWidth 0)
    | RAM1 memory, false ->
        let mem = getRamStateMemory numStep (simStepOld) comp.State memory
#if ASSERTS
        let addressW, dataInW = comp.InputWidth 0, comp.InputWidth 1
        assertThat (addressW = mem.AddressWidth) <| sprintf "RAM received address with wrong width: expected %d but got %A" mem.AddressWidth addressW
        assertThat (dataInW = mem.WordWidth) <| sprintf "RAM received data-in with wrong width: expected %d but got %A" mem.WordWidth dataInW
#endif
        let address, dataIn, write = insOldUInt32 0, insOldUInt32 1, insOldUInt32 2
        // If write flag is on, write the memory content.
        let mem, dataOut =
            match write with
            // Read memory address and return memory unchanged.
            | 0u -> mem, readMemoryAddrUInt32DataUInt32 mem address
            // Update memory and return old content.
            // NB - this was previously new content - but that is inconsistent and less useful.
            | 1u -> writeMemoryAddrUInt32DataUInt32 mem address dataIn, readMemoryAddrUInt32DataUInt32 mem address
            | _ -> failwithf $"simulation error: invalid 1 bit write value {write}"
        putState (RamState mem)
        putUInt32 0 dataOut
    | RAM1 memory, true ->
        let mem = getRamStateMemory numStep (simStepOld) comp.State memory
#if ASSERTS
        let addressW, dataInW = comp.InputWidth 0, comp.InputWidth 1
        assertThat (addressW = mem.AddressWidth) <| sprintf "RAM received address with wrong width: expected %d but got %A" mem.AddressWidth addressW
        assertThat (dataInW = mem.WordWidth) <| sprintf "RAM received data-in with wrong width: expected %d but got %A" mem.WordWidth dataInW
#endif
        match comp.BigIntState with
        | None -> failwithf "RAM received data with wrong width: expected %d but got %A" mem.WordWidth (comp.InputWidth 0)
        | Some { InputIsBigInt = ins; OutputIsBigInt = outs } ->
            match ins[0], outs[0] with
            | false, false -> failwithf "RAM received data with wrong width: expected %d but got %A" mem.WordWidth (comp.InputWidth 0)
            | true, true ->
                let address, dataIn, write = insOldBigInt 0, insOldBigInt 1, insOldUInt32 2
                let mem, dataOut =
                    match write with
                    | 0u -> mem, readMemoryAddrBigIntDataBigInt mem address
                    | 1u -> writeMemoryAddrBigIntDataBigInt mem address dataIn, readMemoryAddrBigIntDataBigInt mem address
                    | _ -> failwithf $"simulation error: invalid 1 bit write value {write}"
                putState (RamState mem)
                putBigInt 0 dataOut
            | false, true ->
                let address, dataIn, write = insOldUInt32 0, insOldBigInt 1, insOldUInt32 2
                let mem, dataOut =
                    match write with
                    | 0u -> mem, readMemoryAddrUInt32DataBigInt mem address
                    | 1u -> writeMemoryAddrUInt32DataBigInt mem address dataIn, readMemoryAddrUInt32DataBigInt mem address
                    | _ -> failwithf $"simulation error: invalid 1 bit write value {write}"
                putState (RamState mem)
                putBigInt 0 dataOut
            | true, false ->
                let address, dataIn, write =  insOldBigInt 0, insOldUInt32 1, insOldUInt32 2
                let mem, dataOut =
                    match write with
                    | 0u -> mem, readMemoryAddrBigIntDataUInt32 mem address
                    | 1u -> writeMemoryAddrBigIntDataUInt32 mem address dataIn, readMemoryAddrBigIntDataUInt32 mem address
                    | _ -> failwithf $"simulation error: invalid 1 bit write value {write}"
                putState (RamState mem)
                putUInt32 0 dataOut

    // AsyncRAM1 component must be evaluated twice. Once (first) as clocked component
    // to update state based on previous cycle. Then again as combinational component to update output
    | AsyncRAM1 memory, false ->
#if ASSERTS
        let addressW, dataInW = comp.InputWidth 0, comp.InputWidth 1
        assertThat (addressW = memory.AddressWidth) <| sprintf "RAM received address with wrong width: expected %d but got %A" memory.AddressWidth addressW
        assertThat (dataInW = memory.WordWidth) <| sprintf "RAM received data-in with wrong width: expected %d but got %A" memory.WordWidth dataInW
#endif
        if isClockedReduction then
            // here we propagate the state to current timestep, doing a state change if need be.
            let mem = getRamStateMemory numStep (simStepOld) comp.State memory
            let address, dataIn, write = insOldUInt32 0, insOldUInt32 1, insOldUInt32 2
            // If write flag is on, write the memory content.
            let mem =
                match write with
                // Read memory address and return memory unchanged.
                | 0u -> mem
                // Update memory and return old content.
                // NB - this was previously new content - but that is inconsistent and less useful.
                | 1u -> writeMemoryAddrUInt32DataUInt32 mem address dataIn
                | _ -> failwithf $"simulation error: invalid 1 bit write value {write}"
            putState (RamState mem)
        else
            // here we do the async read using current step address and state
            // note that state will have been written for this step previously by clocked invocation of this component
            let mem = getRamStateMemory (numStep + 1) simStep comp.State memory
            let address = insUInt32 0
            let data = readMemoryAddrUInt32DataUInt32 mem address
            //printfn $"reading {data} from addr={address} with state = {RamState mem}"
            putUInt32 0 data
    | AsyncRAM1 mem, true ->
#if ASSERTS
        let addressW, dataInW = comp.InputWidth 0, comp.InputWidth 1
        assertThat (addressW = mem.AddressWidth) <| sprintf "RAM received address with wrong width: expected %d but got %A" mem.AddressWidth addressW
        assertThat (dataInW = mem.WordWidth) <| sprintf "RAM received data-in with wrong width: expected %d but got %A" mem.WordWidth dataInW
#endif
        match comp.BigIntState with
        | None -> failwithf "RAM received data with wrong width: expected %d but got %A" mem.WordWidth (comp.InputWidth 0)
        | Some { InputIsBigInt = ins; OutputIsBigInt = outs } ->
            match ins[0], outs[0] with
            | false, false -> failwithf "RAM received data with wrong width: expected %d but got %A" mem.WordWidth (comp.InputWidth 0)
            | true, true ->
                if isClockedReduction then
                    let mem = getRamStateMemory numStep (simStepOld) comp.State mem
                    let address, dataIn, write = insOldBigInt 0, insOldBigInt 1, insOldUInt32 2
                    let mem =
                        match write with
                        | 0u -> mem
                        | 1u -> writeMemoryAddrBigIntDataBigInt mem address dataIn
                        | _ -> failwithf $"simulation error: invalid 1 bit write value {write}"
                    putState (RamState mem)
                else
                    let mem = getRamStateMemory (numStep + 1) simStep comp.State mem
                    let address = insBigInt 0
                    let data = readMemoryAddrBigIntDataBigInt mem address
                    putBigInt 0 data
            | false, true ->
                if isClockedReduction then
                    let mem = getRamStateMemory numStep (simStepOld) comp.State mem
                    let address, dataIn, write = insOldUInt32 0, insOldBigInt 1, insOldUInt32 2
                    let mem =
                        match write with
                        | 0u -> mem
                        | 1u -> writeMemoryAddrUInt32DataBigInt mem address dataIn
                        | _ -> failwithf $"simulation error: invalid 1 bit write value {write}"
                    putState (RamState mem)
                else
                    let mem = getRamStateMemory (numStep + 1) simStep comp.State mem
                    let address = insUInt32 0
                    let data = readMemoryAddrUInt32DataBigInt mem address
                    putBigInt 0 data
            | true,false ->
                if isClockedReduction then
                    let mem = getRamStateMemory numStep (simStepOld) comp.State mem
                    let address, dataIn, write = insOldBigInt 0, insOldUInt32 1, insOldUInt32 2
                    let mem =
                        match write with
                        | 0u -> mem
                        | 1u -> writeMemoryAddrBigIntDataUInt32 mem address dataIn
                        | _ -> failwithf $"simulation error: invalid 1 bit write value {write}"
                    putState (RamState mem)
                else
                    let mem = getRamStateMemory (numStep + 1) simStep comp.State mem
                    let address = insOldBigInt 0
                    let data = readMemoryAddrBigIntDataUInt32 mem address
                    putUInt32 0 data
    | _ -> failwithf $"simulation error: deprecated component type {componentType}"

