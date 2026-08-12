module EvalReference

open CommonTypes
open SimGraphTypes
open SimTypes
open NumberHelpers
open System
open Helpers
open EvalKernel

//---------------------------------------------------------------------------------------//
// ------------------------------MAIN COMPONENT SIMULATION FUNCTION----------------------//
//--------------------------------------fastReduce---------------------------------------//
//---------------------------------------------------------------------------------------//

/// Given a component, compute its outputs from its inputs, which must already be evaluated.
/// Outputs and inputs are both contained as time sequences in arrays. This function will calculate
/// simStep outputs from (previously calculated) simStep outputs and clocked (simStep-1) outputs.
/// Memory has state separate from simStep-1 output, for this the state is recalculated.
/// Inputs and outputs come from either UInt32Step or BigIntStep arrays in IOArray record.
let fastReduce (step: StepIndex) (isClockedReduction: bool) (comp: FastComponent) : Unit =
    let componentType = comp.FType
    let numStep = step.NumStep
    let simStep = step.SimStep
    let simStepOld = step.SimStepOld

#if ASSERTS
    Log.warn "simulation is running with ASSERTS on for debugging - this will be very slow"
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

    /// The RAM's store, for a step that may write to it.
    ///
    /// Every slot of the state array holds the same store, so which one is read does not matter
    /// except at step 0, where the previous slot is the wrapped-out end of the last run and may
    /// never have been written at all. Step 0 is also the restart, so the contents go back to
    /// what the memory was built with - the Memory1 version carried the old run's contents
    /// forward instead, which was a bug.
    let inline ramStateForStep () =
        if numStep = 0 then
            let state = getRamState simStep comp.State
            RamStore.reset (ramStoreOf state)
            state
        else
            getRamState simStepOld comp.State

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
    //
    // MASKING INVARIANT: every value stored in a step array is already within its bus width.
    // Nothing masks on read: bus compare matches the raw value, mux selects dispatch on it,
    // memory addresses index with it, and extraction returns it. A case must therefore mask
    // its result exactly when its operation can overflow the width (add, multiply, not,
    // shift, constants) and, for speed, not otherwise. NB on the uint32 path a width of
    // exactly 32 needs care: 1u <<< 32 wraps to 1u, so build the mask another way or use
    // uint32 wrap-around.

    match componentType, comp.UseBigInt with
    | ROM _, _
    | RAM _, _
    | AsyncROM _, _ -> failwithf "What? Legacy RAM component types should never occur"
    | Input _, _ -> failwithf "Legacy Input component types should never occur"
    | Input1(width, _), false ->
        if comp.Active then //REVIEW - is this neccessary?
            let bits = insUInt32 0
            checkWidth width (comp.InputWidth 0)
            putUInt32 0 bits
    | Input1(width, _), true ->
        if comp.Active then
            let bits = insBigInt 0
            // the input width, as in the uint32 branch above (this read OutputWidth)
            checkWidth width (comp.InputWidth 0)
            putBigInt 0 bits
    // twosComp, not a 32 bit conversion: a negative constant (e.g. -1 at width 4) must
    // store its width-wide two's complement bit pattern, as everything downstream relies
    // on stored values being within their width
    | Constant1(width, cVal, _), false
    | Constant(width, cVal), false -> putUInt32 0 <| uint32 (twosComp width cVal)
    | Constant1(width, cVal, _), true
    | Constant(width, cVal), true -> putBigInt 0 <| twosComp width cVal
    | Output width, false ->
        let bits = insUInt32 0
        checkWidth width (comp.InputWidth 0)
        putUInt32 0 bits
    | Output width, true ->
        let bits = insBigInt 0
        checkWidth width (comp.InputWidth 0)
        putBigInt 0 bits
    | Viewer width,false ->
        let bits = insUInt32 0
        checkWidth width (comp.InputWidth 0)
        putUInt32 0 bits
    | Viewer width,true ->
        let bits = insBigInt 0
        checkWidth width (comp.InputWidth 0)
        putBigInt 0 bits

    | IOLabel,false ->
        let bits = insUInt32 0
        //let bits = comp.InputLinks[0][simStep]
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
                    | Some Multiply ->
                        // at width 32 the mask is not needed - and (1u <<< 32) would be 1u,
                        // making the mask 0 - since uint32 multiplication wraps at 2^32
                        match comp.InputWidth 0 with
                        | 32 -> a * b
                        | w -> (a * b) &&& ((1u <<< w) - 1u)

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
        let res =
            match w with
            | 32 -> ~~~a
            // the mask belongs here: at w = 32 it was computed and discarded, and 1u <<< 32
            // being 1u would have made it 0
            | _ -> ((1u <<< w) - 1u) &&& (~~~a)
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
            | 1u -> if numberOfBits <> 32 then (1u <<< numberOfBits) - 1u else UInt32.MaxValue
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
    // Shift: input 0 is the data bus, input 1 the shift amount (at most 32 bits wide).
    // Shifting by the bus width or more gives 0, or all-sign-bits for ASR.
    | Shift(width, _, shiftType), false ->
        let mask = if width = 32 then UInt32.MaxValue else (1u <<< width) - 1u
        let bits = insUInt32 0 &&& mask
        let amt = insUInt32 1
        let signSet = (bits >>> (width - 1)) &&& 1u = 1u
        let res =
            if amt = 0u then
                bits
            elif amt >= uint32 width then
                match shiftType with
                | LSL | LSR -> 0u
                | ASR -> if signSet then mask else 0u
            else
                let amt = int amt
                match shiftType with
                | LSL -> (bits <<< amt) &&& mask
                | LSR -> bits >>> amt
                | ASR when signSet -> (bits >>> amt) ||| (mask &&& (mask <<< (width - amt)))
                | ASR -> bits >>> amt
        putUInt32 0 res
    | Shift(width, _, shiftType), true ->
        let mask = bigIntMask width
        let bits = insBigInt 0 &&& mask
        let amt = insUInt32 1
        let signSet = (bits >>> (width - 1)) &&& 1I = 1I
        let res =
            if amt >= uint32 width then
                match shiftType with
                | LSL | LSR -> 0I
                | ASR -> if signSet then mask else 0I
            else
                let amt = int amt
                match shiftType with
                | LSL -> (bits <<< amt) &&& mask
                | LSR -> bits >>> amt
                | ASR when signSet -> (bits >>> amt) ||| (mask &&& (mask <<< (width - amt)))
                | ASR -> bits >>> amt
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
    // Little endian, input 0 occupies the least significant bits. Folding over the indices
    // downwards shifts each input above the ones already accumulated. The lists of widths and
    // of indices this used to fold over were rebuilt from constants on every step, which cost
    // two list allocations per component per step to do a shift and an or.
    | MergeN n , false ->
        let mutable res = 0u
        for i = n - 1 downto 0 do
            res <- (res <<< comp.InputWidth i) ||| insUInt32 i
        putUInt32 0 res
    | MergeN n , true ->
        match comp.BigIntState with
        | None -> failwith "MergeN with BigIntState"
        | Some { InputIsBigInt = ins; OutputIsBigInt = outs } ->
            // Total width > 32 so the output is always a bigint; each input may be either.
            let mutable res = 0I
            for i = n - 1 downto 0 do
                let input = if ins[i] then insBigInt i else bigint (insUInt32 i)
                res <- (res <<< comp.InputWidth i) ||| input
            putBigInt 0 res
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
    // Each output's msb is width + lsb - 1, derived here from the two constant lists rather
    // than from a third list of msbs built on every step
    | SplitN (n, outputWidths, lsBits), false ->
        let bits = insUInt32 0
#if ASSERTS
        let maxMsb = List.map2 (fun width lsb -> width + lsb - 1) outputWidths lsBits |> List.max
        let w = comp.InputWidth 0
        assertThat (w >= maxMsb+1)
        <| sprintf "SplitWire received too few bits: expected at least %d but got %d" (maxMsb + 1) w
#endif
        (outputWidths, lsBits)
        ||> List.iteri2 (
            fun index width lsb ->
                let outBits = getBitsFromUInt32 (width + lsb - 1) lsb bits
                putUInt32 index outBits)
    | SplitN(n, outputWidths, lsBits), true ->
        match comp.BigIntState with
        | None -> failwith "SplitN with BigIntState"
        | Some { InputIsBigInt = ins; OutputIsBigInt = outs } ->
            // Outputs are slices of the input, so a bigint state means the input is a bigint;
            // each output may be either.
            let bits = insBigInt 0
#if ASSERTS
            let maxMsb = List.map2 (fun width lsb -> width + lsb - 1) outputWidths lsBits |> List.max
            let w = comp.InputWidth 0
            assertThat (w >= maxMsb + 1)
            <| sprintf "SplitN received too few bits: expected at least %d but got %d" (maxMsb + 1) w
#endif
            (outputWidths, lsBits)
            ||> List.iteri2 (fun index width lsb ->
                let msb = width + lsb - 1
                if outs[index] then
                    putBigInt index (getBitsFromBigInt msb lsb bits)
                else
                    putUInt32 index (getBitsFromBigIntToUInt32 msb lsb bits))
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
            | 1u, 0u -> incrementWithinWidth width (getLastCycleOutUInt32 0)
            | 1u, 1u -> bits
            | _ -> getLastCycleOutUInt32 0
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
            | 0u -> incrementWithinWidth width (getLastCycleOutUInt32 0)
            | 1u -> bits
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
            | 1u -> incrementWithinWidth width (getLastCycleOutUInt32 0)
            | 0u -> getLastCycleOutUInt32 0
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
        putUInt32 0 (incrementWithinWidth width (getLastCycleOutUInt32 0))
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
    // Read/write memories keep their contents in a RamStore, which is mutable and shared by
    // every step rather than snapshotted into one. So there is no new memory to thread out of a
    // write, and the store is fetched from the previous step's slot exactly as the Memory1 used
    // to be. Step 0 is the restart: it puts the contents back to what the memory was built with,
    // which the Memory1 version failed to do - it read the slot left by the wrapped-out end of
    // the previous run. See [docs/dev/ramRepresentation.md].
    | RAM1 memory, false ->
        let memState = ramStateForStep ()
        let mem = ramStoreOf memState
#if ASSERTS
        let addressW, dataInW = comp.InputWidth 0, comp.InputWidth 1
        assertThat (addressW = mem.AddressWidth) <| sprintf "RAM received address with wrong width: expected %d but got %A" mem.AddressWidth addressW
        assertThat (dataInW = mem.WordWidth) <| sprintf "RAM received data-in with wrong width: expected %d but got %A" mem.WordWidth dataInW
#endif
        let address, dataIn, write = insOldUInt32 0, insOldUInt32 1, insOldUInt32 2
        // The old contents are the output whether or not the write happens.
        // NB - this was previously new content - but that is inconsistent and less useful.
        let dataOut = readRamAddrUInt32DataUInt32 mem address
        match write with
        | 0u -> ()
        | 1u -> writeRamAddrUInt32DataUInt32 mem numStep address dataIn
        | _ -> failwithf $"simulation error: invalid 1 bit write value {write}"
        putState memState
        putUInt32 0 dataOut
    | RAM1 memory, true ->
        let memState = ramStateForStep ()
        let mem = ramStoreOf memState
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
                let dataOut = readRamAddrBigIntDataBigInt mem address
                match write with
                | 0u -> ()
                | 1u -> writeRamAddrBigIntDataBigInt mem numStep address dataIn
                | _ -> failwithf $"simulation error: invalid 1 bit write value {write}"
                putState memState
                putBigInt 0 dataOut
            | false, true ->
                let address, dataIn, write = insOldUInt32 0, insOldBigInt 1, insOldUInt32 2
                let dataOut = readRamAddrUInt32DataBigInt mem address
                match write with
                | 0u -> ()
                | 1u -> writeRamAddrUInt32DataBigInt mem numStep address dataIn
                | _ -> failwithf $"simulation error: invalid 1 bit write value {write}"
                putState memState
                putBigInt 0 dataOut
            | true, false ->
                let address, dataIn, write =  insOldBigInt 0, insOldUInt32 1, insOldUInt32 2
                let dataOut = readRamAddrBigIntDataUInt32 mem address
                match write with
                | 0u -> ()
                | 1u -> writeRamAddrBigIntDataUInt32 mem numStep address dataIn
                | _ -> failwithf $"simulation error: invalid 1 bit write value {write}"
                putState memState
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
            let memState = ramStateForStep ()
            let mem = ramStoreOf memState
            let address, dataIn, write = insOldUInt32 0, insOldUInt32 1, insOldUInt32 2
            match write with
            | 0u -> ()
            | 1u -> writeRamAddrUInt32DataUInt32 mem numStep address dataIn
            | _ -> failwithf $"simulation error: invalid 1 bit write value {write}"
            putState memState
        else
            // here we do the async read using current step address and state
            // note that state will have been written for this step previously by clocked invocation of this component
            let mem = getRamStore simStep comp.State
            let address = insUInt32 0
            let data = readRamAddrUInt32DataUInt32 mem address
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
                    let memState = ramStateForStep ()
                    let mem = ramStoreOf memState
                    let address, dataIn, write = insOldBigInt 0, insOldBigInt 1, insOldUInt32 2
                    match write with
                    | 0u -> ()
                    | 1u -> writeRamAddrBigIntDataBigInt mem numStep address dataIn
                    | _ -> failwithf $"simulation error: invalid 1 bit write value {write}"
                    putState memState
                else
                    let mem = getRamStore simStep comp.State
                    let address = insBigInt 0
                    let data = readRamAddrBigIntDataBigInt mem address
                    putBigInt 0 data
            | false, true ->
                if isClockedReduction then
                    let memState = ramStateForStep ()
                    let mem = ramStoreOf memState
                    let address, dataIn, write = insOldUInt32 0, insOldBigInt 1, insOldUInt32 2
                    match write with
                    | 0u -> ()
                    | 1u -> writeRamAddrUInt32DataBigInt mem numStep address dataIn
                    | _ -> failwithf $"simulation error: invalid 1 bit write value {write}"
                    putState memState
                else
                    let mem = getRamStore simStep comp.State
                    let address = insUInt32 0
                    let data = readRamAddrUInt32DataBigInt mem address
                    putBigInt 0 data
            | true,false ->
                if isClockedReduction then
                    let memState = ramStateForStep ()
                    let mem = ramStoreOf memState
                    let address, dataIn, write = insOldBigInt 0, insOldUInt32 1, insOldUInt32 2
                    match write with
                    | 0u -> ()
                    | 1u -> writeRamAddrBigIntDataUInt32 mem numStep address dataIn
                    | _ -> failwithf $"simulation error: invalid 1 bit write value {write}"
                    putState memState
                else
                    let mem = getRamStore simStep comp.State
                    let address = insBigInt 0
                    let data = readRamAddrBigIntDataUInt32 mem address
                    putUInt32 0 data
    | _ -> failwithf $"simulation error: deprecated component type {componentType}"

/// fastReduce for a one-off reduction, working out the step indices for itself. Anything
/// reducing many components, or the same component over many steps, should compute them once
/// with simStepOf and simStepOldOf and call fastReduce directly.
let fastReduceStep (maxArraySize: int) (numStep: int) (isClockedReduction: bool) (comp: FastComponent) : Unit =
    fastReduce (stepIndexOf maxArraySize numStep) isClockedReduction comp
