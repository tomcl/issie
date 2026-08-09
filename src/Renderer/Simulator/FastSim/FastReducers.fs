/// Per-component reducers, chosen and bound once when the simulation is built.
///
/// fastReduce works out afresh, for every component of every clock step, things that were
/// settled when the simulation was built: which component type this is, whether it is on the
/// uint32 or the bigint path, which of its inputs are bigints, what its bus masks are, and
/// which arrays its ports live in. A reducer built here has all of that already: it closes
/// over the step arrays themselves and over the constants, and its body is only the work that
/// actually differs from step to step.
///
/// reducerFor returns None for a component type it does not handle, and the caller falls back
/// to fastReduce. So this file can be filled in a type at a time, and the fallback is always
/// the definition of what a reducer must do.
///
/// Two rules the closures depend on, both guaranteed by the caller (see installReducers):
///
///  - The step arrays captured here must be the ones the simulation actually uses. Reducers
///    are therefore installed after every re-linking pass, including the one
///    addWavesToFastSimulation does for custom components, and only for components that are
///    reduced. Capturing an array that is later re-pointed would silently simulate the wrong
///    signal.
///  - Values in the step arrays are already within their bus width (the masking invariant), so
///    a reducer masks its result exactly when its own operation can overflow, and reads
///    without masking. On the uint32 path a width of exactly 32 needs care: 1u <<< 32 is 1u.
module FastReducers

open CommonTypes
open SimTypes
open NumberHelpers
open FastReduce

/// The uint32 step array of input i / output i. Read once, when the reducer is built.
let inline private inU (fc: FastComponent) (i: int) = fc.InputLinks[i].UInt32Step
let inline private outU (fc: FastComponent) (i: int) = fc.Outputs[i].UInt32Step

/// A clocked component reads its inputs and its own output as they were on the previous step.
/// Before the first clock edge there is no previous step, and it reads 0.
let inline private oldU (arr: uint32 array) (step: StepIndex) =
    if step.NumStep = 0 then
        0u
    else
        arr[step.SimStepOld]

/// Mask of the low w bits, for w up to and including 32
let inline private maskOf (w: int) =
    if w = 32 then
        System.UInt32.MaxValue
    else
        (1u <<< w) - 1u

/// Extract bits msb..lsb, with the shift and mask worked out once rather than per step as
/// getBitsFromUInt32 does
let inline private sliceOf (msb: int) (lsb: int) =
    struct (lsb, maskOf (msb - lsb + 1))

/// n outputs of a demultiplexer: the selected one gets the input, the rest 0
let private demuxU (fc: FastComponent) (n: int) =
    let src = inU fc 0
    let sel = inU fc 1
    let outs = Array.init n (outU fc)

    Some(fun (step: StepIndex) ->
        let s = step.SimStep
        let selected = int sel[s]
        let bits = src[s]

        for i = 0 to n - 1 do
            outs[i][s] <- if i = selected then bits else 0u)

/// The four adder variants differ only in whether there is a carry in and a carry out
let private adderU (fc: FastComponent) (hasCin: bool) (hasCout: bool) =
    let firstOperand = if hasCin then 1 else 0
    let cin = if hasCin then inU fc 0 else Array.empty
    let a = inU fc firstOperand
    let b = inU fc (firstOperand + 1)
    let sumOut = outU fc 0
    let coutOut = if hasCout then outU fc 1 else Array.empty
    let w = fc.InputWidth (firstOperand + 1)
    let mask = maskOf w

    if w = 32 then
        // 32 bit addition can carry out of a uint32, so it is done in 64 bits
        Some(fun (step: StepIndex) ->
            let s = step.SimStep
            let carryIn = if hasCin then uint64 (cin[s] &&& 1u) else 0UL
            let total = uint64 a[s] + uint64 b[s] + carryIn
            sumOut[s] <- uint32 total

            if hasCout then
                coutOut[s] <- uint32 (total >>> 32) &&& 1u)
    else
        Some(fun (step: StepIndex) ->
            let s = step.SimStep
            let carryIn = if hasCin then cin[s] &&& 1u else 0u
            let total = a[s] + b[s] + carryIn
            sumOut[s] <- total &&& mask

            if hasCout then
                coutOut[s] <- (total >>> w) &&& 1u)

/// A counter's next value, given whether it has load and enable inputs. The inputs are read as
/// they were on the previous step, as for any clocked component.
let private counterU (fc: FastComponent) (width: int) (hasLoad: bool) (hasEnable: bool) =
    // input order is load data, load, enable - each present only if the variant has it
    let loadData = if hasLoad then inU fc 0 else Array.empty
    let load = if hasLoad then inU fc 1 else Array.empty
    let enable =
        if hasEnable then
            inU fc (if hasLoad then 2 else 0)
        else
            Array.empty
    let dst = outU fc 0

    Some(fun (step: StepIndex) ->
        let lastOut = oldU dst step

        dst[step.SimStep] <-
            if hasEnable && oldU enable step <> 1u then lastOut
            elif hasLoad && oldU load step = 1u then oldU loadData step
            else incrementWithinWidth width lastOut)

/// Build the reducer for one component, or None to leave it to fastReduce.
/// isClockedReduction distinguishes the two passes the hybrid (asynchronous RAM) components
/// need; every other component ignores it.
let reducerFor (fc: FastComponent) (isClockedReduction: bool) : (StepIndex -> unit) option =
    match fc.FType, fc.UseBigInt with

    // --- straight copies -------------------------------------------------------------

    | IOLabel, false
    | Output _, false
    | Viewer _, false ->
        let src = inU fc 0
        let dst = outU fc 0
        Some(fun step -> dst[step.SimStep] <- src[step.SimStep])

    | Input1 _, false ->
        // Active is mutable and read per step, not captured
        let src = inU fc 0
        let dst = outU fc 0

        Some(fun step ->
            if fc.Active then
                dst[step.SimStep] <- src[step.SimStep])

    | NotConnected, _ -> Some(fun _ -> ())

    // --- constants -------------------------------------------------------------------

    | Constant1(width, cVal, _), false
    | Constant(width, cVal), false ->
        let dst = outU fc 0
        // a negative constant is stored as its width-wide two's complement bit pattern
        let value = uint32 (twosComp width cVal)
        Some(fun step -> dst[step.SimStep] <- value)

    // --- gates -----------------------------------------------------------------------

    | Not, false ->
        let src = inU fc 0
        let dst = outU fc 0
        Some(fun step -> dst[step.SimStep] <- src[step.SimStep] ^^^ 1u)

    | GateN(gateType, 2), false ->
        let a = inU fc 0
        let b = inU fc 1
        let dst = outU fc 0

        // one closure per gate type, so that the operation is not dispatched per step
        match gateType with
        | And -> Some(fun step -> dst[step.SimStep] <- a[step.SimStep] &&& b[step.SimStep])
        | Or -> Some(fun step -> dst[step.SimStep] <- a[step.SimStep] ||| b[step.SimStep])
        | Xor -> Some(fun step -> dst[step.SimStep] <- a[step.SimStep] ^^^ b[step.SimStep])
        | Nand -> Some(fun step -> dst[step.SimStep] <- (a[step.SimStep] &&& b[step.SimStep]) ^^^ 1u)
        | Nor -> Some(fun step -> dst[step.SimStep] <- (a[step.SimStep] ||| b[step.SimStep]) ^^^ 1u)
        | Xnor -> Some(fun step -> dst[step.SimStep] <- (a[step.SimStep] ^^^ b[step.SimStep]) ^^^ 1u)

    | GateN(gateType, n), false ->
        let ins = Array.init n (inU fc)
        let dst = outU fc 0
        let negated =
            match gateType with
            | Nand | Nor | Xnor -> true
            | And | Or | Xor -> false

        let combine: uint32 -> uint32 -> uint32 =
            match gateType with
            | And | Nand -> (&&&)
            | Or | Nor -> (|||)
            | Xor | Xnor -> (^^^)

        Some(fun step ->
            let s = step.SimStep
            let mutable acc = ins[0][s]

            for i = 1 to n - 1 do
                acc <- combine acc (ins[i][s])

            dst[s] <- if negated then acc ^^^ 1u else acc)

    // --- multiplexers ----------------------------------------------------------------

    | Mux2, false ->
        let a = inU fc 0
        let b = inU fc 1
        let sel = inU fc 2
        let dst = outU fc 0

        Some(fun step ->
            let s = step.SimStep
            dst[s] <- if sel[s] = 0u then a[s] else b[s])

    | Mux4, false
    | Mux8, false ->
        let n =
            match fc.FType with
            | Mux4 -> 4
            | _ -> 8
        let ins = Array.init n (inU fc)
        let sel = inU fc n
        let dst = outU fc 0
        // the select bus is 2 or 3 bits wide and its stored value is within that width, so it
        // always indexes an input
        Some(fun step ->
            let s = step.SimStep
            dst[s] <- ins[int sel[s]][s])

    | Demux2, false -> demuxU fc 2
    | Demux4, false -> demuxU fc 4
    | Demux8, false -> demuxU fc 8

    // --- bus shaping -----------------------------------------------------------------

    | BusSelection(width, lsb), false ->
        let src = inU fc 0
        let dst = outU fc 0
        let struct (shift, mask) = sliceOf (lsb + width - 1) lsb
        Some(fun step -> dst[step.SimStep] <- (src[step.SimStep] >>> shift) &&& mask)

    | BusCompare(width, compareVal), false
    | BusCompare1(width, compareVal, _), false ->
        let src = inU fc 0
        let dst = outU fc 0
        // narrow the comparison value once. The input is within a width of at most 32, so a
        // compareVal that does not fit a uint32 can never match it - this is what fastReduce
        // spends a heap bigint per step discovering.
        if compareVal >= 0I && compareVal <= 4294967295I then
            let target = uint32 compareVal
            Some(fun step -> dst[step.SimStep] <- (if src[step.SimStep] = target then 1u else 0u))
        else
            Some(fun step -> dst[step.SimStep] <- 0u)

    | MergeWires, false ->
        let a = inU fc 0
        let b = inU fc 1
        let dst = outU fc 0
        // little endian: the top wire's bits are the least significant
        let shift = fc.InputWidth 0

        Some(fun step ->
            let s = step.SimStep
            dst[s] <- (b[s] <<< shift) ||| a[s])

    | SplitWire topWireWidth, false ->
        let src = inU fc 0
        let lo = outU fc 0
        let hi = outU fc 1
        let struct (loShift, loMask) = sliceOf (topWireWidth - 1) 0
        let struct (hiShift, hiMask) = sliceOf (fc.InputWidth 0 - 1) topWireWidth

        Some(fun step ->
            let s = step.SimStep
            let bits = src[s]
            lo[s] <- (bits >>> loShift) &&& loMask
            hi[s] <- (bits >>> hiShift) &&& hiMask)

    | MergeN n, false ->
        let ins = Array.init n (inU fc)
        let widths = Array.init n fc.InputWidth
        let dst = outU fc 0

        Some(fun step ->
            let s = step.SimStep
            let mutable acc = 0u

            for i = n - 1 downto 0 do
                acc <- (acc <<< widths[i]) ||| ins[i][s]

            dst[s] <- acc)

    | SplitN(n, outputWidths, lsBits), false ->
        let src = inU fc 0
        let outs = Array.init n (outU fc)
        let slices =
            List.map2 (fun width lsb -> struct (lsb, maskOf width)) outputWidths lsBits
            |> Array.ofList

        Some(fun step ->
            let s = step.SimStep
            let bits = src[s]

            for i = 0 to n - 1 do
                let struct (shift, mask) = slices[i]
                outs[i][s] <- (bits >>> shift) &&& mask)

    | NbitSpreader numberOfBits, false ->
        let src = inU fc 0
        let dst = outU fc 0
        let allOnes = maskOf numberOfBits
        Some(fun step -> dst[step.SimStep] <- (if src[step.SimStep] = 0u then 0u else allOnes))

    // --- n-bit arithmetic and logic ---------------------------------------------------

    | NbitsAnd _, false ->
        let a = inU fc 0
        let b = inU fc 1
        let dst = outU fc 0
        Some(fun step -> dst[step.SimStep] <- a[step.SimStep] &&& b[step.SimStep])

    | NbitsOr _, false ->
        let a = inU fc 0
        let b = inU fc 1
        let dst = outU fc 0
        Some(fun step -> dst[step.SimStep] <- a[step.SimStep] ||| b[step.SimStep])

    | NbitsXor(_, None), false ->
        let a = inU fc 0
        let b = inU fc 1
        let dst = outU fc 0
        Some(fun step -> dst[step.SimStep] <- a[step.SimStep] ^^^ b[step.SimStep])

    | NbitsXor(_, Some Multiply), false ->
        let a = inU fc 0
        let b = inU fc 1
        let dst = outU fc 0
        let w = fc.InputWidth 0

        if w = 32 then
            // uint32 multiplication wraps at 2^32, which is the masking wanted at width 32
            Some(fun step -> dst[step.SimStep] <- a[step.SimStep] * b[step.SimStep])
        else
            let mask = maskOf w
            Some(fun step -> dst[step.SimStep] <- (a[step.SimStep] * b[step.SimStep]) &&& mask)

    | NbitsNot _, false ->
        let src = inU fc 0
        let dst = outU fc 0
        let w = fc.InputWidth 0

        if w = 32 then
            Some(fun step -> dst[step.SimStep] <- ~~~src[step.SimStep])
        else
            let mask = maskOf w
            Some(fun step -> dst[step.SimStep] <- mask &&& (~~~src[step.SimStep]))

    | NbitsAdder _, false -> adderU fc true true
    | NbitsAdderNoCout _, false -> adderU fc true false
    | NbitsAdderNoCin _, false -> adderU fc false true
    | NbitsAdderNoCinCout _, false -> adderU fc false false

    // --- clocked ----------------------------------------------------------------------

    | DFF, false
    | Register _, false ->
        let src = inU fc 0
        let dst = outU fc 0
        Some(fun step -> dst[step.SimStep] <- oldU src step)

    | DFFE, false
    | RegisterE _, false ->
        let src = inU fc 0
        let enable = inU fc 1
        let dst = outU fc 0

        Some(fun step ->
            dst[step.SimStep] <-
                if oldU enable step = 1u then
                    oldU src step
                else
                    oldU dst step)

    | Counter width, false -> counterU fc width true true
    | CounterNoEnable width, false -> counterU fc width true false
    | CounterNoLoad width, false -> counterU fc width false true
    | CounterNoEnableLoad width, false -> counterU fc width false false

    // Everything else - the bigint paths, memories, shifts - is left to fastReduce
    | _ -> None
