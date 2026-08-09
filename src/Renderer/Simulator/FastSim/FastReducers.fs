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
open Fable.Core
open Fable.Core.JsInterop

/// Array access without a bounds check.
///
/// Under Fable an ordinary arr[i] on a local binding compiles to fable-library's item/setItem,
/// which compare the index twice and can throw. Profiling the app found those two functions
/// taking 70% of all simulation time - more than the component logic and the dispatch put
/// together. (fastReduce escapes them by accident: it writes through comp.Outputs[n].UInt32Step[s],
/// a property chain, which Fable emits as a raw index.)
///
/// Every index here is either a step index, which is < MaxArraySize by construction and so
/// always within a step array, or a multiplexer select, which the masking invariant keeps
/// within its bus width and so within the array of that many ports. Under .NET these stay
/// ordinary checked accesses, so the tests still run against bounds-checked arrays.
#if FABLE_COMPILER
[<Emit("$0[$1]")>]
let private getA (arr: 'a array) (i: int) : 'a = jsNative

[<Emit("$0[$1] = $2")>]
let private setA (arr: 'a array) (i: int) (v: 'a) : unit = jsNative
#else
let inline private getA (arr: 'a array) (i: int) : 'a = arr[i]
let inline private setA (arr: 'a array) (i: int) (v: 'a) : unit = arr[i] <- v
#endif

/// The uint32 step array of input i / output i. Read once, when the reducer is built.
let inline private inU (fc: FastComponent) (i: int) = fc.InputLinks[i].UInt32Step
let inline private outU (fc: FastComponent) (i: int) = fc.Outputs[i].UInt32Step

/// A clocked component reads its inputs and its own output as they were on the previous step.
/// Before the first clock edge there is no previous step, and it reads 0.
let inline private oldU (arr: uint32 array) (step: StepIndex) =
    if step.NumStep = 0 then
        0u
    else
        getA arr step.SimStepOld

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

/// Above this width a ROM lookup table stops being a sensible thing to hold in memory
/// (2^16 words is 256kB), and the component keeps the general path.
let private maxRomTableAddressWidth = 16

/// A ROM read out into a flat table, once. Its contents never change - they are part of the
/// component type - so every read afterwards is an array index. fastReduce instead builds a
/// bigint from the address, looks it up in a Map keyed by bigint, and converts the result
/// back, on every read of every step.
/// Built from the entries the memory actually has rather than by probing every address, so
/// this costs the size of the data and not the size of the address space.
let private romTable (mem: Memory1) : uint32 array =
    let table = Array.zeroCreate<uint32> (1 <<< mem.AddressWidth)
    // unwritten addresses read as 0, which is what zeroCreate has already put there
    mem.Data
    |> Map.iter (fun addr value ->
        if addr >= 0I && addr < bigint table.Length then
            table[int addr] <- convertBigintToUInt32 mem.WordWidth value)
    table

/// n outputs of a demultiplexer: the selected one gets the input, the rest 0
let private demuxU (fc: FastComponent) (n: int) =
    let src = inU fc 0
    let sel = inU fc 1
    let outs = Array.init n (outU fc)

    Some(fun (step: StepIndex) ->
        let s = step.SimStep
        let selected = int (getA sel s)
        let bits = (getA src s)

        for i = 0 to n - 1 do
            setA (getA outs i) s (if i = selected then bits else 0u))

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
            let carryIn = if hasCin then uint64 ((getA cin s) &&& 1u) else 0UL
            let total = uint64 (getA a s) + uint64 (getA b s) + carryIn
            setA sumOut s (uint32 total)

            if hasCout then
                setA coutOut s (uint32 (total >>> 32) &&& 1u))
    else
        Some(fun (step: StepIndex) ->
            let s = step.SimStep
            let carryIn = if hasCin then (getA cin s) &&& 1u else 0u
            let total = (getA a s) + (getA b s) + carryIn
            setA sumOut s (total &&& mask)

            if hasCout then
                setA coutOut s ((total >>> w) &&& 1u))

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

        setA dst step.SimStep (
            if hasEnable && oldU enable step <> 1u then lastOut
            elif hasLoad && oldU load step = 1u then oldU loadData step
            else incrementWithinWidth width lastOut))

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
        Some(fun step -> setA dst step.SimStep ((getA src step.SimStep)))

    | Input1 _, false ->
        // Active is mutable and read per step, not captured
        let src = inU fc 0
        let dst = outU fc 0

        Some(fun step ->
            if fc.Active then
                setA dst step.SimStep ((getA src step.SimStep)))

    | NotConnected, _ -> Some(fun _ -> ())

    // --- constants -------------------------------------------------------------------

    | Constant1(width, cVal, _), false
    | Constant(width, cVal), false ->
        let dst = outU fc 0
        // a negative constant is stored as its width-wide two's complement bit pattern
        let value = uint32 (twosComp width cVal)
        Some(fun step -> setA dst step.SimStep (value))

    // --- gates -----------------------------------------------------------------------

    | Not, false ->
        let src = inU fc 0
        let dst = outU fc 0
        Some(fun step -> setA dst step.SimStep ((getA src step.SimStep) ^^^ 1u))

    | GateN(gateType, 2), false ->
        let a = inU fc 0
        let b = inU fc 1
        let dst = outU fc 0

        // one closure per gate type, so that the operation is not dispatched per step
        match gateType with
        | And -> Some(fun step -> setA dst step.SimStep ((getA a step.SimStep) &&& (getA b step.SimStep)))
        | Or -> Some(fun step -> setA dst step.SimStep ((getA a step.SimStep) ||| (getA b step.SimStep)))
        | Xor -> Some(fun step -> setA dst step.SimStep ((getA a step.SimStep) ^^^ (getA b step.SimStep)))
        | Nand -> Some(fun step -> setA dst step.SimStep (((getA a step.SimStep) &&& (getA b step.SimStep)) ^^^ 1u))
        | Nor -> Some(fun step -> setA dst step.SimStep (((getA a step.SimStep) ||| (getA b step.SimStep)) ^^^ 1u))
        | Xnor -> Some(fun step -> setA dst step.SimStep (((getA a step.SimStep) ^^^ (getA b step.SimStep)) ^^^ 1u))

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
            let mutable acc = getA (getA ins 0) s

            for i = 1 to n - 1 do
                acc <- combine acc (getA (getA ins i) s)

            setA dst s (if negated then acc ^^^ 1u else acc))

    // --- multiplexers ----------------------------------------------------------------

    | Mux2, false ->
        let a = inU fc 0
        let b = inU fc 1
        let sel = inU fc 2
        let dst = outU fc 0

        Some(fun step ->
            let s = step.SimStep
            setA dst s (if (getA sel s) = 0u then (getA a s) else (getA b s)))

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
            setA dst s (getA (getA ins (int (getA sel s))) s))

    | Demux2, false -> demuxU fc 2
    | Demux4, false -> demuxU fc 4
    | Demux8, false -> demuxU fc 8

    // --- bus shaping -----------------------------------------------------------------

    | BusSelection(width, lsb), false ->
        let src = inU fc 0
        let dst = outU fc 0
        let struct (shift, mask) = sliceOf (lsb + width - 1) lsb
        Some(fun step -> setA dst step.SimStep (((getA src step.SimStep) >>> shift) &&& mask))

    | BusCompare(width, compareVal), false
    | BusCompare1(width, compareVal, _), false ->
        let src = inU fc 0
        let dst = outU fc 0
        // narrow the comparison value once. The input is within a width of at most 32, so a
        // compareVal that does not fit a uint32 can never match it - this is what fastReduce
        // spends a heap bigint per step discovering.
        if compareVal >= 0I && compareVal <= 4294967295I then
            let target = uint32 compareVal
            Some(fun step -> setA dst step.SimStep ((if (getA src step.SimStep) = target then 1u else 0u)))
        else
            Some(fun step -> setA dst step.SimStep (0u))

    | MergeWires, false ->
        let a = inU fc 0
        let b = inU fc 1
        let dst = outU fc 0
        // little endian: the top wire's bits are the least significant
        let shift = fc.InputWidth 0

        Some(fun step ->
            let s = step.SimStep
            setA dst s (((getA b s) <<< shift) ||| (getA a s)))

    | SplitWire topWireWidth, false ->
        let src = inU fc 0
        let lo = outU fc 0
        let hi = outU fc 1
        let struct (loShift, loMask) = sliceOf (topWireWidth - 1) 0
        let struct (hiShift, hiMask) = sliceOf (fc.InputWidth 0 - 1) topWireWidth

        Some(fun step ->
            let s = step.SimStep
            let bits = (getA src s)
            setA lo s ((bits >>> loShift) &&& loMask)
            setA hi s ((bits >>> hiShift) &&& hiMask))

    | MergeN n, false ->
        let ins = Array.init n (inU fc)
        let widths = Array.init n fc.InputWidth
        let dst = outU fc 0

        Some(fun step ->
            let s = step.SimStep
            let mutable acc = 0u

            for i = n - 1 downto 0 do
                acc <- (acc <<< (getA widths i)) ||| getA (getA ins i) s

            setA dst s (acc))

    | SplitN(n, outputWidths, lsBits), false ->
        let src = inU fc 0
        let outs = Array.init n (outU fc)
        let slices =
            List.map2 (fun width lsb -> struct (lsb, maskOf width)) outputWidths lsBits
            |> Array.ofList

        Some(fun step ->
            let s = step.SimStep
            let bits = (getA src s)

            for i = 0 to n - 1 do
                let struct (shift, mask) = (getA slices i)
                setA (getA outs i) s ((bits >>> shift) &&& mask))

    | NbitSpreader numberOfBits, false ->
        let src = inU fc 0
        let dst = outU fc 0
        let allOnes = maskOf numberOfBits
        Some(fun step -> setA dst step.SimStep ((if (getA src step.SimStep) = 0u then 0u else allOnes)))

    // --- n-bit arithmetic and logic ---------------------------------------------------

    | NbitsAnd _, false ->
        let a = inU fc 0
        let b = inU fc 1
        let dst = outU fc 0
        Some(fun step -> setA dst step.SimStep ((getA a step.SimStep) &&& (getA b step.SimStep)))

    | NbitsOr _, false ->
        let a = inU fc 0
        let b = inU fc 1
        let dst = outU fc 0
        Some(fun step -> setA dst step.SimStep ((getA a step.SimStep) ||| (getA b step.SimStep)))

    | NbitsXor(_, None), false ->
        let a = inU fc 0
        let b = inU fc 1
        let dst = outU fc 0
        Some(fun step -> setA dst step.SimStep ((getA a step.SimStep) ^^^ (getA b step.SimStep)))

    | NbitsXor(_, Some Multiply), false ->
        let a = inU fc 0
        let b = inU fc 1
        let dst = outU fc 0
        let w = fc.InputWidth 0

        if w = 32 then
            // uint32 multiplication wraps at 2^32, which is the masking wanted at width 32
            Some(fun step -> setA dst step.SimStep ((getA a step.SimStep) * (getA b step.SimStep)))
        else
            let mask = maskOf w
            Some(fun step -> setA dst step.SimStep (((getA a step.SimStep) * (getA b step.SimStep)) &&& mask))

    | NbitsNot _, false ->
        let src = inU fc 0
        let dst = outU fc 0
        let w = fc.InputWidth 0

        if w = 32 then
            Some(fun step -> setA dst step.SimStep (~~~(getA src step.SimStep)))
        else
            let mask = maskOf w
            Some(fun step -> setA dst step.SimStep (mask &&& (~~~(getA src step.SimStep))))

    | NbitsAdder _, false -> adderU fc true true
    | NbitsAdderNoCout _, false -> adderU fc true false
    | NbitsAdderNoCin _, false -> adderU fc false true
    | NbitsAdderNoCinCout _, false -> adderU fc false false

    // --- clocked ----------------------------------------------------------------------

    | DFF, false
    | Register _, false ->
        let src = inU fc 0
        let dst = outU fc 0
        Some(fun step -> setA dst step.SimStep (oldU src step))

    | DFFE, false
    | RegisterE _, false ->
        let src = inU fc 0
        let enable = inU fc 1
        let dst = outU fc 0

        Some(fun step ->
            setA dst step.SimStep (
                if oldU enable step = 1u then
                    oldU src step
                else
                    oldU dst step))

    | Counter width, false -> counterU fc width true true
    | CounterNoEnable width, false -> counterU fc width true false
    | CounterNoLoad width, false -> counterU fc width false true
    | CounterNoEnableLoad width, false -> counterU fc width false false

    // --- shifts -----------------------------------------------------------------------

    // Input 0 is the data, input 1 the shift amount. Shifting by the bus width or more gives
    // 0, or all sign bits for ASR. One closure per shift kind, so the kind is not re-examined
    // every step, and the mask is worked out once.
    | Shift(width, _, shiftType), false ->
        let src = inU fc 0
        let amtIn = inU fc 1
        let dst = outU fc 0
        let mask = maskOf width
        let signBit = width - 1

        match shiftType with
        | LSL ->
            Some(fun step ->
                let s = step.SimStep
                let bits = getA src s &&& mask
                let amt = getA amtIn s

                setA dst s (
                    if amt >= uint32 width then
                        0u
                    else
                        (bits <<< int amt) &&& mask))
        | LSR ->
            Some(fun step ->
                let s = step.SimStep
                let bits = getA src s &&& mask
                let amt = getA amtIn s

                setA dst s (
                    if amt >= uint32 width then
                        0u
                    else
                        bits >>> int amt))
        | ASR ->
            Some(fun step ->
                let s = step.SimStep
                let bits = getA src s &&& mask
                let amt = getA amtIn s
                let signSet = (bits >>> signBit) &&& 1u = 1u

                setA dst s (
                    if amt = 0u then
                        bits
                    elif amt >= uint32 width then
                        (if signSet then mask else 0u)
                    elif signSet then
                        let a = int amt
                        (bits >>> a) ||| (mask &&& (mask <<< (width - a)))
                    else
                        bits >>> int amt))

    // --- read-only memories -------------------------------------------------------------

    | AsyncROM1 mem, false when mem.AddressWidth <= maxRomTableAddressWidth ->
        let src = inU fc 0
        let dst = outU fc 0
        let table = romTable mem
        // the address bus is AddressWidth wide and its stored value is within that width, so
        // it always indexes the table
        Some(fun step ->
            let s = step.SimStep
            setA dst s (getA table (int (getA src s))))

    | ROM1 mem, false when mem.AddressWidth <= maxRomTableAddressWidth ->
        // synchronous: the address is the one presented on the previous clock edge
        let src = inU fc 0
        let dst = outU fc 0
        let table = romTable mem
        Some(fun step -> setA dst step.SimStep (getA table (int (oldU src step))))

    // Everything else is left to fastReduce: the bigint paths, the read/write memories, and
    // ROMs too wide to tabulate. RAM needs its state representation changed before it can have
    // a reducer of this kind - the per-step history it writes for the waveform viewer is a Map
    // keyed by bigint, and that Map, not the reducer, is what a RAM read costs.
    | _ -> None
