module FastReduceTT

open CommonTypes
open SimGraphTypes
open SimTypes
open NumberHelpers
open System
open Helpers
open FastReduce

//-----------------------------------------------------------------------------------------------------//
//---------------------------------TRUTH TABLE SIMULATION FUNCTION-------------------------------------//
//----------------------------------------FastReduceFData----------------------------------------------//
//-----------------------------------------------------------------------------------------------------//
//-----------------------------------------------------------------------------------------------------//

/// Version of fastReduce which operates on the IOArray.FDataStep arrays,
/// and simulates using posibly algebraic data.
/// Given the input port values for a component comp, work out its output in the same clock cycle.
/// Used by TruthTable simulations, which use FData type that includes algebraic data.
/// Because TruthTable simulations are only combinational the match statement is much simpler than normal FastReduce.
/// Clocked operations need not be implemented.
let fastReduceFData (maxArraySize: int) (numStep: int) (isClockedReduction: bool) (comp: FastComponent) : Unit =

// TODO - the isClockedReduction = true code below under asyncRAM should be deleted. It is not used.
// However, doing this needs careful testing juts in case I am wrong!


    let componentType = comp.FType

    let simStep = numStep % maxArraySize // the arrays (for v long simulations) warap round as circular arrays
    let simStepOld = // previous step may be wrapped around
        if simStep = 0 then
            maxArraySize - 1
        else
            simStep - 1
#if ASSERTS
    printfn "Warning: simulation is running with ASSERTS on for debugging -this will be very slow!"
#endif

    ///  get data feom input i of component
    let inline ins i =
#if ASSERTS
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
#endif
        let fd = comp.InputLinks[i].FDataStep[simStep]
        fd

    /// get last cycle data from output i (for clocked components)
    let inline getLastCycleOut n =
        let fd =
            match comp.OutputWidth n, numStep with
            | 0, _ -> failwithf "Can't reduce %A (%A) because outputwidth is not known" comp.FullName comp.FType
            | w, 0 ->
                if w < 33 then
                    Data { Dat = Word 0u; Width = w }
                else
                    Data { Dat = BigWord 0I; Width = w }
            | w, _ -> comp.Outputs[n].FDataStep[simStepOld]

        fd

    /// get last cycle data from output i for component
    let inline insOld i =
#if ASSERTS
        assertThat
            (i < n)
            (sprintf
                "What? Invalid input port (%d:step%d) used by %s (%A) reducer with %d Ins"
                i
                simStep
                comp.FullName
                componentType
                n)
#endif

        let fd = comp.GetInputFData (simStepOld) (InputPortNumber i)
        fd

    /// Write current step output data for output port 0
    let inline put pn fd =
        comp.PutOutputFData (simStep) (OutputPortNumber pn) fd

    /// Write current State (used only for RAMs, DFFs and registers use previous cycle output as state)
    let inline putState state =
        match comp.State with
        | None -> failwithf "Attempt to put state into component %s without state array" comp.FullName
        | Some stateArr -> stateArr.Step[simStep] <- state

    // /// implement a binary combinational operation
    // let inline getBinaryGateReducer
    //     (bitOp: uint32 -> uint32 -> uint32)
    //     (algOp: FastAlgExp -> FastAlgExp -> FastAlgExp)
    //     : Unit
    //     =
    //     match (ins 0), (ins 1) with
    //     | Data d1, Data d2 ->
    //         let bit0 = d1.GetQUint32
    //         let bit1 = d2.GetQUint32

    //         put 0 <| Data { Width = 1; Dat = Word(bitOp bit1 bit0) }
    //     | A, B -> put 0 <| Alg(algOp A.toExp B.toExp)
    
    /// implement an N-input binary combinational operation
    let inline getNInpBinaryGateReducer
        (n: int)
        (gateType: GateComponentType)
        : Unit
        =
        let isAlgExp inp =
            match inp with
            | Data _ -> false
            | _ -> true
        
        let inputs =
            [0..n-1]
            |> List.map ins
        
        if (List.exists isAlgExp inputs) then
            // get nested binary expressions
            inputs
            |> List.map (fun el -> el.toExp)
            |> List.reduce (algGate gateType)
            |> function
                | exp when (isNegated gateType) ->
                    UnaryExp(NotOp, exp)
                | exp -> exp
            |> Alg
            |> put 0
            
        else
            // compute the result
            inputs
            |> List.map
                (function
                    | Data d -> d.GetQUint32
                    | Alg _ -> failwith "Can't encounter algebraic expression here")
            |> List.reduce (bitGate gateType)
            |> function
                | x when (isNegated gateType) ->
                    if x = 1u then
                        Data { Width = 1; Dat = Word(0u)}
                    else
                        Data { Width = 1; Dat = Word(1u)}
                | x ->
                    Data { Width = 1; Dat = Word(x)}
            |> put 0
    // | Alg exp1, Alg exp2 ->
    //     put 0 <| Alg (algOp exp1 exp2)
    // | Alg exp1, Data d
    // | Data d, Alg exp1 ->
    //     let exp2 = DataLiteral d
    //     put 0 <| Alg (algOp exp1 exp2)

    /// Error checking (not required in production code) check widths are consistent
    let inline checkWidth width (bits: FData) =
#if ASSERTS
        assertThat
            (bits.Width = width)
            (sprintf
                "Input node reducer for (%A:%A - STEP %d) received wrong number of bits: expected %d but got %d"
                comp.FullName
                comp.FType
                simStep
                width
                bits.Width)
#else
        ()
#endif

    let makeSplitOutput fd outNum width lsb =
        match fd with
            | Data bits ->
                let bits = getBits (lsb + width - 1) lsb bits
                // Little endian, bits leaving from the top wire are the least
                // significant.
                put outNum <| Data bits

            | Alg(UnaryExp(BitRangeOp(l, u), exp)) ->
                let exp = UnaryExp(BitRangeOp(l + lsb, l + lsb + width - 1), exp)
                put outNum <| Alg exp

            | Alg(UnaryExp(NotOp, exp)) ->
                let w = getAlgExpWidth (UnaryExp(NotOp, exp))
                let exp = UnaryExp(BitRangeOp(lsb, lsb + width - 1), exp)
                put outNum <| Alg(UnaryExp(NotOp, exp))


            | Alg exp ->
                let w = getAlgExpWidth exp
                let exp = UnaryExp(BitRangeOp(lsb, lsb + width - 1), exp)
                put outNum <| Alg exp

    // Reduce the component in this match.
    // Each case case calculated the output for its type of component
    // NB CustomComponent is not needed since these are removed in the FastComponent
    // generation stage - being replaced by the relavant sheet logic.
    match componentType with
    | ROM _
    | RAM _
    | AsyncROM _ -> failwithf "What? Legacy RAM component types should never occur"
    | Input _ -> failwithf "Legacy Input component types should never occur"
    | Input1(width, _) ->
        if comp.Active then
            let bits = ins 0
            //printfn "Got input 0 = %A Links=<%A> len=%d" bits comp.InputLinks comp.InputLinks.Length
            checkWidth width bits
            //printfn "output array = %A" comp.Outputs
            put 0 bits
    //printfn "Finished!"

    | Constant1(width, cVal, _)
    | Constant(width, cVal) -> put 0 <| Data(convertBigintToFastData width cVal)
    | Output width ->
        let bits = ins 0
        //printfn "In output bits=%A, ins = %A" bits comp.InputLinks
        checkWidth width bits
        put 0 bits
    | Viewer width ->
        let bits = ins 0
        //printfn "In output bits=%A, ins = %A" bits comp.InputLinks
        checkWidth width bits
        put 0 bits

    | IOLabel ->
        let bits = ins 0
        //let bits = comp.InputLinks[0][simStep]
        //printfn "Reducing IOLabel %A" comp.SimComponent.Label
        put 0 bits
    | NotConnected -> ()
    | Not ->
        match (ins 0) with
        | Data _ ->
            let bit = extractBitFData (ins 0) 1
            put 0 <| packBitFData (bitNot bit)
        | Alg exp -> put 0 <| Alg(algNot exp)
    | BusSelection(width, lsb) ->
        match (ins 0) with
        | Data bits ->
#if ASSERTS
            assertThat
                (bits.Width >= width + lsb)
                (sprintf "Bus Selection received too few bits: expected at least %d but got %d" (width + lsb) bits.Width)
#endif
            let outBits = getBits (lsb + width - 1) lsb bits
            put 0 <| Data outBits
        | Alg exp ->
#if ASSERTS
            assertThat
                ((getAlgExpWidth exp) >= width + lsb)
                (sprintf
                    "Bus Selection received too few bits: expected at least %d but got %d"
                    (width + lsb)
                    (getAlgExpWidth exp))
#endif
            let newExp = UnaryExp(BitRangeOp(lsb, lsb + width - 1), exp)
            put 0 <| Alg newExp

    | BusCompare(width, compareVal) ->
        //printfn "Reducing compare %A" comp.SimComponent.Label
        match (ins 0) with
        | Data bits ->
#if ASSERTS
            assertThat
                (bits.Width = width)
                ($"Bus Compare {comp.FullName} received wrong number of bits: expecting  {width} but got {bits.Width}")
#endif
            let inputNum = convertFastDataToBigint bits

            let outNum: FData =
                if inputNum = compareVal then
                    1u
                else
                    0u
                |> packBitFData

            put 0 outNum
        | Alg exp ->
#if ASSERTS
            assertThat
                ((getAlgExpWidth exp) = width)
                ($"Bus Compare {comp.FullName} received wrong number of bits: expecting  {width} but got {(getAlgExpWidth exp)}")
#endif
            put 0 <| Alg(ComparisonExp(exp, Equals, compareVal))
    | BusCompare1(width, compareVal, dialogText) ->
        //printfn "Reducing compare %A" comp.SimComponent.Label
        match (ins 0) with
        | Data bits ->
#if ASSERTS
            assertThat
                (bits.Width = width)
                ($"Bus Compare {comp.FullName} received wrong number of bits: expecting  {width} but got {bits.Width}")
#endif
            let inputNum = convertFastDataToBigint bits

            let outNum: FData =
                if inputNum = compareVal then
                    1u
                else
                    0u
                |> packBitFData

            put 0 outNum
        | Alg exp ->
#if ASSERTS
            assertThat
                ((getAlgExpWidth exp) = width)
                ($"Bus Compare {comp.FullName} received wrong number of bits: expecting  {width} but got {(getAlgExpWidth exp)}")
#endif
            put 0 <| Alg(ComparisonExp(exp, Equals, compareVal))

    | GateN (gateType, n) -> getNInpBinaryGateReducer n gateType

    | Mux2 ->
        match (ins 0), (ins 1), (ins 2) with
        | Alg exp1, Alg exp2, Data bitSelect ->
#if ASSERT
            assertThat (getAlgExpWidth exp1 = getAlgExpWidth exp2)
            <| sprintf
                "Mux2 %s received two inputs with different widths: (%A) <> (%A)"
                comp.FullName
                (expToString exp1)
                (expToString exp2)
#endif
            let out =
                if (extractBitFData (Data bitSelect) 1) = 0u then
                    Alg exp1
                else
                    Alg exp2

            put 0 out

        | Alg exp, Data bits, Data bitSelect ->
#if ASSERT
            assertThat (bits.Width = getAlgExpWidth exp)
            <| sprintf
                "Mux2 %s received two inputs with different widths: (%A) <> (%A)"
                comp.FullName
                (expToString exp)
                bits
#endif
            let out =
                if (extractBitFData (Data bitSelect) 1) = 0u then
                    Alg exp
                else
                    Data bits

            put 0 out
        | Data bits, Alg exp, Data bitSelect ->
#if ASSERT
            assertThat (bits.Width = getAlgExpWidth exp)
            <| sprintf
                "Mux2 %s received two inputs with different widths: (%A) <> (%A)"
                comp.FullName
                bits
                (expToString exp)
#endif
            let out =
                if (extractBitFData (Data bitSelect) 1) = 0u then
                    Data bits
                else
                    Alg exp

            put 0 out
        | Data bits0, Data bits1, Data bitSelect ->
#if ASSERT
            assertThat (bits0.Width = bits1.Width)
            <| sprintf "Mux2 %s received two inputs with different widths: (%A) <> (%A)" comp.FullName bits0 bits1
#endif
            let out =
                if (extractBitFData (Data bitSelect) 1) = 0u then
                    bits0
                else
                    bits1

            put 0 <| Data out

        | _, _, Alg _ ->
            let err =
                { ErrType = AlgInpNotAllowed "The chosen set of Algebraic inputs results in algebra being passed to the
                    SEL port of a Mux2. Only values can be passed to this port."
                  InDependency = Some(comp.FullName)
                  ComponentsAffected = [ comp.cId ]
                  ConnectionsAffected = [] }
            // Algebra at SEL port is not supported
            raise (AlgebraNotImplemented err)
    | Mux4 ->
        match ins 0, ins 1, ins 2, ins 3, ins 4 with
        | fd0, fd1, fd2, fd3, Data bitSelect ->
#if ASSERT
            assertThat (
                bits0.Width = bits1.Width
                && bits0.Width = bits2.Width
            )
            <| sprintf
                "Mux4 %s received two inputs with different widths: (%A) <> (%A)"
                comp.FullName
                fd0.fdToString
                fd1.fdToString
#endif

            let out =
                match (extractBitFData (Data bitSelect) 2) with
                | 0u -> fd0
                | 1u -> fd1
                | 2u -> fd2
                | 3u -> fd3
                | _ -> failwithf "Cannot happen"

            put 0 out

        | _, _, _, _, Alg _ ->
            let err =
                { ErrType = AlgInpNotAllowed "The chosen set of Algebraic inputs results in algebra being passed to the
                    SEL port of a Mux4. Only values can be passed to this port."
                  InDependency = Some(comp.FullName)
                  ComponentsAffected = [ comp.cId ]
                  ConnectionsAffected = [] }
            // Algebra at SEL port is not supported
            raise (AlgebraNotImplemented err)
    | Mux8 ->
        //let bits0, bits1, bits2, bits3, bits4, bits5, bits6, bits7, bitSelect = ins 0, ins 1, ins 2, ins 3, ins 4, ins 5, ins 6, ins 7, ins 8
        match ins 0, ins 1, ins 2, ins 3, ins 4, ins 5, ins 6, ins 7, ins 8 with
        | fd0, fd1, fd2, fd3, fd4, fd5, fd6, fd7, Data bitSelect ->
#if ASSERT
            assertThat (
                fd0.Width = fd1.Width
                && fd0.Width = fd2.Width
                && fd0.Width = fd3.Width
                && fd0.Width = fd4.Width
                && fd0.Width = fd5.Width
                && fd0.Width = fd6.Width
                && fd0.Width = fd7.Width
            )
            <| sprintf
                "Mux8 %s received two inputs with different widths: (%A) <> (%A)"
                comp.FullName
                fd0.fdToString
                fd1.fdToString
#endif

            let out =
                match (extractBitFData (Data bitSelect) 3) with
                | 0u -> fd0
                | 1u -> fd1
                | 2u -> fd2
                | 3u -> fd3
                | 4u -> fd4
                | 5u -> fd5
                | 6u -> fd6
                | 7u -> fd7
                | _ -> failwithf "Cannot happen"

            put 0 out

        | _, _, _, _, _, _, _, _, Alg _ ->
            let err =
                { ErrType = AlgInpNotAllowed "The chosen set of Algebraic inputs results in algebra being passed to the
                    SEL port of a Mux8. Only values can be passed to this port."
                  InDependency = Some(comp.FullName)
                  ComponentsAffected = [ comp.cId ]
                  ConnectionsAffected = [] }
            // Algebra at SEL port is not supported
            raise (AlgebraNotImplemented err)
    | Demux2 ->
        match ins 0, ins 1 with
        | fdIn, Data bitSelect ->
            let zeros = Data <| convertIntToFastData fdIn.Width 0u

            let out0, out1 =
                if (extractBitFData (Data bitSelect) 1) = 0u then
                    fdIn, zeros
                else
                    zeros, fdIn

            let w = fdIn.Width
            put 0 out0
            put 1 out1


        | _, Alg _ ->
            let err =
                { ErrType = AlgInpNotAllowed "The chosen set of Algebraic inputs results in algebra being passed to the
                    SEL port of a Demux2. Only values can be passed to this port."
                  InDependency = Some(comp.FullName)
                  ComponentsAffected = [ comp.cId ]
                  ConnectionsAffected = [] }
            // Algebra at SEL port is not supported
            raise (AlgebraNotImplemented err)
    | Demux4 ->
        match ins 0, ins 1 with
        | fdIn, Data bitSelect ->
            let zeros = Data <| convertIntToFastData fdIn.Width 0u

            let out0, out1, out2, out3 =
                match (extractBitFData (Data bitSelect) 2) with
                | 0u -> fdIn, zeros, zeros, zeros
                | 1u -> zeros, fdIn, zeros, zeros
                | 2u -> zeros, zeros, fdIn, zeros
                | 3u -> zeros, zeros, zeros, fdIn
                | _ -> failwithf "Cannot happen"

            let w = fdIn.Width
            put 0 out0
            put 1 out1
            put 2 out2
            put 3 out3




        | _, Alg _ ->
            let err =
                { ErrType = AlgInpNotAllowed "The chosen set of Algebraic inputs results in algebra being passed to the
                    SEL port of a Demux4. Only values can be passed to this port."
                  InDependency = Some(comp.FullName)
                  ComponentsAffected = [ comp.cId ]
                  ConnectionsAffected = [] }
            // Algebra at SEL port is not supported
            raise (AlgebraNotImplemented err)
    | Demux8 ->
        match ins 0, ins 1 with
        | fdIn, Data bitSelect ->
            let zeros = Data <| convertIntToFastData fdIn.Width 0u

            let out0, out1, out2, out3, out4, out5, out6, out7 =
                match (extractBitFData (Data bitSelect) 3) with
                | 0u -> fdIn, zeros, zeros, zeros, zeros, zeros, zeros, zeros
                | 1u -> zeros, fdIn, zeros, zeros, zeros, zeros, zeros, zeros
                | 2u -> zeros, zeros, fdIn, zeros, zeros, zeros, zeros, zeros
                | 3u -> zeros, zeros, zeros, fdIn, zeros, zeros, zeros, zeros
                | 4u -> zeros, zeros, zeros, zeros, fdIn, zeros, zeros, zeros
                | 5u -> zeros, zeros, zeros, zeros, zeros, fdIn, zeros, zeros
                | 6u -> zeros, zeros, zeros, zeros, zeros, zeros, fdIn, zeros
                | 7u -> zeros, zeros, zeros, zeros, zeros, zeros, zeros, fdIn
                | _ -> failwithf "Cannot happen"

            let w = fdIn.Width
            put 0 out0
            put 1 out1
            put 2 out2
            put 3 out3
            put 4 out4
            put 5 out5
            put 6 out6
            put 7 out7








        | _, Alg _ ->
            let err =
                { ErrType = AlgInpNotAllowed "The chosen set of Algebraic inputs results in algebra being passed to the
                    SEL port of a Demux8. Only values can be passed to this port."
                  InDependency = Some(comp.FullName)
                  ComponentsAffected = [ comp.cId ]
                  ConnectionsAffected = [] }
            // Algebra at SEL port is not supported
            raise (AlgebraNotImplemented err)
    | NbitsAdder numberOfBits
    | NbitsAdderNoCout numberOfBits ->
        //let cin, A, B = ins 0, ins 1, ins 2
        match ins 0, ins 1, ins 2 with
        | Data cin, Data A, Data B ->
            let sum, cout =
                let cin = convertFastDataToInt cin
                let w = A.Width

                match A.Dat, B.Dat with
                | BigWord a, BigWord b ->
                    let mask = bigIntMask w
                    let a = a &&& mask
                    let b = b &&& mask
                    let sumInt =
                        if cin = 0u then
                            a + b
                        else
                            a + b + 1I

                    let sum = { Dat = BigWord(sumInt &&& bigIntMask w); Width = w }

                    let cout =
                        if (sumInt >>> w) = 0I then
                            0u
                        else
                            1u
                    sum, packBitFData cout
                | Word a, Word b ->
                    let mask = (1ul <<< w) - 1ul

                    if w = 32 then
                        // mask is not needed, but 64 bit adition is needed!
                        let sumInt = uint64 a + uint64 b + uint64 (cin &&& 1u)
                        let cout = uint32 (sumInt >>> w) &&& 1u
                        let sum = convertIntToFastData w (uint32 sumInt)
                        sum, packBitFData cout
                    else
                        let sumInt = (a &&& mask) + (b &&& mask) + (cin &&& 1u)
                        let cout = (sumInt >>> w) &&& 1u
                        let sum = convertIntToFastData w (sumInt &&& mask)
                        sum, packBitFData cout
                | a, b -> failwithf $"Inconsistent inputs to NBitsAdder {comp.FullName} A={a},{A}; B={b},{B}"

            match componentType with
            | NbitsAdder _ ->
                put 0 <| Data sum
                put 1 cout
            | _ -> put 0 <| Data sum
        | cin, A, B ->
            let cinExp, aExp, bExp = cin.toExp, A.toExp, B.toExp
            let newExp = BinaryExp(BinaryExp(aExp, AddOp, bExp), AddOp, cinExp)
            let out0 = newExp
            let out1 = UnaryExp(CarryOfOp, newExp)

            match componentType with
            | NbitsAdder _ ->
                put 0 <| Alg out0
                put 1 <| Alg out1
            | _ -> put 0 <| Alg out0
    | NbitsAdderNoCin numberOfBits
    | NbitsAdderNoCinCout numberOfBits ->
        //let cin, A, B = ins 0, ins 1, ins 2
        match ins 0, ins 1 with
        | Data A, Data B ->
            let sum, cout =
                let cin = 0u
                let w = A.Width

                match A.Dat, B.Dat with
                | BigWord a, BigWord b ->
                    let mask = bigIntMask w
                    let a = a &&& mask
                    let b = b &&& mask
                    let sumInt =
                        if cin = 0u then
                            a + b
                        else
                            a + b + 1I

                    let sum = { Dat = BigWord(sumInt &&& bigIntMask w); Width = w }

                    let cout =
                        if (sumInt >>> w) = 0I then
                            0u
                        else
                            1u
                    sum, packBitFData cout
                | Word a, Word b ->
                    let mask = (1ul <<< w) - 1ul

                    if w = 32 then
                        // mask is not needed, but 64 bit adition is needed!
                        let sumInt = uint64 a + uint64 b + uint64 (cin &&& 1u)
                        let cout = uint32 (sumInt >>> w) &&& 1u
                        let sum = convertIntToFastData w (uint32 sumInt)
                        sum, packBitFData cout
                    else
                        let sumInt = (a &&& mask) + (b &&& mask) + (cin &&& 1u)
                        let cout = (sumInt >>> w) &&& 1u
                        let sum = convertIntToFastData w (sumInt &&& mask)
                        sum, packBitFData cout
                | a, b -> failwithf $"Inconsistent inputs to NBitsAdder {comp.FullName} A={a},{A}; B={b},{B}"

            match componentType with
            | NbitsAdderNoCin _ ->
                put 0 <| Data sum
                put 1 cout
            | _ -> put 0 <| Data sum
        | A, B ->
            let aExp, bExp = A.toExp, B.toExp
            let newExp = BinaryExp(aExp, AddOp, bExp)
            let out0 = newExp
            let out1 = UnaryExp(CarryOfOp, newExp)

            match componentType with
            | NbitsAdderNoCin _ ->
                put 0 <| Alg out0
                put 1 <| Alg out1
            | _ -> put 0 <| Alg out0
    | NbitsXor(numberOfBits, op) ->
        //let A, B = ins 0, ins 1
        match ins 0, ins 1 with
        | Data A, Data B ->
            let outDat =
                match A.Dat, B.Dat with
                | BigWord a, BigWord b ->
                    BigWord(
                        match op with
                        | None -> a ^^^ b
                        | Some Multiply -> (a * b) &&& ((1I <<< A.Width) - 1I)
                    )
                | Word a, Word b ->
                    Word(
                        match op with
                        | None -> a ^^^ b
                        | Some Multiply -> (a * b) &&& ((1u <<< A.Width) - 1u)
                    )
                | a, b -> failwithf $"Inconsistent inputs to NBitsXOr {comp.FullName} A={a},{A}; B={b},{B}"

            put 0 <| Data { A with Dat = outDat }
        | Alg exp, Data { Dat = (Word num); Width = w }
        | Data { Dat = (Word num); Width = w }, Alg exp ->
            let minusOne = (2.0 ** w) - 1.0 |> uint32

            if num = minusOne then
                put 0 <| Alg(UnaryExp(NotOp, exp))
            //Alg (BinaryExp(UnaryExp(NegOp,exp),SubOp,DataLiteral {Dat = Word 1u; Width=w}))
            else
                let numExp = (packBitFData num).toExp
                put 0 <| Alg(BinaryExp(exp, AddOp, numExp))
        | A, B ->
            let aExp, bExp = A.toExp, B.toExp
            put 0 <| Alg(BinaryExp(aExp, BitXorOp, bExp))
    | NbitsOr numberOfBits ->
        //let A, B = ins 0, ins 1
        match ins 0, ins 1 with
        | Data A, Data B ->
            let outDat =
                match A.Dat, B.Dat with
                | BigWord a, BigWord b -> BigWord(a ||| b)
                | Word a, Word b -> Word(a ||| b)
                | a, b -> failwithf $"Inconsistent inputs to NBitsXOr {comp.FullName} A={a},{A}; B={b},{B}"

            put 0 <| Data { A with Dat = outDat }
        | Alg exp, Data { Dat = (Word num); Width = w }
        | Data { Dat = (Word num); Width = w }, Alg exp ->
            let minusOne = (2.0 ** w) - 1.0 |> uint32

            if num = 0u then
                put 0 <| Alg exp
            // else if num=minusOne
            //     put 0 <| Alg
            else
                let numExp = (Data { Dat = (Word num); Width = w }).toExp
                put 0 <| Alg(BinaryExp(exp, BitOrOp, numExp))
        | A, B ->
            let aExp, bExp = A.toExp, B.toExp
            put 0 <| Alg(BinaryExp(aExp, BitOrOp, bExp))
    | NbitsAnd numberOfBits ->
        match ins 0, ins 1 with
        | Data A, Data B ->
            let outDat =
                match A.Dat, B.Dat with
                | BigWord a, BigWord b -> BigWord(a &&& b)
                | Word a, Word b -> Word(a &&& b)
                | a, b -> failwithf $"Inconsistent inputs to NBitsAnd {comp.FullName} A={a},{A}; B={b},{B}"

            put 0 <| Data { A with Dat = outDat }
        | Alg exp, Data { Dat = (Word num); Width = w }
        | Data { Dat = (Word num); Width = w }, Alg exp ->
            let minusOne = (2.0 ** w) - 1.0 |> uint32

            if num = minusOne then
                put 0 <| Alg exp
            else
                let numExp = (Data { Dat = (Word num); Width = w }).toExp
                put 0 <| Alg(BinaryExp(exp, BitAndOp, numExp))
        | A, B ->
            let aExp, bExp = A.toExp, B.toExp
            put 0 <| Alg(BinaryExp(aExp, BitAndOp, bExp))
    | NbitsNot numberOfBits ->
        match ins 0 with
        | Data A ->
            let outDat =
                match A.Dat with
                | BigWord a ->
                    // failwithf $"TODO: fable does not support op_OnesComplement function"
                    // BigWord (System.Numerics.BigInteger.op_OnesComplement a)  FIX: 2^n-1-a
                    let w = A.Width
                    // (bigint^w)
                    let (minusOne: bigint) = (1I <<< w) - 1I
                    BigWord(minusOne - a)
                | Word a -> Word(~~~a)

            put 0 <| Data { A with Dat = outDat }
        | Alg exp -> put 0 <| Alg(algNot exp)

    | NbitSpreader numberOfBits ->
        match ins 0 with
        | Data A ->
            let outDat =
                match (convertFastDataToInt A) with
                | 0u -> convertIntToFastData numberOfBits 0u
                | 1u ->
                    match numberOfBits with
                    | n when n <= 32 -> convertIntToFastData numberOfBits ((1u <<< numberOfBits) - 1u)
                    | _ -> convertBigintToFastData numberOfBits ((1I <<< numberOfBits) - 1I)
                | _ -> failwithf $"Can't happen"

            put 0 <| Data outDat
        | _ ->
            let err =
                { ErrType = AlgInpNotAllowed "The chosen set of Algebraic inputs results in algebra being passed to the
                    input port of a Bit-Spreader. Only values can be passed to this port."
                  InDependency = Some(comp.FullName)
                  ComponentsAffected = [ comp.cId ]
                  ConnectionsAffected = [] }
            // Algebra at bit spreader is not supported
            raise (AlgebraNotImplemented err)
    | Custom c ->
        // Custom components are removed
        failwithf "what? Custom components are removed before the fast simulation: %A" c
    | MergeWires ->
        //let bits0, bits1 = ins 0, ins 1
        match ins 0, ins 1 with
        | Data bits0, Data bits1 ->
            // Little endian, bits coming from the top wire are the least
            // significant.
            let wOut = bits0.Width + bits1.Width

            let outBits =
                if wOut <= 32 then
                    match bits0.Dat, bits1.Dat with
                    | Word b0, Word b1 ->
                        (b1 <<< bits0.Width) ||| b0
                        |> (fun n -> convertIntToFastData wOut n)
                    | _ -> failwithf $"inconsistent merge widths: {bits0},{bits1}"
                else
                    let b0 = convertFastDataToBigint bits0
                    let b1 = convertFastDataToBigint bits1
                    (b1 <<< bits0.Width) ||| b0
                    |> convertBigintToFastData wOut

            put 0 <| Data outBits

        | Alg(AppendExp exps0), Alg(AppendExp exps1) ->
            let newExp = exps1 @ exps0 |> foldAppends |> AppendExp
            put 0 <| Alg newExp

        | Alg(AppendExp exps0), fd1 ->
            let exp1 = fd1.toExp
            let newExp = exp1 :: exps0 |> foldAppends |> AppendExp
            put 0 <| Alg newExp

        | fd0, Alg(AppendExp exps1) ->
            let exp0 = fd0.toExp
            let newExp = exps1 @ [ exp0 ] |> foldAppends |> AppendExp
            put 0 <| Alg newExp

        | fd0, fd1 ->
            let exp0, exp1 = fd0.toExp, fd1.toExp
            let newExp = [ exp1; exp0 ] |> foldAppends |> AppendExp
            put 0 <| Alg newExp
    //add MergeN
    
    | MergeN n -> 
        let fDataL = List.map ins [0..n-1]
        let isAllData = 
            fDataL 
            |> List.forall (fun fdata -> 
                match fdata with 
                | Data _ -> true
                | _ -> false)
        match isAllData with
        | true -> 
            let (bitsList: FastData List) = List.map (fun n -> 
                match n with 
                | Data bits -> bits 
                | _ -> failwithf $"Cannot simulate truth tables with merge inputs part-algebraic") fDataL
            let wOut = List.sumBy (fun (bits: FastData) -> bits.Width) bitsList
            
            let outBits= 
                if wOut <= 32 then
                    let inBitsAndWidths =
                        bitsList
                        |> List.map (fun bits ->
                            match bits.Dat with
                            | Word b -> b, bits.Width
                            | _ -> failwithf $"inconsistent MergeN data for Truth Tables - cannot mix > 32 bit and < 32 bit") 
                    let mergeTwoValues (lSWidth: int) (mSValue: uint32) (lSValue: uint32) =
                        (mSValue <<< lSWidth) ||| lSValue
                    ((0u,0), inBitsAndWidths)
                    ||> List.fold (fun (lSBits,lSWidth) (inBits,inWidth) ->
                            mergeTwoValues lSWidth inBits lSBits, inWidth + lSWidth)
                    |> (fun (n,_) -> convertIntToFastData wOut n)
                else // identical to int32 case except use bigints
                    let inBitsAndWidths =
                        bitsList
                        |> List.map (fun bits ->
                            match bits.Dat with
                            | BigWord b -> b, bits.Width
                            | _ -> failwithf $"inconsistent MergeN data for Truth Tables - cannot mix > 32 bit and < 32 bit") 
                    let mergeTwoValues (lSWidth: int) (mSValue: bigint) (lSValue: bigint) =
                        (mSValue <<< lSWidth) ||| lSValue
                    ((0I,0), inBitsAndWidths)
                    ||> List.fold (fun (lSBits,lSWidth) (inBits,inWidth) ->
                            mergeTwoValues lSWidth inBits lSBits, inWidth + lSWidth)
                    |> (fun (n,_) -> convertBigintToFastData wOut n)
            put 0 <| Data outBits
        | false ->
            ([], fDataL)
            ||> List.fold (fun acc data -> 
                    match data with 
                    | Alg(AppendExp exps) -> exps @ acc
                    | fd -> (fd.toExp)::acc) 
            |> foldAppends
            |> AppendExp
            |> Alg
            |> put 0
    
    | SplitWire topWireWidth ->
        let fd = ins 0
#if ASSERTS
        assertThat (fd.Width >= topWireWidth + 1)
        <| sprintf "SplitWire received too few bits: expected at least %d but got %d" (topWireWidth + 1) fd.Width
#endif
        match fd with
        | Data bits ->
            let bits0, bits1 =
                let bits1 = getBits (bits.Width - 1) topWireWidth bits
                let bits0 = getBits (topWireWidth - 1) 0 bits
                bits0, bits1
            // Little endian, bits leaving from the top wire are the least
            // significant.
            put 0 <| Data bits0
            put 1 <| Data bits1

        | Alg(UnaryExp(BitRangeOp(l, u), exp)) ->
            let exp1 = UnaryExp(BitRangeOp(l + topWireWidth, u), exp)
            let exp0 = UnaryExp(BitRangeOp(l, l + topWireWidth - 1), exp)
            put 0 <| Alg exp0
            put 1 <| Alg exp1

        | Alg(UnaryExp(NotOp, exp)) ->
            let w = getAlgExpWidth (UnaryExp(NotOp, exp))
            let exp1 = UnaryExp(BitRangeOp(topWireWidth, w - 1), exp)
            let exp0 = UnaryExp(BitRangeOp(0, topWireWidth - 1), exp)
            put 0 <| Alg(UnaryExp(NotOp, exp0))
            put 1 <| Alg(UnaryExp(NotOp, exp1))

        | Alg exp ->
            let w = getAlgExpWidth exp
            let exp1 = UnaryExp(BitRangeOp(topWireWidth, w - 1), exp)
            let exp0 = UnaryExp(BitRangeOp(0, topWireWidth - 1), exp)
            put 0 <| Alg exp0
            put 1 <| Alg exp1

    | SplitN (n, widths, lsbs) -> 
        let fd = ins 0
        List.zip widths lsbs
        |> List.iteri (fun i (width,lsb) -> makeSplitOutput fd i width lsb )
#if ASSERTS
        assertThat (fd.Width >= topWireWidth + 1)
        <| sprintf "SplitWire received too few bits: expected at least %d but got %d" (topWireWidth + 1) fd.Width
#endif
        match fd with
        | Data bits ->
            List.iteri2 (fun i width lsb ->
                getBits (lsb+width-1) lsb bits 
                |> Data
                |> put i
                ) widths lsbs

        | Alg(UnaryExp(BitRangeOp(l, u), exp)) ->
            List.iteri2 (fun i width lsb ->
                UnaryExp(BitRangeOp(l + lsb, l+lsb+width-1), exp)
                |> Alg
                |> put i
            ) widths lsbs

        | Alg(UnaryExp(NotOp, exp)) ->
            List.iteri2 (fun i width lsb ->
                let expi = UnaryExp(BitRangeOp(lsb, lsb+width-1), exp)
                Alg(UnaryExp(NotOp, expi))
                |> put i
            ) widths lsbs

        | Alg exp ->
            List.iteri2 (fun i width lsb ->
                UnaryExp(BitRangeOp(lsb, lsb+width-1), exp)
                |> Alg
                |> put i
            ) widths lsbs

    | DFF ->
        match insOld 0 with
        | Data bits ->
            let d = extractBitFData (Data bits) 1
            put 0 (packBitFData d)
        | _ ->
            let err =
                { ErrType = AlgInpNotAllowed "Algebraic Simulation not implemented for DFF."
                  InDependency = Some(comp.FullName)
                  ComponentsAffected = [ comp.cId ]
                  ConnectionsAffected = [] }

            raise (AlgebraNotImplemented err)
    | DFFE ->
        match insOld 0, insOld 1 with
        | Data bits0, Data bits1 ->
            let d, en = extractBitFData (Data bits0) 1, extractBitFData (Data bits1) 1

            if en = 1u then
                put 0 <| packBitFData d
            else
                put 0 (getLastCycleOut 0)
        | _ ->
            let err =
                { ErrType = AlgInpNotAllowed "The chosen set of Algebraic inputs results in algebra being passed to a
                    DFFE. Algebraic Simulation has not been implemented for this component."
                  InDependency = Some(comp.FullName)
                  ComponentsAffected = [ comp.cId ]
                  ConnectionsAffected = [] }

            raise (AlgebraNotImplemented err)
    | Register width ->
        let fd = insOld 0
#if ASSERTS
        assertThat (fd.Width = width)
        <| sprintf "Register received data with wrong width: expected %d but got %A" width fd.Width
#endif
        match fd with
        | Data bits -> put 0 <| Data bits
        | _ ->
            let err =
                { ErrType = AlgInpNotAllowed "The chosen set of Algebraic inputs results in algebra being passed to a
                    Register. Algebraic Simulation has not been implemented for this component."
                  InDependency = Some(comp.FullName)
                  ComponentsAffected = [ comp.cId ]
                  ConnectionsAffected = [] }

            raise (AlgebraNotImplemented err)

    | RegisterE width ->
        //let bits, enable = insOld 0, insOld 1
        match insOld 0, insOld 1 with
        | Data bits, Data enable ->
#if ASSERTS
            assertThat (bits.Width = width)
            <| sprintf "RegisterE received data with wrong width: expected %d but got %A" width bits.Width
#endif
            if (extractBitFData (Data enable) 1 = 1u) then
                put 0 <| Data bits
            else
                put 0 (getLastCycleOut 0)
        | _ ->
            let err =
                { ErrType = AlgInpNotAllowed "The chosen set of Algebraic inputs results in algebra being passed to a
                    RegisterE. Algebraic Simulation has not been implemented for this component."
                  InDependency = Some(comp.FullName)
                  ComponentsAffected = [ comp.cId ]
                  ConnectionsAffected = [] }

            raise (AlgebraNotImplemented err)
    | Counter width ->
        //let bits, enable = insOld 0, insOld 1
        match insOld 0, insOld 1, insOld 2 with
        | Data bits, Data load, Data enable ->
#if ASSERTS
            assertThat (bits.Width = width)
            <| sprintf "Counter received data with wrong width: expected %d but got %A" width bits.Width
#endif
            if
                (extractBitFData (Data enable) 1 = 1u)
                && (extractBitFData (Data load) 1 = 0u)
            then
                let lastOut = (getLastCycleOut 0)
                let n = lastOut.toFastData.GetBigInt + 1I
                let res = FastData.MakeFastData bits.Width n
                put 0 <| Data res
            elif
                (extractBitFData (Data enable) 1 = 1u)
                && (extractBitFData (Data load) 1 = 1u)
            then
                put 0 <| Data bits
            else
                put 0 (getLastCycleOut 0)
        | _ ->
            let err =
                { ErrType = AlgInpNotAllowed "The chosen set of Algebraic inputs results in algebra being passed to a
                    Counter. Algebraic Simulation has not been implemented for this component."
                  InDependency = Some(comp.FullName)
                  ComponentsAffected = [ comp.cId ]
                  ConnectionsAffected = [] }

            raise (AlgebraNotImplemented err)
    | CounterNoEnable width ->
        //let bits, enable = insOld 0, insOld 1
        match insOld 0, insOld 1 with
        | Data bits, Data load ->
#if ASSERTS
            assertThat (bits.Width = width)
            <| sprintf "Counter received data with wrong width: expected %d but got %A" width bits.Width
#endif
            if (extractBitFData (Data load) 1 = 0u) then
                let lastOut = (getLastCycleOut 0)
                let n = lastOut.toFastData.GetBigInt + 1I
                let res = FastData.MakeFastData bits.Width n
                put 0 <| Data res
            else
                put 0 <| Data bits

        | _ ->
            let err =
                { ErrType = AlgInpNotAllowed "The chosen set of Algebraic inputs results in algebra being passed to a
                    Counter. Algebraic Simulation has not been implemented for this component."
                  InDependency = Some(comp.FullName)
                  ComponentsAffected = [ comp.cId ]
                  ConnectionsAffected = [] }

            raise (AlgebraNotImplemented err)
    | CounterNoLoad width ->
        //let bits, enable = insOld 0, insOld 1
        match insOld 0 with
        | Data enable ->
            if (extractBitFData (Data enable) 1 = 1u) then
                let lastOut = (getLastCycleOut 0)
                let n = lastOut.toFastData.GetBigInt + 1I
                let n' =
                    if n = (1I <<< width) then
                        0I
                    else
                        n
                let res = FastData.MakeFastData width n'
                put 0 <| Data res
            else
                put 0 (getLastCycleOut 0)
        | _ ->
            let err =
                { ErrType = AlgInpNotAllowed "The chosen set of Algebraic inputs results in algebra being passed to a
                    Counter. Algebraic Simulation has not been implemented for this component."
                  InDependency = Some(comp.FullName)
                  ComponentsAffected = [ comp.cId ]
                  ConnectionsAffected = [] }

            raise (AlgebraNotImplemented err)
    | CounterNoEnableLoad width ->
        let lastOut = (getLastCycleOut 0)
        let n = lastOut.toFastData.GetBigInt + 1I
        let n' =
            if n = (1I <<< width) then
                0I
            else
                n
        let res = FastData.MakeFastData width n'
        put 0 <| Data res
    | AsyncROM1 mem -> // Asynchronous ROM.
        let fd = ins 0
#if ASSERTS
        assertThat (fd.Width = mem.AddressWidth)
        <| sprintf "ROM received address with wrong width: expected %d but got %A" mem.AddressWidth fd
#endif
        match fd with
        | Data addr ->
            let outData = readMemoryFData mem (Data addr)
            put 0 outData
        | _ ->
            let err =
                { ErrType = AlgInpNotAllowed "The chosen set of Algebraic inputs results in algebra being passed to
                    AsyncRom. Algebraic Simulation has not been implemented for this component."
                  InDependency = Some(comp.FullName)
                  ComponentsAffected = [ comp.cId ]
                  ConnectionsAffected = [] }

            raise (AlgebraNotImplemented err)
    | ROM1 mem -> // Synchronous ROM.
        let fd = insOld 0
#if ASSERTS
        assertThat (fd.Width = mem.AddressWidth)
        <| sprintf "ROM received address with wrong width: expected %d but got %A" mem.AddressWidth fd
#endif
        match fd with
        | Data addr ->
            let outData = readMemoryFData mem (Data addr)
            put 0 outData
        | _ ->
            let err =
                { ErrType = AlgInpNotAllowed "The chosen set of Algebraic inputs results in algebra being passed to
                    ROM. Algebraic Simulation has not been implemented for this component."
                  InDependency = Some(comp.FullName)
                  ComponentsAffected = [ comp.cId ]
                  ConnectionsAffected = [] }

            raise (AlgebraNotImplemented err)
    | RAM1 memory ->
        let mem = getRamStateMemory numStep (simStepOld) comp.State memory

        match insOld 0, insOld 1 with
        | Data address, Data dataIn ->
#if ASSERTS
            assertThat (address.Width = mem.AddressWidth)
            <| sprintf "RAM received address with wrong width: expected %d but got %A" mem.AddressWidth address
#endif
#if ASSERTS
            assertThat (dataIn.Width = mem.WordWidth)
            <| sprintf "RAM received data-in with wrong width: expected %d but got %A" mem.WordWidth dataIn
#endif
            let write = extractBitFData (insOld 2) 1
            // If write flag is on, write the memory content.
            let mem, dataOut =
                match write with
                | 0u ->
                    // Read memory address and return memory unchanged.
                    mem, readMemoryFData mem (Data address)
                | 1u ->
                    // Update memory and return old content.
                    // NB - this was previously new content - but that is inconsistent and less useful.
                    writeMemory mem address dataIn, readMemoryFData mem (Data address)
                | _ -> failwithf $"simulation error: invalid 1 bit write value {write}"

            putState (RamState mem)
            put 0 dataOut
        | _ ->
            let err =
                { ErrType = AlgInpNotAllowed "The chosen set of Algebraic inputs results in algebra being passed to
                    RAM. Algebraic Simulation has not been implemented for this component."
                  InDependency = Some(comp.FullName)
                  ComponentsAffected = [ comp.cId ]
                  ConnectionsAffected = [] }

            raise (AlgebraNotImplemented err)
    // AsyncRAM1 component must be evaluated twice. Once (first) as clocked component
    // to update state based on previous cycle. Then again as combinational component to update output
    //
    | AsyncRAM1 memory ->
        // Exception (just in case)
        let err =
            { ErrType = AlgInpNotAllowed "The chosen set of Algebraic inputs results in algebra being passed to
                    AsyncRam. Algebraic Simulation has not been implemented for this component."
              InDependency = Some(comp.FullName)
              ComponentsAffected = [ comp.cId ]
              ConnectionsAffected = [] }

        if isClockedReduction then
            // here we propagate the state to current timestep, doing a state change if need be.
            let mem = getRamStateMemory numStep (simStepOld) comp.State memory

            let address =
                match insOld 0 with
                | Data d -> d
                | Alg _ -> raise (AlgebraNotImplemented err)
#if ASSERTS
            assertThat (address.Width = mem.AddressWidth)
            <| sprintf "RAM received address with wrong width: expected %d but got %A" mem.AddressWidth address
#endif
            let dataIn =
                match insOld 1 with
                | Data d -> d
                | Alg _ -> raise (AlgebraNotImplemented err)
#if ASSERTS
            assertThat (dataIn.Width = mem.WordWidth)
            <| sprintf "RAM received data-in with wrong width: expected %d but got %A" mem.WordWidth dataIn
#endif
            let io2 =
                match insOld 2 with
                | Data d -> d
                | Alg _ -> raise (AlgebraNotImplemented err)

            let write = extractBitFData (insOld 2) 1
            // If write flag is on, write the memory content.
            let mem =
                match write with
                | 0u ->
                    // Read memory address and return memory unchanged.
                    mem
                | 1u ->
                    // Update memory and return old content.
                    // NB - this was previously new content - but that is inconsistent and less useful.
                    writeMemory mem address dataIn
                | _ -> failwithf $"simulation error: invalid 1 bit write value {write}"

            putState (RamState mem)
        else
            // here we do the async read using current step address and state
            // note that state will have been written for this step previously by clocked invocation of this component
            let mem = getRamStateMemory (numStep + 1) simStep comp.State memory

            let address =
                match ins 0 with
                | Data d -> d
                | Alg _ -> raise (AlgebraNotImplemented err)

            let data = readMemoryFData mem (Data address)
            //printfn $"reading {data} from addr={address} with state = {RamState mem}"
            put 0 data
    | _ -> failwithf $"simulation error: deprecated component type {componentType}"


