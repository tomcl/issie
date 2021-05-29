module Fast
open Fable.Core
open CommonTypes
open Helpers
open SimulatorTypes
open SynchronousUtils
open NumberHelpers

//------------------------------------------------------------------------------//
//-----------------------------Fast Simulation----------------------------------//
//------------------------------------------------------------------------------//



//------------------------------------------------------------------------------//
// ----------Subfunctions used by fastReduce to evaluate a component-----------//
//------------------------------------------------------------------------------//

let inline assertThat cond msg = 
    if not cond
    then failwithf "what? assert failed: %s" msg

/// Assert that the FData only contain a single bit, and return such bit.
let inline extractBit (fd: FData) : uint32 =
#if ASSERTS
    assertThat (fd.Width = 1)
    <| sprintf "extractBit called with wireData: %A" fd
#endif
    match fd.Dat with | Word n -> n | BigWord _ -> failwithf $"Can't extract 1 bit from BigWord data {fd.Dat} of width {fd.Width}"

let inline packBit (bit: uint32) : FData = if bit = 0u then {Dat=Word 0u; Width = 1} else {Dat = Word 1u; Width = 1}


/// Read the content of the memory at the specified address.
let private readMemory (mem: Memory1) (address: FData) : FData =
    let intAddr = convertFastDataToInt64 address
    let outDataInt = Helpers.getMemData (int64 intAddr) mem
    convertInt64ToFastData mem.WordWidth outDataInt

/// Write the content of the memory at the specified address.
let private writeMemory (mem: Memory1) (address: FastData) (data: FastData) : Memory1 =
    let intAddr = int64 <| convertFastDataToInt64 address
    let intData = int64 <| convertFastDataToInt64 data

    { mem with
          Data = Map.add intAddr intData mem.Data }

let private getRamStateMemory numSteps step (state: StepArray<SimulationComponentState> option) memory : Memory1 =
    match state, numSteps with
    | _, 1 -> memory
    | Some arr, _ ->
        match arr.Step.[step] with
        | RamState memory -> memory
        | _ -> failwithf "What? getRamStateMemory called with invalid state"
    | _ -> failwithf "what? getRamStateMemory called with an invalid state: %A" state

let getRomStateMemory comp =
    match comp.FType with
    | ROM memory
    | AsyncROM memory -> memory
    | _ -> failwithf "What? getRomStateMemory called with invalid state"


let inline private bitNot bit = bit ^^^ 1u

let inline private bitAnd bit0 bit1 = bit0 &&& bit1


let inline private bitOr bit0 bit1 = bit0 ||| bit1


let inline private bitXor bit0 bit1 = bit0 ^^^ bit1


let inline private bitNand bit0 bit1 = bitAnd bit0 bit1 |> bitNot

let inline private bitNor bit0 bit1 = bitOr bit0 bit1 |> bitNot

let inline private bitXnor bit0 bit1 = bitXor bit0 bit1 |> bitNot




//---------------------------------------------------------------------------------------//
// ------------------------------MAIN COMPONENT SIMULATION FUNCTION----------------------//
//---------------------------------------------------------------------------------------//


/// Given a component, compute its outputs from its inputs, which must already be evaluated.
/// Outputs and inputs are both contained as time sequences in arrays. This function will calculate
/// simStep outputs from 9previously calculated) simStep outputs and clocked (simStep-1) outputs.
/// Memory has state separate from simStep-1 output, for this the state is recalculated.
let fastReduce (maxArraySize: int) (numStep: int) (comp: FastComponent) : Unit =
    let componentType = comp.FType

    //printfn "Reducing %A...%A %A (step %d)"  comp.FType comp.ShortId comp.FullName simStep
    let n = comp.InputLinks.Length

    let simStep = numStep % maxArraySize 
    let simStepOld = if simStep = 0 then maxArraySize - 1 else simStep - 1


    ///  get data feom input i of component
    let inline ins i =
#if ASSERTS
        assertThat (i < n) (sprintf "What? Invalid input port (%d:step%d) used by %s:%s (%A) reducer with %d Ins"
                                    i simStep comp.FullName comp.ShortId  componentType n)
#endif
        let fd = comp.InputLinks.[i].Step.[simStep]
        fd

    /// get last cycle data from output i (for clocked components)
    let inline getLastCycleOut n =
        let fd =
            match comp.OutputWidth.[n], numStep with
            | None, _ -> failwithf "Can't reduce %A (%A) because outputwidth is not known" comp.FullName comp.FType
            | Some w, 0 -> if w < 33 then {Dat=Word 0u; Width = w} else {Dat =BigWord (bigint 0); Width = w}
            | Some w, _ -> comp.Outputs.[n].Step.[simStepOld]
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

        let fd =
            comp.GetInput(simStepOld) (InputPortNumber i) 
        fd

    /// Write current step output data for output port 0
    let inline put0 fd =
        comp.PutOutput(simStep) (OutputPortNumber 0) fd

    /// Write current step output data for output port 1
    let inline put1 fd =
        comp.PutOutput(simStep) (OutputPortNumber 1) fd

    /// Write current step output data for output port 2
    let inline put2 fd =
        comp.PutOutput(simStep) (OutputPortNumber 2) fd
    
    /// Write current step output data for output port 3
    let inline put3 fd =
        comp.PutOutput(simStep) (OutputPortNumber 3) fd
    
    /// Write current step output data for output port 4
    let inline put4 fd =
        comp.PutOutput(simStep) (OutputPortNumber 4) fd

    /// Write current State (used only for RAMs, DFFs and registers use previous cycle output as state)
    let inline putState state =
        match comp.State with
        | None -> failwithf "Attempt to put state into component %s without state array" comp.FullName
        | Some stateArr -> stateArr.Step.[simStep] <- state

    let inline putW num w = comp.OutputWidth.[num] <- Some w

    /// implement a binary combinational operation
    let inline getBinaryGateReducer (op: uint32 ->uint32 -> uint32) : Unit =
        let bit0 = (ins 0).GetQUint32
        let bit1 = (ins 1).GetQUint32
        put0 <| {Width=1; Dat = Word (op bit1 bit0)}

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

    // reduce the component in this match
    match componentType with
    | ROM _ | RAM _ | AsyncROM _ -> 
        failwithf "What? Legacy RAM component types should never occur"
    | Input width ->
        if comp.Active then
            let bits = ins 0
            //printfn "Got input 0 = %A Links=<%A> len=%d" bits comp.InputLinks comp.InputLinks.Length
            checkWidth width bits
            //printfn "output array = %A" comp.Outputs
            put0 bits
    //printfn "Finished!"

    | Constant1 (width, cVal,_) | Constant (width,cVal)->
        put0
        <| convertInt64ToFastData width cVal
    | Output width ->
        let bits = ins 0
        //printfn "In output bits=%A, ins = %A" bits comp.InputLinks
        checkWidth width bits
        put0 bits
    | Viewer width ->
        let bits = ins 0
        //printfn "In output bits=%A, ins = %A" bits comp.InputLinks
        checkWidth width bits
        put0 bits

    | IOLabel ->
        let bits = ins 0
        //let bits = comp.InputLinks.[0].[simStep]
        //printfn "Reducing IOLabel %A" comp.SimComponent.Label
        put0 bits
    | Not ->
        let bit = extractBit (ins 0)
        put0 <| packBit (bitNot bit)
    | BusSelection (width, lsb) ->
        let bits = ins 0
#if ASSERTS
        assertThat
            (bits.Width >= width + lsb)
            (sprintf "Bus Selection received too few bits: expected at least %d but got %d" (width + lsb) bits.Width)
#endif
        let outBits = getBits (lsb + width - 1) lsb bits
        put0 outBits
    | BusCompare (width, compareVal) ->
        //printfn "Reducing compare %A" comp.SimComponent.Label
        let bits = ins 0
#if ASSERTS
        assertThat
            (bits.Width = width)
            ($"Bus Compare {comp.FullName} received wrong number of bits: expecting  {width} but got {bits.Width}")
#endif
        let inputNum = convertFastDataToInt bits

        let outNum : FData =
            if inputNum = compareVal then 1u else 0u
            |> packBit


        put0 outNum
    | And -> getBinaryGateReducer bitAnd
    | Or -> getBinaryGateReducer bitOr
    | Xor -> getBinaryGateReducer bitXor
    | Nand -> getBinaryGateReducer bitNand
    | Nor -> getBinaryGateReducer bitNor
    | Xnor -> getBinaryGateReducer bitXnor
    | Mux2 ->
        let bits0, bits1, bitSelect = ins 0, ins 1, ins 2
#if ASSERT
        assertThat (bits0.Width = bits1.Width)
        <| sprintf "Mux2 %s received two inputs with different widths: (%A) <> (%A)" comp.FullName bits0 bits1
#endif
        let out =
            if (extractBit bitSelect) = 0u then
                bits0
            else
                bits1

        put0 out
        putW 0 bits0.Width
    | Demux2 ->
        let bitsIn, bitSelect = ins 0, ins 1
        let zeros = convertIntToFastData bitsIn.Width 0u

        let out0, out1 =
            if (extractBit bitSelect) = 0u then
                bitsIn, zeros
            else
                zeros, bitsIn

        let w = bitsIn.Width
        put0 out0
        put1 out1
        putW 0 w
        putW 1 w
    | NbitsAdder numberOfBits ->
        let cin, A, B = ins 0, ins 1, ins 2

        let sum, cout =
            let cin = convertFastDataToInt cin
            let w = A.Width
            match A.Dat, B.Dat with
            | BigWord a, BigWord b ->
                let sumInt = if cin = 0u then a + b else a + b + bigint 1
                let sum = getBits (w-1) 0 {Dat = BigWord sumInt; Width = w}
                let cout = if (sumInt >>> w) = bigint 0 then 0u else 1u
                sum, packBit cout
            | Word a, Word b ->
                let sumInt = uint64 a + uint64 b + uint64 cin
                let cout = uint32 (sumInt >>> 32)
                let sum = convertIntToFastData w (uint32 sumInt)
                sum, packBit cout
            | a, b -> 
                failwithf $"Inconsistent inputs to NBitsAdder {comp.FullName} A={a},{A}; B={b},{B}"

        put0 sum
        put1 cout
    | NbitsXor numberOfBits ->
        let A, B = ins 0, ins 1
        let outDat =
            match A.Dat, B.Dat with
            | BigWord a, BigWord b ->
                BigWord (a ^^^ b)
            | Word a, Word b -> 
                Word (a ^^^ b)
            | a,b -> 
                failwithf $"Inconsistent inputs to NBitsXOr {comp.FullName} A={a},{A}; B={b},{B}"

        put0 {A with Dat = outDat}
    | Decode4 ->
        let select, data = ins 0, ins 1
        let selN = convertFastDataToInt select |> int
        let dataN = convertFastDataToInt data |> int

        let outs =
            [| 0 .. 3 |]
            |> Array.map
                (fun n ->
                    let outBit = if n = selN then dataN else 0
                    convertIntToFastData 1 (uint32 outBit))

        put0 outs.[0]
        put1 outs.[1]
        put2 outs.[2]
        put3 outs.[3]

    | Custom c ->
        // Custom components are removed
        failwithf "what? Custom components are removed before the fast simulation: %A" c
    | MergeWires ->
        let bits0, bits1 = ins 0, ins 1
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
        put0 outBits
        putW 0 outBits.Width
    | SplitWire topWireWidth ->
        let bits = ins 0
#if ASSERTS
        assertThat (bits.Width >= topWireWidth + 1)
        <| sprintf "SplitWire received too little bits: expected at least %d but got %d" (topWireWidth + 1) bits.Width
#endif
        let bits0, bits1 =
                let bits1 = getBits (bits.Width - 1) topWireWidth bits
                let bits0 = getBits (topWireWidth-1) 0 bits
                bits0, bits1

            
        // Little endian, bits leaving from the top wire are the least
        // significant.
        put0 bits0
        put1 bits1
        putW 1 bits1.Width
    | DFF ->
        let d = extractBit (insOld 0)
        put0 (packBit d)
    | DFFE ->
        let d, en =
            extractBit (insOld 0), extractBit (insOld 1)

        if en = 1u then
            put0 <| packBit d
        else
            put0 (getLastCycleOut 0)

    | Register width ->
        let bits = insOld 0
#if ASSERTS
        assertThat (bits.Width = width)
        <| sprintf "Register received data with wrong width: expected %d but got %A" width bits.Width
#endif
        put0 bits

    | RegisterE width ->
        let bits, enable = insOld 0, insOld 1
#if ASSERTS
        assertThat (bits.Width = width)
        <| sprintf "RegisterE received data with wrong width: expected %d but got %A" width bits.Width
#endif
        if (extractBit enable = 1u) then
            put0 bits
        else
            put0 (getLastCycleOut 0)
    | AsyncROM1 mem -> // Asynchronous ROM.
        let addr = ins 0
#if ASSERTS
        assertThat (addr.Width = mem.AddressWidth)
        <| sprintf "ROM received address with wrong width: expected %d but got %A" mem.AddressWidth addr
#endif
        let outData = readMemory mem addr
        put0 outData
    | ROM1 mem -> // Synchronous ROM.
        let addr = insOld 0
#if ASSERTS
        assertThat (addr.Width = mem.AddressWidth)
        <| sprintf "ROM received address with wrong width: expected %d but got %A" mem.AddressWidth addr
#endif
        let outData = readMemory mem addr
        put0 outData
    | RAM1 memory ->
        let mem =
            getRamStateMemory numStep (simStepOld) comp.State memory

        let address = insOld 0
#if ASSERTS
        assertThat (address.Width = mem.AddressWidth)
        <| sprintf "RAM received address with wrong width: expected %d but got %A" mem.AddressWidth address
#endif
        let dataIn = insOld 1
#if ASSERTS
        assertThat (dataIn.Width = mem.WordWidth)
        <| sprintf "RAM received data-in with wrong width: expected %d but got %A" mem.WordWidth dataIn
#endif
        let write = extractBit (insOld 2)
        // If write flag is on, write the memory content.
        let mem, dataOut =
            match write with
            | 0u ->
                // Read memory address and return memory unchanged.
                mem, readMemory mem address
            | 1u ->
                // Update memory and return old content.
                // NB - this was previously new content - but that is inconsistent and less useful.
                writeMemory mem address dataIn, readMemory mem address
            | _ -> failwithf $"simulation error: invalid 1 bit write value {write}"

        putState (RamState mem)
        put0 dataOut


//------------------------------------------------------------------------------//
//-----------------------------Fast Simulation----------------------------------//
//------------------------------------------------------------------------------//

let private makeStepArray (arr: 'T array) : StepArray<'T> = { Step = arr }

let private emptyGather =
    { Labels = Map.empty
      Simulation = Map.empty
      CustomInputCompLinks = Map.empty
      CustomOutputCompLinks = Map.empty
      CustomOutputLookup = Map.empty
      AllComps = Map.empty }

let emptyFastSimulation () =
    { ClockTick = 0
      MaxStepNum = -1 // this must be over-written
      MaxArraySize = 600 // must be larger than max number of wavesim clocks
      FGlobalInputComps = Array.empty
      FConstantComps = Array.empty
      FClockedComps = Array.empty
      FOrderedComps = Array.empty
      FIOActive = Map.empty
      FIOLinks = []
      FComps = Map.empty
      FSComps = Map.empty
      FCustomOutputCompLookup = Map.empty
      G = emptyGather }

let private getPathIds (cid, ap) =
    let rec getPath ap =
        match ap with
        | [] -> []
        | cid :: rest -> (cid, List.rev rest) :: getPath rest

    getPath (List.rev ap) |> List.rev


let private getFid (cid: ComponentId) (ap: ComponentId list) =
    let ff (ComponentId Id) = Id
    (cid, ap)


let private getPortNumbers (sc: SimulationComponent) =
    let ins,outs =
        match sc.Type with
        | Constant1 _ | Constant _ ->
            0,1
        | Input _
        | Output _
        | Viewer _ 
        | BusSelection _
        | BusCompare _
        | Not
        | DFF
        | Register _
        | IOLabel  
        | ROM1 _ 
        | AsyncROM1 _->
            1,1
        | MergeWires
        | NbitsXor _
        | RegisterE _
        | DFFE -> 
            2,1
        | SplitWire _ -> 
            1,2
        | Mux2 _ -> 
            3,1
        | NbitsAdder _ -> 
            3,2
        | RAM1 _ -> 
            2,1
        | Decode4 -> 
            2,4
        | Demux2 -> 
            2,2
        | Not | And | Or | Xor | Nand | Nor | Xnor -> 2,1
        | Custom _ -> failwithf "Custom components should not occur in fast simulation"
        | AsyncROM _ | RAM _ | ROM _ -> failwithf "legacy component type is not supported"

    ins, outs

let private getOutputWidths (sc: SimulationComponent) (wa: int option array) =

    let putW0 w = wa.[0] <- Some w
    let putW1 w = wa.[1] <- Some w
    let putW2 w = wa.[2] <- Some w
    let putW3 w = wa.[3] <- Some w

    match sc.Type with
    | ROM _ | RAM _ | AsyncROM _ -> 
        failwithf "What? Legacy RAM component types should never occur"
    | Input w
    | Output w
    | Viewer w
    | Register w
    | RegisterE w
    | SplitWire w
    | BusSelection (w, _)
    | Constant1 (w, _,_)
    | Constant (w,_)
    | NbitsXor w -> putW0 w
    | NbitsAdder w ->
        putW0 w
        putW1 1
    | Not
    | And
    | Or
    | Xor
    | Nand
    | Nor
    | Xnor
    | BusCompare _ -> putW0 1
    | AsyncROM1 mem
    | ROM1 mem
    | RAM1 mem -> putW0 mem.WordWidth
    | Custom _ -> ()
    | DFF
    | DFFE -> putW0 1
    | Decode4 ->
        putW0 1
        putW1 1
        putW2 1
        putW3 1
    | Demux2
    | Mux2
    | IOLabel
    | MergeWires -> ()

    wa



let private createFastComponent (numSteps: int) (sComp: SimulationComponent) (accessPath: ComponentId list) =
    let inPortNum, outPortNum = getPortNumbers sComp
    // dummy arrays wil be replaced by real ones when components are linked after being created
    let ins =
        [| 0 .. inPortNum - 1 |]
        |> Array.map (fun n -> Array.create (numSteps + 1) emptyFastData)
        |> Array.map makeStepArray

    let outs =
        [| 0 .. outPortNum - 1 |]
        |> Array.map (fun n -> Array.create (numSteps + 1) emptyFastData)
        |> Array.map makeStepArray

    let inps =
        let dat =
            match accessPath, sComp.Type with
            // top-level input needs special inputs because they can't be calculated
            | [], Input width -> List.replicate width Zero
            | _ -> []

        [| 0 .. inPortNum - 1 |]
        |> Array.map (fun i -> (Array.create (numSteps + 1) dat))

    let state =
        if couldBeSynchronousComponent sComp.Type then
            Some(Array.create numSteps NoState)
        else
            None

    let fId = getFid sComp.Id accessPath

    { OutputWidth = getOutputWidths sComp (Array.create outPortNum None)
      State = Option.map makeStepArray state
      SimComponent = sComp
      fId = fId
      cId = sComp.Id
      FType = sComp.Type
      AccessPath = accessPath
      Touched = false
      InputLinks = ins
      InputDrivers = Array.create inPortNum None
      Outputs = outs
      FullName = ""
      VerilogOutputName = Array.create outPortNum ""
      VerilogComponentName = ""
      Active =
          match sComp.Type with
          | IOLabel _ -> false
          | _ -> true }

/// extends the simulation data arrays of the component to allow more steps
let private extendFastComponent (numSteps: int) (fc: FastComponent) =
    let oldNumSteps = fc.Outputs.[0].Step.Length


    if numSteps + 1 <= oldNumSteps   then
        () // done
    else
        let extendArray (arr: StepArray<'T>) (dat: 'T) =
            let oldArr = arr.Step
            let a =
                Array.init
                    (numSteps + 1)
                    (fun i ->
                        if i < Array.length oldArr then
                            oldArr.[i]
                        else
                            dat)

            arr.Step <- a

        let inPortNum, outPortNum = getPortNumbers fc.SimComponent

        // Input inputs at top level are a special case not mapped to outputs.
        // They must be separately extended.
        match fc.FType, fc.AccessPath with
        | Input _, [] -> extendArray fc.InputLinks.[0] fc.InputLinks.[0].Step.[oldNumSteps - 1]
        | _ -> ()

        [| 0 .. outPortNum - 1 |]
        |> Array.iter (fun n -> extendArray fc.Outputs.[n] emptyFastData)

        Option.iter
            (fun (stateArr: StepArray<SimulationComponentState>) ->
                extendArray stateArr stateArr.Step.[oldNumSteps - 1])
            fc.State


/// extends the simulation data arrays of all components to allow more steps
/// also truncates fast simulation to prevent memory overuse.
let private extendFastSimulation (numSteps: int) (fs: FastSimulation) =
    if numSteps + 1 < fs.MaxStepNum then
        ()
    else
        [| fs.FOrderedComps
           fs.FConstantComps
           fs.FClockedComps
           fs.FGlobalInputComps |]
        |> Array.iter (Array.iter (extendFastComponent numSteps))

        fs.MaxStepNum <- numSteps


/// Create an initial gatherdata object with inputs, non-ordered components, simulationgraph, etc
/// This must explore graph recursively extracting all the initial information.
/// Custom components are scanned and links added, one for each input and output
let rec private gatherPhase (ap: ComponentId list) (numSteps: int) (graph: SimulationGraph) (gather: GatherData) : GatherData =
    (gather, graph)
    ||> Map.fold
            (fun gather cid comp ->
                // add this component
                let gather =
                    { gather with
                          AllComps = Map.add (cid, ap) (comp, ap) gather.AllComps
                          Labels = Map.add cid ((fun (ComponentLabel s) -> s) comp.Label) gather.Labels
                    }
                match comp.Type, comp.CustomSimulationGraph, ap with
                | Custom ct, Some csg, _ ->
                    let ap' = ap @ [ cid ]
                    let allComps = Map.toList csg |> List.map snd
                    /// Function making links to custom component input or output components
                    /// For those component types selected by compSelectFun (inputs or ouputs):
                    /// Link label and width (which will also be the custom comp port label and width)
                    /// to the Id of the relevant Input or output component.
                    let getCustomNameIdsOf compSelectFun =
                        allComps
                        |> List.filter (fun comp -> compSelectFun comp.Type)
                        |> List.map
                            (fun comp ->
                                (comp.Label,
                                 match comp.Type with
                                 | Input n -> n
                                 | Output n -> n
                                 | _ -> -1),
                                comp.Id)
                        |> Map.ofList

                    let outputs = getCustomNameIdsOf isOutput
                    /// maps Output component Id to corresponding Custom component Id & output port
                    let outLinks =
                        ct.OutputLabels
                        |> List.mapi
                            (fun i (lab, labOutWidth) ->
                                (outputs.[ComponentLabel lab, labOutWidth], ap'), ((cid, ap), OutputPortNumber i))
                        |> Map.ofList

                    let inputs = getCustomNameIdsOf isInput
                    /// maps Custom Component Id and input port number to corresponding Input Component Id
                    let inLinks =
                        ct.InputLabels
                        |> List.mapi
                            (fun i (lab, labOutWidth) ->
                                (((cid, ap), InputPortNumber i), (inputs.[ComponentLabel lab, labOutWidth], ap')))
                        |> Map.ofList


                    let g = gatherPhase ap' numSteps csg gather

                    { g with
                          Simulation = graph
                          CustomInputCompLinks = mapUnion inLinks g.CustomInputCompLinks
                          CustomOutputCompLinks = mapUnion outLinks g.CustomOutputCompLinks
                          AllComps =
                              mapUnion
                                  ((Map.toList
                                    >> List.map (fun (k, v) -> (k, ap), (v, ap))
                                    >> Map.ofList)
                                      graph)
                                  g.AllComps }
                | _ -> gather)
    |> (fun g ->
        { g with
              CustomOutputLookup = mapInverse g.CustomOutputCompLinks })

let private printGather (g: GatherData) =
    printfn "%d components" g.AllComps.Count

    Map.iter
        (fun (cid, ap) (comp: SimulationComponent, ap') -> printfn "%s: %A" (g.getFullName (cid, ap)) comp.Outputs)
        g.AllComps


let rec private createInitFastCompPhase (numSteps: int) (g: GatherData) (f: FastSimulation) =
    let makeFastComp cid =
        let comp, ap = g.AllComps.[cid]
        let fc = createFastComponent numSteps comp ap
        let fc = { fc with FullName = g.getFullName cid }

        let outs : StepArray<FData> array =
            (if isOutput comp.Type then
                 let outs =
                     [| Array.create (numSteps + 1) emptyFastData |> makeStepArray |]

                 outs
             else
                 fc.Outputs)

        //printfn "***Making %A with %d outs" comp.Type outs.Length
        { fc with Outputs = outs }

    let comps : Map<FComponentId, FastComponent> =
        (Map.empty, g.AllComps)
        ||> Map.fold
                (fun m cid (comp, ap) ->
                    if isCustom comp.Type then
                        m
                    else
                        Map.add (comp.Id, ap) (makeFastComp (comp.Id, ap)) m)

    let customOutLookup =
        g.CustomOutputCompLinks
        |> Map.toList
        |> List.map (fun (a, b) -> b, a)
        |> Map.ofList

    { f with
          FComps = comps
          MaxStepNum = numSteps
          FSComps = g.AllComps
          FCustomOutputCompLookup = customOutLookup }

/// has side effect of making IOLabels of same name (in the same graph) all use same output array
/// this means that an input to any one will produce an output on all, for no effort.
/// IOLabels without driven inputs that are thus not used are later on flagged inactive
/// they must not be reduced, and will not be included in the ordered component list
let private reLinkIOLabels (fs: FastSimulation) =
    // Go through all the components driven by IOLabels and link them from the active label
    // at this point exactly one out of every labelled set will be active, and contained in FIOActive
    fs.FIOLinks
    |> List.iter (fun ((fcDriven, InputPortNumber ipn), ioDriver) -> 
        let labKey = ioDriver.SimComponent.Label, ioDriver.AccessPath
        let fcActiveDriver = fs.FIOActive.[labKey]
        fcDriven.InputLinks.[ipn] <- fcActiveDriver.Outputs.[0]
        fcDriven.InputDrivers.[ipn] <- Some (fcActiveDriver.fId, OutputPortNumber 0)
        ioDriver.Outputs.[0] <- fcActiveDriver.Outputs.[0])

/// Use the Outputs links from the original SimulationComponents in gather to link together the data arrays
/// of the FastComponents.
/// InputLinks.[i] array is set equal to the correct driving Outputs array so that Input i reads the data reduced by the
/// correct output of the component that drives it.
/// The main work is dealing with custom components which represent whole design sheets with recursively defined component graphs
/// The custom component itself is not linked, and does not exist as a FastComponent. Instead its CustomSimulationGraph Input and Output components
/// are linked to the components that connect the corresponding inputs and outputs of the custom component.
let private linkFastComponents (g: GatherData) (f: FastSimulation) =
    let outer = List.rev >> List.tail >> List.rev
    let sComps = g.AllComps
    let fComps = f.FComps
    let getSComp (cid, ap) = fst sComps.[cid, ap]
    let apOf fid = fComps.[fid].AccessPath
    /// This function recursively propagates a component output across Custom component boundaries to find the
    ///
    let rec getLinks ((cid, ap): FComponentId) (opn: OutputPortNumber) (ipnOpt: InputPortNumber option) =
        let sComp = getSComp (cid, ap)
        //printfn "Getting links: %A %A %A" sComp.Type opn ipnOpt
        match isOutput sComp.Type, isCustom sComp.Type, ipnOpt with
        | true, _, None when apOf (cid, ap) = [] -> [||] // no links in this case from global output
        | true, _, None ->
            //printfn "checking 1:%A %A" (g.getFullName(cid,ap)) (Map.map (fun k v -> g.getFullName k) g.CustomOutputCompLinks)
            let cid, opn = g.CustomOutputCompLinks.[cid, ap]
#if ASSERTS
            assertThat (isCustom (fst sComps.[cid]).Type) "What? this should be a custom component output"
#endif
            getLinks cid opn None // go from inner output to CC output and recurse
        | false, true, Some ipn ->
            //printfn "checking 2:%A:IPN<%A>" (g.getFullName(cid,ap)) ipn
            //printfn "CustomInCompLinks=\n%A" (Map.map (fun (vfid,vipn) fid ->
            //sprintf "%A:%A -> %A\n" (g.getFullName vfid) vipn (g.getFullName fid) ) g.CustomInputCompLinks |> mapValues)
            //printfn "Done"
            [| g.CustomInputCompLinks.[(cid, ap), ipn], opn, InputPortNumber 0 |] // go from CC input to inner input: must be valid
        | _, false, Some ipn -> [| (cid, ap), opn, ipn |] // must be a valid link
        | false, _, None ->
            sComp.Outputs
            |> Map.toArray
            |> Array.filter (fun (opn', _) -> opn' = opn)
            |> Array.collect
                (fun (opn, lst) ->
                    lst
                    |> List.toArray
                    |> Array.collect (fun (cid, ipn) -> getLinks (cid, ap) opn (Some ipn)))

        | x -> failwithf "Unexpected link match: %A" x

    let mutable linkCheck : Map<(FComponentId * InputPortNumber), (FComponentId * OutputPortNumber)> = Map.empty

    f.FComps
    |> Map.iter
        (fun fDriverId fDriver ->
            let outs = fDriver.Outputs
            fDriver.Outputs
            |> Array.iteri
                (fun iOut _ ->
                    getLinks fDriverId (OutputPortNumber iOut) None
                    |> Array.map (fun (fid, _, ip) -> fid, iOut, ip)
                    |> Array.iter
                        (fun (fDrivenId, opn, (InputPortNumber ipn)) ->
                            let linked =
                                Map.tryFind (fDrivenId, InputPortNumber ipn) linkCheck

                            match linked with
                            | None -> ()
                            | Some (fid, opn) ->
                                failwithf "Multiple linkage: (previous driver was %A,%A)" (g.getFullName fid) opn

                            linkCheck <- Map.add (fDrivenId, InputPortNumber ipn) (fDriverId, OutputPortNumber opn) linkCheck
                            let fDriven = f.FComps.[fDrivenId]
                            let (_, ap) = fDrivenId

                            // we have a link from fDriver to fDriven

                            if isIOLabel fDriven.FType then
                                // fDriven is a driven label of a set of IOlabels
                                let labelKey = fDriven.SimComponent.Label, ap
                                if not (Map.containsKey labelKey f.FIOActive) then 
                                    // Make this then unique driven label in the fast simulation
                                    f.FIOActive <-  Map.add labelKey fDriven f.FIOActive
                                    fDriven.Active <- true

                            if isIOLabel fDriver.FType then
                                // we do not yet know which label will be active, so record all links from
                                // labels for later resolution
                                f.FIOLinks <- ((fDriven, InputPortNumber ipn), fDriver) :: f.FIOLinks
                            else
                                // if driver is not IO label make the link now
                                fDriven.InputLinks.[ipn] <- fDriver.Outputs.[opn]
                                fDriven.InputDrivers.[ipn] <- Some (fDriver.fId, OutputPortNumber opn)
                                )))
    reLinkIOLabels f
    f


/// Invalid data is used as default to determine which inputs have been given data when ordering components
let private isValidData (fd: FastData) = fd <> emptyFastData

/// True if the component is combinational
let private isComb (comp: FastComponent) =
    match comp.FType with
    | Input _ when comp.AccessPath = [] -> false
    | ct when couldBeSynchronousComponent ct -> false
    | _ -> true

/// True if all conditions are fulfiled for the component to be in the next batch to be reduced.
/// Used when ordering components.
let canBeReduced (fs: FastSimulation) (step: int) (fc: FastComponent) =
    isComb fc
    && not fc.Touched
    && fc.Active
    && Array.forall 
        (function 
            | (Some (fid: FComponentId,_)) -> fs.FComps.[fid].Touched 
            | None -> 
                let drivers = $"Input drivers: {fc.InputDrivers}"
                failwithf "Missing input link on %A\n\n. %s" fc.FullName drivers ) fc.InputDrivers


/// print function for debugging
let printComp (fs:FastSimulation) (step: int) (fc: FastComponent) =
    let attr =
        [ if isComb fc then "Co" else "  "
          if fc.Touched then "T" else "U"
          if fc.Active then "Act" else "Inact"
          "    "
          (fc.InputLinks
           |> Array.map (fun (arr: StepArray<FData>) -> arr.Step.Length > 0 && isValidData arr.Step.[step])
           |> Array.map
               (function
               | true -> "*"
               | false -> "X")
           |> String.concat "") ]
        |> String.concat ""

    let ins = (
        fc.InputDrivers 
        |> Array.map ( Option.map (fun (fid,_) -> 
            let fc = fs.FComps.[fid]
            fc.FullName, fc.ShortId)))


    sprintf "%25s %s %15s %A %A" fc.ShortId fc.FullName attr (canBeReduced fs step fc) ins

/// print function for debugging
let private printComps (step: int) (fs: FastSimulation) =

    fs.FComps
    |> mapValues
    |> Array.map (fun fComp -> printComp fs step fComp)
    |> String.concat "\n"
    |> printfn "COMPONENTS\n----------------\n%s\n---------------"


/// Create arrays of components in corrected format for efficient reduction
/// Combinational components are ordered: clokced, constant, global input components are
/// separated.
let private orderCombinationalComponents (numSteps: int) (fs: FastSimulation) : FastSimulation =

    let init fc = 
        fastReduce 0 0 fc
        fc.Touched <- true

    let initInput (fc: FastComponent) =
        //printfn "Init input..."
        fc.InputLinks.[0].Step
        |> Array.iteri
            (fun i _ -> fc.InputLinks.[0].Step.[i] <- (convertIntToFastData (Option.defaultValue 1 fc.OutputWidth.[0]) 0u))
        //printfn "Initialised input: %A" fc.InputLinks
        fastReduce fs.MaxArraySize 0 fc
        fc.Touched <- true

    let initClockedOuts (fc: FastComponent) =
        fc.Outputs
        |> Array.iteri
            (fun i vec ->
                fc.Touched <- true

                match fc.FType, fc.OutputWidth.[i] with
                | RAM1 mem, Some w ->
                    match fc.State with
                    | Some arr -> arr.Step.[0] <- RamState mem
                    | _ -> failwithf "Component %s does not have correct state vector" fc.FullName

                    let initD =
                        match Map.tryFind 0L mem.Data with
                        | Some n -> convertInt64ToFastData w n
                        | _ -> convertIntToFastData w 0u
                    // change simulation semantics to output 0 in cycle 0
                    vec.Step.[0] <- convertIntToFastData w 0u
                | RAM1 _, _ ->
                    failwithf "What? Bad initial values for RAM %s output %d state <%A>" fc.FullName i fc.FType
                | _, Some w -> vec.Step.[0] <- convertIntToFastData w 0u
                | _ -> failwithf "What? Can't find width for %s output %d" fc.FullName i)
    /// print function for debugging
    let pp (fL: FastComponent array) =
        Array.map (fun fc -> sprintf "%A (%A)" fc.FullName fc.FType) fL
        |> String.concat ","
    //printComps 0 fs
    //printfn "Ordering %d clocked outputs: " fs.FClockedComps.Length
    fs.FClockedComps |> Array.iter initClockedOuts
    //printfn "Ordering %d constants" fs.FConstantComps.Length
    fs.FConstantComps |> Array.iter init
    //printfn "Ordering %d global inputs" fs.FGlobalInputComps.Length
    fs.FGlobalInputComps |> Array.iter initInput
    //printComps 0 fs
    //printfn "Setup done..."
    //printfn "Constants: %A\nClocked: %A\nInputs:%A\n" (pp fs.FConstantComps) (pp fs.FClockedComps) (pp fs.FGlobalInputComps)
    let mutable orderedComps : FastComponent list = fs.FConstantComps |> Array.toList
    let fComps = mapValues fs.FComps
    //printfn "%A" (fComps |> Array.map (fun comp -> comp.SimComponent.Label))
    let mutable nextBatch = Array.filter (canBeReduced fs 0) fComps
    //printfn "Loop init done"
    printfn
        "%d constant, %d input, %d clocked, %d read to reduce, from %d"
        fs.FConstantComps.Length
        fs.FGlobalInputComps.Length
        fs.FClockedComps.Length
        nextBatch.Length
        fs.FComps.Count

    while nextBatch.Length <> 0 do
        //printf "Adding %d combinational components %A" nextBatch.Length (pp nextBatch)
        nextBatch
        |> Array.iter
            (fun fc ->
                if (not fc.Touched) then
                    fastReduce fs.MaxArraySize 0 fc
                    orderedComps <- fc :: orderedComps
                    fc.Touched <- true)
        //printfn "Total is now %d" orderedComps.Length
        //printComps 0 fs
        // work out new components that can still be added
        nextBatch <- Array.filter (canBeReduced fs 0) (mapValues fs.FComps)

    let orderedSet =
        orderedComps
        |> List.toArray
        |> Array.map (fun co -> co.fId)
        |> Set
    /// this is the input set of all (fast) components.
    /// it corresponds to thise in SimulationGraph except
    /// there are no custom components
    let allSet =
        mapValues fs.FComps
        |> Array.map (fun co -> co.fId)
        |> Set

    /// These components will never be ordered
    let fixedSet =
        [ fs.FClockedComps
          fs.FGlobalInputComps 
          fs.FConstantComps]
        |> Array.concat
        |> Array.map (fun fc -> fc.fId)
        |> Set

    let notOrdered =
        Set.difference allSet (Set.union orderedSet fixedSet)
    //printfn "Ordering finished\n"

    /// should be empty
    let badComps =
        notOrdered
        |> Set.toList
        |> List.map (fun fId -> fs.FComps.[fId])
        |> List.filter
            (function
            | { Active = a } -> a)

    badComps
    |> List.iter
        (fun fc ->
            printfn
                "\n-----------------\n%A: inputs=%A, touched=%A, canbereduced=%A"
                fc.FullName
                fc.InputLinks
                fc.Touched
                (canBeReduced fs 0 fc)

            printfn
                "%A: inputvalid=%A\n--------------\n"
                fc.FullName
                (Array.map
                    (fun (arr: StepArray<FData>) -> arr.Step.Length > 5 && isValidData arr.Step.[0])
                    fc.InputLinks))
#if ASSERTS
    assertThat
        (badComps.Length = 0)
        (sprintf "Components not linked: %A\n" (badComps |> List.map (fun fc -> fc.FullName)))
#endif

    { fs with
          FOrderedComps = orderedComps |> Array.ofList |> Array.rev }

/// Check all the active FastComponents to ensure everything is valid
/// Use data from initialisation to write any not-yet-written component output widths
let checkAndValidate (fs:FastSimulation) =
    let activeComps = 
        fs.FComps 
        |> mapValues
        |> Array.filter (fun fc -> fc.Active)
    let inSimulationComps =
        [|
            fs.FClockedComps
            fs.FGlobalInputComps
            fs.FOrderedComps
        |] |> Array.concat
    if (activeComps.Length <> inSimulationComps.Length) then
            printf "Validation problem: %d active components, %d components in simulation"
                   activeComps.Length
                   inSimulationComps.Length
            inSimulationComps
            |> Array.iter (fun fc -> printfn "Simulation: %s\n" (printComp fs 0 fc))
            fs.FComps
            |> Map.iter (fun fid fc -> printfn "FComps: %s\n" (printComp fs 0 fc))

            failwithf "What? this should never happen..."

    // check and add (if necessary) output widths
    activeComps
    |> Array.iter ( fun fc ->
        fc.OutputWidth
        |> Array.iteri ( fun i opn ->
            let data = fc.Outputs.[i].Step.[0]
            match data.Width, fc.OutputWidth.[i] with
            | n, Some m when n <> m ->
                failwithf "Inconsistent simulation data %A data found on signal output width %d from %s:%d" data m fc.FullName i
            | 0, _ ->
                failwithf "Unexpected output data %A found on initialised component %s:%d" data fc.FullName i
            | n, None ->
                fc.OutputWidth.[i] <- Some n
            | _ -> () // Ok in this case
        ))
    fs
    



/// Create a fast simulation data structure, with all necessary arrays, and components
/// ordered for evaluation.
/// This function also creates the reducer functions for each component
/// similar to the reducer builder in Builder, but with inputs and outputs using the FastSimulation
/// mutable arrays
let buildFastSimulation (numberOfSteps: int) (graph: SimulationGraph) : FastSimulation =
    let gather =
        gatherPhase [] numberOfSteps graph (emptyGather)
    //printGather gather
    let fs =
        createInitFastCompPhase numberOfSteps gather (emptyFastSimulation ())
    //printfn "Fast components:"
    //(mapKeys fs.FComps) |> Array.iter (fun x -> printfn "%s" (gather.getFullName x))
    let fs = linkFastComponents gather fs
    //printfn "Linking finished"
    let getArrayOf pred fComps =
        fComps
        |> Map.filter (fun cid comp -> pred comp)
        |> Map.toArray
        |> Array.map snd

    { fs with
          FGlobalInputComps =
              fs.FComps
              |> getArrayOf (fun fc -> isInput fc.FType && fc.AccessPath = [])
          FConstantComps =
              fs.FComps
              |> getArrayOf
                  (fun fc ->
                      match fc.FType with
                      | Constant1 _ -> true
                      | _ -> false)
          FClockedComps =
              fs.FComps
              |> getArrayOf (fun fc -> couldBeSynchronousComponent fc.FType)
          FOrderedComps = Array.empty
          FSComps = gather.AllComps
          G = gather }
    |> orderCombinationalComponents numberOfSteps
    |> checkAndValidate


/// sets up default no-change input values for the next step
let private propagateInputsFromLastStep (step: int) (fastSim: FastSimulation) =
    if step > 0 then
        fastSim.FGlobalInputComps
        |> Array.iter
            (fun fc ->
                let vec = fc.Outputs.[0]
                vec.Step.[step] <- vec.Step.[step - 1])

/// advance the simulation one step
let private stepSimulation (fs: FastSimulation) =
    let index = (fs.ClockTick + 1) % fs.MaxArraySize
    propagateInputsFromLastStep index fs
    Array.iter
        (fastReduce fs.MaxArraySize index)
        (Array.concat [ fs.FClockedComps
                        fs.FOrderedComps ])

    fs.ClockTick <- fs.ClockTick + 1

/// sets the mutable simulation data for a given input at a given time step
let private setSimulationInput (cid: ComponentId) (fd: FData) (step: int) (fs: FastSimulation) =
    match Map.tryFind (cid, []) fs.FComps with
    | Some fc -> fc.Outputs.[0].Step.[step % fs.MaxArraySize] <- fd
    | None -> failwithf "Can't find %A in FastSim" cid



/// Re-evaluates the combinational logic for the given timestep - used if a combinational
/// input has changed
let private runCombinationalLogic (step: int) (fastSim: FastSimulation) =
    fastSim.FOrderedComps
    |> Array.iter (fastReduce fastSim.MaxArraySize (step % fastSim.MaxArraySize))

/// Change an input and make simulation correct. N.B. step must be the latest
/// time-step since future steps are not rerun (TODO: perhaps they should be!)
let changeInput (cid: ComponentId) (wd: WireData) (step: int) (fastSim: FastSimulation) =
    let fd = (wd |> convertWireDataToInt |> convertInt64ToFastData wd.Length)
    setSimulationInput cid fd step fastSim
    printfn $"Changing {fastSim.FComps.[cid,[]].FullName} to {fd}"
    runCombinationalLogic step fastSim

let extractStatefulComponents (step: int) (fastSim: FastSimulation) =
    fastSim.FClockedComps
    |> Array.collect
        (fun fc ->
            match fc.AccessPath with
            | [] ->
                match fc.FType with
                | DFF _
                | DFFE _
                | Register _
                | RegisterE _ -> [| fc, RegisterState fc.Outputs.[0].Step.[step % fastSim.MaxArraySize] |]
                | ROM1 state -> [| fc, RamState state |]
                | RAM1 _ ->
                    match fc.State
                          |> Option.map (fun state -> state.Step.[step % fastSim.MaxArraySize]) with
                    | None -> failwithf "Missing RAM state for step %d of %s" step fc.FullName
                    | Some memState -> [| fc, memState |]
                | _ -> failwithf "Unsupported state extraction from clocked component type %s %A" fc.FullName fc.FType
            | _ -> [||])


/// Run an existing fast simulation up to the given number of steps. This function will mutate the write-once data arrays
/// of simulation data and only simulate the new steps needed, so it may return immediately doing no work.
/// If the simulation data arrays are not large enough they are extended up to a limit. After that, they act as a circular buffer.
let runFastSimulation (numberOfSteps: int) (fs: FastSimulation) : Unit =

        let simStartTime = getTimeMs()
        let stepsToDo = float (numberOfSteps - fs.ClockTick)
        let numComponents = float fs.FComps.Count
   
        if numberOfSteps > fs.MaxStepNum then
            if fs.MaxStepNum < fs.MaxArraySize then
                let newMaxNum =
                    min
                        fs.MaxArraySize
                        (numberOfSteps + max 50 (int (float numberOfSteps * 1.5)))

                //printfn $"In Tick {fs.ClockTick} Creating simulation array length of {newMaxNum} steps" 
                extendFastSimulation newMaxNum fs

        let start = fs.ClockTick + 1

        [ start .. numberOfSteps ]
        |> List.iter
            (fun n ->
                //if n % (100 * (int (1. + 3000. / numComponents))) = 0 then printfn "Step %d" n
                stepSimulation fs)
        let sTime = getTimeMs() - simStartTime
        if sTime > 50. then
            printfn $"Simulation speed: {numComponents*stepsToDo/sTime} Component-Steps/ms ({int stepsToDo} steps, {int numComponents} components)"

/// Run a fast simulation for a given number of steps building it from the graph
let runSimulationZeroInputs (steps: int) (graph: SimulationGraph) : FastSimulation =
    let fs = buildFastSimulation steps graph
    runFastSimulation steps fs
    fs

/// Look up a simulation (not a FastSimulation) component or return None.
let rec findSimulationComponentOpt ((cid, ap): ComponentId * ComponentId list) (graph: SimulationGraph) =
    match ap with
    | [] -> Map.tryFind cid graph
    | customCompId :: ap' ->
        Map.tryFind customCompId graph
        |> Option.bind
            (fun graph ->
                graph.CustomSimulationGraph
                |> Option.bind (fun graph -> findSimulationComponentOpt (cid, ap') graph))

/// Look up  a simulation component (not a FastComponent)
let findSimulationComponent ((cid, ap): ComponentId * ComponentId list) (sd: SimulationData) =
    let fs = sd.FastSim
    let graph = sd.Graph

    match findSimulationComponentOpt (cid, ap) graph with
    | None -> failwithf "What? Can't find component %A in SimulationData" fs.FComps.[cid, ap].FullName
    | Some sComp -> sComp



/// return output port data from simulation
let rec extractFastSimulationOutput
    (fs: FastSimulation)
    (step: int)
    ((cid, ap): ComponentId * ComponentId list)
    (opn: OutputPortNumber) : WireData
    =
    let (OutputPortNumber n) = opn

    match Map.tryFind (cid, ap) fs.FComps with
    | Some fc ->
        match Array.tryItem (step % fs.MaxArraySize) fc.Outputs.[n].Step with
        | None -> failwithf $"What? extracting output {n} in step {step} from {fc.FullName} failed with clockTick={fs.ClockTick}"
        | Some fd -> fd
        |> (fun fd -> fd |> convertFastDataToInt64 |> int64 |>  convertIntToWireData fd.Width)
    | None ->
        /// if it is a custom component output extract from the corresponding Output FastComponent
        match Map.tryFind ((cid, ap), opn) fs.G.CustomOutputLookup with
        | Some (cid, ap) -> extractFastSimulationOutput fs step (cid, ap) (OutputPortNumber 0)
        | None -> failwithf "What? extracting component data failed - can't find component from id"

/// return state data from simulation
let rec extractFastSimulationState
    (fs: FastSimulation)
    (step: int)
    ((cid, ap): ComponentId * ComponentId list) =

    match Map.tryFind (cid, ap) fs.FComps with
    | Some fc ->
        match fc.State with
        | None -> failwithf "What? extracting State in step %d from %s failed" step fc.FullName
        | Some stepArr ->
            match Array.tryItem (step% fs.MaxArraySize) stepArr.Step with
            | Some state -> state
            | None ->
                failwithf $"What? Can't extract state in step {step} from {fc.FullName}"
    | None -> 
        failwithf $"What? Can't find fast component {(cid,ap)}"



/// Extract top-level inputs or outputs with names and wire widths. Used by legacy code.
let extractFastSimulationIOs
    (simIOs: SimulationIO list)
    (simulationData: SimulationData)
    : (SimulationIO * WireData) list =
    let fs = simulationData.FastSim
    let inputs = simulationData.Inputs

    simIOs
    |> List.map
        (fun ((cid, label, width) as io) ->
            let wd = extractFastSimulationOutput fs simulationData.ClockTickNumber (cid, []) (OutputPortNumber 0)
            //printfn $"Extrcating: {io} --- {wd}"
            io, wd)

let getFLabel (fs:FastSimulation) (fId:FComponentId) =
    let fc = fs.FComps.[fId]
    let (ComponentLabel name) = fc.SimComponent.Label
    name, fc.FullName

let extractFastSimulationWidth (fs:FastSimulation)   (fid: FComponentId) (opn: OutputPortNumber) =
    let (OutputPortNumber n) = opn
    fs.FComps.[fid].OutputWidth.[n]
    




/// Extract all Viewer components with names and wire widths. Used by legacy code.
let extractViewers
    (simulationData: SimulationData)
    : ((string*string) * int * WireData) list =
    let fs = simulationData.FastSim

    let comps = 
        simulationData.FastSim.FComps
        |> Map.map (fun fid fc -> fc.FType)
        |> mapValues

    let viewers = 
        simulationData.FastSim.FComps
        |> Map.filter (fun fid fc -> match fc.FType with |Viewer _ -> true | _ -> false)
    viewers
    |> Map.toList
    |> List.map
        (fun (fid,fc) ->
            let width = Option.get fc.OutputWidth.[0]
            getFLabel fs fid, width, extractFastSimulationOutput fs simulationData.ClockTickNumber fid (OutputPortNumber 0))
