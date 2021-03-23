module Fast
open Fable.Core
open CommonTypes
open Helpers
open SimulatorTypes
open SynchronousUtils
open NumberHelpers
    

[<Emit("typeof $0")>]
let jsType (var: obj) : unit = jsNative

//----------------------------------------------------------------------------------------------//
//--------------------------------Fast Digital Bus Data Type------------------------------------//
//----------------------------------------------------------------------------------------------//
// data is stored differently according to its buswidth.
// We use all three options for efficiency
// Bit is more efficient than word for known boolean ops but it can be normalised to Word 
// to make implementation of multiple bit components (that may carry one bit) simpler.
// BigWord is needed for > 32 bits, and much less efficient for < 32 bits.
type FastData =
    | Bit of uint32 // must be 0 or 1, allows bitwise logical operators
    | Word of dat:uint32 * width:int
    | BigWord of dat:bigint * width:int with
    /// convert to form where single Bit is turned into Word equivalent
    member inline this.Normalise = 
        match this with 
        | Bit n -> Word(n, 1) 
        | BigWord(x, n) when n <= 32 -> Word(uint32 x, n)
        | x -> x
    /// return width of the data
    member inline this.Width = 
        match this with 
        | Bit _ -> 1 
        | Word (_,n) -> n 
        | BigWord(_,n) -> n
    /// return Some 0 or Some 1 as the single bit, or None if not a single bit
    member inline this.GetBitAsInt = // not possible if too large
        match this with 
        | Bit n -> Some n 
        | Word(n, 1) -> Some n 
        | _ -> None
    member inline this.GetBigInt = // always possible
        match this with
        | Bit n -> bigint n
        | Word(n,_) -> bigint n
        | BigWord(n,_) -> n
    /// return Some uint32 representing data if possible else None
    member inline this.GetUint32 = // not possible if too large
        match this with
        | Bit n -> Some n
        | Word(n,w) -> Some n
        | BigWord(n, w) when w <= 32 -> Some (uint32 n)
        | _ -> None
    
let fastBit (n: uint32) =
    assertThat (n < 2u) (sprintf "Can't convert %d to a single bit FastData" n)
    Bit n

let rec bitsToInt (lst:Bit list) =
    match lst with
    | [] -> 0u
    | x :: rest -> (if x = Zero then 0u else 1u) * 2u + bitsToInt rest

let rec bitsToBig (lst:Bit list) =
    match lst with
    | [] -> bigint 0
    | x :: rest -> (if x = Zero then bigint 0 else bigint 1) * bigint 2 + bitsToBig rest
      
/// convert Wiredata to FastData equivalent
let rec wireToFast (w: WireData) =
    let n = w.Length
    match w with
    | [Zero] -> Bit 0u
    | [One] -> Bit 1u
    | w when n <= 32 -> Word (bitsToInt w, w.Length)
    | w -> BigWord(bitsToBig w, n)
    
/// convert FastData to Wiredata equivalent
let rec fastToWire (f: FastData) =
    match f with
    | Bit 0u | Word(0u,1) -> 
        [Zero]
    | Bit 1u | Word(1u,1) -> 
        [One]
    | Word(x,l) -> 
        [0..l-1]
        |> List.map (fun n -> if (x &&& (1u <<< n)) = 0u then Zero else One)
    | BigWord(n,l) ->
        if l < 30 then 
            fastToWire (Word(uint32 n,l)) 
        else 
            let b = BigWord(n >>> 30, l-30)
            let digit = Word( uint32 (n % (bigint 1 <<< 30)), 30)
            fastToWire digit @ fastToWire b
    | _ -> failwithf "%A is not a valid FastData value" f

/// Extract bit field (msb:lsb) from f. Bits are numbered little-endian from 0.
/// Note that for a single bit result the un-normalised version is used, so it will
/// be compatible with fast implementation of boolean logic.
let getBits (msb: int) (lsb: int) (f: FastData)  =
    assertThat 
        (msb <= f.Width - 1 && lsb <= msb && lsb >= 0)
        (sprintf "Bits selected out of range (%d:%d) from %A" msb lsb f)
    let f = f.Normalise // get rid of Bit
    match f with
    | Word(x, n) -> 
        let bits = (x >>> lsb) % (1u <<< (msb + 1))
        match f.Width, bits with
        | 1, a -> Bit a
        | n, a -> Word(a, n)
    | BigWord(x,n) ->
        let bits = (x >>> lsb) % (bigint 1 <<< (msb + 1))
        if n <= 32 then
            Word(uint32 bits,n)
        else
            BigWord( bits, n)

    | _ -> failwith "What? Impossible after Normalise"

let appendBits (fMS: FastData) (fLS: FastData) : FastData =
    let ms = fMS.Normalise
    let ls = fMS.Normalise
    let w = fMS.Width + fLS.Width
    match ms,ls with
    | Word(x1, _), Word(x2,m) when w <= 32 ->
        Word ((x1 <<< m) + x2, w)
    | Word(x1, _), Word(x2,m)  ->
        BigWord ((bigint x1 <<< m) + bigint x2, w)
    | _ ->  
        BigWord((ms.GetBigInt <<< ls.Width) ||| ls.GetBigInt, w)
            
        
//--------------------------------Fast Simulation Data Structure-------------------------//
//---------------------------------------------------------------------------------------//
            

    


[<Erase>]
type SimStep = | SimStep of int

   
type FComponentId = ComponentId * ComponentId list


type FData = WireData // for now...

type FastComponent = {
    fId: FComponentId
    cId: ComponentId
    FType: ComponentType
    mutable State: SimulationComponentState
    mutable Inactive: bool
    OutputWidth: int option array
    InputLinks: FData array array
    Outputs: FData array array
    SimComponent: SimulationComponent
    AccessPath: ComponentId list
    FullName: string
    mutable Touched: bool

    } with

    member inline this.GetInput (SimStep epoch)  (InputPortNumber n) = this.InputLinks.[n].[epoch]
                                                                    
    member inline this.PutOutput (SimStep epoch) (OutputPortNumber n) dat = this.Outputs.[n].[epoch] <- dat
    member inline this.Id = this.SimComponent.Id
    
let getFid (cid: ComponentId) (ap: ComponentId list) =
    let ff (ComponentId Id) = Id
    (cid,ap)
      

let getPortNumbers (sc: SimulationComponent) =
    let ins =
        match sc.Type with
        | Constant _ -> 0
        | Input _ | Output _ | BusSelection _ | BusCompare _ | Not | DFF 
        | Register _ | IOLabel | SplitWire _ -> 1
        | Mux2 _ | NbitsAdder _ -> 3
        | _ -> 2
    let outs =
        match sc.Type with
        | Decode4 -> 4
        | NbitsAdder _ | SplitWire _ -> 2
        | _ -> 1
    ins,outs

let getOutputWidths (sc: SimulationComponent) (wa: int option array) =

    let putW0 w = wa.[0] <- Some w
    let putW1 w = wa.[1] <- Some w
    let putW2 w = wa.[2] <- Some w
    let putW3 w = wa.[3] <- Some w

    match sc.Type with
    | Input w | Output w | Register w | RegisterE w | SplitWire w | BusSelection(w,_)
    | Constant (w,_) | NbitsXor w -> putW0 w
    | NbitsAdder w -> putW0 w; putW1 1
    | Not | And | Or  | Xor  | Nand  | Nor   | Xnor | BusCompare _ -> putW0 1
    | AsyncROM mem | ROM mem | RAM mem -> putW0 mem.WordWidth; putW1 mem.AddressWidth
    | Custom _ -> ()
    | DFF | DFFE -> putW0 1
    | Decode4 -> putW0 1; putW1 1; putW2 1; putW3 1
    | Demux2 | Mux2 | IOLabel | MergeWires -> ()
    wa

        

let createFastComponent 
        (numSteps: int) 
        (sComp: SimulationComponent) 
        (accessPath: ComponentId list) =
    let inPortNum,outPortNum = getPortNumbers sComp
    // dummy arrays wil be replaced by real ones when components are linked after being created
    let ins = 
        [|0..inPortNum-1|]
        |> Array.map (fun n -> Array.create numSteps [])
    let outs = 
        [|0..outPortNum-1|]
        |> Array.map (fun n -> Array.create numSteps [])
    let inps =
        let dat =
            match accessPath,sComp.Type with 
            // top-level input needs special inputs because they can't be calculated
            | [], Input width ->  List.replicate width Zero
            | _ -> []
        Array.create inPortNum (Array.create numSteps dat)
    {
        OutputWidth  = getOutputWidths sComp (Array.create outPortNum None)
        State = NoState
        SimComponent = sComp
        fId = getFid sComp.Id accessPath
        cId = sComp.Id
        FType = sComp.Type
        AccessPath = accessPath
        Touched = false 
        InputLinks = ins
        Outputs = outs
        FullName = ""
        Inactive = false
    }
    
      

// The fast simulation components are similar to the issie components they are based on but with addition of arrays
// for direct lookup of inputs and fast access of outputs. The input arrays contain pointers to the output arrays the
// inputs are connected to, the InputPortNumber integer indexes this.
// In addition outputs are contained in a big array indexed by epoch (simulation time). This allows results for multiple
// steps to begin built efficiently and also allows clocked outputs for the next cycle to be constructed without overwriting
// previous outputs needed for that construction.
//
// For reasons of efficiency Issie's list-style WireData type is optimised by using integers as bit arrays.
//
// For ease of implementation Input and Output components are given a single output (input) port not present on issie.
// this allows sub-sheet I/Os to be linked as normal in the constructed graph via their respective Input and Output connections.
//
// Although keeping input and output connections in the new graph is slightly less efficient it makes things simpler because there is a
// 1-1 connection between components (except for custom components which are removed by the gathering process).
// Note that custom component info is still kept because each component in the graph has a path - the list of custom component ids
// between its graph and root. Following issie this does not include a custom component for the sheet being simulated, which is viewed as
// root. Since custom components have been removed this no longer complicates the simulation.

type FastSimulation = {
    Step: SimStep
    FGlobalInputComps: FastComponent array
    FConstantComps: FastComponent array
    FClockedComps: FastComponent array
    FOrderedComps: FastComponent array
    FComps: Map<FComponentId,FastComponent>
    FSComps: Map<FComponentId,SimulationComponent * ComponentId list>
    G: GatherData
    } 

and  GatherData = {
    /// existing Issie data structure representing circuit for simulation - generated by runCanvasStateChecksAndBuildGraph
    Simulation: SimulationGraph
    /// link from Custom Input component to its driving output (for Inputs not in GInputs)
    CustomInputCompLinks: Map<FComponentId * InputPortNumber, FComponentId>
    /// link from component input port to its custom driving output component
    CustomOutputCompLinks: Map<FComponentId,  FComponentId * OutputPortNumber>
    Labels: Map<ComponentId,string>
    AllComps: Map<ComponentId*ComponentId list,SimulationComponent * ComponentId list> // maps to component and its path in the graph
    /// List of component Input components that are driven externally to simulation
    } with


    member this.getFullName (cid,ap) = 
            List.map ( fun cid -> match Map.tryFind cid this.Labels with | Some lab -> lab | None -> "*" ) (ap @ [cid])
            |> String.concat "."

let emptyGather = {
    Simulation = Map.empty
    Labels = Map.empty
    CustomInputCompLinks=Map.empty
    CustomOutputCompLinks=Map.empty
    AllComps=Map.empty
}

let emptyFastSimulation() =
    {
        Step = SimStep 0
        FGlobalInputComps = Array.empty
        FConstantComps = Array.empty
        FClockedComps = Array.empty
        FOrderedComps = Array.empty
        FComps = Map.empty
        FSComps = Map.empty
        G = emptyGather
    }
let getPathIds (cid,ap) =
    let rec getPath ap =
        match ap with
        | [] -> []
        | cid :: rest ->
            (cid, List.rev rest) :: getPath rest                
    getPath (List.rev ap) |> List.rev
    

 
                
                



let mapUnion m1 m2 =
    (m2, m1)
    ||> Map.fold (fun m key value -> Map.add key value m )


    
/// Assert that the wireData only contain a single bit, and return such bit.
let private extractBit (wireData : WireData) : Bit =
    assertThat (wireData.Length = 1) <| sprintf "extractBit called with wireData: %A" wireData
    wireData.[0]
    
let private packBit (bit : Bit) : WireData = [bit]
    

/// Read the content of the memory at the specified address.
let private readMemory (mem : Memory) (address : WireData) : WireData =
    let intAddr = convertWireDataToInt address
    let outDataInt = Helpers.getMemData intAddr mem
    convertIntToWireData mem.WordWidth outDataInt
    
/// Write the content of the memory at the specified address.
let private writeMemory (mem : Memory) (address : WireData) (data : WireData) : Memory =
    let intAddr = convertWireDataToInt address
    let intData = convertWireDataToInt data
    {mem with Data = Map.add intAddr intData mem.Data}
    
let private getRamStateMemory state =
    match state with
    | RamState memory -> memory
    | _ -> failwithf "what? getRamStateMemory called with an invalid state: %A" state

let inline private bitNot bit =
    match bit with
    | Zero -> One
    | One -> Zero
    
let inline private bitAnd bit0 bit1 =
    match bit0, bit1 with
    | One, One -> One
    | _, _ -> Zero
    
let inline private bitOr bit0 bit1 =
    match bit0, bit1 with
    | Zero, Zero -> Zero
    | _, _ -> One
    
let inline private bitXor bit0 bit1 =
    match bit0, bit1 with
    | Zero, One | One, Zero -> One
    | _, _ -> Zero
    
let inline private bitNand bit0 bit1 =
    bitAnd bit0 bit1 |> bitNot
    
let inline private bitNor bit0 bit1 =
    bitOr bit0 bit1 |> bitNot
    
let inline private bitXnor bit0 bit1 =
    bitXor bit0 bit1 |> bitNot
    
/// Given a component type, return a function takes a ReducerInput and
/// transform it into a ReducerOuptut.
/// The ReducerOutput should have Outputs set to None if there are not enough
/// ReducerInput.Inputs to calculate the outputs.
/// For custom components, return a fake version of the reducer, that has to be
/// replaced when resolving the dependencies.
/// TODO: some components reducers are quite similar, for example Register and
/// RegisterE and DFF and DFFE. It is probably a good idea to merge them
/// together to avoid duplicated logic.
let private fastReduce (simStep: int) (comp : FastComponent) : Unit =
    let componentType = comp.FType
       
    //printfn "Reducing %A...%A"  comp.FType (comp.FullName)
    let n = comp.InputLinks.Length
    let inline ins i = 
        assertThat (i < n) (sprintf "What? Invalid input port (%d:step%d) used by %s (%A) reducer with %d Ins" 
                                    i simStep comp.FullName componentType n)
        let fd = comp.GetInput (SimStep simStep) (InputPortNumber i)
        assertThat (fd <> []) (sprintf "What? Invalid data %A  found in input port (%d:step%d) used by %s (%A) reducer" 
                                    fd i simStep comp.FullName componentType)
        fd

    let inline getLastCycleOut n = 
        match comp.OutputWidth.[n], simStep with
        | None, _ -> failwithf "Can't reduce %A (%A) because outputwidth is not known" comp.FullName comp.FType
        | Some w, 0 -> List.replicate w Zero
        | Some w, t -> comp.Outputs.[n].[t]
            

    let inline insOld i = 
            assertThat (i < n) (sprintf "What? Invalid input port (%d:step%d) used by %s (%A) reducer with %d Ins" 
                                    i simStep comp.FullName componentType n)
            let fd = comp.GetInput (SimStep (simStep-1)) (InputPortNumber i)
            assertThat (fd <> []) 
                       (sprintf "What? Invalid data %A  found in input port (%d:step%d) used by %s (%A) reducer" 
                                    fd i simStep comp.FullName componentType)
            fd
             
    let inline put0 fd = comp.PutOutput (SimStep simStep) (OutputPortNumber 0) fd
    let inline put1 fd = comp.PutOutput (SimStep simStep) (OutputPortNumber 1) fd
    let inline put2 fd = comp.PutOutput (SimStep simStep) (OutputPortNumber 2) fd
    let inline put3 fd = comp.PutOutput (SimStep simStep) (OutputPortNumber 3) fd
    let inline put4 fd = comp.PutOutput (SimStep simStep) (OutputPortNumber 4) fd

    let inline putW num w = comp.OutputWidth.[num] <- Some w

    let inline getBinaryGateReducer (op : Bit -> Bit -> Bit) : Unit =
        let bit0 = extractBit (ins 0)
        let bit1 = extractBit (ins 1)
        put0 <| packBit (op bit1 bit0)

    let inline checkWidth width (bits:FData) =
        assertThat 
            (bits.Length = width)
            (sprintf "Input node reducer received wrong number of bits: expected %d but got %d" width bits.Length)


    match componentType with
    | Input width ->
        if not comp.Inactive then
            let bits = ins 0
            //printfn "Got input 0 = %A Links=<%A> len=%d" bits comp.InputLinks comp.InputLinks.Length
            checkWidth width bits
            //printfn "output array = %A" comp.Outputs
            put0 bits
            //printfn "Finished!"
               
    | Constant (width, cVal) ->
            put0 <| convertIntToWireData width (int64 (uint32 cVal))
    | Output width ->
        let bits = ins 0
        //printfn "In output bits=%A, ins = %A" bits comp.InputLinks
        checkWidth width bits
        put0 bits
    | IOLabel -> 
        if not comp.Inactive then
            let bits = ins 0
            //printfn "Reducing IOLabel %A" comp.SimComponent.Label
            put0 bits
    | Not ->
        let bit = extractBit (ins 0)
        put0 <| packBit (bitNot bit)
    | BusSelection(width, lsb) ->
        let bits = ins 0
        assertThat 
            (bits.Length >= width + lsb)
            (sprintf "Bus Selection received too few bits: expected at least %d but got %d" (width + lsb) bits.Length)
        let outBits = bits.[lsb .. lsb + width - 1]
        put0 outBits
    | BusCompare(width, compareVal) ->
        //printfn "Reducing compare %A" comp.SimComponent.Label
        let bits = ins 0
        assertThat (bits.Length = width)
                    (sprintf "Bus Compare received wrong number of bits: expecting  %d but got %d" width bits.Length)
        let inputNum = convertWireDataToInt bits
        let outNum: WireData = [if inputNum = int64 compareVal then One else Zero]
        put0 outNum
    | And  -> getBinaryGateReducer bitAnd
    | Or   -> getBinaryGateReducer bitOr
    | Xor  -> getBinaryGateReducer bitXor
    | Nand -> getBinaryGateReducer bitNand
    | Nor  -> getBinaryGateReducer bitNor
    | Xnor -> getBinaryGateReducer bitXnor
    | Mux2 ->
        let bits0, bits1, bitSelect = ins 0, ins 1, ins 2
        assertThat (bits0.Length = bits1.Length)
        <| sprintf "Mux2 %s received two inputs with different widths: (%A) <> (%A)" comp.FullName bits0 bits1
        let out = if (extractBit bitSelect) = Zero then bits0 else bits1
        put0 out
        putW 0 bits0.Length
    | Demux2 ->
        let bitsIn, bitSelect = ins 0, ins 1
        let zeros = List.replicate bitsIn.Length Zero
        let out0, out1 = if (extractBit bitSelect) = Zero
                            then bitsIn, zeros 
                            else zeros, bitsIn
        let w = bitsIn.Length
        put0 out0
        put1 out1
        putW 0 w
        putW 1 w
    | NbitsAdder numberOfBits ->
        let cin, A, B = ins 0, ins 1, ins 2
        let sum, cout =
            [cin; A; B]
            |> List.map convertWireDataToInt
            |> List.reduce (+)
            |> convertIntToWireData (numberOfBits + 1)
            |> List.splitAt numberOfBits
        put0 sum
        put1 cout
    | NbitsXor numberOfBits ->
        let A, B = ins 0, ins 1
        let outVal =
            [A ; B]
            |> List.map convertWireDataToInt
            |> (function | [A;B] -> A ^^^ B | _ -> failwithf "What? impossible!")
            |> convertIntToWireData (numberOfBits)              
        put0 outVal   
    | Decode4 ->
        let select, data = ins 0, ins 1
        let selN = convertWireDataToInt select |> int
        let dataN = convertWireDataToInt data |> int
        let outs =
            [|0..3|] |> 
            Array.map (fun n -> 
                let outBit = if n = selN then dataN else 0
                convertIntToWireData 1 (int64 outBit))
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
        let outBits = bits0 @ bits1
        put0 outBits
        putW 0 outBits.Length
    | SplitWire topWireWidth ->
        let bits = ins 0
        assertThat (bits.Length >= topWireWidth + 1)
        <| sprintf "SplitWire received too little bits: expected at least %d but got %d" (topWireWidth + 1) bits.Length
        let bits0, bits1 = List.splitAt topWireWidth bits
        // Little endian, bits leaving from the top wire are the least
        // significant.
        put0 bits0
        put1 bits1
        putW 1 bits1.Length
    | DFF ->
        let d = extractBit (insOld 0)
        put0 (packBit d)
    | DFFE ->
        let d,en = extractBit(insOld 0), extractBit(insOld 1)
        if en = One then
            put0 <| packBit d
        else
            put0 (getLastCycleOut 0)

    | Register width ->
        let bits = insOld 0
        assertThat (bits.Length = width)
        <| sprintf "Register received data with wrong width: expected %d but got %A" width bits.Length
        put0 bits

    | RegisterE width ->
        let bits, enable = insOld 0, insOld 1
        assertThat (bits.Length = width)
        <| sprintf "RegisterE received data with wrong width: expected %d but got %A" width bits.Length
        if (extractBit enable = One)
        then put0 bits
    | AsyncROM mem -> // Asynchronous ROM.
        let addr = ins 0
        assertThat (addr.Length = mem.AddressWidth)
        <| sprintf "ROM received address with wrong width: expected %d but got %A" mem.AddressWidth addr
        let outData = readMemory mem addr
        put0 outData
    | ROM mem -> // Synchronous ROM.
        let addr = insOld 0
        assertThat (addr.Length = mem.AddressWidth)
        <| sprintf "ROM received address with wrong width: expected %d but got %A" mem.AddressWidth addr
        let outData = readMemory mem addr
        put0 outData
    | RAM _ ->
        let mem = getRamStateMemory comp.State
        let address = insOld 0
        assertThat (address.Length = mem.AddressWidth)
        <| sprintf "RAM received address with wrong width: expected %d but got %A" mem.AddressWidth address
        let dataIn = insOld 1
        assertThat (dataIn.Length = mem.WordWidth)
        <| sprintf "RAM received data-in with wrong width: expected %d but got %A" mem.WordWidth dataIn
        let write = extractBit (insOld 2)
        // If write flag is on, write the memory content.
        let mem, dataOut =
            match write with
            | Zero ->
                // Read memory address and return memory unchanged.
                mem, readMemory mem address
            | One ->
                // Update memory and return new content.
                writeMemory mem address dataIn, dataIn
        comp.State <-  RamState mem
        put0 dataOut
   

//------------------------------------------------------------------------------//
//-----------------------------Fast Simulation----------------------------------//
//------------------------------------------------------------------------------//

/// Create an initial gatherdata object with inputs, non-ordered components, simulationgraph, etc
/// This must explore graph recursively extracting all the initial information.
/// Custom components are scanned and links added, one for each input and output
let rec gatherPhase 
        (ap: ComponentId list) 
        (numSteps:int) 
        (graph: SimulationGraph) 
        (gather: GatherData) : GatherData =
    (gather, graph)
    ||> Map.fold ( fun gather cid comp ->
        let gather = 
            { gather with 
                AllComps = Map.add (cid,ap) (comp,ap) gather.AllComps
                Labels = Map.add cid ((fun (ComponentLabel s) -> s) comp.Label) gather.Labels
            }
        match comp.Type, comp.CustomSimulationGraph, ap with
        | Custom ct, Some csg, _->
            let ap' = ap @ [cid]
            let allComps = Map.toList csg |> List.map snd
               
            let getCustomNameIdsOf compSelectFun = 
                allComps
                |> List.filter (fun comp -> compSelectFun comp.Type)
                |> List.map ( fun comp -> (comp.Label,match comp.Type with | Input n -> n | Output n -> n | _ -> -1), comp.Id)
                |> Map.ofList

            let outputs = getCustomNameIdsOf isOutput
            let outLinks =
                ct.OutputLabels
                |> List.mapi (fun i (lab,labOutWidth) -> (outputs.[ComponentLabel lab, labOutWidth],ap'), ((cid,ap), OutputPortNumber i) )
                |> Map.ofList

            let inputs = getCustomNameIdsOf isInput
            let inLinks =
                ct.InputLabels
                |> List.mapi (fun i (lab,labOutWidth) -> (((cid,ap), InputPortNumber i), (inputs.[ComponentLabel lab,labOutWidth],ap')))
                |> Map.ofList

            let g = gatherPhase ap' numSteps csg gather
            {g with
                Simulation = graph
                CustomInputCompLinks = mapUnion inLinks g.CustomInputCompLinks
                CustomOutputCompLinks = mapUnion outLinks g.CustomOutputCompLinks
                AllComps = mapUnion ((Map.toList >> List.map (fun (k,v) -> (k,ap),(v,ap)) >> Map.ofList) graph) g.AllComps  
            } 
            | _ -> gather
        )

let printGather (g: GatherData) =
    printfn "%d components" g.AllComps.Count
    Map.iter (fun (cid,ap) ((comp:SimulationComponent),ap') -> 
        printfn "%s: %A" (g.getFullName (cid,ap))  comp.Outputs) g.AllComps

    
let rec createInitFastCompPhase (numSteps:int) (g:GatherData) (f:FastSimulation) =
    let makeFastComp cid =
        let comp, ap = g.AllComps.[cid]
        let fc = createFastComponent numSteps comp ap
        let fc = {fc with FullName = g.getFullName cid}
        let outs: FData array array = 
            (if isOutput comp.Type then
                let n = Option.defaultValue 1 fc.OutputWidth.[0]
                let outs = [| Array.create numSteps [] |]
                outs.[0].[0] <- List.replicate n Zero
                outs
            else 
                fc.Outputs)

        printfn "***Making %A with %d outs" comp.Type outs.Length
        {fc with Outputs = outs}
    let comps: Map<FComponentId,FastComponent> = 
        (Map.empty, g.AllComps)
        ||> Map.fold (fun m cid (comp,ap) -> if isCustom comp.Type then m else Map.add (comp.Id,ap) (makeFastComp (comp.Id,ap)) m)
    {f with 
        FComps = comps
        FSComps = g.AllComps
    }        

/// has side effect of making IOLabels of same name all use same output array
/// this means that an input to any one will produce an output on all, for no effort.
/// IOLabels without driven inputs that are thus not used are later on flagged inactive
/// they must not be reduced, and will not be included in the ordered component list
let reLinkIOLabels (fs: FastSimulation) =
    (Map.empty,fs.FComps)
    ||> Map.fold (fun labs fid fComp -> 
        if fComp.FType = IOLabel then
            let (_,ap) = fid
            let name = (fst fs.FSComps.[fid]).Label
            match Map.tryFind (name,ap) labs with
            | None -> Map.add (name,ap) fComp.Outputs.[0] labs
            | Some (outArray) -> 
                fComp.Outputs.[0] <- outArray
                labs
            //|> (fun labs -> printfn "IOLabel outputs: <%A>" fComp.Outputs;labs)

        else labs) 
    |> ignore // execute this only for its side effect of commoning up the IOLabel output arrays
   
                

let linkFastComponents (g: GatherData) (f:FastSimulation) =
    let outer = List.rev >> List.tail >> List.rev
    let sComps = g.AllComps
    let fComps = f.FComps
    let getSComp (cid,ap) = fst sComps.[cid,ap]
    let apOf fid = fComps.[fid].AccessPath 
        
    let rec getLinks ((cid,ap): FComponentId) (opn: OutputPortNumber ) (ipnOpt: InputPortNumber option) =
        let sComp = getSComp (cid,ap)
        //printfn "Getting links: %A %A %A" sComp.Type opn ipnOpt
        match  isOutput sComp.Type, isCustom sComp.Type, ipnOpt with
        | true, _, None when apOf (cid,ap) = [] -> 
            [||] // no links in this case from global output
        | true, _, None -> 
            //printfn "checking 1:%A %A" (g.getFullName(cid,ap)) (Map.map (fun k v -> g.getFullName k) g.CustomOutputCompLinks)
            let cid,opn = g.CustomOutputCompLinks.[cid,ap]
            assertThat (isCustom (fst sComps.[cid]).Type) "What? this should be a custom component output"
            getLinks  cid opn None // go from inner output to CC output and recurse
        | false, true, Some ipn -> 
            //printfn "checking 2:%A:IPN<%A>" (g.getFullName(cid,ap)) ipn
            //printfn "CustomInCompLinks=\n%A" (Map.map (fun (vfid,vipn) fid -> 
                //sprintf "%A:%A -> %A\n" (g.getFullName vfid) vipn (g.getFullName fid) ) g.CustomInputCompLinks |> mapValues)
            //printfn "Done"
            [|g.CustomInputCompLinks.[(cid, ap),ipn], opn, InputPortNumber 0|] // go from CC input to inner input: must be valid
        | _, false, Some ipn -> 
            [|(cid,ap), opn, ipn|] // must be a valid link
        | false, _ , None -> 
            sComp.Outputs
            |> Map.toArray
            |> Array.filter (fun (opn',_) ->opn' = opn )
            |> Array.collect (fun (opn,lst) -> 
                lst 
                |> List.toArray
                |> Array.collect (fun (cid,ipn) -> 
                    getLinks (cid,ap) opn (Some ipn)))
                    
        | x -> failwithf "Unexpected link match: %A" x
    reLinkIOLabels f
    let mutable linkCheck: Map<(FComponentId * InputPortNumber),(FComponentId * OutputPortNumber)> = Map.empty
    f.FComps
    |> Map.iter (fun fid fComp ->
        let outs = fComp.Outputs
        //printfn "linking %d outputs from %A<%A>" outs.Length (g.getFullName fid) fComp.FType
        fComp.Outputs
        |> Array.iteri (fun iOut _ -> 
            getLinks fid (OutputPortNumber iOut) None
            |> Array.map (fun (fid,_,ip) -> fid,iOut,ip)
            //|> (fun x -> printfn "Link array from getlinks has %d items: %A" x.Length x ; x )
            |> Array.iter (fun (cid, opn, InputPortNumber ipn) ->
                //printfn "\nLink found %s-%d -> %s-%d (opn=%d)" (g.getFullName fid) iOut (g.getFullName cid) ipn opn
                //printfn "Link %A.%A <- %A.%A\n\n"  (g.getFullName cid) ipn  (g.getFullName fComp.fId) opn
                let linked = Map.tryFind (cid,InputPortNumber ipn) linkCheck
                match linked with
                | None -> ()
                | Some (fid,opn) ->
                    failwithf "Multiple linkage: (previous driver was %A,%A)" (g.getFullName fid) opn

                linkCheck <- Map.add (cid,InputPortNumber ipn) (fComp.fId,OutputPortNumber opn) linkCheck
                f.FComps.[cid].Inactive <- true
                f.FComps.[cid].InputLinks.[ipn] <- fComp.Outputs.[opn])))
    f
 
                        
let orderCombinationalComponents (numSteps: int) (fs: FastSimulation): FastSimulation =

    let isValidData (fd: WireData) = fd <> []

    let isComb (comp:FastComponent) =
        match comp.FType with
        | Input _ when comp.AccessPath = [] -> false
        | ct when couldBeSynchronousComponent ct -> false
        | _ -> true

    let canBeReduced (fc:FastComponent) =
        //printfn "checking %A can be reduced <%A> len=%d" fc.FType fc.InputLinks fc.InputLinks.Length
        //printfn "Checking canbereduced: %A:<%A>" fc.SimComponent.Label fc.InputLinks
        //assertThat ((not (isNull fc.InputLinks)) && fc.InputLinks.Length > 0) <| sprintf "No input links"
        let can = isComb fc && not fc.Touched && Array.forall (fun (arr: FData array) -> 
                    arr.Length > 5 && isValidData arr.[0]) fc.InputLinks
        //let name = fc.SimComponent.Label
        //printf "canbereduced %A %A: notTtouched %A, comb %A, Inputs <%A>, <=%A>" 
        //        can name (not fc.Touched) (isComb fc) fc.InputLinks (Array.map (fun (arr: FData array) -> isValidData arr.[0]) fc.InputLinks)
        //printfn "num inputs = %d" fc.InputLinks.Length

        can

    let init fc =
        fastReduce 0 fc
        fc.Touched <- true

    let initInput (fc:FastComponent) =
        //printfn "Init input..."
        fc.InputLinks.[0] <- Array.create numSteps (List.replicate (Option.defaultValue 1 fc.OutputWidth.[0]) Zero)
        //printfn "Initialised input: %A" fc.InputLinks
        fastReduce 0 fc
        fc.Touched <- true

    let initClockedOuts (fc: FastComponent) =
        fc.Outputs
        |> Array.iteri  (fun i vec -> 
            match fc.OutputWidth.[i] with
            | Some n ->  
                printfn "Initialising OutputPort %d of %s to 0 (width %d)" i fc.FullName n
                vec.[0] <- List.replicate n Zero
            | _ -> ()
            match fc.FType with
            | RAM mem ->
                fc.State <- RamState mem
            | _ -> ())

    printfn "Ordering %d clocked outputs" fs.FClockedComps.Length
    fs.FClockedComps |> Array.iter initClockedOuts    
    printfn "Ordering %d constants" fs.FConstantComps.Length
    fs.FConstantComps |> Array.iter init
    printfn "Ordering %d global inputs" fs.FGlobalInputComps.Length
    fs.FGlobalInputComps |> Array.iter initInput
    printfn "Setup done..."
    let mutable orderedComps: FastComponent list = 
        List.ofArray <| Array.concat [|fs.FClockedComps ; fs.FConstantComps ; fs.FGlobalInputComps|]
    let fComps = mapValues fs.FComps
    printfn "%A" (fComps |> Array.map (fun comp -> comp.SimComponent.Label))
    let mutable nextBatch = Array.filter canBeReduced fComps
    printfn "Loop init done"
    printfn "%d constant, %d input, %d clocked, %d read to reduce, from %d" 
        fs.FConstantComps.Length
        fs.FGlobalInputComps.Length
        fs.FClockedComps.Length
        nextBatch.Length
        fs.FComps.Count
    while nextBatch.Length <> 0 do
        printf "Adding %d combinational components" nextBatch.Length
        nextBatch
        |> Array.iter (fun fc -> 
            if (not fc.Touched) then 
                fastReduce 0 fc
                fc.Inactive <- false // in case it was an IOLabel, this one is the driver.
                orderedComps <- fc :: orderedComps
            fc.Touched <- true
            if fc.SimComponent.Type = IOLabel then
                let name = fc.SimComponent.Label
                let ap = fc.AccessPath
                fComps
                |> Array.iter ( fun fc' -> 
                    let sc = fc'.SimComponent
                    //printf "labrl=%A, outs=%A" sc.Label fc.Outputs
                    if sc.Label = name && sc.Type = IOLabel && fc'.AccessPath = ap && fc'.fId <> fc.fId
                    then 
                        //printfn "Touching %A" sc.Label
                        fc'.Inactive <- true // prevent any otehr labels from being reduced
                 ))
        printfn "Total is now %d" orderedComps.Length
        // work out new components that can still be added
        nextBatch <- Array.filter canBeReduced (mapValues fs.FComps)
    let orderedSet =
        orderedComps 
        |> List.toArray
        |> Array.map (fun co -> co.fId) 
        |> Set
    let allSet = 
        mapValues fs.FComps
        |> Array.map (fun co -> co.fId) 
        |> Set  
    let notOrdered = Set.difference allSet orderedSet
    printfn "Component order:"
    allSet
    |> Set.map (fun fid -> fs.G.getFullName fid, fid)
    |> Set.toList
    |> List.map (fun (s,fid) -> sprintf " %s %s\n" (if Set.contains fid notOrdered then "*" else " ") s)
    |> List.sort
    |> List.iter (printfn "%s")
    printfn "Ordering finished\n"
    let badComps =
        notOrdered
        |> Set.toList
        |> List.map (fun fId -> fs.FComps.[fId])
        |> List.filter (function | {Inactive = inactive} -> not inactive)
    badComps
    |> List.iter (fun fc -> 
                printfn "\n-----------------\n%A: inputs=%A, touched=%A, canbereduced=%A" 
                    fc.FullName 
                    fc.InputLinks 
                    fc.Touched (canBeReduced fc)
                printfn "%A: inputvalid=%A\n--------------\n" 
                    fc.FullName (Array.map (fun (arr: FData array) -> 
                                            arr.Length > 5 && isValidData arr.[0]) fc.InputLinks))
    assertThat 
        (badComps.Length = 0)
        (sprintf "Components not linked: %A\n" (badComps |> List.map (fun fc -> fc.FullName)))
    {fs with FOrderedComps = orderedComps |> Array.ofList |> Array.rev}
       
let stepSimulation (fs: FastSimulation) =
    let (SimStep n) = fs.Step
    Array.iter (fastReduce (n + 1)) fs.FOrderedComps 
    {fs with Step = SimStep(n+1)}

            


      
           

/// create a fast simulation data structure, with all necessary arrays, and components
/// ordered for evaluation.
/// this function also creates the reducer functions for each component
/// similar to the reducer builder in Builder, but with inputs and outputs using the FastSimulation
/// mutable arrays
let buildFastSimulation (numberOfSteps: int) (graph: SimulationGraph) : FastSimulation =
    let numberOfSteps = 1000
    let gather = gatherPhase [] numberOfSteps graph (emptyGather)
    printGather gather
    let fs = createInitFastCompPhase numberOfSteps gather (emptyFastSimulation())
    printfn "Fast components:"
    (mapKeys fs.FComps) |> Array.iter (fun x -> printfn "%s" (gather.getFullName x))
    let fs = linkFastComponents gather fs
    printfn "Linking finished"
    let getArrayOf pred fComps =
        fComps
        |> Map.filter (fun cid comp -> pred comp)
        |> Map.toArray
        |> Array.map snd
    {fs with 
        FGlobalInputComps = fs.FComps |> getArrayOf (fun fc -> isInput fc.FType && fc.AccessPath = [])
        FConstantComps = fs.FComps |> getArrayOf (fun fc -> match fc.FType with Constant _ -> true | _ -> false)
        FClockedComps = fs.FComps |> getArrayOf (fun fc -> couldBeSynchronousComponent fc.FType)
        FOrderedComps = Array.empty
        FSComps = gather.AllComps
        G = gather
    }
    |> orderCombinationalComponents numberOfSteps
    |> (fun fs -> 
            (fs, [0..998])
            ||> List.fold (fun fs n -> 
                if n % 20 = 0 then printfn "Step %d" n
                stepSimulation fs))
 
        

/// converts from WireData to FastData and sets simulation inputs from SimulationGraph
let setSimulationData (fSim: FastSimulation) (graph: SimulationGraph) = failwithf "Not Implemented"

/// write Simulation data back to an Issie structure.
let writeSimulationData (fSim: FastSimulation) (step: SimStep) (graph: SimulationGraph) : SimulationGraph = failwithf "Not Implemented"

/// run a simulation for a given number of steps
let runSimulation (steps: int) (fSim: FastSimulation) : FastSimulation = failwithf "Not Implemented"
