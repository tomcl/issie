module Fast
    open Fable.Core
    open CommonTypes
    open Helpers
    open SimulatorTypes
    open SynchronousUtils
    

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

    type FData = WireData // for now...

    type FastComponent = {
        Inputs: FData array
        InputLinks: (FData array array) array
        Outputs: FData array array
        SimComponent: SimulationComponent
        AccessPath: ComponentId list
        Touched: bool

        } with

        member inline this.GetInput (SimStep epoch)  (InputPortNumber n) = this.InputLinks.[n].[epoch]
                                                                    
        member inline this.PutOutput (SimStep epoch) (OutputPortNumber n) dat = this.Outputs.[n].[epoch] <- dat
        member inline this.Id = this.SimComponent.Id
    
    let getPortNumbers (sc: SimulationComponent) =
        let ins =
            match sc.Type with
            | Constant _ -> 0
            | Input _ | Output _ | BusSelection _ | BusCompare _ | Not | DFF 
            | Register _   -> 1
            | Mux2 _ | NbitsAdder _ -> 3
            | _ -> 2
        let outs =
            match sc.Type with
            | Decode4 -> 4
            | NbitsAdder _ | SplitWire _ -> 2
            | _ -> 1
        ins,outs

    let createFastComponent 
            (numSteps: int) 
            (sComp: SimulationComponent) 
            (accessPath: ComponentId list) =
        let inPortNum,outPortNum = getPortNumbers sComp
        // dummy arrays wil be replaced by real ones when components are linked after being created
        let ins = 
            [|0..inPortNum|]
            |> Array.map (fun n -> Array.empty)
        let outs = 
            [|0..outPortNum-1|]
            |> Array.map (fun n -> Array.create numSteps [])
        {
            SimComponent = sComp
            AccessPath = accessPath
            Touched = false 
            Inputs = Array.create inPortNum []
            InputLinks = ins
            Outputs = outs
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
        FGlobalInputs: FData array array
        FClockedComps: FastComponent array
        FInputComps: FastComponent array
        FCombComps: FastComponent array
        FComps: Map<ComponentId,FastComponent>
        } 
            
    type GatherData = {
        /// existing Issie data structure representing circuit for simulation - generated by runCanvasStateChecksAndBuildGraph
        Simulation: SimulationGraph
        /// link from Custom Input component to its driving output (for Inputs not in GInputs)
        CustomInputCompLinks: Map<ComponentId * InputPortNumber, ComponentId>
        /// link from component input port to its custom driving output component
        CustomOutputCompLinks: Map<ComponentId * OutputPortNumber, ComponentId>
        AllComps: Map<ComponentId,SimulationComponent * ComponentId list> // maps to component and its path in the graph
        /// List of component Input components that are driven externally to simulation
        GlobalInputs: ComponentId Set
        OrderedComps: ComponentId list
        ClockedComps: ComponentId Set
        CombComps: ComponentId Set
        }

    let emptyGather = {
            Simulation = Map.empty
            CustomInputCompLinks=Map.empty
            CustomOutputCompLinks=Map.empty
            AllComps=Map.empty
            GlobalInputs = Set []
            ClockedComps = Set []
            CombComps = Set []
            OrderedComps = []
        }

    let mapUnion m1 m2 =
        (m2, m1)
        ||> Map.fold (fun m key value -> Map.add key value m )


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
            let gather = {gather with AllComps = Map.add cid (comp,ap) gather.AllComps}
            match comp.Type, comp.CustomSimulationGraph, ap with
            | Custom ct, Some csg, _->
                let allComps = Map.toList csg |> List.map snd
                let getCustomNameIdsOf compSelectFun = 
                    allComps
                    |> List.filter (fun comp -> compSelectFun comp.Type)
                    |> List.map ( fun comp -> comp.Label, comp.Id)
                    |> Map.ofList

                let outputs = getCustomNameIdsOf isOutput
                let outLinks =
                    ct.OutputLabels
                    |> List.map (fun (lab,labOut) -> (cid,(OutputPortNumber labOut)), outputs.[ComponentLabel lab])
                    |> Map.ofList

                let inputs = getCustomNameIdsOf isInput
                let inLinks =
                    ct.InputLabels
                    |> List.map (fun (lab,labOut) -> ((cid, InputPortNumber labOut), inputs.[ComponentLabel lab]))
                    |> Map.ofList

                let g = gatherPhase (cid:: ap) numSteps csg gather
                {gather with
                    CustomInputCompLinks = mapUnion inLinks g.CustomInputCompLinks
                    CustomOutputCompLinks = mapUnion outLinks g.CustomOutputCompLinks
                    AllComps = mapUnion (Map.map (fun k v -> v,ap) graph) g.AllComps  
                }
            | Input _ , _, [] ->
                {gather with GlobalInputs = Set.add comp.Id gather.GlobalInputs}
            | _ when couldBeSynchronousComponent comp.Type ->
                {gather with ClockedComps = Set.add comp.Id gather.ClockedComps}
            | _ ->
                {gather with CombComps = Set.add comp.Id gather.CombComps}               
            )
    
    let rec createInitFastCompPhase (numSteps:int) (g:GatherData) (f:FastSimulation) =
        let getDrivenInput cid ipn =
            let comp,ap = g.AllComps.[cid]
            if isCustom comp.Type then
                g.CustomInputCompLinks.[cid,ipn], InputPortNumber 0
            else cid,ipn

        let makeFastComp cid =
            let comp, ap = g.AllComps.[cid]
            let fc = createFastComponent numSteps comp ap
            let outs = 
                if isOutput comp.Type then
                    [|OutputPortNumber 0, comp.Inputs.[InputPortNumber 0]|]
                else 
                   comp.Outputs
                   |> List.map (fun (opn, (cid',ipn)) -> g.AllComps.[cid'].Inputs.[ipn])
                   |> List.toArray
                |> Array.map (fun fd ->
                    let a = Array.create numSteps [] 
                    a.[0] <- fd
                    a)
            {fc with Outputs = outs}
        let comps: Map<ComponentId,FastComponent> = 
            (Map.empty, g.AllComps)
            ||> Map.fold (fun m cid (comp,ap) -> if isCustom comp.Type then m else Map.add comp.Id (makeFastComp comp.Id) m)
        {f with FComps = fc}

                



        

    /// Add components in order (starting with clocked components and inputs).
    /// The gathering process iteratively extracts components from AllComponents and adds
    /// them to orderedComponents such that a later component depends only on earlier components.
    /// Therefore evaluation can be done without checking inputs in reverse order of this list.
    let gatherComponents (gd: GatherData): GatherData = failwithf "Not Implemented"

    /// create a fast simulation data structure, with all necessary arrays, and components
    /// ordered for evaluation.
    /// this function also creates the reducer functions for each component
    /// similar to the reducer builder in Builder, but with inputs and outputs using the FastSimulation
    /// mutable arrays
    let buildFastSimulation (numberOfEpochs: int) (gd: GatherData) : FastSimulation = failwithf "Not Implemented"

    /// converts from WireData to FastData and sets simulation inputs from SimulationGraph
    let setSimulationData (fSim: FastSimulation) (graph: SimulationGraph) = failwithf "Not Implemented"

    /// write Simulation data back to an Issie structure.
    let writeSimulationData (fSim: FastSimulation) (step: SimStep) (graph: SimulationGraph) : SimulationGraph = failwithf "Not Implemented"

    /// run a simulation for a given number of steps
    let runSimulation (steps: int) (fSim: FastSimulation) : FastSimulation = failwithf "Not Implemented"
    

