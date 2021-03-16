module Fast
    open Fable.Core
    open CommonTypes
    open Helpers
    open SimulatorTypes

    // We use all three options for efficiency
    // Bit is more efficient than word for known boolean ops but it can be normalised to Word to make implementation
    // of multiple bit components (that may carry one bit) simpler.
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
        member inline this.GetBitAsInt = 
            match this with 
            | Bit n -> Some n 
            | Word(n, 1) -> Some n 
            | _ -> None
        member inline this.GetBigInt =
            match this with
            | Bit n -> bigint n
            | Word(n,_) -> bigint n
            | BigWord(n,_) -> n
        /// return Some uint32 representing data if possible else None
        member inline this.GetUint32 =
            match this with
            | Bit n -> Some n
            | Word(n,w) -> Some n
            | BigWord(n, w) when w <= 32 -> Some (uint32 n)
            | _ -> None
            

    let rec bitsToInt (lst:Bit list) =
        match lst with
        | [] -> 0u
        | x :: rest -> (if x = Zero then 0u else 1u) + 2u * bitsToInt rest

    let rec bitsToBig (lst:Bit list) =
        match lst with
        | [] -> bigint 0
        | x :: rest -> (if x = Zero then bigint 0 else bigint 1) + ((bitsToBig rest) <<< 1)
      
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
            
        

            

    
    [<Erase>]
    type InputPortNumber = | InputPortNumber of int

    [<Erase>]
    type OutputPortNumber = | OutputPortNumber of int

    [<Erase>]
    type Epoch = | Epoch of int

    type FastComponent = {
        Inputs: (FastData array array * OutputPortNumber) array
        Outputs: FastData array array
        SimComponent: SimulationComponent
        accessPath: ComponentId list
        } with
        member inline this.GetInput (Epoch epoch)  (InputPortNumber n) = let a, (OutputPortNumber index) = this.Inputs.[n]
                                                                         a.[epoch].[index]
        member inline this.PutOutput (Epoch epoch) (OutputPortNumber n) dat = this.Outputs.[epoch].[n] <- dat
        member inline this.Id = this.SimComponent.Id
                 
    // The fast simulation components are similar to the issie conmponents they are based on but with addition of arrays
    // for direct lookup of inputs an fast access of outputs. The input arrays contain pointers to the output arrays the
    // inputs are connected to, the InputportNumber integer indexes this.
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
        Epoch: Epoch
        SimulationInputs: FastData array
        ClockedComponents: FastComponent array
        CombinationalComponents: FastComponent array
        }

    type GatherData = {
        /// existing Issie data structure representing circuit for simulation - generated by runCanvasStateChecksAndBuildGraph
        Simulation: SimulationGraph
        InputLinks: Map<ComponentId,ComponentId * InputPortNumber>
        OutputLinks: Map<ComponentId * OutputPortNumber, ComponentId>
        AllComponents: Map<ComponentId,SimulationComponent * ComponentId list> // maps to component and its path in the graph
        GInputs: ComponentId list
        OrderedComponents: ComponentId list
        }
    /// Create an initial gatherdata object with inputs, non-ordered components, simulationgraph, etc
    /// This must explore graph recursively extracting all the initial information.
    /// Custom components are scanned and links added, one for each input and output
    let startGather (graph: SimulationGraph) : GatherData = failwithf "Not implemented"

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
    let writeSimulationData (fSim: FastSimulation) (epoch: Epoch) (graph: SimulationGraph) : SimulationGraph = failwithf "Not Implemented"

    /// run a simulation for a given number of steps
    let runSimulation (steps: int) (fSim: FastSimulation) : FastSimulation = failwithf "Not Implemented"
    

