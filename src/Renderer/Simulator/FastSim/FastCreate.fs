module FastCreate

open CommonTypes
open TimeHelpers
open SimGraphTypes
open SimTypes
open SynchronousUtils
open NumberHelpers
open Helpers

//------------------------------------------------------------------------------//
//------------Functions To Create Fast Simulation Data Structures---------------//
//------------------------------------------------------------------------------//

//-----------------------------Fast Simulation Creation-------------------------//

let inline assertThat cond msg =
    if not cond then
        failwithf "what? assert failed: %s" msg

let emptyGather =
    { Labels = Map.empty
      Simulation = Map.empty
      CustomInputCompLinks = Map.empty
      CustomOutputCompLinks = Map.empty
      CustomOutputLookup = Map.empty
      AllComps = Map.empty }

let emptyFastSimulation diagramName =

    { ClockTick = 0
      TotalArraySizePerStep = 0
      MaxArraySize = 0 // must be larger than max number of wavesim clocks
      FGlobalInputComps = Array.empty
      FConstantComps = Array.empty
      FClockedComps = Array.empty
      FOrderedComps = Array.empty
      FIOActive = Map.empty
      FIOLinks = []
      FComps = Map.empty
      FCustomComps = Map.empty
      WaveComps = Map.empty
      FSComps = Map.empty
      FCustomOutputCompLookup = Map.empty
      G = emptyGather
      NumStepArrays = 0 // this will be overwritten by createInitFastCompPhase
      Drivers = Array.empty
      WaveIndex = Array.empty
      ConnectionsByPort = Map.empty
      ComponentsById = Map.empty
      SimulatedCanvasState = []
      SimulatedTopSheet = diagramName }

let simulationPlaceholder = emptyFastSimulation ""
let getFid (cid: ComponentId) (ap: ComponentId list) =
    let ff (ComponentId Id) = Id
    (cid, ap)

let getPortNumbers (sc: SimulationComponent) =
    let ins, outs =
        match sc.Type with
        | Constant1 _
        | Constant _
        | CounterNoEnableLoad _ -> 0, 1
        | Input1 _
        | Output _
        | Viewer _
        | BusSelection _
        | BusCompare _
        | BusCompare1 _
        | Not
        | DFF
        | Register _
        | IOLabel
        | ROM1 _
        | AsyncROM1 _
        | NbitsNot _
        | NbitSpreader _
        | CounterNoLoad _ -> 1, 1
        | NotConnected -> 1, 0
        | MergeWires
        | NbitsXor _
        | NbitsOr _
        | NbitsAnd _
        | RegisterE _
        | DFFE
        | CounterNoEnable _ -> 2, 1
        | SplitWire _ -> 1, 2
        | Mux2
        | NbitsAdderNoCout _
        | Counter _ -> 3, 1
        | Mux4 -> 5, 1
        | Mux8 -> 9, 1
        | NbitsAdder _ -> 3, 2
        | NbitsAdderNoCin _ -> 2, 2
        | NbitsAdderNoCinCout _
        | Shift _ -> 2, 1
        | AsyncRAM1 _
        | RAM1 _ -> 3, 1
        | Decode4 -> 2, 4
        | Demux2 -> 2, 2
        | Demux4 -> 2, 4
        | Demux8 -> 2, 8
        // | And
        // | Or
        // | Xor
        // | Nand
        // | Nor
        // | Xnor -> 2, 1
        | GateN (_, n) -> n, 1
        | MergeN n -> n, 1
        | SplitN (n, _, _) -> 1, n
        | Custom ct -> ct.InputLabels.Length, ct.OutputLabels.Length
        | AsyncROM _
        | RAM _
        | ROM _ -> failwithf "legacy component type is not supported"
        | Input _ -> failwithf "Legacy Input component types should never occur"

    ins, outs

let compType t =
    match t with
    | Custom c -> c.Name
    | _ -> t.ToString()

let findBigIntState (fc: FastComponent) =
    match fc.FType with
    // 1-bit components
    | Not
    | GateN _
    | DFF
    | DFFE -> false, None
    // N-bits components
    | Constant(w, _)
    | Constant1(w, _, _)
    | Input w
    | Input1(w, _)
    | Output w
    | Viewer w
    | NbitsAnd w
    | NbitsOr w
    | NbitsNot w
    | NbitsAdder w
    | NbitsAdderNoCin w
    | NbitsAdderNoCout w
    | NbitsAdderNoCinCout w
    | NbitSpreader w
    | NbitsXor(w, _)
    | Register w
    | RegisterE w
    | Counter w
    | CounterNoLoad w
    | CounterNoEnable w
    | CounterNoEnableLoad w
    | BusCompare(w, _)
    | BusCompare1(w, _, _) -> w > 32, None
    // Components with implicit width
    | IOLabel
    | Mux2
    | Mux4
    | Mux8
    | Demux2
    | Demux4
    | Demux8 -> fc.OutputWidth 0 > 32, None
    | NotConnected -> false, None
    // Components with variable width
    | MergeWires ->
        fc.InputWidth 0 > 32
        || fc.InputWidth 1 > 32
        || fc.OutputWidth 0 > 32,
        Some
            { InputIsBigInt = [| fc.InputWidth 0 > 32; fc.InputWidth 1 > 32 |]
              OutputIsBigInt = [| fc.OutputWidth 0 > 32 |] }
    | MergeN n -> 
        fc.OutputWidth 0 > 32
        || List.exists (fun n -> fc.InputWidth n > 32) [0..n-1],
        Some 
            { InputIsBigInt = Array.ofList(List.map (fun n -> fc.InputWidth n > 32) [0..n-1])
              OutputIsBigInt = [| fc.OutputWidth 0 > 32 |] }
    | SplitN (n, _, _) -> 
        fc.InputWidth 0 > 32
        || List.exists (fun n -> fc.OutputWidth n > 32) [0..n-1], 
        Some { 
            InputIsBigInt = [| fc.InputWidth 0 > 32 |] 
            OutputIsBigInt = Array.ofList(List.map (fun n -> fc.OutputWidth n > 32) [0..n-1])
                }
    | SplitWire _ ->
        fc.InputWidth 0 > 32,
        Some
            { InputIsBigInt = [| fc.InputWidth 0 > 32 |]
              OutputIsBigInt = [| fc.OutputWidth 0 > 32; fc.OutputWidth 1 > 32 |] }
    | BusSelection _ ->
        fc.InputWidth 0 > 32,
        Some
            { InputIsBigInt = [| fc.InputWidth 0 > 32 |]
              OutputIsBigInt = [| fc.OutputWidth 0 > 32 |] }
    | AsyncROM1 m
    | ROM1 m
    | RAM1 m
    | AsyncRAM1 m ->
        match m.WordWidth > 32, m.AddressWidth > 32 with
        | false, false -> false, None
        | false, true -> true, Some { InputIsBigInt = [| true |]; OutputIsBigInt = [| false |] }
        | true, false -> true, Some { InputIsBigInt = [| false |]; OutputIsBigInt = [| true |] }
        | true, true -> true, Some { InputIsBigInt = [| true |]; OutputIsBigInt = [| true |] }
    // Custom components
    | Custom c -> false, None // NOTE - custom components will not be reduced, so we don't need to worry about their width
    // Legacy components
    | Decode4
    | Shift _
    | AsyncROM _
    | ROM _
    | RAM _ -> failwith "Legacy components, not Implemented"

let mutable stepArrayIndex = -1

let makeStepArray (arr: 'T array) : StepArray<'T> =
    stepArrayIndex <- stepArrayIndex + 1
    { Step = arr; Index = stepArrayIndex }

let makeIOArray size =
    stepArrayIndex <- stepArrayIndex + 1
    { FDataStep = Array.create 2 (Data <| emptyFastData) // NOTE - 2 should be enough for FData arrays as they are only used in Truthtable
      UInt32Step = Array.empty
      BigIntStep = Array.empty
      Width = 0
      Index = stepArrayIndex }

let makeIOArrayW w size =
    stepArrayIndex <- stepArrayIndex + 1
    match w with
    | w when w <= 32 ->
        { FDataStep = Array.create 2 (Data <| { Width = w; Dat = Word 0u }) // NOTE - 2 should be enough for FData arrays as they are only used in Truthtable
          UInt32Step = Array.create size 0u
          BigIntStep = Array.empty
          Width = w
          Index = stepArrayIndex }
    | _ ->
        { FDataStep = Array.create 2 (Data <| { Width = w; Dat = BigWord 0I }) // NOTE - 2 should be enough for FData arrays as they are only used in Truthtable
          UInt32Step = Array.empty
          BigIntStep = Array.create size 0I
          Width = w
          Index = stepArrayIndex }

/// create a FastComponent data structure with data arrays from a SimulationComponent.
/// numSteps is the number of past clocks data kept - arrays are managed as circular buffers.
let createFastComponent (maxArraySize: int) (sComp: SimulationComponent) (accessPath: ComponentId list) =
    let inPortNum, outPortNum = getPortNumbers sComp
    // dummy arrays wil be replaced by real ones when components are linked after being created
    let ins =
        [| 0 .. inPortNum - 1 |]
        |> Array.map (fun n ->
            match sComp.Type with
            | Input1 (width, defVal)->
                // special case - add real input arrays now to avoid excption during initialse
                makeIOArrayW width maxArraySize
            | _ ->
                makeIOArray maxArraySize)
    
    //printfn "Type: %A; ins array: %A" sComp.Type ins
    let outs =
        match sComp.Type, sComp.OutputWidths.Length with
        | IOLabel, 0 -> [| makeIOArray maxArraySize |] // NOTE - create dumpy Outputs array for inavtive IOLabels
        | _ ->
            sComp.OutputWidths
            |> Array.map (fun w -> makeIOArrayW w maxArraySize)
    //printfn "Type: %A; outs array: %A" sComp.Type outs

    let state =
        if couldBeSynchronousComponent sComp.Type then
            Some(Array.create maxArraySize NoState)
        else
            None

    let fId = getFid sComp.Id accessPath

    let reduceIfHybrid sc ipn =
        if isHybridComponent sc.Type then
            [ 0..ipn ]
            |> List.sumBy (fun ipn ->
                getHybridComponentAsyncOuts sc.Type (InputPortNumber ipn)
                |> function
                    | None
                    | Some [] -> 0
                    | Some _ -> 1)
        else
            ipn

    match sComp.Type with
    | Input1(w, d) -> ins[0] <- { ins[0] with Width = w }
    | _ -> ()

    { UseBigInt = false // dump value, will be set when Input Widths are avaiable after linkFastComponents
      BigIntState = None // dump value, will be set when Input Widths are avaiable after linkFastComponents
      State = Option.map makeStepArray state
      SimComponent = sComp
      fId = fId
      cId = sComp.Id
      FType = sComp.Type
      AccessPath = accessPath
      SheetName = []
      Touched = false
      DrivenComponents = []
      NumMissingInputValues = reduceIfHybrid sComp inPortNum
      InputLinks = ins
      InputDrivers = Array.create inPortNum None
      Outputs = outs
      FullName = ""
      FLabel = extractLabel sComp.Label
      VerilogOutputName = Array.create outPortNum ""
      VerilogComponentName = ""
      Active =
        match sComp.Type with
        | IOLabel -> false
        | _ -> true }

/// Create an initial flattened and expanded version of the simulation graph with inputs, non-ordered components, simulationgraph, etc
/// This must explore graph recursively extracting all the initial information.
/// Custom components are scanned and links added, one for each input and output
let rec private createFlattenedSimulation (ap: ComponentId list) (graph: SimulationGraph) =
    let graphL = Map.toList graph
    let allComps =
        graphL
        |> List.map (fun (cid, comp) -> (cid, ap), (comp, ap))

    let labels =
        List.map (fun (cid, comp) -> cid, ((fun (ComponentLabel s) -> s) comp.Label)) graphL

    let topGather =
        { Labels = labels
          AllCompsT = allComps
          CustomInputCompLinksT = []
          CustomOutputCompLinksT = [] }

    let customComps =
        graphL
        |> List.collect (fun (cid, comp) ->
            match comp.Type, comp.CustomSimulationGraph with
            | Custom ct, Some csg -> [ cid, ct, csg ]
            | _ -> [])

    let insideCustomGathers =
        customComps
        |> List.map (fun (cid, ct, csg) ->
            let ap' = ap @ [ cid ]
            let gatherT = createFlattenedSimulation ap' csg
            let compsInCustomComp = Map.toList csg |> List.map snd

            /// Function making links to custom component input or output components
            /// For those component types selected by compSelectFun (inputs or ouputs):
            /// Link label and width (which will also be the custom comp port label and width)
            /// to the Id of the relevant Input or output component.
            let getCustomNameIdsOf compSelectFun =
                compsInCustomComp
                |> List.filter (fun comp -> compSelectFun comp.Type)
                |> List.map (fun comp ->
                    (comp.Label,
                     match comp.Type with
                     | Input1(n, _) -> n
                     | Output n -> n
                     | _ -> -1),
                    comp.Id)

            let outputs = getCustomNameIdsOf isOutput

            /// maps Output component Id to corresponding Custom component Id & output port
            let outLinks =
                ct.OutputLabels
                |> List.mapi (fun i (lab, labOutWidth) ->
                    let out =
                        List.find (fun (k, v) -> k = (ComponentLabel lab, labOutWidth)) outputs
                        |> snd

                    (out, ap'), ((cid, ap), OutputPortNumber i))

            let inputs = getCustomNameIdsOf isInput

            /// maps Custom Component Id and input port number to corresponding Input Component Id
            let inLinks =
                ct.InputLabels
                |> List.mapi (fun i (lab, labOutWidth) ->
                    let inp =
                        List.find (fun (k, v) -> k = (ComponentLabel lab, labOutWidth)) inputs
                        |> snd

                    (((cid, ap), InputPortNumber i), (inp, ap')))

            { CustomInputCompLinksT = inLinks @ gatherT.CustomInputCompLinksT
              CustomOutputCompLinksT = outLinks @ gatherT.CustomOutputCompLinksT
              Labels = labels @ gatherT.Labels
              AllCompsT = gatherT.AllCompsT })

    (topGather, insideCustomGathers)
    ||> List.fold (fun total thisGather ->
        { CustomInputCompLinksT =
            thisGather.CustomInputCompLinksT
            @ total.CustomInputCompLinksT
          CustomOutputCompLinksT =
            thisGather.CustomOutputCompLinksT
            @ total.CustomOutputCompLinksT
          Labels = thisGather.Labels @ total.Labels
          AllCompsT = thisGather.AllCompsT @ total.AllCompsT

        })

/// Convert the data in the a SimulationGraph, created from the circuit
/// into a final GatherData structure suitable for simulation stored as a set of maps
/// Calls createFlattenedSimulation as first step.
let gatherSimulation (graph: SimulationGraph) =
    let startTime = getTimeMs ()

    createFlattenedSimulation [] graph
    |> (fun g ->
        { Simulation = graph
          CustomInputCompLinks = Map.ofList g.CustomInputCompLinksT
          CustomOutputCompLinks = Map.ofList g.CustomOutputCompLinksT
          Labels = Map.ofList g.Labels
          AllComps = Map.ofList g.AllCompsT
          CustomOutputLookup = Map.ofList (List.map (fun (k, v) -> v, k) g.CustomOutputCompLinksT) })
    |> instrumentInterval "gatherGraph" startTime

/// Add one driver changing the fs.Driver array reference.
/// Return a WaveIndex reference.
/// WaveIndex refrences are bound to specific component ports
/// and not unique per driver.
let addComponentWaveDrivers (f: FastSimulation) (fc: FastComponent) (pType: PortType) =
    let makeWaveIndex index pn pType arr =
        { SimArrayIndex = index; Id = fc.fId; PortType = pType; PortNumber = pn }

    let addStepArray pn index stepA =
        f.Drivers[index] <-
            Some
            <| Option.defaultValue { Index = index; DriverData = stepA; DriverWidth = 0 } f.Drivers[index]

        let addWidth w optDriver =
            Option.map (fun d -> { d with DriverWidth = w }) optDriver

        fc.Outputs[pn]
        |> (fun output -> f.Drivers[index] <- addWidth output.Width f.Drivers[index])

    let ioLabelIsActive fc =
        f.FIOActive[ComponentLabel fc.FLabel, snd fc.fId].fId
        <> fc.fId

    match pType with
    | PortType.Output -> fc.Outputs
    | PortType.Input -> fc.InputLinks
    |> Array.mapi (fun pn stepA ->
        let index = stepA.Index

        let addDriver, addWave =
            match fc.FType, pType with
            | IOLabel, PortType.Input
            | Input1 _, PortType.Input
            | Viewer _, PortType.Input
            | NotConnected, PortType.Input
            | Output _, PortType.Input -> false, false
            | Constant1 _, _ -> // special case because constant output drivers are needed!
                true, false
            | IOLabel, _ when ioLabelIsActive fc -> false, false
            | _ -> true, true

        if pType = PortType.Output && addDriver then
            addStepArray pn index stepA

        if addWave then
            match fc.FType with
            | SplitWire _
            | BusSelection _
            | MergeWires
            | MergeN _
            | SplitN _
            | Constant1 _ -> [||]
            | Output _ when fc.SubSheet <> [] -> [||]
            | Input1 _ when fc.SubSheet <> [] -> [||]
            | _ -> [| makeWaveIndex index pn pType stepA |]
        else
            [||])

/// Called after the fs.Drivers array is created.
/// waveComps must contain all components that can be viewed in the wave simulation.
/// This function mutates fs.Drivers adding the correct arrays where
/// these are used. In some cases an array may never be used and therefore is not added.
/// In parallel with this, the function returns an array of WaveIndexT records that
/// reference component ports which can be viewed in a wave simulation.
/// Every WaveIndex references an element of fs.Drivers from which the simulation data is found.
let addWaveIndexAndDrivers (waveComps: Map<FComponentId, FastComponent>) (f: FastSimulation) : WaveIndexT array =
    let comps = waveComps |> Map.toArray |> Array.map snd

    let addDrivers pType =
        Array.collect (fun fc -> addComponentWaveDrivers f fc pType)

    let outs = addDrivers PortType.Output comps
    let ins = addDrivers PortType.Input comps
    Array.append outs ins |> Array.concat

/// Changes all the custom component in and out StepArray links so they point to the correct drivers.
/// (fid, fc) must be a custom component.
/// Called after the simulation has been fully constructed and linked.
let linkFastCustomComponentsToDriverArrays (fs: FastSimulation) (fid: FComponentId) (fc: FastComponent) : Unit =
    let cid, ap' = fid
    let ap = ap' @ [ cid ]

    let ct =
        match fc.FType with
        | Custom ct -> ct
        | _ -> failwithf "linkFastCustomComponent must be called with a custom component"

    let graph =
        match fc.SimComponent.CustomSimulationGraph with
        | Some g -> g
        | None -> failwithf "What? Can't find customSimulationGraph"

    graph
    |> Map.iter (fun cid sc ->
        match sc.Type with
        | Input1(w, _) ->
            let portNum =
                ct.InputLabels
                |> List.indexed
                |> List.find (fun (i, (lab, _)) -> (ComponentLabel lab = sc.Label))
                |> fst

            fc.InputLinks[portNum] <- fs.FComps[cid, ap].Outputs[0]
        | Output w ->
            let portNum =
                ct.OutputLabels
                |> List.indexed
                |> List.find (fun (i, (lab, _)) -> ComponentLabel lab = sc.Label)
                |> fst

            fc.Outputs[portNum] <- fs.FComps[cid, ap].InputLinks[0]
        | _ -> ())

/// Adds WaveComps, Drivers and WaveIndex fields to a fast simulation.
/// For use by waveform Simulator.
/// Needs to be run after widths are calculated.
let addWavesToFastSimulation (fs: FastSimulation) : FastSimulation =
    fs.FCustomComps
    |> Map.iter (linkFastCustomComponentsToDriverArrays fs)

    let waveComps =
        (fs.FComps, fs.FCustomComps)
        ||> Map.fold (fun s fid fc -> Map.add fid fc s)
    // Add WaveComps
    // Create null driver array large enough for all created step arrays
    // each step array is given a sequentially generated id as it is created
    // however, some of these arrays will never be used and end up as None
    // elements of the driver array.
    { fs with WaveComps = waveComps; Drivers = Array.create fs.NumStepArrays None }
    // Generate all waves, add (mutably) step arrays to driver array replacing None
    // by Some array in the index unique to the array added as these are needed
    // by wave component ports.
    // One array can be referenced by multiple ports.
    // The mutable changes to fs.Drivers here are write-once, from None to Some array.
    |> (fun x ->
        x)
    |> (fun fs -> { fs with WaveIndex = addWaveIndexAndDrivers waveComps fs })
/// This function will create the initial FastSimulation data structure.
/// It may not complete this. In which case fields NumCreateSteps and NumCreateStepsDone will not be equal.
let rec createInitFastCompPhase (simulationArraySize: int) (g: GatherData) (f: FastSimulation) =
    let numSteps = simulationArraySize
    stepArrayIndex <- -1
    let start = getTimeMs ()
    //printfn $"Creating init fast comp phase of sim with {numSteps} array size"

    let makeFastComp fid =
        let comp, ap = g.AllComps[fid]
        let fc = createFastComponent numSteps comp ap

        { fc with FullName = g.getFullName fid; SheetName = g.getSheetName fid }

    let comps, customComps =
        ((Map.empty, Map.empty), g.AllComps)
        ||> Map.fold (fun (m, mc) cid (comp, ap) ->
            if isCustom comp.Type then
                m, Map.add (comp.Id, ap) (makeFastComp (comp.Id, ap)) mc
            else
                Map.add (comp.Id, ap) (makeFastComp (comp.Id, ap)) m, mc)

    let customOutLookup =
        g.CustomOutputCompLinks
        |> Map.toList
        |> List.map (fun (a, b) -> b, a)
        |> Map.ofList

    instrumentTime "createInitFastCompPhase" start

    { f with
        FComps = comps
        FCustomComps = customComps
        MaxArraySize = simulationArraySize
        FSComps = g.AllComps
        FCustomOutputCompLookup = customOutLookup
        NumStepArrays = stepArrayIndex + 1
        Drivers = Array.empty }

/// Has side effect of making IOLabels of same name (in the same graph) all use same output array
/// this means that an input to any one will produce an output on all, for no effort.
/// IOLabels without driven inputs that are thus not used are later on flagged inactive
/// they must not be reduced, and will not be included in the ordered component list
let private reLinkIOLabels (fs: FastSimulation) =
    // Go through all the components driven by IOLabels and link them from the active label
    // at this point exactly one out of every labelled set will be active, and contained in FIOActive
    fs.FIOLinks
    |> List.iter (fun ((fcDriven, InputPortNumber ipn), ioDriver) ->
        let labKey = ioDriver.SimComponent.Label, ioDriver.AccessPath
        let fcActiveDriver = fs.FIOActive[labKey]
        fcDriven.InputLinks[ipn] <- fcActiveDriver.Outputs[0]
        fcDriven.InputDrivers[ipn] <- Some(fcActiveDriver.fId, OutputPortNumber 0)
        // DrivenComponents must only include asynchronous drive paths on hybrid components
        // on clocked components, or combinational components, it can include all drive paths
        match getHybridComponentAsyncOuts fcDriven.FType (InputPortNumber ipn) with
        | None
        | Some(_ :: _) -> fcActiveDriver.DrivenComponents <- fcDriven :: fcActiveDriver.DrivenComponents
        | _ -> ()

        ioDriver.Outputs[0] <- fcActiveDriver.Outputs[0])

/// Use the Outputs links from the original SimulationComponents in gather to link together the data arrays
/// of the FastComponents.
/// InputLinks[i] array is set equal to the correct driving Outputs array so that Input i reads the data reduced by the
/// correct output of the component that drives it.
/// The main work is dealing with custom components which represent whole design sheets with recursively defined component graphs
/// The custom component itself is not linked, and does not exist as a simulatable FastComponent.
/// Note: custom components are linked in later as unsimulatable placeholders to allow wave simulation to access ports
/// Instead its CustomSimulationGraph Input and Output components
/// are linked to the components that connect the corresponding inputs and outputs of the custom component.
let linkFastComponents (g: GatherData) (f: FastSimulation) =
    let start = getTimeMs ()
    let outer = List.rev >> List.tail >> List.rev
    let sComps = g.AllComps
    let fComps = f.FComps

    let getSComp (cid, ap) =
        let x = Map.tryFind (cid, ap) sComps

        match x with
        | None -> failwithf $"Error in linkFastComponents: can't find\n---{cid}\n----{ap}\n"
        | Some comp -> fst comp

    let apOf fid = fComps[fid].AccessPath

    /// This function recursively propagates a component output across Custom component boundaries to find the
    ///
    let rec getLinks ((cid, ap): FComponentId) (opn: OutputPortNumber) (ipnOpt: InputPortNumber option) =
        let sComp = getSComp (cid, ap)
        //printfn "Getting links: %A %A %A" sComp.Type opn ipnOpt
        match isOutput sComp.Type, isCustom sComp.Type, ipnOpt with
        | true, _, None when apOf (cid, ap) = [] -> [||] // no links in this case from global output
        | true, _, None ->
            //printfn "checking 1:%A %A" (g.getFullName(cid,ap)) (Map.map (fun k v -> g.getFullName k) g.CustomOutputCompLinks)
            let fid, opn = g.CustomOutputCompLinks[cid, ap]
#if ASSERTS
            assertThat (isCustom (fst sComps[fid]).Type) "What? this should be a custom component output"
#endif
            getLinks fid opn None // go from inner output to CC output and recurse
        | false, true, Some ipn ->
            //printfn "checking 2:%A:IPN<%A>" (g.getFullName(cid,ap)) ipn
            //printfn "CustomInCompLinks=\n%A" (Map.map (fun (vfid,vipn) fid ->
            //sprintf "%A:%A -> %A\n" (g.getFullName vfid) vipn (g.getFullName fid) ) g.CustomInputCompLinks |> mapValues)
            //printfn "Done"
            [| g.CustomInputCompLinks[(cid, ap), ipn], opn, InputPortNumber 0 |] // go from CC input to inner input: must be valid
        | _, false, Some ipn -> [| (cid, ap), opn, ipn |] // must be a valid link
        | false, _, None ->
            sComp.Outputs
            |> Map.toArray
            |> Array.filter (fun (opn', _) -> opn' = opn)
            |> Array.collect (fun (opn, lst) ->
                lst
                |> List.toArray
                |> Array.collect (fun (cid, ipn) -> getLinks (cid, ap) opn (Some ipn)))

        | x -> failwithf "Unexpected link match: %A" x

    let mutable linkCheck: Map<(FComponentId * InputPortNumber), (FComponentId * OutputPortNumber)> =
        Map.empty

    f.FComps
    |> Map.iter (fun fDriverId fDriver ->
        fDriver.Outputs
        |> Array.iteri (fun iOut _ ->
            getLinks fDriverId (OutputPortNumber iOut) None
            |> Array.map (fun (fid, _, ip) -> fid, iOut, ip)
            |> Array.iter (fun (fDrivenId, opn, (InputPortNumber ipn)) ->
                let linked = Map.tryFind (fDrivenId, InputPortNumber ipn) linkCheck

                match linked with
                | None -> ()
                | Some(fid, opn) -> failwithf "Multiple linkage: (previous driver was %A,%A)" (g.getFullName fid) opn

                linkCheck <- Map.add (fDrivenId, InputPortNumber ipn) (fDriverId, OutputPortNumber opn) linkCheck
                let fDriven = f.FComps[fDrivenId]
                let (_, ap) = fDrivenId

                // we have a link from fDriver to fDriven

                if isIOLabel fDriven.FType then
                    // fDriven is a driven label of a set of IOlabels
                    let labelKey = fDriven.SimComponent.Label, ap

                    if not (Map.containsKey labelKey f.FIOActive) then
                        // Make this then unique driven label in the fast simulation
                        f.FIOActive <- Map.add labelKey fDriven f.FIOActive
                        fDriven.Active <- true

                if isIOLabel fDriver.FType then
                    // we do not yet know which label will be active, so record all links from
                    // labels for later resolution
                    f.FIOLinks <-
                        ((fDriven, InputPortNumber ipn), fDriver)
                        :: f.FIOLinks
                else
                    // if driver is not IO label make the link now
                    fDriven.InputLinks[ipn] <- fDriver.Outputs[opn]
                    // DrivenComponents must only include asynchronous drive paths on hybrid components
                    // on clocked components, or combinational components, it can include all drive paths
                    match getHybridComponentAsyncOuts fDriven.FType (InputPortNumber ipn) with
                    | None
                    | Some(_ :: _) -> fDriver.DrivenComponents <- fDriven :: fDriver.DrivenComponents
                    | _ -> ()

                    fDriven.InputDrivers[ipn] <- Some(fDriver.fId, OutputPortNumber opn))))

    reLinkIOLabels f
    instrumentTime "linkFastComponents" start
    f

// This function is called after linkFastComponents (when width of IO of all components are resolved) to resolve the UseBigInt and BigIntState fields of all components
let determineBigIntState (f: FastSimulation) =
    f.FComps
    |> Map.iter (fun _ fc ->
        let (u, state) = findBigIntState fc
        fc.UseBigInt <- u
        fc.BigIntState <- state)
    f
