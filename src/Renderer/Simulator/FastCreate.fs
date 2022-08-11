module FastCreate
open CommonTypes
open TimeHelpers
open SimulatorTypes
open SynchronousUtils
open NumberHelpers


//------------------------------------------------------------------------------//
//------------Functions To Create Fast Simulation Data Structures---------------//
//------------------------------------------------------------------------------//

//-----------------------------Fast Simulation Creation-------------------------//

let inline assertThat cond msg = 
    if not cond
    then failwithf "what? assert failed: %s" msg

let makeStepArray (arr: 'T array) : StepArray<'T> = { Step = arr }

let emptyGather =
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

let getPathIds (cid, ap) =
    let rec getPath ap =
        match ap with
        | [] -> []
        | cid :: rest -> (cid, List.rev rest) :: getPath rest

    getPath (List.rev ap) |> List.rev


let getFid (cid: ComponentId) (ap: ComponentId list) =
    let ff (ComponentId Id) = Id
    (cid, ap)


let getPortNumbers (sc: SimulationComponent) =
    let ins,outs =
        match sc.Type with
        | Constant1 _ | Constant _ ->
            0,1
        | Input1 _
        | Output _
        | Viewer _ 
        | BusSelection _
        | BusCompare _
        | Not
        | DFF
        | Register _
        | IOLabel  
        | ROM1 _ 
        | AsyncROM1 _
        | NbitsNot _ ->
            1,1
        | MergeWires
        | NbitsXor _
        | NbitsAnd _
        | RegisterE _
        | DFFE -> 
            2,1
        | SplitWire _ -> 
            1,2
        | Mux2 _ -> 
            3,1
        | Mux4 _ ->
            5,1
        | Mux8 _ ->
            9,1
        | NbitsAdder _ -> 
            3,2
        | AsyncRAM1 _
        | RAM1 _ -> 
            2,1
        | Decode4 -> 
            2,4
        | Demux2 -> 
            2,2
        | Demux4 -> 
            2,4
        | Demux8 ->
            2,8
        | Not | And | Or | Xor | Nand | Nor | Xnor -> 2,1
        | Custom _ -> failwithf "Custom components should not occur in fast simulation"
        | AsyncROM _ | RAM _ | ROM _ -> failwithf "legacy component type is not supported"
        | Input _ -> failwithf "Legacy Input component types should never occur"

    ins, outs

let getOutputWidths (sc: SimulationComponent) (wa: int option array) =

    let putW0 w = wa[0] <- Some w
    let putW1 w = wa[1] <- Some w
    let putW2 w = wa[2] <- Some w
    let putW3 w = wa[3] <- Some w

    match sc.Type with
    | ROM _ | RAM _ | AsyncROM _ -> 
        failwithf "What? Legacy RAM component types should never occur"
    | Input _ ->
        failwithf "Legacy Input component types should never occur"
    | Input1 (w, _)
    | Output w
    | Viewer w
    | Register w
    | RegisterE w
    | SplitWire w
    | BusSelection (w, _)
    | Constant1 (w, _,_)
    | Constant (w,_)
    | NbitsAnd w
    | NbitsNot w
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
    | RAM1 mem
    | AsyncRAM1 mem -> putW0 mem.WordWidth
    | Custom _ -> ()
    | DFF
    | DFFE -> putW0 1
    | Decode4 ->
        putW0 1
        putW1 1
        putW2 1
        putW3 1
    | Demux2
    | Demux4
    | Demux8
    | Mux2
    | Mux4
    | Mux8
    | IOLabel
    | MergeWires -> ()

    wa


/// create a FastComponent data structure with data arrays from a SimulationComponent.
/// numSteps is the number of past clocks data kept - arrays are managed as circular buffers.
let createFastComponent (numSteps: int) (sComp: SimulationComponent) (accessPath: ComponentId list) =
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
            | [], Input1 (width, defaultVal) -> List.replicate width Zero
            | _ -> []

        [| 0 .. inPortNum - 1 |]
        |> Array.map (fun i -> (Array.create (numSteps + 1) dat))

    let state =
        if couldBeSynchronousComponent sComp.Type then
            Some(Array.create numSteps NoState)
        else
            None

    let fId = getFid sComp.Id accessPath

    let reduceIfHybrid sc ipn =
        if isHybridComponent sc.Type then
            [0..ipn]
            |> List.sumBy (fun ipn -> 
                getHybridComponentAsyncOuts sc.Type (InputPortNumber ipn)
                |> function | None | Some [] -> 0 | Some _ -> 1)
        else ipn

    { OutputWidth = getOutputWidths sComp (Array.create outPortNum None)
      State = Option.map makeStepArray state
      SimComponent = sComp
      fId = fId
      cId = sComp.Id
      FType = sComp.Type
      AccessPath = accessPath
      Touched = false
      DrivenComponents = []
      NumMissingInputValues = reduceIfHybrid sComp inPortNum
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
/// No longer used now arrays are circular?
let extendFastComponent (numSteps: int) (fc: FastComponent) =
    let oldNumSteps = fc.Outputs[0].Step.Length


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
                            oldArr[i]
                        else
                            dat)

            arr.Step <- a

        let inPortNum, outPortNum = getPortNumbers fc.SimComponent

        // Input inputs at top level are a special case not mapped to outputs.
        // They must be separately extended.
        match fc.FType, fc.AccessPath with
        | Input1 _, [] -> extendArray fc.InputLinks[0] fc.InputLinks[0].Step[oldNumSteps - 1]
        | _ -> ()

        [| 0 .. outPortNum - 1 |]
        |> Array.iter (fun n -> extendArray fc.Outputs[n] emptyFastData)

        Option.iter
            (fun (stateArr: StepArray<SimulationComponentState>) ->
                extendArray stateArr stateArr.Step[oldNumSteps - 1])
            fc.State


/// extends the simulation data arrays of all components to allow more steps
/// also truncates fast simulation to prevent memory overuse.
let extendFastSimulation (numSteps: int) (fs: FastSimulation) =
    if numSteps + 1 < fs.MaxStepNum then
        ()
    else
        [| fs.FOrderedComps
           fs.FConstantComps
           Array.filter (fun fc -> not (isHybridComponent fc.FType)) fs.FClockedComps
           fs.FGlobalInputComps |]
        |> Array.iter (Array.iter (extendFastComponent numSteps))

        fs.MaxStepNum <- numSteps


/// Create an initial flattened and expanded version of the simulation graph with inputs, non-ordered components, simulationgraph, etc
/// This must explore graph recursively extracting all the initial information.
/// Custom components are scanned and links added, one for each input and output
let rec private createFlattenedSimulation (ap: ComponentId list) (graph: SimulationGraph) =
    let graphL = Map.toList graph
    let allComps = 
        graphL
        |> List.map (fun (cid,comp) ->  (cid, ap),(comp, ap))
    let labels = List.map (fun (cid,comp) -> cid, ((fun (ComponentLabel s) -> s) comp.Label)) graphL
    let topGather =
        {
            Labels = labels
            AllCompsT = allComps
            CustomInputCompLinksT = []
            CustomOutputCompLinksT = []
        }
    let customComps = 
        graphL
        |> List.collect ( fun (cid,comp) -> 
            match comp.Type, comp.CustomSimulationGraph with 
            | Custom ct, Some csg -> [cid, ct, csg] 
            | _ -> [])
    let insideCustomGathers = 
        customComps
        |> List.map (fun  (cid, ct, csg) ->
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
                    |> List.map
                        (fun comp ->
                            (comp.Label,
                                match comp.Type with
                                | Input1 (n, _) -> n
                                | Output n -> n
                                | _ -> -1),
                            comp.Id)

                let outputs = getCustomNameIdsOf isOutput
                /// maps Output component Id to corresponding Custom component Id & output port
                let outLinks =
                    ct.OutputLabels
                    |> List.mapi
                        (fun i (lab, labOutWidth) ->
                            let out = 
                                List.find (fun (k,v) -> k = (ComponentLabel lab, labOutWidth)) outputs
                                |> snd
                            (out, ap'), ((cid, ap), OutputPortNumber i))

                let inputs = getCustomNameIdsOf isInput
                /// maps Custom Component Id and input port number to corresponding Input Component Id
                let inLinks =
                    ct.InputLabels
                    |> List.mapi
                        (fun i (lab, labOutWidth) ->
                            let inp = 
                                List.find (fun (k,v) -> k = (ComponentLabel lab, labOutWidth)) inputs
                                |> snd
                            (((cid, ap), InputPortNumber i), (inp, ap')))
                {
                     CustomInputCompLinksT = inLinks @ gatherT.CustomInputCompLinksT
                     CustomOutputCompLinksT = outLinks @ gatherT.CustomOutputCompLinksT
                     Labels = labels @ gatherT.Labels
                     AllCompsT = gatherT.AllCompsT                      
                })
    (topGather, insideCustomGathers)
    ||> List.fold (fun total thisGather ->
        {
            CustomInputCompLinksT = thisGather.CustomInputCompLinksT @ total.CustomInputCompLinksT
            CustomOutputCompLinksT = thisGather.CustomOutputCompLinksT @ total.CustomOutputCompLinksT
            Labels = thisGather.Labels @ total.Labels
            AllCompsT = thisGather.AllCompsT @ total.AllCompsT                     

        })
/// convert the data in the flattened structure into maps for easy access
let gatherSimulation (graph: SimulationGraph) =
    let startTime = getTimeMs()
    createFlattenedSimulation [] graph
    |> (fun g ->
        { 
            Simulation = graph
            CustomInputCompLinks = Map.ofList g.CustomInputCompLinksT
            CustomOutputCompLinks = Map.ofList g.CustomOutputCompLinksT
            Labels = Map.ofList g.Labels
            AllComps = Map.ofList g.AllCompsT                     
            CustomOutputLookup = Map.ofList (List.map (fun (k,v) -> v,k) g.CustomOutputCompLinksT)
        })
    |> instrumentInterval "gatherGraph" startTime
            

let printGather (g: GatherData) =
    printfn "%d components" g.AllComps.Count

    Map.iter
        (fun (cid, ap) (comp: SimulationComponent, ap') -> printfn "%s (%A:%A): %A" (g.getFullName (cid, ap)) cid ap comp.Outputs)
        g.AllComps
    
    Map.iter
        (fun ((cid, ap),ipn) (cid', ap') -> printfn "inlink: %s -> %A" (g.getFullName (cid, ap)) (cid',ap'))
        g.CustomInputCompLinks

    Map.iter
        (fun (cid', ap') ((cid, ap),opn) -> printfn "outlink: %A -> %s" (cid',ap') (g.getFullName (cid, ap)) )
        g.CustomOutputCompLinks

let rec createInitFastCompPhase (numSteps: int) (g: GatherData) (f: FastSimulation) =
    let start = getTimeMs()
    let makeFastComp cid =
        let comp, ap = g.AllComps[cid]
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
    instrumentTime "createInitFastCompPhase" start
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
        let fcActiveDriver = fs.FIOActive[labKey]
        fcDriven.InputLinks[ipn] <- fcActiveDriver.Outputs[0]
        fcDriven.InputDrivers[ipn] <- Some (fcActiveDriver.fId, OutputPortNumber 0)
        // DrivenComponents must only include asynchronous drive paths on hybrid components
        // on clocked components, or combinational components, it can include all drive paths
        match getHybridComponentAsyncOuts fcDriven.FType (InputPortNumber ipn) with
        | None | Some (_ :: _) ->
            fcActiveDriver.DrivenComponents <- fcDriven :: fcActiveDriver.DrivenComponents
        | _ -> ()
        ioDriver.Outputs[0] <- fcActiveDriver.Outputs[0])

/// Use the Outputs links from the original SimulationComponents in gather to link together the data arrays
/// of the FastComponents.
/// InputLinks[i] array is set equal to the correct driving Outputs array so that Input i reads the data reduced by the
/// correct output of the component that drives it.
/// The main work is dealing with custom components which represent whole design sheets with recursively defined component graphs
/// The custom component itself is not linked, and does not exist as a FastComponent. Instead its CustomSimulationGraph Input and Output components
/// are linked to the components that connect the corresponding inputs and outputs of the custom component.
let linkFastComponents (g: GatherData) (f: FastSimulation) =
    let start = getTimeMs()
    let outer = List.rev >> List.tail >> List.rev
    let sComps = g.AllComps
    let fComps = f.FComps
    let getSComp (cid, ap) =
        let x = Map.tryFind (cid, ap) sComps
        match x with
        | None -> 
            failwithf $"Error in linkFastComponents: can't find\n---{cid}\n----{ap}\n"
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
                            let fDriven = f.FComps[fDrivenId]
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
                                fDriven.InputLinks[ipn] <- fDriver.Outputs[opn]
                                // DrivenComponents must only include asynchronous drive paths on hybrid components
                                // on clocked components, or combinational components, it can include all drive paths
                                match getHybridComponentAsyncOuts fDriven.FType (InputPortNumber ipn) with
                                | None | Some (_ :: _) ->
                                    fDriver.DrivenComponents <- fDriven :: fDriver.DrivenComponents
                                | _ -> ()

                                fDriven.InputDrivers[ipn] <- Some (fDriver.fId, OutputPortNumber opn)
                                )))
    reLinkIOLabels f
    instrumentTime "linkFastComponents" start
    f

