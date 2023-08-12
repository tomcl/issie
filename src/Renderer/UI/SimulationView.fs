//(*
//    SimulationView.fs
//
//    View for simulation in the right tab.
//*)
//
module SimulationView

open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props
open Elmish

open NumberHelpers
open Helpers
open TimeHelpers
open JSHelpers
open DiagramStyle
open Notifications
open PopupHelpers
open MemoryEditorView
open ModelType
open CommonTypes
open SimulatorTypes
open Extractor
open Simulator
open Sheet.SheetInterface
open DrawModelType
open ModelHelpers

open Optics
open Optics.Optic
open Optics.Operators

module Constants =
    let maxArraySize = 550
    let boxMaxChars = 34

/// save verilog file
/// TODO: the simulation error display here is shared with step simulation and also waveform simulation -
/// maybe it should be a subfunction.
let verilogOutput (vType: Verilog.VMode) (model: Model) (dispatch: Msg -> Unit) =
    printfn "Verilog output"
    match FileMenuView.updateProjectFromCanvas model dispatch, model.Sheet.GetCanvasState() with
        | Some proj, state ->
            match model.UIState with  //TODO should this be its own UI operation?
            | Some _ ->
                () // do nothing if in middle of I/O operation
            | None ->
                startCircuitSimulation 2 proj.OpenFileName (state) proj.LoadedComponents
                |> (function 
                    | Ok sim -> 
                        let path = FilesIO.pathJoin [| proj.ProjectPath; proj.OpenFileName + ".v" |]
                        printfn "writing %s" proj.ProjectPath
                        try 
                            let code = (Verilog.getVerilog vType sim.FastSim Verilog.CompilationProfile.Release)
                            FilesIO.writeFile path code
                        with
                        | e -> 
                            printfn $"Error in Verilog output: {e.Message}"
                            Error e.Message
                        |> Notifications.displayAlertOnError dispatch
                        dispatch <| ChangeRightTab Simulation
                        let note = successSimulationNotification $"verilog output written to file {path}"
                        dispatch  <| SetSimulationNotification note
                    | Error simError ->
                       printfn $"Error in simulation prevents verilog output {(errMsg simError.ErrType)}"
                       dispatch <| ChangeRightTab Simulation
                       if simError.InDependency.IsNone then
                           // Highlight the affected components and connection only if
                           // the error is in the current diagram and not in a
                           // dependency.
                           (simError.ComponentsAffected, simError.ConnectionsAffected)
                           |> SetHighlighted |> dispatch
                       Error simError
                       |> StartSimulation
                       |> dispatch)
        | _ -> () // do nothing if no project is loaded

let setFastSimInputsToDefault (fs:FastSimulation) =
    fs.FComps
    |> Map.filter (fun cid fc -> fc.AccessPath = [] && match fc.FType with | Input1 _ -> true | _ -> false)
    |> Map.map (fun cid fc -> fst cid, match fc.FType with | Input1 (w,defVal) -> (w,defVal) | _ -> failwithf "What? Impossible")
    |> Map.toList
    |> List.map (fun ( _, (cid, (w,defaultVal ))) -> 
        match w,defaultVal with
        | _, Some defaultVal -> cid, convertInt64ToFastData w (int64 defaultVal)
        | _, None -> cid, convertIntToFastData w 0u)
    |> List.iter (fun (cid, wire) -> FastRun.changeInput cid (FSInterface.IData wire) 0 fs)

let InputDefaultsEqualInputs fs (model:Model) =
    let tick = fs.ClockTick
    fs.FComps
    |> Map.filter (fun cid fc -> fc.AccessPath = [] && match fc.FType with | Input1 _ -> true | _ -> false)
    |> Map.map (fun fid fc ->
        let cid = fst fid
        if Map.containsKey cid (Optic.get SheetT.symbols_ model.Sheet) then
            let newDefault =
                if fc.OutputWidth 0 > 32 then
                    convertBigIntToInt32 fc.Outputs[0].BigIntStep[tick % fs.MaxArraySize]
                else
                    int fc.Outputs[0].UInt32Step[tick % fs.MaxArraySize]
            let typ = (Optic.get (SheetT.symbolOf_ cid) model.Sheet).Component.Type
            match typ with
            | Input1(_, Some d) -> d = newDefault
            | _ -> newDefault = 0
        else
            true)
    |> Map.values
    |> Seq.forall id

let setInputDefaultsFromInputs fs (dispatch: Msg -> Unit) =
    let setInputDefault (newDefault: int) (sym: SymbolT.Symbol) =
        let comp = sym.Component
        let comp' = 
            let ct =
                match comp.Type with 
                | Input1(w,defVal) -> Input1(w,Some newDefault)
                | x -> x
            {comp with Type = ct}
        {sym with Component = comp'}
    let tick = fs.ClockTick
    fs.FComps
    |> Map.filter (fun cid fc -> fc.AccessPath = [] && match fc.FType with | Input1 _ -> true | _ -> false)
    |> Map.map (fun fid fc ->
        let cid = fst fid
        let newDefault =
            if fc.OutputWidth 0 > 32 then
                convertBigIntToInt32 fc.Outputs[0].BigIntStep[tick % fs.MaxArraySize]
            else
                int fc.Outputs[0].UInt32Step[tick % fs.MaxArraySize]
        SymbolUpdate.updateSymbol (setInputDefault (int newDefault)) cid
        |> Optic.map DrawModelType.SheetT.symbol_
        |> Optic.map ModelType.sheet_
        |> UpdateModel
        |> dispatch)
    |> ignore


//----------------------------View level simulation helpers------------------------------------//


type SimCache = {
    Name: string
    StoredState: LoadedComponent list
    StoredResult: Result<SimulationData, SimulationError>
    }



let simCacheInit () = {
    Name = ""; 
    StoredState = []
    StoredResult = Ok {
        FastSim = 
            printfn "Creating cache"
            FastCreate.simulationPlaceholder
        Graph = Map.empty 
        Inputs = []
        Outputs = []
        IsSynchronous=false
        NumberBase = NumberBase.Hex
        ClockTickNumber = 0
        }
    }
        
/// Used to store last canvas state and its simulation
let mutable simCache: SimCache = simCacheInit ()

let cacheIsEqual (cache: SimCache) (ldcs: LoadedComponent list ) : bool=
    match cache.StoredResult with
    | Error _ -> false
    | Ok {FastSim =fs} -> 
        fs.SimulatedCanvasState
        |> List.forall (fun ldc' ->
            ldcs
            |> List.tryFind (fun ldc'' -> ldc''.Name = ldc'.Name)
            |> Option.map (loadedComponentIsEqual ldc')
            |> (=) (Some true))
            

/// Start up a simulation, doing all necessary checks and generating simulation errors
/// if necesary. The code to do this is quite long so results are memoized. 
let prepareSimulationMemoized
        (simulationArraySize: int)
        (openFileName: string)
        (diagramName : string)
        (canvasState : CanvasState)
        (loadedDependencies : LoadedComponent list)
        : Result<SimulationData, SimulationError> * CanvasState =
    //printfn $"Diagram{diagramName}, open={openFileName}, deps = {loadedDependencies |> List.map (fun dp -> dp.Name)}"
    let storedArraySize =
        match simCache.StoredResult with
        | Ok sd -> sd.FastSim.MaxArraySize
        | _ -> 0
    let ldcs = addStateToLoadedComponents openFileName canvasState loadedDependencies
    let isSame = 
            storedArraySize = simulationArraySize &&
            diagramName = simCache.Name &&
            cacheIsEqual simCache ldcs
    if  isSame then
        simCache.StoredResult, canvasState
    else
        printfn "New simulation"
        let name, state, ldcs = getStateAndDependencies diagramName ldcs
        let simResult = startCircuitSimulation simulationArraySize diagramName state ldcs 
        simCache <- {
            Name = diagramName
            StoredState = ldcs
            StoredResult = simResult
            }
        simResult, canvasState
   
let makeDummySimulationError msg = {
        ErrType = GenericSimError msg
        InDependency = None
        ConnectionsAffected = []
        ComponentsAffected = []
    }

let simReset dispatch =
    dispatch CloseSimulationNotification // Close error notifications.
    dispatch ClosePropertiesNotification
    dispatch <| Sheet (SheetT.ResetSelection) // Remove highlights.
    dispatch <| (JSDiagramMsg << InferWidths) () // Repaint connections.

/// Start simulating the current Diagram.
/// Return SimulationData that can be used to extend the simulation
/// as needed, or error if simulation fails.
/// Note that simulation is only redone if current canvas changes.
let simulateModel (simulatedSheet: string option) (simulationArraySize: int) openSheetCanvasState model =
    let start = TimeHelpers.getTimeMs()
    match openSheetCanvasState, model.CurrentProj with
    | _, None -> 
        Error (makeDummySimulationError "What - Internal Simulation Error starting simulation - I don't think this can happen!"), openSheetCanvasState
    | canvasState, Some project ->
        let simSheet = Option.defaultValue project.OpenFileName simulatedSheet
        let otherComponents = 
            project.LoadedComponents 
            |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        (canvasState, otherComponents)
        ||> prepareSimulationMemoized simulationArraySize project.OpenFileName simSheet 
        |> TimeHelpers.instrumentInterval "MakeSimData" start

let changeBase dispatch numBase = numBase |> SetSimulationBase |> dispatch

/// A line that can be used for an input, an output, or a state.
let private splittedLine leftContent rightConent =
    Level.level [Level.Level.Props [Style [MarginBottom "10px"]]] [
        Level.left [] [
            Level.item [] [ leftContent ]
        ]
        Level.right [] [
            Level.item [] [ rightConent ]
        ]
    ]

/// Pretty print a label with its width.
let makeIOLabel label width =
    let label = cropToLength 15 true label
    match width with
    | 1 -> label
    | w -> sprintf "%s (%d bits)" label w

let private viewSimulationInputs
        (numberBase : NumberBase)
        (simulationData : SimulationData)
        (inputs : (SimulationIO * FSInterface) list)
        dispatch =

    let simulationGraph = simulationData.Graph
    let makeInputLine ((ComponentId inputId, ComponentLabel inputLabel, width), inputVals) =
        let valueHandle =
            match inputVals with
            | IData {Dat = (Word bit); Width =1} ->
                // For simple bits, just have a Zero/One button.
                Button.button [
                    Button.Props [ simulationBitStyle ]
                    //Button.Color IsPrimary
                    (match bit with 0u -> Button.Color Color.IsGreyLighter | _ -> Button.Color IsPrimary)
                    Button.IsHovered false
                    Button.OnClick (fun _ ->
                        let newBit = 1u - bit
                        let graph = simulationGraph
                        FastRun.changeInput (ComponentId inputId) (IData {Dat = Word newBit; Width = 1}) simulationData.ClockTickNumber simulationData.FastSim
                        dispatch <| SetSimulationGraph(graph, simulationData.FastSim)
                    )
                ] [ str <| bitToString (match bit with 0u -> Zero | _ -> One)]
            | IData bits ->
                let defValue = fastDataToPaddedString  Constants.boxMaxChars numberBase bits
                Input.text [
                    Input.Key (numberBase.ToString())
                    Input.DefaultValue defValue
                    Input.Props [
                        simulationNumberStyle
                        OnChange (getTextEventValue >> (fun text ->
                            match strToIntCheckWidth width text with
                            | Error err ->
                                let note = errorPropsNotification err
                                dispatch  <| SetSimulationNotification note
                            | Ok num ->
                                let bits = convertInt64ToFastData width num
                                // Close simulation notifications.
                                CloseSimulationNotification |> dispatch
                                // Feed input.
                                let graph = simulationGraph
                                FastRun.changeInput (ComponentId inputId) (IData bits) simulationData.ClockTickNumber simulationData.FastSim
                                dispatch <| SetSimulationGraph(graph, simulationData.FastSim)
                        ))
                    ]
                ]
            | IAlg _ -> failwithf "what? Algebra in Step Simulation (not yet implemented)"
        splittedLine (str <| makeIOLabel inputLabel width) valueHandle
    div [] <| List.map makeInputLine inputs

let private staticBitButton bit =
    Button.button [
        Button.Props [ simulationBitStyle ]
        //Button.Color IsPrimary
        (match bit with Zero -> Button.Color IsGreyLighter | One -> Button.Color IsPrimary)
        Button.IsHovered false
        Button.Disabled true
    ] [ str <| bitToString bit ]

let private staticNumberBox maxChars numBase (bits: FastData) =
    let value = fastDataToPaddedString maxChars numBase bits
    Input.text [
        Input.IsReadOnly true
        Input.Value value
        Input.Props [simulationNumberStyle]
    ]

let private viewSimulationOutputs numBase (simOutputs : (SimulationIO * FSInterface) list) =
    let makeOutputLine ((ComponentId _, ComponentLabel outputLabel, width), inputVals) =
        let valueHandle =
            match inputVals with
            | IData {Dat = Word b; Width = 1} -> staticBitButton (match b with 0u -> Zero | _ -> One)
            | IData bits -> staticNumberBox Constants.boxMaxChars numBase bits
            | IAlg _ -> failwithf "what? Algebra in Step Simulation (not yet implemented)"
        splittedLine (str <| makeIOLabel outputLabel width) valueHandle
    div [] <| List.map makeOutputLine simOutputs

let private viewViewers numBase (simViewers : ((string*string) * int * FSInterface) list) =
    let makeViewerOutputLine ((label,fullName), width, inputVals) =
        let valueHandle =
            match inputVals with
            | IData {Dat = Word b; Width = 1} -> staticBitButton (match b with 0u -> Zero | _ -> One)
            | IData bits -> staticNumberBox Constants.boxMaxChars numBase bits
            | IAlg _ -> failwithf "what? Algebra in Step Simulation (not yet implemented)"
        let addToolTip tip react = 
            div [ 
                HTMLAttr.ClassName $"{Tooltip.ClassName} has-tooltip-right"
                Tooltip.dataTooltip tip
            ] [react]
        let line = 
            str <| makeIOLabel label width
            |> (fun r -> if fullName <> "" then addToolTip fullName r else r)
        splittedLine line valueHandle
    div [] <| List.map makeViewerOutputLine simViewers

let private viewStatefulComponents step comps numBase model dispatch =
    let getWithDefault (lab:string) = if lab = "" then "no-label" else lab
    let makeStateLine ((fc,state) : FastComponent*SimulationComponentState) =
        let label = getWithDefault fc.FullName
        match state with
        | RegisterState fd when fd.Width = 1 ->
            let bit = if fd = SimulatorTypes.fastDataZero then Zero else One
            let label = sprintf "DFF: %s" <| label
            [ splittedLine (str label) (staticBitButton bit) ]
        | RegisterState bits ->
            let label = sprintf "Register: %s (%d bits)" label bits.Width
            [ splittedLine (str label) (staticNumberBox Constants.boxMaxChars numBase bits) ]
        | RamState mem ->
            let label = sprintf "RAM: %s" <| label
            let initialMem compType = match compType with RAM1 m | AsyncRAM1 m -> m | _ -> failwithf "what? viewStatefulComponents expected RAM component but got: %A" compType
            let viewDiffBtn =
                Button.button [
                    Button.Props [ simulationBitStyle ]
                    Button.Color IsPrimary
                    Button.OnClick (fun _ ->
                        openMemoryDiffViewer (initialMem fc.FType) mem model dispatch
                    )
                ] [ str "View" ]
            [ splittedLine (str label) viewDiffBtn ]
        | _ -> []
    div [] (List.collect makeStateLine comps )

let getSimErrFeedbackMessages (simError:SimulatorTypes.SimulationError) (model:Model) : (Msg list) =
    if simError.InDependency.IsNone then
        // Highlight the affected components and connection only if
        // the error is in the current diagram and not in a
        // dependency.
        let (badComps,badConns) = (simError.ComponentsAffected, simError.ConnectionsAffected)
        let msgs = [SetHighlighted (badComps,badConns)]
        if not (Sheet.isAllVisible model.Sheet badConns badComps) then
            // make whole diagram visible if any of the errors are not visible
            msgs @ [Sheet (SheetT.KeyPress SheetT.KeyboardMsg.CtrlW)]
        else
            msgs
    else
        []

let setSimErrorFeedback (simError:SimulatorTypes.SimulationError) (model:Model) (dispatch: Msg -> Unit) =
    // let sheetDispatch sMsg = dispatch (Sheet sMsg)
    // let keyDispatch = SheetT.KeyPress >> sheetDispatch
    // if simError.InDependency.IsNone then
    //     // Highlight the affected components and connection only if
    //     // the error is in the current diagram and not in a
    //     // dependency.
    //     let (badComps,badConns) = (simError.ComponentsAffected, simError.ConnectionsAffected)
    //     dispatch <| SetHighlighted (badComps,badConns)
    //     if not (Sheet.isAllVisible model.Sheet badConns badComps) then
    //         // make whole diagram visible if any of the errors are not visible
    //         keyDispatch <| SheetT.KeyboardMsg.CtrlW
    getSimErrFeedbackMessages simError model
    |> List.iter dispatch



/// get the position for inserting a new Not Connected component next to the component comp
/// returns None if another symbol is in the way
let getNCPos (symbolMap: Map<ComponentId, SymbolT.Symbol>) (comp: Component) =
    let isPosInBoundingBox  (pos: XYPos) (boundingBox: BoundingBox) =
        (pos.X > boundingBox.TopLeft.X && pos.X < boundingBox.TopLeft.X + boundingBox.W &&
        pos.Y > boundingBox.TopLeft.Y && pos.Y < boundingBox.TopLeft.Y + boundingBox.H)

    let pos = {X = comp.X + comp.W * 1.5; Y = comp.Y + comp.H/2.}
    symbolMap
    |> Map.toList
    |> List.map (fun (_, sym) -> Symbol.getSymbolBoundingBox sym)
    |> List.exists (isPosInBoundingBox pos)
    |> function
        | true -> None
        | false -> Some pos
    

let viewSimulationError
    (comps: Component list, conns: Connection list)
    (simError : SimulationError)
    (model: Model)
    simType
    dispatch
    =
    let sheetDispatch sMsg = dispatch <| Sheet sMsg
    let busWireDispatch bMsg = sheetDispatch <| SheetT.Msg.Wire bMsg
    let symbolDispatch symMsg = busWireDispatch <| BusWireT.Msg.Symbol symMsg

    let changeAdderType (compId: ComponentId) (targetType: ComponentType) =
        model.Sheet.ChangeAdderComp sheetDispatch compId (targetType)
    
    let changeCounterType (compId: ComponentId) (targetType: ComponentType) =
        model.Sheet.ChangeCounterComp sheetDispatch compId (targetType)

    // this does not use tryFind because the IDs given in the error component list
    // should exist
    let getComponentById (compId: ComponentId) =
        comps
        |> List.tryFind (fun comp -> ComponentId comp.Id = compId)
        |> Option.defaultWith (fun _ -> failwith "viewSimulationError: given component ID does not exist")

    // more robust version which returns empty list if there are no components
    let getComponentByIdListOpt (compId: ComponentId) =
        comps
        |> List.tryFind (fun comp -> ComponentId comp.Id = compId)
        |> function | Some comp -> [comp]
                    | None ->
                        printfn "Warning: errored component from simulation is missing - it will be ignored"
                        []

    // this does not use tryFind because the IDs given in the error connection list
    // should exist    
    let getConnectionById connId =
        conns
        |> List.tryFind (fun conn -> conn.Id = connId)
        |> Option.defaultWith (fun _ -> failwith "viewSimulationError: given connection ID does not exist")

    // more robust version which returns empty list if there are no connections
    let getConnectionByIdLstOpt connId =
        conns
        |> List.tryFind (fun conn -> conn.Id = connId)
        |> function | Some comp -> [comp]
                    | None ->
                        printfn "Warning: errored connection from simulation is missing - it will be ignored"
                        []


    /// If affected component has been deleted after simulation started we do not include it -
    /// so worst case this list can be empty!
    let reacListOfCompsAffected =
        simError.ComponentsAffected
        |> List.collect getComponentByIdListOpt
        |> List.map (fun comp -> li [] [str comp.Label])

    let getCompAndPortAffectedMsg (comp: Component) (port: Port) = comp.Label + "." + CanvasStateAnalyser.getPortName comp port


    let cleanup() =
        simReset dispatch
        dispatch (TryStartSimulationAfterErrorFix simType)

    let error =
        let comps = List.collect getComponentByIdListOpt simError.ComponentsAffected
        match comps, simError.ErrType with
        | [comp], OutputConnError (0, port, rmInfo) ->

            let buttonOrText =
                match rmInfo with
                | Removable targetType ->
                    let deletePort() =
                        changeAdderType (ComponentId comp.Id) targetType
                        cleanup()
                    Button.button [
                        Button.Color IsSuccess
                        Button.OnClick (fun _ -> deletePort())
                    ] [ str "Fix by deleting the port on the component" ]
                | Unremovable ->
                    getNCPos model.Sheet.Wire.Symbol.Symbols comp
                    |> function
                        | Some pos -> 
                            let addNCComp() =
                                busWireDispatch <| BusWireT.AddNotConnected
                                    ((ModelHelpers.tryGetLoadedComponents model),
                                    port,
                                    {X = comp.X + comp.W * 1.5; Y = comp.Y + comp.H/2.})
                                cleanup()

                            Button.button [
                                Button.Color IsSuccess
                                Button.OnClick (fun _ -> addNCComp())
                            ] [ str "Fix by adding 'Not Connected' component" ]
                        | None ->
                            str "Please insert a 'Not Connected' component manually"
            

            div [] [
                str (errMsg simError.ErrType)
                br []
                br []
                str (getCompAndPortAffectedMsg comp port)
                br []
                buttonOrText
            ]
        | [comp], InputConnError (0, port, rmInfo) ->
            let compAndPortAffectedMsg = comp.Label + "." + CanvasStateAnalyser.getPortName comp port
            let compId = ComponentId comp.Id
            let removeInPorts() =
                match rmInfo with
                | Removable targetType ->
                    match targetType with
                    | NbitsAdder _ | NbitsAdderNoCin _ | NbitsAdderNoCout _ | NbitsAdderNoCinCout _ ->
                        changeAdderType compId targetType
                    | Counter _ | CounterNoEnable _ | CounterNoLoad _ | CounterNoEnableLoad _ ->
                        changeCounterType compId targetType
                    | _ -> ()
                | Unremovable -> failwithf "This function should never be called if not input ports can be removed"
                simReset dispatch
                dispatch (TryStartSimulationAfterErrorFix simType)
                // restartFn (comps, conns) model dispatch ()
            
            let showButton =
                match rmInfo with
                | Removable _ -> true
                | Unremovable -> false
            div [] [
                str (errMsg simError.ErrType)
                br []
                br []
                str (getCompAndPortAffectedMsg comp port)
                br []
                if showButton then
                    Button.button [
                        Button.Color IsSuccess
                        Button.OnClick (fun _ -> removeInPorts())
                    ] [str "Fix by deleting input port"]
            ]
        | _, UnnecessaryNC ->
            let removeNCAndChangeAdderType() =
                let NCsToDelete =
                    simError.ConnectionsAffected
                    |> List.collect (fun (ConnectionId cid) -> getConnectionByIdLstOpt cid)
                    |> List.map (fun conn ->
                        ComponentId conn.Target.HostId)
                // delete NotConnected components
                symbolDispatch <| SymbolT.DeleteSymbols NCsToDelete
                // delete affected connections
                busWireDispatch <| BusWireT.DeleteWires simError.ConnectionsAffected

                simError.ComponentsAffected
                |> List.collect getComponentByIdListOpt
                |> List.iter (fun comp ->
                    match comp.Type with
                    | NbitsAdder w -> changeAdderType (ComponentId comp.Id) (NbitsAdderNoCout w)
                    | NbitsAdderNoCin w -> changeAdderType (ComponentId comp.Id) (NbitsAdderNoCinCout w)
                    | _ -> failwithf "Unexpected adder type. Should only encounter these 2 types with this error message")
                
                simReset dispatch
                // restartFn (comps, conns) model dispatch ()
                dispatch (TryStartSimulationAfterErrorFix simType)

            div [] [
                str (errMsg simError.ErrType)
                br []
                br []
                ul [] reacListOfCompsAffected
                br []
                Button.button [
                    Button.Color IsSuccess
                    Button.OnClick (fun _ -> removeNCAndChangeAdderType())
                ] [str "Fix by deleting unnecessary 'Not Connected' components"]
            ]
        | _ ->
            match simError.InDependency with
            | None ->
                div [] [
                    str (errMsg simError.ErrType)
                    br []
                    str <| "Please fix the error and retry."
                ]
            | Some dep ->
                div [] [
                    str <| "Error found in sheet '" + dep + "' which is a dependency:"
                    br []
                    str (errMsg simError.ErrType)
                    br []
                    str <| "Please fix the error in this sheet and retry."
                ]
    div [] [
        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Errors" ]
        error
    ]

let private simulationClockChangePopup (simData: SimulationData) (dispatch: Msg -> Unit) (model':Model) =
    let dialog = model'.PopupDialogData
    let step = simData.ClockTickNumber
    div [] 
        [
            h6 [] [str $"This simulation contains {simData.FastSim.FComps.Count} components"]
            (match dialog.Int with 
            | Some n when n > step -> 
                Text.p [
                    Modifiers [Modifier.TextWeight TextWeight.Bold] 
                  ] [str "Goto Tick:"]
            | _ -> Text.p [
                            Modifiers [
                                Modifier.TextWeight TextWeight.Bold
                                Modifier.TextColor IsDanger] 
                          ] [str $"The clock tick must be > {step}"])
            br []
            Input.number [
                Input.Props [AutoFocus true;Style [Width "100px"]]
                Input.DefaultValue <| sprintf "%d" step
                Input.OnChange (getIntEventValue >> Some >> SetPopupDialogInt >> dispatch)
            ]

        ]

let simulateWithTime timeOut steps simData =
    let startTime = getTimeMs()
    FastRun.runFastSimulation None (steps + simData.ClockTickNumber) simData.FastSim |> ignore
    getTimeMs() - startTime

let cmd block =
    Elmish.Cmd.OfAsyncWith.perform block

let doBatchOfMsgsAsynch (msgs: seq<Msg>) =
    msgs
    |> Seq.map Elmish.Cmd.ofMsg 
    |> Elmish.Cmd.batch
    |> ExecCmdAsynch
    |> Elmish.Cmd.ofMsg



let simulateWithProgressBar (simProg: SimulationProgress) (model:Model) =
    match model.CurrentStepSimulationStep, model.PopupDialogData.Progress with
    | Some (Ok simData), Some barData ->
        let nComps = float simData.FastSim.FComps.Count
        let oldClock = simData.FastSim.ClockTick
        let clock = min simProg.FinalClock (simProg.ClocksPerChunk + oldClock)
        let t1 = getTimeMs()
        FastRun.runFastSimulation None clock simData.FastSim |> ignore
        printfn $"clokctick after runFastSim {clock} from {oldClock} is {simData.FastSim.ClockTick}"
        let t2 = getTimeMs()
        let speed = if t2 = t1 then 0. else (float clock - float oldClock) * nComps / (t2 - t1)
        let messages =
            if clock - oldClock < simProg.ClocksPerChunk then [   
                SetSimulationGraph(simData.Graph, simData.FastSim)
                IncrementSimulationClockTick (clock - oldClock); 
                SetPopupProgress None ]
            else [
                SetSimulationGraph(simData.Graph, simData.FastSim)
                IncrementSimulationClockTick simProg.ClocksPerChunk
                UpdatePopupProgress (fun barData -> {barData with Value = clock - simProg.InitialClock; Speed = speed})
                SimulateWithProgressBar simProg ]
        model, doBatchOfMsgsAsynch messages       
    | _ -> 
        model, Elmish.Cmd.ofMsg (SetPopupProgress None)
    
    

let simulationClockChangeAction dispatch simData (model': Model) =
    let dialog = model'.PopupDialogData
    let clock = 
        match dialog.Int with
        | None -> failwithf "What - must have some number from dialog"
        | Some clock -> clock
    let initClock = simData.ClockTickNumber
    let steps = clock - initClock
    let numComps = simData.FastSim.FComps.Count
    let initChunk = min steps (20000/(numComps + 1))
    let initTime = getTimeMs()
    let estimatedTime = (float steps / float initChunk) * (simulateWithTime None initChunk simData + 0.0000001)
    let chunkTime = min 2000. (estimatedTime / 5.)
    let chunk = int <| float steps * chunkTime / estimatedTime
    if steps > 2*initChunk && estimatedTime > 500. then 
        printfn "test1"
        dispatch <| SetPopupProgress 
            (Some {
                Speed = float (numComps * steps) / estimatedTime
                Value=initChunk; 
                Max=steps; 
                Title= "running simulation..."
                })
        [
            SetSimulationGraph(simData.Graph, simData.FastSim)
            IncrementSimulationClockTick (initChunk)
            ClosePopup
            SimulateWithProgressBar {
                FinalClock = clock; 
                InitialClock = initChunk + initClock; 
                ClocksPerChunk = chunk 
                }
        ]
        |> Seq.map Elmish.Cmd.ofMsg 
        |> Elmish.Cmd.batch
        |> ExecCmdAsynch
        |> dispatch
    else
        FastRun.runFastSimulation None clock simData.FastSim |> ignore
        printfn $"test2 clock={clock}, clokcticknumber= {simData.ClockTickNumber}, {simData.FastSim.ClockTick}"
        [
            SetSimulationGraph(simData.Graph, simData.FastSim)
            IncrementSimulationClockTick (clock - simData.ClockTickNumber)
            ClosePopup
        ]
        |> Seq.map Elmish.Cmd.ofMsg 
        |> Elmish.Cmd.batch
        |> ExecCmdAsynch
        |> dispatch



let private viewSimulationData (step: int) (simData : SimulationData) model dispatch =
    let viewerWidthList =
        FastRun.extractViewers simData
        |> List.map (fun (_, width, _) -> width)
    let outputWidthList =
        simData.Outputs 
        |> List.map (fun (_,_,w) -> w)       
    let hasMultiBitOutputs =
        (List.append outputWidthList viewerWidthList)|> List.map ((>) 1) |> List.isEmpty |> not
    let maybeBaseSelector =
        match hasMultiBitOutputs with
        | false -> div [] []
        | true -> baseSelector simData.NumberBase (changeBase dispatch)
    let maybeClockTickBtn =
        let step = simData.ClockTickNumber
        match simData.IsSynchronous with
        | false -> div [] []
        | true ->
            div [] [
                Button.button [
                    Button.Color IsSuccess
                    Button.OnClick (fun _ ->
                        let isDisabled (model': Model) =
                            let dialogData = model'.PopupDialogData
                            match dialogData.Int with
                            | Some n -> n <= step
                            | None -> true
                        dialogPopup 
                            "Advance Simulation"
                            (simulationClockChangePopup simData dispatch)
                            "Goto Tick"
                            (simulationClockChangeAction dispatch simData)
                            isDisabled
                            []
                            dispatch)
                        ] [ str "Goto" ]
                str " "
                str " "
                Button.button [
                    Button.Color IsSuccess
                    Button.OnClick (fun _ ->
                        if SimulationRunner.simTrace <> None then
                            printfn "*********************Incrementing clock from simulator button******************************"
                            printfn "-------------------------------------------------------------------------------------------"
                        //let graph = feedClockTick simData.Graph
                        FastRun.runFastSimulation None (simData.ClockTickNumber+1) simData.FastSim |> ignore
                        dispatch <| SetSimulationGraph(simData.Graph, simData.FastSim)                    
                        if SimulationRunner.simTrace <> None then
                            printfn "-------------------------------------------------------------------------------------------"
                            printfn "*******************************************************************************************"
                        IncrementSimulationClockTick 1 |> dispatch
                    )
                ] [ str <| sprintf "Clock Tick %d" simData.ClockTickNumber ]
            ]
    let maybeStatefulComponents() =
        let stateful = 
            FastRun.extractStatefulComponents simData.ClockTickNumber simData.FastSim
            |> Array.toList
        match List.isEmpty stateful with
        | true -> div [] []
        | false -> div [] [
            Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Stateful components" ]
            viewStatefulComponents step stateful simData.NumberBase model dispatch
        ]
    let questionIcon = str "\u003F"

    let tip tipTxt txt =
        span [
                // Style [Float FloatOptions.Left]
                HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline}"
                Tooltip.dataTooltip tipTxt
            ]
            [
                Text.span [
                    Modifiers [
                        Modifier.TextColor IsPrimary
                    ]
                    Props [
                        Style [
                            Display DisplayOptions.InlineBlock
                            Width "80px"
                            TextAlign TextAlignOptions.Center]]
            ] [str txt] ]
    div [] [
        splittedLine maybeBaseSelector maybeClockTickBtn

        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Inputs" ]
        viewSimulationInputs
            simData.NumberBase
            simData
            (FastRun.extractFastSimulationIOs simData.Inputs simData)
            dispatch


        Heading.h5 [ 
            Heading.Props [ Style [ MarginTop "15px" ] ] 
            ] [ 
                str "Outputs &" 
                tip "Add Viewer components to any sheet in the simulation" "Viewers"
            ]
        viewViewers simData.NumberBase <| List.sort (FastRun.extractViewers simData)
        viewSimulationOutputs simData.NumberBase
        <| FastRun.extractFastSimulationIOs simData.Outputs simData

        maybeStatefulComponents()
    ]


let tryGetSimData canvasState model =
    printfn "startSimulation called"
    let model = MemoryEditorView.updateAllMemoryComps model
    simCache <- simCacheInit ()
    simulateModel None Constants.maxArraySize canvasState model
    |> function
        | Ok (simData), state -> 
            if simData.FastSim.ClockTick = 0 then 
                setFastSimInputsToDefault simData.FastSim
            Ok simData
        | Error simError, state ->
            printfn $"ERROR:{simError}"
            Error simError

let viewSimulation canvasState model dispatch =
    printf "Viewing Simulation"
    let startSimulation() =
        tryGetSimData canvasState model
        |> function
            | Ok simData -> Ok simData
            | Error simError ->
                setSimErrorFeedback simError model dispatch
                Error simError
        |> StartSimulation
        |> dispatch

    // let JSState = model.Diagram.GetCanvasState ()
    match model.CurrentStepSimulationStep with
    | None ->
        let simRes = simulateModel None Constants.maxArraySize canvasState model
        let isSync = match simRes with | Ok {IsSynchronous=true},_ -> true | _ -> false
        let buttonColor, buttonText = 
            match simRes with
            | Ok _, _ -> IsSuccess, "Start Simulation"
            | Error _, _ -> IsWarning, "See Problems"
        div [] [
            str "Simulate simple logic using this tab."
            br []
            str (if isSync then "You can also use the Wave Simulation tab to view waveforms" else "")
            br []; br []
            Button.button
                [ 
                    Button.Color buttonColor; 
                    Button.OnClick (fun _ -> startSimulation()) ; 
                ]
                [ str buttonText ]
        ]
    | Some sim ->
        let body = match sim with
                    | Error simError -> viewSimulationError canvasState simError model StepSim dispatch
                    | Ok simData -> viewSimulationData simData.ClockTickNumber simData model dispatch
        let setDefaultButton =
            match sim with
            | Error _ -> div [] []
            | Ok simData ->
            div [] [
                Button.button
                    [ 
                        Button.Color IsInfo;
                        Button.Disabled (InputDefaultsEqualInputs simData.FastSim model)
                        Button.OnClick (fun _ -> setInputDefaultsFromInputs simData.FastSim dispatch) ; 
                        Button.Props [Style [Display DisplayOptions.Inline; Float FloatOptions.Right ]]
                    ]
                    [ str "Save current input values as default" ]
            ]
        
        div [Style [Height "100%"]] [
            Button.button
                [
                    Button.Color IsDanger;
                    Button.OnClick (fun _ ->
                        simReset dispatch
                        dispatch EndSimulation);
                    Button.Props [Style [Display DisplayOptions.Inline; Float FloatOptions.Left ]]]
                [ str "End simulation" ]
            setDefaultButton
            br []; br []
            str "The simulation uses the diagram as it was at the moment of
                 pressing the \"Start simulation\" button."
            hr []
            body
        ]

let tryStartSimulationAfterErrorFix (simType:SimSubTab) (model:Model) =
    let withMsg msg model = model, Cmd.ofMsg msg
    let withMsgs msgs model = model, Cmd.batch (msgs |> List.map Cmd.ofMsg)
    let withCmdTTMsg ttMsg model = model, Cmd.ofMsg (TruthTableMsg ttMsg)
    let conns = BusWire.extractConnections model.Sheet.Wire
    let comps = SymbolUpdate.extractComponents model.Sheet.Wire.Symbol
    let canvasState = comps,conns
    let simErrFeedback simErr otherMsg =
            (getSimErrFeedbackMessages simErr model) @ [otherMsg]

    match simType with
        | StepSim ->
            tryGetSimData canvasState model
            |> function
                | Ok (simData) -> 
                    model
                    |> set currentStepSimulationStep_ (simData |> Ok |> Some)
                    |> withMsg (StartSimulation (Ok simData))
                | Error simError ->
                    model
                    |> set currentStepSimulationStep_ (simError |> Error |> Some)
                    |> withMsgs (simErrFeedback simError (StartSimulation (Error simError)))

        | TruthTable ->
            simulateModel None 2 canvasState model
            |> function
                | Ok (simData), state ->
                    if simData.IsSynchronous = false then
                        model
                        |> set currentStepSimulationStep_ (simData |> Ok |> Some)
                        |> withCmdTTMsg (GenerateTruthTable (Some (Ok simData, state)))
                    else
                        { model with CurrentStepSimulationStep = None }
                        |> withCmdTTMsg CloseTruthTable
                | Error simError, state ->
                    let feedbackMsg = GenerateTruthTable (Some (Error simError, state)) |> TruthTableMsg
                    model
                    |> set currentStepSimulationStep_ (simError |> Error |> Some)
                    |> withMsgs (simErrFeedback simError feedbackMsg)

        | WaveSim ->
            let model = MemoryEditorView.updateAllMemoryComps model
            let wsSheet = 
                match model.WaveSimSheet with
                | None -> Option.get (getCurrFile model)
                | Some sheet -> sheet
            let model = 
                model
                |> removeAllSimulationsFromModel
                |> fun model -> {model with WaveSimSheet = Some wsSheet}
            let wsModel = getWSModel model
            //printfn $"simSheet={wsSheet}, wsModel sheet = {wsModel.TopSheet},{wsModel.FastSim.SimulatedTopSheet}, state={wsModel.State}"
            match simulateModel
                    model.WaveSimSheet
                    (Constants.maxLastClk + Constants.maxStepsOverflow)
                    canvasState
                    model with
            //| None ->
            //    dispatch <| SetWSModel { wsModel with State = NoProject; FastSim = FastCreate.emptyFastSimulation "" }
            | (Error simError, _) ->
                model
                |> set currentStepSimulationStep_ (simError |> Error |> Some)
                |> withMsgs (simErrFeedback simError (SetWSModelAndSheet ({ wsModel with State = SimError simError }, wsSheet)))
            | (Ok simData, canvState) ->
                if simData.IsSynchronous then
                    setFastSimInputsToDefault simData.FastSim
                    let wsModel = { wsModel with State = Loading ; FastSim = simData.FastSim }
                    model
                    |> set currentStepSimulationStep_ (simData |> Ok |> Some)
                    |> withMsgs [SetWSModelAndSheet (wsModel, wsSheet) ; RefreshWaveSim wsModel]
                else
                    model
                    |> set currentStepSimulationStep_ (simData |> Ok |> Some)
                    |> withMsg (SetWSModelAndSheet ({ wsModel with State = NonSequential }, wsSheet))

