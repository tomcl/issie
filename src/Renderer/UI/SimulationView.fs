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
    let ncPortDist = 30.

/// save verilog file
/// TODO: the simulation error display here is shared with step simulation and also waveform simulation -
/// maybe it should be a subfunction.
let verilogOutput (vType: Verilog.VMode) (model: Model) (dispatch: Msg -> Unit) =
    printfn "Verilog output"
    match MenuHelpers.updateProjectFromCanvas model dispatch, model.Sheet.GetCanvasState() with
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
        | _, Some defaultVal -> cid, convertBigintToFastData w defaultVal
        | _, None -> cid, convertBigintToFastData w 0I)
    |> List.iter (fun (cid, wire) -> FastRun.changeInput cid (FSInterface.IData wire) 0 fs)

let InputDefaultsEqualInputs fs (model:Model) (clocktick : int)=
    let tick = clocktick
    fs.FComps
    |> Map.filter (fun cid fc -> fc.AccessPath = [] && match fc.FType with | Input1 _ -> true | _ -> false)
    |> Map.map (fun fid fc ->
        let cid = fst fid
        if Map.containsKey cid (Optic.get SheetT.symbols_ model.Sheet) then
            let newDefault =
                if fc.OutputWidth 0 > 32 then
                    fc.Outputs[0].BigIntStep[tick % fs.MaxArraySize]
                else
                    bigint fc.Outputs[0].UInt32Step[tick % fs.MaxArraySize]
            let typ = (Optic.get (SheetT.symbolOf_ cid) model.Sheet).Component.Type
            match typ with
            | Input1(_, Some d) -> d = newDefault
            | _ -> newDefault = 0I
        else
            true)
    |> Map.values
    |> Seq.forall id

let InputDefaultsEqualInputsRefresh fs (model:Model) =
    let tick = fs.ClockTick
    fs.FComps
    |> Map.filter (fun cid fc -> fc.AccessPath = [] && match fc.FType with | Input1 _ -> true | _ -> false)
    |> Map.map (fun fid fc ->
        let cid = fst fid
        if Map.containsKey cid (Optic.get SheetT.symbols_ model.Sheet) then
            let typ = (Optic.get (SheetT.symbolOf_ cid) model.Sheet).Component.Type
            let currdefault = match typ with
                                    | Input1(_, Some d) -> d
                                    | _ -> 0I
            let outputarray =
                if fc.OutputWidth 0 > 32 then
                    fc.Outputs[0].BigIntStep
                else
                    Array.map (fun (x: uint32) -> twosComp (fc.OutputWidth 0) (bigint x)) fc.Outputs[0].UInt32Step
            let slicedArray = Array.sub outputarray 0 ((tick+1) % fs.MaxArraySize)
            let areAllElementsSame (arr: bigint array) =
                match tick with
                | n when n < 2 ->
                    true
                | _ ->
                    Array.forall (fun elem -> elem = currdefault) arr
            areAllElementsSame slicedArray
        else
            true)
    |> Map.values
    |> Seq.forall id


let setInputDefaultsFromInputs fs (dispatch: Msg -> Unit) (clocktick: int)=
    let setInputDefault (newDefault: bigint) (sym: SymbolT.Symbol) =
        let comp = sym.Component
        let comp' = 
            let ct =
                match comp.Type with 
                | Input1(w,defVal) -> Input1(w,Some newDefault)
                | x -> x
            {comp with Type = ct}
        {sym with Component = comp'}
    let tick = clocktick
    fs.FComps
    |> Map.filter (fun cid fc -> fc.AccessPath = [] && match fc.FType with | Input1 _ -> true | _ -> false)
    |> Map.map (fun fid fc ->
        let cid = fst fid
        let newDefault =
            if fc.OutputWidth 0 > 32 then
                fc.Outputs[0].BigIntStep[tick % fs.MaxArraySize]
            else
                bigint fc.Outputs[0].UInt32Step[tick % fs.MaxArraySize]
        SymbolUpdate.updateSymbol (setInputDefault newDefault) cid
        |> Optic.map DrawModelType.SheetT.symbol_
        |> Optic.map ModelType.sheet_
        |> UpdateModel
        |> dispatch)
    |> ignore



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
                                let bits = convertBigintToFastData width num
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
            let initialMem compType =
                match compType with
                | RAM1 m | AsyncRAM1 m ->
                    m
                | _ ->
                    failwithf "what? viewStatefulComponents expected RAM component but got: %A" compType
            let viewDiffBtn =
                Button.button [
                    Button.Props [ simulationBitStyle ]
                    Button.Color IsPrimary
                    Button.OnClick (fun _ -> dispatch <| ExecFuncInMessage(
                        (fun model _ -> openMemoryDiffViewer (initialMem fc.FType) mem model dispatch), dispatch)
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



/// get the position and rotation for inserting a new component next to the given port
/// at a given distance
/// the rotation is such that the original left side of the component (input side)
/// faces the given port
/// returns None if another symbol is in the way
let getPosRotNextToPort (port: Port) (model: SymbolT.Model) (dist: float) =
    let isPosInBoundingBox  (pos: XYPos) (boundingBox: BoundingBox) =
        (pos.X > boundingBox.TopLeft.X && pos.X < boundingBox.TopLeft.X + boundingBox.W &&
        pos.Y > boundingBox.TopLeft.Y && pos.Y < boundingBox.TopLeft.Y + boundingBox.H)
    
    let sym =
        model.Symbols
        |> Map.toList
        |> List.tryFind (fun (_, sym) -> sym.Component.Id = port.HostId)
        |> function
            | Some (_, sym) -> sym
            | None -> failwithf "The given component should be in the list of symbols"

    let edge = sym.PortMaps.Orientation[port.Id]
    let portPos = Symbol.getPortPos sym port
    let pos, rot =
        match edge with
        | Right ->
            {X = sym.Pos.X + portPos.X + dist; Y = sym.Pos.Y + portPos.Y},
            Degree0
        | Top ->
            {X = sym.Pos.X + portPos.X; Y = sym.Pos.Y + portPos.Y - dist},
            Degree90
        | Left ->
            {X = sym.Pos.X + portPos.X - dist; Y = sym.Pos.Y + portPos.Y},
            Degree180
        | Bottom ->
            {X = sym.Pos.X + portPos.X; Y = sym.Pos.Y + portPos.Y + dist},
            Degree270

    model.Symbols
    |> Map.toList
    |> List.map (fun (_, sym) -> Symbol.getSymbolBoundingBox sym)
    |> List.exists (isPosInBoundingBox pos)
    |> function
        | true -> None
        | false -> Some (pos, rot)
    

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

    let changeAdderType (compId: ComponentId) (targetType: ComponentType) (model: Model) _ =
        model.Sheet.ChangeAdderComp sheetDispatch compId (targetType)
    
    let changeCounterType (compId: ComponentId) (targetType: ComponentType) (model: Model) _ =
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
                    let deletePort model _ =
                        changeAdderType (ComponentId comp.Id) targetType model ()
                        cleanup()
                    Button.button [
                        Button.Color IsSuccess
                        Button.OnClick (fun _ -> dispatch <| ExecFuncInMessage((deletePort,dispatch)))
                    ] [ str "Fix by deleting the port on the component" ]
                | Unremovable ->
                    getPosRotNextToPort port model.Sheet.Wire.Symbol Constants.ncPortDist
                    |> function
                        | Some (pos, rot) ->
                            let addNCComp model _ =
                                sheetDispatch <| SheetT.AddNotConnected
                                    ((ModelHelpers.tryGetLoadedComponents model),
                                    port,
                                    pos,
                                    rot)
                                cleanup()

                            Button.button [
                                Button.Color IsSuccess
                                Button.OnClick (fun _ -> dispatch <| ExecFuncInMessage(addNCComp,dispatch))
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
            let removeInPorts (moel: Model) _ =
                match rmInfo with
                | Removable targetType ->
                    match targetType with
                    | NbitsAdder _ | NbitsAdderNoCin _ | NbitsAdderNoCout _ | NbitsAdderNoCinCout _ ->
                        changeAdderType compId targetType model ()
                    | Counter _ | CounterNoEnable _ | CounterNoLoad _ | CounterNoEnableLoad _ ->
                        changeCounterType compId targetType model ()
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
                        Button.OnClick (fun _ -> dispatch <| ExecFuncInMessage(removeInPorts,dispatch))
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
                    | NbitsAdder w -> dispatch <| ExecFuncInMessage ((changeAdderType (ComponentId comp.Id) (NbitsAdderNoCout w)),dispatch)
                    | NbitsAdderNoCin w -> dispatch <| ExecFuncInMessage((changeAdderType (ComponentId comp.Id) (NbitsAdderNoCinCout w)),dispatch)
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
    let restartsimrequired (lastStepNeeded: int) = (simData.FastSim.ClockTick - lastStepNeeded) >= simData.FastSim.MaxArraySize
    div [] 
        [
            h6 [] [str $"This simulation contains {simData.FastSim.FComps.Count} components"]
            (match dialog.Int with 
            | Some n when restartsimrequired n -> 
                Text.p 
                    [Modifiers [
                        Modifier.TextWeight TextWeight.Bold
                        Modifier.TextColor IsDanger] 
                    ] 
                    [str $"To generate data for time step {n}, 
                          the hardware will be resimulated using default inputs. "]
            | _ -> 
                Text.p [Modifiers [
                    Modifier.TextWeight TextWeight.Bold]] 
                    [str $"Go to Tick:"])
            br []
            Input.number [
                Input.Props [AutoFocus true;Style [Width "100px"]]
                Input.DefaultValue <| sprintf "%d" step
                Input.OnChange (getIntEventValue >> Some >> SetPopupDialogInt >> dispatch)
            ]

        ]

let simulateWithTime timeOut steps (simData: SimulationData) =
    let startTime = getTimeMs()
    FastRun.runFastSimulation None steps simData.FastSim |> ignore
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
    let initClock = 
        if clock > simData.ClockTickNumber then 
            simData.ClockTickNumber
        else 
            0
    let steps = 
        if clock > simData.ClockTickNumber then 
            clock - simData.ClockTickNumber
        else 
            clock
    let numComps = simData.FastSim.FComps.Count
    let initChunk = min steps (20000/(numComps + 1))
    let initTime = getTimeMs()
    let estimatedTime = 
        match clock - simData.FastSim.ClockTick with
        | n when n > 0 -> 
            (float steps / float initChunk) * (simulateWithTime None (initChunk+initClock) simData + 0.0000001)
        | n when n <= -simData.FastSim.MaxArraySize -> 
            (float steps / float initChunk) * (simulateWithTime None initChunk simData + 0.0000001)
        | _ -> 
            (float steps / float initChunk) * (simulateWithTime None steps simData + 0.0000001)
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
            IncrementSimulationClockTick (initChunk-simData.ClockTickNumber+initClock)
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
        printfn $"test2 clock={clock}, clockticknumber= {simData.ClockTickNumber}, {simData.FastSim.ClockTick}"
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
                    Button.Disabled (simData.ClockTickNumber = 0)
                    Button.OnClick (fun _ ->
                        if SimulationRunner.simTrace <> None then
                            printfn "*********************Incrementing clock from simulator button******************************"
                            printfn "-------------------------------------------------------------------------------------------"
                        //let graph = feedClockTick simData.Graph
                        printfn "clock %d "simData.ClockTickNumber
                        FastRun.runFastSimulation None (simData.ClockTickNumber-1) simData.FastSim |> ignore
                        dispatch <| SetSimulationGraph(simData.Graph, simData.FastSim)                    
                        if SimulationRunner.simTrace <> None then
                            printfn "-------------------------------------------------------------------------------------------"
                            printfn "*******************************************************************************************"
                        IncrementSimulationClockTick -1 |> dispatch
                    )
                ] [ str "◀" ]
                str " "
                str " "
                Button.button [
                    Button.Props [Tooltip.dataTooltip "Click to goto"]
                    Button.Color IsSuccess
                    Button.OnClick (fun _ ->
                        let isDisabled (model': Model) =
                            let dialogData = model'.PopupDialogData
                            match dialogData.Int with
                            | Some n -> n < 0
                            | None -> true
                        dialogPopup 
                            "Advance Simulation"
                            (simulationClockChangePopup simData dispatch)
                            "Goto Tick"
                            (simulationClockChangeAction dispatch simData)
                            isDisabled
                            []
                            dispatch)
                        ] [ str <| sprintf "Clock Tick %d" simData.ClockTickNumber ]
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
                ] [ str "▶" ]
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


let tryGetSimData isWaveSim canvasState model =
    let model = MemoryEditorView.updateAllMemoryComps model
    if isWaveSim then
        simCacheWS <- simCacheInit ()
    else
        simCache <- simCacheInit ()
    printfn $"TyyGetSimData: WS={isWaveSim}"
    simulateModel isWaveSim None Constants.maxArraySize canvasState model
    |> function
        | Ok (simData), state -> 
            if simData.FastSim.ClockTick = 0 then 
                setFastSimInputsToDefault simData.FastSim
            Ok simData
        | Error simError, state ->
            printfn $"ERROR:{simError}"
            Error simError

let viewSimulation canvasState model dispatch =
    let startSimulation model _ =
        tryGetSimData false canvasState model
        |> function
            | Ok simData -> 
                Ok simData
            | Error simError ->
                setSimErrorFeedback simError model dispatch
                Error simError
        |> StartSimulation
        |> dispatch
        match model.CurrentProj with
        | Some project ->
            let loadedDependencies = project.LoadedComponents |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
            let ldcs = addStateToLoadedComponents simCache.Name canvasState loadedDependencies
            simCache <- {simCache with StoredState = ldcs}
        | None -> ()

    let hasCanvasChanged
        (currentCanvasState)
        (simCache)
        (model)
        : bool = 
        match model.CurrentProj with
        | Some project ->
            let loadedDependencies = project.LoadedComponents |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
            let ldcs = addStateToLoadedComponents simCache.Name currentCanvasState loadedDependencies
            let isSame = storedstateisEqual simCache ldcs
            not isSame
        | _ -> false
    
    let simRes = simulateModel false None Constants.maxArraySize canvasState model
    // let JSState = model.Diagram.GetCanvasState ()
    match model.CurrentStepSimulationStep with
    | None ->
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
                    Button.OnClick (fun _ -> dispatch <| ExecFuncInMessage(startSimulation, dispatch)) ; 
                ]
                [ str buttonText ]
        ]
    | Some sim ->
        let canvasStateChange = hasCanvasChanged canvasState simCache model
        let body = match sim with
                    | Error simError -> viewSimulationError canvasState simError model StepSim dispatch
                    | Ok simData -> 
                        if simCache.RestartSim then
                            let clock = simData.ClockTickNumber
                            startSimulation model ()
                            simCache <- {simCache with RestartSim = false}
                            simCache <- {simCache with ClockTickRefresh = clock}
                        if (simData.ClockTickNumber = 0 && not (simCache.ClockTickRefresh = 0)) then
                            IncrementSimulationClockTick simCache.ClockTickRefresh |> dispatch
                            FastRun.runFastSimulation None simCache.ClockTickRefresh simData.FastSim |> ignore
                            simCache <- {simCache with ClockTickRefresh = 0}
                        viewSimulationData simData.ClockTickNumber simData model dispatch
        let setDefaultButton =
            match sim with
            | Error _ -> div [] []
            | Ok simData ->
                Button.button
                    [ 
                        Button.Color IsInfo;
                        Button.Disabled (InputDefaultsEqualInputs simData.FastSim model simData.ClockTickNumber)
                        Button.OnClick (fun _ -> setInputDefaultsFromInputs simData.FastSim dispatch simData.ClockTickNumber) ; 
                        Button.Props [Style [Display DisplayOptions.Inline; Float FloatOptions.Right ]]
                    ]
                    [ str "Save current input values as default" ]

        let confirmRefreshPopup (model:Model) dispatch (simData: SimulationData) =
            fun (model:Model) ->
                div [] 
                    [
                    div [Style [Height "60px"; Display DisplayOptions.Block; MarginBottom "5px"]] [
                    h6 [Style [Width "80%"; Float FloatOptions.Left;]] [str $"Refresh the simulation using current values of the inputs and the latest design? 
                    The current values will be used as default for future simulations."]
                    Button.button [
                        Button.Color IsSuccess
                        Button.OnClick (fun _ ->
                            printfn "did a refresh with ok"
                            setInputDefaultsFromInputs simData.FastSim dispatch simData.ClockTickNumber
                            simCache <- {simCache with RestartSim = true}
                            ClosePopup |> dispatch
                        )
                        Button.Props [Style [Display DisplayOptions.Inline; Float FloatOptions.Right; MarginTop "10px";]]
                    ] [ str "Ok" ]]
                    hr [Style [Width "100%"; Float FloatOptions.Left;]]
                    div [Style [Height "50px"; Display DisplayOptions.Block;]] [
                    h6 [Style [Width "80%"; Float FloatOptions.Left ]] [str $"Refresh the simulation using default values of inputs, current values will be lost."]
                    Button.button [
                        Button.Color IsInfo
                        Button.OnClick (fun _ ->
                            let clock = simData.ClockTickNumber
                            startSimulation model dispatch
                            simCache <- {simCache with ClockTickRefresh = clock}
                            ClosePopup |> dispatch
                        )
                        Button.Props [Style [Display DisplayOptions.Inline; Float FloatOptions.Right ]]
                    ] [ str "Reset" ]]
                ]

        let buttonColor, buttonIcon = 
            match simRes with
            | Ok _, _ -> IsSuccess, refreshSvg "white" "20px"
            | Error _, _ -> IsWarning, str "See Problems"

        let createRefreshButton buttonColor buttonIcon onClick =
            Button.button [
                Button.Color buttonColor;
                Button.OnClick onClick
                Button.Props [Style [Display DisplayOptions.Inline; Float FloatOptions.None; MarginLeft "5px"]]
            ] [buttonIcon]

        let startSimulationUpdateCache clock =
            startSimulation model ()
            simCache <- { simCache with ClockTickRefresh = clock }

        let createRefreshButtonForSimData sim model dispatch =
            match sim with
            | Ok (simData: SimulationData) ->
                if InputDefaultsEqualInputsRefresh simData.FastSim model then
                    createRefreshButton buttonColor buttonIcon (fun _ ->
                        let clock = simData.ClockTickNumber
                        startSimulationUpdateCache clock)
                else
                    createRefreshButton buttonColor buttonIcon (fun _ ->
                    match simRes with
                    | Ok _, _ ->
                        dialogPopupRefresh
                            "Refresh"
                            (confirmRefreshPopup model dispatch simData)
                            []
                            dispatch
                    | Error _, _ -> startSimulationUpdateCache simData.ClockTickNumber)
            | _ -> emptyRefreshSVG

        let createRefreshButtonError =
            createRefreshButton buttonColor buttonIcon (fun _ ->
                let clock = simCache.ClockTickRefresh
                startSimulationUpdateCache clock)

        let refreshButton =
            match canvasStateChange, sim with
            | true, Ok _ -> createRefreshButtonForSimData sim model dispatch
            | true, Error _ -> createRefreshButtonError
            | _ -> emptyRefreshSVG
    
        div [Style [Height "100%"]] [
            div [Style [Height "40px"]] [
            Button.button
                [
                    Button.Color IsDanger;
                    Button.OnClick (fun _ ->
                        simReset dispatch
                        dispatch EndSimulation);
                    Button.Props [Style [Display DisplayOptions.Inline; Float FloatOptions.Left;]]]
                [ str "End simulation" ]
            refreshButton
            setDefaultButton
            ]
            br []; 
            div [Style [Display DisplayOptions.Block;]] []
            str "The simulation uses the diagram as it was at the moment of
                 pressing the \"Start simulation\" or \"Refresh\" button using default input values."
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
            tryGetSimData false canvasState model
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
            simulateModel false None 2 canvasState model
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
                    true
                    model.WaveSimSheet
                    (wsModel.WSConfig.LastClock + Constants.maxStepsOverflow)
                    canvasState
                    model with

            | (Error simError, _) ->
                model
                |> set currentStepSimulationStep_ (simError |> Error |> Some)
                |> withMsgs (simErrFeedback simError (SetWSModelAndSheet ({ wsModel with State = SimError simError }, wsSheet)))
            | (Ok simData, canvState) ->
                if simData.IsSynchronous then
                    setFastSimInputsToDefault simData.FastSim
                    let wsModel = { wsModel with State = Loading}
                    model
                    |> set currentStepSimulationStep_ (simData |> Ok |> Some)
                    |> withMsgs [SetWSModelAndSheet (wsModel, wsSheet) ; RefreshWaveSim wsModel]
                else
                    model
                    |> set currentStepSimulationStep_ (simData |> Ok |> Some)
                    |> withMsg (SetWSModelAndSheet ({ wsModel with State = NonSequential }, wsSheet))
