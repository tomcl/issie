module WaveSim

open Fulma
open Fable.React
open Fable.React.Props

open CommonTypes
open ModelType
open ModelHelpers
open WaveSimStyle
open WaveSimHelpers
open TopMenuView
open SimulatorTypes
open NumberHelpers
open DrawModelType
open WaveSimNavigation
//open WaveSimSelect
open DiagramStyle

open WaveSimWaves.Constants

/// Start or update a spinner popup
let updateSpinner (name:string) payload (numToDo:int) (model: Model) =
    match model.SpinnerPayload with
    | Some sp when sp.Name = name ->
        {model with SpinnerPayload = Some {Name = name; Payload = payload; ToDo = numToDo; Total = sp.Total}}
    | _ ->
        {model with SpinnerPayload = Some {Name = name; Payload = payload; ToDo = numToDo; Total = numToDo + 1}}

    


/// remove the spinner popup
let cancelSpinner (model:Model) =
    {model with SpinnerPayload = None}
    

/// Major function called after changes to extend simulation and/or redo waveforms.
/// Note that after design change simulation muts be redonne externally, and function called with
/// newSimulation = true.
/// First extend simulation, if needed, with timeout and callback from Spinner if needed.
/// Then remake any waveforms which have changed and not yet been remade. Again if needed with
/// timeOut and callback from Spinner.
/// Spinner (in reality a progress bar) is used if the estimated time to completion is longer than
/// a constant. To get the estimate some initial execution must be completed (1 clock cycle and one waveform).
let rec refreshWaveSim (newSimulation: bool) (wsModel: WaveSimModel) (model: Model): Model * Elmish.Cmd<Msg> = 
    let isSameWave (wi:WaveIndexT) (wi': WaveIndexT) =
        wi.Id = wi'.Id && wi.PortNumber = wi'.PortNumber && wi.PortType = wi'.PortType
    // use given (more uptodate) wsModel
    let model = updateWSModel (fun _ -> wsModel) model
    let start = TimeHelpers.getTimeMs ()
    let fs = wsModel.FastSim
    if fs.NumStepArrays = 0 then
        model, Elmish.Cmd.none
    else
    // starting runSimulation
        //printfn "Starting refresh"
        let lastCycleNeeded = (wsModel.StartCycle + wsModel.ShownCycles - 1)*wsModel.CycleMultiplier + 1

        FastRun.runFastSimulation (Some Constants.initSimulationTime) lastCycleNeeded fs
        |> (fun speedOpt ->
            let cyclesToDo = lastCycleNeeded - wsModel.FastSim.ClockTick
            match speedOpt with
            | Some speed when  float cyclesToDo / speed + Constants.initSimulationTime > Constants.maxSimulationTimeWithoutSpinner  &&
                               Option.isNone model.Spinner ->
                // long simulation, set spinner on and dispatch another refresh 
                let spinnerFunc = fun model ->
                    fst (refreshWaveSim newSimulation wsModel model)
                let model = model |> updateSpinner "Waveforms simulation..." spinnerFunc cyclesToDo
                //printfn "ending refresh with continuation..."
                model, Elmish.Cmd.none
                |> TimeHelpers.instrumentInterval "refreshWaveSim" start
            | _ ->
                if speedOpt <> None then 
                    //printfn "Force running simulation"
                    // force simulation to finish now
                    FastRun.runFastSimulation None lastCycleNeeded fs |> ignore                
                // simulation has finished so can generate waves

                let allWavesStart = TimeHelpers.getTimeMs ()    
                    //printfn "starting getwaves"
                // redo waves based on new simulation
                let allWaves = 
                    if newSimulation then
                        //printfn "making new waves..."
                        WaveSimWaves.getWaves wsModel fs 
                    else wsModel.AllWaves
                let model = updateWSModel (fun ws -> {ws with AllWaves = allWaves}) model
                // redo viewer width (and therefore shown cycles etc) based on selected waves names
                // which are currently only calculatable after getwaves has generated waves
                let model = updateViewerWidthInWaveSim model.WaveSimViewerWidth model 
                // extract wsModel from updated model for processing below
                let wsModel = getWSModel model

                let simulationIsUptodate = wsModel.FastSim.ClockTick > wsModel.ShownCycles + wsModel.StartCycle

                match simulationIsUptodate with
                | falae ->
                // need to use isSameWave here because array index may have changed
                let wavesToBeMade =
                    allWaves
                    |> Map.filter (fun wi wave ->
                        // Only generate waveforms for selected waves.
                        // Regenerate waveforms whenever they have changed
                        let hasChanged = not <| WaveSimWaves.waveformIsUptodate wsModel wave
                        //if List.contains index ws.SelectedWaves then
                        List.exists (fun wi' -> isSameWave wi wi') wsModel.SelectedWaves && hasChanged && simulationIsUptodate)
                    |> Map.toList                   
                    |> List.map fst

                let model, allWaves, spinnerPayload, numToDo =
                    //printfn $"{wavesToBeMade.Length} waves to make."
                    let numToDo = wavesToBeMade.Length
                    WaveSimWaves.makeWaveformsWithTimeOut (Some Constants.initSimulationTime) wsModel allWaves wavesToBeMade
                    |> (fun (allWaves, numDone, timeOpt) ->
                            match wavesToBeMade.Length - numDone, timeOpt with
                            | n, None -> 
                                model, allWaves, None, n // finished
                            | _ when numDone = 0 -> 
                                failwithf "What? makewaveformsWithTimeOut must make at least one waveform"
                            | numToDo, Some t when 
                                    float wavesToBeMade.Length * t / float numDone < Constants.maxSimulationTimeWithoutSpinner ->
                                let (allWaves, numDone, timeOpt) = WaveSimWaves.makeWaveformsWithTimeOut None wsModel allWaves wavesToBeMade
                                model, allWaves, None, numToDo - numDone
                            | numToDo, _ ->
                                let payload = Some ("Making waves", refreshWaveSim false {wsModel with AllWaves = allWaves} >> fst)
                                model,  allWaves, payload, numToDo)

                let scrollbarInfo = generateScrollbarInfo wsModel

                let ramComps =
                    let isRAMOrROM fcid (fc: FastComponent) =
                        match fc.FType with
                        | RAM1 _ | ROM1 _ | AsyncRAM1 _ | AsyncROM1 _ ->
                            true
                        | _ -> false
                    Map.filter isRAMOrROM fs.FComps
                    |> Map.toList
                    |> List.map (fun (fcid,fc) -> fc)
                    |> List.sortBy (fun fc -> fc.FullName)

                let ramCompIds = List.map (fun (fc: FastComponent) -> fc.fId) ramComps
                let allWaveA = Map.keys allWaves |> Seq.toArray
                // arrayIndex may have changed, so we have to use new arrayIndex
                // if we cannot find it, then the selected wave no longer exists and is dropped
                let selectedWaves = 
                    wsModel.SelectedWaves
                    |> List.collect (fun wi -> match Array.tryFind (isSameWave wi) allWaveA with Some w -> [w] | None -> [])

                let selectedRams = Map.filter (fun ramfId _ -> List.contains ramfId ramCompIds) wsModel.SelectedRams

                let ws =  
                    {
                        wsModel with
                            State = Success
                            AllWaves = allWaves
                            SelectedWaves = selectedWaves
                            RamComps = ramComps
                            SelectedRams = selectedRams
                            FastSim = fs
                    } |> validateScrollBarInfo

                let model = 
                    match spinnerPayload with
                    | None -> cancelSpinner model
                    | Some sp -> 
                        updateSpinner (fst sp) (snd sp) numToDo model
                    |> updateWSModel (fun _ -> ws)
                model, Elmish.Cmd.none)
                //|> TimeHelpers.instrumentInterval "refreshWaveSim" start)
//}

/// Refresh the state of the wave simulator according to the model and canvas state.
/// Redo a new simulation. Set inputs to default values. Then call refreshWaveSim via RefreshWaveSim message.
/// 1st parameter ofrefreshWaveSin will be set true which causes all waves to be necessarily regenerated.
let refreshButtonAction canvasState model dispatch = fun _ ->
    let model = MemoryEditorView.updateAllMemoryComps model
    let wsSheet = 
        match model.WaveSimSheet with
        | None ->
            Option.get (getCurrFile model)
        | Some sheet ->
            sheet
    printfn $"Refresh Button with width = {model.WaveSimViewerWidth}"
    let model = 
        model
        |> removeAllSimulationsFromModel
        |> fun model -> {model with WaveSimSheet = Some wsSheet}
    let wsModel = getWSModel model
    //printfn $"simSheet={wsSheet}, wsModel sheet = {wsModel.TopSheet},{wsModel.FastSim.SimulatedTopSheet}, state={wsModel.State}"
    match SimulationView.simulateModel model.WaveSimSheet (ModelHelpers.Constants.maxLastClk + ModelHelpers.Constants.maxStepsOverflow)  canvasState model with
    //| None ->
    //    dispatch <| SetWSModel { wsModel with State = NoProject; FastSim = FastCreate.emptyFastSimulation "" }
    | (Error e, _) ->
        dispatch <| SetWSModelAndSheet ({ wsModel with State = SimError e }, wsSheet)
    | (Ok simData, canvState) ->
        if simData.IsSynchronous then
            SimulationView.setFastSimInputsToDefault simData.FastSim
            let wsModel = { wsModel with State = Loading ; FastSim = simData.FastSim }
            dispatch <| SetWSModelAndSheet (wsModel, wsSheet)
            dispatch <| RefreshWaveSim wsModel 
        else
            dispatch <| SetWSModelAndSheet ({ wsModel with State = NonSequential }, wsSheet)
           
/// ReactElement showing instructions and wave sim buttons
let topHalf canvasState (model: Model) dispatch : ReactElement * bool =
    let title =
        match model.WaveSimSheet with
        | None -> "Waveform Viewer for:", model.WaveSimOrCurrentSheet
        | Some sheet -> "Simulating:", sheet
        |> fun (text,sheet) ->
            div [Style [WhiteSpace WhiteSpaceOptions.Nowrap]]
                [str text  ; span [Style [Color "#3e8ed0"; MarginLeft "5px"]] [str $"{sheet}"]]
    let wsModel = getWSModel model
    //printfn $"Active wsModel sheet={model.WaveSimSheet}, state = {wsModel.State}"
    //printfn $"""Wavesim states: {model.WaveSim |> Map.toList |> List.map (fun (sh, ws) -> sh, ws.State.ToString(),ws.Sheets)}"""
    let loading =
        match wsModel.State with
        | Loading -> true
        | _ -> false

    let titleLine() =       
        div [ Style [
                inlineNoWrap;
                MarginBottom (if model.WaveSimSheet = None then "50px" else "20px")
                FontSize "24px"
                LineHeight "24px"
                FontWeight 600
                OverflowX OverflowOptions.Hidden ;
                Display DisplayOptions.Inline;
                ];

              Id "WaveSimHelp"] [title]

    let refreshStartEndButton() =
        let refreshButtonSvg = if loading then emptyRefreshSVG else refreshSvg "white" "20px"
        let startOrRenew model = refreshButtonAction canvasState model dispatch
        let waveEnd model = endButtonAction canvasState model dispatch
        let wbo = getWaveSimButtonOptions canvasState model wsModel
        let startEndButton =
            button 
                (topHalfButtonProps wbo.StartEndColor "startEndButton" false) 
                (fun ev -> dispatch <| ExecFuncInMessage(
                    (fun model _ -> if wbo.IsRunning then waveEnd model ev  else startOrRenew model ev),dispatch))
                (str wbo.StartEndMsg)
        let needsRefresh = wbo.IsDirty && wbo.IsRunning
        div 
            [Style [inlineNoWrap]]                     
            (if not wbo.IsRunning then [
                startEndButton
            ] 
            else [
                if needsRefresh then
                    button
                        (topHalfButtonProps IsSuccess "RefreshButton" false)
                        (fun ev -> dispatch <| ExecFuncInMessage((fun model _ -> startOrRenew model ev), dispatch))
                        refreshButtonSvg
                startEndButton
            ])

    let messageOrControlButtons =
        let simError e =
            SimulationView.setSimErrorFeedback e model dispatch
            div [ errorMessageStyle ]
                [ SimulationView.viewSimulationError canvasState e model WaveSim dispatch ]

        let notRunning = 
            false, div [ errorMessageStyle ] [ str "Start the waveform viewer by pressing the Start button." ]

        match model.WaveSimSheet, wsModel.State with
        | Some sheet as sheetOpt, SimError e when sheetOpt <> getCurrFile model ->
            dispatch <| UpdateModel( fun model -> {model with WaveSimSheet = None})
            dispatch <| UpdateModel( updateWSModelOfSheet sheet (fun ws -> {ws with State = Ended}))
            notRunning

        | None, SimError e  ->
            notRunning

        | _,SimError e ->
            false, simError e
            
        | _,NonSequential ->
            false, div [ errorMessageStyle ] [ str "There is no clocked logic in this circuit. Add clocked logic to simulate waveforms." ]

        | _,Empty | _,Ended | None,_ | Some "", _->
            notRunning

        | Some sheet, _ when wsModel.FastSim.SimulatedTopSheet = "" ->
            notRunning
        
        | _,NoProject ->
            false, div [ errorMessageStyle ] [ str "Please open a project to use the waveform viewer." ]

        | _, (Loading | Success) when List.isEmpty wsModel.SelectedWaves && Map.isEmpty wsModel.SelectedRams->
            false, div [Id "WaveSimHelp"] [str "Use 'Select Waves' to add waves for simulation. Right-click for help."]

        | _, Success ->
            true, div [Style [Height Constants.rowHeight; Display DisplayOptions.Flex; JustifyContent "space-between"; Margin "5px"; MarginTop "30px" ; MarginBottom "15px"]]  [

                        zoomButtons wsModel dispatch

                        multiplierMenuButton wsModel dispatch
                        
                        WaveSimWaveforms.radixButtons wsModel dispatch
  
                        clkCycleButtons wsModel dispatch
                    ]
        | _ -> notRunning

    let needsBottomHalf, messageOrControlLine = messageOrControlButtons

    div [ topHalfStyle ] [
        titleLine()
 
        div [Style [MarginTop 20.; Display DisplayOptions.Flex; JustifyContent "space-between"]] [
            refreshStartEndButton()
            div [Style [inlineNoWrap; Flex "0 1"]] [
                WaveSimSelect.selectWavesButton wsModel dispatch
                WaveSimSelect.selectRamButton wsModel dispatch]
            ]
        
        messageOrControlLine], needsBottomHalf
        
        

/// Entry point to the waveform simulator.
let viewWaveSim canvasState (model: Model) dispatch : ReactElement =
    let wsModel = getWSModel model

    let top, needsBottomHalf = topHalf canvasState model dispatch
    let needsRAMs = not <| Map.isEmpty wsModel.SelectedRams
    let height = calcWaveformAndScrollBarHeight wsModel
    let bottomHalf = // this has fixed height
        div [HTMLAttr.Id "BottomHalf" ; showWaveformsAndRamStyle (if needsRAMs then screenHeight() else height)] (
            if wsModel.SelectedWaves.Length > 0 then [
                WaveSimWaveforms.showWaveforms model wsModel dispatch               
                makeScrollbar wsModel dispatch ]
            else []
            @
            [WaveSimRams.ramTables wsModel] 
        )

    div [Style [OverflowX OverflowOptions.Clip]] [
        WaveSimSelect.selectRamModal wsModel dispatch
        WaveSimSelect.selectWavesModal wsModel dispatch
        div [ viewWaveSimStyle ]
            [
                //printfn $"WSmodel state: {wsModel.State}"
                top
                //hr [ Style [ MarginBottom "0px";  MarginTop "0px"]]
                
                if needsBottomHalf then bottomHalf else div [] []
                //hr [ Style [ MarginBottom "0px"; MarginTop "0px" ]]
            ]
        
    ]

