
/// Top-level functions for Waveform Simulator
module WaveSimTop

//---------------------------------------------------------------------------------------//
//-----------------------Top-level functions for Waveform Simulator---------------------//
//---------------------------------------------------------------------------------------//

// refreshWaveSim - updates the simulation and/or waveforms to match current WaveSimModel parameters
// viewWaveSim - creates the DOM of the waveform simulator

open Fulma
open Fable.React
open Fable.React.Props

open CommonTypes
open ModelType
open ModelHelpers
open WaveSimStyle
open WaveSimHelpers
open SimulatorTypes
open WaveSimNavigation
open DiagramStyle

open WaveSimSVGs.Constants

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

/// module used to have some local types
module Refresh =

    type SimulationT =
        | FinishedSim
        | FinishSimNow
        | ContinueSimWithSpinner
        | ContinueSimAfterStartingSpinner

    /// Major function called after changes to extend simulation and/or redo waveforms.
    /// Note that after design change simulation must be recreated externally, and the function called with
    /// newSimulation = true. That is because this function has no way to know that the simulation has changed
    /// This function performs (as required) three actions.
    /// 1. Extend the simulation to the current cycle (if not already done).
    /// 2. Remake the Wave headers, one for each selected waveform.
    /// 3. Remake (or make for first time) the saved waveform SVGs for all selected waveforms.
    let rec refreshWaveSim (newSimulation: bool) (wsModel: WaveSimModel) (model: Model): Model * Elmish.Cmd<Msg> =
        // The function performs the first part of the main long functions to determine their time and as needed splits
        // the rest of the work into multiple function calls using a spinner to alert the user to the delay.
        // The Spinner (in reality a progress bar) is used if the estimated time to completion is longer than
        // a constant. To get the estimate some initial execution must be completed (1 clock cycle and one waveform).
        /// check whether two Wave structures are the same
        let isSameWave (wi:WaveIndexT) (wi': WaveIndexT) =
            wi.Id = wi'.Id && wi.PortNumber = wi'.PortNumber && wi.PortType = wi'.PortType
        /// Make sure we always have consistent parameters. They will be written back to model after this function terminates.
        /// the validation may be done more than onece because this function is recursive, but that is OK.
        /// validateSimparas is idempotent unless model changes.
        let wsModel =
            let createWaves (wsModel: WaveSimModel) =
                {wsModel with AllWaves = WaveSimSVGs.getWaves wsModel (Simulator.getFastSim())}
            validateSimParas wsModel
            |> if newSimulation then createWaves else id
        // Use the given (more uptodate) wsModel. This will also be updated in model when it is returned from this function
        let model = updateWSModel (fun _ -> wsModel) model

        // start timing - used to decide whether all can be done in one go.
        let start = TimeHelpers.getTimeMs ()
        // special case if simulation is empty there is nothing to do. Not sure why this is needed.
        let fs = Simulator.getFastSim()
        if fs.NumStepArrays = 0 then
            model, Elmish.Cmd.none
        else
            // The simulation must be run to the last cycle needed for the current view.
            // This may require no work, in which case runFastSimulation will return immediately.
            // NB during waveform simulation the simulation buffer is NOT used as a circular buffer. Simulation
            // length is therefore limited to the size of the buffer.
            // All date from time = 0 is stored.
            let lastCycleNeeded = (wsModel.StartCycle + wsModel.ShownCycles - 1) * wsModel.CycleMultiplier + 1

            FastRun.runFastSimulation (Some Constants.initSimulationTime) lastCycleNeeded fs
            |> (fun speedOpt -> // if not None the simulation has timed out and has not yet complete
                    let cyclesToDo = lastCycleNeeded - fs.ClockTick // may be negative
                    let action =
                        match speedOpt, Option.isSome model.Spinner with
                        | None, _ ->
                            FinishedSim
                        | Some speed, false when float cyclesToDo / speed + Constants.initSimulationTime > Constants.maxSimulationTimeWithoutSpinner ->
                            ContinueSimAfterStartingSpinner
                        | Some speed, false ->
                            FinishSimNow
                        | Some speed, true ->
                            ContinueSimWithSpinner          
                    match action with
                    | ContinueSimAfterStartingSpinner ->
                        // long simulation, set spinner on and dispatch another refresh 
                        let spinnerFunc = fun model ->
                            fst (refreshWaveSim false wsModel model)
                        let model = model |> updateSpinner "Waveforms simulation..." spinnerFunc cyclesToDo
                        //printfn "ending refresh with continuation..."
                        model, Elmish.Cmd.none
                        |> TimeHelpers.instrumentInterval "refreshWaveSim" start
                    | ContinueSimWithSpinner
                    | FinishSimNow
                    | FinishedSim ->
                        if action = FinishSimNow || action = ContinueSimWithSpinner then 
                            // force simulation to finish now
                            FastRun.runFastSimulation None lastCycleNeeded fs |> ignore
                            
                        // simulation has now always finished so can generate waves

                        let model = updateViewerWidthInWaveSim model.WaveSimViewerWidth model
                        let wsModel = getWSModel model
                        
                        // redo waves based on simulation data which is now correct
                        let allWaves = wsModel.AllWaves

                        let simulationIsUptodate = Simulator.getFastSim().ClockTick > (wsModel.ShownCycles + wsModel.StartCycle-1)*wsModel.CycleMultiplier

                        // need to use isSameWave here because array index may have changed
                        let wavesToBeMade =
                            allWaves
                            |> Map.filter (fun wi wave ->
                                // Only generate waveforms for selected waves.
                                // Regenerate waveforms whenever they have changed
                                let hasChanged = not <| WaveSimSVGs.waveformIsUptodate wsModel wave
                                let isSelected = List.exists (fun wi' -> isSameWave wi wi') wsModel.SelectedWaves
                                //printfn $"Wave {wi.SimArrayIndex} hasChanged={hasChanged} isSelected={isSelected}"
                                //if List.contains index ws.SelectedWaves then
                                isSelected && hasChanged && simulationIsUptodate)
                            |> Map.toList                   
                            |> List.map fst
                        if wsModel.StartCycle < 0 then
                            failwithf $"Sanity check failed: wsModel.StartCycle = {wsModel.StartCycle}"
                        let endCycle = (wsModel.StartCycle + wsModel.ShownCycles - 1) * wsModel.CycleMultiplier
                        if endCycle > wsModel.WSConfig.LastClock then
                            failwithf $"Sanity check failed: EndCycle ({endCycle}) > maxLastClk ({wsModel.WSConfig.LastClock})"
                        //printfn $"Waves to be made:{wavesToBeMade.Length}, sim uptodate = {simulationIsUptodate}"
                        //printfn $"Waves to be made:{wavesToBeMade.Length}, sim uptodate = {simulationIsUptodate}"
                        let spinnerInfo =  
                            //printfn $"{wavesToBeMade.Length} waves to make."
                            let numToDo = wavesToBeMade.Length
                            //printfn $"numToDo = {numToDo}"
                            WaveSimSVGs.makeWaveformsWithTimeOut (Some Constants.initSimulationTime) wsModel  wavesToBeMade
                            |> (fun res ->
                                    match wavesToBeMade.Length - res.NumberDone, res.TimeTaken with
                                    | n, None -> 
                                        {| WSM=res.WSM; SpinnerPayload=None; NumToDo=numToDo|} // finished
                                    | _ when res.NumberDone = 0 -> 
                                        failwithf "What? makewaveformsWithTimeOut must make at least one waveform"
                                    | numToDo, Some t when float numToDo * t / float res.NumberDone < Constants.maxSimulationTimeWithoutSpinner ->
                                        let res2 = WaveSimSVGs.makeWaveformsWithTimeOut None res.WSM wavesToBeMade
                                        {| WSM= res2.WSM; SpinnerPayload=None; NumToDo = numToDo - res2.NumberDone|}
                                    | numToDo, _ ->
                                        let payload = Some ("Making waves", refreshWaveSim false res.WSM >> fst)
                                        {| WSM=res.WSM; SpinnerPayload=payload; NumToDo=numToDo|})

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
                                    AllWaves = spinnerInfo.WSM.AllWaves
                                    SelectedWaves = selectedWaves
                                    RamComps = ramCompIds
                                    SelectedRams = selectedRams
                            } //|> validateScrollBarInfo

                        let model = putWaveSim ws model

                        let model = 
                            match spinnerInfo.SpinnerPayload with
                            | None -> cancelSpinner model
                            | Some (spinnerName, spinnerAction) -> 
                                updateSpinner spinnerName spinnerAction spinnerInfo.NumToDo model
                            |> updateWSModel (fun _ -> ws)
                        model, Elmish.Cmd.none)
                        //|> TimeHelpers.instrumentInterval "refreshWaveSim" start)


/// Refresh the state of the wave simulator according to the model and canvas state.
/// Redo a new simulation. Set inputs to default values. Then call refreshWaveSim via RefreshWaveSim message.
/// 1st parameter ofrefreshWaveSin will be set true which causes all waves to be necessarily regenerated.
let refreshButtonAction canvasState model dispatch = fun _ ->
    let waveSimArraySteps =
        ( 0, model.WaveSim)
        ||> Map.fold (fun sum sheet ws  ->
                Simulator.getFastSim().MaxArraySize + sum)
    let waves =
        ( 0, model.WaveSim)
        ||> Map.fold (fun sum sheet ws  ->
                ws.AllWaves.Count + sum)
    printf $"Starting wavesim with {waveSimArraySteps} steps and {waves} waves."

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
    let simRes =
        ModelHelpers.simulateModel
            true
            model.WaveSimSheet
            (wsModel.WSConfig.LastClock + ModelHelpers.Constants.maxStepsOverflow)
            canvasState model

    match simRes with
    //| None ->
    //    dispatch <| SetWSModel { wsModel with State = NoProject; FastSim = FastCreate.emptyFastSimulation "" }
    | (Error e, _) ->
        dispatch <| SetWSModelAndSheet ({ wsModel with State = SimError e }, wsSheet)
    | (Ok simData, canvState) ->
        if simData.IsSynchronous then
            SimulationView.setFastSimInputsToDefault simData.FastSim
            let wsModel = { wsModel with State = Loading}
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
                MarginBottom (if model.WaveSimSheet = None then "50px" else "10px")
                FontSize "24px"
                LineHeight "24px"
                FontWeight 600
                OverflowX OverflowOptions.Clip ;
                Display DisplayOptions.Inline;
                Height Constants.rowHeight
                Flex "0 0.5"
                AlignSelf AlignSelfOptions.FlexStart
                MarginRight 5
                MarginLeft 5
                ];

              Id "WaveSimHelp"]
              [
                title
              ]

    let refreshStartEndButton() =
        let refreshButtonSvg = if loading then emptyRefreshSVG else refreshSvg "white" "20px"
        let startOrRenew model = refreshButtonAction canvasState model dispatch
        let waveEnd model = endButtonAction canvasState model dispatch
        let wbo = getWaveSimButtonOptions canvasState model wsModel
        let startEndButton =
            button 
                (topHalfButtonProps wbo.StartEndColor "startEndButton" false) 
                (fun ev -> dispatch <| ExecFuncInMessage((fun model _ ->
                                if wbo.IsRunning then waveEnd model ev  else startOrRenew model ev),dispatch)
                           dispatch <| ExecFuncInMessage ((fun model _ -> ()), dispatch))
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

    let needsBottomHalf, messageOrControlLine =
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

        | Some sheet, _ when Simulator.getFastSim().SimulatedTopSheet = "" ->
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


    div [ topHalfStyle ] [
        div [Style [MarginTop 20.; Display DisplayOptions.Flex; JustifyContent "space-between"]] [
            titleLine()
            UIPopups.makeWSConfigButton dispatch model
            waveInfoButton (match wsModel.State with | Success -> "Instructions" | _ ->"Getting Started") dispatch
        ]
 
        div [Style [MarginTop 15.; Display DisplayOptions.Flex; JustifyContent "space-between"]] [
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
    let bottomHalf() = // this has fixed height
        div [HTMLAttr.Id "BottomHalf" ; showWaveformsAndRamStyle (if needsRAMs then screenHeight() else height)] (
            if wsModel.SelectedWaves.Length > 0 then [
                WaveSimWaveforms.showWaveforms model wsModel dispatch               
                makeScrollbar wsModel dispatch ]
            else []
            @
            [WaveSimRams.ramTables dispatch wsModel model] 
        )

    div [Style [OverflowX OverflowOptions.Clip]] [
        WaveSimSelect.selectRamModal wsModel dispatch
        WaveSimSelect.selectWavesModal wsModel dispatch
        div [ viewWaveSimStyle ]
            [
                //printfn $"WSmodel state: {wsModel.State}"
                top
                //hr [ Style [ MarginBottom "0px";  MarginTop "0px"]]
                
                if needsBottomHalf then bottomHalf() else div [] []
                //hr [ Style [ MarginBottom "0px"; MarginTop "0px" ]]
            ]
        
    ]

