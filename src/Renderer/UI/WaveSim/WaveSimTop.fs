
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
open SimGraphTypes
open SimTypes
open WaveSimNavigation
open DiagramStyle

open WaveSimSVGs.Constants
open Optics
open Optics.Operators

/// Start or update a spinner popup
let setProgressBar (name:string) payload (numToDo:int) (model: Model) =
    match model.SpinnerPayload with
    | Some sp when sp.Name = name -> // continuation of an existing progress bar
        {model with SpinnerPayload = Some {sp with ToDo = numToDo}}
    | _ -> // A new progress bar is needed
        {model with SpinnerPayload = Some {UseProgressBar = true; Name = name; ToDo = numToDo; Total = numToDo + 1}}
    |> (fun model -> {model with RunAfterRenderWithSpinner = Some {FnToRun=payload; ButtonSpinnerOn=true}})

let setButtonSpinner payload (model: Model) =
    {model with SpinnerPayload = Some {UseProgressBar = false; Name = ""; ToDo = 0; Total = 0}}
    |> (fun model -> {model with RunAfterRenderWithSpinner = Some {FnToRun = payload; ButtonSpinnerOn = true}})

/// remove the spinner popup
let cancelSpinner (model:Model) =
    {model with SpinnerPayload = None}



/// Major function called after changes to extend simulation and/or redo waveforms.
/// Note that after design change simulation must be recreated externally, and the function called with
/// newSimulation = true. That is because this function has no way to know that the simulation has changed.
/// This function performs (as required) three actions.
/// 1. Extend the simulation to the current cycle (if not already done).
/// 2. Remake the Wave headers, one for each selected waveform.
/// 3. Remake (or make for first time) the saved waveform SVGs for all selected waveforms.
let rec refreshWaveSim (newSimulation: bool) (wsModel: WaveSimModel) (model: Model): Model * Elmish.Cmd<Msg> =
    // The function performs immediately the first part of the main long functions to determine their time and as needed splits
    // the rest of the work into multiple function calls using a spinner to alert the user to the delay.
    // The Spinner (in reality a progress bar) is used if the estimated time to completion is longer than
    // a constant. To get the estimate some initial execution must be completed (1 clock cycle and one waveform).

    /// Check whether two Wave structures are the same
    let isSameWave (wi:WaveIndexT) (wi': WaveIndexT) =
        wi.Id = wi'.Id && wi.PortNumber = wi'.PortNumber && wi.PortType = wi'.PortType

    /// This make the cursor control box focus after the next render.
    /// Necessary for intuitive UI typing in the cursor control box
    /// if this leads to simulation progress bars
    /// that otherwise remove focus.
    let dispatchFocusAfterRender model =
        let focusCurrClk1 _ model =
            let el = Browser.Dom.document.getElementById "clkCycleInput"
            if el <> null then el.focus()
            model
        { model with RunAfterRenderWithSpinner = Some <| Option.defaultValue {FnToRun=focusCurrClk1;ButtonSpinnerOn=false} model.RunAfterRenderWithSpinner }

           
    /// Make sure we always have consistent parameters. They will be written back to model after this function terminates.
    /// The validation may be done more than once because this function is recursive, but that is OK.
    /// validateSimparas is idempotent unless model changes.
    let wsModel =
        let createWaves (wsModel: WaveSimModel) =
            {wsModel with AllWaves = WaveSimSVGs.getWaves wsModel (Simulator.getFastSim())}
        validateSimParas wsModel
        |> if newSimulation then createWaves else id

    // Use the given (more uptodate) wsModel. This ensures it is returned from this function.
    let model = updateWSModel (fun _ -> wsModel) model

    // local containing the current fast simulation to be examined and extended if need be.
    let fs = Simulator.getFastSim()

    /// This is the highest simulation cycle that might be required in this simulation
    /// as determined by the current WSConfig. The limit for this refresh will be the minimum
    /// of this and what is required for the current view.
    let cycleLimit = 
        (wsModel.ShownCycles + wsModel.StartCycle)*wsModel.SamplingZoom //last shown cycle + 1, to get transitions
        |> min (wsModel.WSConfig.LastClock + Constants.maxStepsOverflow - 1 + wsModel.SamplingZoom) // cannot go beyond the array

    if cycleLimit >= fs.MaxArraySize then
        failwithf $"Sanity check failed: lastCycleNeeded = {cycleLimit} >= fs.MaxArraySize = {fs.MaxArraySize}"

    if fs.NumStepArrays = 0 then
        // Special case if simulation is empty there is nothing to do. Not sure why this is needed.
        model, Elmish.Cmd.none
            
    else
        // The simulation must be run to the last cycle needed for the current view.
        // This may require no work, in which case runFastSimulation will return immediately.
        // NB during waveform simulation the simulation buffer is NOT used as a circular buffer. Simulation
        // length is therefore limited to the size of the buffer.
        // All date from time = 0 is stored.

        /// This function calculates the last cycle needed for the simulation for the current view.
        let lastCycleNeeded wsModel =
            (wsModel.ShownCycles + wsModel.StartCycle)*wsModel.SamplingZoom + 1
            |> min cycleLimit
        /// This function is called when the simulation is running and the spinner is needed.
        /// It dispatches a continuation which will recursively call refreshWaveSim
        let runSimulationWithSpinner cyclesToDo model =
            let spinnerFunc = fun _dispatch model ->
                let wsModel = getWSModel model
                fst (refreshWaveSim false wsModel model)  // get model after refreshWaveSim has run
            let model =
                model
                |> setProgressBar $"Extending Circuit Simulation..." spinnerFunc cyclesToDo
            model, Elmish.Cmd.none
 
        FastRun.runFastSimulation (Some Constants.initSimulationTime) (lastCycleNeeded wsModel)  fs
        |> (fun speedOpt -> // if not None the fast simulation has timed out and has not yet completed
                let cyclesToDo = (lastCycleNeeded wsModel) - fs.ClockTick // may be negative

                match speedOpt with
                | Some speed when float cyclesToDo / speed + Constants.initSimulationTime > Constants.maxSimulationTimeWithoutSpinner ->
                    // The simulation is taking too long. We need to use a spinner.
                    runSimulationWithSpinner cyclesToDo model // A callback to refreshWaveSim is made dispatched from this function
                | _ ->
                    // Force simulation to finish now in case it is not finished.
                    // We know this will be quick enough not to need a spinner.
                    FastRun.runFastSimulation None (lastCycleNeeded wsModel) fs |> ignore
                            
                    // Simulation has now always finished so we can generate the waves
                    // this again may need to be done in a spinner if it takes too long.
                    // That decision is made below with the help of makeWaveformsWithTimeOut.

                    // Validate and update all parameters affecting waveforms.
                    let model =
                        updateViewerWidthInWaveSim model.WaveSimViewerWidth model
                        // cancel any spinner so that when a new one is started
                        // it will have teh correct total number of steps to do.
                        //|> (fun model -> {model with SpinnerPayload = None})
                    let wsModel =
                        getWSModel model
                        
                    // redo waves based on simulation data which is now correct
                    let allWaves = wsModel.AllWaves

                    let simulationIsStillUptodate = Simulator.getFastSim().ClockTick >= lastCycleNeeded wsModel                              
                    if not simulationIsStillUptodate then
                        // The simulation has changed due to viewer width change. We need to redo the simulation.
                        // This is done by calling refreshWaveSim again, we will come back here
                        // after the simulation has been redone and is uptodate.
                        // TODO: maybe the viewer width check should be earlier in this function?
                        refreshWaveSim newSimulation wsModel model
                    else
                        // need to use isSameWave here because array index may have changed
                        let wavesToBeMade =
                            allWaves
                            |> Map.filter (fun wi wave ->
                                // Only generate waveforms for selected waves.
                                // Regenerate waveforms whenever they have changed
                                let hasChanged = not <| WaveSimSVGs.waveformIsUptodate wsModel wave
                                let isSelected = List.exists (fun wi' -> isSameWave wi wi') wsModel.SelectedWaves
                                isSelected && hasChanged)
                            |> Map.toList                   
                            |> List.map fst
                        if wsModel.StartCycle < 0 then
                            failwithf $"Sanity check failed: wsModel.StartCycle = {wsModel.StartCycle}"
                        let spinnerInfo =  
                            let numToDo = wavesToBeMade.Length
                            WaveSimSVGs.makeWaveformsWithTimeOut (Some <| Constants.initWaveformTime ) wsModel  wavesToBeMade
                            |> (fun res ->
                                    match wavesToBeMade.Length - res.NumberDone, res.TimeTaken with
                                    | _, None | 0, _-> 
                                        {| WSM=res.WSM; SpinnerPayload=None; NumToDo=numToDo|} // finished
                                    | numToDo, Some t when float numToDo * t / float res.NumberDone < Constants.maxWaveCreationTimeWithoutSpinner ->
                                        let res2 = WaveSimSVGs.makeWaveformsWithTimeOut None res.WSM wavesToBeMade
                                        {| WSM= res2.WSM; SpinnerPayload=None; NumToDo = numToDo - res2.NumberDone|}
                                    | numToDo, _ ->
                                        if res.NumberDone = 0 && numToDo > 0 then
                                            printf $"No waves completed when {numToDo} are required. This is probably an error. Retrying refreshWaveSim"
                                        let payload = Some ("Updating Waveform Display", refreshWaveSim false res.WSM >> fst)
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
                            }

                        let model = putWaveSim ws model

                        match spinnerInfo.SpinnerPayload with
                        | None ->
                            cancelSpinner model
                            |> dispatchFocusAfterRender
                        | Some (spinnerName, spinnerAction) -> 
                            setButtonSpinner (fun _dispatch model -> spinnerAction model)  model
                        |> updateWSModel (fun _ -> {ws with DefaultCursor = Default})
                        |> (fun model -> model, Elmish.Cmd.none))

/// Refresh the state of the wave simulator according to the model and canvas state.
/// Redo a new simulation. Set inputs to default values. Then call refreshWaveSim via RefreshWaveSim message.
/// 1st parameter ofrefreshWaveSin will be set true which causes all waves to be necessarily regenerated.
let refreshButtonAction canvasState model dispatch = fun _ ->
    /// estimate of the memory resources used by this simulation
    let startWaveSimulation dispatch model =
        let waveSimArraySteps =
            ( 0, model.WaveSim)
            ||> Map.fold (fun sum sheet ws  ->
                    Simulator.getFastSim().MaxArraySize + sum)
        let waves =
            ( 0, model.WaveSim)
            ||> Map.fold (fun sum sheet ws  ->
                    ws.AllWaves.Count + sum)
        /// update the model memories to match any updated linked initial contents files
        let model = MemoryEditorView.updateAllMemoryComps model
        let wsSheet = 
            match model.WaveSimSheet with
            | None ->
                Option.get (getCurrFile model)
            | Some sheet ->
                sheet
        let model = 
            model
            |> removeAllSimulationsFromModel
            |> fun model -> {model with WaveSimSheet = Some wsSheet}
        let wsModel =
            getWSModel model
            |> fun wsModel -> {wsModel with ScrollbarBkgRepCycs= Constants.scrollbarBkgRepCyclesInit}
        //printfn $"simSheet={wsSheet}, wsModel sheet = {wsModel.TopSheet},{wsModel.FastSim.SimulatedTopSheet}, state={wsModel.State}"
        let simRes =
            // Here is where the new fast simulation is created
            ModelHelpers.simulateModel
                true
                model.WaveSimSheet
                (Constants.waveSimRequiredArraySize wsModel)
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
                dispatch <| UpdateWSModel (fun wsModel -> {wsModel with  DefaultCursor = Default})
            else
                dispatch <| SetWSModelAndSheet ({ wsModel with State = NonSequential}, wsSheet)
    dispatch <| RunAfterRender(true, fun dispatch model -> startWaveSimulation dispatch model; model)
    





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
        /// This is the only action for creating a new (or changed) waveform simulator fast simulation
        /// Once a simulation is created is maxClkCycle is fixed and cannot be changed. However the length
        /// of the simulation can be changed by extending the simulation to any value less than its maxClkCycle.
        let startOrRenew model =
            refreshButtonAction canvasState model dispatch

        let waveEnd model = endButtonAction canvasState model dispatch
        let wbo = getWaveSimButtonOptions canvasState model wsModel
        let isLoading =
            match model.RunAfterRenderWithSpinner, model.SpinnerPayload with
            | Some {ButtonSpinnerOn = true}, _ -> true
            | _ , Some _ -> true
            | _ -> false
        let startEndButton =
            button 
                (topHalfButtonPropsLoading isLoading wbo.StartEndColor "startEndButton" false) 
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
    //printfn $"WaveSimTop: viewWaveSim called with spinner = {wsModel.DefaultCursor.Text()}."
    div [
        Style [
            OverflowX OverflowOptions.Clip;
            Cursor <| wsModel.DefaultCursor.Text()
        ]
    ] [
        WaveSimSelect.selectRamModal wsModel dispatch
        WaveSimSelect.selectWavesModal wsModel dispatch
        div [ viewWaveSimStyle ]
            [
                top
                if needsBottomHalf && (match model.SpinnerPayload with | Some {UseProgressBar=true  } -> false | _ -> true) then bottomHalf() else div [] []
            ]
        
    ]

