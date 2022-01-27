//(*
//WaveformSimulationView.fs
//
//View for waveform simulator in tab
//*)
//
module rec WaveformSimulationView
//
//(*******************************************************************************************
//
//Waveform simulation simulates the current sheet circuit and generates waveforms as react SVG elements
//on an SVG canvas. The waveform display changes interactively based on zoom and cursor movement buttons.
//
//In addition the SVG canvas is an element wider then the pane in which it is displayed, and uses HTML scrolling
//navigate. Zooming will recreate a new (wider) canvas as necessary so that the entire visible window contains valid
//waveforms.
//
//In addition the canvas consists only of those waveforms currently selected to be displayed, via a wave editor pane
//which displayed the names of all possible waveforms and allows them to be displayed or not. See waveeditor below
//for details.
//
//When the selected waveforms are changed the SVG canvas must also be recreated with new waveforms, although the
//simulation need not be advanced.
//
//Waveform simulation is initiated by the Waveforms >> button. This button is recreated by mainview a suitable color:
//green: a changed (or initial) circuit exists which can be (re-)simulated with waveform simulator
//orange: errors exist in circuit - no change to waveform simulation.
//white: circuit has not chnaged and is still the same as what is currently displaying waveforms
//
//The waveforms last simulated are thus displayed while the circuit can be updated, until the button is clicked. On
//this click, if the circuit has errors the old waveforms are replaced by an error message, otherwise a new simulation
//replaces the old simulation.
//
//Allowing simultaneous view (and interactive change as above) of old waveforms while letting the design be changed
//requires careful state management:
//
//model.LastSimulatedCanvasState - circuit last simulated
//model.Simulation - time = 0 SimGraph or simulation error for LastSimulatedCanvasState. A simulation can be extended by advancing the 
//SimGraph one or more simulation steps.
//
//Wavesim specific parameters and temporary state are inside a WaveSimModel record accessed via a Map field in the model:
//
//WaveSim: Map<string,waveSimModel>
//accessed as:
//model.WaveSim[openSheetName]: WaveSimModel
//
//TODO: The WaveSimModel is only valid when a project is open and the Wavesim data in reality should be part of
//the current sheet LoadedComponent record, tus WaveSim record moves inside LoadedComponent record and no longer
//needs to be a Map.
//
//The WaveSimModel data is changed on opening a different sheet or saving / restoring a project in exactly the same way as the
//rest of the sheet data LoadedComponentData. The simulation parameters last used for each sheet are saved and restored. Inside
//WaveSim a subrecord WaveSim.WaveData contains transient information about the current simulation which is not saved across sheet
//changes. In addition the field SimDataCache is transient and replaced whenever LastSimulatedCanvasState changes.
//
//TODO: make SimDataCache a field of WaveData. Move WaveData from WaveSimModel to Model - so there is just one copy of it, not
//one per sheet.
//
//waveform Simulation State Changes
//
//(1) SimulateButtonFunc definition:
//waveforms >> button pressed with SimIsStale = true, makeSimData model returns (Ok simData), Canvas exists and contains canvasData
//
//--> startNewWaveSimulation: this updates model with 
//        SimIsStale = false, 
//        WaveSim data for current sheet (waveSim) all initialised, 
//        waveSim.WaveData initialised from simData and canvasData
//        LastSimulatedCanvasState updated to equal the new circuit (?)
//
//*******************************************************************************************)
//
open Fulma
open Fable.React
open Fable.React.Props


open ModelType
open DiagramStyle
open CommonTypes
open WaveSimHelpers
open FileMenuView
open SimulatorTypes



/// maximum width of the waveform simulator viewer
let maxUsedViewerWidth (wSMod: WaveSimModel) =
    let strWidth s = 
        DrawHelpers.getTextWidthInPixels (s, "12px segoe ui") //not sure which font
    let curLblColWidth =
        match cursorValueStrings wSMod with
        | [||] -> 0.0
        | cVS ->
            Array.map (Array.map strWidth >> Array.max) cVS
            |> Array.max
            |> max 25.0
    let namesColWidth =
        match wSMod.SimParams.DispNames with
        | [||] -> 0.0
        | wN ->
            Array.map strWidth wN
            |> Array.max
            |> max 100.0
    let svgWaveColWidth =
        match dispWaves wSMod with
        | [||] -> 600.0
        | _ -> maxWavesColWidthFloat wSMod
    let checkBoxCol = 25.0
    let extraWidth = 45.0 
    
    curLblColWidth + namesColWidth + svgWaveColWidth + checkBoxCol + extraWidth |> int






///////////////////////
//// WaveSim actions //
///////////////////////

/// change selection of a waveform's connections
let private changeWaveConnsSelect  (selFun: WaveformSpec -> bool) (model:Model) (wSModel: WaveSimModel) name (dispatch: Msg -> unit) =
    let wave =  wSModel.AllWaves[name] 
    let on = selFun wave
    match wave.WType with
    | ViewerWaveform _ -> 
        let wSModel' = {wSModel with AllWaves = Map.add wave.WId {wave with WType = ViewerWaveform on} wSModel.AllWaves}
        dispatch <| SetWSMod wSModel'
    | NormalWaveform ->
        selectWaveConns model (selFun wave) wave  dispatch

/// toggle selection of a waveform's connections
let private toggleWaveConnsSelect (model: Model) (wSMod: WaveSimModel) (name:string) (dispatch: Msg -> unit) =
    changeWaveConnsSelect (fun wave -> isWaveSelected model wave |> not) model wSMod name dispatch

/// stretch waveforms horizontally
let private changeViewerZoom compIds plus (m: Model) (wsModel: WaveSimModel) dispatch =
    let rec adjustLastClk viewW wsModel =
        let pars = wsModel.SimParams
        if viewW * 1.4 < float (maxUsedViewerWidth wsModel) then
            adjustLastClk viewW (setSimParams (fun sp -> {sp with LastClkTime = pars.LastClkTime + 2u}) wsModel)
        else
            printfn "New LastClk=%d" pars.LastClkTime
            pars.LastClkTime
        
    let netList = wsModel2netList wsModel
    let pars = wsModel.SimParams
    let newClkW =
        if plus then zoomFactor else 1.0 / zoomFactor
        |> (*) pars.ClkSvgWidth
        |> max minZoom
        |> min maxZoom
    let wSModNewClk = setSimParams (fun sp -> {sp with ClkSvgWidth=newClkW}) wsModel
    let newPars =
        match int (float m.WaveSimViewerWidth) > maxUsedViewerWidth wSModNewClk with
        | true ->
                { pars with 
                    LastClkTime = adjustLastClk (float m.WaveSimViewerWidth) wSModNewClk
                    ClkSvgWidth = newClkW
                }
        | false ->  
                {pars with 
                    ClkSvgWidth = newClkW 
                }
    dispatch <| InitiateWaveSimulation(WSViewerOpen, newPars)
        

/// change cursor value
let private changeCursorPos (wSModel: WaveSimModel) dispatch newCursorPos =
    let pars = wSModel.SimParams
    let curs' = min maxLastClk newCursorPos
    match 0u <= curs', curs' <= pars.LastClkTime with
    | true, true ->
        wSModel
        |> setSimParams (fun sp -> {sp with CursorTime = curs' })
        |> SetWSMod |> dispatch
        UpdateScrollPos true |> dispatch
    | true, false ->
        let pars' = { pars with CursorTime = curs'; ClkSvgWidth = pars.ClkSvgWidth; LastClkTime = pars.LastClkTime }
        dispatch <| InitiateWaveSimulation(WSViewerOpen, pars')
        UpdateScrollPos true |> dispatch
    | false, _ -> ()

/// change cursor value by 1 up or down
let private cursorMove increase (wSMod: WaveSimModel) dispatch =
    match increase, wSMod.SimParams.CursorTime with
    | true, n -> n + 1u |> changeCursorPos wSMod dispatch
    | false, n -> n - 1u |> changeCursorPos wSMod dispatch

/// change the order of the waveforms in the simulator
let private moveWave (model:Model) (wSMod: WaveSimModel) up =
    let moveBy = if up then -1.5 else 1.5
    let addLastPort arr p =
        Array.mapi (fun i el -> if i <> Array.length arr - 1 then el
                                else fst el, Array.append (snd el) [| p |]) arr
    let svgCache = wSMod.DispWaveSVGCache
    let movedNames =
        wSMod.SimParams.DispNames
        |> Array.map (fun name -> isWaveSelected model wSMod.AllWaves[name], name)
        |> Array.fold (fun (arr, prevSel) (sel,p) -> 
            match sel, prevSel with 
            | true, true -> addLastPort arr p, sel
            | s, _ -> Array.append arr [| s, [|p|] |], s ) ([||], false)
        |> fst
        |> Array.mapi (fun i (sel, ports) -> if sel
                                               then float i + moveBy, ports
                                               else float i, ports)
        |> Array.sortBy fst
        |> Array.collect snd 
    setDispNames movedNames wSMod
    |> SetWSMod

let private viewerWaveSet (wavesToSet: WaveformSpec array) (on:bool) (wSMod: WaveSimModel) dispatch =
    let waves =
        wSMod.AllWaves
        |> Map.map (fun wid wave -> 
            match wave.WType with
            | ViewerWaveform _ when Array.contains wave wavesToSet ->
                {wave with WType = ViewerWaveform on}
            | _ -> wave)
    dispatch <| SetWSMod {wSMod with AllWaves = waves}


/// select all waveforms in the WaveAdder View
let private waveAdderSelectAll model (wSMod: WaveSimModel) (on: bool) dispatch =
    let waves = mapValues wSMod.AllWaves
    let conns =
        match  on with
        | true -> 
            waves
            |> Array.collect (fun wave -> wave.Conns)
        | false ->
            [||]
    selectExactConns model conns dispatch
    viewerWaveSet waves on wSMod dispatch

let showSimulationLoading (wsModel: WaveSimModel) (dispatch: Msg ->Unit) =
    let nv = wsModel.WSTransition
    let v = wsModel.WSViewState
    let resetV waves =
         waves
         |> Map.map (fun wid wave -> 
             match wave.WType with
             | ViewerWaveform _  ->
                 {wave with WType = ViewerWaveform false}
             | _ -> wave)
    match nv, v with
    | None, _ -> false
    | Some _, _ -> 
        dispatch <| WaveSimulateNow
        dispatch <| Sheet Sheet.ResetSelection
        dispatch <| UpdateWSModel (fun wsm -> {wsm with AllWaves = resetV wsm.AllWaves})
        true


//////////////////////////////////////////////
// ReactElements of the Waveformm Simulator //
//////////////////////////////////////////////


/// labels displayed in the right column of the simulator
let private makeCursVals wsModel  =
    let string2Lbl = Array.map (fun l -> label [ Class "cursVals" ] [ str l ])
    Array.map string2Lbl <| cursorValueStrings wsModel

let private makeRamReactCol (wsModel: WaveSimModel) ramPath =
    let makeReactEl l = 
        
        [| label [ Class "ramVals"; Style [FontSize "12px"] ] [ str l ] |]
    let ramName, ramLocs = getRamInfoToDisplay wsModel ramPath
    [| label [ Class "ramVals"; ] [p [Style [FontWeight "Bold"; TextAlign TextAlignOptions.Center]] [str ramName]] |] :: List.map makeReactEl ramLocs
    |> List.toArray



/// tuple of React elements of middle column, left column, right column.
/// shows waveforms and labels and cursor col.
/// The vertical order is fixed and as in DispNames
let private waveSimViewerRows compIds model (wsMod: WaveSimModel) (dispatch: Msg -> unit) =
    let allWaves = wsMod.AllWaves
    let names = wsMod.SimParams.DispNames
    let labelCols =
        names
        |> makeLabels 
        |> Array.zip names
        |> Array.map (fun (name,lab) ->
            if Map.tryFind name allWaves = None then
                printfn "Help - cannot lookup %A in allwaves for label %A" name lab
                failwithf "Terminating!"
            tr [ Class "rowHeight" ]
                [ td [ Class "checkboxCol" ]
                      [ input
                          [ Type "checkbox"
                            Class "check"
                            Checked <| isWaveSelected model allWaves[name]
                            Style [ Float FloatOptions.Left ]
                            OnChange(fun _ -> toggleWaveConnsSelect model wsMod name dispatch) ] ]
                  td
                      [ Class "waveNamesCol"
                        Style [ TextAlign TextAlignOptions.Right ] ] [ lab ] ])

    let cursValCol = 
        makeCursVals wsMod
        |> Array.map (fun c -> tr [ Class "rowHeight" ] [ td [ Class "cursValsCol" ] c ])

    wsMod.DispWaveSVGCache, labelCols, cursValCol

/// ReactElement of the tabs for changing displayed radix
let private radixTabs (wsModel: WaveSimModel) dispatch =
    let radixString =
        [ Dec,  "uDec"
          Bin,  "Bin"
          Hex,  "Hex"
          SDec, "sDec" ] |> Map.ofList

    let radTab rad =
        Tabs.tab
            [ Tabs.Tab.IsActive(wsModel.SimParams.WaveViewerRadix = rad)
              Tabs.Tab.Props
                  [ Style
                      [ Width "35px"
                        Height "30px" ] ] ]
            [ a
                [ Style
                    [ Padding "0 0 0 0"
                      Height "30px" ]
                  OnClick(fun _ ->
                      InitiateWaveSimulation (WSViewerOpen,{wsModel.SimParams with WaveViewerRadix = rad})
                      |> dispatch) ] [ str (radixString[rad]) ] ]
    Tabs.tabs
        [ Tabs.IsToggle
          Tabs.Props
              [ Style
                  [ Width "140px"
                    Height "30px"
                    FontSize "80%"
                    Float FloatOptions.Right
                    Margin "0 10px 0 10px" ] ] ]
        [ radTab Bin
          radTab Hex
          radTab Dec
          radTab SDec ]

/// ReactElement of the buttons for changing the cursor value
let private cursorButtons (model: Model) wSMod dispatch =
    div [ Class "cursor" ]
        [ Button.button
            [ Button.CustomClass "cursLeft"
              Button.OnClick(fun _ -> cursorMove false wSMod dispatch) ] [ str "◀" ]
          Input.number
              [ Input.Props
                  [ Min 0
                    Class "cursor form"
                    SpellCheck false
                    Step 1 ]
                Input.Id "cursor"
                match currWaveSimModel model with
                | Some wSMod when wSMod.CursorBoxIsEmpty = false -> 
                    string wSMod.SimParams.CursorTime
                | Some _ -> ""
                | None -> "0"
                |> Input.Value 
                Input.OnChange(fun c ->
                    match System.Int32.TryParse c.Value with
                    | true, n when n >= 0 -> 
                        { wSMod with CursorBoxIsEmpty = false }
                        |> SetWSMod |> dispatch
                        changeCursorPos wSMod dispatch <| uint n
                    | false, _ when c.Value = "" -> 
                        { wSMod with CursorBoxIsEmpty = true }
                        |> SetWSMod |> dispatch
                        changeCursorPos wSMod dispatch 0u
                    | _ -> 
                        { wSMod with CursorBoxIsEmpty = false }
                        |> SetWSMod |> dispatch ) ]
          button [ Button.CustomClass "cursRight" ] (fun _ -> cursorMove true wSMod dispatch) "▶" ]

/// ReactElement of the loading button
let private loadingButton wsMod dispatch =
    if showSimulationLoading wsMod dispatch then 
        button [Button.Color Color.IsBlackBis; Button.IsLoading true] (fun _ -> ()) ""
    else str ""

/// React Element of the buttons Bar at the top of the waveform simulator
let private viewWaveSimButtonsBar model wSMod dispatch =
    div [ Style [ Height "45px" ] ]
        [ loadingButton wSMod dispatch
          radixTabs wSMod dispatch
          cursorButtons model wSMod dispatch ]

/// ReactElement of the right column of the waveform simulator
let private cursorValuesCol rows =
    let rightCol = Array.append [| tr [ Class "rowHeight" ] [ td [ Class "rowHeight" ] [] ] |] rows
    div
        [ Style
            [ Float FloatOptions.Right
              Height "100%"
              BorderTop "2px solid rgb(219,219,219)"
              BorderLeft "2px solid rgb(219,219,219)" ] ] [ table [] [ tbody [] rightCol ] ]

/// ReactElement of the waveforms' name labels column
let private nameLabelsCol model (wsMod: WaveSimModel) labelRows (dispatch: Msg -> unit) =
    let waveAddDelBut =        
        th [ Class "waveNamesCol" ] 
           [ Button.button
           [ Button.CustomClass "newWaveButton"
             Button.Color IsSuccess
             Button.OnClick(fun _ -> 
                openEditorFromViewer model  dispatch) ] [ str "Edit list..." ] ]

    let top =
        [| tr [ Class "rowHeight" ]
               [ th [ Class "checkboxCol" ]
                     [ div [ Class "updownDiv" ]
                     [ Button.button
                         [ Button.CustomClass "updownBut"
                           Button.OnClick(fun _ -> moveWave model wsMod true |> dispatch) ] [ str "▲" ]
                       Button.button
                           [ Button.CustomClass "updownBut"
                             Button.OnClick(fun _ -> moveWave model wsMod false |> dispatch) ] [ str "▼" ] ] ]
                 waveAddDelBut ] |]

    let bot =
        [| tr [ Class "fullHeight" ]
               [ td [ Class "checkboxCol" ] []
                 td [] [] ] |]

    let leftCol = Array.concat [| top; labelRows; bot |]

    div
        [ Style
            [ Float FloatOptions.Left
              Height "100%" ] ] [ table [ Class "leftTable" ] [ tbody [] leftCol ] ]

/// ReactElement of the waveform SVGs' column
let private allWaveformsTableElement model (wSModel: WaveSimModel) waveformSvgRows dispatch =
    let pars = wSModel.SimParams
    let element =  ref None
    /// get reference to HTML elemnt that is scrolled
    let allWaveformsHtmlRef (el: Browser.Types.Element) =
        if not (isNull el) then // el can be Null, in which case we do nothing
            element.Value <- Some el // set mutable reference to the HTML element for later use
            let scrollPos = el.clientWidth + el.scrollLeft
            if Some scrollPos <> pars.LastScrollPos then
                SetLastScrollPos (Some scrollPos) |> ignore
        match element.Value with
        | Some e -> 
            match model.CheckWaveformScrollPosition with
            | true when not (isCursorVisible wSModel e.clientWidth e.scrollLeft) -> 
                e.scrollLeft <- makeCursorVisiblePos wSModel e.clientWidth
                UpdateScrollPos false |> dispatch
            | _ -> ()
        | None ->
            UpdateScrollPos false |> dispatch

    let scrollFun (ev:Browser.Types.UIEvent) = // function called whenever scroll position is changed
        match element.Value with // element should now be the HTMl element that is scrolled
        | None -> () // do nothing
        | Some e ->
            if e.scrollWidth - e.clientWidth - e.scrollLeft < 10.0
                then 
                    let pars' =
                        { pars with
                            ClkSvgWidth = pars.ClkSvgWidth
                            CursorTime = pars.CursorTime
                            LastClkTime =  
                                max ((float pars.LastClkTime + 1.0) * 0.1 |> uint) 10u 
                                |> (+) pars.LastClkTime
                                |> min maxLastClk 
                        }
                    dispatch <| InitiateWaveSimulation(WSViewerOpen, pars')
                    printfn "working"
                else printfn "not working"
            //e.scrollLeft <- 100. // this shows how to set scroll position COMMENT THIS OUT
            // can use dispatch here to make something happen based on scroll position
            // scroll position = min or max => at end
            //printfn "scrolling with scrollPos=%f" e.scrollLeft
    let waves = 
        wSModel.SimParams.DispNames 
        |> Array.map (fun name -> waveformSvgRows.Waves[name]) 
    div [ Ref allWaveformsHtmlRef 
          OnScroll scrollFun 
          Style [ MaxWidth(maxWavesColWidth wSModel)
                  MinHeight "100%" ]
          Class "wavesTable" ]
        [ div [ Class "cursorRectStyle"; cursRectStyle wSModel.SimParams ] [ str " " ]
          table [ Style [ Height "100%" ] ] 
                [ tbody [ Style [ Height "100%" ] ] (Array.concat [|waveformSvgRows.Top; waves; waveformSvgRows.Bottom|]) ] ]

let waveSimViewerRamDisplay wSMod ramPath =
    makeRamReactCol wSMod ramPath
    |> Array.map (fun c -> tr [] [ td [ Class "cursValsCol" ] c ])
    |> (fun r -> [|div [Style [Height "calc(100vh - 150px)"; OverflowY OverflowOptions.Auto]] r|])
  

/// ReactElement of the bottom part of the waveform simulator when waveforms are being displayed
let private viewWaveformViewer compIds model wSMod (dispatch: Msg -> unit) =
    let tableWaves, nameColMiddle, cursValsRows = waveSimViewerRows compIds model wSMod dispatch
    div
        [ Style
            [ Height "calc(100% - 45px)"
              Width "100%"
              OverflowY OverflowOptions.Auto ] ]
        ((List.map (fun ramPath -> cursorValuesCol (waveSimViewerRamDisplay wSMod ramPath)) wSMod.SimParams.MoreWaves) @
        [
          cursorValuesCol cursValsRows
          div [ Style [ Height "100%" ] ]
              [ nameLabelsCol model wSMod nameColMiddle dispatch
                allWaveformsTableElement model wSMod tableWaves dispatch ] 
        ])

/// ReactElement of the zoom buttons
let private viewZoomDiv compIds model wSMod dispatch =
    div [ Class "zoomDiv"]
        [ button [Button.Option.IsOutlined; Button.CustomClass "zoomButLeft"] (fun _ -> changeViewerZoom compIds false model wSMod dispatch) "-"
          button [ Button.CustomClass "zoomButRight" ] (fun _ -> changeViewerZoom compIds true model wSMod dispatch) "+" ]

/// ReactElement of the top row of the WaveAdder (where Select All is)
let private waveEditorSelectAllRow model wSModel dispatch =
    let allOn = Map.forall (fun k wave -> isWaveSelected model wave) wSModel.AllWaves
    tr
        [ Class "rowHeight"
          Style [ VerticalAlign "middle" ] ]
        [ td
            [ Class "wACheckboxCol"
              Class "rowHeight"
              Style [ VerticalAlign "middle" ] ]
              [ input
                  [ Type "checkbox"
                    Class "check"
                    Checked allOn
                    Style [ Float FloatOptions.Left ]
                    OnChange(fun _ -> waveAdderSelectAll model wSModel (not allOn) dispatch ) ] ]
          td [ Style [ FontWeight "bold" ] ] [ str "Select All" ] ]

/// ReactElement of Wave Editor waveform row
/// displays a tickbox and the NetGroup driver name
let private waveEditorTickBoxAndNameRow model  wSModel name (dispatch: Msg -> unit) =
    let allWaves = wSModel.AllWaves

    let getColorProp name  =
        if Array.contains name wSModel.SimParams.DispNames then
            [FontWeight "Bold"]
        else
            []
    tr
        [ Class "rowHeight"
          Style [ VerticalAlign "middle" ] ]
        [ td
            [ Class "wAcheckboxCol"
              Class "rowHeight"
              Style [ VerticalAlign "middle" ] ]
              [ input
                  [ Type "checkbox"
                    Class "check"
                    Checked <| isWaveSelected model allWaves[name]
                    Style [ Float FloatOptions.Left ]
                    OnChange(fun _ -> toggleWaveConnsSelect model wSModel name dispatch) ] ]
          td [] [ label [Style (getColorProp name)] [ str <| allWaves[name].DisplayName ] ] ]

let sortEditorNameOrder wsModel =
    let otherNames = 
        wsModel.AllWaves
        |> Map.filter (fun wId wSpec -> not <| Array.contains wId wsModel.SimParams.DispNames)
    Array.append wsModel.SimParams.DispNames (mapKeys otherNames)

/// ReactElement of all Wave Editor waveform rows
let private waveEditorTickBoxRows model wsModel (dispatch: Msg -> unit) = 
    let editorNameOrder = sortEditorNameOrder wsModel
    sortEditorNameOrder wsModel
    |> Array.map (fun name -> waveEditorTickBoxAndNameRow model wsModel name dispatch)

/// ReactElement of the bottom section of the WaveAdder.
/// Contains tick-boxes for NetGroups
let private waveEditorTickBoxesAndNames (model: Model) wSModel (dispatch: Msg -> unit) =
    div [ Style [ Position PositionOptions.Absolute
                  Top "300px" ] ]
        [ table []
                [ tbody [] 
                        (Array.append [| waveEditorSelectAllRow model wSModel dispatch |] 
                                      (waveEditorTickBoxRows model wSModel dispatch)) ] ]

/// ReactElement of the buttons of the WaveAdder
let private waveEditorButtons (model: Model) (wSModel:WaveSimModel) dispatch =
        

            
    /// this is what actually gets displayed when editor exits
    let closeWaveSimButtonAction _ev =
        dispatch <| StartUICmd CloseWaveSim
        dispatch <| SetWSMod {wSModel with InitWaveSimGraph=None; WSViewState=WSClosed; WSTransition = None}
        dispatch <| SetWaveSimIsOutOfDate true
        dispatch <| UnlockTabsFromWaveSim
        dispatch <| Sheet (Sheet.ResetSelection)
        dispatch <| Sheet (Sheet.SetWaveSimMode false)
        dispatch ClosePropertiesNotification
        dispatch FinishUICmd
    
    /// Return the RAM etc view options window. data is the current (initial) set of RAMs to be viewed.
    let getWavePopup dispatch (data: MoreWaveSetup option) = 
        match data with
        | None -> div [] []
        | Some moreData ->
            div [] [
                reactMoreWaves moreData (Option.get wSModel.InitWaveSimGraph).Graph dispatch; 
                div [Style [Display DisplayOptions.Flex; AlignItems AlignItemsOptions.Center; JustifyContent "Center"]] [
                    Button.button [Button.OnClick (fun _ -> dispatch <| ClosePopup)] [str "Close"]]
                    ]

    let moreWaveEditorButtonAction _ =
        dispatch <| SetPopupWaveSetup (getWaveSetup wSModel model)
        PopupView.showWaveSetupPopup (Some "RAM Viewer Setup: tick to view RAM or ROM contents") (getWavePopup dispatch) None [] dispatch


    
    let waveEditorViewSimButtonAction =
        let allWaves = wSModel.AllWaves
        let viewableWaves = 
            allWaves
            
            |> standardOrderWaves wSModel.SimParams.DispNames (fun s -> 
                Map.containsKey s allWaves && isWaveSelected model allWaves[s])
    
        
        match viewableWaves.Length with
        | 0 -> [ Button.IsLight; Button.Color IsSuccess ]
        | _ ->
            [ 
                Button.Color IsSuccess
                Button.IsLoading (showSimulationLoading wSModel dispatch)
                Button.OnClick(fun _ ->
                    let par' = {wSModel.SimParams with DispNames = viewableWaves }
                    let msgs = 
                        [
                         (StartUICmd ViewWaveSim)
                         ClosePropertiesNotification
                         (InitiateWaveSimulation( WSViewerOpen, par'))
                        ]
                    dispatch (Sheet (Sheet.SetSpinner true))
                    dispatch <| SendSeqMsgAsynch msgs)
            ]
        |> (fun lst -> 
                Button.Props [ Style [ MarginLeft "10px" ] ] :: lst)
    let cancelButton =
        Button.button
            [ Button.Color IsSuccess
              Button.OnClick(closeWaveSimButtonAction) ] [ str "Close" ]

    let moreButton =
        Button.button
            [ Button.Color IsSuccess
              Button.Props [ Style [ MarginRight "10px" ] ]
              Button.OnClick(moreWaveEditorButtonAction) ] [ str "RAM" ]

    let actionButtons =
        match dispWaves wSModel with
        | [||] -> [ moreButton ;  Button.button waveEditorViewSimButtonAction [ str "View selected" ] ]
        | _ -> [ moreButton; cancelButton; Button.button waveEditorViewSimButtonAction [ str "View" ] ]
    div [ Style [ Display DisplayOptions.Block ] ] actionButtons

/// ReactElement list of the WaveAdder 
let private waveEditorView model wSMod (dispatch: Msg -> unit) =
    [ div
       [ Style
            [ Width "90%"
              MarginLeft "5%"
              MarginTop "15px" ] ]
       [ Heading.h4 [] [ str "Waveform Simulation" ] 
         str "Ctrl-click on diagram connections or use tick boxes below to add or remove waveforms."
         str "Test combinational logic by closing this simulator and using Simulate tab."
         hr []
         div []
            [ waveEditorButtons model wSMod dispatch
              waveEditorTickBoxesAndNames model wSMod dispatch ] ] ]

/// ReactElement list of the waveforms view
let private waveformsView compIds model netList wSMod dispatch =
    [ div
        [ Style
            [ Width "calc(100% - 10px)"
              Height "100%"
              MarginLeft "0%"
              MarginTop "0px"
              OverflowX OverflowOptions.Hidden ] ]
          [ viewWaveSimButtonsBar model wSMod dispatch
            viewWaveformViewer compIds model wSMod dispatch
            viewZoomDiv compIds model wSMod dispatch ] ]



//-------------------------------------------------------------------//
//         TOP-LEVEL WAVE SIMULATION TRANSITIONS AND VIEWS           //
//-------------------------------------------------------------------//





/// TRANSITION: Switch between WaveformEditor (WSEditorOpen) and WaveformViewer (WSVieweropen) view
/// sets wsModel for the new view
let private openEditorFromViewer model (dispatch: Msg -> Unit) : Unit = 
    let wsModel = getWSModelOrFail model "What? no wsModel in openCloseWaveEditor"
    // set the currently displayed waves as selected in wave editor
    let waves = wsModel.SimParams.DispNames |> Array.map (fun name -> wsModel.AllWaves[name])
    Array.iter (fun wave -> selectWaveConns model true wave dispatch) waves
    viewerWaveSet waves true wsModel dispatch
    dispatch <| UpdateWSModel (setEditorView WSEditorOpen)


//-----------------------------------------------------------------------------------------------------------------

/// TRANSITION
/// This starts a wave simulation of a new circuit
/// Set wsModel to show the list of waveforms that can be selected from a new simulation
/// Sets data persistent over editor open and close.
/// Sets WaveSim to have editor open
let startWaveSim compIds rState (simData: SimulatorTypes.SimulationData) model (dispatch: Msg -> unit) _ev =
    let comps,conns = model.Sheet.GetCanvasState()
    let compIds = comps |> List.map (fun c -> ComponentId c.Id) |> Set
    let rState = Extractor.extractReducedState (comps,conns)
    // subfunction to generate popup over waveeditor screen if there are undriven input connections
    let inputWarningPopup (simData:SimulatorTypes.SimulationData) dispatch =
        if simData.Inputs <> [] then
            let inputs = 
                simData.Inputs
                |> List.map (fun (_,ComponentLabel lab,_) -> lab)
                |> String.concat ","
            let popup = Notifications.warningPropsNotification (sprintf "Inputs (%s) will be set to 0." inputs)
            dispatch <| SetPropertiesNotification popup

    let startingWsModel =
        let modelWithWaveSimSheet = {model with WaveSimSheet = Option.get (getCurrFile model)}
        let wsModel = getWSModelOrFail modelWithWaveSimSheet "What? Can't get wsModel at start of new simulation"
        let okCompNum = (Set.intersect compIds (simData.Graph |> mapKeys |> Set)).Count
        let simCompNum = simData.Graph.Count (*
        printfn 
            "DEBUG: sheet=%s, modelSheet=%s, okNum = %d, wavesim num = %d drawNum=%d"
            modelWithWaveSimSheet.WaveSimSheet 
            model.WaveSimSheet
            okCompNum
            simCompNum
            compIds.Count
        printfn
            "DEBUG: simComps=%A\n\ndrawcomps=%A\n\n"  
            (simData.Graph |> mapValues |> Array.map (fun c -> c.Label) |> Array.sort)
            (model.Sheet.GetCanvasState() |> fst |> List.map (fun c -> c.Label) |> List.sort) *)

        let wSpecs = 
            SimulatorTypes.getWaveformSpecifications (netGroup2Label compIds) simData rState
        // printfn "Starting comps = %A" (fst rState |> List.map (fun comp -> comp.Label, comp.Type))
        // SimulatorTypes.printSimGraph simData.Graph
        let allowedNames = 
            wsModel.SimParams.DispNames
            |> Array.filter (fun name -> Map.containsKey name wSpecs)
        Array.iter (fun wave -> selectWaveConns model false wave dispatch) (mapValues wSpecs)
        let waves = allowedNames |> Array.map (fun name -> wSpecs[name])
        Array.iter (fun wave -> selectWaveConns model true wave dispatch) waves
        let wSpecs =
            wSpecs
            |> Map.map (fun name spec ->
                match spec.WType, Array.contains name allowedNames with
                | ViewerWaveform _, isDisplayed -> {spec with WType = ViewerWaveform isDisplayed}
                | _ -> spec)
        { 
        wsModel with       
            AllWaves = wSpecs
            SimParams = {wsModel.SimParams with DispNames = allowedNames}
            InitWaveSimGraph = Some simData // start with 0 sample only
            SimDataCache = [| simData |]
            LastCanvasState = Some rState 
            WSViewState = WSEditorOpen; 
            WSTransition = None
        }
    dispatch EndSimulation // end a step simulation just in case
    dispatch <| SetWSModAndSheet(startingWsModel, Option.get (getCurrFile model))
    dispatch <| SetViewerWidth minViewerWidth 
    dispatch <| SetLastSimulatedCanvasState (Some rState) 
    dispatch <| SetWaveSimIsOutOfDate false
    inputWarningPopup simData dispatch
    dispatch <| LockTabsToWaveSim
    dispatch FinishUICmd

//------------------------------------------------------------------------------------------------------------


/// Waveforms >> Button React element with colour determined by current circuit error state
let WaveformButtonFunc compIds model dispatch =
    // based on simulation results determine color of button and what happens if it is clicked
    let simulationButton =
        match currWaveSimModel model with

        | None ->
            // If we have never yet run wavesim create initial wSModel
            match model.CurrentProj with
            | Some _ -> 
                initFileWS model dispatch
            | None -> ()
            Button.button 
                [ Button.OnClick(fun _ -> ())]
        | Some wSModel ->
            match wSModel.WSViewState, model.WaveSimulationIsOutOfDate, SimulationView.makeSimData model with
            | WSClosed, _, Some (Ok simData, rState)
            | _, true, Some (Ok simData, rState) ->
                let isClocked = SynchronousUtils.hasSynchronousComponents simData.Graph
                if isClocked then
                    // display the WaveEditor window if circuit is OK
                    Button.button
                        [ 
                            Button.Color IsSuccess
                            Button.OnClick(fun ev -> 
                                dispatch (StartUICmd StartWaveSim)
                                (startWaveSim compIds rState simData model dispatch ev))
                              ]
                else
                    Button.button
                        [ 
                            Button.Color IsSuccess
                            Button.IsLight
                            Button.OnClick(fun _ -> 
                                let popup = Notifications.errorPropsNotification "Combinational logic does not have waveforms"
                                dispatch <| SetPropertiesNotification popup
                                )
                        ]
            | WSClosed, _, Some( Error err,_)        
            | _, true, Some (Error err, _) -> 
                // display the current error if circuit has errors
                Button.button
                    [   Button.Color IsWarning
                        Button.OnClick(fun _ ->
                          dispatch <| SetWSError (Some err) 
                          SimulationView.SetSimErrorFeedback err model dispatch) 
                    ]
            | x,y,z -> 
                //printfn "other%A %A %A" x y (match z with | None -> "None" | Some ((Error c),_) -> "Some Error" | Some ((Ok _),_) -> "Some Ok")
                Button.button 
                    [ Button.Color IsSuccess
                      Button.IsLight
                      Button.OnClick(fun _ -> ())]
    simulationButton [ str "View Waveforms" ]

/// This is the top-level view function entry for the wave simulator after it has been set up.
/// ReactElement list of the whole waveform simulator
let viewWaveSim (model: Model) dispatch =
    let compIds = getComponentIds model
    match currWaveSimModel model, snd model.WaveSim with

    // normal case, display waveform adder editor or waveforms
    | Some wSModel, None ->
        // we derive all the waveSim circuit details from LastSimulatedCanvasState which does not change until a new simulation is run
        let netList = Helpers.getNetList  <| Option.defaultValue ([],[]) model.LastSimulatedCanvasState
        match wSModel.WSViewState, wSModel.WSTransition with
        | WSEditorOpen, _ -> // display waveAdder if simulation has not finished and adder is open
            let start = TimeHelpers.getTimeMs()
            waveEditorView  model wSModel dispatch  
            |> TimeHelpers.instrumentInterval "Wave Editor open" start
        | WSViewerOpen, _  ->         // otherwise display waveforms 
            waveformsView compIds model netList wSModel dispatch  
        | _, prog  -> 
            let compIds = getComponentIds model
            let button = WaveformButtonFunc compIds model dispatch
            [ div
                [ Style
                    [   Width "90%"
                        MarginLeft "5%"
                        MarginTop "15px" ] ]
            [   Heading.h4 [] [ str "Waveform Simulation" ]
                str "Use this tab to view Waveforms for sequential logic."
                str "Test combinational logic by closing this simulator and using Step Simulator tab."
                hr []
                br []
                button
            ]]

    // Set the current simulation error message
    | Some _, Some simError ->

        [ div [ Style [ Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ]
              [ SimulationView.viewSimulationError simError
                button [ Button.Color IsDanger ] (fun _ -> 
                    dispatch CloseSimulationNotification // Close error notifications.
                    dispatch <| Sheet(Sheet.ResetSelection) // Remove highlights.
                    dispatch <| (JSDiagramMsg << InferWidths) () // Repaint connections.
                    dispatch <| SetWSError None
                    match getCurrentWSMod model with
                    | Some ws -> 
                        dispatch <| SetWSMod {ws with InitWaveSimGraph=None}
                    | _ -> ()                   
                    dispatch <| ChangeRightTab Catalogue 
                    ) 
                    "Ok" ] ]
    
    // no WaveSim data, so start with initial data
    // derived from model
    | None, _ ->
        initFileWS model dispatch
        []

