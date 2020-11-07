(*
WaveformSimulationView.fs

View for waveform simulator in tab
*)

module rec WaveformSimulationView

(*******************************************************************************************

Waveform simulation simulates the current sheet circuit and generates waveforms as react SVG elements
on an SVG canvas. The waveform display changes interactively based on zoom and cursor movement buttons.

In addition the SVG canvas is an element wider then the pane in which it is displayed, and uses HTML scrolling
navigate. Zooming will recreate a new (wider) canvas as necessary so that the entire visible window contains valid
waveforms.

In addition the canvas consists only of those waveforms currently selected to be displayed, via a wave editor pane
which displayed the names of all possible waveforms and allows them to be displayed or not. See waveeditor below
for details.

When the selected waveforms are changed the SVG canvas must also be recreated with new waveforms, although the
simulation need not be advanced.

Waveform simulation is initiated by the Waveforms >> button. This button is recreated by mainview a suitable color:
green: a changed (or initial) circuit exists which can be (re-)simulated with waveform simulator
orange: errors exist in circuit - no change to waveform simulation.
white: circuit has not chnaged and is still the same as what is currently displaying waveforms

The waveforms last simulated are thus displayed while the circuit can be updated, until the button is clicked. On
this click, if the circuit has errors the old waveforms are replaced by an error message, otherwise a new simulation
replaces the old simulation.

Allowing simultaneous view (and interactive chnage as above) of old waveforms while letting the design be changed
requires careful state management:

model.LastSimulatedCanvasState - circuit last simulated
model.Simulation - time = 0 SimGraph or simulation error for LastSimulatedCanvasState. A simulation can be extended by advancing the 
SimGraph one or more simulation steps.

Wavesim specific parameters and temporary state are inside a WaveSimModel record accessed via a Map field in the model:

WaveSim: Map<string,waveSimModel>
accessed as:
model.WaveSim.[openSheetName]: WaveSimModel

TODO: The WaveSimModel is only valid when a project is open and the Wavesim data in reality should be part of
the current sheet LoadedComponent record, tus WaveSim record moves inside LoadedComponent record and no longer
needs to be a Map.

The WaveSimModel data is changed on opening a different sheet or saving / restoring a project in exactly the same way as the
rest of the sheet data LoadedComponentData. The simulation parameters last used for each sheet are saved and restored. Inside
WaveSim a subrecord WaveSim.WaveData contains transient information about the current simulation which is not saved across sheet
changes. In addition the field SimDataCache is transient and replaced whenever LastSimulatedCanvasState changes.

TODO: make SimDataCache a field of WaveData. Move WaveData from WaveSimModel to Model - so there is just one copy of it, not
one per sheet.

waveform Simulation State Changes

(1) SimulateButtonFunc definition:
waveforms >> button pressed with SimIsStale = true, makeSimData model returns (Ok simData), Canvas exists and contains canvasData

--> startNewWaveSimulation: this updates model with 
        SimIsStale = false, 
        WaveSim data for current sheet (waveSim) all initialised, 
        waveSim.WaveData initialised from simData and canvasData
        LastSimulatedCanvasState updated to equal the new circuit (?)

*******************************************************************************************)

open Fulma
open Fable.React
open Fable.React.Props

open MessageType
open ModelType
open DiagramStyle
open CommonTypes
open WaveSimHelpers
open FileMenuView



/// strings of the values displayed in the rigth column of the simulator
let private cursValStrings (wSMod: WaveSimModel) (waveData: Sample [] []) =
    let paras = wSMod.SimParams
    let pref =
        match paras.Radix with
        | Bin -> "0b"
        | Hex -> "0x"
        | _ -> ""

    let makeCursVal sample =
        match sample with
        | Wire w when w.NBits > 1u -> [| pref + n2StringOfRadix w.BitData w.NBits paras.Radix |]
        | Wire w -> [| pref + string w.BitData |]
        | StateSample s -> s

    match int paras.Cursor < Array.length wSMod.SimDataCache with
    | true -> Array.map makeCursVal waveData.[int paras.Cursor]
    | false -> [||]

/// maximum width of the waveform simulator viewer
let maxWidth (wSMod: WaveSimModel) =
    let strWidth s = 
        JSHelpers.getTextWidthInPixels (s, "12px segoe ui") //not sure which font
    let curLblColWidth =
        match cursValStrings wSMod (getWaveData wSMod) with
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
    let waveColWidth =
        match dispPorts wSMod with
        | [||] -> 600.0
        | _ -> maxWavesColWidthFloat wSMod
    let checkboxCol = 25.0
    let extraWidth = 45.0 
    
    curLblColWidth + namesColWidth + waveColWidth + checkboxCol + extraWidth |> int






/////////////////////
// WaveSim actions //
/////////////////////

/// change selection of a waveform's connections
let private changeNetGroupConnsSelect  (selFun: NetGroup -> bool) (diagram) (wSMod: WaveSimModel) name =
    let netGroup =  wSMod.AllPorts.[name]
    netGroup
    |> selFun
    |> selectNetGrpConns diagram netGroup

/// toggle selection of a waveform's connections
let private toggleNetGroupConnsSelect (diagram) (wSMod: WaveSimModel) (netList:NetList) name =
    changeNetGroupConnsSelect (isWaveSelected diagram netList >> not) diagram wSMod name

/// select all waveforms
let private selectAllOn model (wSMod: WaveSimModel) =
    Array.map (fun trgtLstGroup -> selectNetGrpConns model trgtLstGroup true) (dispPorts wSMod)

/// stretch waveforms horizontally
let private zoom compIds plus (m: Model) (wSMod: WaveSimModel) dispatch =
    let rec adjustLastClk viewW wsModel =
        let pars = wsModel.SimParams
        if viewW * 1.4 < float (maxWidth wsModel) then
            let lastClk' = pars.LastClk+2u
            adjustLastClk viewW (setSimParams (fun sp -> {sp with LastClk = lastClk'}) wsModel)
        else
            printfn "New LastClk=%d" pars.LastClk
            pars.LastClk
        
    let netList = wsModel2netList wSMod
    let pars = wSMod.SimParams
    let newClkW =
        if plus then zoomFactor else 1.0 / zoomFactor
        |> (*) pars.ClkWidth
        |> max minZoom
        |> min maxZoom
    let wSModNewClk = setSimParams (fun sp -> {sp with ClkWidth=newClkW}) wSMod
    let newPars =
        match int (float m.ViewerWidth) > maxWidth wSModNewClk with
        | true ->
                { pars with LastClk = adjustLastClk (float m.ViewerWidth) wSModNewClk}
        | false ->  
                {pars with 
                    ClkWidth = newClkW 
                }
    dispatch <| InitiateWaveSimulation(WSViewerOpen, newPars)
        

/// change cursor value
let private changeCurs (wSMod: WaveSimModel) dispatch newCurs =
    let pars = wSMod.SimParams
    let curs' = min maxLastClk newCurs
    match 0u <= curs', curs' <= pars.LastClk with
    | true, true ->
        wSMod
        |> setSimParams (fun sp -> {sp with Cursor = curs' })
        |> SetCurrFileWSMod |> dispatch
        UpdateScrollPos true |> dispatch
    | true, false ->
        let pars' = { pars with Cursor = curs'; ClkWidth = pars.ClkWidth; LastClk = pars.LastClk }
        dispatch <| InitiateWaveSimulation(WSViewerOpen, pars')
        UpdateScrollPos true |> dispatch
    | false, _ -> ()

/// change cursor value by 1 up or down
let private cursorMove increase (wSMod: WaveSimModel) dispatch =
    match increase, wSMod.SimParams.Cursor with
    | true, n -> n + 1u |> changeCurs wSMod dispatch
    | false, n -> n - 1u |> changeCurs wSMod dispatch

/// change the order of the waveforms in the simulator
let private moveWave diagram netList (wSMod: WaveSimModel) up =
    let moveBy = if up then -1.5 else 1.5
    let addLastPort arr p =
        Array.mapi (fun i el -> if i <> Array.length arr - 1 then el
                                else fst el, Array.append (snd el) [| p |]) arr
    let svgCache = wSMod.DispWaveSVGCache
    let wTFirst, wTLast = svgCache.Top, svgCache.Bottom
    let movedNames =
        wSMod.SimParams.DispNames
        |> Array.map (fun name -> isWaveSelected diagram netList (wSMod.AllPorts.[name]), name)
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
    |> SetCurrFileWSMod

/// display set of netGroups in a standard order
/// dispPort ones first.
/// otherwise alphabetical.
let standardOrderGroups groups (wSModel:WaveSimModel) =
    let dispP = dispPorts wSModel
    groups
    |> Array.map (fun ng -> waveNameOf wSModel ng, ng)
    |> Array.groupBy (fun (_, wave) -> Array.contains wave dispP)
    |> Array.sortByDescending fst
    |> Array.collect (fun (isDisp, nameNgList) -> if isDisp then nameNgList else Array.sortBy fst nameNgList)
    |> Array.unzip

/// standard order of waveforms in the WaveAdder
let private standardWaveformOrderWaveAdder wSModel =
    wSModel
    |> standardOrderGroups (mapValues wSModel.AllPorts)
    |> (fun (names, ports) -> { wSModel with AllWaveNames = names})




/// select all waveforms in the WaveAdder View
let private waveAdderSelectAll model netList (wSMod: WaveSimModel) =
    let setTo =
        mapValues wSMod.AllPorts
        |> Array.forall (isWaveSelected model netList) 

    mapValues wSMod.AllPorts
    |> Array.map (fun netGrp -> selectNetGrpConns model netGrp (not setTo)) 
    |> ignore







//////////////////////////////////////////////
// ReactElements of the Waveformm Simulator //
//////////////////////////////////////////////


/// labels displayed in the right column of the simulator
let private makeCursVals model waveData =
    let string2Lbl = Array.map (fun l -> label [ Class "cursVals" ] [ str l ])
    Array.map string2Lbl <| cursValStrings model waveData




/// tuple of React elements of middle column, left column, right column.
/// shows waveforms and labels and cursor col.
/// The vertical order is fixed and as in DispNames
let private waveSimRows compIds diagram (netList: NetList) (wsMod: WaveSimModel) dispatch =
    let waveData = getWaveData wsMod
    let allPorts = wsMod.AllPorts
    let labelCols =
        makeLabels wsMod.SimParams.DispNames
        |> Array.zip wsMod.SimParams.DispNames
        |> Array.map (fun (name, lab) ->
            tr [ Class "rowHeight" ]
                [ td [ Class "checkboxCol" ]
                      [ input
                          [ Type "checkbox"
                            Class "check"
                            Checked <| isWaveSelected diagram netList allPorts.[name]
                            Style [ Float FloatOptions.Left ]
                            OnChange(fun _ -> toggleNetGroupConnsSelect diagram wsMod netList name) ] ]
                  td
                      [ Class "waveNamesCol"
                        Style [ TextAlign TextAlignOptions.Right ] ] [ lab ] ])

    let cursValCol = 
        makeCursVals wsMod waveData 
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
            [ Tabs.Tab.IsActive(wsModel.SimParams.Radix = rad)
              Tabs.Tab.Props
                  [ Style
                      [ Width "35px"
                        Height "30px" ] ] ]
            [ a
                [ Style
                    [ Padding "0 0 0 0"
                      Height "30px" ]
                  OnClick(fun _ ->
                      InitiateWaveSimulation (WSViewerOpen,{wsModel.SimParams with Radix = rad})
                      |> dispatch) ] [ str (radixString.[rad]) ] ]
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
                | Some wSMod when wSMod.CursorEmpty = false -> 
                    string wSMod.SimParams.Cursor
                | Some _ -> ""
                | None -> "0"
                |> Input.Value 
                Input.OnChange(fun c ->
                    match System.Int32.TryParse c.Value with
                    | true, n when n >= 0 -> 
                        { wSMod with CursorEmpty = false }
                        |> SetCurrFileWSMod |> dispatch
                        changeCurs wSMod dispatch <| uint n
                    | false, _ when c.Value = "" -> 
                        { wSMod with CursorEmpty = true }
                        |> SetCurrFileWSMod |> dispatch
                        changeCurs wSMod dispatch 0u
                    | _ -> 
                        { wSMod with CursorEmpty = false }
                        |> SetCurrFileWSMod |> dispatch ) ]
          button [ Button.CustomClass "cursRight" ] (fun _ -> cursorMove true wSMod dispatch) "▶" ]

/// ReactElement of the loading button
let private loadingButton wsMod dispatch =
    if showSimulationLoading wsMod dispatch then 
        button [Button.Color IsDanger; Button.IsLoading true] (fun _ -> ()) ""
    else str ""

/// React Element of the buttons Bar at the top of the waveform simulator
let private viewWaveSimButtonsBar model wSMod dispatch =
    div [ Style [ Height "45px" ] ]
        [ loadingButton wSMod dispatch
          radixTabs wSMod dispatch
          cursorButtons model wSMod dispatch ]

/// ReactElement of the right column of the waveform simulator
let private cursValsCol rows =
    let rightCol = Array.append [| tr [ Class "rowHeight" ] [ td [ Class "rowHeight" ] [] ] |] rows
    div
        [ Style
            [ Float FloatOptions.Right
              Height "100%"
              BorderTop "2px solid rgb(219,219,219)"
              BorderLeft "2px solid rgb(219,219,219)" ] ] [ table [] [ tbody [] rightCol ] ]

/// ReactElement of the waveforms' name labels column
let private nameLabelsCol model netList (wsMod: WaveSimModel) labelRows dispatch =
    let waveAddDelBut =        
        th [ Class "waveNamesCol" ] 
           [ Button.button
           [ Button.CustomClass "newWaveButton"
             Button.Color IsSuccess
             Button.OnClick(fun _ -> 
                printfn "opening editor with Dispnames = %A" wsMod.SimParams.DispNames
                openCloseWaveEditor model  WSEditorOpen dispatch) ] [ str "Edit list..." ] ]

    let top =
        [| tr [ Class "rowHeight" ]
               [ th [ Class "checkboxCol" ]
                     [ div [ Class "updownDiv" ]
                     [ Button.button
                         [ Button.CustomClass "updownBut"
                           Button.OnClick(fun _ -> moveWave model.Diagram netList wsMod true |> dispatch) ] [ str "▲" ]
                       Button.button
                           [ Button.CustomClass "updownBut"
                             Button.OnClick(fun _ -> moveWave model.Diagram netList wsMod false |> dispatch) ] [ str "▼" ] ] ]
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
            element := Some el // set mutable reference to the HTML element for later use
            let scrollPos = el.clientWidth + el.scrollLeft
            if Some scrollPos <> pars.LastScrollPos then
                SetLastScrollPos (Some scrollPos) |> ignore
        match !element with
        (*| (Some (MakeSVGs par)) as simAction, _ -> 
            dispatch <| SimulateWhenInProgress simAction
        | Some (ChangeParameters par), Some e -> 
            let newPars = adjustPars wSModel par (e.clientWidth + e.scrollLeft) dispatch
            dispatch <| InitiateWaveSimulation(WSViewerOpen, newPars)*)
        | Some e -> 
            match model.CheckScrollPos with
            | true when not (isCursorVisible wSModel e.clientWidth e.scrollLeft) -> 
                e.scrollLeft <- makeCursorVisiblePos wSModel e.clientWidth
                UpdateScrollPos false |> dispatch
            | _ -> ()
        | None ->
            //dispatch <| SetSimInProgress None
            UpdateScrollPos false |> dispatch

    let scrollFun (ev:Browser.Types.UIEvent) = // function called whenever scroll position is changed
        match !element with // element should now be the HTMl element that is scrolled
        | None -> () // do nothing
        | Some e ->
            if e.scrollWidth - e.clientWidth - e.scrollLeft < 10.0
                then 
                    let pars' =
                        { pars with
                            ClkWidth = pars.ClkWidth
                            Cursor = pars.Cursor
                            LastClk =  
                                max ((float pars.LastClk + 1.0) * 0.1 |> uint) 10u 
                                |> (+) pars.LastClk
                                |> min maxLastClk 
                        }
                    dispatch <| InitiateWaveSimulation(WSViewerOpen, pars')
                    printfn "working"
                else printfn "not working"
            //e.scrollLeft <- 100. // this shows how to set scroll position COMMENT THIS OUT
            // can use dispatch here to make something happen based on scroll position
            // scroll position = min or max => at end
            printfn "scrolling with scrollPos=%f" e.scrollLeft
    let waves = 
        wSModel.SimParams.DispNames 
        |> Array.map (fun name -> waveformSvgRows.Waves.[name]) 
    div [ Ref allWaveformsHtmlRef 
          OnScroll scrollFun 
          Style [ MaxWidth(maxWavesColWidth wSModel)
                  MinHeight "100%" ]
          Class "wavesTable" ]
        [ div [ Class "cursorRectStyle"; cursRectStyle wSModel.SimParams ] [ str " " ]
          table [ Style [ Height "100%" ] ] 
                [ tbody [ Style [ Height "100%" ] ] (Array.concat [|waveformSvgRows.Top; waves; waveformSvgRows.Bottom|]) ] ]

/// ReactElement of the bottom part of the waveform simulator when waveforms are being displayed
let private viewWaveformViewer compIds model netList wSMod dispatch =
    let tableWaves, nameColMiddle, cursValsRows = waveSimRows compIds model.Diagram netList wSMod dispatch
    div
        [ Style
            [ Height "calc(100% - 45px)"
              Width "100%"
              OverflowY OverflowOptions.Auto ] ]
        [ cursValsCol cursValsRows
          div [ Style [ Height "100%" ] ]
              [ nameLabelsCol model netList wSMod nameColMiddle dispatch
                allWaveformsTableElement model wSMod tableWaves dispatch ] ]

/// ReactElement of the zoom buttons
let private viewZoomDiv compIds model wSMod dispatch =
    div [ Class "zoomDiv"]
        [ button [Button.Option.IsOutlined; Button.CustomClass "zoomButLeft"] (fun _ -> zoom compIds false model wSMod dispatch) "-"
          button [ Button.CustomClass "zoomButRight" ] (fun _ -> zoom compIds true model wSMod dispatch) "+" ]

/// ReactElement of the top row of the WaveAdder (where Select All is)
let private waveEditorSelectAllRow model netList wSModel =
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
                    Checked(Array.forall (isWaveSelected model netList) (getAllNetGroups wSModel))
                    Style [ Float FloatOptions.Left ]
                    OnChange(fun _ -> waveAdderSelectAll model netList wSModel) ] ]
          td [ Style [ FontWeight "bold" ] ] [ str "Select All" ] ]

/// ReactElement of WaveAdder waveform row
/// displays a tickbox and the NetGroup driver name
let private waveAdderTickBoxRow model netList wSModel name dispatch =
    let allPorts = wSModel.AllPorts
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
                    Checked <| isWaveSelected model.Diagram netList allPorts.[name]
                    Style [ Float FloatOptions.Left ]
                    OnChange(fun _ -> toggleNetGroupConnsSelect model.Diagram wSModel netList name) ] ]
          td [] [ label [] [ str name ] ] ]

/// ReactElement of all WaveAdder waveform rows
let private waveEditorTickBoxRows model netList wSModel dispatch = 
    Array.map (fun name -> waveAdderTickBoxRow model netList wSModel name dispatch) wSModel.AllWaveNames

/// ReactElement of the bottom section of the WaveAdder.
/// Contains tick-boxes for NetGroups
let private waveEditorTickBoxesAndNames (model: Model) netList wSModel dispatch =
    div [ Style [ Position PositionOptions.Absolute
                  Top "300px" ] ]
        [ table []
                [ tbody [] 
                        (Array.append [| waveEditorSelectAllRow model.Diagram netList wSModel |] 
                                      (waveEditorTickBoxRows model netList wSModel dispatch)) ] ]

/// ReactElement of the buttons of the WaveAdder
let private waveEditorButtons (model: Model) netList (wSModel:WaveSimModel) dispatch =
    let isSelected (ng:NetGroup) = isWaveSelected model.Diagram netList ng
    /// this is what actually gets displayed when editor exits
    let closeWaveSimButtonAction _ev =
        dispatch <| SetCurrFileWSMod {wSModel with InitWaveSimGraph=None}
        dispatch <| ChangeRightTab Catalogue
        dispatch <| SetWaveSimIsStale true
        

    let waveEditorViewSimButtonAction =
        //
        let viewableNetGroups = 
            getAllNetGroups wSModel
            |> (fun gps -> standardOrderGroups gps wSModel)
            |> snd
            |> Array.filter isSelected
        
        match viewableNetGroups.Length with
        | 0 -> [ Button.CustomClass "disabled" ]
        | _ ->
            [ 
                Button.Color IsSuccess
                Button.IsLoading (showSimulationLoading wSModel dispatch)
                Button.OnClick(fun _ -> 
                    dispatch ClosePropertiesNotification
                    let par' = {wSModel.SimParams with DispNames = Array.map (getDispName wSModel) viewableNetGroups }            
                    dispatch <|  InitiateWaveSimulation( WSViewerOpen, par'))
            ]
        |> (fun lst -> 
                Button.Props [ Style [ MarginLeft "10px" ] ] :: lst)
    let cancelButton =
        Button.button
            [ Button.Color IsSuccess
              Button.OnClick(closeWaveSimButtonAction) ] [ str "Close" ]

    let actionButtons =
        match dispPorts wSModel with
        | [||] -> [ Button.button waveEditorViewSimButtonAction [ str "View selected" ] ]
        | _ -> [ cancelButton; Button.button waveEditorViewSimButtonAction [ str "View" ] ]
    div [ Style [ Display DisplayOptions.Block ] ] actionButtons

/// ReactElement list of the WaveAdder 
/// netList comes from model.LastSimulatedCanvasState
let private waveEditorView model netList wSMod dispatch =
    [ div
    [ Style
        [ Width "90%"
          MarginLeft "5%"
          MarginTop "15px" ] ]
      [ Heading.h4 [] [ str "Waveform Simulation" ]
        str "Add nets to view waveforms by clicking connections in diagram or tick-boxes below."
        str "Test combinational logic using Simulate tab."
        hr []
        div []
            [ waveEditorButtons model netList wSMod dispatch
              waveEditorTickBoxesAndNames model netList wSMod dispatch ] ] ]

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
            viewWaveformViewer compIds model netList wSMod dispatch
            viewZoomDiv compIds model wSMod dispatch ] ]



////////////////////////////////////////////////////////////////////////
///         TOP-LEVEL WAVE SIMULATION TRANSITIONS AND VIEWS          ///
////////////////////////////////////////////////////////////////////////

let SetSimErrorFeedback (simError:SimulatorTypes.SimulationError) (dispatch: Msg -> Unit) =
    if simError.InDependency.IsNone then
       // Highlight the affected components and connection only if
       // the error is in the current diagram and not in a
       // dependency.
       let thingsToHighlight = (simError.ComponentsAffected, simError.ConnectionsAffected)
       dispatch <| SetHighlighted thingsToHighlight



/// TRANSITION: Switch between WaveformEditor (editorIsOn = true) and WaveformViewer (editorIsOn = false) view
/// sets wsModel for the new view

let private openCloseWaveEditor model (editorState: WSViewT) dispatch = 
    let wsModel = getWSModelOrFail model "What? no wsModel in openCloseWaveEditor"
    let wsModel' =
        wsModel
        |> standardWaveformOrderWaveAdder // work out correct order for waveadder
        |> setEditorNextView editorState wsModel.SimParams 
    dispatch <| SetCurrFileWSMod wsModel'
    
   


/// This function sets up an initial waveSim for waveform viewing.
/// initSimDat must be the initial clock tick 0 simulation data.
/// It returns the correct simulation data.
/// In addition it works out from initSimDat the Netgroup etc data needed for the viewer.
let makeNewSimulationWsModel (rState: CanvasState) (initSimDat: SimulatorTypes.SimulationData) (ws: WaveSimModel)  (model: Model) =
    let compIds = // TODO - remove compids refs - should not be needed.
        ws.InitWaveSimGraph
        |> Option.map (fun g -> mapKeys g.Graph)
        |> Option.defaultValue [||]
        |> set
    // current diagram netlist
    let netList = Helpers.getNetList <| rState
    // all ports on current diagram
    // order does not matter.
    let netGroups = netList2NetGroups netList
    let allNames = Array.map (netGroup2Label compIds initSimDat.Graph netList) netGroups
    let allPorts = Array.zip allNames netGroups |> Map.ofArray
    let lookup name = Map.tryFind name allPorts
    let dispNames = // filter remembered names by whether they are still valid, lookup new ports (in case those have changed)
        ws.SimParams.DispNames
        |> Array.filter (fun name -> match lookup  name with | Some netGroup ->true | None -> false)
    //
    // simulation data of correct length
    // simulation here is done immediately in the transition function, without GUI warning
    let sD' = Array.append [| initSimDat |] (extractSimData initSimDat ws.SimParams.LastClk)
    { ws with 
        InitWaveSimGraph = Some initSimDat
        SimDataCache = sD'
        AllPorts = allPorts
        AllWaveNames = allNames // must be same order as AllNetgroups     
        LastCanvasState = Some rState 
    }
    |> setDispNames dispNames   


/// TRANSITION HELPER: 
/// Does the correct length simulation to display initial waveView, and adds in the correct waveform SVGs.
/// Creates updated wsModel data from initial simulation of current canvasState.
/// Returns wsModel, does not actually make the wsmodel state change
let setupWaveViewerSimulation compIds (model:Model) (ws: WaveSimModel) : WaveSimModel =
    match SimulationView.makeSimData model with
    | Some (Ok sD, rState) ->
        makeNewSimulationWsModel rState sD ws model
        |> addSVGToWaveSimModel
    | Some (Error _, _) | None -> initWS [||] Map.empty


//-----------------------------------------------------------------------------------------------------------------
/// TRANSITION
/// This starts a wave simulation of a new circuit
/// Set wsModel to show the list of waveforms that can be selected from a new simulation
/// Sets data persistent over editor open and close.
/// Sets WaveSim to have editor open
let startWaveSim compIds rState (simData: SimulatorTypes.SimulationData) model dispatch _ev =
    /// subfunction to generate popup over waveeditor screen if there are undriven input connections
    let inputWarningPopup (simData:SimulatorTypes.SimulationData) dispatch =
        if simData.Inputs <> [] then
            let inputs = 
                simData.Inputs
                |> List.map (fun (_,ComponentLabel lab,_) -> lab)
                |> String.concat ","
            let popup = PopupView.warningPropsNotification (sprintf "Inputs (%s) will be set to 0." inputs)
            dispatch <| SetPropertiesNotification popup

    let startingWsModel =
        let wsModel = getWSModelOrFail model "What? Can't get wsModel at start of new simulation"
        let netList = Helpers.getNetList rState
        let netGroups = netList2NetGroups netList
        let nameOf ng = netGroup2Label compIds simData.Graph netList ng
        let allPorts = 
            netGroups 
            |> Array.map (fun ng -> nameOf ng, ng)
            |> Map.ofArray
        let allNames = mapKeys allPorts
        let dispNames = 
            wsModel.SimParams.DispNames
            |> Array.filter (fun name -> Array.contains name allNames)
        Array.iter (fun ng -> selectNetGrpConns model.Diagram ng false) netGroups
        printfn "***Netgroups=%A***" (Array.length netGroups)
        { 
        wsModel with       
            AllPorts = allPorts 
            AllWaveNames = allNames
            SimParams = {wsModel.SimParams with DispNames = dispNames}
            InitWaveSimGraph = Some simData // start with 0 sample only
            LastCanvasState = Some rState 
            WSState = {wsModel.WSState with View = WSEditorOpen; NextView = None}
        }
    dispatch <| SetCurrFileWSMod startingWsModel
    dispatch <| SetViewerWidth minViewerWidth 
    dispatch <| SetLastSimulatedCanvasState (Some rState) 
    dispatch <| SetWaveSimIsStale false
    inputWarningPopup simData dispatch
    dispatch <| ChangeRightTab WaveSim

//------------------------------------------------------------------------------------------------------------


/// Waveforms >> Button React element with colour determined by current circuit error state
let WaveformButtonFunc compIds model dispatch =
    // based on simulation results determine color of button and what happens if it is clicked
    let simulationButton =
        match currWaveSimModel model with

        | None ->
            // If we have never yet run wavesim create initial wSModel
            match model.CurrProject with
            | Some _ -> 
                initFileWS model dispatch
            | None -> ()
            Button.button 
                [ Button.OnClick(fun _ -> 
                    dispatch <| ChangeRightTab WaveSim)
                ]
        | Some wSModel ->
            match model.WaveSimulationIsStale, SimulationView.makeSimData model with
            | true, Some (Ok simData, rState) ->
                let isClocked = SynchronousUtils.hasSynchronousComponents simData.Graph
                if isClocked then
                    // display the WaveEditor window if circuit is OK
                    Button.button
                        [ 
                            Button.Color IsSuccess
                            Button.OnClick( startWaveSim compIds  rState simData model dispatch)
                              ]
                else
                    Button.button
                        [ 
                            //Button.Color White
                            Button.OnClick(fun _ -> 
                                let popup = PopupView.errorPropsNotification "Combinational logic does not have waveforms"
                                dispatch <| SetPropertiesNotification popup
                                )
                        ]
                    
            | true, Some (Error err, _) -> 
                // display the current error if circuit has errors
                Button.button
                    [   Button.Color IsWarning
                        Button.OnClick(fun _ ->
                          dispatch <| SetWSError (Some err) 
                          dispatch <| ChangeRightTab WaveSim
                          SetSimErrorFeedback err dispatch) 
                    ]
            | _ -> 
                Button.button 
                    [ Button.OnClick(fun _ -> 
                          dispatch <| ChangeRightTab WaveSim) 
                    ]
    simulationButton [ str "Waveforms >>" ]

/// This is the top-level view function entry for the wave simulator after it has been set up.
/// ReactElement list of the whole waveform simulator
let viewWaveSim (model: Model) dispatch =
    let compIds = getComponentIds model
    match currWaveSimModel model, snd model.WaveSim with

    // normal case, display waveform adder editor or waveforms
    | Some wSModel, None ->
        // we derive all the waveSim circuit details from LastSimulatedCanvasState which does not change until a new simulation is run
        let netList = Helpers.getNetList  <| Option.defaultValue ([],[]) model.LastSimulatedCanvasState
        match wSModel.WSState.View, wSModel.WSState.NextView with
        | WSEditorOpen, _ -> // display waveAdder if simulation has not finished and adder is open
            waveEditorView  model netList wSModel dispatch      
        | WSViewerOpen, _  ->         // otherwise display waveforms 
            waveformsView compIds model netList wSModel dispatch  
        | _, prog  -> 
            printfn "ViewWaveSim should not be called when WaveSimEditorOpen =%A, inProgress = %A" wSModel.WSState prog
            [ div [] [] ]

    // Set the current simulation error message
    | Some _, Some simError ->

        [ div [ Style [ Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ]
              [ SimulationView.viewSimulationError simError
                button [ Button.Color IsDanger ] (fun _ -> 
                    dispatch CloseSimulationNotification // Close error notifications.
                    dispatch <| SetHighlighted ([], []) // Remove highlights.
                    dispatch <| (JSDiagramMsg << InferWidths) () // Repaint connections.
                    dispatch <| SetWSError None
                    match getCurrFileWSMod model with
                    | Some ws -> 
                        dispatch <| SetCurrFileWSMod {ws with InitWaveSimGraph=None}
                    | _ -> ()                   
                    dispatch <| ChangeRightTab Catalogue 
                    ) 
                    "Ok" ] ]
    
    // no WaveSim data, so start with initial data
    // derived from model
    | None, _ ->
        initFileWS model dispatch
        []

