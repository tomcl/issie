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
    let pref =
        match wSMod.Radix with
        | Bin -> "0b"
        | Hex -> "0x"
        | _ -> ""

    let makeCursVal sample =
        match sample with
        | Wire w when w.NBits > 1u -> [| pref + n2StringOfRadix w.BitData w.NBits wSMod.Radix |]
        | Wire w -> [| pref + string w.BitData |]
        | StateSample s -> s

    match int wSMod.Cursor < Array.length wSMod.SimDataCache with
    | true -> Array.map makeCursVal waveData.[int wSMod.Cursor]
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
        match wSMod.DispWaveNames with
        | [||] -> 0.0
        | wN ->
            Array.map strWidth wN
            |> Array.max
            |> max 100.0
    let waveColWidth =
        match wSMod.DispPorts with
        | [||] -> 600.0
        | _ -> maxWavesColWidthFloat wSMod
    let checkboxCol = 25.0
    let extraWidth = 45.0 
    
    curLblColWidth + namesColWidth + waveColWidth + checkboxCol + extraWidth |> int






/////////////////////
// WaveSim actions //
/////////////////////

/// change selection of a waveform's connections
let private changeNetGroupConnsSelect  (selFun: NetGroup -> bool) (diagram) (wSMod: WaveSimModel) netList ind =
    let netGroup = 
        match  wSMod.WaveSimState, wSMod.WaveData with
        | true, Some {AllNetGroups = adderPorts} -> adderPorts.[ind]
        | _ ->  wSMod.DispPorts.[ind]
    netGroup
    |> selFun
    |> selectNetGrpConns diagram netGroup

/// toggle selection of a waveform's connections
let private toggleNetGroupConnsSelect (diagram) (wSMod: WaveSimModel) netList ind =
    changeNetGroupConnsSelect (isWaveSelected diagram netList >> not) diagram wSMod netList ind

/// select all waveforms
let private selectAllOn model (wSMod: WaveSimModel) =
    Array.map (fun trgtLstGroup -> selectNetGrpConns model trgtLstGroup true) wSMod.DispPorts

/// stretch waveforms horizontally
let private zoom compIds plus (m: Model) (wSMod: WaveSimModel) dispatch =
    let netList = wsModel2netList wSMod
    let newClkW =
        if plus then zoomFactor else 1.0 / zoomFactor
        |> (*) wSMod.ClkWidth
        |> max minZoom
        |> min maxZoom
    match int (float m.ViewerWidth * zoomFactor) > maxWidth wSMod with
    | true ->
        {| LastClk = (wSMod.LastClk + 1u) * (uint zoomFactor) + 10u
           Curs = wSMod.Cursor
           ClkW = newClkW |}
    | false -> 
        {| LastClk = wSMod.LastClk
           Curs = wSMod.Cursor
           ClkW = newClkW |}
    |> Error |> SetSimInProgress |> dispatch

/// change cursor value
let private changeCurs (wSMod: WaveSimModel) dispatch newCurs =
    let curs' = min maxLastClk newCurs
    match 0u <= curs', curs' <= wSMod.LastClk with
    | true, true ->
        { wSMod with Cursor = curs' }
        |> SetCurrFileWSMod |> dispatch
        UpdateScrollPos true |> dispatch
    | true, false ->
        {| Curs = curs'; ClkW = wSMod.ClkWidth; LastClk = wSMod.LastClk |}
        |> Error |> SetSimInProgress |> dispatch
        UpdateScrollPos true |> dispatch
    | false, _ -> ()

/// change cursor value by 1 up or down
let private cursorMove increase (wSMod: WaveSimModel) dispatch =
    match increase, wSMod.Cursor with
    | true, n -> n + 1u |> changeCurs wSMod dispatch
    | false, n -> n - 1u |> changeCurs wSMod dispatch

/// change the order of the waveforms in the simulator
let private moveWave diagram netList (wSMod: WaveSimModel) up =
    let moveBy = if up then -1.5 else 1.5
    let addLastPort arr p =
        Array.mapi (fun i el -> if i <> Array.length arr - 1 then el
                                else fst el, Array.append (snd el) [| p |]) arr
    let wTFirst, wTLast = (fun (a: ReactElement array) -> a.[0], a.[Array.length a - 1]) wSMod.DispWaveSVGCache
    Array.zip wSMod.DispPorts wSMod.DispWaveSVGCache.[1 .. Array.length wSMod.DispWaveSVGCache - 2]
    |> Array.map (fun p -> isWaveSelected diagram netList (fst p), p) 
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
    |> Array.unzip 
    |> (fun (p, wT) -> {wSMod with DispPorts = p
                                   DispWaveNames = Array.map (waveNameOfWSModel wSMod) p
                                   DispWaveSVGCache = Array.concat [[|wTFirst|];wT;[|wTLast|]]})
    |> SetCurrFileWSMod


/// display set of netGroups in a standard order
/// dispPort ones first.
/// otherwise alphabetical.
let standardOrderGroups groups (wSModel:WaveSimModel) =
    groups
    |> Array.map (fun ng -> waveNameOfWSModel wSModel ng, ng)
    |> Array.groupBy (fun (_, wave) -> Array.contains wave wSModel.DispPorts)
    |> Array.sortByDescending fst
    |> Array.collect (fun (isDisp, nameNgList) -> if isDisp then nameNgList else Array.sortBy fst nameNgList)
    |> Array.unzip

/// standard order of waveforms in the WaveAdder
let private standardWaveformOrderWaveAdder wSModel =
    match wSModel with
    | {WaveData = Some wa}  ->
        standardOrderGroups wa.AllNetGroups wSModel
        |> (fun (a, b) -> { wa with AllWaveNames = a 
                                    AllNetGroups = b } )

    | _ -> failwithf "What? expected waveAdder not found in model"



/// select all waveforms in the WaveAdder View
let private waveAdderSelectAll model netList (wSMod: WaveSimModel) =
    match wSMod.WaveData with
    | None -> ()
    | Some wa ->
        let setTo = Array.forall (isWaveSelected model netList) wa.AllNetGroups
        wa.AllNetGroups
        |> Array.map (fun netGrp -> selectNetGrpConns model netGrp (not setTo)) 
        |> ignore


/// open/close the WaveAdder view 
/// sets wavesim for new view
let private openCloseWaveEditor model (netList:NetList) (wSModel: WaveSimModel) on dispatch = 

    let compIds = getComponentIds model
    let wA = 
        match on with 
        | true -> 
            selectAllOn model.Diagram wSModel |> ignore
            wSModel // startwith DispPorts order - for waveforms currently displayed
            |> updateWaveSimFromInitData compIds  model
            |> standardWaveformOrderWaveAdder // work out correct order for waveadder
        | false ->
            Option.defaultValue (initWA (availableNetGroups model)) wSModel.WaveData

    { wSModel with WaveSimState = on
                   WaveData = Some wA }
    |> SetCurrFileWSMod 
    |> dispatch




//////////////////////////////////////////////
// ReactElements of the Waveformm Simulator //
//////////////////////////////////////////////


/// labels displayed in the right column of the simulator
let private makeCursVals model waveData =
    let string2Lbl = Array.map (fun l -> label [ Class "cursVals" ] [ str l ])
    Array.map string2Lbl <| cursValStrings model waveData




/// tuple of React elements of middle column, left column, right column.
/// shows waveforms and labels and cursor col.
/// The vertical order is fixed and as in DispPorts, DispWaveNames and DispWaveSVGCache
let private waveSimRows compIds diagram (netList: NetList) (wsMod: WaveSimModel) dispatch =
    let waveData = getWaveData wsMod
    let netGroups = wsMod.DispPorts
    let labelCols =
        makeLabels wsMod.DispWaveNames
        |> Array.mapi (fun i l ->
            tr [ Class "rowHeight" ]
                [ td [ Class "checkboxCol" ]
                      [ input
                          [ Type "checkbox"
                            Class "check"
                            Checked <| isWaveSelected diagram netList netGroups.[i]
                            Style [ Float FloatOptions.Left ]
                            OnChange(fun _ -> toggleNetGroupConnsSelect diagram wsMod netList i) ] ]
                  td
                      [ Class "waveNamesCol"
                        Style [ TextAlign TextAlignOptions.Right ] ] [ l ] ])

    let cursValCol = 
        makeCursVals wsMod waveData 
        |> Array.map (fun c -> tr [ Class "rowHeight" ] [ td [ Class "cursValsCol" ] c ])

    wsMod.DispWaveSVGCache, labelCols, cursValCol

/// ReactElement of the tabs for changing displayed radix
let private radixTabs (model: WaveSimModel) dispatch =
    let radixString =
        [ Dec,  "uDec"
          Bin,  "Bin"
          Hex,  "Hex"
          SDec, "sDec" ] |> Map.ofList

    let radTab rad =
        Tabs.tab
            [ Tabs.Tab.IsActive(model.Radix = rad)
              Tabs.Tab.Props
                  [ Style
                      [ Width "35px"
                        Height "30px" ] ] ]
            [ a
                [ Style
                    [ Padding "0 0 0 0"
                      Height "30px" ]
                  OnClick(fun _ ->
                      { model with Radix = rad }
                      |> SetCurrFileWSMod
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
                    string wSMod.Cursor
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
let private loadingButton model =
    match model.SimulationInProgress with
    | Some _ -> button [Button.Color IsDanger] (fun _ -> ()) "loading..."
    | None -> str ""

/// React Element of the buttons Bar at the top of the waveform simulator
let private viewWaveSimButtonsBar model wSMod dispatch =
    div [ Style [ Height "45px" ] ]
        [ loadingButton model
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
             Button.OnClick(fun _ -> openCloseWaveEditor model netList wsMod true dispatch) ] [ str "Edit list..." ] ]

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
let private wavesCol model (wSModel: WaveSimModel) rows dispatch =
    let element =  ref None
    /// get reference to HTML elemnt that is scrolled
    let htmlElementRef (el: Browser.Types.Element) =
        if not (isNull el) then // el can be Null, in which case we do nothing
            element := Some el // set mutable reference to the HTML element for later use
       
        match model.SimulationInProgress, !element with
        | Some (Ok par), _ -> Ok par |> SimulateWhenInProgress |> dispatch
        | Some (Error par), Some e -> 
            adjustPars wSModel par (e.clientWidth + e.scrollLeft) dispatch
            |> Error |> SimulateWhenInProgress |> dispatch
        | None, Some e -> 
            match model.CheckScrollPos with
            | true when not (isCursorVisible wSModel e.clientWidth e.scrollLeft) -> 
                e.scrollLeft <- makeCursorVisiblePos wSModel e.clientWidth
                UpdateScrollPos false |> dispatch
            | _ -> ()
        | _, None ->
            SetSimNotInProgress |> dispatch
            UpdateScrollPos false |> dispatch

    let scrollFun (ev:Browser.Types.UIEvent) = // function called whenever scroll position is changed
        match !element with // element should now be the HTMl element that is scrolled
        | None -> () // do nothing
        | Some e ->
            if e.scrollWidth - e.clientWidth - e.scrollLeft < 10.0
                then {| ClkW = wSModel.ClkWidth
                        Curs = wSModel.Cursor
                        LastClk = max ((float wSModel.LastClk + 1.0) * 0.1 |> uint) 10u 
                                  |> (+) wSModel.LastClk
                                  |> min maxLastClk |}
                     |> Error |> SetSimInProgress |> dispatch
                     printfn "working"
                else printfn "not working"
            //e.scrollLeft <- 100. // this shows how to set scroll position COMMENT THIS OUT
            // can use dispatch here to make something happen based on scroll position
            // scroll position = min or max => at end
            printfn "scrolling with scrollPos=%f" e.scrollLeft
        
    div [ Ref htmlElementRef 
          OnScroll scrollFun 
          Style [ MaxWidth(maxWavesColWidth wSModel)
                  MinHeight "100%" ]
          Class "wavesTable" ]
        [ div [ Class "cursorRectStyle"; cursRectStyle wSModel ] [ str " " ]
          table [ Style [ Height "100%" ] ] 
                [ tbody [ Style [ Height "100%" ] ] rows ] ]

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
                wavesCol model wSMod tableWaves dispatch ] ]

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
let private waveAdderTickBoxRow model netList wSModel ind dispatch =
    let allNetGroups = getAllNetGroups wSModel
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
                    Checked <| isWaveSelected model.Diagram netList allNetGroups.[ind]
                    Style [ Float FloatOptions.Left ]
                    OnChange(fun _ -> toggleNetGroupConnsSelect model.Diagram wSModel netList ind) ] ]
          td [] [ label [] [ str (getAdderOrInit model wSModel).AllWaveNames.[ind] ] ] ]

/// ReactElement of all WaveAdder waveform rows
let private waveEditorTickBoxRows model netList wSModel dispatch = 
    Array.mapi (fun i _ -> waveAdderTickBoxRow model netList wSModel i dispatch) (getAllNetGroups wSModel) 

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
    let waveEditorViewSimButtonAction =
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
                Button.OnClick(fun _ -> 
                    viewableNetGroups                
                    |> Ok
                    |> SetSimInProgress |> dispatch)
            ]
        |> (fun lst -> Button.Props [ Style [ MarginLeft "10px" ] ] :: lst)
    let cancelButton =
        Button.button
            [ Button.Color IsDanger
              Button.OnClick(fun _ -> openCloseWaveEditor model netList wSModel false dispatch) ] [ str "Cancel" ]

    let actionButtons =
        match wSModel.DispPorts with
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

/// ReactElement list of the whole waveform simulator
let viewWaveSim (model: Model) dispatch =
    let compIds = getComponentIds model
    match currWaveSimModel model, snd model.WaveSim with

    // normal case, display waveform adder window or waveforms
    | Some wSModel, None ->
        // we derive all the waveSim circuit details from LastSimulatedCanvasstate which does not chnage until a new simulation is run
        let netList = Helpers.getNetList  <| Option.defaultValue ([],[]) model.LastSimulatedCanvasState
        match wSModel.WaveSimState, model.SimulationInProgress with
        | true, None -> // display waveAdder if simulation has not finished and adder is open
            waveEditorView  model netList wSModel dispatch      
        | _  ->         // otherwise display waveforms 
            waveformsView compIds model netList wSModel dispatch  

    // Set the current simulation error message
    | Some _, Some simError ->
        [ div [ Style [ Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ]
              [ SimulationView.viewSimulationError simError
                button [ Button.Color IsDanger ] (fun _ -> 
                    None |> SetWSError |> dispatch
                    ChangeRightTab Catalogue |> dispatch
                    ) 
                    "Ok" ] ]
    
    // no WaveSim data, so start with initial data
    // derived from model
    | None, _ ->
        initFileWS model dispatch
        []


////////////////////////////////////////////////////////////////////////
///                        Simulation                                ///
////////////////////////////////////////////////////////////////////////

///  Set up wavesim data that may not exist and is needed
let updateWaveSimFromInitData compIds (model:Model) (ws: WaveSimModel) : WaveSimModel =
    match SimulationView.makeSimData model with
    | Some (Ok sD, rState) ->
        // current diagram netlist
        let netList = Helpers.getNetList <| rState
        // all ports on current diagram
        // order does not matter.
        let allPorts = netList2NetGroups netList
        let allNames = Array.map (netGroup2Label compIds sD.Graph netList) allPorts
        let nameMap = Array.zip allNames allPorts |> Map.ofArray
        let lookup name = Map.tryFind name nameMap
        let dispNames, dispPorts = // filter remembered names by whether they are still valid, lookup new ports (in case those have changed)
            ws.DispWaveNames
            |> Array.collect (fun name -> match lookup  name with | Some netGroup -> [|name,netGroup|] | None -> [||])
            |> Array.unzip
        //
        // simulation data of correct length
        let sD' = Array.append [| sD |] (extractSimData sD ws.LastClk)
        { ws with 
            WaveData = Some { 
                InitWaveSimGraph = Some sD
                AllNetGroups = allPorts
                AllWaveNames = allNames // must be same order as AllNetgroups
            }
            DispPorts = dispPorts
            DispWaveNames = dispNames        
            LastCanvasState = getReducedCanvState model 
        }
        |> addSVGToWaveSimModel
    | Some (Error _, _) | None -> initWS 

/// Waveforms >> Button React element with actions triggered by pressing the button
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
                [ Button.OnClick(fun _ -> ChangeRightTab WaveSim |> dispatch) ]
        | Some wSModel ->
            match model.SimulationIsStale, SimulationView.makeSimData model with
            | true, Some (Ok simData, rState) ->
                let isClocked = SynchronousUtils.hasSynchronousComponents simData.Graph
                if isClocked then
                    // display the waveAdder window if circuit is OK
                    Button.button
                        [ 
                            Button.Color IsSuccess
                            Button.OnClick(fun _ ->
                                if simData.Inputs <> [] then
                                    let inputs = 
                                        simData.Inputs
                                        |> List.map (fun (_,ComponentLabel lab,_) -> lab)
                                        |> String.concat ","
                                    PopupView.warningNotification (sprintf "Inputs (%s) will be set to 0." inputs) ClosePropertiesNotification
                                    |> SetPropertiesNotification |> dispatch
                                startNewWaveSimulation compIds model wSModel dispatch simData rState
                                ChangeRightTab WaveSim |> dispatch )
                              ]
                else
                    Button.button
                        [ 
                            //Button.Color IsSuccess
                            Button.OnClick(fun _ -> 
                                            PopupView.errorNotification "Combinational logic does not have waveforms" ClosePropertiesNotification
                                            |> SetPropertiesNotification |> dispatch
                                )
                              ]
                    
            | true, Some (Error err, _) -> 
                // display the current error if circuit has errors
                Button.button
                    [   Button.Color IsWarning
                        Button.OnClick(fun _ ->
                          Some err |> SetWSError |> dispatch
                          ChangeRightTab WaveSim |> dispatch ) ]
            | _ -> 
                Button.button 
                        [ Button.OnClick(fun _ -> ChangeRightTab WaveSim |> dispatch) ]
    simulationButton [ str "Waveforms >>" ]
