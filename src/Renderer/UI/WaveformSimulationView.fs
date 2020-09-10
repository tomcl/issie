(*
WaveformSimulationView.fs

View for waveform simulator in tab
*)

module WaveformSimulationView

open Fulma
open Fable.React
open Fable.React.Props

open DiagramMessageType
open DiagramModelType
open DiagramStyle
open CommonTypes
open WaveSimHelpers
open FileMenuView

/// get values position of bus labels from the transition gaps (for one Waveform)
let gaps2pos (wSModel: WaveSimModel) (wave: Waveform) gaps =
    let clkWidth = int wSModel.ClkWidth
    let nSpaces (g: {| GapLen: int; GapStart: int |}) = 
        (g.GapLen * clkWidth / (maxBusValGap + 1) + 2)
    let gapAndInd2Pos (g: {| GapLen: int; GapStart: int |}) i =
        float g.GapStart + float i * float g.GapLen / float (nSpaces g)
    gaps
    |> Array.map (fun (gap: {| GapLen: int; GapStart: int |}) ->
        {| Sample = wave.[gap.GapStart]
           XPosArray = Array.map (gapAndInd2Pos gap) [| 1 .. nSpaces gap - 1 |] |} )

/// get values position of bus labels
let private busLabels (model: Model) waveData =
    match currWS model with
    | Some wSModel ->
        (Array.transpose waveData, Array.map makeGaps (transitions waveData)) 
        ||> Array.map2 (gaps2pos wSModel)
    | None -> failwith "busLabels called when currWS model is None"

/// get SVG of a single waveform for one clock cycle
let private makeSegment (clkW: float) (xInd: int) (data: Sample) (trans: int * int) =
    let top = spacing
    let bot = top + sigHeight - sigLineThick
    let left = float xInd * clkW
    let right = left + float clkW

    let makeSigLine =
        makeLinePoints
            [ Class "sigLineStyle"
              Style [ Stroke("blue") ] ]

    match data with
    | Wire w when w.NBits = 1u ->
        let y =
            match w.BitData with
            | n when n = bigint 1 -> top
            | _ -> bot
        let sigLine = makeSigLine (left, y) (right, y)
        match snd trans with
        | 1 -> [| makeSigLine (right, bot + sigLineThick / 2.0) (right, top - sigLineThick / 2.0) |]
        | 0 -> [||]
        | _ -> failwith "What? Transition has value other than 0 or 1"
        |> Array.append [| sigLine |]
    | _ ->
        let leftInner = if fst trans = 1 then left + transLen else left
        let rightInner = if snd trans = 1 then right - transLen else right
        let cen = (top + bot) / 2.0

        //make lines
        let topL = makeSigLine (leftInner, top) (rightInner, top)
        let botL = makeSigLine (leftInner, bot) (rightInner, bot)
        let topLeft = makeSigLine (left, cen) (leftInner, top)
        let botLeft = makeSigLine (left, cen) (leftInner, bot)
        let topRight = makeSigLine (right, cen) (rightInner, top)
        let botRight = makeSigLine (right, cen) (rightInner, bot)

        match trans with
        | 1, 1 -> [| topLeft; botLeft; topRight; botRight |]
        | 1, 0 -> [| topLeft; botLeft |]
        | 0, 1 -> [| topRight; botRight |]
        | 0, 0 -> [||]
        | _ -> failwith "What? Transition has value other than 0 or 1"
        |> Array.append [| topL; botL |]

/// get the labels of a waveform for a period in which the value doesn't change
let private busLabelOneValue wsMod (busLabelValAndPos: {| Sample: Sample; XPosArray: float [] |}) =
    let addLabel nLabels xInd = makeText (inWaveLabel nLabels xInd wsMod)
    match busLabelValAndPos.Sample with
    | Wire w when w.NBits > 1u ->
        Array.map (fun xInd -> 
            addLabel 1 xInd (n2StringOfRadix w.BitData w.NBits wsMod.Radix)) busLabelValAndPos.XPosArray
    | _ -> [||]

/// get SVG of a single waveform
let waveSvg model wsMod waveData  =
    let valueLabels =
        busLabels model waveData
        |> Array.map (Array.collect (busLabelOneValue wsMod))

    let makeWaveSvg (sampArr: Waveform) (transArr: (int * int) []): ReactElement [] =
        (sampArr, transArr)
        ||> Array.mapi2 (makeSegment wsMod.ClkWidth)
        |> Array.concat

    let padTrans t =
        match Array.length t with
        | 0 -> [| 1, 1 |]
        | 1 ->
            [| (1, t.[0])
               (t.[0], 1) |]
        | _ ->
            Array.pairwise t
            |> (fun pairs ->
                Array.concat
                    [ [| 1, fst pairs.[0] |]
                      pairs
                      [| snd (Array.last pairs), 1 |] ])

    transitions waveData
    |> Array.map padTrans
    |> Array.map2 makeWaveSvg (Array.transpose waveData)
    |> Array.map2 Array.append valueLabels

/// SVG of the clock numbers above the waveforms
let clkRulerSvg (model: WaveSimModel) =
    let makeClkRulLbl i =
        match model.ClkWidth with
        | clkW when clkW < 0.5 && i % 5 <> 0 -> [||]
        | _ -> [| makeText (cursRectText model i) (string i) |]
    [| 0 .. int model.LastClk |]
    |> Array.collect makeClkRulLbl
    |> (fun arr ->
        [ backgroundSvg model
          [| makeRect (cursRectStyle model) |]
          arr ])
    |> Array.concat
    |> makeSvg (clkRulerStyle model)

/// toggle selection of a waveform when clicking its checkbox
let private toggleSelect (model: Model) (wSMod: WaveSimModel) netList ind =
    let trgtLstGroup = 
        if wSMod.WaveAdderOpen 
        then wSMod.WaveAdder.Ports.[ind]
        else wSMod.Ports.[ind]
    trgtLstGroup
    |> isWaveSelected model netList
    |> not
    |> selWave2selConn model trgtLstGroup

/// select all waveforms
let private selectAllOn model (wSMod: WaveSimModel) =
    Array.map (fun trgtLstGroup -> selWave2selConn model trgtLstGroup true) wSMod.Ports

/// get wave name labels from waveforms names
let private makeLabels waveNames =
    let makeLbl l = label [ Class "waveLbl" ] [ str l ]
    Array.map makeLbl waveNames

/// strings of the values dislayed in the rigth column of the simulator
let private cursValStrings compIds (wSMod: WaveSimModel) (waveData: Sample [] []) =
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

    match int wSMod.Cursor < Array.length wSMod.SimData with
    | true -> Array.map makeCursVal waveData.[int wSMod.Cursor]
    | false -> [||]

/// labels displayed in the right column of the simulator
let private makeCursVals compIds model waveData =
    let string2Lbl = Array.map (fun l -> label [ Class "cursVals" ] [ str l ])
    Array.map string2Lbl <| cursValStrings compIds model waveData

/// get waveform names
let private getWaveNames compIds netList (wsMod: WaveSimModel) =
    match wsMod.WaveAdder.SimData with
    | Some sD -> Array.map (nlTrgtLstGroup2Label compIds sD.Graph netList) wsMod.Ports
    | None -> [||]

/// tuple of React elements of middle column, left column, right column
let private waveSimRows compIds model (netList: NetList) (wsMod: WaveSimModel) dispatch =
    let waveData = getWaveData wsMod
    let waveNames = getWaveNames compIds netList wsMod

    let labelCols =
        makeLabels waveNames
        |> Array.mapi (fun i l ->
            tr [ Class "rowHeight" ]
                [ td [ Class "checkboxCol" ]
                      [ input
                          [ Type "checkbox"
                            Class "check"
                            Checked <| isWaveSelected model netList wsMod.Ports.[i]
                            Style [ Float FloatOptions.Left ]
                            OnChange(fun _ -> toggleSelect model wsMod netList i) ] ]
                  td
                      [ Class "waveNamesCol"
                        Style [ TextAlign TextAlignOptions.Right ] ] [ l ] ])

    let cursValCol = 
        makeCursVals compIds wsMod waveData 
        |> Array.map (fun c -> tr [ Class "rowHeight" ] [ td [ Class "cursValsCol" ] c ])

    wsMod.WaveTable, labelCols, cursValCol

/// maximum width of the waveform simulator viewer
let maxWidth compIds netList (wSMod: WaveSimModel) =
    let strWidth s = 
        JSHelpers.getTextWidthInPixels (s, "12px segoe ui") //not sure which font
    let curLblColWidth =
        match cursValStrings compIds wSMod (getWaveData wSMod) with
        | [||] -> 0.0
        | cVS ->
            Array.map (Array.map strWidth >> Array.max) cVS
            |> Array.max
            |> max 25.0
    let waveNames =
        getWaveNames compIds netList wSMod 
    let namesColWidth =
        match waveNames with
        | [||] -> 0.0
        | wN ->
            Array.map strWidth wN
            |> Array.max
            |> max 100.0
    let waveColWidth =
        match wSMod.Ports with
        | [||] -> 600.0
        | _ -> maxWavesColWidthFloat wSMod
    let checkboxCol = 25.0
    let extraWidth = 45.0 
    
    curLblColWidth + namesColWidth + waveColWidth + checkboxCol + extraWidth |> int

/// stretch waveforms horizontally
let private zoom compIds plus (m: Model) (wSMod: WaveSimModel) dispatch =
    let netList = wsModel2netList wSMod
    let newClkW =
        if plus then zoomFactor else 1.0 / zoomFactor
        |> (*) wSMod.ClkWidth
        |> max minZoom
        |> min maxZoom
    match int (float m.ViewerWidth * zoomFactor) > maxWidth compIds netList wSMod with
    | true ->
        {| LastClk = (wSMod.LastClk + 1u) * (uint zoomFactor) + 10u
           Curs = wSMod.Cursor
           ClkW = newClkW |}
    | false -> 
        {| LastClk = wSMod.LastClk
           Curs = wSMod.Cursor
           ClkW = newClkW |}
    |> Error |> SetSimInProgress |> dispatch

let private button options func label = 
    Button.button (List.append options [ Button.OnClick func ]) [ str label ]

/// change cursor value
let private changeCurs (wSMod: WaveSimModel) dispatch newCurs =
    let curs' = min 500u newCurs
    match 0u <= curs' with
    | true ->
        {| Curs = curs'; ClkW = wSMod.ClkWidth; LastClk = wSMod.LastClk |}
        |> Error |> SetSimInProgress |> dispatch
    | _ -> ()

/// change cursor value by 1 up or down
let private cursorMove increase (wSMod: WaveSimModel) dispatch =
    match increase, wSMod.Cursor with
    | true, n -> n + 1u |> changeCurs wSMod dispatch
    | false, n -> n - 1u |> changeCurs wSMod dispatch

/// change the order of the waveforms in the simulator
let private moveWave (model: Model) netList (wSMod: WaveSimModel) up =
    let moveBy = if up then -1.5 else 1.5
    let addLastPort arr p =
        Array.mapi (fun i el -> if i <> Array.length arr - 1 then el
                                else fst el, Array.append (snd el) [| p |]) arr
    let wTFirst, wTLast = (fun (a: ReactElement array) -> a.[0], a.[Array.length a - 1]) wSMod.WaveTable
    Array.zip wSMod.Ports wSMod.WaveTable.[1 .. Array.length wSMod.WaveTable - 2]
    |> Array.map (fun p -> isWaveSelected model netList (fst p), p) 
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
    |> (fun (p, wT) -> {wSMod with Ports = p
                                   WaveTable = Array.concat [[|wTFirst|];wT;[|wTLast|]]})
    |> SetCurrFileWSMod

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
                match currWS model with
                | Some wSMod when wSMod.CursorEmpty = false -> 
                    string wSMod.Cursor
                | Some _ -> ""
                | None -> "0"
                |> Input.Value 
                //Input.DefaultValue <| sprintf "%d" model.WaveSim.Cursor
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

/// React Element of the loading button
let private loadingBut model =
    match model.SimulationInProgress with
    | Some _ -> button [Button.Color IsDanger] (fun _ -> ()) "loading..."
    | None -> str ""

/// React Element of the buttons Bar at the top of the waveform simulator
let private viewWaveSimButtonsBar model wSMod dispatch =
    div [ Style [ Height "45px" ] ]
        [ loadingBut model
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

/// open/close the WaveAdder view 
let private openCloseWA model (wSMod: WaveSimModel) on dispatch = 
    if on then selectAllOn model wSMod |> ignore
    { wSMod with WaveAdderOpen = on }
    |> SetCurrFileWSMod |> dispatch

/// select all waveforms in the WaveAdder View
let private waveAdderSelectAll model netList (wSMod: WaveSimModel) =
    let setTo = 
        wSMod.WaveAdder.Ports 
        |> Array.forall (isWaveSelected model netList)

    wSMod.WaveAdder.Ports
    |> Array.map (fun trgtLstGroup -> selWave2selConn model trgtLstGroup (not setTo)) 
    |> ignore

/// ReactElement of the waveforms' name labels column
let private nameLabelsCol model netList (wsMod: WaveSimModel) labelRows dispatch =
    let waveAddDelBut =        
        th [ Class "waveNamesCol" ] 
           [ Button.button
           [ Button.CustomClass "newWaveButton"
             Button.Color IsSuccess
             Button.OnClick(fun _ -> openCloseWA model wsMod true dispatch) ] [ str "Edit list..." ] ]

    let top =
        [| tr [ Class "rowHeight" ]
               [ th [ Class "checkboxCol" ]
                     [ div [ Class "updownDiv" ]
                     [ Button.button
                         [ Button.CustomClass "updownBut"
                           Button.OnClick(fun _ -> moveWave model netList wsMod true |> dispatch) ] [ str "▲" ]
                       Button.button
                           [ Button.CustomClass "updownBut"
                             Button.OnClick(fun _ -> moveWave model netList wsMod false |> dispatch) ] [ str "▼" ] ] ]
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

/// React Element of the waveform SVGs' column
let private wavesCol wSMod rows =
    div [ Style [ MaxWidth(maxWavesColWidth wSMod)
                  MinHeight "100%" ]
          Class "wavesTable" ] 
            [ table [ Style [ Height "100%" ] ] 
                    [ tbody [ Style [ Height "100%" ] ] rows ] ]

/// React Element of the bottom part of the waveform simulator when waveforms are being displayed
let private viewWaveformViewer compIds model netList wSMod dispatch =
    let tableWaves, leftColMid, cursValsRows = waveSimRows compIds model netList wSMod dispatch
    div
        [ Style
            [ Height "calc(100% - 45px)"
              Width "100%"
              OverflowY OverflowOptions.Auto ] ]
        [ cursValsCol cursValsRows
          div [ Style [ Height "100%" ] ]
              [ nameLabelsCol model netList wSMod leftColMid dispatch
                wavesCol wSMod tableWaves ] ]

/// React Element of the zoom buttons
let private viewZoomDiv compIds model wSMod dispatch =
    div [ Class "zoomDiv" ]
        [ button [ Button.CustomClass "zoomButLeft" ] (fun _ -> zoom compIds false model wSMod dispatch) "-"
          button [ Button.CustomClass "zoomButRight" ] (fun _ -> zoom compIds true model wSMod dispatch) "+" ]

/// React Element of the top row of the WaveAdder (where Select All is)
let private waveAdderTopRow model netList wSMod =
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
                    Checked(Array.forall (isWaveSelected model netList) wSMod.WaveAdder.Ports)
                    Style [ Float FloatOptions.Left ]
                    OnChange(fun _ -> waveAdderSelectAll model netList wSMod) ] ]
          td [ Style [ FontWeight "bold" ] ] [ str "Select All" ] ]

/// ReactElement of WaveAdder waveform row
let private addWaveRow model netList wSMod ind dispatch =
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
                    Checked <| isWaveSelected model netList wSMod.WaveAdder.Ports.[ind]
                    Style [ Float FloatOptions.Left ]
                    OnChange(fun _ -> toggleSelect model wSMod netList ind) ] ]
          td [] [ label [] [ str wSMod.WaveAdder.WaveNames.[ind] ] ] ]

/// ReactElement of all WaveAdder waveform rows
let private addWaveRows model netList wSMod dispatch = 
    Array.mapi (fun i _ -> addWaveRow model netList wSMod i dispatch) wSMod.WaveAdder.Ports 

/// ReactElement of the bottom section of the WaveAdder
let private waveAdderBottom (model: Model) netList wSMod dispatch =
    div [ Style [ Position PositionOptions.Absolute
                  Top "300px" ] ]
        [ table []
                [ tbody [] 
                        (Array.append [| waveAdderTopRow model netList wSMod |] 
                                      (addWaveRows model netList wSMod dispatch)) ] ]

/// ReactElement of the buttons of the WaveAdder
let private waveAdderButs (model: Model) netList wSMod dispatch =
    let simButStyle =
        match Array.exists (isWaveSelected model netList) wSMod.WaveAdder.Ports with
        | true ->
            [ Button.Color IsSuccess
              Button.OnClick(fun _ -> 
                Array.filter (isWaveSelected model netList) wSMod.WaveAdder.Ports                
                |> Ok
                |> SetSimInProgress |> dispatch) ]
        | false -> [ Button.CustomClass "disabled" ]
        |> (fun lst -> Button.Props [ Style [ MarginLeft "10px" ] ] :: lst)
    let cancBut =
        Button.button
            [ Button.Color IsDanger
              Button.OnClick(fun _ -> openCloseWA model wSMod false dispatch) ] [ str "Cancel" ]

    let buts =
        match wSMod.Ports with
        | [||] -> [ Button.button simButStyle [ str "View selected" ] ]
        | _ -> [ cancBut; Button.button simButStyle [ str "View" ] ]
    div [ Style [ Display DisplayOptions.Block ] ] buts

/// ReactElement list of the WaveAdder 
let private waveAdderView model netList wSMod dispatch =
    [ div
    [ Style
        [ Width "90%"
          MarginLeft "5%"
          MarginTop "15px" ] ]
      [ Heading.h4 [] [ str "Waveform Simulation" ]
        str
            "Add waveforms to view simulation. \n You can also add them by selecting components/connections in the editor and clicking \"Simulate\" on the top left menu bar"
        hr []
        div []
            [ waveAdderButs model netList wSMod dispatch
              waveAdderBottom model netList wSMod dispatch ] ] ]

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
    match currWS model, snd model.WaveSim with
    | Some wSMod, None ->
        match wSMod.WaveAdderOpen, model.SimulationInProgress with
        | _, Some _ | false, None -> waveformsView compIds
        | true, None -> waveAdderView 
        |> (fun f -> f model (wsModel2netList wSMod) wSMod dispatch)
    | Some _, Some simError ->
        [ div [ Style [ Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ]
              [ SimulationView.viewSimulationError simError
                button [ Button.Color IsDanger ] (fun _ -> 
                    None |> SetWSError |> dispatch
                    ChangeRightTab Catalogue |> dispatch
                    ) 
                    "Ok" ] ]
    | None, _ ->
        initFileWS model dispatch
        []
