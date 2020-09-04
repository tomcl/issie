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
open FileMenuView
open Extractor
open SimulatorTypes

//auxiliary functions to the viewer function

let private connId2JSConn (model: Model) connId =
    match model.Diagram.GetCanvasState() with
    | Some (_, jsConns) -> 
        List.tryFind (fun jsConn -> (extractConnection jsConn).Id = connId) jsConns
    | None -> None
    |> function
       | Some jsConn -> [ jsConn ]
       | None -> []

let private isWaveSelected model (wSMod: WaveSimModel) port = 
    let simD = 
        match wSMod.WaveAdder.SimData with
        | Some sD -> sD
        | None -> failwith "isWaveSelected called when WaveAdder.SimData is None"
    let canvState =
        match wSMod.LastCanvasState with
        | Some cS -> cS
        | _ -> failwith "isWaveSelected called when wSMod.LastCanvasState is None"
    getSelected model
    |> compsConns2portLst simD canvState
    |> Array.contains port

let private selWave2selConn model (wSMod: WaveSimModel) ind dispatch on = 
    if wSMod.WaveAdderOpen 
    then wSMod.WaveAdder.Ports.[ind]
    else wSMod.Ports.[ind]
    |> port2ConnId model 
    |> List.collect (fun (ConnectionId cId) -> connId2JSConn model cId) 
    |> model.Diagram.SetSelected on

let private allSelected model wSMod = 
    Array.forall (isWaveSelected model wSMod) wSMod.Ports

let private anySelected model wSMod = 
    Array.exists (isWaveSelected model wSMod) wSMod.Ports

let private toggleSelect (model: Model) (wSMod: WaveSimModel) ind dispatch =
    if wSMod.WaveAdderOpen 
    then wSMod.WaveAdder.Ports.[ind]
    else wSMod.Ports.[ind]
    |> isWaveSelected model wSMod
    |> not
    |> selWave2selConn model wSMod ind dispatch

let selectAllOn model wSMod dispatch =
    Array.mapi (fun i _ -> selWave2selConn model wSMod i dispatch true) wSMod.Ports

let private selectAll model wSMod dispatch =
    let setAllOn = not <| allSelected model wSMod
    Array.mapi (fun i _ -> selWave2selConn model wSMod i dispatch setAllOn) wSMod.Ports

let private makeLabels (wSMod: WaveSimModel) waveNames =
    let makeLbl l = label [ Class "waveLbl" ] [ str l ]
    Array.map makeLbl waveNames

// functions for bus labels


let private cursValStrings (wSMod: WaveSimModel) (waveData: Sample [] []) =
    let pref =
        match wSMod.Radix with
        | Bin -> "0b"
        | Hex -> "0x"
        | _ -> ""

    let makeCursVal sample =
        match sample with
        | Wire w when w.NBits > 1u -> [| pref + radixChange w.BitData w.NBits wSMod.Radix |]
        | Wire w -> [| pref + string w.BitData |]
        | StateSample s -> s

    match int wSMod.Cursor < Array.length wSMod.SimData with
    | true -> Array.map makeCursVal waveData.[int wSMod.Cursor]
    | false -> [||]

let private makeCursVals model waveData =
    let string2Lbl = Array.map (fun l -> label [ Class "cursVals" ] [ str l ])
    Array.map string2Lbl <| cursValStrings model waveData

//container box and clock lines

let private makeWaveNames (wsMod: WaveSimModel) =
    match wsMod.WaveAdder.SimData with
    | Some sD -> Array.map (wSPort2Name sD.Graph) wsMod.Ports
    | None -> [||]

let private waveSimRows model (wsMod: WaveSimModel) dispatch =
    let waveData = makeWaveData wsMod
    let waveNames = makeWaveNames wsMod

    let labelCols =
        makeLabels wsMod waveNames
        |> Array.mapi (fun i l ->
            tr [ Class "rowHeight" ]
                [ td [ Class "checkboxCol" ]
                      [ input
                          [ Type "checkbox"
                            Class "check"
                            Checked <| isWaveSelected model wsMod wsMod.Ports.[i]
                            Style [ Float FloatOptions.Left ]
                            OnChange(fun _ -> toggleSelect model wsMod i dispatch) ] ]
                  td
                      [ Class "waveNamesCol"
                        Style [ TextAlign TextAlignOptions.Right ] ] [ l ] ])

    let cursValCol = 
        makeCursVals wsMod waveData 
        |> Array.map (fun c -> tr [ Class "rowHeight" ] [ td [ Class "cursValsCol" ] c ])

    wsMod.WaveTable, labelCols, cursValCol

// view function helpers
let maxWidth (wSMod: WaveSimModel) =
    let strWidth s = 
        JSHelpers.getTextWidthInPixels (s, "12px segoe ui") //not sure which font
    let curLblColWidth =
        match cursValStrings wSMod (makeWaveData wSMod) with
        | [||] -> 0.0
        | cVS ->
            Array.map (Array.map strWidth >> Array.max) cVS
            |> Array.max
            |> max 25.0
    let namesColWidth =
        match (makeWaveNames wSMod) with
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

let private zoom plus (m: Model) (wSMod: WaveSimModel) dispatch =
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


let private button options func label = 
    Button.button (List.append options [ Button.OnClick func ]) [ str label ]

let private changeCurs (wSMod: WaveSimModel) dispatch newCurs =
    let curs' = min 500u newCurs
    match 0u <= curs' with
    | true ->
        {| Curs = curs'; ClkW = wSMod.ClkWidth; LastClk = wSMod.LastClk |}
        |> Error |> SetSimInProgress |> dispatch
    | _ -> ()

let private cursorMove increase (wSMod: WaveSimModel) dispatch =
    match increase, wSMod.Cursor with
    | true, n -> n + 1u |> changeCurs wSMod dispatch
    | false, n -> n - 1u |> changeCurs wSMod dispatch

let private delSelected model wSMod =
    let ports' =
        Array.map (fun p -> isWaveSelected model wSMod p, p) wSMod.Ports 
        |> Array.filter (fun (sel, _) -> not sel)
        |> Array.map snd
    { wSMod with Ports = ports' }
    |> SetCurrFileWSMod

let private moveWave (model: Model) (wSMod: WaveSimModel) up =
    let lastEl (arr: 'a []) = Array.last arr

    let move arr =
        let rev a =
            if up then a else Array.rev a
        rev arr
        |> Array.fold (fun st (bl: {| Sel: bool; Indxs: int [] |}) ->
            match st with
            | [||] -> [| bl |]
            | _ ->
                if bl.Sel then
                    Array.concat
                        [| st.[0..Array.length st - 2]
                           [| bl |]
                           [| lastEl st |] |]
                else
                    Array.append st [| bl |]) [||]
        |> rev

    let indexes' =
        match Array.length wSMod.Ports with
        | len when len < 2 -> [| 0 .. len - 1 |]
        | _ ->
            Array.map (isWaveSelected model wSMod) wSMod.Ports
            |> Array.indexed 
            |> Array.fold (fun (blocks: {| Sel: bool; Indxs: int [] |} []) (ind, sel') ->
                match blocks, sel' with
                | [||], s' ->
                    Array.append blocks
                        [| {| Sel = s'
                              Indxs = [| ind |] |} |]
                | bl, true when (lastEl bl).Sel = true ->
                    Array.append blocks.[0..Array.length blocks - 2]
                        [| {| (lastEl blocks) with Indxs = Array.append (lastEl blocks).Indxs [| ind |] |} |]
                | _, s' ->
                    Array.append blocks
                        [| {| Sel = s'
                              Indxs = [| ind |] |} |]) [||]
            |> move
            |> Array.collect (fun block -> block.Indxs)

    let reorder (arr: 'b []) = Array.map (fun i -> arr.[i]) indexes'

    { wSMod with Ports = reorder wSMod.Ports }
    |> SetCurrFileWSMod

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

let private loadingBut model =
    match model.SimulationInProgress with
    | Some _ -> button [Button.Color IsDanger] (fun _ -> ()) "loading..."
    | None -> str ""

let private viewWaveSimButtonsBar model wSMod dispatch =
    div [ Style [ Height "45px" ] ]
        [ loadingBut model
          radixTabs wSMod dispatch
          cursorButtons model wSMod dispatch ]

let private cursValsCol rows =
    let rightCol = Array.append [| tr [ Class "rowHeight" ] [ td [ Class "rowHeight" ] [] ] |] rows
    div
        [ Style
            [ Float FloatOptions.Right
              Height "100%"
              BorderTop "2px solid rgb(219,219,219)"
              BorderLeft "2px solid rgb(219,219,219)" ] ] [ table [] [ tbody [] rightCol ] ]

let private openCloseWA model (wSMod: WaveSimModel) on dispatch = 
    if on then selectAllOn model wSMod dispatch |> ignore
    { wSMod with WaveAdderOpen = on }
    |> SetCurrFileWSMod |> dispatch

let private waveAdderSelectAll model (wSMod: WaveSimModel) dispatch =
    let setTo = 
        wSMod.WaveAdder.Ports 
        |> Array.forall (isWaveSelected model wSMod)

    [| 0 .. Array.length wSMod.WaveAdder.Ports - 1 |]
    |> Array.map (fun i -> selWave2selConn model wSMod i dispatch (not setTo)) 
    |> ignore

let private nameLabelsCol model (wsMod: WaveSimModel) labelRows dispatch =
    let waveAddDelBut =
        match anySelected model wsMod with
        | true ->
            [ Button.button
                [ Button.CustomClass "delWaveButton"
                  Button.Color IsDanger
                  Button.OnClick(fun _ -> delSelected model wsMod |> dispatch) ] [ str "del" ]
              div [ Class "updownDiv" ]
                  [ Button.button
                      [ Button.CustomClass "updownBut"
                        Button.OnClick(fun _ -> moveWave model wsMod true |> dispatch) ] [ str "▲" ]
                    Button.button
                        [ Button.CustomClass "updownBut"
                          Button.OnClick(fun _ -> moveWave model wsMod false |> dispatch) ] [ str "▼" ] ] ]
        | false ->
            [ Button.button
                [ Button.CustomClass "newWaveButton"
                  Button.Color IsSuccess
                  Button.OnClick(fun _ -> openCloseWA model wsMod true dispatch) ] [ str "Edit list..." ] ]
        |> (fun children -> th [ Class "waveNamesCol" ] children)

    let top =
        [| tr [ Class "rowHeight" ]
               [ th [ Class "checkboxCol" ]
                     [ input
                         [ Type "checkbox"
                           Class "check"
                           Checked(allSelected model wsMod)
                           OnChange(fun t -> selectAll model wsMod dispatch
                                             |> ignore) ] ]
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

let private wavesCol wSMod rows =
    div [ Style [ MaxWidth(maxWavesColWidth wSMod)
                  MinHeight "100%" ]
          Class "wavesTable" ] 
            [ table [ Style [ Height "100%" ] ] 
                    [ tbody [ Style [ Height "100%" ] ] rows ] ]

let private viewWaveformViewer model wSMod dispatch =
    let tableWaves, leftColMid, cursValsRows = waveSimRows model wSMod dispatch
    div
        [ Style
            [ Height "calc(100% - 45px)"
              Width "100%"
              OverflowY OverflowOptions.Auto ] ]
        [ cursValsCol cursValsRows
          div [ Style [ Height "100%" ] ]
              [ nameLabelsCol model wSMod leftColMid dispatch
                wavesCol wSMod tableWaves ] ]

let private viewZoomDiv model wSMod dispatch =
    div [ Class "zoomDiv" ]
        [ button [ Button.CustomClass "zoomButLeft" ] (fun _ -> zoom false model wSMod dispatch) "-"
          button [ Button.CustomClass "zoomButRight" ] (fun _ -> zoom true model wSMod dispatch) "+" ]

let private waveAdderTopRow model wSMod dispatch =
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
                    Checked(Array.forall (isWaveSelected model wSMod) wSMod.WaveAdder.Ports)
                    Style [ Float FloatOptions.Left ]
                    OnChange(fun _ -> waveAdderSelectAll model wSMod dispatch) ] ]
          td [ Style [ FontWeight "bold" ] ] [ str "Select All" ] ]

let private addWaveRow model wSMod ind dispatch =
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
                    Checked <| isWaveSelected model wSMod wSMod.WaveAdder.Ports.[ind]
                    Style [ Float FloatOptions.Left ]
                    OnChange(fun _ -> toggleSelect model wSMod ind dispatch) ] ]
          td [] [ label [] [ str wSMod.WaveAdder.WaveNames.[ind] ] ] ]

let private addWaveRows model wSMod dispatch = 
    Array.mapi (fun i _ -> addWaveRow model wSMod i dispatch) wSMod.WaveAdder.Ports 

let private viewWaveAdder (model: Model) wSMod dispatch =
    div [ Style [ Position PositionOptions.Absolute
                  Top "300px" ] ]
        [ table []
                [ tbody [] 
                        (Array.append [| waveAdderTopRow model wSMod dispatch |] (addWaveRows model wSMod dispatch)) ] ]

let private waveAdderButs (model: Model) wSMod dispatch =
    let simButStyle =
        match Array.exists (isWaveSelected model wSMod) wSMod.WaveAdder.Ports with
        | true ->
            [ Button.Color IsSuccess
              Button.OnClick(fun _ -> 
                Array.filter (isWaveSelected model wSMod) wSMod.WaveAdder.Ports                
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

let private waveAdderView model wSMod dispatch =
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
            [ waveAdderButs model wSMod dispatch
              viewWaveAdder model wSMod dispatch ] ] ]

let private waveformsView model wSMod dispatch =
    [ div
        [ Style
            [ Width "calc(100% - 10px)"
              Height "100%"
              MarginLeft "0%"
              MarginTop "0px"
              OverflowX OverflowOptions.Hidden ] ]
          [ viewWaveSimButtonsBar model wSMod dispatch
            viewWaveformViewer model wSMod dispatch
            viewZoomDiv model wSMod dispatch ] ]

let viewWaveSim (model: Model) dispatch =
    match currWS model, snd model.WaveSim with
    | Some wSMod, None ->
        match wSMod.WaveAdderOpen, model.SimulationInProgress with
        | _, Some _ | false, None -> waveformsView
        | true, None -> waveAdderView 
        |> (fun f -> f model wSMod dispatch)
    | Some _, Some simError ->
        [ div [ Style [ Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ]
              [ SimulationView.viewSimulationError simError
                button [ Button.Color IsDanger ] (fun _ -> None |> SetWSError |> dispatch) "Ok" ] ]
    | None, _ ->
        initFileWS model dispatch
        []
