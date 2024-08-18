﻿/// showWaveforms and subfunctions to display in DOM the waveforms
/// Waveform SVGs themselves are generated from simulation in WaveSimWaves module
module WaveSimWaveforms

//---------------------------------------------------------------------------------------//
//-----------------------Waveform display DOM Generation---------------------------------//
//---------------------------------------------------------------------------------------//

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
open WaveSimSVGs
open WaveSimSelect
open DiagramStyle

/// ReactElement of the tabs for changing displayed radix
let radixButtons (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let radixString = [
        Bin,  "Bin"
        Hex,  "Hex"
        Dec,  "uDec"
        SDec, "sDec"
    ]

    let radixTab (radix, radixStr) =
        Tabs.tab [
            Tabs.Tab.IsActive(wsModel.Radix = radix)
            Tabs.Tab.Props radixTabProps
        ] [ a [
            radixTabAStyle
            OnClick(fun _ -> dispatch <| GenerateWaveforms {wsModel with Radix = radix})
            ] [ str radixStr ]
        ]

    Tabs.tabs [
        Tabs.IsToggle
        Tabs.Props [ radixTabsStyle ]
    ] (List.map (radixTab) radixString)


let highlightCircuit fs comps wave (dispatch: Msg -> Unit) =
    dispatch <| Sheet (SheetT.Msg.Wire (BusWireT.Msg.Symbol (SymbolT.SelectSymbols comps)))
    // Filter out any non-existent wires
    let conns = connsOfWave fs wave 
    dispatch <| Sheet (SheetT.Msg.SelectWires conns)    

/// Create label of waveform name for each selected wave.
/// Note that this is generated after calling selectedWaves. Any changes to this function
/// must also be made to valueRows and waveRows, as the order of the waves matters here.
/// This is because the wave viewer is comprised of three columns of many rows, rather
/// than many rows of three columns.
let nameRows (model: Model) (wsModel: WaveSimModel) dispatch: ReactElement list =
    selectedWaves wsModel
    |> List.map (fun wave ->
        let visibility =
            if wsModel.HoveredLabel = Some wave.WaveId then
                "visible"
            else "hidden"

        Level.level [
            Level.Level.Option.Props [
                nameRowLevelStyle (wsModel.HoveredLabel = Some wave.WaveId)
                let execWithModel (f: Model -> Unit) = ExecFuncInMessage((fun model _ -> f model), dispatch)
                OnMouseOver (fun _ -> dispatch <| execWithModel (fun model ->
                    if wsModel.DraggedIndex = None then
                        dispatch <| SetWSModel {wsModel with HoveredLabel = Some wave.WaveId}
                        // Check if symbol exists on Canvas
                        let symbols = model.Sheet.Wire.Symbol.Symbols
                        match Map.tryFind (fst wave.WaveId.Id) symbols with
                        | Some {Component={Type=IOLabel;Label=lab}} ->
                            let labelComps =
                                symbols
                                |> Map.toList
                                |> List.map (fun (_,sym) -> sym.Component)
                                |> List.filter (function | {Type=IOLabel;Label = lab'} when lab' = lab -> true |_ -> false)
                                |> List.map (fun comp -> ComponentId comp.Id)
                            highlightCircuit wsModel.FastSim labelComps wave dispatch                            
                        | Some sym ->
                            highlightCircuit wsModel.FastSim [fst wave.WaveId.Id] wave dispatch
                        | None -> ())
                        
                )
                OnMouseOut (fun _ ->
                    dispatch <| SetWSModel {wsModel with HoveredLabel = None}
                    dispatch <| Sheet (SheetT.Msg.Wire (BusWireT.Msg.Symbol (SymbolT.SelectSymbols [])))
                    dispatch <| Sheet (SheetT.Msg.UpdateSelectedWires (connsOfWave wsModel.FastSim wave, false))
                )

                Draggable true

                OnDragStart (fun ev ->
                    ev.dataTransfer.effectAllowed <- "move"
                    ev.dataTransfer.dropEffect <- "move"
                    dispatch <| SetWSModel {
                        wsModel with
                            DraggedIndex = Some wave.WaveId
                            PrevSelectedWaves = Some wsModel.SelectedWaves
                        }
                )

                OnDrag (fun ev -> 
                    ev.dataTransfer.dropEffect <- "move"
                    let nameColEl = Browser.Dom.document.getElementById "namesColumn"
                    let bcr = nameColEl.getBoundingClientRect ()

                    // If the user drags the label outside the bounds of the wave name column
                    if ev.clientX < bcr.left || ev.clientX > bcr.right ||
                        ev.clientY < bcr.top || ev.clientY > bcr.bottom
                    then
                        dispatch <| SetWSModel {
                            wsModel with
                                HoveredLabel = Some wave.WaveId
                                // Use wsModel.SelectedValues if somehow PrevSelectedWaves not set
                                SelectedWaves = Option.defaultValue wsModel.SelectedWaves wsModel.PrevSelectedWaves
                            }
                )

                OnDragOver (fun ev -> ev.preventDefault ())

                OnDragEnter (fun ev ->
                    ev.preventDefault ()
                    ev.dataTransfer.dropEffect <- "move"
                    let nameColEl = Browser.Dom.document.getElementById "namesColumn"
                    let bcr = nameColEl.getBoundingClientRect ()
                    let index = int (ev.clientY - bcr.top) / Constants.rowHeight - 1
                    let draggedWave =
                        match wsModel.DraggedIndex with
                        | Some waveId -> [waveId]
                        | None -> []

                    let selectedWaves =
                        wsModel.SelectedWaves
                        |> List.except draggedWave
                        |> List.insertManyAt index draggedWave

                    dispatch <| SetWSModel {wsModel with SelectedWaves = selectedWaves}
                )

                OnDragEnd (fun _ ->
                    dispatch <| SetWSModel {
                        wsModel with
                            DraggedIndex = None
                            PrevSelectedWaves = None
                        }
                )
            ]
        ] [ Level.left
                [ Props (nameRowLevelLeftProps visibility) ]
                [ Delete.delete [
                    Delete.Option.Size IsSmall
                    Delete.Option.Props [
                        OnClick (fun _ ->
                            let selectedWaves = List.except [wave.WaveId] wsModel.SelectedWaves
                            dispatch <| SetWSModel {wsModel with SelectedWaves = selectedWaves}
                        )
                    ]
                  ] []
                ]
            Level.right
                [ Props [ Style [ PaddingRight Constants.labelPadding ] ] ]
                [ label [ nameLabelStyle (wsModel.HoveredLabel = Some wave.WaveId) ] [ wave.ViewerDisplayName|> str ] ]
        ]
    )

/// Create column of waveform names
let namesColumn model wsModel dispatch : ReactElement =
    let start = TimeHelpers.getTimeMs ()
    let rows = 
        nameRows model wsModel dispatch
    div (namesColumnProps wsModel)
        (List.concat [ topRow []; rows ])
    //|> TimeHelpers.instrumentInterval "namesColumn" start


/// Create label of waveform value for each selected wave at a given clk cycle.
/// Note that this is generated after calling selectedWaves.
/// Any changes to this function must also be made to nameRows
/// and waveRows, as the order of the waves matters here. This is
/// because the wave viewer is comprised of three columns of many
/// rows, rather than many rows of three columns.
/// Return required width of values column in pixels, and list of cloumn react elements.
let valueRows (wsModel: WaveSimModel) =
    let valueColWidth, valueColNumChars =
        valuesColumnSize wsModel
    selectedWaves wsModel
    |> List.map (fun wave -> getWaveValue wsModel.CurrClkCycleDetail wave wave.Width)
    |> List.map (fun fd ->
        match fd.Width, fd.Dat with
        | 1, Word b -> $" {b}" 
        | _ -> fastDataToPaddedString valueColNumChars wsModel.Radix fd)
    |> List.map (fun value -> label [ valueLabelStyle ] [ str value ])
    |> (fun rows -> valueColWidth, rows)


/// Generate a row of numbers in the waveforms column.
/// Numbers correspond to clock cycles multiplied by the current multiplier
let clkCycleNumberRow (wsModel: WaveSimModel) =
    let makeClkCycleLabel i =
        let n = i * wsModel.CycleMultiplier
        match singleWaveWidth wsModel with
        | width when width < float Constants.clkCycleNarrowThreshold && i % 5 <> 0 ->
            []
        | width when n >= 1000 && width <  (float Constants.clkCycleNarrowThreshold * 4. / 3.) && i % 10 <> 0 ->
            []
        | _ ->
            [ text (clkCycleText wsModel i) [str (string n)] ]
            

    [ wsModel.StartCycle .. endCycle wsModel]
    |> List.collect makeClkCycleLabel
    |> svg (clkCycleNumberRowProps wsModel)

/// Create column of waveform values
let private valuesColumn wsModel : ReactElement =
    let start = TimeHelpers.getTimeMs ()
    let width, rows = valueRows wsModel
    let cursorClkNum = wsModel.CurrClkCycleDetail
    let topRowNumber = [ div [Style [FontSize "10pt" ; VerticalAlign "bottom"; FontWeight "bold"; PaddingLeft "4pt"]] [str (string <| cursorClkNum)] ]
    div [ HTMLAttr.Id "ValuesCol" ; valuesColumnStyle width]
        (List.concat [ topRow topRowNumber ; rows ])
    //|> TimeHelpers.instrumentInterval "valuesColumn" start

/// Generate a column of waveforms corresponding to selected waves.
let waveformColumn (wsModel: WaveSimModel) dispatch : ReactElement =
    let start = TimeHelpers.getTimeMs ()
    /// Note that this is generated after calling selectedWaves.
    /// Any changes to this function must also be made to nameRows
    /// and valueRows, as the order of the waves matters here. This is
    /// because the wave viewer is comprised of three columns of many
    /// rows, rather than many rows of three columns.
    let waves = selectedWaves wsModel
    if List.exists (fun wave -> wave.SVG = None) waves then
        dispatch <| GenerateCurrentWaveforms
    let waveRows : ReactElement list =
        waves
        |> List.map (fun wave ->
            match wave.SVG with
            | Some waveform ->
                waveform
            | None ->
                div [] [] // the GenerateCurrentWaveforms message will soon update this
        )

    div [ waveformColumnStyle ]
        [
            cursorCycleHighlightSVG wsModel dispatch
            div [ waveRowsStyle <| wsModel.WaveformColumnWidth]
                ([ clkCycleNumberRow wsModel ] @
                    waveRows
                )
        ]
    |> TimeHelpers.instrumentInterval "waveformColumn" start

/// Display the names, waveforms, and values of selected waveforms
let showWaveforms (model: Model) (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    if List.isEmpty wsModel.SelectedWaves then
        div [] [] // no waveforms
    else
        let wHeight = calcWaveformHeight wsModel
        let fixedHeight = Constants.softScrollBarWidth + Constants.topHalfHeight
        let cssHeight =
            if wsModel.SelectedRams.Count > 0 then
                $"min( calc(50vh - (0.5 * {fixedHeight}px)) ,  {wHeight}px)"
            else
                $"min( calc(100vh - {fixedHeight}px) ,  {wHeight}px)"

        div [ HTMLAttr.Id "Scroller";  Style [ Height cssHeight; Width "100%"; CSSProp.Custom("overflow", "hidden auto")]] [
            div [ HTMLAttr.Id "WaveCols" ;showWaveformsStyle ]
                [
                    namesColumn model wsModel dispatch 
                    waveformColumn wsModel dispatch
                    valuesColumn wsModel
                ]
            ]

