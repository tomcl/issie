module WaveSim

open Fulma
open Fable.React
open Fable.React.Props

open CommonTypes
open ModelType
open ModelHelpers
open WaveSimStyle
open WaveSimHelpers
open FileMenuView
open SimulatorTypes
open NumberHelpers
open DrawModelType
open WaveSimSelect

/// Generates SVG to display values on non-binary waveforms when there is enough space.
/// TODO: Fix this so it does not generate all 500 cycles.
let displayValuesOnWave wsModel (waveValues: FData array) (transitions: NonBinaryTransition array) : ReactElement list =
    /// Find all clock cycles where there is a NonBinaryTransition.Change
    let changeTransitions =
        transitions
        |> Array.indexed
        |> Array.filter (fun (_, x) -> x = Change)
        |> Array.map (fun (i, _) -> i)

    /// Find start and length of each gap between a Change transition
    let gaps : Gap array =
        // Append dummy transition to end to check final gap length
        Array.append changeTransitions [|wsModel.StartCycle + transitions.Length - 1|]
        |> Array.pairwise
        // Get start of gap and length of gap
        |> Array.map (fun (i1, i2) -> {
                Start = i1
                Length = i2 - i1
            }
        )

    gaps
    // Create text react elements for each gap
    |> Array.map (fun gap ->
        let waveValue =
            let wd = waveValues[gap.Start]
            valToPaddedString wd.Width  wsModel.Radix wd.toInt64
  

        /// Amount of whitespace between two Change transitions minus the crosshatch
        let availableWidth = (float gap.Length * (singleWaveWidth wsModel)) - 2. * Constants.nonBinaryTransLen
        /// Required width to display one value
        let requiredWidth = DrawHelpers.getTextWidthInPixels (waveValue, Constants.valueOnWaveText)
        /// Width of text plus whitespace between a repeat
        let widthWithPadding = 2. * requiredWidth + Constants.valueOnWavePadding

        // Display nothing if there is not enough space
        if availableWidth < requiredWidth then
            []
        else
            let valueText i =
                text (valueOnWaveProps wsModel i gap.Start widthWithPadding)
                    [ str waveValue ]

            /// Calculate how many times the value can be shown in the space available
            let repeats = int <| availableWidth / widthWithPadding

            [ 0 .. repeats ]
            |> List.map valueText
    )
    |> List.concat

/// Called when InitiateWaveSimulation msg is dispatched
/// and when wave simulator is refreshed.
/// Generates the SVG for a specific waveform.
let generateWaveform (wsModel: WaveSimModel) (index: WaveIndexT) (wave: Wave): Wave =
    // Only generate waveforms for selected waves
    //printfn $"generating {wave.DisplayName} of {wsModel.SelectedWaves.Length} waves"
    if List.contains index wsModel.SelectedWaves then
        let waveform =
            match wave.Width with
            | 0 -> failwithf "Cannot have wave of width 0"
            // Binary waveform
            | 1 ->
                //printfn "starting binary"
                let start = TimeHelpers.getTimeMs ()
                let transitions = calculateBinaryTransitions wave.WaveValues
                /// TODO: Fix this so that it does not generate all 500 points.
                /// Currently takes in 0, but this should ideally only generate the points that
                /// are shown on screen, rather than all 500 cycles.
                let wavePoints =
                    Array.mapi (binaryWavePoints (singleWaveWidth wsModel) 0) transitions 
                    |> Array.concat
                    |> Array.distinct

                svg (waveRowProps wsModel)
                    [ polyline (wavePolylineStyle wavePoints) [] ]
                |> TimeHelpers.instrumentInterval "binary waveform" start
            // Non-binary waveform
            | _ ->
                //printfn "starting non-binary"
                let start = TimeHelpers.getTimeMs ()

                let transitions = calculateNonBinaryTransitions wave.WaveValues
                //printfn "calculating trans..."
                /// TODO: Fix this so that it does not generate all 500 points.
                /// Currently takes in 0, but this should ideally only generate the points that
                /// are shown on screen, rather than all 500 cycles.
                let fstPoints, sndPoints =
                    Array.mapi (nonBinaryWavePoints (singleWaveWidth wsModel) 0) transitions 
                    |> Array.unzip
                //printfn "points"
                let makePolyline points = 
                    let points =
                        points
                        |> Array.concat
                        |> Array.distinct
                    polyline (wavePolylineStyle points) []

                let valuesSVG = displayValuesOnWave wsModel wave.WaveValues transitions
                //printfn "values"
                svg (waveRowProps wsModel)
                    (List.append [makePolyline fstPoints; makePolyline sndPoints] valuesSVG)
                //|> (fun x -> printfn "makepolyline"; x)
                |> TimeHelpers.instrumentInterval "nonbinary waveform" start
        //printfn "end generate"
        {wave with SVG = Some waveform}
    else wave



/// Set highlighted clock cycle number
let private setClkCycle (wsModel: WaveSimModel) (dispatch: Msg -> unit) (newClkCycle: int) : unit =
    let start = TimeHelpers.getTimeMs ()
    let newClkCycle = min Constants.maxLastClk newClkCycle |> max 0

    if newClkCycle <= endCycle wsModel then
        if newClkCycle < wsModel.StartCycle then
            dispatch <| GenerateWaveforms
                {wsModel with 
                    StartCycle = newClkCycle
                    CurrClkCycle = newClkCycle
                    ClkCycleBoxIsEmpty = false
                }
        else
            dispatch <| SetWSModel
                {wsModel with
                    CurrClkCycle = newClkCycle
                    ClkCycleBoxIsEmpty = false
                }
    else
        dispatch <| GenerateWaveforms
            {wsModel with
                StartCycle = newClkCycle - (wsModel.ShownCycles - 1)
                CurrClkCycle = newClkCycle
                ClkCycleBoxIsEmpty = false
            }
    |> TimeHelpers.instrumentInterval "setClkCycle" start

/// if zoomIn, then increase width of clock cycles (i.e.reduce number of visible cycles)
let changeZoom (wsModel: WaveSimModel) (zoomIn: bool) (dispatch: Msg -> unit) =
    let start = TimeHelpers.getTimeMs ()
    let shownCycles =
        if zoomIn then
            let newCycles = int <| float wsModel.ShownCycles * 0.8

            // If number of cycles after casting to int does not change
            if newCycles = int wsModel.ShownCycles then
                wsModel.ShownCycles - 1
            // Require at least one visible cycle
            else max 1 (newCycles)
        else
            let newCycles = int <| float wsModel.ShownCycles * 1.25

            // If number of cycles after casting to int does not change
            if newCycles = wsModel.ShownCycles then
                wsModel.ShownCycles + 1
            // If width of clock cycle is too small
            else if wsModel.WaveformColumnWidth / float newCycles < Constants.minCycleWidth then
                wsModel.ShownCycles
            else newCycles

    dispatch <| GenerateWaveforms { wsModel with ShownCycles = shownCycles }
    |> TimeHelpers.instrumentInterval "changeZoom" start

/// Click on these buttons to change the number of visible clock cycles.
let zoomButtons (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    div [ clkCycleButtonStyle ]
        [
            button [ Button.Props [clkCycleLeftStyle] ]
                (fun _ -> changeZoom wsModel false dispatch)
                zoomOutSVG
            button [ Button.Props [clkCycleRightStyle] ]
                (fun _ -> changeZoom wsModel true dispatch)
                zoomInSVG
        ]

/// Click on these to change the highlighted clock cycle.
let clkCycleButtons (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    /// Controls the number of cycles moved by the "◀◀" and "▶▶" buttons
    let bigStepSize = max 2 (wsModel.ShownCycles / 2)

    let scrollWaveformsBy (numCycles: int) =
        setClkCycle wsModel dispatch (wsModel.CurrClkCycle + numCycles)

    div [ clkCycleButtonStyle ]
        [
            // Move left by bigStepSize cycles
            button [ Button.Props [clkCycleLeftStyle] ]
                (fun _ -> scrollWaveformsBy -bigStepSize)
                (str "◀◀")

            // Move left by one cycle
            button [ Button.Props [clkCycleInnerStyle] ]
                (fun _ -> scrollWaveformsBy -1)
                (str "◀")

            // Text input box for manual selection of clock cycle
            Input.number [
                Input.Props clkCycleInputProps

                Input.Value (
                    match wsModel.ClkCycleBoxIsEmpty with
                    | true -> ""
                    | false -> string wsModel.CurrClkCycle
                )
                // TODO: Test more properly with invalid inputs (including negative numbers)
                Input.OnChange(fun c ->
                    match System.Int32.TryParse c.Value with
                    | true, n ->
                        setClkCycle wsModel dispatch n
                    | false, _ when c.Value = "" ->
                        dispatch <| SetWSModel {wsModel with ClkCycleBoxIsEmpty = true}
                    | _ ->
                        dispatch <| SetWSModel {wsModel with ClkCycleBoxIsEmpty = false}
                )
            ]

            // Move right by one cycle
            button [ Button.Props [clkCycleInnerStyle] ]
                (fun _ -> scrollWaveformsBy 1)
                (str "▶")

            // Move right by bigStepSize cycles
            button [ Button.Props [clkCycleRightStyle] ]
                (fun _ -> scrollWaveformsBy bigStepSize)
                (str "▶▶")
        ]

/// ReactElement of the tabs for changing displayed radix
let private radixButtons (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
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

                OnMouseOver (fun _ ->
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
                                |> List.filter (function | {Type=IOLabel;Label = lab} -> true |_ -> false)
                                |> List.map (fun comp -> ComponentId comp.Id)
                            highlightCircuit wsModel.FastSim labelComps wave dispatch                            
                        | Some sym ->
                            highlightCircuit wsModel.FastSim [fst wave.WaveId.Id] wave dispatch
                        | None -> ()
                        
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
        (List.concat [ topRow; rows ])
    |> TimeHelpers.instrumentInterval "namesColumn" start


/// Create label of waveform value for each selected wave at a given clk cycle.
/// Note that this is generated after calling selectedWaves.
/// Any changes to this function must also be made to nameRows
/// and waveRows, as the order of the waves matters here. This is
/// because the wave viewer is comprised of three columns of many
/// rows, rather than many rows of three columns.
let valueRows (wsModel: WaveSimModel) =
    selectedWaves wsModel
    |> List.map (getWaveValue wsModel.CurrClkCycle)
    |> List.map (valToString wsModel.Radix)
    |> List.map (fun value -> label [ valueLabelStyle ] [ str value ])

/// Create column of waveform values
let private valuesColumn wsModel : ReactElement =
    let start = TimeHelpers.getTimeMs ()
    let rows = valueRows wsModel

    div [ valuesColumnStyle ]
        (List.concat [ topRow; rows ])
    |> TimeHelpers.instrumentInterval "valuesColumn" start

/// Generate a row of numbers in the waveforms column.
/// Numbers correspond to clock cycles.
let clkCycleNumberRow (wsModel: WaveSimModel) =
    let makeClkCycleLabel i =
        match (singleWaveWidth wsModel) with
        | width when width < Constants.clkCycleNarrowThreshold && i % 5 <> 0 -> []
        | _ -> [ text (clkCycleText wsModel i) [str (string i)] ]

    [ wsModel.StartCycle .. endCycle wsModel]
    |> List.collect makeClkCycleLabel
    |> svg (clkCycleNumberRowProps wsModel)

/// Generate a column of waveforms corresponding to selected waves.
let waveformColumn (wsModel: WaveSimModel) dispatch : ReactElement =
    let start = TimeHelpers.getTimeMs ()
    /// Note that this is generated after calling selectedWaves.
    /// Any changes to this function must also be made to nameRows
    /// and valueRows, as the order of the waves matters here. This is
    /// because the wave viewer is comprised of three columns of many
    /// rows, rather than many rows of three columns.
    let waveRows : ReactElement list =
        selectedWaves wsModel
        |> List.map (fun wave ->
            match wave.SVG with
            | Some waveform ->
                waveform
            | None ->
                printf "no waveform generated for %A" wave.DisplayName
                div [] []
        )

    div [ waveformColumnStyle ]
        [
            clkCycleHighlightSVG wsModel dispatch
            div [ waveRowsStyle wsModel.WaveformColumnWidth]
                ([ clkCycleNumberRow wsModel ] @
                    waveRows
                )
        ]
    |> TimeHelpers.instrumentInterval "waveformColumn" start

/// Display the names, waveforms, and values of selected waveforms
let showWaveforms (model: Model) (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    div [ showWaveformsStyle ]
        [
            namesColumn model wsModel dispatch
            waveformColumn wsModel dispatch
            valuesColumn wsModel
        ]

/// Table row that shows the address and data of a RAM component.
let ramTableRow ((addr, data,rowType): string * string * RamRowType): ReactElement =

    tr [ Style <| ramTableRowStyle rowType ] [
        td [] [ str addr ]
        td [] [ str data ]
    ]

/// Table showing contents of a RAM component.
let ramTable (wsModel: WaveSimModel) ((ramId, ramLabel): FComponentId * string) : ReactElement =

    let fs = wsModel.FastSim
    let fc = wsModel.FastSim.FComps[ramId]
    let step = wsModel.CurrClkCycle
    let memData =
        match fc.FType with
        | ROM1 mem
        | AsyncROM1 mem -> mem
        | RAM1 mem
        | AsyncRAM1 mem -> 
            match FastRun.extractFastSimulationState fs wsModel.CurrClkCycle ramId with
            |RamState mem -> mem
            | _ -> failwithf $"What? Can't find state from RAM component '{ramLabel}'"
        | _ -> failwithf $"Given a component {fc.FType} which is not a vaild RAM"
    let aWidth,dWidth = memData.AddressWidth,memData.WordWidth

    let print w (a:int64) = NumberHelpers.valToPaddedString w wsModel.Radix (((1L <<< w) - 1L) &&& a)

    let lastLocation = int64 ((2 <<< memData.AddressWidth - 1) - 1)

    /// print a single 0 location as one table row
    let print1 (a:int64,b:int64,rw:RamRowType) = $"{print aWidth a}",$"{print dWidth b}",rw
    /// print a range of zero locations as one table row

    let print2 (a1:int64) (a2:int64) (d:int64) = $"{print aWidth (a1+1L)}..{print aWidth (a2-1L)}", $"{print dWidth d}",RAMNormal

    /// output info for one table row filling the given zero memory gap or arbitrary size, or no line if there is no gap.
    let printGap (gStart:int64) (gEnd:int64) =
        match gEnd - gStart with
        | 1L -> []
        | 2L -> [print1 ((gEnd + gStart) / 2L, 0L,RAMNormal)]
        | n when n > 2L ->
            [print2 gStart gEnd 0L]
        | _ ->
            failwithf $"What? gEnd={gEnd},gStart={gStart}: negative or zero gaps are impossible..."

    /// transform Sparse RAM info into strings to print in a table, adding extra lines for zero gaps
    /// line styling is controlled by a RamRowtype value and added later when the table row react is generated
    let addGapLines (items: (int64*int64*RamRowType) list) = 
        let startItem =
            match items[0] with
            | -1L,_,_ -> []
            | gStart,dStart,rw-> [print1 (gStart,dStart,rw)]
        List.pairwise items
        |> List.collect (fun ((gStart,_,_),(gEnd,dEnd,rwe)) -> 
            let thisItem = if gEnd = lastLocation + 1L then [] else [print1 (gEnd,dEnd,rwe)]
            [printGap gStart gEnd; thisItem])
        |> List.concat

    /// Add a RAMNormal RamRowType value to every location in mem.
    /// Add in additional locations for read and/or write if needed.
    /// Set RamRowValue type to RAMWritten or RAMRead for thse locations.
    /// Write is always 1 cycle after WEN=1 and address.
    /// Read is 1 (0) cycles after address for sync (asynch) memories.
    let addReadWrite (fc:FastComponent) (step:int) (mem: Map<int64,int64>) =
        let getFData (fd: FData) =
            match fd with
            | Data {Dat= Word w} -> int64 w
            | Data {Dat=BigWord bw} -> int64 bw
            | _ -> 
                printfn $"Help! Can'd find data from {fd}"
                int64 <| -1

        let readStep =
            match fc.FType with
            | AsyncROM1 _ | AsyncRAM1 _ -> step
            | ROM1 _ | RAM1 _ -> step - 1
            | _ -> failwithf $"What? {fc.FullName} should be a memory component"

        let addrSteps step = fc.InputLinks[0].Step[step]

        let readOpt =
            match step, fc.FType with
            | 0,ROM1 _ | 0, RAM1 _ -> None
            | _ -> 
                addrSteps readStep
                |> getFData
                |> Some
        let writeOpt =
            match step, fc.FType with
            | _, ROM1 _ 
            | _, AsyncROM1 _
            | 0, _ -> None
            | _, RAM1 _ | _, AsyncRAM1 _ when getFData fc.InputLinks[2].Step[step-1] = 1L -> 
                addrSteps (step-1)
                |> Some
            | _ ->  
                None
            |> Option.map getFData

        /// Mark addr in memory map as being rType
        /// if addr does not exist - create it
        let addToMap rType addr mem:Map<int64,int64*RamRowType> =
            match Map.tryFind addr mem with
            | Some (d,_) -> Map.add addr (d,rType) mem
            | None  ->  Map.add addr (0L,rType) mem
    

        Map.map (fun k v -> v,RAMNormal) mem
        |> (fun mem ->
            match readOpt with
            | Some addr -> addToMap RAMRead addr mem
            | None -> mem
            |> (fun mem ->
                match writeOpt with // overwrite RAMRead here is need be
                | Some addr -> addToMap RAMWritten addr mem
                | None -> mem))
 

    /// add fake locations beyong normal address range so that
    /// addGapLines fills these (if need be). These locations are then removed
    let addEndPoints (items:(int64*int64*RamRowType) list)  =
        let ad (a,d,rw) = a
        match items.Length with
        | 0 -> [-1L,0L,RAMNormal;  lastLocation,0L,RAMNormal]
        | _ ->
            if ad items[0] < 0L then items else List.insertAt 0 (-1L,-1L,RAMNormal) items
            |> (fun items ->
                if ad items[items.Length-1] = lastLocation then 
                    items else 
                List.insertAt items.Length (lastLocation+1L,0L,RAMNormal) items)
    

    let lineItems =
        memData.Data
        |> addReadWrite fc step
        |> Map.toList
        |> List.map (fun (a,(d,rw)) -> a,d,rw)
        |> List.filter (fun (a,d,rw) -> d<>0L || rw <> RAMNormal)
        |> List.sort
        |> addEndPoints 
        |> addGapLines
        


    Level.item [
        Level.Item.Option.Props ramTableLevelProps
        Level.Item.Option.HasTextCentered
    ] [
        Heading.h6 [
            Heading.Option.Props [ centerAlignStyle ]
        ] [ str ramLabel ]
        div [Style [MaxHeight "600px";OverflowY OverflowOptions.Auto]] [
        Table.table [
            Table.IsFullWidth
            Table.IsBordered
        ] [ thead [] [
                tr [] [
                    th [ centerAlignStyle ] [ str "Address"]
                    th [ centerAlignStyle ] [ str "Data"; sub [Style [MarginLeft "2px"; FontSize "10px"]] [str (string wsModel.CurrClkCycle)]]
                ]
            ]
            tbody []
                (List.map ramTableRow lineItems) 
        ] ]
        br []
    ]

/// Bulma Level component of tables showing RAM contents.
let ramTables (wsModel: WaveSimModel) : ReactElement =
    let inlineStyle (styles:CSSProp list) = div [Style (Display DisplayOptions.Inline :: styles)]
    let start = TimeHelpers.getTimeMs ()
    let selectedRams = Map.toList wsModel.SelectedRams
    if List.length selectedRams > 0 then
        let headerRow =
            ["read", RAMRead; "overwritten",RAMWritten]
            |> List.map (fun (op, opStyle) -> inlineStyle [] [inlineStyle (ramTableRowStyle  opStyle) [str op]])
            |> function | [a;b] -> [str "Key: Memory location is " ; a; str ", or " ;b; str ". Click waveforms or use control to change cycle."] | _ -> failwithf "What? Can't happen!"
        List.map (fun ram -> td [Style [BorderColor "white"]] [ramTable wsModel ram])  selectedRams
        |> (fun tables -> [tbody [] [tr [] [th [ColSpan selectedRams.Length] [inlineStyle [] headerRow]]; tr [Style [Border "10px"]] tables]])
        |> Fulma.Table.table [Table.TableOption.Props ramTablesLevelProps; Table.IsFullWidth; Table.IsBordered; Table.Props [Style [Height "100%"]]]
    else div [] []
    |> TimeHelpers.instrumentInterval "ramTables" start

let refreshWaveSim (wsModel: WaveSimModel, simData, (comps, conns)) : WaveSimModel = 
    let start = TimeHelpers.getTimeMs ()
    // starting runSimulation
    FastRun.runFastSimulation (wsModel.StartCycle + wsModel.ShownCycles+1) simData.FastSim
    |> TimeHelpers.instrumentInterval "runFastSimulation" start
    let fs = simData.FastSim

    let allWaves =
        let allWavesStart = TimeHelpers.getTimeMs ()
        //printfn "starting getwaves"
        getWaves simData 
        //|> (fun x -> printfn "getwaves done";x)
        |> TimeHelpers.instrumentInterval "getWaves" allWavesStart
        |> Map.map (generateWaveform wsModel)
        //|> (fun x -> printfn "generatemap done.";x)
        |> TimeHelpers.instrumentInterval "allWaves" allWavesStart

    let ramComps =
        let isRAMOrROM fcid (fc: FastComponent) =
            match fc.FType with
            | RAM1 _ | ROM1 _ | AsyncRAM1 _ | AsyncROM1 _ ->
                true
            | _ -> false
        Map.filter isRAMOrROM simData.FastSim.FComps
        |> Map.toList
        |> List.map (fun (fcid,fc) -> fc)
        |> List.sortBy (fun fc -> fc.FullName)

    let ramCompIds = List.map (fun (fc: FastComponent) -> fc.fId) ramComps

    let selectedWaves = List.filter (fun key -> Map.containsKey key allWaves) wsModel.SelectedWaves
    let selectedRams = Map.filter (fun ramfId _ -> List.contains ramfId ramCompIds) wsModel.SelectedRams

    //return 
    {
        wsModel with
            State = Success
            AllWaves = allWaves
            SelectedWaves = selectedWaves
            RamComps = ramComps
            SelectedRams = selectedRams
            FastSim = simData.FastSim
    }
    |> TimeHelpers.instrumentInterval "refreshWaveSim" start
//}

/// Refresh the state of the wave simulator according to the model and canvas state.
let refreshButtonAction canvasState model dispatch = fun _ ->
    let wsSheet = 
        match model.WaveSimSheet with
        | None -> Option.get (getCurrFile model)
        | Some sheet -> sheet
    let wsModel = getWSModel model
    //printfn $"simSheet={wsSheet}, wsModel sheet = {wsModel.TopSheet},{wsModel.FastSim.SimulatedTopSheet}, state={wsModel.State}"
    match SimulationView.makeSimData model.WaveSimSheet (WaveSimHelpers.Constants.maxLastClk + 1)  canvasState model with
    | None ->
        dispatch <| SetWSModel { wsModel with State = NoProject }
    | Some (Error e, _) ->
        dispatch <| SetWSModelAndSheet ({ wsModel with State = SimError e }, wsSheet)
    | Some (Ok simData, canvState) ->
        if simData.IsSynchronous then
            SimulationView.setFastSimInputsToDefault simData.FastSim
            let wsModel = { wsModel with State = Loading }
            dispatch <| SetWSModelAndSheet (wsModel, wsSheet)
            dispatch <| RefreshWaveSim (wsModel, simData, canvState)
        else
            dispatch <| SetWSModelAndSheet ({ wsModel with State = NonSequential }, wsSheet)
           
/// ReactElement showing instructions and wave sim buttons
let topHalf canvasState (model: Model) dispatch : ReactElement =
    let title =
        match model.WaveSimSheet with
        | None -> "Waveform Viewer"
        | Some sheet -> $"Simulating sheet '{sheet}'"
    let wsModel = getWSModel model
    //printfn $"Active wsModel sheet={model.WaveSimSheet}, state = {wsModel.State}"
    //printfn $"""Wavesim states: {model.WaveSim |> Map.toList |> List.map (fun (sh, ws) -> sh, ws.State.ToString(),ws.Sheets)}"""
    let loading =
        match wsModel.State with
        | Loading -> true
        | _ -> false
    let refreshButtonSvg = if loading then emptyRefreshSVG else refreshSvg "white" "20px"

    div [ topHalfStyle ] [
        br []
        Level.level [] [
            Level.left [] [
                Heading.h4 [] [ 
                                (div [Style [Display DisplayOptions.Inline; MarginRight "10px"]] [str title]) 
                                button 
                                    (topHalfButtonProps IsInfo)
                                    (fun _ -> PopupView.viewWaveInfoPopup dispatch)
                                    (str Constants.infoSignUnicode)
 ]
            ]
            Level.right [] [
                let startOrRenew = refreshButtonAction canvasState model dispatch
                let waveEnd = endButtonAction canvasState model dispatch
                let wbo = getWaveSimButtonOptions canvasState model wsModel
                //printfn $"Sim is Dirty{wbo.IsDirty}" 
                div []
                    (let needsRefresh = wbo.IsDirty && wbo.IsRunning
                    (if not wbo.IsRunning then 
                        [] 
                    else
                        [                        
                            button
                                (Button.Disabled (not needsRefresh) :: topHalfButtonPropsWithWidth IsSuccess)
                                startOrRenew
                                refreshButtonSvg
                        ]))

                button 
                    (topHalfButtonPropsWithWidth wbo.StartEndColor) 
                    (fun ev -> if wbo.IsRunning then waveEnd ev else startOrRenew ev)
                    (str wbo.StartEndMsg)
                ]
        ]

        Columns.columns [] [
            Column.column [] (
                if model.WaveSimSheet <> None then 
                    [
                        str "View clocked logic waveforms by selecting waves. "
                        str "Select RAMs or ROMs to view contents during the simulation. "
                        str "View or change any sheet with simulation running. "
                        str "After design changes use "
                        refreshSvg "black" "12px"
                        str " to update waveforms."
                    ] else
                    [
                        str "Use 'Start Simulation' button to simulate current sheet."
                        str "Drag diver to change width of Viewer."
                    ])

            Column.column [
                Column.Option.Width (Screen.All, Column.IsNarrow)
            ] [ Level.level [] [
                    Level.item [ ] [
                        Button.list [] [
                            selectWavesButton wsModel dispatch
                            selectWavesModal wsModel dispatch

                            selectRamButton wsModel dispatch
                            selectRamModal wsModel dispatch
                        ]
                    ]
                ]
                Level.level [] [
                    Level.left [] [
                        zoomButtons wsModel dispatch
                    ]
                    Level.right [] [
                        radixButtons wsModel dispatch
                    ]
                ]
                clkCycleButtons wsModel dispatch
            ]
        ]
        hr [ Style [ MarginBottom "5px" ] ]
        br []
    ]

/// Entry point to the waveform simulator.
let viewWaveSim canvasState (model: Model) dispatch : ReactElement =
    let wsModel = getWSModel model
    let notRunning = 
        div [ errorMessageStyle ] [ str "Start the waveform viewer by pressing the Start button." ]

    let simError e =
        SimulationView.setSimErrorFeedback e model dispatch
        div [ errorMessageStyle ]
            [ SimulationView.viewSimulationError e ]
        
    div [ viewWaveSimStyle ]
        [
            topHalf canvasState model dispatch
            match model.WaveSimSheet, wsModel.State with
            | Some sheet as sheetOpt, SimError e when sheetOpt <> getCurrFile model ->
                dispatch <| UpdateModel( fun model -> {model with WaveSimSheet = None})
                dispatch <| UpdateModel( updateWSModelOfSheet sheet (fun ws -> {ws with State = Ended}))
                notRunning
            | None, SimError e  ->
                notRunning
            | _,SimError e ->
                simError e               
            | _,NonSequential ->
                div [ errorMessageStyle ]
                    [ str "There is no clocked logic in this circuit. Add clocked logic to simulate waveforms" ]
            | _,Empty | _,Ended | None,_ | Some "", _-> notRunning
            | Some sheet, _ when wsModel.FastSim.SimulatedTopSheet = "" -> notRunning              
            | _,NoProject ->
                div [ errorMessageStyle ]
                    [ str "Please open a project to use the waveform viewer." ]
            | _,Loading | _,Success ->
                //printfn $"Showing waveforms: fs= {wsModel.FastSim}"
                div [showWaveformsAndRamStyle] [

                    showWaveforms model wsModel dispatch

                    hr []

                    ramTables wsModel
                    ]

            hr []
        ]
