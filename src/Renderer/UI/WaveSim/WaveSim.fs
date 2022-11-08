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
            match waveValues[gap.Start] with
            | Data fastDat -> fastDataToPaddedString WaveSimHelpers.Constants.waveLegendMaxChars wsModel.Radix fastDat
            | Alg _ -> "Error - algebraic data!"

        /// Amount of whitespace between two Change transitions minus the crosshatch
        let cycleWidth = singleWaveWidth wsModel
        let availableWidth = (float gap.Length * cycleWidth) - 2. * Constants.nonBinaryTransLen
        /// Required width to display one value
        let requiredWidth = 1.1 * DrawHelpers.getTextWidthInPixels (waveValue, Constants.valueOnWaveText)
        /// Width of text plus whitespace between a repeat
        let widthWithPadding = 2. * requiredWidth + Constants.valueOnWavePadding

        // Display nothing if there is not enough space
        if availableWidth < requiredWidth then
            []
        else

            /// Calculate how many times the value can be shown in the space available
            let repeats =
                availableWidth / widthWithPadding
                |> System.Math.Floor
                |> int
                |> max 1

            let repeatSpace = (availableWidth - float repeats * requiredWidth) / ((float repeats + 1.) * cycleWidth)

            let valueText i =
                text (valueOnWaveProps wsModel i (float gap.Start + repeatSpace) widthWithPadding)
                    [ str waveValue ]

            [ 0 .. repeats - 1]
            |> List.map valueText
    )
    |> List.concat

/// Detects if SVG is correct, based on zoom & position & existence
/// The fast simulation data is assumed unchanged
let waveformIsUptodate (ws: WaveSimModel) (wave:Wave) =
    wave.SVG <> None &&
    wave.ShownCycles = ws.ShownCycles &&
    wave.StartCycle = ws.StartCycle &&
    wave.CycleWidth = singleWaveWidth ws &&
    wave.Radix = ws.Radix

/// Called when InitiateWaveSimulation msg is dispatched
/// and when wave simulator is refreshed.
/// Generates or updates the SVG for a specific waveform whetehr needed or not.
/// The SVG depends on cycle width as well as start/stop clocks and design.
/// Assumes that the fast simulation data has not changed and has enough cycles
let generateWaveform (ws: WaveSimModel) (index: WaveIndexT) (wave: Wave): Wave =
        let waveform =
            match wave.Width with
            | 0 -> failwithf "Cannot have wave of width 0"
            // Binary waveform
            | 1 ->
                //printfn "starting binary"
                let start = TimeHelpers.getTimeMs ()
                let transitions = calculateBinaryTransitions wave.WaveValues.Step
                /// TODO: Fix this so that it does not generate all 500 points.
                /// Currently takes in 0, but this should ideally only generate the points that
                /// are shown on screen, rather than all 500 cycles.
                let wavePoints =
                    Array.mapi (binaryWavePoints (singleWaveWidth ws) 0) transitions 
                    |> Array.concat
                    |> Array.distinct

                svg (waveRowProps ws)
                    [ polyline (wavePolylineStyle wavePoints) [] ]
            // Non-binary waveform
            | _ ->
                //printfn "starting non-binary"
                let start = TimeHelpers.getTimeMs ()

                let transitions = calculateNonBinaryTransitions wave.WaveValues.Step
                //printfn "calculating trans..."
                /// TODO: Fix this so that it does not generate all 500 points.
                /// Currently takes in 0, but this should ideally only generate the points that
                /// are shown on screen, rather than all 500 cycles.
                let fstPoints, sndPoints =
                    Array.mapi (nonBinaryWavePoints (singleWaveWidth ws) 0) transitions 
                    |> Array.unzip
                //printfn "points"
                let makePolyline points = 
                    let points =
                        points
                        |> Array.concat
                        |> Array.distinct
                    polyline (wavePolylineStyle points) []

                let valuesSVG = displayValuesOnWave ws wave.WaveValues.Step transitions
                //printfn "values"
                svg (waveRowProps ws)
                    (List.append [makePolyline fstPoints; makePolyline sndPoints] valuesSVG)
                //|> (fun x -> printfn "makepolyline"; x)
        //printfn "end generate"
        {wave with 
            Radix = ws.Radix
            ShownCycles = ws.ShownCycles
            StartCycle = ws.StartCycle
            CycleWidth = singleWaveWidth ws
            SVG = Some waveform}
    



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

/// If zoomIn, then increase width of clock cycles (i.e.reduce number of visible cycles).
/// otherwise reduce width. GenerateWaveforms message will reconstitute SVGs after the change.
let changeZoom (wsModel: WaveSimModel) (zoomIn: bool) (dispatch: Msg -> unit) =
    let start = TimeHelpers.getTimeMs ()
    let shownCycles =
        let wantedCycles = int (float wsModel.ShownCycles / Constants.zoomChangeFactor)
        if zoomIn then
            // try to reduce number of cycles displayed
            wantedCycles
            // If number of cycles after casting to int does not change
            |> (fun nc -> if nc = wsModel.ShownCycles then nc - 1 else nc )
            // Require a minimum of cycles
            |> (fun nc -> 
                    let minVis = min wsModel.ShownCycles Constants.minVisibleCycles
                    max nc minVis)
        else
            let wantedCycles = int (float wsModel.ShownCycles * Constants.zoomChangeFactor)
            // try to increase number of cycles displayed
            wantedCycles
            // If number of cycles after casting to int does not change
            |> (fun nc -> if nc = wsModel.ShownCycles then nc + 1 else nc )
            |> (fun nc -> 
                let maxNc = int (wsModel.WaveformColumnWidth / float Constants.minCycleWidth)
                max wsModel.ShownCycles (min nc maxNc))
    let startCycle =
        // preferred start cycle to keep centre of screen ok
        let sc = (wsModel.StartCycle - (shownCycles - wsModel.ShownCycles)/2)
        let cOffset = wsModel.CurrClkCycle - sc
        sc
        // try to keep cursor on screen
        |> (fun sc -> 
            if cOffset > shownCycles - 1 then
                sc + cOffset - shownCycles + 1
            elif cOffset < 0 then
                (sc + cOffset)
            else
                sc)
        // final limits check so no cycle is outside allowed range
        |> max 0
        |> min (Constants.maxLastClk - shownCycles)
        
    dispatch <| GenerateWaveforms { wsModel with ShownCycles = shownCycles; StartCycle = startCycle }
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
                                |> List.filter (function | {Type=IOLabel;Label = lab'} when lab' = lab -> true |_ -> false)
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
    |> List.map (fun wave -> getWaveValue wsModel.CurrClkCycle wave wave.Width)
    |> List.map (fun fd ->
        match fd.Width, fd.Dat with
        | 1, Word b -> $" {b}" 
        | _ -> fastDataToPaddedString WaveSimHelpers.Constants.valueColumnMaxChars wsModel.Radix fd)
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
            |> function 
                | [a;b] -> [str "Key: Memory location is " ; a; str ", or " ;b; str ". Click waveforms or use cursor control to change current cycle."] 
                | _ -> failwithf "What? Can't happen!"
        List.map (fun ram -> td [Style [BorderColor "white"]] [ramTable wsModel ram])  selectedRams
        |> (fun tables -> [tbody [] [tr [] [th [ColSpan selectedRams.Length] [inlineStyle [] headerRow]]; tr [Style [Border "10px"]] tables]])
        |> Fulma.Table.table [Table.TableOption.Props ramTablesLevelProps; Table.IsFullWidth; Table.IsBordered; Table.Props [Style [Height "100%"]]]
    else div [] []
    |> TimeHelpers.instrumentInterval "ramTables" start

/// This function regenerates all the waveforms listed on wavesToBeMade.
/// Generation is subject to timeout, so may not complete.
/// Returns tuple: 
/// allWaves (with new waveforms); 
/// numberDone (no of waveforms made);
/// timeToDo; Some (time actually taken) (> timeout) or None if complete with no timeOut.
let makeWaveformsWithTimeOut
        (timeOut: float option) 
        (ws: WaveSimModel)
        (allWaves: Map<WaveIndexT,Wave>) 
        (wavesToBeMade: WaveIndexT list) =
    let start = TimeHelpers.getTimeMs()
    let allWaves, numberDone, timeToDo =
        ((allWaves, 0, None), wavesToBeMade)
        ||> List.fold (fun (all,n, _) wi ->
                match timeOut, TimeHelpers.getTimeMs() - start with
                | Some timeOut, timeSoFar when timeOut < timeSoFar ->
                    all, n, Some timeSoFar
                | _ -> 
                    (Map.change wi (Option.map (generateWaveform ws wi)) all), n+1, None)
//    printfn $"Making {numberDone} waveforms from {wavesToBeMade.Length}."
    allWaves, numberDone, timeToDo

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
        let lastCycleNeeded = wsModel.StartCycle + wsModel.ShownCycles + 1

        FastRun.runFastSimulation (Some Constants.initSimulationTime) lastCycleNeeded fs
        |> (fun speedOpt ->
            let cyclesToDo = lastCycleNeeded - wsModel.FastSim.ClockTick
            match speedOpt with
            | Some speed when  float cyclesToDo / speed + Constants.initSimulationTime > Constants.maxSimulationTimeWithoutSpinner  &&
                               Option.isNone model.Spinner ->
                // long simulation, set spinner on and dispatch another refresh 
                let spinnerFunc = fun model -> fst (refreshWaveSim newSimulation wsModel model)
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

                printfn $"Ending refresh now at Tick {fs.ClockTick}..."
                let allWavesStart = TimeHelpers.getTimeMs ()    
                    //printfn "starting getwaves"
                // redo waves based on new simulation
                let allWaves = 
                    if newSimulation then
                        //printfn "making new waves..."
                        getWaves wsModel fs 
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
                    
                printfn $"Similationuptodate: {simulationIsUptodate}"
                // need to use isSameWave here becasue sarray index may have changed
                let wavesToBeMade =
                    allWaves
                    |> Map.filter (fun wi wave ->
                        // Only generate waveforms for selected waves.
                        // Regenerate waveforms whenever they have changed
                        let hasChanged = not <| waveformIsUptodate wsModel wave
                        //if List.contains index ws.SelectedWaves then 
                        List.exists (fun wi' -> isSameWave wi wi') wsModel.SelectedWaves && hasChanged && simulationIsUptodate)
                    |> Map.toList                   
                    |> List.map fst

                let model, allWaves, spinnerPayload, numToDo =
                    //printfn $"{wavesToBeMade.Length} waves to make."
                    let numToDo = wavesToBeMade.Length
                    makeWaveformsWithTimeOut (Some Constants.initSimulationTime) wsModel allWaves wavesToBeMade
                    |> (fun (allWaves, numDone, timeOpt) ->
                            match wavesToBeMade.Length - numDone, timeOpt with
                            | n, None -> 
                                model, allWaves, None, n // finished
                            | _ when numDone = 0 -> 
                                failwithf "What? makewaveformsWithTimeOut must make at least one waveform"
                            | numToDo, Some t when 
                                    float wavesToBeMade.Length * t / float numDone < Constants.maxSimulationTimeWithoutSpinner ->
                                let (allWaves, numDone, timeOpt) = makeWaveformsWithTimeOut None wsModel allWaves wavesToBeMade
                                model, allWaves, None, numToDo - numDone
                            | numToDo, _ ->
                                let payload = Some ("Making waves", refreshWaveSim false {wsModel with AllWaves = allWaves} >> fst)
                                model,  allWaves, payload, numToDo)

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
                    }

                let model = 
                    match spinnerPayload with
                    | None -> cancelSpinner model
                    | Some sp -> 
                        updateSpinner (fst sp) (snd sp) numToDo model
                    |> updateWSModel (fun _ -> ws)
                model, Elmish.Cmd.none
                |> TimeHelpers.instrumentInterval "refreshWaveSim" start)
//}

/// Refresh the state of the wave simulator according to the model and canvas state.
/// Redo a new simulation. Set inputs to default values. Then call refreshWaveSim via RefreshWaveSim message.
/// 1st parameter ofrefreshWaveSin will be set true which causes all waves to be necessarily regenerated.
let refreshButtonAction canvasState model dispatch = fun _ ->
    let model = MemoryEditorView.updateAllMemoryComps model
    let wsSheet = 
        match model.WaveSimSheet with
        | None -> Option.get (getCurrFile model)
        | Some sheet -> sheet
    printfn $"Refresh Button with width = {model.WaveSimViewerWidth}"
    let model = 
        model
        |> removeAllSimulationsFromModel
        |> fun model -> {model with WaveSimSheet = Some wsSheet}
    let wsModel = getWSModel model
    //printfn $"simSheet={wsSheet}, wsModel sheet = {wsModel.TopSheet},{wsModel.FastSim.SimulatedTopSheet}, state={wsModel.State}"
    match SimulationView.simulateModel model.WaveSimSheet (WaveSimHelpers.Constants.maxLastClk + WaveSimHelpers.Constants.maxStepsOverflow)  canvasState model with
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
        Columns.columns [] [
            Column.column [Column.Props [Style [Height "200px"; OverflowY OverflowOptions.Clip]]] [
                Heading.h4 [] [ 
                    (div [Style [Display DisplayOptions.Inline; MarginRight "10px"]] [str title]) 
                    button 
                        (infoButtonProps IsInfo)
                        (fun _ -> PopupView.viewWaveInfoPopup dispatch)
                        (str Constants.infoSignUnicode)                        
                ]
                div [] 
                    (if model.WaveSimSheet <> None then 
                        [
                            str "View clocked logic waveforms by selecting waves. "
                            str "Select RAMs or ROMs to view contents during the simulation. "
                            str "View or change any sheet with the simulation running. "
                            str "After design changes use "
                            refreshSvg "black" "12px"
                            str " to update waveforms."
                        ] else
                        [
                            str "Use 'Start Simulation' button to simulate current sheet."
                            str "Drag the grey divider to change the Viewer width."
                        ])
                ]

            Column.column 
                [
                    Column.Option.Width (Screen.All, Column.IsNarrow)
                ] 
                [ 
                    let startOrRenew = refreshButtonAction canvasState model dispatch
                    let waveEnd = endButtonAction canvasState model dispatch
                    let wbo = getWaveSimButtonOptions canvasState model wsModel
                    let startEndButton =
                        button 
                            (topHalfButtonProps wbo.StartEndColor) 
                            (fun ev -> if wbo.IsRunning then waveEnd ev else startOrRenew ev)
                            (str wbo.StartEndMsg)
                    let needsRefresh = wbo.IsDirty && wbo.IsRunning
                    div 
                        [Style [MarginBottom "20px" ]]                      
                        (if not wbo.IsRunning then [
                            startEndButton
                         ] 
                        else [
                            startEndButton
                            button
                                (Button.Disabled (not needsRefresh) :: topHalfButtonProps IsSuccess)
                                startOrRenew
                                refreshButtonSvg
                        ])

                    Level.level [] [
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
                        Level.left [GenericOption.Props [Style [MarginLeft "5px"]]] [
                            zoomButtons wsModel dispatch
                        ]
                        Level.right [] [
                            radixButtons wsModel dispatch
                        ]
                    ]
                    clkCycleButtons wsModel dispatch
                ]
            ]
        hr [ Style [ MarginBottom "0px" ] ]
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
    div [] [
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
                        [ str "There is no clocked logic in this circuit. Add clocked logic to simulate waveforms." ]
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
        
    ]

