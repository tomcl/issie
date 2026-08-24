/// RAM display in waveform simulator
module WaveSimRams

//---------------------------------------------------------------------------------------//
//-------Functions to implement the RAM display in the waveform simulator----------------//
//---------------------------------------------------------------------------------------//

open Fulma
open Fable.React
open Fable.React.Props

open CommonTypes
open ModelType
open ModelHelpers
open FastExtract
open WaveSimTypes
open WaveSimStyle
open SimGraphTypes
open SimTypes
open RamView
open Optics
open Optics.Operators


/// Table row that shows the address and data of a RAM component, and what the .ram file it was
/// initialised from had to say about that location. The comment column is there only when the
/// memory has comments, so that every row of a table has the same cells.
let ramTableRow (hasComments: bool) ((addr, data, comment, rowType): string * string * string * RamRowType): ReactElement =

    tr [ Style <| ramTableRowStyle rowType ] [
        td [] [ str addr ]
        td [] [ str data ]
        if hasComments then td [Style [Color "grey"]] [ str comment ]
    ]

/// Table showing contents of a RAM component.
let ramTable (dispatch: Msg -> unit) (wsModel: WaveSimModel) (model: Model) ((ramId, ramLabel): FComponentId * string) : ReactElement =
    let wanted = calcWaveformAndScrollBarHeight wsModel
    let fs = Simulator.getFastSim()
    match Map.tryFind ramId fs.FComps with
    | None -> div [] []
    | Some fc -> 
        let step = wsModel.CursorExactClkCycle
        if fs.ClockTick < step then
            Log.dbg Log.Wave $"extending the fast simulation to cycle {step} for the RAM table"
        //FastRun.runFastSimulation None step fs |> ignore // not sure why this is needed
        // in some cases fast sim is run for one cycle less than currClockCycle
        /// The memory as the component declares it. Its widths and its .ram-file comments are
        /// facts about the component's TYPE, so they are read here in both modes - the renderer
        /// builds a simulation for structure whichever simulator is running. Only the contents
        /// come from the simulator that has them.
        let mem =
            match fc.FType with
            | ROM1 m
            | AsyncROM1 m
            | RAM1 m
            | AsyncRAM1 m -> m
            | _ -> failwithf $"Given a component {fc.FType} which is not a vaild RAM"

        let aWidth,dWidth = mem.AddressWidth,mem.WordWidth

        let print w (a:bigint) = NumberHelpers.valToPaddedString w wsModel.Radix (((1I <<< w) - 1I) &&& a)

        let lastLocation = (1I <<< aWidth) - 1I

        let opticPath fc = waveSimModel_ >-> ramStartLocation_ >-> Optics.Map.valueWithDefault_ ("",0I) fc
        let loc = {
            TextOptic_ = opticPath ramId >-> Optics.fst_
            ValOptic_ = opticPath ramId >-> Optics.snd_
            }

        /// What this table is asking for. RamData computes it for the fetch too, so the request
        /// that goes out and the cache this reads cannot be of different questions.
        let key = RamData.keyOf model ramId

        /// The rows, from whichever simulator holds the memory. `None` from the remote one means
        /// the reply for this cycle has not landed yet - the request goes out from the update
        /// function, and this render draws what it has.
        let view =
            if model.SimulateInRenderer then
                RamView.ofFastSim fs ramId step key.SparseUpTo key.Start Constants.maxRamRowsDisplayed
                |> function
                    | Ok v -> Some v
                    | Error e ->
                        Log.warn $"reading RAM '{ramLabel}' at cycle {step}: {e}"
                        None
            else
                RamData.held model ramId

        let startDisplayLoc, windowedDisplay, viewRows =
            match view with
            | Some(RamSparse rows) -> 0I, false, rows
            | Some(RamWindow(start, rows)) -> start, true, rows
            | None -> key.Start, true, []

        let maxHeight =
            max (screenHeight() - (min wanted (screenHeight()/2.)) - 300.) 30.
            |> (fun h -> h - 40.)

        /// Comments written against locations in the .ram file this memory came from.
        let comments = Option.defaultValue Map.empty mem.Comments
        let hasComments = not (Map.isEmpty comments)

        /// print a single 0 location as one table row
        let print1 (a:bigint,b:bigint,rw:RamRowType) =
            $"{print aWidth a}", $"{print dWidth b}", (Option.defaultValue "" (Map.tryFind a comments)), rw

        /// print a range of zero locations as one table row. A range covers many locations, so no
        /// one comment belongs against it.
        let print2 (a1:bigint) (a2:bigint) (d:bigint) =
            $"{print aWidth (a1+1I)} ... {print aWidth (a2-1I)}", $"{print dWidth d}", "", RAMNormal

        /// output info for one table row filling the given zero memory gap or arbitrary size, or no line if there is no gap.
        let printGap (gStart:bigint) (gEnd:bigint) =
            let gapSize = gEnd - gStart
            if gapSize = 1I || windowedDisplay then []            
            elif gapSize = 2I then  [print1 ((gEnd + gStart) / 2I, 0I, RAMNormal)]
            elif  gapSize > 2I then [print2 gStart gEnd 0I]
            else
                failwithf $"What? gEnd={gEnd},gStart={gStart}: negative or zero gaps are impossible..."



        /// add fake locations beyond normal address range so that
        /// addGapLines fills these (if need be). These locations are then removed
        let addEndPoints (items:(bigint*bigint*RamRowType) list)  =
            let start = 0I
            let ad (a,d,rw) = a
            match items.Length with
            | 0 -> [-1I, 0I ,RAMNormal;  lastLocation, 0I, RAMNormal]
            | _ ->
                if ad items[0] < start then items else List.insertAt 0 (start - 1I, start - 1I, RAMNormal) items
                |> (fun items ->
                    if ad items[items.Length-1] = lastLocation || windowedDisplay then 
                        items else 
                    List.insertAt items.Length (lastLocation+1I,0I,RAMNormal) items)

        /// Transform RAM info into strings to print in a table, adding extra lines for zero gaps if the display is sparse.
        /// line styling is controlled by a RamRowtype value and added later when the table row react is generated
        let addGapLines (addGaps: bool) (items: (bigint*bigint*RamRowType) list) =
            List.pairwise items
            |> List.collect (fun ((gStart,_,_),(gEnd,dEnd,rwe)) -> 
                let thisItem = if gEnd = lastLocation + 1I  then [] else [print1 (gEnd,dEnd,rwe)]
                [
                    if addGaps then printGap gStart gEnd else []
                    thisItem
                ] )
            |> List.concat

            
        let lineItems =
            let isInWindow loc = loc >= startDisplayLoc && loc < startDisplayLoc + bigint Constants.maxRamRowsDisplayed
            let triples = viewRows |> List.map (fun r -> r.Addr, r.Value, r.Row)
            if windowedDisplay then
                triples
                |> List.sort
                |> List.sortBy (fun (start,_,_) -> if  isInWindow start then 0 else 1) // put read and write at bottom if outside window
                |> List.map print1
            else
                triples
                |> List.filter (fun (a,d,rw) -> d<>0I || rw <> RAMNormal)
                |> List.sort
                |> addEndPoints
                |> addGapLines true
            

        

        /// The window may start anywhere the memory has, which includes its last location - a window
        /// showing one row is a window. The bound used to be strict, so the range refused was one
        /// short of the range the message quoted.
        let goodStartAddress big =
            if big >= 0I && big <= lastLocation then
                ""
            else
                $"Address {big} is out of required range: 0 - {lastLocation}"
            
 
        let inputBox =
            let props: IHTMLProp list = [Style [Width 200]; AutoFocus false]
            ModelHelpers.inputBigint props "Window Start"  loc (fun big _ -> goodStartAddress big = "") dispatch model

        Level.item [
            Level.Item.Option.Props ramTableLevelProps
            Level.Item.Option.HasTextCentered
        ] [
            Heading.h6 [
                Heading.Option.Props [ centerAlignStyle ]
            ] [str ramLabel ; br [];  inputBox]
            div [Style [MaxHeight maxHeight;OverflowY OverflowOptions.Auto]] [
            Table.table [
                Table.IsFullWidth
                Table.IsBordered
            ] [ thead [] [
                    tr [] [
                        th [ centerAlignStyle ] [ str "Address"]
                        th [ centerAlignStyle ] [ str "Data"; sub [Style [MarginLeft "2px"; FontSize "10px"]] [str (string wsModel.CursorExactClkCycle)]]
                        if hasComments then th [ centerAlignStyle ] [ str "Comment"]
                    ]
                ]
                tbody [] (List.map (fun item -> ramTableRow hasComments item) lineItems)
                                   
            ] ]
            br []
        ]

/// Bulma Level component of tables showing RAM contents.
let ramTables (dispatch: Msg -> unit) (wsModel: WaveSimModel) (model: Model): ReactElement =
    let start = TimeHelpers.getTimeMs ()
    try
        let inlineStyle (styles:CSSProp list) = div [Style (Display DisplayOptions.Inline :: styles)]
        
        let selectedRams = Map.toList wsModel.SelectedRams
        if List.length selectedRams > 0 then
            let tables = 
                let headerRow =
                    ["read", RAMRead; "overwritten",RAMWritten]
                    |> List.map (fun (op, opStyle) -> inlineStyle [Margin "0px"] [inlineStyle (ramTableRowStyle  opStyle) [str op]])
                    |> function 
                        | [a;b] -> [str "Key: Memory location is " ; a; str ", or " ;b; str ". Click waveforms or use cursor control to change current cycle."] 
                        | x ->
                            Log.warn $"unexpected header row in ramTables: {x}"
                            failwithf "What? Can't happen!"
                List.map (fun ram -> td [Style [BorderColor "white"]] [ramTable dispatch wsModel model ram])  selectedRams
                |> (fun tables -> [tbody [] [tr [] [th [ColSpan selectedRams.Length] [inlineStyle [] headerRow]]; tr [Style [Border "10px"]] tables]])
                |> Fulma.Table.table [
                    Table.TableOption.Props ramTablesLevelProps;
                    Table.IsFullWidth;
                    Table.IsBordered;
                    ]
            div [HTMLAttr.Id "TablesDiv"] [ hr [ Style [ Margin "5px"]]; br [ Style [ Margin "0px"]]; tables]
        else div [] []
    with
        // An error here is probably because the view code is displaying RAMs before simulation had finished.
        // It is not fatal, and does no harm to the simulation. This error boundary ignores the error printing
        // a message to the console, and displaying a blank div.
        | e -> Log.dbg Log.Wave $"RAM table drawn before the simulation finished: {e.Message}"
               div [] []
    |> TimeHelpers.instrumentInterval "ramTables" start


