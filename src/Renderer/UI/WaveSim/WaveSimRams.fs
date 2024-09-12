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
open WaveSimStyle
open SimGraphTypes
open SimTypes
open Optics
open Optics.Operators


/// Table row that shows the address and data of a RAM component.
let ramTableRow ((addr, data,rowType): string * string * RamRowType): ReactElement =

    tr [ Style <| ramTableRowStyle rowType ] [
        td [] [ str addr ]
        td [] [ str data ]
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
            printf "Extending Fast Simulation to cycle %d\n in ramTable" step
        //FastRun.runFastSimulation None step fs |> ignore // not sure why this is needed
        // in some cases fast sim is run for one cycle less than currClockCycle
        let memData =
            match fc.FType with
            | ROM1 mem
            | AsyncROM1 mem -> mem
            | RAM1 mem
            | AsyncRAM1 mem -> 
                match FastExtract.extractFastSimulationState fs wsModel.CursorExactClkCycle ramId with
                |RamState mem -> mem
                | x -> printf $"What? Unexpected state from cycle {wsModel.CursorExactClkCycle} \
                        in RAM component '{ramLabel}'. FastSim step = {fs.ClockTick}"
                       printfn $"State is {x}"
                       failwithf "unexpected Error in ramTable - see printed message"
            | _ ->
                failwithf $"Given a component {fc.FType} which is not a vaild RAM"
        let aWidth,dWidth = memData.AddressWidth,memData.WordWidth

        let print w (a:bigint) = NumberHelpers.valToPaddedString w wsModel.Radix (((1I <<< w) - 1I) &&& a)

        let lastLocation = (1I <<< memData.AddressWidth) - 1I

        let opticPath fc = waveSimModel_ >-> ramStartLocation_ >-> Optics.Map.valueWithDefault_ ("",0I) fc
        let loc = {
            TextOptic_ = opticPath ramId >-> Optics.fst_
            ValOptic_ = opticPath ramId >-> Optics.snd_
            }

        let startDisplayLoc, windowedDisplay =
            match Optic.get loc.ValOptic_ model,  Optic.get loc.TextOptic_ model with
            | start, _ when memData.Data.Count > Constants.maxRamLocsWithSparseDisplay -> start, true
            | _, text when text = "" -> 0I, false
            | start, _ -> start, true

        let maxHeight =
            max (screenHeight() - (min wanted (screenHeight()/2.)) - 300.) 30.
            |> (fun h -> h - 40.)

        /// print a single 0 location as one table row
        let print1 (a:bigint,b:bigint,rw:RamRowType) = $"{print aWidth a}",$"{print dWidth b}",rw

        /// print a range of zero locations as one table row
        let print2 (a1:bigint) (a2:bigint) (d:bigint) = $"{print aWidth (a1+1I)} ... {print aWidth (a2-1I)}", $"{print dWidth d}",RAMNormal

        /// output info for one table row filling the given zero memory gap or arbitrary size, or no line if there is no gap.
        let printGap (gStart:bigint) (gEnd:bigint) =
            let gapSize = gEnd - gStart
            if gapSize = 1I || windowedDisplay then []            
            elif gapSize = 2I then  [print1 ((gEnd + gStart) / 2I, 0I, RAMNormal)]
            elif  gapSize > 2I then [print2 gStart gEnd 0I]
            else
                failwithf $"What? gEnd={gEnd},gStart={gStart}: negative or zero gaps are impossible..."



        /// Add a RAMNormal RamRowType value to every location in mem.
        /// Add in additional locations for read and/or write if needed.
        /// Set RamRowValue type to RAMWritten or RAMRead for thse locations.
        /// Write is always 1 cycle after WEN=1 and address.
        /// Read is 1 (0) cycles after address for sync (asynch) memories.
        let addReadWrite (fc:FastComponent) (step:int) (mem: Map<bigint,bigint>) =


            let readStep =
                match fc.FType with
                | AsyncROM1 _ | AsyncRAM1 _ -> step
                | ROM1 _ | RAM1 _ -> step - 1
                | _ -> failwithf $"What? {fc.FullName} should be a memory component"

            let addrSteps step = getFastComponentInput fc 0 step

            let readOpt =
                match step, fc.FType with
                | 0,ROM1 _ | 0, RAM1 _ -> None
                | _ -> 
                    addrSteps readStep
                    |> Some
            let writeOpt =
                match step, fc.FType with
                | _, ROM1 _ 
                | _, AsyncROM1 _
                | 0, _ -> None
                | _, RAM1 _ | _, AsyncRAM1 _ when getFastComponentInput fc 2 (step-1) = 1I -> 
                    addrSteps (step-1)
                    |> Some
                | _ ->  
                    None

            /// Mark addr in memory map as being rType
            /// if addr does not exist - create it
            let addToMap rType addr mem:Map<bigint,bigint*RamRowType> =
                match Map.tryFind addr mem with
                | Some (d,_) -> Map.add addr (d,rType) mem
                | None  ->  Map.add addr (0I,rType) mem
    

            Map.map (fun k v -> v,RAMNormal) mem
            |> (fun mem ->
                match readOpt with
                | Some addr -> addToMap RAMRead addr mem
                | None -> mem
                |> (fun mem ->
                    match writeOpt with // overwrite RAMRead here is need be
                    | Some addr -> addToMap RAMWritten addr mem
                    | None -> mem))


        /// If using a windowed (not-sparse) display, prune the memory map to the given range adding zeros for missing locations.
        let generatewindowlocations (startLoc:bigint) (numOfLocs:int) (mem:Map<bigint,bigint>) =
            Array.map (fun loc -> loc, (Map.tryFind loc mem |> Option.defaultValue 0I)) [| startLoc..startLoc + bigint numOfLocs-1I |]
            |> Map.ofArray
             


                

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
            memData.Data
            |> (if windowedDisplay then
                    generatewindowlocations startDisplayLoc Constants.maxRamRowsDisplayed
                    >> addReadWrite fc step
                    >> Map.toList
                    >> List.map (fun (a,(d,rw)) -> a,d,rw)
                    >> List.sort
                    >> List.sortBy (fun (start,_,_) -> if  isInWindow start then 0 else 1) // put read and write at bottom if outside window
                    >> List.map print1
                else
                    addReadWrite fc step
                    >> Map.toList
                    >> List.map (fun (a,(d,rw)) -> a,d,rw)
                    >> List.filter (fun (a,d,rw) -> d<>0I || rw <> RAMNormal)
                    >> List.sort
                    >> addEndPoints
                    >> addGapLines true)
            

        

        let goodStartAddress big =
            if big >= 0I && big < lastLocation then  
                ""
            else
                $"Address ${big} is out of required range: 0 - {lastLocation}"
            
 
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
                    ]
                ]
                tbody [] (List.map (fun item -> ramTableRow item) lineItems)
                                   
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
                            printfn $"Unexpected failure in ramTables: x = {x}"
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
        | e -> printfn "Error in ramTables display: %A" e.Message
               div [] []
    |> TimeHelpers.instrumentInterval "ramTables" start


