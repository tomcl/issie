/// The rows a RAM table shows at one clock, worked out from a simulation that holds the memory.
///
/// Below the UI because two simulators produce it: the renderer's own, and the .NET sidecar,
/// which has a `FastSimulation` of its own and answers `SimReadRam` from exactly this code. That
/// is the point of the module - the alternative is the sidecar reimplementing which row counts as
/// read and which as written, and two answers that agree until they do not.
///
/// `SimInterface` named the obstacle: reading a RAM "needs a row type that is declared in the
/// waveform UI and has to move first". This is that move. `WaveSimStyle` keeps the styling.
///
/// What is NOT here is display: the radix, the padding, the comment column, and the collapsing of
/// a run of zero locations into one "0x0100 ... 0x01FF" row. Those are the table's own business
/// and stay in `WaveSimRams`, which is why this returns locations and values rather than strings.
module RamView

open CommonTypes
open SimGraphTypes
open SimTypes

/// Whether the design read or overwrote this location at the clock being shown. A write lands one
/// clock after the address and write-enable that caused it; a read is the same clock for an
/// asynchronous memory and one after for a synchronous one.
type RamRowType =
    | RAMWritten
    | RAMRead
    | RAMNormal

type RamRow =
    { Addr: bigint
      Value: bigint
      Row: RamRowType }

/// What a held set of rows answers: the clock, how many locations were worth listing rather than
/// windowing, and where the window starts. Everything that changes the rows.
///
/// The request that goes out and the table that reads the result are built from the same key, so
/// a table cannot ask for one thing, be sent it, and then look for another.
type RamKey =
    { Cycle: int
      SparseUpTo: int
      Start: bigint }

/// Which of the two displays a table is showing, and its rows.
type RamView =
    /// every location that is non-zero at this clock, in address order - there were few enough
    /// to list, so a gap between two of them means every location in it is zero
    | RamSparse of RamRow list
    /// a window of consecutive locations from `Start`, zeros included, because there were too
    /// many to list
    | RamWindow of start: bigint * rows: RamRow list

/// The address the design read at this clock, and the one it overwrote, if any.
///
/// A write is visible one clock after the address and WEN that caused it, so it is looked up at
/// the step before; a read is at this step for an asynchronous memory and the step before for a
/// synchronous one, since a synchronous read presents its data a clock late.
///
/// **Two different numbers, and they were the same one.** Which clock this is - and so whether
/// there IS a clock before it - is a question about the clock number; where that clock's port
/// values LIE is an index into the circular step arrays, which `stepIndexOf` wraps. They agree
/// only until the arrays wrap, which the waveform simulator never does and the step simulator does
/// on its 551st cycle: past that, `cycle - 1` used as an index read another port's region of the
/// shared slab, and past the end of the slab it threw - on the sidecar, inside the serve loop,
/// which drops the connection and takes every request in flight with it.
///
/// A cycle the arrays have run past is shown WITHOUT markers rather than refused. What the memory
/// held then is still known - `RamStore` keeps its writes by absolute clock, not in the step
/// arrays - so the only thing lost is which location the ports named, and a table of contents with
/// nothing highlighted is a better answer than no table.
let private readAndWritten (fs: FastSimulation) (fc: FastComponent) (cycle: int) =
    if cycle < 0 || fs.ClockTick - cycle >= fs.MaxArraySize then
        None, None
    else

    let step = stepIndexOf fs.MaxArraySize cycle
    let addressAt s = FastExtract.getFastComponentInput fc 0 s

    let readAt =
        match fc.FType with
        | AsyncROM1 _
        | AsyncRAM1 _ -> step.SimStep
        | ROM1 _
        | RAM1 _ -> step.SimStepOld
        | _ -> failwithf $"What? {fc.FullName} should be a memory component"

    let read =
        match cycle, fc.FType with
        | 0, ROM1 _
        | 0, RAM1 _ -> None
        | _ -> Some(addressAt readAt)

    let written =
        match cycle, fc.FType with
        | _, ROM1 _
        | _, AsyncROM1 _
        | 0, _ -> None
        | _, RAM1 _
        | _, AsyncRAM1 _ when FastExtract.getFastComponentInput fc 2 step.SimStepOld = 1I ->
            Some(addressAt step.SimStepOld)
        | _ -> None

    read, written

/// Mark the read and written locations, adding a row for either if the display does not already
/// have one - a location read or written is worth a row whatever it holds.
let private markReadWrite (fs: FastSimulation) (fc: FastComponent) (cycle: int) (rows: (bigint * bigint) list) =
    let read, written = readAndWritten fs fc cycle

    let marked =
        (rows |> List.map (fun (a, v) -> a, (v, RAMNormal)) |> Map.ofList, [ read, RAMRead; written, RAMWritten ])
        ||> List.fold (fun acc (addr, rowType) ->
            match addr with
            | None -> acc
            | Some a ->
                // a write overwrites a read mark on the same location, which is why written is
                // folded second
                match Map.tryFind a acc with
                | Some(v, _) -> Map.add a (v, rowType) acc
                | None -> Map.add a (0I, rowType) acc)

    marked
    |> Map.toList
    |> List.map (fun (a, (v, r)) -> { Addr = a; Value = v; Row = r })

/// The store a memory keeps its contents in. A ROM has none - its contents are part of its type
/// and never change - so it gets a read-only one built here.
let private storeOf (fs: FastSimulation) (fid: FComponentId) (fc: FastComponent) (cycle: int) =
    match fc.FType with
    | ROM1 mem
    | AsyncROM1 mem -> Ok(RamStore.fixedOf mem)
    | RAM1 _
    | AsyncRAM1 _ ->
        match FastExtract.extractFastSimulationState fs cycle fid with
        | RamState ram -> Ok ram
        | other -> Error $"unexpected state {other} in a memory at cycle {cycle}"
    | other -> Error $"component is not a memory but a {other}"

/// The rows a RAM table shows at one clock.
///
/// `sparseUpTo` is the most non-zero locations worth listing: below it every one of them is
/// returned, above it a window of `rows` locations from `start` instead. A caller that wants a
/// window whatever the memory holds asks for `sparseUpTo = 0`.
///
/// Which of the two comes back is the implementation's to decide, not the caller's, because only
/// it knows how much the memory holds - and knowing that is the walk being decided about.
/// `RamStore.sparseUpTo` is bounded whatever the answer; see docs/dev/ramOverTheWire.md.
let ofFastSim
    (fs: FastSimulation)
    (fid: FComponentId)
    (cycle: int)
    (sparseUpTo: int)
    (start: bigint)
    (rows: int)
    : Result<RamView, string> =
    match fs.ComponentOf fid with
    | None -> Error "no such component in this simulation"
    | Some fc ->
        storeOf fs fid fc cycle
        |> Result.map (fun ram ->
            let lastLocation = (1I <<< ram.AddressWidth) - 1I

            match RamStore.sparseUpTo ram cycle sparseUpTo with
            | Some locations -> RamSparse(markReadWrite fs fc cycle locations)
            | None ->
                let window =
                    [ start .. min lastLocation (start + bigint rows - 1I) ]
                    |> List.map (fun addr -> addr, RamStore.wordAt ram cycle addr)

                RamWindow(start, markReadWrite fs fc cycle window))
