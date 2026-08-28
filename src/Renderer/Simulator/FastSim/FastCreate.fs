module FastCreate
open EEExtensions
open CommonTypes
open TimeHelpers
open SimGraphTypes
open SimTypes
open SynchronousUtils
open NumberHelpers
open Helpers

//------------------------------------------------------------------------------//
//------------Functions To Create Fast Simulation Data Structures---------------//
//------------------------------------------------------------------------------//

//-----------------------------Fast Simulation Creation-------------------------//

let inline assertThat cond msg =
    if not cond then
        failwithf "what? assert failed: %s" msg

let emptyFastSimulation diagramName =

    { ClockTick = 0
      StepCost = { TypedArrayBytes = 0; HeapBytes = 0 }
      MaxArraySize = 0 // must be larger than max number of wavesim clocks
      FGlobalInputComps = Array.empty
      FConstantComps = Array.empty
      FClockedComps = Array.empty
      FOrderedComps = Array.empty
      FIOActive = Map.empty
      FIOLinks = []
      FCompsByIndex = Array.empty
      FIndexOf = Map.empty
      FCustomOutputCompLookup = Map.empty
      NumStepArrays = 0 // this will be overwritten by createInitFastCompPhase
      Drivers = Array.empty
      WaveIndex = Array.empty
      Design = { emptySimulatedDesign with DesignTopSheet = diagramName }
}

let simulationPlaceholder = emptyFastSimulation ""
let getFid (cid: ComponentId) (ap: ComponentId list) =
    let ff (ComponentId Id) = Id
    (cid, ap)

let getPortNumbers (sc: SimulationComponent) =
    let ins, outs =
        match sc.Type with
        | Constant1 _
        | Constant _
        | CounterNoEnableLoad _ -> 0, 1
        | Input1 _
        | Output _
        | Viewer _
        | BusSelection _
        | BusCompare _
        | BusCompare1 _
        | Not
        | DFF
        | Register _
        | IOLabel
        | ROM1 _
        | AsyncROM1 _
        | NbitsNot _
        | NbitSpreader _
        | CounterNoLoad _ -> 1, 1
        | NotConnected -> 1, 0
        | MergeWires
        | NbitsXor _
        | NbitsOr _
        | NbitsAnd _
        | RegisterE _
        | DFFE
        | CounterNoEnable _ -> 2, 1
        | SplitWire _ -> 1, 2
        | Mux2
        | NbitsAdderNoCout _
        | Counter _ -> 3, 1
        | Mux4 -> 5, 1
        | Mux8 -> 9, 1
        | NbitsAdder _ -> 3, 2
        | NbitsAdderNoCin _ -> 2, 2
        | NbitsAdderNoCinCout _
        | Shift _ -> 2, 1
        | AsyncRAM1 _
        | RAM1 _ -> 3, 1
        | Decode4 -> 2, 4
        | Demux2 -> 2, 2
        | Demux4 -> 2, 4
        | Demux8 -> 2, 8
        // | And
        // | Or
        // | Xor
        // | Nand
        // | Nor
        // | Xnor -> 2, 1
        | GateN (_, n) -> n, 1
        | MergeN n -> n, 1
        | SplitN (n, _, _) -> 1, n
        | Custom ct -> ct.InputLabels.Length, ct.OutputLabels.Length
        | AsyncROM _
        | RAM _
        | ROM _ -> failwithf "legacy component type is not supported"
        | Input _ -> failwithf "Legacy Input component types should never occur"

    ins, outs

let compType t =
    match t with
    | Custom c -> c.Name
    | _ -> t.ToString()

let findBigIntState (fc: FastComponent) =
    match fc.FType with
    // 1-bit components
    | Not
    | GateN _
    | DFF
    | DFFE -> false, None
    // N-bits components
    | Constant(w, _)
    | Constant1(w, _, _)
    | Input w
    | Input1(w, _)
    | Output w
    | Viewer w
    | NbitsAnd w
    | NbitsOr w
    | NbitsNot w
    | NbitsAdder w
    | NbitsAdderNoCin w
    | NbitsAdderNoCout w
    | NbitsAdderNoCinCout w
    | NbitSpreader w
    | NbitsXor(w, _)
    | Register w
    | RegisterE w
    | Counter w
    | CounterNoLoad w
    | CounterNoEnable w
    | CounterNoEnableLoad w
    | BusCompare(w, _)
    | BusCompare1(w, _, _) -> w > 32, None
    // Components with implicit width
    | IOLabel
    | Mux2
    | Mux4
    | Mux8
    | Demux2
    | Demux4
    | Demux8 -> fc.OutputWidth 0 > 32, None
    | NotConnected -> false, None
    // Components with variable width
    | MergeWires ->
        fc.InputWidth 0 > 32
        || fc.InputWidth 1 > 32
        || fc.OutputWidth 0 > 32,
        Some
            { InputIsBigInt = [| fc.InputWidth 0 > 32; fc.InputWidth 1 > 32 |]
              OutputIsBigInt = [| fc.OutputWidth 0 > 32 |] }
    | MergeN n -> 
        fc.OutputWidth 0 > 32
        || List.exists (fun n -> fc.InputWidth n > 32) [0..n-1],
        Some 
            { InputIsBigInt = Array.ofList(List.map (fun n -> fc.InputWidth n > 32) [0..n-1])
              OutputIsBigInt = [| fc.OutputWidth 0 > 32 |] }
    | SplitN (n, _, _) -> 
        fc.InputWidth 0 > 32
        || List.exists (fun n -> fc.OutputWidth n > 32) [0..n-1], 
        Some { 
            InputIsBigInt = [| fc.InputWidth 0 > 32 |] 
            OutputIsBigInt = Array.ofList(List.map (fun n -> fc.OutputWidth n > 32) [0..n-1])
                }
    | SplitWire _ ->
        fc.InputWidth 0 > 32,
        Some
            { InputIsBigInt = [| fc.InputWidth 0 > 32 |]
              OutputIsBigInt = [| fc.OutputWidth 0 > 32; fc.OutputWidth 1 > 32 |] }
    | BusSelection _ ->
        fc.InputWidth 0 > 32,
        Some
            { InputIsBigInt = [| fc.InputWidth 0 > 32 |]
              OutputIsBigInt = [| fc.OutputWidth 0 > 32 |] }
    | AsyncROM1 m
    | ROM1 m
    | RAM1 m
    | AsyncRAM1 m ->
        match m.WordWidth > 32, m.AddressWidth > 32 with
        | false, false -> false, None
        | false, true -> true, Some { InputIsBigInt = [| true |]; OutputIsBigInt = [| false |] }
        | true, false -> true, Some { InputIsBigInt = [| false |]; OutputIsBigInt = [| true |] }
        | true, true -> true, Some { InputIsBigInt = [| true |]; OutputIsBigInt = [| true |] }
    // Custom components
    | Custom c -> false, None // NOTE - custom components will not be reduced, so we don't need to worry about their width
    // Shift: the data bus may be wide; the shift amount input is at most 32 bits
    | Shift(width, shifterWidth, _) ->
        match width > 32 with
        | false -> false, None
        | true ->
            true,
            Some
                { InputIsBigInt = [| true; shifterWidth > 32 |]
                  OutputIsBigInt = [| true |] }
    // Legacy components
    | Decode4
    | AsyncROM _
    | ROM _
    | RAM _ -> failwith "Legacy components, not Implemented"

let mutable stepArrayIndex = -1

let makeStepArray (arr: 'T array) : StepArray<'T> =
    stepArrayIndex <- stepArrayIndex + 1
    { Step = arr; Index = stepArrayIndex }

let makeIOArray size =
    stepArrayIndex <- stepArrayIndex + 1
    { FDataStep = Array.create 2 (Data <| emptyFastData) // NOTE - 2 should be enough for FData arrays as they are only used in Truthtable
      UInt32Slab = Array.empty
      BigIntSlab = Array.empty
      StepBase = 0
      StepLength = 0
      Width = 0
      Index = stepArrayIndex }

(*
    What a simulation costs in memory, worked out before any of it is allocated.

    The two branches of makeIOArrayW below do not merely differ in size, they come out of different
    memory, with different limits, and a budget that added them together would be wrong about both:

      w <= 32   a Uint32Array. Four bytes a step, and one object for the garbage collector to
                trace however long it is. Bounded by the machine rather than by V8: allocation ran
                to 15.5GB on a 32GB machine, four times the heap limit in force.

      w > 32    a plain array of BigInt. A reference a step, and - once the simulation runs and
                each step is written with a value of its own - a separate BigInt object per step: a
                header plus a 64-bit digit per 64 bits of width. This is inside V8's 4GB pointer
                compression cage, which the model, the design and everything else the renderer
                holds must also fit inside, and which no flag lifts.

    So most designs, which are 32 bits and under, are limited by the machine, and a design with wide
    buses is limited by something much smaller and shared. Hence two budgets rather than one.
*)

/// One clock cycle of a bus of this width, in bytes of the memory it is stored in. See above, and
/// keep in step with makeIOArrayW immediately below: they describe the same allocation.
let stepBytesForWidth (w: int) =
    if w <= 32 then
        4 // one Uint32Array element
    else
        // the reference held in the array, then the BigInt it points at: object header, then one
        // 64-bit digit per 64 bits of the bus
        4 + 8 + 8 * ((w + 63) / 64)

/// What one clock cycle of this design will cost, read straight off the merged SimulationGraph -
/// before it is flattened, before a FastComponent exists, and allocating nothing itself.
///
/// One step array is allocated per output port of every component of every INSTANCE, so that is
/// what is counted: the graph is walked the way the flatten walks it, descending into each custom
/// component's own graph, so a sheet used ten times is charged ten times.
///
/// It is deliberately taken from the graph and not from the flattened design. The flatten now
/// creates the step arrays as it goes, so a cost worked out from its output would be worked out
/// after the memory it is meant to refuse had been taken - see checkSimulationFits below, whose
/// whole point is to come first. Reading the graph keeps the check where it belongs and, as a
/// bonus, is what lets the waveform simulator's configuration dialog price a design
/// (ModelHelpers.waveSimStepCost) without building any of it.
///
/// Custom components are counted, not skipped: their output arrays are allocated and then
/// replaced by links to the arrays inside them (linkFastCustomComponentsToDriverArrays), and the
/// replaced ones stay in the step-array arena for the simulation's whole life. So this count is
/// exactly what a built simulation occupies, the replaced quarter included, not an estimate of it.
///
/// The per-step State array is counted too. Only RAMs ever write it, but createFastComponent
/// allocates one for every component that could be synchronous - customs included - so on a
/// register-heavy design it is real memory, and the estimate that omitted it said a design was
/// smaller than it is.
/// The count comes back alongside the cost because both are wanted at the same moment, by the
/// same caller, from the same walk: the build sizes its component store from the count so that
/// the store never has to grow, and refuses the design outright on the cost.
let costAndSizeOfGraph (graph: SimulationGraph) : StepCost * int =
    let rec walk (acc: int * int * int) (graph: SimulationGraph) =
        (acc, graph)
        ||> Map.fold (fun (typed, heap, count) _ sComp ->
            let typed, heap =
                ((typed, heap), sComp.OutputWidths)
                ||> Array.fold (fun (typed, heap) w ->
                    if w <= 32 then typed + stepBytesForWidth w, heap
                    else typed, heap + stepBytesForWidth w)
            // a reference per step, pointing at NoState until a RAM writes it
            let typed, heap =
                if couldBeSynchronousComponent sComp.Type then typed, heap + 4 else typed, heap
            // descend exactly where the flatten descends: one expansion per custom INSTANCE
            match sComp.Type, sComp.CustomSimulationGraph with
            | Custom _, Some csg -> walk (typed, heap, count + 1) csg
            | _ -> typed, heap, count + 1)

    walk (0, 0, 0) graph
    |> fun (typed, heap, count) -> { TypedArrayBytes = typed; HeapBytes = heap }, count

let stepCostOfGraph (graph: SimulationGraph) : StepCost = fst (costAndSizeOfGraph graph)

/// The most clock cycles of a design costing this much that will be allowed, whichever of the two
/// budgets binds first. Used both to refuse a simulation and to say in the waveform simulator's
/// configuration what may be asked for, so that the two cannot disagree.
let maxCyclesFor (cost: StepCost) : int =
    let limit bytesPerStep budget =
        if bytesPerStep = 0 then infinity else budget / float bytesPerStep
    min
        (limit cost.TypedArrayBytes SimulationBudget.maxTypedArrayBytes)
        (limit cost.HeapBytes SimulationBudget.maxHeapBytes)
    // a design of a few narrow buses would otherwise be allowed more cycles than an int can hold
    |> min (float System.Int32.MaxValue)
    |> floor
    |> int

/// The largest WSConfig.LastClock a design costing this much can be configured to. maxCyclesFor
/// bounds the step ARRAYS, and the arrays carry a zoom margin past the last clock - up to
/// CommonTypes.waveSimMaxArrayMargin - so the two numbers differ by exactly that margin. Every
/// message that tells the user what may be ASKED for must quote this one: quoting the array
/// bound as a configuration value told the user to set a number that was itself refused.
let maxLastClockFor (cost: StepCost) : int =
    max 0 (maxCyclesFor cost - CommonTypes.waveSimMaxArrayMargin)

/// Refuse a simulation whose step arrays would not fit, before a byte of them is allocated.
///
/// Before rather than after, because the arrays ARE what exhausts memory: a check that had to build
/// them first would be the thing it is meant to prevent. Everything it needs is known by then - the
/// flattened design gives every width, and the caller has said how many cycles it wants - so the
/// answer is exact rather than a guess.
///
/// A Result and not an exception: this is a limit an ordinary user reaches by asking for a long
/// waveform simulation of a big design, so it travels the same path as any other simulation error
/// and is shown the same way, saying what would fit instead.
///
/// The line enforced here is the budget times SimulationBudget.runtimeHeadroom, not the budget:
/// this check is the crash guard of last resort, and the configuration dialog - which holds
/// users to the budgets exactly - is the advertised limit. The gap is for the simulation that
/// arrives without passing that dialog, a LastClock saved into a sheet on a larger machine above
/// all, which should run if it safely can rather than fail the moment Start is pressed. The
/// advice in the refusal still quotes the dialog's own bound, so following it always works.
let checkSimulationFits (arraySize: int) (cost: StepCost) : Result<unit, SimulationError> =
    let cycles = float arraySize

    let check (bytesPerStep: int) (budget: float) (ofWhat: string) =
        let needed = float bytesPerStep * cycles
        let enforced = budget * SimulationBudget.runtimeHeadroom
        if bytesPerStep = 0 || needed <= enforced then
            Ok()
        else
            Error
                { ErrType =
                    GenericSimError
                        $"This design needs {SimulationBudget.formatBytes (float bytesPerStep)} of {ofWhat} for every \
                          clock cycle, so the {arraySize} cycles of step storage this simulation asks for \
                          (its last clock cycle plus the zoom margin) would need \
                          {SimulationBudget.formatBytes needed} - more than the {SimulationBudget.formatBytes enforced} Issie will risk. \
                          Set the waveform simulator's last clock cycle, in its configuration, to at most \
                          {maxLastClockFor cost}, or simulate one subsheet rather than the whole design."
                  InDependency = None
                  ComponentsAffected = []
                  ConnectionsAffected = [] }

    check cost.TypedArrayBytes SimulationBudget.maxTypedArrayBytes "simulation memory"
    |> Result.bind (fun () ->
        // said separately because it is a different, much smaller, resource: a design of wide buses
        // can be refused while a design of the same size in 32-bit buses is allowed
        check cost.HeapBytes SimulationBudget.maxHeapBytes "heap memory, which buses wider than 32 bits need,")

(*
    The step-array arena.

    A large flattened design needs one step region per output port - half a million of them on
    the designs this was built for. Allocated as individual arrays, each was a separate
    allocation for the runtime to account, trigger collections over, and sweep; while a
    simulation is being built they are instead REGIONS of large shared slabs, handed out
    bump-pointer fashion and named by an integer base offset in the IOArray. The allocation
    count for a whole build falls from hundreds of thousands to dozens, under BOTH runtimes -
    this used to be Fable-only, done with Uint32Array views over ArrayBuffer slabs, and the
    explicit StepBase offset is what made the same packing expressible under .NET, where no
    zero-copy array view exists.

    Be honest about what this does and does not buy. A 480,000-component design at 2000 cycles
    (5.8GB of step arrays) used to end its build ELEVEN MINUTES in with the renderer at 10GB and
    no simulation to show for it; with the arena the same build completes. What it does not do is
    make that build quick - the time was measured afterwards to be almost entirely algorithmic,
    in phases that scale quadratically where gather stays linear (link, order and waves - see the
    perf-category phase table), and those are a separate fix.

    Two consequences to know about:
    - A slab is retained while ANY region of it is referenced, so a simulation's arrays free as
      one unit when the last reference goes - and one leaked IOArray pins its whole slab.
      Keeping ended simulations properly released (ModelHelpers.releaseWaveSimData and friends)
      is what makes this safe; it went in first.
    - The custom-component output regions that linking replaces occupy arena space for the
      simulation's whole life instead of becoming garbage, which is the same ~quarter that
      stepCostOfGraph has always charged for. What the budget counts, the arena keeps.

    The first slab is sized from that estimate and each one after it is twice the last, up to 64M
    uint32 steps (256MB) or 4M bigint steps. It grows rather than starting at the maximum because
    the maximum was ruinous for small builds - every simulation, however tiny, allocated and zeroed
    256MB - and it doubles rather than trusting the estimate because the estimate is LOW: a
    three-component build plans 200 words and asks for more. What the budget charges for and what
    the build allocates are close but not equal, which is worth knowing wherever the budget is used
    to decide what will fit. A single region bigger than a slab (a step count no budget would ever
    allow for the uint32 path) gets a dedicated exactly-sized slab instead of failing. Outside a build (stepArena None - odd one-off
    allocations, some tests) every region is its own exactly-sized array with StepBase 0, which
    is also what every IOArray looks like to code reading it: nothing downstream knows whether
    an arena was open.
*)

type private StepArena =
    { mutable U32Slab: uint32 array
      mutable U32Next: int
      /// how big to make the next uint32 slab: what this build was expected to need in total, then
      /// twice the last one each time that turns out not to be enough
      mutable U32Planned: int
      mutable BigSlab: bigint array
      mutable BigNext: int
      mutable BigPlanned: int }

/// The largest slab either store will take in one piece. A build needing more than this gets
/// several, which is what makes a huge design possible at all; a build needing less takes what it
/// needs, which is what makes a small one cheap.
let private u32SlabSize = 64 * 1024 * 1024
let private bigSlabSize = 4 * 1024 * 1024

/// How many slab elements a build of this cost and length will use, or the slab limit if it is
/// bigger than that. Computed in floating point because the byte count of a long simulation does
/// not fit in an int.
let private plannedElements (bytesPerStep: int) (steps: int) (elementBytes: int) (limit: int) =
    float bytesPerStep * float steps / float elementBytes
    |> min (float limit)
    |> int
    |> max 0

/// The arena the build in progress is drawing step regions from, or None outside a build.
/// Module-level for the same reason as stepArrayIndex just above: the allocation sites are
/// leaves of the build and threading an allocator through every layer would put plumbing in a
/// dozen signatures for the benefit of two call sites. Reset by every build.
let mutable private stepArena: StepArena option = None

/// Start drawing step regions from arena slabs, sized for what this build is about to need.
///
/// The size matters more than it looks. A slab used to be a fixed 256MB whatever was being built,
/// so EVERY simulation - a three-component test circuit as much as a CPU - allocated and zeroed
/// 256MB before it could hold its first cycle. Under .NET that is a large-object allocation per
/// build, and a test suite that builds a few thousand small simulations spent eight minutes of its
/// twelve doing nothing else. The cost of a build is now the cost of what it holds.
///
/// The numbers come from `stepCostOfGraph`, which is computed for the memory budget before the
/// arrays are allocated and charges for exactly what the arena keeps - so the first slab is the
/// whole of an ordinary build. A build that needs more than a slab, or more than was planned for,
/// simply takes another; nothing depends on the estimate being right.
///
/// Callers must pair this with finishStepArena however the build ends, or the next truth-table
/// build would draw from a slab nobody meant it to share.
let startStepArena (cost: StepCost) (steps: int) =
    stepArena <-
        Some
            { U32Slab = Array.empty
              U32Next = 0
              U32Planned = plannedElements cost.TypedArrayBytes steps 4 u32SlabSize
              BigSlab = Array.empty
              BigNext = 0
              // the heap cost counts a reference and the BigInt behind it, where the slab holds
              // one element per step per wide bus - so this over-estimates, which costs nothing
              BigPlanned = plannedElements cost.HeapBytes steps 4 bigSlabSize }

let finishStepArena () = stepArena <- None

/// One uint32 step region of `size` steps: (slab, base) - from the arena when a build has one
/// open, a dedicated exactly-sized slab otherwise.
let private makeU32Region (size: int) : uint32 array * int =
    match stepArena with
    | Some arena when size <= u32SlabSize ->
        if arena.U32Slab.Length - arena.U32Next < size then
            let slab = max size (min u32SlabSize arena.U32Planned)
            arena.U32Slab <- Array.zeroCreate slab
            // the next one is twice this: a build that needed more than was planned for reaches
            // what it needs in a few doublings, where jumping to the maximum made a circuit of
            // three components allocate 256MB
            arena.U32Planned <- min u32SlabSize (slab * 2)
            arena.U32Next <- 0

        let regionBase = arena.U32Next
        arena.U32Next <- regionBase + size
        arena.U32Slab, regionBase
    | _ -> Array.zeroCreate size, 0

/// One bigint step region of `size` steps, filled with 0I as bigint step stores always were.
let private makeBigRegion (size: int) : bigint array * int =
    match stepArena with
    | Some arena when size <= bigSlabSize ->
        if arena.BigSlab.Length - arena.BigNext < size then
            let slab = max size (min bigSlabSize arena.BigPlanned)
            arena.BigSlab <- Array.create slab 0I
            arena.BigPlanned <- min bigSlabSize (slab * 2)
            arena.BigNext <- 0

        let regionBase = arena.BigNext
        arena.BigNext <- regionBase + size
        arena.BigSlab, regionBase
    | _ -> Array.create size 0I, 0

let makeIOArrayW w size =
    stepArrayIndex <- stepArrayIndex + 1
    match w with
    | w when w <= 32 ->
        let slab, regionBase = makeU32Region size
        { FDataStep = Array.create 2 (Data <| { Width = w; Dat = Word 0u }) // NOTE - 2 should be enough for FData arrays as they are only used in Truthtable
          UInt32Slab = slab
          BigIntSlab = Array.empty
          StepBase = regionBase
          StepLength = size
          Width = w
          Index = stepArrayIndex }
    | _ ->
        let slab, regionBase = makeBigRegion size
        { FDataStep = Array.create 2 (Data <| { Width = w; Dat = BigWord 0I }) // NOTE - 2 should be enough for FData arrays as they are only used in Truthtable
          UInt32Slab = Array.empty
          BigIntSlab = slab
          StepBase = regionBase
          StepLength = size
          Width = w
          Index = stepArrayIndex }

/// create a FastComponent data structure with data arrays from a SimulationComponent.
/// numSteps is the number of past clocks data kept - arrays are managed as circular buffers.
///
/// fullName and sheetName are given rather than filled in afterwards: the flatten stores this
/// object in its index space, and `{ fc with FullName = ... }` afterwards would put a DIFFERENT
/// object there than the one the caller kept - a copy of a 24-field record per component, and a
/// reference-equal identity broken, for two strings that are known when it is made.
let createFastComponent
    (maxArraySize: int)
    (sComp: SimulationComponent)
    (accessPath: ComponentId list)
    (fullName: string)
    (sheetName: string list)
    =
    let inPortNum, outPortNum = getPortNumbers sComp
    // dummy arrays wil be replaced by real ones when components are linked after being created
    let ins =
        [| 0 .. inPortNum - 1 |]
        |> Array.map (fun n ->
            match sComp.Type with
            | Input1 (width, defVal)->
                // special case - add real input arrays now to avoid excption during initialse
                makeIOArrayW width maxArraySize
            | _ ->
                makeIOArray maxArraySize)
    
    let outs =
        match sComp.Type, sComp.OutputWidths.Length with
        | IOLabel, 0 -> [| makeIOArray maxArraySize |] // NOTE - create dumpy Outputs array for inavtive IOLabels
        | _ ->
            sComp.OutputWidths
            |> Array.map (fun w -> makeIOArrayW w maxArraySize)

    let state =
        if couldBeSynchronousComponent sComp.Type then
            Some(Array.create maxArraySize NoState)
        else
            None

    let fId = getFid sComp.Id accessPath

    let reduceIfHybrid sc ipn =
        if isHybridComponent sc.Type then
            [ 0..ipn ]
            |> List.sumBy (fun ipn ->
                getHybridComponentAsyncOuts sc.Type (InputPortNumber ipn)
                |> function
                    | None
                    | Some [] -> 0
                    | Some _ -> 1)
        else
            ipn

    match sComp.Type with
    | Input1(w, d) -> ins[0] <- { ins[0] with Width = w }
    | _ -> ()

    { UseBigInt = false // dump value, will be set when Input Widths are avaiable after linkFastComponents
      BigIntState = None // dump value, will be set when Input Widths are avaiable after linkFastComponents
      State = Option.map makeStepArray state
      SimComponent = sComp
      fId = fId
      cId = sComp.Id
      FType = sComp.Type
      AccessPath = accessPath
      SheetName = sheetName
      // placeholders: the real reducers need EvalReference, which is compiled after this, and
      // cannot be built until widths and bigint state are known anyway. installReducers puts
      // them in once the simulation is linked.
      ReduceComb = fun _ -> failwithf "Reducer for %A was never installed" sComp.Type
      ReduceClocked = fun _ -> failwithf "Reducer for %A was never installed" sComp.Type
      // stamped by LookupArray.addItem the moment this is stored; the links are filled in by the
      // flatten as it resolves them, and dropped again by linkFastComponents
      Index = FastCompIndex -1
      OutLinks = Array.empty
      CustomInLinks =
        match sComp.Type with
        | Custom _ -> Array.create inPortNum (FastCompIndex -1)
        | _ -> Array.empty
      CustomOutIndex = FastCompIndex -1
      CustomOutPort = 0
      Touched = false
      DrivenComponents = []
      NumMissingInputValues = reduceIfHybrid sComp inPortNum
      InputLinks = ins
      InputDrivers = Array.create inPortNum None
      Outputs = outs
      FullName = fullName
      FLabel = extractLabel sComp.Label
      VerilogOutputName = Array.create outPortNum ""
      VerilogComponentName = ""
      Active =
        match sComp.Type with
        | IOLabel -> false
        | _ -> true }

/// Scratch used only by the flatten: design ComponentId -> the store index of that component,
/// within the sheet INSTANCE currently being walked.
///
/// One array serves the whole build. It is overwritten for each instance the walk enters, which
/// is safe because the only thing read from it is a sibling: sComp.Outputs names design ids in
/// the same graph, and every one of those was written moments earlier at this same level.
type private SiblingIndex = { mutable Ix: int array }

/// The largest step by which the component store grows when it has to. It should never have to -
/// the store is created at exactly the size costAndSizeOfGraph counted - so this is the backstop
/// for a flatten that sees more components than that walk did, which would be a bug.
let private storeGrowthCap = 65536

/// Make room in both arrays the flatten indexes by DESIGN ComponentId. Those ids are dense from 1
/// across the whole design (ComponentId), which is what makes an array the right
/// lookup - but the largest one is not known until the walk has met it, and walking the design
/// twice to find out would cost more than growing costs.
let private ensureId (g: GatherData) (sib: SiblingIndex) (i: int) =
    if i >= g.Labels.Length then
        let bigger = Array.create (max (2 * g.Labels.Length) (i + 1)) "*"
        Array.blit g.Labels 0 bigger 0 g.Labels.Length
        g.Labels <- bigger

    if i >= sib.Ix.Length then
        let bigger: int array = Array.zeroCreate (max (2 * sib.Ix.Length) (i + 1))
        Array.blit sib.Ix 0 bigger 0 sib.Ix.Length
        sib.Ix <- bigger

/// Flatten and expand one sheet instance of the simulation graph into the build's index space,
/// creating its FastComponents as it goes, and recurse into the custom components it holds.
///
/// This used to be two walks. The first returned four lists concatenated with @ up the recursion,
/// which became four Maps keyed by (ComponentId, access path); the second folded one of those
/// maps to make the FastComponents. Everything after them then paid a structurally-keyed lookup -
/// a boxed comparison of an id and a list per tree level - for every link of every component.
/// Creating each component as it is met, stamping it with its index, and resolving every link the
/// walk can already see into those indices leaves the phases after this doing arithmetic on ints.
///
/// parent is the store index of the custom component this instance is the innards of, with its
/// type, or None for the top sheet. It is how the inner Input and Output components are tied to
/// that component's ports: the match is by label WITHIN a level, so it belongs where a level is
/// in hand rather than in a map built for it afterwards.
///
/// The order below is the whole of the correctness argument, and each step depends on the last:
/// labels before creation (a component's name is the labels along its access path), creation
/// before sibling links and before the recursion (a link is a store index, so the index has to
/// exist), and the recursion last of all (it overwrites the sibling scratch).
let rec private flattenLevel
    (g: GatherData)
    (sib: SiblingIndex)
    (maxArraySize: int)
    (ap: ComponentId list)
    (graph: SimulationGraph)
    (parent: (int * CustomComponentType) option)
    : unit
    =
    let graphL = Map.toList graph

    graphL
    |> List.iter (fun (ComponentId i, comp) ->
        ensureId g sib i
        g.Labels[i] <- extractLabel comp.Label)

    /// this instance's components, in graph order, each paired with the FastComponent now in the
    /// store for it. Custom components included, and created BEFORE the recursion into what they
    /// contain, so their index exists when the links to their innards are made.
    let created =
        graphL
        |> List.map (fun (ComponentId i as cid, comp) ->
            let fid = getFid cid ap

            let fc =
                createFastComponent maxArraySize comp ap (g.getFullSimName fid) (g.getFullSimPath fid)
                |> fun fc -> LookupArray.addItem fc g.Comps

            sib.Ix[i] <- fastCompIndexValue fc.Index
            comp, fc)

    // Sibling links. sComp.Outputs names design ids in this same instance, so they resolve to
    // store indices here, once, instead of being followed through a map on every link.
    created
    |> List.iter (fun (comp, fc) ->
        let ports =
            (max fc.Outputs.Length 1, comp.Outputs)
            ||> Map.fold (fun n (OutputPortNumber k) _ -> max n (k + 1))

        let outLinks: (FastCompIndex * InputPortNumber) array array = Array.create ports [||]

        comp.Outputs
        |> Map.iter (fun (OutputPortNumber k) driven ->
            outLinks[k] <-
                driven
                |> List.toArray
                |> Array.map (fun (ComponentId j, ipn) -> FastCompIndex sib.Ix[j], ipn))

        fc.OutLinks <- outLinks)

    // Tie this instance's Input and Output components to the ports of the custom component it is
    // the innards of. Both ends are known here; nothing has to be remembered for later.
    match parent with
    | None -> ()
    | Some(customIndex, ct) ->
        let custom = LookupArray.item customIndex g.Comps

        /// the width a custom component's port label is matched on, which is the width declared
        /// by the Input or Output component inside it
        let portWidth t =
            match t with
            | Input1(n, _) -> n
            | Output n -> n
            | _ -> -1

        let indexOf (candidates: (SimulationComponent * FastComponent) list) (lab: string, w: int) =
            candidates
            |> List.find (fun (comp, _) -> comp.Label = ComponentLabel lab && portWidth comp.Type = w)
            |> snd
            |> fun fc -> fc.Index

        let outputs = created |> List.filter (fun (comp, _) -> isOutput comp.Type)

        ct.OutputLabels
        |> List.iteri (fun i labelled ->
            let inner = LookupArray.item (fastCompIndexValue (indexOf outputs labelled)) g.Comps
            inner.CustomOutIndex <- FastCompIndex customIndex
            inner.CustomOutPort <- i)

        let inputs = created |> List.filter (fun (comp, _) -> isInput comp.Type)

        ct.InputLabels
        |> List.iteri (fun i labelled -> custom.CustomInLinks[i] <- indexOf inputs labelled)

    // Last, because everything above reads the sibling scratch this level filled, and this
    // overwrites it.
    created
    |> List.iter (fun (comp, fc) ->
        match comp.Type, comp.CustomSimulationGraph with
        | Custom ct, Some csg ->
            flattenLevel g sib maxArraySize (ap @ [ comp.Id ]) csg (Some(fastCompIndexValue fc.Index, ct))
        | _ -> ())

/// Flatten the SimulationGraph into the one index space the rest of the build works in, creating
/// every FastComponent - and so every step array - as it goes.
///
/// size is how many components the expanded design has, from costAndSizeOfGraph, so the store is
/// made at exactly the right size and never grows. That same walk is what priced the design and
/// refused it if it would not fit, which is why this may allocate at all: by the time it runs the
/// budget has been checked and the step-array arena opened for what it said.
let gatherSimulation (maxArraySize: int) (size: int) (graph: SimulationGraph) =
    let startTime = getTimeMs ()
    stepArrayIndex <- -1

    let g =
        { Comps =
            LookupArray.create
                (fun (fc: FastComponent) -> fastCompIndexValue fc.Index)
                // in place: FastComponent is [<ReferenceEquality>] and already carries mutable
                // fields, so the stamp costs no record copy and the identity the simulator relies
                // on survives it
                (fun fc i ->
                    fc.Index <- FastCompIndex i
                    fc)
                size
                storeGrowthCap
          // grown by ensureId as ids are met; 64 is where the smallest design starts
          Labels = Array.create 64 "*" }

    flattenLevel g { Ix = Array.zeroCreate 64 } maxArraySize [] graph None
    instrumentInterval "gatherGraph" startTime g

/// Add one driver changing the fs.Driver array reference.
/// Return a WaveIndex reference.
/// WaveIndex refrences are bound to specific component ports
/// and not unique per driver.
/// Whether a port of a component carries a waveform the user can watch.
///
/// The rules the wave index is built from, said once. They used to live only inside
/// addComponentWaveDrivers, which walks every port of every component of the whole expanded
/// simulation; anything wanting to know which ports one INSTANCE offers - without walking the
/// expansion to find out - has to decide it the same way, and two copies of a rule like this
/// drift. The builder below now asks this, so there is one answer.
///
/// What is excluded, and why each: the input side of components whose input is not a signal of
/// its own (an IOLabel, an Input1, a Viewer, a NotConnected, an Output); a Constant1, whose
/// output drives but is not worth watching; every IOLabel of a same-named group except the one
/// elected to drive it, since they are all one net; the wiring components, which carry no signal
/// distinct from what they are wired to; and the Input1 and Output components INSIDE a subsheet,
/// whose signal is the enclosing custom component's port and is offered there instead.
let portCarriesWave (f: FastSimulation) (fc: FastComponent) (pType: PortType) =
    let ioLabelIsActive () =
        f.FIOActive[ComponentLabel fc.FLabel, snd fc.fId].fId <> fc.fId

    match fc.FType, pType with
    | IOLabel, PortType.Input
    | Input1 _, PortType.Input
    | Viewer _, PortType.Input
    | NotConnected, PortType.Input
    | Output _, PortType.Input -> false
    | Constant1 _, _ -> false
    | IOLabel, _ when ioLabelIsActive () -> false
    | _ ->
        match fc.FType with
        | SplitWire _
        | BusSelection _
        | MergeWires
        | MergeN _
        | SplitN _
        | Constant1 _ -> false
        | Output _ when fc.SubSheet <> [] -> false
        | Input1 _ when fc.SubSheet <> [] -> false
        | _ -> true

let addComponentWaveDrivers (f: FastSimulation) (fc: FastComponent) (pType: PortType) =
    let makeWaveIndex (index: int) pn pType arr =
        { SimArrayIndex = DriverIndex index; Id = fc.fId; PortType = pType; PortNumber = pn }

    let addStepArray pn (index: int) stepA =
        f.Drivers[index] <-
            Some
            <| Option.defaultValue
                { Index = DriverIndex index; DriverData = stepA; DriverWidth = 0 }
                f.Drivers[index]

        let addWidth w optDriver =
            Option.map (fun d -> { d with DriverWidth = w }) optDriver

        fc.Outputs[pn]
        |> (fun output -> f.Drivers[index] <- addWidth output.Width f.Drivers[index])

    let ioLabelIsActive fc =
        f.FIOActive[ComponentLabel fc.FLabel, snd fc.fId].fId
        <> fc.fId

    match pType with
    | PortType.Output -> fc.Outputs
    | PortType.Input -> fc.InputLinks
    |> Array.mapi (fun pn stepA ->
        let index = stepA.Index

        // a DRIVER is registered more widely than a wave is offered: a Constant1's output has to
        // drive the things wired to it even though nobody watches it, and an inactive IOLabel
        // shares the driver of the one elected for its group
        let addDriver =
            match fc.FType, pType with
            | IOLabel, PortType.Input
            | Input1 _, PortType.Input
            | Viewer _, PortType.Input
            | NotConnected, PortType.Input
            | Output _, PortType.Input -> false
            | Constant1 _, _ -> true
            | IOLabel, _ when ioLabelIsActive fc -> false
            | _ -> true

        if pType = PortType.Output && addDriver then
            addStepArray pn index stepA

        if portCarriesWave f fc pType then
            [| makeWaveIndex index pn pType stepA |]
        else
            [||])

/// Called after the fs.Drivers array is created.
/// waveComps must contain all components that can be viewed in the wave simulation.
/// This function mutates fs.Drivers adding the correct arrays where
/// these are used. In some cases an array may never be used and therefore is not added.
/// In parallel with this, the function returns an array of WaveIndexT records that
/// reference component ports which can be viewed in a wave simulation.
/// Every WaveIndex references an element of fs.Drivers from which the simulation data is found.
let addWaveIndexAndDrivers (comps: FastComponent array) (f: FastSimulation) : WaveIndexT array =
    let addDrivers pType =
        Array.collect (fun fc -> addComponentWaveDrivers f fc pType)

    let outs = addDrivers PortType.Output comps
    let ins = addDrivers PortType.Input comps
    Array.append outs ins |> Array.concat

/// Changes all the custom component in and out StepArray links so they point to the correct drivers.
/// (fid, fc) must be a custom component.
/// Called after the simulation has been fully constructed and linked.
let linkFastCustomComponentsToDriverArrays (fs: FastSimulation) (fid: FComponentId) (fc: FastComponent) : Unit =
    let cid, ap' = fid
    let ap = ap' @ [ cid ]

    let ct =
        match fc.FType with
        | Custom ct -> ct
        | _ -> failwithf "linkFastCustomComponent must be called with a custom component"

    let graph =
        match fc.SimComponent.CustomSimulationGraph with
        | Some g -> g
        | None -> failwithf "What? Can't find customSimulationGraph"

    graph
    |> Map.iter (fun cid sc ->
        match sc.Type with
        | Input1(w, _) ->
            let portNum =
                ct.InputLabels
                |> List.indexed
                |> List.find (fun (i, (lab, _)) -> (ComponentLabel lab = sc.Label))
                |> fst

            fc.InputLinks[portNum] <- (fs.FastComponentOf(cid, ap)).Outputs[0]
        | Output w ->
            let portNum =
                ct.OutputLabels
                |> List.indexed
                |> List.find (fun (i, (lab, _)) -> ComponentLabel lab = sc.Label)
                |> fst

            fc.Outputs[portNum] <- (fs.FastComponentOf(cid, ap)).InputLinks[0]
        | _ -> ())

/// Point every custom component's ports at the arrays of the Input and Output components inside
/// it, so that reading a custom component's port reads the signal it actually carries.
///
/// Not optional, and not part of the wave tables below even though it used to be in the same
/// phase: a port that points at the dummy array it was created with reads as nothing. Cheap - one
/// re-pointing per port of each custom component, and there are far fewer of those than of
/// ordinary ones.
///
/// `comps` is every component of the build in creation order - the array the gather filled, not
/// the maps. See the note on createFastArrays: walking the maps here means walking the components
/// in a different order from the one they were allocated in, and on a design of any size that is
/// most of what this phase costs.
let linkCustomComponentPorts (comps: FastComponent array) (fs: FastSimulation) : FastSimulation =
    comps
    |> Array.iter (fun fc -> if isCustom fc.FType then linkFastCustomComponentsToDriverArrays fs fc.fId fc)

    fs

/// Adds the Drivers and WaveIndex fields to a fast simulation.
/// For use by waveform Simulator.
/// Needs to be run after widths are calculated.
///
/// **What a simulation needs only in order to be DRAWN**, and all of it sized by the expansion:
/// a map of every component of every instance, an entry per step array, and an entry per
/// wave-carrying port. `WaveTables` is what decides whether a build pays for them; a simulator
/// that runs and answers reads by name does not.
let addWavesToFastSimulation (comps: FastComponent array) (fs: FastSimulation) : FastSimulation =
    // Create null driver array large enough for all created step arrays
    // each step array is given a sequentially generated id as it is created
    // however, some of these arrays will never be used and end up as None
    // elements of the driver array.
    { fs with Drivers = Array.create fs.NumStepArrays None }
    // Generate all waves, add (mutably) step arrays to driver array replacing None
    // by Some array in the index unique to the array added as these are needed
    // by wave component ports.
    // One array can be referenced by multiple ports.
    // The mutable changes to fs.Drivers here are write-once, from None to Some array.
    |> (fun fs -> { fs with WaveIndex = addWaveIndexAndDrivers comps fs })
/// The door between the build's index space and the rest of the program.
///
/// It used to create the FastComponents as well; the flatten does that now, so what is left of it
/// is the boundary. What a built simulation offers is the STORE - FCompsByIndex, in gather order -
/// and one map, FIndexOf, from the design-time name a caller arrives with to a place in it. The
/// name is what survives a rebuild and so what the renderer holds; the index is what everything
/// inside the simulation works in.
///
/// Custom against ordinary is decided here, by a predicate over the one store. It is not a second
/// index space and must not become one: the links the flatten resolved are positions in this
/// store, and splitting it would make the same integer mean two different things.
let createInitFastCompPhase (simulationArraySize: int) (g: GatherData) (f: FastSimulation) =
    let start = getTimeMs ()
    let all = LookupArray.toArray g.Comps

    /// The one map a built simulation keeps: where each design-time name sits in the store.
    /// Custom components are in it too - the wave viewer reaches their ports through it.
    let indexOfName =
        (Map.empty, all) ||> Array.fold (fun m fc -> Map.add fc.fId fc.Index m)

    /// which Output component inside a custom component drives each of its output ports - the
    /// inverse of the link the flatten stamped onto that Output component
    let customOutLookup =
        (Map.empty, all)
        ||> Array.fold (fun m fc ->
            if fastCompIndexValue fc.CustomOutIndex < 0 then
                m
            else
                let custom = LookupArray.item (fastCompIndexValue fc.CustomOutIndex) g.Comps
                Map.add (custom.fId, OutputPortNumber fc.CustomOutPort) fc.Index m)

    instrumentTime "createInitFastCompPhase" start

    { f with
        FCompsByIndex = all
        FIndexOf = indexOfName
        MaxArraySize = simulationArraySize
        FCustomOutputCompLookup = customOutLookup
        NumStepArrays = stepArrayIndex + 1
        Drivers = Array.empty }

/// Has side effect of making IOLabels of same name (in the same graph) all use same output array
/// this means that an input to any one will produce an output on all, for no effort.
/// IOLabels without driven inputs that are thus not used are later on flagged inactive.
/// They must not be reduced, and will not be included in the ordered component list
let private reLinkIOLabels (fs: FastSimulation) =
    // Go through all the components driven by IOLabels and link them from the active label
    // at this point exactly one out of every labelled set will be active, and contained in FIOActive
    fs.FIOLinks
    |> List.iter (fun ((fcDriven, InputPortNumber ipn), ioDriver) ->
        let labKey = ioDriver.SimComponent.Label, ioDriver.AccessPath
        let fcActiveDriver = fs.FIOActive[labKey]
        fcDriven.InputLinks[ipn] <- fcActiveDriver.Outputs[0]
        fcDriven.InputDrivers[ipn] <- Some(fcActiveDriver.Index, OutputPortNumber 0)
        // DrivenComponents must only include asynchronous drive paths on hybrid components
        // on clocked components, or combinational components, it can include all drive paths
        match getHybridComponentAsyncOuts fcDriven.FType (InputPortNumber ipn) with
        | None
        | Some(_ :: _) -> fcActiveDriver.DrivenComponents <- fcDriven :: fcActiveDriver.DrivenComponents
        | _ -> ()

        ioDriver.Outputs[0] <- fcActiveDriver.Outputs[0])

/// Use the links the flatten resolved to tie the FastComponents' data arrays together.
/// InputLinks[i] is set equal to the driving Outputs array, so that input i reads the data
/// reduced by the correct output of the component that drives it.
/// The main work is dealing with custom components, which represent whole design sheets with
/// recursively defined component graphs. The custom component itself is not linked and does not
/// exist as a simulatable FastComponent - instead its graph's Input and Output components are
/// linked to whatever connects to the corresponding ports of the custom component.
/// Note: custom components are linked in later as unsimulatable placeholders, to let the wave
/// simulation reach their ports.
///
/// Everything here works in store indices. It used to work in (ComponentId, access path) pairs
/// against four maps, and that was the build's shape problem: a structural key costs a boxed
/// comparison of a GUID-like id and a list per tree level, and this function does one per link of
/// every component of the expanded design.
let linkFastComponents (g: GatherData) (f: FastSimulation) =
    let start = getTimeMs ()
    let store = g.Comps

    /// Follow one component output across custom component boundaries to the real inputs it
    /// drives, as store indices. Every step of this used to be a map lookup; they are all array
    /// reads now, off links the flatten resolved while it had both ends in hand.
    let rec getLinks (i: FastCompIndex) (opn: int) (ipnOpt: InputPortNumber option) : (FastCompIndex * InputPortNumber) array =
        let fc = LookupArray.item (fastCompIndexValue i) store

        match isOutput fc.FType, isCustom fc.FType, ipnOpt with
        | true, _, None when fc.AccessPath = [] -> [||] // no links in this case from global output
        | true, _, None ->
#if ASSERTS
            assertThat
                (isCustom (LookupArray.item (fastCompIndexValue fc.CustomOutIndex) store).FType)
                "What? this should be a custom component output"
#endif
            getLinks fc.CustomOutIndex fc.CustomOutPort None // go from inner output to CC output and recurse
        | false, true, Some(InputPortNumber ipn) ->
            [| fc.CustomInLinks[ipn], InputPortNumber 0 |] // go from CC input to inner input: must be valid
        | _, false, Some ipn -> [| i, ipn |] // must be a valid link
        | false, _, None -> fc.OutLinks[opn] |> Array.collect (fun (j, ipn) -> getLinks j opn (Some ipn))
        | x -> failwithf "Unexpected link match: %A" x

    // One entry per driven input, to catch an input driven twice, keyed by the unique Index of
    // the input's own step array - read before linking replaces the array, so it identifies
    // (component, input port) exactly. Step-array indices are handed out densely from 0, so this
    // is a pair of plain arrays: which component drives each input and from which of its ports,
    // with -1 for one not yet driven. It was a map keyed structurally by (FComponentId, port),
    // which on a 480,000-component design was a measured fifth of the whole build, and then a
    // Dictionary<int,_> to escape that. An array needs neither.
    let driverOf: int array = Array.create f.NumStepArrays (-1)
    let driverPort: int array = Array.zeroCreate f.NumStepArrays

    store
    |> LookupArray.iteri (fun iDriver fDriver ->
        // a custom component drives nothing: its ports are links to the components inside it,
        // which getLinks has already followed through. The store holds it all the same, so that
        // one index space covers the whole design.
        if not (isCustom fDriver.FType) then
            fDriver.Outputs
            |> Array.iteri (fun iOut _ ->
                getLinks (FastCompIndex iDriver) iOut None
                |> Array.iter (fun (iDriven, InputPortNumber ipn) ->
                    let fDriven = LookupArray.item (fastCompIndexValue iDriven) store
                    let inputKey = fDriven.InputLinks[ipn].Index

                    match driverOf[inputKey] with
                    | -1 -> ()
                    | previous ->
                        failwithf
                            "Multiple linkage: (previous driver was %A,%A)"
                            (LookupArray.item previous store).FullName
                            (OutputPortNumber driverPort[inputKey])

                    driverOf[inputKey] <- iDriver
                    driverPort[inputKey] <- iOut
                    let ap = fDriven.AccessPath

                    // we have a link from fDriver to fDriven

                    if isIOLabel fDriven.FType then
                        // fDriven is a driven label of a set of IOlabels
                        let labelKey = fDriven.SimComponent.Label, ap

                        if not (Map.containsKey labelKey f.FIOActive) then
                            // Make this then unique driven label in the fast simulation
                            f.FIOActive <- Map.add labelKey fDriven f.FIOActive
                            fDriven.Active <- true

                    if isIOLabel fDriver.FType then
                        // we do not yet know which label will be active, so record all links from
                        // labels for later resolution
                        f.FIOLinks <- ((fDriven, InputPortNumber ipn), fDriver) :: f.FIOLinks
                    else
                        // if driver is not IO label make the link now
                        fDriven.InputLinks[ipn] <- fDriver.Outputs[iOut]
                        // DrivenComponents must only include asynchronous drive paths on hybrid components
                        // on clocked components, or combinational components, it can include all drive paths
                        match getHybridComponentAsyncOuts fDriven.FType (InputPortNumber ipn) with
                        | None
                        | Some(_ :: _) -> fDriver.DrivenComponents <- fDriven :: fDriver.DrivenComponents
                        | _ -> ()

                        fDriven.InputDrivers[ipn] <- Some(fDriver.Index, OutputPortNumber iOut))))

    reLinkIOLabels f

    // The link tables have done their work. Dropping them now is what stops a built simulation
    // carrying an outgoing link table per component for the rest of its life - tens of megabytes
    // on a design that expands, traced by every major collection, for something no later phase
    // reads. What the simulation still needs of them was copied into FCustomOutputCompLookup.
    store
    |> LookupArray.iteri (fun _ fc ->
        fc.OutLinks <- Array.empty
        fc.CustomInLinks <- Array.empty)

    instrumentTime "linkFastComponents" start
    f

// This function is called after linkFastComponents (when width of IO of all components are resolved) to resolve the UseBigInt and BigIntState fields of all components
let determineBigIntState (comps: FastComponent array) (f: FastSimulation) =
    // in creation order, over the array the gather filled - see createFastArrays
    comps
    |> Array.iter (fun fc ->
        if not (isCustom fc.FType) then
            let (u, state) = findBigIntState fc
            fc.UseBigInt <- u
            fc.BigIntState <- state)
    f
