module SimTypes

//---------------------------------------------------------------------------------------//
//--------------------------------Fast Simulation Data Structures------------------------//
//---------------------------------------------------------------------------------------//

open Fable.Core
open EEExtensions
open CommonTypes
open SimGraphTypes

// type FComponentId = ComponentId * ComponentId list moved to CommonTypes

type FData =
    | Data of FastData
    | Alg of FastAlgExp

    member this.Width =
        match this with
        | Data d -> d.Width
        | Alg exp -> getAlgExpWidth exp

    member this.fdToString =
        match this with
        | Data { Dat = Word w; Width = _ } -> string w
        | Data { Dat = BigWord w; Width = _ } -> w.ToString()
        | Alg exp -> expToKatex exp

    member this.toExp =
        match this with
        | Alg exp -> exp
        | Data fd -> DataLiteral fd

    member this.toFastData =
        match this with
        | Data fd -> fd
        | _ -> failwithf "Expected data, found Alg FData"

/// Wrapper to allow arrays to be resized for longer simulations while keeping the links between inputs
/// and outputs
type StepArray<'T> = { Step: 'T array; Index: int }

// [<Struct>] // TODO - check whether fable optimzed Struct

/// Step-store access without a Fable bounds check.
///
/// Under Fable an ordinary arr[i] on a local binding compiles to fable-library's item/setItem,
/// which profiling once found taking 70% of all simulation time (see EvalCompiled's getA/setA,
/// the same shim for the same reason). The old per-IO step arrays escaped it by accident -
/// property-chain access emits a raw index - but the members below bind `this` to a local, so
/// without this shim every read on the simulator's hot path became an item() call, measured at
/// 3x whole-simulation cost. Every index is StepBase + a step below StepLength, in range by
/// construction; under .NET these stay ordinary checked accesses.
#if FABLE_COMPILER
[<Emit("$0[$1]")>]
let stepGet (arr: 'a array) (i: int) : 'a = Fable.Core.Util.jsNative

[<Emit("$0[$1] = $2")>]
let stepSet (arr: 'a array) (i: int) (v: 'a) : unit = Fable.Core.Util.jsNative
#else
let inline stepGet (arr: 'a array) (i: int) : 'a = arr[i]
let inline stepSet (arr: 'a array) (i: int) (v: 'a) : unit = arr[i] <- v
#endif

/// This type represents an array of time steps of simulation data.
/// In any simulation, for a given IOArray, only one of the three step stores will be used.
/// For (very strong) efficiency reasons this cannot be implemented as a disjoint union:
/// the code that reads and writes IOArray elements will access the appropriate store.
/// Truthtable simulations use FDataStep everywhere.
/// Normal simulations use the UInt32 or BigInt store according to the size of the relevant bus.
///
/// The uint32 and bigint stores are REGIONS OF SHARED SLABS, not arrays of their own: step s of
/// this IO lives at `slab[StepBase + s]`, and `StepLength` steps belong to it. FastCreate's
/// arena packs every step region of a build into a few large slabs (under both runtimes), so a
/// design has dozens of step allocations rather than one per output port, and a port's data is
/// named by an integer offset. Always go through the members below - indexing a slab without
/// adding StepBase reads another port's data, which no bounds check will ever catch.
type IOArray =
    { FDataStep: FData array
      UInt32Slab: uint32 array
      BigIntSlab: bigint array
      StepBase: int
      StepLength: int
      Width: int
      Index: int }

    /// uint32 value at a step
    member inline this.U32 step = stepGet this.UInt32Slab (this.StepBase + step)
    /// write a uint32 value at a step
    member inline this.SetU32 step dat = stepSet this.UInt32Slab (this.StepBase + step) dat
    /// bigint value at a step
    member inline this.Big step = stepGet this.BigIntSlab (this.StepBase + step)
    /// write a bigint value at a step
    member inline this.SetBig step dat = stepSet this.BigIntSlab (this.StepBase + step) dat
    /// Array.tryItem over the uint32 region: None when this IO is not on the uint32 path or
    /// the step is outside the region
    member inline this.TryU32 step =
        if this.UInt32Slab.Length = 0 || uint step >= uint this.StepLength then
            None
        else
            Some(stepGet this.UInt32Slab (this.StepBase + step))
    /// Array.tryItem over the bigint region
    member inline this.TryBig step =
        if this.BigIntSlab.Length = 0 || uint step >= uint this.StepLength then
            None
        else
            Some(stepGet this.BigIntSlab (this.StepBase + step))
    /// the whole uint32 region as a fresh array - a copy, for cold extraction paths only;
    /// empty when this IO is not on the uint32 path, as the store itself once was
    member this.U32Contents =
        if this.UInt32Slab.Length = 0 then
            Array.empty
        else
            Array.sub this.UInt32Slab this.StepBase this.StepLength

    /// the whole bigint region as a fresh array - a copy, for cold extraction paths only;
    /// empty when this IO is not on the bigint path
    member this.BigContents =
        if this.BigIntSlab.Length = 0 then
            Array.empty
        else
            Array.sub this.BigIntSlab this.StepBase this.StepLength

/// Where one clock step sits in the circular simulation arrays: the step number itself, its
/// index into the arrays, and the index of the step before it. All three follow from the step
/// number, so the simulation loop works them out once per step and hands the same value to
/// every component - numStep % maxArraySize is an integer division, and it used to be redone
/// for every component of every step. A struct so that passing it costs nothing.
[<Struct>]
type StepIndex =
    { NumStep: int
      SimStep: int
      SimStepOld: int }

let inline stepIndexOf (maxArraySize: int) (numStep: int) =
    let simStep = numStep % maxArraySize

    { NumStep = numStep
      SimStep = simStep
      SimStepOld =
        if simStep = 0 then
            maxArraySize - 1
        else
            simStep - 1 }

/// Used for efficiency reasons.
/// For a given normal simulation these arrays show whether the corresponding
/// component input or output is a bigint or a unint32 type bus, and therefore
/// show IOArray array is used for the data.
type BigIntState =
    { InputIsBigInt: bool array // NOTE - whether each input uses BigInt or UInt32
      OutputIsBigInt: bool array }

/// FastComponent represents a physical component in a simulation. Because sheets can be
/// instantiated in multiple places a given sheet component can have multiple FastComponents
/// in the simulation.
/// Arrays on FastComponent are filled up with simulation data per clock step as a clocked
/// simulation progresses.
/// Equality is by reference: a FastComponent is a mutable object with an identity, holding
/// step arrays that can run to megabytes, so comparing two of them field by field would be
/// both meaningless and ruinous. It also carries its own reducers, which are functions and
/// so have no structural equality at all.
[<ReferenceEquality>]
type FastComponent =
    {
      /// contains component path to root of simulation - unique
      fId: FComponentId
      /// allows access to the underlying component
      cId: ComponentId
      /// convenience access to the Type of the underlying component
      FType: ComponentType
      /// Used only by clocked components, contains an array of the component state in
      /// every clock cycle. Filled as simulation progresses.
      State: StepArray<SimulationComponentState> option
      mutable Active: bool
      /// Most components have all bus inputs and outputs the same width. This gives the
      /// default store to use - the BigInt or UInt32 slab region - in IOArray.
      mutable UseBigInt: bool
      /// components that may have variable inputs and output widths use this instead of UseBigInt to
      /// determine the correct array.
      mutable BigIntState: BigIntState option // This is only used for components that have variable input/output widths
      /// Input data - this an array of fxed links to the relevant driver output data arrays
      InputLinks: IOArray array
      /// info on where the drivers are for each input
      InputDrivers: (FComponentId * OutputPortNumber) option array
      /// the output data for this component (this gets linked to all the conmponents driven
      Outputs: IOArray array
      /// the legacy SimulationConmponent from which this FastComponent is generated.
      SimComponent: SimulationComponent
      /// Path from thsi component to root of simulation, if it is in a subsheet.
      AccessPath: ComponentId list
      /// for human use: long name of component
      FullName: string
      /// label of component
      FLabel: string
      /// The component's full path in the simulation: the LABELS of the custom component
      /// instances it sits within, from the root of the simulation, followed by its own label.
      /// All upper-cased. Built by GatherData.getFullSimPath.
      /// Despite the name these are component labels, NOT sheet names - a component on the top
      /// sheet has a single-element path holding its own label. For which INSTANCE a component
      /// is in use Instance below, and for that instance's sheet use getSheetNameOfInstance.
      /// SubSheet below drops the last element to give just the enclosing instances.
      SheetName: string list
      /// This component's reducer, bound to this component, as a combinational and as a
      /// clocked reduction. Installed once by installReducers when the simulation is built,
      /// so that the loop calls the component's own code instead of dispatching on FType for
      /// every component of every step. Only the hybrid components (asynchronous RAM) differ
      /// between the two.
      mutable ReduceComb: StepIndex -> unit
      mutable ReduceClocked: StepIndex -> unit
      /// Where this component sits in the build's one index space: the index it was stamped with
      /// as the flatten created it, and the index the link fields below are expressed in. Written
      /// once, by LookupArray.addItem, and never again.
      mutable Index: int
      /// This instance's outgoing links, by output port number, already resolved to store
      /// indices - so linking neither looks a design id up nor walks a map. Build scaffolding:
      /// linkFastComponents drops it, along with the two fields below, once it has used them, so
      /// that a built simulation does not carry a link table per component for the rest of its life.
      mutable OutLinks: (int * InputPortNumber) array array
      /// For a Custom component: the store index of the inner Input component each of its input
      /// ports maps to. Empty for every other type. Build scaffolding, dropped with OutLinks.
      mutable CustomInLinks: int array
      /// For an Output component INSIDE a custom component: the store index of that custom
      /// component, and which of its output ports this one is. -1 when there is no such link -
      /// a top-level Output, or any other type. Build scaffolding, dropped with OutLinks.
      mutable CustomOutIndex: int
      mutable CustomOutPort: int
      // these fields are used only to determine component ordering for correct evaluation
      mutable Touched: bool // legacy field
      mutable DrivenComponents: FastComponent list
      mutable NumMissingInputValues: int
      // these fields are used only by the Verilog output code
      mutable VerilogOutputName: string array
      mutable VerilogComponentName: string }
    /// WHICH INSTANCE of a sheet this component sits in.
    ///
    /// The access path is the identity, and always was: a dotted upper-cased path of custom
    /// component LABELS used to be stored beside it, computed from exactly this and from nothing
    /// else. That string had to be unique, which cost a collision hack for the case of a custom
    /// component labelled with the top sheet's own name, and it read as a sheet name without
    /// being one. The path is unique by construction, needs no casing, and cannot be mistaken for
    /// a name. Labels are for showing the user, and are worked out where they are shown.
    member inline this.Instance = InstancePath this.AccessPath

    /// Number of component inputs
    member inline this.InputWidth(n) = this.InputLinks[n].Width
    /// Number of component outputs
    member inline this.OutputWidth(n) = this.Outputs[n].Width
    /// Get the uint32 data array for a given input
    member inline this.GetInputUInt32 (epoch) (InputPortNumber n) = this.InputLinks[n].U32 epoch
    /// Get the BigInt data array for a given input
    member inline this.GetInputBigInt (epoch) (InputPortNumber n) = this.InputLinks[n].Big epoch
    /// Get the FData array for a given input
    member inline this.GetInputFData (epoch) (InputPortNumber n) = this.InputLinks[n].FDataStep[epoch]
    /// for debugging - get a short usually unique truncation of the fId
    member this.ShortId =
        let (ComponentId sid, ap) = this.fId
        string sid
    /// write data to the Unint32Step output array for the given time step (epoch) and output (n)
    member inline this.PutOutputUInt32 (epoch) (OutputPortNumber n) dat =
        this.Outputs[n].SetU32 epoch dat
    /// write data to the BigInt output store for the given time step (epoch) and output (n)
    member inline this.PutOutputBigInt (epoch) (OutputPortNumber n) dat =
        this.Outputs[n].SetBig epoch dat
    /// write data to the FData output array for the given time step (epoch) and output (n)
    member inline this.PutOutputFData (epoch) (OutputPortNumber n) dat =
        this.Outputs[n].FDataStep[ epoch ] <- dat
    member inline this.Id = this.SimComponent.Id
    /// The labels of the custom component instances this component sits within, outermost first.
    /// Empty for a component on the sheet being simulated. This is SheetName without the
    /// component's own label, so like it these are component labels, not sheet names.
    member inline this.SubSheet = this.SheetName[0 .. this.SheetName.Length - 2]

/// Convenience array used so that waveform simulation can access
/// component outputs (drivers) without a Map lookup
type Driver =
    {
      /// Index of this driver in the array of drivers
      Index: int
      /// Bus width of the driven bus
      DriverWidth: int
      /// Simulation data for the driven bus
      DriverData: IOArray }

/// Type used to tie component ports to simulation data
/// for advanved wavefor simulation features.
type SheetPort = {
    Sheet: string;
    PortOnComp: Port
    } // must include port number (which ports on connections do not)

/// What one clock cycle of a design costs in step arrays, kept apart by which memory it comes from.
///
/// The two are not interchangeable, and measurably so - though not in the way performance.memory
/// suggests. usedJSHeapSize counts Uint32Arrays at every size, so it cannot tell the two apart;
/// what separates them is the LIMIT. Uint32Array allocation ran to 15.5GB on a 32GB machine
/// against a jsHeapSizeLimit of 3.7GB, so those are bounded by the machine and not by V8's pointer
/// compression cage. Buses wider than 32 bits are held as a plain array of BigInt, which is
/// ordinary heap - 400MB of values cost 454MB - inside that 4GB cage, shared with the model, the
/// design and the waveforms. So a design can be refused for the second while nowhere near the first.
/// FastCreate.stepBytesForWidth works out one port's share; FastCreate.stepCostOfGraph totals it.
type StepCost =
    { /// Uint32Array storage, which the V8 heap limit does not bind
      TypedArrayBytes: int
      /// BigInt step arrays and the per-step state references, which it does
      HeapBytes: int }

    member this.TotalBytes = this.TypedArrayBytes + this.HeapBytes

/// How much memory a simulation may take, and of which kind.
///
/// Here rather than beside the code that spends it because two different parts of the simulator
/// spend it: GraphMerger, which expands the design into a graph, and FastCreate, which allocates
/// the step arrays. Both come out of the same memory and GraphMerger is compiled first.
///
/// Sizes are float and not int64 on purpose. Fable compiles int64 to BigInt, so every comparison
/// here would allocate one - a poor thing to spend on deciding whether a design is too big. A float
/// carries integers exactly to 2^53 and the largest number reached here is a few hundred GB.
module SimulationBudget =

    /// Share of the machine's physical memory a simulation's Uint32Arrays may take.
    ///
    /// A third, because that is comfortably clear of where the allocator actually gives up:
    /// measured on a 32GB machine, Uint32Array allocation failed at 15.5GB, a little under half of
    /// physical. A third leaves the operating system, the rest of Chromium and whatever else the
    /// user has open their room, and still allows several million cycles of a real design.
    let typedArrayShareOfMachine = 0.33

    /// Share of the V8 heap limit a simulation may take, for the expanded design and for the step
    /// arrays of buses wider than 32 bits.
    ///
    /// Not half, although half of USABLE heap is the intention. The heap limit is not usable in
    /// full: the scavenger needs its to-space free, mark-compact needs somewhere to evacuate pages
    /// to, and what a simulation puts there is millions of small objects promoted out of new space,
    /// which is the shape old space handles least well. Filling the cage makes the renderer stop
    /// responding well before the limit is reached. A third of the limit is about half of what can
    /// really be used.
    ///
    /// The two heap checks - the expanded design, and the step arrays - are made separately and
    /// each against this whole figure, so a design that passed both could in the worst case use
    /// twice it. That needs a design that is both deeply instantiated AND full of wide buses, and
    /// even then 70% of the limit is the boundary rather than past it.
    let heapShareOfLimit = 0.35

    /// What one component of an expanded design costs in heap, by how many ports it has (ports
    /// here is inputs plus outputs, as GraphMerger.expandedSize counts them).
    ///
    /// Measured rather than derived, on a hierarchy of 120,000 expanded components with one output
    /// port each and 2.9 ports each in all, by GC-forced usedJSHeapSize deltas at two step-array
    /// lengths (which separates the fixed cost from the per-step cost - the fixed part was the
    /// same at both lengths to within noise). What one waveform simulation retains, per component:
    ///
    ///   the SimulationGraph            ~15 bytes - merger shares non-custom nodes between
    ///                                  instances of a sheet, so this is two orders of magnitude
    ///                                  down on the rest
    ///   the FastComponents            ~990 bytes fixed - the 25-field record, its map node, the
    ///                                  output IOArrays with their typed-array wrappers (~120 B a
    ///                                  wrapper, measured), path strings, reducer closures
    ///   the Wave records               ~430 bytes a wave, at ~0.6 waves per port when this was
    ///                                  measured - the Wave record's strings, built by the
    ///                                  waveform simulator only, but charged here because this
    ///                                  guard cannot know which simulator is coming and the
    ///                                  waveform one is both the heavier and the one large designs
    ///                                  are opened in
    ///
    /// That last term is no longer spent. The waveform simulator described every wave the
    /// simulation offered and kept the lot in its model; it now describes the waves of the sheet
    /// instances the selector is drawing, as it draws them, and keeps records only for the ones
    /// SELECTED - at most maxAllowedViewerWaves, which is a hundred. So about 260 of the 350 bytes
    /// per port below is a cost that has gone, and this guard now refuses designs it has the memory
    /// for. The number is left where it is because it was MEASURED and the replacement has not
    /// been: subtracting a term from someone else's measurement is how a guard ends up on the wrong
    /// side. Remeasure and lower it.
    ///
    /// The step arrays themselves are NOT here - they scale with cycles, not components, and are
    /// StepCost's business. The formula sits ~15% above the measured total at 2.9 ports, which is
    /// the right side to miss on for a guard. To remeasure after changing these structures: build
    /// phase deltas are logged under --log=perf (FastBuild.buildFastSimulation and the defaultWaves
    /// line), and `node scripts/drive.js` + window.issieDev.simStats() gives the exact component,
    /// port and wave counts to divide by.
    let heapBytesPerComponent (ports: float) = 1000.0 + 350.0 * ports

    /// The most Uint32Array step-array memory one simulation may take: memory outside the V8 heap,
    /// so bounded by the machine rather than by anything Issie is built with. Most designs are 32
    /// bits and under, so this is the budget that decides how long they may run.
    ///
    /// Mutable because it is a fact about the machine, discovered once at startup - see
    /// setBudgetsFromMachine. The value here is the fallback for when there is no machine to ask,
    /// which is every run of the test suite: those run under plain .NET with no Electron.
    let mutable maxTypedArrayBytes = 2.0e9

    /// The most V8 heap one simulation may take, for the expanded design and for the BigInt step
    /// arrays alike. Far smaller than the budget above, because V8's pointer compression caps the
    /// whole heap at 4GB - a limit no flag can lift, since raising it needs V8 built without
    /// pointer compression - and the model, the design, the waveforms and everything else the
    /// renderer holds come out of that same 4GB.
    let mutable maxHeapBytes = 1.0e9

    /// How far past a budget the runtime memory check (FastCreate.checkSimulationFits) lets a
    /// simulation go before refusing to build it.
    ///
    /// The budgets are deliberately conservative - a third of physical where allocation was
    /// measured to fail just under half, and 0.35 of a heap limit of which roughly 0.7 is
    /// usable - so there is real margin between "past the budget" and "will crash". The
    /// configuration dialog holds users to the budgets exactly (FastCreate.maxLastClockFor);
    /// this headroom exists for the simulation that arrives WITHOUT passing that dialog - a
    /// LastClock saved into a sheet on a machine with more memory than this one, most commonly -
    /// which should run if it safely can, not be refused over a bound it only just misses. At
    /// 1.5 the worst case stays at half of physical for the typed arrays and just over half of
    /// the heap limit for the BigInt arrays: inside the measured margin on both.
    let runtimeHeadroom = 1.5

    /// Size both budgets to the machine this is running on. Called once from renderer startup.
    ///
    /// physicalBytes comes from process.getSystemMemoryInfo, heapLimitBytes from
    /// performance.memory.jsHeapSizeLimit - the limit actually in force, whatever Main.fs asked for
    /// and whatever V8 decided to grant. Either being zero or absent leaves that budget at its
    /// fallback, so a machine that cannot answer is never told it has no memory.
    let setBudgetsFromMachine (physicalBytes: float) (heapLimitBytes: float) =
        if physicalBytes > 0.0 then
            maxTypedArrayBytes <- physicalBytes * typedArrayShareOfMachine
        if heapLimitBytes > 0.0 then
            maxHeapBytes <- heapLimitBytes * heapShareOfLimit

    /// A size in bytes, written the way a message should read it.
    let formatBytes (bytes: float) =
        let gb = bytes / 1024.0 ** 3.0
        let mb = bytes / 1024.0 ** 2.0
        if gb >= 1.0 then $"%.1f{gb} GB"
        elif mb >= 1.0 then $"%.0f{mb} MB"
        else $"%.0f{bytes / 1024.0} KB"

/// What a run of a fast simulation did.
///
/// Not a rate. It used to return cycles per millisecond, with None meaning both "nothing to do" and
/// "finished" - so a caller could not tell those apart, and the one caller that used the number
/// divided by it to guess how long the rest would take. Guessing elapsed time from work done is
/// what breaks when a machine sleeps mid-run: the rate collapses towards zero and instant work is
/// predicted to take minutes. This says what happened and nothing more.
type RunOutcome =
    /// the clock reached the cycle asked for
    | RunCompleted
    /// the time budget ran out first, with the clock here. Running again continues from it
    | RunStoppedAt of clock: int

// The fast simulation components are similar to the issie components they are based on but with addition of arrays
// for direct lookup of inputs and fast access of outputs. The input arrays contain pointers to the output arrays the
// inputs are connected to, the InputPortNumber integer indexes this.
// In addition outputs are contained in a big array indexed by epoch (simulation time). This allows results for multiple
// steps to begin built efficiently and also allows clocked outputs for the next cycle to be constructed without overwriting
// previous outputs needed for that construction.
//
// For reasons of efficiency Issie's list-style WireData type is optimised by using integers as bit arrays.
//
// For ease of implementation Input and Output components are given a single output (input) port not present on issie.
// this allows sub-sheet I/Os to be linked as normal in the constructed graph via their respective Input and Output connections.
//
// Although keeping input and output connections in the new graph is slightly less efficient it makes things simpler because there is a
// 1-1 connection between components (except for custom components which are removed by the gathering process).
// Note that custom component info is still kept because each component in the graph has a path - the list of custom component ids
// between its graph and root. Following issie this does not include a custom component for the sheet being simulated, which is viewed as
// root. Since custom components have been removed this no longer complicates the simulation.
type FastSimulation =
    {
        /// last step number (starting from 0) which is simulated.
        mutable ClockTick: int
        /// Maximum size of simulation arrays - after which they form a circular buffer
        MaxArraySize: int
        /// top-level inputs to the simulation
        FGlobalInputComps: FastComponent array
        /// constants
        FConstantComps: FastComponent array
        /// clocked components
        FClockedComps: FastComponent array
        /// Components that will be reduced in order allowing sequential reduction to implement simulation
        FOrderedComps: FastComponent array
        /// which is the active component for each set of labels?
        mutable FIOActive: Map<ComponentLabel * ComponentId list, FastComponent>
        /// list of deferred links driven from inactive IOlabls - at end of linkage the
        /// corresponding active IOLabel can be substituted as driver an dthe link made
        mutable FIOLinks: ((FastComponent * InputPortNumber) * FastComponent) list
        /// Fast components: this array is longer than FOrderedComps because it contains
        /// IOlabel components that are redundant in the simulation.
        /// It doe snot contain custom Components
        FComps: Map<FComponentId, FastComponent>
        /// Custom Components.
        FCustomComps: Map<FComponentId, FastComponent>
        /// Fast components: this map is longer than FComps because it contains
        /// Custom components not used by Fast simulation but needed in Waveform simulation
        WaveComps: Map<FComponentId, FastComponent>
        /// look up from output port of custom component to the relevant Output component
        FCustomOutputCompLookup: Map<(ComponentId * ComponentId list) * OutputPortNumber, FComponentId>
        /// Total number of step arrays (= drivers)
        NumStepArrays: int
        /// Each driver represents one output with its data
        Drivers: Driver option array
        /// Each wave index represents one component port with associated driver and data
        WaveIndex: WaveIndexT array
        /// Connections on simulated sheets indexed by directly connected port. Each connection appears twice.
        ConnectionsByPort: Map<SheetPort, Connection list>
        /// This contains all components in the sheets of the simulation indexed by ComponentId.
        /// Subsheet components are indexed only once.
        /// Contrast this with Fast Components - which have the design expanded out with
        /// one per instance: the different versions correspond to distinct access paths.
        ComponentsById: Map<string, Map<ComponentId, Component>>
        /// Circuit simulated (sheet and all dependencies)
        SimulatedCanvasState: LoadedComponent list
        /// The root sheet being simulated
        SimulatedTopSheet: string

        /// What one clock cycle of this design costs in step arrays. Worked out before the arrays
        /// were allocated, by FastCreate.stepCostOfGraph, and kept so that the waveform
        /// simulator's configuration can say what a given number of cycles would come to.
        StepCost: StepCost
    } with

    /// The custom component an instance is the innards of, or None for the top sheet.
    ///
    /// A map from instance to parent used to be built and stored for this. It is a step off the
    /// end of the path: the last id on the path IS the custom component, and what it sits in is
    /// the rest of the path. One lookup, nothing to keep in step, and one entry per sheet INSTANCE
    /// no longer allocated at build time - tens of thousands of them on a design that expands.
    member this.parentCustomOf(InstancePath ap) : FastComponent option =
        match List.tryLast ap with
        | None -> None
        | Some cid -> Map.tryFind (cid, ap[0 .. ap.Length - 2]) this.FCustomComps

    /// The design-time name of the sheet an instance is of: what the user called it, and what they
    /// see in the Sheets menu. An instance is a path; this says which sheet it is a copy of.
    member this.getSheetNameOfInstance(instance: InstancePath) =
        match this.parentCustomOf instance with
        | Some fc ->
            match fc.FType with
            | Custom ct -> ct.Name
            | _ -> this.SimulatedTopSheet
        // no parent means the top sheet, which is the one instance named after its sheet
        | None -> this.SimulatedTopSheet

 

/// Scaffolding for building a FastSimulation, and alive only while one is built.
///
/// It used to be four `Map`s: the flattened design indexed the several ways the phases after the
/// flatten needed it, all of them keyed structurally by (ComponentId, access path). Every one of
/// those keys cost a boxed comparison per tree level of every lookup, and the build does millions
/// of lookups - a measured fifth of a 480,000-component build went on one of them. They are now a
/// single index space instead: the flatten creates each FastComponent, stamps it with its position
/// in `Comps`, and expresses every link it finds as those indices. Nothing here is keyed by
/// anything but an int, and the `Map`s a built simulation offers the rest of the program
/// (FComps, FCustomComps, FCustomOutputCompLookup) are made once at the end, from this.
///
/// One store and one index space, holding the FastComponents themselves: custom against ordinary
/// is a PREDICATE over it, never a second store. Splitting them is the obvious tidy-up and it is
/// what would break this - the indices the links carry would then mean two different things.
///
/// Deliberately not kept after the build. It holds a SimulationComponent per component INSTANCE
/// through the FastComponents it stores, so on a large design it is one of the biggest things the
/// simulator ever allocates, and a FastSimulation left holding one made every later edit slower by
/// giving each major GC all of it to trace.
and GatherData =
    {
      /// Every component of the expanded design, one entry per INSTANCE, in the order the flatten
      /// visited them - which is the order that assigns step-array indices, and so drivers and
      /// wave indices. Custom components are included.
      Comps: LookupArray.LookupArray<FastComponent>
      /// Shortcut to find the label of a component, indexed by the component's DESIGN id.
      /// An array and not a map because design ComponentIds are allocated densely from 1
      /// (CommonTypes.ComponentId), and getFullSimName below does one lookup per element of the
      /// access path for every component of the expanded design - millions of them on a design
      /// that expands, each one a boxed comparison per tree level as a Map. An id the design does
      /// not have reads as "*", which is what the Map gave for a key it did not hold.
      /// Notice that the access path is not needed here because labels of the graph inside a
      /// custom component are identical for different instances of the component.
      /// Mutable because the flatten grows it as it meets ids: the largest design ComponentId is
      /// not known until the whole design has been walked, and walking it twice to find out would
      /// cost more than the growth does.
      mutable Labels: string array
     }
    /// The label of one design component, or "*" for an id the design does not have.
    member this.labelOf(ComponentId i) =
        if i >= 0 && i < this.Labels.Length then this.Labels[i] else "*"

    /// human readable dot-separated name of component in simulation.
    /// This uses the component labels to the root of the simulation and therefore is unique.
    member this.getFullSimName ((cid, ap):FComponentId) =
        List.map (fun cid -> this.labelOf cid) (ap @ [ cid ])
        |> String.concat "."

    /// The same path as getFullSimName, upper-cased and as a list rather than dot-separated.
    /// These are component labels, not sheet names: it becomes FastComponent.SheetName, whose
    /// name is misleading.
    member this.getFullSimPath((cid, ap):FComponentId) =
        List.map (fun cid -> (this.labelOf cid).ToUpper()) (ap @ [ cid ])




/// - Top level data tracking a simulation
type SimulationData =
    { FastSim: FastSimulation
      Graph: SimulationGraph
      // For each input/output, keep its Id and Label to easily access it.
      Inputs: SimulationIO list
      Outputs: SimulationIO list
      // Whether the graph contains synchronous logic.
      IsSynchronous: bool
      // The base that should be used to display numbers in the simulation.
      NumberBase: NumberBase
      // Keep track of the number of clock ticks of the simulation.
      ClockTickNumber: int }

let graph_ = Optics.Lens.create (fun a -> a.Graph) (fun s a -> {a with Graph = s})
let fastSim_ = Optics.Lens.create (fun a -> a.FastSim) (fun s a -> {a with FastSim = s})
let numberBase_ = Optics.Lens.create (fun a -> a.NumberBase) (fun s a -> {a with NumberBase = s})
let clockTickNumber_ = Optics.Lens.create (fun a -> a.ClockTickNumber) (fun s a -> {a with ClockTickNumber = s})

/// document current status of a simulation as used by waveform simulator
type SimulationRunStatus =
    | SimEmpty // simulation has been created but not yet setup from a circuit
    | SimOutOfDate // one of more of the sheets being simulated has changed after the simulation was setup
    | SimValidSameSheet
    /// simulation has run and is currently uptodate. The current sheet is being simulated
    | SimValidDifferentSheet // The simulation is uptodate, but a differnt sheet from the current one is being simulated
    | SimNoProject // there is no open project - this should not normally happen.


//-------------------------------------------------------------------------------------//
//-------------------Helper functions for simulation types-----------------------------//
//-------------------------------------------------------------------------------------//

let sprintSimComponent (sComp: SimulationComponent) =
    sprintf "'%A': %20s" sComp.Label (sComp.Type.ToString() |> Helpers.sprintInitial 20)

let shortPSComp (comp: SimulationComponent) =
    let lab =
        match comp.Label with
        | ComponentLabel lab' -> lab'

    match comp.Type with
    | Custom sc -> sprintf "%s:Custom.(%s.%A->%A)" lab sc.Name sc.InputLabels sc.OutputLabels
    | _ -> sprintf "%s:%A" lab comp.Type

let tryGetCompLabel (compId: ComponentId) (sg: SimulationGraph) =
    Map.tryPick (fun k v -> if k = compId then Some v else None) sg
    |> Option.map (fun comp -> comp.Label)
    |> Option.map (fun (ComponentLabel s) -> s)
    |> Option.defaultValue "'Not in SimGraph'"

let extractLabel (label: ComponentLabel) =
    let (ComponentLabel name) = label
    name

//-------------------------------------------------------------------------------------//
//-------------------Helper functions for WaveformSim----------------------------------//
//-------------------------------------------------------------------------------------//

// NB - all the NetGroup functions assume a working netlist in which NO NET IS UNDRIVEN
// Every Net must be driven by exactly one componnet output port (NLSource).
// IOLabels are nout counted as drivers themselves; every group of same label IOlabels
// and all of their output nets
// makes a netgroup which must be driven by just one NLSource (connected to one of the IOLabel inputs).
// every net is therefore part of one netgroup which is either a single net, or a group of nets associated
// with a set of IOLabel connectors having a given common label.

let mapKeys (map: Map<'a, 'b>) = Map.keysA map
let mapValues (map: Map<'a, 'b>) = Map.valuesA map
let mapItems (map: Map<'a, 'b>) = Map.toArray map


