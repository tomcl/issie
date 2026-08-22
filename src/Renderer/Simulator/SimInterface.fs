/// What the waveform viewer and the step simulator ask of a simulator, and what a simulator
/// promises them - whichever process it runs in.
///
/// **The layering, because it is the point of this file.**
///
/// ```
///   view code  --reads-->  the cache (synchronous)  <--fills--  ISimulator (asynchronous)
///                                                                 |- in the renderer
///                                                                 '- in the .NET sidecar
/// ```
///
/// The UI talks only to the cache and never awaits, because `getWaveValue`, the cursor column,
/// the hover tooltip, the schematic probe and the whole step panel are called from inside `view`.
/// Only the update function calls an `ISimulator`, and only to fill that cache.
///
/// **Why this interface is narrow and the old one was not.** What the renderer uses today is not
/// an interface at all: it is field access into a `FastSimulation` - `FComps`, `Drivers`,
/// `WaveIndex`, `WaveComps`, `SimSheetStructure`. Every one of those is proportional to the
/// EXPANDED simulation, so promoting them to an interface would oblige a remote simulator to
/// rebuild all of it renderer-side, which is the cost this exists to remove. So the interface
/// asks small questions - which instances are inside this one, which ports does this instance
/// offer, read me this window - and a `FastSimulation` becomes the private business of the
/// implementation that has one.
///
/// **A local implementation costs nothing.** Its cache is read-through: `WaveSlice.ofLocalDriver`
/// names a step array where it lies rather than copying it, so "filling the cache" is recording
/// references. The only thing that differs between implementations is what a MISS does - a local
/// one reads through and always hits, so the UI never sees "not yet", while a remote one answers
/// None and schedules a fetch. That is what keeps one code path in the UI with no
/// `SimulateInRenderer` fork in view code.
///
/// **Widths come from here and nowhere else.** Parameters are resolved when a design is
/// elaborated and can change widths, so a width is a fact about the elaborated INSTANCE, not
/// about the sheet it instantiates. The renderer must never infer one from a design.
module SimInterface

open Fable.Core
open CommonTypes
open SimGraphTypes

/// One elaborated instance of a sheet, as the selector needs to show it.
///
/// DESIGN-sized: one of these per instance the selector is DRAWING, never one per instance the
/// design expands to. The distinction is the whole memory argument - a design that expands to
/// 49,152 copies of a sheet still only ever draws a handful of them at once.
type SimInstance =
    { InstId: SimSheetId
      /// the design-time sheet this instantiates
      InstSheet: SheetName
      /// the label on the custom component above it, for display
      InstLabel: string
      InstParent: SimSheetId option }

/// One port of one elaborated component that a waveform can be taken of.
///
/// Carries its own width AND its own labels. The width because of elaboration (see the module
/// note). The labels because deriving them needs the simulation too: an IOLabel's name and width
/// come from which member of a same-named group actually drives the net, which is a fact about
/// the built simulation and does not exist in the renderer when the sidecar is simulating.
///
/// `PortCompType` rather than the UI's `ComponentGroup`: that type is declared in ModelType,
/// which compiles long after this, and classifying a component is a pure function of its type.
type SimPort =
    { PortSignal: SignalId
      PortWidth: int
      PortCompLabel: string
      PortLabel: string
      PortCompType: ComponentType }

/// A top-level input or output of the simulated sheet, with its value at one cycle.
type SimIo =
    { IoComp: ComponentId
      IoLabel: string
      IoWidth: int
      IoValue: bigint }

/// A Viewer anywhere in the hierarchy, with its value at one cycle.
type SimViewer =
    { VwLabel: string
      /// the full dotted path, for the tooltip
      VwFullName: string
      VwWidth: int
      VwValue: bigint }

/// One clocked component's state at one cycle, for the step simulator's state pane.
type SimStateEntry =
    { StName: string
      StComp: SimComponentId
      StState: SimulationComponentState }

/// Everything the step simulator's panel shows at one cycle.
///
/// One value, because it is fetched in one request. The panel currently re-reads inputs, outputs,
/// viewers and state separately on every render, from inside `view`; over a wire that would be
/// four round trips per repaint, so the unit of transfer is the panel and not the field.
type StepPanelSnapshot =
    { SpCycle: int
      SpInputs: SimIo list
      SpOutputs: SimIo list
      SpViewers: SimViewer list
      SpStateful: SimStateEntry list }

/// A simulator, wherever it runs.
///
/// Every member is asynchronous because one implementation is a separate process. An in-renderer
/// implementation returns already-resolved promises, which costs a microtask and buys one code
/// path in the caller.
///
/// Not yet here, and deliberately: reading a RAM's contents, which needs a row type that is
/// declared in the waveform UI and has to move first; and the simulation errors a build can
/// return, which are threaded through as they are today.
type ISimulator =

    // ---- lifecycle ----

    /// Bumped by every Build. Anything cached or in flight carries the epoch it belongs to, so an
    /// answer from a superseded build is discarded rather than shown.
    abstract Epoch: SimEpoch

    /// Build a simulation of `top` from `design`, sized for `arraySize` cycles - or keep the one
    /// already built if it is of the same thing.
    ///
    /// The implementation KEEPS `design`. Instance enumeration is answered from it, which is what
    /// lets the renderer stop building a simulation of its own: which instances exist is a
    /// design-time fact, and only widths and data need the elaborated one.
    abstract Build: design: LoadedComponent list * top: SheetName * arraySize: int -> JS.Promise<Result<unit, SimulationError>>

    /// Advance towards `cycle`, in chunks, reporting the clock reached after each so that a
    /// progress bar can move and a Cancel can be answered.
    abstract RunTo: cycle: int * onProgress: (int -> unit) -> JS.Promise<Result<int, string>>

    /// Drop the simulation and whatever it holds.
    abstract Release: unit -> JS.Promise<unit>

    // ---- enumeration: design-sized ----

    abstract TopInstance: SimSheetId

    /// The instances of `sheet` directly inside `parent`. Answered from the design, so it costs
    /// one pass over a sheet's own components however many times that sheet is instantiated.
    abstract InstancesInside: parent: SimSheetId * sheet: SheetName -> JS.Promise<SimInstance list>

    abstract SheetOfInstance: SimSheetId -> JS.Promise<SheetName option>

    // ---- what can be watched, with widths ----

    /// The ports of several instances at once, because the selector draws a whole collapsed
    /// hierarchy and one round trip for it is the difference between a pause and a stutter.
    abstract PortsOfInstances: SimSheetId list -> JS.Promise<Map<SimSheetId, SimPort list>>

    /// The ports of one canvas component, and how many elaborated copies the simulation holds -
    /// the question the schematic's right-click menu asks.
    abstract PortsOfCanvasComp: ComponentId -> JS.Promise<SimPort list * int>

    // ---- handles ----

    /// Turn stable signal names into this build's read handles, with each signal's width.
    ///
    /// A signal named on an INPUT port resolves to the handle of the OUTPUT driving it: a net has
    /// one driver, and it is the driver that has data. Doing that here rather than in the caller
    /// is what lets a remote simulator answer at all, since the mapping needs the elaborated
    /// simulation.
    abstract HandlesFor: SignalId list -> JS.Promise<(SignalId * SignalHandle * int) list>

    // ---- reading: every one of these fills the cache ----

    /// The samples the current view draws, for these signals, plus the cursor column - which is
    /// separate because the drawn window samples every `Multiplier` cycles while the cursor sits
    /// on an exact one.
    abstract ReadWindow: SignalHandle list * window: WaveSlice.Window * cursorCycle: int -> JS.Promise<Result<unit, string>>

    /// One signal at one cycle - a hover tooltip, or the schematic probe.
    abstract ReadPoint: SignalHandle * cycle: int -> JS.Promise<Result<unit, string>>

    // ---- the step simulator ----

    abstract SetInput: comp: ComponentId * value: bigint * cycle: int -> JS.Promise<Result<unit, string>>

    /// Everything the step panel shows at one cycle, in one request.
    abstract ReadStepPanel: cycle: int -> JS.Promise<Result<unit, string>>
