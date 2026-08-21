/// One waveform over one view window: what the waveform viewer asks for, draws from, and - when
/// the simulator is the .NET sidecar - fetches across the wire.
///
/// **Why a slice rather than an array.** The viewer never wants a whole waveform. It wants
/// `ShownCycles` samples taken every `SamplingZoom` cycles from `StartCycle`, which is exactly
/// what `SidecarClient.simRead` asks the sidecar for and exactly what the local step arrays can
/// be read as. Making that window a value with a name gives the two simulators one shape to
/// meet in: a slice from the local `FastSimulation` and a slice fetched from the sidecar are
/// the same thing to everything downstream, so the drawing code neither knows nor cares which
/// simulator ran.
///
/// **Why the drawing code goes through the operations here and not the array.** What a waveform
/// actually needs is its *transitions* - where the value changes - which is a run-length
/// encoding of the samples in all but name. Today a slice holds dense samples and the
/// operations below compute runs from them; the intended next step is for the sidecar to send
/// runs, for a slice to hold runs, and for these same operations to return them with no work at
/// all. That change should not reach the SVG code, so the SVG code must not index samples
/// itself. Point access (a cursor value, a hover tooltip) is a handful of calls per render and
/// can afford to go through a function; the drawing path asks for transitions once per
/// waveform.
///
/// **Reading costs nothing extra.** A slice does not copy: it names the array its samples
/// already live in, where sample 0 starts, and how far apart samples are. Local step arrays are
/// read with a stride of the sampling multiplier; a sidecar response is already sampled, so its
/// stride is one and its base is the start of that signal's row in the reply.
module WaveSlice

open SimTypes

/// The window a slice covers: `SampleCount` samples, one every `Multiplier` clock cycles,
/// the first at clock cycle `StartSample * Multiplier`.
///
/// **StartSample counts samples, not clock cycles**, which is the waveform simulator's own
/// convention and the easiest thing here to get wrong: `WaveSimModel.StartCycle` is a DISPLAY
/// position, and the cycle it refers to is `StartCycle * SamplingZoom` - as
/// `lastCycleNeeded = (ShownCycles + StartCycle) * SamplingZoom` and
/// `CursorExactClkCycle = CursorDisplayCycle * SamplingZoom` both attest. The field is named
/// for what it counts so that a reader has to notice.
type Window =
    { StartSample: int
      Multiplier: int
      SampleCount: int }

    /// the clock cycle sample i of this window is taken at
    member inline this.CycleOfSample(i: int) = (this.StartSample + i) * this.Multiplier

    /// the clock cycle the window starts at
    member inline this.FirstCycle = this.StartSample * this.Multiplier

    /// the last clock cycle the window needs the simulation to have reached
    member inline this.LastCycle = this.CycleOfSample(this.SampleCount - 1)

/// Where a slice's samples live. Dense today - see the note above on runs. `Base` is the index
/// of sample 0 and `Stride` the distance between samples, so sample i is at Base + i*Stride:
/// local step data is read in place at the sampling multiplier, and a sidecar reply, already
/// sampled, at a stride of one.
type Samples =
    | Words of data: uint32 array * Base: int * Stride: int
    | Bigs of data: bigint array * Base: int * Stride: int

type WaveSlice =
    { Window: Window
      /// bus width, which decides which of the two sample stores is in use
      Width: int
      Samples: Samples
      /// whether the sample one step BEFORE the window is also readable (index -1).
      ///
      /// A binary waveform's first drawn cycle needs the value before it to know whether it
      /// opens with a transition. Where the window starts at cycle 0 there is no such value and
      /// the first sample stands in for it, which is what the viewer has always drawn.
      HasLeadIn: bool }

/// Sample i as a uint32, for a slice of width <= 32. Index -1 is the lead-in sample, which is
/// sample 0 where the window starts at cycle 0.
let inline wordAt (slice: WaveSlice) (i: int) : uint32 =
    match slice.Samples with
    | Words(data, b, stride) ->
        let i = if i < 0 && not slice.HasLeadIn then 0 else i
        data[b + i * stride]
    | Bigs _ -> failwithf "wordAt on a slice of width %d, which holds bigints" slice.Width

/// Sample i as a bigint, for a slice of width > 32.
let inline bigAt (slice: WaveSlice) (i: int) : bigint =
    match slice.Samples with
    | Bigs(data, b, stride) ->
        let i = if i < 0 && not slice.HasLeadIn then 0 else i
        data[b + i * stride]
    | Words _ -> failwithf "bigAt on a slice of width %d, which holds words" slice.Width

/// Sample i whatever the width, for the value readouts - the cursor column and the hover
/// tooltip - which want a number and not a representation.
let sampleValue (slice: WaveSlice) (i: int) : bigint =
    match slice.Samples with
    | Words _ -> bigint (wordAt slice i)
    | Bigs _ -> bigAt slice i

/// A slice over a driver's local step data. Nothing is copied: the step arrays are read where
/// they lie, at the sampling stride.
///
/// `None` when the window is not inside what the simulation holds, which is the caller's cue
/// that the simulation has to run further first - the same condition the old direct reads
/// expressed by returning an empty array of transitions.
let ofLocalDriver (io: IOArray) (window: Window) : WaveSlice option =
    let leadIn = window.StartSample > 0
    let firstNeeded = if leadIn then window.FirstCycle - window.Multiplier else window.FirstCycle

    if
        window.SampleCount <= 0
        || window.Multiplier < 1
        || firstNeeded < 0
        || window.LastCycle >= io.StepLength
    then
        None
    else
        let samples =
            if io.UInt32Slab.Length > 0 then
                Words(io.UInt32Slab, io.StepBase + window.FirstCycle, window.Multiplier)
            else
                Bigs(io.BigIntSlab, io.StepBase + window.FirstCycle, window.Multiplier)

        Some
            { Window = window
              Width = io.Width
              Samples = samples
              HasLeadIn = leadIn }

/// A slice over one signal's row of a sidecar reply, which is already sampled - so a stride of
/// one, and a base of where that row starts. `leadIn` says whether the fetch asked for the
/// extra sample before the window, in which case the row's first entry is that one and sample 0
/// is the next.
let ofFetchedWords (data: uint32 array) (rowBase: int) (width: int) (window: Window) (leadIn: bool) : WaveSlice =
    { Window = window
      Width = width
      Samples = Words(data, (if leadIn then rowBase + 1 else rowBase), 1)
      HasLeadIn = leadIn }

// ---------------------------------------------------------------------------------------------
// Transitions: what a waveform is drawn from. These are the run-length encoding the note at the
// top is about - today computed from dense samples, later expected to arrive as runs.
// ---------------------------------------------------------------------------------------------

/// Whether a binary waveform changes at the start of a clock cycle, and what it changes between.
type BinaryTransition =
    | ZeroToZero
    | ZeroToOne
    | OneToZero
    | OneToOne

/// Whether a non-binary waveform changes value at the start of a clock cycle.
type NonBinaryTransition =
    | Change
    | Const

/// The transitions of a binary (one-bit) waveform, one per drawn cycle.
let binaryTransitions (slice: WaveSlice) : BinaryTransition array =
    Array.init slice.Window.SampleCount (fun i ->
        match wordAt slice (i - 1) &&& 1u, wordAt slice i &&& 1u with
        | 0u, 0u -> ZeroToZero
        | 0u, 1u -> ZeroToOne
        | 1u, 0u -> OneToZero
        | _ -> OneToOne)

/// The transitions of a wider waveform, with the sampled values beside them - the values are
/// what the viewer writes into each run, so they come back together rather than being read
/// twice.
let nonBinaryTransitionsWords (slice: WaveSlice) : NonBinaryTransition array * uint32 array =
    let values = Array.init slice.Window.SampleCount (wordAt slice)

    let transitions =
        Array.init values.Length (fun i ->
            if i = 0 then Change
            elif values[i] = values[i - 1] then Const
            else Change)

    transitions, values

/// nonBinaryTransitionsWords for a slice wider than 32 bits.
let nonBinaryTransitionsBigs (slice: WaveSlice) : NonBinaryTransition array * bigint array =
    let values = Array.init slice.Window.SampleCount (bigAt slice)

    let transitions =
        Array.init values.Length (fun i ->
            if i = 0 then Change
            elif values[i] = values[i - 1] then Const
            else Change)

    transitions, values
