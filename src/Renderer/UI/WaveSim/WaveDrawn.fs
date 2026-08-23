/// What each waveform on screen is, and what it was drawn from.
///
/// A drawn waveform is a pure function of the data for one signal over one window and a handful of
/// display settings - so it does not belong in the model, and the model no longer holds it. What is
/// kept here is the memo of that function: the SVG last made for each waveform, with the exact
/// inputs it was made from. The view asks for the waveform it wants and gets it back without making
/// it twice, and NOTHING here is consulted to decide what should happen next.
///
/// Two things make the memo more than an optimisation.
///
/// The first is the fallback. Where a signal's data has not arrived - the window moved and the
/// answer is still on the wire - what is kept is what is on screen, and keeping it is better than
/// emptying the row: waveforms a moment out of date are what a viewer over a wire looks like, a
/// viewer that blanks itself on every scroll is what a broken one looks like. The stored Spec says
/// which view the row is actually showing, which is how anything that has to agree with the screen
/// - the hover tooltip - can be made to agree with it rather than with the controls.
///
/// The second is that a render costs nothing when nothing changed. Issie renders the whole app on
/// every message and does not memoise components, so this function is called for every waveform on
/// every keystroke; without the memo each of those would rebuild a hundred SVGs.
///
/// Module state, in the sense docs/mutableState.md allows: it is not model state written somewhere
/// else, and no decision reads it. Discard it whenever the simulation it was drawn from is
/// replaced - a driver index means something different in the next build, and a stale hit would
/// draw the old design's signal under the new design's name.
module WaveDrawn

open Fable.React
open CommonTypes
open ModelType
open SimGraphTypes

/// Everything a drawn waveform is a function of.
///
/// Two waveforms with equal specs are the same picture, so this is what the memo is keyed on. It is
/// not a description of the waveform for anyone to read: it exists to be compared.
///
/// `Config` is carried whole rather than picked apart because the drawing reads the font from it,
/// and a field added there that the drawing then uses would otherwise silently stop invalidating
/// the memo. Everything else is named individually because it is not all in one place.
type WaveSpec =
    { /// which signal: the index of its driver in the simulation
      Driver: int
      Width: int
      /// which cycles, at what sampling: see WaveSlice.Window
      Window: WaveSlice.Window
      Radix: NumberBase
      /// pixels across the whole waveform column, which with ShownCycles sets the cycle width
      ColumnWidth: float
      Config: WSConfig }

/// One waveform as it is on screen.
type Drawn =
    { Spec: WaveSpec
      Svg: ReactElement
      /// where the value printed on the wave was left out for want of room, which is what the hover
      /// tooltip is for. Made while drawing, because that is when it is known.
      Gaps: GapStore
      /// the samples this picture was drawn from, kept so that anything describing what is on
      /// screen - the value column, the hover tooltip - reads the same numbers the pixels came
      /// from. Not a copy: it points into the array the fetch delivered, which the cache may have
      /// replaced with a newer window since. That is exactly why it is kept here rather than looked
      /// up again.
      Samples: WaveSlice.WaveSlice }

/// The waveform drawn for each driver.
///
/// One entry per row on screen - the viewer draws at most a hundred - and pruned to the selection
/// whenever the viewer refreshes.
let mutable private drawn: Map<int, Drawn> = Map.empty

/// What the viewer is asking for, for one wave, right now.
let specOf (ws: WaveSimModel) (wave: Wave) : WaveSpec =
    { Driver = wave.DriverIndex
      Width = wave.Width
      Window =
        { StartSample = ws.StartCycle
          Multiplier = ws.SamplingZoom
          SampleCount = ws.ShownCycles }
      Radix = ws.Radix
      ColumnWidth = ws.WaveformColumnWidth
      Config = ws.WSConfig }

/// What is on screen for one wave, if anything ever has been.
let tryDrawn (driver: int) = Map.tryFind driver drawn

let put (d: Drawn) = drawn <- Map.add d.Spec.Driver d drawn

/// Forget the waveforms of waves no longer selected. Called from the refresh, which is where the
/// selection is settled.
let keepOnly (drivers: Set<int>) =
    drawn <- drawn |> Map.filter (fun driver _ -> Set.contains driver drivers)

/// Forget everything. Called when the simulation is replaced: a driver index names a different
/// signal in the next build, so every entry here is now a picture of something else.
let forget () = drawn <- Map.empty

/// The value of one drawn waveform at one of its samples, or None outside it.
///
/// Indexed by SAMPLE within the picture rather than by clock cycle, because that is what "under the
/// cursor" means: the cursor is drawn at a column of the waveform, and the value beside it should
/// be the one that column was drawn from. While the picture is a window behind the controls - the
/// data for the view asked for is still on its way - reading it by absolute cycle instead says
/// nothing at all, which is where the value column's row of "?" beside perfectly good waveforms
/// came from.
let valueAtSample (d: Drawn) (sample: int) =
    if sample < 0 || sample >= d.Spec.Window.SampleCount then
        None
    else
        let v = WaveSlice.sampleValue d.Samples sample

        Some(
            if d.Spec.Width > 32 then
                { Dat = BigWord v; Width = d.Spec.Width }
            else
                { Dat = Word(uint32 v); Width = d.Spec.Width }
        )

/// How many of these waves are showing a view other than the one the controls ask for.
///
/// Zero at rest. Not zero for a moment after the window moves, which is the fallback working; not
/// zero for a long time means data that was asked for is not arriving.
let staleCount (ws: WaveSimModel) (waves: Wave list) =
    waves
    |> List.filter (fun wave ->
        match tryDrawn wave.DriverIndex with
        | Some d -> d.Spec <> specOf ws wave
        | None -> false)
    |> List.length
