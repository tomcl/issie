/// The one simulation session the sidecar holds, and the renderer's picture of it.
///
/// The sidecar simulates one design at a time - `SimSession` keeps a single session and every
/// build replaces it - so there is one place here that knows what it is holding, rather than one
/// per feature. The waveform simulator and the step simulator both draw on this, and because they
/// share it neither can build over the other's session without the other finding out: a command
/// naming a session the sidecar no longer holds is refused by name.
///
/// This module is deliberately below the UI. It was extracted from `WaveProvider` when the step
/// simulator needed the same two operations, and `SimulationView` compiles long before that.
///
/// **None of this is model state.** It is what a separate process is believed to hold, which the
/// model cannot know and has no better place for (docs/mutableState.md). WHICH simulator is
/// running IS a model fact - `Model.SimulateInRenderer` - and is passed to the callers rather
/// than mirrored here, because a second copy of a model fact is a thing that can disagree with it.
module SidecarSession

open Fable.Core
open Fable.Core.JsInterop
open Fable.SimpleJson // Json.serialize, the renderer's wire encoder (an extension member)
open CommonTypes

module Constants =
    /// How long one SimRun chunk may take. The renderer does nothing while the sidecar simulates,
    /// so this is what keeps a progress bar moving and Cancel answerable.
    let runChunkMs = 100

/// How far the sidecar's simulation has been run, as it last reported. The renderer keeps its own
/// simulator's clock in the FastSimulation; the sidecar's is in the sidecar, and this is the
/// renderer's copy of it. Written only from the chunk replies of `runTo`, which is where the
/// sidecar says what it has reached.
let mutable private clockTick = 0

/// How far the current session has been run.
let clockReached () = clockTick

/// Forget how far the sidecar has run, so the next use starts from nothing. Called when a
/// simulation ends or the design changes. WHICH session the sidecar holds is model state -
/// `Model.SidecarBuild` - and is not touched here.
let forget () = clockTick <- 0

/// The error text of a sidecar reply, or None when it is not an error. Every reply that can fail
/// answers with a JSON object whose only key is "error".
let errorIn (reply: string) =
    if reply.StartsWith "{\"error\"" then Some reply else None

[<Emit("JSON.parse($0)")>]
let parseJson (text: string) : obj = jsNative


/// Build the design on the sidecar, and answer with the session epoch that build issued.
///
/// **Unconditional.** Whether a build is needed at all is a question about what the sidecar is
/// believed to hold, which is `Model.SidecarBuild`, and it is answered before this is called - by
/// the update function, synchronously, from the model. This module used to hold that belief and
/// decide for itself, which put a fact the UI has to draw somewhere the UI could not read.
///
/// **One caller at a time, by construction rather than by guarding.** Issie runs one simulation:
/// starting the waveform simulator ends the step simulator and starting the step simulator ends
/// the waveform one (Update.endWaveSimulation), and a build is now a message, so the model is in
/// `SidecarBuilding` from the moment one starts until it answers - which is what stops a second
/// being started while the first is in flight.
///
/// It matters that this stays true: a design is uploaded one sheet per message and index 0 begins
/// an upload, discarding any abandoned one, so two builds interleaving leave the sidecar holding
/// half of each. That is reported rather than silent - the build fails with "no sheet called X in
/// the design" - but it is a simulation the user asked for and did not get.
let build (design: SimpleDesign) (arraySize: int) : JS.Promise<Result<int, string>> =
    promise {
        do! SidecarClient.connect ()
        let sheetJsons = design.Sheets |> List.map Json.serialize<SimpleSheet>
        let! sent = SidecarClient.sendDesign design.TopSheet sheetJsons

        match errorIn sent with
        | Some e -> return Error e
        | None ->
            let! reply = SidecarClient.simBuild arraySize

            match errorIn reply with
            | Some e -> return Error e
            | None ->
                let epoch = SidecarClient.epochOf reply

                if epoch = 0 then
                    // a build that issued no epoch built nothing, whatever else the reply said
                    return Error $"the sidecar's build reply named no session: {reply}"
                else
                    clockTick <- 0
                    return Ok epoch
    }

/// Advance the sidecar's simulation to `cycle`, a chunk at a time so the renderer stays live.
/// `onProgress` is told the clock tick after each chunk, which is what lets a progress bar move
/// and a Cancel be answered - the same contract the renderer's own progress loop uses against
/// local simulation.
let runTo (epoch: int) (cycle: int) (onProgress: int -> unit) : JS.Promise<Result<unit, string>> =
    let rec chunk () =
        promise {
            let! reply = SidecarClient.simRun epoch cycle Constants.runChunkMs

            match errorIn reply with
            | Some e -> return Error e
            | None ->
                let parsed = parseJson reply
                let tick: int = unbox parsed?clockTick
                let finished: bool = unbox parsed?``done``
                clockTick <- tick
                onProgress tick

                if finished then
                    return Ok()
                else
                    return! chunk ()
        }

    chunk ()
