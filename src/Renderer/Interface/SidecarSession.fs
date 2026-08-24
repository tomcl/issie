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

/// What the sidecar has been told to build: the top sheet, the step-array size it was built for,
/// and the epoch that build issued.
///
/// The epoch is the part that makes the other two checkable. Without it this record is a belief -
/// the sidecar could have restarted, or been built over - and every command sent on the strength
/// of it would be answered as though the belief were true. With it, a command that names a session
/// the sidecar does not hold is refused by name.
let mutable private built: (string * int * int) option = None

/// How far the sidecar's simulation has been run, as it last reported. The renderer keeps its own
/// simulator's clock in the FastSimulation; the sidecar's is in the sidecar, and this is the
/// renderer's copy of it. Written only from the chunk replies of `runTo`, which is where the
/// sidecar says what it has reached.
let mutable private clockTick = 0

/// The session the sidecar is believed to hold, or None.
let current () = built

/// How far that session has been run.
let clockReached () = clockTick

/// Forget what the sidecar holds, so the next use builds again. Called when a simulation ends or
/// the design changes.
let forget () =
    built <- None
    clockTick <- 0

/// The error text of a sidecar reply, or None when it is not an error. Every reply that can fail
/// answers with a JSON object whose only key is "error".
let errorIn (reply: string) =
    if reply.StartsWith "{\"error\"" then Some reply else None

[<Emit("JSON.parse($0)")>]
let parseJson (text: string) : obj = jsNative


/// Build the design on the sidecar if it does not already hold it at a big enough array size.
///
/// **One caller at a time, by construction rather than by guarding.** Issie runs one simulation:
/// starting the waveform simulator ends the step simulator and starting the step simulator ends
/// the waveform one (Update.endWaveSimulation), and within the waveform simulator everything one
/// update wants is fetched by a single command in order
/// (WaveSimTop.fetchWhatIsMissing). So nothing here arbitrates between two builds - there are
/// never two. An in-flight guard lived here briefly, and it was covering for a second asker that
/// should not have existed.
///
/// It matters that this stays true: a design is uploaded one sheet per message and index 0 begins
/// an upload, discarding any abandoned one, so two builds interleaving leave the sidecar holding
/// half of each. That is reported rather than silent - the build fails with "no sheet called X in
/// the design" and `built` is left None, so the next attempt starts cleanly - but it is a
/// simulation the user asked for and did not get.
let ensureBuilt (design: SimpleDesign) (arraySize: int) : JS.Promise<Result<int, string>> =
    match built with
    | Some(top, size, epoch) when top = design.TopSheet && size >= arraySize -> Promise.lift (Ok epoch)
    | _ ->
        promise {
            do! SidecarClient.connect ()
            let sheetJsons = design.Sheets |> List.map Json.serialize<SimpleSheet>
            let! sent = SidecarClient.sendDesign design.TopSheet sheetJsons

            match errorIn sent with
            | Some e ->
                built <- None
                return Error e
            | None ->
                let! reply = SidecarClient.simBuild arraySize

                match errorIn reply with
                | Some e ->
                    built <- None
                    return Error e
                | None ->
                    let epoch = SidecarClient.epochOf reply

                    if epoch = 0 then
                        // a build that issued no epoch built nothing, whatever else the reply said
                        built <- None
                        return Error $"the sidecar's build reply named no session: {reply}"
                    else
                        built <- Some(design.TopSheet, arraySize, epoch)
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
