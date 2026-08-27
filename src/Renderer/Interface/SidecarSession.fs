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
    /// How long one SimRun chunk may take.
    ///
    /// A second, not a tenth of one. The renderer does nothing while the sidecar simulates, so
    /// this is the granularity at which a progress bar moves and at which a Cancel is noticed -
    /// and a second is fine for both: a bar that steps once a second is a bar, and cancelling is
    /// rare enough that waiting out the chunk in progress costs nobody anything. Ten times a
    /// second bought no more of either, and cost ten times the messages.
    let runChunkMs = 1000

[<Emit("JSON.parse($0)")>]
let parseJson (text: string) : obj = jsNative

[<Emit("(function(o){ return typeof o.error === 'string' ? o.error : null })($0)")>]
let private errorField (parsed: obj) : string = jsNative

/// The error text of a sidecar reply, or None when it is not an error. Every reply that can fail
/// answers with a JSON object whose only key is "error" - and what is UNDER that key is the
/// message written for the user (a refused build says exactly what to set the cycle count to),
/// so this unwraps it rather than passing the wire envelope on to a screen.
let errorIn (reply: string) =
    if reply.StartsWith "{\"error\"" then
        let inner =
            try
                match errorField (parseJson reply) with
                | null -> reply
                | text -> text
            with _ ->
                reply

        // the simulator's own prefix restates what the context already says
        Some(inner.Replace("simulation build failed: GenericSimError   '", "").TrimEnd('''))
    else
        None


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
                    return Ok epoch
    }

/// Run the session towards `cycle` for one chunk, and answer with the clock it reached and
/// whether it got there.
///
/// **One chunk, not a loop.** A run is a SEQUENCE of these and the sequence belongs in the update
/// function, which is where the sequencing of everything else on this protocol lives.
///
/// **No operation is ever cancelled in the middle.** Every one runs to completion and answers -
/// this one answers with the clock it reached and whether that was the cycle asked for. Cancelling
/// a run is deciding not to ask for the next chunk, which needs nothing of the protocol, nothing
/// of the sidecar, and nothing of the promise already running. Reaching into an operation in
/// flight would be a mechanism this protocol has nowhere else, and the whole reason a run is
/// chunked at all is so that it does not need one.
///
/// It is also what lets the clock be model state - `SidecarSession`, updated by each answer - so
/// that a progress bar is drawn from it like anything else.
///
/// The loop that used to be here reported progress through a callback and checked no cancellation.
/// Every caller passed `ignore`, so ten round trips a second bought neither of the two things
/// chunking is for.
let runChunk (epoch: int) (cycle: int) : JS.Promise<Result<int * bool, string>> =
    promise {
        let! reply = SidecarClient.simRun epoch cycle Constants.runChunkMs

        match errorIn reply with
        | Some e -> return Error e
        | None ->
            let parsed = parseJson reply
            let tick: int = unbox parsed?clockTick
            let finished: bool = unbox parsed?``done``
            return Ok(tick, finished)
    }
