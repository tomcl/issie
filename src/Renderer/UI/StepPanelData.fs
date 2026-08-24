/// What the step simulator's panel shows, when the .NET sidecar is the one simulating.
///
/// The same shape as the waveform viewer's cache and for the same reason: view code must read
/// values synchronously, on every render, and a separate process cannot be asked synchronously.
/// So the update function fills this and the view reads it (see the diagram at the top of
/// `SimInterface.fs`).
///
/// One snapshot, not a history. The panel shows one clock cycle - whichever the user has stepped
/// to - so a fetch replaces what was here rather than adding to it, and everything in it is of
/// one cycle of one session. That is what makes staleness impossible to show: a value is only
/// ever read back for the cycle and epoch it was fetched for, and there is nothing else in here
/// to read by mistake.
///
/// A `ComponentId` IS an integer here - the whole design is reduced to integer ids when a project
/// is opened (`Helpers.RegenerateIds`), which is what lets the sidecar name components at all.
module StepPanelData

open Fable.Core
open CommonTypes

/// One thing the panel shows: a component output, named the way the sidecar names it.
type PanelSignal =
    { Comp: int
      /// the instance the component is in, root first; empty for a top-level component
      Path: int list
      Port: int }

/// The cycle of the session this holds, and the value of every signal that was asked for.
///
/// Held rather than modelled for the reason `docs/mutableState.md` allows: it is a read-through
/// cache of another process's memory, read from `view` on every render and written from inside
/// the promise that fetched it, where there is no dispatch.
let mutable private snapshot: (int * int * Map<int * int list * int, bigint>) option = None

/// Drop what was fetched. Called when the simulation ends, the design changes, or the simulator
/// is switched - anything that makes the cycle this was of no longer the cycle being shown.
let forget () = snapshot <- None

/// The cycle the held values are of, or None when nothing has been fetched.
let cycleHeld () = snapshot |> Option.map (fun (_, cycle, _) -> cycle)

/// The value of one signal, if this snapshot is of the cycle asked for. `None` means "not
/// fetched", which the caller must show as unknown rather than as zero: on a design being
/// stepped, a missing value and a value of zero look identical and only one of them is true.
let valueAt (cycle: int) (signal: PanelSignal) : bigint option =
    match snapshot with
    | Some(_, held, values) when held = cycle ->
        Map.tryFind (signal.Comp, signal.Path, signal.Port) values
    | _ -> None

/// Read every signal the panel shows, at one cycle, in one request.
///
/// One request and not one per row: the panel is redrawn on every render and has a row per
/// top-level input, output, viewer and register, which on a large design is hundreds. `simRead`
/// takes a list, so this is a single round trip of a few hundred bytes.
let fill (epoch: int) (cycle: int) (signals: PanelSignal list) : JS.Promise<Result<unit, string>> =
    if List.isEmpty signals then
        snapshot <- Some(epoch, cycle, Map.empty)
        Promise.lift (Ok())
    else
        let requested = signals |> List.map (fun s -> s.Comp, s.Port, s.Path)

        promise {
            let! frame = SidecarClient.simRead epoch cycle 1 1 requested
            let asText = SidecarClient.decodeText frame

            if asText.StartsWith "{" then
                return Error asText
            else
                // the reply states its own layout, so a signal whose width the renderer has stale
                // is still read the way the sender wrote it
                let wordsPerSample = SidecarClient.simReadWordsPerSample frame

                let data: uint32 array =
                    unbox (SidecarClient.viewSimReadData frame (List.length requested * wordsPerSample))

                // least significant word first, whatever the width - see Protocol.SimRead
                let valueOf row =
                    (0I, [ wordsPerSample - 1 .. -1 .. 0 ])
                    ||> List.fold (fun acc w -> (acc <<< 32) + bigint (data[row * wordsPerSample + w]))

                match SidecarSession.current () with
                | Some(_, _, held) when held = epoch ->
                    snapshot <-
                        Some(
                            epoch,
                            cycle,
                            signals
                            |> List.mapi (fun row s -> (s.Comp, s.Path, s.Port), valueOf row)
                            |> Map.ofList
                        )
                | _ ->
                    // the session this was asked of ended while it was in flight, so these values
                    // are of a simulation that is no longer the one on screen
                    Log.dbg Log.Sim $"a step-panel read for session {epoch} landed after that session ended - dropped"

                return Ok()
        }
