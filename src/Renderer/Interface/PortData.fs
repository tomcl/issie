/// The port slices of one sidecar build, held for the renderer to derive instance views from.
///
/// The same shape as `WaveData` and `StepPanelData` and for the same reason: view code reads
/// synchronously, on every render, and a separate process cannot be asked synchronously - so the
/// update function fills this and the views read it (the diagram at the top of SimInterface.fs).
///
/// What is held is small and immutable by construction. For one simulation the circuit is frozen:
/// the design the sidecar simulates is exactly the design the renderer holds, and draw-block edits
/// touch neither until a refresh builds anew. So a slice is fetched once per (build, instance),
/// never invalidated, never re-fetched - the whole store simply dies with its build. And the
/// instances held are the ones the UI references - the selection's, the top sheet, the ones the
/// selector shows - bounded by what is on screen, never by the expansion.
///
/// Held rather than modelled for the reason docs/mutableState.md allows: a read-through memo of
/// another process's answers, written from inside the promise that fetched them.
module PortData

open Fable.Core
open CommonTypes

/// The build the held slices belong to - the carrier itself, by reference - its epoch, and the
/// slices. Keying by the BUILD is what makes staleness impossible rather than managed: a slice
/// is served only to the exact fs it was fetched for, so a new build reads nothing of the old
/// one's, with no clearing choreography to get right. None when nothing has been described.
let mutable private held:
    (obj * int * System.Collections.Generic.Dictionary<InstancePath, PortView.ComponentSlots list>) option =
    None

/// Release what is held. Memory hygiene only - correctness never needs it, because the keying
/// above already refuses stale answers.
let forget () = held <- None

/// Start holding for a build: the carrier `fs` under session `epoch`.
let startEpoch (fs: SimTypes.FastSimulation) (epoch: int) =
    held <- Some(box fs, epoch, System.Collections.Generic.Dictionary())

/// The instances of `wanted` this build has not yet been asked about.
let missingOf (wanted: InstancePath list) : InstancePath list =
    match held with
    | None -> []
    | Some(_, _, slices) -> wanted |> List.distinct |> List.filter (fun i -> not (slices.ContainsKey i))

/// Fetch and hold the slices of some instances, sequentially in one promise - a slice is a few
/// hundred bytes and a round trip a fifth of a millisecond, so even a whole design is tens of
/// milliseconds, and the selector's ask is a handful. Answers landing for a superseded build are
/// dropped by the epoch check at store time.
let fetch (epoch: int) (instances: InstancePath list) : JS.Promise<Result<int, string>> =
    let rec go remaining fetched =
        match remaining with
        | [] -> Promise.lift (Ok fetched)
        | (InstancePath ap as instance) :: rest ->
            SidecarClient.simPorts epoch (ap |> List.map (fun (ComponentId c) -> c))
            |> Promise.bind (function
                | Error e -> Promise.lift (Error $"describing {instance}: {e}")
                | Ok slice ->
                    (match held with
                     | Some(_, heldEpoch, slices) when heldEpoch = epoch -> slices[instance] <- slice
                     | _ -> ())

                    go rest (fetched + 1))

    go (List.distinct instances) 0

/// Test-only: what is held and whether it is keyed to `fs`, for the dev harness.
let describeHeld (fs: SimTypes.FastSimulation) : string =
    match held with
    | None -> "nothing held"
    | Some(heldFs, epoch, slices) ->
        let current = System.Object.ReferenceEquals(heldFs, box fs)
        $"epoch {epoch}, {slices.Count} instances, of the current build: {current}"

/// Test-only: hold one instance's slice as if it had arrived on the wire.
let storeForTest (epoch: int) (instance: InstancePath) (slice: PortView.ComponentSlots list) =
    match held with
    | Some(_, heldEpoch, slices) when heldEpoch = epoch -> slices[instance] <- slice
    | _ -> ()

// Static wiring, not lifecycle: from the moment this module loads, a CARRIER's slices come from
// here - and only the exact build they were fetched for is ever answered, by reference. Local
// builds never consult this (PortView derives the source from the build itself).
do
    PortView.sliceSource <-
        Some(fun fs instance ->
            match held with
            | Some(heldFs, _, slices) when System.Object.ReferenceEquals(heldFs, box fs) ->
                match slices.TryGetValue instance with
                | true, slice -> Some slice
                | _ -> None
            | _ -> None)
