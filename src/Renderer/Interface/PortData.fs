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

/// The build the held slices belong to, and the slices themselves. None when the sidecar is not
/// the simulator, or nothing has been built.
let mutable private held: (int * System.Collections.Generic.Dictionary<InstancePath, PortView.ComponentSlots list>) option =
    None

/// Drop everything, and stop being the slice source. Called when a simulation ends or the
/// renderer's own simulator takes over.
let forget () =
    held <- None
    PortView.sliceSource <- None

/// Start holding for a build, and become the slice source: from this moment instance views in
/// sidecar mode answer from what has arrived, and an instance not yet described answers None -
/// which every display draws as nothing-yet, and reconciliation keeps unresolved.
let startEpoch (epoch: int) =
    held <- Some(epoch, System.Collections.Generic.Dictionary())

    PortView.sliceSource <-
        Some(fun instance ->
            match held with
            | Some(_, slices) ->
                match slices.TryGetValue instance with
                | true, slice -> Some slice
                | _ -> None
            | None -> None)

/// The build the held slices are of, or None when nothing is held.
let epochHeld () = held |> Option.map fst

/// The instances of `wanted` this build has not yet been asked about.
let missingOf (wanted: InstancePath list) : InstancePath list =
    match held with
    | None -> []
    | Some(_, slices) -> wanted |> List.distinct |> List.filter (fun i -> not (slices.ContainsKey i))

/// Fetch and hold the slices of some instances, sequentially in one promise - a slice is a few
/// hundred bytes and a round trip a fifth of a millisecond, so even a whole design is tens of
/// milliseconds, and the selector's ask is a handful. Answers landing for a superseded build are
/// dropped by the epoch check in `store`-time `held` match.
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
                     | Some(heldEpoch, slices) when heldEpoch = epoch -> slices[instance] <- slice
                     | _ -> ())

                    go rest (fetched + 1))

    go (List.distinct instances) 0
