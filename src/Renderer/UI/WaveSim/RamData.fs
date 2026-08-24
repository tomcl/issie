/// Asking the .NET simulator for what a RAM table draws.
///
/// **There is no cache here, and there does not need to be.** The rows are small - at most a
/// hundred a table - so they live in the model, in `WaveSimModel.RamRows`, and the view reads them
/// the way it reads everything else. That is not a stylistic preference: the waveform pane is
/// memoised on the model, so a reply landing in a module of its own changes nothing the renderer
/// can see, and the table stays empty until something unrelated happens to redraw it. Held in the
/// model, arriving IS a redraw.
///
/// The waveform data proper is the case that genuinely cannot do this and lives outside
/// (`WaveData`): megabytes of typed arrays, read per render, per wave.
///
/// What is left here is the asking - which belongs in the update function and not in a render, so
/// that a request is made when the question changes rather than on every frame.
module RamData

open Fable.Core
open CommonTypes
open ModelType
open ModelHelpers
open RamView
open Optics
open Optics.Operators

/// What the table for one RAM is going to ask for, taken from the model.
///
/// Here rather than in the view because the fetch and the render must agree exactly: a key
/// computed two ways is a table that asks for one thing, is sent it, and then reads the model for
/// another - which looks exactly like a reply that never arrived.
///
/// `SparseUpTo` is zero when the user has typed a start address, since that is a request for a
/// window whatever the memory holds - and asking for a listing that will be thrown away is a read
/// that need not happen.
let keyOf (model: Model) (ram: FComponentId) : RamKey =
    // waveSimModel_ throughout, not getWSModel: the two index the WaveSim map by different
    // fields - WaveSimOrCurrentSheet and WaveSimSheet - and where they disagree the rows would be
    // written to one entry and looked for in the other, which reads as a reply that never came
    // and asks again on every render.
    let ws = Optic.get waveSimModel_ model
    let typed, start =
        Map.tryFind ram ws.RamStartLocation |> Option.defaultValue ("", 0I)

    { Cycle = ws.CursorExactClkCycle
      SparseUpTo = (if typed = "" then WaveSimTypes.Constants.maxRamLocsWithSparseDisplay else 0)
      Start = start }

/// The rows held for one RAM, if they answer exactly the question the table is asking.
let held (model: Model) (ram: FComponentId) : RamView option =
    match Map.tryFind ram (Optic.get waveSimModel_ model).RamRows with
    | Some(heldKey, view) when heldKey = keyOf model ram -> Some view
    | _ -> None

/// Whether this RAM's rows have to be asked for: nothing held answers the question the table is
/// about to ask. The caller uses it to decide whether to issue a command at all - a command that
/// always resolved would dispatch a message, which would ask again, for ever.
let needed (model: Model) (ram: FComponentId) = (held model ram).IsNone

/// Read one RAM's rows from the sidecar, for the update function to put in the model.
///
/// The reply carries the epoch it was asked of. A build since then means it describes a simulation
/// that is no longer the one on screen, so it is dropped rather than shown beside rows of the one
/// that is - both would look equally trustworthy.
let fetch
    (epoch: int)
    (ram: FComponentId)
    (key: RamKey)
    (rows: int)
    : JS.Promise<(FComponentId * (RamKey * RamView)) option> =
    let (ComponentId cid), path = ram
    let pathIds = path |> List.map (fun (ComponentId p) -> p)

    promise {
        let! reply = SidecarClient.simReadRam epoch key.Cycle cid pathIds key.SparseUpTo key.Start rows

        match reply with
        | Error e ->
            Log.warn $"reading a RAM from the .NET simulator: {e}"
            return None
        | Ok view ->
            match SidecarSession.current () with
            | Some(_, _, current) when current = epoch -> return Some(ram, (key, view))
            | _ ->
                Log.dbg Log.Wave $"a RAM read for session {epoch} landed after that session ended - dropped"
                return None
    }
