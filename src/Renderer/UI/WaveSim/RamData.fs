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

/// The rows held for one RAM, whatever question they answer.
///
/// What the table DRAWS, as against what it asks for. A table that draws nothing until the rows
/// for the cycle it is on arrive flickers empty on every cursor move - and with two memories
/// selected it flickers twice, since a pass fetches one of them and the next pass the other. The
/// waveforms beside it have always behaved the other way round: they keep the last window that
/// arrived and are redrawn when the next one does, because a viewer that empties itself while its
/// data is in the air is what a broken viewer looks like.
///
/// The rows carry the cycle they are of, and the table says which that is, so nothing on screen
/// claims to be of a cycle it is not.
let heldAny (model: Model) (ram: FComponentId) : (RamKey * RamView) option =
    Map.tryFind ram (Optic.get waveSimModel_ model).RamRows

/// Whether this memory's rows have to be asked for: nothing held answers the question the table is
/// about to ask. The caller uses it to decide whether to issue a command at all - a command that
/// always resolved would dispatch a message, which would ask again, for ever.
///
/// **A ROM's cycle does not count.** What a ROM holds is part of its type and cannot change as the
/// simulation runs, so rows fetched at one cycle answer for every cycle: only the location it is
/// READING moves, and that is marked here from the address wave rather than asked for again
/// (WaveSimRams). So a ROM is fetched when the window of addresses being shown changes, and not
/// when the cursor moves - which on a design whose ROM is its program is most of the fetching that
/// used to happen.
let needed (model: Model) (ram: FComponentId) =
    let wanted = keyOf model ram

    match heldAny model ram with
    | None -> true
    | Some(heldKey, _) ->
        if EvilHoverCache.isReadOnlyMemory (Simulator.getFastSim ()) ram then
            heldKey.SparseUpTo <> wanted.SparseUpTo || heldKey.Start <> wanted.Start
        else
            heldKey <> wanted

/// Read one RAM's rows from the sidecar, for the update function to put in the model.
///
/// The reply carries the epoch it was asked of, and whether that is still the session on screen is
/// settled where the model is, when the completion message lands. A build since then means these
/// rows describe a simulation that is no longer the one being drawn, and they would look exactly
/// as trustworthy as ones that did.
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
        | Ok view -> return Some(ram, (key, view))
    }
