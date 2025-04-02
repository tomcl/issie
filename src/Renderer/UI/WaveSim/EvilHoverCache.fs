module EvilHoverCache

//---------------------------------------------------------------------------------------//
//----------Mutable Cache For Fast HTML Dynamic Tooltips in Waveform Simulator-----------//
//---------------------------------------------------------------------------------------//

(*
The code here is used to cache the data required to quickly determine if a wave is hatched at a given cycle.
This is used to display tooltips in the waveform simulator, which show the value of a wave at a given cycle.
The cache is mutable, as it is updated as gaps are added to the waveform.
It is evil because it is mutable, but this is necessary for performance reasons.
The cache should have minimal space impact (TODO - check this) because it consists of one small typed array
For every wave in the simulation.

*)

open ModelType
open WaveSimTypes

/// A gap in the wave simulator, represented by a start cycle and a length.
/// the only gaps that are stored are relevant are those that correspond to hatched
/// parts of the waveform in which the wave value is not printed.
let initGapStore maxGaps =
    {Gaps = Array.zeroCreate maxGaps; NextGap = 0 ; GapStart = 0; GapEnd = 0}

/// Add a gap to the store, merging it with the previous gap if it is adjacent.
let addGapToStore (store:GapStore) (gap:Gap) =
    if store.GapEnd = gap.Start then
        store.GapEnd <- store.GapEnd + gap.Length
    else
        store.Gaps[store.NextGap] <- {Start = store.GapStart; Length = store.GapEnd - store.GapStart}
        store.NextGap <- store.NextGap + 1
        store.GapStart <- gap.Start
        store.GapEnd <- gap.Start + gap.Length

/// Finalise the store by adding the last gap to the store.
let finaliseStore (store:GapStore) =
    if store.NextGap > 0 && store.GapStart = store.Gaps.[store.NextGap-1].Start + store.Gaps.[store.NextGap-1].Length then
        store.Gaps.[store.NextGap-1] <- {Start = store.GapStart; Length = store.GapEnd - store.GapStart}
    else
        store.Gaps[store.NextGap] <- {Start = store.GapStart; Length = store.GapEnd - store.GapStart}
        store.NextGap <- store.NextGap + 1

/// Check if a wave is hatched at a given cycle.
/// This is done by checking if the cycle is within any of the gaps in a mutable store,
/// which is updated as gaps are added.
let checkIfHatched (store:GapStore) (cycle:int) =
    store.Gaps[0..store.NextGap-1]
    |> Array.exists (fun gap -> gap.Start <= cycle && gap.Length + gap.Start > cycle)

/// Use cached "gap" data to determine if a wave is hatched at a given cycle.
/// If so, return text for a tooltip based on the correct wave value for that cycle.
/// If not, return an empty string.
let getWaveToolTip (cycle:int) (waveNum: int) (ws:WaveSimModel) =
    match List.tryItem waveNum ws.SelectedWaves with
    | None -> ""
    | Some index ->
        let wave = ws.AllWaves[index]
        let start = wave.StartCycle
        //printfn $"Wave - Start: {start}, Cycle: {cycle}, {wave.ViewerDisplayName}, NextGap={wave.HatchedCycles.NextGap}"
        if checkIfHatched wave.HatchedCycles (cycle - start)
        then
            match Simulator.simCacheWS.FastSim.Drivers[wave.DriverIndex] with
            | Some {DriverData = data} ->
                if data.Width <= 32 then
                     NumberHelpers.UInt32ToPaddedString Constants.waveLegendMaxChars ws.Radix data.Width data.UInt32Step[cycle]
                else
                    NumberHelpers.BigIntToPaddedString Constants.waveLegendMaxChars ws.Radix data.Width data.BigIntStep[cycle]
            | None ->
                ""
        else
             ""

