/// A deterministic text rendering of a design's observable simulation behaviour, shared by both
/// runtimes so they can be compared byte for byte.
///
/// This is the golden-model render, moved here from the test suite unchanged so that the dotnet
/// sidecar can produce the SAME text the tests pin in their .golden files: every top-level input
/// driven by the deterministic stimulus, every output, viewer and clocked component printed per
/// cycle, RAM contents at the end. Electron computes it locally and the sidecar answers the
/// SimDigest protocol command with its own; any difference is a real cross-runtime divergence,
/// located by the first differing line.
module SimDigest

open CommonTypes
open SimGraphTypes
open SimTypes

module Constants =
    /// the step-array size every digest simulation is built with, so array wrap behaviour is
    /// identical everywhere the digest is computed (and identical to the golden tests)
    let maxArraySize = 250

/// Deterministic per-input stimulus, stable across runs and runtimes
let stimulus (inputIndex: int) (tick: int) (width: int) : bigint =
    let raw = bigint (tick + 1) * 2654435761I + bigint (inputIndex + 1) * 40503I
    raw % (1I <<< width)

let private toBigint (i: FSInterface) =
    match i with
    | IData fd -> fd.GetBigInt
    | IAlg _ -> failwith "Algebraic value in digest simulation"

/// Simulate `ticks` clock cycles of `topSheet`, driving all top-level inputs with the
/// deterministic stimulus, and render the observable behaviour as text. Error when the design
/// does not build.
let render (ldcs: LoadedComponent list) (topSheet: string) (ticks: int) : Result<string, string> =
    match ldcs |> List.tryFind (fun ldc -> ldc.Name = topSheet) with
    | None -> Error $"digest: no sheet called {topSheet}"
    | Some top ->
        match Simulator.startCircuitSimulation Constants.maxArraySize topSheet top.CanvasState ldcs with
        | Error e -> Error $"digest: simulation of {topSheet} failed: %A{e.ErrType}"
        | Ok simData ->
            let fs = simData.FastSim
            let byLabel ios =
                ios |> List.sortBy (fun (_, ComponentLabel label, _) -> label)
            let inputs = byLabel simData.Inputs
            let outputs = byLabel simData.Outputs
            // watch every clocked component and viewer at every level of the hierarchy: designs
            // like CPUs have no top-level outputs, so their registers are the observable state
            let clocked =
                fs.FClockedComps
                |> Array.filter (fun fc -> match fc.FType with ROM1 _ -> false | _ -> true)
                |> Array.sortBy (fun fc -> fc.FullName)
            let viewers =
                fs.FComps
                |> Map.toList
                |> List.choose (fun (_, fc) -> match fc.FType with Viewer _ -> Some fc | _ -> None)
                |> List.sortBy (fun fc -> fc.FullName)
            let isRam (fc: FastComponent) =
                match fc.FType with
                | RAM1 _ | AsyncRAM1 _ -> true
                | _ -> false
            let sb = System.Text.StringBuilder()
            for tick in 0 .. ticks - 1 do
                if tick > 0 then
                    FastRun.runFastSimulation None tick fs |> ignore
                inputs
                |> List.iteri (fun i (cid, _, width) ->
                    let fd = NumberHelpers.convertBigintToFastData width (stimulus i tick width)
                    FastExtract.changeInput cid (IData fd) tick fs)
                let index = tick % fs.MaxArraySize
                for cid, ComponentLabel label, _ in outputs do
                    let value =
                        FastExtract.extractFastSimulationOutput fs tick (cid, []) (OutputPortNumber 0)
                        |> toBigint
                    sb.AppendLine $"{tick},{label},{value}" |> ignore
                for fc in viewers do
                    sb.AppendLine $"{tick},{fc.FullName},{FastExtract.getFastComponentOutput fc 0 index}" |> ignore
                for fc in clocked do
                    if not (isRam fc) then
                        sb.AppendLine $"{tick},{fc.FullName},{FastExtract.getFastComponentOutput fc 0 index}" |> ignore
            // memory contents only at the end: they are bulky and cumulative
            for fc in clocked do
                if isRam fc then
                    let data =
                        fc.State
                        |> Option.map (fun st -> st.Step[(ticks - 1) % fs.MaxArraySize])
                        |> function
                            | Some (RamState ram) ->
                                (RamStore.toMemory ram (ticks - 1)).Data
                                |> Map.toList
                                |> List.map (fun (addr, value) -> $"{addr}:{value}")
                                |> String.concat " "
                            | _ -> "?"
                    sb.AppendLine $"ram,{fc.FullName},{data}" |> ignore
            Ok (sb.ToString().Replace("\r\n", "\n"))
