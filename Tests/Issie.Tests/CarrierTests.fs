/// The design-only simulation carrier: what the renderer holds when the .NET sidecar simulates.
/// These pin the resolve lifecycle that makes it work with no ordering at all: a selection
/// enumerated from the design alone is KEPT unresolved while no slice has arrived, and resolves
/// completely once the slices land - the exact flow the app runs between a build reply and its
/// describe answers.
module CarrierTests

open Expecto
open CommonTypes
open SimTypes
open TestFixtures

let tests =
    testList "Carrier" [
        test "a design-enumerated selection survives the slice gap and then resolves" {
            let ldcs = loadProject "3cpu"
            let top = ldcs |> List.find (fun l -> l.Name = "eep1")
            let deps = ldcs |> List.filter (fun l -> l.Name <> "eep1")

            // the carrier, exactly as sidecar-mode start builds it
            PortData.activate ()
            let carrier =
                match Simulator.designOnlySimulation 2 "eep1" top.CanvasState ldcs with
                | Ok sd -> sd
                | Error e -> failwith $"carrier: %A{e.ErrType}"
            let cfs = carrier.FastSim
            Expect.equal cfs.SimulatedTopSheet "eep1" "the carrier knows its sheet"
            Expect.equal cfs.FComps.Count 0 "and holds no components at all"

            // the design enumeration the harness selects with
            let rec instances (InstancePath ap as inst) sheet =
                inst :: (cfs.Design.SubSheetsOf sheet |> List.collect (fun (cid, c) -> instances (InstancePath(ap @ [cid])) c))
            let waves =
                instances (InstancePath []) "eep1"
                |> List.collect (PortView.waveIndicesOfDesign cfs.Design)
                |> List.truncate 100
            Expect.equal (List.length waves) 100 "the design enumerates the asked-for waves"

            // resolution BEFORE any slice: must keep, unresolved
            let kept = waves |> List.choose (WaveSimHelpers.reResolveWave cfs)
            Expect.equal (List.length kept) 100 "before any slice arrives every wave is KEPT"
            Expect.equal (kept |> List.filter (fun w -> w.SimArrayIndex >= 0) |> List.length) 0
                "and none is resolved - there is nothing to resolve against"

            // slices from a REAL build (identical to what the wire returns - proven earlier)
            let realFs =
                match Simulator.startCircuitSimulation 250 "eep1" top.CanvasState ldcs with
                | Ok sd -> sd.FastSim
                | Error e -> failwith $"real: %A{e.ErrType}"
            PortData.startEpoch 1
            for inst in instances (InstancePath []) "eep1" do
                PortData.storeForTest 1 inst (PortView.sheetSliceOf realFs inst)

            let resolved = waves |> List.choose (WaveSimHelpers.reResolveWave cfs)
            Expect.equal (List.length resolved) 100 "once the slices land every wave is still here"
            Expect.equal (resolved |> List.filter (fun w -> w.SimArrayIndex >= 0) |> List.length) 100
                "and every one is resolved to the build's own handle"
            PortData.forget ()
        }
    ]
