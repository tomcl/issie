/// Reading a value from a custom component's OWN output port.
///
/// A custom component is not in FComps: it has no reducer and no step arrays of its own, because
/// the sheet inside it was flattened into the components that do the work. Its output port is the
/// Output component at the bottom of that sheet, and FCustomOutputCompLookup is what says which
/// one. So extractFastSimulationOutput takes a second path for these - the one this pins - and
/// nothing else in the suite does, because reading by label or through simData.Outputs always
/// names a component that is in FComps.
///
/// The lookup has to hold for an instance, not a sheet: two instances of one sheet have separate
/// Output components, and asking for TWIN1's output must not answer with TWIN2's.
module CustomOutputExtraction

open Expecto
open CommonTypes
open SimGraphTypes
open SimTypes
open CanvasBuilder

let private maxArraySize = 10

/// A sheet that inverts: IN -> NOT -> OUT.
let private notSheet =
    let i = makeComp 1 0 1 (Input1(1, None)) "A"
    let n = makeComp 2 1 1 Not "N"
    let o = makeComp 3 1 0 (Output 1) "Y"
    makeLdc "notsheet" None ([ i; n; o ], [ conn i 0 n 0; conn n 0 o 0 ])

/// Top sheet: IN drives two instances of notSheet in series, so that the two instances hold
/// opposite values and an answer from the wrong one is visible rather than a coincidence.
///   IN -> INV1 -> INV2 -> OUT      (INV1 = not IN, INV2 = IN)
let private top =
    let inp = makeComp 1 0 1 (Input1(1, None)) "IN"
    let inv1 = makeComp 2 1 1 (customOf notSheet [ "A", 1 ] [ "Y", 1 ] None) "INV1"
    let inv2 = makeComp 3 1 1 (customOf notSheet [ "A", 1 ] [ "Y", 1 ] None) "INV2"
    let out = makeComp 4 1 0 (Output 1) "OUT"
    makeLdc "top" None
        ([ inp; inv1; inv2; out ],
         [ conn inp 0 inv1 0; conn inv1 0 inv2 0; conn inv2 0 out 0 ])

/// Simulate `top` with IN set to `inValue`, and return the FastSimulation to read from.
let private simulateWith (inValue: bigint) =
    match Simulator.startCircuitSimulation maxArraySize top.Name top.CanvasState [ top; notSheet ] with
    | Error e -> failtest $"Simulation failed: %A{e}"
    | Ok simData ->
        simData.Inputs
        |> List.iter (fun (cid, _, width) ->
            FastExtract.changeInput cid (IData(NumberHelpers.convertBigintToFastData width inValue)) 0 simData.FastSim)
        simData.FastSim

/// The value on output port 0 of the component with the given id, at the top level of the design.
let private outputOf (fs: FastSimulation) (compId: int) =
    match FastExtract.extractFastSimulationOutput fs 0 (ComponentId compId, []) (OutputPortNumber 0) with
    | IData fd -> fd.GetBigInt
    | IAlg _ -> failtest "algebraic value from a non-algebraic simulation"

let tests =
    testList "CustomOutputExtraction" [
        test "a custom component's output port reads as the sheet inside it drives it" {
            let fs = simulateWith 0I
            Expect.equal (outputOf fs 2) 1I "INV1 inverts its input, so IN=0 gives 1"
            Expect.equal (outputOf fs 3) 0I "INV2 inverts INV1, so IN=0 gives 0"
        }

        test "and follows the input rather than reporting a constant" {
            let fs = simulateWith 1I
            Expect.equal (outputOf fs 2) 0I "INV1 inverts its input, so IN=1 gives 0"
            Expect.equal (outputOf fs 3) 1I "INV2 inverts INV1, so IN=1 gives 1"
        }

        test "each instance of a sheet answers with its own Output component" {
            // The two instances hold opposite values at every moment, so a lookup keyed by sheet
            // rather than by instance would have to give one of them the other's value.
            let fs = simulateWith 0I
            Expect.notEqual
                (outputOf fs 2)
                (outputOf fs 3)
                "two instances of one sheet, chained, must not report the same value"
        }

        test "the custom component agrees with the top-level Output it drives" {
            let fs = simulateWith 1I
            // top-out is an ordinary component found in FComps, so this reads by the other path:
            // the two paths must meet at the same value.
            Expect.equal
                (outputOf fs 3)
                (outputOf fs 4)
                "OUT is wired straight from INV2, so the two ways of reading it must agree"
        }
    ]
