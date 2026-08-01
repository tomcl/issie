/// The sheet-description DSL and the layout that realises it. These check the two things a
/// generated sheet has to get right: that it describes the circuit the caller meant, and that it
/// is laid out well enough to read.
module SheetDescriptionTests

open Expecto
open CommonTypes
open SimGraphTypes
open SimTypes
open SheetDescription
open SheetDescription.Operators   // safe here: this file opens neither JsInterop nor FsCheck

let private expectOk (result: Result<'a, string>) =
    match result with
    | Ok v -> v
    | Error e -> failtest $"expected success but got: {e}"

let private expectError (result: Result<'a, string>) =
    match result with
    | Ok _ -> failtest "expected an error"
    | Error e -> e

/// a 4-bit adder: two inputs, an adder, one output
let private adderSheet =
    describeSheet "adder" [
        comp "A" (Input1(4, None))
        comp "B" (Input1(4, None))
        comp "ADD" (NbitsAdderNoCinCout 4)
        comp "S" (Output 4)
    ] [
        "A" ==> "ADD/P"
        "B" ==> "ADD/Q"
        "ADD/SUM" ==> "S"
    ]

let tests =
    testList "SheetDescription" [

        test "ports resolve by name, by index, and by being the only one" {
            let byIndex =
                describeSheet "byIndex" [
                    comp "A" (Input1(1, None))
                    comp "G" (GateN(And, 2))
                    comp "B" (Input1(1, None))
                    comp "O" (Output 1)
                ] [
                    // GateN has no port names at all, so an index is the only way to say this
                    "A" ==> "G/0"
                    "B" ==> "G/1"
                    "G/0" ==> "O"
                ]
            let comps, conns = SheetLayout.toCanvasState byIndex |> expectOk
            Expect.equal (List.length comps) 4 "all components built"
            Expect.equal (List.length conns) 3 "all connections resolved"
            // and by name, on a type that has names
            let comps, conns = SheetLayout.toCanvasState adderSheet |> expectOk
            Expect.equal (List.length conns) 3 "named ports resolved"
            let addInputs = comps |> List.find (fun c -> c.Id = "ADD") |> fun c -> c.InputPorts
            let pPort = addInputs |> List.item 0
            Expect.isTrue
                (conns |> List.exists (fun c -> c.Target.Id = pPort.Id))
                "ADD/P resolved to the adder's first input"
        }

        test "SUM resolves despite its trailing space in portNames" {
            // NbitsAdder's output names are "SUM " and "COUT " - a name lookup that did not trim
            // would miss both
            let sheet =
                describeSheet "trailing" [
                    comp "A" (Input1(4, None))
                    comp "B" (Input1(4, None))
                    comp "ADD" (NbitsAdderNoCinCout 4)
                    comp "S" (Output 4)
                ] [ "A" ==> "ADD/P"; "B" ==> "ADD/Q"; "ADD/sum" ==> "S" ]
            let _, conns = SheetLayout.toCanvasState sheet |> expectOk
            Expect.equal (List.length conns) 3 "lower case and untrimmed name still resolved"
        }

        test "a bad port reference says what is wrong and what is available" {
            let sheet =
                describeSheet "bad" [
                    comp "A" (Input1(4, None))
                    comp "ADD" (NbitsAdderNoCinCout 4)
                ] [ "A" ==> "ADD/NOSUCH" ]
            let err = SheetLayout.toCanvasState sheet |> expectError
            Expect.stringContains err "NOSUCH" "names the port asked for"
            Expect.stringContains err "ADD" "names the component"
            Expect.stringContains err "P" "lists the port names it does have"
        }

        test "an unknown component and a duplicate name are both refused" {
            let missing =
                describeSheet "missing" [ comp "A" (Input1(1, None)) ] [ "A" ==> "GHOST/0" ]
            Expect.stringContains (SheetLayout.toCanvasState missing |> expectError) "GHOST"
                "names the missing component"
            let duplicate =
                describeSheet "dup" [
                    comp "A" (Input1(1, None))
                    comp "A" (Output 1)
                ] []
            Expect.stringContains (SheetLayout.toCanvasState duplicate |> expectError) "more than once"
                "duplicate component names are refused"
        }

        test "an ambiguous port reference is refused rather than guessed" {
            let sheet =
                describeSheet "ambiguous" [
                    comp "A" (Input1(1, None))
                    comp "G" (GateN(And, 2))
                ] [ "A" ==> "G" ]        // G has two inputs, so "G" alone means nothing
            let err = SheetLayout.toCanvasState sheet |> expectError
            Expect.stringContains err "must be named" "explains that the port has to be named"
        }

        test "no two components overlap" {
            // a deliberately awkward sheet: a wide fan-out and a couple of clusters
            let sheet =
                describeSheet "spread" (
                    [ comp "IN" (Input1(1, None)); comp "OUT" (Output 1) ]
                    @ [ for i in 1 .. 12 -> comp $"G{i}" (GateN(And, 2)) ])
                    ([ for i in 1 .. 12 -> connect "IN" $"G{i}/0" ]
                     @ [ for i in 1 .. 11 -> connect $"G{i}/0" $"G{i + 1}/1" ]
                     @ [ connect "G12/0" "OUT" ])
            let comps, _ = SheetLayout.toCanvasState sheet |> expectOk
            let boxes =
                comps
                |> List.map (fun c ->
                    c.Id, ({ TopLeft = { X = c.X; Y = c.Y }; W = c.W; H = c.H }: BoundingBox))
            let overlaps =
                List.allPairs boxes boxes
                |> List.filter (fun ((idA, a), (idB, b)) -> idA < idB && BlockHelpers.overlap2DBox a b)
                |> List.map (fun ((idA, _), (idB, _)) -> $"{idA}/{idB}")
            Expect.isEmpty overlaps $"""overlapping symbols: {String.concat ", " overlaps}"""
        }

        test "inputs go left, outputs go right, in declaration order" {
            let comps, _ = SheetLayout.toCanvasState adderSheet |> expectOk
            let at name = comps |> List.find (fun c -> c.Id = name)
            Expect.isLessThan ((at "A").X) ((at "ADD").X) "inputs are left of the body"
            Expect.isLessThan ((at "ADD").X) ((at "S").X) "outputs are right of the body"
            Expect.isLessThan ((at "A").Y) ((at "B").Y) "inputs keep declaration order top to bottom"
        }

        test "the sheet's port order follows declaration order" {
            // this is the reason I/O placement is pinned: parseDiagramSignature sorts by (Y, X),
            // so layout decides the order of the sheet's own ports
            let canvas = SheetLayout.toCanvasState adderSheet |> expectOk
            let inputs, outputs = CanvasExtractor.parseDiagramSignature canvas
            Expect.equal (inputs |> List.map fst) [ "A"; "B" ] "input order matches declaration"
            Expect.equal (outputs |> List.map fst) [ "S" ] "output order matches declaration"
        }

        test "saving and reloading gives a sheet with routed wires" {
            // the whole point: a description written here becomes a .dgm that Issie loads, and the
            // load path is what supplies the wire geometry the description never had
            let folder =
                System.IO.Path.Combine(System.IO.Path.GetTempPath(), $"issie-dsl-{System.Guid.NewGuid()}")
            System.IO.Directory.CreateDirectory folder |> ignore
            try
                SheetLayout.saveSheet folder adderSheet |> expectOk
                let path = System.IO.Path.Combine(folder, "adder.dgm")
                Expect.isTrue (System.IO.File.Exists path) "the .dgm was written"
                match FilesIO.tryLoadComponentFromPath path with
                | Error e -> failtest $"the generated sheet would not load: {e}"
                | Ok ldc ->
                    let comps, conns = ldc.CanvasState
                    Expect.equal (List.length comps) 4 "components survive the round trip"
                    Expect.equal (List.length conns) 3 "connections survive the round trip"
                    Expect.equal (ldc.InputLabels |> List.map fst) [ "A"; "B" ] "port order survives"
                    // positions must survive: they are what makes the sheet readable
                    Expect.isTrue
                        (comps |> List.forall (fun c -> c.X > 0. && c.Y > 0.))
                        "every component kept a real position"
            finally
                try System.IO.Directory.Delete(folder, true) with _ -> ()
        }

        test "the generated canvas simulates" {
            let canvas = SheetLayout.toCanvasState adderSheet |> expectOk
            let ldc = CanvasBuilder.makeLdc "adder" None canvas
            match Simulator.startCircuitSimulation 10 "adder" canvas [ ldc ] with
            | Error e -> failtest $"simulation of the generated sheet failed: %A{e}"
            | Ok simData ->
                simData.Inputs
                |> List.iter (fun (cid, ComponentLabel label, width) ->
                    let value = if label = "A" then 9I else 5I
                    FastExtract.changeInput cid (IData (NumberHelpers.convertBigintToFastData width value)) 0 simData.FastSim)
                let out =
                    simData.Outputs
                    |> List.head
                    |> fun (cid, _, _) ->
                        match FastExtract.extractFastSimulationOutput simData.FastSim 0 (cid, []) (OutputPortNumber 0) with
                        | IData fd -> fd.GetBigInt
                        | IAlg _ -> failtest "algebraic output"
                Expect.equal out 14I "9 + 5 = 14 through the generated circuit"
        }
    ]
