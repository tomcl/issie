/// The sheet-description DSL and the layout that realises it. These check the two things a
/// generated sheet has to get right: that it describes the circuit the caller meant, and that it
/// is laid out well enough to read.
module SheetDescriptionTests

open Expecto
open CommonTypes
open ParameterTypes
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

/// an instance of a sheet called "child", with the given port labels
let private customWith inLabels outLabels =
    Custom
        { Name = "child"
          InputLabels = inLabels
          OutputLabels = outLabels
          Form = Some User
          ParameterBindings = None
          Description = None }

/// the laid-out Component for one custom instance, alone on a sheet
let private customComponent compType =
    let description = describeSheet "host" [ comp "C" compType; comp "O" (Output 1) ] []
    description
    |> SheetLayout.toCanvasState
    |> expectOk
    |> fst
    |> List.find (fun c -> c.Id = SheetLayout.componentIdIn description "C")

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
            let addInputs =
                comps
                |> List.find (fun c -> c.Id = SheetLayout.componentIdIn adderSheet "ADD")
                |> fun c -> c.InputPorts
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

        test "blocks are ordered so inputs sit near what they drive" {
            // four independent bit-slices. Bisection finds the slices, but on its own it does not
            // know where the I/O columns are, so it could put the slice fed by A3 above the one
            // fed by A1 and send both sets of wires the height of the sheet.
            let slice i =
                [ comp $"A{i}" (Input1(1, None)); comp $"B{i}" (Input1(1, None))
                  comp $"AND{i}" (GateN(And, 2)); comp $"XOR{i}" (GateN(Xor, 2))
                  comp $"OR{i}" (GateN(Or, 2)); comp $"S{i}" (Output 1) ]
            let sliceConns i =
                [ connect $"A{i}" $"AND{i}/0"; connect $"B{i}" $"AND{i}/1"
                  connect $"A{i}" $"XOR{i}/0"; connect $"B{i}" $"XOR{i}/1"
                  connect $"AND{i}/0" $"OR{i}/0"; connect $"XOR{i}/0" $"OR{i}/1"
                  connect $"OR{i}/0" $"S{i}" ]
            let sheet =
                describeSheet "slices"
                    ([ 1 .. 4 ] |> List.collect slice)
                    ([ 1 .. 4 ] |> List.collect sliceConns)
            let comps, conns = SheetLayout.toCanvasState sheet |> expectOk
            let centreY (c: Component) = c.Y + c.H / 2.
            let byId =
                comps |> List.map (fun c -> c.Id, c) |> Map.ofList
            let at name = byId[SheetLayout.componentIdIn sheet name]
            let height =
                (comps |> List.map (fun c -> c.Y + c.H) |> List.max)
                - (comps |> List.map (fun c -> c.Y) |> List.min)
            // every gate should sit nearer its own slice's input than the sheet is tall by half
            let worst =
                [ 1 .. 4 ]
                |> List.collect (fun i ->
                    [ $"AND{i}"; $"XOR{i}"; $"OR{i}" ]
                    |> List.map (fun g -> abs (centreY (at g) - centreY (at $"A{i}"))))
                |> List.max
            Expect.isLessThan worst (height / 2.)
                $"every gate sits within half the sheet height of its own input                   (worst {worst}, sheet height {height}). Without the axis choice in floorplan                   the four slices land in a 2x2 grid that a single input column cannot match,                   and this measures 457.5 against a 337.5 bound."
        }

        test "inputs go left, outputs go right, in declaration order" {
            let comps, _ = SheetLayout.toCanvasState adderSheet |> expectOk
            let at name = comps |> List.find (fun c -> c.Id = SheetLayout.componentIdIn adderSheet name)
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

        test "a sheet parameter drives a slot, and the resolved value reaches the canvas" {
            let sheet =
                describeSheet "param" [
                    comp "A" (Input1(1, None))
                    comp "V" (Viewer 1)
                ] [ "A" ==> "V" ]
                |> withParam "W" 8 "width of the data bus in bits"
                |> withSlot "V" Buswidth "W"
                |> withSlot "A" (IO "A") "W-4"
            let comps, _ = SheetLayout.toCanvasState sheet |> expectOk
            let typeOf name =
                comps |> List.find (fun c -> c.Id = SheetLayout.componentIdIn sheet name) |> fun c -> c.Type
            // the canvas carries the RESOLVED value, with the expression kept beside it
            Expect.equal (typeOf "V") (Viewer 8) "Buswidth slot resolved to the parameter default"
            Expect.equal (typeOf "A") (Input1(4, None)) "W-4 evaluated by the properties-pane parser"
            match SheetLayout.paramDefsOf sheet |> expectOk with
            | None -> failtest "expected parameter definitions"
            | Some defs ->
                Expect.equal (Map.count defs.DefaultBindings) 1 "one declaration"
                Expect.equal (Map.count defs.ParamSlots) 2 "two slots"
                Expect.equal
                    (defs.DefaultBindings[ParamName "W"].Description)
                    "width of the data bus in bits"
                    "the description is kept"
        }

        test "bad parameter usage is refused with a useful message" {
            let undeclared =
                describeSheet "p1" [ comp "V" (Viewer 1) ] [] |> withSlot "V" Buswidth "W"
            Expect.stringContains (SheetLayout.toCanvasState undeclared |> expectError) "does not declare"
                "a slot using an undeclared parameter is refused"
            let unparseable =
                describeSheet "p2" [ comp "V" (Viewer 1) ] []
                |> withParam "W" 4 "width"
                |> withSlot "V" Buswidth "W +* 2"
            Expect.stringContains (SheetLayout.toCanvasState unparseable |> expectError) "W +* 2"
                "an expression that will not parse is refused, quoting it"
            let missingComp =
                describeSheet "p3" [ comp "V" (Viewer 1) ] []
                |> withParam "W" 4 "width"
                |> withSlot "GHOST" Buswidth "W"
            Expect.stringContains (SheetLayout.toCanvasState missingComp |> expectError) "GHOST"
                "a slot on a component that is not there is refused"
        }

        test "a slot the component does not have is refused, not silently ignored" {
            // it would otherwise be recorded, shown in the properties pane, and do nothing
            let withSlotOn name compType slot =
                describeSheet "p" [ comp name compType; comp "O" (Output 1) ] []
                |> withParam "W" 4 "width"
                |> withSlot name slot "W"
                |> SheetLayout.toCanvasState
            // an input count is a change of shape, not a value: these have no slots at all
            Expect.stringContains (withSlotOn "G" (GateN(And, 2)) Buswidth |> expectError) "no Buswidth slot"
                "a gate cannot be parameterised"
            Expect.stringContains (withSlotOn "M" (MergeN 3) Buswidth |> expectError) "no Buswidth slot"
                "nor can a MergeN"
            // a SplitN's number of outputs is a shape too, but each output's width and position
            // are values - so the slot exists exactly for the outputs that exist
            let splitN = SplitN(2, [ 1; 3 ], [ 0; 1 ])
            Expect.isOk (withSlotOn "S" splitN (SplitNWidth 1)) "the second output has a width"
            Expect.isOk (withSlotOn "S" splitN (SplitNLSB 1)) "and a bit position"
            Expect.stringContains (withSlotOn "S" splitN (SplitNWidth 2) |> expectError) "no SplitNWidth 2 slot"
                "there is no third output"
            Expect.stringContains (withSlotOn "S" splitN Buswidth |> expectError) "no Buswidth slot"
                "the number of outputs is not a parameter"
        }

        test "an .ldgm library is written and read back, with no Issie running" {
            // the whole point of the DSL being plain .NET: a library can be built by a program
            let halfAdd =
                describeSheet "halfAdd" [
                    comp "A" (Input1(1, None)); comp "B" (Input1(1, None))
                    comp "X" (GateN(Xor, 2)); comp "N" (GateN(And, 2))
                    comp "SUM" (Output 1); comp "CARRY" (Output 1)
                ] [ "A" ==> "X/0"; "B" ==> "X/1"; "A" ==> "N/0"; "B" ==> "N/1"
                    "X/0" ==> "SUM"; "N/0" ==> "CARRY" ]
            let folder =
                System.IO.Path.Combine(System.IO.Path.GetTempPath(), $"issie-lib-{System.Guid.NewGuid()}")
            try
                SheetLayout.saveLibraryComponent folder "one-bit half adder" [] halfAdd |> expectOk
                let path = System.IO.Path.Combine(folder, "halfAdd.ldgm")
                Expect.isTrue (System.IO.File.Exists path) "the .ldgm was written"
                match ComponentLibraries.tryReadComponentFile path with
                | Error e -> failtest $"the generated .ldgm would not read: {e}"
                | Ok (header, body) ->
                    Expect.equal header.Name "halfAdd" "name"
                    Expect.equal header.Description "one-bit half adder" "description"
                    Expect.isTrue header.OfferedInCatalogue "offered in the catalogue"
                    Expect.isEmpty header.Requires "no dependencies"
                    Expect.stringContains body "NewCanvasWithFileWaveSheetInfoAndNewConns"
                        "the body is the text of a .dgm"
            finally
                try System.IO.Directory.Delete(folder, true) with _ -> ()
        }

        // A Custom component reports no size of its own: Symbol.autoScaleHAndW widens it to fit
        // its port labels, which means measuring text. That used to need a browser, so the layout
        // guessed - and always guessed the minimum width, so a sheet using a custom component with
        // long port names was spaced as though it were narrow. It now goes through the same
        // createSymbolRecord the app uses on load.
        //
        // Sizes are asserted by relation rather than in pixels: the text width behind them is
        // reconstructed outside the browser, and DrawBlockTests is where that reconstruction is
        // held to what the browser measures. The one absolute check available agrees - the halfAdd
        // instance Issie itself wrote into static/libraries/arithmetic/fullAdd.ldgm is 120 x 110,
        // which is what this code computes for that name and those port labels.

        test "a custom component is sized to fit its port labels" {
            let narrow = customComponent (customWith [ "A", 1 ] [ "B", 1 ])
            let wide =
                customComponent (customWith [ "CARRY_IN_FROM_PREVIOUS", 1 ] [ "SUM_OUT_TO_NEXT", 1 ])
            Expect.isGreaterThan wide.W narrow.W
                "long port labels make a wider symbol - the guess this replaced returned the same
                 minimum width whatever the labels said"
            // the minimum is 4 grid squares, and both keep the same height: one port a side
            Expect.isGreaterThanOrEqual narrow.W (4. * SheetLayout.Constants.grid)
                "even a narrow custom component is at least the minimum width"
            Expect.equal wide.H narrow.H "height counts ports, which these share"
        }

        test "a custom component grows in height with its ports" {
            let onePort = customComponent (customWith [ "A", 1 ] [ "B", 1 ])
            let fourPorts =
                customComponent (customWith [ "A", 1; "B", 1; "C", 1; "D", 1 ] [ "E", 1 ])
            Expect.isGreaterThan fourPorts.H onePort.H "more ports on a side means a taller symbol"
        }

        test "a sheet using a custom component lays out without overlaps" {
            // the size feeds straight into placement, so a wrong one shows up as symbols on top
            // of each other rather than as a wrong number
            let wide = customWith [ "CARRY_IN_FROM_PREVIOUS", 1 ] [ "SUM_OUT_TO_NEXT", 1 ]
            let sheet =
                describeSheet "uses" [
                    comp "IN" (Input1(1, None))
                    comp "C1" wide
                    comp "C2" wide
                    comp "OUT" (Output 1)
                ] [ "IN" ==> "C1/0"; "C1/0" ==> "C2/0"; "C2/0" ==> "OUT" ]
            let comps, conns = SheetLayout.toCanvasState sheet |> expectOk
            Expect.equal (List.length conns) 3 "the custom instances wire up by port index"
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
