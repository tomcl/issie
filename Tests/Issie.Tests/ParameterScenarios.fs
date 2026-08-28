/// Parameter resolution scenarios: sheets with parameterised slots, instantiated with
/// different bindings, simulated through the full pipeline. These pin down the
/// GraphMerger behaviour: effective bindings per instance, memoised resolution,
/// CustomCompParam propagation, and informative errors for bad expressions.
module ParameterScenarios

open Expecto
open CommonTypes
open SimGraphTypes
open SimTypes
open ParameterTypes
open CanvasBuilder

let private maxArraySize = 10

/// A parameter declaration. Descriptions are compulsory on a sheet parameter but say nothing
/// these tests depend on, so they are filled in from the parameter name.
let private declares (name: string) (expr: ParamExpression) =
    ParamName name, {Expression = expr; Description = $"test parameter {name}"}

/// The parameterised child sheet: S = (A + B) truncated to W bits, W defaulting to 4
let private childLdc =
    let a = makeComp 1 0 1 (Input1(4, None)) "A"
    let b = makeComp 2 0 1 (Input1(4, None)) "B"
    let add = makeComp 3 2 1 (NbitsAdderNoCinCout 4) "ADD"
    let s = makeComp 4 1 0 (Output 4) "S"
    let canvas = [ a; b; add; s ], [ conn a 0 add 0; conn b 0 add 1; conn add 0 s 0 ]
    let wExpr = { Expression = PParameter(ParamName "W"); Constraints = [] }
    let paramDefs =
        { DefaultBindings = Map [ declares "W" (PInt 4I) ]
          ParamSlots =
            Map [ {CompId = ComponentId 1; CompSlot = IO "A" }, wExpr
                  {CompId = ComponentId 2; CompSlot = IO "B" }, wExpr
                  {CompId = ComponentId 3; CompSlot = Buswidth }, wExpr
                  {CompId = ComponentId 4; CompSlot = IO "S" }, wExpr ] }
    makeLdc "pchild" (Some paramDefs) canvas

/// An instance of the child at width `w` (bindings given unless w is the default 4)
let private childInstance (id: int) (label: string) (w: int) (bindings: ParamBindings option) =
    makeComp id 2 1 (customOf childLdc [ "A", w; "B", w ] [ "S", w ] bindings) label

/// Simulate a parent sheet: drive the given input values, read all outputs by label
let private run (parent: LoadedComponent) (deps: LoadedComponent list) (inputVals: Map<string, bigint>) =
    match Simulator.startCircuitSimulation maxArraySize parent.Name parent.CanvasState (parent :: deps) with
    | Error e -> Error e
    | Ok simData ->
        simData.Inputs
        |> List.iter (fun (cid, ComponentLabel label, width) ->
            let fd = NumberHelpers.convertBigintToFastData width inputVals[label]
            FastExtract.changeInput cid (IData fd) 0 simData.FastSim)
        simData.Outputs
        |> List.map (fun (cid, ComponentLabel label, _) ->
            match FastExtract.extractFastSimulationOutput simData.FastSim 0 (cid, []) (OutputPortNumber 0) with
            | IData fd -> label, fd.GetBigInt
            | IAlg _ -> failwith "algebraic output")
        |> Map.ofList
        |> Ok

let private expectOk result =
    match result with
    | Ok v -> v
    | Error e -> failtest $"Simulation failed: %A{e}"

/// A simple parent: n-bit inputs X,Y into one child instance, output S
let private parentWith (w: int) (bindings: ParamBindings option) =
    let x = makeComp 1 0 1 (Input1(w, None)) "X"
    let y = makeComp 2 0 1 (Input1(w, None)) "Y"
    let cc = childInstance 3 "CC" w bindings
    let out = makeComp 4 1 0 (Output w) "S"
    makeLdc "pparent" None ([ x; y; cc; out ], [ conn x 0 cc 0; conn y 0 cc 1; conn cc 0 out 0 ])

/// A child sheet selecting one bit out of an 8-bit input, at a parameterised LSB.
let private selectLdc =
    let inp = makeComp 1 0 1 (Input1(8, None)) "IN"
    let sel = makeComp 2 1 1 (BusSelection(1, 0)) "SEL"
    let out = makeComp 3 1 0 (Output 1) "O"
    let canvas = [ inp; sel; out ], [ conn inp 0 sel 0; conn sel 0 out 0 ]
    let paramDefs =
        { DefaultBindings = Map [ declares "L" (PInt 0I) ]
          ParamSlots =
            Map [ {CompId = ComponentId 2; CompSlot = IO "SEL" },
                  { Expression = PParameter(ParamName "L"); Constraints = [] } ] }
    makeLdc "pselect" (Some paramDefs) canvas

/// A parent driving one instance of the bit selector
let private selectParent (bindings: ParamBindings option) =
    let x = makeComp 1 0 1 (Input1(8, None)) "X"
    let cc = makeComp 2 1 1 (customOf selectLdc [ "IN", 8 ] [ "O", 1 ] bindings) "SCC"
    let o = makeComp 3 1 0 (Output 1) "O"
    makeLdc "pselparent" None ([ x; cc; o ], [ conn x 0 cc 0; conn cc 0 o 0 ])

let tests =
    testList "ParameterScenarios" [

        test "child instantiated at default width" {
            let parent = parentWith 4 None
            let outs = run parent [ childLdc ] (Map [ "X", 12I; "Y", 7I ]) |> expectOk
            Expect.equal outs["S"] 3I "12+7 wraps to 3 at the default width 4"
        }

        test "instance binding overrides the default width" {
            let parent = parentWith 8 (Some(Map [ ParamName "W", PInt 8I ]))
            let outs = run parent [ childLdc ] (Map [ "X", 200I; "Y", 100I ]) |> expectOk
            Expect.equal outs["S"] 44I "200+100 wraps to 44 at the bound width 8"
        }

        test "two instances with different bindings resolve independently" {
            let x8 = makeComp 1 0 1 (Input1(8, None)) "X8"
            let y8 = makeComp 2 0 1 (Input1(8, None)) "Y8"
            let x16 = makeComp 3 0 1 (Input1(16, None)) "X16"
            let y16 = makeComp 4 0 1 (Input1(16, None)) "Y16"
            let cc8 = childInstance 5 "CC8" 8 (Some(Map [ ParamName "W", PInt 8I ]))
            let cc16 = childInstance 6 "CC16" 16 (Some(Map [ ParamName "W", PInt 16I ]))
            let s8 = makeComp 7 1 0 (Output 8) "S8"
            let s16 = makeComp 8 1 0 (Output 16) "S16"
            let parent =
                makeLdc "pboth" None (
                    [ x8; y8; x16; y16; cc8; cc16; s8; s16 ],
                    [ conn x8 0 cc8 0; conn y8 0 cc8 1; conn cc8 0 s8 0
                      conn x16 0 cc16 0; conn y16 0 cc16 1; conn cc16 0 s16 0 ])
            let outs =
                run parent [ childLdc ] (Map [ "X8", 200I; "Y8", 100I; "X16", 200I; "Y16", 100I ])
                |> expectOk
            Expect.equal outs["S8"] 44I "8-bit instance wraps"
            Expect.equal outs["S16"] 300I "16-bit instance does not wrap"
        }

        test "two instances with the same bindings both resolve correctly" {
            // exercises the memoised walk: the second instance reuses the first resolution
            let xa = makeComp 1 0 1 (Input1(8, None)) "XA"
            let ya = makeComp 2 0 1 (Input1(8, None)) "YA"
            let xb = makeComp 3 0 1 (Input1(8, None)) "XB"
            let yb = makeComp 4 0 1 (Input1(8, None)) "YB"
            let cca = childInstance 5 "CCA" 8 (Some(Map [ ParamName "W", PInt 8I ]))
            let ccb = childInstance 6 "CCB" 8 (Some(Map [ ParamName "W", PInt 8I ]))
            let sa = makeComp 7 1 0 (Output 8) "SA"
            let sb = makeComp 8 1 0 (Output 8) "SB"
            let parent =
                makeLdc "psame" None (
                    [ xa; ya; xb; yb; cca; ccb; sa; sb ],
                    [ conn xa 0 cca 0; conn ya 0 cca 1; conn cca 0 sa 0
                      conn xb 0 ccb 0; conn yb 0 ccb 1; conn ccb 0 sb 0 ])
            let outs =
                run parent [ childLdc ] (Map [ "XA", 200I; "YA", 100I; "XB", 30I; "YB", 5I ])
                |> expectOk
            Expect.equal outs["SA"] 44I "first instance"
            Expect.equal outs["SB"] 35I "second instance"
        }

        test "binding an arithmetic expression of the parent parameter" {
            // parent parameter P=8 feeds the child's W through the instance binding
            let x = makeComp 1 0 1 (Input1(8, None)) "X"
            let y = makeComp 2 0 1 (Input1(8, None)) "Y"
            let cc = childInstance 3 "CC" 8 (Some(Map [ ParamName "W", PParameter(ParamName "P") ]))
            let out = makeComp 4 1 0 (Output 8) "S"
            let paramDefs =
                { DefaultBindings = Map [ declares "P" (PInt 8I) ]
                  ParamSlots =
                    Map [ {CompId = ComponentId 1; CompSlot = IO "X" }, { Expression = PParameter(ParamName "P"); Constraints = [] }
                          {CompId = ComponentId 2; CompSlot = IO "Y" }, { Expression = PParameter(ParamName "P"); Constraints = [] }
                          {CompId = ComponentId 4; CompSlot = IO "S" }, { Expression = PParameter(ParamName "P"); Constraints = [] } ] }
            let parent =
                makeLdc "pexpr" (Some paramDefs) (
                    [ x; y; cc; out ],
                    [ conn x 0 cc 0; conn y 0 cc 1; conn cc 0 out 0 ])
            let outs = run parent [ childLdc ] (Map [ "X", 200I; "Y", 100I ]) |> expectOk
            Expect.equal outs["S"] 44I "child width follows the parent parameter"
        }

        test "CustomCompParam slot carries a parameter down two levels" {
            // grandparent binds P=8 on its instance of pmid; pmid's CustomCompParam slot
            // binds the child's W to P; the child must resolve at width 8
            let a2 = makeComp 1 0 1 (Input1(4, None)) "A2"
            let b2 = makeComp 2 0 1 (Input1(4, None)) "B2"
            let ccc = childInstance 3 "CCC" 4 None
            let s2 = makeComp 4 1 0 (Output 4) "S2"
            let pExpr = { Expression = PParameter(ParamName "P"); Constraints = [] }
            let midDefs =
                { DefaultBindings = Map [ declares "P" (PInt 4I) ]
                  ParamSlots =
                    Map [ {CompId = ComponentId 1; CompSlot = IO "A2" }, pExpr
                          {CompId = ComponentId 2; CompSlot = IO "B2" }, pExpr
                          {CompId = ComponentId 4; CompSlot = IO "S2" }, pExpr
                          {CompId = ComponentId 3; CompSlot = CustomCompParam "W" }, pExpr ] }
            let midLdc =
                makeLdc "pmid" (Some midDefs) (
                    [ a2; b2; ccc; s2 ],
                    [ conn a2 0 ccc 0; conn b2 0 ccc 1; conn ccc 0 s2 0 ])
            let gx = makeComp 1 0 1 (Input1(8, None)) "GX"
            let gy = makeComp 2 0 1 (Input1(8, None)) "GY"
            let gcc =
                makeComp 3 2 1
                    (customOf midLdc [ "A2", 8; "B2", 8 ] [ "S2", 8 ] (Some(Map [ ParamName "P", PInt 8I ])))
                    "GCC"
            let gs = makeComp 4 1 0 (Output 8) "GS"
            let gp =
                makeLdc "pgrand" None (
                    [ gx; gy; gcc; gs ],
                    [ conn gx 0 gcc 0; conn gy 0 gcc 1; conn gcc 0 gs 0 ])
            let outs = run gp [ midLdc; childLdc ] (Map [ "GX", 200I; "GY", 100I ]) |> expectOk
            Expect.equal outs["GS"] 44I "width 8 reaches the child through two levels"
        }

        // --- error reporting ---

        test "undefined parameter is an informative simulation error" {
            let a = makeComp 1 0 1 (Input1(4, None)) "A"
            let s = makeComp 2 1 0 (Output 4) "S"
            let defs =
                { DefaultBindings = Map.empty
                  ParamSlots =
                    Map [ {CompId = ComponentId 1; CompSlot = IO "A" },
                          { Expression = PParameter(ParamName "NOPE"); Constraints = [] } ] }
            let ldc = makeLdc "perr1" (Some defs) ([ a; s ], [ conn a 0 s 0 ])
            match run ldc [] (Map [ "A", 1I ]) with
            | Ok _ -> failtest "expected a simulation error"
            // the sheet declares nothing, so the message says what to do rather than listing a
            // scope that is empty; the name still reaches the user through the expression text
            | Error e -> Expect.stringContains $"%A{e}" "NOPE" "names the missing parameter"
        }

        test "division by zero is an informative simulation error" {
            let a = makeComp 1 0 1 (Input1(4, None)) "A"
            let s = makeComp 2 1 0 (Output 4) "S"
            let defs =
                { DefaultBindings = Map [ declares "W" (PDivide(PInt 4I, PInt 0I)) ]
                  ParamSlots =
                    Map [ {CompId = ComponentId 1; CompSlot = IO "A" },
                          { Expression = PParameter(ParamName "W"); Constraints = [] } ] }
            let ldc = makeLdc "perr2" (Some defs) ([ a; s ], [ conn a 0 s 0 ])
            match run ldc [] (Map [ "A", 1I ]) with
            | Ok _ -> failtest "expected a simulation error"
            | Error e -> Expect.stringContains $"%A{e}" "divided by 0" "reports the zero divisor"
        }

        // A memory's widths are parameters like any other value, and its contents are one map
        // shared by every instance of the sheet. So the same memory can hold its data in one
        // instance and not in another, and the instance that cannot is a fault in the design that
        // only simulation can find - at which point the memory has one definite shape.
        test "a memory whose contents do not fit its resolved width is a simulation error" {
            let memWith addressWidth =
                { Init = FromData
                  AddressWidth = addressWidth
                  WordWidth = 8
                  // one word at the top of a 4-bit address space, so it fits at W=4 and not below
                  Data = Map [ 15I, 200I ]
                  Comments = None }
            /// A sheet holding a ROM whose address width is its parameter W. Asynchronous, so
            /// that the word read appears without a clock tick and one step is enough to see it.
            let romSheet =
                let addr = makeComp 1 0 1 (Input1(4, None)) "ADDR"
                let rom = makeComp 2 1 1 (AsyncROM1 (memWith 4)) "ROM"
                let dout = makeComp 3 1 0 (Output 8) "DOUT"
                let wExpr = { Expression = PParameter(ParamName "W"); Constraints = [] }
                let defs =
                    { DefaultBindings = Map [ declares "W" (PInt 4I) ]
                      ParamSlots =
                        Map [ {CompId = ComponentId 2; CompSlot = MemoryAddressWidth }, wExpr
                              {CompId = ComponentId 1; CompSlot = IO "ADDR" }, wExpr ] }
                makeLdc "pmem" (Some defs)
                    ([ addr; rom; dout ], [ conn addr 0 rom 0; conn rom 0 dout 0 ])
            /// A parent driving one instance of that sheet at the given address width.
            let parentAt (w: int) =
                let x = makeComp 1 0 1 (Input1(w, None)) "X"
                let cc =
                    makeComp 2 1 1
                        (customOf romSheet [ "ADDR", w ] [ "DOUT", 8 ]
                            (Some (Map [ ParamName "W", PInt (bigint w) ])))
                        "MCC"
                let o = makeComp 3 1 0 (Output 8) "D"
                makeLdc "pmemparent" None ([ x; cc; o ], [ conn x 0 cc 0; conn cc 0 o 0 ])

            match run (parentAt 4) [ romSheet ] (Map [ "X", 15I ]) with
            | Ok outputs -> Expect.equal outputs[ "D" ] 200I "at four address bits the word is read"
            | Error e -> failtest $"expected the memory to fit at W=4: %A{e}"

            match run (parentAt 3) [ romSheet ] (Map [ "X", 0I ]) with
            | Ok _ -> failtest "expected a simulation error: address 15 has no place in 8 locations"
            | Error e ->
                Expect.stringContains $"%A{e}" "ROM" "names the memory that cannot hold its contents"
                Expect.stringContains $"%A{e}" "15" "and the location that does not fit"
        }

        test "self-referential parameter is an informative simulation error" {
            let a = makeComp 1 0 1 (Input1(4, None)) "A"
            let s = makeComp 2 1 0 (Output 4) "S"
            let defs =
                { DefaultBindings = Map [ declares "W" (PParameter(ParamName "W")) ]
                  ParamSlots =
                    Map [ {CompId = ComponentId 1; CompSlot = IO "A" },
                          { Expression = PParameter(ParamName "W"); Constraints = [] } ] }
            let ldc = makeLdc "perr3" (Some defs) ([ a; s ], [ conn a 0 s 0 ])
            match run ldc [] (Map [ "A", 1I ]) with
            | Ok _ -> failtest "expected a simulation error"
            | Error e -> Expect.stringContains $"%A{e}" "in terms of itself" "reports the cycle"
        }

        // A BusSelection's LSB and a BusCompare's comparison value live in an IO slot, not a
        // Buswidth one: the component's own width has already taken Buswidth. GraphMerger has to
        // apply those, or the canvas and the simulation disagree wherever an instance binds the
        // parameter away from its default. See SelectedComponentView.makeLsbBitNumberField.

        test "a parameterised BusSelection LSB is applied at the sheet default" {
            let outs = run (selectParent None) [ selectLdc ] (Map [ "X", 8I ]) |> expectOk
            Expect.equal outs["O"] 0I "bit 0 of 0b1000 at the default LSB of 0"
        }

        test "a parameterised BusSelection LSB is applied at an instance binding" {
            let bound = Some(Map [ ParamName "L", PInt 3I ])
            let outs = run (selectParent bound) [ selectLdc ] (Map [ "X", 8I ]) |> expectOk
            Expect.equal outs["O"] 1I "bit 3 of 0b1000 at the bound LSB of 3"
        }

        // The port widths shown for an instance are worked out by evaluating the CHILD sheet's IO
        // expressions. The environment for that is the child's declared defaults overridden by the
        // instance's bindings evaluated in the PARENT - not the instance bindings on their own.

        test "instance port widths resolve a parameter bound to a same-named parent parameter" {
            let parentDefs =
                { DefaultBindings = Map [ declares "W" (PInt 8I) ]; ParamSlots = Map.empty }
            let parentLdc = makeLdc "pwidths" (Some parentDefs) ([], [])
            let widths =
                ParameterView.portWidthsOfInstance
                    [ childLdc; parentLdc ]
                    (bindingsOf parentDefs.DefaultBindings)
                    childLdc.Name
                    (Map [ ParamName "W", PParameter(ParamName "W") ])
            // evaluating PParameter W against the bindings alone looks self-referential, and the
            // width used to come out as 0
            Expect.equal (Map.tryFind "A" widths) (Some 8) "input A takes the parent's W"
            Expect.equal (Map.tryFind "S" widths) (Some 8) "output S takes the parent's W"
        }

        test "instance port widths use the child default for a parameter the instance leaves unbound" {
            let widths =
                ParameterView.portWidthsOfInstance [ childLdc ] Map.empty childLdc.Name Map.empty
            Expect.equal (Map.tryFind "A" widths) (Some 4) "input A takes the child's declared default"
        }


        // An IO slot is stored under the label its component had when the slot was created, and
        // nothing rewrites it when the component is renamed. Every reader ignores it, so the width
        // must still be applied - it used to be dropped, and the port silently kept whatever width
        // the sheet was saved with.

        test "a renamed sheet input is still sized by its parameter slot in simulation" {
            let a = makeComp 1 0 1 (Input1(4, None)) "RENAMED"
            let s = makeComp 2 1 0 (Output 4) "S"
            let defs =
                { DefaultBindings = Map [ declares "W" (PInt 4I) ]
                  // the slot was created while the input was called A
                  ParamSlots =
                    Map [ {CompId = ComponentId 1; CompSlot = IO "A" }, { Expression = PParameter(ParamName "W"); Constraints = [] }
                          {CompId = ComponentId 2; CompSlot = IO "S" }, { Expression = PParameter(ParamName "W"); Constraints = [] } ] }
            let renamedLdc = makeLdc "prenamed" (Some defs) ([ a; s ], [ conn a 0 s 0 ])
            let x = makeComp 1 0 1 (Input1(8, None)) "X"
            let cc =
                makeComp 2 1 1
                    (customOf renamedLdc [ "RENAMED", 8 ] [ "S", 8 ] (Some(Map [ ParamName "W", PInt 8I ])))
                    "RCC"
            let o = makeComp 3 1 0 (Output 8) "O"
            let parent =
                makeLdc "prenamedparent" None ([ x; cc; o ], [ conn x 0 cc 0; conn cc 0 o 0 ])
            let outs = run parent [ renamedLdc ] (Map [ "X", 200I ]) |> expectOk
            Expect.equal outs["O"] 200I "200 passes through undamaged at the bound width of 8"
        }
    ]
