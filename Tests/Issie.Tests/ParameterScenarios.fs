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
    let a = makeComp "a" 0 1 (Input1(4, None)) "A"
    let b = makeComp "b" 0 1 (Input1(4, None)) "B"
    let add = makeComp "add" 2 1 (NbitsAdderNoCinCout 4) "ADD"
    let s = makeComp "s" 1 0 (Output 4) "S"
    let canvas = [ a; b; add; s ], [ conn a 0 add 0; conn b 0 add 1; conn add 0 s 0 ]
    let wExpr = { Expression = PParameter(ParamName "W"); Constraints = [] }
    let paramDefs =
        { DefaultBindings = Map [ declares "W" (PInt 4) ]
          ParamSlots =
            Map [ { CompId = "a"; CompSlot = IO "A" }, wExpr
                  { CompId = "b"; CompSlot = IO "B" }, wExpr
                  { CompId = "add"; CompSlot = Buswidth }, wExpr
                  { CompId = "s"; CompSlot = IO "S" }, wExpr ] }
    makeLdc "pchild" (Some paramDefs) canvas

/// An instance of the child at width `w` (bindings given unless w is the default 4)
let private childInstance (id: string) (w: int) (bindings: ParamBindings option) =
    makeComp id 2 1 (customOf childLdc [ "A", w; "B", w ] [ "S", w ] bindings) (id.ToUpper())

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
    let x = makeComp "x" 0 1 (Input1(w, None)) "X"
    let y = makeComp "y" 0 1 (Input1(w, None)) "Y"
    let cc = childInstance "cc" w bindings
    let out = makeComp "sout" 1 0 (Output w) "S"
    makeLdc "pparent" None ([ x; y; cc; out ], [ conn x 0 cc 0; conn y 0 cc 1; conn cc 0 out 0 ])

/// A child sheet selecting one bit out of an 8-bit input, at a parameterised LSB.
let private selectLdc =
    let inp = makeComp "sin" 0 1 (Input1(8, None)) "IN"
    let sel = makeComp "sel" 1 1 (BusSelection(1, 0)) "SEL"
    let out = makeComp "sout" 1 0 (Output 1) "O"
    let canvas = [ inp; sel; out ], [ conn inp 0 sel 0; conn sel 0 out 0 ]
    let paramDefs =
        { DefaultBindings = Map [ declares "L" (PInt 0) ]
          ParamSlots =
            Map [ { CompId = "sel"; CompSlot = IO "SEL" },
                  { Expression = PParameter(ParamName "L"); Constraints = [] } ] }
    makeLdc "pselect" (Some paramDefs) canvas

/// A parent driving one instance of the bit selector
let private selectParent (bindings: ParamBindings option) =
    let x = makeComp "sx" 0 1 (Input1(8, None)) "X"
    let cc = makeComp "scc" 1 1 (customOf selectLdc [ "IN", 8 ] [ "O", 1 ] bindings) "SCC"
    let o = makeComp "so" 1 0 (Output 1) "O"
    makeLdc "pselparent" None ([ x; cc; o ], [ conn x 0 cc 0; conn cc 0 o 0 ])

let tests =
    testList "ParameterScenarios" [

        test "child instantiated at default width" {
            let parent = parentWith 4 None
            let outs = run parent [ childLdc ] (Map [ "X", 12I; "Y", 7I ]) |> expectOk
            Expect.equal outs["S"] 3I "12+7 wraps to 3 at the default width 4"
        }

        test "instance binding overrides the default width" {
            let parent = parentWith 8 (Some(Map [ ParamName "W", PInt 8 ]))
            let outs = run parent [ childLdc ] (Map [ "X", 200I; "Y", 100I ]) |> expectOk
            Expect.equal outs["S"] 44I "200+100 wraps to 44 at the bound width 8"
        }

        test "two instances with different bindings resolve independently" {
            let x8 = makeComp "x8" 0 1 (Input1(8, None)) "X8"
            let y8 = makeComp "y8" 0 1 (Input1(8, None)) "Y8"
            let x16 = makeComp "x16" 0 1 (Input1(16, None)) "X16"
            let y16 = makeComp "y16" 0 1 (Input1(16, None)) "Y16"
            let cc8 = childInstance "cc8" 8 (Some(Map [ ParamName "W", PInt 8 ]))
            let cc16 = childInstance "cc16" 16 (Some(Map [ ParamName "W", PInt 16 ]))
            let s8 = makeComp "s8" 1 0 (Output 8) "S8"
            let s16 = makeComp "s16" 1 0 (Output 16) "S16"
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
            let xa = makeComp "xa" 0 1 (Input1(8, None)) "XA"
            let ya = makeComp "ya" 0 1 (Input1(8, None)) "YA"
            let xb = makeComp "xb" 0 1 (Input1(8, None)) "XB"
            let yb = makeComp "yb" 0 1 (Input1(8, None)) "YB"
            let cca = childInstance "cca" 8 (Some(Map [ ParamName "W", PInt 8 ]))
            let ccb = childInstance "ccb" 8 (Some(Map [ ParamName "W", PInt 8 ]))
            let sa = makeComp "sa" 1 0 (Output 8) "SA"
            let sb = makeComp "sb" 1 0 (Output 8) "SB"
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
            let x = makeComp "x" 0 1 (Input1(8, None)) "X"
            let y = makeComp "y" 0 1 (Input1(8, None)) "Y"
            let cc = childInstance "cc" 8 (Some(Map [ ParamName "W", PParameter(ParamName "P") ]))
            let out = makeComp "sout" 1 0 (Output 8) "S"
            let paramDefs =
                { DefaultBindings = Map [ declares "P" (PInt 8) ]
                  ParamSlots =
                    Map [ { CompId = "x"; CompSlot = IO "X" }, { Expression = PParameter(ParamName "P"); Constraints = [] }
                          { CompId = "y"; CompSlot = IO "Y" }, { Expression = PParameter(ParamName "P"); Constraints = [] }
                          { CompId = "sout"; CompSlot = IO "S" }, { Expression = PParameter(ParamName "P"); Constraints = [] } ] }
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
            let a2 = makeComp "a2" 0 1 (Input1(4, None)) "A2"
            let b2 = makeComp "b2" 0 1 (Input1(4, None)) "B2"
            let ccc = childInstance "ccc" 4 None
            let s2 = makeComp "s2" 1 0 (Output 4) "S2"
            let pExpr = { Expression = PParameter(ParamName "P"); Constraints = [] }
            let midDefs =
                { DefaultBindings = Map [ declares "P" (PInt 4) ]
                  ParamSlots =
                    Map [ { CompId = "a2"; CompSlot = IO "A2" }, pExpr
                          { CompId = "b2"; CompSlot = IO "B2" }, pExpr
                          { CompId = "s2"; CompSlot = IO "S2" }, pExpr
                          { CompId = "ccc"; CompSlot = CustomCompParam "W" }, pExpr ] }
            let midLdc =
                makeLdc "pmid" (Some midDefs) (
                    [ a2; b2; ccc; s2 ],
                    [ conn a2 0 ccc 0; conn b2 0 ccc 1; conn ccc 0 s2 0 ])
            let gx = makeComp "gx" 0 1 (Input1(8, None)) "GX"
            let gy = makeComp "gy" 0 1 (Input1(8, None)) "GY"
            let gcc =
                makeComp "gcc" 2 1
                    (customOf midLdc [ "A2", 8; "B2", 8 ] [ "S2", 8 ] (Some(Map [ ParamName "P", PInt 8 ])))
                    "GCC"
            let gs = makeComp "gs" 1 0 (Output 8) "GS"
            let gp =
                makeLdc "pgrand" None (
                    [ gx; gy; gcc; gs ],
                    [ conn gx 0 gcc 0; conn gy 0 gcc 1; conn gcc 0 gs 0 ])
            let outs = run gp [ midLdc; childLdc ] (Map [ "GX", 200I; "GY", 100I ]) |> expectOk
            Expect.equal outs["GS"] 44I "width 8 reaches the child through two levels"
        }

        // --- error reporting ---

        test "undefined parameter is an informative simulation error" {
            let a = makeComp "a" 0 1 (Input1(4, None)) "A"
            let s = makeComp "s" 1 0 (Output 4) "S"
            let defs =
                { DefaultBindings = Map.empty
                  ParamSlots =
                    Map [ { CompId = "a"; CompSlot = IO "A" },
                          { Expression = PParameter(ParamName "NOPE"); Constraints = [] } ] }
            let ldc = makeLdc "perr1" (Some defs) ([ a; s ], [ conn a 0 s 0 ])
            match run ldc [] (Map [ "A", 1I ]) with
            | Ok _ -> failtest "expected a simulation error"
            | Error e -> Expect.stringContains $"%A{e}" "not defined" "names the missing parameter"
        }

        test "division by zero is an informative simulation error" {
            let a = makeComp "a" 0 1 (Input1(4, None)) "A"
            let s = makeComp "s" 1 0 (Output 4) "S"
            let defs =
                { DefaultBindings = Map [ declares "W" (PDivide(PInt 4, PInt 0)) ]
                  ParamSlots =
                    Map [ { CompId = "a"; CompSlot = IO "A" },
                          { Expression = PParameter(ParamName "W"); Constraints = [] } ] }
            let ldc = makeLdc "perr2" (Some defs) ([ a; s ], [ conn a 0 s 0 ])
            match run ldc [] (Map [ "A", 1I ]) with
            | Ok _ -> failtest "expected a simulation error"
            | Error e -> Expect.stringContains $"%A{e}" "divided by 0" "reports the zero divisor"
        }

        test "self-referential parameter is an informative simulation error" {
            let a = makeComp "a" 0 1 (Input1(4, None)) "A"
            let s = makeComp "s" 1 0 (Output 4) "S"
            let defs =
                { DefaultBindings = Map [ declares "W" (PParameter(ParamName "W")) ]
                  ParamSlots =
                    Map [ { CompId = "a"; CompSlot = IO "A" },
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
            let bound = Some(Map [ ParamName "L", PInt 3 ])
            let outs = run (selectParent bound) [ selectLdc ] (Map [ "X", 8I ]) |> expectOk
            Expect.equal outs["O"] 1I "bit 3 of 0b1000 at the bound LSB of 3"
        }

        // The port widths shown for an instance are worked out by evaluating the CHILD sheet's IO
        // expressions. The environment for that is the child's declared defaults overridden by the
        // instance's bindings evaluated in the PARENT - not the instance bindings on their own.

        test "instance port widths resolve a parameter bound to a same-named parent parameter" {
            let parentDefs =
                { DefaultBindings = Map [ declares "W" (PInt 8) ]; ParamSlots = Map.empty }
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
    ]
