/// Property-based tests: the parameter expression language against a reference
/// evaluator and through the render/parse round-trip, number conversions, and the
/// >32-bit (bigint) simulation paths that exhaustive small-width tests cannot reach.
module Properties

open Expecto
open FsCheck
open CommonTypes
open ParameterTypes

// --- generators ---

let private paramNames = [ "pa"; "pb"; "pc" ]

let rec private genExpr size =
    let leaf =
        Gen.oneof [
            // negative literals included: they are written with unary minus, and the render/parse
            // round-trip below is what holds that reading and writing agree about it
            Gen.choose (-40, 40) |> Gen.map PInt
            Gen.elements paramNames |> Gen.map (ParamName >> PParameter)
        ]
    if size <= 0 then
        leaf
    else
        let sub = genExpr (size / 2)
        let binary op = Gen.map2 (fun a b -> op (a, b)) sub sub
        Gen.oneof [ leaf; binary PAdd; binary PSubtract; binary PMultiply; binary PDivide; binary PRemainder ]

// must be public: FsCheck discovers generator members reflectively
type ExprGens =
    static member ParamExpression() = Arb.fromGen (Gen.sized genExpr)

let private config =
    { FsCheckConfig.defaultConfig with
        maxTest = 500
        arbitrary = [ typeof<ExprGens> ] }

/// Bindings giving every generated parameter a small nonzero-capable value
let private bindingsFor (a: int) (b: int) (c: int) : ParamBindings =
    Map [ ParamName "pa", PInt a; ParamName "pb", PInt b; ParamName "pc", PInt c ]

/// Straight-line reference evaluator: no cycle detection needed since bindings are literals
let rec private refEval (bindings: Map<string, int>) (expr: ParamExpression) : Result<int, unit> =
    let binary op l r =
        match refEval bindings l, refEval bindings r with
        | Ok l, Ok r -> op l r
        | _ -> Error()
    match expr with
    | PInt n -> Ok n
    | PParameter(ParamName name) -> Ok bindings[name]
    | PAdd(l, r) -> binary (fun l r -> Ok(l + r)) l r
    | PSubtract(l, r) -> binary (fun l r -> Ok(l - r)) l r
    | PMultiply(l, r) -> binary (fun l r -> Ok(l * r)) l r
    | PDivide(l, r) -> binary (fun l r -> if r = 0 then Error() else Ok(l / r)) l r
    | PRemainder(l, r) -> binary (fun l r -> if r = 0 then Error() else Ok(l % r)) l r

let private sameOutcome (production: Result<int, string>) (reference: Result<int, unit>) =
    match production, reference with
    | Ok p, Ok r -> p = r
    | Error _, Error _ -> true
    | _ -> false

let tests =
    testList "Properties" [

        testPropertyWithConfig config "evaluateParamExpression agrees with reference evaluator"
        <| fun (expr: ParamExpression) (a: int) (b: int) (c: int) ->
            let a, b, c = abs a % 20, abs b % 20, abs c % 20
            let refBindings = Map [ "pa", a; "pb", b; "pc", c ]
            sameOutcome (evaluateParamExpression (bindingsFor a b c) expr) (refEval refBindings expr)

        testPropertyWithConfig config "render then parse preserves meaning"
        <| fun (expr: ParamExpression) (a: int) (b: int) (c: int) ->
            let a, b, c = abs a % 20, abs b % 20, abs c % 20
            let bindings = bindingsFor a b c
            match parseExpression (renderParamExpression expr 0) with
            | Error err -> failwith $"Rendered expression failed to parse: {err}"
            | Ok reparsed ->
                match evaluateParamExpression bindings expr, evaluateParamExpression bindings reparsed with
                | Ok v1, Ok v2 -> v1 = v2
                | Error _, Error _ -> true
                | _ -> false

        // A name that does not resolve is nearly always one of two mistakes, and the message has to
        // tell the user which one: they reached for a parameter that exists under another name, or
        // they did not know a value has to be declared as a parameter before it can be used.
        test "undefined parameter names the parameters that are in scope" {
            let bindings = Map [ ParamName "WIDTH", PInt 8; ParamName "DEPTH", PInt 4 ]
            match evaluateParamExpression bindings (PParameter(ParamName "WITDH")) with
            | Ok v -> failtest $"expected an error, got {v}"
            | Error e ->
                Expect.stringContains e "WITDH" "names the parameter that did not resolve"
                Expect.stringContains e "DEPTH, WIDTH" "lists the parameters of the sheet, sorted"
        }

        test "undefined parameter where the sheet declares none says the value must be numeric" {
            match evaluateParamExpression Map.empty (PParameter(ParamName "WIDTH")) with
            | Ok v -> failtest $"expected an error, got {v}"
            | Error e ->
                Expect.stringContains e "must be numeric" "says what to type instead"
                Expect.stringContains e "added to the sheet" "says how to get a parameter"
        }

        // --- what a parameter may be called ---
        //
        // The name rule and the parser's name token are one rule (isValidParamName), because a name
        // that cannot be written in an expression is of no use. They used to differ: names were
        // accepted as [a-zA-Z0-9]+ while the tokenizer read letters-then-digits, so W2X could be
        // declared and then never referred to.

        test "a name of letters and digits is valid and can be written in an expression" {
            Expect.isTrue (isValidParamName "W2X") "a letter, a digit and a letter"
            Expect.equal (parseExpression "W2X") (Ok(PParameter(ParamName "W2X")))
                "and the parser reads it as one name"
            Expect.equal (parseExpression "W2X*2") (Ok(PMultiply(PParameter(ParamName "W2X"), PInt 2)))
                "including in the middle of an expression"
        }

        test "a name must start with a letter" {
            Expect.isFalse (isValidParamName "2W") "a leading digit would be indistinguishable from a number"
            Expect.isFalse (isValidParamName "123") "and this one entirely so"
            Expect.isFalse (isValidParamName "") "a name is required"
            Expect.isFalse (isValidParamName "W_2") "an underscore is not part of the language"
            Expect.isTrue (isValidParamName "W") "the ordinary case"
        }

        test "a number run into a name says which of the two mistakes it is" {
            match parseExpression "2W" with
            | Ok e -> failtest $"expected an error, got {e}"
            | Error err ->
                Expect.stringContains err "start with a letter" "the name might predate the rule"
                Expect.stringContains err "2*W" "or a multiplication sign is missing"
        }

        test "every valid name parses as one name, whatever the letters and digits do" {
            [ "W"; "w"; "WIDTH"; "W2"; "W2X"; "A1B2C3"; "x9"; "Q0q0" ]
            |> List.iter (fun name ->
                Expect.isTrue (isValidParamName name) $"{name} is a valid name"
                Expect.equal (parseExpression name) (Ok(PParameter(ParamName name)))
                    $"and {name} parses as exactly that name")
        }

        // --- negative values ---
        //
        // `-4` used to read the minus sign as though it were a parameter name and then complain
        // about the 4, so a negative value could not be written at all.

        test "a negative literal parses as that literal" {
            Expect.equal (parseExpression "-4") (Ok(PInt -4)) "folded, so it renders back as typed"
            Expect.equal (renderParamExpression (PInt -4) 0) "-4" "and renders as the user wrote it"
        }

        test "unary minus binds to the operand it precedes" {
            let bindings = Map [ ParamName "W", PInt 8 ]
            let value text =
                parseExpression text
                |> Result.bind (evaluateParamExpression bindings)
            Expect.equal (value "-W") (Ok -8) "negating a parameter"
            Expect.equal (value "3--4") (Ok 7) "subtracting a negative"
            Expect.equal (value "2*-3") (Ok -6) "negating the right operand of a product"
            Expect.equal (value "-(W+1)") (Ok -9) "negating a parenthesised expression"
            Expect.equal (value "-W+10") (Ok 2) "and binding tighter than addition"
        }

        // --- constraints ---
        //
        // evaluateConstraints returns what is unmet rather than dispatching it. It used to send a
        // notification from inside a List.filter, and one of its callers is editParameterBox's
        // isDisabled - which the popup asks WHILE RENDERING whether its button should be greyed
        // out. A constraint that could not be evaluated would have dispatched from a render,
        // re-rendered, and dispatched again. Being a function of its arguments, it is testable
        // here at all, which is the other half of the point.

        test "a value within its constraints meets them" {
            let spec = { Expression = PInt 8; Constraints = [ MinVal(PInt 1, "too small"); MaxVal(PInt 16, "too big") ] }
            Expect.isOk (ParameterView.evaluateConstraints Map.empty [ spec ]) "8 is between 1 and 16"
        }

        test "a value outside a constraint comes back as that constraint" {
            let tooSmall = { Expression = PInt 0; Constraints = [ MinVal(PInt 1, "Width must be positive") ] }
            match ParameterView.evaluateConstraints Map.empty [ tooSmall ] with
            | Ok () -> failtest "expected the constraint to be unmet"
            | Error [ MinVal(_, message) ] ->
                Expect.equal message "Width must be positive" "the author's message reaches the caller unchanged"
            | Error other -> failtest $"expected one unmet MinVal, got {other}"
        }

        test "a limit that cannot be worked out fails the value it guards, and says why" {
            // the case that used to dispatch from a render: the bound refers to a parameter that
            // is not in scope, so nothing can be said about whether the value is within it
            let spec =
                { Expression = PInt 8
                  Constraints = [ MaxVal(PParameter(ParamName "MISSING"), "Width must fit the bus") ] }
            match ParameterView.evaluateConstraints Map.empty [ spec ] with
            | Ok () -> failtest "an unusable limit must not pass the value it guards"
            | Error [ MinVal(_, message) | MaxVal(_, message) ] ->
                Expect.stringContains message "Width must fit the bus" "keeps the author's message"
                Expect.stringContains message "could not be worked out" "and says what went wrong with it"
            | Error other -> failtest $"expected one unmet constraint, got {other}"
        }

        test "a value that cannot be worked out fails its constraints" {
            let spec =
                { Expression = PParameter(ParamName "MISSING"); Constraints = [ MinVal(PInt 1, "too small") ] }
            Expect.isError (ParameterView.evaluateConstraints Map.empty [ spec ])
                "returning no failures would let an undefined value through the guard"
        }

        testPropertyWithConfig config "wire data round-trip"
        <| fun (width: int) (value: bigint) ->
            let width = 1 + abs width % 64
            let value = (abs value) % (1I <<< width)
            NumberHelpers.convertWireDataToInt (NumberHelpers.convertIntToWireData width value) = value

        testPropertyWithConfig config "fast data round-trip"
        <| fun (width: int) (value: bigint) ->
            let width = 1 + abs width % 64
            let value = (abs value) % (1I <<< width)
            (NumberHelpers.convertBigintToFastData width value |> NumberHelpers.convertFastDataToBigint) = value

        testPropertyWithConfig config "hex format round-trip"
        <| fun (width: int) (value: bigint) ->
            let width = 1 + abs width % 64
            let value = (abs value) % (1I <<< width)
            NumberHelpers.strToIntCheckWidth width (NumberHelpers.hexBignum value) = Ok value

        // bigint simulation paths: 40-bit components exercise the BigIntStep arrays
        testPropertyWithConfig { config with maxTest = 40 } "40-bit adder matches bigint addition"
        <| fun (a: bigint) (b: bigint) (cin: bool) ->
            let w = 40
            let a, b = abs a % (1I <<< w), abs b % (1I <<< w)
            let cin = if cin then 1I else 0I
            let sum = a + b + cin
            let expected = [ sum % (1I <<< w); sum >>> w ]
            ComponentSemantics.simulate (NbitsAdder w) [ 1; w; w ] [ w; 1 ] [ cin; a; b ] = expected

        testPropertyWithConfig { config with maxTest = 40 } "40-bit logic matches bigint operators"
        <| fun (a: bigint) (b: bigint) ->
            let w = 40
            let a, b = abs a % (1I <<< w), abs b % (1I <<< w)
            ComponentSemantics.simulate (NbitsAnd w) [ w; w ] [ w ] [ a; b ] = [ a &&& b ]
            && ComponentSemantics.simulate (NbitsXor(w, None)) [ w; w ] [ w ] [ a; b ] = [ a ^^^ b ]
            && ComponentSemantics.simulate (NbitsNot w) [ w ] [ w ] [ a ]
               = [ a ^^^ ((1I <<< w) - 1I) ]

        testPropertyWithConfig { config with maxTest = 40 } "40-bit shifts match bigint shifts"
        <| fun (a: bigint) (amt: int) ->
            let w = 40
            let mask = (1I <<< w) - 1I
            let a = abs a % (1I <<< w)
            let amt = abs amt % (1 <<< 6)   // 6-bit shifter: amounts 0..63 cross the bus width
            let amtB = bigint amt
            let signSet = a >>> (w - 1) = 1I
            let expectLsl = if amt >= w then 0I else (a <<< amt) &&& mask
            let expectLsr = if amt >= w then 0I else a >>> amt
            let expectAsr =
                if amt >= w then (if signSet then mask else 0I)
                elif signSet then (a >>> amt) ||| (mask &&& (mask <<< (w - amt)))
                else a >>> amt
            ComponentSemantics.simulate (Shift(w, 6, LSL)) [ w; 6 ] [ w ] [ a; amtB ] = [ expectLsl ]
            && ComponentSemantics.simulate (Shift(w, 6, LSR)) [ w; 6 ] [ w ] [ a; amtB ] = [ expectLsr ]
            && ComponentSemantics.simulate (Shift(w, 6, ASR)) [ w; 6 ] [ w ] [ a; amtB ] = [ expectAsr ]

        // MergeN/SplitN crossing 32 bits: merge mixes a bigint input with uint32 inputs,
        // split produces one uint32 slice and one bigint slice from a bigint input
        testPropertyWithConfig { config with maxTest = 40 } "MergeN and SplitN at >32-bit widths"
        <| fun (a: bigint) (b: bigint) (c: bigint) ->
            let a = abs a % (1I <<< 40)
            let b = abs b % (1I <<< 8)
            let c = abs c % (1I <<< 8)
            let merged = a ||| (b <<< 40) ||| (c <<< 48)
            ComponentSemantics.simulate (MergeN 3) [ 40; 8; 8 ] [ 56 ] [ a; b; c ] = [ merged ]
            && ComponentSemantics.simulate (SplitN(2, [ 8; 40 ], [ 0; 8 ])) [ 56 ] [ 8; 40 ] [ merged ]
               = [ merged % (1I <<< 8); (merged >>> 8) % (1I <<< 40) ]

        testPropertyWithConfig config "shifter width is minimal but sufficient for every shift amount"
        <| fun (w: int) ->
            let w = 1 + abs w % 256
            let sw = shifterWidthFor w
            // 2^sw distinct values cover amounts 0 .. w-1, and one bit fewer would not
            (1 <<< sw) >= w && (sw = 1 || (1 <<< (sw - 1)) < w)

        // all-uint32 inputs merging to a bigint output
        testPropertyWithConfig { config with maxTest = 40 } "MergeN of two uint32 inputs to a >32-bit output"
        <| fun (x: bigint) (y: bigint) ->
            let x = abs x % (1I <<< 20)
            let y = abs y % (1I <<< 20)
            ComponentSemantics.simulate (MergeN 2) [ 20; 20 ] [ 40 ] [ x; y ] = [ x ||| (y <<< 20) ]
    ]
