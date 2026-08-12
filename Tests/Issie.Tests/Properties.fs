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
            Gen.choose (-40, 40) |> Gen.map (bigint >> PInt)
            // literals no int could hold: ParamInt is bigint, and a literal beyond Int32.MaxValue
            // used to parse as a parameter whose name was all digits
            Gen.choose (0, 40) |> Gen.map (fun n -> PInt ((1I <<< 40) + bigint n))
            Gen.elements paramNames |> Gen.map (ParamName >> PParameter)
        ]
    if size <= 0 then
        leaf
    else
        let sub = genExpr (size / 2)
        let binary op = Gen.map2 (fun a b -> op (a, b)) sub sub
        Gen.oneof [
            leaf
            binary PAdd
            binary PSubtract
            binary PMultiply
            binary PDivide
            binary PRemainder
            Gen.map PCLog2 sub
            binary (fun (a, b) -> PBinFunc (PMin, a, b))
            binary (fun (a, b) -> PBinFunc (PMax, a, b))
        ]

// must be public: FsCheck discovers generator members reflectively
type ExprGens =
    static member ParamExpression() = Arb.fromGen (Gen.sized genExpr)

let private config =
    { FsCheckConfig.defaultConfig with
        maxTest = 500
        arbitrary = [ typeof<ExprGens> ] }

/// Bindings giving every generated parameter a small nonzero-capable value
let private bindingsFor (a: bigint) (b: bigint) (c: bigint) : ParamBindings =
    Map [ ParamName "pa", PInt a; ParamName "pb", PInt b; ParamName "pc", PInt c ]

/// Ceiling log2 by repeated halving, which is a different computation from the shifting one in
/// ParameterTypes.clog2 - so this really checks the implementation rather than restating it.
let rec private refClog2 (n: bigint) (acc: bigint) =
    if n <= 1I then acc else refClog2 ((n + 1I) / 2I) (acc + 1I)

/// Straight-line reference evaluator: no cycle detection needed since bindings are literals
let rec private refEval (bindings: Map<string, bigint>) (expr: ParamExpression) : Result<bigint, unit> =
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
    | PDivide(l, r) -> binary (fun l r -> if r = 0I then Error() else Ok(l / r)) l r
    | PRemainder(l, r) -> binary (fun l r -> if r = 0I then Error() else Ok(l % r)) l r
    | PCLog2 e ->
        match refEval bindings e with
        | Ok n when n < 0I -> Error()
        | Ok n -> Ok(refClog2 n 0I)
        | Error() -> Error()
    | PBinFunc(PMin, l, r) -> binary (fun l r -> Ok(min l r)) l r
    | PBinFunc(PMax, l, r) -> binary (fun l r -> Ok(max l r)) l r

/// A generated number spread over a bus width.
///
/// FsCheck's bigint and int generators make SMALL numbers - a 200-sample check of
/// `abs a % (1I <<< 40)` found none above 99, none with a bit set above the seventh. So the
/// properties below, which look like they exercise 40-bit values, were exercising the bigint
/// arrays with values that fit in one byte: the sign bit was never set, nothing ever carried out
/// of the top, and no mask ever had anything to remove. Multiplying by a large odd constant before
/// masking spreads the generated number over the whole width, so the high bits vary as the low
/// ones do and about half the values have their sign bit set.
let private atWidth (w: int) (n: bigint) : bigint =
    (abs n * 6364136223846793005I) &&& ((1I <<< w) - 1I)

let private sameOutcome (production: Result<bigint, string>) (reference: Result<bigint, unit>) =
    match production, reference with
    | Ok p, Ok r -> p = r
    | Error _, Error _ -> true
    | _ -> false

let tests =
    testList "Properties" [

        testPropertyWithConfig config "evaluateParamExpression agrees with reference evaluator"
        <| fun (expr: ParamExpression) (a: int) (b: int) (c: int) ->
            let a, b, c = bigint (abs a % 20), bigint (abs b % 20), bigint (abs c % 20)
            let refBindings = Map [ "pa", a; "pb", b; "pc", c ]
            sameOutcome (evaluateParamExpression (bindingsFor a b c) expr) (refEval refBindings expr)

        testPropertyWithConfig config "render then parse preserves meaning"
        <| fun (expr: ParamExpression) (a: int) (b: int) (c: int) ->
            let a, b, c = bigint (abs a % 20), bigint (abs b % 20), bigint (abs c % 20)
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
            let bindings = Map [ ParamName "WIDTH", PInt 8I; ParamName "DEPTH", PInt 4I ]
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
            Expect.equal (parseExpression "W2X*2") (Ok(PMultiply(PParameter(ParamName "W2X"), PInt 2I)))
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
            Expect.equal (parseExpression "-4") (Ok(PInt -4I)) "folded, so it renders back as typed"
            Expect.equal (renderParamExpression (PInt -4I) 0) "-4" "and renders as the user wrote it"
        }

        test "unary minus binds to the operand it precedes" {
            let bindings = Map [ ParamName "W", PInt 8I ]
            let value text =
                parseExpression text
                |> Result.bind (evaluateParamExpression bindings)
            Expect.equal (value "-W") (Ok -8I) "negating a parameter"
            Expect.equal (value "3--4") (Ok 7I) "subtracting a negative"
            Expect.equal (value "2*-3") (Ok -6I) "negating the right operand of a product"
            Expect.equal (value "-(W+1)") (Ok -9I) "negating a parenthesised expression"
            Expect.equal (value "-W+10") (Ok 2I) "and binding tighter than addition"
        }

        // --- built-in functions ---
        //
        // clog2, min and max. A width derived from a size is a ceiling log2 - an address bus for an
        // N-word memory, the shift amount for an N-bit shifter - and before these there was no way
        // to write one: an identifier was unconditionally a parameter reference, so clog2(N) failed
        // with "Unexpected characters at end of expression: (N)".

        test "clog2 is the number of bits needed to index that many things" {
            let value n = evaluateParamExpression Map.empty (PCLog2(PInt n))
            Expect.equal (value 0I) (Ok 0I) "as Verilog's $clog2 gives for 0"
            Expect.equal (value 1I) (Ok 0I) "one thing needs no bits to pick out"
            Expect.equal (value 2I) (Ok 1I) "two things need one bit"
            Expect.equal (value 3I) (Ok 2I) "three need two, the ceiling rather than the floor"
            Expect.equal (value 4I) (Ok 2I) "and four still need two"
            Expect.equal (value 5I) (Ok 3I) "five need three"
            Expect.equal (value 8I) (Ok 3I) "an exact power of two is the log itself"
            Expect.equal (value 9I) (Ok 4I) "and one past it rounds up"
            Expect.equal (value (1I <<< 100)) (Ok 100I) "a size no int could hold"
        }

        // Not merely undefined: >>> on a negative bigint is an arithmetic shift and never reaches
        // zero, so an unguarded clog2 would hang the renderer rather than return anything.
        test "clog2 of a negative value is an error that names the function" {
            match evaluateParamExpression Map.empty (PCLog2(PInt -1I)) with
            | Ok v -> failtest $"expected an error, got {v}"
            | Error e ->
                Expect.stringContains e "clog2" "names the function that could not be worked out"
                Expect.stringContains e "negative" "and says what was wrong with the argument"
        }

        test "min and max choose between their two arguments" {
            let value text =
                parseExpression text |> Result.bind (evaluateParamExpression Map.empty)
            Expect.equal (value "min(3,8)") (Ok 3I) "the smaller"
            Expect.equal (value "max(3,8)") (Ok 8I) "the larger"
            Expect.equal (value "min(-3,-8)") (Ok -8I) "negative values compare as numbers"
            Expect.equal (value "max(2+3,4)") (Ok 5I) "an argument is a whole expression"
        }

        // The reason all three exist: a width derived from a size, clamped so that it is never 0.
        test "the functions compose with the rest of the language" {
            let bindings = Map [ ParamName "N", PInt 9I ]
            let value text = parseExpression text |> Result.bind (evaluateParamExpression bindings)
            Expect.equal (value "max(clog2(N),1)") (Ok 4I) "the case these were added for"
            Expect.equal (value "max(clog2(1),1)") (Ok 1I) "where the clamp is what decides it"
            Expect.equal (value "clog2(N)*2") (Ok 8I) "a call is an operand like any other"
            Expect.equal (value "-clog2(N)") (Ok -4I) "including under unary minus"
            Expect.equal (value "min(clog2(N),clog2(N*N))") (Ok 4I) "and calls nest"
        }

        test "a built-in function may be written in any case, and renders back in lower case" {
            let expected = Ok(PCLog2(PInt 8I))
            Expect.equal (parseExpression "clog2(8)") expected "lower case"
            Expect.equal (parseExpression "CLOG2(8)") expected "upper case"
            Expect.equal (parseExpression "CLog2(8)") expected "and mixed"
            Expect.equal (parseExpression "MAX(1,2)") (Ok(PBinFunc(PMax, PInt 1I, PInt 2I))) "likewise max"
            Expect.equal (renderParamExpression (PBinFunc(PMax, PInt 1I, PInt 2I)) 0) "max(1,2)"
                "and one spelling is written back, so the round-trip settles"
        }

        test "whitespace around a call is allowed, as everywhere else" {
            Expect.equal (parseExpression "clog2 ( 8 )") (Ok(PCLog2(PInt 8I))) "the tokenizer drops it"
            Expect.equal (parseExpression "min( 1 , 2 )") (Ok(PBinFunc(PMin, PInt 1I, PInt 2I))) "including round the comma"
        }

        test "a call written wrongly says how to write it" {
            let errorOf text =
                match parseExpression text with
                | Ok e -> failtest $"expected an error for '{text}', got {e}"
                | Error err -> err
            Expect.stringContains (errorOf "clog2") "clog2(expression)" "a function with no arguments at all"
            Expect.stringContains (errorOf "clog2 W") "clog2(expression)" "a function with no brackets"
            Expect.stringContains (errorOf "min(1)") "min(a,b)" "min with one argument"
            Expect.stringContains (errorOf "min(1,2,3)") "min(a,b)" "min with three"
            Expect.stringContains (errorOf "clog2(1,2)") "clog2(expression)" "clog2 with two"
            Expect.stringContains (errorOf "1,2") "comma" "and a comma outside any call"
        }

        // A parameter named min could be declared and then never written in an expression, since
        // the parser reads min as the function - the same reason a name may not start with a digit.
        test "a built-in function name cannot be a parameter name" {
            [ "clog2"; "min"; "max"; "CLOG2"; "Min"; "MAX" ]
            |> List.iter (fun name ->
                Expect.isFalse (isValidParamName name) $"{name} is a built-in function"
                Expect.isTrue (isReservedParamName name) $"and {name} is reported as reserved rather than malformed")
            [ "minimum"; "maxVal"; "clog2x"; "m"; "W" ]
            |> List.iter (fun name ->
                Expect.isTrue (isValidParamName name) $"{name} merely contains or resembles one"
                Expect.isFalse (isReservedParamName name) $"and {name} is not reserved")
        }

        // The list of two-argument functions is derived from ParamBinFunc by reflection, so that
        // adding a case reserves its name and reaches the parser with no second edit. Fable erases
        // reflection where the type argument is not resolved at the call site, and an empty list
        // here would quietly un-reserve the names and make min parse as a parameter.
        test "every case of ParamBinFunc is found by reflection and has a name" {
            let cases = EEExtensions.Union.allCases<ParamBinFunc> ()
            Expect.equal (List.length cases) 2 "PMin and PMax"
            Expect.equal (List.map binFuncName cases) [ "min"; "max" ] "in declaration order, named in lower case"
            cases
            |> List.iter (fun case ->
                Expect.equal (tryBuiltinBinFunc (binFuncName case)) (Some case) "and each name maps back to its case")
        }

        // --- values larger than an int ---
        //
        // ParamInt is bigint because the fields parameters feed are: a constant's value, a bus
        // comparison value and an input's default are all bigint, and a bus may be thousands of
        // bits wide.

        test "a literal too large for an int is a number, not a name" {
            let big = (1I <<< 40) + 7I
            Expect.equal (parseExpression (string big)) (Ok(PInt big))
                "read as int32 it fell through to being a parameter whose name was all digits"
            Expect.equal (renderParamExpression (PInt big) 0) (string big) "and renders back unchanged"
        }

        test "arithmetic on large values does not wrap" {
            let bindings = Map [ ParamName "W", PInt 100000I ]
            let value text = parseExpression text |> Result.bind (evaluateParamExpression bindings)
            Expect.equal (value "W*W") (Ok 10000000000I) "a product past Int32.MaxValue"
            Expect.equal (value "W*W/W") (Ok 100000I) "and it divides back down again"
        }

        // The one place a parameter value stops being a bigint. A width, an index and a bit
        // position are all int in ComponentType, and wrapping a value that does not fit is how a
        // nonsensical width would reach the canvas.
        test "narrowing to a component field refuses a value that does not fit" {
            Expect.equal (tryIntOfParamInt 42I) (Some 42) "an ordinary value"
            Expect.equal (tryIntOfParamInt (bigint System.Int32.MaxValue)) (Some System.Int32.MaxValue) "the largest that fits"
            Expect.equal (tryIntOfParamInt (bigint System.Int32.MinValue)) (Some System.Int32.MinValue) "and the smallest"
            Expect.equal (tryIntOfParamInt (bigint System.Int32.MaxValue + 1I)) None "one past the top"
            Expect.equal (tryIntOfParamInt (1I <<< 100)) None "and far past it"
        }

        test "a slot whose field is an int refuses a value too large to be one" {
            Expect.equal (ComponentSlots.trySetSlotValue Buswidth 16I (Register 8)) (Some(Register 16))
                "an ordinary width is applied"
            Expect.isNone (ComponentSlots.trySetSlotValue Buswidth (1I <<< 40) (Register 8))
                "one too large for an int is no more applicable than a slot the component lacks"
            // the whole reason parameter values are bigint: these two fields hold values no int can
            Expect.equal (ComponentSlots.trySetSlotValue (IO "x") (1I <<< 40) (BusCompare(64, 0I)))
                (Some(BusCompare(64, 1I <<< 40))) "a comparison value is a bigint and is passed through"
            Expect.equal (ComponentSlots.trySetSlotValue InputDefault (1I <<< 40) (Input1(64, None)))
                (Some(Input1(64, Some(1I <<< 40)))) "as is an input's default value"
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
            let spec = { Expression = PInt 8I; Constraints = [ MinVal(PInt 1I, "too small"); MaxVal(PInt 16I, "too big") ] }
            Expect.isOk (ParameterView.evaluateConstraints Map.empty [ spec ]) "8 is between 1 and 16"
        }

        test "a value outside a constraint comes back as that constraint" {
            let tooSmall = { Expression = PInt 0I; Constraints = [ MinVal(PInt 1I, "Width must be positive") ] }
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
                { Expression = PInt 8I
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
                { Expression = PParameter(ParamName "MISSING"); Constraints = [ MinVal(PInt 1I, "too small") ] }
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
            let a, b = atWidth w a, atWidth w b
            let cin = if cin then 1I else 0I
            let sum = a + b + cin
            let expected = [ sum % (1I <<< w); sum >>> w ]
            ComponentSemantics.simulate (NbitsAdder w) [ 1; w; w ] [ w; 1 ] [ cin; a; b ] = expected

        testPropertyWithConfig { config with maxTest = 40 } "40-bit logic matches bigint operators"
        <| fun (a: bigint) (b: bigint) ->
            let w = 40
            let a, b = atWidth w a, atWidth w b
            ComponentSemantics.simulate (NbitsAnd w) [ w; w ] [ w ] [ a; b ] = [ a &&& b ]
            && ComponentSemantics.simulate (NbitsXor(w, None)) [ w; w ] [ w ] [ a; b ] = [ a ^^^ b ]
            && ComponentSemantics.simulate (NbitsNot w) [ w ] [ w ] [ a ]
               = [ a ^^^ ((1I <<< w) - 1I) ]



        testPropertyWithConfig { config with maxTest = 40 } "40-bit shifts match bigint shifts"
        <| fun (a: bigint) (amt: int) ->
            let w = 40
            let mask = (1I <<< w) - 1I
            let a = atWidth w a
            let amt = abs amt % (1 <<< 6)   // 6-bit shifter: amounts 0..63 cross the bus width
            let amtB = bigint amt
            let signSet = a >>> (w - 1) = 1I
            let expectLsl = if amt >= w then 0I else (a <<< amt) &&& mask
            let expectLsr = if amt >= w then 0I else a >>> amt
            // An arithmetic shift is a division of the SIGNED value, rounding towards minus
            // infinity, put back into the width. Said that way this is an independent answer;
            // written as a right shift with the top bits filled in it would be the reducer's own
            // expression copied, which can only catch a change to it and never an error in it.
            // .NET's >>> on a negative bigint is itself arithmetic, which is what makes this short.
            let expectAsr =
                let signed = if signSet then a - (1I <<< w) else a
                let shifted =
                    if amt >= w then (if signSet then -1I else 0I)
                    else signed >>> amt
                shifted &&& mask
            ComponentSemantics.simulate (Shift(w, 6, LSL)) [ w; 6 ] [ w ] [ a; amtB ] = [ expectLsl ]
            && ComponentSemantics.simulate (Shift(w, 6, LSR)) [ w; 6 ] [ w ] [ a; amtB ] = [ expectLsr ]
            && ComponentSemantics.simulate (Shift(w, 6, ASR)) [ w; 6 ] [ w ] [ a; amtB ] = [ expectAsr ]

        // MergeN/SplitN crossing 32 bits: merge mixes a bigint input with uint32 inputs,
        // split produces one uint32 slice and one bigint slice from a bigint input
        testPropertyWithConfig { config with maxTest = 40 } "MergeN and SplitN at >32-bit widths"
        <| fun (a: bigint) (b: bigint) (c: bigint) ->
            let a = atWidth 40 a
            let b = atWidth 8 b
            let c = atWidth 8 c
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

        // shifterWidthFor is ceil(log2 w) clamped to 1, and is now clog2 rather than its own bit
        // count - so that clog2 in an expression means exactly what the SHIFT input does. It used
        // to count the bits of w-1 itself, which at w <= 0 shifted -1 right for ever.
        test "shifter width is the clamped clog2, and terminates at a width of zero" {
            [ 1..64 ]
            |> List.iter (fun w ->
                Expect.equal (shifterWidthFor w) (max 1 (int (ParameterTypes.clog2 (bigint w))))
                    $"shifterWidthFor {w} is its clamped clog2")
            Expect.equal (shifterWidthFor 0) 1 "a width of zero gives the clamp rather than hanging"
        }

        // all-uint32 inputs merging to a bigint output
        testPropertyWithConfig { config with maxTest = 40 } "MergeN of two uint32 inputs to a >32-bit output"
        <| fun (x: bigint) (y: bigint) ->
            let x = atWidth 20 x
            let y = atWidth 20 y
            ComponentSemantics.simulate (MergeN 2) [ 20; 20 ] [ 40 ] [ x; y ] = [ x ||| (y <<< 20) ]
    ]
