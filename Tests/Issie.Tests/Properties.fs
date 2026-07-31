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
            Gen.choose (0, 40) |> Gen.map PInt
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

        // all-uint32 inputs merging to a bigint output
        testPropertyWithConfig { config with maxTest = 40 } "MergeN of two uint32 inputs to a >32-bit output"
        <| fun (x: bigint) (y: bigint) ->
            let x = abs x % (1I <<< 20)
            let y = abs y % (1I <<< 20)
            ComponentSemantics.simulate (MergeN 2) [ 20; 20 ] [ 40 ] [ x; y ] = [ x ||| (y <<< 20) ]
    ]
