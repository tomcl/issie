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
    ]
