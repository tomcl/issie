/// Tests for NumberHelpers: the numeric conversions and the width validation that
/// every value-entry path in the UI (step simulator inputs, constant and bus-compare
/// dialogs, the memory editor, .ram loading, Verilog) goes through.
module NumberHelpersTests

open Expecto
open SimGraphTypes
open NumberHelpers

/// The width-4 signed range is -8..7 and the unsigned range 0..15
let private widthCheckTests =
    testList "checkWidth" [
        test "accepts the unsigned range" {
            Expect.isNone (checkWidth 4 0I) "0 fits"
            Expect.isNone (checkWidth 4 15I) "15 is the largest 4-bit unsigned value"
        }
        test "rejects above the unsigned range" {
            Expect.isSome (checkWidth 4 16I) "16 needs 5 bits"
        }
        test "accepts the signed range" {
            Expect.isNone (checkWidth 4 (-1I)) "-1 fits"
            Expect.isNone (checkWidth 4 (-8I)) "-8 is the most negative 4-bit value"
        }
        test "rejects below the signed range" {
            // a negative needs a sign bit, so at width 4 it has only 3 bits of magnitude.
            // These used to be accepted and then silently converted: -9 became 7, -16 became 0
            Expect.isSome (checkWidth 4 (-9I)) "-9 does not fit in 4 bits"
            Expect.isSome (checkWidth 4 (-16I)) "-16 does not fit in 4 bits"
            Expect.isSome (checkWidth 4 (-17I)) "-17 does not fit in 4 bits"
        }
        test "width 1 holds only 0, 1 and -1" {
            Expect.isNone (checkWidth 1 0I) "0 fits"
            Expect.isNone (checkWidth 1 1I) "1 fits"
            Expect.isNone (checkWidth 1 (-1I)) "-1 fits"
            Expect.isSome (checkWidth 1 2I) "2 does not"
            Expect.isSome (checkWidth 1 (-2I)) "-2 does not"
        }
        testProperty "anything accepted converts back inside the width"
        <| fun (w: int) (n: bigint) ->
            let width = 1 + abs (w % 16)
            match checkWidth width n with
            | Some _ -> true // rejected, nothing to check
            | None ->
                let converted = twosComp width n
                converted >= 0I && converted < (1I <<< width)
        test "the reason names both permitted ranges" {
            // "Expected 4 or less bits" did not say why -9 is refused when 9 is accepted
            let msg = Expect.wantSome (checkWidth 4 (-9I)) "-9 is rejected at width 4"
            Expect.stringContains msg "0 to 15" "gives the unsigned range"
            Expect.stringContains msg "-8 to -1" "gives the negative range"
        }
    ]

let private wireDataTests =
    testList "wire data conversion" [
        test "non-negative values round-trip" {
            for n in 0I .. 15I do
                let wd = convertIntToWireData 4 n
                Expect.equal (List.length wd) 4 "width 4"
                Expect.equal (convertWireDataToInt wd) n $"round trip of {n}"
        }
        test "negative values become their two's complement" {
            // this used to mask with 2^width rather than 2^width - 1, which picks out the
            // single bit above the width - set for every in-range negative - so that every
            // negative came back as all-ones
            let cases = [ -1I, 15I; -2I, 14I; -5I, 11I; -8I, 8I ]
            for input, expected in cases do
                let got = convertIntToWireData 4 input |> convertWireDataToInt
                Expect.equal got expected $"{input} at width 4 is {expected}"
        }
        test "negatives are distinct from one another" {
            let values =
                [ -1I .. -1I .. -8I ]
                |> List.map (convertIntToWireData 4 >> convertWireDataToInt)
            Expect.equal (List.distinct values |> List.length) 8 "8 distinct negative values"
        }
        test "convertFastDataToWireData terminates on a BigWord" {
            // a shadowed `let rec` binding of the same name used to make this self-recursive
            // with unchanged arguments: an unbreakable infinite loop, reached from truth
            // table generation for any output or viewer wider than 32 bits
            let fd = { Dat = BigWord 5I; Width = 40 }
            let t = System.Threading.Tasks.Task.Run(fun () -> convertFastDataToWireData fd)

            if not (t.Wait(System.TimeSpan.FromSeconds 10.0)) then
                failtest "convertFastDataToWireData did not terminate on a BigWord"

            Expect.equal (List.length t.Result) 40 "40 bits out"
            Expect.equal (convertWireDataToInt t.Result) 5I "value preserved"
        }
        test "convertFastDataToWireData agrees with the Word path at width 32" {
            let asWord = convertFastDataToWireData { Dat = Word 12345u; Width = 32 }
            let asBig = convertFastDataToWireData { Dat = BigWord 12345I; Width = 32 }
            Expect.equal asBig asWord "the two representations give the same bits"
        }
    ]

let private parsingTests =
    testList "strToBigint" [
        test "the empty string does not throw" {
            // the length guard sat after an unguarded str[0] - && binds tighter than || - so
            // it never ran, and threw under .NET while returning Ok 0 under Fable
            Expect.equal (strToBigint "") (Ok 0I) "empty string parses as 0"
        }
        test "parses decimal, hex and binary" {
            Expect.equal (strToBigint "42") (Ok 42I) "decimal"
            Expect.equal (strToBigint "x2a") (Ok 42I) "hex"
            Expect.equal (strToBigint "b101010") (Ok 42I) "binary"
        }
        test "parses with commas and whitespace" {
            Expect.equal (strToBigint " x1,0000 ") (Ok 65536I) "commas stripped"
        }
        test "rejects nonsense" {
            Expect.isError (strToBigint "zz") "not a number"
        }
        test "strToIntCheckWidth applies the width check" {
            Expect.isError (strToIntCheckWidth 4 "-16") "-16 does not fit in 4 bits"
            Expect.equal (strToIntCheckWidth 4 "-8") (Ok(-8I)) "-8 does"
            Expect.equal (strToIntCheckWidth 4 "") (Ok 0I) "empty is 0"
        }
    ]

let private fastDataTests =
    testList "FastData" [
        test "MakeFastData masks into the width" {
            // every value stored in a step array must be within its bus width
            Expect.equal (FastData.MakeFastData 2 4I).GetBigInt 0I "4 at width 2 is 0"
            Expect.equal (FastData.MakeFastData 2 3I).GetBigInt 3I "3 at width 2 is 3"
            Expect.equal (FastData.MakeFastData 4 (-1I)).GetBigInt 15I "-1 at width 4 is 15"
            Expect.equal (FastData.MakeFastData 4 (-3I)).GetBigInt 13I "-3 at width 4 is 13"
        }
        test "MakeFastData picks Word or BigWord by width" {
            Expect.equal (FastData.MakeFastData 32 1I).Dat (Word 1u) "width 32 is a Word"

            match (FastData.MakeFastData 33 1I).Dat with
            | BigWord _ -> ()
            | d -> failtest $"width 33 should be a BigWord, got {d}"
        }
        testProperty "MakeFastData always produces a value inside its width"
        <| fun (w: int) (n: bigint) ->
            let width = 1 + abs (w % 64)
            let fd = FastData.MakeFastData width n
            fd.GetBigInt >= 0I && fd.GetBigInt < (1I <<< width)
    ]

/// FilesIO.readMemLines is the .ram parser. It reports the first bad line only, so that one
/// message has to say exactly where the problem is and what was wrong with it.
let private ramFileTests =
    let read (lines: string list) = FilesIO.readMemLines 4 8 (Array.ofList lines)

    testList "readMemLines" [
        test "parses a well-formed file" {
            let r = read [ "0 12"; "1 x1f"; "2 b1010" ]
            Expect.equal r (Ok [| 0I, 12I; 1I, 31I; 2I, 10I |]) "three definitions"
        }
        test "blank lines are allowed and do not shift line numbers" {
            // the line index used to be taken after blank lines were filtered out, so it
            // counted non-blank lines from 0 rather than numbering the file's lines
            let msg = Expect.wantError (read [ ""; "0 12"; ""; "1 999" ]) "line 4 is bad"
            Expect.stringContains msg "Line 4" "the real file line number"
        }
        test "a bad address says so, with its line and the reason" {
            let msg = Expect.wantError (read [ "0 12"; "99 3" ]) "99 needs more than 4 bits"
            Expect.stringContains msg "Line 2" "where"
            Expect.stringContains msg "invalid address (99)" "which item"
            Expect.stringContains msg "0 to 15" "why"
        }
        test "a bad data item says so, with its line and the reason" {
            // the data-item message used to omit the line number that the address one gave
            let msg = Expect.wantError (read [ "0 12"; "1 -200" ]) "-200 does not fit in 8 bits"
            Expect.stringContains msg "Line 2" "where"
            Expect.stringContains msg "invalid data item (-200)" "which item"
            Expect.stringContains msg "-128 to -1" "why"
        }
        test "a line with the wrong number of items says so" {
            let msg = Expect.wantError (read [ "0 12"; "1 2 3" ]) "three items"
            Expect.stringContains msg "Line 2" "where"
            Expect.stringContains msg "3 items" "why"
        }
        test "only the first error is reported" {
            let msg = Expect.wantError (read [ "99 1"; "98 1" ]) "both lines are bad"
            Expect.stringContains msg "Line 1" "the first one"
            Expect.isFalse (msg.Contains "Line 2") "and only that one"
        }
        test "repeated addresses are rejected" {
            Expect.isError (read [ "1 2"; "1 3" ]) "address 1 defined twice"
        }
    ]

let tests =
    testList
        "NumberHelpers"
        [ widthCheckTests; wireDataTests; parsingTests; fastDataTests; ramFileTests ]
