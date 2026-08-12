/// `ListPairs`, which decides what a pairwise list function does when the lists are not the same
/// length, and what to use instead where that matters.
///
/// The point of these is that they run under .NET while the app runs under Fable: the shim exists
/// because the two disagreed, so a test that only pinned .NET's behaviour would pin the half that
/// was never in question. What is checked here is the behaviour both are now held to.
module ListPairsTests

open Expecto
open CommonTypes
open CanvasBuilder

let tests =
    testList "ListPairs" [

        // Truncating rather than raising is the whole decision. BusWireRoute pairs one more vertex
        // index than there are segments on every wire it routes, and BuildView pads a list so that
        // the surplus is dropped - so raising here would be an exception on every wire drawn.
        test "pairing stops at the shorter list rather than raising" {
            Expect.equal (List.zip [ 1; 2; 3 ] [ "a"; "b" ]) [ 1, "a"; 2, "b" ]
                "zip stops when the shorter list runs out"
            Expect.equal (List.map2 (+) [ 1; 2; 3 ] [ 10; 20 ]) [ 11; 22 ]
                "and so does map2, whichever list is shorter"
            Expect.equal (List.map2 (+) [ 1; 2 ] [ 10; 20; 30 ]) [ 11; 22 ]
                "including when it is the first"
            Expect.isTrue (List.forall2 (=) [ 1; 2; 3 ] [ 1; 2 ])
                "forall2 asks only about the pairs that exist"
            Expect.equal (List.fold2 (fun acc a b -> acc + a * b) 0 [ 1; 2; 3 ] [ 1; 1 ]) 3
                "fold2 folds only over those pairs"
            let mutable seen = []
            List.iter2 (fun a b -> seen <- (a, b) :: seen) [ 1; 2; 3 ] [ 10 ]
            Expect.equal seen [ 1, 10 ] "iter2 visits only those pairs"
            Expect.equal (List.map3 (fun a b c -> a + b + c) [ 1; 2; 3 ] [ 10; 20 ] [ 100; 200; 300 ]) [ 111; 222 ]
                "and the three-list versions stop at the shortest of them"
        }

        test "equal lengths are paired in full" {
            Expect.equal (List.zip [ 1; 2 ] [ "a"; "b" ]) [ 1, "a"; 2, "b" ] "nothing is dropped"
            Expect.equal (List.map2 (+) [ 1; 2 ] [ 10; 20 ]) [ 11; 22 ] "nor here"
        }

        // The other half: where a mismatch means something, the checked versions say so and hand
        // back what an error message about it has to quote.
        test "the checked versions report each list's length" {
            Expect.equal (List.checkedZip [ 1; 2 ] [ "a"; "b" ]) (Ok [ 1, "a"; 2, "b" ])
                "equal lengths pair as normal"
            Expect.equal (List.checkedZip [ 1; 2; 3 ] [ "a" ]) (Error(3, 1))
                "and a mismatch gives both lengths, in order"
            Expect.equal (List.checkedMap2 (+) [ 1 ] [ 10; 20 ]) (Error(1, 2))
                "map2 likewise"
            Expect.equal (List.checkedMap3 (fun a b c -> a + b + c) [ 1 ] [ 2; 2 ] [ 3; 3; 3 ]) (Error(1, 2, 3))
                "and the three-list version gives all three"
            Expect.equal (List.checkedZip3 [ 1 ] [ 2 ] [ 3 ]) (Ok [ 1, 2, 3 ]) "when they agree"
        }

        // What that buys: a SplitN whose three parts disagree is rejected where it can be explained,
        // instead of silently splitting fewer bits than the sheet says. This is the check that lets
        // the twelve places pairing those two lists do so without asking.
        test "a SplitN whose widths and LSBs disagree is rejected with the numbers in the message" {
            let splitWith n widths lsbs =
                let comp = makeComp "s" 1 (List.length widths) (SplitN(n, widths, lsbs)) "S"
                let src = makeComp "i" 0 1 (Input1(8, None)) "I"
                let canvas = [ src; comp ], [ conn src 0 comp 0 ]
                BusWidthInferer.inferConnectionsWidth canvas

            match splitWith 2 [ 2; 2 ] [ 0 ] with
            | Ok _ -> failtest "a SplitN with two outputs and one LSB should not infer"
            | Error e ->
                Expect.stringContains e.Msg "2 outputs" "says how many outputs it claims"
                Expect.stringContains e.Msg "2 widths" "and how many widths it carries"
                Expect.stringContains e.Msg "1 LSBs" "and how many LSBs"

            match splitWith 2 [ 2; 2 ] [ 0; 2 ] with
            | Ok _ -> ()
            | Error e -> failtest $"a consistent SplitN should infer, but got: {e.Msg}"
        }
    ]
