/// The array sorts, made to keep ties in the same order under .NET as under Fable.
///
/// F#'s `Array.sort` family is not stable: on .NET it runs on `System.Array.Sort`, an introsort
/// which reorders equal elements once a partition grows past its insertion-sort threshold. Fable
/// compiles the same calls to JavaScript's `Array.prototype.sort`, which ES2019 requires to be
/// stable. So any array sorted on a key that can tie comes out in a different order in the app
/// than in the tests - and both orders look deterministic from inside their own runtime, which is
/// what makes the difference so hard to see.
///
/// It was found through wire separation: `BusWireSeparate.makeLines` sorted its line array by
/// coordinate, coincident same-port segments tied, and everything downstream read the array by
/// index. The app and the test suite laid out the same sheet differently from byte-identical
/// input - the eep1 TEST1/TEST2 pair was drawn as one trunk by the tests and as a loop by the
/// app - and the recorded quality numbers described a layout no user ever saw. That site now
/// sorts on a total key, which is the better fix where the order matters; this module is for
/// everywhere else, so the next tying key cannot reopen the gap.
///
/// **These are stable in both runtimes.** That is Fable's behaviour, and Fable is what ships, so
/// it is the behaviour Issie has always had. `List.sortBy` and `Seq.sortBy` need none of this:
/// F# specifies both as stable, and Fable agrees.
///
/// **They shadow the FSharp.Core versions everywhere, with no `open` at any call site.** The
/// module is `[<AutoOpen>]` and the inner module is named `Array`, so an ordinary `Array.sortBy`
/// resolves here in both projects. Name resolution still falls through to FSharp.Core for
/// everything not defined below.
///
/// Under Fable each function *is* the built-in and adds nothing. Under .NET each decorates the
/// elements with their index, sorts on (key, index) - a total key, so the built-in's instability
/// has nothing left to decide - and strips the index off again. That costs an allocation per
/// element, and .NET is the tests and the headless tooling, not the app.
[<AutoOpen>]
module ArraySorts

module Array =

    let inline sortWith (comparer: 'a -> 'a -> int) (xs: 'a[]) : 'a[] =
#if FABLE_COMPILER
        Microsoft.FSharp.Collections.Array.sortWith comparer xs
#else
        xs
        |> Microsoft.FSharp.Collections.Array.mapi (fun i x -> (x, i))
        |> Microsoft.FSharp.Collections.Array.sortWith (fun (a, i) (b, j) ->
            match comparer a b with
            | 0 -> compare i j
            | c -> c)
        |> Microsoft.FSharp.Collections.Array.map fst
#endif

    let inline sortBy (projection: 'a -> 'b) (xs: 'a[]) : 'a[] =
#if FABLE_COMPILER
        Microsoft.FSharp.Collections.Array.sortBy projection xs
#else
        xs
        |> Microsoft.FSharp.Collections.Array.mapi (fun i x -> ((projection x, i), x))
        |> Microsoft.FSharp.Collections.Array.sortBy fst
        |> Microsoft.FSharp.Collections.Array.map snd
#endif

    let inline sort (xs: 'a[]) : 'a[] =
#if FABLE_COMPILER
        Microsoft.FSharp.Collections.Array.sort xs
#else
        sortBy id xs
#endif

    let inline sortByDescending (projection: 'a -> 'b) (xs: 'a[]) : 'a[] =
#if FABLE_COMPILER
        Microsoft.FSharp.Collections.Array.sortByDescending projection xs
#else
        // descending on the key, but still ascending on the index: a stable descending sort keeps
        // tied elements in their original order, not reversed.
        xs
        |> Microsoft.FSharp.Collections.Array.mapi (fun i x -> ((projection x, i), x))
        |> Microsoft.FSharp.Collections.Array.sortWith (fun ((a, i), _) ((b, j), _) ->
            match compare b a with
            | 0 -> compare i j
            | c -> c)
        |> Microsoft.FSharp.Collections.Array.map snd
#endif

    let inline sortDescending (xs: 'a[]) : 'a[] =
#if FABLE_COMPILER
        Microsoft.FSharp.Collections.Array.sortDescending xs
#else
        sortByDescending id xs
#endif

    let inline sortInPlaceWith (comparer: 'a -> 'a -> int) (xs: 'a[]) : unit =
#if FABLE_COMPILER
        Microsoft.FSharp.Collections.Array.sortInPlaceWith comparer xs
#else
        System.Array.Copy(sortWith comparer xs, xs, xs.Length)
#endif

    let inline sortInPlaceBy (projection: 'a -> 'b) (xs: 'a[]) : unit =
#if FABLE_COMPILER
        Microsoft.FSharp.Collections.Array.sortInPlaceBy projection xs
#else
        System.Array.Copy(sortBy projection xs, xs, xs.Length)
#endif

    let inline sortInPlace (xs: 'a[]) : unit =
#if FABLE_COMPILER
        Microsoft.FSharp.Collections.Array.sortInPlace xs
#else
        System.Array.Copy(sort xs, xs, xs.Length)
#endif
