/// The pairwise list functions, made to mean the same thing under .NET as under Fable.
///
/// F# defines `List.map2` and its relatives only for lists of equal length and raises otherwise.
/// Fable's library instead builds most of them on a `fold2` that stops at the shorter list, so one
/// call raised under .NET and quietly returned a truncated answer in the app - and neither runtime
/// told you about the other. `CanvasExtractor.verticesAreSame` is what that was found through: it
/// folded two wires' vertices together before comparing their lengths, which was right in the app
/// and an exception in a test.
///
/// **These stop at the shorter list, in both runtimes.** That is Fable's behaviour, and Fable is
/// what ships, so it is the behaviour Issie has always had: `BusWireRoute` pairs one more vertex
/// index than there are segments on every wire it routes, and `BuildView` pads a bit list with
/// surplus entries precisely so that the extra ones are dropped. Making the two agree the other way
/// - raising, as F# specifies - turns working code into exceptions on the drawing and routing
/// paths, for an invariant nothing was relying on.
///
/// So these are deliberately total, and **a length mismatch that matters must be caught where it
/// means something** - by whatever returns an error to the user - rather than by an exception out
/// of a fold. `WidthInferer` is the model: it rejects a `SplitN` whose output widths and LSBs
/// disagree, which is what lets the twelve places that zip those two lists do so without asking.
///
/// **They shadow the FSharp.Core versions everywhere, with no `open` at any call site.** The module
/// is `[<AutoOpen>]` and the inner module is named `List`, so an ordinary `List.map2` resolves here
/// in both projects. Name resolution still falls through to FSharp.Core for everything not defined
/// below, which is how extending a core module works.
///
/// Under Fable each function *is* the built-in and adds nothing. Under .NET each trims to the
/// common length first, which costs two length walks when the lists already match - and .NET is the
/// tests and the headless tooling, not the app.
///
/// `List.exists2` and `List.foldBack2` are deliberately not here: Fable raises in both of those, so
/// they already agree with .NET and there is nothing to reconcile. Arrays need none of this either
/// - `Array.map2` and its relatives raise in both runtimes.
[<AutoOpen>]
module ListPairs

module List =

#if !FABLE_COMPILER
    /// Trim two lists to their common length, so that the built-in below cannot raise. Returns them
    /// unchanged when they already match, which is every call that was never in doubt.
    let inline private trim2 (xs: 'a list) (ys: 'b list) =
        let n1, n2 = xs.Length, ys.Length

        if n1 = n2 then
            xs, ys
        else
            let n = min n1 n2

            Microsoft.FSharp.Collections.List.truncate n xs, Microsoft.FSharp.Collections.List.truncate n ys

    let inline private trim3 (xs: 'a list) (ys: 'b list) (zs: 'c list) =
        let n1, n2, n3 = xs.Length, ys.Length, zs.Length

        if n1 = n2 && n1 = n3 then
            xs, ys, zs
        else
            let n = min n1 (min n2 n3)

            Microsoft.FSharp.Collections.List.truncate n xs,
            Microsoft.FSharp.Collections.List.truncate n ys,
            Microsoft.FSharp.Collections.List.truncate n zs
#endif

    let inline iter2 action (xs: 'a list) (ys: 'b list) =
#if FABLE_COMPILER
        Microsoft.FSharp.Collections.List.iter2 action xs ys
#else
        let xs, ys = trim2 xs ys
        Microsoft.FSharp.Collections.List.iter2 action xs ys
#endif

    let inline iteri2 action (xs: 'a list) (ys: 'b list) =
#if FABLE_COMPILER
        Microsoft.FSharp.Collections.List.iteri2 action xs ys
#else
        let xs, ys = trim2 xs ys
        Microsoft.FSharp.Collections.List.iteri2 action xs ys
#endif

    let inline map2 mapping (xs: 'a list) (ys: 'b list) =
#if FABLE_COMPILER
        Microsoft.FSharp.Collections.List.map2 mapping xs ys
#else
        let xs, ys = trim2 xs ys
        Microsoft.FSharp.Collections.List.map2 mapping xs ys
#endif

    let inline mapi2 mapping (xs: 'a list) (ys: 'b list) =
#if FABLE_COMPILER
        Microsoft.FSharp.Collections.List.mapi2 mapping xs ys
#else
        let xs, ys = trim2 xs ys
        Microsoft.FSharp.Collections.List.mapi2 mapping xs ys
#endif

    let inline fold2 folder state (xs: 'a list) (ys: 'b list) =
#if FABLE_COMPILER
        Microsoft.FSharp.Collections.List.fold2 folder state xs ys
#else
        let xs, ys = trim2 xs ys
        Microsoft.FSharp.Collections.List.fold2 folder state xs ys
#endif

    let inline forall2 predicate (xs: 'a list) (ys: 'b list) =
#if FABLE_COMPILER
        Microsoft.FSharp.Collections.List.forall2 predicate xs ys
#else
        let xs, ys = trim2 xs ys
        Microsoft.FSharp.Collections.List.forall2 predicate xs ys
#endif

    let inline zip (xs: 'a list) (ys: 'b list) =
#if FABLE_COMPILER
        Microsoft.FSharp.Collections.List.zip xs ys
#else
        let xs, ys = trim2 xs ys
        Microsoft.FSharp.Collections.List.zip xs ys
#endif

    let inline map3 mapping (xs: 'a list) (ys: 'b list) (zs: 'c list) =
#if FABLE_COMPILER
        Microsoft.FSharp.Collections.List.map3 mapping xs ys zs
#else
        let xs, ys, zs = trim3 xs ys zs
        Microsoft.FSharp.Collections.List.map3 mapping xs ys zs
#endif

    let inline zip3 (xs: 'a list) (ys: 'b list) (zs: 'c list) =
#if FABLE_COMPILER
        Microsoft.FSharp.Collections.List.zip3 xs ys zs
#else
        let xs, ys, zs = trim3 xs ys zs
        Microsoft.FSharp.Collections.List.zip3 xs ys zs
#endif

    //---------------------------------------------------------------------------------------------
    // Where a mismatch does mean something
    //---------------------------------------------------------------------------------------------
    //
    // The functions above stop at the shorter list, which is what to do where the lists cannot
    // disagree or where it does not matter if they do. Where it does matter the caller has to ask,
    // and what it then has to say is how long each list was - so these hand both numbers back with
    // the failure, rather than leaving the call site to walk the lists again to find out. A check
    // written that way is one line and its error message is built from what the check returned:
    //
    //     match List.checkedMap3 f [0..n-1] widths lsbs with
    //     | Error(nOuts, nWidths, nLsbs) -> Error $"...{nOuts}...{nWidths}...{nLsbs}..."
    //     | Ok msbs -> ...

    /// Pair two lists, or say how long each of them was.
    let checkedZip (xs: 'a list) (ys: 'b list) : Result<('a * 'b) list, int * int> =
        let n1, n2 = xs.Length, ys.Length

        if n1 = n2 then
            Ok(Microsoft.FSharp.Collections.List.zip xs ys)
        else
            Error(n1, n2)

    /// Combine two lists, or say how long each of them was.
    let checkedMap2 mapping (xs: 'a list) (ys: 'b list) : Result<'c list, int * int> =
        let n1, n2 = xs.Length, ys.Length

        if n1 = n2 then
            Ok(Microsoft.FSharp.Collections.List.map2 mapping xs ys)
        else
            Error(n1, n2)

    /// Pair three lists, or say how long each of them was.
    let checkedZip3 (xs: 'a list) (ys: 'b list) (zs: 'c list) : Result<('a * 'b * 'c) list, int * int * int> =
        let n1, n2, n3 = xs.Length, ys.Length, zs.Length

        if n1 = n2 && n1 = n3 then
            Ok(Microsoft.FSharp.Collections.List.zip3 xs ys zs)
        else
            Error(n1, n2, n3)

    /// Combine three lists, or say how long each of them was.
    let checkedMap3 mapping (xs: 'a list) (ys: 'b list) (zs: 'c list) : Result<'d list, int * int * int> =
        let n1, n2, n3 = xs.Length, ys.Length, zs.Length

        if n1 = n2 && n1 = n3 then
            Ok(Microsoft.FSharp.Collections.List.map3 mapping xs ys zs)
        else
            Error(n1, n2, n3)
