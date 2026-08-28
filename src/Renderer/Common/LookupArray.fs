/// A growable, index-addressed store: items are added in creation order, each one stamped with
/// the index it was stored at, and read back by that index.
///
/// It exists because the simulator's build phase was indexing its own components by structural
/// keys. A `Map<FComponentId, _>` - a component id and its access path - costs a boxed comparison
/// of the id and one of the list per lookup - 200,000 lookups into a 10,000-entry map measured
/// 77.2 ms as such a Map and 0.2 ms as an array index - and the build does millions of them.
/// The fix is not a faster map but an index: the thing being looked up is created by the same
/// walk that later reads it, so it can be given its position as it is made.
///
/// Nothing past `Count` is ever read, which is what lets the backing array be oversized without
/// an `option` per slot - the codebase forbids nulls, and `Array.zeroCreate` on a reference type
/// yields them. `Count` is the whole of the contract that keeps them out of sight.
module LookupArray

/// A store of 'T addressed by the index each item was stamped with when it was added.
///
/// Mutable, deliberately, and build-scoped: see docs/mutableState.md. Nothing outside the module
/// should write `Items` or `Count`.
type LookupArray<'T> =
    { /// backing store; entries at or past Count are undefined and must never be read
      mutable Items: 'T array
      mutable Count: int
      /// read an item's own record of its index
      GetIndex: 'T -> int
      /// write it, returning the stamped item
      AddIndexStamp: 'T -> int -> 'T
      /// growth cap, so a large array extends by a fixed step rather than doubling
      MaxIncrement: int }

/// A store of `capacity` items before it has to grow, extending by at most `maxIncrement` slots
/// at a time once it does.
///
/// The two functions are given separately rather than as a `Lens<'T,int>` so that the caller
/// chooses what stamping costs. A lens setter is a record copy, and the simulator's use of this
/// stores a 24-field record created hundreds of thousands of times per build - while that record
/// is `[<ReferenceEquality>]` and already carries mutable fields, so an index written in place
/// costs nothing and keeps the object identity the simulator relies on:
///
///     // in place - no copy, the same object back
///     LookupArray.create (fun fc -> fc.Index) (fun fc i -> fc.Index <- i; fc) n maxInc
///     // through a lens, where the type is immutable
///     LookupArray.create (Optic.get index_) (fun t i -> Optic.set index_ i t) n maxInc
///
/// It also keeps Optics out of this file, which is why it can sit anywhere in compile order.
let create (getIndex: 'T -> int) (addIndexStamp: 'T -> int -> 'T) (capacity: int) (maxIncrement: int) : LookupArray<'T> =
    { Items = Array.zeroCreate (max 0 capacity)
      Count = 0
      GetIndex = getIndex
      AddIndexStamp = addIndexStamp
      MaxIncrement = max 1 maxIncrement }

/// Double the backing store, or extend it by MaxIncrement, whichever is smaller. Doubling is
/// what makes a build whose size was not known in advance cheap; the cap is what stops a store
/// of millions of items asking for twice that again.
let private grow (store: LookupArray<'T>) =
    let old = store.Items.Length
    let increment = max 4 (min old store.MaxIncrement)
    let bigger: 'T array = Array.zeroCreate (old + increment)
    Array.blit store.Items 0 bigger 0 store.Count
    store.Items <- bigger

/// Add an item, stamping it with the index it is stored at.
///
/// **Use the returned item, never the argument.** With an in-place stamp the two are the same
/// object and with a lens they are not, so `addItem x store |> ignore` is correct in one flavour
/// and silently wrong in the other. The compiler cannot tell them apart.
let addItem (item: 'T) (store: LookupArray<'T>) : 'T =
    if store.Count >= store.Items.Length then
        grow store

    let stamped = store.AddIndexStamp item store.Count
    store.Items[store.Count] <- stamped
    store.Count <- store.Count + 1
    stamped

/// Replace the item at an index already in the store. Does not re-stamp: the index is the one
/// the item was given when it was added.
let updateItem (i: int) (item: 'T) (store: LookupArray<'T>) : unit =
    if i < 0 || i >= store.Count then
        failwithf $"LookupArray.updateItem: index {i} is outside the {store.Count} items stored"

    store.Items[i] <- item

/// The item at an index. The hot operation, and the reason this module exists.
let inline item (i: int) (store: LookupArray<'T>) : 'T = store.Items[i]

/// How many items have been added.
let inline count (store: LookupArray<'T>) = store.Count

/// The index an item was stamped with, read back off the item itself.
let inline indexOf (item: 'T) (store: LookupArray<'T>) = store.GetIndex item

/// A copy of what is stored, truncated to Count - so it never exposes an unwritten slot.
let toArray (store: LookupArray<'T>) : 'T array = Array.sub store.Items 0 store.Count

/// Apply a function to every stored item and its index, in creation order.
let iteri (f: int -> 'T -> unit) (store: LookupArray<'T>) : unit =
    for i in 0 .. store.Count - 1 do
        f i store.Items[i]
