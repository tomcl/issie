/// The growable, index-addressed store the simulator's build phase indexes itself with.
///
/// What is worth pinning here is not the array indexing - that is obvious - but the two things
/// that would fail silently. Growth must not lose or reorder what was already stored, and no
/// operation may ever hand back a slot that was allocated but never written: the backing array
/// is oversized on purpose and, for a reference type, `Array.zeroCreate` fills it with the nulls
/// the rest of the codebase refuses to have. `Count` is the whole of what keeps them out of
/// sight, so it is what is tested.
module LookupArrayTests

open Expecto
open LookupArray

/// A reference type stamped in place, which is the flavour the simulator uses: no record copy
/// per item, and the object identity preserved.
type private Boxed = { Name: string; mutable Index: int }

let private inPlaceStore capacity maxIncrement =
    create (fun (b: Boxed) -> b.Index) (fun b i -> b.Index <- i; b) capacity maxIncrement

/// An immutable record stamped through a copy, which is the other flavour create is built for.
type private Stamped = { Value: int; Idx: int }

let private copyingStore capacity maxIncrement =
    create (fun (s: Stamped) -> s.Idx) (fun s i -> { s with Idx = i }) capacity maxIncrement

let tests =
    testList "LookupArray" [

        test "an added item is stamped with the index it is stored at" {
            let store = inPlaceStore 4 16
            let a = addItem { Name = "a"; Index = -1 } store
            let b = addItem { Name = "b"; Index = -1 } store
            Expect.equal a.Index 0 "the first item goes at 0"
            Expect.equal b.Index 1 "and the next at 1"
            Expect.equal (item 0 store).Name "a" "read back by index"
            Expect.equal (item 1 store).Name "b" "read back by index"
        }

        test "the stamped item is the same object when the stamp is in place" {
            let store = inPlaceStore 4 16
            let original = { Name = "a"; Index = -1 }
            let returned = addItem original store
            Expect.isTrue (System.Object.ReferenceEquals(returned, original))
                "an in-place stamp gives the caller back what it passed in"
            Expect.isTrue (System.Object.ReferenceEquals(item 0 store, original))
                "and stores that same object"
        }

        test "a copying stamp stores the stamped copy, not the argument" {
            // the trap the doc comment on addItem warns about: with this flavour the argument is
            // NOT what ends up in the store, so ignoring the return value loses the stamp
            let store = copyingStore 4 16
            let original = { Value = 42; Idx = -1 }
            let returned = addItem original store
            Expect.equal returned.Idx 0 "the returned copy carries the index"
            Expect.equal original.Idx -1 "the argument is untouched, being immutable"
            Expect.equal (item 0 store).Idx 0 "and what is stored is the stamped copy"
            Expect.equal (indexOf (item 0 store) store) 0 "which GetIndex reads back"
        }

        test "growth from an empty store keeps every item and its index" {
            let store = inPlaceStore 0 16
            let added = [ for i in 1..50 -> addItem { Name = string i; Index = -1 } store ]
            Expect.equal (count store) 50 "all fifty are stored"
            added
            |> List.iteri (fun i b ->
                Expect.equal b.Index i "each was stamped with its own position"
                Expect.equal (item i store).Name (string (i + 1)) "and is at that position")
        }

        test "growth across the MaxIncrement boundary neither loses nor reorders" {
            // capacity 2 and an increment cap of 3: it doubles while small, then extends by 3,
            // so the 40 items below cross the cap several times
            let store = inPlaceStore 2 3
            for i in 0..39 do
                addItem { Name = string i; Index = -1 } store |> ignore
            Expect.equal (count store) 40 "count follows the adds, not the backing array"
            Expect.equal
                (toArray store |> Array.map (fun b -> b.Name))
                [| for i in 0..39 -> string i |]
                "creation order is preserved across every growth"
            Expect.equal
                (toArray store |> Array.map (fun b -> b.Index))
                [| 0..39 |]
                "and so are the stamps"
        }

        test "Count never exposes an unwritten slot" {
            // the backing array is deliberately oversized and, for a reference type, full of the
            // nulls the codebase forbids. Nothing that reads the store may reach one.
            let store = inPlaceStore 1000 1000
            addItem { Name = "only"; Index = -1 } store |> ignore
            Expect.equal (count store) 1 "one item, however big the backing array"
            Expect.equal (toArray store).Length 1 "toArray is truncated to Count"
            Expect.isGreaterThan store.Items.Length (count store)
                "the backing array really is bigger, so this is not a vacuous test"
            let seen = ResizeArray<int * string>()
            iteri (fun i b -> seen.Add(i, b.Name)) store
            Expect.equal (List.ofSeq seen) [ 0, "only" ] "iteri stops at Count too"
        }

        test "toArray is a copy, not a view of the backing store" {
            let store = inPlaceStore 4 16
            addItem { Name = "a"; Index = -1 } store |> ignore
            let snapshot = toArray store
            addItem { Name = "b"; Index = -1 } store |> ignore
            Expect.equal snapshot.Length 1 "the copy taken before the second add is unchanged"
        }

        test "updateItem replaces in place and leaves the index alone" {
            let store = copyingStore 4 16
            addItem { Value = 1; Idx = -1 } store |> ignore
            addItem { Value = 2; Idx = -1 } store |> ignore
            updateItem 0 { Value = 99; Idx = 0 } store
            Expect.equal (item 0 store).Value 99 "the slot now holds the new item"
            Expect.equal (item 1 store).Value 2 "and its neighbour is untouched"
            Expect.equal (count store) 2 "updating is not adding"
        }

        test "updateItem past the end is refused rather than corrupting the store" {
            let store = inPlaceStore 100 100
            addItem { Name = "a"; Index = -1 } store |> ignore
            // slot 1 exists in the backing array but has never been written: writing it would
            // put an item into the store that Count says is not there
            Expect.throws
                (fun () -> updateItem 1 { Name = "b"; Index = 1 } store)
                "an index at or past Count is outside the store"
            Expect.throws
                (fun () -> updateItem -1 { Name = "b"; Index = -1 } store)
                "and so is a negative one"
        }

        test "an empty store has nothing in it" {
            let store = inPlaceStore 0 16
            Expect.equal (count store) 0 "no items"
            Expect.equal (toArray store) [||] "and nothing to enumerate"
        }
    ]
