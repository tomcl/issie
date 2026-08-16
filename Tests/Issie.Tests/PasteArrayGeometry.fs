/// Where the copies of a pasted array go, and which way round to offer.
///
/// The Paste array dialog is built entirely out of these: which direction it defaults to, how many
/// copies it will allow, and when it warns before pasting. They are pure arithmetic over one
/// bounding box, so they can be checked here rather than by opening the dialog and reading it -
/// and the numbers in them (a fifth of the circuit between copies, three to one before warning)
/// are exactly what someone changing the feature would want stated somewhere.
module PasteArrayGeometry

open Expecto
open CommonTypes
open DrawModelType.SheetT

/// A box `w` by `h`, away from the origin so that a mistake using TopLeft as a size shows up.
let private box w h = { TopLeft = { X = 500.; Y = 300. }; W = w; H = h }

/// what a sheet's canvas is, near enough: big compared with anything drawn on it
let private canvas = 3500.

let tests =
    testList "Issie.PasteArrayGeometry" [

        test "a wide fragment is arrayed vertically, a tall one horizontally" {
            // copies stack up beside each other rather than end to end
            Expect.equal (Sheet.arrayDirectionFor (box 400. 100.)) ArrayVertical "wide"
            Expect.equal (Sheet.arrayDirectionFor (box 100. 400.)) ArrayHorizontal "tall"
            Expect.equal (Sheet.arrayDirectionFor (box 200. 200.)) ArrayVertical "square"
        }

        test "the gap between copies is a fifth of the fragment along the array" {
            Expect.equal (Sheet.arrayStep ArrayVertical (box 400. 100.)) 120. "vertical step"
            Expect.equal (Sheet.arrayStep ArrayHorizontal (box 400. 100.)) 480. "horizontal step"
        }

        test "each copy is one step further along, and only along" {
            let b = box 400. 100.
            Expect.equal (Sheet.arrayOffset ArrayVertical b 0) { X = 0.; Y = 0. } "first copy"
            Expect.equal (Sheet.arrayOffset ArrayVertical b 3) { X = 0.; Y = 360. } "fourth copy"
            Expect.equal (Sheet.arrayOffset ArrayHorizontal b 2) { X = 960.; Y = 0. } "third across"
        }

        test "an array is n copies plus the gaps between them, and no wider" {
            let arrayed = Sheet.arrayBox ArrayVertical 4 (box 400. 100.)
            // 4 copies of 100 and 3 gaps of 20
            Expect.equal arrayed.H 460. "height"
            Expect.equal arrayed.W 400. "width is unchanged"
            Expect.equal arrayed.TopLeft (box 400. 100.).TopLeft "starts where the fragment does"
        }

        test "one copy is an array of no gaps" {
            Expect.equal (Sheet.arrayBox ArrayVertical 1 (box 400. 100.)).H 100. "no gap added"
        }

        test "the copy count is the most that fit on the sheet" {
            let b = box 400. 1000.
            let n = Sheet.maxArrayCopies ArrayVertical canvas b
            Expect.isGreaterThan n 1 "some copies fit"
            Expect.isLessThanOrEqual (Sheet.arrayBox ArrayVertical n b).H canvas
                "the most that fit do fit"
            Expect.isGreaterThan (Sheet.arrayBox ArrayVertical (n + 1) b).H canvas
                "one more would not"
        }

        test "a fragment too big for two copies allows none" {
            // 2000 high, so two of them and a gap need 4400 of a 3500 canvas
            Expect.equal (Sheet.maxArrayCopies ArrayVertical canvas (box 400. 2000.)) 1 "no array"
        }

        test "arraying along the long side is warned about beyond three to one" {
            let wide = box 400. 100.        // 4:1
            let squarish = box 250. 100.    // 2.5:1
            Expect.isTrue (Sheet.arrayIsAgainstShape ArrayHorizontal wide)
                "a wide fragment arrayed horizontally runs along its long side"
            Expect.isFalse (Sheet.arrayIsAgainstShape ArrayVertical wide)
                "the same fragment arrayed vertically does not"
            Expect.isFalse (Sheet.arrayIsAgainstShape ArrayHorizontal squarish)
                "2.5:1 is not lopsided enough to warn about"
            Expect.isTrue (Sheet.arrayIsAgainstShape ArrayVertical (box 100. 400.))
                "a tall fragment arrayed vertically runs along its long side"
        }
    ]
