/// The recent-projects list: what it holds, in what order, and how many.
///
/// The list is compared by string, so the form the paths are stored in IS the identity of a
/// project here. Before they were normalised the same folder opened from a dialog and from the
/// settings file was two different strings, and the list showed it twice - which these hold it to
/// not doing again. Both functions are pure and reachable under plain .NET, unlike the parts of the
/// same feature that need a Model.
module RecentProjects

open Expecto
open FilesIO
open MenuHelpers

/// What the list is expected to store: whatever this platform's separator makes of the path.
let private stored = normalisePath

let private limit = MenuHelpers.Constants.numberOfRecentProjects

let tests =
    testList "RecentProjects" [

        test "the same project spelled two ways is one entry" {
            // "a//b" and "a/./b" are the same folder written differently on either platform
            let recents = None |> addToRecents "proj/a//b" |> addToRecents "proj/a/./b"
            Expect.equal recents (Some [ stored "proj/a/b" ]) "one entry, whichever spelling arrived"
        }

        test "opening one again moves it to the front rather than adding it" {
            let recents =
                None
                |> addToRecents "p/one"
                |> addToRecents "p/two"
                |> addToRecents "p//one"

            Expect.equal recents (Some [ stored "p/one"; stored "p/two" ]) "newest first, still two"
        }

        test "a list saved with both spellings collapses when it is read" {
            let saved = Some [ "p/a"; "p//a"; "p/./a"; "p/b" ]
            Expect.equal (tidyRecents saved) (Some [ stored "p/a"; stored "p/b" ]) "two projects, not four"
        }

        test "the oldest goes when the limit is reached" {
            let recents =
                [ 1 .. limit + 1 ]
                |> List.fold (fun acc i -> addToRecents $"p/{i}" acc) None

            let expected = [ limit + 1 .. -1 .. 2 ] |> List.map (fun i -> stored $"p/{i}")
            Expect.equal recents (Some expected) "the limit holds, newest first"
        }

        test "a longer list saved by an older version is cut on the way in" {
            let saved = [ 1 .. limit + 3 ] |> List.map (fun i -> $"p/{i}") |> Some
            Expect.equal (tidyRecents saved |> Option.get |> List.length) limit "trimmed to the limit"
        }

        // The bug that started this is Windows-only: on POSIX a backslash is an ordinary filename
        // character, so two spellings there really are two different folders.
        if PathHelpers.sepChar = '\\' then
            testList "windows" [
                test "the two separators name the same project" {
                    let recents = None |> addToRecents "C:/proj/a" |> addToRecents "C:\\proj\\a"
                    Expect.equal recents (Some [ "C:\\proj\\a" ]) "one entry, written the platform's way"
                }
            ]
        else
            testList "posix" [
                test "a backslash is part of the name, so those are two projects" {
                    let recents = None |> addToRecents "proj/a" |> addToRecents "proj\\a"
                    Expect.equal (recents |> Option.get |> List.length) 2 "not the same folder here"
                }
            ]
    ]
