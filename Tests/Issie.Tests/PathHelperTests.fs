/// The path arithmetic FilesIO is built on.
///
/// These used to be node's `path` under Fable and System.IO.Path under .NET - two implementations
/// that quietly disagreed, and neither of which any test could reach. PathHelpers is one pure
/// implementation for both targets, which is what makes this file possible: every function here runs
/// under plain .NET exactly as it runs in the app.
///
/// The exhaustive check against node's own path module is not here - it cannot be, since node is not
/// running. It lives alongside as a differential harness run against the emitted JS while the
/// renderer still has node, and it is what established that these functions match node on 234 cases
/// including UNC and mixed separators. What this file holds is the behaviour Issie actually depends
/// on, so that a future edit to PathHelpers has to keep it.
module PathHelperTests

open Expecto
open PathHelpers

/// Written so the file passes on either platform: the expectations that differ are built from
/// sepChar rather than hard-coded, and the Windows-only shapes are in their own group.
let private sep = string sepChar

let private j (parts: string list) = join (Array.ofList parts)

let tests =
    testList "PathHelpers" [

        testList "join" [
            test "joins segments with the platform separator" {
                Expect.equal (j [ "a"; "b" ]) ("a" + sep + "b") "two segments"
                Expect.equal (j [ "a"; "b"; "c" ]) ("a" + sep + "b" + sep + "c") "three segments"
            }

            test "drops empty segments rather than doubling the separator" {
                Expect.equal (j [ "a"; ""; "b" ]) ("a" + sep + "b") "empty in the middle"
                Expect.equal (j [ ""; "a" ]) "a" "empty at the front"
                Expect.equal (j [ "a"; "" ]) "a" "empty at the back"
            }

            test "an empty join is the current directory, as node has it" {
                Expect.equal (j []) "." "no segments"
                Expect.equal (j [ "" ]) "." "one empty segment"
            }

            test "squeezes repeated separators" {
                Expect.equal (j [ "a/"; "/b" ]) ("a" + sep + "b") "separator on both sides"
            }

            test "resolves . and .. between segments" {
                Expect.equal (j [ "a"; "."; "b" ]) ("a" + sep + "b") "a dot segment"
                Expect.equal (j [ "a"; ".."; "b" ]) "b" "climbing one level"
                Expect.equal (j [ "a"; "b"; ".."; ".." ]) "." "climbing to nothing"
            }

            test "a relative path may keep leading .. because there is nothing to cancel it" {
                Expect.equal (j [ ".."; "a" ]) (".." + sep + "a") "cannot be simplified"
            }
        ]

        testList "basename, dirname and extname" [
            test "basename is the last segment" {
                Expect.equal (basename (j [ "proj"; "main.dgm" ])) "main.dgm" "file"
                Expect.equal (basename "main.dgm") "main.dgm" "already bare"
            }

            test "trailing separators do not change the basename" {
                Expect.equal (basename ("a" + sep + "b" + sep)) "b" "trailing separator"
            }

            test "dirname of a joined path is the directory it was joined from" {
                let dir = j [ "proj"; "backup" ]
                Expect.equal (dirname (j [ dir; "main.dgm" ])) dir "round trip"
            }

            test "dirname of a bare name is the current directory" {
                Expect.equal (dirname "main.dgm") "." "no directory part"
            }

            test "extname is the last suffix, and a dotfile has none" {
                Expect.equal (extname "main.dgm") ".dgm" "ordinary"
                Expect.equal (extname "a.b.c") ".c" "several dots"
                Expect.equal (extname ".gitignore") "" "dotfile"
                Expect.equal (extname "main") "" "no extension"
                Expect.equal (extname "." ) "" "the current directory is not an extension"
                Expect.equal (extname "..") "" "nor is the parent"
            }

            test "the shapes FilesIO builds: sheet, backup and settings paths" {
                // these are the three joins that carry user data, spelled as the callers spell them
                let proj = j [ "root"; "myProject" ]
                Expect.equal (basename (j [ proj; "cpu.dgm" ])) "cpu.dgm" "a sheet"
                Expect.equal (dirname (j [ proj; "cpu.dgm" ])) proj "its project"
                Expect.equal
                    (basename (j [ dirname (j [ proj; "cpu.dgm" ]); "backup" ]))
                    "backup"
                    "the backup directory beside it"
                Expect.equal (extname (j [ proj; "backup"; "cpu-001-2026-08-10-12h-00m.dgm" ]))
                    ".dgm"
                    "a timestamped backup keeps its extension despite the dots and dashes"
            }
        ]

        testList "roots" [
            test "a bare name is not absolute" {
                Expect.isFalse (isAbsolute "a") "relative"
                Expect.isFalse (isAbsolute ("a" + sep + "b")) "relative with a separator"
            }

            test "a rooted path is absolute and joining keeps the root" {
                let root = if sepChar = '\\' then "C:\\" else "/"
                Expect.isTrue (isAbsolute root) "the root itself"
                Expect.isTrue (isAbsolute (j [ root; "a" ])) "something under it"
                Expect.equal (dirname (j [ root; "a" ])) root "dirname returns the root"
                Expect.equal (basename root) "" "a root has no basename"
            }

            test ".. cannot climb above a root" {
                let root = if sepChar = '\\' then "C:\\" else "/"
                Expect.equal (j [ root; ".."; "a" ]) (root + "a") "stays at the root"
            }
        ]

        // Windows spells more kinds of root than POSIX does, and Issie is run from networked
        // locations on cluster machines, so UNC is not hypothetical here.
        if sepChar = '\\' then
            testList "windows" [
                test "forward slashes are separators too, and output is normalised to backslash" {
                    Expect.equal (join [| "C:/a"; "b" |]) "C:\\a\\b" "mixed input"
                    Expect.equal (basename "C:/a/b.dgm") "b.dgm" "basename over slashes"
                }

                test "a drive without a separator is relative to that drive" {
                    Expect.isFalse (isAbsolute "C:") "bare drive"
                    Expect.isFalse (isAbsolute "C:a") "drive-relative path"
                    Expect.isTrue (isAbsolute "C:\\") "drive root"
                }

                test "a UNC share is a root, and .. cannot climb out of it" {
                    let share = "\\\\server\\share"
                    Expect.isTrue (isAbsolute share) "UNC is absolute"
                    Expect.equal (join [| share; "p"; "s.dgm" |]) (share + "\\p\\s.dgm") "joining under a share"
                    Expect.equal (join [| share; "a"; ".."; "b" |]) (share + "\\b") "climbing within"
                    Expect.equal (join [| share; ".."; ".."; "x" |]) (share + "\\x") "cannot climb out"
                    Expect.equal (basename (share + "\\a.dgm")) "a.dgm" "basename under a share"
                }
            ]
        else
            testList "posix" [
                test "a backslash is an ordinary filename character" {
                    Expect.equal (basename "a/b\\c") "b\\c" "not a separator here"
                }
            ]
    ]
