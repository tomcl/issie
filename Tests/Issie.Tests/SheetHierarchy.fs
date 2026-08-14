/// The tree of sheets the Sheets menu and the design-hierarchy breadcrumbs are drawn from.
///
/// It is built in the VIEW, so it is rebuilt on every render - which is every mouse move of a
/// drag. That makes the cost of building it a property worth pinning, and not only its shape: a
/// tree that comes out right after walking every instance in the design looks perfect in a test
/// of its contents while making the editor unusable.
module SheetHierarchy

open Expecto
open CommonTypes
open CanvasBuilder
open MenuHelpers

let private ldc (name: string) (canvas: CanvasState) : LoadedComponent =
    { Name = name
      LoadedComponentIsOutOfDate = false
      WaveInfo = None
      TimeStamp = System.DateTime.Now
      FilePath = name + ".dgm"
      CanvasState = canvas
      InputLabels = []
      OutputLabels = []
      Form = Some User
      Description = None
      LCParameterSlots = None
      IsTopSheet = false }

/// an instance of sheet `name`, as a Custom component
let private instanceOf (name: string) (label: string) =
    { makeComp label 0 0 (Input1(1, None)) label with
        Type =
            Custom
                { Name = name; InputLabels = []; OutputLabels = []
                  Form = None; Description = None; ParameterBindings = None } }

/// A chain of `levels` sheets, each holding `instances` copies of the next one down.
/// Sheet 0 is the top; sheet (levels-1) is empty. The design has `levels` sheets in it and
/// expands to instances^(levels-1) instances of the bottom one.
let private nested (levels: int) (instances: int) : Project =
    let sheetName i = $"s{i}"
    let sheets =
        [ for i in 0 .. levels - 1 ->
            let contents =
                if i = levels - 1 then []
                else [ for j in 1 .. instances -> instanceOf (sheetName (i + 1)) $"U{i}_{j}" ]
            ldc (sheetName i) (contents, []) ]
    { ProjectPath = "."
      OpenFileName = sheetName 0
      WorkingFileName = Some (sheetName 0)
      LoadedComponents = sheets }

let private nodeCount (tree: SheetTree) =
    let rec count (t: SheetTree) = 1 + List.sumBy count t.SubSheets
    count tree

let private treeOf allInstances (p: Project) =
    getSheetTreesFiltered (fun _ -> true) allInstances p |> Map.find p.OpenFileName

let tests =
    testList "SheetHierarchy" [

        test "one instance of each sheet is what the menu tree holds" {
            let tree = nested 4 3 |> treeOf false
            // s0 -> s1 -> s2 -> s3, one node each, however many instances there are of each
            Expect.equal (nodeCount tree) 4 "one node per sheet in the chain"
            Expect.equal tree.Depth 3 "the chain is three deep below the top"
            Expect.equal tree.Size 4 "Size counts the tree it is part of"
        }

        test "every instance is a node when the caller asks for them" {
            // The waveform simulator's sheet selector wants each instance separately: they are
            // separate simulation sheets, with separate waveforms.
            let tree = nested 4 3 |> treeOf true
            Expect.equal (nodeCount tree) (1 + 3 + 9 + 27) "the full expansion"
            Expect.equal tree.Size (1 + 3 + 9 + 27) "Size is the size of the tree returned"
        }

        // The regression this file exists for. Instances of a sheet expand identically, so the
        // menu's tree - which keeps one of each - can be built without ever descending into the
        // others. Built the other way round, by expanding everything and thinning afterwards, this
        // design makes 7 million nodes and the answer is the same tree: the shape assertions above
        // pass either way, and only the clock tells them apart.
        //
        // The bound is loose on purpose. The work is 15 nodes, which is microseconds; the design
        // it is being held apart from is five orders of magnitude larger, so no machine, and no
        // amount of CI noise, puts the two on the same side of a second.
        test "building the menu tree costs the design, not its expansion" {
            let project = nested 15 3
            let timer = System.Diagnostics.Stopwatch.StartNew()
            let tree = treeOf false project
            timer.Stop()
            Expect.equal (nodeCount tree) 15 "one node per sheet"
            Expect.isLessThan timer.ElapsedMilliseconds 1000L
                $"14 million instances must not be walked to show 15 sheets \
                  (took {timer.ElapsedMilliseconds}ms)"
        }

        test "a sheet used twice at different depths appears under each of them" {
            // Not a chain: the top holds `leaf` directly and also holds `mid`, which holds `leaf`.
            // Deduplication is between siblings, so both routes to leaf are kept.
            let project =
                { ProjectPath = "."
                  OpenFileName = "top"
                  WorkingFileName = Some "top"
                  LoadedComponents =
                    [ ldc "top" ([ instanceOf "leaf" "L1"; instanceOf "mid" "M1" ], [])
                      ldc "mid" ([ instanceOf "leaf" "L2" ], [])
                      ldc "leaf" ([], []) ] }
            let tree = treeOf false project
            Expect.equal (nodeCount tree) 4 "top, its leaf, mid, and mid's leaf"
            let names = tree.SubSheets |> List.map (fun s -> s.SheetName) |> List.sort
            Expect.equal names [ "leaf"; "mid" ] "both children of the top are there"
        }

        test "a sheet that contains itself does not recur for ever" {
            let project =
                { ProjectPath = "."
                  OpenFileName = "top"
                  WorkingFileName = Some "top"
                  LoadedComponents = [ ldc "top" ([ instanceOf "top" "SELF" ], []) ] }
            let tree = treeOf false project
            Expect.equal (nodeCount tree) 2 "the instance is a node, and it is not descended into"
        }
    ]
