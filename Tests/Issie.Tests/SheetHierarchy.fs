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

/// `levels` sheets, each holding one instance of each of the next two down. Every sheet from the
/// third onwards is therefore reached by more than one route, and the number of routes to the
/// bottom is Fibonacci in the number of sheets - so the design is linear and its hierarchy is not.
let private diamond (levels: int) : Project =
    let sheetName i = $"s{i}"
    let sheets =
        [ for i in 0 .. levels - 1 ->
            let contents =
                [ for j in i + 1 .. min (i + 2) (levels - 1) -> instanceOf (sheetName j) $"U{i}_{j}" ]
            ldc (sheetName i) (contents, []) ]
    { ProjectPath = "."
      OpenFileName = sheetName 0
      WorkingFileName = Some (sheetName 0)
      LoadedComponents = sheets }

/// A sheet holding an instance of itself.
let private selfContaining =
    { ProjectPath = "."
      OpenFileName = "top"
      WorkingFileName = Some "top"
      LoadedComponents = [ ldc "top" ([ instanceOf "top" "SELF" ], []) ] }

let private nodeCount (tree: SheetTree) =
    let rec count (t: SheetTree) = 1 + List.sumBy count t.SubSheets
    count tree

let private treeOf allInstances (p: Project) =
    getSheetTreesFiltered (fun _ -> true) allInstances p |> Map.find p.OpenFileName

let private shapesOf (p: Project) = getSheetShapes (fun _ -> true) p

/// What the wave selector asks of materialiseTree: the sheets inside a sheet that more than one
/// route reaches are not built until the user opens one of the places it is reached.
let private selectorExpand (shapes: SheetShapes) (root: string) (opened: Set<string list>) =
    let multiPath = multiPathSheets shapes root
    fun (key: string list) ->
        match List.tryLast key with
        | Some sheet when Set.contains sheet multiPath -> Set.contains key opened
        | _ -> true

/// Every node of a tree, by SheetPath.
let private nodesByPath (tree: SheetTree) =
    let rec walk (t: SheetTree) = (t.SheetPath, t) :: List.collect walk t.SubSheets
    walk tree |> Map.ofList

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
            let tree = treeOf false selfContaining
            Expect.equal (nodeCount tree) 2 "the instance is a node, and it is not descended into"
        }

        //--------------------------------------------------------------------------------------//
        // Sharing the exploration.
        //
        // Collapsing several instances of one sheet inside one parent is enough for a chain, which
        // is what `nested` is. It is not enough for a design where one sheet is reached by several
        // routes: built a route at a time, `diamond` costs a Fibonacci number of nodes for a
        // linear number of sheets. What is inside each sheet is read once instead, and only the
        // occurrences that are going to be drawn are ever made into nodes.
        //--------------------------------------------------------------------------------------//

        test "what is inside each sheet is read once per sheet, not once per occurrence" {
            let shapes = shapesOf (diamond 30)
            Expect.equal (Map.count shapes) 30 "one entry per sheet in the design"
            Expect.equal
                (shapes["s0"] |> List.map (fun inst -> inst.InstSheet))
                [ "s1"; "s2" ]
                "a sheet's entry names the sheets inside it, in canvas order"
            Expect.equal shapes["s29"] [] "the bottom sheet holds nothing"
        }

        test "a sheet that contains itself is an ordinary entry" {
            // The shapes name each other rather than holding each other, so a cycle in a design is
            // a cycle in a Map and costs nothing. Only materialiseTree has to stop, and it stops on
            // the ancestor path rather than on anything worked out here.
            let shapes = shapesOf selfContaining
            Expect.equal (Map.count shapes) 1 "one sheet, one entry"
            Expect.equal (shapes["top"] |> List.map (fun inst -> inst.InstSheet)) [ "top" ] "naming itself"
        }

        test "sheets more than one route reaches are the ones found" {
            let chain = shapesOf (nested 6 3)
            Expect.isEmpty (multiPathSheets chain "s0" |> Set.toList)
                "a chain reaches each of its sheets one way, so nothing has to be suppressed"

            // s0 holds s1 and s2; s1 holds s2 and s3. So s2 is reached from both s0 and s1, and
            // everything below a sheet reached twice is itself reached twice.
            let dag = shapesOf (diamond 8)
            Expect.equal
                (multiPathSheets dag "s0" |> Set.toList |> List.sort)
                [ "s2"; "s3"; "s4"; "s5"; "s6"; "s7" ]
                "everything from the third sheet down"

            Expect.equal (multiPathSheets (shapesOf selfContaining) "top" |> Set.toList) [ "top" ]
                "a sheet reached from inside itself is reached more than once"
        }

        test "a suppressed node is a leaf, with its size and depth to match" {
            let shapes = shapesOf (diamond 8)
            let tree = materialiseTree (selectorExpand shapes "s0" Set.empty) false shapes "s0"
            let suppressed = (nodesByPath tree)[[ "s0"; "s2" ]]
            Expect.isEmpty suppressed.SubSheets "nothing below it was built"
            Expect.equal suppressed.Size 1 "Size counts what was built"
            Expect.equal suppressed.Depth 0 "and so does Depth"
            Expect.equal (nodeCount tree) tree.Size "Size is the tree that is there"
        }

        test "opening one occurrence of a sheet materialises that one and no other" {
            let shapes = shapesOf (diamond 8)
            let opened = Set.ofList [ [ "s0"; "s1"; "s2" ] ]
            let nodes = materialiseTree (selectorExpand shapes "s0" opened) false shapes "s0" |> nodesByPath
            Expect.isNonEmpty nodes[[ "s0"; "s1"; "s2" ]].SubSheets "the occurrence that was opened has its sheets"
            Expect.isEmpty nodes[[ "s0"; "s2" ]].SubSheets "the other occurrence of the same sheet has none"
        }

        // The companion to "building the menu tree costs the design, not its expansion", for the
        // shape a chain does not have. `diamond 30` is 1.6 million nodes fully expanded and 30
        // sheets on disk; the bound is loose on purpose, since the two are five orders of magnitude
        // apart and no amount of CI noise puts them on the same side of a second.
        test "exploring a design many routes cross costs the design, not its routes" {
            let project = diamond 30
            let timer = System.Diagnostics.Stopwatch.StartNew()
            let shapes = shapesOf project
            let tree = materialiseTree (selectorExpand shapes "s0" Set.empty) false shapes "s0"
            timer.Stop()
            Expect.isLessThan (nodeCount tree) 61
                "a node per sheet reached, and one suppressed node per route into it"
            Expect.isLessThan timer.ElapsedMilliseconds 1000L
                $"1.6 million occurrences must not be walked to show 30 sheets \
                  (took {timer.ElapsedMilliseconds}ms)"
        }
    ]
