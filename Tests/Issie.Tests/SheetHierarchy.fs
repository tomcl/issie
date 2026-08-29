/// The tree of sheets the Sheets menu and the design-hierarchy breadcrumbs are drawn from.
///
/// It is built in the VIEW, so it is rebuilt on every render - which is every mouse move of a
/// drag. That makes the cost of building it a property worth pinning, and not only its shape: a
/// tree that comes out right after walking every instance in the design looks perfect in a test
/// of its contents while making the editor unusable.
module SheetHierarchy

open Expecto
open CommonTypes
open SimTypes
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
let private instanceOf (id: int) (name: string) (label: string) =
    { makeComp id 0 0 (Input1(1, None)) label with
        Type =
            Custom
                { Name = name; InputLabels = []; OutputLabels = []
                  Form = None; Description = None; ParameterBindings = None } }

/// The same sheet, saved `minutes` after the epoch these tests date from - so that "most recently
/// saved" is a fact of the project rather than of how fast the test ran.
let private savedAt (minutes: float) (ldc: LoadedComponent) =
    { ldc with TimeStamp = System.DateTime(2020, 1, 1).AddMinutes minutes }

/// A chain of `levels` sheets, each holding `instances` copies of the next one down.
/// Sheet 0 is the top; sheet (levels-1) is empty. The design has `levels` sheets in it and
/// expands to instances^(levels-1) instances of the bottom one.
let private nested (levels: int) (instances: int) : Project =
    let sheetName i = $"s{i}"
    let sheets =
        [ for i in 0 .. levels - 1 ->
            let contents =
                if i = levels - 1 then []
                else [ for j in 1 .. instances -> instanceOf j (sheetName (i + 1)) $"U{i}_{j}" ]
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
                [ for j in i + 1 .. min (i + 2) (levels - 1) -> instanceOf j (sheetName j) $"U{i}_{j}" ]
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
      LoadedComponents = [ ldc "top" ([ instanceOf 1 "top" "SELF" ], []) ] }

let private nodeCount (tree: SheetTree) =
    let rec count (t: SheetTree) = 1 + List.sumBy count t.SubSheets
    count tree

let private treeOf allInstances (p: Project) =
    getSheetTreesFiltered (fun _ -> true) allInstances p |> Map.find p.OpenFileName

let private shapesOf (p: Project) = getSheetShapes (fun _ -> true) p.LoadedComponents

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

/// A SimulatedDesign over a project's sheets: what a built simulation carries so that the
/// renderer can ask design questions without walking the expansion.
let private designOf (p: Project) : SimulatedDesign =
    { emptySimulatedDesign with
        DesignSheets = p.LoadedComponents
        DesignTopSheet = p.OpenFileName
        DesignComponentsById =
            p.LoadedComponents
            |> List.map (fun ldc ->
                ldc.Name,
                fst ldc.CanvasState |> List.map (fun comp -> comp.Id, comp) |> Map.ofList)
            |> Map.ofList }

let private memory = RAM1 { Init = FromData; AddressWidth = 2; WordWidth = 3; Data = Map.empty; Comments = None }

/// The same project with one more component drawn on one of its sheets.
let private withComponentOn (sheet: string) (comp: Component) (p: Project) =
    { p with
        LoadedComponents =
            p.LoadedComponents
            |> List.map (fun ldc ->
                if ldc.Name <> sheet then
                    ldc
                else
                    { ldc with CanvasState = comp :: fst ldc.CanvasState, snd ldc.CanvasState }) }

let private isMemory (comp: Component) =
    match comp.Type with
    | RAM1 _ | ROM1 _ | AsyncRAM1 _ | AsyncROM1 _ -> true
    | _ -> false

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
                    [ ldc "top" ([ instanceOf 1 "leaf" "L1"; instanceOf 2 "mid" "M1" ], [])
                      ldc "mid" ([ instanceOf 1 "leaf" "L2" ], [])
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

        // What the RAM selector and the step panel viewer list are built from. A component drawn
        // on a sheet is that component in every instance of the sheet, so the whole list follows
        // from the design - which is the point, because the alternative is the expansion.
        test "every instance of a component is found, named by the labels leading to it" {
            let design = nested 4 3 |> withComponentOn "s2" (makeComp 99 1 1 memory "M1") |> designOf
            let found = design.InstancesOfComponents isMemory

            // three instances of s1, each holding three of s2 - nine
            Expect.equal (List.length found) 9 "one per instance of the sheet the memory is drawn on"

            let depths =
                found |> List.map (fun (_, InstancePath ap) -> List.length ap) |> List.distinct
            Expect.equal depths [ 2 ] "each is two custom components deep"
            Expect.equal (List.length (List.distinct (List.map snd found))) 9
                "and they are nine DIFFERENT instances"

            let names = found |> List.map design.FullNameOf |> List.sort
            Expect.equal names.Head "U0_1.U1_1.M1"
                "named by the labels of the instances entered, then its own"
            Expect.equal (List.length (List.distinct names)) 9 "every one distinctly"
        }

        test "a component the design does not draw is not found" {
            let design = nested 4 3 |> designOf
            Expect.isEmpty (design.InstancesOfComponents isMemory) "no memory is drawn anywhere"
        }

        // The reason this can replace a walk of the FastComponents. `nested 15 3` expands to about
        // 7 million instances; the memory is on s2, so nine of them hold one and the twelve levels
        // below hold none. Not descending into those is the whole difference between this and
        // filtering the expansion, and the bound is loose because the two are orders of magnitude
        // apart.
        test "finding them costs the instances that hold one, not the expansion" {
            let design = nested 15 3 |> withComponentOn "s2" (makeComp 99 1 1 memory "M1") |> designOf
            let timer = System.Diagnostics.Stopwatch.StartNew()
            let found = design.InstancesOfComponents isMemory
            timer.Stop()
            Expect.equal (List.length found) 9 "nine instances of s2 hold the memory"
            Expect.isLessThan timer.ElapsedMilliseconds 1000L
                $"7 million instances must not be walked to find 9 memories \
                  (took {timer.ElapsedMilliseconds}ms)"
        }

        // Which components are clocked decides how the draw block colours a symbol, and - through
        // clockedSheets - how it colours every sheet containing one. isClockedPrimitive is a
        // second copy of the simulator own predicate, because it lives in CommonTypes and the
        // simulator is compiled after it, so the two are held to agreeing here.
        test "isClockedPrimitive agrees with the simulator about every component type" {
            let sample: ComponentType list =
                [ DFF; DFFE; Register 4; RegisterE 4; Counter 4; CounterNoEnable 4
                  CounterNoLoad 4; CounterNoEnableLoad 4
                  RAM1 { Init = FromData; AddressWidth = 2; WordWidth = 3; Data = Map.empty; Comments = None }
                  AsyncRAM1 { Init = FromData; AddressWidth = 2; WordWidth = 3; Data = Map.empty; Comments = None }
                  ROM1 { Init = FromData; AddressWidth = 2; WordWidth = 3; Data = Map.empty; Comments = None }
                  AsyncROM1 { Init = FromData; AddressWidth = 2; WordWidth = 3; Data = Map.empty; Comments = None }
                  Input1(1, None); Output 1; Viewer 1; IOLabel; Not; GateN(And, 2); Mux2; Demux2
                  NbitsAdder 4; BusSelection(1, 0); MergeWires; SplitWire 1 ]

            sample
            |> List.iter (fun compType ->
                Expect.equal
                    (isClockedPrimitive compType)
                    (SynchronousUtils.couldBeSynchronousComponent compType)
                    $"{compType}: the two predicates disagree")

            // spelled out, because these are the ones that were wrong: the memory types the
            // simulator actually uses were absent and the LEGACY ones were matched instead, so
            // every memory read as combinational
            Expect.isTrue (isClockedPrimitive (RAM1 { Init = FromData; AddressWidth = 2; WordWidth = 3; Data = Map.empty; Comments = None })) "a RAM is clocked"
            Expect.isFalse (isClockedPrimitive (AsyncROM1 { Init = FromData; AddressWidth = 2; WordWidth = 3; Data = Map.empty; Comments = None }))
                "an asynchronous ROM presents its data in the cycle of its address, so it is not"
        }

        test "a sheet whose only clocked component is a memory is a clocked sheet" {
            let project = nested 3 2 |> withComponentOn "s2" (makeComp 99 1 1 memory "M1")
            let clocked = clockedSheets project.LoadedComponents
            Expect.isTrue (Set.contains "s2" clocked) "the sheet the memory is drawn on"
            Expect.isTrue (Set.contains "s1" clocked) "and every sheet containing it, at any depth"
            Expect.isTrue (Set.contains "s0" clocked) "including the top"

            let plain = clockedSheets (nested 3 2).LoadedComponents
            Expect.isEmpty plain "a design of nothing but custom components is not clocked"
        }

        //--------------------------------------------------------------------------------------//
        // Which sheet a project opens at.
        //
        // The timestamp says which sheet was last worked on. What the user wants to see is the
        // DESIGN that sheet belongs to, opened at its top - a block on its own is out of context,
        // and climbing back up to the top was the first thing anyone did.
        //--------------------------------------------------------------------------------------//

        test "the root of the design the last-saved sheet belongs to is opened" {
            let project =
                [ ldc "top" ([ instanceOf 1 "mid" "M1" ], []) |> savedAt 0.
                  ldc "mid" ([ instanceOf 1 "leaf" "L1" ], []) |> savedAt 1.
                  ldc "leaf" ([], []) |> savedAt 2. ]
            Expect.equal (sheetOpenedOnLoad project) (Some "top")
                "the leaf was last saved, and the design it is part of tops out at top"
        }

        test "a root that was itself last saved is opened" {
            let project =
                [ ldc "top" ([ instanceOf 1 "leaf" "L1" ], []) |> savedAt 2.
                  ldc "leaf" ([], []) |> savedAt 0. ]
            Expect.equal (sheetOpenedOnLoad project) (Some "top") "a root is already its own top"
        }

        test "the last-saved of several unrelated sheets picks its own design" {
            // Two designs in one project: the one that was worked on is the one that opens.
            let project =
                [ ldc "topA" ([ instanceOf 1 "leafA" "L1" ], []) |> savedAt 0.
                  ldc "leafA" ([], []) |> savedAt 1.
                  ldc "topB" ([ instanceOf 2 "leafB" "L2" ], []) |> savedAt 5.
                  ldc "leafB" ([], []) |> savedAt 9. ]
            Expect.equal (sheetOpenedOnLoad project) (Some "topB") "leafB was last saved"
        }

        test "two roots reaching one sheet open that sheet" {
            // shared belongs to both designs and there is no saying which was meant, so the answer
            // is the highest sheet every route to it passes through - which is shared itself.
            let project =
                [ ldc "topA" ([ instanceOf 1 "shared" "S1" ], []) |> savedAt 0.
                  ldc "topB" ([ instanceOf 2 "shared" "S2" ], []) |> savedAt 0.
                  ldc "shared" ([], []) |> savedAt 5. ]
            Expect.equal (sheetOpenedOnLoad project) (Some "shared")
                "neither root is the answer, so the sheet itself is"
        }

        test "the sheet two designs meet at is as high as it goes" {
            // Both designs reach leaf, and both do so through shared. So shared is the highest
            // sheet every route to leaf passes through, and neither root is.
            let project =
                [ ldc "topA" ([ instanceOf 1 "shared" "S1" ], []) |> savedAt 0.
                  ldc "topB" ([ instanceOf 2 "shared" "S2" ], []) |> savedAt 0.
                  ldc "shared" ([ instanceOf 3 "leaf" "L1" ], []) |> savedAt 0.
                  ldc "leaf" ([], []) |> savedAt 5. ]
            Expect.equal (sheetOpenedOnLoad project) (Some "shared") "the sheet the designs share, and no higher"
        }

        test "a sheet reached twice within one design still opens at the root" {
            // Two routes from ONE top is not ambiguity: the design is the same either way. Both
            // routes start at top, so top is what every route to leaf passes through - even though
            // leaf itself has two parents, which a walk up single parents would have stopped at.
            let project =
                [ ldc "top" ([ instanceOf 1 "leaf" "L1"; instanceOf 2 "mid" "M1" ], []) |> savedAt 0.
                  ldc "mid" ([ instanceOf 1 "leaf" "L2" ], []) |> savedAt 0.
                  ldc "leaf" ([], []) |> savedAt 5. ]
            Expect.equal (sheetOpenedOnLoad project) (Some "top")
                "one design, so its root is the answer whatever the routes inside it are"
        }

        test "a sheet that contains itself has no root above it and is opened as it is" {
            Expect.equal (sheetOpenedOnLoad selfContaining.LoadedComponents) (Some "top")
                "nothing above the cycle is a root, so there is nothing to climb to"
        }

        test "a library sheet is neither opened nor climbed through" {
            // A library component is one thing rather than a sheet with innards, so its instance
            // does not make the sheet holding it a subsheet of anything.
            let library name canvas =
                { ldc name canvas with Form = Some (Library("stdlib", name)) }
            let project =
                [ ldc "top" ([ instanceOf 1 "L1_reg" "R1" ], []) |> savedAt 5.
                  library "L1_reg" ([], []) |> savedAt 9. ]
            Expect.equal (sheetOpenedOnLoad project) (Some "top")
                "the library sheet is not a candidate, and top is still a root"
        }

        test "a project with nothing the user can open has no sheet to open" {
            let library name = { ldc name ([], []) with Form = Some (Library("stdlib", name)) }
            Expect.equal (sheetOpenedOnLoad [ library "L1_reg" ]) None "nothing to choose"
        }
    ]
